package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.ChiselSim
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import scala.collection.mutable
import utility.MemReqSource

// M1: read-only observations. No force, no production state modification.
class CDPAuditProbe(implicit val p: Parameters) extends Bundle {
  val candidate = Valid(new OceanusCDPCandidate)
  val allocated = Bool(); val duplicate = Bool(); val full = Bool()
  val terminal = Valid(new L2PrefetchTranslationResult)
  val request = Valid(UInt(50.W))
  val filterDrop = Valid(UInt(4.W))
  val output = Valid(new OceanusCDPRequest)
  val outputIndex = UInt(4.W)
  val resident = UInt(5.W)
  val enable = Bool()
}
object CDPAuditProbe {
  def connect(o: CDPAuditProbe, c: OceanusCDP, read: Data => Data): Unit = {
    def b[T <: Data](x: T): T = read(x).asInstanceOf[T]
    val s = c.sentUnit.get
    o.candidate.valid := b(s.io.in.valid) && b(s.io.in.ready)
    o.candidate.bits := b(s.io.in.bits)
    o.allocated := b(s.allocate); o.duplicate := b(s.duplicate); o.full := b(s.fullDrop)
    o.terminal.valid := b(s.io.tlb.resp.valid) && b(s.io.tlb.resp.ready)
    o.terminal.bits := b(s.io.tlb.resp.bits)
    o.request.valid := b(s.io.tlb.req.valid) && b(s.io.tlb.req.ready)
    o.request.bits := b(s.io.tlb.req.bits)
    o.filterDrop.valid := b(s.filterDrop); o.filterDrop.bits := b(s.idx2)
    o.output.valid := b(c.io.req.valid) && b(c.io.req.ready)
    o.output.bits := b(c.io.req.bits)
    o.outputIndex := b(s.output.io.deq.bits.index)
    o.resident := PopCount(b(s.valids)); o.enable := b(c.io.enable)
  }
}

// Finite identities live only from admission to exactly one terminal outcome.
// A page transaction binds its contemporaneous waiting set, not future offsets.
class CDPCandidateLedger extends ChiselSim {
  case class Item(id: Long, va: BigInt, depth: BigInt, source: BigInt, hit: Boolean,
    var epoch: Option[Long], var pa: Option[BigInt] = None)
  private val live = mutable.Map.empty[Int, Item]
  private val pages = mutable.Map.empty[BigInt, Long]
  private var identity = 0L; private var epoch = 0L
  private var outputs = 0L; private var filters = 0L; private var translations = 0L
  private var duplicates = 0L; private var full = 0L
  def pending: Int = live.size
  def counters:Map[String,Long]=Map("allocated"->identity,"output"->outputs,"filterDrop"->filters,
    "translationDrop"->translations,"pending"->live.size.toLong,"pagePending"->pages.size.toLong)
  def f(x: Bool): Boolean = x.litOption.getOrElse(x.peek().litValue)!=0
  def v(x: UInt): BigInt = x.litOption.getOrElse(x.peek().litValue)
  def sample(probe: CDPAuditProbe, cycle: Int): Unit = {
    val o=probe.peek()
    assert(v(o.resident)==live.size,s"M1 resident cycle=$cycle hardware=${v(o.resident)} identities=${live.size}")
    val candidate = if(f(o.candidate.valid)) Some((v(o.candidate.bits.pfVAddr)>>6<<6,
      v(o.candidate.bits.pfDepth),v(o.candidate.bits.pfSource),f(o.candidate.bits.isHit))) else None
    // Admission uses pre-edge state, even if a slot retires at this same edge.
    val isDuplicate=candidate.exists(x=>live.values.exists(_.va==x._1))
    val slot=(0 until 16).find(!live.contains(_))
    assert(f(o.duplicate)==isDuplicate && f(o.full)==(candidate.nonEmpty && !isDuplicate && slot.isEmpty))
    assert(f(o.allocated)==(candidate.nonEmpty && !isDuplicate && slot.nonEmpty))
    if(isDuplicate) duplicates+=1
    if(f(o.full)) full+=1
    if(f(o.terminal.valid)) {
      val t=o.terminal.bits; val page=v(t.vaddr)>>12
      val e=pages.remove(page).getOrElse(throw new AssertionError(s"M1 terminal has no bound page cycle=$cycle page=$page"))
      val waiting=live.toVector.filter { case (_,i)=>i.pa.isEmpty && i.va>>12==page }
      assert(waiting.nonEmpty && waiting.exists(_._2.epoch.contains(e)),s"M1 unowned terminal epoch=$e")
      waiting.foreach { case (index,i)=>
        assert(i.epoch.contains(e),s"M1 stale/future authorization id=${i.id} epoch=${i.epoch} result=$e")
        if(f(t.dropped)) { assert(live.remove(index).contains(i)); translations+=1 }
        else i.pa=Some((v(t.paddr)>>12<<12)|(i.va&0xfc0))
      }
    }
    if(f(o.request.valid)) {
      val page=v(o.request.bits)>>12
      assert(!pages.contains(page),s"M1 overlapping page epoch page=$page")
      val waiting=live.values.filter(i=>i.pa.isEmpty && i.va>>12==page).toVector
      assert(waiting.nonEmpty)
      pages(page)=epoch; waiting.foreach(_.epoch=Some(epoch)); epoch+=1
    }
    def retire(index: Int, output: Boolean): Unit = {
      val i=live.remove(index).getOrElse(throw new AssertionError(s"M1 duplicate/unknown retirement cycle=$cycle slot=$index"))
      assert(i.pa.nonEmpty && i.epoch.nonEmpty,s"M1 retirement before translation id=${i.id}")
      if(output) {
        val a=o.output.bits
        assert(f(o.enable),s"M1 disabled output cycle=$cycle")
        assert(v(a.paddr)==i.pa.get && v(a.pfDepth)==i.depth,s"M1 identity mismatch id=${i.id} VA=${i.va} PA=${i.pa} depth=${i.depth}")
        assert(v(a.pfSource)==9 && v(a.reqSource)==MemReqSource.Prefetch2L2CDP.id && !f(a.needT),"M1 CDP transport fields")
        outputs+=1
      } else filters+=1
    }
    if(f(o.filterDrop.valid)) retire(v(o.filterDrop.bits).toInt,false)
    if(f(o.output.valid)) retire(v(o.outputIndex).toInt,true)
    if(f(o.allocated)) {
      val (va,depth,source,hit)=candidate.get
      assert(!live.contains(slot.get))
      live(slot.get)=Item(identity,va,depth,source,hit,pages.get(va>>12)); identity+=1
    }
    assert(live.size<=16 && identity==outputs+filters+translations+live.size,"M1 exactly-once conservation")
  }
  def checkConservation(): Unit=assert(identity==outputs+filters+translations+live.size,"M4 candidate conservation")
  def finish(context: String): Unit = {
    assert(live.isEmpty && pages.isEmpty,s"M1 residual identities=$live pages=$pages")
    println(s"CDP_IDENTITY_RESULT pass=true $context admitted=$identity output=$outputs filterDrop=$filters translationDrop=$translations duplicate=$duplicates full=$full residual=${live.size} pageResidual=${pages.size}")
  }
}

// Explicit backend cap: verilator defaults to -j0, which supersedes make
// environment flags. Bound generated C++ compilation for every audit runner.
object CDPAuditSimulator {
  def bounded:chisel3.simulator.HasSimulator=chisel3.simulator.HasSimulator.simulators.verilator(
    verilatorSettings=svsim.verilator.Backend.CompilationSettings.default.withParallelism(
      Some(svsim.verilator.Backend.CompilationSettings.Parallelism.Uniform.default.withNum(2))))
}
