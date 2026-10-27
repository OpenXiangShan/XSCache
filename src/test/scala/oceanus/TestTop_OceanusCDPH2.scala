package oceanus

// H2 admission-window counterexample, isolated from cache/VPN training. The
// original C5 overlap workload remains a separate real-cache entry point.
// Both senders see one identical, immutable all-deny physical Filter image;
// this isolates translation residency from output queues and Filter training.
import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import scala.collection.mutable
import java.nio.file.{Files, Paths}

class CDPH2Harness(implicit p: Parameters) extends CDPC4Harness() {
  val dt = RegEnable(prod.getFilterTag(read(prod.pftArb.io.out.bits.paddr)), prod.io.ftQueryReq.fire)
  val rt = RegEnable(reference.getFilterTag(read(reference.pftArb.io.out.bits).addr), reference.io.ftQueryReq.fire)
  prod.io.ftQueryRsp.validVec(0) := true.B
  reference.io.ftQueryRsp.validVec(0) := true.B
  prod.io.ftQueryRsp.tagVec(0) := dt
  reference.io.ftQueryRsp.tagVec(0) := rt
  for(i <- 0 until 64) {
    prod.io.ftQueryRsp.satVec(0)(i) := 3.U
    reference.io.ftQueryRsp.satVec(0)(i) := 3.U
  }
  val debugFields = mutable.ArrayBuffer.empty[(String,UInt)]
  def add(n: String, d: Data): Unit = debugFields += n -> read(d).asUInt
  def mask(n: String, ds: Seq[Bool]): Unit = add(n, VecInit(ds).asUInt)
  add("d.valid",read(prod.valids).asUInt); add("r.valid",read(reference.valids).asUInt)
  mask("d.pa",read(prod.entries).map(_.paddrValid)); mask("r.pa",read(reference.entries).map(_.paddrValid))
  add("d.inflight",read(prod.reqInflight).asUInt); add("r.inflight",read(reference.reqInflight).asUInt)
  add("d.allocate",prod.allocate); add("d.duplicate",prod.duplicate); add("d.index",prod.freeEntryIdx)
  add("r.allocate",reference.in.valid && !read(reference.entryHit) && read(reference.hasFreeEntry))
  add("r.duplicate",reference.in.valid && read(reference.entryHit)); add("r.index",reference.freeEntryIdx)
  add("d.tlb",prod.io.tlb.req.fire); add("d.tlbVA",prod.io.tlb.req.bits)
  add("r.tlb",reference.io.tlbReq.req.fire); add("r.tlbVA",reference.io.tlbReq.req.bits.vaddr)
  add("d.terminal",prod.io.tlb.resp.fire); add("d.terminalVA",prod.io.tlb.resp.bits.vaddr)
  add("r.terminal",read(reference.tlb_s3_valid) && read(reference.tlb_s3_rsp_valid) && !read(reference.tlb_s3_rsp_bits).miss)
  add("r.terminalVA",reference.tlb_s3_addr)
  add("d.query",prod.io.ftQueryReq.fire); add("d.queryIndex",prod.pftArb.io.chosen)
  add("r.query",reference.io.ftQueryReq.fire); add("r.queryIndex",reference.pftArb.io.chosen)
  add("d.drop",prod.filterDrop); add("d.dropIndex",prod.idx2)
  add("r.drop",read(reference.pft_s2_valid) && !read(reference.canPft)); add("r.dropIndex",reference.pft_s2_chosen_idx)
  add("d.out",prod.io.out.fire); add("r.out",reference.io.out.fire)
  debugFields += "d.queue" -> 0.U(5.W) // Removed queue: retain legacy trace width without boring a literal.
  val debug = IO(Output(UInt(debugFields.map(_._2.getWidth).sum.W)))
  debug := Cat(debugFields.map(_._2).reverse.toSeq)
  val debugWidths = debugFields.map {case(n,d) => n -> d.getWidth}.toVector
}
object TestTop_OceanusCDPH2 extends App with ChiselSim {
  Constantin.init(false)
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  val dest=Paths.get(sys.props("oceanus.h2.trace")); Files.createDirectories(dest)
  // The final 24 candidates in original epoch1001, cycles247449..247472.
  val addresses=Vector(0x31d604,0x31d984,0x31db44,0x31de84,0x31d644,0x31d9c4,
    0x31db84,0x31dec4,0x31d684,0x31da04,0x31dbc4,0x31df04,0x31db84,0x31d444,
    0x31dc04,0x31d744,0x31dbc4,0x31d484,0x31dc44,0x31d784,0x31dc04,0x31d4c4,0x31dc84,0x31d7c4)
  simulate(new CDPH2Harness()(params)) { dut =>
    def decode():Map[String,BigInt] = {
      var v=dut.debug.peek().litValue
      dut.debugWidths.map {case(n,w) => val x=v&((BigInt(1)<<w)-1);v >>= w;n->x}.toMap
    }
    def scene(keep:Set[Int], delay:Int, save:Boolean=false, fixed:Boolean=false):Boolean = {
      dut.io.fixed.poke(fixed);dut.io.candidate.poke(0);dut.io.gate.poke(true);dut.io.delay.poke(delay)
      dut.io.ready.poke(true);dut.io.queryReady.poke(true);dut.io.filter.poke(false)
      dut.io.vbop.poke(false);dut.io.vbopVA.poke(0)
      dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false)
      var mismatch=false;val rows=mutable.ArrayBuffer.empty[ujson.Value]
      val live=Array.fill(2)(mutable.Map.empty[Int,Int]);val retired=Array.fill(2)(mutable.Set.empty[Int])
      for(c <- 0 until 100) {
        val valid=keep(c);val va=if(c<addresses.size)addresses(c)else 0
        val in=if(valid)(BigInt(1)<<57)|(BigInt(va)<<7)|(BigInt(3)<<5)|(BigInt(9)<<1) else BigInt(0)
        dut.io.candidate.poke(in)
        val r=decode()
        if(c==16 && r("d.allocate")==1 && r("r.duplicate")==1)mismatch=true
        for((side,name)<-Seq(0->"d",1->"r")) {
          assert(r(name+".valid")==live(side).keys.map(i=>BigInt(1)<<i).foldLeft(BigInt(0))(_|_))
          if(r(name+".drop")!=0) {
            val id=live(side).remove(r(name+".dropIndex").toInt).get
            assert(retired(side).add(id),"entry retired twice")
          }
          if(r(name+".allocate")!=0)live(side)(r(name+".index").toInt)=c
          assert(r(name+".out")==0,"all-deny Filter emitted")
        }
        if(save) rows+=ujson.Obj("cycle"->c,"originalCycle"->(247449+c),"candidate"->valid,
          "va"->va,"signals"->ujson.Obj.from(r.map {case(k,v)=>k->ujson.Str(v.toString(16))}))
        dut.clock.step()
      }
      assert(live.forall(_.isEmpty))
      if(save) Files.writeString(dest.resolve("h2-minimal.json"),ujson.Obj("head"->sys.props("oceanus.h2.head"),
        "delay"->delay,"kept"->ujson.Arr.from(keep.toVector.sorted),"mismatch"->mismatch,
        "rows"->ujson.Arr.from(rows)).render())
      mismatch
    }
    val full=addresses.indices.toSet
    val delays=(1 to 7).filter(d=>scene(full,d))
    require(delays.nonEmpty,"isolated counterexample did not reproduce")
    val delay=delays.head
    var keep=full;var changed=true
    while(changed) {
      changed=false
      for(i<-keep.toVector.sorted if i!=10 && i!=16) {
        val next=keep-i
        if(scene(next,delay)){keep=next;changed=true}
      }
    }
    assert(scene(keep,delay,save=true))
    assert(!scene(keep,1,fixed=true), "same translation timing retained the counterexample")
    for(i<-keep if i!=10 && i!=16)assert(!scene(keep-i,delay),s"not 1-minimal: $i")
    println(s"CDP_H2_MINIMAL pass=true discrepancyPreserved=true delay=$delay candidates=${keep.size} kept=${keep.toVector.sorted} outputs=0 filterFrozen=true outputReady=true oneMinimal=true fixedTranslationAligned=true")
  }
}
