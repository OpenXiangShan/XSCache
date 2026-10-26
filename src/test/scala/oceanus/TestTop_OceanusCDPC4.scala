// C4: unmodified ReferenceCDP.SentUnit is the algorithm oracle. Comparison
// epochs admit a whole page behind a closed TLB gate, then drain before the
// next epoch. This aligns the accepted set / page fanout despite D1 latency.
// Arbitrary-delay, backpressure and late-arrival cases additionally use an
// independent host ownership/permission/conservation/order scoreboard.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.{cdpreference => ref}
import org.chipsalliance.cde.config._
import scala.collection.mutable
import scala.util.Random
import java.nio.file.{Files, Paths}
import utility._

// Ordered, ID-less deterministic model. Its response depends only on VA,
// independently counted attempts, and the selected latency. PMP follows by 1.
class CDPC4TlbModel extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(UInt(50.W)))
    val gate = Input(Bool()); val delay = Input(UInt(8.W))
    val resp = Valid(new Bundle {
      val va = UInt(50.W); val pa = UInt(48.W); val miss = Bool()
      val fault = UInt(2.W); val pmp = Bool(); val mmio = Bool(); val pbmt = UInt(2.W)
    })
    val attempt = Output(UInt(4.W))
  })
  val now = RegInit(0.U(32.W)); now := now + 1.U
  val attempts = RegInit(VecInit(Seq.fill(256)(0.U(4.W))))
  class Pending extends Bundle { val va = UInt(50.W); val due = UInt(32.W); val attempt = UInt(4.W) }
  val q = Module(new Queue(new Pending, 32))
  io.req.ready := io.gate && q.io.enq.ready
  q.io.enq.valid := io.req.valid && io.gate
  q.io.enq.bits.va := io.req.bits
  q.io.enq.bits.due := now + io.delay
  q.io.enq.bits.attempt := attempts(io.req.bits(19,12)) + 1.U
  when(io.req.fire) { attempts(io.req.bits(19,12)) := attempts(io.req.bits(19,12)) + 1.U }
  q.io.deq.ready := q.io.deq.bits.due <= now
  io.resp.valid := q.io.deq.valid && q.io.deq.ready
  val va = q.io.deq.bits.va; val kind = va(15,12)
  io.resp.bits.va := va
  io.resp.bits.pa := Cat(va(47,12) ^ "h123456".U(36.W), va(11,0))
  io.resp.bits.miss := kind === 2.U || (kind === 1.U && q.io.deq.bits.attempt === 1.U)
  io.resp.bits.fault := Mux(kind >= 3.U && kind <= 5.U, kind - 2.U, 0.U)
  io.resp.bits.pmp := kind === 6.U
  io.resp.bits.mmio := kind === 7.U
  io.resp.bits.pbmt := Mux(kind >= 8.U && kind <= 10.U, kind - 7.U, 0.U)
  io.attempt := q.io.deq.bits.attempt
}

class CDPC4Harness(val checkFixedCycle: Boolean = true)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val candidate = Input(UInt(58.W)) // [valid|VA50|depth2|source4|hit]
    val gate = Input(Bool()); val delay = Input(UInt(8.W))
    val ready = Input(Bool()); val queryReady = Input(Bool())
    val filter = Input(Bool()); val vbop = Input(Bool()); val vbopVA = Input(UInt(50.W))
    val fixed = Input(Bool())
    val packed = Output(UInt(621.W))
  })
  val prod = Module(new OceanusCDPSentUnit)
  val reference = Module(new ref.SentUnit)
  val adapter = Module(new L2PrefetchTlbAdapter)
  val actualTlb = Module(new CDPC4TlbModel)
  val referenceTlb = Module(new CDPC4TlbModel)
  prod.io.in.valid := io.candidate(57)
  prod.io.in.bits.pfVAddr := io.candidate(56,7)
  prod.io.in.bits.pfDepth := io.candidate(6,5)
  prod.io.in.bits.pfSource := io.candidate(4,1)
  prod.io.in.bits.isHit := io.candidate(0)
  reference.io.in.valid := prod.io.in.valid
  reference.io.in.bits := prod.io.in.bits.asTypeOf(reference.io.in.bits)
  prod.io.out.ready := io.ready; reference.io.out.ready := io.ready
  // Fixed bridge reproduces the original next-cycle TLB/next-cycle PMP
  // schedule for hit/permission cases, before testing D1 through the adapter.
  adapter.io.clients(1).req.valid := prod.io.tlb.req.valid && !io.fixed
  adapter.io.clients(1).req.bits := prod.io.tlb.req.bits
  prod.io.tlb.req.ready := Mux(io.fixed, actualTlb.io.req.ready, adapter.io.clients(1).req.ready)
  adapter.io.clients(1).resp.ready := prod.io.tlb.resp.ready
  prod.io.tlb.resp.valid := Mux(io.fixed, RegNext(actualTlb.io.resp.valid, false.B), adapter.io.clients(1).resp.valid)
  prod.io.tlb.resp.bits := adapter.io.clients(1).resp.bits
  when(io.fixed) {
    val paired = RegNext(actualTlb.io.resp.bits)
    prod.io.tlb.resp.bits := 0.U.asTypeOf(prod.io.tlb.resp.bits)
    prod.io.tlb.resp.bits.vaddr := paired.va
    prod.io.tlb.resp.bits.paddr := paired.pa
    prod.io.tlb.resp.bits.fault := paired.fault =/= 0.U || paired.pmp
    prod.io.tlb.resp.bits.mmio := paired.mmio
    prod.io.tlb.resp.bits.uncache := paired.pbmt === 1.U || paired.pbmt === 2.U
    prod.io.tlb.resp.bits.dropped := paired.fault =/= 0.U || paired.pmp || paired.mmio || paired.pbmt === 1.U || paired.pbmt === 2.U
    when(actualTlb.io.resp.valid) { assert(!actualTlb.io.resp.bits.miss) }
  }
  adapter.io.clients(0).req.valid := io.vbop
  adapter.io.clients(0).req.bits := io.vbopVA
  adapter.io.clients(0).resp.ready := true.B
  actualTlb.io.req.valid := Mux(io.fixed, prod.io.tlb.req.valid, adapter.io.tlb.req.valid)
  actualTlb.io.req.bits := Mux(io.fixed, prod.io.tlb.req.bits, adapter.io.tlb.req.bits.vaddr)
  adapter.io.tlb.req.ready := actualTlb.io.req.ready
  referenceTlb.io.req.valid := reference.io.tlbReq.req.valid
  referenceTlb.io.req.bits := reference.io.tlbReq.req.bits.vaddr
  reference.io.tlbReq.req.ready := referenceTlb.io.req.ready
  actualTlb.io.gate := io.gate; actualTlb.io.delay := Mux(io.fixed, 1.U, io.delay)
  referenceTlb.io.gate := io.gate; referenceTlb.io.delay := 1.U
  adapter.io.tlb.resp.valid := actualTlb.io.resp.valid && !io.fixed
  adapter.io.tlb.resp.bits := 0.U.asTypeOf(adapter.io.tlb.resp.bits)
  reference.io.tlbReq.resp.valid := referenceTlb.io.resp.valid
  reference.io.tlbReq.resp.bits := 0.U.asTypeOf(reference.io.tlbReq.resp.bits)
  val a = actualTlb.io.resp.bits; val r = referenceTlb.io.resp.bits
  adapter.io.tlb.resp.bits.paddr.head := a.pa
  adapter.io.tlb.resp.bits.miss := a.miss; adapter.io.tlb.resp.bits.pbmt := a.pbmt
  adapter.io.tlb.resp.bits.excp.head.pf.ld := a.fault === 1.U
  adapter.io.tlb.resp.bits.excp.head.gpf.ld := a.fault === 2.U
  adapter.io.tlb.resp.bits.excp.head.af.ld := a.fault === 3.U
  reference.io.tlbReq.resp.bits.paddr.head := r.pa
  reference.io.tlbReq.resp.bits.miss := r.miss; reference.io.tlbReq.resp.bits.pbmt := r.pbmt
  reference.io.tlbReq.resp.bits.excp.head.pf.ld := r.fault === 1.U
  reference.io.tlbReq.resp.bits.excp.head.gpf.ld := r.fault === 2.U
  reference.io.tlbReq.resp.bits.excp.head.af.ld := r.fault === 3.U
  adapter.io.tlb.pmp_resp := 0.U.asTypeOf(adapter.io.tlb.pmp_resp)
  reference.io.tlbReq.pmp_resp := 0.U.asTypeOf(reference.io.tlbReq.pmp_resp)
  adapter.io.tlb.pmp_resp.ld := RegNext(actualTlb.io.resp.valid && a.pmp, false.B)
  adapter.io.tlb.pmp_resp.mmio := RegNext(actualTlb.io.resp.valid && a.mmio, false.B)
  reference.io.tlbReq.pmp_resp.ld := RegNext(referenceTlb.io.resp.valid && r.pmp, false.B)
  reference.io.tlbReq.pmp_resp.mmio := RegNext(referenceTlb.io.resp.valid && r.mmio, false.B)
  // Independent static physical Filter image: tag PA[47:24], set PA[23:18],
  // slot PA[17:12]. Every fifth slot is saturated; the other slots score 2.
  val filterTag = (BigInt("123456",16) >> 12).U(24.W)
  prod.io.ftQueryReq.ready := io.queryReady
  reference.io.ftQueryReq.ready := io.queryReady
  prod.io.ftQueryRsp := 0.U.asTypeOf(prod.io.ftQueryRsp)
  reference.io.ftQueryRsp := 0.U.asTypeOf(reference.io.ftQueryRsp)
  val filterValid = RegEnable(io.filter, prod.io.ftQueryReq.fire)
  val refFilterValid = RegEnable(io.filter, reference.io.ftQueryReq.fire)
  prod.io.ftQueryRsp.validVec(0) := filterValid
  reference.io.ftQueryRsp.validVec(0) := refFilterValid
  prod.io.ftQueryRsp.tagVec(0) := filterTag
  reference.io.ftQueryRsp.tagVec(0) := filterTag
  for (i <- 0 until 64) {
    prod.io.ftQueryRsp.satVec(0)(i) := (if (i % 5 == 0) 3 else 2).U
    reference.io.ftQueryRsp.satVec(0)(i) := (if (i % 5 == 0) 3 else 2).U
  }
  def read[T <: Data](x: T): T = BoringUtils.bore(x)
  val rv = read(reference.valids); val re = read(reference.entries)
  val missAddress = read(reference.tlb_s2_addr); val missValid = read(reference.tlb_s2_valid)
  val pmpAddress = read(reference.tlb_s3_addr); val pmpValid = read(reference.tlb_s3_valid)
  val finalValid = read(reference.tlb_s3_rsp_valid); val finalBits = read(reference.tlb_s3_rsp_bits)
  val refDrops = VecInit((0 until 16).map(i => rv(i) && (
    (missValid && reference.io.tlbReq.resp.valid && r.miss && re(i).retryEn && re(i).vTag(43,6) === missAddress(49,12)) ||
    (pmpValid && finalValid && !finalBits.miss && read(reference.s3_drop) && re(i).vTag(43,6) === pmpAddress(49,12)))))
  when(io.fixed && checkFixedCycle.B && !reset.asBool) {
    assert(read(prod.valids).asUInt === rv.asUInt, "fixed valid state")
    val pe = read(prod.entries)
    for (i <- 0 until 16) {
      when(rv(i)) {
        assert(pe(i).vTag === re(i).vTag && pe(i).pfDepth === re(i).pfDepth)
        assert(pe(i).pfSource === re(i).pfSource && pe(i).isHit === re(i).isHit)
        assert(pe(i).retryEn === re(i).retryEn && pe(i).retryTimer === re(i).retryTimer)
        assert(pe(i).paddrValid === re(i).paddrValid)
        when(re(i).paddrValid) { assert(pe(i).pTag === re(i).pTag) }
      }
    }
    assert(prod.io.tlb.req.valid === reference.io.tlbReq.req.valid)
    when(prod.io.tlb.req.valid) { assert(prod.io.tlb.req.bits === reference.io.tlbReq.req.bits.vaddr) }
    assert(read(prod.reqInflight).asUInt === read(reference.reqInflight).asUInt)
    when(prod.io.ftQueryReq.valid) {
      assert(read(prod.pftArb.io.chosen) === read(reference.pftArb.io.chosen))
    }
    assert(prod.io.ftQueryReq.valid === reference.io.ftQueryReq.valid)
    when(prod.io.ftQueryReq.valid) { assert(prod.io.ftQueryReq.bits.setIdx === reference.io.ftQueryReq.bits.setIdx) }
    assert(prod.io.out.valid === reference.io.out.valid)
    when(prod.io.out.valid) {
      assert(prod.io.out.bits.paddr === reference.io.out.bits.addr)
      assert(prod.io.out.bits.pfDepth === reference.io.out.bits.cdpPfDepth.get)
    }
  }
  // Source/depth/sequence use ONLY the production transport helper.
  val seq = RegInit(0.U(2.W)); when(prod.io.out.fire) { seq := seq + 1.U }
  val txn = L2PrefetchTxnID.encode(prod.io.out.bits.pfSource, prod.io.out.bits.pfDepth, seq, 8)
  when(prod.io.out.valid) {
    assert(L2PrefetchTxnID.decode(txn) === 9.U && L2PrefetchTxnID.depth(txn) === prod.io.out.bits.pfDepth)
    assert(prod.io.out.bits.reqSource === 16.U && !prod.io.out.bits.needT)
  }
  val fields = Seq(
    io.candidate(57), read(prod.allocate), read(prod.duplicate), read(prod.fullDrop), read(prod.freeEntryIdx),
    read(prod.translationDrop).asUInt, read(prod.filterDrop), read(prod.idx2),
    prod.io.out.valid, prod.io.out.bits.paddr, prod.io.out.bits.pfDepth, read(prod.output.io.deq.bits.index), txn,
    prod.io.ftQueryReq.fire, read(prod.pftArb.io.chosen), read(prod.pftArb.io.out.bits.paddr), read(prod.pftArb.io.out.bits.pfDepth),
    read(reference.entryHit), read(reference.hasFreeEntry), read(reference.freeEntryIdx), refDrops.asUInt,
    read(reference.pft_s2_valid) && !read(reference.canPft), read(reference.pft_s2_chosen_idx),
    reference.io.out.valid, reference.io.out.bits.addr, reference.io.out.bits.cdpPfDepth.get,
    read(reference.pft_s2_chosen_idx),
    prod.io.tlb.req.fire, prod.io.tlb.req.bits,
    adapter.io.clients(0).req.fire, adapter.io.clients(0).resp.fire,
    actualTlb.io.req.fire, actualTlb.io.req.bits,
    actualTlb.io.resp.valid, a.va, a.miss, actualTlb.io.attempt,
    prod.io.tlb.resp.fire, prod.io.tlb.resp.bits.vaddr, prod.io.tlb.resp.bits.dropped,
    read(prod.valids).asUInt, read(reference.valids).asUInt)
  val total = fields.map(_.getWidth).sum
  require(total <= 621, s"packed width $total")
  io.packed := Cat(fields.reverse).pad(621)
}

object TestTop_OceanusCDPC4 extends App with ChiselSim {
  Constantin.init(false)
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  val seeds = args.find(_.startsWith("--seeds=")).map(_.drop(8).toInt).getOrElse(3)
  val traceDir = Paths.get(sys.props.getOrElse("oceanus.c4.trace", "/data/zhuy/pf-framework/c4/traces"))
  Files.createDirectories(traceDir)
  val coverage = mutable.Map.empty[String,Long].withDefaultValue(0L)
  var totalCycles = 0L; var comparisons = 0L; var scenes = 0
  simulate(new CDPC4OrderingHarness()(params)) { dut =>
    val ordering=new CDPOrderingTracker(dut,traceDir)
    case class Item(va: BigInt, depth: Int, id: Int)
    val live = Array.fill(2)(mutable.Map.empty[Int,Item])
    val results = Array.fill(2)(mutable.ArrayBuffer.empty[String])
    val discarded = Array.fill(2)(mutable.ArrayBuffer.empty[String])
    val issueOrder = mutable.Queue.empty[(BigInt,Int,Int)]
    val pageAttempts = mutable.Map.empty[BigInt,Int].withDefaultValue(0)
    val misses = mutable.Map.empty[BigInt,Int]
    val activePages = mutable.Set.empty[BigInt]
    var tickNo = 0; var inputID = 0
    var held: Option[(BigInt,Int)] = None
    var currentTrace: java.io.BufferedWriter = null
    var vbAccepted = 0; var vbCompleted = 0
    var fixedBridge = false
    var exactRetryScene = false
    def count(s: String, n: Int=1): Unit = coverage(s) += n
    def pa(va: BigInt): BigInt = ((va >> 12) ^ BigInt("123456",16)) << 12 | (va & 4032)
    def kind(va: BigInt): Int = ((va >> 12) & 15).toInt
    def deny(va: BigInt): Boolean = (2 to 9).contains(kind(va))
    def blocked(va: BigInt, filter: Boolean): Boolean = filter && (((pa(va) >> 12) & 63).toInt % 5 == 0)
    def tick(candidate: Option[(BigInt,Int)]=None, gate: Boolean=true, delay: Int=1,
      ready: Boolean=true, query: Boolean=true, filter: Boolean=false,
      vbop: Boolean=false, vbVA: BigInt=BigInt(0xf0000)): Unit = {
      val encoded = candidate.map { case (va,d) => (BigInt(1)<<57)|(va<<7)|(BigInt(d)<<5)|(BigInt(9)<<1) }.getOrElse(BigInt(0))
      dut.io.fixed.poke(fixedBridge)
      dut.io.candidate.poke(encoded); dut.io.gate.poke(gate); dut.io.delay.poke(delay)
      dut.io.ready.poke(ready); dut.io.queryReady.poke(query); dut.io.filter.poke(filter)
      dut.io.vbop.poke(vbop); dut.io.vbopVA.poke(vbVA)
      ordering.sample(encoded,ready,filter,query)
      val packed = dut.io.packed.peek().litValue
      var bits = packed
      def get(n: Int): BigInt = {val v=bits & ((BigInt(1)<<n)-1); bits >>= n; v}
      def flag(): Boolean = get(1)!=0
      val inv=flag();val alloc=flag();val dup=flag();val full=flag();val ai=get(4).toInt
      val td=get(16);val fd=flag();val fi=get(4).toInt
      val ov=flag();val opa=get(48);val od=get(2).toInt;val oi=get(4).toInt;val txn=get(8).toInt
      val qf=flag();val qi=get(4).toInt;val qpa=get(48);val qd=get(2).toInt
      val rh=flag();val rf=flag();val ri=get(4).toInt;val rtd=get(16)
      val rfd=flag();val rfi=get(4).toInt;val rov=flag();val rpa=get(48);val rd=get(2).toInt;val roi=get(4).toInt
      val cf=flag();val cva=get(50);val vf=flag();val vr=flag()
      val tf=flag();val tva=get(50);val rsp=flag();val rsva=get(50);val miss=flag();val attempt=get(4).toInt
      val term=flag();val termva=get(50);val drop=flag();val mask=get(16);val rmask=get(16)
      assert(bits==0)
      assert(mask==live(0).keys.foldLeft(BigInt(0))((m,i)=>m.setBit(i)), s"live entry mismatch cycle=$tickNo")
      assert(rmask==live(1).keys.foldLeft(BigInt(0))((m,i)=>m.setBit(i)))
      val item = candidate.map {case (va,d)=>Item(va & ~BigInt(63),d,inputID)}
      val acceptFlags = Array(alloc, inv && !rh && rf)
      val allocIdx = Array(ai,ri)
      for (side <- 0 to 1) {
        item.foreach { it =>
          val expectedDup = live(side).values.exists(_.va==it.va)
          val expectedFull = !expectedDup && live(side).size==16
          assert((if(side==0)dup else rh)==expectedDup)
          assert(acceptFlags(side)==(!expectedDup && !expectedFull))
          if(side==0){if(expectedDup)count("duplicate");if(expectedFull)count("full")}
        }
        val drops=if(side==0)td else rtd
        for (i <- 0 until 16 if drops.testBit(i)) {
          val it=live(side).remove(i).get
          assert(deny(it.va),s"unlicensed translation drop $it")
          discarded(side) += s"${it.id}:tlb:${it.va}:${it.depth}"
          if(side==0) count("drop-kind-"+kind(it.va))
        }
        if(if(side==0)fd else rfd) {
          val it=live(side).remove(if(side==0)fi else rfi).get
          assert(!deny(it.va) && blocked(it.va,filter),s"unexpected physical filter drop $it")
          discarded(side) += s"${it.id}:filter:${it.va}:${it.depth}"
          if(side==0)count("filter-drop")
        }
      }
      if(qf) {
        val it=live(0)(qi)
        assert(qpa==pa(it.va) && qd==it.depth && !deny(it.va))
        if(!blocked(it.va,filter)) issueOrder.enqueue((qpa,qd,qi))
        count("physical-query")
      }
      held.foreach {case(p,d)=>assert(ov && opa==p && od==d,"output changed under backpressure")}
      held=if(ov && !ready)Some((opa,od))else None
      if(ov && ready) {
        val it=live(0).remove(oi).get
        assert(!deny(it.va) && !blocked(it.va,filter) && opa==pa(it.va) && od==it.depth)
        assert(issueOrder.dequeue()==((opa,od,oi)),"query/completion FIFO order")
        assert((txn>>4)==9 && ((txn>>2)&3)==od)
        results(0) += s"${it.id}:$opa:$od";count("output");count("depth-"+od)
      }
      if(rov && ready) {
        val it=live(1).remove(roi).get
        assert(!deny(it.va) && rpa==pa(it.va) && rd==it.depth)
        results(1) += s"${it.id}:$rpa:$rd"
      }
      for(side<-0 to 1 if acceptFlags(side))live(side)(allocIdx(side))=item.get
      if(item.nonEmpty)inputID+=1
      if(cf) {
        assert(activePages.add(cva>>12),"duplicate outstanding page transaction")
        count("page-request")
      }
      if(vf){vbAccepted+=1;count("vbop-accept")}
      if(vr){vbCompleted+=1;count("vbop-result")}
      if(tf) {
        val page=tva>>12;pageAttempts(page)+=1
        assert(pageAttempts(page)<=2,s"more than one retry page=$page")
        if(pageAttempts(page)==2) {
          val delta=tickNo-misses(page)
          assert(delta>=11,s"retry before timer expiry delta=$delta")
          count("retry");if(delta==11)count("retry-exact-11")
        }
        count("external-request")
      }
      if(exactRetryScene)misses.get(BigInt(0x21)).foreach { at =>
        val elapsed=tickNo-at
        if(elapsed==9 || elapsed==10) {
          assert(pageAttempts(BigInt(0x21))==1,"retry fired at timer 9/10 boundary")
          count("retry-boundary-"+elapsed)
        }
        if(elapsed==11) {
          assert(tf && (tva>>12)==BigInt(0x21) && pageAttempts(BigInt(0x21))==2)
          count("retry-boundary-11")
        }
      }
      if(rsp && miss && attempt==1)misses(rsva>>12)=tickNo
      if(term){assert(activePages.remove(termva>>12));assert(drop==deny(termva));count("terminal")}
      if(currentTrace!=null)currentTrace.write(s"$tickNo $encoded $gate $delay $ready $query $filter $vbop $packed\n")
      dut.clock.step();tickNo+=1;totalCycles+=1
    }
    def reset(label: String): Unit = {
      currentTrace=Files.newBufferedWriter(traceDir.resolve(label+".trace"))
      currentTrace.write("HEAD="+sys.props.getOrElse("oceanus.c4.head","unknown")+"\n")
      dut.io.fixed.poke(fixedBridge)
      dut.io.candidate.poke(0);dut.io.gate.poke(false);dut.io.delay.poke(1)
      dut.io.ready.poke(false);dut.io.queryReady.poke(false);dut.io.filter.poke(false)
      dut.io.vbop.poke(false);dut.io.vbopVA.poke(0)
      dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false)
      live.foreach(_.clear());results.foreach(_.clear());discarded.foreach(_.clear())
      issueOrder.clear();pageAttempts.clear();misses.clear();activePages.clear()
      held=None;tickNo=0;inputID=0;vbAccepted=0;vbCompleted=0
      ordering.reset(label)
    }
    def finish(compare: Boolean, compareSet: Boolean=false): Unit = {
      assert(live.forall(_.isEmpty) && activePages.isEmpty && issueOrder.isEmpty)
      assert(vbAccepted==vbCompleted)
      if(compare) {
        assert(results(0)==results(1),s"request event order mismatch DUT=${results(0)} REF=${results(1)}")
        assert(discarded(0).sorted==discarded(1).sorted,s"drop-event mismatch DUT=${discarded(0)} REF=${discarded(1)}")
        comparisons += 1
      }
      if(compareSet) {
        assert(results(0).sorted==results(1).sorted,"D1 complete request set mismatch")
        assert(discarded(0).sorted==discarded(1).sorted,"D1 complete discard set mismatch")
        count("d1-set-epochs")
        if(results(0)!=results(1))count("d1-order-difference")
      }
      ordering.finish(results.map(_.toVector).toVector,compareSet,requireSourceOrder=args.contains("--require-source-order"))
      currentTrace.close();currentTrace=null;scenes+=1
    }
    // Entire input set is present before either page translation can start.
    // Fixed reference latency=1; DUT 1..7, independently selected per epoch.
    def epoch(label: String, seed: Int, k: Int, size: Int, filt: Boolean): Unit = {
      reset(label)
      val random=new Random(seed)
      val page=BigInt(0x20+k)<<12
      val requests=Vector.tabulate(size)(i=>(page+((i%18)*64)+random.nextInt(8)*4,random.nextInt(4)))
      val delay=1+random.nextInt(7)
      for(i<-0 until 32)tick(if(i<requests.size)Some(requests(i))else None,gate=false,delay=delay,filter=filt)
      for(i<-0 until 96)tick(delay=delay,query=i%7!=0,filter=filt)
      finish(compare=true);count("kind-"+k);count("delay-"+delay)
    }
    fixedBridge=true
    for(k<-Seq(0,3,4,5,6,7,8,9,10))epoch(s"fixed-cycle-$k",17+k,k,20,filt=true)
    fixedBridge=false;count("fixed-cycle-epochs",9)
    for(k<-0 to 10)epoch(s"direct-kind-$k",100+k,k,20,filt=false)
    epoch("physical-filter",42,0,20,filt=true)
    for(seed<-0 until seeds;k<-Seq(0,1,2,3,6,7,8,10))
      epoch(s"seed-$seed-kind-$k",seed*7919+k,k,1+(seed*13+k*7)%24,filt=seed%2==0)
    // Multiple outstanding pages, full/drop and both query/output backpressure.
    // Freeze admission before translation on both sides: compare the COMPLETE
    // outcome sets. Timing-dependent raw output permutations are counted, while
    // every DUT output must retain its independently checked Filter-query order.
    for(seed<-0 until seeds) {
      reset(s"stress-$seed")
      val random=new Random(0x1234+seed)
      val filt=seed%2==0
      for(i<-0 until 256) {
        val in=if(i<24)Some(((BigInt(0x30+random.nextInt(16))<<12)+random.nextInt(8)*64,random.nextInt(4)))else None
        tick(in,gate=i>=32,delay=1+random.nextInt(7),ready=i>=64 && random.nextBoolean(),query=random.nextInt(3)!=0,filter=filt)
      }
      finish(compare=false,compareSet=true)
    }
    // Late same-page arrivals during the adapter's ten-cycle retry reservation.
    exactRetryScene=true
    reset("late-retry-and-backpressure")
    tick(Some((BigInt(0x21000),1)))
    for(i<-0 until 90)tick(if(i==8)Some((BigInt(0x21040),3))else None,delay=1,ready=i>=45,query=i%4!=0)
    assert(pageAttempts(BigInt(0x21))==2 && results(0).size==2)
    assert(coverage("retry-exact-11")>0)
    finish(compare=false,compareSet=true);exactRetryScene=false;count("late-during-retry")
    // Occupy every adapter slot with distinct pages while output is blocked.
    reset("capacity-vbop-and-arbitrary-delay")
    var vbPending=true
    for(i<-0 until 64) {
      val offer=vbPending
      val before=vbAccepted
      tick(if(i<24)Some((BigInt(0x30000)+(BigInt(i)<<12),i%4))else None,
        gate=i>=28,delay=7,ready=false,query=i%3!=0,vbop=offer)
      if(vbAccepted>before)vbPending=false
    }
    assert(!vbPending && vbAccepted==1)
    for(i<-0 until 192)tick(delay=1+i%7,ready=i%5!=0,query=i%4!=0)
    finish(compare=false,compareSet=true);count("capacity-and-vbop")
    // Same-cycle VBOP/CDP admission: hold external issue while CDP's s1 fills.
    reset("simultaneous-vbop-priority")
    tick(Some((BigInt(0x20000),3)),gate=false);tick(gate=false)
    tick(gate=false,vbop=true)
    assert(vbAccepted==1 && activePages.isEmpty,"VBOP did not win simultaneous admission")
    for(i<-0 until 96)tick(delay=1+i%7)
    finish(compare=true);count("simultaneous-priority")
    val required=Seq("duplicate","full","filter-drop","retry","retry-exact-11","late-during-retry","capacity-and-vbop","simultaneous-priority","retry-boundary-9","retry-boundary-10","retry-boundary-11","d1-set-epochs")++
      (0 to 10).map("kind-"+_)++(0 to 3).map("depth-"+_)++(1 to 7).map("delay-"+_)++(2 to 9).map("drop-kind-"+_)
    required.foreach(k=>assert(coverage(k)>0,s"missing coverage $k"))
    coverage.toSeq.sortBy(_._1).foreach{case(k,v)=>println(s"CDP_C4_COVER $k=$v")}
    println(s"CDP_C4_ORDER_RESULT independentScheduleChecked=true illegalPermutationRejected=true fullMultisetsRetained=true contractApproval=D5-pending scenes=$scenes")
    println(s"CDP_C4_RESULT pass=true seeds=$seeds cycles=$totalCycles scenes=$scenes reference_epochs=$comparisons request_and_drop_mismatches=0")
  }
}
