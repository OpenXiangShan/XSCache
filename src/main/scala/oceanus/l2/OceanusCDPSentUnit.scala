package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.TwoLevelRRArbiter

// C4 source ledger: CDP.scala:1044-1111 admission and virtual-line identity;
// :1113-1218 adapted under D1 to the existing transaction TLB client. The
// adapter alone owns miss/retry/PMP. A page stays reserved through its terminal
// response, including retry; late entries share that transaction. This extends
// reference's fixed s1/s2/s3 sharing window and its late-arrival retry behavior.
// :1220-1317 retains two-level RR and the physical Filter pipeline. As in
// CDP.scala:1293-1307, each completed query releases reqInflight. An allowed
// request under output backpressure stays live and re-enters arbitration to
// query the current Filter; only a Filter rejection or external fire retires it.
class OceanusCDPSentUnit(implicit p: Parameters) extends OceanusCDPModule {
  require(retryInterval == 10, "C4 uses the existing adapter's ten-cycle retry")
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new OceanusCDPCandidate))
    val out = Decoupled(new OceanusCDPRequest)
    val tlb = Flipped(new L2PrefetchTranslationClientIO)
    val ftQueryReq = Decoupled(new OceanusCDPFtQueryReq)
    val ftQueryRsp = Input(new OceanusCDPFtQueryRsp)
  })
  def blockAddr(a: UInt): UInt = a(a.getWidth - 1, 6)
  def samePage(a: UInt, b: UInt): Bool = a(49, 12) === b(49, 12)
  val valids = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(false.B)))
  val entries = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(0.U.asTypeOf(new OceanusCDPEntry))))
  val reqInflight = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(false.B)))
  val translating = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(false.B)))
  val entryHit = VecInit(entries.zip(valids).map { case (e, v) => v && e.vTag === blockAddr(io.in.bits.pfVAddr) }).asUInt.orR
  val free = VecInit(valids.map(!_))
  val hasFreeEntry = free.asUInt.orR
  val freeEntryIdx = PriorityEncoder(free)
  io.in.ready := true.B
  // Named events are independently checked by the module-level scoreboard.
  val duplicate = io.in.valid && entryHit
  val fullDrop = io.in.valid && !entryHit && !hasFreeEntry
  val allocate = io.in.valid && !entryHit && hasFreeEntry

  val tlbArb = Module(new TwoLevelRRArbiter(UInt(50.W), reqFilterEntryNum))
  val tlbHeld = RegInit(false.B)
  val tlbAddress = Reg(UInt(50.W))
  io.tlb.req.valid := tlbHeld
  io.tlb.req.bits := tlbAddress
  io.tlb.resp.ready := true.B
  val tlbReady = !tlbHeld || io.tlb.req.ready
  tlbArb.io.out.ready := tlbReady
  when(tlbReady) {
    tlbHeld := tlbArb.io.out.valid
    when(tlbArb.io.out.valid) { tlbAddress := tlbArb.io.out.bits }
  }
  for (i <- 0 until reqFilterEntryNum) {
    val va = Cat(entries(i).vTag, 0.U(6.W))
    val pagePending = VecInit((0 until reqFilterEntryNum).map(j =>
      translating(j) && samePage(va, Cat(entries(j).vTag, 0.U(6.W))))).asUInt.orR
    tlbArb.io.in(i).valid := valids(i) && !entries(i).paddrValid && !pagePending &&
      !(tlbHeld && samePage(va, tlbAddress))
    tlbArb.io.in(i).bits := va
    when(io.tlb.req.fire && valids(i) && !entries(i).paddrValid && samePage(va, tlbAddress)) {
      translating(i) := true.B
    }
  }
  val terminal = io.tlb.resp.fire
  val result = io.tlb.resp.bits
  val complete = Wire(Vec(reqFilterEntryNum, Bool()))
  val translationDrop = Wire(Vec(reqFilterEntryNum, Bool()))
  for (i <- 0 until reqFilterEntryNum) {
    val va = Cat(entries(i).vTag, 0.U(6.W))
    complete(i) := terminal && valids(i) && !entries(i).paddrValid && samePage(va, result.vaddr)
    translationDrop(i) := complete(i) && result.dropped
    when(complete(i)) {
      translating(i) := false.B
      when(result.dropped) { valids(i) := false.B }
        .otherwise {
          entries(i).paddrValid := true.B
          entries(i).pTag := Cat(result.paddr(47, 12), va(11, 6))
        }
    }
  }
  when(terminal) {
    assert(VecInit((0 until reqFilterEntryNum).map(i => translating(i) && complete(i))).asUInt.orR,
      "CDP terminal translation has no page owner")
  }
  when(allocate) {
    val e = WireInit(0.U.asTypeOf(new OceanusCDPEntry))
    e.vTag := blockAddr(io.in.bits.pfVAddr)
    e.pfDepth := io.in.bits.pfDepth
    e.pfSource := io.in.bits.pfSource
    e.isHit := io.in.bits.isHit
    entries(freeEntryIdx) := e
    valids(freeEntryIdx) := true.B
    val busy = VecInit((0 until reqFilterEntryNum).map(i => translating(i) &&
      samePage(io.in.bits.pfVAddr, Cat(entries(i).vTag, 0.U(6.W))))).asUInt.orR ||
      (io.tlb.req.fire && samePage(io.in.bits.pfVAddr, tlbAddress))
    // Allocation at the terminal edge was not in the pre-edge waiting set.
    translating(freeEntryIdx) := busy && !(terminal && samePage(io.in.bits.pfVAddr, result.vaddr))
  }

  val pftArb = Module(new TwoLevelRRArbiter(new OceanusCDPRequest, reqFilterEntryNum))
  for (i <- 0 until reqFilterEntryNum) {
    pftArb.io.in(i).valid := valids(i) && entries(i).paddrValid && !reqInflight(i)
    pftArb.io.in(i).bits := entries(i).toRequest()
    when(pftArb.io.in(i).fire) { reqInflight(i) := true.B }
  }
  pftArb.io.out.ready := io.ftQueryReq.ready
  io.ftQueryReq.valid := pftArb.io.out.valid
  io.ftQueryReq.bits.setIdx := getFilterSet(pftArb.io.out.bits.paddr)
  val pftS1 = RegNext(io.ftQueryReq.fire, false.B)
  val pftS2 = RegNext(pftS1, false.B)
  val idx1 = RegEnable(pftArb.io.chosen, io.ftQueryReq.fire)
  val idx2 = RegEnable(idx1, pftS1)
  val req1 = RegEnable(pftArb.io.out.bits, io.ftQueryReq.fire)
  val req2 = RegEnable(req1, pftS1)
  val rsp2 = RegEnable(io.ftQueryRsp, pftS1)
  val hitVec = rsp2.validVec.zip(rsp2.tagVec).map { case (v, t) => v && t === getFilterTag(req2.paddr) }
  val canPft = if (useFilterTable) !VecInit(hitVec).asUInt.orR ||
    rsp2.satVec(PriorityEncoder(hitVec))(getFilterOffset(req2.paddr)) =/= 3.U else true.B
  val filterDrop = pftS2 && !canPft
  io.out.valid := pftS2 && canPft
  io.out.bits := req2
  when(pftS2) {
    reqInflight(idx2) := false.B
    when(!canPft || io.out.fire) { valids(idx2) := false.B }
  }
  for (i <- 0 until reqFilterEntryNum) {
    when(translating(i)) { assert(valids(i) && !entries(i).paddrValid) }
    when(reqInflight(i)) { assert(valids(i) && entries(i).paddrValid) }
  }
}
