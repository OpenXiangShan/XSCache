// Intent (audit-fix-1): filter TSHR candidates before PriorityMux and omit
// CDP half queues/counters when the top disables CDP observation.
package oceanus.l2

// Intent (1c-2): buffer lossy CDP half-lines per slice without cache backpressure;
// preserve parallel training/eviction events and count all disabled training.

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.l2._
import oceanus.l2.L2Common._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.SeqBoolBitwiseOps
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.compactchi.CCHIOpcode._
import xscache.oceanus.compactchi.HasCCHIParameters


trait L2SliceLocatable {

  val sliceNum: Int
  val sliceIdx: Int
  val sliceNID: Int

  def getTSHRIdFromDnTxnID(txnId: UInt) = txnId >> log2Ceil(sliceNum)

  def getTSHRIdFromUpTxnID(txnId: UInt) = txnId

  def getSliceIdxFromTxnID(txnId: UInt) = if (sliceNum > 1) txnId(log2Ceil(sliceNum) - 1, 0) else 0.U

  def routeSliceFromTxnID(txnId: UInt) = getSliceIdxFromTxnID(txnId) === sliceIdx.U

  def getTxnIDFromTSHRId(tshrId: Int) = (tshrId.U << log2Ceil(sliceNum)) | sliceIdx.U(log2Ceil(sliceNum).W)
}

class L2TSHRCtrl(val sliceNum: Int, val sliceIdx: Int, val sliceNID: Int, val nodeId: Int, val pfTrainEnable: Boolean = false)(implicit val p: Parameters) extends Module
    with HasL2Params 
    with L2SliceLocatable
    with HasCCHIParameters {

  // Phase 1b: broadcast context beside REQ; only the allocator-selected RBE
  // accepts it, using exactly the existing alloc/reuse REQ pulse.
  val io = IO(new Bundle {
    val consts = Input(new L2SliceConsts(sliceNum))

    val UpRXEVT = Flipped(Decoupled(new FlitEVT))
    val DnRXSNP = Flipped(Decoupled(new CHIBundleSNP))
    val UpRXREQ = Flipped(Decoupled(new FlitREQ))
    val UpRXREQTrain = if (pfTrainEnable) Some(Input(new L2PrefetchTrainContext)) else None

    val UpTXSNP = Decoupled(new FlitSNP)

    val DnTXREQ = Decoupled(new CHIBundleREQ)

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))
    val UpRXDAT = Flipped(Valid(new FlitUpDAT))
    val UpTXRSP = Decoupled(new FlitDnRSP)
    val UpTXDAT = Decoupled(new FlitDnDAT)

    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))
    val DnTXRSP = Decoupled(new CHIBundleRSP)
    val DnTXDAT = Decoupled(new CHIBundleDAT)

    val toPCreditPool = Vec(paramL2.mshrSize, Valid(new L2PCreditPool.Entry))
    val fromPCreditPool = Input(Vec(paramL2.mshrSize, Bool()))

    val toClientTableREQ = Output(Vec(paramL2.mshrSize, UInt(paramCCHI.UpstreamNodeID_Width.W)))
    val fromClientTableREQ = Input(Vec(paramL2.mshrSize, Vec(1, Bool()))) // TODO: parameterize with coherent l2 client count

    val toClientTableEVT = Output(Vec(paramL2.mshrSize, UInt(paramCCHI.UpstreamNodeID_Width.W)))
    val fromClientTableEVT = Input(Vec(paramL2.mshrSize, Vec(1, Bool()))) // TODO: parameterize with coherent l2 client count

    val bopTrain = Valid(new L2PrefetchTrain(pfTrainEnable))
    // R10: raw pulses and physical source identities before local selection.
    val bopTrainRaw = if (pfTrainEnable) Some(Output(Vec(nMSHR, Bool()))) else None
    val trainSourceNID = if (pfTrainEnable) Some(Output(Vec(nMSHR, UInt(paramCCHI.UpstreamNodeID_Width.W)))) else None
    val cdpData = Decoupled(new L2CDPHalf)
    val cdpTrain = Output(Vec(paramL2.mshrSize, Valid(new L2CDPTrain)))
    val cdpEvict = Output(Vec(paramL2.mshrSize, Valid(new L2CDPEvict)))
    val cdpHalfDrops = Output(UInt(64.W))
    val cdpCollisionDrops = Output(UInt(64.W))
    val trainDisabled = Output(UInt(64.W))
  })

  //
  val EVB = Wire(Decoupled(new L2VPipeREQ.FlitEVB))

  // -- RX channel connections
  val tshrs = Seq.tabulate(paramL2.mshrSize)(i => Module(new L2TSHR(sliceNum, i, pfTrainEnable)))

  tshrs.zipWithIndex.foreach { case (t, i) =>
    t.io.consts.tshrId := i.U
    t.io.consts.sliceIdx := io.consts.sliceIdx
    t.io.consts.sliceNID := io.consts.sliceNID
    t.io.consts.nodeId := io.consts.nodeId
    t.io.consts.clientNID := io.consts.clientNID
    t.io.consts.pfSrcID := io.consts.pfSrcID
  }

  tshrs.foreach { case t => 
    t.io.UpRXEVT := io.UpRXEVT.bits
    t.io.DnRXSNP := io.DnRXSNP.bits
    t.io.UpRXEVB := EVB.bits
    t.io.UpRXREQ := io.UpRXREQ.bits
    t.io.UpRXREQTrain.foreach(_ := io.UpRXREQTrain.get)

    t.io.UpRXRSP.valid := io.UpRXRSP.valid && getTSHRIdFromUpTxnID(io.UpRXRSP.bits.TxnID) === t.tshrId.U 
    t.io.UpRXRSP.bits := io.UpRXRSP.bits

    t.io.UpRXDAT.valid := io.UpRXDAT.valid && getTSHRIdFromUpTxnID(io.UpRXDAT.bits.TxnID) === t.tshrId.U
    t.io.UpRXDAT.bits := io.UpRXDAT.bits

    t.io.DnRXRSP.valid := io.DnRXRSP.valid && getTSHRIdFromDnTxnID(io.DnRXRSP.bits.TxnID.get) === t.tshrId.U
    t.io.DnRXRSP.bits := io.DnRXRSP.bits

    t.io.DnRXDAT.valid := io.DnRXDAT.valid && getTSHRIdFromDnTxnID(io.DnRXDAT.bits.TxnID.get) === t.tshrId.U
    t.io.DnRXDAT.bits := io.DnRXDAT.bits
  }

  assert(!io.UpRXRSP.valid || (PopCount(tshrs.map(_.io.UpRXRSP.valid)) =/= 0.U), "UpRXRSP valid but no TSHR selected, flit lost")
  assert(!io.UpRXDAT.valid || (PopCount(tshrs.map(_.io.UpRXDAT.valid)) =/= 0.U), "UpRXDAT valid but no TSHR selected, flit lost")
  assert(!io.DnRXRSP.valid || (PopCount(tshrs.map(_.io.DnRXRSP.valid)) =/= 0.U), "DnRXRSP valid but no TSHR selected, flit lost")
  assert(!io.DnRXDAT.valid || (PopCount(tshrs.map(_.io.DnRXDAT.valid)) =/= 0.U), "DnRXDAT valid but no TSHR selected, flit lost")
  io.bopTrain := L2PrefetchTrainSelect.local(tshrs.map(_.io.bopTrain))
  val rawBopTrains = PopCount(tshrs.map(_.io.bopTrain.valid))
  val eligibleBopTrains = PopCount(tshrs.map(t => L2PrefetchTrainSelect.enabled(t.io.bopTrain)))
  XSPerfAccumulate("bop_train_raw", rawBopTrains)
  XSPerfAccumulate("bop_train_eligible", eligibleBopTrains)
  XSPerfAccumulate("bop_train_disabled", rawBopTrains - eligibleBopTrains)
  XSPerfAccumulate("bop_train_local_unselected", eligibleBopTrains - io.bopTrain.valid.asUInt)
  XSPerfAccumulate("bop_train_slice_selected", io.bopTrain.valid)

  if (pfTrainEnable) {
    tshrs.zipWithIndex.foreach { case (t,i) =>
      io.bopTrainRaw.get(i) := t.io.bopTrain.valid
      io.trainSourceNID.get(i) := t.io.bopTrain.bits.sourceId.get >> paramCCHI.TxnID_Width
    }
  }
  if (p(L2CDPObserveEnableKey)) {
    val cdpQueue = Module(new L2CDPHalfQueue)
    val cdpValids = tshrs.map(_.io.cdpData.valid)
    cdpQueue.io.in.valid := cdpValids.reduce(_ || _)
    cdpQueue.io.in.bits := PriorityMux(tshrs.map(t => t.io.cdpData.valid -> t.io.cdpData.bits))
    io.cdpData <> cdpQueue.io.out
    io.cdpHalfDrops := cdpQueue.io.dropped
    val cdpCollisionDrops = RegInit(0.U(64.W))
    val cdpLineCount = PopCount(cdpValids)
    when(cdpLineCount > 1.U) { cdpCollisionDrops := cdpCollisionDrops + ((cdpLineCount - 1.U) << 1) }
    io.cdpCollisionDrops := cdpCollisionDrops
    tshrs.zipWithIndex.foreach { case (t, i) =>
      io.cdpTrain(i) := t.io.cdpTrain
      io.cdpEvict(i) := t.io.cdpEvict
    }
    // Count all eligible demand training, including ordinary CDP hits, before
    // any lossy arbiter; simultaneous disabled events all count independently.
    val trainDisabled = RegInit(0.U(64.W))
    val disabledThisCycle = PopCount(tshrs.map(t => t.io.cdpTrain.valid &&
      !t.io.cdpTrain.bits.trainEnable))
    trainDisabled := trainDisabled + disabledThisCycle
    io.trainDisabled := trainDisabled
    XSPerfAccumulate("trainEnableZero", disabledThisCycle)
  } else {
    io.cdpData.valid := false.B
    io.cdpData.bits := 0.U.asTypeOf(io.cdpData.bits)
    io.cdpTrain := 0.U.asTypeOf(io.cdpTrain)
    io.cdpEvict := 0.U.asTypeOf(io.cdpEvict)
    io.cdpHalfDrops := 0.U
    io.cdpCollisionDrops := 0.U
    io.trainDisabled := 0.U
  }
  // ----------------------------------------------------------------

  // Per-slice BOP effectiveness. Sum EVERY TSHR event: OR would lose
  // simultaneous completions. Each metric has one counting boundary only;
  // REQ/TSHR/SNP provide pulses, and XSPerf accumulates only here. Nested
  // eviction/snoop ownership follows the first actual valid -> I local write.
  val bopPerf = Wire(Vec(L2BOPPerf.names.size, UInt(log2Ceil(paramL2.mshrSize + 1).W)))
  L2BOPPerf.names.zipWithIndex.foreach { case (name, i) =>
    bopPerf(i) := PopCount(tshrs.map(_.io.bopPerf(i)))
    XSPerfAccumulate(s"bop_$name", bopPerf(i))
  }

  // -- TSHR Allocation connections
  val tshrAlloc = Module(new L2TSHRAlloc(new L2TSHRAllocConfig(
    cluster = Seq(Seq(L2TSHRAllocTarget.EVT), Seq(L2TSHRAllocTarget.SNP, L2TSHRAllocTarget.EVB, L2TSHRAllocTarget.REQ)), // TODO: parameterize clustering
    resv = Seq(
      (paramL2.mshrSize - 1, L2TSHRResvTarget.L2EVB),
      (paramL2.mshrSize - 2, L2TSHRResvTarget.L1EVT),
      (paramL2.mshrSize - 3, L2TSHRResvTarget.L3SNP)) // TODO: parameterize reservation
  )))

  tshrAlloc.io.fromTSHRCtrl.RXEVT <> io.UpRXEVT
  tshrAlloc.io.fromTSHRCtrl.RXSNP <> io.DnRXSNP
  tshrAlloc.io.fromTSHRCtrl.RXEVB <> EVB
  tshrAlloc.io.fromTSHRCtrl.RXREQ <> io.UpRXREQ
  tshrAlloc.io.pfSrcID.foreach(_ := io.consts.pfSrcID)

  tshrs.foreach { case t => 
    tshrAlloc.io.fromTSHR(t.tshrId).bits := t.io.toAlloc
    tshrAlloc.io.fromTSHR(t.tshrId).valid := t.io.valid
    t.io.fromAlloc := tshrAlloc.io.toTSHR(t.tshrId)
  }
  // ----------------------------------------------------------------

  // -- TX channel connections
  // rotateOnBlock: a head EVB rejected by the allocator (nest target's rbeEVB
  // full, or Clause-A drain refusal mid-refill) must not seal this channel —
  // the refill TSHRs whose commits would unblock it may hold EVBs queued behind
  // that head (s5011 deadlock: T1/T2/T10 circular wait). Rotate past the blocked
  // head so deliverable EVBs drain; the blocked one re-participates next rotation.
  fastArb(tshrs.map(_.io.UpTXEVB), EVB, Some("TSHRsToUpTXEVB"), rotateOnBlock = true)

  // rotateOnBlock observability: every blocked (valid && !ready) cycle rotates the
  // RR base once; the histogram records the consecutive-rotation length of each
  // blocked episode, so a pathological spin shows up as a long tail.
  XSPerfAccumulate("TSHRsToUpTXEVB_rotateOnBlock", EVB.valid && !EVB.ready)
  val evbRotateLen = RegInit(0.U(32.W))
  when (EVB.fire) {
    evbRotateLen := 0.U
  }.elsewhen (EVB.valid && !EVB.ready) {
    evbRotateLen := evbRotateLen + 1.U
  }
  XSPerfHistogram("TSHRsToUpTXEVB_rotateOnBlock_len", evbRotateLen, EVB.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("TSHRsToUpTXEVB_rotateOnBlock_len", evbRotateLen, EVB.fire, 40, 800, 40, left_strict = true)

  fastArb(tshrs.map(_.io.UpTXSNP), io.UpTXSNP, Some("TSHRsToUpTXSNP"))

  fastArb(tshrs.map(_.io.UpTXRSP), io.UpTXRSP, Some("TSHRsToUpTXRSP"))
  fastArb(tshrs.map(_.io.UpTXDAT), io.UpTXDAT, Some("TSHRsToUpTXDAT"))

  fastArb(tshrs.map(_.io.DnTXREQ), io.DnTXREQ, Some("TSHRsToDnTXREQ"))

  fastArb(tshrs.map(_.io.DnTXRSP), io.DnTXRSP, Some("TSHRsToDnTXRSP"))
  fastArb(tshrs.map(_.io.DnTXDAT), io.DnTXDAT, Some("TSHRsToDnTXDAT"))
  // ----------------------------------------------------------------

  // -- Directory connections
  val directory = Module(new Directory)

  directory.io.toDir.zip(tshrs.map(_.io.toDir)).foreach { case (sink, source) => { 
    sink := source 
  }}

  directory.io.fromDir.zip(tshrs.map(_.io.fromDir)).foreach { case (source, sink) => {
    sink := source
  }}

  directory.io.tshrIdle := tshrs.map(!_.io.valid)

  // DRRIP release train, entrance path: one-hot reduce the per-TSHR WayValid
  // EVT entrance trains into the directory's single evtRelTrain input. The
  // allocator guarantees at most one EVT entrance per cycle per slice
  // (L2TSHRAlloc assertions), so the reduce stays one-hot. The whole reduce is
  // elaborated only when the entrance path exists (directory port present).
  directory.io.evtRelTrain.foreach { in =>
    val replTrains = tshrs.flatMap(_.io.replTrain)
    assert(PopCount(replTrains.map(_.valid)) <= 1.U,
      "multiple EVT entrance release trains in one cycle")
    in.valid := replTrains.map(_.valid).orR
    in.bits  := Mux1H(replTrains.map(t => (t.valid, t.bits)))
  }
  // ----------------------------------------------------------------

  // -- Data Storage connections
  val dataStorage = Module(new DataStorage)

  dataStorage.io.fromTSHR.zip(tshrs.map(_.io.toDS)).foreach { case (sink, source) => {
    sink := source
  }}

  dataStorage.io.toTSHR.zip(tshrs.map(_.io.fromDS)).foreach { case (source, sink) => {
    sink := source
  }}
  // ----------------------------------------------------------------

  // -- TSHR Inter-Unlocking connections
  tshrs.map(_.io.self_unlock_dir).zipWithIndex.foreach { case (sink, i) => {
    sink := tshrs.map(_.io.peer_unlock_dir(i)).orR
  }}

  tshrs.map(_.io.self_unlock_dir_tshrId).zipWithIndex.foreach { case (sink, i) => {
    sink := Mux1H(tshrs.map(_.io.peer_unlock_dir(i)), tshrs.map(_.tshrId.U))
  }}

  tshrs.map(_.io.self_unlock_ds).zipWithIndex.foreach { case (sink, i) => {
    sink := tshrs.map(_.io.peer_unlock_ds(i)).orR
  }}

  tshrs.map(_.io.self_unlock_dir_ack).zipWithIndex.foreach { case (sink, i) => {
    sink := tshrs.map(t => t.io.peer_unlock_ack && t.io.peer_unlock_ack_tshrId === i.U).orR
  }}
  // ----------------------------------------------------------------

  // -- P-Credit Pool connections
  io.toPCreditPool.zip(tshrs.map(_.io.toPCreditPool)).foreach { case (sink, source) => {
    sink := source
  }}

  io.fromPCreditPool.zip(tshrs.map(_.io.fromPCreditPool)).foreach { case (source, sink) => {
    sink := source
  }}
  // ----------------------------------------------------------------

  // -- Client Table connections
  io.toClientTableREQ.zip(tshrs.map(_.io.toClientTableREQ)).foreach { case (sink, source) => {
    sink := source
  }}

  io.fromClientTableREQ.zip(tshrs.map(_.io.fromClientTableREQ)).foreach { case (source, sink) => {
    sink := source
  }}

  io.toClientTableEVT.zip(tshrs.map(_.io.toClientTableEVT)).foreach { case (sink, source) => {
    sink := source
  }}

  io.fromClientTableEVT.zip(tshrs.map(_.io.fromClientTableEVT)).foreach { case (source, sink) => {
    sink := source
  }}
  // ----------------------------------------------------------------
}
