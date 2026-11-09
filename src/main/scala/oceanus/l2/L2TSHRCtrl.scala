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

class L2TSHRCtrl(val sliceNum: Int, val sliceIdx: Int, val sliceNID: Int, val nodeId: Int, val pfTrainEnable: Boolean = false, val hintTargets: Seq[Int] = Nil, val hintMissEntries: Int = 16, val hintObserveTargets: Seq[Int] = Nil)(implicit val p: Parameters) extends Module
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
    val refillHint = Decoupled(new L2RefillHint(paramCCHI.TxnID_Width, paramCCHI.UpstreamNodeID_Width))
    val hintQuiescent = Output(Bool())
    val hintQueueFull = Input(Bool())
    val hintOtherSlicesBusy = Input(Bool())
    val hintChannelCooldown = Input(Bool())
    val hintArbLost = Input(Bool())

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
    val bopTrainRaw = if (pfTrainEnable) Some(Output(Vec(paramL2.mshrSize, Bool()))) else None
    val trainSourceNID = if (pfTrainEnable) Some(Output(Vec(paramL2.mshrSize, UInt(paramCCHI.UpstreamNodeID_Width.W)))) else None
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

  // Non-preempting reservation: the ORIGINAL arbiter above remains the only
  // DAT selector. A hint is admitted only with no other live owner locally
  // and, at the top, no live owner in any other slice. A fresh allocation
  // cannot traverse directory/RBE/DS and become DAT within the four-cycle
  // reservation horizon. Uncertain or competing transactions simply skip hint.
  io.hintQuiescent := !tshrs.map(_.io.valid).reduce(_ || _)
  io.refillHint.valid := false.B
  io.refillHint.bits := 0.U.asTypeOf(io.refillHint.bits)
  tshrs.foreach(_.io.hintLock := false.B)
  // Per-entry observation state is independent of functional scheduling.
  val hintAdmissionFailures = Wire(Vec(paramL2.mshrSize,
    Vec(L2HintCoverage.admissionReasons.size, Bool())))
  hintAdmissionFailures.foreach(_ := VecInit(Seq.fill(L2HintCoverage.admissionReasons.size)(false.B)))
  val hintAccepted = Wire(Vec(paramL2.mshrSize, Bool()))
  hintAccepted.foreach(_ := false.B)
  val hintCovEvents = Wire(Vec(paramL2.mshrSize, UInt(4.W)))
  val hintCovReasons = Wire(Vec(paramL2.mshrSize, UInt(L2HintCoverage.reasons.size.W)))
  if (hintTargets.nonEmpty) {
    val width = math.max(1, log2Ceil(paramL2.mshrSize))
    val pointers = Module(new L2HintPointerQueue(width))
    val reserved = RegInit(VecInit(Seq.fill(paramL2.mshrSize)(false.B)))
    val candidates = VecInit(tshrs.map(t => t.io.hitHintSafe &&
      hintTargets.map(n => t.io.UpTXDAT.bits.TgtID === n.U).reduce(_ || _) &&
      t.io.UpTXDAT.bits.TxnID < hintMissEntries.U))
    val soleOwner = PopCount(VecInit(tshrs.map(_.io.valid))) === 1.U
    val chosen = OHToUInt(candidates)
    val dat = VecInit(tshrs.map(_.io.UpTXDAT.bits))(chosen)
    val eligible = candidates.asUInt.orR && soleOwner && !reserved.asUInt.orR
    io.refillHint.valid := eligible && pointers.io.enq.ready
    io.refillHint.bits.sourceId := dat.TxnID
    io.refillHint.bits.targetId := dat.TgtID
    io.refillHint.bits.isKeyword := false.B
    pointers.io.enq.valid := io.refillHint.fire
    pointers.io.enq.bits := chosen
    when(io.refillHint.fire) {
      assert(PopCount(candidates) === 1.U)
      assert(pointers.io.enq.fire, "local reservation must be atomic with external hint")
      reserved(chosen) := true.B
    }
    tshrs.zipWithIndex.foreach { case (t, i) => t.io.hintLock := reserved(i) }
    tshrs.zipWithIndex.foreach { case (t, i) =>
      hintAccepted(i) := io.refillHint.fire && chosen === i.U
      val safe = t.io.hitHintSafe
      val sourceOK = t.io.UpTXDAT.bits.TxnID < hintMissEntries.U
      val localEligible = safe && sourceOK && soleOwner && !reserved.asUInt.orR
      val offered = localEligible && pointers.io.enq.ready
      val topEligible = offered && !io.hintOtherSlicesBusy
      val topReady = topEligible && !io.hintQueueFull && !io.hintChannelCooldown
      val checks = Seq(
        safe && !sourceOK,
        safe && sourceOK && !soleOwner,
        safe && sourceOK && reserved.asUInt.orR,
        localEligible && pointers.io.full,
        localEligible && !pointers.io.full && !pointers.io.enq.ready,
        offered && io.hintOtherSlicesBusy,
        topEligible && io.hintQueueFull,
        topEligible && !io.hintQueueFull && io.hintChannelCooldown,
        topReady && io.hintArbLost,
        topReady && !io.hintArbLost && !io.refillHint.fire,
        false.B, false.B, false.B)
      hintAdmissionFailures(i) := VecInit(checks)
    }
    val head = pointers.io.out.bits
    val fires = VecInit(tshrs.map(_.io.UpTXDAT.fire))
    pointers.io.out.ready := io.UpTXDAT.fire && fires(head)
    when(pointers.io.out.valid) {
      assert(reserved(head) && pointers.io.out.fire, "non-preempting local reservation missed")
      assert(io.UpTXDAT.bits.DataID === pointers.io.second.asUInt, "hint beat phase")
    }
    when(pointers.io.out.fire && pointers.io.second) { reserved(head) := false.B }
    val otherBlocked = tshrs.zipWithIndex.map { case (t, i) =>
      !reserved(i) && t.io.UpTXDAT.valid && !t.io.UpTXDAT.ready
    }.reduce(_ || _)
    XSPerfAccumulate("hint_hit_sent", io.refillHint.fire)
    XSPerfAccumulate("hint_skip_fifo_full", candidates.asUInt.orR &&
      (pointers.io.full || io.hintQueueFull))
    XSPerfAccumulate("hint_skip_unsafe", candidates.asUInt.orR && !io.refillHint.fire &&
      !pointers.io.full && !io.hintQueueFull)
    // Conservative upper bound: even ordinary blocked competition is counted.
    // DAT valid/ready has no reservation mux or mask, so hint cannot steal it.
    XSPerfAccumulate("hint_reservation_delayed_other", pointers.io.out.valid && otherBlocked)
    assert(!(pointers.io.out.valid && otherBlocked), "unexpected competing DAT inside quiet reservation")
  }

  tshrs.zipWithIndex.foreach { case (t, i) =>
    val o = t.io.hitHintObserve
    val live = RegInit(false.B)
    val hit = RegInit(false.B)
    val firstSent = RegInit(false.B)
    val decided = RegInit(false.B)
    val hinted = RegInit(false.B)
    val failures = RegInit(0.U(L2HintCoverage.reasons.size.W))
    val decision = Cat(hintAdmissionFailures(i).asUInt, o.failures.asUInt)
    when(!t.io.valid) { live := false.B }
    when(o.request) {
      live := true.B
      hit := o.hit
      firstSent := false.B
      decided := false.B
      hinted := false.B
      failures := 0.U
    }
    when(live && !firstSent && o.dsResponse && !decided) {
      decided := true.B
      failures := decision
    }
    when(hintAccepted(i)) { hinted := true.B }
    val toL1D = hintObserveTargets.map(n => t.io.UpTXDAT.bits.TgtID === n.U)
      .foldLeft(false.B)(_ || _)
    val first = live && !firstSent && t.io.UpTXDAT.fire && toL1D
    when(first) { firstSent := true.B }
    val hitSent = first && hit
    val skipped = hitSent && !hinted
    val disabled = (hintTargets.isEmpty).B
    val noDS = !decided
    val fallback = Mux(disabled,
      (BigInt(1) << L2HintCoverage.reasons.indexOf("hint_disabled")).U,
      Mux(noDS, (BigInt(1) << L2HintCoverage.reasons.indexOf("no_ds_response")).U,
        Mux(failures.orR, failures,
          (BigInt(1) << L2HintCoverage.reasons.indexOf("no_recorded_reason")).U)))
    hintCovReasons(i) := Mux(skipped, fallback, 0.U)
    hintCovEvents(i) := Cat(skipped, hitSent && hinted, first && !hit, hitSent)
    when(first && !hit) { assert(!hinted, "directory miss must not issue a hit hint") }
  }
  // L1D transactions are counted exactly once at their first actual DAT fire.
  // Consequently warmup/window boundaries use the same event for both ratio terms.
  val hintCovCounts = Wire(Vec(4, UInt(log2Ceil(paramL2.mshrSize + 1).W)))
  Seq("hit_txn", "miss_txn", "hit_hinted", "hit_skipped").zipWithIndex.foreach { case (name, bit) =>
    hintCovCounts(bit) := PopCount(VecInit(hintCovEvents.map(_(bit))))
    XSPerfAccumulate(s"hint_cov_$name", hintCovCounts(bit))
  }
  val hintCovPrimary = VecInit(hintCovReasons.map(x => PriorityEncoderOH(x)))
  L2HintCoverage.reasons.zipWithIndex.foreach { case (name, bit) =>
    XSPerfAccumulate(s"hint_cov_skip_$name", PopCount(VecInit(hintCovPrimary.map(_(bit)))))
    XSPerfAccumulate(s"hint_cov_condition_$name", PopCount(VecInit(hintCovReasons.map(_(bit)))))
  }


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
