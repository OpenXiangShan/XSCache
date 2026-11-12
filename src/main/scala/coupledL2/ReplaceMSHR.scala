/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * *************************************************************************************
 */

package xscache.coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.MemReqSource
import xscache.chi.{CHIChannel, CHIREQ, HasCHIOpcodes}
import xscache.chi.CHICohStates._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import xscache.coupledL2.MetaData._

class ReplaceMSHRIO(implicit p: Parameters) extends CoupledL2Bundle {
  val id = Input(UInt(mshrBits.W))
  val alloc = Flipped(ValidIO(new ReplaceRequest))
  val sinkC = Flipped(ValidIO(new RespInfoBundle))
  val releaseDataSeen = Input(Bool())
  val rxrsp = Flipped(ValidIO(new RespInfoBundle))
  val tasks = new MSHRTasks
  val txreqPending = Input(Bool())
  val victimDataCaptured = Input(Bool())
  val victimDataNeedsWriteback = Input(Bool())
  val nestedReleaseWritePending = Input(Bool())
  val acceptsLateReleaseData = Output(Bool())
  val snoopStarted = Input(Bool())
  val localReservationHold = Input(Bool())
  val snoopAdmissionIdle = Output(Bool())
  val sinkCOwnerHold = Input(Bool())
  val sourceBHold = Input(Bool())
  val sourceBAccepted = Input(Bool())
  val snoopDoneCount = Input(UInt(log2Ceil(mshrsAll + 3).W))
  val snoopMeta = Flipped(ValidIO(new MetaEntry))
  val snoopToInvalDone = Input(Bool())
  val copyBackDone = Input(Bool())
  val parentRelease = Input(Bool())
  val info = ValidIO(new ReplaceMSHRInfo)
  val txnId = Output(UInt(mshrBits.W))
  val done = ValidIO(new ReplaceDone)
}

class ReplaceMSHR(implicit p: Parameters) extends CoupledL2Module with HasCHIOpcodes {
  val io = IO(new ReplaceMSHRIO)

  private val replaceTxnBase = mshrsAll.U(mshrBits.W)
  io.txnId := io.id + replaceTxnBase

  val valid = RegInit(false.B)
  val probeSent = RegInit(false.B)
  val probeAck = RegInit(false.B)
  val releaseSent = RegInit(false.B)
  val releaseAck = RegInit(false.B)
  val copyBackSent = RegInit(false.B)
  val copyBackComplete = RegInit(false.B)
  val compReceived = RegInit(false.B)
  val releaseNeedsData = RegInit(false.B)
  val postSnoopToInval = RegInit(false.B)
  // Count each accepted snoop and each terminal response; simultaneous start/done deltas must all apply.
  val snoopInFlightWidth = log2Up(2 * mshrsAll + 8)
  val snoopInFlightMax = ((BigInt(1) << snoopInFlightWidth) - 1).U
  val snoopInFlightCnt = RegInit(0.U(snoopInFlightWidth.W))
  val snoopInFlight = snoopInFlightCnt =/= 0.U
  val snoopConsumed = RegInit(false.B)
  val captureIssued = RegInit(false.B)
  val releaseBufCaptured = RegInit(false.B)
  val probeAckDataRecv = RegInit(false.B)
  val releaseDataPending = RegInit(false.B)
  val donePulse = RegInit(false.B)
  val victim = RegInit(0.U.asTypeOf(new ReplaceMSHRInfo))
  val mode = RegInit(ReplaceMSHRMode.victim)
  val needProbe = RegInit(false.B)
  val probeParam = RegInit(toN)
  val captureFromDS = RegInit(false.B)
  val releaseTgtId = RegInit(0.U(NODEID_WIDTH.W))
  val releaseDbId = RegInit(0.U(DBID_WIDTH.W))
  val compAckPending = RegInit(false.B)

  val isCmoClean = mode === ReplaceMSHRMode.cmoClean
  val isCmoFlush = mode === ReplaceMSHRMode.cmoFlush
  val isCmoInval = mode === ReplaceMSHRMode.cmoInval
  val isVictim = mode === ReplaceMSHRMode.victim
  // CBO.inval discards data even for a dirty line; its release is control-only.
  val needsData = isCmoClean ||
    (!isCmoInval && (victim.meta.dirty || probeAckDataRecv))
  val victimWriteBackFull = isVictim && isT(victim.meta.state) &&
    (victim.meta.dirty || probeAckDataRecv)
  val victimWriteEvictOrEvict = isVictim && afterIssueEbOrElse(!victimWriteBackFull, false.B)
  val releaseBufRequired = captureFromDS || needsData
  val copyBackCancelled = compReceived || compAckPending ||
    io.rxrsp.valid && io.rxrsp.bits.chiOpcode.get === Comp && victimWriteEvictOrEvict
  // Once a lower release requests data, later snoops cannot drop that transport obligation.
  val activeReleaseNeedsData = Mux(releaseSent, releaseNeedsData, needsData)
  val needsCopyBack = Mux(isCmoInval, false.B, activeReleaseNeedsData) && !copyBackCancelled
  io.acceptsLateReleaseData := valid && releaseSent && needsCopyBack && !copyBackSent
  val releaseOpcode = Mux(isCmoClean, WriteCleanFull,
    Mux(isCmoInval, Evict,
      Mux(victimWriteBackFull, WriteBackFull,
        Mux(victimWriteEvictOrEvict, WriteEvictOrEvict,
          Mux(needsData, WriteBackFull, Evict)))))

  when (io.alloc.valid) {
    assert(!valid, "replace context allocated while still valid")
    assert(!io.txreqPending, "replace context allocated before its TXREQ skid drained")
    valid := true.B
    probeSent := false.B
    probeAck := false.B
    releaseSent := false.B
    releaseAck := false.B
    copyBackSent := false.B
    copyBackComplete := false.B
    releaseNeedsData := false.B
    postSnoopToInval := false.B
    snoopConsumed := false.B
    captureIssued := false.B
    releaseBufCaptured := false.B
    probeAckDataRecv := false.B
    releaseDataPending := false.B
    donePulse := false.B
    compAckPending := false.B
    compReceived := false.B
    mode := io.alloc.bits.mode
    needProbe := io.alloc.bits.needProbe
    probeParam := io.alloc.bits.probeParam
    captureFromDS := io.alloc.bits.captureFromDS
    victim.valid := true.B
    victim.parentId := io.alloc.bits.parentId
    victim.parentEpoch := io.alloc.bits.parentEpoch
    victim.parentAttached := io.alloc.bits.parentAttached
    victim.detached := !io.alloc.bits.parentAttached || !io.alloc.bits.needProbe
    victim.releaseBufReady := false.B
    victim.refillReady := false.B
    victim.set := io.alloc.bits.dirResult.set
    victim.tag := io.alloc.bits.dirResult.tag
    victim.way := io.alloc.bits.dirResult.way
    victim.meta := io.alloc.bits.dirResult.meta
    victim.probeDone := !io.alloc.bits.needProbe
    victim.releaseStarted := false.B
    victim.copyBackIssued := false.B
    victim.snoopInvalidated := false.B
    victim.releaseAck := false.B
    victim.releaseToClean := io.alloc.bits.mode === ReplaceMSHRMode.cmoClean
  }

  when (!io.alloc.valid) {
    donePulse := false.B
  }
  when (io.parentRelease && valid && !io.alloc.valid) {
    victim.parentAttached := false.B
  }

  val probeTask = WireInit(0.U.asTypeOf(new SourceBReq))
  probeTask.tag := victim.tag
  probeTask.set := victim.set
  probeTask.off := 0.U
  probeTask.opcode := Probe
  probeTask.param := probeParam
  probeTask.alias.foreach(_ := victim.meta.alias.getOrElse(0.U))
  io.tasks.source_b.valid := valid && needProbe && !probeSent
  io.tasks.source_b.bits := probeTask
  when (valid && needProbe && mode === ReplaceMSHRMode.victim) {
    assert(probeParam === toN,
      "victim ReplaceMSHR must invalidate upper clients before eviction")
  }

  val captureTask = WireInit(0.U.asTypeOf(new TaskBundle))
  captureTask.channel := "b001".U
  captureTask.txChannel := 0.U
  captureTask.tag := victim.tag
  captureTask.set := victim.set
  captureTask.off := 0.U
  captureTask.size := log2Ceil(blockBytes).U
  captureTask.mshrTask := true.B
  captureTask.mshrId := io.id
  captureTask.mshrContext := 2.U
  captureTask.replaceCapture := true.B
  captureTask.way := victim.way
  captureTask.meta := MetaEntry()
  captureTask.wayMask := 0.U
  captureTask.reqSource := MemReqSource.NoWhere.id.U

  val releaseTask = WireInit(0.U.asTypeOf(new TaskBundle))
  releaseTask.channel := "b001".U
  releaseTask.txChannel := CHIChannel.TXREQ
  releaseTask.tag := victim.tag
  releaseTask.set := victim.set
  releaseTask.off := 0.U
  releaseTask.opcode := Mux(needsData, ReleaseData, Release)
  releaseTask.param := 0.U
  releaseTask.size := log2Ceil(blockBytes).U
  releaseTask.sourceId := 0.U
  releaseTask.bufIdx := 0.U
  releaseTask.needProbeAckData := false.B
  releaseTask.denied := false.B
  releaseTask.corrupt := false.B
  releaseTask.mshrTask := true.B
  releaseTask.mshrId := io.id
  releaseTask.mshrContext := 2.U
  releaseTask.replaceTask := true.B
  releaseTask.replaceCapture := false.B
  releaseTask.useProbeData := isCmoClean
  releaseTask.readProbeDataDown := false.B
  releaseTask.mshrRetry := false.B
  releaseTask.way := victim.way
  releaseTask.metaWen := isCmoClean || isCmoFlush || isCmoInval
  releaseTask.tagWen := false.B
  releaseTask.dsWen := isCmoClean && probeAckDataRecv
  releaseTask.meta := Mux(isCmoClean, victim.meta, MetaEntry())
  releaseTask.meta.dirty := false.B
  releaseTask.meta.state := Mux(isCmoClean,
    Mux(victim.meta.state === TRUNK, TIP, victim.meta.state), INVALID)
  releaseTask.replTask := false.B
  releaseTask.cmoTask := isCmoClean || isCmoFlush || isCmoInval
  releaseTask.cmoAll := false.B
  releaseTask.wayMask := 0.U
  releaseTask.reqSource := MemReqSource.NoWhere.id.U
  releaseTask.mergeA := false.B
  releaseTask.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
  releaseTask.snpHitRelease := false.B
  releaseTask.snpHitReleaseToInval := false.B
  releaseTask.snpHitReleaseToClean := false.B
  releaseTask.snpHitReleaseWithData := false.B
  releaseTask.snpHitReleaseIdx := 0.U
  releaseTask.snpHitReleaseMeta := 0.U.asTypeOf(new MetaEntry)
  releaseTask.tgtID.get := 0.U
  releaseTask.srcID.get := 0.U
  releaseTask.txnID.get := io.txnId
  releaseTask.homeNID.get := 0.U
  releaseTask.dbID.get := 0.U
  releaseTask.fwdNID.get := 0.U
  releaseTask.fwdTxnID.get := 0.U
  releaseTask.chiOpcode.get := releaseOpcode
  releaseTask.resp.get := 0.U
  releaseTask.fwdState.get := 0.U
  releaseTask.pCrdType.get := 0.U
  releaseTask.retToSrc.get := false.B
  releaseTask.likelyshared.get := victimWriteEvictOrEvict && victim.meta.state === BRANCH
  releaseTask.expCompAck.get := victimWriteEvictOrEvict
  releaseTask.allowRetry.get := false.B
  releaseTask.memAttr.get.cacheable := true.B
  releaseTask.memAttr.get.allocate := victimWriteBackFull || victimWriteEvictOrEvict
  releaseTask.memAttr.get.device := false.B
  releaseTask.memAttr.get.ewa := true.B
  releaseTask.traceTag.get := false.B

  val copyBackTask = WireInit(releaseTask)
  copyBackTask.txChannel := CHIChannel.TXDAT
  copyBackTask.chiOpcode.get := CopyBackWrData
  copyBackTask.txnID.get := releaseDbId
  copyBackTask.tgtID.get := releaseTgtId
  copyBackTask.readProbeDataDown := true.B
  copyBackTask.useProbeData := true.B
  copyBackTask.metaWen := false.B
  copyBackTask.dsWen := false.B
  val victimWritebackDirty = victim.meta.dirty || probeAckDataRecv
  val victimWritebackState = Mux(victim.meta.state === BRANCH,
    Mux(victimWritebackDirty, SD, SC),
    Mux(victimWritebackDirty, UD, UC))
  copyBackTask.resp.get := Mux(postSnoopToInval, I, setPD(victimWritebackState, victimWritebackDirty))

  val probeDone = !needProbe || probeAck
  val captureTaskValid = valid && captureFromDS && probeDone && !probeAckDataRecv && !captureIssued &&
    !io.nestedReleaseWritePending
  // An earlier nested snoop wins until release acceptance; accepted lower transport remains owned.
  val releaseTaskValid = valid && probeDone && !releaseSent && !snoopConsumed &&
    !snoopInFlight && !io.snoopStarted && !releaseDataPending && !io.releaseDataSeen &&
    (!releaseBufRequired || releaseBufCaptured) && !io.nestedReleaseWritePending
  val copyBackTaskValid = valid && releaseAck && needsCopyBack && !copyBackSent &&
    !snoopInFlight && !io.snoopStarted && !io.txreqPending &&
    !releaseDataPending && !io.releaseDataSeen && !io.nestedReleaseWritePending && releaseBufCaptured
  io.tasks.txreq.valid := releaseTaskValid && mode === ReplaceMSHRMode.victim
  io.tasks.txreq.bits := releaseTask.toCHIREQBundle()
  io.tasks.txdat.valid := copyBackTaskValid && mode === ReplaceMSHRMode.victim
  io.tasks.txdat.bits := copyBackTask
  val mainpipeIntent = Wire(Decoupled(new TaskBundle))
  val mainpipeIssuedVariant = Wire(UInt(4.W))
  val mainpipeProposedVariant = Reg(UInt(4.W))
  mainpipeIntent.valid := captureTaskValid ||
    (copyBackTaskValid && mode =/= ReplaceMSHRMode.victim) ||
    (releaseTaskValid && mode =/= ReplaceMSHRMode.victim)
  mainpipeIntent.bits := Mux(mainpipeProposedVariant === 0.U, captureTask,
    Mux(mainpipeProposedVariant === 1.U, copyBackTask, releaseTask))
  val mainpipeInvalidate = io.alloc.valid || io.sinkC.valid || io.rxrsp.valid ||
    io.releaseDataSeen || io.victimDataCaptured || io.victimDataNeedsWriteback ||
    io.nestedReleaseWritePending ||
    io.snoopStarted || io.snoopDoneCount.orR || io.snoopMeta.valid || io.snoopToInvalDone ||
    io.copyBackDone || io.parentRelease || io.sourceBAccepted ||
    io.tasks.txreq.fire || io.tasks.txdat.fire || io.tasks.txrsp.fire
  mainpipeProposedVariant := MainpipeLeaseIO.freeze(io.tasks.preparedLease,
    mainpipeIntent, io.tasks.mainpipe,
    Mux(captureTaskValid, 0.U(4.W), Mux(copyBackTaskValid, 1.U(4.W), 2.U(4.W))),
    Mux(captureTaskValid, 3.U(3.W), 4.U(3.W)), mainpipeInvalidate)
  mainpipeIssuedVariant := mainpipeProposedVariant
  val compAckRsp = io.tasks.txrsp.bits
  compAckRsp := 0.U.asTypeOf(io.tasks.txrsp.bits.cloneType)
  compAckRsp.tgtID := releaseTgtId
  compAckRsp.srcID := 0.U
  compAckRsp.txnID := releaseDbId
  compAckRsp.dbID := 0.U
  compAckRsp.opcode := CompAck
  compAckRsp.resp := 0.U
  compAckRsp.fwdState := 0.U
  compAckRsp.traceTag := false.B
  io.tasks.txrsp.valid := valid && compAckPending

  when (io.sourceBAccepted) {
    probeSent := true.B
  }
  when (io.tasks.txreq.fire) {
    assert(!releaseSent, "a ReplaceMSHR may capture only one TXREQ flit per episode")
    releaseSent := true.B
    releaseNeedsData := needsData || victimWriteEvictOrEvict
  }
  when (io.tasks.txrsp.fire) {
    compAckPending := false.B
  }
  when (io.tasks.txdat.fire) {
    copyBackSent := true.B
  }
  when (io.tasks.mainpipe.fire) {
    when (mainpipeIssuedVariant === 0.U) {
      captureIssued := true.B
    }.elsewhen (mainpipeIssuedVariant === 2.U) {
      releaseSent := true.B
      releaseNeedsData := needsData || victimWriteEvictOrEvict
    }.elsewhen (mainpipeIssuedVariant === 1.U) {
      copyBackSent := true.B
    }
  }
  when (io.victimDataCaptured) {
    releaseBufCaptured := true.B
    releaseDataPending := false.B
  }
  when (io.victimDataNeedsWriteback) {
    probeAckDataRecv := true.B
    victim.meta.dirty := true.B
  }
  when (io.copyBackDone) { copyBackComplete := true.B }
  when (io.snoopStarted) {
    assert(valid && !copyBackSent,
      "a nested snoop must reserve ReplaceMSHR before CopyBackWrData is issued")
  }
  assert(!io.txreqPending || valid, "TXREQ skid holds a flit without a live ReplaceMSHR")
  val snoopAvailable = snoopInFlightCnt +& io.snoopStarted.asUInt
  when(io.snoopDoneCount.orR) {
    assert(valid && io.snoopDoneCount <= snoopAvailable,
      "nested snoop completions must each own a reserved ReplaceMSHR service")
  }
  val snoopAfterEvents = snoopAvailable - io.snoopDoneCount
  assert(snoopAfterEvents <= snoopInFlightMax, "nested snoop reservation tally overflow")
  val snoopInFlightNext = Mux(io.alloc.valid, 0.U, snoopAfterEvents)
  snoopInFlightCnt := snoopInFlightNext
  when (io.snoopToInvalDone) {
    when (!releaseSent) {
      assert(valid, "an invalidating nested snoop arrived without ReplaceMSHR")
      snoopConsumed := true.B
    }.otherwise {
      postSnoopToInval := true.B
      victim.snoopInvalidated := true.B
    }
  }
  when (io.sinkC.valid) {
    when (io.sinkC.bits.opcode === ProbeAck || io.sinkC.bits.opcode === ProbeAckData) {
      probeAck := probeAck || io.sinkC.bits.last
      victim.probeDone := victim.probeDone || io.sinkC.bits.last
      victim.detached := victim.detached || io.sinkC.bits.last
    }
    when (io.sinkC.bits.opcode === ProbeAckData) {
      probeAckDataRecv := true.B
      victim.meta.dirty := true.B
    }
  }
  when (valid && io.snoopMeta.valid) {
    victim.meta := io.snoopMeta.bits
  }
  when (io.releaseDataSeen && !io.victimDataCaptured) {
    releaseDataPending := true.B
  }
  when (io.rxrsp.valid) {
    when (io.rxrsp.bits.chiOpcode.get === CompDBIDResp || io.rxrsp.bits.chiOpcode.get === Comp) {
      releaseAck := true.B
      victim.releaseAck := true.B
      releaseTgtId := io.rxrsp.bits.srcID.getOrElse(0.U)
      releaseDbId := io.rxrsp.bits.dbID.getOrElse(0.U)
    }
    when (io.rxrsp.bits.chiOpcode.get === Comp && victimWriteEvictOrEvict) {
      compAckPending := true.B
      compReceived := true.B
    }
  }
  // Retire only after nested services, accepted TXREQ and required copyback/CompAck all drain.
  val consumedRetire = snoopConsumed && !releaseSent &&
    !io.txreqPending && !snoopInFlight && !io.snoopStarted
  val transportRetire = releaseAck && !compAckPending &&
    (!needsCopyBack || copyBackComplete || io.copyBackDone) &&
    !snoopInFlight && !io.snoopStarted && !io.txreqPending
  val retireEligible = valid && (consumedRetire || transportRetire) &&
    !io.nestedReleaseWritePending && !io.sourceBHold && !io.sinkCOwnerHold
  val mayRetire = retireEligible && !io.localReservationHold
  val retirementPending = RegInit(false.B)
  retirementPending := retireEligible && !io.tasks.preparedLease.held.valid && !io.tasks.preparedLease.proposalHeld && !mainpipeInvalidate
  val willFree = retirementPending && mayRetire &&
    !io.tasks.preparedLease.held.valid && !io.tasks.preparedLease.proposalHeld && !mainpipeInvalidate
  io.tasks.preparedLease.canPrepare := valid && !retireEligible && !retirementPending
  MainpipeLeaseIO.acknowledge(io.tasks.preparedLease, valid, io.alloc.valid, io.tasks.mainpipe.fire)
  when (willFree) {
    when (!consumedRetire && needsCopyBack) {
      assert(copyBackSent, "CopyBackWrData completed without being issued by ReplaceMSHR")
    }
    valid := false.B
    victim.valid := false.B
    donePulse := true.B
  }

  io.snoopAdmissionIdle := !snoopInFlight && !releaseDataPending &&
    !io.releaseDataSeen && !io.sinkC.valid && !io.victimDataCaptured &&
    !io.victimDataNeedsWriteback && !io.snoopMeta.valid && !io.snoopDoneCount.orR
  io.info.valid := valid
  io.info.bits := victim
  io.info.bits.releaseStarted := releaseSent
  io.info.bits.copyBackIssued := copyBackSent
  io.info.bits.releaseBufReady := releaseBufCaptured && !io.nestedReleaseWritePending
  io.info.bits.refillReady := probeDone && (!releaseBufRequired || releaseBufCaptured) &&
    !io.nestedReleaseWritePending
  io.done.valid := donePulse
  io.done.bits.parentId := victim.parentId
  io.done.bits.parentEpoch := victim.parentEpoch
}
