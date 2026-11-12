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
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import utility.MemReqSource
import xscache.coupledL2.MetaData._
import xscache.chi.CHICohStates._
import xscache.chi.{CHIChannel, CHIREQ, CHIRSP, HasCHIOpcodes}
import xscache.coupledL2.prefetch.DemandRefillBundle

class SnoopMSHRIO(implicit p: Parameters) extends MSHRIO {
  val allocDirectoryHit = Input(Bool())
  val pairSearchPending = Input(Bool())
  val normalRefillPayloadFromProbeAck = Input(Bool())
  val normalRefillPostGrantReleasePending = Input(Bool())
  val normalRefillPayloadFromPostGrantRelease = Input(Bool())
  val pairedNormalId = Flipped(ValidIO(UInt(mshrBits.W)))
  val allocPostGrantReleasePending = Input(Bool())
  val allocPostGrantReleasePayload = Input(Bool())
  val allocPairedNormalId = Flipped(ValidIO(UInt(mshrBits.W)))
  val releaseBufCopyDone = Input(Bool())
  val nestedReleaseWritePending = Input(Bool())
  val nestedReleaseWriteDone = Input(Bool())
  val postGrantReleaseResponse = Output(Bool())
  val snpRespDone = Input(Bool())
  val dctDone = Input(Bool())
}

class SnoopMSHR(implicit p: Parameters) extends MSHRContextBase {
  val io = IO(new SnoopMSHRIO)

  require(chiOpt.isDefined)

  val valid = RegInit(false.B)
  val sourceBProbeIssued = RegInit(false.B)
  io.allocEpoch := false.B
  io.readDBID.valid := false.B
  io.readDBID.bits := 0.U
  io.readTxnEpoch := 0.U
  io.replaceParentRelease := false.B
  io.sidecarDelegated := false.B
  io.probeIssued := sourceBProbeIssued
  io.nestedReleaseNeedsReplace := false.B
  io.replReadInFlight := false.B
  io.replReadWanted := false.B
  io.refillPayloadFromProbeAck := false.B
  io.postGrantReleaseWindow := false.B
  io.postGrantReleaseHold := false.B
  io.postGrantReleasePayloadReady := false.B
  io.postGrantReleaseSnoopOutcomeToN := false.B
  io.postGrantReleaseSnoopOutcomeToB := false.B
  io.postGrantReleaseSnoopOutcomeToClean := false.B
  val req = RegInit(0.U.asTypeOf(new TaskBundle))
  val dirResult = RegInit(0.U.asTypeOf(new DirResult))
  val directoryWayValid = RegInit(false.B)
  val stateInit = Wire(new FSMState)
  stateInit.elements.foreach(_._2 := true.B)
  val state = RegInit(stateInit)
  val probeDirty = RegInit(false.B)
  val normalProbePayloadLatched = RegInit(false.B)
  val normalPostGrantReleasePendingLatched = RegInit(false.B)
  val normalPostGrantReleasePayloadLatched = RegInit(false.B)
  val releaseBufPayloadReady = RegInit(false.B)
  val releaseBufCopyInFlight = RegInit(false.B)
  val pairedNormalIdValid = RegInit(false.B)
  val pairedNormalId = RegInit(0.U(mshrBits.W))
  val snpRespDone = RegInit(false.B)
  val dctDone = RegInit(false.B)

  when (io.alloc.valid) {
    assert(io.alloc.bits.task.fromB, "SnoopMSHR only accepts B-channel requests")
    assert(!io.alloc.bits.dirResult.hit || io.allocDirectoryHit || io.alloc.bits.task.snpHitRelease,
      "a synthetic snoop hit must be backed by replacement metadata")
    valid := true.B
    req := io.alloc.bits.task
    dirResult := io.alloc.bits.dirResult
    directoryWayValid := io.allocDirectoryHit
    state := io.alloc.bits.state
    probeDirty := false.B
    normalProbePayloadLatched := false.B
    normalPostGrantReleasePendingLatched := io.allocPostGrantReleasePending
    normalPostGrantReleasePayloadLatched := io.allocPostGrantReleasePayload
    releaseBufPayloadReady := false.B
    releaseBufCopyInFlight := false.B
    pairedNormalIdValid := io.allocPairedNormalId.valid
    pairedNormalId := io.allocPairedNormalId.bits
    snpRespDone := false.B
    dctDone := false.B
    sourceBProbeIssued := false.B
  }

  val meta = dirResult.meta
  val reqOpcode = req.chiOpcode.get
  val snpToN = isSnpToN(reqOpcode)
  val snpToB = isSnpToB(reqOpcode)
  val hitDirty = dirResult.hit && meta.dirty
  val metaChi = WireDefault(I)
  when (!meta.dirty && meta.state === INVALID) {
    metaChi := I
  }.elsewhen (!meta.dirty && meta.state === BRANCH) {
    metaChi := SC
  }.elsewhen (!meta.dirty && (meta.state === TRUNK || meta.state === TIP)) {
    metaChi := UC
  }.elsewhen (meta.dirty && (meta.state === TRUNK || meta.state === TIP)) {
    metaChi := UD
  }

  val hitWriteBack = req.snpHitRelease && req.snpHitReleaseWithData &&
    req.snpHitReleaseMeta.dirty && req.snpHitReleaseToInval
  val hitWriteClean = req.snpHitRelease && req.snpHitReleaseWithData &&
    req.snpHitReleaseMeta.dirty && req.snpHitReleaseToClean
  val hitWriteEvict = req.snpHitRelease && req.snpHitReleaseWithData &&
    !req.snpHitReleaseMeta.dirty
  val hitWriteX = hitWriteBack || hitWriteClean || hitWriteEvict
  val hitDirtyOrWriteDirty = hitDirty || hitWriteBack || hitWriteClean
  val tagErr = dirResult.hit && (meta.tagErr || dirResult.error)

  val doFwd = isSnpXFwd(reqOpcode) && dirResult.hit
  val doFwdHitRelease = isSnpXFwd(reqOpcode) && hitWriteX
  val neverRespData = isSnpMakeInvalidX(reqOpcode) || isSnpStashX(reqOpcode) ||
    isSnpQuery(reqOpcode) || reqOpcode === SnpOnceFwd || reqOpcode === SnpUniqueFwd ||
    reqOpcode === SnpPreferUniqueFwd
  val shouldRespData = dirResult.hit && (
    (isT(meta.state) && meta.dirty) ||
    (meta.state === TIP && !meta.dirty && reqOpcode === SnpOnce) ||
    (req.retToSrc.get && isSnpXFwd(reqOpcode)) ||
    (req.retToSrc.get && meta.state === BRANCH &&
      (reqOpcode === SnpOnce || reqOpcode === SnpUnique || reqOpcode === SnpPreferUnique || isSnpToBNonFwd(reqOpcode)))
  )
  when (!io.alloc.valid && io.normalRefillPayloadFromProbeAck) {
    normalProbePayloadLatched := true.B
  }
  when (!io.alloc.valid && io.normalRefillPostGrantReleasePending) {
    normalPostGrantReleasePendingLatched := true.B
  }
  when (!io.alloc.valid && io.normalRefillPayloadFromPostGrantRelease) {
    normalPostGrantReleasePayloadLatched := true.B
  }
  when (!io.alloc.valid && io.pairedNormalId.valid) {
    pairedNormalIdValid := true.B
    pairedNormalId := io.pairedNormalId.bits
  }
  val normalProbePayload = io.normalRefillPayloadFromProbeAck || normalProbePayloadLatched
  val normalPostGrantReleasePending = io.normalRefillPostGrantReleasePending ||
    normalPostGrantReleasePendingLatched
  val normalPostGrantSourcePayload = io.normalRefillPayloadFromPostGrantRelease ||
    normalPostGrantReleasePayloadLatched
  val normalPostGrantReleasePayload = normalPostGrantReleasePending && releaseBufPayloadReady &&
    !io.nestedReleaseWritePending
  val normalRefillPayload = normalProbePayload || normalPostGrantReleasePayload
  val doRespData = (shouldRespData || normalRefillPayload) && !neverRespData
  val needLateRefillToReleaseCopy =
    valid && normalPostGrantReleasePending && normalPostGrantSourcePayload &&
      !releaseBufPayloadReady && !releaseBufCopyInFlight &&
      pairedNormalIdValid && !state.s_probeack

  val respCacheState = WireDefault(I)
  respCacheState := metaChi
  when (snpToN || tagErr) {
    respCacheState := I
  }.elsewhen (snpToB) {
    respCacheState := Mux(req.snpHitReleaseToInval, I, SC)
  }.elsewhen (isSnpOnceX(reqOpcode)) {
    respCacheState := Mux(req.snpHitReleaseToInval, I,
      Mux(req.snpHitReleaseToClean, Mux(req.snpHitReleaseMeta.dirty, SC, metaChi),
        Mux(meta.dirty, UD, metaChi)))
  }.elsewhen (isSnpStashX(reqOpcode) || isSnpQuery(reqOpcode)) {
    respCacheState := Mux(meta.dirty, UD, metaChi)
  }.elsewhen (isSnpCleanShared(reqOpcode)) {
    respCacheState := Mux(isT(meta.state), UC, metaChi)
  }
  val respPassDirty = (hitDirtyOrWriteDirty || normalRefillPayload) && !tagErr && (
    snpToB || reqOpcode === SnpUnique || reqOpcode === SnpUniqueStash ||
      reqOpcode === SnpCleanShared || reqOpcode === SnpCleanInvalid || reqOpcode === SnpPreferUnique
  )
  val fwdCacheState = Mux(tagErr, I, Mux(isSnpToBFwd(reqOpcode),
    SC, Mux(isSnpToNFwd(reqOpcode), UC, I)))
  val fwdPassDirty = isSnpToNFwd(reqOpcode) &&
    (hitDirtyOrWriteDirty || normalRefillPayload) && !tagErr

  io.tasks.txreq.valid := false.B
  io.tasks.txreq.bits := 0.U.asTypeOf(new CHIREQ)
  io.tasks.txrsp.valid := false.B
  io.tasks.txrsp.bits := 0.U.asTypeOf(new CHIRSP)
  io.tasks.txdat.valid := false.B
  io.tasks.txdat.bits := 0.U.asTypeOf(new TaskBundle)
  io.dataRefill.valid := false.B
  io.dataRefill.bits := 0.U.asTypeOf(new DemandRefillBundle)

  val nestedwbMatch = valid && dirResult.hit && meta.state =/= INVALID &&
    dirResult.set === io.nestedwb.set && dirResult.tag === io.nestedwb.tag
  // After Probe acceptance, its ProbeAck remains mandatory even if a Release arrives before notification.
  val nestedwbCompletesUnissuedProbe = nestedwbMatch && !state.s_pprobe &&
    !sourceBProbeIssued && !io.sourceBAccepted
  val nestedReleaseReplacesProbe = nestedwbCompletesUnissuedProbe &&
    (io.nestedwb.c_set_dirty || io.nestedwb.c_set_tip)
  io.tasks.source_b.valid := valid && !state.s_pprobe && !normalPostGrantReleasePending &&
    !io.pairSearchPending && !nestedReleaseReplacesProbe
  io.tasks.source_b.bits := 0.U.asTypeOf(new SourceBReq)
  io.tasks.source_b.bits.tag := dirResult.tag
  io.tasks.source_b.bits.set := dirResult.set
  io.tasks.source_b.bits.off := 0.U
  io.tasks.source_b.bits.opcode := Probe
  io.tasks.source_b.bits.param := Mux(snpToB, toB, Mux(snpToN, toN, toT))
  io.tasks.source_b.bits.alias.foreach(_ := meta.alias.getOrElse(0.U))

  val mpCopyValid = needLateRefillToReleaseCopy
  // A post-Grant snoop may respond only after its dirty payload reaches this ReleaseBuf.
  val mpProbeAckValid = valid && !state.s_probeack && !mpCopyValid &&
    (state.w_pprobeacklast ||
      (normalPostGrantReleasePayload && !sourceBProbeIssued && !io.sourceBAccepted)) &&
    (!normalPostGrantReleasePending || normalPostGrantReleasePayload)
  val mpDctValid = valid && !state.s_dct.getOrElse(true.B) && state.s_probeack
  val mainpipeIntent = Wire(Decoupled(new TaskBundle))
  val mainpipeIssuedOperation = Wire(UInt(4.W))
  val mainpipeProposedOperation = Reg(UInt(4.W))
  mainpipeIntent.valid := (mpCopyValid || mpProbeAckValid || mpDctValid) &&
    !io.nestedReleaseWritePending && !io.pairSearchPending &&
    (!normalProbePayload || pairedNormalIdValid)

  val copyTask = WireInit(0.U.asTypeOf(new TaskBundle))
  copyTask.channel := req.channel
  copyTask.txChannel := 0.U
  copyTask.tag := req.tag
  copyTask.set := req.set
  copyTask.off := 0.U
  copyTask.size := log2Ceil(blockBytes).U
  copyTask.mshrTask := true.B
  copyTask.mshrId := io.id
  copyTask.mshrContext := 1.U
  copyTask.refillToReleaseCopy := true.B
  copyTask.refillBufReadId := pairedNormalId
  copyTask.readRefillData := true.B
  copyTask.useProbeData := false.B
  copyTask.way := dirResult.way
  copyTask.meta := MetaEntry()
  copyTask.wayMask := 0.U
  copyTask.reqSource := MemReqSource.NoWhere.id.U

  val postGrantUsesReleaseBuf = normalPostGrantReleasePayload
  val probeAckUsesRefillBuf = normalProbePayload && !postGrantUsesReleaseBuf
  val probeAckTask = WireInit(0.U.asTypeOf(new TaskBundle))
  probeAckTask.channel := req.channel
  probeAckTask.txChannel := Mux(doRespData, CHIChannel.TXDAT, CHIChannel.TXRSP)
  probeAckTask.tag := req.tag
  probeAckTask.set := req.set
  probeAckTask.off := req.off
  probeAckTask.size := log2Ceil(blockBytes).U
  probeAckTask.mshrTask := true.B
  probeAckTask.mshrId := io.id
  probeAckTask.mshrContext := 1.U
  probeAckTask.useProbeData := !probeAckUsesRefillBuf
  probeAckTask.readProbeDataDown := !probeAckUsesRefillBuf &&
    (doRespData || (!(snpToN || tagErr) && probeDirty) || postGrantUsesReleaseBuf)
  probeAckTask.readRefillData := probeAckUsesRefillBuf
  probeAckTask.refillBufReadId := pairedNormalId
  probeAckTask.way := dirResult.way
  probeAckTask.dirty := hitDirty || normalRefillPayload
  probeAckTask.meta := MetaEntry(
    dirty = !tagErr && (!(!dirResult.hit || !meta.dirty || snpToN || snpToB ||
      isSnpCleanShared(reqOpcode) || isSnpOnceX(reqOpcode) && req.snpHitReleaseToClean) ||
      isSnpOnceX(reqOpcode) && probeDirty),
    state = Mux(snpToN || tagErr, INVALID,
      Mux(snpToB || isSnpOnceFwd(reqOpcode) && hitWriteClean, BRANCH, meta.state)),
    clients = meta.clients & Fill(clientBits, !snpToN),
    alias = meta.alias,
    prefetch = !snpToN && meta.prefetch.getOrElse(false.B),
    accessed = !snpToN && meta.accessed
  )
  probeAckTask.metaWen := !req.snpHitReleaseToInval &&
    !(normalPostGrantReleasePayload && !dirResult.hit)
  probeAckTask.dsWen := !(snpToN || tagErr) && probeDirty
  probeAckTask.tgtID.get := req.srcID.get
  probeAckTask.srcID.get := 0.U
  probeAckTask.txnID.get := req.txnID.get
  probeAckTask.homeNID.get := 0.U
  probeAckTask.dbID.get := req.txnID.getOrElse(0.U)
  probeAckTask.chiOpcode.get := MuxLookup(Cat(doFwd || doFwdHitRelease, doRespData), SnpResp)(Seq(
    Cat(false.B, false.B) -> SnpResp,
    Cat(true.B, false.B) -> SnpRespFwded,
    Cat(false.B, true.B) -> SnpRespData,
    Cat(true.B, true.B) -> SnpRespDataFwded
  ))
  probeAckTask.resp.get := setPD(respCacheState, respPassDirty)
  probeAckTask.fwdState.get := setPD(fwdCacheState, fwdPassDirty)
  probeAckTask.retToSrc.get := req.retToSrc.get
  probeAckTask.traceTag.get := req.traceTag.get
  probeAckTask.snpHitRelease := req.snpHitRelease
  probeAckTask.snpHitReleaseToInval := req.snpHitReleaseToInval
  probeAckTask.snpHitReleaseToClean := req.snpHitReleaseToClean
  probeAckTask.snpHitReleaseWithData := req.snpHitReleaseWithData
  probeAckTask.snpHitReleaseIdx := req.snpHitReleaseIdx

  val dctTask = WireInit(0.U.asTypeOf(new TaskBundle))
  dctTask.channel := req.channel
  dctTask.txChannel := CHIChannel.TXDAT
  dctTask.tag := req.tag
  dctTask.set := req.set
  dctTask.off := req.off
  dctTask.size := log2Ceil(blockBytes).U
  dctTask.mshrTask := true.B
  dctTask.mshrId := io.id
  dctTask.mshrContext := 1.U
  dctTask.useProbeData := !probeAckUsesRefillBuf
  dctTask.readProbeDataDown := !probeAckUsesRefillBuf
  dctTask.readRefillData := probeAckUsesRefillBuf
  dctTask.refillBufReadId := pairedNormalId
  dctTask.way := dirResult.way
  dctTask.dirty := hitDirty || normalRefillPayload
  dctTask.tgtID.get := req.fwdNID.get
  dctTask.srcID.get := 0.U
  dctTask.txnID.get := req.fwdTxnID.get
  dctTask.homeNID.get := req.srcID.get
  dctTask.dbID.get := req.txnID.get
  dctTask.chiOpcode.get := CompData
  dctTask.resp.get := setPD(fwdCacheState, fwdPassDirty)
  dctTask.traceTag.get := req.traceTag.get
  dctTask.snpHitRelease := req.snpHitRelease
  dctTask.snpHitReleaseToInval := req.snpHitReleaseToInval
  dctTask.snpHitReleaseToClean := req.snpHitReleaseToClean
  dctTask.snpHitReleaseWithData := req.snpHitReleaseWithData
  dctTask.snpHitReleaseIdx := req.snpHitReleaseIdx
  dctTask.snpHitReleaseMeta := req.snpHitReleaseMeta
  // Select payload from the leased operation Q, never from the live arbitration result.
  mainpipeIntent.bits := Mux(mainpipeProposedOperation(1, 0) === 0.U, copyTask,
    Mux(mainpipeProposedOperation(1, 0) === 1.U, probeAckTask, dctTask))
  val leaseOwnerControls = Cat(io.normalRefillPayloadFromProbeAck,
    io.normalRefillPostGrantReleasePending, io.normalRefillPayloadFromPostGrantRelease,
    io.pairedNormalId.valid, Mux(io.pairedNormalId.valid, io.pairedNormalId.bits, 0.U))
  val leaseOwnerControlsChanged = leaseOwnerControls =/=
    RegNext(leaseOwnerControls, 0.U(leaseOwnerControls.getWidth.W))
  val leaseNestedEvent = io.nestedwb.c_set_dirty || io.nestedwb.c_set_tip ||
    io.nestedwb.b_inv_dirty || io.nestedwb.b_toN.getOrElse(false.B) ||
    io.nestedwb.b_toB.getOrElse(false.B) || io.nestedwb.b_toClean.getOrElse(false.B)
  val mainpipeInvalidate = io.pairSearchPending || io.alloc.valid || io.resps.sinkC.valid ||
    io.resps.rxrsp.valid || io.resps.rxdat.valid || io.releaseBufCopyDone ||
    io.snpRespDone || io.dctDone || leaseNestedEvent || leaseOwnerControlsChanged ||
    io.nestedReleaseWritePending || io.nestedReleaseWriteDone ||
    io.sourceBAccepted
  mainpipeProposedOperation := MainpipeLeaseIO.freeze(io.tasks.preparedLease,
    mainpipeIntent, io.tasks.mainpipe,
    Cat(normalPostGrantReleasePayload,
      Mux(mpCopyValid, 0.U(2.W), Mux(mpProbeAckValid, 1.U(2.W), 2.U(2.W)))), 0.U(3.W), mainpipeInvalidate)
  mainpipeIssuedOperation := mainpipeProposedOperation
  val mainpipeIssuedVariant = mainpipeIssuedOperation(1, 0)
  io.postGrantReleaseResponse := mainpipeIssuedOperation(2) && mainpipeIssuedVariant === 1.U

  when (io.tasks.mainpipe.fire && mainpipeIssuedVariant === 1.U && postGrantUsesReleaseBuf) {
    assert(probeAckTask.txChannel === CHIChannel.TXDAT &&
      probeAckTask.useProbeData && !probeAckTask.readRefillData,
      "post-Grant ReleaseData must return as SnpRespData from ReleaseBuf")
  }
  when (io.tasks.mainpipe.fire && mainpipeIssuedVariant === 1.U && probeAckUsesRefillBuf) {
    assert(probeAckTask.txChannel === CHIChannel.TXDAT &&
      probeAckTask.readRefillData && !probeAckTask.useProbeData,
      "paired ProbeAckData RefillBuf payload must return as SnpRespData")
  }
  when (io.tasks.mainpipe.fire && mainpipeIssuedVariant === 1.U && probeDirty &&
    !normalRefillPayload && !shouldRespData) {
    assert(probeAckTask.txChannel === CHIChannel.TXRSP,
      "standalone ProbeAckData must not force SnpRespData")
  }

  when (io.sourceBAccepted) {
    state.s_pprobe := true.B
    sourceBProbeIssued := true.B
  }
  when (io.tasks.mainpipe.fire) {
    when (mainpipeIssuedVariant === 0.U) {
      releaseBufCopyInFlight := true.B
    }.elsewhen (mainpipeIssuedVariant === 1.U) {
      state.s_probeack := true.B
      when (normalPostGrantReleasePayload) { state.s_pprobe := true.B }
    }.elsewhen (mainpipeIssuedVariant === 2.U) {
      state.s_dct.get := true.B
    }
  }
  when (!io.alloc.valid && io.releaseBufCopyDone) {
    releaseBufPayloadReady := true.B
    releaseBufCopyInFlight := false.B
  }
  when (!io.alloc.valid && io.snpRespDone) { snpRespDone := true.B }
  when (!io.alloc.valid && io.dctDone) { dctDone := true.B }

  when (io.resps.sinkC.valid) {
    val c = io.resps.sinkC.bits
    when (c.opcode === ProbeAck || c.opcode === ProbeAckData) {
      state.w_pprobeackfirst := true.B
      state.w_pprobeacklast := state.w_pprobeacklast || c.last
    }
    when (c.opcode === ProbeAckData) {
      probeDirty := true.B
      meta.dirty := true.B
    }
    when (isToN(c.param)) {
      meta.state := Mux(isT(meta.state), TIP, meta.state)
      meta.clients := Fill(clientBits, false.B)
    }.elsewhen (isToB(c.param)) {
      meta.state := Mux(isT(meta.state), TIP, meta.state)
    }
    when (isParamFromT(c.param)) {
      meta.tagErr := c.denied
      meta.dataErr := c.corrupt
    }
  }

  val mayRetire = state.s_pprobe && state.s_probeack && state.s_dct.getOrElse(true.B) &&
    snpRespDone && (!doFwd || dctDone) && !io.nestedReleaseWritePending &&
    (!sourceBProbeIssued || state.w_pprobeacklast) && !io.pairSearchPending && !io.sourceBHold && !io.sinkCOwnerHold
  val retirementPending = RegInit(false.B)
  retirementPending := mayRetire && !io.tasks.preparedLease.held.valid && !io.tasks.preparedLease.proposalHeld && !mainpipeInvalidate
  val willFree = retirementPending && mayRetire &&
    !io.tasks.preparedLease.held.valid && !io.tasks.preparedLease.proposalHeld && !mainpipeInvalidate
  io.tasks.preparedLease.canPrepare := valid && !mayRetire && !retirementPending
  MainpipeLeaseIO.acknowledge(io.tasks.preparedLease, valid, io.alloc.valid, io.tasks.mainpipe.fire)
  when (valid && willFree) { valid := false.B }

  io.status.valid := valid
  io.status.bits := 0.U.asTypeOf(new MSHRStatus)
  io.status.bits.channel := req.channel
  io.status.bits.txChannel := req.txChannel
  io.status.bits.set := req.set
  io.status.bits.reqTag := req.tag
  io.status.bits.metaTag := dirResult.tag
  io.status.bits.w_c_resp := !state.w_pprobeacklast
  io.status.bits.will_free := willFree
  io.status.bits.reqSource := req.reqSource

  io.statAlloc.valid := io.alloc.valid
  io.statAlloc.bits.is_miss := !io.alloc.bits.dirResult.hit
  io.statAlloc.bits.is_prefetch := false.B
  io.statAlloc.bits.channel := io.alloc.bits.task.channel

  io.msInfo.valid := valid
  io.msInfo.bits := 0.U.asTypeOf(new MSHRInfo)
  io.msInfo.bits.set := req.set
  io.msInfo.bits.way := dirResult.way
  io.msInfo.bits.reqTag := req.tag
  io.msInfo.bits.reqSource := req.reqSource
  io.msInfo.bits.willFree := willFree
  io.msInfo.bits.meta := meta
  io.msInfo.bits.metaTag := dirResult.tag
  io.msInfo.bits.dirHit := dirResult.hit
  io.msInfo.bits.blockRefill := directoryWayValid && dirResult.hit &&
    (doRespData || doFwd || meta.state === TRUNK)
  io.msInfo.bits.w_rprobeacklast := state.w_pprobeacklast
  io.msInfo.bits.blocksSnoop := true.B
  io.msInfo.bits.channel := req.channel

  val nestedwbPostGrantMatch = valid && normalPostGrantReleasePendingLatched &&
    req.set === io.nestedwb.set && req.tag === io.nestedwb.tag
  when (nestedReleaseReplacesProbe) {
    state.s_pprobe := true.B
    state.w_pprobeackfirst := true.B
    state.w_pprobeacklast := true.B
  }
  when (nestedwbMatch) {
    when (io.nestedwb.c_set_dirty) {
      probeDirty := true.B
      meta.dirty := true.B
      meta.state := TIP
      meta.clients := Fill(clientBits, false.B)
    }.elsewhen (io.nestedwb.c_set_tip) {
      meta.state := TIP
      meta.clients := Fill(clientBits, false.B)
    }
  }
  // Pending hides payload visibility without clearing the physical ReleaseBuf residency flag.
  when (!io.alloc.valid && io.nestedReleaseWriteDone) {
    releaseBufPayloadReady := true.B
  }
  io.nestedwbData := (nestedwbMatch || nestedwbPostGrantMatch) && io.nestedwb.c_set_dirty
  io.pCrd.query.valid := false.B
  io.pCrd.query.bits := 0.U.asTypeOf(io.pCrd.query.bits)
}
