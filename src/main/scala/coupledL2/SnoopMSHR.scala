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

/**
  * The B-channel half of a physical MSHR entry.
  *
  * It deliberately contains no L3 read, replacement, retry, CompAck, CMO, or
  * Grant state. Those remain in the single refill/evict [[MSHR]] paired with it.
  * The only transaction state kept here is the state needed to complete a snoop
  * that requires an L1 Probe and/or a DCT response.
  */
class SnoopMSHRIO(implicit p: Parameters) extends MSHRIO {
  // A paired normal MSHR can hold dirty ProbeAckData in RefillBuf. This is
  // distinct from normal RXDAT, which remains with the normal request until
  // the RXSNP refill/Grant interlock has released the snoop.
  val normalRefillPayloadFromProbeAck = Input(Bool())
  // A paired normal context accepted a post-Grant ReleaseData(TtoN). The
  // client has already relinquished the line, so this snoop must not issue a
  // second TL Probe; after the payload reaches this snoop's ReleaseBuf it
  // must return that data as SnpRespData.
  val normalRefillPostGrantReleasePending = Input(Bool())
  // Source-side indicator: the paired normal's RefillBuf already holds the
  // post-Grant ReleaseData. Local ReleaseBuf residency is tracked separately.
  val normalRefillPayloadFromPostGrantRelease = Input(Bool())
  // Physical index of the paired normal context (Valid when pending).
  val pairedNormalId = Flipped(ValidIO(UInt(mshrBits.W)))
  // MainPipe finished RefillBuf[N] -> ReleaseBuf[S] for this snoop.
  val releaseBufCopyDone = Input(Bool())
  // Export the exact B-side state transition to the paired normal context.
  // The normal may have started as a Directory miss and cannot infer this
  // from its own stale Directory snapshot.
  val postGrantReleaseResponseToN = Output(Bool())
  val postGrantReleaseResponseToB = Output(Bool())
  val postGrantReleaseResponseToClean = Output(Bool())
  // MainPipe acceptance only queues a CHI snoop response. Keep this context
  // visible until its terminal TXRSP/TXDAT handshake, so same-address A/B
  // requests cannot pass through the response queueing window.
  val snpRespDone = Input(Bool())
  val dctDone = Input(Bool())
}

class SnoopMSHR(implicit p: Parameters) extends MSHRContextBase {
  val io = IO(new SnoopMSHRIO)

  require(chiOpt.isDefined)

  val valid = RegInit(false.B)
  val sourceBProbeIssued = RegInit(false.B)
  // Only normal MSHR allocations can create replacement transactions.
  io.allocEpoch := false.B
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
  val stateInit = Wire(new FSMState)
  stateInit.elements.foreach(_._2 := true.B)
  val state = RegInit(stateInit)
  val probeDirty = RegInit(false.B)
  // `probeDirty` is local ProbeAckData bookkeeping.  It is not sufficient to
  // reinterpret an arbitrary snoop as the paired normal-refill handoff.
  val normalProbePayloadLatched = RegInit(false.B)
  val normalPostGrantReleasePendingLatched = RegInit(false.B)
  val normalPostGrantReleasePayloadLatched = RegInit(false.B)
  // Local ReleaseBuf[S] residency for post-Grant ReleaseData (nestedwb dual-write
  // or late RefillBuf[N]->ReleaseBuf[S] copy). Independent of normal's RefillBuf.
  val releaseBufPayloadReady = RegInit(false.B)
  val releaseBufCopyInFlight = RegInit(false.B)
  val pairedNormalIdValid = RegInit(false.B)
  val pairedNormalId = RegInit(0.U(mshrBits.W))
  val snpRespDone = RegInit(false.B)
  val dctDone = RegInit(false.B)
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)

  when (io.alloc.valid) {
    assert(io.alloc.bits.task.fromB, "SnoopMSHR only accepts B-channel requests")
    valid := true.B
    req := io.alloc.bits.task
    dirResult := io.alloc.bits.dirResult
    state := io.alloc.bits.state
    probeDirty := false.B
    normalProbePayloadLatched := false.B
    normalPostGrantReleasePendingLatched := io.normalRefillPostGrantReleasePending
    normalPostGrantReleasePayloadLatched := io.normalRefillPayloadFromPostGrantRelease
    releaseBufPayloadReady := false.B
    releaseBufCopyInFlight := false.B
    pairedNormalIdValid := io.pairedNormalId.valid
    pairedNormalId := io.pairedNormalId.bits
    snpRespDone := false.B
    dctDone := false.B
    denied := false.B
    corrupt := false.B
    sourceBProbeIssued := false.B
  }

  val meta = dirResult.meta
  val reqOpcode = req.chiOpcode.get
  val snpToN = isSnpToN(reqOpcode)
  val snpToB = isSnpToB(reqOpcode)
  val hitDirty = dirResult.hit && meta.dirty
  // Start from a fixed-width CHI state so the later setPD operands retain a
  // concrete width through all directory-state combinations.
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
  // Only the paired normal RefillBuf provenance creates the special data
  // response path.  A standalone ProbeAckData must keep the original CHI
  // response decision; otherwise it can overwrite an older ordered read.
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
  // Post-Grant SnpRespData always reads this snoop's ReleaseBuf, after nestedwb
  // dual-write or a late RefillBuf[N]->ReleaseBuf[S] copy.
  val normalPostGrantReleasePayload = normalPostGrantReleasePending && releaseBufPayloadReady
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
    Mux(req.snpHitReleaseToInval, I, SC), Mux(isSnpToNFwd(reqOpcode), UC, I)))
  val fwdPassDirty = isSnpToNFwd(reqOpcode) &&
    (hitDirtyOrWriteDirty || normalRefillPayload) && !tagErr

  io.tasks.txreq.valid := false.B
  io.tasks.txreq.bits := 0.U.asTypeOf(new CHIREQ)
  io.tasks.txrsp.valid := false.B
  io.tasks.txrsp.bits := 0.U.asTypeOf(new CHIRSP)
  io.tasks.txdat.valid := false.B
  io.tasks.txdat.bits := 0.U.asTypeOf(new TaskBundle)

  // A post-Grant ReleaseData proves the client no longer owns this block.
  // Waiting for a new ProbeAck would deadlock against the C release path.
  io.tasks.source_b.valid := valid && !state.s_pprobe && !normalPostGrantReleasePending
  io.tasks.source_b.bits := 0.U.asTypeOf(new SourceBReq)
  io.tasks.source_b.bits.tag := dirResult.tag
  io.tasks.source_b.bits.set := dirResult.set
  io.tasks.source_b.bits.off := 0.U
  io.tasks.source_b.bits.opcode := Probe
  io.tasks.source_b.bits.param := Mux(snpToB, toB, Mux(snpToN, toN, toT))
  io.tasks.source_b.bits.alias.foreach(_ := meta.alias.getOrElse(0.U))

  val mpCopyValid = needLateRefillToReleaseCopy
  // A paired post-Grant snoop must not answer before the dirty payload has
  // actually landed in this slot's ReleaseBuf.  SourceB is suppressed in that
  // case, so w_pprobeacklast is vacuously satisfied; without the final clause
  // the response task fires while the RefillBuf[N]->ReleaseBuf[S] copy is
  // still in flight and emits a data-less SnpResp, dropping the only copy of
  // the line's newest dirty data.
  val mpProbeAckValid = valid && !state.s_probeack && !mpCopyValid &&
    (state.w_pprobeacklast || normalPostGrantReleasePayload) &&
    (!normalPostGrantReleasePending || normalPostGrantReleasePayload)
  val mpDctValid = valid && !state.s_dct.getOrElse(true.B) && state.s_probeack
  io.tasks.mainpipe.valid := mpCopyValid || mpProbeAckValid || mpDctValid

  // Late copy: RefillBuf[pairedNormalId] -> ReleaseBuf[io.id]. No CHI side effect.
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

  // Post-Grant dirty data always lives in this snoop's ReleaseBuf (unified path),
  // including when the allocator happened to choose the same physical index as N.
  // ProbeAckData mirrored into the paired normal RefillBuf still uses readRefillData.
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
  // When the paired normal request has not installed its tag yet, this snoop
  // has no Directory way of its own. The normal context applies the exported
  // transition during its final RefillBuf commit instead.
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
  io.tasks.mainpipe.bits := Mux(mpCopyValid, copyTask,
    Mux(mpProbeAckValid, probeAckTask, dctTask))
  val postGrantReleaseResponseFire = io.tasks.mainpipe.fire && mpProbeAckValid &&
    normalPostGrantReleasePayload
  io.postGrantReleaseResponseToN := postGrantReleaseResponseFire &&
    probeAckTask.meta.state === INVALID
  io.postGrantReleaseResponseToB := postGrantReleaseResponseFire &&
    probeAckTask.meta.state === BRANCH
  io.postGrantReleaseResponseToClean := postGrantReleaseResponseFire &&
    !probeAckTask.meta.dirty

  when (io.tasks.mainpipe.fire && mpProbeAckValid && postGrantUsesReleaseBuf) {
    assert(probeAckTask.txChannel === CHIChannel.TXDAT &&
      probeAckTask.useProbeData && !probeAckTask.readRefillData,
      "post-Grant ReleaseData must return as SnpRespData from ReleaseBuf")
  }
  when (io.tasks.mainpipe.fire && mpProbeAckValid && probeAckUsesRefillBuf) {
    assert(probeAckTask.txChannel === CHIChannel.TXDAT &&
      probeAckTask.readRefillData && !probeAckTask.useProbeData,
      "paired ProbeAckData RefillBuf payload must return as SnpRespData")
  }
  when (io.tasks.mainpipe.fire && mpProbeAckValid && probeDirty &&
    !normalRefillPayload && !shouldRespData) {
    assert(probeAckTask.txChannel === CHIChannel.TXRSP,
      "standalone ProbeAckData must not force SnpRespData")
  }

  when (io.tasks.source_b.fire) {
    state.s_pprobe := true.B
    sourceBProbeIssued := true.B
  }
  when (io.tasks.mainpipe.fire) {
    when (mpCopyValid) {
      // ReleaseBuf write completes later in MainPipe S5; releaseBufCopyDone
      // (or nestedwb claim) marks local residency ready.
      releaseBufCopyInFlight := true.B
    }.elsewhen (mpProbeAckValid) {
      state.s_probeack := true.B
      when (normalPostGrantReleasePayload) { state.s_pprobe := true.B }
    }.elsewhen (mpDctValid) {
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
      denied := denied || c.denied
      corrupt := corrupt || c.corrupt
    }
  }

  val willFree = state.s_pprobe && state.s_probeack && state.s_dct.getOrElse(true.B) &&
    snpRespDone && (!doFwd || dctDone)
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
  // A snoop may still need this DS way as the fallback source for SnpRespData
  // or a forwarded response.  Keep the way out of refill replacement until
  // this context has finished; ReleaseBuf is not guaranteed to contain a copy
  // for every clean response path.
  io.msInfo.bits.blockRefill := dirResult.hit &&
    (doRespData || doFwd || meta.state === TRUNK)
  io.msInfo.bits.w_rprobeacklast := state.w_pprobeacklast
  io.msInfo.bits.blocksSnoop := true.B
  io.msInfo.bits.channel := req.channel

  // A Release( Data ) received before SourceB issues its Probe has already
  // relinquished the client, so it can directly complete the snoop.  Once B
  // has fired, its ProbeAck remains mandatory; completing here would free the
  // context before that already-issued response arrives.
  val nestedwbMatch = valid && dirResult.hit && meta.state =/= INVALID &&
    dirResult.set === io.nestedwb.set && dirResult.tag === io.nestedwb.tag
  // Post-Grant snoops often miss Directory (tag not installed yet). Still claim
  // the nestedwb dirty beat into ReleaseBuf[S] by request address.
  // Use only the registered pending latch here: the live pending IO is derived
  // from alloc.valid / selector eligibility, and feeding it into nestedwbData
  // would close a combinational cycle through orphan/replace allocation.
  // A same-cycle alloc+nestedwb miss is covered by the late RefillBuf copy.
  val nestedwbPostGrantMatch = valid && normalPostGrantReleasePendingLatched &&
    req.set === io.nestedwb.set && req.tag === io.nestedwb.tag
  val nestedwbCompletesUnissuedProbe = nestedwbMatch && !state.s_pprobe
  when (nestedwbCompletesUnissuedProbe && (io.nestedwb.c_set_dirty || io.nestedwb.c_set_tip)) {
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
  // Early dual-write: claim nestedwb dirty into this snoop's ReleaseBuf even on
  // Directory miss, as long as a post-Grant ReleaseData is pending for this line.
  when (!io.alloc.valid && nestedwbPostGrantMatch && io.nestedwb.c_set_dirty) {
    releaseBufPayloadReady := true.B
  }
  io.nestedwbData := (nestedwbMatch || nestedwbPostGrantMatch) && io.nestedwb.c_set_dirty
  io.pCrd.query.valid := false.B
  io.pCrd.query.bits := 0.U.asTypeOf(io.pCrd.query.bits)
}
