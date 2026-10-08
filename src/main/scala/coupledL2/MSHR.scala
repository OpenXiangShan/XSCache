/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package xscache.coupledL2

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import utility.MemReqSource
import utility.ParallelLookUp
import utility.ParallelMux
import utility.ParallelPriorityMux
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xscache.chi.CHIChannel
import xscache.chi.CHICohStateFwdedTransSet
import xscache.coupledL2.prefetch.DemandRefillBundle
import xscache.chi.CHICohStates._
import xscache.chi.CHICohStateTransSet
import xscache.chi.CHIDAT
import xscache.chi.CHIREQ
import xscache.chi.CHIRSP
import xscache.chi.HasCHIOpcodes
import xscache.chi.MemAttr
import xscache.chi.MPAM
import xscache.chi.OrderEncodings
import xscache.chi.RespErrEncodings._
import xscache.coupledL2._
import xscache.coupledL2.MetaData._
import xscache.coupledL2.prefetch.PfSource
import xscache.coupledL2.prefetch.PrefetchTrain

class MSHRTasks(implicit p: Parameters) extends CoupledL2Bundle {
  // outer
  val txreq = DecoupledIO(new CHIREQ) // TODO: no need to use decoupled Shandshake
  val txrsp = DecoupledIO(new CHIRSP) // TODO: no need to use decoupled handshake
  val source_b = DecoupledIO(new SourceBReq)
  val mainpipe = DecoupledIO(new TaskBundle) // To Mainpipe (SourceC or SourceD)
  // Direct victim CopyBackWrData (ReplaceMSHR only): bypasses the mainpipe
  // and enqueues task+data straight into TXDAT after a dedicated ReleaseBuf
  // read.  Other contexts tie this off.
  val txdat = DecoupledIO(new TaskBundle)
  // val prefetchTrain = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain)) // To prefetcher
}

class MSHRResps(implicit p: Parameters) extends CoupledL2Bundle {
  val sinkC = Flipped(ValidIO(new RespInfoBundle))
  val rxrsp = Flipped(ValidIO(new RespInfoBundle))
  val rxdat = Flipped(ValidIO(new RespInfoBundle))
}

class MSHRIO(implicit p: Parameters) extends CoupledL2Bundle {
  val id = Input(UInt(mshrBits.W))
  // Demand-refill completion report for the prefetch controller analysis.
  val dataRefill = ValidIO(new DemandRefillBundle)
  // Changes on every normal allocation and disambiguates a detached
  // ReplaceMSHR from a later tenant of the same physical normal slot.
  val allocEpoch = Output(Bool())
  val status = ValidIO(new MSHRStatus)
  val statAlloc = ValidIO(new MSHRAllocStatus)
  val msInfo = ValidIO(new MSHRInfo)
  val alloc = Flipped(ValidIO(new MSHRRequest))
  val tasks = new MSHRTasks()
  val resps = new MSHRResps()
  val nestedwb = Input(new NestedWriteback)
  val nestedwbData = Output(Bool())
  // An invalid victim was made dirty by nested ReleaseData after selection.
  val nestedReleaseNeedsReplace = Output(Bool())
  val aMergeTask = Flipped(ValidIO(new TaskBundle))
  val replResp = Flipped(ValidIO(new ReplacerResult))
  // Replace transaction control. RefillBuf remains owned by this normal MSHR;
  // the delegated victim uses another physical slot and its ReleaseBuf.
  val replaceActive = Input(Bool())
  // Registered ownership marker exported for arbitration. This intentionally
  // excludes the combinational replaceActive path to avoid a SourceB gate
  // feeding back through the normal context's source_b.valid.
  val sidecarDelegated = Output(Bool())
  // True only after this context has actually handed its current Probe to
  // SourceB. SinkC must not route a ProbeAck merely by address to a context
  // whose Probe is still queued behind another same-line context.
  val probeIssued = Output(Bool())
  val replaceDetached = Input(Bool())
  val replaceRefillReady = Input(Bool())
  // Completion of a CMO child ReplaceMSHR.  The normal CMO context retains
  // only the final CMO control request and CBOAck response.
  val replaceDone = Input(Bool())
  // MainPipe has reached this normal context's refill S3 commit/cancel point.
  // A context cannot retire before this pulse, otherwise a same-line snoop
  // can observe the transient normal-refill state.
  val refillWriteDone = Input(Bool())
  // MainPipe has actually handed this normal context's L1-visible
  // Grant/AccessAckData to GrantBuffer. Merely accepting the task from the
  // MSHR arbiter is not sufficient: MainPipe can still drop it on repl retry.
  val grantSent = Input(Bool())
  // Pulsed when a normal parent finally retires after its refill-only task.
  // Its detached ReplaceMSHR keeps running, but no longer belongs to this
  // physical normal slot after the slot is reused.
  val replaceParentRelease = Output(Bool())
  // N+X mutual exclusion: while a snoop occupies this physical entry, the
  // refill grant is deferred (snoop owns the data slot; victim becomes known
  // only at the grant's MainPipe pass, after the snoop retires).
  val deferGrant = Input(Bool())
  // A matching ReleaseData(TtoN) was accepted before the normal request's
  // ProbeAck.  Its dirty payload must be captured before this MSHR exposes an
  // AccessAckData/GrantData based on a fallback DS snapshot.
  val releaseDataBeforeProbeAck = Input(Bool())
  // A matching ReleaseData(TtoN) was accepted after the normal request's
  // L1-visible Grant but before its delayed RefillBuf commit. This payload
  // supersedes the already received RXDAT and can be required by a later
  // external snoop before it reaches DataStorage.
  val postGrantReleaseData = Input(Bool())
  // A same-line SnoopMSHR is consuming the post-Grant ReleaseData from this
  // normal context's RefillBuf. Keep the normal context and its buffer entry
  // alive until the snoop completes its terminal CHI response.
  val postGrantReleaseSnoopActive = Input(Bool())
  // Result of a paired SnoopMSHR that consumed the post-Grant ReleaseData.
  // This is explicit because the normal request may have begun as a directory
  // miss, in which case MainPipe's generic nested-writeback metadata is not
  // associated with a valid normal Directory hit.
  val postGrantReleaseSnoopToN = Input(Bool())
  val postGrantReleaseSnoopToB = Input(Bool())
  val postGrantReleaseSnoopToClean = Input(Bool())
  // The matching C ReleaseData took the direct MainPipe DS/meta path.  The
  // normal refill-only task must not repeat that update.
  val releaseDataDirectWrite = Input(Bool())
  // The grant pass merged the refill install at MainPipe S3 (folded
  // replacer read returned a no-probe victim).  The later refillOnly commit
  // degenerates to the RefillBuf->DS write alone.
  val grantInstallDone = Input(Bool())
  // A ReleaseData for the stale replacement candidate was accepted by SinkC.
  // Do not re-read the replacer until that C task reaches MainPipe and updates
  // the Directory snapshot used by the next replacement read.
  val blockReplRead = Input(Bool())
  // True while an internal normal-MSHR replacer read is awaiting Directory.
  // MSHRCtl uses this to protect a same-line ReleaseData from that snapshot.
  val replReadInFlight = Output(Bool())
  // Raw request for a replacer lookup. This intentionally excludes
  // blockReplRead so MSHRCtl can reserve a ReplaceMSHR/ReleaseBuf slot before
  // allowing the task into Directory's non-backpressured replacer pipeline.
  val replReadWanted = Output(Bool())
  // RefillBuf data provenance is explicit: only ProbeAckData can be mirrored
  // into a paired snoop response. RXDAT remains owned by the normal request
  // until RXSNP releases the refill/Grant critical window.
  val refillPayloadFromProbeAck = Output(Bool())
  // Export the post-Grant ReleaseData lifetime separately from ProbeAckData
  // provenance. The former is a TtoN writeback and must be visible to a
  // paired SnoopMSHR as SnpRespData.
  val postGrantReleaseWindow = Output(Bool())
  val postGrantReleaseHold = Output(Bool())
  val postGrantReleasePayloadReady = Output(Bool())
  // A paired snoop can resolve after this normal context has already handed a
  // refill-only commit task to RequestArb.  Export the registered result so
  // MainPipe can still apply the final snoop state at S3.
  val postGrantReleaseSnoopOutcomeToN = Output(Bool())
  val postGrantReleaseSnoopOutcomeToB = Output(Bool())
  val postGrantReleaseSnoopOutcomeToClean = Output(Bool())
  val pCrd = new PCrdQueryBundle
}

abstract class MSHRContextBase(implicit p: Parameters) extends CoupledL2Module with HasCHIOpcodes {
  val io: MSHRIO
}

class MSHR(implicit p: Parameters) extends MSHRContextBase {
  val io = IO(new MSHRIO)

  require(chiOpt.isDefined)

  val gotT = RegInit(false.B) // L3 might return T even though L2 wants B
  val gotDirty = RegInit(false.B)
  val gotGrantData = RegInit(false.B)
  val refillPayloadFromProbeAck = RegInit(false.B)
  val probeDirty = RegInit(false.B)
  val releaseDirty = RegInit(false.B)
  val timer = RegInit(0.U(64.W)) // for performance analysis
  val beatCnt = RegInit(0.U(log2Ceil(beatSize).W))

  val req_valid = RegInit(false.B)
  val allocEpoch = RegInit(false.B)
  val req = RegInit(0.U.asTypeOf(new TaskBundle()))
  val dirResult = RegInit(0.U.asTypeOf(new DirResult()))
  val meta = dirResult.meta
  val initState = Wire(new FSMState())
  initState.elements.foreach(_._2 := true.B)
  val state = RegInit(new FSMState(), initState)

  val req_released_chiOpcode = RegInit(0.U.asTypeOf(UInt(OPCODE_WIDTH.W)))
  val req_released_likelyShared = RegInit(false.B)

  assert(
    !(req_valid && dirResult.hit && !isT(meta.state) && meta.dirty),
    "directory valid read with dirty under non-T state"
  )

  /**
    * When all the ways are occupied with some mshr, other mshrs with the same set may retry to find a way to replace
    * over and over again, which may block the entrance of main pipe and lead to potential deadlock. To resolve the
    * problem, we allow mshr to retry immediately for 3 times (backoffThreshold). If it still fails to find a way, the
    * mshr must back off for a period of time (backoffCycles) to yield the opportunity to access main pipe.
    */
  val backoffThreshold = 3
  val backoffCycles = 20
  val retryTimes = RegInit(0.U(log2Up(backoffThreshold).W))
  val backoffTimer = RegInit(0.U(log2Up(backoffCycles).W))

  val tgtid_rcompack = Reg(UInt(NODEID_WIDTH.W)) // TgtID in CompAck of read / dataless transactions
  val txnid_rcompack = Reg(UInt(TXNID_WIDTH.W)) // TxnID in CompAck of read / dataless transactions
  val tgtid_wcompack = Reg(UInt(NODEID_WIDTH.W)) // TgtID in WriteData / CompAck of write transactions
  val txnid_wcompack = Reg(UInt(TXNID_WIDTH.W)) // TxnID in WriteData / CompAck of write transactions
  val srcid_retryack = Reg(UInt(NODEID_WIDTH.W)) // SrcID in RetryAck, only used for protocol retry

  val pcrdtype = RegInit(0.U(PCRDTYPE_WIDTH.W))
  val gotRetryAck = RegInit(false.B)
  // ReplaceMSHR can retire immediately after its CopyBack task enters the
  // main pipe.  Preserve the detach event in the normal context so the
  // normal FSM does not lose the completion condition when the sidecar's
  // valid bit drops in the following cycle.
  val sidecarDetachedLatched = RegInit(false.B)
  // Keep ownership after the replace context retires. Otherwise a parent which
  // has not yet reached will_free can re-issue the victim probe in the gap.
  val sidecarDelegatedLatched = RegInit(false.B)
  val sourceBProbeIssued = RegInit(false.B)
  // A normal request always owns RefillBuf. Its L1-visible Grant is separate
  // from the one normal-context task that finally installs RefillBuf in L2.
  // Once GrantBuffer accepts that Grant, SourceD can no longer retract it.
  // Directory retry must therefore reissue only the internal replacement
  // read, never the L1-visible response.
  val grantIssued = RegInit(false.B)
  // Snapshot the merged-A ownership semantics with the one L1-visible Grant
  // task. The delayed refill commit must install precisely that ownership even
  // if a later Directory revalidation changes live request-side conditions.
  val grantMergeSemanticsLatched = RegInit(false.B)
  val grantMergedClientLatched = RegInit(false.B)
  val grantMergedNeedsTLatched = RegInit(false.B)
  // Directory alias must track the merged Acquire's color, not the original
  // prefetch's (often-zero) meta.alias; otherwise anti-alias Probe misses L1.
  val grantMergedAliasLatched = aliasBitsOpt.map(_ => RegInit(0.U(aliasBitsOpt.get.W)))
  val normalRefillCommitIssued = RegInit(false.B)
  // Step-1 grant/install merge (perf report): for a miss whose victim does not
  // need probing (invalid, or valid with no L1 clients), the sidecar's capture
  // lands long before the refill data arrives, so victimSafeToOverwrite is
  // already true when the L1 grant fires.  In that case the grant task carries
  // the refill install (tag/meta/DS write) and the separate commit task is
  // suppressed, saving one mainpipe pass per such miss.  Probed victims keep
  // the deferred split: the grant stays early and the commit waits for the
  // sidecar's probe + capture.
  val mergeInstallWithGrant = RegInit(false.B)
  // The merged form was actually used at this context's grant fire.  The mode
  // register alone cannot gate the commit: the grant may legally fire before
  // the replResp arrives (early grant in split form), and a later no-probe
  // replResp would then set the mode register on a context whose grant
  // already went out without the install -- suppressing the commit there
  // would deadlock the context.  Only a fire that happened in merge mode
  // (this flag) suppresses the commit.
  val mergedGrantInstalled = RegInit(false.B)
  // The common merge: the grant's folded replacer read returned a no-probe
  // victim in its own MainPipe pass, which snapshotted the victim DS data
  // into the sidecar ReleaseBuf and wrote Directory tag/meta.  Pulsed by
  // MSHRCtl from MainPipe's S3; the later refillOnly commit degenerates to
  // the DS write alone.
  val grantInstallDoneReg = RegInit(false.B)
  // A post-Grant ReleaseData can be consumed by an external snoop before the
  // normal refill-only task reaches MainPipe. Once that snoop has committed
  // its Directory transition, the normal task must retire without restoring
  // the older pre-snoop metadata.
  val postGrantReleaseSnoopConsumed = RegInit(false.B)
  val postGrantReleaseSnoopToB = RegInit(false.B)
  val postGrantReleaseSnoopToClean = RegInit(false.B)
  // If L1 has already observed a normal Grant and a later toN invalidates
  // this line, that Grant remains the ordered old-data response. The delayed
  // RefillBuf commit must retire without re-installing the stale line.
  val normalRefillDroppedByToN = RegInit(false.B)
  // An initial Directory hit can be invalidated or replaced while its early
  // L1 response is in flight. Revalidate it before the delayed DS/tag/meta
  // commit so a stale hit way is never overwritten. This operation selects a
  // new victim on a miss, so it must not be used as a tag-only check for an
  // initial miss.
  val normalRefillRevalidateNeeded = RegInit(false.B)
  val normalRefillRevalidateInFlight = RegInit(false.B)
  val sidecarVictimReadyLatched = RegInit(false.B)
  // MainPipe accepts a refill-only task several cycles before its S3
  // Directory/DS commit. Keep the context visible to RXSNP for that interval,
  // including metadata-only commits with no DS write.
  val refillWritePending = RegInit(false.B)
  // `refillWriteDone` coincides with the S3 normal refill commit, while a
  // same-set refill may already be in Directory's S1--S3 selection pipe.
  // Retain this MSHR's chosen way through that pipe, otherwise an invalid
  // victim can be selected again before the tag/meta write is observable.
  val refillWayCommitHold = RegInit(0.U(3.W))
  val refillWayCommitPending = refillWayCommitHold.orR
  val sidecarCmoDone = RegInit(false.B)
  // A normal read keeps `io.id` as its CHI TxnID until CompAck completes.
  // Snoop ordering is handled by RXSNP and HN response serialization; it must
  // never create another normal CHI read for the same upper request.
  val readTxnInFlight = RegInit(false.B)
  // RequestArb's directory pipeline takes several cycles to return a
  // ReplacerResult.  Keep the request in flight during that interval so a
  // refill-only task cannot issue the same replacement read again.
  val replReadInFlight = RegInit(false.B)
  io.replReadInFlight := replReadInFlight
  io.refillPayloadFromProbeAck := req_valid && refillPayloadFromProbeAck
  // `postGrantReleaseSeen` is set at SinkC acceptance, before the C task
  // reaches MainPipe. This closes the window where an already queued normal
  // refill-only task could otherwise commit stale RXDAT ahead of ReleaseData.
  val postGrantReleaseSeen = RegInit(false.B)
  val postGrantReleasePayloadReady = RegInit(false.B)
  // A clean Release(TtoN) for the request line in the grant->commit window is
  // invisible to Directory (the tag is not installed until the deferred
  // commit), so the C path cannot clear meta.clients for it.  Latch it here;
  // otherwise the commit would install clients=1 for a copy the L1 has
  // already dropped, and every later victim selection would issue a useless
  // Probe to a non-existent client.
  val postGrantCleanReleaseSeen = RegInit(false.B)
  // `grantIssued` is defined only for a non-CMO SourceD Grant path, so no
  // separate CMO qualifier is needed here (and keeping this near the state
  // registers avoids a forward dependency on `cmo_cbo`).
  io.postGrantReleaseWindow := req_valid &&
    (grantIssued || io.grantSent) && !normalRefillCommitIssued
  io.postGrantReleaseHold := req_valid && postGrantReleaseSeen
  io.postGrantReleasePayloadReady := req_valid && postGrantReleasePayloadReady
  // ToN dominates the other response properties: its response meta is
  // INVALID and therefore can also look clean.  ToB similarly dominates the
  // generic clean indication.
  io.postGrantReleaseSnoopOutcomeToN := req_valid && postGrantReleaseSnoopConsumed
  io.postGrantReleaseSnoopOutcomeToB := req_valid && !postGrantReleaseSnoopConsumed &&
    postGrantReleaseSnoopToB
  io.postGrantReleaseSnoopOutcomeToClean := req_valid && !postGrantReleaseSnoopConsumed &&
    !postGrantReleaseSnoopToB && postGrantReleaseSnoopToClean
  assert(
    PopCount(Seq(
      io.postGrantReleaseSnoopOutcomeToN,
      io.postGrantReleaseSnoopOutcomeToB,
      io.postGrantReleaseSnoopOutcomeToClean
    )) <= 1.U,
    "post-Grant ReleaseData snoop outcome must be unique"
  )
  // A matching ReleaseData can arrive in the cycle that the post-replacement
  // refill-only task is selected.  Hold its coherence metadata until that
  // task is emitted, otherwise the task overwrites the newer C-path update.
  val nestedReleaseRefillPending = RegInit(false.B)
  // This is set from SinkC acceptance, not from MainPipe S3.  It closes the
  // window in which a ProbeAck can make an old RefillBuf snapshot grantable
  // before the preceding ReleaseData has reached the data buffer.
  val releaseDataBeforeProbeAckPending = RegInit(false.B)
  val releaseDataDirectWriteSeen = RegInit(false.B)
  val gotPCrdGrant = RegInit(false.B)

  val tagErr = RegInit(false.B) // L2 Tag Error
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)
  val cbWrDataTraceTag = RegInit(false.B)
  val metaChi = ParallelLookUp(
    Cat(meta.dirty, meta.state),
    Seq(
      Cat(false.B, INVALID) -> I,
      Cat(false.B, BRANCH) -> SC,
      Cat(false.B, TRUNK) -> UC,
      Cat(false.B, TIP) -> UC,
      Cat(true.B, TRUNK) -> UD,
      Cat(true.B, TIP) -> UD
    )
  )

  io.pCrd.query.valid := gotRetryAck && !gotPCrdGrant
  io.pCrd.query.bits.pCrdType := pcrdtype
  io.pCrd.query.bits.srcID := srcid_retryack

  /* Allocation */
  when(io.alloc.valid) {
    allocEpoch := !allocEpoch
    req_valid := true.B
    state := io.alloc.bits.state
    dirResult := io.alloc.bits.dirResult
    req := io.alloc.bits.task
    gotT := false.B
    gotDirty := false.B
    gotGrantData := false.B
    refillPayloadFromProbeAck := false.B
    probeDirty := false.B
    releaseDirty := false.B
    timer := 1.U
    beatCnt := 0.U

    gotRetryAck := false.B
    gotPCrdGrant := false.B
    sidecarDetachedLatched := false.B
    sidecarDelegatedLatched := false.B
    sourceBProbeIssued := false.B
    grantIssued := false.B
    grantMergeSemanticsLatched := false.B
    grantMergedClientLatched := false.B
    grantMergedNeedsTLatched := false.B
    grantMergedAliasLatched.foreach(_ := 0.U)
    normalRefillCommitIssued := false.B
    mergeInstallWithGrant := false.B
    mergedGrantInstalled := false.B
    grantInstallDoneReg := false.B
    postGrantReleaseSnoopConsumed := false.B
    postGrantReleaseSnoopToB := false.B
    postGrantReleaseSnoopToClean := false.B
    normalRefillDroppedByToN := false.B
    normalRefillRevalidateNeeded := io.alloc.bits.dirResult.hit && !io.alloc.bits.task.cmoTask
    normalRefillRevalidateInFlight := false.B
    readTxnInFlight := false.B
    replReadInFlight := false.B
    sidecarVictimReadyLatched := false.B
    refillWritePending := false.B
    refillWayCommitHold := 0.U
    sidecarCmoDone := false.B
    nestedReleaseRefillPending := false.B
    releaseDataBeforeProbeAckPending := false.B
    releaseDataDirectWriteSeen := false.B
    postGrantReleaseSeen := false.B
    postGrantReleasePayloadReady := false.B
    postGrantCleanReleaseSeen := false.B

    pcrdtype := 0.U
    tagErr := io.alloc.bits.dirResult.hit && (io.alloc.bits.dirResult.meta.tagErr || io.alloc.bits.dirResult.error)
    denied := false.B
    corrupt := false.B
    cbWrDataTraceTag := false.B

    retryTimes := 0.U
    backoffTimer := 0.U
  }

  val sidecarOwnsVictim = io.replaceActive || sidecarDelegatedLatched
  io.sidecarDelegated := sidecarDelegatedLatched
  io.probeIssued := sourceBProbeIssued
  when(io.replaceActive && !io.alloc.valid) {
    sidecarDelegatedLatched := true.B
  }
  when(io.replaceActive && io.replaceDetached && !io.alloc.valid) {
    sidecarDetachedLatched := true.B
  }
  when(io.replaceActive && io.replaceRefillReady && !io.alloc.valid) {
    sidecarVictimReadyLatched := true.B
  }
  val sidecarDetached = sidecarDetachedLatched || (io.replaceActive && io.replaceDetached)
  val sidecarVictimReady = sidecarVictimReadyLatched || (io.replaceActive && io.replaceRefillReady)

  val nestedReleaseTargetsRefill = req_valid &&
    req.set === io.nestedwb.set && req.tag === io.nestedwb.tag
  val nestedReleaseDirtyThisCycle = nestedReleaseTargetsRefill && io.nestedwb.c_set_dirty
  val releaseDataBlocksGrant = io.releaseDataBeforeProbeAck || releaseDataBeforeProbeAckPending
  when(!io.alloc.valid && nestedReleaseTargetsRefill) {
    // ReleaseData replaces RefillBuf independently of the RXDAT/ProbeAckData
    // race; it must not inherit ProbeAckData provenance.
    refillPayloadFromProbeAck := false.B
    // A clean Release(TtoN) on the request line drops the only client copy.
    when(io.nestedwb.c_set_tip) {
      postGrantCleanReleaseSeen := true.B
    }
  }
  when(!io.alloc.valid) {
    when(io.postGrantReleaseData) {
      assert(io.postGrantReleaseWindow,
        "post-Grant ReleaseData must target a live uncommitted normal context")
      postGrantReleaseSeen := true.B
    }
    when(io.postGrantReleaseSnoopToN) {
      postGrantReleaseSnoopConsumed := true.B
      normalRefillRevalidateNeeded := false.B
      state.w_replResp := true.B
    }
    when(io.postGrantReleaseSnoopToB) {
      postGrantReleaseSnoopToB := true.B
      normalRefillRevalidateNeeded := false.B
    }
    when(io.postGrantReleaseSnoopToClean) {
      postGrantReleaseSnoopToClean := true.B
      normalRefillRevalidateNeeded := false.B
    }
    when(nestedReleaseDirtyThisCycle && postGrantReleaseSeen) {
      postGrantReleasePayloadReady := true.B
    }
    when(io.releaseDataBeforeProbeAck) {
      releaseDataBeforeProbeAckPending := true.B
    }.elsewhen(nestedReleaseDirtyThisCycle) {
      releaseDataBeforeProbeAckPending := false.B
    }
    when(io.releaseDataDirectWrite) {
      releaseDataDirectWriteSeen := true.B
    }
    when(io.grantInstallDone) {
      grantInstallDoneReg := true.B
    }
  }

  /* ======== Enchantment ======== */
  val meta_pft = meta.prefetch.getOrElse(false.B)
  val meta_no_client = !meta.clients.orR

  val req_needT = needT(req.opcode, req.param)
  val req_needB = needB(req.opcode, req.param)
  val req_acquire =
    req.opcode === AcquireBlock && req.fromA || req.opcode === AcquirePerm // AcquireBlock and Probe share the same opcode
  val req_acquirePerm = req.opcode === AcquirePerm
  val req_get = req.opcode === Get
  val req_prefetch = req.opcode === Hint
  val req_isDemandOrPrefetch = req_acquire || req_get || req_prefetch

  val req_mayRepl = req_acquire || req_get || req_prefetch

  val req_chiOpcode = req.chiOpcode.get

  val snpToN = isSnpToN(req_chiOpcode)
  val snpToB = isSnpToB(req_chiOpcode)

  val req_cboClean = req.fromA && req.opcode === CBOClean
  val req_cboFlush = req.fromA && req.opcode === CBOFlush
  val req_cboInval = req.fromA && req.opcode === CBOInval

  val cmo_cbo = req_cboClean || req_cboFlush || req_cboInval

  // A valid victim may be overwritten only after its ReplaceMSHR has either
  // captured the required old data in ReleaseBuf or established that no data
  // needs to be retained. Invalid ways have no sidecar and are immediately
  // safe once their ReplacerResult reaches the normal MSHR.
  val victimSafeToOverwrite = !sidecarOwnsVictim ||
    (sidecarDetached && sidecarVictimReady)

  // *NOTICE: WriteBack/WriteClean(s) with nested snoops that passed dirty were not considered as
  //          a nested hit here, which would no longer pass latest data to lower tier memories.
  val hitDirty = dirResult.hit && meta.dirty
  val hitWriteBack =
    req.snpHitRelease && req.snpHitReleaseWithData && req.snpHitReleaseMeta.dirty && req.snpHitReleaseToInval
  val hitWriteClean =
    req.snpHitRelease && req.snpHitReleaseWithData && req.snpHitReleaseMeta.dirty && req.snpHitReleaseToClean
  val hitWriteEvict = req.snpHitRelease && req.snpHitReleaseWithData && !req.snpHitReleaseMeta.dirty

  val hitWriteX = hitWriteBack || hitWriteClean || hitWriteEvict
  val hitWriteDirty = hitWriteBack || hitWriteClean
  val hitDirtyOrWriteDirty = hitDirty || hitWriteDirty

  val releaseToClean = req_cboClean

  /**
    * About which snoop should echo SnpRespData[Fwded] instead of SnpResp[Fwded]:
    * 1. When the snooped block is dirty, always echo SnpRespData[Fwded], except for SnpMakeInvalid*, SnpStash*,
    *    SnpOnceFwd, and SnpUniqueFwd.
    * 2. When the snoop opcode is SnpCleanFwd, SnpNotSharedDirtyFwd or SnpSharedFwd, always echo SnpRespDataFwded
    *    if RetToSrc = 1 as long as the snooped block is valid.
    *    if L2 tagErr, not forward data
    * 3. When the snoop opcode is non-forwarding non-stashing snoop, echo SnpRespData if RetToSrc = 1 as long as the
    *    cache line is Shared Clean and the snoopee retains a copy of the cache line.
    */
  val doRespData_dirty = hitDirtyOrWriteDirty && (
    req_chiOpcode === SnpOnce ||
      snpToB ||
      req_chiOpcode === SnpUnique ||
      req_chiOpcode === SnpUniqueStash ||
      req_chiOpcode === SnpCleanShared ||
      req_chiOpcode === SnpCleanInvalid ||
      req_chiOpcode === SnpPreferUnique
  )
  // *NOTICE: Careful on future implementation of adding 'isSnpToNFwd' into condition
  //          'doRespData_retToSrc_fwd'. For now, 'isSnpToNFwd' only covers SnpUniqueFwd,
  //          which should never return data to Home Node except No Fwd to Requester.
  //          No Fwds on DCT are not implemented because Fwded responses are always perferred.
  val doRespData_retToSrc_fwd = req.retToSrc.get &&
    (isSnpToBFwd(req_chiOpcode) /*|| isSnpToNFwd(req_chiOpcode)*/ )
  val doRespData_retToSrc_nonFwd = req.retToSrc.get && (
    dirResult.hit && meta.state === BRANCH &&
      (isSnpToBNonFwd(req_chiOpcode) || isSnpToNNonFwd(req_chiOpcode) || isSnpOnce(req_chiOpcode))
  )
  // doRespData_once includes
  //  1. SnpOnceFwd : UD -> I     (nesting WriteBack)
  //  2. SnpOnceFwd : UD -> SC    (nesting WriteClean)
  //  3. SnpOnce    : UC -> UC    (non-nesting)
  //  4. SnpOnce    : UC -> I     (nesting WriteEvict)
  val doRespData_once = (hitWriteBack || hitWriteClean) &&
    isSnpOnceFwd(req_chiOpcode) ||
    (dirResult.hit && !meta.dirty && meta.state =/= BRANCH || hitWriteEvict) &&
      isSnpOnce(req_chiOpcode)
  val doRespData =
    (doRespData_dirty || doRespData_retToSrc_fwd || doRespData_retToSrc_nonFwd || doRespData_once) && !tagErr

  dontTouch(doRespData_dirty)
  dontTouch(doRespData_retToSrc_fwd)
  dontTouch(doRespData_retToSrc_nonFwd)

  // *NOTICE: SnpUniqueStash was included in condition 'doRespData_retToSrc_nonFwd', while
  //          the 'retToSrc' of SnpUniqueStash must be bound to 0, and whether responding
  //          SnpRespData or SnpResp was not determined by 'retToSrc'.
  //          the 'retToSrc' of SnpQuery must be bound to 0
  assert(
    !(req_valid && req_chiOpcode === SnpUniqueStash && req.retToSrc.get),
    "specification failure: received SnpUniqueStash with RetToSrc = 1"
  )
  assert(
    !(req_valid && isSnpQuery(req_chiOpcode) && req.retToSrc.get),
    "specification failure: received SnpQuery with RetToSrc = 1"
  )

  /**
    * About which snoop should echo SnpResp[Data]Fwded instead of SnpResp[Data]:
    * 1. When the snoop opcode is Snp*Fwd and the snooped block is valid.
    */
  val doFwd = isSnpXFwd(req_chiOpcode) && dirResult.hit
  val doFwdHitRelease = isSnpXFwd(req_chiOpcode) && hitWriteX

  val gotUD = meta.dirty // TC/TTC -> UD
  val promoteT_normal = dirResult.hit && meta_no_client && meta.state === TIP
  val promoteT_L3 = !dirResult.hit && gotT
  val promoteT_alias = dirResult.hit && req.aliasTask.getOrElse(false.B) && (meta.state === TRUNK || meta.state === TIP)
  // under above circumstances, we grant T to L1 even if it wants B
  val req_promoteT = (req_acquire || req_get || req_prefetch) && (promoteT_normal || promoteT_L3 || promoteT_alias)

  assert(!(req_valid && req_prefetch && dirResult.hit), "MSHR can not receive prefetch hit req")

  /* ======== Task allocation ======== */
  // All old-line release traffic belongs to ReplaceMSHR.  A normal MSHR keeps
  // only read/acquire and final CMO control requests on its direct TXREQ path.
  val release_valid1 = false.B
  val release_valid2 = false.B
  val cmoProbeDone = state.w_rprobeacklast || sidecarDetached
  val retryReissue = !state.s_reissue.getOrElse(false.B) && !state.w_grant &&
    gotRetryAck && gotPCrdGrant
  val cmoInitialIssue = cmo_cbo && !state.s_acquire &&
    cmoProbeDone && state.w_releaseack && state.s_cmometaw && state.s_cbwrdata.get
  // A normal request issues exactly one CHI read.  Same-line snoops are
  // ordered by the HN and RXSNP, not by replaying this request.
  val normalInitialIssue = !cmo_cbo && !state.s_acquire &&
    !readTxnInFlight
  io.tasks.txreq.valid := normalInitialIssue || cmoInitialIssue || retryReissue
  val rcompack_valid = !state.s_rcompack.get && state.w_grant &&
    // For issue B, CompAck must not be sent until all transfers of read data have been received.
    // For issue C and afterwards, CompAck is allowed to be sent after at least one CompData packet is received.
    afterIssueCOrElse(state.w_grantfirst, state.w_grantlast)
  val wcompack_valid = !state.s_wcompack.get && state.s_rcompack.get // wcompack can only be sent after rcompack
  io.tasks.txrsp.valid := rcompack_valid || wcompack_valid
  io.tasks.source_b.valid := !sidecarOwnsVictim && (!state.s_pprobe || !state.s_rprobe)
  // The direct TXDAT path is ReplaceMSHR-only; the normal context's
  // CopyBackWrData keeps its mainpipe pass.
  io.tasks.txdat.valid := false.B
  io.tasks.txdat.bits := 0.U.asTypeOf(new TaskBundle)
  val mp_release_valid = release_valid1
  val mp_cbwrdata_valid = !sidecarOwnsVictim && !state.s_cbwrdata.getOrElse(true.B) && state.w_releaseack
  val mp_probeack_valid = !state.s_probeack && state.w_pprobeacklast
  // Grant is L1-visible and is issued exactly once. A delegated victim has a
  // separate refill-only task below, so it must never make this predicate true
  // after the first grant has completed.
  // A data-bearing grant (AccessAckData/GrantData) must have a data source:
  // refill data (gotGrantData), probe data (probeDirty), or the hit line's DS
  // data (dirResult.hit). RXSNP holds a same-line snoop until one of these
  // sources is stable for the response path.
  val grantIsData = odOpGen(req.opcode) === AccessAckData || odOpGen(req.opcode) === GrantData
  // A concurrent snoop delays the L1-visible Grant, but a miss must still
  // query the replacer. This is an internal Directory-only task; it never
  // commits the normal RefillBuf.
  val replReadBlockedBySnoopOrReplace = io.replaceActive
  val grantNeedsReplRead = !dirResult.hit && !state.w_replResp &&
    !replReadInFlight && !io.blockReplRead && !denied && !replReadBlockedBySnoopOrReplace
  val normalGrantPending = !cmo_cbo && !state.s_refill && !grantIssued &&
    req_valid
  val cmoGrantPending = cmo_cbo && !state.s_cmoresp && state.w_releaseack && state.s_cbwrdata.get
  // The grant no longer waits for a delegated VICTIM probe: for a miss
  // (dirResult.hit=0) the victim's L1 revocation is the sidecar's business and
  // inclusion is enforced at the commit (victimSafeToOverwrite requires
  // sidecarDetached && sidecarVictimReady).  BUT the rprobe wait is still
  // required when it covers the REQUEST LINE's own probe: hit-at-alloc
  // contexts born for cache_alias / Get-on-TRUNK (need_probe_s3_a) clear
  // w_rprobeacklast at alloc and their GrantData must wait for the L1 probe
  // to return the dirty data.  The two cases are disjoint: request-line
  // probes only exist on hits, victim probes only on misses.
  // CMO keeps its own stronger gate (cmoGrantPending requires w_releaseack,
  // which implies the sidecar's probe + writeback already completed).
  val grantTaskPending = (normalGrantPending || cmoGrantPending) &&
    state.w_grantlast && state.w_grant && (state.w_rprobeacklast || !dirResult.hit) &&
    (!grantIsData || gotGrantData || probeDirty || dirResult.hit)
  val grantTaskAllowed = retryTimes < backoffThreshold.U || backoffTimer === backoffCycles.U
  // Before the L1 Grant, deferGrant makes the replacer read internal. After
  // an irrevocable Grant, retry must likewise use an internal-only task.
  val internalReplRead = grantNeedsReplRead && (io.deferGrant || grantIssued)
  // ReplacerResult carries only the physical MSHR id. Keep every Directory
  // lookup for this normal context serialized so that id is sufficient to
  // identify the returning result.
  val replLookupInFlight = replReadInFlight || normalRefillRevalidateInFlight
  val replReadTaskPending = req_valid &&
    (grantTaskPending || (!cmo_cbo && grantIssued))
  io.replReadWanted := replReadTaskPending && grantTaskAllowed && !dirResult.hit && !state.w_replResp &&
    !replReadInFlight && !denied && !replReadBlockedBySnoopOrReplace
  val mp_repl_read_valid = replReadTaskPending && internalReplRead && grantTaskAllowed && !replLookupInFlight
  assert(
    !(mp_repl_read_valid && replReadBlockedBySnoopOrReplace),
    "normal MSHR must not select a replacer victim during snoop/child recovery"
  )
  // `mp_grant` is the sole L1-visible task. In merge mode it also performs
  // the refill install, so it must additionally wait for the victim to be
  // safe to overwrite (a no-probe sidecar's capture is normally long done by
  // data-arrival).  In split mode it defers the install to the commit.
  val mp_grant_valid = grantTaskPending && !io.deferGrant && !releaseDataBlocksGrant && grantTaskAllowed &&
    !replLookupInFlight &&
    (!mergeInstallWithGrant || (state.w_replResp && victimSafeToOverwrite))
  // A revalidation must not fire once a Grant-path replRead has secured a
  // replacer result for this context (that result may already own a live
  // ReplaceMSHR child).  This is enforced structurally instead: a successful
  // non-revalidate ReplacerResult clears normalRefillRevalidateNeeded below,
  // so no !state.w_replResp term may appear here -- a hit-at-alloc context is
  // born with w_replResp set and still legitimately needs revalidation.
  val mp_normal_refill_revalidate_valid = req_valid && !cmo_cbo && state.s_refill &&
    state.w_grantlast && state.w_grant && normalRefillRevalidateNeeded &&
    !normalRefillDroppedByToN && !replLookupInFlight && victimSafeToOverwrite &&
    !io.deferGrant && !nestedReleaseDirtyThisCycle
  val mp_normal_refill_commit_valid = req_valid && !cmo_cbo && state.s_refill &&
    state.w_grantlast && state.w_grant && state.w_replResp &&
    !mergedGrantInstalled &&
    victimSafeToOverwrite && !normalRefillCommitIssued &&
    !normalRefillRevalidateNeeded && !replLookupInFlight &&
    !io.deferGrant && !nestedReleaseDirtyThisCycle &&
    (!postGrantReleaseSeen || postGrantReleasePayloadReady) &&
    !io.postGrantReleaseSnoopActive
  val mp_dct_valid = !state.s_dct.getOrElse(true.B) && state.s_probeack
  // CMO metadata is committed by its child release task.  A CMO without a
  // valid old line still uses the existing parent-only compensation path.
  val mp_cmometaw_valid = !state.s_cmometaw && (!cmo_cbo || !sidecarOwnsVictim)
  io.tasks.mainpipe.valid :=
    mp_release_valid ||
      mp_probeack_valid ||
      mp_repl_read_valid ||
      mp_grant_valid ||
      mp_normal_refill_revalidate_valid ||
      mp_normal_refill_commit_valid ||
      mp_cbwrdata_valid ||
      mp_dct_valid ||
      mp_cmometaw_valid
  assert(!(mp_repl_read_valid && mp_grant_valid), "replacement read and L1-visible grant must be distinct tasks")
  assert(
    !(mp_grant_valid && mp_normal_refill_commit_valid),
    "L1-visible grant and normal refill commit must be distinct tasks"
  )
  assert(
    !(mp_normal_refill_revalidate_valid && mp_normal_refill_commit_valid),
    "normal refill revalidation and commit must be distinct tasks"
  )

  when(!io.alloc.valid) {
    when(nestedReleaseDirtyThisCycle) {
      nestedReleaseRefillPending := true.B
    }.elsewhen(io.tasks.mainpipe.fire && mp_normal_refill_commit_valid) {
      nestedReleaseRefillPending := false.B
    }
  }
  // io.tasks.prefetchTrain.foreach(t => t.valid := !state.s_triggerprefetch.getOrElse(true.B))

  assert(state.s_refill || state.s_cmoresp, "refill not allowed on CMO operation")

  // replReadTaskPending also covers the post-Grant internal replRead phase
  // (grantIssued).  There grantTaskPending stays low forever, so gating the
  // backoff timer on it would freeze backoffTimer at zero once retryTimes
  // reaches the threshold, permanently revoking grantTaskAllowed and
  // deadlocking a Grant-issued context whose replacer read was retried.
  when(
    replReadTaskPending &&
      backoffTimer < backoffCycles.U &&
      retryTimes === backoffThreshold.U
  ) {
    backoffTimer := backoffTimer + 1.U
  }

  // resp and fwdState
  // *NOTICE: Snp*Fwd would enter MSHR on directory missing
  val respCacheState = ParallelPriorityMux(
    Seq(
      (snpToN || tagErr) -> I,
      snpToB -> Mux(req.snpHitReleaseToInval, I, SC),
      isSnpOnceX(req_chiOpcode) ->
        Mux(
          req.snpHitReleaseToInval,
          I,
          Mux(
            req.snpHitReleaseToClean,
            Mux(req.snpHitReleaseMeta.dirty, SC, metaChi),
            Mux(meta.dirty, UD, metaChi)
          )
        ),
      (isSnpStashX(req_chiOpcode) || isSnpQuery(req_chiOpcode)) ->
        Mux(meta.dirty, UD, metaChi),
      isSnpCleanShared(req_chiOpcode) ->
        Mux(isT(meta.state), UC, metaChi)
    )
  )
  val respPassDirty = hitDirtyOrWriteDirty && !tagErr && (
    snpToB ||
      req_chiOpcode === SnpUnique ||
      req_chiOpcode === SnpUniqueStash ||
      req_chiOpcode === SnpCleanShared ||
      req_chiOpcode === SnpCleanInvalid ||
      req_chiOpcode === SnpPreferUnique
  ) || hitWriteDirty && isSnpOnceFwd(req_chiOpcode)
  val fwdCacheState = Mux(
    tagErr,
    I,
    Mux(
      isSnpToBFwd(req_chiOpcode),
      SC,
      Mux(isSnpToNFwd(req_chiOpcode), UC /*UC_UD*/, I)
    )
  )
  val fwdPassDirty = isSnpToNFwd(req_chiOpcode) && hitDirtyOrWriteDirty && !tagErr

  /*TXRSP for CompAck */
  val orsp = io.tasks.txrsp.bits
  orsp := 0.U.asTypeOf(io.tasks.txrsp.bits.cloneType)
  orsp.tgtID := Mux(wcompack_valid, tgtid_wcompack, tgtid_rcompack)
  orsp.srcID := 0.U
  orsp.txnID := Mux(wcompack_valid, txnid_wcompack, txnid_rcompack)
  orsp.dbID := 0.U
  orsp.opcode := CompAck
  orsp.resp := 0.U
  orsp.fwdState := 0.U
  orsp.traceTag := req.traceTag.get

  /*TXREQ for Transaction Request*/
  // *NOTICE: By the time of issuing Write Back (WriteBackFull or Evict), the directory
  //          was already updated by replacing, so we should never check directory hit
  //          on replacer-issued WriteBackFull condition.
  val isWriteCleanFull = req_cboClean
  val isWriteBackFull = !req_cboClean && !req_cboInval && isT(meta.state) && meta.dirty
  val isWriteEvictFull = false.B
  val isWriteEvictOrEvict = afterIssueEbOrElse(
    !req_cboFlush && !req_cboInval && !isWriteCleanFull && !isWriteBackFull && !isWriteEvictFull,
    false.B
  )
  val isEvict = !isWriteCleanFull && !isWriteBackFull && !isWriteEvictFull && !isWriteEvictOrEvict
  val a_task = {
    val oa = io.tasks.txreq.bits
    oa := 0.U.asTypeOf(io.tasks.txreq.bits.cloneType)
    oa.qos := Fill(QOS_WIDTH, 1.U(1.W)) - 1.U // TODO
    oa.tgtID := Mux(!state.s_reissue.getOrElse(false.B), srcid_retryack, 0.U)
    oa.srcID := 0.U
    oa.txnID := io.id
    oa.returnNID := 0.U
    oa.stashNID := 0.U
    oa.stashNIDValid := false.B

    /**
      *           TL                  CHI
      *  --------------------------------------------
      *  Get                  |  ReadNotSharedDirty
      *  AcquireBlock NtoB    |  ReadNotSharedDirty
      *  AcquireBlock NtoT    |  ReadUnique
      *  AcquirePerm hit      |  MakeUnique
      *  AcquirePerm miss     |  ReadUnique
      *  PrefetchRead         |  ReadNotSharedDirty
      *  PrefetchWrite        |  ReadUnique
      */
    oa.opcode := ParallelPriorityMux(
      Seq(
        release_valid2 -> req_released_chiOpcode,
        req_cboClean -> CleanShared,
        req_cboFlush -> CleanInvalid,
        req_cboInval -> MakeInvalid,
        // MakeUnique is data-less. It is valid only when this L2 already owns
        // the line; after an AcquirePerm miss the requester may retain a clean
        // copy while DataStorage has no matching data. Fetch the line with
        // ReadUnique so a later clean ProbeAck is backed by valid L2 data.
        (req_acquirePerm && dirResult.hit) -> MakeUnique,
        req_needT -> ReadUnique,
        req_needB /* Default */ -> ReadNotSharedDirty
      )
    )
    oa.size := log2Ceil(blockBytes).U
    oa.addr := Cat(Mux(release_valid2, dirResult.tag, req.tag), req.set, 0.U(offsetBits.W))
    oa.ns := enableNS.B
    // set 'LikelyShared' to 1 here when:
    //  - WriteEvictOrEvict (on retry) with SC state
    oa.likelyshared := afterIssueEbOrElse(
      Mux(release_valid2, req_released_likelyShared, false.B),
      false.B
    )
    oa.allowRetry := state.s_reissue.getOrElse(false.B)
    oa.order := OrderEncodings.None
    oa.pCrdType := Mux(!state.s_reissue.getOrElse(false.B), pcrdtype, 0.U)
    // set 'ExpCompAck' to 1 here when:
    //  - MakeUnique
    //  - ReadUnique, ReadNotSharedDirty
    //  - WriteEvictOrEvict (on retry)
    oa.expCompAck := Mux(
      release_valid2,
      afterIssueEbOrElse(req_released_chiOpcode === WriteEvictOrEvict, false.B),
      !cmo_cbo
    )
    oa.memAttr := MemAttr(
      cacheable = true.B,
      allocate = !release_valid2 || !isEvict && !cmo_cbo,
      device = false.B,
      ewa = true.B
    )
    oa.snpAttr := true.B
    oa.lpIDWithPadding := 0.U
    oa.excl := false.B
    oa.snoopMe := false.B
    oa.traceTag := false.B
    oa.mpam.foreach(_ := MPAM(oa.ns))
    oa
  }

  val b_task = {
    val ob = io.tasks.source_b.bits
    ob.tag := dirResult.tag
    ob.set := dirResult.set
    ob.off := 0.U
    ob.opcode := Probe
    ob.param := Mux(
      !state.s_pprobe,
      Mux(
        snpToB,
        toB,
        Mux(snpToN, toN, toT)
      ),
      Mux(
        // *NOTICE: CBOClean derives upper Probe toB for now.
        (req_get || req_cboClean) && dirResult.hit && meta.state === TRUNK,
        toB,
        toN
      )
    )
    ob.alias.foreach(_ := meta.alias.getOrElse(0.U))
    ob
  }

  val mp_release, mp_probeack, mp_repl_read, mp_grant, mp_normal_refill_revalidate, mp_normal_refill_commit,
    mp_cbwrdata, mp_dct, mp_cmometaw =
    WireInit(0.U.asTypeOf(new TaskBundle))
  val mp_release_task = {
    mp_release.channel := req.channel
    mp_release.txChannel := CHIChannel.TXREQ
    mp_release.tag := dirResult.tag
    mp_release.set := req.set
    mp_release.off := 0.U
    mp_release.alias.foreach(_ := 0.U)
    mp_release.vaddr.foreach(_ := 0.U)
    mp_release.isKeyword.foreach(_ := false.B)
    // if dirty, we must ReleaseData
    // if accessed, we ReleaseData to keep the data in L3, for future access to be faster
    // [Access] TODO: consider use a counter
    mp_release.opcode := 0.U // use chiOpcode
    mp_release.param := Mux(isT(meta.state), TtoN, BtoN)
    mp_release.size := log2Ceil(blockBytes).U
    mp_release.sourceId := 0.U(sourceIdBits.W)
    mp_release.bufIdx := 0.U(bufIdxBits.W)
    mp_release.needProbeAckData := false.B
    mp_release.mshrTask := true.B
    mp_release.mshrId := io.id
    mp_release.aliasTask.foreach(_ := false.B)
    // mp_release definitely read releaseBuf and refillBuf at ReqArb
    // and it needs to write refillData to DS, so useProbeData is set false according to DS.wdata logic
    // * but on CMO requests, data were not fetched by the refill procedure, but written to releaseBuf
    //   by mainpipe, so useProbeData is set to true to write data from releaseBuf into DS
    mp_release.useProbeData := false.B
    mp_release.readProbeDataDown := false.B
    mp_release.mshrRetry := false.B
    mp_release.way := dirResult.way
    mp_release.fromL2pft.foreach(_ := false.B)
    mp_release.needHint.foreach(_ := false.B)
    mp_release.dirty := false.B // meta.dirty && meta.state =/= INVALID || probeDirty
    mp_release.metaWen := false.B
    mp_release.meta := MetaEntry()
    mp_release.tagWen := false.B
    // write refillData to DS on refill, write releaseData to DS on CMO
    // When refillBuf has no valid data, it should be avoided to write data of RefillBuf to DS which is MCP2
    mp_release.dsWen := !req_acquirePerm
    mp_release.replTask := true.B
    mp_release.cmoTask := cmo_cbo
    mp_release.wayMask := 0.U(cacheParams.ways.W)
    mp_release.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_release.mergeA := false.B
    mp_release.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    mp_release.denied := denied
    mp_release.corrupt := corrupt

    // CHI
    // *NOTICE: See 'isWriteBackFull' above.
    mp_release.tgtID.get := 0.U
    mp_release.srcID.get := 0.U
    mp_release.txnID.get := io.id
    mp_release.homeNID.get := 0.U
    mp_release.dbID.get := 0.U
    mp_release.chiOpcode.get := ParallelPriorityMux(
      Seq(
        isWriteBackFull -> WriteBackFull,
        isWriteEvictFull -> WriteEvictFull,
        isWriteEvictOrEvict -> afterIssueEbOrElse(WriteEvictOrEvict, DontCare),
        isEvict /* Default */ -> Evict
      )
    )
    mp_release.resp.get := 0.U // DontCare
    mp_release.fwdState.get := 0.U // DontCare
    mp_release.pCrdType.get := 0.U // DontCare // TODO: consider retry of WriteBackFull/Evict
    mp_release.retToSrc.get := req.retToSrc.get
    mp_release.likelyshared.get := Mux(isWriteEvictOrEvict, meta.state === BRANCH, false.B)
    mp_release.expCompAck.get := isWriteEvictOrEvict
    mp_release.allowRetry.get := state.s_reissue.getOrElse(false.B)
    mp_release.memAttr.get := MemAttr(allocate = !isEvict, cacheable = true.B, device = false.B, ewa = true.B)

    // CMO
    when(cmo_cbo) {
      mp_release.useProbeData := true.B
      mp_release.readProbeDataDown := ParallelPriorityMux(
        Seq(
          req_cboClean -> true.B,
          req_cboFlush -> isWriteBackFull,
          req_cboInval -> false.B
        )
      )
      mp_release.param := ParallelPriorityMux(
        Seq(
          req_cboClean -> TtoB,
          req_cboFlush -> Mux(isT(meta.state), TtoN, BtoN),
          req_cboInval -> Mux(isT(meta.state), TtoN, BtoN)
        )
      )
      mp_release.meta := Mux(req_cboClean, meta, MetaEntry())
      mp_release.meta.dirty := false.B
      mp_release.meta.state := Mux(
        req_cboClean,
        // *NOTICE: CBOClean derives upper Probe toB for now,
        //          so TRUNK should be turned into TIP.
        //
        //          ** IMPORTANT **
        //          For operations that require subsequent Release, derived upper Probes
        //          must be set to 'toB' to simplify and correct Release nesting mechanism.
        Mux(meta.state === TRUNK, TIP, meta.state),
        INVALID
      )
      mp_release.metaWen := true.B
      mp_release.dsWen := probeDirty
      mp_release.replTask := false.B
      mp_release.chiOpcode.get := ParallelPriorityMux(
        Seq(
          req_cboClean -> WriteCleanFull,
          req_cboFlush -> Mux(isWriteBackFull, WriteBackFull, Evict),
          req_cboInval -> Evict
        )
      )
      mp_release.likelyshared.get := false.B
      mp_release.memAttr.get := MemAttr(allocate = false.B, cacheable = true.B, device = false.B, ewa = true.B)
    }

    mp_release
  }

  val mp_cbwrdata_task = {
    mp_cbwrdata.channel := req.channel
    mp_cbwrdata.txChannel := CHIChannel.TXDAT
    mp_cbwrdata.tag := dirResult.tag
    mp_cbwrdata.set := req.set
    mp_cbwrdata.off := 0.U
    mp_cbwrdata.alias.foreach(_ := 0.U)
    mp_cbwrdata.vaddr.foreach(_ := 0.U)
    mp_cbwrdata.isKeyword.foreach(_ := false.B)
    mp_cbwrdata.opcode := 0.U
    mp_cbwrdata.param := 0.U
    mp_cbwrdata.size := log2Ceil(blockBytes).U
    mp_cbwrdata.sourceId := 0.U(sourceIdBits.W)
    mp_cbwrdata.bufIdx := 0.U(bufIdxBits.W)
    mp_cbwrdata.needProbeAckData := false.B
    mp_cbwrdata.mshrTask := true.B
    mp_cbwrdata.mshrId := io.id
    mp_cbwrdata.aliasTask.foreach(_ := false.B)
    mp_cbwrdata.useProbeData := false.B // DontCare
    mp_cbwrdata.readProbeDataDown := true.B
    mp_cbwrdata.mshrRetry := false.B
    mp_cbwrdata.way := dirResult.way
    mp_cbwrdata.fromL2pft.foreach(_ := false.B)
    mp_cbwrdata.needHint.foreach(_ := false.B)
    mp_cbwrdata.dirty := false.B // DontCare
    mp_cbwrdata.metaWen := false.B
    mp_cbwrdata.meta := MetaEntry()
    mp_cbwrdata.tagWen := false.B
    mp_cbwrdata.dsWen := false.B
    mp_cbwrdata.replTask := false.B
    mp_cbwrdata.cmoTask := cmo_cbo
    mp_cbwrdata.wayMask := 0.U
    mp_cbwrdata.reqSource := 0.U
    mp_cbwrdata.mergeA := false.B
    mp_cbwrdata.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    mp_cbwrdata.denied := denied
    mp_cbwrdata.corrupt := corrupt

    // CHI
    mp_cbwrdata.tgtID.get := tgtid_wcompack
    mp_cbwrdata.srcID.get := 0.U
    mp_cbwrdata.txnID.get := txnid_wcompack
    mp_cbwrdata.homeNID.get := 0.U
    mp_cbwrdata.dbID.get := 0.U
    mp_cbwrdata.chiOpcode.get := CopyBackWrData
    mp_cbwrdata.resp.get := setPD(metaChi, meta.dirty)
    mp_cbwrdata.fwdState.get := 0.U
    mp_cbwrdata.pCrdType.get := 0.U // TODO
    mp_cbwrdata.retToSrc.get := req.retToSrc.get // DontCare
    mp_cbwrdata.likelyshared.get := false.B
    mp_cbwrdata.expCompAck.get := false.B
    mp_cbwrdata.traceTag.get := cbWrDataTraceTag
    mp_cbwrdata
  }

  val mp_probeack_task = {
    mp_probeack.channel := req.channel
    mp_probeack.txChannel := Mux(doRespData, CHIChannel.TXDAT, CHIChannel.TXRSP)
    mp_probeack.tag := req.tag
    mp_probeack.set := req.set
    mp_probeack.off := req.off
    mp_probeack.alias.foreach(_ := 0.U)
    mp_probeack.vaddr.foreach(_ := 0.U)
    mp_probeack.isKeyword.foreach(_ := false.B)
    mp_probeack.opcode := 0.U
    mp_probeack.param := DontCare
    mp_probeack.size := log2Ceil(blockBytes).U
    mp_probeack.sourceId := 0.U(sourceIdBits.W)
    mp_probeack.bufIdx := 0.U(bufIdxBits.W)
    mp_probeack.needProbeAckData := false.B
    mp_probeack.mshrTask := true.B
    mp_probeack.mshrId := io.id
    mp_probeack.aliasTask.foreach(_ := false.B)
    // Get-on-TRUNK / cache-alias ProbeAckData is captured into this slot's
    // RefillBuf (the normal context never touches ReleaseBuf), so the probeack
    // task reads RefillBuf and writes DS from it.
    mp_probeack.useProbeData := false.B
    mp_probeack.readProbeDataDown := false.B
    mp_probeack.readRefillData := true.B
    mp_probeack.mshrRetry := false.B
    mp_probeack.way := dirResult.way
    mp_probeack.fromL2pft.foreach(_ := false.B)
    mp_probeack.needHint.foreach(_ := false.B)
    mp_probeack.dirty := hitDirty
    mp_probeack.meta := MetaEntry(
      /**
        * Under what circumstances should the dirty bit be cleared:
        * 1. If the snoop belongs to SnpToN
        * 2. If the snoop belongs to SnpToB
        * 3. If the snoop is SnpCleanShared
        * 4. If the snoop is SnpOnce/SnpOnceFwd and nesting WriteCleanFull
        * 5. If the snoop encounters tagErr
        * Otherwise, the dirty bit should stay the same as before.
        */
      dirty = !tagErr && (!(
        !dirResult.hit || !meta.dirty ||
          snpToN ||
          snpToB ||
          isSnpCleanShared(req_chiOpcode) ||
          isSnpOnceX(req_chiOpcode) && req.snpHitReleaseToClean
      ) || isSnpOnceX(req_chiOpcode) && probeDirty),
      state = Mux(
        snpToN || tagErr,
        INVALID,
        Mux(
          // On SnpOnceFwd nesting WriteCleanFull with UD, we went UD -> SC (T -> B here)
          snpToB || isSnpOnceFwd(req_chiOpcode) && hitWriteClean,
          BRANCH,
          meta.state
        )
      ),
      clients = meta.clients & Fill(clientBits, !snpToN),
      alias = meta.alias, // [Alias] Keep alias bits unchanged
      prefetch = !snpToN && meta_pft,
      accessed = !snpToN && meta.accessed
    )
    mp_probeack.metaWen := !req.snpHitReleaseToInval
    mp_probeack.tagWen := false.B
    mp_probeack.dsWen := !(snpToN || tagErr) && probeDirty && !releaseDirty
    mp_probeack.wayMask := 0.U(cacheParams.ways.W)
    mp_probeack.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_probeack.replTask := false.B
    mp_probeack.cmoTask := cmo_cbo
    mp_probeack.mergeA := false.B
    mp_probeack.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_probeack.tgtID.get := req.srcID.get
    mp_probeack.srcID.get := 0.U
    mp_probeack.txnID.get := req.txnID.get
    mp_probeack.homeNID.get := 0.U
    // For SnpRespData or SnpRespData, DBID is set to the same value as the TxnID of the snoop.
    // For SnpRespDataFwded or SnpRespDataFwded, DBID is not defined and can be any value.
    mp_probeack.dbID.get := req.txnID.getOrElse(0.U)
    mp_probeack.chiOpcode.get := MuxLookup(
      Cat(doFwd || doFwdHitRelease, doRespData),
      SnpResp
    )(
      Seq(
        Cat(false.B, false.B) -> SnpResp,
        Cat(true.B, false.B) -> SnpRespFwded,
        Cat(false.B, true.B) -> SnpRespData, // ignore SnpRespDataPtl for now
        Cat(true.B, true.B) -> SnpRespDataFwded
      )
    )
    mp_probeack.resp.get := setPD(respCacheState, respPassDirty)
    mp_probeack.fwdState.get := setPD(fwdCacheState, fwdPassDirty)
    mp_probeack.pCrdType.get := 0.U
    mp_probeack.retToSrc.get := req.retToSrc.get // DontCare
    mp_probeack.likelyshared.get := false.B
    mp_probeack.expCompAck.get := false.B
    mp_probeack.traceTag.get := req.traceTag.get
    mp_probeack.snpHitRelease := req.snpHitRelease
    mp_probeack.snpHitReleaseToInval := req.snpHitReleaseToInval
    mp_probeack.snpHitReleaseToClean := req.snpHitReleaseToClean
    mp_probeack.snpHitReleaseWithData := req.snpHitReleaseWithData
    mp_probeack.snpHitReleaseIdx := req.snpHitReleaseIdx

    mp_probeack
  }

  val mergeA = RegInit(false.B)
  when(io.aMergeTask.valid) {
    mergeA := true.B
  }.elsewhen(io.alloc.valid) {
    mergeA := false.B
  }
  // T-intent accumulated over merged A requests (merge_task only latches the
  // last one).  A merged request granted TRUNK hands the client a writable
  // copy; the install must then record the client, or ReplaceMSHR's victim
  // probe (needProbe = clients.orR) is skipped and a late dirty writeback
  // from that client is evicted without data and silently lost.
  val mergeAneedT = RegInit(false.B)
  when(io.aMergeTask.valid) {
    mergeAneedT := mergeAneedT || needT(io.aMergeTask.bits.opcode, io.aMergeTask.bits.param)
  }.elsewhen(io.alloc.valid) {
    mergeAneedT := false.B
  }
  // Keep client presence and permission level as separate latched semantics:
  // an NtoB merge still installs a client copy, while mergeAneedT only says
  // whether that copy has write permission. Both Grant and refill commit use
  // these same values instead of re-deriving them independently.
  val mergedClientValid = RegInit(false.B)
  when(io.aMergeTask.valid) {
    mergedClientValid := true.B
  }.elsewhen(io.alloc.valid) {
    mergedClientValid := false.B
  }
  // Live merge alias until Grant snapshots it. Prefetch itself has alias=0;
  // the later Acquire's color is what L1 installs and what Probe must use.
  val mergedAlias = aliasBitsOpt.map(_ => RegInit(0.U(aliasBitsOpt.get.W)))
  when(io.aMergeTask.valid) {
    mergedAlias.foreach(_ := io.aMergeTask.bits.alias.getOrElse(0.U))
  }.elsewhen(io.alloc.valid) {
    mergedAlias.foreach(_ := 0.U)
  }
  val mergedGrantNeedsT = mergedClientValid && (mergeAneedT || req_promoteT)
  val mergedGrantClients = Fill(clientBits, mergedClientValid)
  val mergedGrantState = Mux(mergedGrantNeedsT, TRUNK, BRANCH)
  // A merged prefetch is no longer a prefetch-only ownership state: the
  // merged Acquire determines the state installed for the L1 client.
  val grantMetaState = Mux(
    mergedClientValid,
    mergedGrantState,
    Mux(req_promoteT || req_needT, Mux(req_prefetch, TIP, TRUNK), BRANCH)
  )
  val commitMergedClientValid = Mux(
    grantMergeSemanticsLatched,
    grantMergedClientLatched,
    mergedClientValid
  )
  val commitMergedNeedsT = Mux(
    grantMergeSemanticsLatched,
    grantMergedNeedsTLatched,
    mergedGrantNeedsT
  )
  val commitMergedGrantClients = Fill(clientBits, commitMergedClientValid)
  val commitGrantMetaState = Mux(
    commitMergedClientValid,
    Mux(commitMergedNeedsT, TRUNK, BRANCH),
    Mux(req_promoteT || req_needT, Mux(req_prefetch, TIP, TRUNK), BRANCH)
  )
  val commitMergedAlias = aliasBitsOpt.map { _ =>
    Mux(grantMergeSemanticsLatched, grantMergedAliasLatched.get, mergedAlias.get)
  }
  // Prefetch/Get keep Directory alias unchanged unless a merged Acquire owns
  // the client copy — then Directory must record that Acquire's alias.
  def aliasFinal(useMergedClient: Bool): UInt = Mux(
    useMergedClient,
    commitMergedAlias.getOrElse(0.U),
    Mux(req_get || req_prefetch, meta.alias.getOrElse(0.U), req.alias.getOrElse(0.U))
  )
  val mp_grant_task = {
    mp_grant.channel := req.channel
    mp_grant.tag := req.tag
    mp_grant.set := req.set
    mp_grant.off := req.off
    mp_grant.sourceId := req.sourceId
    mp_grant.alias.foreach(_ := 0.U)
    mp_grant.vaddr.foreach(_ := 0.U)
    mp_grant.isKeyword.foreach(_ := req.isKeyword.getOrElse(false.B))
    mp_grant.opcode := odOpGen(req.opcode)
    mp_grant.param := Mux(
      req_get || req_prefetch,
      0.U, // Get -> AccessAckData
      MuxLookup( // Acquire -> Grant
        req.param,
        req.param
      )(
        Seq(
          NtoB -> Mux(req_promoteT, toT, toB),
          BtoT -> toT,
          NtoT -> toT
        )
      )
    )
    mp_grant.size := 0.U(msgSizeBits.W)
    mp_grant.bufIdx := 0.U(bufIdxBits.W)
    mp_grant.needProbeAckData := false.B
    mp_grant.denied := denied
    mp_grant.corrupt := corrupt
    mp_grant.mshrTask := true.B
    mp_grant.mshrId := io.id
    mp_grant.refillOnly := false.B
    mp_grant.deferRefillWrite := !cmo_cbo
    mp_grant.normalRefillRevalidate := false.B
    mp_grant.way := dirResult.way
    // Get/Prefetch keep Directory alias unchanged unless mergeA installs a
    // client copy; then Directory must match the Acquire GrantBuffer uses.
    val grantAliasFinal = aliasFinal(mergedClientValid)
    mp_grant.alias.foreach(_ := grantAliasFinal)
    mp_grant.aliasTask.foreach(_ := req.aliasTask.getOrElse(false.B))
    // The normal MSHR snapshots hit data in its RefillBuf when it is
    // allocated. Later ProbeAckData overwrites that snapshot if it carries
    // newer data. ReleaseBuf remains reserved for ReplaceMSHR/SnoopMSHR.
    mp_grant.useProbeData := false.B
    mp_grant.readProbeDataDown := false.B
    mp_grant.readRefillData := dirResult.hit && !gotGrantData && !probeDirty && mp_grant.opcode(0)
    mp_grant.dirty := false.B

    mp_grant.meta := MetaEntry(
      dirty = gotDirty || dirResult.hit && meta.dirty,
      state = Mux(
        req_get,
        Mux( // Get
          dirResult.hit,
          Mux(isT(meta.state), TIP, BRANCH),
          Mux(req_promoteT, TIP, BRANCH)
        ),
        grantMetaState
      ),
      // A merged Acquire always creates an L1 client copy. mergeAneedT and
      // req_promoteT only determine whether that copy is TRUNK or BRANCH;
      // clients must remain recorded for both permission levels so a later
      // Snoop can issue the required Probe.
      clients = Mux(
        req_prefetch,
        Mux(dirResult.hit, meta.clients,
          mergedGrantClients),
        Fill(clientBits, !(req_get && (!dirResult.hit || meta_no_client)))
      ),
      alias = Some(grantAliasFinal),
      prefetch = req_prefetch || dirResult.hit && meta_pft,
      pfsrc = PfSource.fromMemReqSource(req.reqSource),
      accessed = req_acquire || req_get,
      cdpPfDepth = req.cdpPfDepth.getOrElse(0.U)
    )
    // This task may carry a replacement read, but it is always SourceD-only.
    // The normal refill commit owns all DS and Directory side effects.
    mp_grant.metaWen := false.B
    mp_grant.tagWen := false.B
    mp_grant.dsWen := false.B
    // Request-side pre-filter for the merge-install: corner cases that the
    // deferred commit handles specially (denied retry, CMO, merged Acquire,
    // a pre-grant nested ReleaseData) keep the split path.  The victim-side
    // decision is made at MainPipe S3 from the returning ReplacerResult.
    mp_grant.grantInstallEn := !cmo_cbo && !mergeA && !denied && !nestedReleaseRefillPending
    mp_grant.fromL2pft.foreach(_ := req.fromL2pft.get)
    mp_grant.needHint.foreach(_ := false.B)
    mp_grant.replTask := grantNeedsReplRead
    mp_grant.cmoTask := cmo_cbo
    mp_grant.wayMask := 0.U(cacheParams.ways.W)
    mp_grant.mshrRetry := !state.s_retry
    mp_grant.reqSource := req.reqSource

    // Add merge grant task for Acquire and late Prefetch
    mp_grant.mergeA := mergeA

    val merge_task = RegEnable(io.aMergeTask.bits, 0.U.asTypeOf(new TaskBundle), io.aMergeTask.valid)

    mp_grant.aMergeTask.off := merge_task.off
    mp_grant.aMergeTask.alias.foreach(_ := merge_task.alias.getOrElse(0.U))
    mp_grant.aMergeTask.vaddr.foreach(_ := merge_task.vaddr.getOrElse(0.U))
    mp_grant.aMergeTask.isKeyword.foreach(_ := merge_task.isKeyword.getOrElse(false.B))
    mp_grant.aMergeTask.opcode := odOpGen(merge_task.opcode)
    mp_grant.aMergeTask.param := MuxLookup( // Acquire -> Grant
      merge_task.param,
      merge_task.param
    )(
      Seq(
        NtoB -> Mux(req_promoteT, toT, toB),
        BtoT -> toT,
        NtoT -> toT
      )
    )
    mp_grant.aMergeTask.sourceId := merge_task.sourceId
    mp_grant.aMergeTask.meta := MetaEntry(
      dirty = gotDirty || dirResult.hit && meta.dirty,
      state = mergedGrantState,
      clients = mergedGrantClients,
      alias = Some(merge_task.alias.getOrElse(0.U)),
      prefetch = false.B,
      accessed = true.B
    )
    mp_grant.aMergeTask.pc.foreach(_ := merge_task.pc.getOrElse(0.U))

    mp_grant
  }

  // A snoop can temporarily block the L1 response while the normal MSHR still
  // needs to query Directory's replacer. Keep this task internal: it only
  // produces the ReplacerResult and cannot expose or install a refill.
  val mp_repl_read_task = {
    mp_repl_read := mp_grant
    mp_repl_read.refillOnly := true.B
    mp_repl_read.deferRefillWrite := true.B
    mp_repl_read.metaWen := false.B
    mp_repl_read.tagWen := false.B
    mp_repl_read.dsWen := false.B
    mp_repl_read.readRefillData := false.B
    mp_repl_read.replTask := true.B
    mp_repl_read
  }

  // This task has the RequestArb/Directory replacement-read shape, but it
  // never exposes data or writes L2. Directory returns either the still-live
  // hit way or a fresh replacement candidate to this normal MSHR.
  val mp_normal_refill_revalidate_task = {
    mp_normal_refill_revalidate := mp_grant
    mp_normal_refill_revalidate.refillOnly := true.B
    mp_normal_refill_revalidate.deferRefillWrite := true.B
    mp_normal_refill_revalidate.normalRefillRevalidate := true.B
    mp_normal_refill_revalidate.metaWen := false.B
    mp_normal_refill_revalidate.tagWen := false.B
    mp_normal_refill_revalidate.dsWen := false.B
    mp_normal_refill_revalidate.readRefillData := false.B
    mp_normal_refill_revalidate.replTask := true.B
    mp_normal_refill_revalidate
  }
  when(io.tasks.mainpipe.fire && mp_normal_refill_revalidate_valid) {
    assert(
      mp_normal_refill_revalidate.normalRefillRevalidate &&
        mp_normal_refill_revalidate.refillOnly && mp_normal_refill_revalidate.deferRefillWrite &&
        mp_normal_refill_revalidate.replTask && !mp_normal_refill_revalidate.dsWen &&
        !mp_normal_refill_revalidate.tagWen && !mp_normal_refill_revalidate.metaWen,
      "normal refill revalidation must be Directory-only"
    )
  }

  // This remains a normal-MSHR task even when a ReplaceMSHR owns the victim.
  // Its sole data source is RefillBuf; ReplaceMSHR owns ReleaseBuf and L3
  // writeback. `victimSafeToOverwrite` gates issuance above.
  val mp_normal_refill_commit_task = {
    // Must match GrantBuffer's mergeAtask.alias when mergeA installed a client.
    // Clearing mergeA below is fine (this task is Directory/DS only), but the
    // written meta.alias cannot remain the prefetch's stale/zero color.
    val commitAliasFinal = aliasFinal(commitMergedClientValid)
    mp_normal_refill_commit.channel := req.channel
    mp_normal_refill_commit.tag := req.tag
    mp_normal_refill_commit.set := req.set
    mp_normal_refill_commit.off := req.off
    mp_normal_refill_commit.alias.foreach(_ := commitAliasFinal)
    mp_normal_refill_commit.vaddr.foreach(_ := 0.U)
    mp_normal_refill_commit.isKeyword.foreach(_ := false.B)
    mp_normal_refill_commit.opcode := odOpGen(req.opcode)
    mp_normal_refill_commit.param := Mux(
      req_get || req_prefetch,
      0.U,
      MuxLookup(req.param, req.param)(
        Seq(
          NtoB -> Mux(req_promoteT, toT, toB),
          BtoT -> toT,
          NtoT -> toT
        )
      )
    )
    mp_normal_refill_commit.size := 0.U(msgSizeBits.W)
    mp_normal_refill_commit.sourceId := req.sourceId
    mp_normal_refill_commit.bufIdx := 0.U(bufIdxBits.W)
    mp_normal_refill_commit.needProbeAckData := false.B
    mp_normal_refill_commit.denied := denied
    mp_normal_refill_commit.corrupt := corrupt
    mp_normal_refill_commit.mshrTask := true.B
    mp_normal_refill_commit.mshrId := io.id
    mp_normal_refill_commit.mshrContext := 0.U
    mp_normal_refill_commit.replaceTask := false.B
    mp_normal_refill_commit.refillOnly := true.B
    mp_normal_refill_commit.deferRefillWrite := false.B
    mp_normal_refill_commit.normalRefillRevalidate := false.B
    mp_normal_refill_commit.aliasTask.foreach(_ := false.B)
    mp_normal_refill_commit.useProbeData := false.B
    mp_normal_refill_commit.readProbeDataDown := false.B
    mp_normal_refill_commit.readRefillData := true.B
    mp_normal_refill_commit.mshrRetry := false.B
    mp_normal_refill_commit.dirty := false.B
    mp_normal_refill_commit.way := dirResult.way
    mp_normal_refill_commit.meta := MetaEntry(
      // Revalidation can replace dirResult.meta with an older clean Directory
      // snapshot after a local ProbeAckData. Preserve that payload's dirty
      // ownership until the refill commit makes it visible to Directory.
      dirty = gotDirty || probeDirty || dirResult.hit && meta.dirty,
      state = Mux(
        req_get,
        Mux(dirResult.hit, Mux(isT(meta.state), TIP, BRANCH), Mux(req_promoteT, TIP, BRANCH)),
        commitGrantMetaState
      ),
      // Keep the same merged-client interpretation used by the Grant task.
      // BRANCH copies also need clients=1 so subsequent Snoop traffic probes
      // the L1 rather than taking a no-client direct path.
      clients = Mux(
        req_prefetch,
        Mux(dirResult.hit, meta.clients,
          commitMergedGrantClients),
        Fill(clientBits, !(req_get && (!dirResult.hit || meta_no_client)))
      ),
      alias = Some(commitAliasFinal),
      prefetch = req_prefetch || dirResult.hit && meta_pft,
      pfsrc = PfSource.fromMemReqSource(req.reqSource),
      accessed = req_acquire || req_get
    )
    when(nestedReleaseRefillPending) {
      mp_normal_refill_commit.meta.dirty := true.B
      mp_normal_refill_commit.meta.state := TIP
      mp_normal_refill_commit.meta.clients := Fill(clientBits, false.B)
    }
    when(postGrantReleaseSnoopToB) {
      mp_normal_refill_commit.meta.dirty := false.B
      mp_normal_refill_commit.meta.state := BRANCH
      mp_normal_refill_commit.meta.clients := Fill(clientBits, false.B)
    }.elsewhen(postGrantReleaseSnoopToClean) {
      mp_normal_refill_commit.meta.dirty := false.B
      mp_normal_refill_commit.meta.clients := Fill(clientBits, false.B)
    }
    // A nested clean Release(TtoN) on the request line removes the only client
    // copy: install TIP with no client, mirroring the local meta update made
    // when the release was seen.  Without this the deferred commit would
    // resurrect clients=1 and force a useless Probe on every later victim
    // selection of this line.
    when(postGrantCleanReleaseSeen) {
      mp_normal_refill_commit.meta.state := TIP
      mp_normal_refill_commit.meta.clients := Fill(clientBits, false.B)
    }
    val releaseDataAlreadyCommitted = nestedReleaseRefillPending && releaseDataDirectWriteSeen
    mp_normal_refill_commit.metaWen := !denied && !releaseDataAlreadyCommitted
    mp_normal_refill_commit.tagWen := !dirResult.hit && !denied && !releaseDataAlreadyCommitted
    mp_normal_refill_commit.dsWen :=
      (gotGrantData || probeDirty && (req_get || req.aliasTask.getOrElse(false.B)) ||
        nestedReleaseRefillPending) && !denied && !releaseDataAlreadyCommitted
    mp_normal_refill_commit.fromL2pft.foreach(_ := false.B)
    mp_normal_refill_commit.needHint.foreach(_ := false.B)
    mp_normal_refill_commit.replTask := false.B
    mp_normal_refill_commit.cmoTask := false.B
    mp_normal_refill_commit.cmoAll := false.B
    mp_normal_refill_commit.wayMask := 0.U(cacheParams.ways.W)
    mp_normal_refill_commit.reqSource := MemReqSource.NoWhere.id.U
    mp_normal_refill_commit.mergeA := false.B
    mp_normal_refill_commit.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    // The grant pass already installed Directory tag/meta (merged install);
    // this commit only owes the RefillBuf->DS write.  The DS write itself is
    // unchanged: the victim DS data was read out before the grant's S3, so
    // overwriting the location afterwards is safe.
    when(grantInstallDoneReg) {
      mp_normal_refill_commit.tagWen := false.B
      mp_normal_refill_commit.metaWen := false.B
    }
    when(normalRefillDroppedByToN || postGrantReleaseSnoopConsumed) {
      mp_normal_refill_commit.metaWen := false.B
      mp_normal_refill_commit.tagWen := false.B
      mp_normal_refill_commit.dsWen := false.B
      mp_normal_refill_commit.readRefillData := false.B
    }
    mp_normal_refill_commit
  }

  // Merge mode: the grant carries the refill install.  Write enables and the
  // install meta are exactly the commit's (the pairing overrides in that meta
  // are all post-grant conditions and therefore zero at the merged pass;
  // nestedReleaseRefillPending from a pre-grant ReleaseData is still honored).
  val releaseDataAlreadyCommitted = nestedReleaseRefillPending && releaseDataDirectWriteSeen
  when(mergeInstallWithGrant) {
    mp_grant.tagWen := !dirResult.hit && !denied && !releaseDataAlreadyCommitted
    mp_grant.metaWen := !denied && !releaseDataAlreadyCommitted
    mp_grant.dsWen :=
      (gotGrantData || probeDirty && (req_get || req.aliasTask.getOrElse(false.B)) ||
        nestedReleaseRefillPending) && !denied && !releaseDataAlreadyCommitted
    mp_grant.meta := mp_normal_refill_commit.meta
  }

  when(io.tasks.mainpipe.fire && mp_normal_refill_commit_valid && nestedReleaseRefillPending) {
    assert(
      Mux(
        releaseDataDirectWriteSeen,
        !mp_normal_refill_commit.metaWen && !mp_normal_refill_commit.tagWen && !mp_normal_refill_commit.dsWen,
        mp_normal_refill_commit.meta.dirty && mp_normal_refill_commit.meta.state === TIP &&
          !mp_normal_refill_commit.meta.clients.orR && mp_normal_refill_commit.dsWen
      ),
      "ReleaseData(TtoN) must have exactly one C-path or normal-refill DS/meta commit owner"
    )
  }
  when(io.tasks.mainpipe.fire && mp_normal_refill_commit_valid && probeDirty) {
    assert(normalRefillDroppedByToN || mp_normal_refill_commit.meta.dirty,
      "normal refill commit must preserve dirty ProbeAckData")
  }
  when(io.tasks.mainpipe.fire && mp_grant_valid && !cmo_cbo) {
    assert(
      mergeInstallWithGrant ||
        (!mp_grant.refillOnly && mp_grant.deferRefillWrite &&
          !mp_grant.dsWen && !mp_grant.tagWen && !mp_grant.metaWen),
      "normal L1-visible grant must not update DataStorage or Directory"
    )
  }
  when(io.tasks.mainpipe.fire && mp_grant_valid && !cmo_cbo && mergeInstallWithGrant) {
    assert(
      !mp_grant.refillOnly && mp_grant.deferRefillWrite &&
        (denied || releaseDataAlreadyCommitted ||
          mp_grant.metaWen || mp_grant.tagWen || mp_grant.dsWen),
      "a merge-mode grant must carry the refill install"
    )
  }
  // Both merge forms: the rare pre-registered mode (replResp arrived before
  // the grant fire) and the common S3-combinational merge (folded replacer
  // read returning a no-probe victim in the grant pass itself).
  XSPerfAccumulate("merged_grant_install_cnt",
    (io.tasks.mainpipe.fire && mp_grant_valid && mergeInstallWithGrant) || io.grantInstallDone)
  when(io.tasks.mainpipe.fire && mp_normal_refill_commit_valid) {
    assert(
      mp_normal_refill_commit.refillOnly &&
        !mp_normal_refill_commit.deferRefillWrite &&
        !mp_normal_refill_commit.replaceTask,
      "normal refill commit must be a RefillBuf-only normal-context task"
    )
  }

  val mp_dct_task = {
    mp_dct.channel := req.channel
    mp_dct.txChannel := CHIChannel.TXDAT
    mp_dct.tag := req.tag
    mp_dct.set := req.set
    mp_dct.off := req.off
    mp_dct.alias.foreach(_ := 0.U)
    mp_dct.vaddr.foreach(_ := 0.U)
    mp_dct.isKeyword.foreach(_ := 0.U)
    mp_dct.opcode := 0.U // DontCare
    mp_dct.param := 0.U // DontCare
    mp_dct.size := log2Ceil(blockBytes).U
    mp_dct.sourceId := 0.U(sourceIdBits.W)
    mp_dct.bufIdx := 0.U(sourceIdBits.W)
    mp_dct.needProbeAckData := false.B
    mp_dct.mshrTask := true.B
    mp_dct.mshrId := io.id
    mp_dct.aliasTask.foreach(_ := false.B)
    mp_dct.useProbeData := true.B
    mp_dct.readProbeDataDown := true.B
    mp_dct.mshrRetry := false.B
    mp_dct.way := dirResult.way
    mp_dct.fromL2pft.foreach(_ := false.B)
    mp_dct.needHint.foreach(_ := false.B)
    mp_dct.dirty := hitDirty
    mp_dct.meta := MetaEntry()
    mp_dct.metaWen := false.B // meta is written by SnpResp[Data]Fwded, not CompData
    mp_dct.tagWen := false.B
    mp_dct.dsWen := false.B
    mp_dct.wayMask := 0.U(cacheParams.ways.W)
    mp_dct.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_dct.replTask := false.B
    mp_dct.cmoTask := cmo_cbo
    mp_dct.mergeA := false.B
    mp_dct.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_dct.tgtID.get := req.fwdNID.get
    mp_dct.srcID.get := 0.U
    mp_dct.txnID.get := req.fwdTxnID.get
    mp_dct.homeNID.get := req.srcID.get
    mp_dct.dbID.get := req.txnID.get
    mp_dct.chiOpcode.get := CompData
    mp_dct.resp.get := setPD(fwdCacheState, fwdPassDirty)
    mp_dct.fwdState.get := 0.U
    mp_dct.pCrdType.get := 0.U // DontCare
    mp_dct.retToSrc.get := false.B // DontCare
    mp_dct.likelyshared.get := false.B
    mp_dct.expCompAck.get := false.B // DontCare
    mp_dct.traceTag.get := req.traceTag.get
    mp_dct.snpHitRelease := req.snpHitRelease
    mp_dct.snpHitReleaseToInval := req.snpHitReleaseToInval
    mp_dct.snpHitReleaseToClean := req.snpHitReleaseToClean
    mp_dct.snpHitReleaseWithData := req.snpHitReleaseWithData
    mp_dct.snpHitReleaseIdx := req.snpHitReleaseIdx
    mp_dct.snpHitReleaseMeta := req.snpHitReleaseMeta

    mp_dct
  }

  val mp_cmometaw_task = {
    mp_cmometaw.channel := 0.U
    mp_cmometaw.txChannel := 0.U
    mp_cmometaw.tag := req.tag
    mp_cmometaw.set := req.set
    mp_cmometaw.off := req.off
    mp_cmometaw.alias.foreach(_ := 0.U)
    mp_cmometaw.vaddr.foreach(_ := 0.U)
    mp_cmometaw.isKeyword.foreach(_ := 0.U)
    mp_cmometaw.opcode := 0.U // DontCare
    mp_cmometaw.param := 0.U // DontCare
    mp_cmometaw.size := log2Ceil(blockBytes).U
    mp_cmometaw.sourceId := 0.U(sourceIdBits.W)
    mp_cmometaw.bufIdx := 0.U(sourceIdBits.W)
    mp_cmometaw.needProbeAckData := false.B
    mp_cmometaw.mshrTask := true.B
    mp_cmometaw.mshrId := io.id
    mp_cmometaw.aliasTask.foreach(_ := false.B)
    mp_cmometaw.useProbeData := false.B
    mp_cmometaw.readProbeDataDown := false.B
    mp_cmometaw.mshrRetry := false.B
    mp_cmometaw.way := dirResult.way
    mp_cmometaw.fromL2pft.foreach(_ := false.B)
    mp_cmometaw.needHint.foreach(_ := false.B)
    mp_cmometaw.dirty := hitDirty

    // write meta for compensation of ProbeAck TtoB/TtoN by cbo.clean
    // *NOTICE: There is no possible nest for 'cmometaw' task, snoops should be blocked by RXSNP.
    mp_cmometaw.meta := meta
    mp_cmometaw.meta.clients := meta.clients
    mp_cmometaw.meta.dirty := false.B
    mp_cmometaw.meta.state := TIP // write TIP for compensation of ProbeAck TtoB/TtoN by cbo.clean
    mp_cmometaw.metaWen := true.B

    mp_cmometaw.tagWen := false.B
    mp_cmometaw.dsWen := false.B
    mp_cmometaw.wayMask := 0.U(cacheParams.ways.W)
    mp_cmometaw.reqSource := 0.U(MemReqSource.reqSourceBits.W)
    mp_cmometaw.replTask := false.B
    mp_cmometaw.cmoTask := cmo_cbo
    mp_cmometaw.mergeA := false.B
    mp_cmometaw.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)

    // CHI
    mp_cmometaw.tgtID.get := 0.U
    mp_cmometaw.srcID.get := 0.U
    mp_cmometaw.txnID.get := 0.U
    mp_cmometaw.homeNID.get := 0.U
    mp_cmometaw.dbID.get := 0.U
    mp_cmometaw.chiOpcode.get := 0.U
    mp_cmometaw.resp.get := 0.U
    mp_cmometaw.fwdState.get := 0.U
    mp_cmometaw.pCrdType.get := 0.U // DontCare
    mp_cmometaw.retToSrc.get := false.B // DontCare
    mp_cmometaw.likelyshared.get := false.B
    mp_cmometaw.expCompAck.get := false.B // DontCare
    mp_cmometaw.traceTag.get := 0.U
    mp_cmometaw.snpHitRelease := req.snpHitRelease
    mp_cmometaw.snpHitReleaseToInval := req.snpHitReleaseToInval
    mp_cmometaw.snpHitReleaseToClean := req.snpHitReleaseToClean
    mp_cmometaw.snpHitReleaseWithData := req.snpHitReleaseWithData
    mp_cmometaw.snpHitReleaseIdx := req.snpHitReleaseIdx
    mp_cmometaw.snpHitReleaseMeta := req.snpHitReleaseMeta

    mp_cmometaw
  }

  io.tasks.mainpipe.bits := ParallelPriorityMux(
    Seq(
      mp_repl_read_valid -> mp_repl_read,
      mp_grant_valid -> mp_grant,
      mp_normal_refill_revalidate_valid -> mp_normal_refill_revalidate,
      mp_normal_refill_commit_valid -> mp_normal_refill_commit,
      mp_release_valid -> mp_release,
      mp_cbwrdata_valid -> mp_cbwrdata,
      mp_probeack_valid -> mp_probeack,
      mp_dct_valid -> mp_dct,
      mp_cmometaw_valid -> mp_cmometaw
    )
  )
  io.tasks.mainpipe.bits.reqSource := req.reqSource
  io.tasks.mainpipe.bits.isKeyword.foreach(_ := req.isKeyword.getOrElse(false.B))

  val mp_valid = io.tasks.mainpipe.valid
  val mp = io.tasks.mainpipe.bits

  /* ======== Assertions for common transaction ======== */
  Seq(
    ("CopyBackWrData", CHICohStateTransSet.ofCopyBackWrData(CopyBackWrData)),
    ("CompData", CHICohStateTransSet.ofCompData(CompData)),
    ("SnpResp", CHICohStateTransSet.ofSnpResp(SnpResp)),
    ("SnpRespData", CHICohStateTransSet.ofSnpRespData(SnpRespData)),
    ("SnpRespDataPtl", CHICohStateTransSet.ofSnpRespDataPtl(SnpRespDataPtl)),
    ("NonCopyBackWrData", CHICohStateTransSet.ofNonCopyBackWrData(NonCopyBackWrData)),
    ("WriteDataCancel", CHICohStateTransSet.ofWriteDataCancel(WriteDataCancel))
  ).foreach {
    case (name, set) => {
      assert(
        !mp_valid || CHICohStateTransSet.isValid(set, mp.txChannel, mp.chiOpcode.get, mp.resp.get),
        s"invalid Resp for ${name}"
      )
    }
  }

  ifAfterIssueC {
    Seq(
      ("DataSepResp", CHICohStateTransSet.ofDataSepResp(DataSepResp)),
      ("RespSepData", CHICohStateTransSet.ofRespSepData(RespSepData))
    ).foreach {
      case (name, set) => {
        assert(
          !mp_valid || CHICohStateTransSet.isValid(set, mp.txChannel, mp.chiOpcode.get, mp.resp.get),
          s"invalid Resp for ${name}"
        )
      }
    }
  }

  /* ======== Assertions for DCT forwarded snoop ======== */
  Seq(
    ("SnpRespFwded", CHICohStateFwdedTransSet.ofSnpRespFwded(SnpRespFwded)),
    ("SnpRespDataFwded", CHICohStateFwdedTransSet.ofSnpRespDataFwded(SnpRespDataFwded))
  ).foreach {
    case (name, set) => {
      assert(
        !mp_valid || CHICohStateFwdedTransSet.isValid(
          set,
          mp.txChannel,
          mp.chiOpcode.get,
          mp.resp.get,
          mp.fwdState.get
        ),
        s"invalid combination of Resp and FwdState for ${name}"
      )
    }
  }

  /* ======== Task update ======== */
  when(io.tasks.txreq.fire) {
    // *NOTICE: Two parts of CMO procedure (release, acquire) were all possible to be retried.
    when(!cmo_cbo) {
      assert(!readTxnInFlight, "a normal MSHR must not issue a second CHI read before the first retires")
      assert(
        normalInitialIssue || retryReissue,
        "a normal MSHR may issue a CHI read only initially or after RetryAck + PCrdGrant"
      )
      readTxnInFlight := true.B
    }
    state.s_acquire := Mux(cmo_cbo, state.s_acquire || state.w_releaseack, true.B)
    when(!state.s_reissue.get) {
      state.s_reissue.get := true.B
      gotRetryAck := false.B
      gotPCrdGrant := false.B
    }
  }
  when(io.tasks.txrsp.fire) {
    when(rcompack_valid) {
      state.s_rcompack.get := true.B
    }
    when(wcompack_valid) {
      state.s_wcompack.get := true.B
    }
    assert(!(rcompack_valid && wcompack_valid))
  }
  when(io.tasks.source_b.fire) {
    state.s_pprobe := true.B
    state.s_rprobe := true.B
    sourceBProbeIssued := true.B
  }
  // Advance the MSHR FSM only after MainPipe has actually consumed the task.
  // With normal/snoop/replace contexts sharing the arbiter, `ready` alone does
  // not imply that this MSHR was selected; using it can retire a task that was
  // never issued and leave the context waiting for a response that cannot
  // arrive.
  when(io.tasks.mainpipe.fire) {
    when(mp_repl_read_valid) {
      state.s_retry := true.B
      replReadInFlight := true.B
    }.elsewhen(mp_normal_refill_revalidate_valid) {
      normalRefillRevalidateInFlight := true.B
    }.elsewhen(mp_grant_valid) {
      // L1 may observe the response immediately, but the RefillBuf remains
      // owned by this normal MSHR until the install reaches S3 -- which in
      // merge mode is this very pass, in split mode mp_normal_refill_commit.
      state.s_refill := true.B
      state.s_retry := true.B
      when(mergeInstallWithGrant) {
        // The merged pass is also the install: mark commit issued and hold
        // the way until refillWriteDone (+ way-commit margin) exactly like a
        // deferred commit.  A denied refill installs nothing (no refillWriteDone
        // pulse), and a ReleaseData that already direct-wrote DS/meta has
        // already installed the line: in both cases the commit lifecycle is
        // complete at this fire, so refillWritePending must not be set.
        normalRefillCommitIssued := true.B
        refillWritePending := !(denied || releaseDataAlreadyCommitted)
        mergedGrantInstalled := true.B
      }
      when(mp_grant.replTask) {
        replReadInFlight := true.B
      }
      when(mp_grant.opcode === CBOAck) {
        state.s_cmoresp := true.B
      }
      when(!cmo_cbo) {
        grantMergeSemanticsLatched := true.B
        grantMergedClientLatched := mergedClientValid
        grantMergedNeedsTLatched := mergedGrantNeedsT
        grantMergedAliasLatched.foreach(_ := mergedAlias.getOrElse(0.U))
      }
    }.elsewhen(mp_normal_refill_commit_valid) {
      normalRefillCommitIssued := true.B
      refillWritePending := true.B
    }.elsewhen(mp_release_valid) {
      req_released_chiOpcode := mp_release.chiOpcode.get
      req_released_likelyShared := mp_release.likelyshared.get
      state.s_release := true.B
      state.s_cbwrdata.get := isEvict
      when(isEvict) {
        meta.state := INVALID
        meta.dirty := false.B
      }
    }.elsewhen(mp_cbwrdata_valid) {
      state.s_cbwrdata.get := true.B
      meta.state := INVALID
      meta.dirty := false.B
    }.elsewhen(mp_probeack_valid) {
      state.s_probeack := true.B
    }.elsewhen(mp_dct_valid) {
      state.s_dct.get := true.B
    }.elsewhen(mp_cmometaw_valid) {
      state.s_cmometaw := true.B
    }
  }

  // `io.tasks.mainpipe.fire` only means RequestArb accepted the task. A
  // concurrent replacer retry can still suppress it in MainPipe S3, in which
  // case no SourceD response exists and the normal request must be replayed.
  // Record completion only when the selected task actually handshakes with
  // GrantBuffer through MainPipe's SourceD output.
  // Debug counters for the victim-selection-degradation investigation: cycles
  // from the L1-visible Grant to the deferred refill commit (the window in
  // which this context's chosen way stays locked for other replacements).
  val grantTimerCycle = RegInit(0.U(64.W))
  when(io.grantSent) {
    assert(req_valid && !cmo_cbo, "grantSent must belong to a live non-CMO normal MSHR")
    assert(!grantIssued, "normal MSHR emitted more than one L1-visible Grant")
    grantIssued := true.B
    grantTimerCycle := timer
  }

  val commitDeferralCycles = timer - grantTimerCycle
  val commitDeferralFire = io.tasks.mainpipe.fire && mp_normal_refill_commit_valid
  XSPerfHistogram("commit_deferral", commitDeferralCycles, commitDeferralFire,
    0, 512, 16)

  when(!io.alloc.valid) {
    when(io.refillWriteDone) {
      refillWritePending := false.B
      // A toN paired snoop consumes the RefillBuf and cancels this normal
      // commit at S3, so no DS/tag way was installed to protect. Otherwise
      // retain the chosen way through Directory's request pipe.
      refillWayCommitHold := Mux(postGrantReleaseSnoopConsumed, 0.U, 6.U)
    }.elsewhen(refillWayCommitPending) {
      refillWayCommitHold := refillWayCommitHold - 1.U
    }
  }

  when(io.replaceDone && req_valid && cmo_cbo) {
    // The child has completed the old-line writeback.  From this point the
    // normal context may issue only the CMO control request and later CBOAck.
    sidecarCmoDone := true.B
    state.s_release := true.B
    state.w_releaseack := true.B
    state.s_cbwrdata.foreach(_ := true.B)
    state.s_cmometaw := true.B
  }
  // For normal reads, wait until the final response data and its required
  // CompAck have both completed before recycling `io.id` as a CHI TxnID.
  // Issue-C can send CompAck before the final data beat, hence both grant
  // completion flags are required here.
  val readTxnTerminal = readTxnInFlight && state.w_grant && state.w_grantlast &&
    state.s_rcompack.getOrElse(true.B)
  when(!io.alloc.valid && readTxnTerminal) {
    readTxnInFlight := false.B
  }

  /*                      Handling response

      TL                    CHI             CHI Resp              CHI channel
  -----------------------------------------------------------------------------
  AcquireBlock       |  ReadNotShareDirty |  CompData           |    rxdat
  AcquirePerm(hit B) |  MakeUnique        |  Comp               |    rxrsp <-
  Get                |  ReadClean         |  CompData           |    rxdat
  Hint               |  ReadNotShareDirty |  CompData           |    rxdat
  Release            |  WriteBackFull     |  CompDBID           |    rxrsp
                     |  *                 |  RetryAck+PCrdGrant |    rxrsp <-
   */
  val c_resp = io.resps.sinkC
  val rxrsp = io.resps.rxrsp
  val rxdat = io.resps.rxdat
  // Probe core response
  when(c_resp.valid) {
    when(c_resp.bits.opcode === ProbeAck || c_resp.bits.opcode === ProbeAckData) {
      state.w_rprobeackfirst := true.B
      state.w_rprobeacklast := state.w_rprobeacklast || c_resp.bits.last
      state.w_pprobeackfirst := true.B
      state.w_pprobeacklast := state.w_pprobeacklast || c_resp.bits.last
    }
    when(c_resp.bits.opcode === ProbeAckData) {
      probeDirty := true.B
      meta.dirty := true.B
      when(c_resp.bits.last) {
        refillPayloadFromProbeAck := true.B
      }
    }
    when(isToN(c_resp.bits.param)) {
      meta.state := Mux(isT(meta.state), TIP, meta.state)
      meta.clients := Fill(clientBits, false.B)
    }
    when(isToB(c_resp.bits.param)) {
      meta.state := Mux(isT(meta.state), TIP, meta.state)
    }
    when(isParamFromT(c_resp.bits.param)) {
      meta.tagErr := c_resp.bits.denied
      meta.dataErr := c_resp.bits.corrupt
      denied := denied || c_resp.bits.denied
      corrupt := corrupt || c_resp.bits.corrupt
    }

    // CMO update release on ProbeAck/ProbeAckData
    when(req_cboClean) {
      when(c_resp.bits.opcode === ProbeAckData && state.w_rprobeackfirst) {
        state.s_release := false.B
        state.w_releaseack := false.B
      }
      when(c_resp.bits.opcode === ProbeAck) {
        when(meta.dirty) {
          state.s_release := false.B
          state.w_releaseack := false.B
        }.otherwise {
          // meta write compensation on ProbeAck TtoB
          state.s_cmometaw := false.B
        }
      }
    }
  }

  val rxdatIsU = rxdat.bits.resp.get === UC /*UC_UD*/
  val rxdatIsU_PD = rxdat.bits.resp.get === UC_PD

  val rxrspIsU = rxrsp.bits.resp.get === UC /*UC_UD*/

  // RXDAT
  val rxdatLastValid = WireInit(false.B)
  when(rxdat.valid) {
    // RXDAT uses RefillBuf's higher-priority write input, so it supersedes an
    // earlier ProbeAckData snapshot in the same cycle.
    refillPayloadFromProbeAck := false.B
    val nderr = rxdat.bits.respErr.getOrElse(OK) === NDERR
    val derr = rxdat.bits.respErr.getOrElse(OK) === DERR
    val rxdatCorrupt = rxdat.bits.corrupt
    ifAfterIssueC {
      when(rxdat.bits.chiOpcode.get === DataSepResp) {
        require(beatSize == 2) // TODO: This is ugly
        beatCnt := beatCnt + 1.U
        state.w_grantfirst := true.B
        state.w_grantlast := state.w_grantfirst && beatCnt === (beatSize - 1).U
        rxdatLastValid := state.w_grantfirst && beatCnt === (beatSize - 1).U
        state.w_replResp := state.w_replResp || nderr
        gotT := rxdatIsU || rxdatIsU_PD
        gotDirty := gotDirty || rxdatIsU_PD
        gotGrantData := true.B
        denied := denied || nderr
        corrupt := corrupt || derr || nderr || rxdatCorrupt
      }
    }

    when(rxdat.bits.chiOpcode.get === CompData) {
      require(beatSize == 2) // TODO: This is ugly
      state.w_grantfirst := true.B
      state.w_grantlast := state.w_grantfirst
      rxdatLastValid := state.w_grantfirst
      state.w_grant := true.B
      state.w_replResp := state.w_replResp || nderr
      gotT := rxdatIsU || rxdatIsU_PD
      gotDirty := gotDirty || rxdatIsU_PD
      gotGrantData := true.B
      // The TxnID of CompAck is set to the same value as the DBID of the read data.
      txnid_rcompack := rxdat.bits.dbID.getOrElse(0.U)
      // The TgtID of CompAck is set to the same value as the HomeNID of the read data.
      tgtid_rcompack := rxdat.bits.homeNID.getOrElse(0.U)
      denied := denied || nderr
      corrupt := corrupt || derr || nderr || rxdatCorrupt
      req.traceTag.get := req.traceTag.get || rxdat.bits.traceTag.getOrElse(false.B)
    }
  }

  io.dataRefill.valid := rxdat.valid && rxdatLastValid
  io.dataRefill.bits.isDemand := req_acquire || req_get
  io.dataRefill.bits.isPrefetch := req_prefetch
  io.dataRefill.bits.pfReqSrc := req.reqSource
  io.dataRefill.bits.addr := Cat(req.tag, req.set, 0.U(offsetBits.W))
  io.dataRefill.bits.latency := timer

  // RXRSP
  when(rxrsp.valid) {
    val nderr = rxrsp.bits.respErr.getOrElse(OK) === NDERR
    ifAfterIssueC {
      when(rxrsp.bits.chiOpcode.get === RespSepData) {
        state.w_grant := true.B
        state.w_replResp := state.w_replResp || nderr
        // The TgtID of CompAck is set to the same value as the SrcID of the read response.
        tgtid_rcompack := rxrsp.bits.srcID.getOrElse(0.U)
        // The TxnID of CompAck is set to the unique DBID value generated by the Home.
        txnid_rcompack := rxrsp.bits.dbID.getOrElse(0.U)
        denied := denied || nderr
        req.traceTag.get := rxrsp.bits.traceTag.get
      }
    }

    when(rxrsp.bits.chiOpcode.get === Comp) {
      // There is a pending Read transaction waiting for the Comp resp
      // *NOTICE: In CMO transactions, releases (if there was one) always happen before acquire
      when(!state.w_grant && (state.s_cmoresp || state.w_releaseack)) {
        state.w_grantfirst := true.B
        state.w_grantlast := true.B
        state.w_grant := true.B
        gotT := rxrspIsU
        gotDirty := false.B
        denied := denied || nderr
        req.traceTag.get := rxrsp.bits.traceTag.get
        tgtid_rcompack := rxrsp.bits.srcID.getOrElse(0.U)
        txnid_rcompack := rxrsp.bits.dbID.getOrElse(0.U)
      }

      // There is a pending Evict/WriteEvictOrEvict transaction waiting for the Comp resp
      when(!state.w_releaseack) {
        state.w_releaseack := true.B
        // There is no CompAck for Comp in response of Evict. Thus there is no need to record TraceTag.
        // Except on WriteEvictOrEvict:
        when(isWriteEvictOrEvict) {
          req.traceTag.get := rxrsp.bits.traceTag.get
          // For WriteEvictOrEvict, drop CopyBackWrData on Comp
          state.s_cbwrdata.get := true.B
          // Schedule CompAck on Comp
          state.s_wcompack.get := false.B
          tgtid_wcompack := rxrsp.bits.srcID.getOrElse(0.U)
          txnid_wcompack := rxrsp.bits.dbID.getOrElse(0.U)
        }
      }
    }
    when(rxrsp.bits.chiOpcode.get === CompDBIDResp) {
      state.w_releaseack := true.B
      tgtid_wcompack := rxrsp.bits.srcID.getOrElse(0.U)
      txnid_wcompack := rxrsp.bits.dbID.getOrElse(0.U)
      cbWrDataTraceTag := rxrsp.bits.traceTag.get
    }
    when(rxrsp.bits.chiOpcode.get === RetryAck) {
      srcid_retryack := rxrsp.bits.srcID.getOrElse(0.U)
      pcrdtype := rxrsp.bits.pCrdType.getOrElse(0.U)
      gotRetryAck := true.B
      // RetryAck guarantees that the Home did not accept this request, so no
      // later data response can carry this MSHR's current transaction.
      readTxnInFlight := false.B
    }
  }

  // when rxrsp is PCrdGrant
  when(io.pCrd.grant) {
    state.s_reissue.get := false.B
    gotPCrdGrant := true.B
  }

  // replay
  val replResp = io.replResp.bits
  val normalRefillRevalidateResp = io.replResp.valid && normalRefillRevalidateInFlight
  when(io.replResp.valid) {
    assert(replLookupInFlight, "a normal MSHR must have exactly one live replTask when ReplacerResult returns")
    replReadInFlight := false.B
    when(normalRefillRevalidateResp) {
      normalRefillRevalidateInFlight := false.B
    }
  }
  when(io.replResp.valid && replResp.retry) {
    when(!normalRefillRevalidateResp) {
      // A Grant already handed to GrantBuffer may coincide with Directory
      // retry. Preserve its completion state and retry only the internal
      // replacer read. Conversely, a MainPipe task that retry suppresses has
      // not produced SourceD yet, so clear refill and replay it.
      when(!(grantIssued || io.grantSent)) {
        state.s_refill := false.B
        state.s_retry := false.B
      }
      dirResult.way := replResp.way
      when(retryTimes < backoffThreshold.U) {
        retryTimes := retryTimes + 1.U
      }
      backoffTimer := 0.U
    }
  }
  when(io.replResp.valid && !replResp.retry) {
    when(normalRefillRevalidateResp) {
      normalRefillRevalidateNeeded := false.B
      state.w_replResp := true.B
      // A revalidate-miss that adopts a no-probe victim merges install into
      // the grant; a revalidate-hit keeps the deferred commit.
      mergeInstallWithGrant := !cmo_cbo && !mergeA && !replResp.hit &&
        (replResp.meta.state === INVALID || !replResp.meta.clients.orR)
      when(!normalRefillDroppedByToN) {
        when(replResp.hit) {
          assert(replResp.tag === req.tag, "normal refill revalidation hit must return the request tag")
        }
        dirResult.hit := replResp.hit
        dirResult.tag := replResp.tag
        dirResult.way := replResp.way
        dirResult.meta := replResp.meta

        // A miss from revalidation selected a new victim. MSHRCtl allocates a
        // ReplaceMSHR in this cycle; hold the normal commit until that victim is
        // safe to overwrite.
        when(!replResp.hit && replResp.meta.state =/= INVALID) {
          state.s_release := false.B
          state.w_releaseack := false.B
          when(replResp.meta.clients.orR) {
            state.s_rprobe := false.B
            state.w_rprobeackfirst := false.B
            state.w_rprobeacklast := false.B
            sourceBProbeIssued := false.B
          }
        }
      }
    }.otherwise {
      state.w_replResp := true.B
      // Miss with an invalid or client-less victim: install merges into the
      // grant.  A victim with L1 clients needs the sidecar probe first, so it
      // keeps the deferred commit.
      mergeInstallWithGrant := !dirResult.hit && !cmo_cbo && !mergeA &&
        (replResp.meta.state === INVALID || !replResp.meta.clients.orR)
      // A completed Grant-path replacement read has already discharged this
      // context's Directory lookup duty and pinned the victim below (or an
      // invalid way).  Clear the revalidate request so a delayed
      // hit-turned-miss refill cannot select a second, conflicting victim and
      // orphan the ReplaceMSHR allocated for the first one.
      normalRefillRevalidateNeeded := false.B

      // update meta (no need to update hit/set/error/replacerInfo of dirResult)
      dirResult.tag := replResp.tag
      dirResult.way := replResp.way
      dirResult.meta := replResp.meta

      // replacer choosing:
      // 1. an invalid way, release no longer needed
      // 2. the same way, just release as normal (only now we set s_release)
      // 3. differet way, we need to update meta and release that way
      // if meta has client, rprobe client
      when(replResp.meta.state =/= INVALID) {
        // set release flags
        state.s_release := false.B
        state.w_releaseack := false.B
        // rprobe clients if any
        when(replResp.meta.clients.orR) {
          state.s_rprobe := false.B
          state.w_rprobeackfirst := false.B
          state.w_rprobeacklast := false.B
          // The old Probe, if any, does not cover the newly selected victim.
          sourceBProbeIssued := false.B
        }
      }
    }
  }

  // Victim distribution at the initial (grant-path) victim selection.
  // Measures the ceiling for grant+install merge: only the invalid and
  // client-less classes are merge-eligible without waiting on a probe.
  val replRespMissInitial = io.replResp.valid && !replResp.retry && !replResp.hit &&
    !normalRefillRevalidateResp
  XSPerfAccumulate("repl_victim_invalid_cnt",
    req_valid && replRespMissInitial && replResp.meta.state === INVALID)
  XSPerfAccumulate("repl_victim_noclient_cnt",
    req_valid && replRespMissInitial && replResp.meta.state =/= INVALID && !replResp.meta.clients.orR)
  XSPerfAccumulate("repl_victim_hasclient_cnt",
    req_valid && replRespMissInitial && replResp.meta.state =/= INVALID && replResp.meta.clients.orR)
  // Grants whose replacer read is folded into the grant pass itself: the
  // ReplacerResult returns at S3 of that same pass, two cycles after the
  // fire, so the registered merge decision can never apply to them.
  XSPerfAccumulate("grant_folded_replread_cnt",
    io.tasks.mainpipe.fire && mp_grant_valid && mp_grant.replTask)

  when(req_valid) {
    timer := timer + 1.U
  }

  val no_schedule = state.s_refill && state.s_probeack &&
    (state.s_release || sidecarDetached) &&
    state.s_rcompack.getOrElse(true.B) &&
    state.s_wcompack.getOrElse(true.B) &&
    state.s_cbwrdata.getOrElse(true.B) &&
    state.s_reissue.getOrElse(true.B) &&
    state.s_dct.getOrElse(true.B) &&
    state.s_cmoresp &&
    state.s_cmometaw
  val no_wait =
    (state.w_rprobeacklast || sidecarDetached) && state.w_pprobeacklast && state.w_grantlast && state.w_grant &&
      (state.w_releaseack || sidecarDetached) && state.w_replResp
  // A normal context cannot retire until its one RefillBuf-to-DS/Directory
  // commit task has been accepted, irrespective of whether a victim existed.
  val will_free = no_schedule && no_wait &&
    (cmo_cbo || normalRefillCommitIssued) &&
    !refillWritePending && !refillWayCommitPending && !readTxnInFlight &&
    !replLookupInFlight && !io.postGrantReleaseSnoopActive
  io.replaceParentRelease := will_free && req_valid && !cmo_cbo
  when(will_free && req_valid) {
    req_valid := false.B
    timer := 0.U
  }

  assert(!(will_free && readTxnInFlight), "normal MSHR must not retire while a CHI read is live")
  assert(
    !(replReadInFlight && normalRefillRevalidateInFlight),
    "a normal MSHR must not overlap replTask lookup contexts"
  )
  assert(
    req_valid || io.alloc.valid || !io.tasks.mainpipe.valid,
    "an inactive normal MSHR must not emit a MainPipe task"
  )

  // alias: should protect meta from being accessed or occupied
  val releaseNotSent = !state.s_release
  // A directory miss owns its chosen way through the delayed normal refill
  // commit. A grant can now reach L1 well before DS/tag/meta are updated, so
  // releasing this reservation at `s_refill` would let another miss select
  // and later overwrite the same way.
  val refillWayReserved = !dirResult.hit &&
    (!normalRefillCommitIssued || refillWritePending || refillWayCommitPending)
  io.status.valid := req_valid
  io.allocEpoch := allocEpoch
  io.status.bits.channel := req.channel
  io.status.bits.txChannel := req.txChannel // TODO
  io.status.bits.set := req.set
  io.status.bits.reqTag := req.tag
  io.status.bits.metaTag := dirResult.tag
  io.status.bits.needsRepl := releaseNotSent
  // wait for resps, high as valid
  io.status.bits.w_c_resp := !state.w_rprobeacklast || !state.w_pprobeacklast
  io.status.bits.w_d_resp := !state.w_grantlast || !state.w_grant || !state.w_releaseack
  io.status.bits.will_free := will_free
  io.status.bits.is_miss := !dirResult.hit
  io.status.bits.is_prefetch := req_prefetch
  io.status.bits.reqSource := req.reqSource
  // N+X snoop allocation gates: CMO lifetime, and the release data-read window
  // (release_valid1's condition, without the sidecarOwnsVictim gate)
  io.status.bits.isCmo := cmo_cbo
  io.status.bits.releaseDataWindow := false.B

  io.statAlloc.valid := io.alloc.valid
  io.statAlloc.bits.is_miss := !io.alloc.bits.dirResult.hit
  io.statAlloc.bits.is_prefetch := io.alloc.bits.task.opcode === Hint
  io.statAlloc.bits.channel := io.alloc.bits.task.channel

  io.msInfo.valid := req_valid
  io.msInfo.bits.set := req.set
  io.msInfo.bits.way := dirResult.way
  io.msInfo.bits.reqTag := req.tag
  io.msInfo.bits.reqSource := req.reqSource
  io.msInfo.bits.reqTimer := timer
  io.msInfo.bits.aliasTask.foreach(_ := req.aliasTask.getOrElse(false.B))
  io.msInfo.bits.needRelease := !state.w_releaseack
  // if releaseTask is already in mainpipe_s1/s2, while a refillTask in mainpipe_s3, the refill should also be blocked and retry
  // also block refill when the CMO-derived ProbeAckData was not written to DS
  io.msInfo.bits.blockRefill := releaseNotSent || RegNext(releaseNotSent, false.B) || RegNext(
    RegNext(releaseNotSent, false.B),
    false.B
  ) || refillWayReserved
  io.msInfo.bits.dirHit := dirResult.hit
  io.msInfo.bits.metaTag := dirResult.tag
  io.msInfo.bits.meta := meta
  io.msInfo.bits.meta.dirty := meta.dirty
  io.msInfo.bits.willFree := will_free
  io.msInfo.bits.isAcqOrPrefetch := req_acquire || req_prefetch
  io.msInfo.bits.isPrefetch := req_prefetch
  io.msInfo.bits.param := req.param
  io.msInfo.bits.mergeA := mergeA
  // SourceD Grant is irrevocable, but the delayed normal refill commit may
  // still be pending. Export this interval so RXSNP cannot service a
  // same-line snoop using Directory's transient hit=0/clients=0 view.
  val committedGrantTransfersClient = req_acquire || Mux(
    grantMergeSemanticsLatched,
    grantMergedClientLatched,
    mergedClientValid
  )
  io.msInfo.bits.grantPending := req_valid && !cmo_cbo && committedGrantTransfersClient &&
    (grantIssued || io.grantSent) &&
    (!normalRefillCommitIssued || refillWritePending || refillWayCommitPending)
  io.msInfo.bits.w_grantfirst := state.w_grantfirst
  io.msInfo.bits.w_grantlast := state.w_grantlast
  io.msInfo.bits.w_grant := state.w_grant
  io.msInfo.bits.s_release := state.s_release
  io.msInfo.bits.s_refill := state.s_refill
  io.msInfo.bits.s_cmoresp := state.s_cmoresp
  io.msInfo.bits.s_cmometaw := state.s_cmometaw
  io.msInfo.bits.w_releaseack := state.w_releaseack
  io.msInfo.bits.w_replResp := state.w_replResp
  io.msInfo.bits.w_rprobeacklast := state.w_rprobeacklast
  io.msInfo.bits.postGrantReleaseHold := postGrantReleaseSeen
  // High from allocation until the deferred refill commit is fully visible in
  // Directory and DataStorage (plus the way-commit margin).  For a hit-at-alloc
  // context there is still a real commit (meta/DS update), so this must not be
  // qualified with !dirResult.hit.
  io.msInfo.bits.refillCommitPending :=
    !normalRefillCommitIssued || refillWritePending || refillWayCommitPending
  // Commit task accepted AND its S3 write pulse done: the line's final state
  // is visible to later Directory/DS readers (but before the way-commit margin
  // that only protects the replacer's own way accounting).
  io.msInfo.bits.refillCommitLanded := normalRefillCommitIssued && !refillWritePending
  io.msInfo.bits.replaceData := isT(meta.state) && meta.dirty || // including WriteCleanFull
    isWriteEvictFull || isWriteEvictOrEvict
  io.msInfo.bits.releaseToClean := releaseToClean
  io.msInfo.bits.blocksSnoop := false.B
  io.msInfo.bits.channel := req.channel
  io.msInfo.bits.replace := 0.U.asTypeOf(new ReplaceMSHRInfo)

  assert(!(c_resp.valid && !io.status.bits.w_c_resp))
  assert(!(rxrsp.valid && rxrsp.bits.chiOpcode.get =/= PCrdGrant && !io.status.bits.w_d_resp))

  /* ======== Handling Nested C ======== */
  // for A miss, only when replResp do we finally choose a way, allowing nested C
  // for A-alias, always allowing nested C (state.w_replResp === true.B)
  // for CMO, always allowing nested C on directory hit (state.s_cmoresp === false.B)
  val nestedwb_match = req_valid &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag &&
    state.w_replResp &&
    // ReplacerResult preserves the victim metadata but leaves dirResult.hit
    // low. Only a live victim may own nested ReleaseData: matching an invalid
    // old way by tag can otherwise steal a release belonging to another live
    // MSHR on the same set. A ReleaseData that truly misses the directory is
    // routed through MSHRCtl.orphanReleaseData instead.
    dirResult.meta.state =/= INVALID &&
    (req_mayRepl || dirResult.hit) // exclude non-repl tasks (e.g. Forward Snoop) on directory miss
  val nestedwb_hit_match = req_valid && dirResult.hit &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag
  // A B.toN can invalidate a locally satisfied A request while it is still
  // waiting for its own L1 ProbeAck. No lower CHI read exists in this case:
  // `s_acquire` is set solely because the original Directory lookup hit.
  // Convert that lost hit into the request's first lower read. This is
  // intentionally disjoint from an already-issued read, which HN ordering
  // must complete without a snoop-driven replay.
  val lostLocalHit = nestedwb_hit_match && io.nestedwb.b_toN.get && req.fromA &&
    !cmo_cbo && state.s_acquire && !readTxnInFlight && !state.s_refill &&
    !grantIssued && !io.grantSent && !gotGrantData && !probeDirty
  val postGrantToN = nestedwb_hit_match && io.nestedwb.b_toN.get && req.fromA &&
    !cmo_cbo && (grantIssued || io.grantSent)

  when(nestedwb_match) {
    when(io.nestedwb.c_set_dirty) {
      meta.dirty := true.B
      meta.state := TIP
      meta.clients := Fill(clientBits, false.B)
      releaseDirty := true.B
    }
    when(io.nestedwb.c_set_tip) {
      meta.state := TIP
      meta.clients := Fill(clientBits, false.B)
    }
    when(io.nestedwb.b_inv_dirty && req.fromA) {
      meta.dirty := false.B
      meta.state := INVALID
      probeDirty := false.B
    }
  }
  XSPerfAccumulate(
    "cmo_nested_RXSNP_inv_dirty",
    req_valid && nestedwb_match && io.nestedwb.b_inv_dirty && req.fromA && cmo_cbo
  )
  when(nestedwb_hit_match) {
    when(io.nestedwb.b_toClean.get && req.fromA) {
      meta.dirty := false.B
      probeDirty := false.B
    }
    when(io.nestedwb.b_toB.get && req.fromA) {
      meta.state := Mux(meta.state >= BRANCH, BRANCH, INVALID)
    }
    when(io.nestedwb.b_toN.get && req.fromA) {
      meta.state := INVALID
      dirResult.hit := false.B
      meta.clients := Fill(clientBits, false.B)
      state.w_replResp := cmo_cbo // never query replacer on CMO
      req.aliasTask.foreach(_ := false.B)
    }
  }
  when(lostLocalHit) {
    state.s_acquire := false.B
    state.s_rcompack.foreach(_ := false.B)
    state.w_grantfirst := false.B
    state.w_grantlast := false.B
    state.w_grant := false.B
    state.s_refill := false.B
    gotT := false.B
    gotDirty := false.B
    gotGrantData := false.B
    refillPayloadFromProbeAck := false.B
    normalRefillCommitIssued := false.B
    normalRefillRevalidateNeeded := false.B
    normalRefillRevalidateInFlight := false.B
    tagErr := false.B
    denied := false.B
    corrupt := false.B
    beatCnt := 0.U
  }
  when(postGrantToN) {
    normalRefillDroppedByToN := true.B
    normalRefillRevalidateNeeded := false.B
    // The normal request's SourceD response is irrevocable. Let its normal
    // context retire, but do not reinterpret the toN invalidation as a miss
    // that selects another victim and installs old RefillBuf contents.
    state.w_replResp := true.B
  }
  assert(
    !(lostLocalHit && (readTxnInFlight || gotGrantData || probeDirty || grantIssued || io.grantSent)),
    "lostLocalHit must be a pre-read, pre-grant local-hit invalidation"
  )
  when(io.tasks.mainpipe.fire && mp_normal_refill_commit_valid &&
    (normalRefillDroppedByToN || postGrantReleaseSnoopConsumed)) {
    assert(
      !mp_normal_refill_commit.metaWen && !mp_normal_refill_commit.tagWen &&
        !mp_normal_refill_commit.dsWen && !mp_normal_refill_commit.readRefillData,
      "a snoop-consumed normal refill must retire without re-installing RefillBuf"
    )
  }
  XSPerfAccumulate(
    "cmo_nested_RXSNP_toClean",
    req_valid && nestedwb_hit_match && io.nestedwb.b_toClean.get && req.fromA && cmo_cbo
  )
  XSPerfAccumulate(
    "cmo_nested_RXSNP_toB",
    req_valid && nestedwb_hit_match && io.nestedwb.b_toB.get && req.fromA && cmo_cbo
  )
  XSPerfAccumulate(
    "cmo_nested_RXSNP_toN",
    req_valid && nestedwb_hit_match && io.nestedwb.b_toN.get && req.fromA && cmo_cbo
  )
  // A ReleaseData for the current refill is captured by SinkC in this normal
  // MSHR's RefillBuf. A ReleaseData for the selected old victim instead needs
  // a sidecar, whose ReleaseBuf owns the writeback payload.
  // SinkC has replaced the RefillBuf payload for this line.  Its dirty state
  // must survive the pending refill's metadata write as well.
  when(nestedReleaseTargetsRefill && io.nestedwb.c_set_dirty) {
    gotDirty := true.B
  }
  val nestedReleaseNeedsReplace = nestedwb_match && io.nestedwb.c_set_dirty &&
    !nestedReleaseTargetsRefill
  io.nestedReleaseNeedsReplace := nestedReleaseNeedsReplace
  io.nestedwbData := nestedwb_match && io.nestedwb.c_set_dirty &&
    (sidecarOwnsVictim || nestedReleaseNeedsReplace)

  dontTouch(state)

  //
  // deadlock check
  //
  val validCnt = RegInit(0.U(64.W))
  when(io.alloc.valid) {
    validCnt := 0.U
  }

  when(req_valid) {
    validCnt := validCnt + 1.U
  }

  val mshrAddr = Cat(req.tag, req.set, 0.U(6.W)) // TODO: consider multibank
  val VALID_CNT_MAX = 400000.U
  assert(
    validCnt <= VALID_CNT_MAX,
    "validCnt full!, maybe there is a deadlock! addr => 0x%x req_opcode => %d channel => 0b%b",
    mshrAddr,
    req.opcode,
    req.channel
  )

  val evictFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === Evict ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === Evict && io.tasks.mainpipe.bits.toTXREQ
  val wbFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === WriteBackFull ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === WriteBackFull && io.tasks.mainpipe.bits.toTXREQ
  val wcFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === WriteCleanFull ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === WriteCleanFull && io.tasks.mainpipe.bits.toTXREQ
  val weFire = io.tasks.txreq.fire && io.tasks.txreq.bits.opcode === WriteEvictFull ||
    io.tasks.mainpipe.fire && io.tasks.mainpipe.bits.opcode === WriteEvictFull && io.tasks.mainpipe.bits.toTXREQ
  assert(!RegNext(evictFire) || state.s_cbwrdata.get, "There should be no CopyBackWrData after Evict")
  assert(!RegNext(wbFire) || !state.s_cbwrdata.get, "There must be a CopyBackWrData after WriteBack")
  assert(!RegNext(wcFire) || !state.s_cbwrdata.get, "There must be a CopyBackWrData after WriteClean")
  assert(!RegNext(weFire) || !state.s_cbwrdata.get, "There must be a CopyBackWrData after WriteEvictFull")

  /* ======== Performance counters ======== */
  // time stamp
  val acquire_period = Option.when(cacheParams.enablePerf)(IO(ValidIO(UInt(64.W))))
  val release_period = Option.when(cacheParams.enablePerf)(IO(ValidIO(UInt(64.W))))
  if (cacheParams.enablePerf) {
    val acquire_start = io.tasks.txreq.fire && !state.s_acquire
    val release_start = io.tasks.mainpipe.fire && !state.s_release
    val acquire_ts = RegEnable(timer, acquire_start)
    val release_ts = RegEnable(timer, release_start)
    val acquire_finish = state.w_grant && state.w_grantlast
    val release_finish = state.w_releaseack

    acquire_period.get.valid := acquire_finish && !RegNext(acquire_finish)
    acquire_period.get.bits := timer - acquire_ts
    release_period.get.valid := release_finish && !RegNext(release_finish)
    release_period.get.bits := timer - release_ts
  }
}
