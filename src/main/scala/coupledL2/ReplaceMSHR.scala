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

/**
  * Replacement transaction running in its own physical MSHR slot.
  *
  * It owns only the victim transaction and the physical ReleaseBuf entry.
  * MSHRCtl reports each actual ReleaseBuf write, so the sidecar can both issue
  * its writeback with valid victim data and explicitly authorize the normal
  * parent normal context to install its refill into DS.
  */
class ReplaceMSHRIO(implicit p: Parameters) extends CoupledL2Bundle {
  val id = Input(UInt(mshrBits.W))
  val alloc = Flipped(ValidIO(new ReplaceRequest))
  val sinkC = Flipped(ValidIO(new RespInfoBundle))
  // Asserted by SinkC at acceptance of same-line ReleaseData. The data itself
  // reaches ReleaseBuf later through RequestArb and MainPipe.
  val releaseDataSeen = Input(Bool())
  val rxrsp = Flipped(ValidIO(new RespInfoBundle))
  val tasks = new MSHRTasks
  // The direct TXREQ path commits into a one-entry skid inside MSHRCtl, so
  // tasks.txreq.fire is the capture (ownership transfer), not the TXREQ
  // enqueue.  txreqPending is the skid's occupancy for this context.
  val txreqPending = Input(Bool())
  // Pulsed in the same cycle that MSHRCtl routes any victim snapshot to
  // ReleaseBuf. A clean DS fallback snapshot makes the buffer usable by a
  // nested snoop, but must not by itself turn Evict into WriteBackFull.
  val victimDataCaptured = Input(Bool())
  // Pulsed only for data which may be newer than the L2 DS image and thus
  // requires a data-bearing lower-level writeback.
  val victimDataNeedsWriteback = Input(Bool())
  val nestedReleaseWritePending = Input(Bool())
  // An issued data-bearing release still owns its unsent CopyBack payload.
  val acceptsLateReleaseData = Output(Bool())
  // A same-line B has handshaken into RequestArb before the sidecar released
  // its victim. B owns the first data-return opportunity in this case.
  val snoopStarted = Input(Bool())
  // Registered local admission phase protects a possible B capture before
  // its exact same-line service reservation is applied on the following edge.
  val localReservationHold = Input(Bool())
  val sourceBHold = Input(Bool())
  // The whole nested snoop has completed, including forwarded CompData.
  val snoopDoneCount = Input(UInt(log2Ceil(mshrsAll + 3).W))
  // Keep the snapshot current so a later CMO commit cannot restore stale
  // exclusive ownership after the HN response downgraded the line.
  val snoopMeta = Flipped(ValidIO(new MetaEntry))
  // The nested invalidating snoop has actually left the L2. Its response has
  // already made the old victim invalid at the HN, so no lower release remains.
  val snoopToInvalDone = Input(Bool())
  // Final CopyBackWrData beat has handshaken on TXDAT. This is the point at
  // which a late snoop may observe the victim as absent.
  val copyBackDone = Input(Bool())
  // The normal parent has retired after completing its refill-only task.
  // Keep this sidecar alive for writeback, but detach its parent association
  // so a later tenant of the physical normal slot cannot match it by epoch.
  val parentRelease = Input(Bool())
  val info = ValidIO(new ReplaceMSHRInfo)
  val txnId = Output(UInt(mshrBits.W))
  // One-cycle notification after the child has completed its lower-level
  // writeback.  CMO parents use this before issuing their control request.
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
  // A Comp for an outstanding WriteEvictOrEvict means the HN did not keep a
  // copy and never wants the data.  This must outlive the transient
  // compAckPending: once the CompAck is accepted, a non-sticky cancel would
  // let copyBackTaskValid fire again and push the abandoned beat with a DBID
  // the HN may already have reassigned to another transaction.
  val compReceived = RegInit(false.B)
  val releaseNeedsData = RegInit(false.B)
  val postSnoopToInval = RegInit(false.B)
  // Several same-line snoops can be reserved simultaneously: openLLC may have
  // two snoops to one line outstanding at once, RXSNP buffers both, and it
  // keeps admitting more while the earlier responses drain through MainPipe
  // and the TXRSP/TXDAT queues.  The reservation is therefore a tally -- every
  // snoopStarted is matched by exactly one snoopDone, and the sidecar stays
  // reserved until the last one leaves.  Width covers every response that can
  // be in flight at once: TXDAT caps sinkB at 2*mshrsAll (its inflightCnt) and
  // RequestArb/MainPipe add a few in-flight stages.
  val snoopInFlightWidth = log2Up(2 * mshrsAll + 8)
  val snoopInFlightMax = ((BigInt(1) << snoopInFlightWidth) - 1).U
  // start and done may coincide (one snoop starts while another completes) and
  // then cancel exactly.
  val snoopInFlightCnt = RegInit(0.U(snoopInFlightWidth.W))
  val snoopInFlight = snoopInFlightCnt =/= 0.U
  val snoopConsumed = RegInit(false.B)
  val captureIssued = RegInit(false.B)
  val releaseBufCaptured = RegInit(false.B)
  val probeAckDataRecv = RegInit(false.B)
  // ReleaseData accepted by SinkC is carried to ReleaseBuf later through
  // MainPipe. Do not send an earlier ProbeAckData payload while it is pending.
  val releaseDataPending = RegInit(false.B)
  val donePulse = RegInit(false.B)
  val victim = RegInit(0.U.asTypeOf(new ReplaceMSHRInfo))
  val mode = RegInit(ReplaceMSHRMode.victim)
  val needProbe = RegInit(false.B)
  val probeParam = RegInit(toN)
  val captureFromDS = RegInit(false.B)
  val releaseTgtId = RegInit(0.U(NODEID_WIDTH.W))
  val releaseDbId = RegInit(0.U(DBID_WIDTH.W))
  // Comp for WriteEvictOrEvict cancels CopyBackWrData and owes a CompAck.
  // Kept until that CompAck is accepted, so the sidecar cannot retire first.
  val compAckPending = RegInit(false.B)

  // Victim-mode writeback follows the baseline MSHR opcode split.  Only a
  // TIP/TRUNK victim that is dirty, or whose L1 actually returned
  // ProbeAckData, is a data-bearing WriteBackFull.  The L1 may hold dirty
  // data while the L2 directory still shows clean (a store does not notify
  // the L2 until writeback); the baseline mirrors this by setting meta.dirty
  // on ProbeAckData.  Everything else is WriteEvictOrEvict once CHI issue is
  // at least E.b, and Evict before that.  The DS snapshot is still captured
  // unconditionally via captureFromDS: a nested snoop may need it even when
  // the lower-level release carries no data.
  val isCmoClean = mode === ReplaceMSHRMode.cmoClean
  val isCmoFlush = mode === ReplaceMSHRMode.cmoFlush
  val isCmoInval = mode === ReplaceMSHRMode.cmoInval
  val isVictim = mode === ReplaceMSHRMode.victim
  // CBO.inval always evicts without data, even for a dirty line: the
  // invalidation discards the contents (RISC-V CMO semantics), mirroring the
  // baseline's isWriteBackFull, which excludes req_cboInval.  Keeping the two
  // sides consistent is essential -- a data-bearing opcode whose
  // needsCopyBack is false would retire the sidecar before CopyBackWrData,
  // leaving the L3 refill entry waiting for beats forever.
  val needsData = isCmoClean ||
    (!isCmoInval && (victim.meta.dirty || probeAckDataRecv))
  // Baseline isWriteBackFull: TIP/TRUNK (state(1)) and dirty, including data
  // the L1 returned.  Victim mode only; the CMO flush keeps its own rule in
  // the opcode mux below (needsData -> WriteBackFull) because its dirty
  // payload must not be downgraded to a dataless Evict.
  val victimWriteBackFull = isVictim && isT(victim.meta.state) &&
    (victim.meta.dirty || probeAckDataRecv)
  val victimWriteEvictOrEvict = isVictim && afterIssueEbOrElse(!victimWriteBackFull, false.B)
  // All valid replacement victims are captured, including clean ones. The
  // snapshot may be needed only by a later nested snoop; it does not force a
  // data-bearing lower-level release.
  val releaseBufRequired = captureFromDS || needsData
  // Comp for WriteEvictOrEvict cancels the beat outright, including one a
  // snoop starting this cycle would otherwise have deferred and revived.
  // Withdrawing txdat.valid is enough: MSHRCtl drops a staged beat once the
  // selected sidecar's valid falls.
  val copyBackCancelled = compReceived || compAckPending ||
    io.rxrsp.valid && io.rxrsp.bits.chiOpcode.get === Comp && victimWriteEvictOrEvict
  // Preserve the release's data obligation after a snoop changes the
  // protocol-visible victim state to I. CHI still requires an outstanding
  // CopyBack flow to finish after CompDBIDResp.  A cancelled
  // WriteEvictOrEvict has no such flow, so nothing may wait on copyBackDone.
  val activeReleaseNeedsData = Mux(releaseSent, releaseNeedsData, needsData)
  val needsCopyBack = Mux(isCmoInval, false.B, activeReleaseNeedsData) && !copyBackCancelled
  io.acceptsLateReleaseData := valid && releaseSent && needsCopyBack && !copyBackSent
  val releaseOpcode = Mux(isCmoClean, WriteCleanFull,
    Mux(isCmoInval, Evict,
      Mux(victimWriteBackFull, WriteBackFull,
        Mux(victimWriteEvictOrEvict, WriteEvictOrEvict,
          // CMO flush: a dirty line must still carry its data; a clean one
          // evicts dataless, as before this split.
          Mux(needsData, WriteBackFull, Evict)))))

  when (io.alloc.valid) {
    assert(!valid, "replace context allocated while still valid")
    // A live skid entry belongs to the previous episode and still owns its
    // txnID/ReleaseBuf; retirement below cannot release the slot early, so a
    // new allocation must never observe a pending flit.
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
    victim.metaTag := io.alloc.bits.dirResult.tag
    victim.probeDone := !io.alloc.bits.needProbe
    victim.releaseStarted := false.B
    victim.copyBackIssued := false.B
    victim.snoopInvalidated := false.B
    victim.releaseAck := false.B
    victim.needRelease := true.B
    victim.replaceData := io.alloc.bits.dirResult.meta.dirty
    victim.releaseToClean := io.alloc.bits.mode === ReplaceMSHRMode.cmoClean
    victim.leaseHeld := false.B
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
  // TL opcode mirrors the data-bearing decision (used by TL-side decode paths)
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
  // WriteBackFull and WriteEvictOrEvict allocate; the pre-E.b Evict fallback
  // and every CMO release do not.
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
  // The release pass already committed the CMO Directory/DS update. A
  // later CopyBack only transports data: replaying those writes could undo
  // a snoop that invalidated or downgraded the line in the meantime.
  copyBackTask.metaWen := false.B
  copyBackTask.dsWen := false.B
  // A CopyBack request remains live after a late SnpUnique. Its WriteData
  // cache state must describe the state after the snoop, not the earlier UD.
  // TXDAT deasserts byte enables for CopyBackWrData_I.
  // Mirror the baseline MSHR's setPD(metaChi, meta.dirty): report the victim's
  // actual coherence state (BRANCH -> SC/SD, otherwise UC/UD by dirty) so a
  // clean victim pushed into openLLC is encoded as UC rather than UD.
  val victimWritebackDirty = victim.meta.dirty || probeAckDataRecv
  val victimWritebackState = Mux(victim.meta.state === BRANCH,
    Mux(victimWritebackDirty, SD, SC),
    Mux(victimWritebackDirty, UD, UC))
  copyBackTask.resp.get := Mux(postSnoopToInval, I, setPD(victimWritebackState, victimWritebackDirty))

  val probeDone = !needProbe || probeAck
  // DS capture occurs only after all client probes complete and only when none
  // returned ProbeAckData. It is the fallback snapshot for every valid victim;
  // a clean L1 client can correctly respond with a data-less ProbeAck.
  val captureTaskValid = valid && captureFromDS && probeDone && !probeAckDataRecv && !captureIssued &&
    !io.nestedReleaseWritePending
  // Before release enters RequestArb, a matching B has priority. Once this
  // task has fired it becomes an irreversible lower-level transaction, and
  // RXSNP must instead wait for the final CopyBackWrData beat.
  val releaseTaskValid = valid && probeDone && !releaseSent && !snoopConsumed &&
    !snoopInFlight && !io.snoopStarted && !releaseDataPending && !io.releaseDataSeen &&
    (!releaseBufRequired || releaseBufCaptured) && !io.nestedReleaseWritePending
  // A late snoop has priority over CopyBackWrData issue. Its final state must
  // be reflected in the WriteData response, so never freeze CopyBackWrData
  // with the pre-snoop UD state in the same cycle that B enters RequestArb.
  // releaseAck already implies the release left via TXREQ; the explicit skid
  // hold keeps the request-before-data ordering independent of that argument.
  val copyBackTaskValid = valid && releaseAck && needsCopyBack && !copyBackSent &&
    !snoopInFlight && !io.snoopStarted && !io.txreqPending &&
    !releaseDataPending && !io.releaseDataSeen && !io.nestedReleaseWritePending && releaseBufCaptured
  // Victim-mode writeback requests (WriteBackFull etc.) are control-only
  // flits: they need no DS/Directory work.  Send them on the direct
  // MSHR->TXREQ path instead of occupying a MainPipe pass.  CMO-mode releases
  // keep the MainPipe path (they are rare and ordering-sensitive).
  // MSHRCtl captures this flit into a per-context one-entry skid before the
  // shared two-level TXREQ arbiter: ready means "captured", not "enqueued".
  // Capture is the irreversible point (same role the old arbiter fire had),
  // so the live !io.snoopStarted veto above still lets a B preempt until the
  // skid owns the flit.  Afterwards the sidecar keeps valid until releaseAck
  // and the retire condition waits for !txreqPending, so its txnID and
  // ReleaseBuf entry cannot be reused while the flit is still in the skid.
  io.tasks.txreq.valid := releaseTaskValid && mode === ReplaceMSHRMode.victim
  io.tasks.txreq.bits := releaseTask.toCHIREQBundle()
  // Victim-mode CopyBackWrData is a pure ReleaseBuf->TXDAT transfer: its
  // mainpipe pass only moved data between the two.  Present the task on the
  // direct path; MSHRCtl reads ReleaseBuf through the second read port and
  // enqueues task+data into TXDAT.  CMO-mode copybacks keep the MainPipe
  // path.  The sidecar only learns the send when TXDAT actually accepts
  // (txdat.fire), so a late snoop that starts before acceptance preempts
  // exactly as it did when the task sat in RequestArb.
  io.tasks.txdat.valid := copyBackTaskValid && mode === ReplaceMSHRMode.victim
  io.tasks.txdat.bits := copyBackTask
  val mainpipeIntent = Wire(Decoupled(new TaskBundle))
  mainpipeIntent.valid := captureTaskValid ||
    (copyBackTaskValid && mode =/= ReplaceMSHRMode.victim) ||
    (releaseTaskValid && mode =/= ReplaceMSHRMode.victim)
  mainpipeIntent.bits := Mux(captureTaskValid, captureTask,
    Mux(copyBackTaskValid, copyBackTask, releaseTask))
  val mainpipeInvalidate = io.alloc.valid || io.sinkC.valid || io.rxrsp.valid ||
    io.releaseDataSeen || io.victimDataCaptured || io.victimDataNeedsWriteback ||
    io.nestedReleaseWritePending ||
    io.snoopStarted || io.snoopDoneCount.orR || io.snoopMeta.valid || io.snoopToInvalDone ||
    io.copyBackDone || io.parentRelease || io.tasks.source_b.fire ||
    io.tasks.txreq.fire || io.tasks.txdat.fire || io.tasks.txrsp.fire
  val mainpipeIssuedVariant = MainpipeLeaseIO.freeze(io.tasks.preparedLease,
    mainpipeIntent, io.tasks.mainpipe,
    Mux(captureTaskValid, 0.U(4.W), Mux(copyBackTaskValid, 1.U(4.W), 2.U(4.W))), mainpipeInvalidate)
  // Comp for WriteEvictOrEvict is the only response that owes CompAck.  The
  // flit stays valid until TXRSP accepts it; retiring earlier would drop it.
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

  when (io.tasks.source_b.fire) {
    probeSent := true.B
  }
  when (io.tasks.txreq.fire) {
    // Victim-mode writeback request was committed to the direct TXREQ skid.
    // releaseSent must be set here (not when the skid drains) so the sidecar
    // can never re-present the same release: exactly one capture per episode.
    assert(!releaseSent, "a ReplaceMSHR may capture only one TXREQ flit per episode")
    releaseSent := true.B
    // needsData only tracks a writeback the directory forces. WriteEvictOrEvict
    // carries data too: L3 replies CompDBIDResp on a local miss and then waits
    // for CopyBackWrData. Latching needsData here would drop that beat and
    // leave both the sidecar and the L3 refill entry waiting forever. Comp is
    // what cancels it, through copyBackCancelled.
    releaseNeedsData := needsData || victimWriteEvictOrEvict
  }
  when (io.tasks.txrsp.fire) {
    compAckPending := false.B
  }
  when (io.tasks.txdat.fire) {
    // Victim-mode CopyBackWrData accepted into the TXDAT queue on the direct
    // path.  This is the ordering point after which a further snoop must be
    // treated as late (the WriteData state is now frozen in the queue).
    copyBackSent := true.B
  }
  when (io.tasks.mainpipe.fire) {
    when (mainpipeIssuedVariant === 0.U) {
      captureIssued := true.B
    }.elsewhen (mainpipeIssuedVariant === 2.U) {
      releaseSent := true.B
      releaseNeedsData := needsData || victimWriteEvictOrEvict
    }.elsewhen (mainpipeIssuedVariant === 1.U) {
      // CMO-mode copyback still travels via MainPipe.
      copyBackSent := true.B
    }
  }
  when (io.victimDataCaptured) {
    // Every source, including a clean DS fallback, makes ReleaseBuf ready for
    // a nested snoop. Writeback policy is carried separately below.
    releaseBufCaptured := true.B
    victim.replaceData := true.B
    releaseDataPending := false.B
  }
  when (io.victimDataNeedsWriteback) {
    probeAckDataRecv := true.B
    // This payload supersedes the clean victim snapshot.  RXSNP uses the
    // sidecar meta to choose SnpResp versus SnpRespData, so preserve the
    // dirty ownership alongside the ReleaseBuf data.
    victim.meta.dirty := true.B
  }
  when (io.copyBackDone) { copyBackComplete := true.B }
  when (io.snoopStarted) {
    assert(valid && !copyBackSent,
      "a nested snoop must reserve ReplaceMSHR before CopyBackWrData is issued")
  }
  // A skid entry is the committed tail of a still-live episode.  The retire
  // holds keep the context valid until it drains, so it can never own a flit
  // whose txnID/ReleaseBuf already moved on.
  assert(!io.txreqPending || valid, "TXREQ skid holds a flit without a live ReplaceMSHR")
  // Preserve every source completion and same-cycle start/done net change.
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
      // The data may already be committed to a pending CopyBack transaction,
      // but the snoop defines the RN's state as I from this point onward.
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
  // If MainPipe commits the queued ReleaseData in this same cycle, the
  // capture event wins: ReleaseBuf now contains the authoritative payload.
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
    // WriteEvictOrEvict's Comp means the HN kept no copy: drop the CopyBack
    // beat (txdat.valid falls through copyBackCancelled, which also drops a
    // beat already staged in MSHRCtl) and answer with CompAck.  CompDBIDResp
    // still wants the data and no CompAck.
    when (io.rxrsp.bits.chiOpcode.get === Comp && victimWriteEvictOrEvict) {
      compAckPending := true.B
      compReceived := true.B
    }
  }
  // An invalidating snoop that found no release in flight consumes the sidecar.
  // Retire in the same cycle, as before, unless another reserved response is
  // still travelling -- retiring then would make that response's completion
  // arrive at an invalid sidecar. snoopInFlightNext subtracts this snoop's own
  // reservation, so the single-snoop case is unchanged.
  // Retiring frees the txnID/ReleaseBuf for reuse, so it must also wait until
  // the skid has handed the release flit to TXREQ.  For the first branch
  // !releaseSent already implies an empty skid; the explicit term documents
  // the invariant.  The releaseAck branch implies the flit left long ago.
  val consumedRetire = (snoopConsumed || io.snoopToInvalDone) && !releaseSent &&
    !io.txreqPending && snoopInFlightNext === 0.U
  val transportRetire = releaseAck && !compAckPending &&
    (!needsCopyBack || copyBackComplete || io.copyBackDone) &&
    !snoopInFlight && !io.snoopStarted && !io.txreqPending
  val retireEligible = valid && (consumedRetire || transportRetire) &&
    !io.nestedReleaseWritePending && !io.sourceBHold
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

  io.info.valid := valid
  io.info.bits := victim
  io.info.bits.releaseStarted := releaseSent
  io.info.bits.copyBackIssued := copyBackSent
  io.info.bits.awaitingProbeAck := valid && needProbe && probeSent && !probeAck
  io.info.bits.releaseBufReady := releaseBufCaptured && !io.nestedReleaseWritePending
  io.info.bits.refillReady := probeDone && (!releaseBufRequired || releaseBufCaptured) &&
    !io.nestedReleaseWritePending
  io.done.valid := donePulse
  io.done.bits.parentId := victim.parentId
  io.done.bits.parentEpoch := victim.parentEpoch
}
