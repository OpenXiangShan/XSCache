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
  // Pulsed in the same cycle that MSHRCtl routes any victim snapshot to
  // ReleaseBuf. A clean DS fallback snapshot makes the buffer usable by a
  // nested snoop, but must not by itself turn Evict into WriteBackFull.
  val victimDataCaptured = Input(Bool())
  // Pulsed only for data which may be newer than the L2 DS image and thus
  // requires a data-bearing lower-level writeback.
  val victimDataNeedsWriteback = Input(Bool())
  // A same-line B has handshaken into RequestArb before the sidecar released
  // its victim. B owns the first data-return opportunity in this case.
  val snoopStarted = Input(Bool())
  // The nested snoop response has completed on TXRSP or TXDAT.
  val snoopDone = Input(Bool())
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
  val releaseNeedsData = RegInit(false.B)
  val postSnoopToInval = RegInit(false.B)
  val snoopInFlight = RegInit(false.B)
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

  // A victim-mode release is always data-bearing WriteBackFull with
  // MemAttr.allocate, mirroring the baseline MSHR's WriteEvictOrEvict: every
  // evicted line -- clean or dirty -- is pushed into the lower-level cache,
  // so openLLC retains victim contents and later L2 misses can hit there
  // instead of going to DRAM.  The DS snapshot needed for this is captured
  // unconditionally via captureFromDS.
  //
  // A dirty writeback is *required* when the victim directory entry is dirty,
  // or when the L1 actually returns ProbeAckData -- the L1 may hold dirty data
  // while the L2 directory still shows clean (a store does not notify the L2
  // until writeback).  The baseline MSHR mirrors this by setting meta.dirty on
  // ProbeAckData; fixating needsData at alloc time would emit Evict (no data)
  // for such victims and silently drop the dirty data.
  val isCmoClean = mode === ReplaceMSHRMode.cmoClean
  val isCmoFlush = mode === ReplaceMSHRMode.cmoFlush
  val isCmoInval = mode === ReplaceMSHRMode.cmoInval
  val victimAlwaysData = mode === ReplaceMSHRMode.victim
  val needsData = victimAlwaysData || victim.meta.dirty || probeAckDataRecv || isCmoClean
  // All valid replacement victims are captured, including clean ones. The
  // snapshot may be needed only by a later nested snoop; it does not force a
  // data-bearing lower-level release.
  val releaseBufRequired = captureFromDS || needsData
  // CBO.inval mirrors the existing normal-MSHR path: its Evict carries no
  // CopyBackWrData.  Clean and dirty Flush use the data-bearing path.
  // Preserve the release's data obligation after a snoop changes the
  // protocol-visible victim state to I. CHI still requires an outstanding
  // CopyBack flow to finish after CompDBIDResp.
  val activeReleaseNeedsData = Mux(releaseSent, releaseNeedsData, needsData)
  val needsCopyBack = Mux(isCmoInval, false.B, activeReleaseNeedsData)
  val releaseOpcode = Mux(isCmoClean, WriteCleanFull,
    Mux(isCmoFlush, Mux(needsData, WriteBackFull, Evict),
      Mux(needsData, WriteBackFull, Evict)))

  when (io.alloc.valid) {
    assert(!valid, "replace context allocated while still valid")
    valid := true.B
    probeSent := false.B
    probeAck := false.B
    releaseSent := false.B
    releaseAck := false.B
    copyBackSent := false.B
    releaseNeedsData := false.B
    postSnoopToInval := false.B
    snoopInFlight := false.B
    snoopConsumed := false.B
    captureIssued := false.B
    releaseBufCaptured := false.B
    probeAckDataRecv := false.B
    releaseDataPending := false.B
    donePulse := false.B
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
    victim.releaseToClean := false.B
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
  releaseTask.likelyshared.get := false.B
  releaseTask.expCompAck.get := false.B
  releaseTask.allowRetry.get := false.B
  releaseTask.memAttr.get.cacheable := true.B
  releaseTask.memAttr.get.allocate := !isCmoClean && !isCmoFlush && !isCmoInval && needsData
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
  val captureTaskValid = valid && captureFromDS && probeDone && !probeAckDataRecv && !captureIssued
  // Before release enters RequestArb, a matching B has priority. Once this
  // task has fired it becomes an irreversible lower-level transaction, and
  // RXSNP must instead wait for the final CopyBackWrData beat.
  val releaseTaskValid = valid && probeDone && !releaseSent && !snoopConsumed &&
    !snoopInFlight && !io.snoopStarted && !releaseDataPending && !io.releaseDataSeen &&
    (!releaseBufRequired || releaseBufCaptured)
  // A late snoop has priority over CopyBackWrData issue. Its final state must
  // be reflected in the WriteData response, so never freeze CopyBackWrData
  // with the pre-snoop UD state in the same cycle that B enters RequestArb.
  val copyBackTaskValid = valid && releaseAck && needsCopyBack && !copyBackSent &&
    !snoopInFlight && !io.snoopStarted
  // Victim-mode writeback requests (WriteBackFull etc.) are control-only
  // flits: they need no DS/Directory work.  Send them on the direct
  // MSHR->TXREQ path instead of occupying a MainPipe pass.  CMO-mode releases
  // keep the MainPipe path (they are rare and ordering-sensitive).
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
  io.tasks.mainpipe.valid := captureTaskValid ||
    (copyBackTaskValid && mode =/= ReplaceMSHRMode.victim) ||
    (releaseTaskValid && mode =/= ReplaceMSHRMode.victim)
  io.tasks.mainpipe.bits := Mux(captureTaskValid, captureTask,
    Mux(copyBackTaskValid, copyBackTask, releaseTask))
  io.tasks.txrsp.valid := false.B
  io.tasks.txrsp.bits := 0.U.asTypeOf(io.tasks.txrsp.bits)

  when (io.tasks.source_b.fire) {
    probeSent := true.B
  }
  when (io.tasks.txreq.fire) {
    // Victim-mode writeback request left on the direct TXREQ path.
    releaseSent := true.B
    releaseNeedsData := needsData
  }
  when (io.tasks.txdat.fire) {
    // Victim-mode CopyBackWrData accepted into the TXDAT queue on the direct
    // path.  This is the ordering point after which a further snoop must be
    // treated as late (the WriteData state is now frozen in the queue).
    copyBackSent := true.B
  }
  when (io.tasks.mainpipe.fire) {
    when (captureTaskValid) {
      captureIssued := true.B
    }.elsewhen (releaseTaskValid) {
      releaseSent := true.B
      releaseNeedsData := needsData
    }.elsewhen (copyBackTaskValid) {
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
  when (io.snoopStarted) {
    assert(valid && !copyBackSent,
      "a nested snoop must reserve ReplaceMSHR before CopyBackWrData is issued")
    snoopInFlight := true.B
  }
  when (io.snoopDone) {
    assert(valid && snoopInFlight,
      "nested snoop completion arrived without a reserved ReplaceMSHR")
    snoopInFlight := false.B
  }
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
  }
  when (valid && io.snoopToInvalDone && !releaseSent) {
    valid := false.B
    victim.valid := false.B
    donePulse := true.B
  }.elsewhen (valid && releaseAck && (!needsCopyBack || io.copyBackDone)) {
    when (needsCopyBack) {
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
  io.info.bits.releaseBufReady := releaseBufCaptured
  io.info.bits.refillReady := probeDone && (!releaseBufRequired || releaseBufCaptured)
  io.done.valid := donePulse
  io.done.bits.parentId := victim.parentId
  io.done.bits.parentEpoch := victim.parentEpoch
}
