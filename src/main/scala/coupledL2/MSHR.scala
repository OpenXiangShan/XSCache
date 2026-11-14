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

// An operation token remains leased through its terminal event and registered return acknowledgment.
class MainpipeLeaseToken(implicit p: Parameters) extends CoupledL2Bundle {
  val generation = UInt(8.W)
  val sequence = UInt(8.W)
}
class MainpipeLeaseDone(implicit p: Parameters) extends CoupledL2Bundle {
  val mshrId = UInt(mshrBits.W)
  val mshrContext = UInt(2.W)
  val token = new MainpipeLeaseToken
}
class NormalSlotReservation(implicit p: Parameters) extends CoupledL2Bundle {
  val id = UInt(mshrBits.W)
  val token = new MainpipeLeaseToken
  val task = new TaskBundle
}
class SnoopSlotToken(implicit p: Parameters) extends CoupledL2Bundle {
  val valid = Bool()
  val slot = UInt(mshrBits.W)
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val txnID = UInt(TXNID_WIDTH.W)
  val source = UInt(sourceIdBits.W)
  val generation = UInt(8.W)
  val sequence = UInt(8.W)
}
class OrphanReleaseSlotRequest(implicit p: Parameters) extends CoupledL2Bundle {
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val sequence = UInt(8.W)
}
class OrphanReleaseSlotClaim(implicit p: Parameters) extends CoupledL2Bundle {
  val valid = Bool()
  val needsAllocation = Bool()
  val slot = UInt(mshrBits.W)
  val generation = UInt(8.W)
  val set = UInt(setBits.W)
  val tag = UInt(tagBits.W)
  val sequence = UInt(8.W)
}
class MainpipeCandidate extends Bundle {
  val operation = UInt(4.W)
  val qosClass = UInt(3.W)
}
class MainpipeLeaseIO(implicit p: Parameters) extends CoupledL2Bundle {
  val candidate = ValidIO(new MainpipeCandidate)
  val prepare = Flipped(ValidIO(new MainpipeLeaseToken))
  val held = Flipped(ValidIO(new MainpipeLeaseToken))
  val cancel = ValidIO(new MainpipeLeaseToken)
  val release = Flipped(ValidIO(new MainpipeLeaseToken))
  val released = ValidIO(new MainpipeLeaseToken)
  val canPrepare = Output(Bool())
  val proposalHeld = Output(Bool())
}
object MainpipeLeaseIO {
  // Materialize and freeze payload only after the registered proposal wins its lease.
  def freeze(port: MainpipeLeaseIO, intent: DecoupledIO[TaskBundle],
      output: DecoupledIO[TaskBundle], variant: UInt, qosClass: UInt, invalidate: Bool): UInt = {
    val task = Reg(chiselTypeOf(intent.bits))
    val operation = Reg(UInt(4.W))
    val proposalClass = Reg(UInt(3.W))
    val proposal = RegInit(false.B)
    val candidateValid = RegInit(false.B)
    val payloadFrozen = RegInit(false.B)
    val token = Reg(chiselTypeOf(port.prepare.bits))
    val issued = RegInit(false.B)
    val canceled = RegInit(false.B)
    // Lease return does not advance the FSM; fence the completed operation until intent changes.
    val terminalFence = RegInit(false.B)
    val terminalFenceOperation = RegInit(0.U(4.W))
    val prepareSeen = RegNext(port.prepare.valid, false.B)
    port.proposalHeld := proposal
    when(terminalFence && !intent.valid) {
      terminalFence := false.B
    }
    val terminalFenceBlocks = terminalFence && variant === terminalFenceOperation
    when(!proposal && !port.held.valid && intent.valid && port.canPrepare &&
      !invalidate && !terminalFenceBlocks) {
      operation := variant
      proposalClass := qosClass
      proposal := true.B
      candidateValid := true.B
    }
    when(proposal && !port.held.valid && invalidate && !port.prepare.valid) {
      proposal := false.B
      candidateValid := false.B
    }
    when(port.prepare.valid) {
      assert(!port.held.valid && proposal, "prepare must acquire a registered source proposal")
      proposal := false.B
      token := port.prepare.bits
      issued := false.B
      canceled := invalidate
      when (intent.valid && !invalidate) {
        terminalFence := true.B
        terminalFenceOperation := operation
      }
      payloadFrozen := intent.valid && !invalidate
      candidateValid := intent.valid && !invalidate
      when (intent.valid && !invalidate) {
        task := intent.bits
      }
    }.elsewhen(!port.held.valid) {
      issued := false.B
      canceled := false.B
      payloadFrozen := false.B
      when(prepareSeen) { candidateValid := false.B }
    }.elsewhen(!issued && invalidate) {
      canceled := true.B
      candidateValid := false.B
    }
    port.cancel.valid := port.held.valid && !issued &&
      (canceled || (!payloadFrozen && !candidateValid))
    port.cancel.bits := token
    port.candidate.valid := candidateValid
    port.candidate.bits.operation := operation
    port.candidate.bits.qosClass := proposalClass
    output.valid := port.held.valid && !issued && candidateValid && payloadFrozen
    output.bits := Mux(port.held.valid, task, 0.U.asTypeOf(intent.bits))
    intent.ready := false.B
    when(output.fire) {
      assert(port.held.valid && !port.cancel.valid, "issue must own an uncanceled frozen variant")
      issued := true.B
      candidateValid := false.B
      terminalFence := true.B
      terminalFenceOperation := operation
    }
    when(port.released.valid) {
      when(!issued) { terminalFence := false.B }
    }
    when(port.held.valid) {
      assert(token.asUInt === port.held.bits.asUInt, "source snapshot and controller lease must match")
    }
    // The proposal edge must capture incoming variant; operation Q still names the previous task.
    val nextOperation = !proposal && !port.held.valid && intent.valid &&
      port.canPrepare && !invalidate && !terminalFenceBlocks
    Mux(nextOperation, variant, operation)
  }

  def acknowledge(port: MainpipeLeaseIO, active: Bool, allocating: Bool, issue: Bool): Unit = {
    port.released := RegNext(port.release, 0.U.asTypeOf(port.release))
    when(port.held.valid) {
      assert(active, "prepared lease must pin a live source allocation")
      assert(!allocating, "source allocation cannot be reused before lease return acknowledgment")
    }
    when(port.release.valid) {
      assert(port.held.valid && port.release.bits.asUInt === port.held.bits.asUInt,
        "lease return must name the held operation")
    }
    when(port.released.valid) {
      assert(port.held.valid && port.released.bits.asUInt === port.held.bits.asUInt,
        "lease acknowledgment must retain its source and version")
    }
    when(issue) {
      assert(port.held.valid, "MainPipe source issue requires a prepared lifetime lease")
    }
  }
}

class MSHRTasks(implicit p: Parameters) extends CoupledL2Bundle {
  val preparedLease = new MainpipeLeaseIO

  // outer
  val txreq = DecoupledIO(new CHIREQ) // TODO: no need to use decoupled Shandshake
  val txrsp = DecoupledIO(new CHIRSP) // TODO: no need to use decoupled handshake
  val source_b = DecoupledIO(new SourceBReq)
  val mainpipe = DecoupledIO(new TaskBundle) // To Mainpipe (SourceC or SourceD)
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
  val dataRefill = ValidIO(new DemandRefillBundle)
  val allocEpoch = Output(Bool())
  val status = ValidIO(new MSHRStatus)
  val statAlloc = ValidIO(new MSHRAllocStatus)
  val msInfo = ValidIO(new MSHRInfo)
  val alloc = Flipped(ValidIO(new MSHRRequest))
  val tasks = new MSHRTasks()
  val resps = new MSHRResps()
  val readDBID = Output(ValidIO(UInt(TXNID_WIDTH.W)))
  val readTxnEpoch = Output(UInt((TXNID_WIDTH - bankBits - 2 - mshrBits).W))
  val nestedwb = Input(new NestedWriteback)
  val nestedwbData = Output(Bool())
  val nestedReleaseNeedsReplace = Output(Bool())
  val aMergeTask = Flipped(ValidIO(new TaskBundle))
  val replResp = Flipped(ValidIO(new ReplacerResult))
  val replaceActive = Input(Bool())
  val replaceAccepted = Input(Bool())
  val sidecarDelegated = Output(Bool())
  val probeIssued = Output(Bool())
  val sinkCOwnerHold = Input(Bool())
  val pairLifetimeHold = Input(Bool())
  val sourceBHold = Input(Bool())
  val sourceBAccepted = Input(Bool())
  val bufferWritePending = Input(Bool())
  val nestedRefillWriteDone = Input(Bool())
  val replaceDetached = Input(Bool())
  val replaceRefillReady = Input(Bool())
  val replaceDone = Input(Bool())
  val refillWriteDone = Input(Bool())
  val grantSent = Input(Bool())
  val replaceParentRelease = Output(Bool())
  val deferGrant = Input(Bool())
  val releaseDataBeforeProbeAck = Input(Bool())
  val postGrantReleaseData = Input(Bool())
  val postGrantReleaseSnoopActive = Input(Bool())
  val postGrantReleaseSnoopToN = Input(Bool())
  val postGrantReleaseSnoopToB = Input(Bool())
  val postGrantReleaseSnoopToClean = Input(Bool())
  val releaseDataDirectWrite = Input(Bool())
  val grantInstallDone = Input(Bool())
  val blockReplRead = Input(Bool())
  val replReadInFlight = Output(Bool())
  val replReadWanted = Output(Bool())
  val refillPayloadFromProbeAck = Output(Bool())
  val postGrantReleaseWindow = Output(Bool())
  val postGrantReleaseHold = Output(Bool())
  val postGrantReleasePayloadReady = Output(Bool())
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
  val mainpipeIntent = Wire(Decoupled(new TaskBundle))
  val mainpipeVariant = Wire(UInt(2.W))
  val mainpipeIssuedVariant = Wire(UInt(2.W))
  val mainpipeInvalidate = Wire(Bool())
  val mainpipeProposedVariant = Reg(UInt(2.W))

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
  val initState = Wire(new NormalFSMState())
  initState.elements.foreach(_._2 := true.B)
  val state = RegInit(new NormalFSMState(), initState)


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
  val srcid_retryack = Reg(UInt(NODEID_WIDTH.W)) // SrcID in RetryAck, only used for protocol retry

  val pcrdtype = RegInit(0.U(PCRDTYPE_WIDTH.W))
  val gotRetryAck = RegInit(false.B)
  val sidecarDetachedLatched = RegInit(false.B)
  val sidecarDelegatedLatched = RegInit(false.B)
  val sourceBProbeIssued = RegInit(false.B)
  val grantIssued = RegInit(false.B)
  val grantMergeSemanticsLatched = RegInit(false.B)
  val grantMergedClientLatched = RegInit(false.B)
  val grantMergedNeedsTLatched = RegInit(false.B)
  // Grant and refill commit must retain the merged client alias and permission.
  val grantMergedAliasLatched = aliasBitsOpt.map(_ => RegInit(0.U(aliasBitsOpt.get.W)))
  val normalRefillCommitIssued = RegInit(false.B)
  val mergeInstallWithGrant = RegInit(false.B)
  val mergedGrantInstalled = RegInit(false.B)
  // Only a Grant that actually installed its refill may suppress the later commit.
  val grantInstallDoneReg = RegInit(false.B)
  val postGrantReleaseSnoopConsumed = RegInit(false.B)
  val postGrantReleaseSnoopToB = RegInit(false.B)
  val postGrantReleaseSnoopToClean = RegInit(false.B)
  // A post-Grant ToN cancels refill installation without replaying the irrevocable L1 response.
  val normalRefillDroppedByToN = RegInit(false.B)
  val normalRefillRevalidateNeeded = RegInit(false.B)
  val normalRefillRevalidateInFlight = RegInit(false.B)
  val sidecarVictimReadyLatched = RegInit(false.B)
  val refillWritePending = RegInit(false.B)
  // Hold the installed way through the Directory request pipeline after the S3 commit.
  val refillWayCommitHold = RegInit(0.U(3.W))
  val refillWayCommitPending = refillWayCommitHold.orR
  val readTxnInFlight = RegInit(false.B)
  // The per-read epoch occupies [TXNID_WIDTH-bankBits-3:mshrBits], above the physical slot ID.
  val readTxnSlotBits = mshrBits
  val readTxnEpochWidth = (TXNID_WIDTH - bankBits - 2) - readTxnSlotBits
  val readTxnEpochGen = RegInit(0.U(readTxnEpochWidth.W))
  val readTxnEpochSent = RegInit(0.U(readTxnEpochWidth.W))
  val replReadInFlight = RegInit(false.B)
  io.replReadInFlight := replReadInFlight
  io.refillPayloadFromProbeAck := req_valid && refillPayloadFromProbeAck
  val postGrantReleaseSeen = RegInit(false.B)
  val postGrantReleasePayloadReady = RegInit(false.B)
  val postGrantCleanReleaseSeen = RegInit(false.B)
  io.postGrantReleaseWindow := req_valid &&
    (state.s_refill || grantIssued) && !normalRefillCommitIssued
  io.postGrantReleaseHold := req_valid && postGrantReleaseSeen
  io.postGrantReleasePayloadReady := req_valid && postGrantReleasePayloadReady
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
  val nestedReleaseRefillPending = RegInit(false.B)
  val releaseDataBeforeProbeAckPending = RegInit(false.B)
  val releaseDataDirectWriteSeen = RegInit(false.B)
  val gotPCrdGrant = RegInit(false.B)

  val tagErr = RegInit(false.B) // L2 Tag Error
  val denied = RegInit(false.B)
  val corrupt = RegInit(false.B)
  io.pCrd.query.valid := gotRetryAck && !gotPCrdGrant
  io.pCrd.query.bits.pCrdType := pcrdtype
  io.pCrd.query.bits.srcID := srcid_retryack

  /* Allocation */
  when(io.alloc.valid) {
    assert(io.alloc.bits.task.fromA && io.alloc.bits.task.opcode =/= CBOClean &&
      io.alloc.bits.task.opcode =/= CBOFlush && io.alloc.bits.task.opcode =/= CBOInval,
      "normal MSHR allocation must contain a non-CMO A request")
    allocEpoch := !allocEpoch
    req_valid := true.B
    state.elements.foreach { case (name, field) => field := io.alloc.bits.state.elements(name) }
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
    normalRefillRevalidateNeeded := io.alloc.bits.dirResult.hit
    normalRefillRevalidateInFlight := false.B
    readTxnInFlight := false.B
    replReadInFlight := false.B
    sidecarVictimReadyLatched := false.B
    refillWritePending := false.B
    refillWayCommitHold := 0.U
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

    retryTimes := 0.U
    backoffTimer := 0.U
  }

  val replRespForThisParent = io.replResp.valid && io.replResp.bits.mshrId === io.id
  val sidecarOwnsVictim = io.replaceActive || sidecarDelegatedLatched || replRespForThisParent
  io.sidecarDelegated := sidecarDelegatedLatched
  io.probeIssued := sourceBProbeIssued
  when(io.replaceAccepted && !io.alloc.valid) {
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
    refillPayloadFromProbeAck := false.B
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
    when(io.nestedRefillWriteDone && postGrantReleaseSeen) {
      postGrantReleasePayloadReady := true.B
    }
    when(io.releaseDataBeforeProbeAck) {
      releaseDataBeforeProbeAckPending := true.B
    }.elsewhen(io.nestedRefillWriteDone) {
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

  // Overwrite a live victim only after its child detaches and retains the required payload.
  val victimSafeToOverwrite = !sidecarOwnsVictim ||
    (sidecarDetached && sidecarVictimReady)

  val promoteT_normal = dirResult.hit && meta_no_client && meta.state === TIP
  val promoteT_L3 = !dirResult.hit && gotT
  val promoteT_alias = dirResult.hit && req.aliasTask.getOrElse(false.B) && (meta.state === TRUNK || meta.state === TIP)
  // under above circumstances, we grant T to L1 even if it wants B
  val req_promoteT = (req_acquire || req_get || req_prefetch) && (promoteT_normal || promoteT_L3 || promoteT_alias)

  assert(!(req_valid && req_prefetch && dirResult.hit), "MSHR can not receive prefetch hit req")

  /* ======== Task allocation ======== */
  val retryReissue = !state.s_reissue.getOrElse(false.B) && !state.w_grant &&
    gotRetryAck && gotPCrdGrant
  val normalInitialIssue = !state.s_acquire && !readTxnInFlight
  io.tasks.txreq.valid := normalInitialIssue || retryReissue
  val rcompack_valid = !state.s_rcompack.get && state.w_grant &&
    // For issue B, CompAck must not be sent until all transfers of read data have been received.
    // For issue C and afterwards, CompAck is allowed to be sent after at least one CompData packet is received.
    afterIssueCOrElse(state.w_grantfirst, state.w_grantlast)
  io.tasks.txrsp.valid := rcompack_valid
  val probeAdmissionBlocked = sidecarDelegatedLatched || io.replResp.valid ||
    io.nestedwb.c_set_dirty
  io.tasks.source_b.valid := !probeAdmissionBlocked && !state.s_rprobe
  when(io.sourceBAccepted) {
    assert(req_valid && io.sourceBHold, "accepted normal Probe must retain its source lifetime")
  }
  io.tasks.txdat.valid := false.B
  io.tasks.txdat.bits := 0.U.asTypeOf(new TaskBundle)
  val grantIsData = odOpGen(req.opcode) === AccessAckData || odOpGen(req.opcode) === GrantData
  val replReadBlockedBySnoopOrReplace = io.replaceActive || replRespForThisParent
  val grantNeedsReplRead = !dirResult.hit && !state.w_replResp &&
    !replReadInFlight && !io.blockReplRead && !denied && !replReadBlockedBySnoopOrReplace
  val normalGrantPending = !state.s_refill && !grantIssued &&
    req_valid
  // A hit Grant waits for its request-line probe; a miss delegates the victim probe to ReplaceMSHR.
  val grantTaskPending = normalGrantPending &&
    state.w_grantlast && state.w_grant && (state.w_rprobeacklast || !dirResult.hit) &&
    (!grantIsData || gotGrantData || probeDirty || dirResult.hit)
  val grantTaskAllowed = retryTimes < backoffThreshold.U || backoffTimer === backoffCycles.U
  val internalReplRead = grantNeedsReplRead && (io.deferGrant || grantIssued)
  val replLookupInFlight = replReadInFlight || normalRefillRevalidateInFlight
  val replReadTaskPending = req_valid &&
    (grantTaskPending || grantIssued)
  io.replReadWanted := replReadTaskPending && grantTaskAllowed && !dirResult.hit && !state.w_replResp &&
    !replReadInFlight && !denied && !io.replaceActive
  val mp_repl_read_valid = replReadTaskPending && internalReplRead && grantTaskAllowed && !replLookupInFlight
  assert(
    !(mp_repl_read_valid && replReadBlockedBySnoopOrReplace),
    "normal MSHR must not select a replacer victim during snoop/child recovery"
  )
  val mp_grant_valid = grantTaskPending && !io.deferGrant && !releaseDataBlocksGrant && grantTaskAllowed &&
    !replLookupInFlight &&
    (!mergeInstallWithGrant || (state.w_replResp && victimSafeToOverwrite))
  // Revalidation is legal for hit-at-allocation contexts even when w_replResp is already set.
  val mp_normal_refill_revalidate_valid = req_valid && state.s_refill &&
    state.w_grantlast && state.w_grant && normalRefillRevalidateNeeded &&
    !normalRefillDroppedByToN && !replLookupInFlight && victimSafeToOverwrite &&
    !io.deferGrant && !nestedReleaseDirtyThisCycle
  val mp_normal_refill_commit_valid = req_valid && state.s_refill &&
    state.w_grantlast && state.w_grant && state.w_replResp &&
    !mergedGrantInstalled &&
    victimSafeToOverwrite && !normalRefillCommitIssued &&
    !normalRefillRevalidateNeeded && !replLookupInFlight &&
    !io.deferGrant && !nestedReleaseDirtyThisCycle &&
    (!postGrantReleaseSeen || postGrantReleasePayloadReady) &&
    !io.postGrantReleaseSnoopActive
  mainpipeIntent.valid := mp_repl_read_valid || mp_grant_valid ||
    mp_normal_refill_revalidate_valid || mp_normal_refill_commit_valid
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
    }.elsewhen(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 3.U) {
      nestedReleaseRefillPending := false.B
    }
  }
  // io.tasks.prefetchTrain.foreach(t => t.valid := !state.s_triggerprefetch.getOrElse(true.B))


  when(
    replReadTaskPending &&
      backoffTimer < backoffCycles.U &&
      retryTimes === backoffThreshold.U
  ) {
    backoffTimer := backoffTimer + 1.U
  }

  // resp and fwdState
  // *NOTICE: Snp*Fwd would enter MSHR on directory missing
  val orsp = io.tasks.txrsp.bits
  orsp := 0.U.asTypeOf(io.tasks.txrsp.bits.cloneType)
  orsp.tgtID := tgtid_rcompack
  orsp.srcID := 0.U
  orsp.txnID := txnid_rcompack
  orsp.dbID := 0.U
  orsp.opcode := CompAck
  orsp.resp := 0.U
  orsp.fwdState := 0.U
  orsp.traceTag := req.traceTag.get

  val a_task = {
    val oa = io.tasks.txreq.bits
    oa := 0.U.asTypeOf(io.tasks.txreq.bits.cloneType)
    oa.qos := Fill(QOS_WIDTH, 1.U(1.W)) - 1.U // TODO
    oa.tgtID := Mux(!state.s_reissue.getOrElse(false.B), srcid_retryack, 0.U)
    oa.srcID := 0.U
    oa.txnID := Cat(readTxnEpochGen, io.id(readTxnSlotBits - 1, 0))
    oa.returnNID := 0.U
    oa.stashNID := 0.U
    oa.stashNIDValid := false.B

    oa.opcode := ParallelPriorityMux(
      Seq(
        // AcquirePerm misses require ReadUnique so a later clean ProbeAck has valid backing data.
        (req_acquirePerm && dirResult.hit) -> MakeUnique,
        req_needT -> ReadUnique,
        req_needB /* Default */ -> ReadNotSharedDirty
      )
    )
    oa.size := log2Ceil(blockBytes).U
    oa.addr := Cat(req.tag, req.set, 0.U(offsetBits.W))
    oa.ns := enableNS.B
    oa.likelyshared := false.B
    oa.allowRetry := state.s_reissue.getOrElse(false.B)
    oa.order := OrderEncodings.None
    oa.pCrdType := Mux(!state.s_reissue.getOrElse(false.B), pcrdtype, 0.U)
    oa.expCompAck := true.B
    oa.memAttr := MemAttr(cacheable = true.B, allocate = true.B, device = false.B, ewa = true.B)
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
    ob.param := Mux(req_get && dirResult.hit && meta.state === TRUNK, toB, toN)
    ob.alias.foreach(_ := meta.alias.getOrElse(0.U))
    ob
  }

  val mp_repl_read, mp_grant, mp_normal_refill_revalidate, mp_normal_refill_commit =
    WireInit(0.U.asTypeOf(new TaskBundle))

  val mergeA = RegInit(false.B)
  when(io.aMergeTask.valid) {
    mergeA := true.B
  }.elsewhen(io.alloc.valid) {
    mergeA := false.B
  }
  val mergeAneedT = RegInit(false.B)
  when(io.aMergeTask.valid) {
    mergeAneedT := mergeAneedT || needT(io.aMergeTask.bits.opcode, io.aMergeTask.bits.param)
  }.elsewhen(io.alloc.valid) {
    mergeAneedT := false.B
  }
  val mergedAlias = aliasBitsOpt.map(_ => RegInit(0.U(aliasBitsOpt.get.W)))
  when(io.aMergeTask.valid) {
    mergedAlias.foreach(_ := io.aMergeTask.bits.alias.getOrElse(0.U))
  }.elsewhen(io.alloc.valid) {
    mergedAlias.foreach(_ := 0.U)
  }
  val mergedGrantNeedsT = mergeA && (mergeAneedT || req_promoteT)
  val mergedGrantClients = Fill(clientBits, mergeA)
  val mergedGrantState = Mux(mergedGrantNeedsT, TRUNK, BRANCH)
  val grantMetaState = Mux(
    mergeA,
    mergedGrantState,
    Mux(req_promoteT || req_needT, Mux(req_prefetch, TIP, TRUNK), BRANCH)
  )
  val commitMergedClientValid = Mux(
    grantMergeSemanticsLatched,
    grantMergedClientLatched,
    mergeA
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
    mp_grant.deferRefillWrite := true.B
    mp_grant.normalRefillRevalidate := false.B
    mp_grant.way := dirResult.way
    val grantAliasFinal = aliasFinal(mergeA)
    mp_grant.alias.foreach(_ := grantAliasFinal)
    mp_grant.aliasTask.foreach(_ := req.aliasTask.getOrElse(false.B))
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
    mp_grant.metaWen := false.B
    mp_grant.tagWen := false.B
    mp_grant.dsWen := false.B
    mp_grant.grantInstallEn := !mergeA && !denied && !nestedReleaseRefillPending
    mp_grant.fromL2pft.foreach(_ := req.fromL2pft.get)
    mp_grant.needHint.foreach(_ := false.B)
    mp_grant.replTask := grantNeedsReplRead
    mp_grant.cmoTask := false.B
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

  val mp_repl_read_task = {
    mp_repl_read := RegNext(mp_grant, 0.U.asTypeOf(mp_grant))
    mp_repl_read.refillOnly := mainpipeProposedVariant === 0.U || true.B
    mp_repl_read.deferRefillWrite := true.B
    mp_repl_read.metaWen := false.B
    mp_repl_read.tagWen := false.B
    mp_repl_read.dsWen := false.B
    mp_repl_read.readRefillData := false.B
    mp_repl_read.replTask := true.B
    mp_repl_read
  }

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
  when(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 2.U) {
    assert(
      mp_normal_refill_revalidate.normalRefillRevalidate &&
        mp_normal_refill_revalidate.refillOnly && mp_normal_refill_revalidate.deferRefillWrite &&
        mp_normal_refill_revalidate.replTask && !mp_normal_refill_revalidate.dsWen &&
        !mp_normal_refill_revalidate.tagWen && !mp_normal_refill_revalidate.metaWen,
      "normal refill revalidation must be Directory-only"
    )
  }

  val mp_normal_refill_commit_task = {
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
      dirty = gotDirty || probeDirty || dirResult.hit && meta.dirty,
      state = Mux(
        req_get,
        Mux(dirResult.hit, Mux(isT(meta.state), TIP, BRANCH), Mux(req_promoteT, TIP, BRANCH)),
        commitGrantMetaState
      ),
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

  val releaseDataAlreadyCommitted = nestedReleaseRefillPending && releaseDataDirectWriteSeen
  when(mergeInstallWithGrant) {
    mp_grant.tagWen := !dirResult.hit && !denied && !releaseDataAlreadyCommitted
    mp_grant.metaWen := !denied && !releaseDataAlreadyCommitted
    mp_grant.dsWen :=
      (gotGrantData || probeDirty && (req_get || req.aliasTask.getOrElse(false.B)) ||
        nestedReleaseRefillPending) && !denied && !releaseDataAlreadyCommitted
    mp_grant.meta := mp_normal_refill_commit.meta
  }

  when(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 3.U && nestedReleaseRefillPending) {
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
  when(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 3.U && probeDirty) {
    assert(normalRefillDroppedByToN || mp_normal_refill_commit.meta.dirty,
      "normal refill commit must preserve dirty ProbeAckData")
  }
  when(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 1.U) {
    assert(
      mergeInstallWithGrant ||
        (!mp_grant.refillOnly && mp_grant.deferRefillWrite &&
          !mp_grant.dsWen && !mp_grant.tagWen && !mp_grant.metaWen),
      "normal L1-visible grant must not update DataStorage or Directory"
    )
  }
  when(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 1.U && mergeInstallWithGrant) {
    assert(
      !mp_grant.refillOnly && mp_grant.deferRefillWrite &&
        (denied || releaseDataAlreadyCommitted ||
          mp_grant.metaWen || mp_grant.tagWen || mp_grant.dsWen),
      "a merge-mode grant must carry the refill install"
    )
  }
  XSPerfAccumulate("merged_grant_install_cnt",
    (io.tasks.mainpipe.fire && mainpipeIssuedVariant === 1.U && mergeInstallWithGrant) || io.grantInstallDone)
  when(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 3.U) {
    assert(
      mp_normal_refill_commit.refillOnly &&
        !mp_normal_refill_commit.deferRefillWrite &&
        !mp_normal_refill_commit.replaceTask,
      "normal refill commit must be a RefillBuf-only normal-context task"
    )
  }

  mainpipeIntent.bits := ParallelPriorityMux(
    Seq(
      (mainpipeProposedVariant === 0.U) -> mp_repl_read,
      (mainpipeProposedVariant === 1.U) -> mp_grant,
      (mainpipeProposedVariant === 2.U) -> mp_normal_refill_revalidate,
      (mainpipeProposedVariant === 3.U) -> mp_normal_refill_commit
    )
  )
  mainpipeIntent.bits.reqSource := req.reqSource
  mainpipeIntent.bits.isKeyword.foreach(_ := req.isKeyword.getOrElse(false.B))
  mainpipeVariant := PriorityEncoder(VecInit(Seq(
    mp_repl_read_valid, mp_grant_valid, mp_normal_refill_revalidate_valid,
    mp_normal_refill_commit_valid)).asUInt)
  val grantOpcode = odOpGen(req.opcode)
  val grantClass = grantOpcode === Grant || grantOpcode === GrantData ||
    grantOpcode === AccessAckData || grantOpcode === HintAck
  val mainpipeClass = Mux(mainpipeVariant === 1.U && grantClass, 1.U(3.W), 2.U(3.W))
  mainpipeProposedVariant := MainpipeLeaseIO.freeze(io.tasks.preparedLease,
    mainpipeIntent, io.tasks.mainpipe, mainpipeVariant, mainpipeClass, mainpipeInvalidate)
  mainpipeIssuedVariant := mainpipeProposedVariant

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
    assert(!readTxnInFlight, "a normal MSHR must not issue a second CHI read before the first retires")
    assert(normalInitialIssue || retryReissue,
      "a normal MSHR may issue a CHI read only initially or after RetryAck + PCrdGrant")
    readTxnInFlight := true.B
    readTxnEpochSent := readTxnEpochGen
    readTxnEpochGen := readTxnEpochGen + 1.U
    state.s_acquire := true.B
    when(!state.s_reissue.get) {
      state.s_reissue.get := true.B
      gotRetryAck := false.B
      gotPCrdGrant := false.B
    }
  }
  when(io.tasks.txrsp.fire) {
    state.s_rcompack.get := true.B
  }
  when(io.sourceBAccepted) {
    state.s_rprobe := true.B
    sourceBProbeIssued := true.B
  }
  // Advance state on actual MainPipe fire, using the operation retained by its lease.
  when(io.tasks.mainpipe.fire) {
    when(mainpipeIssuedVariant === 0.U) {
      state.s_retry := true.B
      replReadInFlight := true.B
    }.elsewhen(mainpipeIssuedVariant === 2.U) {
      normalRefillRevalidateInFlight := true.B
    }.elsewhen(mainpipeIssuedVariant === 1.U) {
      state.s_refill := true.B
      state.s_retry := true.B
      when(mergeInstallWithGrant) {
        normalRefillCommitIssued := true.B
        refillWritePending := !(denied || releaseDataAlreadyCommitted)
        mergedGrantInstalled := true.B
      }
      when(io.tasks.mainpipe.bits.replTask) {
        replReadInFlight := true.B
      }
      grantMergeSemanticsLatched := true.B
      grantMergedClientLatched := mergeA
      grantMergedNeedsTLatched := mergedGrantNeedsT
      grantMergedAliasLatched.foreach(_ := mergedAlias.getOrElse(0.U))
    }.elsewhen(mainpipeIssuedVariant === 3.U) {
      normalRefillCommitIssued := true.B
      refillWritePending := true.B
    }
  }

  // Only the SourceD handoff marks the L1-visible Grant complete; RequestArb acceptance may retry.
  val grantTimerCycle = RegInit(0.U(64.W))
  when(io.grantSent) {
    assert(req_valid, "grantSent must belong to a live non-CMO normal MSHR")
    assert(!grantIssued, "normal MSHR emitted more than one L1-visible Grant")
    grantIssued := true.B
    grantTimerCycle := timer
  }

  val commitDeferralCycles = timer - grantTimerCycle
  val commitDeferralFire = io.tasks.mainpipe.fire && mainpipeIssuedVariant === 3.U
  XSPerfHistogram("commit_deferral", commitDeferralCycles, commitDeferralFire,
    0, 512, 16)

  when(!io.alloc.valid) {
    when(io.refillWriteDone) {
      refillWritePending := false.B
      refillWayCommitHold := Mux(postGrantReleaseSnoopConsumed, 0.U, 6.U)
    }.elsewhen(refillWayCommitPending) {
      refillWayCommitHold := refillWayCommitHold - 1.U
    }
  }

  val readTxnTerminal = readTxnInFlight && state.w_grant && state.w_grantlast &&
    state.s_rcompack.getOrElse(true.B)
  io.readDBID.valid := req_valid && state.w_grantfirst
  io.readDBID.bits := txnid_rcompack
  io.readTxnEpoch := readTxnEpochSent
  when(!io.alloc.valid && readTxnTerminal) {
    readTxnInFlight := false.B
  }

  val c_resp = io.resps.sinkC
  val rxrsp = io.resps.rxrsp
  val rxdat = io.resps.rxdat
  // Probe core response
  when(c_resp.valid) {
    when(c_resp.bits.opcode === ProbeAck || c_resp.bits.opcode === ProbeAckData) {
      state.w_rprobeacklast := state.w_rprobeacklast || c_resp.bits.last
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

  }

  val rxdatIsU = rxdat.bits.resp.get === UC /*UC_UD*/
  val rxdatIsU_PD = rxdat.bits.resp.get === UC_PD

  val rxrspIsU = rxrsp.bits.resp.get === UC /*UC_UD*/

  // RXDAT
  val rxdatLastValid = WireInit(false.B)
  when(rxdat.valid) {
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
      when(!state.w_grant) {
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

    }
    when(rxrsp.bits.chiOpcode.get === RetryAck) {
      srcid_retryack := rxrsp.bits.srcID.getOrElse(0.U)
      pcrdtype := rxrsp.bits.pCrdType.getOrElse(0.U)
      gotRetryAck := true.B
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
      mergeInstallWithGrant := !mergeA && !replResp.hit &&
        (replResp.meta.state === INVALID || !replResp.meta.clients.orR)
      when(!normalRefillDroppedByToN) {
        when(replResp.hit) {
          assert(replResp.tag === req.tag, "normal refill revalidation hit must return the request tag")
        }
        dirResult.hit := replResp.hit
        dirResult.tag := replResp.tag
        dirResult.way := replResp.way
        dirResult.meta := replResp.meta

        when(!replResp.hit && replResp.meta.state =/= INVALID) {
          state.s_release := false.B
          state.w_releaseack := false.B
          when(replResp.meta.clients.orR) {
            state.s_rprobe := false.B
            state.w_rprobeacklast := false.B
            sourceBProbeIssued := false.B
          }
        }
      }
    }.otherwise {
      state.w_replResp := true.B
      mergeInstallWithGrant := !dirResult.hit && !mergeA &&
        (replResp.meta.state === INVALID || !replResp.meta.clients.orR)
      normalRefillRevalidateNeeded := false.B

      dirResult.tag := replResp.tag
      dirResult.way := replResp.way
      dirResult.meta := replResp.meta

      when(replResp.meta.state =/= INVALID) {
        state.s_release := false.B
        state.w_releaseack := false.B
        when(replResp.meta.clients.orR) {
          state.s_rprobe := false.B
          state.w_rprobeacklast := false.B
          sourceBProbeIssued := false.B
        }
      }
    }
  }

  val replRespMissInitial = io.replResp.valid && !replResp.retry && !replResp.hit &&
    !normalRefillRevalidateResp
  XSPerfAccumulate("repl_victim_invalid_cnt",
    req_valid && replRespMissInitial && replResp.meta.state === INVALID)
  XSPerfAccumulate("repl_victim_noclient_cnt",
    req_valid && replRespMissInitial && replResp.meta.state =/= INVALID && !replResp.meta.clients.orR)
  XSPerfAccumulate("repl_victim_hasclient_cnt",
    req_valid && replRespMissInitial && replResp.meta.state =/= INVALID && replResp.meta.clients.orR)
  XSPerfAccumulate("grant_folded_replread_cnt",
    io.tasks.mainpipe.fire && mainpipeIssuedVariant === 1.U && mp_grant.replTask)

  when(req_valid) {
    timer := timer + 1.U
  }

  val no_schedule = state.s_refill && (state.s_release || sidecarDetached) &&
    state.s_rcompack.getOrElse(true.B) &&
    state.s_reissue.getOrElse(true.B)
  val no_wait =
    (state.w_rprobeacklast || sidecarDetached) && state.w_grantlast && state.w_grant &&
      (state.w_releaseack || sidecarDetached) && state.w_replResp
  val mayRetire = no_schedule && no_wait &&
    normalRefillCommitIssued &&
    !refillWritePending && !refillWayCommitPending && !readTxnInFlight &&
    !replLookupInFlight && !io.postGrantReleaseSnoopActive && !io.pairLifetimeHold && !io.sourceBHold && !io.bufferWritePending && !io.sinkCOwnerHold
  val retirementPending = RegInit(false.B)
  retirementPending := mayRetire && !io.tasks.preparedLease.held.valid && !io.tasks.preparedLease.proposalHeld && !mainpipeInvalidate
  val will_free = retirementPending && mayRetire &&
    !io.tasks.preparedLease.held.valid && !io.tasks.preparedLease.proposalHeld && !mainpipeInvalidate
  io.tasks.preparedLease.canPrepare := req_valid && !mayRetire && !retirementPending
  MainpipeLeaseIO.acknowledge(io.tasks.preparedLease, req_valid, io.alloc.valid, io.tasks.mainpipe.fire)
  io.replaceParentRelease := will_free && req_valid
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
  // A miss owns its refill way only after a replacement response is accepted.
  val refillWayReserved = !dirResult.hit && state.w_replResp &&
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
  io.status.bits.w_c_resp := !state.w_rprobeacklast
  io.status.bits.w_d_resp := !state.w_grantlast || !state.w_grant || !state.w_releaseack
  io.status.bits.will_free := will_free
  io.status.bits.is_miss := !dirResult.hit
  io.status.bits.is_prefetch := req_prefetch
  io.status.bits.reqSource := req.reqSource

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
  val committedGrantTransfersClient = req_acquire || Mux(
    grantMergeSemanticsLatched,
    grantMergedClientLatched,
    mergeA
  )
  val grantAccepted = state.s_refill || grantIssued
  io.msInfo.bits.grantPending := req_valid && committedGrantTransfersClient &&
    grantAccepted &&
    (!normalRefillCommitIssued || refillWritePending || refillWayCommitPending)
  when(io.grantSent) {
    assert(grantAccepted, "SourceD Grant must follow registered RequestArb acceptance")
  }
  io.msInfo.bits.w_grantfirst := state.w_grantfirst
  io.msInfo.bits.w_grantlast := state.w_grantlast
  io.msInfo.bits.w_grant := state.w_grant
  io.msInfo.bits.s_release := state.s_release
  io.msInfo.bits.s_refill := state.s_refill
  io.msInfo.bits.w_releaseack := state.w_releaseack
  io.msInfo.bits.w_replResp := state.w_replResp
  io.msInfo.bits.w_rprobeacklast := state.w_rprobeacklast
  io.msInfo.bits.postGrantReleaseHold := postGrantReleaseSeen
  io.msInfo.bits.refillCommitPending :=
    !normalRefillCommitIssued || refillWritePending || refillWayCommitPending
  io.msInfo.bits.refillCommitLanded := normalRefillCommitIssued && !refillWritePending
  io.msInfo.bits.replaceData := isT(meta.state) && meta.dirty
  io.msInfo.bits.releaseToClean := false.B
  io.msInfo.bits.blocksSnoop := false.B
  io.msInfo.bits.channel := req.channel

  assert(!(c_resp.valid && !io.status.bits.w_c_resp))
  assert(!(rxrsp.valid && rxrsp.bits.chiOpcode.get =/= PCrdGrant && !io.status.bits.w_d_resp))

  /* ======== Handling Nested C ======== */
  // for A miss, only when replResp do we finally choose a way, allowing nested C
  // for A-alias, always allowing nested C (state.w_replResp === true.B)
  val nestedwb_match = req_valid &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag &&
    state.w_replResp &&
    dirResult.meta.state =/= INVALID &&
    (req_mayRepl || dirResult.hit) // exclude non-repl tasks (e.g. Forward Snoop) on directory miss
  val nestedwb_hit_match = req_valid && dirResult.hit &&
    dirResult.set === io.nestedwb.set &&
    dirResult.tag === io.nestedwb.tag
  // A pre-read local hit lost to ToN starts its first lower read; an existing read is never replayed.
  val lostLocalHit = nestedwb_hit_match && io.nestedwb.b_toN.get && req.fromA &&
    state.s_acquire && !readTxnInFlight && !state.s_refill &&
    !grantIssued && !io.grantSent && !gotGrantData && !probeDirty
  val postGrantToN = nestedwb_hit_match && io.nestedwb.b_toN.get && req.fromA &&
    (grantIssued || io.grantSent)

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
      state.w_replResp := false.B
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
    state.w_replResp := true.B
  }
  assert(
    !(lostLocalHit && (readTxnInFlight || gotGrantData || probeDirty || grantIssued || io.grantSent)),
    "lostLocalHit must be a pre-read, pre-grant local-hit invalidation"
  )
  when(io.tasks.mainpipe.fire && mainpipeIssuedVariant === 3.U &&
    (normalRefillDroppedByToN || postGrantReleaseSnoopConsumed)) {
    assert(
      !mp_normal_refill_commit.metaWen && !mp_normal_refill_commit.tagWen &&
        !mp_normal_refill_commit.dsWen && !mp_normal_refill_commit.readRefillData,
      "a snoop-consumed normal refill must retire without re-installing RefillBuf"
    )
  }
  when(nestedReleaseTargetsRefill && io.nestedwb.c_set_dirty) {
    gotDirty := true.B
  }
  val nestedReleaseNeedsReplace = nestedwb_match && io.nestedwb.c_set_dirty &&
    !nestedReleaseTargetsRefill
  io.nestedReleaseNeedsReplace := nestedReleaseNeedsReplace
  io.nestedwbData := nestedwb_match && io.nestedwb.c_set_dirty &&
    (sidecarDelegatedLatched || nestedReleaseNeedsReplace)

  dontTouch(state)

  val leaseOwnerControls = Cat(io.replaceActive, io.replaceAccepted,
    io.replaceDetached, io.replaceRefillReady, io.deferGrant,
    io.postGrantReleaseSnoopActive, io.blockReplRead)
  val leaseOwnerControlsChanged = leaseOwnerControls =/=
    RegNext(leaseOwnerControls, 0.U(leaseOwnerControls.getWidth.W))
  val leaseNestedEvent = io.nestedwb.c_set_dirty || io.nestedwb.c_set_tip ||
    io.nestedwb.b_inv_dirty || io.nestedwb.b_toN.getOrElse(false.B) ||
    io.nestedwb.b_toB.getOrElse(false.B) || io.nestedwb.b_toClean.getOrElse(false.B)
  mainpipeInvalidate := io.bufferWritePending || io.alloc.valid || io.resps.sinkC.valid || io.resps.rxrsp.valid ||
    io.resps.rxdat.valid || io.replResp.valid || io.aMergeTask.valid ||
    io.replaceDone || io.refillWriteDone || io.grantSent ||
    io.releaseDataBeforeProbeAck || io.postGrantReleaseData ||
    io.postGrantReleaseSnoopToN || io.postGrantReleaseSnoopToB ||
    io.postGrantReleaseSnoopToClean || io.releaseDataDirectWrite || io.grantInstallDone ||
    io.pCrd.grant || leaseNestedEvent || leaseOwnerControlsChanged ||
    io.tasks.txreq.fire || io.tasks.txrsp.fire || io.sourceBAccepted

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
