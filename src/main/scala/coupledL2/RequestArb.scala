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
import utility._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import org.chipsalliance.cde.config.Parameters
import xscache.coupledL2._
import xscache.chi.HasCHIOpcodes

class RequestArb(implicit p: Parameters) extends L2Module with HasCHIOpcodes {

  val io = IO(new Bundle() {
    /* receive incoming tasks */
    val sinkA = Flipped(DecoupledIO(new TaskBundle))
    val aCommit = Output(Bool())
    val ATag = Input(UInt(tagBits.W)) // !TODO: very dirty, consider optimize structure
    val ASet = Input(UInt(setBits.W)) // To pass A entrance status to MP for blockA-info of ReqBuf
    val s1Entrance = ValidIO(new L2Bundle {
      val set = UInt(setBits.W)
    })

    val sinkB = Flipped(DecoupledIO(new TaskBundle))
    val sinkC = Flipped(DecoupledIO(new TaskBundle))
    val mshrTask = Flipped(DecoupledIO(new TaskBundle))
    val mshrLeaseToken = Input(new MainpipeLeaseToken)
    val taskLeaseToken_s2 = Output(new MainpipeLeaseToken)
    // B is captured first, then reserves its service/ReleaseBuf slot, and
    // only afterwards executes the Directory read. This event is registered.
    val bServiceStart = ValidIO(new TaskBundle)
    val sidecarReservationHold = Output(Bool())
    val reservedB = Output(Bool())
    val contextReservation = Output(Bool())
    val normalReserveRequest = DecoupledIO(new TaskBundle)
    val normalReserveAck = Flipped(ValidIO(new NormalSlotReservation))
    val normalReserveConsume = Output(Bool())
    val normalReserveCancel = Output(Bool())

    /* read/write directory */
    val dirRead_s1 = DecoupledIO(new DirRead()) // To directory, read meta/tag

    /* send task to mainpipe */
    val taskToPipe_s2 = ValidIO(new TaskBundle())
    /* send s1 task info to mainpipe to help hint */
    val mshrHintQInfo = ValidIO(new TaskBundle)
    val sinkCHintQInfo = ValidIO(new TaskBundle)
    /* hint staging queue status from mainpipe, to block hint-generating fires */
    val hint_s1QueueFull = Input(Bool())

    /* send mshrBuf read request */
    val refillBufRead_s2 = ValidIO(new MSHRBufRead)
    val releaseBufRead_s2 = ValidIO(new MSHRBufRead)

    /* status of each pipeline stage */
    val status_s1 = Output(new PipeEntranceStatus) // set & tag of entrance status
    val status_vec = Vec(2, ValidIO(new PipeStatus))
    val status_vec_toTX = Vec(2, ValidIO(new PipeStatusWithCHI))

    /* handle set conflict, capacity conflict */
    val fromMSHRCtl = Input(new BlockInfo())
    val fromMainPipe = Input(new BlockInfo())
    // Refill-only normal commits read the same RefillBuf return path as
    // SourceD tasks. They wait in S1 until preceding data responses drain.
    val blockRefillOnlyCommit = Input(Bool())
    val fromGrantBuffer = Input(new Bundle() {
      val blockSinkReqEntrance = new BlockInfo()
      val blockMSHRReqEntrance = Bool()
    })
    val fromTXDAT = Input(new TXDATBlockBundle)
    val fromTXRSP = Input(new TXRSPBlockBundle)
    val fromTXREQ = Input(new TXBlockBundle)

    /* MSHR Status */
    val msInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
  })

  /* ======== Reset ======== */
  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  /* block reqs when reset */
  when(!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when(resetIdx === 0.U) {
    resetFinish := true.B
  }

  val s0_fire = Wire(Bool())
  val s1_fire = Wire(Bool())
  val s1_cango = Wire(Bool())
  val s2_ready = Wire(Bool())
  val mshr_task_s1 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val mshrLeaseToken_s1 = Reg(new MainpipeLeaseToken)
  // A channel task is first selected locally, then admitted to the Directory
  // through this one-entry token.  The token contains the narrow read payload
  // plus the owner/provenance identity of an immutable source. Capturing A
  // commits RequestBuffer's hold; B is already locally retained and C uses
  // a registered arbiter hold. The complete payload stays on those register
  // outputs until the same source/Directory handshake consumes the command.
  class ChannelDirReadAdmissionToken extends Bundle {
    val valid = Bool()
    val source = UInt(2.W) // 0=C, 1=B, 2=A (sinkValids priority order)
    val dirRead = new DirRead
    val owner = UInt(mshrBits.W)
    val generation = UInt(8.W)
    val sequence = UInt(8.W)
    val paired = Bool()
    // Payload provenance is not used to rebuild the Directory request, but is
    // locked and rechecked so a held token cannot be paired with a different
    // RefillBuf/ReleaseBuf owner after an upstream context transition.
    val mshrContext = UInt(2.W)
    val refillToReleaseCopy = Bool()
    val refillBufReadId = UInt(mshrBits.W)
    val useProbeData = Bool()
    val readProbeDataDown = Bool()
    val readRefillData = Bool()
    val readDataFromDS = Bool()
    val snpHitRelease = Bool()
    val snpHitReleaseIdx = UInt(mshrBits.W)
  }
  val channelDirReadAdmissionToken = RegInit(0.U.asTypeOf(new ChannelDirReadAdmissionToken))
  val channelDirReadAdmissionValid = channelDirReadAdmissionToken.valid
  val channelDirReadAdmissionFire = Wire(Bool())
  // D109's replacement-read hold keeps its own fire event; this wire remains
  // distinct so a channel token cannot advance mshr_task_s1 by accident.
  // Directory read admission is owned locally after capture.  The hold keeps
  // the complete request and its lease provenance stable while the Directory
  // port is blocked by a same-bank write; upstream valid/ready signals only
  // observe the one-entry occupancy and never the Directory ready directly.
  val dirReadAdmissionValid = RegInit(false.B)
  val dirReadAdmissionBits = RegInit(0.U.asTypeOf(new DirRead()))
  val dirReadAdmissionTask = RegInit(0.U.asTypeOf(new TaskBundle()))
  val dirReadAdmissionToken = RegInit(0.U.asTypeOf(new MainpipeLeaseToken))
  val dirReadAdmissionFire = Wire(Bool())
  // Reserve the fixed-latency Directory result/apply slot before S1. This
  // short local execution slot ends after S3 applies; response collection,
  // ProbeAck/GrantAck and outgoing queues remain independent of it.
  val localTransitionCycles = RegInit(0.U(3.W))
  val localTransitionIdle = localTransitionCycles === 0.U
  // Registered admission turns keep the two ingress handshakes exclusive.
  // A live MSHR valid must not decide B.ready: B reserves a sidecar whose
  // task eligibility can in turn revoke that MSHR valid in the same cycle.
  val admitMshrTurn = RegInit(false.B)
  val bPending = RegInit(0.U.asTypeOf(Valid(new TaskBundle)))
  val bServiceReserved = RegInit(false.B)
  val bExecute = Wire(Bool())
  io.bServiceStart.valid := bPending.valid && !bServiceReserved
  io.bServiceStart.bits := bPending.bits
  // This finite capture phase is chosen by registered scheduling state,
  // before looking at live B.valid. It prevents a sidecar from issuing or
  // retiring at the edge at which a B snapshot may acquire its service.
  io.reservedB := bPending.valid
  io.sidecarReservationHold := (bPending.valid && !bServiceReserved) ||
    (localTransitionIdle && !admitMshrTurn && !mshr_task_s1.valid && !bPending.valid)
  when(localTransitionIdle && !mshr_task_s1.valid && !bPending.valid &&
    !channelDirReadAdmissionValid) {
    admitMshrTurn := !admitMshrTurn
  }
  when(s1_fire) {
    localTransitionCycles := 4.U
  }.elsewhen(!localTransitionIdle) {
    localTransitionCycles := localTransitionCycles - 1.U
  }

  val s1_needs_replRead = mshr_task_s1.valid && mshr_task_s1.bits.fromA && mshr_task_s1.bits.replTask && (
    mshr_task_s1.bits.opcode === Grant ||
      mshr_task_s1.bits.opcode === GrantData ||
      mshr_task_s1.bits.opcode === AccessAckData ||
      mshr_task_s1.bits.opcode === HintAck
  )
  val normalRefillOnlyCommit_s1 = mshr_task_s1.valid &&
    mshr_task_s1.bits.mshrContext === 0.U &&
    mshr_task_s1.bits.refillOnly && mshr_task_s1.bits.readRefillData &&
    !mshr_task_s1.bits.replaceTask
  val mshr_refillOnlyCommit_stall = normalRefillOnlyCommit_s1 && io.blockRefillOnlyCommit

  // A deferred refill task may use RequestArb only to obtain a ReplacerResult.
  // Its later normal-context commit owns the DataStorage/Directory write.
  val deferredReplRead = mshr_task_s1.bits.deferRefillWrite
  assert(
    !s1_needs_replRead || mshr_task_s1.bits.opcode =/= GrantData ||
      mshr_task_s1.bits.dsWen || deferredReplRead,
    "replTask of GrantData without DataStorage write must defer refill commit"
  )

  assert(
    !s1_needs_replRead || mshr_task_s1.bits.opcode =/= AccessAckData ||
      mshr_task_s1.bits.dsWen || deferredReplRead,
    "replTask of AccessAckData without DataStorage write must defer refill commit"
  )

  assert(
    !s1_needs_replRead || mshr_task_s1.bits.opcode =/= HintAck ||
      mshr_task_s1.bits.dsWen || deferredReplRead,
    "replTask of HintAck without DataStorage write must defer refill commit"
  )

  /* ======== Stage 0 ======== */
  // if mshr_task_s1 is replRead, it might stall and wait for dirRead.ready, so we block new mshrTask from entering
  // TODO: will cause msTask path vacant for one-cycle after replRead, since not use Flow so as to avoid ready propagation
  val mshrTaskReadyBasic = localTransitionIdle && admitMshrTurn && !mshr_task_s1.valid && !bPending.valid &&
    !channelDirReadAdmissionValid &&
    !io.fromGrantBuffer.blockMSHRReqEntrance &&
    !io.fromTXDAT.blockMSHRReqEntrance &&
    !io.fromTXRSP.blockMSHRReqEntrance &&
    !io.fromTXREQ.blockMSHRReqEntrance

  s0_fire := io.mshrTask.valid && io.mshrTask.ready

  /* ======== Stage 1 ======== */
  /* latch mshr_task from s0 to s1 */
  val mshr_replRead_stall =
    mshr_task_s1.valid && s1_needs_replRead && (!dirReadAdmissionFire || io.fromMainPipe.blockG_s1)
  mshr_task_s1.valid := mshr_task_s1.valid && !s1_fire || s0_fire

  when(s0_fire) {
    mshr_task_s1.bits := io.mshrTask.bits
    mshrLeaseToken_s1 := io.mshrLeaseToken
  }

  /* Channel interaction from s1 */
  val A_task = io.sinkA.bits
  // The upstream candidate is still revocable until its real S1 handshake.
  // A registered claim binds one concrete slot before the Directory read.
  // Reconfirm the complete retained request: input-flow and CMO-all candidates
  // may change while unconsumed, including optional training/alias fields.
  val normalReservedTask = io.normalReserveAck.bits.task
  // CMO-all enumerates set/way internally; unused fields are sourced from
  // the blocked external A bus and need not stay stable. Its logical identity
  // is that set/way, and execution uses the retained complete payload below.
  val sameCmoAllLine = A_task.cmoAll && normalReservedTask.cmoAll &&
    A_task.set === normalReservedTask.set && A_task.way === normalReservedTask.way
  val normalReservationMatches = io.sinkA.valid &&
    (sameCmoAllLine || A_task.asUInt === normalReservedTask.asUInt)
  val normalYieldForB = RegInit(false.B)
  val normalReserved = io.normalReserveAck.valid && normalReservationMatches && !normalYieldForB
  io.normalReserveCancel := io.normalReserveAck.valid && !channelDirReadAdmissionValid &&
    (!normalReservationMatches || normalYieldForB)
  io.normalReserveConsume := io.sinkA.fire
  io.normalReserveRequest.bits := A_task
  val B_task = Mux(bPending.valid, bPending.bits, io.sinkB.bits)
  val C_task = io.sinkC.bits
  val block_A =
    io.fromMSHRCtl.blockA_s1 || io.fromMainPipe.blockA_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockA_s1
  // An older claim keeps priority while A can execute. If a newly live
  // sidecar/snoop blocks that A, return the unconsumed claim before admitting
  // a later B. Otherwise B can be the response that releases A's blocker,
  // while normalReserved forbids that B forever (and may hold its last slot).
  when(!io.normalReserveAck.valid) {
    normalYieldForB := false.B
  }.elsewhen(!channelDirReadAdmissionValid && normalReservationMatches && block_A && io.sinkB.valid) {
    normalYieldForB := true.B
  }
  val block_B =
    io.fromMSHRCtl.blockB_s1 || io.fromMainPipe.blockB_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1 ||
      io.fromTXDAT.blockSinkBReqEntrance ||
      io.fromTXRSP.blockSinkBReqEntrance
  val block_C =
    io.fromMSHRCtl.blockC_s1 || io.fromMainPipe.blockC_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockC_s1

//  val noFreeWay = Wire(Bool())

  // A captured B owns its capacity reservation; do not apply the pool-full
  // test to that reservation again while it waits for execution. Transport
  // space and existing GrantAck conflicts can still delay the local read.
  val blockReservedB = io.fromMainPipe.blockB_s1 ||
    io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1
  val sinkValids = VecInit(
    Seq(
      io.sinkC.valid && !block_C && !bPending.valid,
      bPending.valid && bServiceReserved && !blockReservedB,
      io.sinkA.valid && normalReserved && !block_A && !bPending.valid
    )
  ).asUInt

  // Select one channel candidate without consulting the Directory ready.  A
  // candidate can therefore be captured into the registered token even while
  // the set bank is occupied by a write.  The source ready signals below stay
  // low until the token's Directory handshake, so each upstream payload is
  // stable for the whole hold interval.
  val channelCandidateValid = localTransitionIdle &&
    (bPending.valid || !admitMshrTurn) && !mshr_task_s1.valid &&
    sinkValids.orR && resetFinish &&
    !channelDirReadAdmissionValid
  val channelCandidateSource = OHToUInt(PriorityEncoderOH(sinkValids))
  io.aCommit := channelCandidateValid && channelCandidateSource === 2.U
  val channelCandidateTask = Wire(new TaskBundle())
  channelCandidateTask := ParallelPriorityMux(sinkValids, Seq(C_task, B_task, A_task))
  when(sinkValids(2) && !sinkValids(0) && !sinkValids(1)) {
    channelCandidateTask := normalReservedTask
    channelCandidateTask.mshrId := io.normalReserveAck.bits.id
  }

  val channelCandidateOwner = Mux(channelCandidateSource === 2.U,
    io.normalReserveAck.bits.id, channelCandidateTask.mshrId)
  val channelCandidateGeneration = Mux(channelCandidateSource === 2.U,
    io.normalReserveAck.bits.token.generation, 0.U(8.W))
  val channelCandidateSequence = Mux(channelCandidateSource === 2.U,
    io.normalReserveAck.bits.token.sequence, 0.U(8.W))
  val channelCandidatePaired = channelCandidateTask.snpHitRelease ||
    channelCandidateTask.refillToReleaseCopy || channelCandidateTask.mshrContext =/= 0.U

  def channelDirReadOf(task: TaskBundle): DirRead = {
    val bits = Wire(new DirRead())
    bits.set := task.set
    bits.tag := task.tag
    bits.wayMask := Fill(cacheParams.ways, "b1".U)
    bits.replacerInfo.opcode := task.opcode
    bits.replacerInfo.channel := task.channel
    bits.replacerInfo.reqSource := task.reqSource
    bits.replacerInfo.refill_prefetch := false.B
    bits.refill := false.B
    bits.normalRefillRevalidate := false.B
    bits.mshrId := task.mshrId
    bits.cmoAll := task.cmoAll
    bits.cmoWay := task.way
    bits
  }
  val channelCandidateDirReadBits = channelDirReadOf(channelCandidateTask)
  val channelTokenTask = Wire(new TaskBundle())
  channelTokenTask := MuxLookup(channelDirReadAdmissionToken.source,
    0.U.asTypeOf(new TaskBundle()))(Seq(
      0.U -> C_task,
      1.U -> B_task,
      2.U -> normalReservedTask
    ))
  when(channelDirReadAdmissionToken.source === 2.U) {
    channelTokenTask := normalReservedTask
    channelTokenTask.mshrId := io.normalReserveAck.bits.id
  }
  val channelTokenDirReadBits = channelDirReadOf(channelTokenTask)
  val channelTokenSourceValid = Mux(channelDirReadAdmissionToken.source === 0.U,
    io.sinkC.valid, Mux(channelDirReadAdmissionToken.source === 1.U,
      bPending.valid && bServiceReserved, io.sinkA.valid && io.normalReserveAck.valid))
  val channelTokenOwnerCurrent = Mux(channelDirReadAdmissionToken.source === 2.U,
    io.normalReserveAck.bits.id, channelTokenTask.mshrId)
  val channelTokenGenerationCurrent = Mux(channelDirReadAdmissionToken.source === 2.U,
    io.normalReserveAck.bits.token.generation, 0.U(8.W))
  val channelTokenSequenceCurrent = Mux(channelDirReadAdmissionToken.source === 2.U,
    io.normalReserveAck.bits.token.sequence, 0.U(8.W))
  val channelTokenPairedCurrent = channelTokenTask.snpHitRelease ||
    channelTokenTask.refillToReleaseCopy || channelTokenTask.mshrContext =/= 0.U
  val channelTokenProvenanceCurrent =
    channelTokenTask.mshrContext === channelDirReadAdmissionToken.mshrContext &&
    channelTokenTask.refillToReleaseCopy === channelDirReadAdmissionToken.refillToReleaseCopy &&
    channelTokenTask.refillBufReadId === channelDirReadAdmissionToken.refillBufReadId &&
    channelTokenTask.useProbeData === channelDirReadAdmissionToken.useProbeData &&
    channelTokenTask.readProbeDataDown === channelDirReadAdmissionToken.readProbeDataDown &&
    channelTokenTask.readRefillData === channelDirReadAdmissionToken.readRefillData &&
    channelTokenTask.readDataFromDS === channelDirReadAdmissionToken.readDataFromDS &&
    channelTokenTask.snpHitRelease === channelDirReadAdmissionToken.snpHitRelease &&
    channelTokenTask.snpHitReleaseIdx === channelDirReadAdmissionToken.snpHitReleaseIdx
  // Ownership was transferred at capture. These comparisons are assertions,
  // never functional valid/ready gates: a later candidate or block signal
  // cannot revoke a command which already reserved its source and capacity.
  val channelTokenIdentityMatches =
    channelDirReadAdmissionToken.dirRead.asUInt === channelTokenDirReadBits.asUInt &&
    channelDirReadAdmissionToken.owner === channelTokenOwnerCurrent &&
    channelDirReadAdmissionToken.generation === channelTokenGenerationCurrent &&
    channelDirReadAdmissionToken.sequence === channelTokenSequenceCurrent &&
    channelDirReadAdmissionToken.paired === channelTokenPairedCurrent &&
    channelTokenProvenanceCurrent
  when(channelDirReadAdmissionValid) {
    assert(channelTokenSourceValid && channelTokenIdentityMatches,
      "a channel Directory token must retain its committed source and provenance")
  }

  // Background-class MSHR tasks (sidecar release/copyback, CMO; qosClass==4)
  // must not preempt sink requests at s1: sinks are on the L1/openLLC
  // critical path while background writeback work is not. The MSHRCtl-level
  // starvation guard promotes a starving background task out of class 4, so
  // this cannot deadlock.
  val mshrTaskBgDefer = io.mshrTask.valid && io.mshrTask.bits.qosClass === 4.U && sinkValids.orR
  io.mshrTask.ready := mshrTaskReadyBasic && !mshrTaskBgDefer
  XSPerfAccumulate("reqarb_bg_defer_cnt", mshrTaskBgDefer)

  // TODO: A Hint is allowed to enter if !s2_ready for mcp2_stall

  // This is the post-token execution event.  Directory ready is no longer
  // part of A/B/C admission; it is observed only after the registered token
  // has been presented to Directory.
  val sink_ready_basic = channelDirReadAdmissionFire

  io.normalReserveRequest.valid := localTransitionIdle && resetFinish &&
    !mshr_task_s1.valid && !bPending.valid && io.sinkA.valid && !block_A &&
    !io.normalReserveAck.valid && !sinkValids(0) && !io.sinkB.valid &&
    !channelDirReadAdmissionValid
  // An acknowledged A claim predates a later B candidate. Let it execute
  // before capturing that B: a CMO claim can reserve the last eligible S/R
  // index, so making the A wait for that blocked B would deadlock.
  // The registered token has already chosen its channel owner. A later C
  // candidate must not veto this A's consume after the same token has fired
  // Directory and advanced S1. Eligibility was committed at token capture;
  // source consumption is the very same event as the retained token fire.
  io.sinkA.ready := sink_ready_basic && channelDirReadAdmissionToken.source === 2.U
  when(io.sinkA.fire) {
    assert(io.dirRead_s1.fire && normalReserved,
      "a normal Directory read must consume its registered concrete slot")
    assert(sameCmoAllLine || A_task.asUInt === normalReservedTask.asUInt,
      "normal reservation must retain the complete upstream request")
  }
  // Upstream handshake captures the complete B, without reading Directory.
  // The next stage creates service/slot ownership; actual execution is a
  // separate handshake and never has to preempt ReplaceMSHR in the same cycle.
  io.sinkB.ready := localTransitionIdle && !admitMshrTurn && resetFinish &&
    !mshr_task_s1.valid && !bPending.valid && !normalReserved && !block_B && !sinkValids(0) &&
    !channelDirReadAdmissionValid
  io.sinkC.ready := sink_ready_basic && channelDirReadAdmissionToken.source === 0.U
  bExecute := sink_ready_basic && channelDirReadAdmissionToken.source === 1.U
  val channelConsumes = VecInit(Seq(io.sinkC.fire, bExecute, io.sinkA.fire))
  assert(channelDirReadAdmissionFire === channelConsumes.asUInt.orR,
    "a channel Directory token must execute and consume its owner atomically")
  assert(PopCount(channelConsumes) <= 1.U,
    "one channel Directory token must consume exactly one channel owner")
  when(io.sinkB.fire) {
    bPending.valid := true.B
    bPending.bits := io.sinkB.bits
    bServiceReserved := false.B
  }.elsewhen(io.bServiceStart.valid) {
    bServiceReserved := true.B
  }.elsewhen(bExecute) {
    bPending.valid := false.B
    bServiceReserved := false.B
  }
  assert(!bExecute || bServiceReserved, "B executed before registered service reservation")

  val chnl_task_s1 = Wire(Valid(new TaskBundle()))
  // A channel task reaches S1 only when its registered Directory token is
  // valid and the Directory accepts that token.  Until then the upstream
  // Decoupled source sees ready low and retains the complete payload.
  // Keep a held token visible to the local pipeline even during a data-bank
  // bubble.  `s2_ready` gates the actual S1 fire below; including it here
  // would feed task_s1.valid back into ds_mcp2_stall and create a FIRRTL
  // combinational cycle.
  chnl_task_s1.valid := channelDirReadAdmissionValid && !mshr_task_s1.valid
  chnl_task_s1.bits := channelTokenTask

  // mshr_task_s1 is s1_[reg]
  // task_s1 is [wire] to s2_reg
  val task_s1 = Mux(mshr_task_s1.valid, mshr_task_s1, chnl_task_s1)
  val s1_to_s2_valid = task_s1.valid && !mshr_replRead_stall

  s1_cango := task_s1.valid && !mshr_replRead_stall && !mshr_refillOnlyCommit_stall &&
    Mux(mshr_task_s1.valid, true.B, io.dirRead_s1.ready)
  s1_fire := s1_cango && s2_ready
  when(!mshr_task_s1.valid) {
    assert(s1_fire === channelDirReadAdmissionFire,
      "a channel S1 advance must be the retained Directory token fire")
  }
  io.contextReservation := s1_fire

  io.mshrHintQInfo.valid := mshr_task_s1.valid && !mshr_replRead_stall && !mshr_refillOnlyCommit_stall && s2_ready
  io.mshrHintQInfo.bits := mshr_task_s1.bits
  io.sinkCHintQInfo.valid := io.sinkC.fire
  io.sinkCHintQInfo.bits := io.sinkC.bits

  /* Meta read request */
  // Build the request from the task being captured, then register the whole
  // request before exposing it to Directory.  The hold is the sole owner of
  // this read until Directory.fire.
  val dirReadCapture = !dirReadAdmissionValid && mshr_task_s1.valid && s1_needs_replRead &&
    s2_ready && !io.fromMainPipe.blockG_s1
  val dirReadCaptureTask = mshr_task_s1.bits
  val dirReadCaptureIsRepl = mshr_task_s1.valid && s1_needs_replRead
  val dirReadCaptureBits = Wire(new DirRead())
  dirReadCaptureBits.set := dirReadCaptureTask.set
  dirReadCaptureBits.tag := dirReadCaptureTask.tag
  // invalid way which causes mshr_retry
  // TODO: random waymask can be used to avoid multi-way conflict
  dirReadCaptureBits.wayMask := Mux(
    mshr_task_s1.valid && mshr_task_s1.bits.mshrRetry,
    (~(1.U(cacheParams.ways.W) << mshr_task_s1.bits.way)),
    Fill(cacheParams.ways, "b1".U)
  )
  dirReadCaptureBits.replacerInfo.opcode := dirReadCaptureTask.opcode
  dirReadCaptureBits.replacerInfo.channel := dirReadCaptureTask.channel
  dirReadCaptureBits.replacerInfo.reqSource := dirReadCaptureTask.reqSource
  dirReadCaptureBits.replacerInfo.refill_prefetch := dirReadCaptureIsRepl &&
    (mshr_task_s1.bits.opcode === HintAck && mshr_task_s1.bits.dsWen)
  dirReadCaptureBits.refill := dirReadCaptureIsRepl
  dirReadCaptureBits.normalRefillRevalidate := mshr_task_s1.valid && dirReadCaptureTask.normalRefillRevalidate
  dirReadCaptureBits.mshrId := dirReadCaptureTask.mshrId
  dirReadCaptureBits.cmoAll := dirReadCaptureTask.cmoAll
  dirReadCaptureBits.cmoWay := dirReadCaptureTask.way

  val mshrReplReadActive = mshr_task_s1.valid && s1_needs_replRead
  io.dirRead_s1.valid := Mux(
    mshrReplReadActive,
    dirReadAdmissionValid && (!dirReadAdmissionBits.refill || !io.fromMainPipe.blockG_s1),
    channelDirReadAdmissionValid &&
      !mshr_task_s1.valid && s2_ready
  )
  io.dirRead_s1.bits := Mux(mshrReplReadActive,
    dirReadAdmissionBits, channelDirReadAdmissionToken.dirRead)
  // D109 replacement reads and the new A/B/C token have separate ownership
  // events.  A channel token is consumed only when the Directory really
  // accepts its registered payload.
  dirReadAdmissionFire := mshrReplReadActive && io.dirRead_s1.fire
  channelDirReadAdmissionFire := !mshr_task_s1.valid &&
    channelDirReadAdmissionValid &&
    s2_ready && io.dirRead_s1.ready
  dirReadAdmissionValid := dirReadAdmissionValid && !dirReadAdmissionFire || dirReadCapture
  when(dirReadCapture) {
    dirReadAdmissionBits := dirReadCaptureBits
    dirReadAdmissionTask := dirReadCaptureTask
    dirReadAdmissionToken := mshrLeaseToken_s1
  }

  when(channelDirReadAdmissionFire) {
    channelDirReadAdmissionToken.valid := false.B
  }
  when(channelCandidateValid) {
    channelDirReadAdmissionToken.valid := true.B
    channelDirReadAdmissionToken.source := channelCandidateSource
    channelDirReadAdmissionToken.dirRead := channelCandidateDirReadBits
    channelDirReadAdmissionToken.owner := channelCandidateOwner
    channelDirReadAdmissionToken.generation := channelCandidateGeneration
    channelDirReadAdmissionToken.sequence := channelCandidateSequence
    channelDirReadAdmissionToken.paired := channelCandidatePaired
    channelDirReadAdmissionToken.mshrContext := channelCandidateTask.mshrContext
    channelDirReadAdmissionToken.refillToReleaseCopy := channelCandidateTask.refillToReleaseCopy
    channelDirReadAdmissionToken.refillBufReadId := channelCandidateTask.refillBufReadId
    channelDirReadAdmissionToken.useProbeData := channelCandidateTask.useProbeData
    channelDirReadAdmissionToken.readProbeDataDown := channelCandidateTask.readProbeDataDown
    channelDirReadAdmissionToken.readRefillData := channelCandidateTask.readRefillData
    channelDirReadAdmissionToken.readDataFromDS := channelCandidateTask.readDataFromDS
    channelDirReadAdmissionToken.snpHitRelease := channelCandidateTask.snpHitRelease
    channelDirReadAdmissionToken.snpHitReleaseIdx := channelCandidateTask.snpHitReleaseIdx
  }

  // block same-set A req
  io.s1Entrance.valid := mshr_task_s1.valid && s2_ready && mshr_task_s1.bits.metaWen || io.sinkC.fire || bExecute
  io.s1Entrance.bits.set := Mux(
    mshr_task_s1.valid && mshr_task_s1.bits.metaWen,
    mshr_task_s1.bits.set,
    Mux(io.sinkC.fire, C_task.set, B_task.set)
  )

  /* ========  Stage 2 ======== */
  val s1_AHint_fire = io.sinkA.fire && io.sinkA.bits.opcode === Hint
  // any req except AHint might access DS; with the DS split into set banks,
  // only two consecutive accesses hitting the same bank need a bubble
  val task_s1_isAHint = !task_s1.bits.mshrTask && task_s1.bits.fromA && task_s1.bits.opcode === Hint
  val prev_ds_fire = RegNext(s1_fire && !s1_AHint_fire, false.B)
  val prev_ds_bank = RegEnable(get_data_bank(task_s1.bits.set), s1_fire)
  val ds_mcp2_stall = prev_ds_fire && task_s1.valid && !task_s1_isAHint &&
    get_data_bank(task_s1.bits.set) === prev_ds_bank

  // mirror of the s1 hint-generation conditions in CustomL1Hint:
  // MSHR Grant/GrantData/AccessAckData/CBOAck (incl. merged A) and any sinkC
  // Release. Such a fire must be blocked when the hint staging queue is full,
  // since the hint it generates would have nowhere to go.
  val mshr_wouldHint_s1 = mshr_task_s1.valid && mshr_task_s1.bits.fromA && (
    mshr_task_s1.bits.opcode === Grant ||
    mshr_task_s1.bits.opcode === GrantData ||
    mshr_task_s1.bits.opcode === AccessAckData ||
    mshr_task_s1.bits.opcode === CBOAck ||
    mshr_task_s1.bits.mergeA && (
      mshr_task_s1.bits.aMergeTask.opcode === Grant ||
      mshr_task_s1.bits.aMergeTask.opcode === GrantData
    )
  )
  val chnl_wouldHint_s1 = !mshr_task_s1.valid && channelDirReadAdmissionValid &&
    channelDirReadAdmissionToken.source === 0.U
  val hint_stall = (mshr_wouldHint_s1 || chnl_wouldHint_s1) && io.hint_s1QueueFull

  s2_ready  := !ds_mcp2_stall && !hint_stall

  val task_s2 = RegInit(0.U.asTypeOf(task_s1))
  task_s2.valid := s1_fire
  when(s1_fire) { task_s2.bits := task_s1.bits }

  /*  val sameSet_s2 = task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask && task_s2.bits.set === A_task.set
  val sameSet_s3 = RegNext(task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask) &&
    RegEnable(task_s2.bits.set, task_s2.valid) === A_task.set
  val sameSetCnt = PopCount(VecInit(io.msInfo.map(s => s.valid && s.bits.set === A_task.set && s.bits.fromA) :+
    sameSet_s2 :+ sameSet_s3).asUInt)
  noFreeWay := sameSetCnt >= cacheParams.ways.U
   */
  io.taskToPipe_s2 := task_s2
  val channelLeaseToken_s1 = Wire(new MainpipeLeaseToken)
  channelLeaseToken_s1.generation := channelDirReadAdmissionToken.generation
  channelLeaseToken_s1.sequence := channelDirReadAdmissionToken.sequence
  io.taskLeaseToken_s2 := RegEnable(Mux(mshr_task_s1.valid,
    mshrLeaseToken_s1, channelLeaseToken_s1), s1_fire)

  // MSHR task
  val mshrTask_s2 = task_s2.valid && task_s2.bits.mshrTask
  // A prefetch parent may complete as HintAck while carrying a merged demand
  // response in aMergeTask. GrantBuffer replaces the parent with that task,
  // so its data must be read from the parent's RefillBuf as well.
  val mergedAData_s2 = task_s2.bits.mergeA && task_s2.bits.aMergeTask.opcode(0)
  val mshrTask_s2_a_upwards = task_s2.bits.fromA &&
    (task_s2.bits.opcode === GrantData || task_s2.bits.opcode === Grant && task_s2.bits.dsWen ||
      task_s2.bits.opcode === AccessAckData || task_s2.bits.opcode === HintAck && task_s2.bits.dsWen ||
      mergedAData_s2)
  // For GrantData, read refillBuffer
  // Caution: GrantData-alias may read DataStorage or ReleaseBuf instead
  // Release-replTask also read refillBuf and then write to DS
  val releaseRefillData = task_s2.bits.replTask && (
    task_s2.bits.toTXREQ && (
      task_s2.bits.chiOpcode.get === WriteBackFull ||
        task_s2.bits.chiOpcode.get === WriteEvictFull ||
        afterIssueEbOrElse(task_s2.bits.chiOpcode.get === WriteEvictOrEvict, false.B) ||
        task_s2.bits.chiOpcode.get === Evict
    )
  )
  val replaceReleaseData = task_s2.bits.replaceTask && task_s2.bits.toTXREQ
  io.refillBufRead_s2.valid := mshrTask_s2 && (releaseRefillData ||
    task_s2.bits.readRefillData ||
    mshrTask_s2_a_upwards && !task_s2.bits.useProbeData && !task_s2.bits.readDataFromDS &&
      (!task_s2.bits.refillOnly || task_s2.bits.readRefillData))
  // Late post-Grant copy reads the paired normal's RefillBuf while writing
  // ReleaseBuf[mshrId]. Ordinary tasks still index RefillBuf by mshrId.
  io.refillBufRead_s2.bits.id := Mux(
    task_s2.bits.refillToReleaseCopy,
    task_s2.bits.refillBufReadId,
    task_s2.bits.mshrId
  )

  // ReleaseData and ProbeAckData read releaseBuffer
  // channel is used to differentiate GrantData and ProbeAckData
  val snoopNeedData = task_s2.bits.fromB && task_s2.bits.toTXDAT && isSnpRespDataX(task_s2.bits.chiOpcode.get)
  val releaseNeedData = task_s2.bits.toTXDAT && task_s2.bits.chiOpcode.get === CopyBackWrData
  val dctNeedData = task_s2.bits.toTXDAT && task_s2.bits.chiOpcode.get === CompData
  val cmoNeedData = task_s2.bits.toTXREQ && task_s2.bits.cmoTask && (
    task_s2.bits.chiOpcode.get === WriteCleanFull ||
      task_s2.bits.chiOpcode.get === WriteBackFull
  )
  val snpHitReleaseNeedData = !mshrTask_s2 && task_s2.bits.fromB && task_s2.bits.snpHitReleaseWithData
  io.releaseBufRead_s2.valid := task_s2.valid && Mux(
    mshrTask_s2,
    replaceReleaseData || task_s2.bits.readProbeDataDown || mshrTask_s2_a_upwards && task_s2.bits.useProbeData,
    snpHitReleaseNeedData
  )
  io.releaseBufRead_s2.bits.id := Mux(
    task_s2.bits.snpHitRelease,
    task_s2.bits.snpHitReleaseIdx,
    task_s2.bits.mshrId
  )

  require(beatSize == 2)

  /* status of each pipeline stage */
  io.status_s1.sets := VecInit(Seq(C_task.set, B_task.set, io.ASet, mshr_task_s1.bits.set))
  io.status_s1.tags := VecInit(Seq(C_task.tag, B_task.tag, io.ATag, mshr_task_s1.bits.tag))
  // `sinkA` is RequestBuffer's output.  Keep this separate from `ASet/ATag`,
  // which intentionally describe RequestBuffer's input for same-set enqueue
  // backpressure in MainPipe.
  io.status_s1.aCandidateValid := io.sinkA.valid
  io.status_s1.aCandidateSet := A_task.set
  io.status_s1.aCandidateTag := A_task.tag
  io.status_s1.bCandidateValid := bPending.valid || io.sinkB.valid
  // A later uncaptured B does not own this transition ahead of an older
  // acknowledged A claim. Its candidate remains visible for conservative
  // owner deferral, but must not create an A<->B admission wait cycle.
  io.status_s1.bValid := bPending.valid || io.sinkB.valid && !block_B && !normalReserved
  io.status_s1.bToN := (bPending.valid || io.sinkB.valid) && isSnpToN(B_task.chiOpcode.get)
  io.status_s1.bFire := bExecute
  io.status_s1.cReleaseData := io.sinkC.valid && C_task.opcode === ReleaseData
  // io.status_s1.isKeyword := VecInit(Seq(C_task.isKeyword, B_task.isKeyword, io.isKeyword, mshr_task_s1.bits.isKeyword))

  require(io.status_vec.size == 2)
  io.status_vec.zip(Seq(task_s1, task_s2)).foreach {
    case (status, task) =>
      status.valid := task.valid
      status.bits.channel := task.bits.channel
  }

  require(io.status_vec_toTX.size == 2)
  io.status_vec_toTX.zip(Seq(task_s1, task_s2)).foreach {
    case (status, task) =>
      status.valid := task.valid
      status.bits.channel := task.bits.channel
      status.bits.txChannel := task.bits.txChannel
      status.bits.mshrTask := task.bits.mshrTask
  }

  dontTouch(io)

  // Performance counters
  XSPerfAccumulate("mshr_req", s0_fire)
  XSPerfAccumulate("mshr_req_stall", io.mshrTask.valid && !io.mshrTask.ready)

  XSPerfAccumulate("sinkA_req", io.sinkA.fire)
  XSPerfAccumulate("sinkB_req", io.sinkB.fire)
  XSPerfAccumulate("sinkC_req", io.sinkC.fire)

  XSPerfAccumulate("sinkA_stall", io.sinkA.valid && !io.sinkA.ready)
  XSPerfAccumulate("sinkB_stall", io.sinkB.valid && !io.sinkB.ready)
  XSPerfAccumulate("sinkC_stall", io.sinkC.valid && !io.sinkC.ready)

  XSPerfAccumulate("sinkA_stall_by_mshrFull", io.sinkA.valid && io.fromMSHRCtl.blockA_s1)
  XSPerfAccumulate("sinkB_stall_by_mshrFull", io.sinkB.valid && io.fromMSHRCtl.blockB_s1)

  XSPerfAccumulate(
    "sinkA_stall_by_mainpipe_conflict",
    io.sinkA.valid && !io.fromMSHRCtl.blockA_s1 && io.fromMainPipe.blockA_s1
  )
  XSPerfAccumulate(
    "sinkB_stall_by_mainpipe_conflict",
    io.sinkB.valid && !io.fromMSHRCtl.blockB_s1 && io.fromMainPipe.blockB_s1
  )
  XSPerfAccumulate(
    "sinkC_stall_by_mainpipe_conflict",
    io.sinkC.valid && !io.fromMSHRCtl.blockC_s1 && io.fromMainPipe.blockC_s1
  )

  XSPerfAccumulate(
    "sinkA_stall_by_grantBuf",
    io.sinkA.valid && !io.fromMSHRCtl.blockA_s1 && !io.fromMainPipe.blockA_s1 &&
      io.fromGrantBuffer.blockSinkReqEntrance.blockA_s1
  )
  XSPerfAccumulate(
    "sinkB_stall_by_grantBuf",
    io.sinkB.valid && !io.fromMSHRCtl.blockB_s1 && !io.fromMainPipe.blockB_s1 &&
      io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1
  )
  XSPerfAccumulate(
    "sinkC_stall_by_grantBuf",
    io.sinkC.valid && !io.fromMSHRCtl.blockC_s1 && !io.fromMainPipe.blockC_s1 &&
      io.fromGrantBuffer.blockSinkReqEntrance.blockC_s1
  )

  XSPerfAccumulate(
    "sinkB_stall_by_TXDAT",
    io.sinkB.valid && !io.fromMSHRCtl.blockB_s1 && !io.fromMainPipe.blockB_s1 &&
      !io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1 &&
      io.fromTXDAT.blockSinkBReqEntrance
  )
  XSPerfAccumulate(
    "sinkB_stall_by_TXRSP",
    io.sinkB.valid && !io.fromMSHRCtl.blockB_s1 && !io.fromMainPipe.blockB_s1 &&
      !io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1 &&
      !io.fromTXDAT.blockSinkBReqEntrance &&
      io.fromTXRSP.blockSinkBReqEntrance
  )

  XSPerfAccumulate("sinkA_stall_by_dir", io.sinkA.valid && !block_A && !io.dirRead_s1.ready)
  XSPerfAccumulate("sinkB_stall_by_dir", io.sinkB.valid && !block_B && !io.dirRead_s1.ready)
  XSPerfAccumulate("sinkC_stall_by_dir", io.sinkC.valid && !block_C && !io.dirRead_s1.ready)

  XSPerfAccumulate("sinkA_stall_by_mshrTask", io.sinkA.valid && !block_A && dirReadAdmissionFire && mshr_task_s1.valid)
  XSPerfAccumulate("sinkB_stall_by_mshrTask", io.sinkB.valid && !block_B && dirReadAdmissionFire && mshr_task_s1.valid)
  XSPerfAccumulate("sinkC_stall_by_mshrTask", io.sinkC.valid && !block_C && dirReadAdmissionFire && mshr_task_s1.valid)

  XSPerfAccumulate(
    "sinkA_stall_by_mcp2",
    io.sinkA.valid && !block_A && io.dirRead_s1.ready && !mshr_task_s1.valid && ds_mcp2_stall
  )
  XSPerfAccumulate(
    "sinkB_stall_by_mcp2",
    io.sinkB.valid && !block_B && io.dirRead_s1.ready && !mshr_task_s1.valid && ds_mcp2_stall
  )
  XSPerfAccumulate(
    "sinkC_stall_by_mcp2",
    io.sinkC.valid && !block_C && io.dirRead_s1.ready && !mshr_task_s1.valid && ds_mcp2_stall
  )

  XSPerfAccumulate(
    "sinkA_stall_by_sinkB",
    io.sinkA.valid && sink_ready_basic && !block_A && sinkValids(1) && !sinkValids(0)
  )
  XSPerfAccumulate("sinkA_stall_by_sinkC", io.sinkA.valid && sink_ready_basic && !block_A && sinkValids(0))
  XSPerfAccumulate("sinkB_stall_by_sinkC", io.sinkB.valid && sink_ready_basic && !block_B && sinkValids(0))

}
