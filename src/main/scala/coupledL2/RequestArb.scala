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
    val snoopReservationToken = Input(new SnoopSlotToken)
    val taskSnoopReservationToken_s2 = Output(new SnoopSlotToken)
    val taskGrantReservationToken_s2 = ValidIO(new GrantReservationToken)
    val bServiceStart = ValidIO(new TaskBundle)
    val bServiceConsume = ValidIO(new SnoopSlotToken)
    val sidecarReservationHold = Output(Bool())
    val snoopAdmissionEnable = Output(Bool())
    val snoopQueued = Input(Bool())
    val reservedB = Output(Bool())
    val contextReservation = Output(Bool())
    val grantReservation = ValidIO(new GrantReservationToken)
    val grantReservationReturn = Flipped(Vec(3, ValidIO(new GrantReservationToken)))
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
    val setStatus = Vec(2, ValidIO(new PipeSetStatus))
    val mainPipeSetStatus = Flipped(Vec(5, ValidIO(new PipeSetStatus)))

    /* handle set conflict, capacity conflict */
    val fromMSHRCtl = Input(new BlockInfo())
    val fromMainPipe = Input(new BlockInfo())
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
  val mshrGrantToken_s1 = RegInit(0.U.asTypeOf(Valid(new GrantReservationToken)))
  val grantReservation = Wire(Valid(new GrantReservationToken))
  val grantReservationOwners = RegInit(VecInit(Seq.fill(mshrsAll)(
    0.U.asTypeOf(Valid(new GrantReservationToken)))))
  val grantReservationBusy = VecInit(grantReservationOwners.map(_.valid)).asUInt
  val grantReservationAvailable = !grantReservationBusy.andR
  val grantReservationSlotOH = PriorityEncoderOH(~grantReservationBusy)
  // Capture commits the source owner; payload stays on its Q until the atomic source/Directory fire.
  class ChannelDirReadAdmissionToken extends Bundle {
    val valid = Bool()
    val source = UInt(2.W) // 0=C, 1=B, 2=A (sinkValids priority order)
    val dirRead = new DirRead
    val owner = UInt(mshrBits.W)
    val generation = UInt(8.W)
    val sequence = UInt(8.W)
    val snoopSlotToken = new SnoopSlotToken
    val grantToken = Valid(new GrantReservationToken)
    val paired = Bool()
    val mshrContext = UInt(2.W)
    val refillToReleaseCopy = Bool()
    val refillBufReadId = UInt(mshrBits.W)
    val useProbeData = Bool()
    val readProbeDataDown = Bool()
    val readRefillData = Bool()
    val snpHitRelease = Bool()
    val snpHitReleaseIdx = UInt(mshrBits.W)
  }
  val channelDirReadAdmissionToken = RegInit(0.U.asTypeOf(new ChannelDirReadAdmissionToken))
  val channelDirReadAdmissionValid = channelDirReadAdmissionToken.valid
  val channelDirReadAdmissionFire = Wire(Bool())
  val dirReadAdmissionValid = RegInit(false.B)
  val dirReadAdmissionBits = RegInit(0.U.asTypeOf(new DirRead()))
  val dirReadAdmissionToken = RegInit(0.U.asTypeOf(new MainpipeLeaseToken))
  val dirReadAdmissionFire = Wire(Bool())
  // Serialize S1 through S3 apply while local reservation events and registered credits settle.
  val localTransitionCycles = RegInit(0.U(3.W))
  val localTransitionIdle = localTransitionCycles === 0.U
  val admitMshrTurn = RegInit(false.B)
  val bPending = RegInit(0.U.asTypeOf(Valid(new TaskBundle)))
  val bServiceReserved = RegInit(false.B)
  val bServiceToken = RegInit(0.U.asTypeOf(new SnoopSlotToken))
  val bExecute = Wire(Bool())
  io.bServiceStart.valid := bPending.valid && !bServiceReserved
  io.bServiceStart.bits := bPending.bits
  io.reservedB := bPending.valid
  io.sidecarReservationHold := io.sinkB.valid || (bPending.valid && !bServiceReserved) ||
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
  val blockAllEntrance = io.fromMSHRCtl.blockAll_s1 || io.fromMainPipe.blockAll_s1
  val pendingSnoopSetConflict = Wire(Bool())
  val mshrTaskReadyBasic = localTransitionIdle && admitMshrTurn && !mshr_task_s1.valid && !bPending.valid &&
    !blockAllEntrance &&
    !io.sinkB.valid &&
    !pendingSnoopSetConflict &&
    !channelDirReadAdmissionValid &&
    !io.fromGrantBuffer.blockMSHRReqEntrance &&
    !io.fromTXDAT.blockMSHRReqEntrance &&
    !io.fromTXRSP.blockMSHRReqEntrance &&
    !io.fromTXREQ.blockMSHRReqEntrance &&
    (!(io.mshrTask.bits.fromA || io.mshrTask.bits.fromC) || grantReservationAvailable)

  s0_fire := io.mshrTask.valid && io.mshrTask.ready

  /* ======== Stage 1 ======== */
  /* latch mshr_task from s0 to s1 */
  val mshr_replRead_stall =
    mshr_task_s1.valid && s1_needs_replRead && !dirReadAdmissionFire
  mshr_task_s1.valid := mshr_task_s1.valid && !s1_fire || s0_fire

  when(s0_fire) {
    mshr_task_s1.bits := io.mshrTask.bits
    mshrLeaseToken_s1 := io.mshrLeaseToken
    mshrGrantToken_s1 := grantReservation
  }

  /* Channel interaction from s1 */
  val A_task = io.sinkA.bits
  val normalReservedTask = io.normalReserveAck.bits.task
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
  val B_task = bPending.bits
  val bCandidateStatus = Mux(bPending.valid, bPending.bits, io.sinkB.bits)
  val C_task = io.sinkC.bits
  val block_A =
    io.fromMSHRCtl.blockA_s1 || io.fromMainPipe.blockA_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockA_s1
  when(!io.normalReserveAck.valid) {
    normalYieldForB := false.B
  }.elsewhen(!channelDirReadAdmissionValid && normalReservationMatches && block_A &&
    (io.sinkB.valid || io.snoopQueued)) {
    normalYieldForB := true.B
  }
  val block_B =
    io.fromMSHRCtl.blockB_s1 || io.fromMainPipe.blockB_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1 ||
      io.fromTXDAT.blockSinkBReqEntrance ||
      io.fromTXRSP.blockSinkBReqEntrance
  val block_C =
    io.fromMSHRCtl.blockC_s1 || io.fromMainPipe.blockC_s1 || io.fromGrantBuffer.blockSinkReqEntrance.blockC_s1

  io.snoopAdmissionEnable := io.snoopQueued && !admitMshrTurn && resetFinish &&
    !mshr_task_s1.valid && !bPending.valid && !channelDirReadAdmissionValid &&
    !io.normalReserveAck.valid && !io.sinkC.valid && !io.sinkB.valid && !block_B && !blockAllEntrance

//  val noFreeWay = Wire(Bool())

  val blockReservedB = io.fromMainPipe.blockB_s1 ||
    io.fromGrantBuffer.blockSinkReqEntrance.blockB_s1
  val sinkValids = VecInit(
    Seq(
      io.sinkC.valid && !block_C && !bPending.valid,
      bPending.valid && bServiceReserved && !blockReservedB,
      io.sinkA.valid && normalReserved && !block_A && !bPending.valid
    )
  ).asUInt

  val channelCandidateValid = Wire(Bool())
  val channelCandidateSource = OHToUInt(PriorityEncoderOH(sinkValids))
  io.aCommit := channelCandidateValid && channelCandidateSource === 2.U
  val channelCandidateTask = Wire(new TaskBundle())
  channelCandidateTask := ParallelPriorityMux(sinkValids, Seq(C_task, B_task, A_task))
  when(sinkValids(2) && !sinkValids(0) && !sinkValids(1)) {
    channelCandidateTask := normalReservedTask
    channelCandidateTask.mshrId := io.normalReserveAck.bits.id
  }
  channelCandidateValid := localTransitionIdle && !blockAllEntrance &&
    (bPending.valid || !admitMshrTurn) && !mshr_task_s1.valid &&
    sinkValids.orR && resetFinish &&
    !channelDirReadAdmissionValid && !io.sinkB.valid &&
    (!(channelCandidateTask.fromA || channelCandidateTask.fromC) || grantReservationAvailable)

  val channelCandidateOwner = Mux(channelCandidateSource === 2.U,
    io.normalReserveAck.bits.id, channelCandidateTask.mshrId)
  val channelCandidateGeneration = Mux(channelCandidateSource === 2.U,
    io.normalReserveAck.bits.token.generation, 0.U(8.W))
  val channelCandidateSequence = Mux(channelCandidateSource === 2.U,
    io.normalReserveAck.bits.token.sequence, 0.U(8.W))
  val channelCandidateSnoopToken = Mux(channelCandidateSource === 1.U,
    bServiceToken, 0.U.asTypeOf(new SnoopSlotToken))
  val channelCandidatePaired = channelCandidateTask.snpHitRelease ||
    channelCandidateTask.refillToReleaseCopy || channelCandidateTask.mshrContext =/= 0.U

  val grantReservationChannel = Mux(s0_fire,
    io.mshrTask.bits.channel, channelCandidateTask.channel)
  grantReservation.valid := (s0_fire || channelCandidateValid) &&
    (grantReservationChannel(0) || grantReservationChannel(2))
  grantReservation.bits.channel := grantReservationChannel
  grantReservation.bits.id := OHToUInt(grantReservationSlotOH)
  grantReservation.bits.generation := Mux1H(grantReservationSlotOH,
    grantReservationOwners.map(_.bits.generation)) + 1.U
  assert(!(s0_fire && channelCandidateValid),
    "one local execution slot cannot acquire two GrantBuffer reservations")
  when(grantReservation.valid) {
    assert(grantReservationAvailable && grantReservationSlotOH.orR,
      "an A/C admission must own an idle registered GrantBuffer token")
  }
  io.grantReservation := RegNext(grantReservation, 0.U.asTypeOf(grantReservation))
  grantReservationOwners.zipWithIndex.foreach { case (owner, i) =>
    val returns = io.grantReservationReturn.map(event => event.valid && event.bits.id === i.U)
    assert(PopCount(returns) <= 1.U,
      "one GrantBuffer reservation token cannot return from two terminal stages")
    io.grantReservationReturn.zip(returns).foreach { case (event, matches) =>
      when(matches) {
        assert(owner.valid && event.bits.asUInt === owner.bits.asUInt,
          "a local GrantBuffer return must name its exact outstanding token")
        owner.valid := false.B
      }
    }
    when(grantReservation.valid && grantReservationSlotOH(i)) {
      assert(!owner.valid && !VecInit(returns).asUInt.orR,
        "GrantBuffer token admission must use an idle Q slot")
      owner.valid := true.B
      owner.bits := grantReservation.bits
    }
  }

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
  val channelTokenSnoopToken = Mux(channelDirReadAdmissionToken.source === 1.U,
    bServiceToken, 0.U.asTypeOf(new SnoopSlotToken))
  val channelTokenSourceValid = Mux(channelDirReadAdmissionToken.source === 0.U,
    io.sinkC.valid, Mux(channelDirReadAdmissionToken.source === 1.U,
      bPending.valid && bServiceReserved && bServiceToken.valid,
      io.sinkA.valid && io.normalReserveAck.valid))
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
    channelTokenTask.snpHitRelease === channelDirReadAdmissionToken.snpHitRelease &&
    channelTokenTask.snpHitReleaseIdx === channelDirReadAdmissionToken.snpHitReleaseIdx
  // Retained owner comparisons are assertions; they must not revoke the captured Directory command.
  val channelTokenIdentityMatches =
    channelDirReadAdmissionToken.dirRead.asUInt === channelTokenDirReadBits.asUInt &&
    channelDirReadAdmissionToken.owner === channelTokenOwnerCurrent &&
    channelDirReadAdmissionToken.generation === channelTokenGenerationCurrent &&
    channelDirReadAdmissionToken.sequence === channelTokenSequenceCurrent &&
    channelDirReadAdmissionToken.paired === channelTokenPairedCurrent &&
    channelDirReadAdmissionToken.snoopSlotToken.asUInt === channelTokenSnoopToken.asUInt &&
    channelTokenProvenanceCurrent
  when(channelDirReadAdmissionValid) {
    assert(channelTokenSourceValid && channelTokenIdentityMatches,
      "a channel Directory token must retain its committed source and provenance")
  }

  val mshrTaskBgDefer = io.mshrTask.valid && io.mshrTask.bits.qosClass === 4.U && sinkValids.orR
  io.mshrTask.ready := mshrTaskReadyBasic && !mshrTaskBgDefer
  XSPerfAccumulate("reqarb_bg_defer_cnt", mshrTaskBgDefer)


  val sink_ready_basic = channelDirReadAdmissionFire

  io.normalReserveRequest.valid := resetFinish && !blockAllEntrance &&
    !mshr_task_s1.valid && !bPending.valid && io.sinkA.valid && !block_A &&
    !io.normalReserveAck.valid && !sinkValids(0) && !io.sinkB.valid &&
    !channelDirReadAdmissionValid && !io.snoopAdmissionEnable
  io.sinkA.ready := sink_ready_basic && channelDirReadAdmissionToken.source === 2.U
  when(io.sinkA.fire) {
    assert(io.dirRead_s1.fire && normalReserved,
      "a normal Directory read must consume its registered concrete slot")
    assert(sameCmoAllLine || A_task.asUInt === normalReservedTask.asUInt,
      "normal reservation must retain the complete upstream request")
  }
  // RXSNP already captured and reserved this head; refill bPending during the
  // local execution cooldown while keeping S1/Directory admission serialized.
  val bEntryRefillReady = resetFinish &&
    !mshr_task_s1.valid && !bPending.valid && !channelDirReadAdmissionValid &&
    !normalReserved && !blockReservedB &&
    !io.fromTXDAT.blockSinkBReqEntrance && !io.fromTXRSP.blockSinkBReqEntrance
  io.sinkB.ready := bEntryRefillReady
  io.sinkC.ready := sink_ready_basic && channelDirReadAdmissionToken.source === 0.U
  bExecute := sink_ready_basic && channelDirReadAdmissionToken.source === 1.U
  io.bServiceConsume.valid := bExecute
  io.bServiceConsume.bits := bServiceToken
  val channelConsumes = VecInit(Seq(io.sinkC.fire, bExecute, io.sinkA.fire))
  assert(channelDirReadAdmissionFire === channelConsumes.asUInt.orR,
    "a channel Directory token must execute and consume its owner atomically")
  assert(PopCount(channelConsumes) <= 1.U,
    "one channel Directory token must consume exactly one channel owner")
  when(io.sinkB.fire) {
    bPending.valid := true.B
    bPending.bits := io.sinkB.bits
    bServiceReserved := false.B
    bServiceToken.valid := false.B
  }.elsewhen(io.bServiceStart.valid) {
    assert(io.snoopReservationToken.valid &&
      io.snoopReservationToken.set === bPending.bits.set &&
      io.snoopReservationToken.tag === bPending.bits.tag &&
      io.snoopReservationToken.txnID === bPending.bits.txnID.get &&
      io.snoopReservationToken.source === bPending.bits.sourceId,
      "B service must retain its admitted physical slot token")
    bServiceReserved := true.B
    bServiceToken := io.snoopReservationToken
  }.elsewhen(bExecute) {
    bPending.valid := false.B
    bServiceReserved := false.B
    bServiceToken.valid := false.B
  }
  assert(!bExecute || bServiceReserved, "B executed before registered service reservation")

  val chnl_task_s1 = Wire(Valid(new TaskBundle()))
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
    dirReadAdmissionValid && s2_ready && !mshr_refillOnlyCommit_stall,
    channelDirReadAdmissionValid &&
      !mshr_task_s1.valid && s2_ready
  )
  io.dirRead_s1.bits := Mux(mshrReplReadActive,
    dirReadAdmissionBits, channelDirReadAdmissionToken.dirRead)
  dirReadAdmissionFire := mshrReplReadActive && io.dirRead_s1.fire
  // A retained replacement read owns its same-set execution slot until Directory and S1 fire together.
  when(mshrReplReadActive) {
    assert(dirReadAdmissionFire === s1_fire,
      "a replacement Directory token must execute and advance S1 atomically")
  }
  when(dirReadAdmissionFire) {
    assert(!io.fromMainPipe.blockG_s1,
      "a retained replacement read must keep its reserved same-set execution slot")
    assert(dirReadAdmissionToken.asUInt === mshrLeaseToken_s1.asUInt,
      "a replacement Directory read must consume its retained source lease")
  }
  channelDirReadAdmissionFire := !mshr_task_s1.valid &&
    channelDirReadAdmissionValid &&
    s2_ready && io.dirRead_s1.ready
  dirReadAdmissionValid := dirReadAdmissionValid && !dirReadAdmissionFire || dirReadCapture
  when(dirReadCapture) {
    dirReadAdmissionBits := dirReadCaptureBits
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
    channelDirReadAdmissionToken.snoopSlotToken := channelCandidateSnoopToken
    channelDirReadAdmissionToken.grantToken := grantReservation
    channelDirReadAdmissionToken.paired := channelCandidatePaired
    channelDirReadAdmissionToken.mshrContext := channelCandidateTask.mshrContext
    channelDirReadAdmissionToken.refillToReleaseCopy := channelCandidateTask.refillToReleaseCopy
    channelDirReadAdmissionToken.refillBufReadId := channelCandidateTask.refillBufReadId
    channelDirReadAdmissionToken.useProbeData := channelCandidateTask.useProbeData
    channelDirReadAdmissionToken.readProbeDataDown := channelCandidateTask.readProbeDataDown
    channelDirReadAdmissionToken.readRefillData := channelCandidateTask.readRefillData
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

  s2_ready  := !ds_mcp2_stall && !hint_stall && !blockAllEntrance

  val task_s2 = RegInit(0.U.asTypeOf(task_s1))
  task_s2.valid := s1_fire
  when(s1_fire) { task_s2.bits := task_s1.bits }

  io.taskToPipe_s2 := task_s2
  io.setStatus.zip(Seq(task_s1, task_s2)).foreach { case (status, task) =>
    status.valid := task.valid
    status.bits.set := task.bits.set
    status.bits.pendingSnoop := !task.bits.mshrTask && task.bits.fromB
  }
  pendingSnoopSetConflict := (Seq(io.setStatus(1)) ++ io.mainPipeSetStatus).map { status =>
    status.valid && status.bits.pendingSnoop && status.bits.set === io.mshrTask.bits.set
  }.reduce(_ || _)
  val channelLeaseToken_s1 = Wire(new MainpipeLeaseToken)
  channelLeaseToken_s1.generation := channelDirReadAdmissionToken.generation
  channelLeaseToken_s1.sequence := channelDirReadAdmissionToken.sequence
  io.taskLeaseToken_s2 := RegEnable(Mux(mshr_task_s1.valid,
    mshrLeaseToken_s1, channelLeaseToken_s1), s1_fire)
  io.taskSnoopReservationToken_s2 := RegEnable(Mux(
    !mshr_task_s1.valid && channelDirReadAdmissionToken.source === 1.U,
    channelDirReadAdmissionToken.snoopSlotToken,
    0.U.asTypeOf(new SnoopSlotToken)), s1_fire)
  io.taskGrantReservationToken_s2 := RegEnable(Mux(mshr_task_s1.valid,
    mshrGrantToken_s1, channelDirReadAdmissionToken.grantToken), s1_fire)

  // MSHR task
  val mshrTask_s2 = task_s2.valid && task_s2.bits.mshrTask
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
    mshrTask_s2_a_upwards && !task_s2.bits.useProbeData &&
      (!task_s2.bits.refillOnly || task_s2.bits.readRefillData))
  val snoopRefillData = task_s2.bits.mshrContext === 1.U && task_s2.bits.readRefillData
  io.refillBufRead_s2.bits.id := Mux(
    task_s2.bits.refillToReleaseCopy || snoopRefillData,
    task_s2.bits.refillBufReadId,
    task_s2.bits.mshrId
  )

  // ReleaseData and ProbeAckData read releaseBuffer
  // channel is used to differentiate GrantData and ProbeAckData
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
  io.status_s1.sets := VecInit(Seq(C_task.set, bCandidateStatus.set, io.ASet, mshr_task_s1.bits.set))
  io.status_s1.tags := VecInit(Seq(C_task.tag, bCandidateStatus.tag, io.ATag, mshr_task_s1.bits.tag))
  io.status_s1.aCandidateValid := io.sinkA.valid
  io.status_s1.aCandidateSet := A_task.set
  io.status_s1.aCandidateTag := A_task.tag
  io.status_s1.bCandidateValid := bPending.valid || io.sinkB.valid
  io.status_s1.bValid := bPending.valid || io.sinkB.valid && !block_B && !normalReserved
  io.status_s1.bToN := (bPending.valid || io.sinkB.valid) && isSnpToN(bCandidateStatus.chiOpcode.get)
  io.status_s1.bFire := bExecute
  io.status_s1.cReleaseData := io.sinkC.valid && C_task.opcode === ReleaseData

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
