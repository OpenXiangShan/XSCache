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
import chisel3.util.random.LFSR
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import xscache.coupledL2.prefetch.{BusContentionBundle, DemandRefillBundle, PrefetchTrain}
import xscache.coupledL2._
import xscache.coupledL2.utils._
import xscache.coupledL2.MetaData._
import xscache.chi.{CHIREQ, CHIRSP, HasCHIOpcodes}

class MSHRCtl(implicit p: Parameters) extends CoupledL2Module with HasCHIOpcodes with HasPerfEvents {
  val io = IO(new Bundle() {
    /* interact with req arb */
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus()
    })
    val toReqArb = Output(new BlockInfo())

    /* interact with mainpipe */
    val fromMainPipe = new Bundle() {
      val mainpipeLeaseDone = Flipped(ValidIO(new MainpipeLeaseDone))
      val mshr_alloc_s3 = Flipped(ValidIO(new MSHRRequest()))
      val snoopDirectoryHit = Input(Bool())
      val normalReservationDone = Flipped(ValidIO(new MainpipeLeaseDone))
      val replaceCaptureWrite = Flipped(ValidIO(UInt(mshrBits.W)))
      val refillToReleaseCopyWrite = Flipped(ValidIO(UInt(mshrBits.W)))
      val refillWriteDone = Flipped(ValidIO(new MainpipeLeaseDone))
      val grantSent = Flipped(ValidIO(new MainpipeLeaseDone))
      val releaseDataDirectWrite = Flipped(ValidIO(new TaskBundle))
      val orphanReleaseData = Input(Bool())
      val snoopToNDirectDone = Flipped(ValidIO(new TaskBundle))
      val grantInstallDone = Flipped(ValidIO(UInt(mshrBits.W)))
    }
    val toMainPipe = new Bundle() {
      val mshr_alloc_ptr = Output(UInt(mshrBits.W))
      val normalAllocAdmission = Output(Bool())
      val normalAllocToken = Output(new MainpipeLeaseToken)
      val normalAllocOwner = Output(UInt(mshrBits.W))
      val normalAllocSource = Output(UInt(sourceIdBits.W))
      val replRespRetry = Output(Bool())
      val snoopDataHolds = Vec(mshrsAll, ValidIO(new MSHRInfo()))
      val postGrantReleaseHolds = Vec(mshrsAll, ValidIO(new MSHRInfo()))
      val postGrantReleaseLate = ValidIO(new TaskBundle)
      val grantInstallOK = Output(Bool())
      val grantInstallSlot = Output(UInt(mshrBits.W))
      val replParentGeneration = Output(UInt(8.W))
    }

    val txdatDone = Input(ValidIO(new TaskBundle))
    val txrspDone = Input(ValidIO(new TaskBundle))

    /* to request arbiter */
    // val mshrFull = Output(Bool())
    val mshrTask = DecoupledIO(new TaskBundle())
    val mshrTaskLeaseToken = Output(new MainpipeLeaseToken)
    val bServiceStart = Flipped(ValidIO(new TaskBundle))
    val snoopAdmission = Flipped(DecoupledIO(new TaskBundle))
    val sidecarSnoopReserve = Flipped(ValidIO(UInt(mshrBits.W)))
    val replaceSnoopAdmissionIdle = Output(UInt(mshrsAll.W))
    val sidecarReservationHold = Input(Bool())
    val contextReservation = Input(Bool())
    val normalReserveRequest = Flipped(DecoupledIO(new TaskBundle))
    val normalReserveAck = ValidIO(new NormalSlotReservation)
    val normalReserveConsume = Input(Bool())
    val normalReserveCancel = Input(Bool())

    /* status of s2 and s3 */
    val pipeStatusVec = Flipped(Vec(3, ValidIO(new PipeStatus)))

    /* send reqs */
    val toTXREQ = DecoupledIO(new CHIREQ())
    val replaceTxreqPendingCnt = Output(UInt(log2Up(mshrsAll + 1).W))
    val toTXRSP = DecoupledIO(new CHIRSP()) // TODO: unify with main pipe, which should be TaskBundle
    val toTXDAT = DecoupledIO(new TaskWithData())
    val txdatDirectSpace = Input(Bool())
    val releaseBufDirectRd = ValidIO(new MSHRBufRead)
    val releaseBufDirectResp = Input(new DSBlock)
    val toSourceB = DecoupledIO(new TLBundleB(edgeIn.bundle))

    /* to block sourceB from sending same-addr probe until GrantAck received */
    val grantStatus = Input(Vec(grantBufInflightSize, new GrantStatus()))

    /* receive resps */
    val resps = Input(new Bundle() {
      val sinkC = new RespBundle() //probeAck from core
      val rxrsp = new RespBundle() //releaseAck(CompDBID) from CHI
      val rxdat = new RespBundle() //AcquireBlock(CompData) from CHI
    })
    val sinkCReleaseData = Input(ValidIO(new TaskBundle))
    val sinkCProbeRouteRequest = Input(ValidIO(new TaskBundle))
    val sinkCProbeRouteReady = Output(Bool())
    val sinkCProbeRouteDone = Input(Bool())

    val sinkCProbeAckPayload = Input(new DSBlock)
    val sinkCRefillWrite = Input(Valid(new MSHRBufWrite))
    val sinkCRefillWriteApplied = Output(Valid(new MSHRBufWrite))
    val releaseBufWriteId = Output(UInt(mshrBits.W))
    val releaseBufWriteIdExtra = Output(ValidIO(UInt(mshrBits.W)))
    val releaseBufWriteExtra = Output(ValidIO(new MSHRBufWrite))
    val releaseBufWriteExtraGeneration = Output(UInt(8.W))
    val refillBufWriteId = Output(ValidIO(UInt(mshrBits.W)))
    val snoopRefillBufWriteId = Output(ValidIO(UInt(mshrBits.W)))

    /* nested writeback */
    val nestedwb = Input(new NestedWriteback)
    val nestedwbPayload = Input(new DSBlock)
    val nestedReleaseWrites = Vec(2, ValidIO(new MSHRBufWrite))
    val nestedReleaseAuthority = Output(UInt(mshrsAll.W))
    val nestedRefillWrite = Output(ValidIO(new MSHRBufWrite))
    val refillBufReleaseOwner = Output(UInt(mshrsAll.W))
    val rxdatStaleSlots = Output(UInt(mshrsAll.W))

    val refillInfo = Vec(mshrsAll, ValidIO(new MSHRInfo()))
    val snoopInfo = Vec(mshrsAll, ValidIO(new MSHRInfo()))
    val replaceInfo = Vec(mshrsAll, ValidIO(new ReplaceMSHRInfo))
    val replaceInfoForSnoop = Vec(mshrsAll, ValidIO(new ReplaceMSHRInfo))
    val refillVictimDelegated = Output(UInt(mshrsAll.W))
    val replaceAllocForSnoop = Vec(4, ValidIO(new ReplacementAllocAddress))
    val aMergeTask = Flipped(ValidIO(new AMergeTask))

    /* refill read replacer result */
    val replResp = Flipped(ValidIO(new ReplacerResult))

    /* for TopDown Monitor */
    val msStatus = topDownOpt.map(_ => Vec(mshrsAll, ValidIO(new MSHRStatus)))
    val msAlloc = topDownOpt.map(_ => Vec(mshrsAll, ValidIO(new MSHRAllocStatus)))
    val dataRefill = prefetchOpt.map(_ => ValidIO(new DemandRefillBundle))
    val busContention = prefetchOpt.map(_ => ValidIO(new BusContentionBundle))

    /* to Slice Top for pCrd info.*/
    val pCrd = Vec(2 * mshrsAll, new PCrdQueryBundle)

    /* for TopDown */
    val l2Miss = Output(Bool())
  })

  /*MSHR allocation pointer gen -> to Mainpipe*/
  class MSHRSelector(implicit p: Parameters) extends L2Module {
    val io = IO(new Bundle() {
      val idle = Input(Vec(mshrsAll, Bool()))
      val out = ValidIO(UInt(mshrsAll.W))
    })
    io.out.valid := ParallelOR(io.idle)
    io.out.bits := ParallelPriorityMux(io.idle.zipWithIndex.map {
      case (b, i) => (b, (1.U(mshrsAll.W) << i))
    })
  }

  require(
    mshrsAll < (1 << (mshrBits - 1)),
    s"physical MSHR count $mshrsAll exceeds the ID range reserved for MSHR tasks"
  )
  val refillMshrs = Seq.tabulate(mshrsAll)(_ => Module(new MSHR()))
  val snoopMshrs = Seq.tabulate(mshrsAll)(_ => Module(new SnoopMSHR()))
  val replaceMshrs = Seq.tabulate(mshrsAll)(_ => Module(new ReplaceMSHR()))
  val mshrTaskAllocations = Seq(
    refillMshrs.map(_.io.alloc.valid), snoopMshrs.map(_.io.alloc.valid), replaceMshrs.map(_.io.alloc.valid))
  val mshrTaskGenerations = mshrTaskAllocations.map(_.map { allocate =>
    val generation = RegInit(0.U(8.W))
    when(allocate) { generation := generation + 1.U }
    generation
  })
  // An accepted Probe pins its transmitted key and generation until its final routed ProbeAck.
  class SourceBProbeClaim extends Bundle {
    val valid = Bool()
    val set = UInt(setBits.W)
    val tag = UInt(tagBits.W)
    val generation = UInt(8.W)
  }
  val sourceBProbeClaims = RegInit(VecInit(
    Seq.fill(3 * mshrsAll)(0.U.asTypeOf(new SourceBProbeClaim))))
  val sourceBGenerations = (0 until mshrsAll).flatMap(i =>
    Seq(mshrTaskGenerations(0)(i), mshrTaskGenerations(1)(i))) ++ mshrTaskGenerations(2)
  val normalService = RegInit(0.U.asTypeOf(Valid(new NormalSlotReservation)))
  val normalServiceInFlight = RegInit(false.B)
  val normalReservationSequence = RegInit(0.U(8.W))
  val normalServiceSlotOH = Mux(normalService.valid,
    UIntToOH(normalService.bits.id, mshrsAll), 0.U(mshrsAll.W))
  def normalCmo(task: TaskBundle): Bool = task.opcode === CBOClean ||
    task.opcode === CBOFlush || task.opcode === CBOInval
  val normalServiceCmo = normalService.valid && normalCmo(normalService.bits.task)
  class PairedCompletion extends Bundle {
    val target = UInt(mshrBits.W)
    val targetGeneration = UInt(8.W)
    val targetSet = UInt(setBits.W)
    val targetTag = UInt(tagBits.W)
    val source = UInt(mshrBits.W)
    val sourceToken = new MainpipeLeaseToken
    val outcome = UInt(2.W) // 1: ToN, 2: ToB, 3: clean
  }
  val selectedIssueCompletion = WireDefault(0.U.asTypeOf(Valid(new PairedCompletion)))
  val postGrantCompletion = RegNext(selectedIssueCompletion,
    0.U.asTypeOf(selectedIssueCompletion))
  val completionSourceOH = UIntToOH(postGrantCompletion.bits.source, mshrsAll)
  val completionSourceLease = Mux1H(completionSourceOH,
    snoopMshrs.map(_.io.tasks.preparedLease.held))
  val completionSourceVersionMatches = completionSourceLease.valid &&
    completionSourceLease.bits.asUInt === postGrantCompletion.bits.sourceToken.asUInt

  // Capture retiring source and target versions before either physical slot can be reused.
  class OwnerRetirement extends Bundle {
    val sourceId = UInt(mshrBits.W)
    val sourceGeneration = UInt(8.W)
    val sourceSet = UInt(setBits.W)
    val sourceTag = UInt(tagBits.W)
    val sourceEpoch = Bool()
    val targets = UInt(mshrsAll.W)
    val targetGenerations = Vec(mshrsAll, UInt(8.W))
    val targetSets = Vec(mshrsAll, UInt(setBits.W))
    val targetTags = Vec(mshrsAll, UInt(tagBits.W))
    val targetParents = Vec(mshrsAll, UInt(mshrBits.W))
    val targetParentEpochs = Vec(mshrsAll, Bool())
    val targetParentAttached = Vec(mshrsAll, Bool())
  }
  class SnoopReplaceReservation extends Bundle {
    val targetId = UInt(mshrBits.W)
    val targetGeneration = UInt(8.W)
    val sourceGeneration = UInt(8.W)
    val set = UInt(setBits.W)
    val tag = UInt(tagBits.W)
  }
  val snoopRetireEvents = snoopMshrs.zipWithIndex.map { case (source, i) =>
    val event = RegInit(0.U.asTypeOf(Valid(new OwnerRetirement)))
    val reservation = RegInit(0.U.asTypeOf(Valid(new SnoopReplaceReservation)))
    val retiring = source.io.msInfo.valid && source.io.msInfo.bits.willFree
    // Retain only the Replace route actually reserved by the captured snoop.
    when(source.io.alloc.valid) {
      val task = source.io.alloc.bits.task
      val ownerOH = UIntToOH(task.snpHitReleaseIdx, mshrsAll)
      reservation.valid := task.snpHitRelease
      reservation.bits.targetId := task.snpHitReleaseIdx
      reservation.bits.targetGeneration := Mux1H(ownerOH, mshrTaskGenerations(2))
      reservation.bits.sourceGeneration := mshrTaskGenerations(1)(i) + 1.U
      reservation.bits.set := task.set
      reservation.bits.tag := task.tag
      when(task.snpHitRelease) {
        val target = Mux1H(ownerOH, replaceMshrs.map(_.io.info))
        assert(target.valid && target.bits.set === task.set && target.bits.tag === task.tag,
          "a nested snoop must retain its reserved Replace owner until allocation")
      }
    }
    when(retiring) { reservation.valid := false.B }
    event.valid := retiring
    when(retiring) {
      event.bits.sourceId := i.U
      event.bits.sourceGeneration := mshrTaskGenerations(1)(i)
      event.bits.sourceSet := source.io.msInfo.bits.set
      event.bits.sourceTag := source.io.msInfo.bits.reqTag
      event.bits.sourceEpoch := false.B
      val completedTargets = VecInit(replaceMshrs.zipWithIndex.map { case (target, j) =>
        reservation.valid && reservation.bits.targetId === j.U &&
          reservation.bits.sourceGeneration === mshrTaskGenerations(1)(i) &&
          reservation.bits.targetGeneration === mshrTaskGenerations(2)(j) &&
          target.io.info.valid && target.io.info.bits.set === reservation.bits.set &&
          target.io.info.bits.tag === reservation.bits.tag
      }).asUInt
      event.bits.targets := completedTargets
      when(reservation.valid) {
        assert(reservation.bits.sourceGeneration === mshrTaskGenerations(1)(i) &&
          reservation.bits.set === source.io.msInfo.bits.set &&
          reservation.bits.tag === source.io.msInfo.bits.reqTag && completedTargets.orR,
          "snoop retirement must release its retained source and Replace generations")
      }
      event.bits.targetGenerations := VecInit(mshrTaskGenerations(2))
      event.bits.targetSets := VecInit(replaceMshrs.map(_.io.info.bits.set))
      event.bits.targetTags := VecInit(replaceMshrs.map(_.io.info.bits.tag))
      event.bits.targetParents := VecInit(replaceMshrs.map(_.io.info.bits.parentId))
      event.bits.targetParentEpochs := VecInit(replaceMshrs.map(_.io.info.bits.parentEpoch))
      event.bits.targetParentAttached := VecInit(replaceMshrs.map(_.io.info.bits.parentAttached))
    }
    event
  }
  val normalRetireEvents = refillMshrs.zipWithIndex.map { case (source, i) =>
    val event = RegInit(0.U.asTypeOf(Valid(new OwnerRetirement)))
    event.valid := source.io.replaceParentRelease
    when(source.io.replaceParentRelease) {
      event.bits.sourceId := i.U
      event.bits.sourceGeneration := mshrTaskGenerations(0)(i)
      event.bits.sourceSet := source.io.msInfo.bits.set
      event.bits.sourceTag := source.io.msInfo.bits.reqTag
      event.bits.sourceEpoch := source.io.allocEpoch
      event.bits.targets := VecInit(replaceMshrs.map { target =>
        target.io.info.valid && target.io.info.bits.parentAttached &&
          target.io.info.bits.parentId === i.U && target.io.info.bits.parentEpoch === source.io.allocEpoch
      }).asUInt
      event.bits.targetGenerations := VecInit(mshrTaskGenerations(2))
      event.bits.targetSets := VecInit(replaceMshrs.map(_.io.info.bits.set))
      event.bits.targetTags := VecInit(replaceMshrs.map(_.io.info.bits.tag))
      event.bits.targetParents := VecInit(replaceMshrs.map(_.io.info.bits.parentId))
      event.bits.targetParentEpochs := VecInit(replaceMshrs.map(_.io.info.bits.parentEpoch))
      event.bits.targetParentAttached := VecInit(replaceMshrs.map(_.io.info.bits.parentAttached))
    }
    event
  }
  val sinkCReleaseEvent = RegInit(0.U.asTypeOf(Valid(new TaskBundle)))
  sinkCReleaseEvent.valid := io.sinkCReleaseData.valid
  when(io.sinkCReleaseData.valid) {
    sinkCReleaseEvent.bits := io.sinkCReleaseData.bits
  }
  val sinkCReleaseOwner = RegInit(0.U(mshrsAll.W))
  val sinkCReleaseOwnerGeneration = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(8.W))))
  val sinkCReleaseOwnerCapture = VecInit(refillMshrs.map { refill =>
    io.sinkCReleaseData.valid && refill.io.status.valid &&
      refill.io.status.bits.set === io.sinkCReleaseData.bits.set &&
      refill.io.status.bits.reqTag === io.sinkCReleaseData.bits.tag
  }).asUInt
  when(io.sinkCReleaseData.valid) {
    sinkCReleaseOwner := sinkCReleaseOwnerCapture
    sinkCReleaseOwnerGeneration := VecInit(mshrTaskGenerations(0))
  }.otherwise {
    sinkCReleaseOwner := 0.U
  }
  val sinkCReleaseOwnerLive = VecInit(refillMshrs.zipWithIndex.map { case (refill, i) =>
    sinkCReleaseOwner(i) && refill.io.status.valid &&
      mshrTaskGenerations(0)(i) === sinkCReleaseOwnerGeneration(i)
  }).asUInt
  val postGrantReleaseLateMatch = VecInit(refillMshrs.map { refill =>
    sinkCReleaseEvent.valid && refill.io.status.valid &&
      refill.io.postGrantReleaseWindow &&
      refill.io.status.bits.set === sinkCReleaseEvent.bits.set &&
      refill.io.status.bits.reqTag === sinkCReleaseEvent.bits.tag
  }).asUInt
  io.toMainPipe.postGrantReleaseLate.valid := postGrantReleaseLateMatch.orR
  io.toMainPipe.postGrantReleaseLate.bits := sinkCReleaseEvent.bits
  // Register both a matching normal owner and a known absence before snoop preparation.
  class SnoopPair extends Bundle {
    val snoopGeneration = UInt(8.W)
    val set = UInt(setBits.W)
    val tag = UInt(tagBits.W)
    val normal = Valid(new Bundle {
      val id = UInt(mshrBits.W)
      val generation = UInt(8.W)
      val set = UInt(setBits.W)
      val tag = UInt(tagBits.W)
    })
    val postGrantPending = Bool()
    val payloadReady = Bool()
    val probePayload = Bool()
  }
  val snoopPairSnoopGenerationSeen = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(8.W))))
  val snoopPairNormalGenerationSeen = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(8.W))))
  val snoopPairs = snoopMshrs.zipWithIndex.map { case (snoop, i) =>
    val pair = RegInit(0.U.asTypeOf(Valid(new SnoopPair)))
    val normalMatches = VecInit(refillMshrs.map { normal =>
      snoop.io.status.valid && normal.io.status.valid &&
        normal.io.status.bits.set === snoop.io.status.bits.set &&
        normal.io.status.bits.reqTag === snoop.io.status.bits.reqTag
    }).asUInt
    pair.valid := snoop.io.status.valid
    when(snoop.io.status.valid) {
      assert(PopCount(normalMatches) <= 1.U, "snoop pair must identify one normal generation")
      pair.bits.snoopGeneration := mshrTaskGenerations(1)(i)
      pair.bits.set := snoop.io.status.bits.set
      pair.bits.tag := snoop.io.status.bits.reqTag
      pair.bits.normal.valid := normalMatches.orR
      pair.bits.normal.bits.id := OHToUInt(normalMatches)
      pair.bits.normal.bits.generation := Mux1H(normalMatches, mshrTaskGenerations(0))
      pair.bits.normal.bits.set := Mux1H(normalMatches, refillMshrs.map(_.io.status.bits.set))
      pair.bits.normal.bits.tag := Mux1H(normalMatches, refillMshrs.map(_.io.status.bits.reqTag))
      val releaseStarts = sinkCReleaseEvent.valid &&
        sinkCReleaseEvent.bits.set === snoop.io.status.bits.set &&
        sinkCReleaseEvent.bits.tag === snoop.io.status.bits.reqTag
      pair.bits.postGrantPending := Mux1H(normalMatches, refillMshrs.map(_.io.postGrantReleaseHold)) ||
        (releaseStarts && Mux1H(normalMatches, refillMshrs.map(_.io.postGrantReleaseWindow)))
      pair.bits.payloadReady := Mux1H(normalMatches, refillMshrs.map(_.io.postGrantReleasePayloadReady))
      pair.bits.probePayload := Mux1H(normalMatches, refillMshrs.map(_.io.refillPayloadFromProbeAck))
      snoopPairSnoopGenerationSeen(i) := mshrTaskGenerations(1)(i)
      when(normalMatches.orR) {
        snoopPairNormalGenerationSeen(OHToUInt(normalMatches)) :=
          Mux1H(normalMatches, mshrTaskGenerations(0))
      }
    }
    pair
  }
  val snoopPairKnown = snoopPairs.zipWithIndex.map { case (pair, i) =>
    pair.valid && pair.bits.snoopGeneration === snoopPairSnoopGenerationSeen(i)
  }
  val snoopPairNormalValid = snoopPairs.zipWithIndex.map { case (pair, i) =>
    snoopPairKnown(i) && pair.bits.normal.valid &&
      pair.bits.normal.bits.generation === snoopPairNormalGenerationSeen(pair.bits.normal.bits.id)
  }
  val sinkCRefillRecord = RegInit(0.U.asTypeOf(Valid(new MSHRBufWrite)))
  val sinkCRefillGeneration = RegInit(0.U(8.W))
  sinkCRefillRecord.valid := io.sinkCRefillWrite.valid
  when(io.sinkCRefillWrite.valid) {
    sinkCRefillRecord.bits := io.sinkCRefillWrite.bits
    sinkCRefillGeneration := Mux1H(UIntToOH(io.sinkCRefillWrite.bits.id, mshrsAll),
      mshrTaskGenerations(0))
  }
  val sinkCRefillOwnerOH = UIntToOH(sinkCRefillRecord.bits.id, mshrsAll)
  val sinkCRefillCurrent = Mux1H(sinkCRefillOwnerOH, refillMshrs.map(_.io.status.valid)) &&
    Mux1H(sinkCRefillOwnerOH, mshrTaskGenerations(0)) === sinkCRefillGeneration
  io.sinkCRefillWriteApplied := sinkCRefillRecord
  io.sinkCRefillWriteApplied.valid := sinkCRefillRecord.valid && sinkCRefillCurrent
  val sinkCRefillPending =
    Mux(io.sinkCRefillWrite.valid, UIntToOH(io.sinkCRefillWrite.bits.id, mshrsAll), 0.U(mshrsAll.W)) |
      Mux(sinkCRefillRecord.valid, sinkCRefillOwnerOH, 0.U(mshrsAll.W))
  val sinkCReleaseHold = RegInit(0.U(mshrsAll.W))
  val sinkCReleaseHoldSet = VecInit(refillMshrs.zipWithIndex.map { case (refill, i) =>
    val ownerPinned = sinkCReleaseOwnerLive(i)
    val eventOwner = sinkCReleaseEvent.valid && refill.io.status.valid &&
      refill.io.status.bits.set === sinkCReleaseEvent.bits.set &&
      refill.io.status.bits.reqTag === sinkCReleaseEvent.bits.tag
    (ownerPinned || eventOwner) &&
      (refill.io.msInfo.bits.blockRefill ||
        (refill.io.msInfo.bits.s_refill && refill.io.msInfo.bits.w_grantlast) ||
        refill.io.postGrantReleaseWindow || refill.io.postGrantReleaseHold)
  }).asUInt
  val nestedRefillPending = Wire(UInt(mshrsAll.W))
  val nestedRefillApplied = Wire(UInt(mshrsAll.W))
  val nestedRefillDirectApplied = Wire(UInt(mshrsAll.W))
  val sinkCReleaseDirectWriteMatch = VecInit(refillMshrs.map { refill =>
    io.fromMainPipe.releaseDataDirectWrite.valid && refill.io.status.valid &&
      refill.io.status.bits.set === io.fromMainPipe.releaseDataDirectWrite.bits.set &&
      refill.io.status.bits.reqTag === io.fromMainPipe.releaseDataDirectWrite.bits.tag
  }).asUInt
  val sinkCReleaseHoldClear = VecInit(refillMshrs.zipWithIndex.map { case (refill, i) =>
    (io.sinkCRefillWriteApplied.valid && io.sinkCRefillWriteApplied.bits.id === i.U) ||
      nestedRefillDirectApplied(i)
  }).asUInt
  val sinkCReleaseHoldLive = VecInit(refillMshrs.map(_.io.status.valid)).asUInt
  sinkCReleaseHold :=
    (sinkCReleaseHold | sinkCReleaseHoldSet) & ~sinkCReleaseHoldClear & sinkCReleaseHoldLive
  when(sinkCRefillRecord.valid) {
    assert(sinkCRefillCurrent, "SinkC refill route lost its reserved normal generation")
  }
  val nestedReleasePending = Wire(Vec(2, UInt(mshrsAll.W)))
  val nestedReleaseDone = Wire(Vec(2, UInt(mshrsAll.W)))
  val mshrs: Seq[MSHRContextBase] = refillMshrs.zip(snoopMshrs).flatMap {
    case (refill, snoop) => Seq[MSHRContextBase](refill, snoop)
  }
  val contextPhysicalIds = Seq.tabulate(mshrsAll)(i => Seq(i, i)).flatten
  io.refillVictimDelegated := VecInit(refillMshrs.map(_.io.sidecarDelegated)).asUInt
  require(mshrs.size == 2 * mshrsAll)
  require(mshrsAll >= 2, "independent ReplaceMSHR needs one reservable physical slot")

  val replRespHasVictim = io.replResp.valid && !io.replResp.bits.hit && !io.replResp.bits.retry &&
    io.replResp.bits.meta.state =/= INVALID
  val replRespSidecarConflictOH = VecInit(
    replaceMshrs.map(m =>
      replRespHasVictim && m.io.info.valid &&
        m.io.info.bits.set === io.replResp.bits.set &&
        m.io.info.bits.tag === io.replResp.bits.tag
    )
  ).asUInt
  val replRespSidecarConflict = replRespSidecarConflictOH.orR
  val replRespSnoopConflictOH = VecInit(
    snoopMshrs.map(m =>
      replRespHasVictim && m.io.status.valid &&
        m.io.status.bits.set === io.replResp.bits.set &&
        m.io.status.bits.reqTag === io.replResp.bits.tag
    )
  ).asUInt
  val replRespSnoopConflict = replRespSnoopConflictOH.orR
  val replRespOwnerConflictCapture = replRespSidecarConflict || replRespSnoopConflict
  val replRespParentOHCapture = UIntToOH(io.replResp.bits.mshrId, mshrsAll)
  val replaceParentMatches = Seq.tabulate(mshrsAll) { parent =>
    VecInit(
      replaceMshrs.map(m =>
        m.io.info.valid && m.io.info.bits.parentAttached && m.io.info.bits.parentId === parent.U &&
          m.io.info.bits.parentEpoch === refillMshrs(parent).io.allocEpoch
      )
    ).asUInt
  }
  val replaceActiveForParent = replaceParentMatches.map(_.orR)

  val replResponseApplied = Wire(Valid(new ReplacerResult))
  val replaceReserveValid = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val replaceReserveSlot = Reg(Vec(mshrsAll, UInt(mshrBits.W)))
  val replaceReserveSlotOHByParent = Seq.tabulate(mshrsAll) { parent =>
    Mux(replaceReserveValid(parent), UIntToOH(replaceReserveSlot(parent), mshrsAll), 0.U(mshrsAll.W))
  }
  // Update the physical bitmap on the same edge as each parent reservation.
  val replaceReservedSlotOH = RegInit(0.U(mshrsAll.W))
  assert(replaceReservedSlotOH === replaceReserveSlotOHByParent.reduce(_ | _),
    "physical replacement reservations must agree with their parent records")
  class SnoopReservationToken extends Bundle {
    val valid = Bool()
    val slot = UInt(mshrBits.W)
    val set = UInt(setBits.W)
    val tag = UInt(tagBits.W)
    val txnID = UInt(TXNID_WIDTH.W)
    val source = UInt(sourceIdBits.W)
    val owner = UInt(mshrBits.W)
    val generation = UInt(8.W)
  }
  val snoopReservationSequence = RegInit(0.U(8.W))
  val snoopServiceToken = RegInit(0.U.asTypeOf(new SnoopReservationToken))
  val snoopPendingToken = RegInit(0.U.asTypeOf(new SnoopReservationToken))
  val snoopServiceValid = snoopServiceToken.valid
  val snoopServiceTask = Reg(new TaskBundle)
  val snoopServiceSlotOH = Mux(snoopServiceToken.valid,
    UIntToOH(snoopServiceToken.slot, mshrsAll), 0.U(mshrsAll.W))
  assert(
    PopCount(replaceReservedSlotOH) === PopCount(replaceReserveValid.asUInt),
    "each pending replacer lookup must own one unique S/R reservation"
  )
  val snoopStatusValidOH = VecInit(snoopMshrs.map(_.io.status.valid)).asUInt
  val replaceInfoValidOH = VecInit(replaceMshrs.map(_.io.info.valid)).asUInt
  val refillBlocksSnoopOH = VecInit(refillMshrs.map { refill =>
    refill.io.status.valid && refill.io.status.bits.w_c_resp
  }).asUInt
  val snoopBaseEligibleOH = ~(
    snoopStatusValidOH |
      replaceInfoValidOH |
      replaceReservedSlotOH |
      snoopServiceSlotOH |
      refillBlocksSnoopOH |
      (normalServiceSlotOH & Fill(mshrsAll, normalServiceCmo))
  )
  val replaceSlotUnreservedOH = VecInit((0 until mshrsAll).map { i =>
    !snoopMshrs(i).io.status.valid && !replaceMshrs(i).io.info.valid &&
      !replaceReservedSlotOH(i) && !snoopServiceSlotOH(i) &&
      !(normalServiceCmo && normalServiceSlotOH(i))
  }).asUInt
  val snoopAllocRequest = io.fromMainPipe.mshr_alloc_s3.valid &&
    io.fromMainPipe.mshr_alloc_s3.bits.task.fromB
  val snoopCandidateSeen = RegNext(io.fromReqArb.status_s1.bCandidateValid, false.B)
  val snoopEntering_s2 = RegNext(io.fromReqArb.status_s1.bFire, false.B)
  val snoopEntering_s3 = RegNext(snoopEntering_s2, false.B)
  val snoopEnteringDecision = RegNext(snoopEntering_s3, false.B)
  val snoopEnteringApply = RegNext(snoopEnteringDecision, false.B)
  when(io.fromReqArb.status_s1.bFire) {
    assert(snoopServiceToken.valid, "B entered the pipeline without a service token")
    assert(!snoopPendingToken.valid, "multiple B tokens entered the single service window")
    snoopPendingToken := snoopServiceToken
  }
  when(snoopEnteringApply) {
    assert(snoopServiceToken.valid, "B result applied without its reserved service slot")
    assert(snoopPendingToken.valid, "B result applied without its pending token")
    assert(snoopPendingToken.slot === snoopServiceToken.slot &&
      snoopPendingToken.set === snoopServiceToken.set &&
      snoopPendingToken.tag === snoopServiceToken.tag &&
      snoopPendingToken.generation === snoopServiceToken.generation &&
      snoopPendingToken.txnID === snoopServiceToken.txnID &&
      snoopPendingToken.source === snoopServiceToken.source &&
      snoopPendingToken.owner === snoopServiceToken.owner,
      "B apply consumed a different reservation token")
    snoopPendingToken.valid := false.B
    snoopServiceToken.valid := false.B
  }
  when(io.snoopAdmission.fire) {
    assert(!snoopServiceValid && snoopBaseEligibleOH.orR,
      "B service requires a unique available physical slot")
    val serviceSlot = OHToUInt(PriorityEncoderOH(snoopBaseEligibleOH))
    snoopServiceToken.valid := true.B
    snoopServiceToken.slot := serviceSlot
    snoopServiceToken.set := io.snoopAdmission.bits.set
    snoopServiceToken.tag := io.snoopAdmission.bits.tag
    snoopServiceToken.txnID := io.snoopAdmission.bits.txnID.get
    snoopServiceToken.source := io.snoopAdmission.bits.sourceId
    snoopServiceToken.owner := serviceSlot
    snoopServiceToken.generation := snoopReservationSequence + 1.U
    snoopReservationSequence := snoopReservationSequence + 1.U
    snoopServiceTask := io.snoopAdmission.bits
    when(io.snoopAdmission.bits.snpHitRelease) {
      val owner = Mux1H(UIntToOH(io.snoopAdmission.bits.snpHitReleaseIdx, mshrsAll),
        replaceMshrs.map(_.io.info))
      assert(owner.valid && owner.bits.set === io.snoopAdmission.bits.set &&
        owner.bits.tag === io.snoopAdmission.bits.tag,
        "captured B lost its sidecar owner before service reservation")
    }
  }
  when(io.bServiceStart.valid) {
    assert(snoopServiceValid &&
      snoopServiceTask.asUInt === io.bServiceStart.bits.asUInt,
      "B service must use its already-reserved immutable snoop snapshot")
  }
  val snoopPendingMatchesService = snoopPendingToken.valid && snoopServiceToken.valid &&
    snoopPendingToken.slot === snoopServiceToken.slot &&
    snoopPendingToken.set === snoopServiceToken.set &&
    snoopPendingToken.tag === snoopServiceToken.tag &&
    snoopPendingToken.generation === snoopServiceToken.generation &&
    snoopPendingToken.txnID === snoopServiceToken.txnID &&
    snoopPendingToken.source === snoopServiceToken.source &&
    snoopPendingToken.owner === snoopServiceToken.owner
  val snoopPendingExtra = snoopPendingToken.valid && !snoopPendingMatchesService
  val snoopPendingExtraSlotOH = Mux(snoopPendingExtra,
    UIntToOH(snoopPendingToken.slot, mshrsAll), 0.U(mshrsAll.W))
  val snoopTokenEligibleOH = snoopBaseEligibleOH & ~snoopPendingExtraSlotOH
  val snoopPendingCapacityBlock = snoopPendingExtra && !snoopTokenEligibleOH.orR
  val snoopReservationFence = io.sidecarReservationHold || snoopCandidateSeen ||
    snoopEntering_s2 || snoopEntering_s3 || snoopEnteringDecision || snoopEnteringApply
  val replaceReserveNeedParentOH = VecInit(refillMshrs.zipWithIndex.map {
    case (refill, i) => refill.io.replReadWanted && !replaceReserveValid(i)
  }).asUInt
  val replaceReserveGrantParentOH = PriorityEncoderOH(replaceReserveNeedParentOH)
  val replaceReserveGrantSlotOH = PriorityEncoderOH(replaceSlotUnreservedOH)
  val replaceReserveGrant = !snoopAllocRequest && !snoopReservationFence && !io.nestedwb.c_set_dirty &&
    replaceReserveNeedParentOH.orR && replaceSlotUnreservedOH.orR
  assert(
    !(snoopAllocRequest && replaceReserveGrant),
    "a snoop allocation must preempt a new ReplaceMSHR reservation"
  )
  val replaceReserveAllowsRead = Seq.tabulate(mshrsAll) { parent =>
    replaceReserveValid(parent)
  }
  val replaceReserveReleaseParent = (0 until mshrsAll).map { parent =>
    val reserveResponse = replResponseApplied.valid && replResponseApplied.bits.mshrId === parent.U
    val reserveStale = replaceReserveValid(parent) && !refillMshrs(parent).io.status.valid &&
      !refillMshrs(parent).io.replReadWanted && !refillMshrs(parent).io.replReadInFlight
    reserveResponse || reserveStale
  }
  (0 until mshrsAll).foreach { parent =>
    when(replaceReserveReleaseParent(parent)) {
      replaceReserveValid(parent) := false.B
    }
    when(replaceReserveGrant && replaceReserveGrantParentOH(parent)) {
      replaceReserveValid(parent) := true.B
      replaceReserveSlot(parent) := OHToUInt(replaceReserveGrantSlotOH)
    }
  }
  val replaceReserveReleaseSlots = VecInit((0 until mshrsAll).map { slot =>
    VecInit((0 until mshrsAll).map { parent =>
      replaceReserveReleaseParent(parent) && replaceReserveValid(parent) &&
        replaceReserveSlot(parent) === slot.U
    }).asUInt.orR
  }).asUInt
  replaceReservedSlotOH := (replaceReservedSlotOH & ~replaceReserveReleaseSlots) |
    Mux(replaceReserveGrant, replaceReserveGrantSlotOH, 0.U(mshrsAll.W))
  val replRespParentConflictCapture = replRespHasVictim &&
    Mux1H(replRespParentOHCapture, replaceActiveForParent)
  val replRespNeedsReplaceRawCapture = replRespHasVictim && !replRespOwnerConflictCapture && !replRespParentConflictCapture
  val replRespReservationOHCapture = Mux1H(replRespParentOHCapture, replaceReserveSlotOHByParent)
  val replRespNoReplaceSlotCapture = replRespNeedsReplaceRawCapture && !replRespReservationOHCapture.orR
  val replRespNeedsReplaceCapture = replRespNeedsReplaceRawCapture && !replRespNoReplaceSlotCapture
  io.toMainPipe.replRespRetry := replRespSnoopConflict || replRespParentConflictCapture || replRespNoReplaceSlotCapture
  val replRespNoProbeVictim = io.replResp.valid && !io.replResp.bits.hit && !io.replResp.bits.retry &&
    (io.replResp.bits.meta.state === INVALID || !io.replResp.bits.meta.clients.orR)
  io.toMainPipe.grantInstallOK := replRespNoProbeVictim &&
    !replRespOwnerConflictCapture && !replRespParentConflictCapture &&
    (io.replResp.bits.meta.state === INVALID || replRespReservationOHCapture.orR)
  io.toMainPipe.grantInstallSlot := OHToUInt(replRespReservationOHCapture)
  when(replRespSnoopConflict) {
    assert(PopCount(replRespSnoopConflictOH) === 1.U, "replacement victim matches multiple active snoop contexts")
  }
  class ReplacerDecision extends Bundle {
    val response = new ReplacerResult
    val parentGeneration = UInt(8.W)
    val parentEpoch = Bool()
    val reservation = UInt(mshrsAll.W)
    val slotGeneration = UInt(8.W)
    val needsReplace = Bool()
    val ownerConflict = Bool()
    val parentConflict = Bool()
    val noReplaceSlot = Bool()
  }
  val replDecision = RegInit(0.U.asTypeOf(Valid(new ReplacerDecision)))
  replDecision.valid := io.replResp.valid
  when(io.replResp.valid) {
    replDecision.bits.response := io.replResp.bits
    replDecision.bits.parentGeneration := Mux1H(replRespParentOHCapture, mshrTaskGenerations(0))
    replDecision.bits.parentEpoch := Mux1H(replRespParentOHCapture, refillMshrs.map(_.io.allocEpoch))
    replDecision.bits.reservation := replRespReservationOHCapture
    replDecision.bits.slotGeneration := Mux1H(replRespReservationOHCapture, mshrTaskGenerations(2))
    replDecision.bits.needsReplace := replRespNeedsReplaceCapture
    replDecision.bits.ownerConflict := replRespOwnerConflictCapture
    replDecision.bits.parentConflict := replRespParentConflictCapture
    replDecision.bits.noReplaceSlot := replRespNoReplaceSlotCapture
  }
  io.toMainPipe.replParentGeneration := Mux1H(replRespParentOHCapture, mshrTaskGenerations(0))
  val replRespParentOH = UIntToOH(replDecision.bits.response.mshrId, mshrsAll)
  val replDecisionCurrent = Mux1H(replRespParentOH, refillMshrs.map(_.io.status.valid)) &&
    Mux1H(replRespParentOH, mshrTaskGenerations(0)) === replDecision.bits.parentGeneration
  replResponseApplied.valid := replDecision.valid && replDecisionCurrent
  replResponseApplied.bits := replDecision.bits.response
  val replSlotCurrent = !replDecision.bits.needsReplace ||
    (replDecision.bits.reservation.orR &&
      Mux1H(replDecision.bits.reservation, mshrTaskGenerations(2)) === replDecision.bits.slotGeneration)
  val replRespNeedsReplace = replResponseApplied.valid && replDecision.bits.needsReplace && replSlotCurrent
  val replRespReservationOH = replDecision.bits.reservation
  val replRespOwnerConflict = replDecision.bits.ownerConflict
  val replRespParentConflict = replDecision.bits.parentConflict
  val replRespNoReplaceSlot = replDecision.bits.noReplaceSlot
  when(replDecision.valid) {
    assert(replDecisionCurrent, "replacer decision lost its retained normal parent")
    assert(replSlotCurrent, "replacer decision lost its retained sidecar slot generation")
  }
  val nestedReleaseParentOH = VecInit(refillMshrs.map(_.io.nestedReleaseNeedsReplace)).asUInt
  val nestedReleaseNeedsReplace = nestedReleaseParentOH.orR &&
    !Mux1H(nestedReleaseParentOH, replaceActiveForParent)
  val snoopNestedwbActive = VecInit(snoopMshrs.map(_.io.nestedwbData)).asUInt
  val refillNestedwbActive = VecInit(refillMshrs.map { refill =>
    io.nestedwb.c_set_dirty && refill.io.status.valid &&
    refill.io.status.bits.set === io.nestedwb.set &&
    refill.io.status.bits.reqTag === io.nestedwb.tag
  }).asUInt
  when(io.nestedwb.c_set_dirty) {
    assert(PopCount(refillNestedwbActive) <= 1.U, "ReleaseData matches multiple normal refill contexts")
  }
  val refillBufReleaseOwner = RegInit(0.U(mshrsAll.W))
  val normalAllocOH = VecInit(refillMshrs.map(_.io.alloc.valid)).asUInt
  val sinkCProbeNormalOwnerHold = WireDefault(0.U(mshrsAll.W))
  refillBufReleaseOwner := (refillBufReleaseOwner | refillNestedwbActive) & ~normalAllocOH
  val sinkCRefillAuthority = sinkCReleaseHold | sinkCRefillPending |
    (sinkCProbeNormalOwnerHold & ~Fill(mshrsAll, io.sinkCReleaseData.valid))
  io.refillBufReleaseOwner := refillBufReleaseOwner | refillNestedwbActive | sinkCRefillAuthority
  val orphanReleaseCandidate = io.fromMainPipe.orphanReleaseData &&
    !nestedReleaseParentOH.orR && !snoopNestedwbActive.orR &&
    !refillNestedwbActive.orR
  val orphanReplaceTargetOH = VecInit(
    replaceMshrs.map(m =>
      orphanReleaseCandidate && m.io.info.valid &&
        (!m.io.info.bits.releaseAck || m.io.acceptsLateReleaseData) &&
        m.io.info.bits.set === io.nestedwb.set && m.io.info.bits.tag === io.nestedwb.tag
    )
  ).asUInt
  when(orphanReleaseCandidate) {
    assert(PopCount(orphanReplaceTargetOH) <= 1.U, "orphan ReleaseData matches multiple active ReplaceMSHRs")
  }
  val orphanReleaseTargetsReplace = orphanReplaceTargetOH.orR
  val orphanReleaseData = orphanReleaseCandidate && !orphanReleaseTargetsReplace
  val replaceSlotIdle = replaceSlotUnreservedOH &
    ~Mux(replaceReserveGrant, replaceReserveGrantSlotOH, 0.U(mshrsAll.W))
  val cmoAlloc = io.fromMainPipe.mshr_alloc_s3.valid &&
    io.fromMainPipe.mshr_alloc_s3.bits.task.fromA &&
    normalCmo(io.fromMainPipe.mshr_alloc_s3.bits.task)
  val cmoAllocTask = io.fromMainPipe.mshr_alloc_s3.bits.task
  val cmoAllocDir = io.fromMainPipe.mshr_alloc_s3.bits.dirResult
  val replaceNeedsAlloc = replRespNeedsReplace || nestedReleaseNeedsReplace ||
    orphanReleaseData || cmoAlloc
  val replaceParentOH = Mux(replRespNeedsReplace, replRespParentOH,
    nestedReleaseParentOH)
  val replaceParentId = OHToUInt(replaceParentOH)
  assert(
    PopCount(Cat(replRespNeedsReplace, nestedReleaseNeedsReplace, orphanReleaseData, cmoAlloc)) <= 1.U,
    "two independent ReplaceMSHR allocations requested in one cycle"
  )
  when(nestedReleaseNeedsReplace) {
    assert(PopCount(nestedReleaseParentOH) === 1.U, "a nested ReleaseData must match exactly one normal MSHR")
  }

  val nestedVictim = WireInit(0.U.asTypeOf(new DirResult))
  nestedVictim.hit := false.B
  nestedVictim.tag := Mux1H(nestedReleaseParentOH, refillMshrs.map(_.io.msInfo.bits.metaTag))
  nestedVictim.set := Mux1H(nestedReleaseParentOH, refillMshrs.map(_.io.msInfo.bits.set))
  nestedVictim.way := Mux1H(nestedReleaseParentOH, refillMshrs.map(_.io.msInfo.bits.way))
  nestedVictim.meta := Mux1H(nestedReleaseParentOH, refillMshrs.map(_.io.msInfo.bits.meta))
  nestedVictim.meta.dirty := true.B
  nestedVictim.meta.state := TIP
  nestedVictim.meta.clients := Fill(clientBits, false.B)
  nestedVictim.error := false.B
  nestedVictim.replacerInfo := 0.U.asTypeOf(new ReplacerInfo)
  val incomingVictim = WireInit(nestedVictim)
  when(replRespNeedsReplace) {
    incomingVictim.tag := replResponseApplied.bits.tag
    incomingVictim.set := replResponseApplied.bits.set
    incomingVictim.way := replResponseApplied.bits.way
    incomingVictim.meta := replResponseApplied.bits.meta
  }
  when(cmoAlloc) {
    incomingVictim := cmoAllocDir
    incomingVictim.tag := Mux(cmoAllocTask.cmoAll, cmoAllocDir.tag, cmoAllocTask.tag)
  }
  when(orphanReleaseData) {
    incomingVictim.tag := io.nestedwb.tag
    incomingVictim.set := io.nestedwb.set
    incomingVictim.way := 0.U
    incomingVictim.meta := MetaEntry(
      dirty = true.B,
      state = TIP,
      clients = Fill(clientBits, false.B),
      alias = Some(0.U)
    )
  }

  val incomingReplaceReq = WireInit(0.U.asTypeOf(new ReplaceRequest))
  incomingReplaceReq.parentId := replaceParentId
  incomingReplaceReq.parentEpoch := Mux(replRespNeedsReplace, replDecision.bits.parentEpoch,
    Mux1H(replaceParentOH, refillMshrs.map(_.io.allocEpoch)))
  incomingReplaceReq.parentAttached := !orphanReleaseData && !cmoAlloc
  incomingReplaceReq.dirResult := incomingVictim
  incomingReplaceReq.mode := Mux(cmoAlloc,
    Mux(cmoAllocTask.opcode === CBOClean, ReplaceMSHRMode.cmoClean,
      Mux(cmoAllocTask.opcode === CBOFlush, ReplaceMSHRMode.cmoFlush, ReplaceMSHRMode.cmoInval)),
    ReplaceMSHRMode.victim)
  incomingReplaceReq.needRelease := !cmoAlloc || !io.fromMainPipe.mshr_alloc_s3.bits.state.s_release
  incomingReplaceReq.needProbe := (replRespNeedsReplace || cmoAlloc) &&
    incomingReplaceReq.needRelease && incomingVictim.meta.clients.orR
  incomingReplaceReq.probeParam := Mux(
    cmoAlloc && incomingReplaceReq.mode === ReplaceMSHRMode.cmoClean, toB, toN)
  incomingReplaceReq.captureFromDS := (replRespNeedsReplace &&
    !io.fromMainPipe.grantInstallDone.valid) || cmoAlloc && incomingReplaceReq.needRelease
  incomingReplaceReq.task := 0.U.asTypeOf(new TaskBundle)
  incomingReplaceReq.task.set := incomingVictim.set
  incomingReplaceReq.task.tag := incomingVictim.tag
  when(cmoAlloc) { incomingReplaceReq.task := cmoAllocTask }
  val incomingReplaceInfo = WireInit(0.U.asTypeOf(new ReplaceMSHRInfo))
  incomingReplaceInfo.valid := replaceNeedsAlloc
  incomingReplaceInfo.parentId := replaceParentId
  incomingReplaceInfo.parentEpoch := incomingReplaceReq.parentEpoch
  incomingReplaceInfo.parentAttached := incomingReplaceReq.parentAttached
  incomingReplaceInfo.ownsWay := incomingReplaceReq.parentAttached ||
    cmoAlloc && incomingVictim.hit && isValid(incomingVictim.meta.state)
  incomingReplaceInfo.set := incomingVictim.set
  incomingReplaceInfo.tag := incomingVictim.tag
  incomingReplaceInfo.way := incomingVictim.way
  incomingReplaceInfo.meta := incomingVictim.meta

  val selectedReplaceSlotOH = Mux(
    cmoAlloc, normalServiceSlotOH,
    Mux(replRespNeedsReplace, replRespReservationOH, PriorityEncoderOH(replaceSlotIdle))
  )

  (0 until mshrsAll).foreach { i =>
    val incomingForParent = replaceNeedsAlloc && incomingReplaceReq.parentAttached && replaceParentOH(i)
    when(incomingForParent) {
      assert(refillMshrs(i).io.status.valid, "replacement result without a live parent normal MSHR")
    }
  }
  when(replaceNeedsAlloc) {
    assert(selectedReplaceSlotOH.orR, "replacement allocation arrived without an S/R-mutual slot")
  }
  when(io.fromMainPipe.grantInstallDone.valid && replRespNeedsReplace) {
    assert(
      io.fromMainPipe.grantInstallDone.bits === replaceParentId,
      "a merged grant install must pair with the same parent's replacement allocation"
    )
  }

  val replaceTargetMatches = Seq.tabulate(mshrsAll) { parent =>
    replaceParentMatches(parent) |
      Mux(replaceNeedsAlloc && incomingReplaceReq.parentAttached && replaceParentOH(parent), selectedReplaceSlotOH, 0.U)
  }
  val replaceNestedAllocParentOH = Mux(nestedReleaseNeedsReplace, nestedReleaseParentOH, 0.U(mshrsAll.W))
  val replaceNestedSlotOH = PriorityEncoderOH(replaceSlotIdle)
  val replaceNestedTargetMatches = Seq.tabulate(mshrsAll) { parent =>
    replaceParentMatches(parent) |
      Mux(replaceNestedAllocParentOH(parent), replaceNestedSlotOH, 0.U(mshrsAll.W))
  }
  assert(!(replResponseApplied.valid && io.nestedwb.c_set_dirty),
    "refill result and nested C must correspond to distinct shared-S3 tasks")
  when(nestedReleaseNeedsReplace) {
    assert(replaceNestedSlotOH === selectedReplaceSlotOH,
      "nested payload target must equal the actual allocated physical slot")
  }
  val parentAlreadyHasChild = incomingReplaceReq.parentAttached &&
    Mux1H(replaceParentOH, replaceActiveForParent)
  val replaceSlotAvailable = selectedReplaceSlotOH.orR
  assert(replaceSlotAvailable === selectedReplaceSlotOH.orR,
    "replacement availability must equal the selected-slot nonzero test")
  val replaceAllocValid = replaceNeedsAlloc && replaceSlotAvailable &&
    (!incomingReplaceReq.parentAttached || !parentAlreadyHasChild)
  assert(
    !(replaceNeedsAlloc && incomingReplaceReq.parentAttached && parentAlreadyHasChild),
    "replacer response for a parent that already owns a child ReplaceMSHR"
  )
  // Advertise the winning raw allocation address as one complete registered descriptor, before slot acceptance.
  val replaceAllocSnoopSources = Seq(
    (nestedReleaseNeedsReplace, nestedVictim.set, nestedVictim.tag),
    (replRespNeedsReplace, replResponseApplied.bits.set, replResponseApplied.bits.tag),
    (cmoAlloc, incomingVictim.set, incomingVictim.tag),
    (orphanReleaseData, io.nestedwb.set, io.nestedwb.tag)
  )
  val replaceAllocOriginRequested = ReplacementAllocAddress.requestedOrigins(
    replaceAllocSnoopSources.map(_._1))
  replaceAllocSnoopSources.zipWithIndex.foreach { case ((_, set, tag), i) =>
    io.replaceAllocForSnoop(i).valid := RegNext(replaceAllocOriginRequested(i), false.B)
    io.replaceAllocForSnoop(i).bits.set := RegEnable(set, replaceAllocOriginRequested(i))
    io.replaceAllocForSnoop(i).bits.tag := RegEnable(tag, replaceAllocOriginRequested(i))
    when(replaceAllocOriginRequested(i) && replaceAllocValid) {
      assert(set === incomingVictim.set && tag === incomingVictim.tag,
        "snoop allocation address must equal the accepted replacement victim")
    }
  }

  (0 until mshrsAll).foreach { parent =>
    assert(
      PopCount(replaceParentMatches(parent)) <= 1.U,
      "one normal MSHR must not own multiple ReplaceMSHR transactions"
    )
    assert(PopCount(replaceTargetMatches(parent)) <= 1.U, "one normal MSHR must have one physical ReleaseBuf target")
  }

  val pipeContextCount = PopCount(Cat(io.pipeStatusVec.map(_.valid)))
  val srContextCount = PopCount(Cat((0 until mshrsAll).map { i =>
    snoopMshrs(i).io.status.valid || replaceMshrs(i).io.info.valid ||
      replaceReservedSlotOH(i) || snoopServiceSlotOH(i) ||
      normalServiceCmo && normalServiceSlotOH(i)
  }))
  def allocationCredit(used: UInt): UInt = {
    val credit = RegInit(0.U(log2Ceil(mshrsAll + 1).W))
    credit := Mux(used >= mshrsAll.U, 0.U, mshrsAll.U - used)
    credit
  }
  val normalSlotEligibleOH = Mux(normalCmo(io.normalReserveRequest.bits),
    replaceSlotIdle, VecInit(refillMshrs.zipWithIndex.map { case (refill, i) =>
      !refill.io.status.valid && !(normalServiceSlotOH(i) && !normalServiceCmo)
    }).asUInt)
  val normalSlotPickOH = PriorityEncoderOH(normalSlotEligibleOH)
  io.normalReserveRequest.ready := !normalService.valid && normalSlotEligibleOH.orR &&
    (!normalCmo(io.normalReserveRequest.bits) ||
      (PopCount(replaceSlotIdle) > 1.U && !snoopReservationFence &&
        !replaceNeedsAlloc && !io.snoopAdmission.fire))
  io.normalReserveAck.valid := normalService.valid && !normalServiceInFlight
  io.normalReserveAck.bits := normalService.bits
  when(io.normalReserveRequest.fire) {
    normalService.valid := true.B
    normalServiceInFlight := false.B
    normalService.bits.id := OHToUInt(normalSlotPickOH)
    normalService.bits.task := io.normalReserveRequest.bits
    normalService.bits.token.generation := Mux(normalCmo(io.normalReserveRequest.bits),
      Mux1H(normalSlotPickOH, mshrTaskGenerations(2)),
      Mux1H(normalSlotPickOH, mshrTaskGenerations(0))) + 1.U
    normalService.bits.token.sequence := normalReservationSequence + 1.U
    normalReservationSequence := normalReservationSequence + 1.U
    assert(!io.normalReserveRequest.bits.mshrTask && io.normalReserveRequest.bits.fromA,
      "normal slot claims must come from the A/CMO Directory entrance")
  }
  when(io.normalReserveConsume) {
    assert(io.normalReserveAck.valid && !io.normalReserveCancel,
      "normal S1 must consume a live acknowledged reservation")
    normalServiceInFlight := true.B
  }
  when(io.normalReserveCancel) {
    assert(normalService.valid && !normalServiceInFlight && !io.normalReserveConsume,
      "only an unconsumed normal claim can be canceled")
    normalService.valid := false.B
  }
  val normalReservationDone = io.fromMainPipe.normalReservationDone
  val normalAllocTask = io.fromMainPipe.mshr_alloc_s3.bits.task
  val normalReservationIdentityMatch = normalService.valid && normalServiceInFlight &&
    normalReservationDone.valid &&
    normalReservationDone.bits.mshrId === normalService.bits.id &&
    normalReservationDone.bits.token.asUInt === normalService.bits.token.asUInt &&
    normalAllocTask.fromA && !normalAllocTask.mshrTask &&
    normalAllocTask.sourceId === normalService.bits.task.sourceId

  io.toMainPipe.normalAllocAdmission := normalService.valid && normalServiceInFlight
  io.toMainPipe.normalAllocToken := normalService.bits.token
  io.toMainPipe.normalAllocOwner := normalService.bits.id
  io.toMainPipe.normalAllocSource := normalService.bits.task.sourceId
  when(normalReservationDone.valid && normalReservationIdentityMatch) {
    normalService.valid := false.B
    normalServiceInFlight := false.B
  }
  when(normalService.valid) {
    val occupied = Mux(normalServiceCmo,
      Mux1H(normalServiceSlotOH, replaceMshrs.map(_.io.info.valid)),
      Mux1H(normalServiceSlotOH, refillMshrs.map(_.io.status.valid)))
    val generation = Mux(normalServiceCmo,
      Mux1H(normalServiceSlotOH, mshrTaskGenerations(2)),
      Mux1H(normalServiceSlotOH, mshrTaskGenerations(0)))
    assert(!occupied, "a reserved A slot must stay free until its own allocation")
    assert(generation + 1.U === normalService.bits.token.generation,
      "A reservation must retain its selected pool's next allocation generation")
  }
  val snoopCredit = allocationCredit(srContextCount +& pipeContextCount +&
    io.contextReservation.asUInt +& replaceReserveGrant.asUInt +&
    cmoAlloc.asUInt +& io.snoopAdmission.fire.asUInt)
  val normalMshrFull = !normalService.valid && !normalSlotEligibleOH.orR
  val snoopMshrFull = snoopCredit === 0.U
  val mshrFull = snoopMshrFull
  val a_mshrFull = normalMshrFull
  val mshrSelector = Module(new MSHRSelector())
  val allocIsSnoop = io.fromMainPipe.mshr_alloc_s3.bits.task.fromB
  val selectedMSHROH = Mux(allocIsSnoop,
    Mux(mshrSelector.io.out.valid, mshrSelector.io.out.bits, 0.U(mshrsAll.W)),
    normalServiceSlotOH)

  io.l2Miss := Cat(refillMshrs.map { m =>
    m.io.status.valid && m.io.status.bits.channel(0) && (
      m.io.status.bits.reqSource === MemReqSource.CPULoadData.id.U ||
      m.io.status.bits.reqSource === MemReqSource.CPUStoreData.id.U
    )
  }).orR
  val pendingToNValid = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val pendingToNSet = Reg(Vec(mshrsAll, UInt(setBits.W)))
  val pendingToNTag = Reg(Vec(mshrsAll, UInt(tagBits.W)))
  val pendingToNHasSnoopContext = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val pendingToNSnoopId = Reg(Vec(mshrsAll, UInt(mshrBits.W)))
  val pendingToNSnoopGeneration = Reg(Vec(mshrsAll, UInt(8.W)))
  val pendingToNDirectPath = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val bToNFire = io.bServiceStart.valid && isSnpToN(io.bServiceStart.bits.chiOpcode.get)
  val pendingToNFreeOH = PriorityEncoderOH(VecInit(pendingToNValid.map(x => !x)).asUInt)
  val pendingToNFull = !pendingToNFreeOH.orR
  io.snoopAdmission.ready := !snoopServiceValid && snoopBaseEligibleOH.orR &&
    !nestedReleaseNeedsReplace && !orphanReleaseData &&
    !io.replResp.valid && !replResponseApplied.valid &&
    !(isSnpToN(io.snoopAdmission.bits.chiOpcode.get) && pendingToNFull)
  val pendingToNAllocated = VecInit((0 until mshrsAll).map { i =>
    pendingToNValid(i) && io.fromMainPipe.mshr_alloc_s3.valid &&
      io.fromMainPipe.mshr_alloc_s3.bits.task.fromB &&
      isSnpToN(io.fromMainPipe.mshr_alloc_s3.bits.task.chiOpcode.get) &&
      io.fromMainPipe.mshr_alloc_s3.bits.task.set === pendingToNSet(i) &&
      io.fromMainPipe.mshr_alloc_s3.bits.task.tag === pendingToNTag(i)
  })
  val pendingToNDirectDone = VecInit((0 until mshrsAll).map { i =>
    pendingToNValid(i) && io.fromMainPipe.snoopToNDirectDone.valid &&
      io.fromMainPipe.snoopToNDirectDone.bits.set === pendingToNSet(i) &&
      io.fromMainPipe.snoopToNDirectDone.bits.tag === pendingToNTag(i)
  })
  val pendingToNContextWillFree = VecInit((0 until mshrsAll).map { i =>
    val matches = RegNext(VecInit(snoopRetireEvents.map { event =>
      event.valid && pendingToNValid(i) && pendingToNHasSnoopContext(i) &&
        event.bits.sourceId === pendingToNSnoopId(i) &&
        event.bits.sourceGeneration === pendingToNSnoopGeneration(i) &&
        event.bits.sourceSet === pendingToNSet(i) && event.bits.sourceTag === pendingToNTag(i)
    }).asUInt, 0.U(mshrsAll.W))
    RegNext(matches.orR, false.B)
  })
  val pendingToNDirectRespDone = VecInit((0 until mshrsAll).map { i =>
    val matches = RegNext(VecInit(Seq(io.txdatDone, io.txrspDone).map { done =>
      done.valid && pendingToNValid(i) && done.bits.fromB && !done.bits.mshrTask &&
        done.bits.set === pendingToNSet(i) && done.bits.tag === pendingToNTag(i)
    }).asUInt, 0.U(2.W))
    RegNext(matches.orR, false.B)
  })
  (0 until mshrsAll).foreach { i =>
    when(pendingToNValid(i)) {
      when(pendingToNAllocated(i)) {
        pendingToNHasSnoopContext(i) := true.B
        pendingToNSnoopId(i) := OHToUInt(selectedMSHROH)
        pendingToNSnoopGeneration(i) := Mux1H(selectedMSHROH,
          mshrTaskGenerations(1)) + 1.U
      }
      when(pendingToNDirectDone(i) && !pendingToNAllocated(i)) {
        pendingToNDirectPath(i) := true.B
      }
      when(pendingToNDirectPath(i) && pendingToNDirectRespDone(i)) {
        pendingToNValid(i) := false.B
        pendingToNHasSnoopContext(i) := false.B
        pendingToNDirectPath(i) := false.B
      }.elsewhen(pendingToNHasSnoopContext(i) && pendingToNContextWillFree(i)) {
        pendingToNValid(i) := false.B
        pendingToNHasSnoopContext(i) := false.B
        pendingToNDirectPath(i) := false.B
      }
    }
  }
  when(bToNFire) {
    assert(!pendingToNFull, "toN snoop entered without a pending-snoop hold slot")
    pendingToNValid.zipWithIndex.foreach { case (valid, i) =>
      when(pendingToNFreeOH(i)) {
        valid := true.B
        pendingToNSet(i) := io.bServiceStart.bits.set
        pendingToNTag(i) := io.bServiceStart.bits.tag
        pendingToNHasSnoopContext(i) := false.B
        pendingToNDirectPath(i) := false.B
      }
    }
  }
  // Snoop and replacement share ReleaseBuf exclusively; normal independently owns RefillBuf.
  val allocIsCmo = normalCmo(io.fromMainPipe.mshr_alloc_s3.bits.task)
  val postGrantReleaseSnoopTargetOH = VecInit(refillMshrs.zipWithIndex.map { case (refill, i) =>
    allocIsSnoop && refill.io.status.valid &&
      (refill.io.postGrantReleaseHold || postGrantReleaseLateMatch(i)) &&
      refill.io.status.bits.set === io.fromMainPipe.mshr_alloc_s3.bits.task.set &&
      refill.io.status.bits.reqTag === io.fromMainPipe.mshr_alloc_s3.bits.task.tag
  }).asUInt
  when(allocIsSnoop) {
    assert(PopCount(postGrantReleaseSnoopTargetOH) <= 1.U,
      "one B snoop must match at most one post-Grant ReleaseData holder")
  }
  mshrSelector.io.idle := snoopMshrs.zipWithIndex.map { case (snoop, i) =>
    snoopServiceSlotOH(i) && !snoop.io.status.valid && !replaceMshrs(i).io.info.valid
  }
  io.toMainPipe.mshr_alloc_ptr := Mux(allocIsSnoop,
    OHToUInt(selectedMSHROH), normalService.bits.id)
  when(snoopAllocRequest) {
    assert(snoopServiceValid &&
      io.fromMainPipe.mshr_alloc_s3.bits.task.set === snoopServiceTask.set &&
      io.fromMainPipe.mshr_alloc_s3.bits.task.tag === snoopServiceTask.tag &&
      io.fromMainPipe.mshr_alloc_s3.bits.task.txnID.get === snoopServiceTask.txnID.get,
      "snoop allocation must consume its own registered service reservation")
    assert(!(snoopServiceSlotOH & (snoopStatusValidOH | replaceInfoValidOH | replaceReservedSlotOH)).orR,
      "B service slot was reused before transfer to SnoopMSHR")
  }
  when(io.fromMainPipe.mshr_alloc_s3.valid) {
    when(allocIsSnoop) {
      assert(mshrSelector.io.out.valid,
        "MainPipe snoop allocation must own an eligible physical slot")
    }.otherwise {
      val task = io.fromMainPipe.mshr_alloc_s3.bits.task
      assert(normalReservationDone.valid && normalService.valid && normalServiceInFlight &&
        task.fromA && task.mshrId === normalService.bits.id &&
        task.set === normalService.bits.task.set && task.tag === normalService.bits.task.tag &&
        task.sourceId === normalService.bits.task.sourceId,
        "MainPipe normal allocation must consume its own pre-Directory concrete slot")
      assert(!allocIsCmo || !(normalServiceSlotOH &
        (snoopStatusValidOH | replaceInfoValidOH | replaceReservedSlotOH | snoopServiceSlotOH)).orR,
        "a CMO reservation must retain exclusive S/R ownership until allocation")
    }
  }
  val activeSnoopOH = VecInit(snoopMshrs.map(_.io.status.valid)).asUInt
  val activeReplaceOH = VecInit(replaceMshrs.map(_.io.info.valid)).asUInt
  assert(!(activeSnoopOH & activeReplaceOH).orR,
    "SnoopMSHR and ReplaceMSHR must not own the same ReleaseBuf slot")

  val sinkCProbeResponse = io.resps.sinkC.respInfo.opcode === ProbeAck ||
    io.resps.sinkC.respInfo.opcode === ProbeAckData

  val replaceProbeSinkCMatch = Wire(Vec(mshrsAll, Bool()))
  val replaceReleaseDataMatch = VecInit(replaceMshrs.map { m =>
    sinkCReleaseEvent.valid && m.io.info.valid &&
      (!m.io.info.bits.releaseAck || m.io.acceptsLateReleaseData) &&
    sinkCReleaseEvent.bits.set === m.io.info.bits.set &&
    sinkCReleaseEvent.bits.tag === m.io.info.bits.tag
  })
  val replaceSinkCMatch = replaceProbeSinkCMatch
  val incomingReplaceProbeAckDataCapture = io.resps.sinkC.valid &&
    io.resps.sinkC.respInfo.last &&
    io.resps.sinkC.respInfo.opcode === ProbeAckData &&
    replRespNeedsReplaceCapture && replRespReservationOHCapture.orR &&
    io.resps.sinkC.set === io.replResp.bits.set &&
    io.resps.sinkC.tag === io.replResp.bits.tag
  val activeReplaceProbeAckDataOH = VecInit(replaceMshrs.zipWithIndex.map {
    case (m, i) =>
      io.resps.sinkC.valid && io.resps.sinkC.respInfo.last &&
        io.resps.sinkC.respInfo.opcode === ProbeAckData &&
        m.io.info.valid && !m.io.info.bits.releaseAck && !replaceProbeSinkCMatch(i) &&
        io.resps.sinkC.set === m.io.info.bits.set &&
        io.resps.sinkC.tag === m.io.info.bits.tag
  }).asUInt
  val activeReplaceProbeAckDataCapture = activeReplaceProbeAckDataOH.orR
  val mirroredReplaceProbeAckDataCapture =
    incomingReplaceProbeAckDataCapture || activeReplaceProbeAckDataCapture
  val mirroredReplaceProbeAckDataOH =
    Mux(activeReplaceProbeAckDataCapture, activeReplaceProbeAckDataOH, replRespReservationOHCapture)
  val mirroredReplaceProbeAckDataDoneOH = Wire(UInt(mshrsAll.W))
  val mirroredReplaceProbeAckDataPendingOH = Wire(UInt(mshrsAll.W))
  when(activeReplaceProbeAckDataCapture) {
    assert(PopCount(activeReplaceProbeAckDataOH) === 1.U, "ProbeAckData matches multiple active ReplaceMSHRs")
  }
  val cReleaseReplacerFence = RegInit(0.U(mshrsAll.W))
  val cReleaseReplacerFenceSet = VecInit(refillMshrs.map { refill =>
    sinkCReleaseEvent.valid && refill.io.status.valid &&
    !refill.io.msInfo.bits.w_replResp &&
    refill.io.status.bits.set === sinkCReleaseEvent.bits.set &&
    refill.io.status.bits.metaTag === sinkCReleaseEvent.bits.tag
  }).asUInt
  val cReleaseReplacerFenceClear = VecInit(refillMshrs.map { refill =>
    io.nestedwb.c_set_dirty && refill.io.status.valid &&
    refill.io.status.bits.set === io.nestedwb.set &&
    refill.io.status.bits.metaTag === io.nestedwb.tag
  }).asUInt
  val refillLiveOH = VecInit(refillMshrs.map(_.io.status.valid)).asUInt
  cReleaseReplacerFence :=
    ((cReleaseReplacerFence & ~cReleaseReplacerFenceClear) | cReleaseReplacerFenceSet) &
      refillLiveOH & ~normalAllocOH

  // Pin C owners before capturing the buffer route; all beats use that retained route.
  val probeLookupValid = RegInit(false.B)
  val probeRouteValid = RegInit(false.B)
  val probeNormalOwners = RegInit(0.U(mshrsAll.W))
  val probeSnoopOwners = RegInit(0.U(mshrsAll.W))
  val probeReplaceOwners = RegInit(0.U(mshrsAll.W))
  val probeNormalMirror = RegInit(0.U(mshrsAll.W))
  val probeOwnerGenerations = RegInit(VecInit(Seq.fill(3)(VecInit(Seq.fill(mshrsAll)(0.U(8.W))))))
  val probeQuery = io.sinkCProbeRouteRequest.bits
  val probeContextLookup = mshrs.indices.map { contextIdx =>
    val claim = sourceBProbeClaims(contextIdx)
    claim.valid && claim.generation === sourceBGenerations(contextIdx) &&
      probeQuery.set === claim.set && probeQuery.tag === claim.tag
  }
  val probeReplaceLookup = VecInit(replaceMshrs.indices.map { i =>
    val source = 2 * mshrsAll + i
    val claim = sourceBProbeClaims(source)
    claim.valid && claim.generation === sourceBGenerations(source) &&
      probeQuery.set === claim.set && probeQuery.tag === claim.tag
  }).asUInt
  val probeNormalLookup = VecInit((0 until mshrsAll).map(i => probeContextLookup(2 * i))).asUInt
  val probeSnoopLookup = VecInit((0 until mshrsAll).map(i => probeContextLookup(2 * i + 1))).asUInt
  val probeMirrorLookup = VecInit(refillMshrs.zip(snoopMshrs).zipWithIndex.map {
    case ((refill, snoop), i) =>
      probeContextLookup(2 * i + 1) && refill.io.status.valid && !refill.io.status.bits.will_free &&
        refill.io.status.bits.set === snoop.io.status.bits.set &&
        refill.io.status.bits.reqTag === snoop.io.status.bits.reqTag
  }).asUInt
  when(io.sinkCProbeRouteRequest.valid && !probeLookupValid) {
    probeLookupValid := true.B
    probeNormalOwners := probeNormalLookup
    probeSnoopOwners := probeSnoopLookup
    probeReplaceOwners := probeReplaceLookup
    probeNormalMirror := probeMirrorLookup
    probeOwnerGenerations := VecInit(mshrTaskGenerations.map(VecInit(_)))
    assert(PopCount(Cat(probeNormalLookup, probeSnoopLookup, probeReplaceLookup)) === 1.U,
      "ProbeAck first beat must reserve exactly one outstanding Probe owner")
  }
  class SinkCBufferRoute extends Bundle {
    val release = UInt(mshrBits.W)
    val refill = Valid(UInt(mshrBits.W))
    val mirror = Valid(UInt(mshrBits.W))
  }
  val probeBufferRoute = RegInit(0.U.asTypeOf(new SinkCBufferRoute))
  when(probeLookupValid && !probeRouteValid) {
    probeRouteValid := true.B
    probeBufferRoute.release := OHToUInt(probeSnoopOwners | probeReplaceOwners)
    probeBufferRoute.refill.valid := probeNormalOwners.orR
    probeBufferRoute.refill.bits := OHToUInt(probeNormalOwners)
    probeBufferRoute.mirror.valid := probeNormalMirror.orR
    probeBufferRoute.mirror.bits := OHToUInt(probeNormalMirror)
  }
  when(io.sinkCProbeRouteDone) {
    assert(probeLookupValid && probeRouteValid, "terminal C beat must return its reserved route")
    probeLookupValid := false.B
    probeRouteValid := false.B
  }
  io.sinkCProbeRouteReady := probeRouteValid
  val probeOwnerMasks = Seq(probeNormalOwners | probeNormalMirror, probeSnoopOwners, probeReplaceOwners)
  val sinkCProbeOwnerHold = probeOwnerMasks.map(mask => Mux(probeLookupValid, mask, 0.U(mshrsAll.W)))
  sinkCProbeNormalOwnerHold := sinkCProbeOwnerHold(0)
  probeOwnerMasks.zipWithIndex.foreach { case (mask, g) =>
    (0 until mshrsAll).foreach { i =>
      when(probeLookupValid && mask(i)) {
        assert(mshrTaskGenerations(g)(i) === probeOwnerGenerations(g)(i),
          "reserved C owner generation must stay pinned through its final beat")
      }
    }
  }
  val resp_sinkC_match_vec = (0 until 2 * mshrsAll).map { contextIdx =>
    if (contextIdx % 2 == 0) probeNormalOwners(contextIdx / 2) else probeSnoopOwners(contextIdx / 2)
  }
  replaceProbeSinkCMatch := VecInit((0 until mshrsAll).map(i => probeReplaceOwners(i)))
  val sinkCContextMatches = resp_sinkC_match_vec ++ replaceProbeSinkCMatch.toSeq
  sourceBProbeClaims.zipWithIndex.foreach { case (claim, source) =>
    when(io.resps.sinkC.valid && sinkCProbeResponse &&
        io.resps.sinkC.respInfo.last && sinkCContextMatches(source)) {
      assert(claim.valid && claim.generation === sourceBGenerations(source) &&
        claim.set === io.resps.sinkC.set && claim.tag === io.resps.sinkC.tag,
        "terminal ProbeAck must release exactly its accepted source/key/generation")
      claim.valid := false.B
    }
    when(claim.valid) {
      assert(claim.generation === sourceBGenerations(source),
        "accepted Probe claim must retain its source generation through the terminal C beat")
    }
  }
  when(io.resps.sinkC.valid && sinkCProbeResponse) {
    assert(
      PopCount(Cat(sinkCContextMatches)) === 1.U,
      "every SinkC ProbeAck must match exactly one live normal, snoop, or replace context"
    )
  }

  val nestedwbNormalActive = refillMshrs.map(_.io.nestedwbData)
  val nestedwbSnoopActive = snoopMshrs.map(_.io.nestedwbData)
  refillMshrs.zip(snoopMshrs).zipWithIndex.foreach {
    case ((refill, snoop), i) =>
      when(nestedwbNormalActive(i) && replaceNestedTargetMatches(i).orR) {
        val targetInfo = Mux1H(replaceNestedTargetMatches(i), replaceMshrs.map(_.io.info.bits))
        val targetSet = Mux(replaceNeedsAlloc && replaceParentOH(i), incomingReplaceReq.dirResult.set, targetInfo.set)
        val targetTag = Mux(replaceNeedsAlloc && replaceParentOH(i), incomingReplaceReq.dirResult.tag, targetInfo.tag)
        assert(
          io.nestedwb.set === targetSet && io.nestedwb.tag === targetTag,
          "nested ReleaseData must target its delegated replacement victim"
        )
      }
  }
  val replaceNestedwbCapture = replaceMshrs.zipWithIndex.map {
    case (_, replaceId) =>
      VecInit(nestedwbNormalActive.zipWithIndex.map {
        case (active, parentId) =>
          active && replaceNestedTargetMatches(parentId)(replaceId)
      }).asUInt.orR
  }

  val rxdatStaleSlotOH = WireDefault(VecInit(Seq.fill(mshrsAll)(false.B)))
  refillMshrs.zip(snoopMshrs).zipWithIndex.foreach {
    case ((refill, snoop), i) =>
      Seq[MSHRContextBase](refill, snoop).zipWithIndex.foreach {
        case (m, context) =>
          m.io.id := i.U
          m.io.sinkCOwnerHold := sinkCProbeOwnerHold(context)(i)
          m.io.alloc.valid := selectedMSHROH(i) && io.fromMainPipe.mshr_alloc_s3.valid &&
          (allocIsSnoop === context.B) && !cmoAlloc
          m.io.alloc.bits := io.fromMainPipe.mshr_alloc_s3.bits
          m.io.alloc.bits.task.isKeyword
            .foreach(_ := io.fromMainPipe.mshr_alloc_s3.bits.task.isKeyword.getOrElse(false.B))
          m.io.resps.sinkC.valid := io.resps.sinkC.valid && resp_sinkC_match_vec(2 * i + context)
          m.io.resps.sinkC.bits := io.resps.sinkC.respInfo
          m.io.nestedwb := io.nestedwb
          m.io.aMergeTask.valid := io.aMergeTask.valid && io.aMergeTask.bits.idOH(i) && !context.B
          m.io.aMergeTask.bits := io.aMergeTask.bits.task
      }

      val rxdatForThisSlot = io.resps.rxdat.valid && io.resps.rxdat.mshrId === i.U
      val rxdatDBIDMismatch = refill.io.readDBID.valid &&
        io.resps.rxdat.respInfo.dbID.get =/= refill.io.readDBID.bits
      // Reject responses whose TxnID epoch belongs to an earlier tenant of the slot.
      val readTxnEpochWidth = (TXNID_WIDTH - bankBits - 2) - mshrBits
      val rxdatEpochMismatch = if (readTxnEpochWidth > 0) {
        io.resps.rxdat.txnID(TXNID_WIDTH - bankBits - 3, mshrBits) =/= refill.io.readTxnEpoch
      } else {
        false.B
      }
      rxdatStaleSlotOH(i) := rxdatForThisSlot && (rxdatDBIDMismatch || rxdatEpochMismatch)
      refill.io.resps.rxdat.valid := refill.io.status.valid && rxdatForThisSlot &&
        !rxdatDBIDMismatch && !rxdatEpochMismatch
      refill.io.resps.rxdat.bits := io.resps.rxdat.respInfo
      val rxrspEpochMismatch = if (readTxnEpochWidth > 0) {
        io.resps.rxrsp.txnID(TXNID_WIDTH - bankBits - 3, mshrBits) =/= refill.io.readTxnEpoch
      } else {
        false.B
      }
      refill.io.resps.rxrsp.valid := refill.io.status.valid && io.resps.rxrsp.valid &&
        io.resps.rxrsp.mshrId === i.U && !rxrspEpochMismatch
      refill.io.resps.rxrsp.bits := io.resps.rxrsp.respInfo
      refill.io.replResp.valid := replResponseApplied.valid && replResponseApplied.bits.mshrId === i.U
      val replRespForRefill = WireInit(replResponseApplied.bits)
      when(replRespOwnerConflict || replRespParentConflict || replRespNoReplaceSlot) {
        replRespForRefill.retry := true.B
      }
      refill.io.replResp.bits := replRespForRefill
      refill.io.grantInstallDone :=
        io.fromMainPipe.grantInstallDone.valid && io.fromMainPipe.grantInstallDone.bits === i.U
      refill.io.tasks.txdat.ready := false.B
      val parentReplaceMatch = replaceParentMatches(i)
      val parentReplaceInfo = WireInit(0.U.asTypeOf(new ReplaceMSHRInfo))
      when(parentReplaceMatch.orR) {
        parentReplaceInfo := Mux1H(parentReplaceMatch, replaceMshrs.map(_.io.info.bits))
      }
      refill.io.replaceActive := replaceActiveForParent(i)
      refill.io.replaceAccepted := replaceActiveForParent(i) ||
        replaceAllocValid && incomingReplaceReq.parentAttached && replaceParentOH(i)
      val pendingSnoopForRefill = snoopCandidateSeen &&
        refill.io.status.valid &&
        refill.io.status.bits.set === io.fromReqArb.status_s1.b_set &&
        refill.io.status.bits.reqTag === io.fromReqArb.status_s1.b_tag
      val nestedBToNForRefill = io.nestedwb.b_toN.getOrElse(false.B) &&
        refill.io.status.valid &&
        refill.io.status.bits.set === io.nestedwb.set &&
        refill.io.status.bits.reqTag === io.nestedwb.tag
      val releaseDataBeforeProbeAck = sinkCReleaseEvent.valid &&
        refill.io.status.valid && !refill.io.msInfo.bits.w_rprobeacklast &&
        refill.io.status.bits.set === sinkCReleaseEvent.bits.set &&
        refill.io.status.bits.reqTag === sinkCReleaseEvent.bits.tag
      val postGrantReleaseData = sinkCReleaseEvent.valid &&
        refill.io.postGrantReleaseWindow &&
        refill.io.status.bits.set === sinkCReleaseEvent.bits.set &&
        refill.io.status.bits.reqTag === sinkCReleaseEvent.bits.tag
      val snoopHoldsRefillWay = snoopMshrs.map { candidate =>
        candidate.io.msInfo.valid && candidate.io.msInfo.bits.blockRefill &&
          candidate.io.msInfo.bits.set === refill.io.status.bits.set &&
          candidate.io.msInfo.bits.way === refill.io.msInfo.bits.way
      }.reduce(_ || _)
      refill.io.deferGrant := snoopMshrs(i).io.status.valid ||
        pendingSnoopForRefill || nestedBToNForRefill || snoopHoldsRefillWay || sinkCRefillPending(i)
      refill.io.releaseDataBeforeProbeAck := releaseDataBeforeProbeAck
      refill.io.postGrantReleaseData := postGrantReleaseData
      refill.io.releaseDataDirectWrite := nestedRefillDirectApplied(i)
      refill.io.nestedRefillWriteDone := nestedRefillApplied(i)
      val refillLineKey = Cat(refill.io.status.bits.reqTag, refill.io.status.bits.set)
      val postGrantReleaseSnoopLineMatch = snoopMshrs.map { candidate =>
        candidate.io.status.valid &&
          Cat(candidate.io.status.bits.reqTag, candidate.io.status.bits.set) === refillLineKey
      }
      val postGrantReleaseSnoopActive = postGrantReleaseSnoopLineMatch.reduce(_ || _)
      val postGrantReleaseSnoopAllocating = snoopAllocRequest &&
        io.fromMainPipe.mshr_alloc_s3.bits.task.set === refill.io.status.bits.set &&
        io.fromMainPipe.mshr_alloc_s3.bits.task.tag === refill.io.status.bits.reqTag
      refill.io.bufferWritePending := sinkCRefillPending(i) || sinkCReleaseHold(i) ||
        sinkCProbeOwnerHold(0)(i) || nestedRefillPending(i)
      refill.io.pairLifetimeHold := snoopPairs.zipWithIndex.map { case (pair, s) =>
        snoopPairNormalValid(s) && pair.bits.normal.bits.id === i.U
      }.reduce(_ || _)
      refill.io.postGrantReleaseSnoopActive := refill.io.postGrantReleaseHold &&
        (postGrantReleaseSnoopActive || postGrantReleaseSnoopAllocating ||
          (postGrantCompletion.valid && postGrantCompletion.bits.target === i.U))
      val completionForTarget = postGrantCompletion.valid && postGrantCompletion.bits.target === i.U
      val applyPostGrantCompletion = completionForTarget && refill.io.status.valid &&
        postGrantCompletion.bits.targetGeneration === mshrTaskGenerations(0)(i) &&
        postGrantCompletion.bits.targetSet === refill.io.status.bits.set &&
        postGrantCompletion.bits.targetTag === refill.io.status.bits.reqTag && completionSourceVersionMatches
      when(completionForTarget) {
        assert(applyPostGrantCompletion,
          "registered snoop completion must retain both source and paired normal versions")
      }
      refill.io.postGrantReleaseSnoopToN := applyPostGrantCompletion && postGrantCompletion.bits.outcome === 1.U
      refill.io.postGrantReleaseSnoopToB := applyPostGrantCompletion && postGrantCompletion.bits.outcome === 2.U
      refill.io.postGrantReleaseSnoopToClean := applyPostGrantCompletion && postGrantCompletion.bits.outcome === 3.U
      refill.io.blockReplRead := cReleaseReplacerFence(i) || cReleaseReplacerFenceSet(i) ||
        !replaceReserveAllowsRead(i)
      refill.io.replaceDetached := parentReplaceMatch.orR && parentReplaceInfo.detached
      refill.io.replaceRefillReady := parentReplaceMatch.orR && parentReplaceInfo.refillReady
      refill.io.replaceDone := Cat(replaceMshrs.zipWithIndex.map {
        case (r, j) =>
          r.io.done.valid && r.io.info.bits.parentAttached && r.io.done.bits.parentId === i.U &&
            r.io.done.bits.parentEpoch === refill.io.allocEpoch
      }).orR
      val commitDone = io.fromMainPipe.refillWriteDone
      val commitNamesOwner = commitDone.valid && commitDone.bits.mshrId === i.U
      val commitVersionMatches = refill.io.status.valid &&
        commitDone.bits.mshrContext === 0.U && refill.io.tasks.preparedLease.held.valid &&
        commitDone.bits.token.asUInt === refill.io.tasks.preparedLease.held.bits.asUInt &&
        commitDone.bits.token.generation === mshrTaskGenerations(0)(i)
      refill.io.refillWriteDone := commitNamesOwner && commitVersionMatches
      when(commitNamesOwner) {
        assert(commitVersionMatches, "normal commit apply must name its retained issue version")
      }
      val grantReceipt = io.fromMainPipe.grantSent
      val grantNamesOwner = grantReceipt.valid && grantReceipt.bits.mshrContext === 0.U &&
        grantReceipt.bits.mshrId === i.U
      val grantVersionMatches = refill.io.status.valid && grantReceipt.bits.mshrContext === 0.U &&
        refill.io.tasks.preparedLease.held.valid &&
        grantReceipt.bits.token.asUInt === refill.io.tasks.preparedLease.held.bits.asUInt &&
        grantReceipt.bits.token.generation === mshrTaskGenerations(0)(i)
      refill.io.grantSent := grantNamesOwner && grantVersionMatches
      when(grantNamesOwner) {
        assert(grantVersionMatches, "SourceD receipt must name its retained normal issue version")
      }
      snoop.io.replaceActive := false.B
      snoop.io.replaceAccepted := false.B
      snoop.io.replaceDetached := false.B
      snoop.io.replaceRefillReady := false.B
      snoop.io.replaceDone := false.B
      snoop.io.refillWriteDone := false.B
      snoop.io.grantSent := false.B
      snoop.io.blockReplRead := false.B
      snoop.io.releaseDataBeforeProbeAck := false.B
      snoop.io.postGrantReleaseData := false.B
      snoop.io.postGrantReleaseSnoopActive := false.B
      snoop.io.postGrantReleaseSnoopToN := false.B
      snoop.io.postGrantReleaseSnoopToB := false.B
      snoop.io.postGrantReleaseSnoopToClean := false.B
      snoop.io.releaseDataDirectWrite := false.B
      snoop.io.nestedRefillWriteDone := false.B
      snoop.io.grantInstallDone := false.B
      val pairedProbeAckDataMirrored = io.resps.sinkC.valid && io.resps.sinkC.respInfo.last &&
        io.resps.sinkC.respInfo.opcode === ProbeAckData &&
        resp_sinkC_match_vec(2 * i + 1) && refill.io.status.valid &&
        refill.io.status.bits.set === snoop.io.status.bits.set &&
        refill.io.status.bits.reqTag === snoop.io.status.bits.reqTag
      val pair = snoopPairs(i)
      val pairNormalValid = snoopPairNormalValid(i)
      val releaseStartsForSnoop = sinkCReleaseEvent.valid && snoop.io.status.valid &&
        sinkCReleaseEvent.bits.set === snoop.io.status.bits.set &&
        sinkCReleaseEvent.bits.tag === snoop.io.status.bits.reqTag
      val pairedRefillWritePending = pairNormalValid &&
        (UIntToOH(pair.bits.normal.bits.id, mshrsAll) &
          (sinkCRefillPending | nestedRefillPending)).orR
      snoop.io.pairSearchPending := !snoopPairKnown(i) || releaseStartsForSnoop || pairedRefillWritePending
      snoop.io.normalRefillPayloadFromProbeAck :=
        pairedProbeAckDataMirrored || (pairNormalValid && pair.bits.probePayload)
      snoop.io.normalRefillPostGrantReleasePending := pairNormalValid && pair.bits.postGrantPending
      snoop.io.normalRefillPayloadFromPostGrantRelease :=
        pairNormalValid && pair.bits.postGrantPending && pair.bits.payloadReady
      snoop.io.pairedNormalId.valid := pairNormalValid
      snoop.io.pairedNormalId.bits := pair.bits.normal.bits.id
      snoop.io.pairLifetimeHold := false.B
      snoop.io.bufferWritePending := false.B
      snoop.io.allocPostGrantReleasePending := postGrantReleaseSnoopTargetOH.orR
      snoop.io.allocPostGrantReleasePayload := VecInit(
        refillMshrs.zipWithIndex.map { case (candidate, j) =>
          postGrantReleaseSnoopTargetOH(j) && candidate.io.postGrantReleasePayloadReady
        }).asUInt.orR
      snoop.io.allocPairedNormalId.valid := postGrantReleaseSnoopTargetOH.orR
      snoop.io.allocPairedNormalId.bits := OHToUInt(postGrantReleaseSnoopTargetOH)
      snoop.io.allocDirectoryHit := io.fromMainPipe.snoopDirectoryHit
      when(snoop.io.alloc.valid) {
        assert(!snoop.io.status.valid, "snoop allocation must target an idle context")
      }
      snoop.io.nestedReleaseWritePending := nestedReleasePending(0)(i)
      snoop.io.nestedReleaseWriteDone := nestedReleaseDone(0)(i)
      replaceMshrs(i).io.nestedReleaseWritePending := nestedReleasePending(1)(i)
      snoop.io.releaseBufCopyDone :=
        io.fromMainPipe.refillToReleaseCopyWrite.valid &&
          io.fromMainPipe.refillToReleaseCopyWrite.bits === i.U
      replaceMshrs(i).io.id := i.U
      replaceMshrs(i).io.sinkCOwnerHold := sinkCProbeOwnerHold(2)(i)
      replaceMshrs(i).io.alloc.valid := replaceAllocValid && selectedReplaceSlotOH(i)
      replaceMshrs(i).io.alloc.bits := incomingReplaceReq
      replaceMshrs(i).io.sinkC.valid := io.resps.sinkC.valid && replaceSinkCMatch(i)
      replaceMshrs(i).io.sinkC.bits := io.resps.sinkC.respInfo
      replaceMshrs(i).io.releaseDataSeen := replaceReleaseDataMatch(i)
      val replaceProbeAckDataCapture = io.resps.sinkC.valid && io.resps.sinkC.respInfo.last &&
        io.resps.sinkC.respInfo.opcode === ProbeAckData && replaceProbeSinkCMatch(i)
      val mirroredReplaceProbeAckDataForSlot =
        mirroredReplaceProbeAckDataDoneOH(i)
      val replaceCaptureMatch = io.fromMainPipe.replaceCaptureWrite.valid &&
        io.fromMainPipe.replaceCaptureWrite.bits === i.U
      replaceMshrs(i).io.victimDataCaptured :=
        replaceProbeAckDataCapture || nestedReleaseDone(1)(i) || replaceCaptureMatch ||
        mirroredReplaceProbeAckDataForSlot
      replaceMshrs(i).io.victimDataNeedsWriteback :=
        replaceProbeAckDataCapture || nestedReleaseDone(1)(i) ||
        mirroredReplaceProbeAckDataForSlot
      replaceMshrs(i).io.localReservationHold := io.sidecarReservationHold
      replaceMshrs(i).io.snoopStarted := io.sidecarSnoopReserve.valid &&
        io.sidecarSnoopReserve.bits === i.U
      val snoopResponseMatches = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.fromB && done.bits.snpHitRelease &&
        done.bits.chiOpcode.get =/= CompData &&
        done.bits.snpHitReleaseIdx === i.U
      }
      replaceMshrs(i).io.snoopMeta.valid := snoopResponseMatches.reduce(_ || _)
      replaceMshrs(i).io.snoopMeta.bits := Mux1H(snoopResponseMatches,
        Seq(io.txdatDone.bits.meta, io.txrspDone.bits.meta))
      val contextSnoopDoneMatches = snoopRetireEvents.map { event =>
        event.valid && event.bits.targets(i) && replaceMshrs(i).io.info.valid &&
          event.bits.targetGenerations(i) === mshrTaskGenerations(2)(i) &&
          event.bits.targetSets(i) === replaceMshrs(i).io.info.bits.set &&
          event.bits.targetTags(i) === replaceMshrs(i).io.info.bits.tag
      }
      val directSnoopDoneMatches = Seq(io.txdatDone, io.txrspDone).zip(snoopResponseMatches).map {
        case (done, matches) => matches && !done.bits.mshrTask &&
          replaceMshrs(i).io.info.valid &&
          done.bits.set === replaceMshrs(i).io.info.bits.set &&
          done.bits.tag === replaceMshrs(i).io.info.bits.tag
      }
      val snoopDoneMatches = RegInit(0.U((mshrsAll + 2).W))
      snoopDoneMatches := VecInit(contextSnoopDoneMatches ++ directSnoopDoneMatches).asUInt
      val snoopDoneMatchGeneration = RegNext(mshrTaskGenerations(2)(i), 0.U(8.W))
      val snoopDoneDelta = RegNext(PopCount(snoopDoneMatches),
        0.U(log2Ceil(mshrsAll + 3).W))
      val snoopDoneDeltaGeneration = RegNext(snoopDoneMatchGeneration, 0.U(8.W))
      val snoopDoneTargetCurrent = replaceMshrs(i).io.info.valid &&
        snoopDoneDeltaGeneration === mshrTaskGenerations(2)(i)
      when(snoopDoneDelta.orR) {
        assert(snoopDoneTargetCurrent,
          "a delayed snoop delta must retain its reserved replacement allocation")
      }
      replaceMshrs(i).io.snoopDoneCount := Mux(snoopDoneTargetCurrent, snoopDoneDelta, 0.U)
      val snoopToInvalDone = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.fromB && done.bits.snpHitRelease &&
        done.bits.chiOpcode.get =/= CompData &&
        (done.bits.snpHitReleaseToInval ||
          done.bits.snpHitReleaseToClean && done.bits.resp.get(1, 0) === 0.U) &&
        done.bits.snpHitReleaseIdx === i.U && replaceMshrs(i).io.info.valid &&
        done.bits.set === replaceMshrs(i).io.info.bits.set &&
        done.bits.tag === replaceMshrs(i).io.info.bits.tag
      }.reduce(_ || _)
      val snoopToInvalApplied = RegNext(snoopToInvalDone && replaceMshrs(i).io.info.valid, false.B)
      replaceMshrs(i).io.snoopToInvalDone := snoopToInvalApplied &&
        snoopDoneMatchGeneration === mshrTaskGenerations(2)(i)
      replaceMshrs(i).io.copyBackDone := io.txdatDone.valid &&
      io.txdatDone.bits.mshrTask && io.txdatDone.bits.mshrContext === 2.U &&
      io.txdatDone.bits.mshrId === i.U && io.txdatDone.bits.chiOpcode.get === CopyBackWrData
      val replaceEpochMismatch = if (readTxnEpochWidth > 0) {
        io.resps.rxrsp.txnID(TXNID_WIDTH - bankBits - 3, mshrBits) =/= replaceMshrs(i).io.txnEpoch
      } else false.B
      replaceMshrs(i).io.rxrsp.valid := replaceMshrs(i).io.info.valid && io.resps.rxrsp.valid &&
        io.resps.rxrsp.mshrId === replaceMshrs(i).io.txnId && !replaceEpochMismatch
      replaceMshrs(i).io.rxrsp.bits := io.resps.rxrsp.respInfo

      snoop.io.resps.rxdat.valid := false.B
      snoop.io.resps.rxdat.bits := io.resps.rxdat.respInfo
      snoop.io.resps.rxrsp.valid := false.B
      snoop.io.resps.rxrsp.bits := io.resps.rxrsp.respInfo
      snoop.io.replResp.valid := false.B
      snoop.io.replResp.bits := replResponseApplied.bits
      snoop.io.tasks.txreq.ready := false.B
      snoop.io.tasks.txdat.ready := false.B
      snoop.io.pCrd.grant := false.B
      snoop.io.deferGrant := false.B
      val snoopRespDone = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.mshrTask && done.bits.mshrContext === 1.U &&
          done.bits.mshrId === i.U && done.bits.chiOpcode.get =/= CompData
      }.reduce(_ || _)
      snoop.io.snpRespDone := snoopRespDone
      snoop.io.dctDone := io.txdatDone.valid && io.txdatDone.bits.mshrTask &&
        io.txdatDone.bits.mshrContext === 1.U && io.txdatDone.bits.mshrId === i.U &&
        io.txdatDone.bits.chiOpcode.get === CompData

      val replaceInfoForOutput = WireInit(replaceMshrs(i).io.info)
      when(replaceAllocValid && selectedReplaceSlotOH(i)) {
        replaceInfoForOutput.valid := true.B
        replaceInfoForOutput.bits := incomingReplaceInfo
      }
      io.refillInfo(i) := refill.io.msInfo
      io.snoopInfo(i) := snoop.io.msInfo
      io.replaceInfo(i) := replaceInfoForOutput
      io.replaceInfoForSnoop(i) := replaceMshrs(i).io.info
      io.replaceInfoForSnoop(i).valid := replaceMshrs(i).io.info.valid &&
        replaceMshrs(i).io.ownsSnoopData
      when(replaceAllocValid && selectedReplaceSlotOH(i)) {
        assert(!replaceMshrs(i).io.info.valid,
          "new replacement must not overwrite a registered sidecar")
        assert(!incomingReplaceInfo.probeDone && !incomingReplaceInfo.releaseBufReady,
          "new replacement is not yet a nestable ReleaseBuf owner")
      }
      io.pCrd(i) <> refill.io.pCrd
      io.pCrd(mshrsAll + i) <> replaceMshrs(i).io.pCrd
  }
  io.replaceSnoopAdmissionIdle := VecInit(replaceMshrs.map(_.io.snoopAdmissionIdle)).asUInt
  io.rxdatStaleSlots := rxdatStaleSlotOH.asUInt
  io.toMainPipe.snoopDataHolds := io.snoopInfo
  io.toMainPipe.postGrantReleaseHolds := io.refillInfo
  val sharedReleaseBufOwners = VecInit(snoopMshrs.zip(replaceMshrs).map {
    case (snoop, replace) => snoop.io.status.valid && replace.io.info.valid
  }).asUInt
  assert(!sharedReleaseBufOwners.orR,
    "SnoopMSHR and ReplaceMSHR must not share a ReleaseBuf slot")

  replaceMshrs.zipWithIndex.foreach { case (replace, i) =>
    val parentReleaseMatches = VecInit(normalRetireEvents.map { event =>
      event.valid && event.bits.targets(i) && replace.io.info.valid &&
        event.bits.targetGenerations(i) === mshrTaskGenerations(2)(i) &&
        event.bits.targetSets(i) === replace.io.info.bits.set &&
        event.bits.targetTags(i) === replace.io.info.bits.tag &&
        event.bits.targetParentAttached(i) && replace.io.info.bits.parentAttached &&
        event.bits.targetParents(i) === replace.io.info.bits.parentId &&
        event.bits.targetParentEpochs(i) === replace.io.info.bits.parentEpoch &&
        event.bits.sourceId === replace.io.info.bits.parentId &&
        event.bits.sourceEpoch === replace.io.info.bits.parentEpoch
    }).asUInt
    val parentReleaseMatched = RegNext(parentReleaseMatches, 0.U(mshrsAll.W))
    val parentReleaseMatchGeneration = RegNext(mshrTaskGenerations(2)(i), 0.U(8.W))
    val parentReleaseApply = RegNext(parentReleaseMatched.orR, false.B)
    val parentReleaseApplyGeneration = RegNext(parentReleaseMatchGeneration, 0.U(8.W))
    replace.io.parentRelease := parentReleaseApply && replace.io.info.valid &&
      parentReleaseApplyGeneration === mshrTaskGenerations(2)(i)
    assert(PopCount(parentReleaseMatches) <= 1.U, "a ReplaceMSHR must match at most one retiring normal parent")
  }

  /* Reserve 1 entry for SinkB */
  val cReleaseReplacerWaiters = VecInit(refillMshrs.map { refill =>
    refill.io.status.valid && refill.io.replReadInFlight &&
    refill.io.msInfo.bits.set === io.fromReqArb.status_s1.c_set &&
    refill.io.msInfo.bits.metaTag === io.fromReqArb.status_s1.c_tag
  }).asUInt
  val cReleaseWaitersLatched = RegInit(0.U(mshrsAll.W))
  val cReleaseWaitersTracked = RegInit(false.B)
  when(!io.fromReqArb.status_s1.cReleaseData) {
    cReleaseWaitersLatched := 0.U
    cReleaseWaitersTracked := false.B
  }.elsewhen(!cReleaseWaitersTracked) {
    cReleaseWaitersLatched := cReleaseReplacerWaiters
    cReleaseWaitersTracked := true.B
  }.otherwise {
    cReleaseWaitersLatched := cReleaseWaitersLatched & cReleaseReplacerWaiters
  }
  val cReleaseConflictsReplacer = Mux(
    cReleaseWaitersTracked,
    (cReleaseWaitersLatched & cReleaseReplacerWaiters).orR,
    cReleaseReplacerWaiters.orR
  )
  val aSidecarConflict = VecInit(replaceMshrs.map { replace =>
    replace.io.info.valid &&
    io.fromReqArb.status_s1.aCandidateValid &&
    replace.io.info.bits.set === io.fromReqArb.status_s1.aCandidateSet &&
    replace.io.info.bits.tag === io.fromReqArb.status_s1.aCandidateTag
  }).asUInt.orR
  val aSnoopConflict = VecInit(snoopMshrs.map { snoop =>
    snoop.io.msInfo.valid && snoop.io.msInfo.bits.blocksSnoop &&
      io.fromReqArb.status_s1.aCandidateValid &&
      snoop.io.msInfo.bits.set === io.fromReqArb.status_s1.aCandidateSet &&
      snoop.io.msInfo.bits.reqTag === io.fromReqArb.status_s1.aCandidateTag
  }).asUInt.orR
  val aPendingToNConflict = VecInit((0 until mshrsAll).map { i =>
    pendingToNValid(i) && io.fromReqArb.status_s1.aCandidateValid &&
      pendingToNSet(i) === io.fromReqArb.status_s1.aCandidateSet &&
      pendingToNTag(i) === io.fromReqArb.status_s1.aCandidateTag
  }).asUInt.orR
  val aEnteringToNConflict = io.fromReqArb.status_s1.bValid && io.fromReqArb.status_s1.bToN &&
    io.fromReqArb.status_s1.aCandidateValid &&
    io.fromReqArb.status_s1.b_set === io.fromReqArb.status_s1.aCandidateSet &&
    io.fromReqArb.status_s1.b_tag === io.fromReqArb.status_s1.aCandidateTag
  io.toReqArb.blockC_s1 := io.fromReqArb.status_s1.cReleaseData && cReleaseConflictsReplacer
  io.toReqArb.blockB_s1 := mshrFull ||
    (io.fromReqArb.status_s1.bToN && pendingToNFull) ||
    (io.fromReqArb.status_s1.bCandidateValid &&
      (!snoopCandidateSeen || snoopPendingCapacityBlock))
  io.toReqArb.blockA_s1 := aSidecarConflict || aSnoopConflict ||
    aPendingToNConflict || aEnteringToNConflict
  io.toReqArb.blockG_s1 := false.B

  val replaceTxreqSkids = replaceMshrs.zipWithIndex.map { case (m, i) =>
    val busy = RegInit(false.B)
    val bits = Reg(new CHIREQ)
    val out = Wire(Decoupled(new CHIREQ))
    m.io.tasks.txreq.ready := !busy && (!io.sidecarReservationHold || !m.io.ownsSnoopData)
    m.io.txreqPending := busy
    out.valid := busy
    out.bits := bits
    when (m.io.tasks.txreq.fire) {
      busy := true.B
      bits := m.io.tasks.txreq.bits
    }.elsewhen(out.fire) {
      busy := false.B
    }
    when (m.io.tasks.txreq.fire) {
      assert(!busy, s"replace TXREQ skid $i captured into a non-empty entry")
    }
    assert(!busy || m.io.info.valid,
      s"replace TXREQ skid $i holds a flit without a live sidecar")
    when (m.io.alloc.valid) {
      assert(!busy, s"replace TXREQ skid $i not drained before reallocation")
    }
    out
  }
  val replaceTxreqSkidAnyBusy = VecInit(replaceTxreqSkids.map(_.valid)).asUInt.orR
  io.replaceTxreqPendingCnt := PopCount(VecInit(replaceTxreqSkids.map(_.valid)))
  val replaceTxreqSkidWait = RegInit(0.U(16.W))
  when (replaceTxreqSkidAnyBusy) {
    replaceTxreqSkidWait := Mux(replaceTxreqSkidWait === 65535.U, replaceTxreqSkidWait,
      replaceTxreqSkidWait + 1.U)
  }.otherwise {
    replaceTxreqSkidWait := 0.U
  }
  assert(replaceTxreqSkidWait =/= 65535.U, "replace TXREQ skid failed to drain")
  ArbPerf(twoLevelArb(refillMshrs.map(_.io.tasks.txreq) ++ replaceTxreqSkids, io.toTXREQ, Some("txreq")), "txreq_arb")

  // Register the selected copyback before reading ReleaseBuf; withdraw only before TXDAT accepts it.
  val directCbValids = VecInit(replaceMshrs.map(_.io.tasks.txdat.valid))
  val cbPresent = RegInit(false.B)
  val cbPickOHReg = Reg(UInt(mshrsAll.W))
  val cbPickValidReg = RegInit(false.B)
  val cbSelOH = Reg(UInt(mshrsAll.W))
  val cbTaskReg = Reg(new TaskBundle)
  val cbRrGrantMask = RegInit(0.U(mshrsAll.W))
  val cbRrSelOH = MaskToOH(cbRrGrantMask & directCbValids.asUInt)
  val cbPickOH = Mux((cbRrSelOH & directCbValids.asUInt).orR, cbRrSelOH, MaskToOH(directCbValids.asUInt))
  val cbPick = !cbPresent && !cbPickValidReg && directCbValids.asUInt.orR && io.txdatDirectSpace
  when (cbPick) {
    cbPickOHReg := cbPickOH
    cbPickValidReg := true.B
  }
  val cbPickStillValid = Mux1H(cbPickOHReg, directCbValids)
  val cbRead = cbPickValidReg && cbPickStillValid
  io.releaseBufDirectRd.valid := cbRead
  io.releaseBufDirectRd.bits.id := OHToUInt(cbPickOHReg)
  when (cbRead) {
    cbPickValidReg := false.B
    cbPresent := true.B
    cbSelOH := cbPickOHReg
    cbTaskReg := Mux1H(cbPickOHReg, replaceMshrs.map(_.io.tasks.txdat.bits))
  }.elsewhen (cbPickValidReg && !cbPickStillValid) {
    cbPickValidReg := false.B
  }
  val cbSelStillValid = Mux1H(cbSelOH, directCbValids)
  io.toTXDAT.valid := cbPresent && cbSelStillValid && !io.sidecarReservationHold
  io.toTXDAT.bits.task := cbTaskReg
  io.toTXDAT.bits.data := io.releaseBufDirectResp
  when (cbPresent && !cbSelStillValid) {
    cbPresent := false.B
  }
  when (io.toTXDAT.fire) {
    cbPresent := false.B
    cbRrGrantMask := LowerMaskPrefix(cbSelOH)
  }
  replaceMshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.tasks.txdat.ready := cbPresent && cbSelOH(i) && cbSelStillValid &&
      !io.sidecarReservationHold && io.toTXDAT.ready
  }
  XSPerfAccumulate("cmo_sidecar_alloc_cnt",
    replaceNeedsAlloc && cmoAlloc && replaceAllocValid)
  XSPerfAccumulate("txdat_direct_copyback_cnt", io.toTXDAT.fire)
  XSPerfAccumulate("txdat_direct_abort_cnt", cbPresent && !cbSelStillValid)
  XSPerfAccumulate("txdat_direct_wait_sum",
    PopCount(Cat(replaceMshrs.map(m => m.io.tasks.txdat.valid && !m.io.tasks.txdat.ready))))

  /* Response downwards to TXRSP*/
  val txrspArb = twoLevelArb(mshrs.map(_.io.tasks.txrsp) ++ replaceMshrs.map(_.io.tasks.txrsp), io.toTXRSP, Some("txrsp"))
  ArbPerf(txrspArb, "txrsp_arb")
  XSPerfAccumulate(
    "normal_txrsp_arb_wait",
    PopCount(Cat(refillMshrs.map { m =>
      m.io.tasks.txrsp.valid && !m.io.tasks.txrsp.ready && txrspArb.io.out.ready
    }))
  )
  XSPerfAccumulate(
    "snoop_txrsp_arb_wait",
    PopCount(Cat(snoopMshrs.map { m =>
      m.io.tasks.txrsp.valid && !m.io.tasks.txrsp.ready && txrspArb.io.out.ready
    }))
  )

  /* Probe upwards */
  val replaceSourceBTasks = replaceMshrs.zipWithIndex.map {
    case (m, _) =>
      val task = Wire(Decoupled(new SourceBReq))
      task.valid := m.io.tasks.source_b.valid
      task.bits := m.io.tasks.source_b.bits
      m.io.tasks.source_b.ready := task.ready
      task
  }
  val sourceBTasks = mshrs.zip(contextPhysicalIds).map {
    case (m, _) =>
      val task = Wire(Decoupled(new SourceBReq))
      task.valid := m.io.tasks.source_b.valid
      task.bits := m.io.tasks.source_b.bits
      m.io.tasks.source_b.ready := task.ready
      task
  }

  class SourceBCandidate extends Bundle {
    val request = new SourceBReq
    val source = UInt(log2Ceil(3 * mshrsAll).W)
    val id = UInt(mshrBits.W)
    val generation = UInt(8.W)
  }
  val sourceBInputs = sourceBTasks ++ replaceSourceBTasks
  val sourceBCommit = RegInit(0.U.asTypeOf(Valid(new SourceBCandidate)))
  val sourceBCommitChecked = RegInit(false.B)
  val sourceBCommitConflicts = RegInit(VecInit(Seq.fill(3 * mshrsAll)(false.B)))
  val sourceBPermit = RegInit(0.U.asTypeOf(Valid(new SourceBCandidate)))
  val sourceBCandidateOut = Wire(Decoupled(new SourceBCandidate))
  val sourceBHoldPorts = mshrs.map(_.io.sourceBHold) ++ replaceMshrs.map(_.io.sourceBHold)
  val sourceBAcceptedPorts = mshrs.map(_.io.sourceBAccepted) ++ replaceMshrs.map(_.io.sourceBAccepted)
  val sourceBAccepted = RegInit(VecInit(Seq.fill(sourceBInputs.size)(false.B)))
  sourceBAcceptedPorts.zip(sourceBAccepted).foreach { case (port, accepted) => port := accepted }
  val sourceBTaskCandidates = sourceBInputs.zipWithIndex.map { case (task, i) =>
    val candidate = RegInit(0.U.asTypeOf(Valid(new SourceBCandidate)))
    val selected = sourceBCommit.valid && sourceBCommit.bits.source === i.U
    val permitted = sourceBPermit.valid && sourceBPermit.bits.source === i.U
    sourceBHoldPorts(i) := candidate.valid || selected || permitted || sourceBAccepted(i) ||
      sourceBProbeClaims(i).valid
    when(!candidate.valid && !selected && !permitted && !sourceBAccepted(i) &&
        !sourceBProbeClaims(i).valid && task.valid) {
      candidate.valid := true.B
      candidate.bits.request := task.bits
      candidate.bits.source := i.U
      candidate.bits.id := (if(i < 2 * mshrsAll) i / 2 else i - 2 * mshrsAll).U
      candidate.bits.generation := sourceBGenerations(i)
    }
    val out = Wire(Decoupled(new SourceBCandidate))
    out.valid := candidate.valid
    out.bits := candidate.bits
    when(out.fire) { candidate.valid := false.B }
    task.ready := false.B
    out
  }
  ArbPerf(twoLevelArb(sourceBTaskCandidates, sourceBCandidateOut, Some("source_b_candidate")),
    "source_b_candidate_arb")
  sourceBCandidateOut.ready := !sourceBCommit.valid
  val commitOH = UIntToOH(sourceBCommit.bits.source, sourceBInputs.size)
  val commitCurrent = VecInit(sourceBInputs.zipWithIndex.map { case (task, i) =>
    commitOH(i) && task.valid && sourceBGenerations(i) === sourceBCommit.bits.generation &&
      task.bits.asUInt === sourceBCommit.bits.request.asUInt
  }).asUInt.orR
  sourceBProbeClaims.zipWithIndex.foreach { case (claim, i) =>
    sourceBCommitConflicts(i) := claim.valid &&
      claim.set === sourceBCommit.bits.request.set && claim.tag === sourceBCommit.bits.request.tag
  }
  // Hold one immutable SourceB commit while registered per-source conflicts settle before authorization.
  val commitLineBusy = sourceBCommitConflicts.asUInt.orR
  val sourceB = Module(new SourceB())
  val authorizeSourceB = sourceBCommit.valid && sourceBCommitChecked &&
    commitCurrent && !commitLineBusy && !sourceBPermit.valid
  sourceB.io.task.valid := sourceBPermit.valid
  sourceB.io.task.bits := sourceBPermit.bits.request
  sourceBInputs.zipWithIndex.foreach { case (task, i) =>
    task.ready := false.B
    sourceBAccepted(i) := authorizeSourceB && commitOH(i)
    when(authorizeSourceB && commitOH(i)) {
      assert(!sourceBProbeClaims(i).valid, "a source may own only one accepted Probe")
      sourceBProbeClaims(i).valid := true.B
      sourceBProbeClaims(i).set := sourceBCommit.bits.request.set
      sourceBProbeClaims(i).tag := sourceBCommit.bits.request.tag
      sourceBProbeClaims(i).generation := sourceBCommit.bits.generation
    }
  }
  when(authorizeSourceB) {
    sourceBPermit.valid := true.B
    sourceBPermit.bits := sourceBCommit.bits
  }.elsewhen(sourceB.io.task.fire) {
    sourceBPermit.valid := false.B
  }
  when(sourceBPermit.valid) {
    val permitOH = UIntToOH(sourceBPermit.bits.source, sourceBInputs.size)
    assert(Mux1H(permitOH, sourceBGenerations) === sourceBPermit.bits.generation,
      "accepted SourceB packet must pin its source generation until queue transfer")
  }
  when(sourceBCandidateOut.fire) {
    sourceBCommit.valid := true.B
    sourceBCommit.bits := sourceBCandidateOut.bits
    sourceBCommitChecked := false.B
  }.elsewhen(sourceBCommit.valid && (!commitCurrent || authorizeSourceB)) {
    sourceBCommit.valid := false.B
    sourceBCommitChecked := false.B
  }.elsewhen(sourceBCommit.valid) {
    sourceBCommitChecked := true.B
  }.otherwise {
    sourceBCommitChecked := false.B
  }
  when(authorizeSourceB) {
    assert(PopCount(commitOH) === 1.U,
      "only a unique SourceB authorization can consume a source")
  }
  sourceB.io.grantStatus := io.grantStatus
  io.toSourceB.valid := sourceB.io.sourceB.valid
  io.toSourceB.bits := sourceB.io.sourceB.bits
  sourceB.io.sourceB.ready := io.toSourceB.ready

  refillMshrs.zip(snoopMshrs).zipWithIndex.foreach {
    case ((refill, snoop), i) =>
      val normalSinkCMatch = resp_sinkC_match_vec(2 * i)
      val snoopSinkCMatch = resp_sinkC_match_vec(2 * i + 1)
      val probeAckData = io.resps.sinkC.valid && io.resps.sinkC.respInfo.opcode === ProbeAckData
      when(probeAckData && normalSinkCMatch) {
        assert(refill.io.status.valid, "ProbeAckData arrived without a live normal context")
      }
      when(probeAckData && snoopSinkCMatch) {
        assert(snoop.io.status.valid, "ProbeAckData arrived without a live snoop context")
      }
      when(probeAckData && replaceProbeSinkCMatch(i)) {
        assert(replaceMshrs(i).io.info.valid, "ProbeAckData arrived without a live replace context")
      }
  }

  val mshrTask = Wire(Decoupled(new TaskBundle()))
  val mshrTaskGroups: Seq[Seq[DecoupledIO[TaskBundle]]] = Seq(
    refillMshrs.map(_.io.tasks.mainpipe),
    snoopMshrs.map(_.io.tasks.mainpipe),
    replaceMshrs.map(_.io.tasks.mainpipe)
  )
  require(mshrTaskGroups.map(_.size).distinct.size == 1,
    "all MSHR context types must have the same number of slots")
  val nMshrTaskGroups = mshrTaskGroups.size
  val mshrTaskGroupSize = mshrTaskGroups.head.size
  val mshrTaskSources = mshrTaskGroups.flatten
  val nMshrTaskSrc = mshrTaskSources.size
  val mshrTaskLeasePorts = Seq(
    refillMshrs.map(_.io.tasks.preparedLease),
    snoopMshrs.map(_.io.tasks.preparedLease),
    replaceMshrs.map(_.io.tasks.preparedLease)
  )
  val mshrTaskCandidates = mshrTaskLeasePorts.flatten.map(_.candidate)
  val mshrTaskClasses = mshrTaskCandidates.map(_.bits.qosClass)
  val mshrTaskClassReg = mshrTaskCandidates.zip(mshrTaskClasses).map { case (t, c) =>
    val r = RegInit(0.U(3.W))
    when(t.valid) { r := c }
    r
  }
  val mshrTaskWaitCnt = mshrTaskSources.zip(mshrTaskCandidates).map { case (t, candidate) =>
    val cnt = RegInit(0.U(6.W))
    when(t.fire) {
      cnt := 0.U
    }.elsewhen(candidate.valid && !t.ready) {
      cnt := Mux(cnt === 63.U, 63.U, cnt + 1.U)
    }
    cnt
  }
  val mshrTaskEffClasses = mshrTaskClassReg.zip(mshrTaskWaitCnt).map { case (c, w) =>
    Mux(w === 63.U, 0.U(3.W), c)
  }
  val mshrTaskSrcValids = VecInit(mshrTaskCandidates.map(_.valid)).asUInt

  val mshrTaskGroupOffsets = mshrTaskGroups.scanLeft(0)(_ + _.size).take(nMshrTaskGroups)
  def groupOf(g: Int) = mshrTaskGroups(g)
  def groupValids(g: Int): UInt = {
    val base = mshrTaskGroupOffsets(g)
    VecInit((0 until mshrTaskGroupSize).map(i => mshrTaskSrcValids(base + i))).asUInt
  }
  def groupClasses(g: Int): Vec[UInt] = {
    val base = mshrTaskGroupOffsets(g)
    VecInit((0 until mshrTaskGroupSize).map(i => mshrTaskEffClasses(base + i)))
  }
  def groupBestClass(g: Int, v: UInt): UInt = {
    val cs = groupClasses(g)
    PriorityEncoder(VecInit((0 until 5).map(c =>
      (v & VecInit(cs.map(_ === c.U)).asUInt).orR)).asUInt)
  }
  def groupCandidates(g: Int, v: UInt): UInt = {
    val cs = groupClasses(g)
    val bc = groupBestClass(g, v)
    v & VecInit(cs.map(_ === bc)).asUInt
  }
  // Prepare leases source lifetime; issue or cancel returns that exact version after all local references end.
  class MSHRTaskWinner extends Bundle {
    val group = UInt(log2Up(nMshrTaskGroups).W)
    val source = UInt(log2Up(mshrTaskGroupSize).W)
    val operation = UInt(4.W)
    val qosClass = UInt(3.W)
    val token = new MainpipeLeaseToken
    val completion = Valid(new PairedCompletion)
  }
  class MSHRPreparedTask extends MSHRTaskWinner {
    val task = new TaskBundle()
  }
  class MSHRTaskTicket extends Bundle {
    val group = UInt(log2Up(nMshrTaskGroups).W)
    val source = UInt(log2Up(mshrTaskGroupSize).W)
    val qosClass = UInt(3.W)
  }
  val mshrTaskPreparedValid = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskPrepared = Seq.fill(nMshrTaskGroups)(
    RegInit(0.U.asTypeOf(new MSHRPreparedTask)))
  val mshrTaskLeaseValid = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskWinners = Seq.fill(nMshrTaskGroups)(RegInit(0.U.asTypeOf(new MSHRTaskWinner)))
  val mshrTaskLeaseReturning = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskLeaseReturnSent = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskLeaseAwaitPipe = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskLeaseSequence = Seq.fill(nMshrTaskGroups)(RegInit(0.U(8.W)))
  val mshrTaskGroupWait = Seq.fill(nMshrTaskGroups)(RegInit(0.U(6.W)))
  val mshrTaskGroupClassEff = (0 until nMshrTaskGroups).map { g =>
    Mux(mshrTaskGroupWait(g) === 63.U, 0.U(3.W), mshrTaskPrepared(g).qosClass)
  }
  val mshrTaskGroupCancel = (0 until nMshrTaskGroups).map { g =>
    val sourceOH = UIntToOH(mshrTaskWinners(g).source, mshrTaskGroupSize)
    Mux1H(sourceOH, mshrTaskLeasePorts(g).map(_.cancel))
  }
  val mshrTaskWinnerStale = (0 until nMshrTaskGroups).map { g =>
    val winner = mshrTaskWinners(g)
    val sourceReallocated =
      VecInit(mshrTaskGenerations(g))(winner.source) =/= winner.token.generation
    val pairedOwnerReallocated = if (g == 1) {
      winner.completion.valid &&
        winner.completion.bits.targetGeneration =/=
          snoopPairNormalGenerationSeen(winner.completion.bits.target)
    } else false.B
    sourceReallocated || pairedOwnerReallocated
  }
  val mshrTaskGroupReady = (0 until nMshrTaskGroups).map { g =>
    mshrTaskPreparedValid(g) && mshrTaskLeaseValid(g) && !mshrTaskLeaseReturning(g)
  }

  val mshrTaskTicketValid = RegInit(false.B)
  val mshrTaskTicket = RegInit(0.U.asTypeOf(new MSHRTaskTicket))
  val mshrTaskIssuePermitValid = RegInit(false.B)
  val mshrTaskIssuePermit = RegInit(0.U.asTypeOf(new MSHRPreparedTask))
  val mshrTaskTicketOut = Wire(Decoupled(new MSHRTaskTicket))
  val mshrTaskFinalBestClass = PriorityEncoder(VecInit((0 until 5).map { c =>
    VecInit((0 until nMshrTaskGroups).map { g =>
      mshrTaskGroupReady(g) && mshrTaskGroupClassEff(g) === c.U
    }).asUInt.orR
  }).asUInt)
  val mshrTaskTicketInputs = (0 until nMshrTaskGroups).map { g =>
    val ticket = Wire(Decoupled(new MSHRTaskTicket))
    ticket.valid := mshrTaskGroupReady(g) &&
      !mshrTaskGroupCancel(g).valid &&
      mshrTaskGroupClassEff(g) === mshrTaskFinalBestClass
    ticket.bits.group := g.U
    ticket.bits.source := mshrTaskPrepared(g).source
    ticket.bits.qosClass := mshrTaskGroupClassEff(g)
    ticket
  }
  ArbPerf(twoLevelArb(mshrTaskTicketInputs, mshrTaskTicketOut, Some("mshr_task_ticket")),
    "mshr_task_ticket_arb")
  mshrTaskTicketOut.ready := !mshrTaskTicketValid && !mshrTaskIssuePermitValid
  val mshrTaskTicketGroupOH = UIntToOH(mshrTaskTicket.group, nMshrTaskGroups)
  val mshrTaskTicketReady = Mux1H(mshrTaskTicketGroupOH, (0 until nMshrTaskGroups).map { g =>
    mshrTaskGroupReady(g) && mshrTaskPrepared(g).source === mshrTaskTicket.source &&
      !mshrTaskGroupCancel(g).valid
  })
  val mshrTaskIssuePermitGroupOH = UIntToOH(mshrTaskIssuePermit.group, nMshrTaskGroups)
  val mshrTaskIssuePermitReady = mshrTaskIssuePermitValid && Mux1H(
    mshrTaskIssuePermitGroupOH,
    (0 until nMshrTaskGroups).map { g =>
      mshrTaskGroupReady(g) &&
        mshrTaskPrepared(g).source === mshrTaskIssuePermit.source &&
        mshrTaskPrepared(g).token.asUInt === mshrTaskIssuePermit.token.asUInt &&
        !mshrTaskGroupCancel(g).valid
    }
  )
  val mshrTaskIssuePermitTake = mshrTaskTicketValid && mshrTaskTicketReady &&
    !mshrTaskIssuePermitValid
  when(mshrTaskIssuePermitTake) {
    mshrTaskIssuePermitValid := true.B
    mshrTaskIssuePermit := Mux1H(mshrTaskTicketGroupOH, mshrTaskPrepared)
    mshrTaskTicketValid := false.B
  }.elsewhen(mshrTaskIssuePermitValid && (!mshrTaskIssuePermitReady || mshrTask.fire)) {
    mshrTaskIssuePermitValid := false.B
  }
  mshrTask.valid := mshrTaskIssuePermitReady
  mshrTask.bits := mshrTaskIssuePermit.task
  mshrTask.bits.mshrId := mshrTaskIssuePermit.source
  mshrTask.bits.qosClass := mshrTaskIssuePermit.qosClass
  io.mshrTaskLeaseToken := mshrTaskIssuePermit.token
  selectedIssueCompletion.valid := mshrTask.fire && mshrTaskIssuePermit.completion.valid
  selectedIssueCompletion.bits := mshrTaskIssuePermit.completion.bits
  when(mshrTaskTicketOut.fire) {
    mshrTaskTicketValid := true.B
    mshrTaskTicket := mshrTaskTicketOut.bits
    assert(mshrTaskTicketOut.bits.qosClass === mshrTaskFinalBestClass,
      "selected ticket must belong to the best effective QoS class")
  }.elsewhen(mshrTaskTicketValid && !mshrTaskTicketReady && !mshrTaskIssuePermitTake) {
    mshrTaskTicketValid := false.B
  }.elsewhen(mshrTaskTicketValid &&
    Mux1H(mshrTaskTicketGroupOH, mshrTaskGroupWait.map(_ === 63.U))) {
    mshrTaskTicket.qosClass := 0.U
  }

  (0 until nMshrTaskGroups).foreach { g =>
    val winnerOut = Wire(Decoupled(new MSHRTaskWinner))
    val candidates = groupCandidates(g, groupValids(g))
    val winnerInputs = mshrTaskLeasePorts(g).zipWithIndex.map { case (lease, i) =>
      val in = Wire(Decoupled(new MSHRTaskWinner))
      in.valid := candidates(i)
      in.bits.group := g.U
      in.bits.source := i.U
      in.bits.operation := lease.candidate.bits.operation
      in.bits.qosClass := groupClasses(g)(i)
      in.bits.token.generation := mshrTaskGenerations(g)(i)
      in.bits.token.sequence := mshrTaskLeaseSequence(g)
      in.bits.completion := 0.U.asTypeOf(in.bits.completion)
      if (g == 1) {
        val pair = snoopPairs(i)
        in.bits.completion.valid := lease.candidate.bits.operation(2) &&
          lease.candidate.bits.operation(1, 0) === 1.U
        in.bits.completion.bits.target := pair.bits.normal.bits.id
        in.bits.completion.bits.targetGeneration := pair.bits.normal.bits.generation
        in.bits.completion.bits.targetSet := pair.bits.normal.bits.set
        in.bits.completion.bits.targetTag := pair.bits.normal.bits.tag
        in.bits.completion.bits.source := i.U
        in.bits.completion.bits.sourceToken := in.bits.token
        when(in.fire && in.bits.completion.valid) {
          assert(snoopPairNormalValid(i) && pair.bits.postGrantPending && pair.bits.payloadReady,
            "paired winner must bind the live post-Grant normal owner")
        }
      }
      in
    }
    ArbPerf(twoLevelArb(winnerInputs, winnerOut, Some(s"mshr_task_group_$g")),
      s"mshr_task_group_${g}_arb")
    winnerOut.ready := !mshrTaskLeaseValid(g)
    val winner = mshrTaskWinners(g)
    val sourceOH = UIntToOH(winner.source, mshrTaskGroupSize)
    val payload = Mux1H(sourceOH, groupOf(g).map(_.bits))
    val payloadValid = Mux1H(sourceOH, groupOf(g).map(_.valid))
    val payloadOperation = Mux1H(sourceOH, mshrTaskLeasePorts(g).map(_.candidate.bits.operation))
    val preparePayload = mshrTaskLeaseValid(g) && !mshrTaskLeaseReturning(g) &&
      !mshrTaskPreparedValid(g) && payloadValid && payloadOperation === winner.operation
    val granted = mshrTask.fire && mshrTaskIssuePermitGroupOH(g)
    val returnRequest = mshrTaskLeaseValid(g) && mshrTaskLeaseReturning(g) &&
      !mshrTaskLeaseAwaitPipe(g) && !mshrTaskLeaseReturnSent(g)
    mshrTaskLeasePorts(g).zipWithIndex.foreach { case (lease, i) =>
      lease.prepare.valid := winnerOut.fire && winnerOut.bits.source === i.U
      lease.prepare.bits := winnerOut.bits.token
      lease.held.valid := mshrTaskLeaseValid(g) && sourceOH(i)
      lease.held.bits := winner.token
      lease.release.valid := returnRequest && sourceOH(i)
      lease.release.bits := winner.token
    }
    when(preparePayload) {
      mshrTaskPreparedValid(g) := true.B
      mshrTaskPrepared(g).task := payload
      mshrTaskPrepared(g).group := winner.group
      mshrTaskPrepared(g).source := winner.source
      mshrTaskPrepared(g).operation := winner.operation
      mshrTaskPrepared(g).qosClass := winner.qosClass
      mshrTaskPrepared(g).token := winner.token
      mshrTaskPrepared(g).completion := winner.completion
      if (g == 1) {
        val outcome = Mux(payload.meta.state === INVALID, 1.U,
          Mux(payload.meta.state === BRANCH, 2.U, Mux(!payload.meta.dirty, 3.U, 0.U)))
        mshrTaskPrepared(g).completion.valid := winner.completion.valid && outcome.orR
        mshrTaskPrepared(g).completion.bits.outcome := outcome
        when(winner.completion.valid) {
          assert(payload.set === winner.completion.bits.targetSet &&
            payload.tag === winner.completion.bits.targetTag &&
            winner.completion.bits.targetGeneration ===
              snoopPairNormalGenerationSeen(winner.completion.bits.target),
            "paired payload must retain the winner's normal owner and line")
        }
      }
      assert(payload.mshrId === winner.source && payload.mshrContext === g.U,
        "materialized payload must belong to the registered group winner")
    }
    val returnAck = Mux1H(sourceOH, mshrTaskLeasePorts(g).map(_.released))
    when(winnerOut.fire) {
      mshrTaskLeaseValid(g) := true.B
      mshrTaskWinners(g) := winnerOut.bits
      mshrTaskPreparedValid(g) := false.B
      mshrTaskLeaseReturning(g) := false.B
      mshrTaskLeaseReturnSent(g) := false.B
      mshrTaskLeaseAwaitPipe(g) := false.B
      mshrTaskLeaseSequence(g) := mshrTaskLeaseSequence(g) + 1.U
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(returnAck.valid) {
      assert(mshrTaskLeaseValid(g) && mshrTaskLeaseReturning(g) &&
        mshrTaskLeaseReturnSent(g) && !mshrTaskLeaseAwaitPipe(g) &&
        returnAck.bits.asUInt === winner.token.asUInt,
        "winner lease acknowledgment must match its outstanding return")
      assert(!mshrTaskTicketValid || !mshrTaskTicketGroupOH(g),
        "ticket must drain before the source lifetime lease is released")
      mshrTaskLeaseValid(g) := false.B
      mshrTaskPreparedValid(g) := false.B
      mshrTaskLeaseReturning(g) := false.B
      mshrTaskLeaseReturnSent(g) := false.B
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(mshrTaskLeaseValid(g) && !mshrTaskLeaseReturning(g) &&
      (granted || mshrTaskGroupCancel(g).valid || mshrTaskWinnerStale(g))) {
      mshrTaskLeaseReturning(g) := true.B
      mshrTaskLeaseAwaitPipe(g) := granted
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(mshrTaskLeaseReturning(g)) {
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(mshrTaskLeaseValid(g)) {
      mshrTaskGroupWait(g) := Mux(mshrTaskGroupWait(g) === 63.U, 63.U, mshrTaskGroupWait(g) + 1.U)
    }
    when(mshrTaskGroupCancel(g).valid) {
      assert(mshrTaskLeaseValid(g) && !mshrTaskLeaseAwaitPipe(g) &&
        mshrTaskGroupCancel(g).bits.asUInt === winner.token.asUInt,
        "source cancellation must name its unissued winning version")
      assert(!granted, "canceled winning operation must never issue")
    }
    when(returnRequest) { mshrTaskLeaseReturnSent(g) := true.B }
    val localDone = io.fromMainPipe.mainpipeLeaseDone
    when(localDone.valid && localDone.bits.mshrContext === g.U) {
      assert(mshrTaskLeaseValid(g) && mshrTaskPreparedValid(g) && mshrTaskLeaseReturning(g) &&
        mshrTaskLeaseAwaitPipe(g) && localDone.bits.mshrId === winner.source &&
        localDone.bits.token.asUInt === winner.token.asUInt,
        "MainPipe terminal event must match the issued source lease version")
      mshrTaskLeaseAwaitPipe(g) := false.B
    }
    groupOf(g).zipWithIndex.foreach { case (s, i) =>
      s.ready := granted && mshrTaskIssuePermit.source === i.U
    }
    assert(!winnerOut.fire || !mshrTaskLeaseValid(g),
      "a winning lease must drain or cancel before the group can reload")
    when(mshrTaskPreparedValid(g)) {
      assert(mshrTaskLeaseValid(g) && mshrTaskPrepared(g).source === winner.source &&
        mshrTaskPrepared(g).token.asUInt === winner.token.asUInt,
        "prepared payload must retain its winner and token")
    }
  }
  val mshrTaskGrantSrc = VecInit((0 until nMshrTaskGroups).map { g =>
    Mux(mshrTask.fire && mshrTaskIssuePermitGroupOH(g),
      UIntToOH(mshrTaskIssuePermit.source, mshrTaskGroupSize), 0.U)
  }).asUInt
  val mshrTaskGrantSrcLast = RegNext(mshrTaskGrantSrc, 0.U(nMshrTaskSrc.W))
  assert(!(mshrTaskGrantSrcLast & mshrTaskGrantSrc).orR,
    "an MSHR task source must not be granted twice without fresh preparation")
  when(mshrTask.fire) {
    assert(PopCount(VecInit(mshrTaskSources.map(_.fire))) === 1.U,
      "issuing an MSHR task must consume exactly its owning source")
  }
  ArbPerf(mshrTaskSources.map(_.valid), mshrTaskSources.map(_.ready), mshrTask.ready, "mshr_task_arb")
  XSPerfAccumulate(
    "normal_mainpipe_arb_wait",
    PopCount(Cat(refillMshrs.map { m =>
      m.io.tasks.mainpipe.valid && !m.io.tasks.mainpipe.ready && mshrTask.ready
    }))
  )
  XSPerfAccumulate(
    "snoop_mainpipe_arb_wait",
    PopCount(Cat(snoopMshrs.map { m =>
      m.io.tasks.mainpipe.valid && !m.io.tasks.mainpipe.ready && mshrTask.ready
    }))
  )
  XSPerfAccumulate("qos_snoopresp_wait_sum",
    PopCount(Cat((0 until nMshrTaskSrc).map(i =>
      mshrTaskSrcValids(i) && mshrTaskClasses(i) === 0.U && !mshrTaskSources(i).ready))))
  XSPerfAccumulate("qos_grant_wait_sum",
    PopCount(Cat((0 until nMshrTaskSrc).map(i =>
      mshrTaskSrcValids(i) && mshrTaskClasses(i) === 1.U && !mshrTaskSources(i).ready))))
  XSPerfAccumulate("qos_commit_wait_sum",
    PopCount(Cat((0 until nMshrTaskSrc).map(i =>
      mshrTaskSrcValids(i) && mshrTaskClasses(i) === 2.U && !mshrTaskSources(i).ready))))
  XSPerfAccumulate("qos_capture_wait_sum",
    PopCount(Cat((0 until nMshrTaskSrc).map(i =>
      mshrTaskSrcValids(i) && mshrTaskClasses(i) === 3.U && !mshrTaskSources(i).ready))))
  XSPerfAccumulate("qos_background_wait_sum",
    PopCount(Cat((0 until nMshrTaskSrc).map(i =>
      mshrTaskSrcValids(i) && mshrTaskClasses(i) === 4.U && !mshrTaskSources(i).ready))))
  XSPerfAccumulate("qos_starve_promote_cnt",
    PopCount(Cat((0 until nMshrTaskSrc).map(i =>
      mshrTaskSrcValids(i) && mshrTaskWaitCnt(i) === 63.U))))
  io.mshrTask <> mshrTask

  val sinkCReleaseBufWrite = io.resps.sinkC.valid && io.resps.sinkC.respInfo.last &&
    io.resps.sinkC.respInfo.opcode === ProbeAckData
  val releaseBufMatch = VecInit((0 until mshrsAll).map { i =>
    resp_sinkC_match_vec(2 * i + 1) || replaceProbeSinkCMatch(i)
  })
  io.releaseBufWriteId := probeBufferRoute.release
  val probeAckMirrorCapture = RegInit(0.U.asTypeOf(Valid(new MSHRBufWrite)))
  val probeAckMirrorCaptureGeneration = RegInit(0.U(8.W))
  probeAckMirrorCapture.valid := mirroredReplaceProbeAckDataCapture
  when(mirroredReplaceProbeAckDataCapture) {
    probeAckMirrorCapture.bits.id := OHToUInt(mirroredReplaceProbeAckDataOH)
    probeAckMirrorCapture.bits.data := io.sinkCProbeAckPayload
    probeAckMirrorCapture.bits.beatMask := Fill(beatSize, true.B)
    probeAckMirrorCaptureGeneration := Mux1H(mirroredReplaceProbeAckDataOH,
      mshrTaskGenerations(2)) + (!activeReplaceProbeAckDataCapture).asUInt
  }
  val probeAckMirrorWrite = RegNext(probeAckMirrorCapture, 0.U.asTypeOf(probeAckMirrorCapture))
  val probeAckMirrorGeneration = RegEnable(probeAckMirrorCaptureGeneration, probeAckMirrorCapture.valid)
  val mirrorOwnerOH = UIntToOH(probeAckMirrorWrite.bits.id, mshrsAll)
  val mirrorOwnerCurrent = Mux1H(mirrorOwnerOH, replaceMshrs.map(_.io.info.valid)) &&
    Mux1H(mirrorOwnerOH, mshrTaskGenerations(2)) === probeAckMirrorGeneration
  io.releaseBufWriteExtra := probeAckMirrorWrite
  io.releaseBufWriteExtra.valid := probeAckMirrorWrite.valid && mirrorOwnerCurrent
  io.releaseBufWriteExtraGeneration := probeAckMirrorGeneration
  io.releaseBufWriteIdExtra.valid := io.releaseBufWriteExtra.valid
  io.releaseBufWriteIdExtra.bits := probeAckMirrorWrite.bits.id
  mirroredReplaceProbeAckDataDoneOH := Mux(io.releaseBufWriteExtra.valid,
    mirrorOwnerOH, 0.U(mshrsAll.W))
  mirroredReplaceProbeAckDataPendingOH :=
    Mux(probeAckMirrorCapture.valid, UIntToOH(probeAckMirrorCapture.bits.id, mshrsAll), 0.U(mshrsAll.W)) |
      Mux(probeAckMirrorWrite.valid, mirrorOwnerOH, 0.U(mshrsAll.W))
  when(probeAckMirrorWrite.valid) {
    assert(mirrorOwnerCurrent, "ProbeAckData mirror lost its reserved replacement generation")
  }
  val refillBufMatch = VecInit((0 until mshrsAll).map { i => resp_sinkC_match_vec(2 * i) })
  io.refillBufWriteId.valid := sinkCReleaseBufWrite && probeBufferRoute.refill.valid
  io.refillBufWriteId.bits := probeBufferRoute.refill.bits
  io.snoopRefillBufWriteId.valid := sinkCReleaseBufWrite && probeBufferRoute.mirror.valid
  io.snoopRefillBufWriteId.bits := probeBufferRoute.mirror.bits
  when(sinkCReleaseBufWrite) {
    assert(
      PopCount(releaseBufMatch ++ refillBufMatch) === 1.U,
      "a SinkC ProbeAckData write must target exactly one physical buffer entry"
    )
    assert(
      !(io.refillBufWriteId.valid && io.snoopRefillBufWriteId.valid),
      "a ProbeAckData cannot be both a normal response and a snoop mirror"
    )
  }

  val orphanNestedwbWrite = orphanReleaseData && replaceSlotIdle.orR
  val orphanExistingReplaceWrite = orphanReleaseTargetsReplace
  val nestedReplaceMask = VecInit(replaceNestedwbCapture).asUInt |
    Mux(orphanNestedwbWrite, replaceNestedSlotOH, 0.U(mshrsAll.W)) |
    Mux(orphanExistingReplaceWrite, orphanReplaceTargetOH, 0.U(mshrsAll.W))
  val nestedSnoopMask = VecInit(nestedwbSnoopActive).asUInt
  val nestedReleaseMask = nestedReplaceMask | nestedSnoopMask
  when(orphanReleaseData) {
    assert(orphanNestedwbWrite === replaceAllocValid,
      "orphan nested payload must retain its newly accepted replacement")
  }
  assert(!(nestedReplaceMask & nestedSnoopMask).orR,
    "nested ReleaseData cannot give one physical slot two owners")
  assert(PopCount(nestedReleaseMask) <= 2.U,
    "nested ReleaseData has more than two physical consumers")

  class NestedReleaseMasks extends Bundle {
    val snoop = UInt(mshrsAll.W)
    val replace = UInt(mshrsAll.W)
    val generations = Vec(mshrsAll, UInt(8.W))
    val data = new DSBlock
    val beatMask = UInt(beatSize.W)
  }
  class NestedReleaseTarget extends Bundle {
    val id = UInt(mshrBits.W)
    val replacement = Bool()
    val generation = UInt(8.W)
  }
  class NestedReleaseWrite extends Bundle {
    val targets = Vec(2, Valid(new NestedReleaseTarget))
    val data = new DSBlock
    val beatMask = UInt(beatSize.W)
  }
  val nestedReleaseMasks = RegInit(0.U.asTypeOf(Valid(new NestedReleaseMasks)))
  nestedReleaseMasks.valid := nestedReleaseMask.orR
  when(nestedReleaseMask.orR) {
    nestedReleaseMasks.bits.snoop := nestedSnoopMask
    nestedReleaseMasks.bits.replace := nestedReplaceMask
    nestedReleaseMasks.bits.data := io.nestedwbPayload
    nestedReleaseMasks.bits.beatMask := Fill(beatSize, true.B)
    (0 until mshrsAll).foreach { i =>
      nestedReleaseMasks.bits.generations(i) := Mux(nestedReplaceMask(i),
        mshrTaskGenerations(2)(i) + replaceMshrs(i).io.alloc.valid.asUInt,
        mshrTaskGenerations(1)(i))
    }
  }

  // Capture complete C payload, masks and owner versions before the physical buffer write.
  val nestedReleaseReserved = RegInit(0.U.asTypeOf(Valid(new NestedReleaseMasks)))
  nestedReleaseReserved := nestedReleaseMasks
  val nestedReleaseWrite = RegInit(0.U.asTypeOf(Valid(new NestedReleaseWrite)))
  nestedReleaseWrite.valid := nestedReleaseReserved.valid
  val registeredMask = nestedReleaseReserved.bits.snoop | nestedReleaseReserved.bits.replace
  val nestedFirstOH = PriorityEncoderOH(registeredMask)
  val nestedSecondOH = PriorityEncoderOH(registeredMask & ~nestedFirstOH)
  when(nestedReleaseReserved.valid) {
    nestedReleaseWrite.bits.data := nestedReleaseReserved.bits.data
    nestedReleaseWrite.bits.beatMask := nestedReleaseReserved.bits.beatMask
    Seq(nestedFirstOH, nestedSecondOH).zipWithIndex.foreach { case (targetOH, port) =>
      val target = nestedReleaseWrite.bits.targets(port)
      target.valid := targetOH.orR
      target.bits.id := OHToUInt(targetOH)
      target.bits.replacement := (targetOH & nestedReleaseReserved.bits.replace).orR
      target.bits.generation := Mux1H(targetOH, nestedReleaseReserved.bits.generations)
    }
  }
  def nestedTargetMatches(target: ValidIO[NestedReleaseTarget]): Bool = {
    val targetOH = UIntToOH(target.bits.id, mshrsAll)
    val live = Mux(target.bits.replacement,
      Mux1H(targetOH, replaceMshrs.map(_.io.info.valid)),
      Mux1H(targetOH, snoopMshrs.map(_.io.status.valid)))
    val generation = Mux(target.bits.replacement,
      Mux1H(targetOH, mshrTaskGenerations(2)), Mux1H(targetOH, mshrTaskGenerations(1)))
    !target.valid || (live && generation === target.bits.generation)
  }
  val nestedReleaseVersionsMatch = nestedReleaseWrite.bits.targets.map(nestedTargetMatches).reduce(_ && _)
  val nestedReleaseValidated = RegInit(0.U.asTypeOf(Valid(new NestedReleaseWrite)))
  nestedReleaseValidated.valid := nestedReleaseWrite.valid && nestedReleaseVersionsMatch
  when(nestedReleaseWrite.valid && nestedReleaseVersionsMatch) {
    nestedReleaseValidated.bits := nestedReleaseWrite.bits
  }
  io.nestedReleaseWrites.zip(nestedReleaseValidated.bits.targets).foreach { case (write, target) =>
    write.valid := nestedReleaseValidated.valid && target.valid
    write.bits.id := target.bits.id
    write.bits.data := nestedReleaseValidated.bits.data
    write.bits.beatMask := nestedReleaseValidated.bits.beatMask
  }
  when(nestedReleaseWrite.valid) {
    assert(nestedReleaseVersionsMatch, "nested write lost its reserved owner generation")
  }
  val nestedReleaseApplied = RegInit(0.U.asTypeOf(Valid(new NestedReleaseWrite)))
  nestedReleaseApplied.valid := nestedReleaseValidated.valid
  when(nestedReleaseValidated.valid) {
    nestedReleaseApplied.bits := nestedReleaseValidated.bits
  }
  def descriptorMask(descriptor: ValidIO[NestedReleaseWrite], replacement: Boolean): UInt = {
    descriptor.bits.targets.map { target =>
      Mux(descriptor.valid && target.valid && target.bits.replacement === replacement.B,
        UIntToOH(target.bits.id, mshrsAll), 0.U(mshrsAll.W))
    }.reduce(_ | _)
  }
  // Reserve S/R owners before C target capture; pending holds must read registered descriptors.
  val nestedReleaseCaptureHold = VecInit(io.pipeStatusVec.map(status =>
    status.valid && status.bits.fromC)).asUInt.orR
  val nestedReleaseOwnerHold = nestedReleaseCaptureHold || probeLookupValid
  when(io.nestedwb.c_set_dirty) {
    assert(nestedReleaseCaptureHold, "nested C must already own its pipeline capture reservation")
  }
  when(mirroredReplaceProbeAckDataCapture) {
    assert(probeLookupValid, "ProbeAckData mirror must already own its SinkC reservation")
  }
  (0 until 2).foreach { context =>
    val replacement = context == 1
    val captured = Mux(nestedReleaseReserved.valid,
      if(replacement) nestedReleaseReserved.bits.replace else nestedReleaseReserved.bits.snoop, 0.U(mshrsAll.W))
    val masks = Mux(nestedReleaseMasks.valid,
      if(replacement) nestedReleaseMasks.bits.replace else nestedReleaseMasks.bits.snoop, 0.U(mshrsAll.W))
    val mirror = if(replacement) mirroredReplaceProbeAckDataPendingOH else 0.U(mshrsAll.W)
    nestedReleasePending(context) := Fill(mshrsAll, nestedReleaseOwnerHold) | masks | captured | mirror |
      descriptorMask(nestedReleaseWrite, replacement) | descriptorMask(nestedReleaseValidated, replacement) |
      descriptorMask(nestedReleaseApplied, replacement)
    nestedReleaseDone(context) := descriptorMask(nestedReleaseApplied, replacement)
  }
  val releaseOwnerAlloc = VecInit(snoopMshrs.map(_.io.alloc.valid)).asUInt |
    VecInit(replaceMshrs.map(_.io.alloc.valid)).asUInt
  val nestedReleaseAuthority = RegInit(0.U(mshrsAll.W))
  nestedReleaseAuthority := (nestedReleaseAuthority & ~releaseOwnerAlloc) | nestedReleaseMask
  io.nestedReleaseAuthority := nestedReleaseAuthority

  class NestedRefillCapture extends Bundle {
    val owners = UInt(mshrsAll.W)
    val generations = Vec(mshrsAll, UInt(8.W))
    val direct = UInt(mshrsAll.W)
    val nested = Bool()
    val write = Bool()
    val data = new DSBlock
  }
  class NestedRefillTransaction extends Bundle {
    val write = Bool()
    val direct = Bool()
    val nested = Bool()
    val generation = UInt(8.W)
    val payload = new MSHRBufWrite
  }
  val nestedRefillBlockedBySinkC =
    (refillNestedwbActive & sinkCReleaseHold & ~sinkCReleaseDirectWriteMatch).orR
  val nestedRefillEventOwners = refillNestedwbActive | sinkCReleaseDirectWriteMatch
  val nestedRefillCapture = RegInit(0.U.asTypeOf(Valid(new NestedRefillCapture)))
  nestedRefillCapture.valid := nestedRefillEventOwners.orR
  when(nestedRefillEventOwners.orR) {
    assert(PopCount(nestedRefillEventOwners) === 1.U,
      "nested/direct C event must identify one retained normal owner")
    nestedRefillCapture.bits.owners := nestedRefillEventOwners
    nestedRefillCapture.bits.generations := VecInit(mshrTaskGenerations(0))
    nestedRefillCapture.bits.direct := sinkCReleaseDirectWriteMatch
    nestedRefillCapture.bits.nested := refillNestedwbActive.orR
    nestedRefillCapture.bits.write := refillNestedwbActive.orR && !nestedRefillBlockedBySinkC
    nestedRefillCapture.bits.data := io.nestedwbPayload
  }
  val nestedRefillVersionsMatch = refillMshrs.zipWithIndex.map { case (refill, i) =>
    !nestedRefillCapture.bits.owners(i) || (refill.io.status.valid &&
      nestedRefillCapture.bits.generations(i) === mshrTaskGenerations(0)(i))
  }.reduce(_ && _)
  val nestedRefillWriter = RegInit(0.U.asTypeOf(Valid(new NestedRefillTransaction)))
  nestedRefillWriter.valid := nestedRefillCapture.valid && nestedRefillVersionsMatch
  when(nestedRefillCapture.valid) {
    assert(nestedRefillVersionsMatch, "nested refill lost its reserved normal generation")
    nestedRefillWriter.bits.write := nestedRefillCapture.bits.write
    nestedRefillWriter.bits.direct := nestedRefillCapture.bits.direct.orR
    nestedRefillWriter.bits.nested := nestedRefillCapture.bits.nested
    nestedRefillWriter.bits.generation := Mux1H(nestedRefillCapture.bits.owners,
      nestedRefillCapture.bits.generations)
    nestedRefillWriter.bits.payload.id := OHToUInt(nestedRefillCapture.bits.owners)
    nestedRefillWriter.bits.payload.data := nestedRefillCapture.bits.data
    nestedRefillWriter.bits.payload.beatMask := Fill(beatSize, true.B)
  }
  io.nestedRefillWrite.valid := nestedRefillWriter.valid && nestedRefillWriter.bits.write
  io.nestedRefillWrite.bits := nestedRefillWriter.bits.payload
  // Keep the owner pinned through the edge that physically applies the ValidIO write.
  val nestedRefillReceipt = RegNext(nestedRefillWriter, 0.U.asTypeOf(nestedRefillWriter))
  def nestedRefillOwner(record: ValidIO[NestedRefillTransaction]): UInt =
    Mux(record.valid, UIntToOH(record.bits.payload.id, mshrsAll), 0.U(mshrsAll.W))
  val nestedRefillReceiptOwner = nestedRefillOwner(nestedRefillReceipt)
  when(nestedRefillReceipt.valid) {
    assert(Mux1H(nestedRefillReceiptOwner, refillMshrs.map(_.io.status.valid)) &&
      Mux1H(nestedRefillReceiptOwner, mshrTaskGenerations(0)) === nestedRefillReceipt.bits.generation,
      "nested/direct C receipt must retain its normal generation through application")
  }
  nestedRefillApplied := Mux(nestedRefillReceipt.bits.nested,
    nestedRefillReceiptOwner, 0.U(mshrsAll.W))
  nestedRefillDirectApplied := Mux(nestedRefillReceipt.bits.direct,
    nestedRefillReceiptOwner, 0.U(mshrsAll.W))
  nestedRefillPending := nestedRefillEventOwners |
    Mux(nestedRefillCapture.valid, nestedRefillCapture.bits.owners, 0.U(mshrsAll.W)) |
    nestedRefillOwner(nestedRefillWriter) | nestedRefillReceiptOwner

  /* Status for prefetch controller and topDown monitor */
  prefetchOpt.foreach { _ =>
    io.dataRefill.get.valid := mshrs.map(_.io.dataRefill.valid).reduce(_ || _)
    io.dataRefill.get.bits := ParallelPriorityMux(mshrs.map(_.io.dataRefill.valid).zip(mshrs.map(_.io.dataRefill.bits)))

    val mshrReqAddr = mshrs.map(m => Cat(m.io.status.bits.reqTag, m.io.status.bits.set, 0.U(offsetBits.W)))
    val mshrInFlightDemand = mshrs.map { m =>
      m.io.status.valid && !m.io.status.bits.will_free && m.io.status.bits.is_miss && !m.io.status.bits.is_prefetch
    }
    val delayHitVec = Wire(Vec(mshrsAll, Bool()))
    val busHitVec = Wire(Vec(mshrsAll, Bool()))
    val bankHitVec = Wire(Vec(mshrsAll, Bool()))
    val pfRefillReturn = io.dataRefill.get.valid && io.dataRefill.get.bits.isPrefetch
    for (i <- 0 until mshrsAll) {
      val pfAddr = io.dataRefill.get.bits.addr
      delayHitVec(i) := pfRefillReturn && mshrInFlightDemand(i)
      busHitVec(i) := pfRefillReturn && mshrInFlightDemand(i) &&
        getDramChannel(mshrReqAddr(i)) === getDramChannel(pfAddr)
      bankHitVec(i) := pfRefillReturn && mshrInFlightDemand(i) &&
        getDramBank(mshrReqAddr(i)) === getDramBank(pfAddr)
    }
    io.busContention.get.valid := pfRefillReturn
    io.busContention.get.bits.pfReqSrc := io.dataRefill.get.bits.pfReqSrc
    io.busContention.get.bits.delayHit := delayHitVec.asUInt.orR
    io.busContention.get.bits.busHit := busHitVec.asUInt.orR
    io.busContention.get.bits.bankHit := bankHitVec.asUInt.orR
  }

  topDownOpt.foreach { _ =>
    io.msStatus.get.zip(io.msAlloc.get).zip(refillMshrs).foreach {
      case ((statusOut, allocOut), mshr) =>
        // status
        statusOut := mshr.io.status
        // alloc
        allocOut := mshr.io.statAlloc
    }
  }

  /* Performance counters */
  XSPerfAccumulate("capacity_conflict_to_sinkA", a_mshrFull)
  XSPerfAccumulate("capacity_conflict_to_sinkB", mshrFull)
  XSPerfAccumulate("waylock_normal_sum",
    PopCount(io.refillInfo.map(s => s.valid && s.bits.blockRefill)))
  XSPerfAccumulate("waylock_replace_sum",
    PopCount(io.replaceInfo.map(_.valid)))
  XSPerfAccumulate("waylock_postGrantReleaseHold_sum",
    PopCount(io.refillInfo.map(s => s.valid && s.bits.postGrantReleaseHold)))
  XSPerfAccumulate("replaceMSHR_alloc_cnt",
    PopCount(replaceMshrs.map(_.io.alloc.valid)))
  XSPerfAccumulate("victim_probe_cnt",
    PopCount(replaceMshrs.map(_.io.sourceBAccepted)))
  XSPerfHistogram(
    "mshr_alloc",
    io.toMainPipe.mshr_alloc_ptr,
    enable = io.fromMainPipe.mshr_alloc_s3.valid,
    start = 0,
    stop = mshrsAll,
    step = 1
  )
  if (cacheParams.enablePerf) {
    // val start = 0
    // val stop = 100
    // val step = 5
    val acquire_period = ParallelMux(refillMshrs.map {
      case m => m.acquire_period.get.valid -> m.acquire_period.get.bits
    })
    val release_period = ParallelMux(refillMshrs.map {
      case m => m.release_period.get.valid -> m.release_period.get.bits
    })
    val acquire_period_en = refillMshrs.map(_.acquire_period.get.valid).reduce(_ || _)
    val release_period_en = refillMshrs.map(_.release_period.get.valid).reduce(_ || _)
    XSPerfHistogram("acquire_period", acquire_period, acquire_period_en, 0, 30, 1, true, true)
    XSPerfHistogram("acquire_period", acquire_period, acquire_period_en, 30, 100, 5, true, true)
    XSPerfHistogram("acquire_period", acquire_period, acquire_period_en, 100, 200, 10, true, true)
    XSPerfHistogram("acquire_period", acquire_period, acquire_period_en, 200, 1000, 100, true, true)
    XSPerfHistogram("acquire_period", acquire_period, acquire_period_en, 1000, 5000, 1000, true, false)

    XSPerfHistogram("release_period", release_period, release_period_en, 0, 30, 1, true, true)
    XSPerfHistogram("release_period", release_period, release_period_en, 30, 100, 5, true, true)
    XSPerfHistogram("release_period", release_period, release_period_en, 100, 200, 10, true, true)
    XSPerfHistogram("release_period", release_period, release_period_en, 200, 1000, 100, true, true)
    XSPerfHistogram("release_period", release_period, release_period_en, 1000, 5000, 1000, true, false)

    val timers = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(64.W))))
    for (((timer, m), i) <- timers.zip(refillMshrs).zipWithIndex) {
      when(m.io.alloc.valid) {
        timer := 1.U
      }.otherwise {
        timer := timer + 1.U
      }
      val enable = m.io.status.valid && m.io.status.bits.will_free
      XSPerfHistogram("mshr_latency_" + Integer.toString(i, 10),
        timer, enable, 0, 300, 10, true, true)
      XSPerfHistogram("mshr_latency_" + Integer.toString(i, 10),
        timer, enable, 300, 1000, 50, true, true)
      XSPerfHistogram("mshr_latency_" + Integer.toString(i, 10),
        timer, enable, 1000, 5000, 200, true, false)
      XSPerfMax("mshr_latency_" + Integer.toString(i, 10), timer, enable)
    }
  }

  val hpm_timers = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(10.W))))
  val longMissRetired = Wire(Vec(mshrsAll, Bool()))
  for (((hpm_timer, m), i) <- hpm_timers.zip(refillMshrs).zipWithIndex) {
    when(m.io.alloc.valid) {
      hpm_timer := 1.U
    }.otherwise {
      hpm_timer := hpm_timer + 1.U
    }
    longMissRetired(i) := RegNext(
      m.io.status.valid && m.io.status.bits.will_free && hpm_timer > 200.U,
      false.B)
  }

  val perfEvents = Seq(
    ("l2_cache_refill", io.resps.rxdat.valid && io.resps.rxdat.respInfo.last),
    ("l2_cache_rd_refill", io.resps.rxdat.valid && io.resps.rxdat.respInfo.last),
    ("l2_cache_wr_refill", false.B),
    ("l2_cache_long_miss", PopCount(longMissRetired))
  )
  generatePerfEvent()
}
