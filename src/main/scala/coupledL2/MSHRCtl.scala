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
      // MainPipe signals that a replace capture task (DS read of a client-less
      // victim) wrote the victim data into the replace slot's ReleaseBuf.
      val replaceCaptureWrite = Flipped(ValidIO(UInt(mshrBits.W)))
      // MainPipe finished a late RefillBuf[N] -> ReleaseBuf[S] copy for a
      // post-Grant snoop that allocated after the ReleaseData payload.
      val refillToReleaseCopyWrite = Flipped(ValidIO(UInt(mshrBits.W)))
      // MainPipe has reached S3 and completed a normal refill commit. A
      // post-Grant toN may cancel its DS/meta write at that point.
      val refillWriteDone = Flipped(ValidIO(new MainpipeLeaseDone))
      // MainPipe has actually transferred a normal L1-visible response to
      // GrantBuffer. A task accepted by RequestArb may still be cancelled by
      // the MainPipe replacer-retry path.
      val grantSent = Flipped(ValidIO(UInt(mshrBits.W)))
      // A C ReleaseData direct-write was committed at MainPipe S3.  Match it
      // back to any normal MSHR that owns the same request line.
      val releaseDataDirectWrite = Flipped(ValidIO(new TaskBundle))
      // A dirty C ReleaseData which misses the L2 directory has no normal
      // context to own its lower-level writeback.
      val orphanReleaseData = Input(Bool())
      // A toN snoop which required no SnoopMSHR has committed its Directory
      // invalidation at MainPipe S3.
      val snoopToNDirectDone = Flipped(ValidIO(new TaskBundle))
      // A refill grant merged the refill install into its own S3 pass
      // (victim snapshot read + Directory write).  Bits carry the parent
      // normal-context id; the same cycle any ReplaceMSHR allocation for
      // this ReplacerResult must forgo its own DS capture.
      val grantInstallDone = Flipped(ValidIO(UInt(mshrBits.W)))
    }
    val toMainPipe = new Bundle() {
      val mshr_alloc_ptr = Output(UInt(mshrBits.W))
      // A replacement victim is still owned by another context. MainPipe must
      // drop this grant/replacer pass exactly as it drops Directory.retry;
      // notifying only the parent MSHR would leave an irrevocable SourceD task.
      val replRespRetry = Output(Bool())
      // Data-carrying snoops reserve their hit DS way until the snoop context
      // retires.  MainPipe uses this only as a write-side safety assertion;
      // Directory enforces the actual replacement exclusion.
      val snoopDataHolds = Vec(mshrsAll, ValidIO(new MSHRInfo()))
      // A normal context may hold a post-Grant ReleaseData in RefillBuf even
      // though Directory already sees no L1 client. MainPipe must allocate a
      // paired SnoopMSHR for a same-line B so it can return that payload.
      val postGrantReleaseHolds = Vec(mshrsAll, ValidIO(new MSHRInfo()))
      // The ReplacerResult returning at MainPipe S3 selects a no-probe
      // victim (invalid or client-less) with no ownership conflict and, for
      // a valid victim, a live ReleaseBuf reservation.  MainPipe may then
      // merge the refill install into the grant pass; grantInstallSlot is
      // the reserved sidecar ReleaseBuf slot for the victim snapshot.
      val grantInstallOK = Output(Bool())
      val grantInstallSlot = Output(UInt(mshrBits.W))
      val grantInstallGeneration = Output(UInt(8.W))
      val replParentGeneration = Output(UInt(8.W))
      val replParentEpoch = Output(Bool())
    }

    // Final external TXDAT/TXRSP handshakes. A sidecar cannot release its
    // ReleaseBuf merely because MainPipe has enqueued the message.
    val txdatDone = Input(ValidIO(new TaskBundle))
    val txrspDone = Input(ValidIO(new TaskBundle))

    /* to request arbiter */
    // val mshrFull = Output(Bool())
    val mshrTask = DecoupledIO(new TaskBundle())
    val mshrTaskLeaseToken = Output(new MainpipeLeaseToken)
    val bServiceStart = Flipped(ValidIO(new TaskBundle))
    val sidecarReservationHold = Input(Bool())
    val contextReservation = Input(Bool())

    /* status of s2 and s3 */
    val pipeStatusVec = Flipped(Vec(3, ValidIO(new PipeStatus)))

    /* send reqs */
    val toTXREQ = DecoupledIO(new CHIREQ())
    // Occupancy of the per-ReplaceMSHR TXREQ skids.  These flits have left
    // their sidecar but have not reached the TXREQ queue yet, so TXREQ must
    // count them in inflightCnt/noSpace alongside the pipeline statuses.
    val replaceTxreqPendingCnt = Output(UInt(log2Up(mshrsAll + 1).W))
    val toTXRSP = DecoupledIO(new CHIRSP()) // TODO: unify with main pipe, which should be TaskBundle
    // Direct ReplaceMSHR CopyBackWrData: task+data enqueued into TXDAT
    // without a MainPipe pass.  Data comes from the ReleaseBuf second read
    // port, read when the sidecar is picked below.
    val toTXDAT = DecoupledIO(new TaskWithData())
    val txdatDirectSpace = Input(Bool())
    val releaseBufDirectRd = ValidIO(new MSHRBufRead)
    val releaseBufDirectResp = Input(new MSHRBufResp)
    val toSourceB = DecoupledIO(new TLBundleB(edgeIn.bundle))

    /* to block sourceB from sending same-addr probe until GrantAck received */
    val grantStatus = Input(Vec(grantBufInflightSize, new GrantStatus()))

    /* receive resps */
    val resps = Input(new Bundle() {
      val sinkC = new RespBundle() //probeAck from core
      val rxrsp = new RespBundle() //releaseAck(CompDBID) from CHI
      val rxdat = new RespBundle() //AcquireBlock(CompData) from CHI
    })
    // ReleaseData bypasses RespBundle and travels SinkC -> RequestArb ->
    // MainPipe. SinkC provides its accepted address early so a replacement
    // sidecar cannot copy back stale ProbeAckData before it arrives.
    val sinkCReleaseData = Input(ValidIO(new TaskBundle))

    val sinkCProbeAckPayload = Input(new DSBlock)
    val sinkCRefillWrite = Input(Valid(new MSHRBufWrite))
    val sinkCRefillWriteApplied = Output(Valid(new MSHRBufWrite))
    val releaseBufWriteId = Output(UInt(mshrBits.W))
    // A ProbeAckData can belong to a live snoop and a victim sidecar for the
    // same line. Keep the regular writer and expose the sidecar mirror as an
    // independent ReleaseBuf write port.
    val releaseBufWriteIdExtra = Output(ValidIO(UInt(mshrBits.W)))
    // Registered mirror route carries payload and target generation together.
    val releaseBufWriteExtra = Output(ValidIO(new MSHRBufWrite))
    val releaseBufWriteExtraGeneration = Output(UInt(8.W))
    // ProbeAckData for a normal context (Get-on-TRUNK / cache-alias) is
    // captured into the normal slot's RefillBuf; the sidecar and snoop own
    // the ReleaseBuf entries.
    val refillBufWriteId = Output(ValidIO(UInt(mshrBits.W)))
    // If a snoop and normal context share a physical entry for the same
    // cache line, ProbeAckData must update both contexts.  The snoop still
    // needs ReleaseBuf to produce SnpRespData, while the normal Grant must
    // not consume its earlier RefillBuf snapshot.
    val snoopRefillBufWriteId = Output(ValidIO(UInt(mshrBits.W)))

    /* nested writeback */
    val nestedwb = Input(new NestedWriteback)
    val nestedwbPayload = Input(new DSBlock)
    // Both ports belong to one registered descriptor and complete together.
    val nestedReleaseWrites = Vec(2, ValidIO(new MSHRBufWrite))
    val nestedReleaseAuthority = Output(UInt(mshrsAll.W))
    // ReleaseData for the line currently being refilled supersedes the
    // normal context's earlier CompData/DS snapshot.
    val nestedwbRefillDataId = Output(ValidIO(UInt(mshrBits.W)))
    // Once ReleaseData replaces a normal context's payload, later beats from
    // its already-issued L3 read remain control responses only.
    val refillBufReleaseOwner = Output(UInt(mshrsAll.W))
    // Slots whose current tenant has already accepted a read response with a
    // different DBID than the CompData on io.resps.rxdat.  That beat belongs to
    // an earlier tenant of the same TxnID and must reach neither the MSHR nor
    // its RefillBuf.
    val rxdatStaleSlots = Output(UInt(mshrsAll.W))

    /* Independent normal, snoop, and replacement ownership views. */
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
    val pCrd = Vec(mshrsAll, new PCrdQueryBundle)

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

  // A normal context and a snoop context share one physical MSHR ID. Only the
  // normal context participates in L3 read/replacer/Grant paths.
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
  // Retirement mailboxes capture OLD source and target identities at the
  // actual retirement edge. Apply never reads a retired/reused source slot.
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
  val snoopRetireEvents = snoopMshrs.zipWithIndex.map { case (source, i) =>
    val event = RegInit(0.U.asTypeOf(Valid(new OwnerRetirement)))
    val retiring = source.io.msInfo.valid && source.io.msInfo.bits.willFree
    event.valid := retiring
    when(retiring) {
      event.bits.sourceId := i.U
      event.bits.sourceGeneration := mshrTaskGenerations(1)(i)
      event.bits.sourceSet := source.io.msInfo.bits.set
      event.bits.sourceTag := source.io.msInfo.bits.reqTag
      event.bits.sourceEpoch := false.B
      event.bits.targets := VecInit(replaceMshrs.map { target =>
        target.io.info.valid && target.io.info.bits.set === source.io.msInfo.bits.set &&
          target.io.info.bits.tag === source.io.msInfo.bits.reqTag
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
  // Full pair context, including a known absence, is registered independently
  // of SourceB and group prepare. A late ReleaseStart is part of this capture,
  // so a previously standalone snoop re-evaluates its payload provenance.
  class SnoopPair extends Bundle {
    val snoopId = UInt(mshrBits.W)
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
      pair.bits.snoopId := i.U
      pair.bits.snoopGeneration := mshrTaskGenerations(1)(i)
      pair.bits.set := snoop.io.status.bits.set
      pair.bits.tag := snoop.io.status.bits.reqTag
      pair.bits.normal.valid := normalMatches.orR
      pair.bits.normal.bits.id := OHToUInt(normalMatches)
      pair.bits.normal.bits.generation := Mux1H(normalMatches, mshrTaskGenerations(0))
      pair.bits.normal.bits.set := Mux1H(normalMatches, refillMshrs.map(_.io.status.bits.set))
      pair.bits.normal.bits.tag := Mux1H(normalMatches, refillMshrs.map(_.io.status.bits.reqTag))
      val releaseStarts = io.sinkCReleaseData.valid &&
        io.sinkCReleaseData.bits.set === snoop.io.status.bits.set &&
        io.sinkCReleaseData.bits.tag === snoop.io.status.bits.reqTag
      pair.bits.postGrantPending := Mux1H(normalMatches, refillMshrs.map(_.io.postGrantReleaseHold)) ||
        (releaseStarts && Mux1H(normalMatches, refillMshrs.map(_.io.postGrantReleaseWindow)))
      pair.bits.payloadReady := Mux1H(normalMatches, refillMshrs.map(_.io.postGrantReleasePayloadReady))
      pair.bits.probePayload := normalMatches(i) && refillMshrs(i).io.refillPayloadFromProbeAck
    }
    pair
  }
  val snoopPairKnown = snoopPairs.zipWithIndex.map { case (pair, i) =>
    pair.valid && snoopMshrs(i).io.status.valid &&
      pair.bits.snoopGeneration === mshrTaskGenerations(1)(i) &&
      pair.bits.set === snoopMshrs(i).io.status.bits.set &&
      pair.bits.tag === snoopMshrs(i).io.status.bits.reqTag
  }
  val snoopPairNormalValid = snoopPairs.zipWithIndex.map { case (pair, i) =>
    val normalOH = UIntToOH(pair.bits.normal.bits.id, mshrsAll)
    snoopPairKnown(i) && pair.bits.normal.valid &&
      Mux1H(normalOH, refillMshrs.map(_.io.status.valid)) &&
      Mux1H(normalOH, mshrTaskGenerations(0)) === pair.bits.normal.bits.generation &&
      Mux1H(normalOH, refillMshrs.map(_.io.status.bits.set)) === pair.bits.normal.bits.set &&
      Mux1H(normalOH, refillMshrs.map(_.io.status.bits.reqTag)) === pair.bits.normal.bits.tag
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
  // SinkC ReleaseData is accepted before its two-beat payload reaches the
  // RefillBuf write port.  Pin the matching normal owner across that gap so
  // retirement cannot invalidate the generation between C acceptance and
  // refill-record capture.  Direct C-path writes clear the same hold when
  // MainPipe applies them, while the buffered path clears on sinkCRefillWrite.
  val sinkCReleaseHold = RegInit(0.U(mshrsAll.W))
  val sinkCReleaseHoldSet = VecInit(refillMshrs.map { refill =>
    io.sinkCReleaseData.valid && refill.io.status.valid &&
      refill.io.status.bits.set === io.sinkCReleaseData.bits.set &&
      refill.io.status.bits.reqTag === io.sinkCReleaseData.bits.tag &&
      (refill.io.msInfo.bits.blockRefill ||
        (refill.io.msInfo.bits.s_refill && refill.io.msInfo.bits.w_grantlast))
  }).asUInt
  val sinkCReleaseHoldClear = VecInit(refillMshrs.zipWithIndex.map { case (refill, i) =>
    (io.sinkCRefillWrite.valid && io.sinkCRefillWrite.bits.id === i.U) ||
      (io.fromMainPipe.releaseDataDirectWrite.valid && refill.io.status.valid &&
        refill.io.status.bits.set === io.fromMainPipe.releaseDataDirectWrite.bits.set &&
        refill.io.status.bits.reqTag === io.fromMainPipe.releaseDataDirectWrite.bits.tag)
  }).asUInt
  val sinkCReleaseHoldLive = VecInit(refillMshrs.map(_.io.status.valid)).asUInt
  sinkCReleaseHold :=
    (sinkCReleaseHold | sinkCReleaseHoldSet) & ~sinkCReleaseHoldClear & sinkCReleaseHoldLive
  when(sinkCRefillRecord.valid) {
    assert(sinkCRefillCurrent, "SinkC refill route lost its reserved normal generation")
  }
  // Index0 is snoop, index1 is replacement. These retain each owner until
  // the entire nested-data transaction has physically written and applied.
  val nestedReleasePending = Wire(Vec(2, UInt(mshrsAll.W)))
  val nestedReleaseDone = Wire(Vec(2, UInt(mshrsAll.W)))
  val mshrs: Seq[MSHRContextBase] = refillMshrs.zip(snoopMshrs).flatMap {
    case (refill, snoop) => Seq[MSHRContextBase](refill, snoop)
  }
  val contextPhysicalIds = Seq.tabulate(mshrsAll)(i => Seq(i, i)).flatten
  io.refillVictimDelegated := VecInit(refillMshrs.map(_.io.sidecarDelegated)).asUInt
  require(mshrs.size == 2 * mshrsAll)
  require(mshrsAll >= 2, "independent ReplaceMSHR needs one reservable physical slot")

  // A replacement is an independent transaction. Its physical ID owns only
  // ReleaseBuf/SourceB/CHI writeback; parentId continues to own RefillBuf and
  // the eventual DS install. ReplacerResult has no backpressure, and nested C
  // cannot migrate data between ReleaseBuf entries, so the reserved slot must
  // be allocated in the same cycle as this response.
  // Every valid victim is captured into its sidecar ReleaseBuf before the
  // parent reuses its DS way. Dirty victims later use that snapshot for
  // WriteBackFull; clean victims can still use it for a late nested snoop and
  // otherwise issue data-less Evict. ReplaceMSHR waits for its client Probe,
  // so a real ProbeAckData takes precedence over the DS fallback. The replacer
  // prefers invalid ways (state === INVALID): those are free ways, not
  // victims, and must not create a ReplaceMSHR.
  val replRespHasVictim = io.replResp.valid && !io.replResp.bits.hit && !io.replResp.bits.retry &&
    io.replResp.bits.meta.state =/= INVALID
  // Directory replacement results have no backpressure. A detached orphan
  // sidecar or an active snoop may still own the selected victim. Replacing
  // that line would create two ReleaseBuf owners for one address. Feed the
  // result back as a retry and re-read after the current owner retires.
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
  // A line with a live CMO context must not be evicted from under it: the
  // CMO sidecar will probe/capture/write back that very line.  Force the
  // replacer read to retry until the CMO retires, mirroring the sidecar and
  // snoop conflicts. Keep even a retiring CMO until its req_valid register
  // clears; a same-cycle will_free exemption fed allocation back into retry.
  val replRespCmoConflictCapture = VecInit(refillMshrs.map { refill =>
    replRespHasVictim && refill.io.status.valid && refill.io.status.bits.isCmo &&
      refill.io.status.bits.set === io.replResp.bits.set &&
      refill.io.status.bits.reqTag === io.replResp.bits.tag
  }).asUInt.orR
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

  // A ReplacerResult has no backpressure. Reserve an S/R-mutual ReleaseBuf
  // slot before a normal MSHR may issue the lookup, then transfer that
  // reservation atomically to ReplaceMSHR when the victim is valid.
  val replResponseApplied = Wire(Valid(new ReplacerResult))
  val replaceReserveValid = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val replaceReserveSlot = Reg(Vec(mshrsAll, UInt(mshrBits.W)))
  val replaceReserveSlotOHByParent = Seq.tabulate(mshrsAll) { parent =>
    Mux(replaceReserveValid(parent), UIntToOH(replaceReserveSlot(parent), mshrsAll), 0.U(mshrsAll.W))
  }
  val replaceReservedSlotOH = replaceReserveSlotOHByParent.reduce(_ | _)
  // A captured B owns a concrete S/R slot before its Directory execution.
  // It transfers to SnoopMSHR at result apply, or is returned on the direct
  // response path. No slot can be lent to replacement in the meantime.
  val snoopServiceValid = RegInit(false.B)
  val snoopServiceSlot = Reg(UInt(mshrBits.W))
  val snoopServiceTask = Reg(new TaskBundle)
  val snoopServiceSlotOH = Mux(snoopServiceValid,
    UIntToOH(snoopServiceSlot, mshrsAll), 0.U(mshrsAll.W))
  assert(
    PopCount(replaceReservedSlotOH) === PopCount(replaceReserveValid.asUInt),
    "each pending replacer lookup must own one unique S/R reservation"
  )
  // Keep the S/R candidate view independent from RequestArb's S1 handshake.
  // A B request may complete directly in MainPipe, so reserving a physical
  // slot for every (or every toN) B perturbs ReplaceMSHR selection.
  // Build the eligibility vector with bit-parallel masks. The old per-slot
  // boolean map expressed the same truth table, but replicated the slot-valid
  // fan-in in the blockB_s1 cone that also drives RXSNP/ReleaseBuf admission.
  // This exact physical eligibility is checked at capture/reservation;
  // aggregate admission capacity comes from the registered credit ledger.
  val snoopStatusValidOH = VecInit(snoopMshrs.map(_.io.status.valid)).asUInt
  val replaceInfoValidOH = VecInit(replaceMshrs.map(_.io.info.valid)).asUInt
  val refillBlocksSnoopOH = VecInit(refillMshrs.map { refill =>
    refill.io.status.valid && (
      refill.io.status.bits.isCmo ||
        refill.io.status.bits.w_c_resp ||
        refill.io.status.bits.releaseDataWindow
    )
  }).asUInt
  val snoopBaseEligibleOH = ~(
    snoopStatusValidOH |
      replaceInfoValidOH |
      replaceReservedSlotOH |
      snoopServiceSlotOH |
      refillBlocksSnoopOH
  )
  val replaceSlotUnreservedOH = VecInit((0 until mshrsAll).map { i =>
    !snoopMshrs(i).io.status.valid && !replaceMshrs(i).io.info.valid &&
      !replaceReservedSlotOH(i) && !snoopServiceSlotOH(i)
  }).asUInt
  // A B request has priority over a new replacement reservation.  The
  // reservation is registered only at the next edge, so granting it in the
  // same cycle as a SnoopMSHR allocation would let both contexts select the
  // same ReleaseBuf slot.
  val snoopAllocRequest = io.fromMainPipe.mshr_alloc_s3.valid &&
    io.fromMainPipe.mshr_alloc_s3.bits.task.fromB
  // A B admitted at S1 does not reveal whether it needs a SnoopMSHR until
  // S3. Keep its capacity available across those two stages: a new Replace
  // reservation must not consume the last slot after the S1 admission
  // check. RXSNP's candidate can depend on same-cycle replacement ownership,
  // so let it settle for one cycle before admitting it or fencing reserves.
  val snoopCandidateSeen = RegNext(io.fromReqArb.status_s1.bCandidateValid, false.B)
  val snoopEntering_s2 = RegNext(io.fromReqArb.status_s1.bFire, false.B)
  val snoopEntering_s3 = RegNext(snoopEntering_s2, false.B)
  val snoopEnteringDecision = RegNext(snoopEntering_s3, false.B)
  val snoopEnteringApply = RegNext(snoopEnteringDecision, false.B)
  val snoopEnteringCount = PopCount(Seq(
    snoopEntering_s2, snoopEntering_s3, snoopEnteringDecision, snoopEnteringApply))
  when(snoopEnteringApply) {
    assert(snoopServiceValid, "B result applied without its reserved service slot")
    snoopServiceValid := false.B
  }
  when(io.bServiceStart.valid) {
    assert(!snoopServiceValid && snoopBaseEligibleOH.orR,
      "B service requires a unique available physical slot")
    snoopServiceValid := true.B
    snoopServiceSlot := OHToUInt(PriorityEncoderOH(snoopBaseEligibleOH))
    snoopServiceTask := io.bServiceStart.bits
    when(io.bServiceStart.bits.snpHitRelease) {
      val owner = Mux1H(UIntToOH(io.bServiceStart.bits.snpHitReleaseIdx, mshrsAll),
        replaceMshrs.map(_.io.info))
      assert(owner.valid && owner.bits.set === io.bServiceStart.bits.set &&
        owner.bits.tag === io.bServiceStart.bits.tag,
        "captured B lost its sidecar owner before service reservation")
    }
  }
  val snoopReservationFence = snoopCandidateSeen ||
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
  (0 until mshrsAll).foreach { parent =>
    val reserveResponse = replResponseApplied.valid && replResponseApplied.bits.mshrId === parent.U
    val reserveStale = replaceReserveValid(parent) && !refillMshrs(parent).io.status.valid &&
      !refillMshrs(parent).io.replReadWanted && !refillMshrs(parent).io.replReadInFlight
    when(reserveResponse || reserveStale) {
      replaceReserveValid(parent) := false.B
    }
    when(replaceReserveGrant && replaceReserveGrantParentOH(parent)) {
      replaceReserveValid(parent) := true.B
      replaceReserveSlot(parent) := OHToUInt(replaceReserveGrantSlotOH)
    }
  }
  // A ReplacerResult issued before an invalidating snoop can return after the
  // parent has already delegated a victim to ReplaceMSHR.  It must be retried
  // after that child completes; allocating it now would give one parent two
  // ReleaseBuf-owning children.
  val replRespParentConflictCapture = replRespHasVictim &&
    Mux1H(replRespParentOHCapture, replaceActiveForParent)
  val replRespNeedsReplaceRawCapture = replRespHasVictim && !replRespOwnerConflictCapture && !replRespParentConflictCapture
  val replRespReservationOHCapture = Mux1H(replRespParentOHCapture, replaceReserveSlotOHByParent)
  val replRespNoReplaceSlotCapture = replRespNeedsReplaceRawCapture && !replRespReservationOHCapture.orR
  val replRespNeedsReplaceCapture = replRespNeedsReplaceRawCapture && !replRespNoReplaceSlotCapture
  io.toMainPipe.replRespRetry := replRespSnoopConflict || replRespParentConflictCapture || replRespNoReplaceSlotCapture ||
    replRespCmoConflictCapture
  // A refill grant pass may merge the refill install when its folded
  // ReplacerResult selects a no-probe victim.  The victim/slot/ownership
  // side of that decision lives here (single source); MainPipe adds the
  // task identity and the MSHR pre-filters request-side corner cases.
  val replRespNoProbeVictim = io.replResp.valid && !io.replResp.bits.hit && !io.replResp.bits.retry &&
    (io.replResp.bits.meta.state === INVALID || !io.replResp.bits.meta.clients.orR)
  io.toMainPipe.grantInstallOK := replRespNoProbeVictim &&
    !replRespOwnerConflictCapture && !replRespParentConflictCapture && !replRespCmoConflictCapture &&
    (io.replResp.bits.meta.state === INVALID || replRespReservationOHCapture.orR)
  io.toMainPipe.grantInstallSlot := OHToUInt(replRespReservationOHCapture)
  when(replRespSnoopConflict) {
    assert(PopCount(replRespSnoopConflictOH) === 1.U, "replacement victim matches multiple active snoop contexts")
  }
  // The action, response, reservation and old parent identity form one
  // execution record. No conflict/slot decision is recomputed at apply.
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
    val cmoConflict = Bool()
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
    replDecision.bits.cmoConflict := replRespCmoConflictCapture
  }
  io.toMainPipe.replParentGeneration := Mux1H(replRespParentOHCapture, mshrTaskGenerations(0))
  io.toMainPipe.replParentEpoch := Mux1H(replRespParentOHCapture, refillMshrs.map(_.io.allocEpoch))
  io.toMainPipe.grantInstallGeneration :=
    Mux1H(replRespReservationOHCapture, mshrTaskGenerations(2)) + replRespNeedsReplaceCapture.asUInt
  val replRespParentOH = UIntToOH(replDecision.bits.response.mshrId, mshrsAll)
  val replDecisionCurrent = Mux1H(replRespParentOH, refillMshrs.map(_.io.status.valid)) &&
    Mux1H(replRespParentOH, mshrTaskGenerations(0)) === replDecision.bits.parentGeneration &&
    Mux1H(replRespParentOH, refillMshrs.map(_.io.allocEpoch)) === replDecision.bits.parentEpoch
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
  val replRespCmoConflict = replDecision.bits.cmoConflict
  when(replDecision.valid) {
    assert(replDecisionCurrent, "replacer decision lost its retained normal parent")
    assert(replSlotCurrent, "replacer decision lost its retained sidecar slot generation")
  }
  // An invalid victim may become dirty when a nested C ReleaseData reaches
  // MainPipe after the replacer selected it.  Allocate a sidecar in that same
  // cycle so its payload is routed directly into the new ReleaseBuf entry.
  val nestedReleaseParentOH = VecInit(refillMshrs.map(_.io.nestedReleaseNeedsReplace)).asUInt
  val nestedReleaseNeedsReplace = nestedReleaseParentOH.orR &&
    !Mux1H(nestedReleaseParentOH, replaceActiveForParent)
  // These ownership predicates depend only on registered MSHR state and the
  // incoming C task. Do not use nestedwbData here: normal nestedwbData also
  // observes replaceActive, which is driven by this cycle's allocation.
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
  refillBufReleaseOwner := (refillBufReleaseOwner | refillNestedwbActive) & ~normalAllocOH
  io.refillBufReleaseOwner := refillBufReleaseOwner | refillNestedwbActive
  // Existing contexts own matching nested writes. A directory-miss C write
  // with no normal/snoop owner can still belong to a detached ReplaceMSHR:
  // its parent has retired, but its ReleaseBuf and victim transaction remain
  // live. Reuse that sidecar instead of allocating a second one for the same
  // victim.
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
  // A CMO context whose hit line needs a lower-level writeback requests a
  // CMO-mode ReplaceMSHR (probe+capture+release of its own line).  Lowest
  // priority: it waits behind any replacement-path allocation and never
  // preempts a snoop allocation.  The parent keeps requesting until the
  // child appears, so no reservation is needed.
  val replaceSlotIdle = replaceSlotUnreservedOH &
    ~Mux(replaceReserveGrant, replaceReserveGrantSlotOH, 0.U(mshrsAll.W))
  val cmoSidecarReqOH = VecInit(refillMshrs.map(_.io.cmoSidecarReq)).asUInt
  // Any returning lookup gets this cycle; retry the CMO from its registered
  // request next cycle instead of routing victim-tag conflict logic through
  // the CMO's same-cycle ReleaseData target.
  // Like a new replacer reservation, a background CMO child must preserve
  // capacity already promised to a B candidate or a B in S2/S3. It has no
  // pipeline entry in pipeContextCount, so merely excluding S3 allocation
  // allows it to consume the last S/R slot in the B's S1 handshake cycle.
  // The parent's registered request remains pending until this fence clears.
  val cmoSidecarAlloc = cmoSidecarReqOH.orR && !io.replResp.valid && !replResponseApplied.valid &&
    !nestedReleaseNeedsReplace && !orphanReleaseData && !snoopReservationFence &&
    !snoopAllocRequest &&
    replaceSlotIdle.orR
  val cmoSidecarParentOH = PriorityEncoderOH(cmoSidecarReqOH)
  val replaceNeedsAlloc = replRespNeedsReplace || nestedReleaseNeedsReplace ||
    orphanReleaseData || cmoSidecarAlloc
  val replaceParentOH = Mux(replRespNeedsReplace, replRespParentOH,
    Mux(cmoSidecarAlloc, cmoSidecarParentOH, nestedReleaseParentOH))
  val replaceParentId = OHToUInt(replaceParentOH)
  assert(
    PopCount(Cat(replRespNeedsReplace, nestedReleaseNeedsReplace, orphanReleaseData, cmoSidecarAlloc)) <= 1.U,
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
  when(cmoSidecarAlloc) {
    // The CMO's own hit line plays the victim role: way/meta come from the
    // parent's live registers (a hit never re-reads the replacer).
    incomingVictim.tag := Mux1H(cmoSidecarParentOH, refillMshrs.map(_.io.msInfo.bits.reqTag))
    incomingVictim.set := Mux1H(cmoSidecarParentOH, refillMshrs.map(_.io.msInfo.bits.set))
    incomingVictim.way := Mux1H(cmoSidecarParentOH, refillMshrs.map(_.io.msInfo.bits.way))
    incomingVictim.meta := Mux1H(cmoSidecarParentOH, refillMshrs.map(_.io.msInfo.bits.meta))
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
  incomingReplaceReq.parentAttached := !orphanReleaseData
  incomingReplaceReq.dirResult := incomingVictim
  incomingReplaceReq.mode := Mux(cmoSidecarAlloc,
    Mux1H(cmoSidecarParentOH, refillMshrs.map(_.io.cmoSidecarMode)),
    ReplaceMSHRMode.victim)
  incomingReplaceReq.needProbe := (replRespNeedsReplace || cmoSidecarAlloc) &&
    incomingVictim.meta.clients.orR
  // A victim replacement must revoke every upper copy before its old data is
  // released to L3.  ReplaceRequest is zero-initialized, and zero is toT.
  // CBO.clean keeps the line shared (probe toB); flush/inval revoke it.
  incomingReplaceReq.probeParam := Mux(
    cmoSidecarAlloc && incomingReplaceReq.mode === ReplaceMSHRMode.cmoClean, toB, toN)
  // When the parent's grant pass merges the install this very cycle, it also
  // performs the victim DS snapshot (ReleaseBuf write lands at its S5), so
  // the new sidecar must not issue a second capture of its own.
  // CMO sidecars always capture from DS -- including CBO.inval, whose Evict
  // carries no data but whose ReleaseBuf snapshot lets a racing same-line
  // snoop be answered with the pre-invalidation contents (nestability is
  // what breaks the CMO <-> snoop deadlock cycle).
  incomingReplaceReq.captureFromDS := (replRespNeedsReplace &&
      !io.fromMainPipe.grantInstallDone.valid) || cmoSidecarAlloc
  incomingReplaceReq.task := 0.U.asTypeOf(new TaskBundle)
  incomingReplaceReq.task.set := incomingVictim.set
  incomingReplaceReq.task.tag := incomingVictim.tag
  val incomingReplaceInfo = WireInit(0.U.asTypeOf(new ReplaceMSHRInfo))
  incomingReplaceInfo.valid := replaceNeedsAlloc
  incomingReplaceInfo.parentId := replaceParentId
  incomingReplaceInfo.parentEpoch := incomingReplaceReq.parentEpoch
  incomingReplaceInfo.parentAttached := replaceNeedsAlloc && !orphanReleaseData
  incomingReplaceInfo.set := incomingVictim.set
  incomingReplaceInfo.tag := incomingVictim.tag
  incomingReplaceInfo.way := incomingVictim.way
  incomingReplaceInfo.meta := incomingVictim.meta
  incomingReplaceInfo.metaTag := incomingVictim.tag
  incomingReplaceInfo.needRelease := true.B
  incomingReplaceInfo.replaceData := incomingVictim.meta.dirty

  // Normal and replace contexts use different buffers and may share a local
  // index. Snoop and replace are mutually exclusive because both own
  // ReleaseBuf[i]. A ReplacerResult consumes its parent reservation; nested
  // C/orphan paths use an unreserved S/R slot.
  val selectedReplaceSlotOH = Mux(
    replRespNeedsReplace,
    replRespReservationOH,
    PriorityEncoderOH(replaceSlotIdle)
  )

  (0 until mshrsAll).foreach { i =>
    val incomingForParent = replaceNeedsAlloc && !orphanReleaseData && replaceParentOH(i)
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
      Mux(replaceNeedsAlloc && !orphanReleaseData && replaceParentOH(parent), selectedReplaceSlotOH, 0.U)
  }
  // Nested C and a returning refill lookup are different tasks in the shared
  // S3 pipeline. Only nested/Cmo admission can create a new nested-data target;
  // an unrelated ReplacerResult must not enter that physical-slot mux.
  val replaceNestedAllocParentOH = Mux(nestedReleaseNeedsReplace, nestedReleaseParentOH,
    Mux(cmoSidecarAlloc, cmoSidecarParentOH, 0.U(mshrsAll.W)))
  val replaceNestedSlotOH = PriorityEncoderOH(replaceSlotIdle)
  val replaceNestedTargetMatches = Seq.tabulate(mshrsAll) { parent =>
    replaceParentMatches(parent) |
      Mux(replaceNestedAllocParentOH(parent), replaceNestedSlotOH, 0.U(mshrsAll.W))
  }
  assert(!(replResponseApplied.valid && io.nestedwb.c_set_dirty),
    "refill result and nested C must correspond to distinct shared-S3 tasks")
  when(nestedReleaseNeedsReplace || cmoSidecarAlloc) {
    assert(replaceNestedSlotOH === selectedReplaceSlotOH,
      "nested payload target must equal the actual allocated physical slot")
  }
  // A parent that still owns a child ReplaceMSHR must not allocate a second
  // one: the child's victim data / ReleaseBuf is still live, and the parent
  // only re-queries the replacer after the child's done pulse. Guard both the
  // allocation and the replacer response.
  val parentAlreadyHasChild = Mux1H(replaceParentOH, replaceActiveForParent)
  // Availability needs no priority encoding; the physical onehot remains
  // unchanged for ownership and buffer routing.
  val replaceSlotAvailable = Mux(replRespNeedsReplace,
    replRespReservationOH.orR, replaceSlotIdle.orR)
  assert(replaceSlotAvailable === selectedReplaceSlotOH.orR,
    "replacement availability must equal the selected-slot nonzero test")
  val replaceAllocValid = replaceNeedsAlloc && replaceSlotAvailable &&
    (orphanReleaseData || !parentAlreadyHasChild)
  assert(
    !(replaceNeedsAlloc && !orphanReleaseData && parentAlreadyHasChild),
    "replacer response for a parent that already owns a child ReplaceMSHR"
  )
  // New replacements cannot supply a nested snoop yet: incomingReplaceInfo
  // has neither probeDone nor releaseBufReady. Advertise their addresses
  // independently of slot selection, while keeping registered sidecars
  // indexed as their ReleaseBuf owners.
  //
  // Register each complete pre-acceptance origin independently. The short
  // local execution slot excludes B capture while these S3 allocations
  // apply, so the next capture sees both their registered fence and owner.
  // The accepted allocation still owns actual ReplaceMSHR/ReleaseBuf routing.
  //
  // Lifetime: every raw origin is either a one-cycle pulse (replResp,
  // nested C, orphan ReleaseData) or, for the CMO, already qualified by
  // replaceSlotIdle and the reservation fences, so it coincides with an
  // accepted allocation in any legal cycle. RXSNP additionally yields this
  // block to a nest-ready sidecar, so an origin that outlives its allocation
  // can never block that sidecar's own nested snoop.
  val replaceAllocSnoopSources = Seq(
    (nestedReleaseNeedsReplace, nestedVictim.set, nestedVictim.tag),
    (replRespNeedsReplace, replResponseApplied.bits.set, replResponseApplied.bits.tag),
    (cmoSidecarAlloc,
      Mux1H(cmoSidecarParentOH, refillMshrs.map(_.io.msInfo.bits.set)),
      Mux1H(cmoSidecarParentOH, refillMshrs.map(_.io.msInfo.bits.reqTag))),
    (orphanReleaseData, io.nestedwb.set, io.nestedwb.tag)
  )
  val replaceAllocOriginRequested = ReplacementAllocAddress.requestedOrigins(
    replaceAllocSnoopSources.map(_._1))
  replaceAllocSnoopSources.zipWithIndex.foreach { case ((_, set, tag), i) =>
    // The local transition slot excludes B capture during S3 apply. Register
    // the complete origin at that edge, so the next capture sees registered
    // ownership and address together; never delay only its safety-valid bit.
    io.replaceAllocForSnoop(i).valid := RegNext(replaceAllocOriginRequested(i), false.B)
    io.replaceAllocForSnoop(i).bits.set := RegEnable(set, replaceAllocOriginRequested(i))
    io.replaceAllocForSnoop(i).bits.tag := RegEnable(tag, replaceAllocOriginRequested(i))
    // A raw origin may be rejected by capacity or parent ownership; only an
    // accepted one must equal the victim actually allocated this cycle.
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

  // Normal contexts own RefillBuf and are capacity-independent from the
  // S/R-mutual ReleaseBuf pool. Pipeline occupancy is conservatively counted
  // in both pools so a not-yet-registered allocation cannot be selected twice.
  val pipeContextCount = PopCount(Cat(io.pipeStatusVec.map(_.valid)))
  val normalContextCount = PopCount(Cat(refillMshrs.map(_.io.status.valid)))
  val srContextCount = PopCount(Cat((0 until mshrsAll).map { i =>
    snoopMshrs(i).io.status.valid || replaceMshrs(i).io.info.valid ||
      replaceReservedSlotOH(i) || snoopServiceSlotOH(i)
  }))
  // Capacity is promised at admission, not borrowed combinationally from
  // this cycle's Directory result or context retirement. Include S1 and
  // autonomous reservations as well as all in-flight apply-stage work.
  def allocationCredit(used: UInt): UInt = {
    val credit = RegInit(0.U(log2Ceil(mshrsAll + 1).W))
    credit := Mux(used >= mshrsAll.U, 0.U, mshrsAll.U - used)
    credit
  }
  val normalCredit = allocationCredit(normalContextCount +& pipeContextCount +&
    io.contextReservation.asUInt)
  val snoopCredit = allocationCredit(srContextCount +& pipeContextCount +&
    io.contextReservation.asUInt +& replaceReserveGrant.asUInt +&
    cmoSidecarAlloc.asUInt +& io.bServiceStart.valid.asUInt)
  val normalMshrFull = normalCredit === 0.U
  val snoopMshrFull = snoopCredit === 0.U
  val mshrFull = snoopMshrFull
  val a_mshrFull = normalMshrFull
  val mshrSelector = Module(new MSHRSelector())
  // An empty selector's priority default is not an allocation. Never let
  // those bits consume or overwrite a context, even if an invariant fails.
  val selectedMSHROH = Mux(mshrSelector.io.out.valid,
    mshrSelector.io.out.bits, 0.U(mshrsAll.W))

  io.l2Miss := Cat(refillMshrs.map { m =>
    m.io.status.valid && m.io.status.bits.channel(0) && (
      m.io.status.bits.reqSource === MemReqSource.CPULoadData.id.U ||
      m.io.status.bits.reqSource === MemReqSource.CPUStoreData.id.U
    )
  }).orR
  val allocIsSnoop = io.fromMainPipe.mshr_alloc_s3.bits.task.fromB
  // RequestArb gives B priority over A only in the B handshake cycle.  Keep
  // the exact toN address reserved across B's S2/S3 allocation window, then
  // transfer the reservation to the allocated SnoopMSHR until its final CHI
  // response leaves TXDAT/TXRSP.
  val pendingToNValid = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val pendingToNSet = Reg(Vec(mshrsAll, UInt(setBits.W)))
  val pendingToNTag = Reg(Vec(mshrsAll, UInt(tagBits.W)))
  val pendingToNHasSnoopContext = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val pendingToNDirectPath = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val bToNFire = io.bServiceStart.valid && isSnpToN(io.bServiceStart.bits.chiOpcode.get)
  val pendingToNFreeOH = PriorityEncoderOH(VecInit(pendingToNValid.map(x => !x)).asUInt)
  val pendingToNFull = !pendingToNFreeOH.orR
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
    snoopMshrs.map { snoop =>
      snoop.io.msInfo.valid && snoop.io.msInfo.bits.blocksSnoop && snoop.io.msInfo.bits.willFree &&
        snoop.io.msInfo.bits.set === pendingToNSet(i) && snoop.io.msInfo.bits.reqTag === pendingToNTag(i)
    }.reduce(_ || _)
  })
  val pendingToNDirectRespDone = VecInit((0 until mshrsAll).map { i =>
    Seq(io.txdatDone, io.txrspDone).map { done =>
      done.valid && done.bits.fromB && !done.bits.mshrTask &&
        done.bits.set === pendingToNSet(i) && done.bits.tag === pendingToNTag(i)
    }.reduce(_ || _)
  })
  (0 until mshrsAll).foreach { i =>
    when(pendingToNValid(i)) {
      when(pendingToNAllocated(i)) {
        pendingToNHasSnoopContext(i) := true.B
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
  // S/R are mutually exclusive owners of ReleaseBuf[i]. Normal uses only
  // RefillBuf[i], so N+R is legal and normal allocation must not observe R[i].
  // CMO retains its existing N+S restriction.
  val allocIsCmo = io.fromMainPipe.mshr_alloc_s3.bits.task.cmoTask
  // Match the normal context that holds a post-Grant ReleaseData for this B.
  // Used only for pairing / RefillBuf[N]->ReleaseBuf[S] copy source selection.
  // Allocation no longer pins the snoop onto that same physical index: the
  // snoop keeps its own ReleaseBuf and consumes a mirrored/copied payload.
  val postGrantReleaseSnoopTargetOH = VecInit(refillMshrs.map { refill =>
    allocIsSnoop && refill.io.status.valid && refill.io.postGrantReleaseHold &&
      refill.io.status.bits.set === io.fromMainPipe.mshr_alloc_s3.bits.task.set &&
      refill.io.status.bits.reqTag === io.fromMainPipe.mshr_alloc_s3.bits.task.tag
  }).asUInt
  when(allocIsSnoop) {
    assert(PopCount(postGrantReleaseSnoopTargetOH) <= 1.U,
      "one B snoop must match at most one post-Grant ReleaseData holder")
  }
  // Transfer the exact physical slot reserved before B execution. It has
  // remained unavailable to replacement for the entire lookup/apply window.
  mshrSelector.io.idle := refillMshrs.zip(snoopMshrs).zipWithIndex.map {
    case ((refill, snoop), i) =>
      Mux(
        allocIsSnoop,
        snoopServiceSlotOH(i) && !snoop.io.status.valid && !replaceMshrs(i).io.info.valid,
        !refill.io.status.valid &&
          (!allocIsCmo || !snoop.io.status.valid)
      )
  }
  io.toMainPipe.mshr_alloc_ptr := OHToUInt(selectedMSHROH)
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
    assert(mshrSelector.io.out.valid, "MainPipe requested an MSHR allocation without an eligible physical slot")
  }
  val activeSnoopOH = VecInit(snoopMshrs.map(_.io.status.valid)).asUInt
  val activeReplaceOH = VecInit(replaceMshrs.map(_.io.info.valid)).asUInt
  assert(!(activeSnoopOH & activeReplaceOH).orR,
    "SnoopMSHR and ReplaceMSHR must not own the same ReleaseBuf slot")
  // A normal allocation may legally share its local index with a newly
  // allocated ReplaceMSHR because their RefillBuf/ReleaseBuf ownership is
  // disjoint. A snoop allocation is excluded above from the selected R slot.

  // SinkC carries both ProbeAck( Data ) and proactive Release( Data ).  A
  // proactive release still updates a matching normal MSHR's client state,
  // while only a probe response may complete a snoop or replacement context.
  // Otherwise a ReleaseData can be mistaken for a snoop ProbeAck, or skipped
  // altogether and leave a stale client that will never answer a later Probe.
  val sinkCProbeResponse = io.resps.sinkC.respInfo.opcode === ProbeAck ||
    io.resps.sinkC.respInfo.opcode === ProbeAckData
  val sinkCRelease = io.resps.sinkC.respInfo.opcode === Release ||
    io.resps.sinkC.respInfo.opcode === ReleaseData

  // Only a sidecar currently awaiting its own ProbeAck may consume a probe
  // response by address. A stale replace context with a coincidental set/tag
  // match must not absorb another probe's data.
  val replaceProbeSinkCMatch = VecInit(replaceMshrs.map { m =>
    sinkCProbeResponse && m.io.info.bits.awaitingProbeAck && io.resps.sinkC.set === m.io.info.bits.set &&
    io.resps.sinkC.tag === m.io.info.bits.tag
  })
  // A ReleaseData takes SinkC's task path rather than resps.sinkC. It
  // supersedes any earlier ProbeAckData for the same delegated victim.
  val replaceReleaseDataMatch = VecInit(replaceMshrs.map { m =>
    io.sinkCReleaseData.valid && m.io.info.valid &&
      (!m.io.info.bits.releaseAck || m.io.acceptsLateReleaseData) &&
    io.sinkCReleaseData.bits.set === m.io.info.bits.set &&
    io.sinkCReleaseData.bits.tag === m.io.info.bits.tag
  })
  val replaceSinkCMatch = replaceProbeSinkCMatch
  // The directory can select a dirty snoop-owned victim in the final
  // ProbeAckData cycle.  The new sidecar is not awaiting a probe yet, so it
  // cannot match replaceProbeSinkCMatch.  Mirror this accepted data into the
  // selected slot and mark it captured before a later data-less ProbeAck can
  // cause stale DataStorage contents to be copied back.
  val incomingReplaceProbeAckDataCapture = io.resps.sinkC.valid &&
    io.resps.sinkC.respInfo.last &&
    io.resps.sinkC.respInfo.opcode === ProbeAckData &&
    replRespNeedsReplaceCapture && replRespReservationOHCapture.orR &&
    io.resps.sinkC.set === io.replResp.bits.set &&
    io.resps.sinkC.tag === io.replResp.bits.tag
  // A sidecar can already be live while a snoop owns the ProbeAckData for the
  // same line. The sidecar may not have sent its own Probe yet, so its direct
  // SinkC match is deliberately false. Mirror the authoritative data before
  // the later data-less ProbeAck would otherwise fall back to stale DS data.
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
  when(activeReplaceProbeAckDataCapture) {
    assert(PopCount(activeReplaceProbeAckDataOH) === 1.U, "ProbeAckData matches multiple active ReplaceMSHRs")
  }
  val replaceDelegatedForParent = Seq.tabulate(mshrsAll) { parent =>
    // Keep arbitration intent independent of replaceAllocValid: feeding the
    // accepted grant back into replReadWanted creates a reservation cycle.
    replaceActiveForParent(parent) || replaceNeedsAlloc && replaceParentOH(parent)
  }

  // A retry has no committed ReplacerResult, so a ReleaseData for the stale
  // candidate may proceed. Once SinkC accepts it, hold the matching normal
  // MSHR's next replacer read until that C task reaches MainPipe S3.
  val cReleaseReplacerFence = RegInit(0.U(mshrsAll.W))
  val cReleaseReplacerFenceSet = VecInit(refillMshrs.map { refill =>
    io.sinkCReleaseData.valid && refill.io.status.valid &&
    !refill.io.msInfo.bits.w_replResp &&
    refill.io.status.bits.set === io.sinkCReleaseData.bits.set &&
    refill.io.status.bits.metaTag === io.sinkCReleaseData.bits.tag
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

  /* SinkC(release) search MSHR with PA */
  val resp_sinkC_match_vec = mshrs.zipWithIndex.map {
    case (mshr, contextIdx) =>
      val physicalIdx = contextIdx / 2
      val status = mshr.io.status.bits
      // SnoopMSHR never owns a replacement victim: its status bundle leaves
      // needsRepl at the default false value and always matches SinkC by the
      // request tag. Keep the normal-MSHR victim-tag choice only on the even
      // context to remove an unnecessary mux from the snoop req_tag cone.
      val tag = if (contextIdx % 2 == 0) {
        Mux(status.needsRepl, status.metaTag, status.reqTag)
      } else {
        status.reqTag
      }
      val sinkCMatchesContext = if (contextIdx % 2 == 0) {
        sinkCProbeResponse || sinkCRelease
      } else {
        sinkCProbeResponse
      }
      // A C ProbeAck has no transaction ID. It may be matched by address only
      // after this exact context has put its Probe on SourceB; otherwise a
      // same-line request waiting in the arbiter can steal another context's
      // response. Proactive Release remains routable to a live normal context
      // before it emits a Probe.
      val probeResponseIssued = !sinkCProbeResponse || mshr.io.probeIssued
      sinkCMatchesContext && probeResponseIssued && mshr.io.status.valid && status.w_c_resp &&
      io.resps.sinkC.set === status.set && io.resps.sinkC.tag === tag &&
      // A ReplaceMSHR is delegated only by the normal context.  Its parent
      // association must not mask an independent snoop context sharing that
      // physical index, or the snoop's ProbeAck becomes an orphan response.
      (!(contextIdx % 2 == 0).B || !replaceDelegatedForParent(physicalIdx))
  }
  val respSinkCMatchByEntry = Seq.tabulate(mshrsAll) { i =>
    resp_sinkC_match_vec(2 * i) || resp_sinkC_match_vec(2 * i + 1)
  }
  val sinkCContextMatches = resp_sinkC_match_vec ++ replaceProbeSinkCMatch.toSeq
  when(io.resps.sinkC.valid && sinkCProbeResponse) {
    assert(
      PopCount(Cat(sinkCContextMatches)) === 1.U,
      "every SinkC ProbeAck must match exactly one live normal, snoop, or replace context"
    )
  }

  // A nested ReleaseData can belong to either half of a physical entry.  The
  // normal half redirects an already-delegated victim to ReplaceMSHR; the
  // snoop half keeps its own physical ReleaseBuf entry for SnpRespData. Both
  // halves may legitimately match the same line and therefore need two
  // independent ReleaseBuf writes.
  val nestedwbNormalActive = refillMshrs.zipWithIndex.map { case (refill, i) =>
    val incomingCmoCapture = cmoSidecarAlloc && cmoSidecarParentOH(i) &&
      io.nestedwb.c_set_dirty && refill.io.status.bits.set === io.nestedwb.set &&
      refill.io.status.bits.reqTag === io.nestedwb.tag
    refill.io.nestedwbData || incomingCmoCapture
  }
  val nestedwbSnoopActive = snoopMshrs.map(_.io.nestedwbData)
  refillMshrs.zip(snoopMshrs).zipWithIndex.foreach {
    case ((refill, snoop), i) =>
      when(nestedwbNormalActive(i) && replaceNestedTargetMatches(i).orR) {
        val targetInfo = Mux1H(replaceNestedTargetMatches(i), replaceMshrs.map(_.io.info.bits))
        // During the allocation cycle the target's registers are not valid yet;
        // its address is nevertheless exactly incomingReplaceReq.
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
      nestedwbNormalActive.zipWithIndex.map {
        case (active, parentId) =>
          active && replaceNestedTargetMatches(parentId)(replaceId)
      }.reduce(_ || _)
  }

  /* Port connection of physical MSHR entries and their two contexts. */
  val rxdatStaleSlotOH = WireDefault(VecInit(Seq.fill(mshrsAll)(false.B)))
  refillMshrs.zip(snoopMshrs).zipWithIndex.foreach {
    case ((refill, snoop), i) =>
      Seq[MSHRContextBase](refill, snoop).zipWithIndex.foreach {
        case (m, context) =>
          m.io.id := i.U
          m.io.alloc.valid := selectedMSHROH(i) && io.fromMainPipe.mshr_alloc_s3.valid &&
          (allocIsSnoop === context.B)
          m.io.alloc.bits := io.fromMainPipe.mshr_alloc_s3.bits
          m.io.alloc.bits.task.isKeyword
            .foreach(_ := io.fromMainPipe.mshr_alloc_s3.bits.task.isKeyword.getOrElse(false.B))
          m.io.resps.sinkC.valid := io.resps.sinkC.valid && resp_sinkC_match_vec(2 * i + context)
          m.io.resps.sinkC.bits := io.resps.sinkC.respInfo
          m.io.nestedwb := io.nestedwb
          m.io.aMergeTask.valid := io.aMergeTask.valid && io.aMergeTask.bits.idOH(i) && !context.B
          m.io.aMergeTask.bits := io.aMergeTask.bits.task
      }

      // A CompData is routed by TxnID, which is this slot's index and is reused
      // as soon as the context retires.  A response to the previous tenant can
      // therefore still be in flight and would silently complete the new
      // context's read (tl-test: uncached Get "Data mismatch", the returned
      // data belonging to the address the slot read for its previous tenant).
      // Once this context has accepted its own first beat it latched that
      // response's DBID; a later beat must carry the same one.
      val rxdatForThisSlot = io.resps.rxdat.valid && io.resps.rxdat.mshrId === i.U
      val rxdatDBIDMismatch = refill.io.readDBID.valid &&
        io.resps.rxdat.respInfo.dbID.get =/= refill.io.readDBID.bits
      // The TxnID high bits carry the epoch of the read that was issued.  A
      // response whose epoch is not the one this context sent belongs to an
      // earlier tenant and must be dropped before it can complete the read.
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
      when(replRespOwnerConflict || replRespParentConflict || replRespNoReplaceSlot ||
        replRespCmoConflict) {
        replRespForRefill.retry := true.B
      }
      refill.io.replResp.bits := replRespForRefill
      // The grant pass merged the refill install at S3: the later refillOnly
      // commit of this parent degenerates to the RefillBuf->DS write alone.
      refill.io.grantInstallDone :=
        io.fromMainPipe.grantInstallDone.valid && io.fromMainPipe.grantInstallDone.bits === i.U
      // The direct TXDAT path is ReplaceMSHR-only.
      refill.io.tasks.txdat.ready := false.B
      val parentReplaceMatch = replaceParentMatches(i)
      val parentReplaceInfo = WireInit(0.U.asTypeOf(new ReplaceMSHRInfo))
      when(parentReplaceMatch.orR) {
        parentReplaceInfo := Mux1H(parentReplaceMatch, replaceMshrs.map(_.io.info.bits))
      }
      // Feed the parent the registered child ownership, not the allocation
      // intent: the latter closes the replResp -> replaceActive -> willFree ->
      // RXSNP task-valid cone.  MSHR adds a narrow same-cycle fence on
      // replResp.valid/mshrId for the allocation cycle, so probe/replRead/
      // cbwrdata still see the child on the cycle it is created.
      refill.io.replaceActive := replaceActiveForParent(i)
      refill.io.replaceLive := replaceActiveForParent(i)
      // A full pool (or a same-cycle reservation taking its last free slot)
      // can reject an intent. The parent must keep requesting until accepted.
      refill.io.replaceAccepted := replaceActiveForParent(i) ||
        replaceAllocValid && !orphanReleaseData && replaceParentOH(i)
      // A nested B request reaches MainPipe S3 two cycles before its
      // SnoopMSHR becomes valid. Treat the S1 B candidate as a transient
      // snoop owner until allocation has caught up, so SourceD is deferred
      // until the snoop ordering point is known.
      // A normal MSHR owns its request line only.  `metaTag` is a ReplaceMSHR
      // victim while `needsRepl` is set, so a B.toN for that victim must stay
      // on the replacement/snoop path and must not cancel this normal refill.
      // Use the raw B candidate, not the already-gated bValid: bValid depends
      // on MSHRCtl's blockB_s1, which itself depends on the B's toN status, so
      // feeding it back here closes a long MSHRCtl<->RequestArb cone.  A B
      // candidate blocked only by the snoop/replace pool defers this grant
      // conservatively; that pool drains independently of this normal's
      // commit, so the extra deferral cannot deadlock.
      // snoopCandidateSeen is the registered bCandidateValid.  blockB_s1
      // forbids a B from firing on its first candidate cycle, so every fire
      // cycle has snoopCandidateSeen set and the address compare below still
      // catches it; a blocked candidate only adds a conservative deferral.
      val pendingSnoopForRefill = snoopCandidateSeen &&
        refill.io.status.valid &&
        refill.io.status.bits.set === io.fromReqArb.status_s1.b_set &&
        refill.io.status.bits.reqTag === io.fromReqArb.status_s1.b_tag
      // A matching B candidate defers only the irrevocable L1-visible Grant.
      // It must not revoke or replay the normal MSHR's one lower CHI read:
      // the HN serializes that read response behind the snoop, while RXSNP
      // blocks later snoops during the Refill/Grant critical window.
      val nestedBToNForRefill = io.nestedwb.b_toN.getOrElse(false.B) &&
        refill.io.status.valid &&
        refill.io.status.bits.set === io.nestedwb.set &&
        refill.io.status.bits.reqTag === io.nestedwb.tag
      // A voluntary dirty ReleaseData can precede the ProbeAck for a
      // Get-on-TRUNK request.  In that order the release payload, rather
      // than the old DS fallback, is the data that must reach AccessAckData.
      // SinkC exposes it on its first accepted beat so Grant cannot pass the
      // ProbeAck/ReleaseData crossing before the payload is captured.
      val releaseDataBeforeProbeAck = io.sinkCReleaseData.valid &&
        refill.io.status.valid && !refill.io.msInfo.bits.w_rprobeacklast &&
        refill.io.status.bits.set === io.sinkCReleaseData.bits.set &&
        refill.io.status.bits.reqTag === io.sinkCReleaseData.bits.tag
      val postGrantReleaseData = io.sinkCReleaseData.valid &&
        refill.io.postGrantReleaseWindow &&
        refill.io.status.bits.set === io.sinkCReleaseData.bits.set &&
        refill.io.status.bits.reqTag === io.sinkCReleaseData.bits.tag
      val releaseDataDirectWrite = io.fromMainPipe.releaseDataDirectWrite.valid &&
        refill.io.status.valid &&
        refill.io.status.bits.set === io.fromMainPipe.releaseDataDirectWrite.bits.set &&
        refill.io.status.bits.reqTag === io.fromMainPipe.releaseDataDirectWrite.bits.tag
      // A detached victim can still be held by a snoop in a different slot.
      // Its physical DS way must remain reserved even when the normal request
      // has a different tag. Include the hold before preparing a refill, and
      // let deferGrant's lease invalidation revoke an older unissued proposal.
      val snoopHoldsRefillWay = snoopMshrs.map { candidate =>
        candidate.io.msInfo.valid && candidate.io.msInfo.bits.blockRefill &&
          candidate.io.msInfo.bits.set === refill.io.status.bits.set &&
          candidate.io.msInfo.bits.way === refill.io.msInfo.bits.way
      }.reduce(_ || _)
      refill.io.deferGrant := snoopMshrs(i).io.status.valid ||
        pendingSnoopForRefill || nestedBToNForRefill || snoopHoldsRefillWay || sinkCRefillPending(i)
      refill.io.releaseDataBeforeProbeAck := releaseDataBeforeProbeAck
      refill.io.postGrantReleaseData := postGrantReleaseData
      refill.io.releaseDataDirectWrite := releaseDataDirectWrite
      // The matching snoop can be in a different temporary allocation phase
      // in the cycle before its `status` register becomes valid. Include that
      // allocation pulse so the normal cannot retire or commit around it.
      // The same SnoopMSHR req_set/req_tag match feeds active, ToN, ToB, and
      // ToClean. Compare the packed line address once per candidate so the
      // internal SnoopMSHR request fields do not rebuild four equality cones.
      val refillLineKey = Cat(refill.io.status.bits.reqTag, refill.io.status.bits.set)
      val postGrantReleaseSnoopLineMatch = snoopMshrs.map { candidate =>
        candidate.io.status.valid &&
          Cat(candidate.io.status.bits.reqTag, candidate.io.status.bits.set) === refillLineKey
      }
      val postGrantReleaseSnoopActive = postGrantReleaseSnoopLineMatch.reduce(_ || _)
      // Every selected snoop receives the same task. The allocation request
      // therefore protects this line before its physical slot is selected;
      // OR-ing per-slot alloc.valid would unnecessarily feed the replacer /
      // reservation / selector chain back into the parent's retirement path.
      // The allocation assertion above guarantees that the request has a slot.
      val postGrantReleaseSnoopAllocating = snoopAllocRequest &&
        io.fromMainPipe.mshr_alloc_s3.bits.task.set === refill.io.status.bits.set &&
        io.fromMainPipe.mshr_alloc_s3.bits.task.tag === refill.io.status.bits.reqTag
      // Reserve completion storage at the snoop's issue edge. The live snoop
      // pins its paired normal before capture; the mailbox extends that fence
      // through the following owner-apply edge, even if transport finishes.
      val postGrantCompletion = RegInit(0.U.asTypeOf(Valid(new Bundle {
        val targetGeneration = UInt(8.W)
        val source = UInt(mshrBits.W)
        val sourceToken = new MainpipeLeaseToken
        val outcome = UInt(2.W) // 1: ToN, 2: ToB, 3: clean
      })))
      refill.io.bufferWritePending := sinkCRefillPending(i) || sinkCReleaseHold(i)
      refill.io.pairLifetimeHold := snoopPairs.zipWithIndex.map { case (pair, s) =>
        snoopPairNormalValid(s) && pair.bits.normal.bits.id === i.U
      }.reduce(_ || _)
      refill.io.postGrantReleaseSnoopActive := refill.io.postGrantReleaseHold &&
        (postGrantReleaseSnoopActive || postGrantReleaseSnoopAllocating || postGrantCompletion.valid)
      val postGrantReleaseSnoopToN = snoopMshrs.zip(postGrantReleaseSnoopLineMatch).map {
        case (candidate, sameLine) => candidate.io.postGrantReleaseResponseToN && sameLine
      }.reduce(_ || _)
      val postGrantReleaseSnoopToB = snoopMshrs.zip(postGrantReleaseSnoopLineMatch).map {
        case (candidate, sameLine) => candidate.io.postGrantReleaseResponseToB && sameLine
      }.reduce(_ || _)
      val postGrantReleaseSnoopToClean = snoopMshrs.zip(postGrantReleaseSnoopLineMatch).map {
        case (candidate, sameLine) => candidate.io.postGrantReleaseResponseToClean && sameLine
      }.reduce(_ || _)
      postGrantCompletion.valid := postGrantReleaseSnoopToN ||
        postGrantReleaseSnoopToB || postGrantReleaseSnoopToClean
      when(postGrantReleaseSnoopToN || postGrantReleaseSnoopToB || postGrantReleaseSnoopToClean) {
        val sourceOH = VecInit(snoopMshrs.zip(postGrantReleaseSnoopLineMatch).map {
          case (candidate, sameLine) => sameLine &&
            (candidate.io.postGrantReleaseResponseToN || candidate.io.postGrantReleaseResponseToB ||
              candidate.io.postGrantReleaseResponseToClean)
        }).asUInt
        assert(PopCount(sourceOH) === 1.U, "one snoop issue must own each paired completion")
        postGrantCompletion.bits.targetGeneration := mshrTaskGenerations(0)(i)
        postGrantCompletion.bits.source := OHToUInt(sourceOH)
        postGrantCompletion.bits.sourceToken := Mux1H(sourceOH,
          snoopMshrs.map(_.io.tasks.preparedLease.held.bits))
        postGrantCompletion.bits.outcome := Mux(postGrantReleaseSnoopToN, 1.U,
          Mux(postGrantReleaseSnoopToB, 2.U, 3.U))
      }
      val completionSourceOH = UIntToOH(postGrantCompletion.bits.source, mshrsAll)
      val completionSourceLease = Mux1H(completionSourceOH,
        snoopMshrs.map(_.io.tasks.preparedLease.held))
      val completionSourceVersionMatches = completionSourceLease.valid &&
        completionSourceLease.bits.asUInt === postGrantCompletion.bits.sourceToken.asUInt
      val applyPostGrantCompletion = postGrantCompletion.valid &&
        refill.io.status.valid && postGrantCompletion.bits.targetGeneration === mshrTaskGenerations(0)(i) &&
        completionSourceVersionMatches
      when(postGrantCompletion.valid) {
        assert(applyPostGrantCompletion,
          "registered snoop completion must retain both source and paired normal versions")
      }
      refill.io.postGrantReleaseSnoopToN := applyPostGrantCompletion && postGrantCompletion.bits.outcome === 1.U
      refill.io.postGrantReleaseSnoopToB := applyPostGrantCompletion && postGrantCompletion.bits.outcome === 2.U
      refill.io.postGrantReleaseSnoopToClean := applyPostGrantCompletion && postGrantCompletion.bits.outcome === 3.U
      // Directory's ReplacerResult cannot be backpressured. Do not let this
      // normal context issue a lookup until it owns an S/R-mutual reservation
      // for the resulting ReplaceMSHR and ReleaseBuf slot.
      refill.io.blockReplRead := cReleaseReplacerFence(i) || cReleaseReplacerFenceSet(i) ||
        !replaceReserveAllowsRead(i)
      refill.io.replaceDetached := parentReplaceMatch.orR && parentReplaceInfo.detached
      refill.io.replaceRefillReady := parentReplaceMatch.orR && parentReplaceInfo.refillReady
      // CMO completion: a child ReplaceMSHR (cmoClean/Flush/Inval mode) pulses
      // done when its lower-level writeback finished; the parent normal CMO
      // context proceeds with its control request / CBOAck.
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
      refill.io.grantSent := io.fromMainPipe.grantSent.valid &&
      io.fromMainPipe.grantSent.bits === i.U
      snoop.io.replaceActive := false.B
      snoop.io.replaceLive := false.B
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
      snoop.io.grantInstallDone := false.B
      // Only ProbeAckData provenance may be reused by the paired snoop. A
      // normal RXDAT remains owned by the normal request until RXSNP has
      // released its Refill/Grant critical window.
      val pairedProbeAckDataMirrored = io.resps.sinkC.valid && io.resps.sinkC.respInfo.last &&
        io.resps.sinkC.respInfo.opcode === ProbeAckData &&
        resp_sinkC_match_vec(2 * i + 1) && refill.io.status.valid &&
        refill.io.status.bits.set === snoop.io.status.bits.set &&
        refill.io.status.bits.reqTag === snoop.io.status.bits.reqTag
      val pair = snoopPairs(i)
      val pairNormalValid = snoopPairNormalValid(i)
      val releaseStartsForSnoop = io.sinkCReleaseData.valid && snoop.io.status.valid &&
        io.sinkCReleaseData.bits.set === snoop.io.status.bits.set &&
        io.sinkCReleaseData.bits.tag === snoop.io.status.bits.reqTag
      val pairedRefillWritePending = pairNormalValid &&
        (UIntToOH(pair.bits.normal.bits.id, mshrsAll) & sinkCRefillPending).orR
      snoop.io.pairSearchPending := !snoopPairKnown(i) || releaseStartsForSnoop || pairedRefillWritePending
      snoop.io.normalRefillPayloadFromProbeAck :=
        pairedProbeAckDataMirrored || (pairNormalValid && pair.bits.probePayload)
      snoop.io.normalRefillPostGrantReleasePending := pairNormalValid && pair.bits.postGrantPending
      snoop.io.normalRefillPayloadFromPostGrantRelease :=
        pairNormalValid && pair.bits.postGrantPending && pair.bits.payloadReady
      snoop.io.pairedNormalId.valid := pairNormalValid && pair.bits.postGrantPending
      snoop.io.pairedNormalId.bits := pair.bits.normal.bits.id
      snoop.io.pairLifetimeHold := false.B
      snoop.io.bufferWritePending := false.B
      // Capture the allocation address match at the same edge as req/state.
      // Do not feed alloc.valid/slot-selector into an active SourceB gate.
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
      // MainPipe S5 pulses refillToReleaseCopyWrite with the destination snoop id.
      snoop.io.nestedReleaseWritePending := nestedReleasePending(0)(i)
      snoop.io.nestedReleaseWriteDone := nestedReleaseDone(0)(i)
      replaceMshrs(i).io.nestedReleaseWritePending := nestedReleasePending(1)(i)
      snoop.io.releaseBufCopyDone :=
        io.fromMainPipe.refillToReleaseCopyWrite.valid &&
          io.fromMainPipe.refillToReleaseCopyWrite.bits === i.U
      replaceMshrs(i).io.id := i.U
      replaceMshrs(i).io.alloc.valid := replaceAllocValid && selectedReplaceSlotOH(i)
      replaceMshrs(i).io.alloc.bits := incomingReplaceReq
      replaceMshrs(i).io.sinkC.valid := io.resps.sinkC.valid && replaceSinkCMatch(i)
      replaceMshrs(i).io.sinkC.bits := io.resps.sinkC.respInfo
      replaceMshrs(i).io.releaseDataSeen := replaceReleaseDataMatch(i)
      val replaceProbeAckDataCapture = io.resps.sinkC.valid && io.resps.sinkC.respInfo.last &&
        io.resps.sinkC.respInfo.opcode === ProbeAckData && replaceProbeSinkCMatch(i)
      val mirroredReplaceProbeAckDataForSlot =
        mirroredReplaceProbeAckDataDoneOH(i)
      // Client-less victims are captured from DS by the replace capture task;
      // MainPipe pulses replaceCaptureWrite with the replace slot id.
      val replaceCaptureMatch = io.fromMainPipe.replaceCaptureWrite.valid &&
        io.fromMainPipe.replaceCaptureWrite.bits === i.U
      val orphanReplaceCapture = orphanReleaseTargetsReplace && orphanReplaceTargetOH(i)
      replaceMshrs(i).io.victimDataCaptured :=
        replaceProbeAckDataCapture || nestedReleaseDone(1)(i) || replaceCaptureMatch ||
        mirroredReplaceProbeAckDataForSlot
      // A DS fallback capture only snapshots an already-clean victim for a
      // possible nested snoop. It must not promote the sidecar's lower-level
      // Evict into WriteBackFull. SinkC/ReleaseData captures may carry newer
      // data, so they retain the conservative writeback behavior.
      replaceMshrs(i).io.victimDataNeedsWriteback :=
        replaceProbeAckDataCapture || replaceNestedwbCapture(i) ||
        (orphanReleaseData && replaceAllocValid && selectedReplaceSlotOH(i)) ||
        mirroredReplaceProbeAckDataForSlot || orphanReplaceCapture
      // A B that actually enters RequestArb reserves this sidecar until its
      // response leaves TXRSP/TXDAT. The reservation prevents the sidecar
      // from observing B only in S1, then releasing the same victim while B
      // is still in MainPipe.
      // Any snoop that consumes this sidecar's ReleaseBuf must reserve it --
      // including one arriving after the copyback was issued (a late snoop is
      // still served from the snapshot).  snoopDone is unconditional, so the
      // reservation must be too, or the completion asserts.
      replaceMshrs(i).io.localReservationHold := io.sidecarReservationHold
      replaceMshrs(i).io.snoopStarted := io.bServiceStart.valid &&
        replaceMshrs(i).io.info.valid &&
        replaceMshrs(i).io.info.bits.set === io.bServiceStart.bits.set &&
        replaceMshrs(i).io.info.bits.tag === io.bServiceStart.bits.tag
      val snoopResponseMatches = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.fromB && done.bits.snpHitRelease &&
        done.bits.chiOpcode.get =/= CompData &&
        done.bits.snpHitReleaseIdx === i.U
      }
      replaceMshrs(i).io.snoopMeta.valid := snoopResponseMatches.reduce(_ || _)
      replaceMshrs(i).io.snoopMeta.bits := Mux1H(snoopResponseMatches,
        Seq(io.txdatDone.bits.meta, io.txrspDone.bits.meta))
      // A forwarding snoop owns both its HN response and requester CompData.
      // Count its context retirement once, after both terminal handshakes.
      val contextSnoopDoneCount = PopCount(VecInit(snoopRetireEvents.map { event =>
        event.valid && event.bits.targets(i) && replaceMshrs(i).io.info.valid &&
          event.bits.targetGenerations(i) === mshrTaskGenerations(2)(i) &&
          event.bits.targetSets(i) === replaceMshrs(i).io.info.bits.set &&
          event.bits.targetTags(i) === replaceMshrs(i).io.info.bits.tag
      }))
      val directSnoopDoneCount = PopCount(VecInit(
        Seq(io.txdatDone, io.txrspDone).zip(snoopResponseMatches).map {
          case (done, matches) => matches && !done.bits.mshrTask
        }))
      replaceMshrs(i).io.snoopDoneCount := contextSnoopDoneCount +& directSnoopDoneCount
      val snoopToInvalDone = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.fromB && done.bits.snpHitRelease &&
        done.bits.chiOpcode.get =/= CompData &&
        // A clean sidecar can also lose the line to an invalidating snoop.
        // The response state (I or I_PD) records that outcome; the release
        // mode alone only describes the state before the snoop.
        (done.bits.snpHitReleaseToInval ||
          done.bits.snpHitReleaseToClean && done.bits.resp.get(1, 0) === 0.U) &&
        done.bits.snpHitReleaseIdx === i.U
      }.reduce(_ || _)
      replaceMshrs(i).io.snoopToInvalDone := snoopToInvalDone
      replaceMshrs(i).io.copyBackDone := io.txdatDone.valid &&
      io.txdatDone.bits.mshrTask && io.txdatDone.bits.mshrContext === 2.U &&
      io.txdatDone.bits.mshrId === i.U && io.txdatDone.bits.chiOpcode.get === CopyBackWrData
      replaceMshrs(i).io.rxrsp.valid := io.resps.rxrsp.valid && io.resps.rxrsp.mshrId === replaceMshrs(i).io.txnId
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
      // the snoop context has no grant path to defer
      snoop.io.deferGrant := false.B
      val snoopRespDone = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.mshrTask && done.bits.mshrContext === 1.U &&
          done.bits.mshrId === i.U && done.bits.chiOpcode.get =/= CompData
      }.reduce(_ || _)
      snoop.io.snpRespDone := snoopRespDone
      snoop.io.dctDone := io.txdatDone.valid && io.txdatDone.bits.mshrTask &&
        io.txdatDone.bits.mshrContext === 1.U && io.txdatDone.bits.mshrId === i.U &&
        io.txdatDone.bits.chiOpcode.get === CompData

      // Never multiplex the victim over the normal request address. RXSNP
      // needs both views in the same cycle: normal `reqTag` protects a
      // Grant-to-refill window, while ReplaceMSHR owns only the victim and
      // ReleaseBuf lifecycle.
      val replaceInfoForOutput = WireInit(replaceMshrs(i).io.info)
      when(replaceAllocValid && selectedReplaceSlotOH(i)) {
        replaceInfoForOutput.valid := true.B
        replaceInfoForOutput.bits := incomingReplaceInfo
      }
      io.refillInfo(i) := refill.io.msInfo
      io.snoopInfo(i) := snoop.io.msInfo
      io.replaceInfo(i) := replaceInfoForOutput
      io.replaceInfoForSnoop(i) := replaceMshrs(i).io.info
      when(replaceAllocValid && selectedReplaceSlotOH(i)) {
        assert(!replaceMshrs(i).io.info.valid,
          "new replacement must not overwrite a registered sidecar")
        assert(!incomingReplaceInfo.probeDone && !incomingReplaceInfo.releaseBufReady,
          "new replacement is not yet a nestable ReleaseBuf owner")
      }
      io.pCrd(i) <> refill.io.pCrd
  }
  io.rxdatStaleSlots := rxdatStaleSlotOH.asUInt
  io.toMainPipe.snoopDataHolds := io.snoopInfo
  io.toMainPipe.postGrantReleaseHolds := io.refillInfo
  val sharedReleaseBufOwners = VecInit(snoopMshrs.zip(replaceMshrs).map {
    case (snoop, replace) => snoop.io.status.valid && replace.io.info.valid
  }).asUInt
  assert(!sharedReleaseBufOwners.orR,
    "SnoopMSHR and ReplaceMSHR must not share a ReleaseBuf slot")

  // A normal slot can be reused before its detached child finishes writeback.
  // Release only the parent association at the normal MSHR's actual retire
  // edge; the child remains fully visible as an independent victim context.
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
    replace.io.parentRelease := parentReleaseMatches.orR
    assert(PopCount(parentReleaseMatches) <= 1.U, "a ReplaceMSHR must match at most one retiring normal parent")
  }

  /* Reserve 1 entry for SinkB */
  // A ReleaseData must not update the victim line while a normal MSHR that
  // was already active at its arrival still waits for its replacement read.
  // The replacer snapshots that victim's tag/meta before C writes. A release
  // to another line in the same set is independent and must keep flowing;
  // otherwise it can block the ReleaseAck needed by an outstanding Probe.
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
  // A matching sidecar holds the only up-to-date copy of an evicted line in
  // ReleaseBuf until its writeback has actually left the cache.  Do not admit
  // the RequestBuffer output candidate for that line: a lower-level read
  // could otherwise return the older memory image before the sidecar's
  // release/CopyBack sequence is complete.  `a_tag/a_set` describe ReqBuf's
  // input and can differ from its queued output, so they are not sufficient
  // for this ownership check.
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
  // Account for every B still in S2/S3 before admitting another potential
  // snoop. The first candidate waits for snoopCandidateSeen so the registered
  // reservation fence is already active when that B enters the pipeline.
  io.toReqArb.blockB_s1 := mshrFull ||
    (io.fromReqArb.status_s1.bToN && pendingToNFull) ||
    (io.fromReqArb.status_s1.bCandidateValid &&
      (!snoopCandidateSeen || PopCount(snoopBaseEligibleOH) <= snoopEnteringCount))
  io.toReqArb.blockA_s1 := a_mshrFull || aSidecarConflict || aSnoopConflict ||
    aPendingToNConflict || aEnteringToNConflict
  io.toReqArb.blockG_s1 := false.B

  /* Acquire downwards to TXREQ*/
  // Normal reads (latency-critical) and victim writeback requests share the
  // direct MSHR->TXREQ path; writeback control is control-only and needs no
  // MainPipe pass.
  //
  // R2: insert a one-entry registered skid per ReplaceMSHR before the shared
  // two-level arbiter.  The live sidecar cone (state/snoopStarted ->
  // txreq.valid/bits) previously ran through highArb and the TXREQ queue
  // straight into the queue RAM write.  With the skid, that cone ends at a
  // local valid/bits register and the RAM write starts from registered
  // inputs.  Normal refill reads keep their direct, zero-added-latency path
  // into lowArb.
  //
  // A capture is the sidecar's irreversible commit point (it sets
  // releaseSent in ReplaceMSHR): the skid then owns the flit until the
  // arbiter accepts it.  The sidecar stays valid and the retire/txdat
  // conditions hold on !txreqPending, so the flit cannot be duplicated, and
  // its txnID and ReleaseBuf entry are not reused while it is pending.  The
  // request-before-CopyBackWrData ordering is preserved because the data
  // path additionally waits for releaseAck, which the HN can only return
  // after this request has actually left TXREQ.
  val replaceTxreqSkids = replaceMshrs.zipWithIndex.map { case (m, i) =>
    val busy = RegInit(false.B)
    val bits = Reg(new CHIREQ)
    val out = Wire(Decoupled(new CHIREQ))
    // No bypass, one-entry FIFO: capture only when empty, hold otherwise.
    m.io.tasks.txreq.ready := !busy && !io.sidecarReservationHold
    m.io.txreqPending := busy
    out.valid := busy
    out.bits := bits
    when (m.io.tasks.txreq.fire) {
      // fire == capture: the sidecar is still live, so its bits are stable.
      busy := true.B
      bits := m.io.tasks.txreq.bits
    }.elsewhen(out.fire) {
      busy := false.B
    }
    // Exactly one capture per episode, and never over an undrained flit.
    when (m.io.tasks.txreq.fire) {
      assert(!busy, s"replace TXREQ skid $i captured into a non-empty entry")
    }
    // A skid entry is a committed victim writeback.  It must never outlive
    // the context that owns its txnID/ReleaseBuf.
    assert(!busy || m.io.info.valid,
      s"replace TXREQ skid $i holds a flit without a live sidecar")
    when (m.io.alloc.valid) {
      assert(!busy, s"replace TXREQ skid $i not drained before reallocation")
    }
    out
  }
  val replaceTxreqSkidAnyBusy = VecInit(replaceTxreqSkids.map(_.valid)).asUInt.orR
  io.replaceTxreqPendingCnt := PopCount(VecInit(replaceTxreqSkids.map(_.valid)))
  // Bounded drain watchdog: the two-level RR arbiter alternates the groups
  // and round-robins inside each group, so a skid only waits for downstream
  // bandwidth.  This bound is far above any legitimate backpressure window;
  // hitting it means the handshake was lost.
  val replaceTxreqSkidWait = RegInit(0.U(16.W))
  when (replaceTxreqSkidAnyBusy) {
    replaceTxreqSkidWait := Mux(replaceTxreqSkidWait === 65535.U, replaceTxreqSkidWait,
      replaceTxreqSkidWait + 1.U)
  }.otherwise {
    replaceTxreqSkidWait := 0.U
  }
  assert(replaceTxreqSkidWait =/= 65535.U, "replace TXREQ skid failed to drain")
  ArbPerf(twoLevelArb(refillMshrs.map(_.io.tasks.txreq) ++ replaceTxreqSkids, io.toTXREQ, Some("txreq")), "txreq_arb")

  /* Victim CopyBackWrData direct to TXDAT */
  // The sidecar's copyback task carries no directory/DS side effect; its
  // mainpipe pass only moved ReleaseBuf data into TXDAT.  This path does the
  // transfer directly: pick one pending sidecar (RR), read its ReleaseBuf
  // entry through the dedicated second port (registered response next
  // cycle), then hold task+data for TXDAT.  A sidecar withdraws its request
  // when a nested snoop preempts; if that happens before TXDAT accepts, the
  // staged request is dropped and the sidecar re-presents with the post-snoop
  // state, mirroring the pre-acceptance preemption of the RequestArb path.
  val directCbValids = VecInit(replaceMshrs.map(_.io.tasks.txdat.valid))
  val cbPresent = RegInit(false.B)
  // The copyback pick is registered a cycle before the ReleaseBuf read.  The
  // read id then no longer sits at the end of the txdat.valid/bFire cone, and
  // the pick is re-confirmed when the read is issued: a sidecar withdrawn by
  // a nested snoop drops the pick without touching ReleaseBuf.
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
    // The sidecar withdrew before the read: drop the pick and let it present
    // again with the post-snoop state.
    cbPickValidReg := false.B
  }
  // ReleaseBuf resp2 is registered and holds until the next read, so the
  // data is stable for the whole present window.
  val cbSelStillValid = Mux1H(cbSelOH, directCbValids)
  io.toTXDAT.valid := cbPresent && cbSelStillValid && !io.sidecarReservationHold
  io.toTXDAT.bits.task := cbTaskReg
  io.toTXDAT.bits.data := io.releaseBufDirectResp.data
  when (cbPresent && !cbSelStillValid) {
    // Preempted by a nested snoop before TXDAT acceptance: drop the staged
    // request; the sidecar re-presents with refreshed state afterwards.
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
    replaceNeedsAlloc && cmoSidecarAlloc && replaceAllocValid)
  XSPerfAccumulate("replresp_cmo_conflict_retry_cnt",
    replResponseApplied.valid && replRespCmoConflict)
  XSPerfAccumulate("txdat_direct_copyback_cnt", io.toTXDAT.fire)
  XSPerfAccumulate("txdat_direct_abort_cnt", cbPresent && !cbSelStillValid)
  XSPerfAccumulate("txdat_direct_wait_sum",
    PopCount(Cat(replaceMshrs.map(m => m.io.tasks.txdat.valid && !m.io.tasks.txdat.ready))))

  /* Response downwards to TXRSP*/
  // ReplaceMSHR joins for the CompAck owed by a WriteEvictOrEvict Comp.
  val txrspArb = twoLevelArb(mshrs.map(_.io.tasks.txrsp) ++ replaceMshrs.map(_.io.tasks.txrsp), io.toTXRSP, Some("txrsp"))
  ArbPerf(txrspArb, "txrsp_arb")
  // Sum contending contexts, rather than relying on ArbPerf input positions which
  // differ between the single-context and paired-context implementations.
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
      // S/R mutual exclusion on the physical entry makes the sidecar the sole
      // ReleaseBuf owner while it lives -- no lease arbitration required.
      task.valid := m.io.tasks.source_b.valid
      task.bits := m.io.tasks.source_b.bits
      m.io.tasks.source_b.ready := task.ready
      task
  }
  val sourceBTasks = mshrs.zip(contextPhysicalIds).map {
    case (m, _) =>
      val task = Wire(Decoupled(new SourceBReq))
      // N+X mutual exclusion (see mshrSelector idle gates) guarantees a single
      // active probe owner per physical entry at any time.
      task.valid := m.io.tasks.source_b.valid
      task.bits := m.io.tasks.source_b.bits
      m.io.tasks.source_b.ready := task.ready
      task
  }

  // TileLink C ProbeAck does not carry a transaction identifier: all probes
  // from SourceB use the client source ID.  Therefore two outstanding probes
  // for the same line cannot be distinguished when their C response returns.
  // Once a context has handed its B request to SourceB, keep a later probe to
  // the same line behind that response.  This applies across normal, snoop,
  // and independent ReplaceMSHR contexts.
  val sourceBProbeOutstanding = mshrs.zipWithIndex.map {
    case (m, contextIdx) =>
      val status = m.io.status.bits
      // A normal context delegates the victim probe to ReplaceMSHR. Its
      // source_b.valid is then intentionally low even before any Probe was
      // issued; treating that as an in-flight Probe blocks the sidecar's only
      // request forever. Use the context's registered probeIssued marker,
      // rather than source_b.valid or the combinational replaceActive port.
      // ReplaceMSHR has its own outstanding entry below.
      val normalDelegatedToReplace = if (contextIdx % 2 == 0) {
        m.io.sidecarDelegated
      } else {
        false.B
      }
      val probeOutstanding = m.io.status.valid && !normalDelegatedToReplace &&
        status.w_c_resp && m.io.probeIssued
      // During replacement handoff the status tag can switch between the
      // request line and the victim line. SinkC matching uses either value
      // depending on needsRepl, so protect both while the Probe is outstanding.
      Seq(
        (probeOutstanding, status.set, status.reqTag),
        (probeOutstanding, status.set, status.metaTag)
      )
  }.flatten
  val replaceProbeOutstanding = replaceMshrs.map { m =>
    (m.io.info.valid && m.io.info.bits.awaitingProbeAck, m.io.info.bits.set, m.io.info.bits.tag)
  }
  // SinkC ProbeAck has no transaction ID. Do not issue a second Probe for a
  // line that already has an accepted Probe waiting for its C response. The
  // outstanding tuple intentionally excludes a context whose SourceB task is
  // still queued, so the arbiter can accept the first Probe and establish the
  // ownership marker without a combinational ready/valid feedback loop.
  val sourceBOutstanding = sourceBProbeOutstanding ++ replaceProbeOutstanding
  // Each source publishes a full registered candidate before the 48-way RR.
  // Capturing a candidate never fires the owner or consumes SourceB capacity.
  class SourceBCandidate extends Bundle {
    val request = new SourceBReq
    val source = UInt(log2Ceil(3 * mshrsAll).W)
    val context = UInt(2.W)
    val id = UInt(mshrBits.W)
    val generation = UInt(8.W)
  }
  val sourceBInputs = sourceBTasks ++ replaceSourceBTasks
  val sourceBGenerations = (0 until mshrsAll).flatMap(i =>
    Seq(mshrTaskGenerations(0)(i), mshrTaskGenerations(1)(i))) ++ mshrTaskGenerations(2)
  val sourceBCommit = RegInit(0.U.asTypeOf(Valid(new SourceBCandidate)))
  val sourceBCandidateOut = Wire(Decoupled(new SourceBCandidate))
  val sourceBHoldPorts = mshrs.map(_.io.sourceBHold) ++ replaceMshrs.map(_.io.sourceBHold)
  val sourceBTaskCandidates = sourceBInputs.zipWithIndex.map { case (task, i) =>
    val candidate = RegInit(0.U.asTypeOf(Valid(new SourceBCandidate)))
    val selected = sourceBCommit.valid && sourceBCommit.bits.source === i.U
    sourceBHoldPorts(i) := candidate.valid || selected
    when(!candidate.valid && !selected && task.valid) {
      candidate.valid := true.B
      candidate.bits.request := task.bits
      candidate.bits.source := i.U
      candidate.bits.context := (if(i < 2 * mshrsAll) i % 2 else 2).U
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
  val commitCurrent = Mux1H(commitOH, sourceBInputs.map(_.valid)) &&
    Mux1H(commitOH, sourceBGenerations) === sourceBCommit.bits.generation &&
    Mux1H(commitOH, sourceBInputs.map(_.bits)).asUInt === sourceBCommit.bits.request.asUInt
  // Recheck at the unique commit point: several prepared same-line candidates
  // may coexist, but only the first can establish the outstanding Probe.
  val commitLineBusy = sourceBOutstanding.map { case (valid, set, tag) =>
    valid && set === sourceBCommit.bits.request.set && tag === sourceBCommit.bits.request.tag
  }.reduce(_ || _)
  val sourceB = Module(new SourceB())
  sourceB.io.task.valid := sourceBCommit.valid && commitCurrent && !commitLineBusy
  sourceB.io.task.bits := sourceBCommit.bits.request
  sourceBInputs.zipWithIndex.foreach { case (task, i) =>
    task.ready := sourceB.io.task.fire && commitOH(i)
  }
  when(sourceBCandidateOut.fire) {
    sourceBCommit.valid := true.B
    sourceBCommit.bits := sourceBCandidateOut.bits
  }.elsewhen(sourceBCommit.valid && (!commitCurrent || sourceB.io.task.fire)) {
    sourceBCommit.valid := false.B
  }
  when(sourceB.io.task.fire) {
    assert(PopCount(VecInit(sourceBInputs.map(_.fire))) === 1.U,
      "only a unique SourceB commit can consume a source")
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

  /* Arbitrate MSHR task to RequestArbiter with class-based QoS.
   * Strict class priority (lower class wins) with round-robin inside a class:
   *   0: responses — SnoopMSHR tasks and normal-context B-channel answers
   *      (probeAck/DCT); they sit on the openLLC/L1 snoop critical path.
   *   1: normal L1-visible grant (mp_grant) — directly sets L1 refill latency.
   *   2: normal refill commit / replRead / revalidate — frees the MSHR and
   *      releases its way for follow-up requests.
   *   3: ReplaceMSHR DS capture — gates the deferred commit of its parent.
   *   4: everything else (sidecar release/copyback, CMO, background tasks).
   * Starvation guard: a source valid for 63 consecutive unserved cycles is
   * promoted to class 0 until it fires.
   */
  val mshrTask = Wire(Decoupled(new TaskBundle()))
  // The three context types share one MainPipe admission port, but their
  // payloads are independent.  Group them so each group only arbitrates
  // mshrsAll inputs and registers its winner; the cross-group stage then
  // only sees three candidates.  This keeps the class masks, RR and payload
  // mux at 16 inputs instead of 48 in a single cycle.
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
  def isGrantOp(op: UInt) = op === TLMessages.Grant || op === TLMessages.GrantData ||
    op === TLMessages.AccessAckData || op === TLMessages.HintAck
  def mshrTaskClass(t: DecoupledIO[TaskBundle]): UInt = {
    val isResp = t.bits.mshrContext === 1.U ||
      (t.bits.mshrContext === 0.U && t.bits.fromB)
    val isGrant = t.bits.mshrContext === 0.U && !t.bits.refillOnly && isGrantOp(t.bits.opcode)
    val isCommitOrLookup = t.bits.mshrContext === 0.U &&
      (t.bits.refillOnly || t.bits.replTask || t.bits.normalRefillRevalidate)
    val isCapture = t.bits.mshrContext === 2.U && t.bits.replaceCapture
    Mux(isResp, 0.U(3.W),
      Mux(isGrant, 1.U(3.W),
        Mux(isCommitOrLookup, 2.U(3.W),
          Mux(isCapture, 3.U(3.W), 4.U(3.W)))))
  }
  val mshrTaskClasses = mshrTaskSources.map(mshrTaskClass)
  // Register each source's class while it is valid.  The arbitration then
  // reads a register instead of the deferGrant -> opcode -> class cone; a
  // task type change is visible one cycle later, which only shifts a QoS
  // hint and cannot affect correctness (the starvation guard stays).
  val mshrTaskClassReg = mshrTaskSources.zip(mshrTaskClasses).map { case (t, c) =>
    val r = RegInit(0.U(3.W))
    when(t.valid) { r := c }
    r
  }
  val mshrTaskWaitCnt = mshrTaskSources.map { t =>
    val cnt = RegInit(0.U(6.W))
    when(t.fire) {
      cnt := 0.U
    }.elsewhen(t.valid && !t.ready) {
      cnt := Mux(cnt === 63.U, 63.U, cnt + 1.U)
    }
    cnt
  }
  val mshrTaskEffClasses = mshrTaskClassReg.zip(mshrTaskWaitCnt).map { case (c, w) =>
    Mux(w === 63.U, 0.U(3.W), c)
  }
  // A source scheduled to retire at this edge cannot grant a fresh lease.
  val mshrTaskSrcValids = VecInit(mshrTaskSources.zip(mshrTaskLeasePorts.flatten).map {
    case (source, lease) => source.valid
  }).asUInt

  // Per-group arbitration: best effective class, then round-robin inside it.
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
  // Preparation holds an explicit source lifetime lease. Issue/cancel first
  // registers a return request; the source acknowledges its exact version
  // before the group can refill. Issued operations retain the lease until
  // their final local MainPipe/buffer reference completes. Sources freeze
  // payload and variant on prepare, and revoke by a versioned event cancel.
  class MSHRPreparedTask extends Bundle {
    val task = new TaskBundle()
    val source = UInt(log2Up(mshrTaskGroupSize).W)
    val qosClass = UInt(3.W)
    val token = new MainpipeLeaseToken
  }
  class MSHRTaskTicket extends Bundle {
    val group = UInt(log2Up(nMshrTaskGroups).W)
    val source = UInt(log2Up(mshrTaskGroupSize).W)
    val qosClass = UInt(3.W)
  }
  val mshrTaskPreparedValid = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskPrepared = Seq.fill(nMshrTaskGroups)(
    RegInit(0.U.asTypeOf(new MSHRPreparedTask)))
  val mshrTaskLeaseReturning = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskLeaseReturnSent = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskLeaseAwaitPipe = Seq.fill(nMshrTaskGroups)(RegInit(false.B))
  val mshrTaskLeaseSequence = Seq.fill(nMshrTaskGroups)(RegInit(0.U(8.W)))
  val mshrTaskGroupWait = Seq.fill(nMshrTaskGroups)(RegInit(0.U(6.W)))
  val mshrTaskGroupClassEff = (0 until nMshrTaskGroups).map { g =>
    Mux(mshrTaskGroupWait(g) === 63.U, 0.U(3.W), mshrTaskPrepared(g).qosClass)
  }
  val mshrTaskGroupCancel = (0 until nMshrTaskGroups).map { g =>
    val sourceOH = UIntToOH(mshrTaskPrepared(g).source, mshrTaskGroupSize)
    Mux1H(sourceOH, mshrTaskLeasePorts(g).map(_.cancel))
  }
  val mshrTaskGroupReady = (0 until nMshrTaskGroups).map { g =>
    mshrTaskPreparedValid(g) && !mshrTaskLeaseReturning(g) &&
      !mshrTaskGroupCancel(g).valid &&
      VecInit(mshrTaskGenerations(g))(mshrTaskPrepared(g).source) === mshrTaskPrepared(g).token.generation
  }

  // The cross-group arbiter transports only a narrow ticket.  Register that
  // ticket before selecting a prepared payload. Frozen source variants use
  // narrow token/cancel eligibility rather than a live task-type comparison.
  val mshrTaskTicketValid = RegInit(false.B)
  val mshrTaskTicket = RegInit(0.U.asTypeOf(new MSHRTaskTicket))
  val mshrTaskTicketOut = Wire(Decoupled(new MSHRTaskTicket))
  val mshrTaskFinalBestClass = PriorityEncoder(VecInit((0 until 5).map { c =>
    VecInit((0 until nMshrTaskGroups).map { g =>
      mshrTaskGroupReady(g) && mshrTaskGroupClassEff(g) === c.U
    }).asUInt.orR
  }).asUInt)
  val mshrTaskTicketInputs = (0 until nMshrTaskGroups).map { g =>
    val ticket = Wire(Decoupled(new MSHRTaskTicket))
    ticket.valid := mshrTaskGroupReady(g) && mshrTaskGroupClassEff(g) === mshrTaskFinalBestClass
    ticket.bits.group := g.U
    ticket.bits.source := mshrTaskPrepared(g).source
    ticket.bits.qosClass := mshrTaskGroupClassEff(g)
    ticket
  }
  ArbPerf(twoLevelArb(mshrTaskTicketInputs, mshrTaskTicketOut, Some("mshr_task_ticket")),
    "mshr_task_ticket_arb")
  mshrTaskTicketOut.ready := !mshrTaskTicketValid
  val mshrTaskTicketGroupOH = UIntToOH(mshrTaskTicket.group, nMshrTaskGroups)
  val mshrTaskTicketReady = Mux1H(mshrTaskTicketGroupOH, (0 until nMshrTaskGroups).map { g =>
    mshrTaskGroupReady(g) && mshrTaskPrepared(g).source === mshrTaskTicket.source
  })
  mshrTask.valid := mshrTaskTicketValid && mshrTaskTicketReady
  mshrTask.bits := Mux1H(mshrTaskTicketGroupOH, mshrTaskPrepared.map(_.task))
  mshrTask.bits.mshrId := mshrTaskTicket.source
  mshrTask.bits.qosClass := mshrTaskTicket.qosClass
  io.mshrTaskLeaseToken := Mux1H(mshrTaskTicketGroupOH, mshrTaskPrepared.map(_.token))
  when(mshrTaskTicketOut.fire) {
    mshrTaskTicketValid := true.B
    mshrTaskTicket := mshrTaskTicketOut.bits
    assert(mshrTaskTicketOut.bits.qosClass === mshrTaskFinalBestClass,
      "selected ticket must belong to the best effective QoS class")
  }.elsewhen(mshrTaskTicketValid && (!mshrTaskTicketReady || mshrTask.fire)) {
    // Cancellation consumes no source.  Do not reload on issue/cancel: the
    // next ticket observes the updated ownership and task variant.
    mshrTaskTicketValid := false.B
  }.elsewhen(mshrTaskTicketValid &&
    Mux1H(mshrTaskTicketGroupOH, mshrTaskGroupWait.map(_ === 63.U))) {
    // RequestArb may defer class-4 tasks while sinks have work.  Keep the
    // existing age promotion alive after ticket capture, otherwise a held
    // background ticket could prevent every group from making progress.
    // Only this narrow QoS register changes; the owner and payload stay held.
    mshrTaskTicket.qosClass := 0.U
  }

  (0 until nMshrTaskGroups).foreach { g =>
    val preparedOut = Wire(Decoupled(new MSHRPreparedTask))
    val candidates = groupCandidates(g, groupValids(g))
    val preparedInputs = groupOf(g).zipWithIndex.map { case (s, i) =>
      val in = Wire(Decoupled(new MSHRPreparedTask))
      in.valid := candidates(i)
      in.bits.task := s.bits
      in.bits.source := i.U
      in.bits.qosClass := groupClasses(g)(i)
      in.bits.token.generation := mshrTaskGenerations(g)(i)
      in.bits.token.sequence := mshrTaskLeaseSequence(g)
      in
    }
    ArbPerf(twoLevelArb(preparedInputs, preparedOut, Some(s"mshr_task_group_$g")),
      s"mshr_task_group_${g}_arb")
    preparedOut.ready := !mshrTaskPreparedValid(g)
    val granted = mshrTask.fire && mshrTaskTicketGroupOH(g)
    val sourceOH = UIntToOH(mshrTaskPrepared(g).source, mshrTaskGroupSize)
    val returnRequest = mshrTaskPreparedValid(g) && mshrTaskLeaseReturning(g) &&
      !mshrTaskLeaseAwaitPipe(g) && !mshrTaskLeaseReturnSent(g)
    mshrTaskLeasePorts(g).zipWithIndex.foreach { case (lease, i) =>
      lease.prepare.valid := preparedOut.fire && preparedOut.bits.source === i.U
      lease.prepare.bits := preparedOut.bits.token
      lease.held.valid := mshrTaskPreparedValid(g) && sourceOH(i)
      lease.held.bits := mshrTaskPrepared(g).token
      lease.release.valid := returnRequest && sourceOH(i)
      lease.release.bits := mshrTaskPrepared(g).token
    }
    val returnAck = Mux1H(sourceOH, mshrTaskLeasePorts(g).map(_.released))
    when(preparedOut.fire) {
      mshrTaskPreparedValid(g) := true.B
      mshrTaskPrepared(g) := preparedOut.bits
      mshrTaskLeaseReturning(g) := false.B
      mshrTaskLeaseReturnSent(g) := false.B
      mshrTaskLeaseAwaitPipe(g) := false.B
      mshrTaskLeaseSequence(g) := mshrTaskLeaseSequence(g) + 1.U
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(returnAck.valid) {
      assert(mshrTaskPreparedValid(g) && mshrTaskLeaseReturning(g) &&
        mshrTaskLeaseReturnSent(g) && !mshrTaskLeaseAwaitPipe(g) &&
        returnAck.bits.asUInt === mshrTaskPrepared(g).token.asUInt,
        "prepared lease acknowledgment must match its outstanding return")
      assert(!mshrTaskTicketValid || !mshrTaskTicketGroupOH(g),
        "ticket must drain before the source lifetime lease is released")
      mshrTaskPreparedValid(g) := false.B
      mshrTaskLeaseReturning(g) := false.B
      mshrTaskLeaseReturnSent(g) := false.B
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(mshrTaskPreparedValid(g) && !mshrTaskLeaseReturning(g) &&
      (granted || !mshrTaskGroupReady(g))) {
      // Event cancellation immediately revokes this version; its registered
      // return/ack drains the old ticket before a fresh snapshot can exist.
      mshrTaskLeaseReturning(g) := true.B
      mshrTaskLeaseAwaitPipe(g) := granted
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(mshrTaskLeaseReturning(g)) {
      mshrTaskGroupWait(g) := 0.U
    }.elsewhen(mshrTaskPreparedValid(g)) {
      mshrTaskGroupWait(g) := Mux(mshrTaskGroupWait(g) === 63.U, 63.U, mshrTaskGroupWait(g) + 1.U)
    }
    when(mshrTaskGroupCancel(g).valid) {
      assert(mshrTaskPreparedValid(g) && !mshrTaskLeaseAwaitPipe(g) &&
        mshrTaskGroupCancel(g).bits.asUInt === mshrTaskPrepared(g).token.asUInt,
        "source cancellation must name its unissued prepared version")
      assert(!granted, "canceled prepared operation must never issue")
    }
    when(returnRequest) { mshrTaskLeaseReturnSent(g) := true.B }
    val localDone = io.fromMainPipe.mainpipeLeaseDone
    when(localDone.valid && localDone.bits.mshrContext === g.U) {
      assert(mshrTaskPreparedValid(g) && mshrTaskLeaseReturning(g) &&
        mshrTaskLeaseAwaitPipe(g) && localDone.bits.mshrId === mshrTaskPrepared(g).source &&
        localDone.bits.token.asUInt === mshrTaskPrepared(g).token.asUInt,
        "MainPipe terminal event must match the issued source lease version")
      mshrTaskLeaseAwaitPipe(g) := false.B
    }
    groupOf(g).zipWithIndex.foreach { case (s, i) =>
      // Preparing a task/ticket is revocable; only final issue transfers
      // ownership and advances the source's task state machine.
      s.ready := granted && mshrTaskTicket.source === i.U
    }
    assert(!preparedOut.fire || !mshrTaskPreparedValid(g),
      "a prepared task must drain or cancel before the group can reload")
  }
  // The preparation/ticket stages cannot consume a source.  An issued task
  // drains its local references and returns the lease before fresh preparation.
  val mshrTaskGrantSrc = VecInit((0 until nMshrTaskGroups).map { g =>
    Mux(mshrTask.fire && mshrTaskTicketGroupOH(g),
      UIntToOH(mshrTaskTicket.source, mshrTaskGroupSize), 0.U)
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
  // QoS observability: per-class pending pressure and starvation promotions.
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
  // Every ticket carries the physical slot index for its context group.
  io.mshrTask <> mshrTask

  /* releaseBuf link to MSHR id */
  // ReleaseBuf is owned by the replace sidecar and the snoop context only;
  // a ProbeAckData for a normal context (Get-on-TRUNK / cache-alias) is
  // captured into that normal slot's RefillBuf instead.
  val sinkCReleaseBufWrite = io.resps.sinkC.valid && io.resps.sinkC.respInfo.last &&
    io.resps.sinkC.respInfo.opcode === ProbeAckData
  val releaseBufMatch = VecInit((0 until mshrsAll).map { i =>
    resp_sinkC_match_vec(2 * i + 1) || replaceProbeSinkCMatch(i)
  })
  io.releaseBufWriteId := ParallelPriorityMux(releaseBufMatch, (0 until mshrsAll).map(i => i.U))
  // Independent ProbeAckData mirror route: capture payload, beat mask, target
  // sidecar id and generation before any later ReleaseBuf write. This route
  // cannot be substituted by the nested descriptor because it may arrive
  // after the pair was registered and must retain the newer probe payload.
  val probeAckMirrorCapture = RegInit(0.U.asTypeOf(Valid(new MSHRBufWrite)))
  val probeAckMirrorCaptureGeneration = RegInit(0.U(8.W))
  probeAckMirrorCapture.valid := mirroredReplaceProbeAckDataCapture
  when(mirroredReplaceProbeAckDataCapture) {
    probeAckMirrorCapture.bits.id := OHToUInt(mirroredReplaceProbeAckDataOH)
    probeAckMirrorCapture.bits.data := io.sinkCProbeAckPayload
    probeAckMirrorCapture.bits.beatMask := Fill(beatSize, true.B)
    // The raw replacer decision reserves this slot now and allocates it at
    // next-cycle apply. Retain that next generation even before it is live.
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
    Mux(mirroredReplaceProbeAckDataCapture, mirroredReplaceProbeAckDataOH, 0.U(mshrsAll.W)) |
      Mux(probeAckMirrorCapture.valid, UIntToOH(probeAckMirrorCapture.bits.id, mshrsAll), 0.U(mshrsAll.W)) |
      Mux(probeAckMirrorWrite.valid, mirrorOwnerOH, 0.U(mshrsAll.W))
  when(probeAckMirrorWrite.valid) {
    assert(mirrorOwnerCurrent, "ProbeAckData mirror lost its reserved replacement generation")
  }
  val refillBufMatch = VecInit((0 until mshrsAll).map { i => resp_sinkC_match_vec(2 * i) })
  io.refillBufWriteId.valid := sinkCReleaseBufWrite && Cat(refillBufMatch).orR
  io.refillBufWriteId.bits := ParallelPriorityMux(refillBufMatch, (0 until mshrsAll).map(i => i.U))
  val snoopRefillBufMirrorMatch = VecInit(refillMshrs.zip(snoopMshrs).zipWithIndex.map {
    case ((refill, snoop), i) =>
      resp_sinkC_match_vec(2 * i + 1) && refill.io.status.valid &&
        refill.io.status.bits.set === snoop.io.status.bits.set &&
        refill.io.status.bits.reqTag === snoop.io.status.bits.reqTag
  })
  io.snoopRefillBufWriteId.valid := sinkCReleaseBufWrite && Cat(snoopRefillBufMirrorMatch).orR
  io.snoopRefillBufWriteId.bits := ParallelPriorityMux(snoopRefillBufMirrorMatch, (0 until mshrsAll).map(i => i.U))
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

  /* Nested ReleaseData: owner masks -> encoded descriptor -> writes -> apply. */
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
      // A same-S3 orphan/CMO allocation creates the next generation at capture.
      nestedReleaseMasks.bits.generations(i) := Mux(nestedReplaceMask(i),
        mshrTaskGenerations(2)(i) + replaceMshrs(i).io.alloc.valid.asUInt,
        mshrTaskGenerations(1)(i))
    }
  }

  // Raw C ownership/payload is first held in a reservation register. The
  // descriptor and physical writes consume the registered record one edge
  // later, so raw pending and done cannot cross or lose the first beat.
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
  // Validate the retained owner before the physical write stage. Pending
  // pins it through the next edge, so the generation mux never enters the
  // ReleaseBuf write-enable/decode cone.
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
  // There is no write backpressure. Both selected ports take effect on this
  // edge; only their whole-descriptor event can mark either consumer ready.
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
  (0 until 2).foreach { context =>
    val replacement = context == 1
    val raw = if(replacement) nestedReplaceMask else nestedSnoopMask
    val captured = Mux(nestedReleaseReserved.valid,
      if(replacement) nestedReleaseReserved.bits.replace else nestedReleaseReserved.bits.snoop, 0.U(mshrsAll.W))
    val masks = Mux(nestedReleaseMasks.valid,
      if(replacement) nestedReleaseMasks.bits.replace else nestedReleaseMasks.bits.snoop, 0.U(mshrsAll.W))
    val mirror = if(replacement) mirroredReplaceProbeAckDataPendingOH else 0.U(mshrsAll.W)
    nestedReleasePending(context) := raw | masks | captured | mirror |
      descriptorMask(nestedReleaseWrite, replacement) | descriptorMask(nestedReleaseValidated, replacement) |
      descriptorMask(nestedReleaseApplied, replacement)
    nestedReleaseDone(context) := descriptorMask(nestedReleaseApplied, replacement)
  }
  // ReleaseData is newer than in-flight DS/refill-copy/ProbeAck snapshots.
  // Keep that provenance until physical ReleaseBuf changes owners. Their
  // control completions still apply, but cannot overwrite authoritative data.
  val releaseOwnerAlloc = VecInit(snoopMshrs.map(_.io.alloc.valid)).asUInt |
    VecInit(replaceMshrs.map(_.io.alloc.valid)).asUInt
  val nestedReleaseAuthority = RegInit(0.U(mshrsAll.W))
  nestedReleaseAuthority := (nestedReleaseAuthority & ~releaseOwnerAlloc) | nestedReleaseMask
  io.nestedReleaseAuthority := nestedReleaseAuthority

  // RefillBuf is written at the original S3 event, with its existing stale
  // RXDAT/DS protection; only ReleaseBuf takes the descriptor pipeline.
  io.nestedwbRefillDataId.valid := refillNestedwbActive.orR
  io.nestedwbRefillDataId.bits := OHToUInt(refillNestedwbActive)

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
  // Debug counters for the victim-selection-degradation investigation: cycles
  // each Directory way stays locked by an in-flight normal refill commit
  // (blockRefill) or by a ReplaceMSHR sidecar, plus the post-Grant release
  // hold window. Divide by miss count to get the average lock duration.
  XSPerfAccumulate("waylock_normal_sum",
    PopCount(io.refillInfo.map(s => s.valid && s.bits.blockRefill)))
  XSPerfAccumulate("waylock_replace_sum",
    PopCount(io.replaceInfo.map(_.valid)))
  XSPerfAccumulate("waylock_postGrantReleaseHold_sum",
    PopCount(io.refillInfo.map(s => s.valid && s.bits.postGrantReleaseHold)))
  XSPerfAccumulate("replaceMSHR_alloc_cnt",
    PopCount(replaceMshrs.map(_.io.alloc.valid)))
  XSPerfAccumulate("victim_probe_cnt",
    PopCount(replaceMshrs.map(_.io.tasks.source_b.fire)))
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
  // TODO: Is the width(4.W) proper here?
  val lmiss = Wire(Vec(mshrsAll, UInt(4.W)))
  for (((hpm_timer, m), i) <- hpm_timers.zip(refillMshrs).zipWithIndex) {
    when(m.io.alloc.valid) {
      hpm_timer := 1.U
    }.otherwise {
      hpm_timer := hpm_timer + 1.U
    }
    val enable = m.io.status.valid && m.io.status.bits.will_free
    when(enable && hpm_timer > 200.U) {
      lmiss(i) := 1.U
    }.otherwise {
      lmiss(i) := 0.U
    }
  }

  val perfEvents = Seq(
    ("l2_cache_refill", io.resps.rxdat.valid && io.resps.rxdat.respInfo.last),
    ("l2_cache_rd_refill", io.resps.rxdat.valid && io.resps.rxdat.respInfo.last),
    ("l2_cache_wr_refill", false.B),
    ("l2_cache_long_miss", lmiss.reduce(_ + _))
  )
  generatePerfEvent()
}
