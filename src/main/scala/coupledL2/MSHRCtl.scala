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
      val mshr_alloc_s3 = Flipped(ValidIO(new MSHRRequest()))
      // MainPipe signals that a replace capture task (DS read of a client-less
      // victim) wrote the victim data into the replace slot's ReleaseBuf.
      val replaceCaptureWrite = Flipped(ValidIO(UInt(mshrBits.W)))
      // MainPipe finished a late RefillBuf[N] -> ReleaseBuf[S] copy for a
      // post-Grant snoop that allocated after the ReleaseData payload.
      val refillToReleaseCopyWrite = Flipped(ValidIO(UInt(mshrBits.W)))
      // MainPipe has reached S3 and completed a normal refill commit. A
      // post-Grant toN may cancel its DS/meta write at that point.
      val refillWriteDone = Flipped(ValidIO(UInt(mshrBits.W)))
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
      // A normal refill-only task can already be in RequestArb when its paired
      // SnoopMSHR resolves. MainPipe applies these registered outcomes at S3.
      val normalRefillLateSnoopToN = Output(Vec(mshrsAll, Bool()))
      val normalRefillLateSnoopToB = Output(Vec(mshrsAll, Bool()))
      val normalRefillLateSnoopToClean = Output(Vec(mshrsAll, Bool()))
      // The ReplacerResult returning at MainPipe S3 selects a no-probe
      // victim (invalid or client-less) with no ownership conflict and, for
      // a valid victim, a live ReleaseBuf reservation.  MainPipe may then
      // merge the refill install into the grant pass; grantInstallSlot is
      // the reserved sidecar ReleaseBuf slot for the victim snapshot.
      val grantInstallOK = Output(Bool())
      val grantInstallSlot = Output(UInt(mshrBits.W))
    }

    // Final external TXDAT/TXRSP handshakes. A sidecar cannot release its
    // ReleaseBuf merely because MainPipe has enqueued the message.
    val txdatDone = Input(ValidIO(new TaskBundle))
    val txrspDone = Input(ValidIO(new TaskBundle))

    /* to request arbiter */
    // val mshrFull = Output(Bool())
    val mshrTask = DecoupledIO(new TaskBundle())

    /* status of s2 and s3 */
    val pipeStatusVec = Flipped(Vec(2, ValidIO(new PipeStatus)))

    /* send reqs */
    val toTXREQ = DecoupledIO(new CHIREQ())
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

    val releaseBufWriteId = Output(UInt(mshrBits.W))
    // A ProbeAckData can belong to a live snoop and a victim sidecar for the
    // same line. Keep the regular writer and expose the sidecar mirror as an
    // independent ReleaseBuf write port.
    val releaseBufWriteIdExtra = Output(ValidIO(UInt(mshrBits.W)))
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
    val nestedwbDataId = Output(ValidIO(UInt(mshrBits.W)))
    // An N+X overlap can require the same ReleaseData in both a snoop and a
    // delegated normal replacement buffer.
    val nestedwbDataIdExtra = Output(ValidIO(UInt(mshrBits.W)))
    // ReleaseData for the line currently being refilled supersedes the
    // normal context's earlier CompData/DS snapshot.
    val nestedwbRefillDataId = Output(ValidIO(UInt(mshrBits.W)))
    // Once ReleaseData replaces a normal context's payload, later beats from
    // its already-issued L3 read remain control responses only.
    val refillBufReleaseOwner = Output(UInt(mshrsAll.W))

    /* Independent normal, snoop, and replacement ownership views. */
    val refillInfo = Vec(mshrsAll, ValidIO(new MSHRInfo()))
    val snoopInfo = Vec(mshrsAll, ValidIO(new MSHRInfo()))
    val replaceInfo = Vec(mshrsAll, ValidIO(new ReplaceMSHRInfo))
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
  val mshrs: Seq[MSHRContextBase] = refillMshrs.zip(snoopMshrs).flatMap {
    case (refill, snoop) => Seq[MSHRContextBase](refill, snoop)
  }
  val contextPhysicalIds = Seq.tabulate(mshrsAll)(i => Seq(i, i)).flatten
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
  val replRespOwnerConflict = replRespSidecarConflict || replRespSnoopConflict
  val replRespParentOH = UIntToOH(io.replResp.bits.mshrId, mshrsAll)
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
  val replaceReserveValid = RegInit(VecInit(Seq.fill(mshrsAll)(false.B)))
  val replaceReserveSlot = Reg(Vec(mshrsAll, UInt(mshrBits.W)))
  val replaceReserveSlotOHByParent = Seq.tabulate(mshrsAll) { parent =>
    Mux(replaceReserveValid(parent), UIntToOH(replaceReserveSlot(parent), mshrsAll), 0.U(mshrsAll.W))
  }
  val replaceReservedSlotOH = replaceReserveSlotOHByParent.reduce(_ | _)
  assert(
    PopCount(replaceReservedSlotOH) === PopCount(replaceReserveValid.asUInt),
    "each pending replacer lookup must own one unique S/R reservation"
  )
  val replaceSlotUnreservedOH = VecInit((0 until mshrsAll).map { i =>
    !snoopMshrs(i).io.status.valid && !replaceMshrs(i).io.info.valid && !replaceReservedSlotOH(i)
  }).asUInt
  // A B request has priority over a new replacement reservation.  The
  // reservation is registered only at the next edge, so granting it in the
  // same cycle as a SnoopMSHR allocation would let both contexts select the
  // same ReleaseBuf slot.
  val snoopAllocRequest = io.fromMainPipe.mshr_alloc_s3.valid &&
    io.fromMainPipe.mshr_alloc_s3.bits.task.fromB
  val replaceReserveNeedParentOH = VecInit(refillMshrs.zipWithIndex.map {
    case (refill, i) => refill.io.replReadWanted && !replaceReserveValid(i)
  }).asUInt
  val replaceReserveGrantParentOH = PriorityEncoderOH(replaceReserveNeedParentOH)
  val replaceReserveGrantSlotOH = PriorityEncoderOH(replaceSlotUnreservedOH)
  val replaceReserveGrant = !snoopAllocRequest &&
    replaceReserveNeedParentOH.orR && replaceSlotUnreservedOH.orR
  assert(
    !(snoopAllocRequest && replaceReserveGrant),
    "a snoop allocation must preempt a new ReplaceMSHR reservation"
  )
  val replaceReserveAllowsRead = Seq.tabulate(mshrsAll) { parent =>
    replaceReserveValid(parent) || replaceReserveGrant && replaceReserveGrantParentOH(parent)
  }
  (0 until mshrsAll).foreach { parent =>
    val reserveResponse = io.replResp.valid && io.replResp.bits.mshrId === parent.U
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
  val replRespParentConflict = replRespHasVictim &&
    Mux1H(replRespParentOH, replaceActiveForParent)
  val replRespNeedsReplaceRaw = replRespHasVictim && !replRespOwnerConflict && !replRespParentConflict
  val replRespReservationOH = Mux1H(replRespParentOH, replaceReserveSlotOHByParent)
  val replRespNoReplaceSlot = replRespNeedsReplaceRaw && !replRespReservationOH.orR
  val replRespNeedsReplace = replRespNeedsReplaceRaw && !replRespNoReplaceSlot
  io.toMainPipe.replRespRetry := replRespSnoopConflict || replRespParentConflict || replRespNoReplaceSlot
  // A refill grant pass may merge the refill install when its folded
  // ReplacerResult selects a no-probe victim.  The victim/slot/ownership
  // side of that decision lives here (single source); MainPipe adds the
  // task identity and the MSHR pre-filters request-side corner cases.
  val replRespNoProbeVictim = io.replResp.valid && !io.replResp.bits.hit && !io.replResp.bits.retry &&
    (io.replResp.bits.meta.state === INVALID || !io.replResp.bits.meta.clients.orR)
  io.toMainPipe.grantInstallOK := replRespNoProbeVictim &&
    !replRespOwnerConflict && !replRespParentConflict &&
    (io.replResp.bits.meta.state === INVALID || replRespReservationOH.orR)
  io.toMainPipe.grantInstallSlot := OHToUInt(replRespReservationOH)
  when(replRespSnoopConflict) {
    assert(PopCount(replRespSnoopConflictOH) === 1.U, "replacement victim matches multiple active snoop contexts")
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
      orphanReleaseCandidate && m.io.info.valid && !m.io.info.bits.releaseAck &&
        m.io.info.bits.set === io.nestedwb.set && m.io.info.bits.tag === io.nestedwb.tag
    )
  ).asUInt
  when(orphanReleaseCandidate) {
    assert(PopCount(orphanReplaceTargetOH) <= 1.U, "orphan ReleaseData matches multiple active ReplaceMSHRs")
  }
  val orphanReleaseTargetsReplace = orphanReplaceTargetOH.orR
  val orphanReleaseData = orphanReleaseCandidate && !orphanReleaseTargetsReplace
  val replaceNeedsAlloc = replRespNeedsReplace || nestedReleaseNeedsReplace || orphanReleaseData
  val replaceParentOH = Mux(replRespNeedsReplace, replRespParentOH, nestedReleaseParentOH)
  val replaceParentId = OHToUInt(replaceParentOH)
  assert(
    PopCount(Cat(replRespNeedsReplace, nestedReleaseNeedsReplace, orphanReleaseData)) <= 1.U,
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
    incomingVictim.tag := io.replResp.bits.tag
    incomingVictim.set := io.replResp.bits.set
    incomingVictim.way := io.replResp.bits.way
    incomingVictim.meta := io.replResp.bits.meta
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
  incomingReplaceReq.parentEpoch := Mux1H(replaceParentOH, refillMshrs.map(_.io.allocEpoch))
  incomingReplaceReq.parentAttached := !orphanReleaseData
  incomingReplaceReq.dirResult := incomingVictim
  incomingReplaceReq.mode := ReplaceMSHRMode.victim
  incomingReplaceReq.needProbe := replRespNeedsReplace && incomingVictim.meta.clients.orR
  // A victim replacement must revoke every upper copy before its old data is
  // released to L3.  ReplaceRequest is zero-initialized, and zero is toT.
  incomingReplaceReq.probeParam := toN
  // When the parent's grant pass merges the install this very cycle, it also
  // performs the victim DS snapshot (ReleaseBuf write lands at its S5), so
  // the new sidecar must not issue a second capture of its own.
  incomingReplaceReq.captureFromDS := replRespNeedsReplace && !io.fromMainPipe.grantInstallDone.valid
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
  val replaceSlotIdle = replaceSlotUnreservedOH &
    ~Mux(replaceReserveGrant, replaceReserveGrantSlotOH, 0.U(mshrsAll.W))
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
  // A parent that still owns a child ReplaceMSHR must not allocate a second
  // one: the child's victim data / ReleaseBuf is still live, and the parent
  // only re-queries the replacer after the child's done pulse. Guard both the
  // allocation and the replacer response.
  val parentAlreadyHasChild = Mux1H(replaceParentOH, replaceActiveForParent)
  val replaceAllocValid = replaceNeedsAlloc && selectedReplaceSlotOH.orR &&
    (orphanReleaseData || !parentAlreadyHasChild)
  assert(
    !(replaceNeedsAlloc && !orphanReleaseData && parentAlreadyHasChild),
    "replacer response for a parent that already owns a child ReplaceMSHR"
  )
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
    snoopMshrs(i).io.status.valid || replaceMshrs(i).io.info.valid || replaceReservedSlotOH(i)
  }))
  val normalMshrFull = normalContextCount + pipeContextCount >= mshrsAll.U
  val snoopMshrFull = srContextCount + pipeContextCount >= mshrsAll.U
  val mshrFull = snoopMshrFull
  val a_mshrFull = normalMshrFull
  val mshrSelector = Module(new MSHRSelector())
  val selectedMSHROH = mshrSelector.io.out.bits

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
  val bToNFire = io.fromReqArb.status_s1.bFire && io.fromReqArb.status_s1.bToN
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
        pendingToNSet(i) := io.fromReqArb.status_s1.b_set
        pendingToNTag(i) := io.fromReqArb.status_s1.b_tag
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
  // Compute snoop candidates without treating a pending reservation as an
  // active ReplaceMSHR.  If a B task was already in MainPipe before the
  // reservation became visible, it may have no unreserved candidate left;
  // snoop priority then allows it to steal a reservation, and the replacer
  // response is retried after the reservation is cancelled below.
  val snoopEligibleNoReserveOH = VecInit(refillMshrs.zip(snoopMshrs).zipWithIndex.map {
    case ((refill, snoop), i) =>
      !snoop.io.status.valid &&
        !replaceMshrs(i).io.info.valid &&
        !(replaceAllocValid && selectedReplaceSlotOH(i)) &&
        (!refill.io.status.valid ||
          !refill.io.status.bits.isCmo &&
            !refill.io.status.bits.w_c_resp &&
            !refill.io.status.bits.releaseDataWindow)
  }).asUInt
  val snoopUnreservedEligibleOH = snoopEligibleNoReserveOH & ~replaceReservedSlotOH
  val snoopStealReserveOH = Mux(
    allocIsSnoop && !snoopUnreservedEligibleOH.orR,
    snoopEligibleNoReserveOH & replaceReservedSlotOH,
    0.U(mshrsAll.W)
  )
  mshrSelector.io.idle := refillMshrs.zip(snoopMshrs).zipWithIndex.map {
    case ((refill, snoop), i) =>
      Mux(
        allocIsSnoop,
        snoopUnreservedEligibleOH(i) || snoopStealReserveOH(i),
        !refill.io.status.valid &&
          (!allocIsCmo || !snoop.io.status.valid)
      )
  }
  io.toMainPipe.mshr_alloc_ptr := OHToUInt(selectedMSHROH)
  val snoopStealReservation = io.fromMainPipe.mshr_alloc_s3.valid &&
    allocIsSnoop && (selectedMSHROH & replaceReservedSlotOH).orR
  when(snoopStealReservation) {
    replaceReserveValid.zipWithIndex.foreach { case (valid, parent) =>
      when((replaceReserveSlotOHByParent(parent) & selectedMSHROH).orR) {
        valid := false.B
      }
    }
  }
  when(io.fromMainPipe.mshr_alloc_s3.valid) {
    assert(mshrSelector.io.out.valid, "MainPipe requested an MSHR allocation without an eligible physical slot")
  }
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
    io.sinkCReleaseData.valid && m.io.info.valid && !m.io.info.bits.releaseAck &&
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
    replRespNeedsReplace && replaceAllocValid &&
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
    Mux(activeReplaceProbeAckDataCapture, activeReplaceProbeAckDataOH, selectedReplaceSlotOH)
  when(activeReplaceProbeAckDataCapture) {
    assert(PopCount(activeReplaceProbeAckDataOH) === 1.U, "ProbeAckData matches multiple active ReplaceMSHRs")
  }
  val replaceDelegatedForParent = Seq.tabulate(mshrsAll) { parent =>
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
      val tag = Mux(status.needsRepl, status.metaTag, status.reqTag)
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
  val nestedwbNormalActive = refillMshrs.map(_.io.nestedwbData)
  val nestedwbSnoopActive = snoopMshrs.map(_.io.nestedwbData)
  refillMshrs.zip(snoopMshrs).zipWithIndex.foreach {
    case ((refill, snoop), i) =>
      when(refill.io.nestedwbData && replaceTargetMatches(i).orR) {
        val targetInfo = Mux1H(replaceTargetMatches(i), replaceMshrs.map(_.io.info.bits))
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
          active && replaceTargetMatches(parentId)(replaceId)
      }.reduce(_ || _)
  }

  /* Port connection of physical MSHR entries and their two contexts. */
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

      refill.io.resps.rxdat.valid := refill.io.status.valid && io.resps.rxdat.valid && io.resps.rxdat.mshrId === i.U
      refill.io.resps.rxdat.bits := io.resps.rxdat.respInfo
      refill.io.resps.rxrsp.valid := refill.io.status.valid && io.resps.rxrsp.valid && (io.resps.rxrsp.mshrId === i.U)
      refill.io.resps.rxrsp.bits := io.resps.rxrsp.respInfo
      refill.io.replResp.valid := io.replResp.valid && io.replResp.bits.mshrId === i.U
      val replRespForRefill = WireInit(io.replResp.bits)
      when(replRespOwnerConflict || replRespParentConflict || replRespNoReplaceSlot) {
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
      refill.io.replaceActive := replaceDelegatedForParent(i)
      // A nested B request reaches MainPipe S3 two cycles before its
      // SnoopMSHR becomes valid. Treat the S1 B candidate as a transient
      // snoop owner until allocation has caught up, so SourceD is deferred
      // until the snoop ordering point is known.
      // A normal MSHR owns its request line only.  `metaTag` is a ReplaceMSHR
      // victim while `needsRepl` is set, so a B.toN for that victim must stay
      // on the replacement/snoop path and must not cancel this normal refill.
      val pendingSnoopForRefill = io.fromReqArb.status_s1.bValid &&
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
      refill.io.deferGrant := snoopMshrs(i).io.status.valid ||
      pendingSnoopForRefill || nestedBToNForRefill
      refill.io.releaseDataBeforeProbeAck := releaseDataBeforeProbeAck
      refill.io.postGrantReleaseData := postGrantReleaseData
      refill.io.releaseDataDirectWrite := releaseDataDirectWrite
      // The matching snoop can be in a different temporary allocation phase
      // in the cycle before its `status` register becomes valid. Include that
      // allocation pulse so the normal cannot retire or commit around it.
      val postGrantReleaseSnoopActive = snoopMshrs.map { candidate =>
        val activeSameLine = candidate.io.status.valid &&
          candidate.io.status.bits.set === refill.io.status.bits.set &&
          candidate.io.status.bits.reqTag === refill.io.status.bits.reqTag
        val allocatingSameLine = candidate.io.alloc.valid &&
          candidate.io.alloc.bits.task.set === refill.io.status.bits.set &&
          candidate.io.alloc.bits.task.tag === refill.io.status.bits.reqTag
        activeSameLine || allocatingSameLine
      }.reduce(_ || _)
      refill.io.postGrantReleaseSnoopActive := refill.io.postGrantReleaseHold &&
        postGrantReleaseSnoopActive
      val postGrantReleaseSnoopToN = snoopMshrs.map { candidate =>
        candidate.io.postGrantReleaseResponseToN && candidate.io.status.valid &&
          candidate.io.status.bits.set === refill.io.status.bits.set &&
          candidate.io.status.bits.reqTag === refill.io.status.bits.reqTag
      }.reduce(_ || _)
      val postGrantReleaseSnoopToB = snoopMshrs.map { candidate =>
        candidate.io.postGrantReleaseResponseToB && candidate.io.status.valid &&
          candidate.io.status.bits.set === refill.io.status.bits.set &&
          candidate.io.status.bits.reqTag === refill.io.status.bits.reqTag
      }.reduce(_ || _)
      val postGrantReleaseSnoopToClean = snoopMshrs.map { candidate =>
        candidate.io.postGrantReleaseResponseToClean && candidate.io.status.valid &&
          candidate.io.status.bits.set === refill.io.status.bits.set &&
          candidate.io.status.bits.reqTag === refill.io.status.bits.reqTag
      }.reduce(_ || _)
      refill.io.postGrantReleaseSnoopToN := postGrantReleaseSnoopToN
      refill.io.postGrantReleaseSnoopToB := postGrantReleaseSnoopToB
      refill.io.postGrantReleaseSnoopToClean := postGrantReleaseSnoopToClean
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
      refill.io.refillWriteDone := io.fromMainPipe.refillWriteDone.valid &&
      io.fromMainPipe.refillWriteDone.bits === i.U
      refill.io.grantSent := io.fromMainPipe.grantSent.valid &&
      io.fromMainPipe.grantSent.bits === i.U
      snoop.io.replaceActive := false.B
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
      snoop.io.normalRefillPayloadFromProbeAck :=
        pairedProbeAckDataMirrored ||
        (refill.io.refillPayloadFromProbeAck && refill.io.status.valid && snoop.io.status.valid &&
        refill.io.status.bits.set === snoop.io.status.bits.set &&
        refill.io.status.bits.reqTag === snoop.io.status.bits.reqTag)
      // Post-Grant pending/payload are address-matched across physical slots.
      // Affinity is no longer required: ReleaseBuf[S] receives a nestedwb
      // dual-write or a late RefillBuf[N]->ReleaseBuf[S] copy.
      val snoopLineSet = Mux(snoop.io.status.valid, snoop.io.status.bits.set,
        snoop.io.alloc.bits.task.set)
      val snoopLineTag = Mux(snoop.io.status.valid, snoop.io.status.bits.reqTag,
        snoop.io.alloc.bits.task.tag)
      val snoopLookingForPair = snoop.io.status.valid || snoop.io.alloc.valid
      val postGrantPairOH = VecInit(refillMshrs.map { candidate =>
        snoopLookingForPair && candidate.io.status.valid &&
          candidate.io.postGrantReleaseHold &&
          candidate.io.status.bits.set === snoopLineSet &&
          candidate.io.status.bits.reqTag === snoopLineTag
      }).asUInt
      snoop.io.normalRefillPostGrantReleasePending := postGrantPairOH.orR
      snoop.io.normalRefillPayloadFromPostGrantRelease :=
        VecInit(refillMshrs.zipWithIndex.map { case (candidate, j) =>
          postGrantPairOH(j) && candidate.io.postGrantReleasePayloadReady
        }).asUInt.orR
      snoop.io.pairedNormalId.valid := postGrantPairOH.orR
      snoop.io.pairedNormalId.bits := OHToUInt(postGrantPairOH)
      // MainPipe S5 pulses refillToReleaseCopyWrite with the destination snoop id.
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
        mirroredReplaceProbeAckDataCapture && mirroredReplaceProbeAckDataOH(i)
      // Client-less victims are captured from DS by the replace capture task;
      // MainPipe pulses replaceCaptureWrite with the replace slot id.
      val replaceCaptureMatch = io.fromMainPipe.replaceCaptureWrite.valid &&
        io.fromMainPipe.replaceCaptureWrite.bits === i.U
      val orphanReplaceCapture = orphanReleaseTargetsReplace && orphanReplaceTargetOH(i)
      replaceMshrs(i).io.victimDataCaptured :=
        replaceProbeAckDataCapture || replaceNestedwbCapture(i) || replaceCaptureMatch ||
        (orphanReleaseData && replaceAllocValid && selectedReplaceSlotOH(i)) ||
        mirroredReplaceProbeAckDataForSlot || orphanReplaceCapture
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
      replaceMshrs(i).io.snoopStarted := io.fromReqArb.status_s1.bFire &&
      replaceMshrs(i).io.info.valid &&
      !replaceMshrs(i).io.info.bits.copyBackIssued &&
      replaceMshrs(i).io.info.bits.set === io.fromReqArb.status_s1.b_set &&
      replaceMshrs(i).io.info.bits.tag === io.fromReqArb.status_s1.b_tag
      val snoopDone = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.fromB && done.bits.snpHitRelease &&
        done.bits.snpHitReleaseIdx === i.U
      }.reduce(_ || _)
      replaceMshrs(i).io.snoopDone := snoopDone
      val snoopToInvalDone = Seq(io.txdatDone, io.txrspDone).map { done =>
        done.valid && done.bits.fromB && done.bits.snpHitRelease &&
        done.bits.snpHitReleaseToInval && done.bits.snpHitReleaseIdx === i.U
      }.reduce(_ || _)
      replaceMshrs(i).io.snoopToInvalDone := snoopToInvalDone
      replaceMshrs(i).io.copyBackDone := io.txdatDone.valid &&
      io.txdatDone.bits.mshrTask && io.txdatDone.bits.mshrContext === 2.U &&
      io.txdatDone.bits.mshrId === i.U && io.txdatDone.bits.chiOpcode.get === CopyBackWrData
      replaceMshrs(i).io.rxrsp.valid := io.resps.rxrsp.valid && io.resps.rxrsp.mshrId === replaceMshrs(i).io.txnId
      replaceMshrs(i).io.rxrsp.bits := io.resps.rxrsp.respInfo
      // Victim-mode writeback requests now leave on the direct TXREQ path
      // (the txreq arbiter below includes the replaceMshrs).  Only the snoop
      // response output stays unused.
      replaceMshrs(i).io.tasks.txrsp.ready := false.B

      snoop.io.resps.rxdat.valid := false.B
      snoop.io.resps.rxdat.bits := io.resps.rxdat.respInfo
      snoop.io.resps.rxrsp.valid := false.B
      snoop.io.resps.rxrsp.bits := io.resps.rxrsp.respInfo
      snoop.io.replResp.valid := false.B
      snoop.io.replResp.bits := io.replResp.bits
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
      io.pCrd(i) <> refill.io.pCrd
  }
  io.toMainPipe.snoopDataHolds := io.snoopInfo
  io.toMainPipe.postGrantReleaseHolds := io.refillInfo
  io.toMainPipe.normalRefillLateSnoopToN :=
    VecInit(refillMshrs.map(_.io.postGrantReleaseSnoopOutcomeToN))
  io.toMainPipe.normalRefillLateSnoopToB :=
    VecInit(refillMshrs.map(_.io.postGrantReleaseSnoopOutcomeToB))
  io.toMainPipe.normalRefillLateSnoopToClean :=
    VecInit(refillMshrs.map(_.io.postGrantReleaseSnoopOutcomeToClean))
  val sharedReleaseBufOwners = VecInit(snoopMshrs.zip(replaceMshrs).map {
    case (snoop, replace) => snoop.io.status.valid && replace.io.info.valid
  }).asUInt
  assert(!sharedReleaseBufOwners.orR,
    "SnoopMSHR and ReplaceMSHR must not share a ReleaseBuf slot")

  // A normal slot can be reused before its detached child finishes writeback.
  // Release only the parent association at the normal MSHR's actual retire
  // edge; the child remains fully visible as an independent victim context.
  replaceMshrs.foreach { replace =>
    val parentReleaseMatches = VecInit(refillMshrs.zipWithIndex.map {
      case (refill, parentId) =>
        refill.io.replaceParentRelease && replace.io.info.valid && replace.io.info.bits.parentAttached &&
          replace.io.info.bits.parentId === parentId.U &&
          replace.io.info.bits.parentEpoch === refill.io.allocEpoch
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
    snoop.io.msInfo.valid && snoop.io.msInfo.bits.blocksSnoop && !snoop.io.msInfo.bits.willFree &&
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
    (io.fromReqArb.status_s1.bToN && pendingToNFull)
  io.toReqArb.blockA_s1 := a_mshrFull || aSidecarConflict || aSnoopConflict ||
    aPendingToNConflict || aEnteringToNConflict
  io.toReqArb.blockG_s1 := false.B

  /* Acquire downwards to TXREQ*/
  // Normal reads (latency-critical) and victim writeback requests share the
  // direct MSHR->TXREQ path; writeback control is control-only and needs no
  // MainPipe pass.
  ArbPerf(twoLevelArb(refillMshrs.map(_.io.tasks.txreq) ++ replaceMshrs.map(_.io.tasks.txreq), io.toTXREQ, Some("txreq")), "txreq_arb")

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
  val cbSelOH = Reg(UInt(mshrsAll.W))
  val cbTaskReg = Reg(new TaskBundle)
  val cbRrGrantMask = RegInit(0.U(mshrsAll.W))
  val cbRrSelOH = MaskToOH(cbRrGrantMask & directCbValids.asUInt)
  val cbPickOH = Mux((cbRrSelOH & directCbValids.asUInt).orR, cbRrSelOH, MaskToOH(directCbValids.asUInt))
  val cbPick = !cbPresent && directCbValids.asUInt.orR && io.txdatDirectSpace
  when (cbPick) {
    cbPresent := true.B
    cbSelOH := cbPickOH
    cbTaskReg := Mux1H(cbPickOH, replaceMshrs.map(_.io.tasks.txdat.bits))
  }
  // ReleaseBuf resp2 is registered and holds until the next read, so the
  // data is stable for the whole present window.
  io.releaseBufDirectRd.valid := cbPick
  io.releaseBufDirectRd.bits.id := OHToUInt(cbPickOH)
  val cbSelStillValid = Mux1H(cbSelOH, directCbValids)
  io.toTXDAT.valid := cbPresent && cbSelStillValid
  io.toTXDAT.bits.task := cbTaskReg
  io.toTXDAT.bits.data := io.releaseBufDirectResp.data
  when (cbPresent && !cbSelStillValid) {
    // Preempted by a nested snoop before TXDAT acceptance: drop the staged
    // request; the sidecar re-presents with refreshed state afterwards.
    cbPresent := false.B
  }
  when (io.toTXDAT.fire) {
    cbPresent := false.B
    cbRrGrantMask := VecInit((0 until mshrsAll).map { i =>
      if (i == 0) false.B else cbSelOH(i - 1, 0).orR
    }).asUInt
  }
  replaceMshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.tasks.txdat.ready := cbPresent && cbSelOH(i) && cbSelStillValid && io.toTXDAT.ready
  }
  XSPerfAccumulate("txdat_direct_copyback_cnt", io.toTXDAT.fire)
  XSPerfAccumulate("txdat_direct_abort_cnt", cbPresent && !cbSelStillValid)
  XSPerfAccumulate("txdat_direct_wait_sum",
    PopCount(Cat(replaceMshrs.map(m => m.io.tasks.txdat.valid && !m.io.tasks.txdat.ready))))

  /* Response downwards to TXRSP*/
  val txrspArb = twoLevelArb(mshrs.map(_.io.tasks.txrsp), io.toTXRSP, Some("txrsp"))
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
  val sourceBTaskCandidates = (sourceBTasks ++ replaceSourceBTasks).map { task =>
    val sameLineProbeOutstanding = sourceBOutstanding.map {
      case (valid, set, tag) =>
        valid && set === task.bits.set && tag === task.bits.tag
    }.reduce(_ || _)
    val guardedTask = Wire(Decoupled(new SourceBReq))
    guardedTask.valid := task.valid && !sameLineProbeOutstanding
    guardedTask.bits := task.bits
    task.ready := guardedTask.ready && !sameLineProbeOutstanding
    guardedTask
  }
  val sourceB = Module(new SourceB())
  ArbPerf(twoLevelArb(sourceBTaskCandidates, sourceB.io.task, Some("source_b")), "source_b_arb")
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
  val mshrTaskSources = mshrs.map(_.io.tasks.mainpipe) ++ replaceMshrs.map(_.io.tasks.mainpipe)
  val nMshrTaskSrc = mshrTaskSources.size
  def isGrantOp(op: UInt) = op === TLMessages.Grant || op === TLMessages.GrantData ||
    op === TLMessages.AccessAckData || op === TLMessages.HintAck
  val mshrTaskClasses = mshrTaskSources.map { t =>
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
  val mshrTaskWaitCnt = mshrTaskSources.map { t =>
    val cnt = RegInit(0.U(6.W))
    when(t.fire) {
      cnt := 0.U
    }.elsewhen(t.valid && !t.ready) {
      cnt := Mux(cnt === 63.U, 63.U, cnt + 1.U)
    }
    cnt
  }
  val mshrTaskEffClasses = mshrTaskClasses.zip(mshrTaskWaitCnt).map { case (c, w) =>
    Mux(w === 63.U, 0.U(3.W), c)
  }
  val mshrTaskSrcValids = VecInit(mshrTaskSources.map(_.valid)).asUInt
  val mshrTaskClassMasks = VecInit((0 until 5).map { c =>
    VecInit((0 until nMshrTaskSrc).map(i => mshrTaskSrcValids(i) && mshrTaskEffClasses(i) === c.U)).asUInt
  })
  val mshrTaskBestClass = PriorityEncoder(VecInit(mshrTaskClassMasks.map(_.orR)).asUInt)
  val mshrTaskCandidates = VecInit((0 until nMshrTaskSrc).map { i =>
    mshrTaskSrcValids(i) && mshrTaskEffClasses(i) === mshrTaskBestClass
  }).asUInt
  // round-robin within the best class (same scheme as FastArbiter)
  val mshrTaskChosenOH = Wire(UInt(nMshrTaskSrc.W))
  val mshrTaskRrGrantMask = RegEnable(VecInit((0 until nMshrTaskSrc).map { i =>
    if (i == 0) false.B else mshrTaskChosenOH(i - 1, 0).orR
  }).asUInt, 0.U(nMshrTaskSrc.W), mshrTask.fire)
  val mshrTaskRrSelOH = MaskToOH(mshrTaskRrGrantMask & mshrTaskCandidates)
  val mshrTaskFirstOneOH = MaskToOH(mshrTaskCandidates)
  val mshrTaskRrValid = (mshrTaskRrSelOH & mshrTaskCandidates).orR
  mshrTaskChosenOH := Mux(mshrTaskRrValid, mshrTaskRrSelOH, mshrTaskFirstOneOH)
  mshrTask.valid := mshrTaskCandidates.orR
  mshrTask.bits := Mux1H(mshrTaskChosenOH, mshrTaskSources.map(_.bits))
  mshrTaskSources.zip(mshrTaskChosenOH.asBools).foreach { case (s, g) =>
    s.ready := g && mshrTask.ready
  }
  val mshrTaskChosen = OHToUInt(mshrTaskChosenOH)
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
  // The arbiter input list contains two contexts per physical entry followed
  // by the replace sidecars. RequestArb and all refill/release buffers still
  // use the physical MSHR index, so translate the selected context index here.
  val selectedTaskPhysicalId = Wire(UInt(mshrBits.W))
  when(mshrTaskChosen < (2 * mshrsAll).U) {
    selectedTaskPhysicalId := mshrTaskChosen >> 1
  }.otherwise {
    selectedTaskPhysicalId := mshrTaskChosen - (2 * mshrsAll).U
  }
  io.mshrTask <> mshrTask
  io.mshrTask.bits.mshrId := selectedTaskPhysicalId
  io.mshrTask.bits.qosClass := Mux1H(mshrTaskChosenOH, mshrTaskEffClasses)

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
  io.releaseBufWriteIdExtra.valid := mirroredReplaceProbeAckDataCapture
  io.releaseBufWriteIdExtra.bits := OHToUInt(mirroredReplaceProbeAckDataOH)
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

  /* Nest writeback check */
  // A nested writeback follows the parent normal MSHR's address match, then
  // translates to the delegated ReplaceMSHR's physical ReleaseBuf slot.
  val nestedwbNormalTargetId = nestedwbNormalActive.zipWithIndex.map {
    case (_, parentId) =>
      Mux(replaceTargetMatches(parentId).orR, OHToUInt(replaceTargetMatches(parentId)), parentId.U)
  }
  val nestedwbSnoopTargetId = (0 until mshrsAll).map(_.U(mshrBits.W))
  // An orphan ReleaseData has no normal/snoop owner, but its payload is still
  // carried by MainPipe.nestedwbData. Route it either to a newly allocated
  // ReplaceMSHR or to the matching detached sidecar; merely marking that
  // sidecar captured would otherwise leave its ReleaseBuf stale.
  val orphanNestedwbWrite = orphanReleaseData && replaceAllocValid
  val orphanExistingReplaceWrite = orphanReleaseTargetsReplace
  // Keep normal and snoop matches separate here. Collapsing them per
  // physical MSHR loses the snoop ReleaseBuf write when a normal sidecar is
  // also active on that line.
  val nestedwbConsumers = nestedwbNormalActive ++ nestedwbSnoopActive
  val nestedwbConsumerTargetId = nestedwbNormalTargetId ++ nestedwbSnoopTargetId
  val nestedwbConsumerOH = VecInit(nestedwbConsumers).asUInt
  val nestedwbFirstOH = PriorityEncoderOH(nestedwbConsumerOH)
  val nestedwbSecondOH = PriorityEncoderOH(nestedwbConsumerOH & ~nestedwbFirstOH)
  io.nestedwbDataId.valid := nestedwbConsumerOH.orR || orphanNestedwbWrite || orphanExistingReplaceWrite
  io.nestedwbDataId.bits := Mux(
    orphanNestedwbWrite,
    OHToUInt(selectedReplaceSlotOH),
    Mux(orphanExistingReplaceWrite, OHToUInt(orphanReplaceTargetOH), Mux1H(nestedwbFirstOH, nestedwbConsumerTargetId))
  )
  io.nestedwbDataIdExtra.valid := nestedwbSecondOH.orR
  io.nestedwbDataIdExtra.bits := Mux1H(nestedwbSecondOH, nestedwbConsumerTargetId)
  io.nestedwbRefillDataId.valid := refillNestedwbActive.orR
  io.nestedwbRefillDataId.bits := OHToUInt(refillNestedwbActive)
  // A single ReleaseData can serve a normal replacement sidecar and an
  // independently allocated snoop context. Slice broadcasts it to their two
  // ReleaseBuf entries; any larger fan-out would violate the N+X ownership
  // model and needs an explicit protocol-level merge.
  assert(
    PopCount(nestedwbConsumers) + orphanNestedwbWrite + orphanExistingReplaceWrite <= 2.U,
    "nested ReleaseData has more than two ReleaseBuf consumers"
  )

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
