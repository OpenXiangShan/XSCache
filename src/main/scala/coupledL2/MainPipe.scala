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
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters
import xscache.coupledL2._
import xscache.coupledL2.prefetch.{PrefetchTrain, PfSource, CDPDetectTask, CDPParameters, ReplaceBundle}
import xscache.coupledL2.MetaData._
import xscache.chi.{CHIREQ, HasCHIOpcodes}
import xscache.chi.CHICohStates._

class MainPipe(implicit p: Parameters) extends CoupledL2Module with HasCHIOpcodes with HasPerfEvents {
  val io = IO(new Bundle() {
    /* receive task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new TaskBundle()))
    val taskLeaseToken_s2 = Input(new MainpipeLeaseToken)
    val taskSnoopReservationToken_s2 = Input(new SnoopSlotToken)
    val taskGrantReservationToken_s2 = Flipped(ValidIO(new GrantReservationToken))
    /* status from arbiter at stage1  */
    val mshrHintQInfo = Flipped(ValidIO(new TaskBundle))
    val sinkCHintQInfo = Flipped(ValidIO(new TaskBundle))

    /* handle set conflict in req arb */
    val fromReqArb = Input(new Bundle() {
      val status_s1 = new PipeEntranceStatus
    })
    /* block B and C at Entrance */
    val toReqArb = Output(new BlockInfo())

    /* block A at Entrance */
    val toReqBuf = Output(Vec(4, Bool()))
    val blockRefillOnlyCommit = Output(Bool())
    val setStatus = Vec(5, ValidIO(new PipeSetStatus))

    /* handle capacity conflict of GrantBuffer */
    val status_vec_toD = Vec(5, ValidIO(new PipeStatus))
    val grantReservationReturn = Vec(3, ValidIO(new GrantReservationToken))
    /* handle capacity conflict of TX channels */
    val status_vec_toTX = Vec(5, ValidIO(new PipeStatusWithCHI))

    /* get dir result at stage 3 */
    val dirResp_s3 = Input(new DirResult())
    val metaOnHit_s3 = Input(new MetaEntry())
    val errOnSnp_s3 = Input(Bool())
    val dirWayOH_s3 = Input(UInt(cacheParams.ways.W))
    val cmoHitInvalid = Input(Bool())
    val replResp = Flipped(ValidIO(new ReplacerResult()))
    val retryFastFwd_s2 = Input(Bool())
    val hint_s1QueueFull = Output(Bool())

    /* send task to MSHRCtl at stage 3 */
    val toMSHRCtl = new Bundle() {
      val mainpipeLeaseDone = Vec(3, ValidIO(new MainpipeLeaseDone))
      val mshr_alloc_s3 = ValidIO(new MSHRRequest())
      val snoopReservationDone = ValidIO(new SnoopSlotToken)
      val snoopDirectoryHit = Output(Bool())
      val normalReservationDone = ValidIO(new MainpipeLeaseDone)
      val replaceCaptureWrite = ValidIO(UInt(mshrBits.W))
      val refillToReleaseCopyWrite = ValidIO(UInt(mshrBits.W))
      val refillWriteDone = ValidIO(new MainpipeLeaseDone)
      val grantSent = ValidIO(new MainpipeLeaseDone)
      val releaseDataDirectWrite = ValidIO(new TaskBundle)
      val orphanReleaseReserveRequest = ValidIO(new OrphanReleaseSlotRequest)
      val orphanReleaseReserveDone = ValidIO(new OrphanReleaseSlotClaim)
      val orphanReleaseData = Output(Bool())
      val snoopToNDirectDone = ValidIO(new TaskBundle)
      val grantInstallDone = ValidIO(UInt(mshrBits.W))
    }

    val fromMSHRCtl = new Bundle() {
      val mshr_alloc_ptr = Input(UInt(mshrBits.W))
      val normalAllocAdmission = Input(Bool())
      val normalAllocToken = Input(new MainpipeLeaseToken)
      val normalAllocOwner = Input(UInt(mshrBits.W))
      val normalAllocSource = Input(UInt(sourceIdBits.W))
      val replRespRetry = Input(Bool())
      val postGrantReleaseLate = Flipped(ValidIO(new TaskBundle))
      val grantInstallOK = Input(Bool())
      val grantInstallSlot = Input(UInt(mshrBits.W))
      val replParentGeneration = Input(UInt(8.W))
      val orphanReleaseReservation = Input(new OrphanReleaseSlotClaim)
    }
    val refillInfo = Flipped(Vec(mshrsAll, ValidIO(new MSHRInfo)))
    val snoopInfo = Flipped(Vec(mshrsAll, ValidIO(new MSHRInfo)))

    /* read C-channel Release Data and write into DS */
    val bufResp = Input(new PipeBufferResp)

    /* get ReleaseBuffer and RefillBuffer read result */
    val refillBufResp_s3 = Flipped(ValidIO(new MSHRBufResp))
    val releaseBufResp_s3 = Flipped(ValidIO(new DSBlock))

    /* read or write data storage */
    val toDS = new Bundle() {
      val en_s3 = Output(Bool())
      val req_s3 = ValidIO(new DSRequest)
      val rdata_s5 = Input(new DSBlock)
      val wdata_s3 = Output(new DSBlock)
      val error_s5 = Input(Bool())
    }

    /* send Grant via SourceD channel */
    val toSourceD = DecoupledIO(new TaskWithData())

    /* send req/Comp/CompData via TXREQ/TXRSP/TXDAT channel */
    val toTXREQ = DecoupledIO(new CHIREQ())
    val toTXRSP = DecoupledIO(new TaskBundle())
    val toTXDAT = DecoupledIO(new TaskWithData())

    /* write dir, including reset dir */
    val metaWReq = ValidIO(new MetaWrite)
    val tagWReq = ValidIO(new TagWrite)

    val releaseBufWrite = ValidIO(new MSHRBufWrite())
    val refillBufWrite = ValidIO(new MSHRBufWrite())

    /* nested writeback */
    val nestedwb = Output(new NestedWriteback())
    val nestedwbData = Output(new DSBlock())

    /* l2 refill hint */
    val l1Hint = DecoupledIO(new L2ToL1HintInsideL2())

    /* send prefetchTrain to Prefetch to trigger a prefetch req */
    val prefetchTrain = prefetchOpt.map(_ => DecoupledIO(new PrefetchTrain))
    val replaceRecord = prefetchOpt.map(_ => Output(ValidIO(new ReplaceBundle)))

    /* top-down monitor */
    // TODO

    /* ECC error*/
    val error = ValidIO(new L2CacheErrorInfo)

    /* l2 flush (CMO All) */
    val cmoAllBlock = Option.when(cacheParams.enableL2Flush) (Input(Bool()))
    val cmoLineDone = Option.when(cacheParams.enableL2Flush) (Output(Bool()))

    /* to CDP for detection */
    val cdp_trigger = Option.when(hasCDP) (ValidIO(new CDPDetectTask(dataBits=blockBits)))
  })

  require(chiOpt.isDefined)

  val resetFinish = RegInit(false.B)
  val resetIdx = RegInit((cacheParams.sets - 1).U)
  /* block reqs when reset */
  when (!resetFinish) {
    resetIdx := resetIdx - 1.U
  }
  when (resetIdx === 0.U) {
    resetFinish := true.B
  }

  val txreq_s3, txreq_s4, txreq_s5 = WireInit(0.U.asTypeOf(io.toTXREQ.cloneType))
  val txrsp_s3, txrsp_s4, txrsp_s5 = Wire(io.toTXRSP.cloneType)
  val txdat_s3, txdat_s4, txdat_s5 = Wire(io.toTXDAT.cloneType)
  val d_s3, d_s4, d_s5 = Wire(io.toSourceD.cloneType)

  /* ======== Stage 2 ======== */
  val task_s2 = io.taskFromArb_s2
  val reqWayOH_s2 = UIntToOH(task_s2.bits.way, cacheParams.ways)

  /* ======== Stage 3 ======== */
  val task_s3_capture = RegInit(0.U.asTypeOf(Valid(new TaskBundle)))
  task_s3_capture.valid := task_s2.valid
  when(task_s2.valid) { task_s3_capture.bits := task_s2.bits }
  val task_s3_decide = RegNext(task_s3_capture, 0.U.asTypeOf(task_s3_capture))
  val leaseToken_s3_capture = RegEnable(io.taskLeaseToken_s2, task_s2.valid)
  val leaseToken_s3_decide = RegEnable(leaseToken_s3_capture, task_s3_capture.valid)
  val snoopSlotToken_s3_capture = RegEnable(io.taskSnoopReservationToken_s2, task_s2.valid)
  val snoopSlotToken_s3_decide = RegEnable(snoopSlotToken_s3_capture, task_s3_capture.valid)
  val grantToken_s3_capture = RegEnable(io.taskGrantReservationToken_s2, task_s2.valid)
  val grantToken_s3_decide = RegEnable(grantToken_s3_capture, task_s3_capture.valid)
  when(task_s2.valid && (task_s2.bits.fromA || task_s2.bits.fromC)) {
    assert(io.taskGrantReservationToken_s2.valid &&
      io.taskGrantReservationToken_s2.bits.channel === task_s2.bits.channel,
      "an A/C pipeline task must retain its admission GrantBuffer token")
  }
  // Capture the complete aligned result, decide once, and apply only the retained decision record.
  class NormalS3Action extends Bundle {
    val allocation = new MSHRRequest
    val needMshr = Bool()
    val needAcquire = Bool()
    val acquireOnHit = Bool()
    val acquireOnMiss = Bool()
    val needProbe = Bool()
    val needRelease = Bool()
    val cmo = Bool()
    val cmoHitInvalid = Bool()
    val cmoAllBlock = Bool()
    val alias = Bool()
    val response = new TaskBundle
    val directSourceD = Bool()
    val readData = Bool()
    val captureRefill = Bool()
    val metaWrite = Bool()
    val meta = new MetaEntry
  }
  def decideNormal(task: TaskBundle, dir: DirResult, meta: MetaEntry,
      invalidCmoWay: Bool): NormalS3Action = {
    val action = WireInit(0.U.asTypeOf(new NormalS3Action))
    val fromA = !task.mshrTask && task.fromA
    val acq = fromA && (task.opcode === AcquireBlock || task.opcode === AcquirePerm)
    val prefetch = fromA && task.opcode === Hint
    val get = fromA && task.opcode === Get
    val cmoMeta = Mux(task.cmoAll, dir.meta, meta)
    val cmoHit = dir.hit && isValid(cmoMeta.state)
    val clean = fromA && task.opcode === CBOClean
    action.cmoAllBlock := io.cmoAllBlock.getOrElse(false.B)
    action.cmoHitInvalid := action.cmoAllBlock && invalidCmoWay
    val flush = fromA && task.opcode === CBOFlush
    val inval = fromA && task.opcode === CBOInval
    val wantsT = needT(task.opcode, task.param)
    val promoteT = dir.hit && isT(meta.state)
    action.cmo := clean || flush || inval
    action.alias := acq && dir.hit && meta.clients(0) &&
      meta.alias.getOrElse(0.U) =/= task.alias.getOrElse(0.U)
    action.acquireOnHit := meta.state === BRANCH && wantsT && !prefetch
    action.acquireOnMiss := acq || prefetch || get
    action.needAcquire := task.fromA && (Mux(dir.hit,
      action.acquireOnHit, action.acquireOnMiss) || action.cmo)
    action.needProbe := dir.hit && meta.clients.orR && get && meta.state === TRUNK ||
      cmoHit && cmoMeta.clients.orR && (clean && cmoMeta.state === TRUNK || flush || inval)
    action.needRelease := cmoHit && (clean && (action.needProbe || cmoMeta.dirty) || flush || inval)
    action.needMshr := action.needAcquire || action.needProbe || action.alias

    action.allocation.dirResult := dir
    action.allocation.dirResult.meta := Mux(task.cmoAll, dir.meta, meta)
    action.allocation.task := task
    action.allocation.task.bufIdx := 0.U
    action.allocation.task.mshrTask := false.B
    action.allocation.task.aliasTask.foreach(_ := action.alias)
    action.allocation.task.wayMask := 0.U
    val initial = action.allocation.state
    initial.elements.foreach(_._2 := true.B)
    when(task.fromA) {
      initial.s_refill := action.cmo
      initial.w_replResp := action.cmo || dir.hit
      when(action.needAcquire) {
        initial.s_acquire := action.cmoAllBlock
        initial.s_rcompack.get := action.cmo
        initial.w_grantfirst := action.cmoAllBlock
        initial.w_grantlast := action.cmoAllBlock
        initial.w_grant := action.cmoAllBlock
      }
      when(action.alias || action.needProbe && !(action.cmo && action.needRelease)) {
        initial.s_rprobe := false.B
        initial.w_rprobeackfirst := false.B
        initial.w_rprobeacklast := false.B
      }
      when(action.needRelease) {
        initial.s_release := false.B
        initial.w_releaseack := false.B
      }
    }
    action.response := task
    action.response.mshrId := (1 << (mshrBits - 1)).U + task.sourceId
    action.response.opcode := odOpGen(task.opcode)
    action.response.param := Mux(acq, Mux(task.param === NtoB && !promoteT, toB, toT), 0.U)
    action.directSourceD := fromA && !action.needMshr && !action.response.opcode(0) &&
      task.opcode =/= Hint && !action.cmoAllBlock
    action.readData := dir.hit && (get || fromA && task.opcode === AcquireBlock)
    action.captureRefill := (action.needProbe || action.alias) && action.readData
    action.metaWrite := fromA && !action.needMshr && !get && !prefetch && !action.cmo
    val alias = Mux(get || prefetch, meta.alias.getOrElse(0.U), task.alias.getOrElse(0.U))
    action.meta := MetaEntry(
      dirty = meta.dirty,
      state = Mux(wantsT || promoteT, TRUNK, meta.state),
      clients = Fill(clientBits, !dir.error),
      alias = Some(alias),
      pfsrc = dir.meta.prefetchSrc.getOrElse(0.U),
      accessed = true.B,
      tagErr = meta.tagErr,
      dataErr = meta.dataErr,
      cdpPfDepth = 0.U)
    action
  }
  class S3Decision extends Bundle {
    val task = new TaskBundle
    val token = new MainpipeLeaseToken
    val orphanReleaseSequence = UInt(8.W)
    val snoopSlotToken = new SnoopSlotToken
    val grantToken = Valid(new GrantReservationToken)
    val dir = new DirResult
    val repl = Valid(new ReplacerResult)
    val metaOnHit = new MetaEntry
    val errOnSnp = Bool()
    val wayOH = UInt(cacheParams.ways.W)
    val cPayload = new PipeBufferResp
    val refill = Valid(new MSHRBufResp)
    val release = Valid(new DSBlock)
    val retry = Bool()
    val normalRetry = Bool()
    val grantInstall = Bool()
    val grantInstallSlot = UInt(mshrBits.W)
    val parentGeneration = UInt(8.W)
    val postGrantReleaseSnoop = Bool()
    val normalRefillOverlapsSnoopHold = Bool()
    val bNeedProbe = Bool()
    val bNeedDct = Bool()
    val bNeedMshr = Bool()
    val bAllocState = new FSMState()
    val bDoFwd = Bool()
    val bDoRespData = Bool()
    val bFwdPassDirty = Bool()
    val bFwdCacheState = UInt(3.W)
    val bRespCacheState = UInt(3.W)
    val bMetaWValid = Bool()
    val bMeta = new MetaEntry
    val cWen = Bool()
    val cMetaValid = Bool()
    val cMeta = new MetaEntry
    val cOrphanReleaseData = Bool()
    val cReleaseDataDirectWrite = Bool()
    val normal = new NormalS3Action
  }
  val s3Decision = RegInit(0.U.asTypeOf(Valid(new S3Decision)))
  val retryDecision = io.replResp.bits.validHold &&
    (io.replResp.bits.retry || io.fromMSHRCtl.replRespRetry)
  val normalDecision = decideNormal(task_s3_decide.bits,
    io.dirResp_s3, io.metaOnHit_s3, io.cmoHitInvalid)
  val postGrantReleaseSnoop_decide = task_s3_decide.bits.fromB &&
    io.refillInfo.map { holder =>
      holder.valid && holder.bits.postGrantReleaseHold && holder.bits.refillCommitPending &&
        holder.bits.set === task_s3_decide.bits.set && holder.bits.reqTag === task_s3_decide.bits.tag
    }.reduce(_ || _)
  val postGrantReleaseLate_decide = task_s3_decide.bits.fromB &&
    io.fromMSHRCtl.postGrantReleaseLate.valid &&
    io.fromMSHRCtl.postGrantReleaseLate.bits.set === task_s3_decide.bits.set &&
    io.fromMSHRCtl.postGrantReleaseLate.bits.tag === task_s3_decide.bits.tag
  val refillDecision_decide = task_s3_decide.bits.mshrTask && task_s3_decide.bits.fromA && (
    task_s3_decide.bits.opcode === Grant || task_s3_decide.bits.opcode === GrantData ||
      task_s3_decide.bits.opcode === AccessAckData || task_s3_decide.bits.opcode === HintAck
  )
  val replRespForTask_decide = io.replResp.bits.validHold && refillDecision_decide &&
    io.replResp.bits.mshrId === task_s3_decide.bits.mshrId &&
    io.replResp.bits.meta.state =/= INVALID && !io.replResp.bits.retry
  val commitReplWay_decide = refillDecision_decide &&
    (task_s3_decide.bits.replTask || replRespForTask_decide)
  val normalRefillWay_decide = Mux(commitReplWay_decide,
    io.replResp.bits.way, task_s3_decide.bits.way)
  val normalRefillOverlapsSnoopHold_decide = refillDecision_decide &&
    io.snoopInfo.map { snoop =>
      snoop.valid && snoop.bits.blockRefill && snoop.bits.set === task_s3_decide.bits.set &&
        snoop.bits.way === normalRefillWay_decide
    }.reduce(_ || _)
  // Freeze B actions with aligned Directory metadata; downstream readiness must not enter this snapshot.
  val bNeedProbe_decide = task_s3_decide.bits.fromB && (
    (isSnpOnceX(task_s3_decide.bits.chiOpcode.get) ||
      isSnpQuery(task_s3_decide.bits.chiOpcode.get) ||
      isSnpStashX(task_s3_decide.bits.chiOpcode.get)) &&
      io.dirResp_s3.hit && io.metaOnHit_s3.state === TRUNK && io.metaOnHit_s3.clients.orR ||
    (isSnpToB(task_s3_decide.bits.chiOpcode.get) ||
      task_s3_decide.bits.chiOpcode.get === SnpCleanShared) &&
      io.dirResp_s3.hit && io.metaOnHit_s3.state === TRUNK && io.metaOnHit_s3.clients.orR ||
    (isSnpUniqueX(task_s3_decide.bits.chiOpcode.get) ||
      task_s3_decide.bits.chiOpcode.get === SnpCleanInvalid ||
      isSnpMakeInvalidX(task_s3_decide.bits.chiOpcode.get)) &&
      io.dirResp_s3.hit && io.metaOnHit_s3.clients.orR ||
    (io.errOnSnp_s3 || io.metaOnHit_s3.tagErr) && io.dirResp_s3.hit
  )
  val bNestableHit_decide = Mux(task_s3_decide.bits.snpHitRelease,
    task_s3_decide.bits.snpHitReleaseMeta.state =/= INVALID,
    io.dirResp_s3.hit)
  val bNestableTagErr_decide = Mux(task_s3_decide.bits.snpHitRelease,
    task_s3_decide.bits.snpHitReleaseMeta.tagErr,
    io.metaOnHit_s3.tagErr) || io.errOnSnp_s3
  val bNeedDct_decide = isSnpXFwd(task_s3_decide.bits.chiOpcode.get) &&
    bNestableHit_decide && !bNestableTagErr_decide
  val bNeedMshr_decide = bNeedProbe_decide || bNeedDct_decide ||
    postGrantReleaseSnoop_decide || postGrantReleaseLate_decide
  val bDoFwd_decide = isSnpXFwd(task_s3_decide.bits.chiOpcode.get) &&
    (Mux(task_s3_decide.bits.snpHitRelease,
      task_s3_decide.bits.snpHitReleaseMeta.state =/= INVALID,
      io.dirResp_s3.hit)) &&
    !(Mux(task_s3_decide.bits.snpHitRelease,
      task_s3_decide.bits.snpHitReleaseMeta.tagErr,
      io.metaOnHit_s3.tagErr) || io.errOnSnp_s3)
  val bNestableState_decide = Mux(task_s3_decide.bits.snpHitRelease,
    task_s3_decide.bits.snpHitReleaseMeta.state, io.metaOnHit_s3.state)
  val bNestableDirty_decide = Mux(task_s3_decide.bits.snpHitRelease,
    task_s3_decide.bits.snpHitReleaseMeta.dirty, io.metaOnHit_s3.dirty)
  val bRespNestableHit_decide = task_s3_decide.bits.fromB && bNestableHit_decide
  val bRetToSrc_decide = task_s3_decide.bits.retToSrc.getOrElse(false.B)
  val bNeverRespData_decide = isSnpMakeInvalidX(task_s3_decide.bits.chiOpcode.get) ||
    isSnpStashX(task_s3_decide.bits.chiOpcode.get) ||
    isSnpQuery(task_s3_decide.bits.chiOpcode.get) ||
    task_s3_decide.bits.chiOpcode.get === SnpOnceFwd ||
    task_s3_decide.bits.chiOpcode.get === SnpUniqueFwd ||
    task_s3_decide.bits.chiOpcode.get === SnpPreferUniqueFwd
  val bShouldRespData_decide =
    (bRespNestableHit_decide && (bNestableState_decide === TIP || bNestableState_decide === TRUNK) &&
      bNestableDirty_decide) ||
    (bRespNestableHit_decide && bNestableState_decide === TIP && !bNestableDirty_decide &&
      task_s3_decide.bits.chiOpcode.get === SnpOnce) ||
    (bRespNestableHit_decide && bRetToSrc_decide && isSnpXFwd(task_s3_decide.bits.chiOpcode.get)) ||
    (bRespNestableHit_decide && bRetToSrc_decide && bNestableState_decide =/= INVALID &&
      (task_s3_decide.bits.chiOpcode.get === SnpOnce ||
        task_s3_decide.bits.chiOpcode.get === SnpUnique ||
        task_s3_decide.bits.chiOpcode.get === SnpPreferUnique ||
        isSnpToBNonFwd(task_s3_decide.bits.chiOpcode.get)))
  val bDoRespData_decide = bShouldRespData_decide && !bNeverRespData_decide
  val bFwdPassDirty_decide = bNestableHit_decide &&
    (task_s3_decide.bits.chiOpcode.get === SnpUniqueFwd ||
      task_s3_decide.bits.chiOpcode.get === SnpPreferUniqueFwd) &&
    bNestableState_decide === TIP && bNestableDirty_decide
  val bFwdCacheState_decide = WireInit(I)
  when(bNestableHit_decide) {
    when(isSnpToBFwd(task_s3_decide.bits.chiOpcode.get)) {
      bFwdCacheState_decide := SC
    }.elsewhen(task_s3_decide.bits.chiOpcode.get === SnpUniqueFwd ||
      task_s3_decide.bits.chiOpcode.get === SnpPreferUniqueFwd) {
      bFwdCacheState_decide := Mux(
        bNestableState_decide === TIP && bNestableDirty_decide, UD, UC)
    }
  }
  val bRespCacheState_decide = WireInit(I)
  when(bNestableHit_decide && !(io.dirResp_s3.error || io.dirResp_s3.meta.tagErr)) {
    when(isSnpToB(task_s3_decide.bits.chiOpcode.get)) {
      bRespCacheState_decide := Mux(task_s3_decide.bits.snpHitReleaseToInval, I, SC)
    }
    when(isSnpOnceX(task_s3_decide.bits.chiOpcode.get) ||
      isSnpStashX(task_s3_decide.bits.chiOpcode.get) ||
      isSnpQuery(task_s3_decide.bits.chiOpcode.get)) {
      bRespCacheState_decide := Mux(
        bNestableState_decide === BRANCH,
        SC, Mux(bNestableDirty_decide, UD, UC))
    }
    when(isSnpOnceX(task_s3_decide.bits.chiOpcode.get)) {
      when(task_s3_decide.bits.snpHitReleaseToClean && bNestableDirty_decide) {
        bRespCacheState_decide := SC
      }
      when(task_s3_decide.bits.snpHitReleaseToInval) {
        bRespCacheState_decide := I
      }
    }
    when(task_s3_decide.bits.chiOpcode.get === SnpCleanShared) {
      bRespCacheState_decide := Mux(isT(bNestableState_decide), UC, SC)
    }
  }
  val bAllocState_decide = WireInit(0.U.asTypeOf(new FSMState()))
  bAllocState_decide.elements.foreach(_._2 := true.B)
  when(task_s3_decide.bits.fromB) {
    bAllocState_decide.s_probeack := false.B
    when(bNeedProbe_decide) {
      bAllocState_decide.s_pprobe := false.B
      bAllocState_decide.w_pprobeackfirst := false.B
      bAllocState_decide.w_pprobeacklast := false.B
    }
    when(bNeedDct_decide) {
      bAllocState_decide.s_dct.get := false.B
    }
  }
  val cWen_decide = task_s3_decide.bits.fromC &&
    isParamFromT(task_s3_decide.bits.param) && task_s3_decide.bits.opcode(0) &&
    io.dirResp_s3.hit
  val cMetaValid_decide = task_s3_decide.bits.fromC && io.dirResp_s3.hit
  val cMeta_decide = MetaEntry(
    dirty = io.metaOnHit_s3.dirty || cWen_decide,
    state = Mux(isParamFromT(task_s3_decide.bits.param), TIP, io.metaOnHit_s3.state),
    clients = Fill(clientBits, !isToN(task_s3_decide.bits.param)),
    alias = io.metaOnHit_s3.alias,
    accessed = io.metaOnHit_s3.accessed,
    tagErr = Mux(cWen_decide, task_s3_decide.bits.denied, io.metaOnHit_s3.tagErr),
    dataErr = Mux(cWen_decide, task_s3_decide.bits.corrupt, io.metaOnHit_s3.dataErr),
    pfsrc = io.metaOnHit_s3.prefetchSrc.getOrElse(0.U),
    cdpPfDepth = io.metaOnHit_s3.cdpPfDepth.getOrElse(0.U)
  )
  val cOrphanReleaseData_decide = task_s3_decide.bits.fromC &&
    task_s3_decide.bits.opcode === ReleaseData &&
    isParamFromT(task_s3_decide.bits.param) && !io.dirResp_s3.hit
  val orphanReleaseSequence = RegInit(0.U(8.W))
  val orphanReserveAtDecision = task_s3_decide.valid && cOrphanReleaseData_decide
  val orphanReservationMatches = io.fromMSHRCtl.orphanReleaseReservation.valid &&
    io.fromMSHRCtl.orphanReleaseReservation.sequence === s3Decision.bits.orphanReleaseSequence
  val orphanApplyBlocked = s3Decision.valid && s3Decision.bits.cOrphanReleaseData &&
    !orphanReservationMatches
  val orphanReserveRetry = s3Decision.valid && s3Decision.bits.cOrphanReleaseData &&
    !orphanReservationMatches
  io.toMSHRCtl.orphanReleaseReserveRequest.valid := orphanReserveAtDecision || orphanReserveRetry
  when(orphanReserveAtDecision) {
    io.toMSHRCtl.orphanReleaseReserveRequest.bits.set := task_s3_decide.bits.set
    io.toMSHRCtl.orphanReleaseReserveRequest.bits.tag := task_s3_decide.bits.tag
    io.toMSHRCtl.orphanReleaseReserveRequest.bits.sequence := orphanReleaseSequence
    orphanReleaseSequence := orphanReleaseSequence + 1.U
  }.otherwise {
    io.toMSHRCtl.orphanReleaseReserveRequest.bits.set := s3Decision.bits.task.set
    io.toMSHRCtl.orphanReleaseReserveRequest.bits.tag := s3Decision.bits.task.tag
    io.toMSHRCtl.orphanReleaseReserveRequest.bits.sequence := s3Decision.bits.orphanReleaseSequence
  }
  val cReleaseDataDirectWrite_decide = cWen_decide &&
    task_s3_decide.bits.opcode === ReleaseData &&
    isParamFromT(task_s3_decide.bits.param)
  val bMeta_decide = Mux(
    isSnpToN(task_s3_decide.bits.chiOpcode.get) || task_s3_decide.bits.snpHitReleaseToInval, MetaEntry(),
    MetaEntry(
      dirty = false.B,
      state = Mux(task_s3_decide.bits.chiOpcode.get === SnpCleanShared, io.metaOnHit_s3.state, BRANCH),
      clients = io.metaOnHit_s3.clients,
      alias = io.metaOnHit_s3.alias,
      accessed = io.metaOnHit_s3.accessed,
      tagErr = io.metaOnHit_s3.tagErr,
      dataErr = io.metaOnHit_s3.dataErr,
      pfsrc = io.metaOnHit_s3.prefetchSrc.getOrElse(0.U),
      cdpPfDepth = io.metaOnHit_s3.cdpPfDepth.getOrElse(0.U)
    )
  )
  val bMetaWValid_decide = task_s3_decide.bits.fromB && !task_s3_decide.bits.mshrTask &&
    io.dirResp_s3.hit && (
      task_s3_decide.bits.snpHitReleaseToInval || (!bNeedMshr_decide &&
      (!isSnpOnce(task_s3_decide.bits.chiOpcode.get) ||
        (task_s3_decide.bits.snpHitReleaseToClean &&
          task_s3_decide.bits.snpHitReleaseMeta.dirty)) &&
      !isSnpStashX(task_s3_decide.bits.chiOpcode.get) &&
      !isSnpQuery(task_s3_decide.bits.chiOpcode.get) &&
      (!bDoFwd_decide || !isSnpOnceFwd(task_s3_decide.bits.chiOpcode.get)) && (
        io.metaOnHit_s3.state === TIP ||
        io.metaOnHit_s3.state === BRANCH && isSnpToN(task_s3_decide.bits.chiOpcode.get) ||
        bDoFwd_decide && io.metaOnHit_s3.state === TRUNK
      )))
  s3Decision.valid := task_s3_decide.valid || orphanApplyBlocked
  when(task_s3_decide.valid) {
    s3Decision.bits.task := task_s3_decide.bits
    s3Decision.bits.token := leaseToken_s3_decide
    s3Decision.bits.orphanReleaseSequence := orphanReleaseSequence
    s3Decision.bits.snoopSlotToken := snoopSlotToken_s3_decide
    s3Decision.bits.grantToken := grantToken_s3_decide
    s3Decision.bits.dir := io.dirResp_s3
    s3Decision.bits.repl := io.replResp
    s3Decision.bits.metaOnHit := io.metaOnHit_s3
    s3Decision.bits.errOnSnp := io.errOnSnp_s3
    s3Decision.bits.wayOH := io.dirWayOH_s3
    s3Decision.bits.cPayload := io.bufResp
    s3Decision.bits.refill := io.refillBufResp_s3
    s3Decision.bits.release := io.releaseBufResp_s3
    s3Decision.bits.retry := retryDecision
    s3Decision.bits.normalRetry := retryDecision && task_s3_decide.bits.mshrTask &&
      task_s3_decide.bits.mshrContext === 0.U
    s3Decision.bits.grantInstall := io.fromMSHRCtl.grantInstallOK
    s3Decision.bits.grantInstallSlot := io.fromMSHRCtl.grantInstallSlot
    s3Decision.bits.parentGeneration := io.fromMSHRCtl.replParentGeneration
    s3Decision.bits.postGrantReleaseSnoop := postGrantReleaseSnoop_decide || postGrantReleaseLate_decide
    s3Decision.bits.normalRefillOverlapsSnoopHold := normalRefillOverlapsSnoopHold_decide
    s3Decision.bits.bNeedProbe := bNeedProbe_decide
    s3Decision.bits.bNeedDct := bNeedDct_decide
    s3Decision.bits.bNeedMshr := bNeedMshr_decide
    s3Decision.bits.bAllocState := bAllocState_decide
    s3Decision.bits.bDoFwd := bDoFwd_decide
    s3Decision.bits.bDoRespData := bDoRespData_decide
    s3Decision.bits.bFwdPassDirty := bFwdPassDirty_decide
    s3Decision.bits.bFwdCacheState := bFwdCacheState_decide
    s3Decision.bits.bRespCacheState := bRespCacheState_decide
    s3Decision.bits.bMetaWValid := bMetaWValid_decide
    s3Decision.bits.bMeta := bMeta_decide
    s3Decision.bits.cWen := cWen_decide
    s3Decision.bits.cMetaValid := cMetaValid_decide
    s3Decision.bits.cMeta := cMeta_decide
    s3Decision.bits.cOrphanReleaseData := cOrphanReleaseData_decide
    s3Decision.bits.cReleaseDataDirectWrite := cReleaseDataDirectWrite_decide
    s3Decision.bits.normal := normalDecision
  }
  val task_s3 = Wire(Valid(new TaskBundle))
  task_s3.valid := s3Decision.valid && !orphanApplyBlocked
  task_s3.bits := s3Decision.bits.task
  io.toMSHRCtl.orphanReleaseReserveDone.valid := task_s3.valid &&
    s3Decision.bits.cOrphanReleaseData
  io.toMSHRCtl.orphanReleaseReserveDone.bits := io.fromMSHRCtl.orphanReleaseReservation
  when(orphanApplyBlocked) {
    assert(!task_s3_decide.valid && !task_s3_capture.valid && !task_s2.valid,
      "an orphan slot wait must hold the sole S3 decision without younger stage owners")
  }
  when(task_s3.valid && s3Decision.bits.cOrphanReleaseData) {
    val claim = io.fromMSHRCtl.orphanReleaseReservation
    assert(claim.valid && claim.sequence === s3Decision.bits.orphanReleaseSequence &&
      claim.set === s3Decision.bits.task.set && claim.tag === s3Decision.bits.task.tag,
      "orphan ReleaseData apply must consume its registered slot reservation")
  }
  val leaseToken_s3 = s3Decision.bits.token
  val preApplyTasks = Seq(task_s3_capture, task_s3_decide)
  when(task_s3.valid && task_s3.bits.mshrTask && task_s3.bits.mshrContext === 0.U &&
      s3Decision.bits.repl.valid) {
    assert(s3Decision.bits.parentGeneration === leaseToken_s3.generation,
      "S3 decision must retain the normal parent generation")
  }
  val reqWayOH_s3 = UIntToOH(task_s3.bits.way, cacheParams.ways)
  assert(PopCount(Seq(task_s2.valid, task_s3_capture.valid, task_s3_decide.valid, task_s3.valid)) <= 1.U,
    "the local Directory/result/apply execution slot must have one owner")

  /* ======== Enchantment ======== */
  val dirResult_s3    = s3Decision.bits.dir
  val meta_s3         = dirResult_s3.meta
  val metaOnHit_s3    = s3Decision.bits.metaOnHit
  val req_s3          = task_s3.bits
  val normalAction = s3Decision.bits.normal
  val cmoHitInvalid   = normalAction.cmoHitInvalid

  val mshr_req_s3     = req_s3.mshrTask
  val sink_req_s3     = !mshr_req_s3
  val sinkA_req_s3    = !mshr_req_s3 && req_s3.fromA
  val sinkB_req_s3    = !mshr_req_s3 && req_s3.fromB
  val sinkC_req_s3    = !mshr_req_s3 && req_s3.fromC

  val req_acquire_s3            = sinkA_req_s3 && (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm)
  val req_acquireBlock_s3       = sinkA_req_s3 && req_s3.opcode === AcquireBlock
  val req_prefetch_s3           = sinkA_req_s3 && req_s3.opcode === Hint
  val req_get_s3                = sinkA_req_s3 && req_s3.opcode === Get
  val req_cbo_clean_s3          = sinkA_req_s3 && req_s3.opcode === CBOClean
  val req_cbo_flush_s3          = sinkA_req_s3 && req_s3.opcode === CBOFlush && !cmoHitInvalid
  val req_cbo_inval_s3          = sinkA_req_s3 && req_s3.opcode === CBOInval

  val mshr_grant_s3             = mshr_req_s3 && req_s3.fromA && (req_s3.opcode === Grant || req_s3.opcode === GrantData)
  val mshr_grantdata_s3         = mshr_req_s3 && req_s3.fromA && req_s3.opcode === GrantData
  val mshr_accessackdata_s3     = mshr_req_s3 && req_s3.fromA && req_s3.opcode === AccessAckData
  val mshr_hintack_s3           = mshr_req_s3 && req_s3.fromA && req_s3.opcode === HintAck
  val mshr_cmoresp_s3           = mshr_req_s3 && req_s3.fromA && req_s3.opcode === CBOAck

  val mshr_snpResp_s3           = mshr_req_s3 && req_s3.toTXRSP && req_s3.chiOpcode.get === SnpResp
  val mshr_snpRespFwded_s3      = mshr_req_s3 && req_s3.toTXRSP && req_s3.chiOpcode.get === SnpRespFwded
  val mshr_snpRespData_s3       = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === SnpRespData
  val mshr_snpRespDataPtl_s3    = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === SnpRespDataPtl
  val mshr_snpRespDataFwded_s3  = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === SnpRespDataFwded
  val mshr_snpRespX_s3 = mshr_snpResp_s3 || mshr_snpRespFwded_s3
  val mshr_snpRespDataX_s3 = mshr_snpRespData_s3 || mshr_snpRespDataPtl_s3 || mshr_snpRespDataFwded_s3

  val mshr_dct_s3               = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === CompData

  val mshr_writeCleanFull_s3    = mshr_req_s3 && req_s3.toTXREQ && req_s3.chiOpcode.get === WriteCleanFull
  val mshr_writeBackFull_s3     = mshr_req_s3 && req_s3.toTXREQ && req_s3.chiOpcode.get === WriteBackFull
  val mshr_writeEvictFull_s3    = mshr_req_s3 && req_s3.toTXREQ && req_s3.chiOpcode.get === WriteEvictFull
  val mshr_writeEvictOrEvict_s3 = mshr_req_s3 && req_s3.toTXREQ &&
    afterIssueEbOrElse(req_s3.chiOpcode.get === WriteEvictOrEvict, false.B)
  val mshr_evict_s3             = mshr_req_s3 && req_s3.toTXREQ && req_s3.chiOpcode.get === Evict
  
  val mshr_cbWrData_s3          = mshr_req_s3 && req_s3.toTXDAT && req_s3.chiOpcode.get === CopyBackWrData

  val meta_has_clients_s3       = meta_s3.clients.orR
  val metaOnHit_has_clients_s3    = metaOnHit_s3.clients.orR
  val req_needT_s3              = needT(req_s3.opcode, req_s3.param)

  val cmo_cbo_retention_s3      = req_cbo_clean_s3 || req_cbo_flush_s3
  val cmo_cbo_s3                = req_cbo_clean_s3 || req_cbo_flush_s3 || req_cbo_inval_s3

  val cache_alias               = normalAction.alias

  // *NOTICE: 'nestable_*' must not be used in A Channel related logics.
  val nestable_dirResult_s3     = Wire(chiselTypeOf(dirResult_s3))
  val nestable_meta_s3          = nestable_dirResult_s3.meta
  val nestable_meta_has_clients_s3 = nestable_dirResult_s3.meta.clients.orR
  nestable_dirResult_s3 := dirResult_s3
  nestable_dirResult_s3.meta := metaOnHit_s3
  when (req_s3.snpHitRelease) {
    // Meta states from MSHRs were considered as directory result here.
    // Therefore, meta states were always inferred to be hit when nesting release, no matter the fact that directory
    // was always non-hit on cache replacement subsequent release.
    nestable_dirResult_s3.hit   := req_s3.snpHitReleaseMeta.state =/= INVALID
    nestable_dirResult_s3.meta  := req_s3.snpHitReleaseMeta
    nestable_dirResult_s3.set   := req_s3.set
    nestable_dirResult_s3.tag   := req_s3.tag
  }

  val tagError_s3               = s3Decision.bits.dir.error || meta_s3.tagErr
  val dataError_s3              = meta_s3.dataErr
  val l2Error_s3                = s3Decision.bits.dir.error

  val mshr_refill_s3 = mshr_accessackdata_s3 || mshr_hintack_s3 || mshr_grant_s3 // needs refill to L2 DS
  val mshr_refill_only_s3 = mshr_refill_s3 && req_s3.refillOnly
  val mshr_defer_refill_write_s3 = mshr_refill_s3 && req_s3.deferRefillWrite &&
    !(req_s3.dsWen || req_s3.tagWen || req_s3.metaWen)
  val mshr_replace_capture_s3 = mshr_req_s3 && req_s3.replaceCapture
  val mshr_refill_to_release_copy_s3 = mshr_req_s3 && req_s3.refillToReleaseCopy
  val replResp_valid_hold = s3Decision.bits.repl.bits.validHold
  val retry = Mux(mshr_req_s3 && req_s3.mshrContext === 0.U,
    s3Decision.bits.normalRetry, s3Decision.bits.retry)
  val need_repl = replResp_valid_hold && s3Decision.bits.repl.bits.meta.state =/= INVALID && req_s3.replTask
  val grantReplResp_s3 = replResp_valid_hold && mshr_refill_s3 && !req_s3.refillOnly &&
    req_s3.mshrContext === 0.U && req_s3.replTask && req_s3.grantInstallEn &&
    s3Decision.bits.repl.bits.mshrId === req_s3.mshrId
  val grantMergeInstall_s3 = task_s3.valid && grantReplResp_s3 && s3Decision.bits.grantInstall
  val grantVictimCap_s3 = grantMergeInstall_s3 && s3Decision.bits.repl.bits.meta.state =/= INVALID
  val replRespForTask = replResp_valid_hold && mshr_req_s3 &&
    s3Decision.bits.repl.bits.mshrId === req_s3.mshrId &&
    s3Decision.bits.repl.bits.meta.state =/= INVALID && !s3Decision.bits.repl.bits.retry
  val commitReplWay = mshr_refill_s3 && (req_s3.replTask || replRespForTask)
  val replWayOH_s3 = UIntToOH(s3Decision.bits.repl.bits.way, cacheParams.ways)
  when (replRespForTask) {
    assert(s3Decision.bits.repl.bits.set === req_s3.set,
      "ReplacerResult set must match the MSHR commit task")
  }

  /* ======== Interact with MSHR ======== */
  // *NOTICE: A Channel requests should be blocked by RequestBuffer when MSHR nestable,
  //          'nestable_*' must not be used here.
  val acquire_on_hit_s3 = normalAction.acquireOnHit
  val acquire_on_miss_s3 = normalAction.acquireOnMiss
  val need_acquire_s3_a = normalAction.needAcquire
  val need_probe_s3_a = normalAction.needProbe
  val need_release_s3_a = normalAction.needRelease
  val need_cmoresp_s3_a = normalAction.cmo
  val need_compack_s3_a = !normalAction.cmo
  val need_mshr_s3_a = normalAction.needMshr

  // An A allocation must match the retained normal reservation token, owner and source at apply.
  val normalAllocTokenMatch_s3 = io.fromMSHRCtl.normalAllocAdmission &&
    io.fromMSHRCtl.normalAllocToken.asUInt === leaseToken_s3.asUInt &&
    io.fromMSHRCtl.normalAllocOwner === req_s3.mshrId &&
    io.fromMSHRCtl.normalAllocSource === req_s3.sourceId
  val staleNormalAllocation_s3 = sinkA_req_s3 && need_mshr_s3_a &&
    !normalAllocTokenMatch_s3
  val normalActionAdmitted_s3 = !staleNormalAllocation_s3
  
  /**
    * 1. For SnpOnce/SnpOnceFwd, SnpQuery, and SnpStash, only the latest copy of the cacheline is needed without changing
    *    the state of the cacheline at the snoopee. Therefore L2 should only send pProbe toT (to get the latest copy)
    *    when the state in L2 is TRUNK
    * 2. For SnpClean/SnpCleanFwd, SnpShared/SnpSharedFwd, SnpNotSharedDirty/SnpNotSharedDirtyFwd, and SnpCleanShared,
    *    the snooped cacheline should be degraded into BRANCH state because there is no SharedDirty state or Owner
    *    state (of MOESI) in CoupledL2. Therefore L2 should only send pProbe toB to degrade upper clients when the
    *    state in L2 is TRUNK
    * 3. For SnpUnique/SnpUniqueFwd/SnpUniqueStash, SnpCleanInvalid, SnpMakeInvalid/SnpMakeInvalidStash, the snooped
    *    cacheline should be degraded into INVALID state. Therefore L2 should only send pProbe toN to degrade upper
    *    clients when the state in L2 is TRUNK or BRANCH with clients.orR = 1
    * 4. When tagErr(NDERR), never forward data, and the snoopee should invalidate cache state
    *
    */
  val doFwd = s3Decision.bits.bDoFwd
  val need_pprobe_s3_b_snpStable = req_s3.fromB && (
    isSnpOnceX(req_s3.chiOpcode.get) || isSnpQuery(req_s3.chiOpcode.get) || isSnpStashX(req_s3.chiOpcode.get)
  ) && dirResult_s3.hit && metaOnHit_s3.state === TRUNK && metaOnHit_has_clients_s3
  val need_pprobe_s3_b_snpToB = req_s3.fromB && (
    isSnpToB(req_s3.chiOpcode.get) ||
    req_s3.chiOpcode.get === SnpCleanShared
  ) && dirResult_s3.hit && metaOnHit_s3.state === TRUNK && metaOnHit_has_clients_s3
  val need_pprobe_s3_b_snpToN = req_s3.fromB && (
    isSnpUniqueX(req_s3.chiOpcode.get) ||
    req_s3.chiOpcode.get === SnpCleanInvalid ||
    isSnpMakeInvalidX(req_s3.chiOpcode.get)
  ) && dirResult_s3.hit && metaOnHit_has_clients_s3
  val need_pprobe_s3_b_snpNDERR = req_s3.fromB && (s3Decision.bits.errOnSnp || s3Decision.bits.metaOnHit.tagErr) && dirResult_s3.hit
  val need_pprobe_s3_b = s3Decision.bits.bNeedProbe
  val need_dct_s3_b = s3Decision.bits.bNeedDct
  val postGrantReleaseSnoop = s3Decision.bits.postGrantReleaseSnoop
  val need_mshr_s3_b = s3Decision.bits.bNeedMshr

  val need_mshr_s3 = need_mshr_s3_a || need_mshr_s3_b

  io.toMSHRCtl.snoopToNDirectDone.valid := task_s3.valid && sinkB_req_s3 &&
    isSnpToN(req_s3.chiOpcode.get) && !need_mshr_s3_b
  io.toMSHRCtl.snoopToNDirectDone.bits := req_s3

  /* Signals to MSHR Ctl */
  val alloc_state = WireInit(0.U.asTypeOf(new FSMState()))
  alloc_state.elements.foreach(_._2 := true.B)
  io.toMSHRCtl.mshr_alloc_s3.valid := task_s3.valid && !mshr_req_s3 && need_mshr_s3 &&
    normalActionAdmitted_s3
  assert(
    !io.toMSHRCtl.mshr_alloc_s3.valid || !sinkC_req_s3,
    "a SinkC Release( Data ) must not allocate a normal or snoop MSHR"
  )
  io.toMSHRCtl.snoopDirectoryHit := dirResult_s3.hit
  io.toMSHRCtl.mshr_alloc_s3.bits.dirResult := nestable_dirResult_s3
  io.toMSHRCtl.mshr_alloc_s3.bits.state := alloc_state
  io.toMSHRCtl.mshr_alloc_s3.bits.snoopSlotToken := s3Decision.bits.snoopSlotToken
  io.toMSHRCtl.mshr_alloc_s3.bits.task match { case task =>
    task := req_s3
    task.bufIdx := 0.U(bufIdxBits.W)
    task.mshrTask := false.B
    task.aliasTask.foreach(_ := cache_alias)
    task.wayMask := 0.U(cacheParams.ways.W)
    // TODO
  }
  when(sinkA_req_s3) {
    io.toMSHRCtl.mshr_alloc_s3.bits := normalAction.allocation
  }
  io.toMSHRCtl.normalReservationDone.valid := task_s3.valid && sinkA_req_s3
  io.toMSHRCtl.normalReservationDone.bits.mshrId := req_s3.mshrId
  io.toMSHRCtl.normalReservationDone.bits.mshrContext := Mux(normalAction.cmo, 2.U, 0.U)
  io.toMSHRCtl.normalReservationDone.bits.token := leaseToken_s3
  io.toMSHRCtl.snoopReservationDone.valid := task_s3.valid && sinkB_req_s3
  io.toMSHRCtl.snoopReservationDone.bits := s3Decision.bits.snoopSlotToken
  when(task_s3.valid && sinkB_req_s3) {
    assert(s3Decision.bits.snoopSlotToken.valid &&
      s3Decision.bits.snoopSlotToken.set === req_s3.set &&
      s3Decision.bits.snoopSlotToken.tag === req_s3.tag &&
      s3Decision.bits.snoopSlotToken.txnID === req_s3.txnID.get &&
      s3Decision.bits.snoopSlotToken.source === req_s3.sourceId,
      "MainPipe B task must retain its admitted slot token through apply")
  }

  /* ======== Resps to SinkA/B/C Reqs ======== */
  val sink_resp_s3 = WireInit(0.U.asTypeOf(Valid(new TaskBundle)))
  val sink_resp_s3_a_promoteT = dirResult_s3.hit && isT(metaOnHit_s3.state)

  val doRespData = s3Decision.bits.bDoRespData
  dontTouch(doRespData)

  // On directory hit under non-invalidating snoop nesting WriteCleanFull, 
  // excluding SnpStashX and SnpQuery:
  //  1. SnpCleanShared[1-sink_resp]  : UD -> UC_PD, UC -> UC, SC -> SC
  //  2. SnpOnce*[2-sink_resp]        : UD -> SC_PD, UC -> SC, SC -> SC
  //  3. snpToB                       : UD -> SC_PD, UC -> SC, SC -> SC
  // 
  // *NOTE[1-sink_resp]:
  //    UD -> SC transitions were not used on WriteCleanFull without nesting snoop, and
  //    only UD -> UC update could be observed on directory in this case
  //    Therefore, it was unnecessary to observe cache state from nested WriteCleanFull MSHRs, while
  //    extracting PassDirty from MSHRs
  //
  // *NOTE[2-sink_resp]:
  //    UD -> UC transitions were not allowed on SnpOnce*, while permitting UD -> UD and UC -> UC
  //    On SnpOnce*, UD/UC were turned into SC on nested WriteClean, on which directory must hit
  //    Otherwise, the cache state was fast forwarded to I by default
  //    Directory might be missing after multiple nesting snoops on WriteClean, indicating losing UD
  //
  // *NOTE[tagErr/NDERR]:
  //    ALL -> I, snoopee invalidates local copy
  //
  // Resp[2: 0] = {PassDirty, CacheState[1: 0]}
  val respCacheState = s3Decision.bits.bRespCacheState
  val respPassDirty = doRespData && nestable_dirResult_s3.hit && isT(nestable_meta_s3.state) && nestable_meta_s3.dirty &&
    (req_s3.chiOpcode.get =/= SnpOnce || req_s3.snpHitRelease) &&
    !(isSnpStashX(req_s3.chiOpcode.get) || isSnpQuery(req_s3.chiOpcode.get))
  // FwdState[2: 0] = {PassDirty, CacheState[1: 0]}
  val fwdCacheState = s3Decision.bits.bFwdCacheState
  val fwdPassDirty = s3Decision.bits.bFwdPassDirty

  val sink_resp_s3_b_meta = MetaEntry()
  val sink_resp_s3_b_metaWen = Wire(Bool())

  sink_resp_s3.valid := task_s3.valid && !mshr_req_s3 && !need_mshr_s3
  sink_resp_s3.bits := task_s3.bits
  sink_resp_s3.bits.mshrId := (1 << (mshrBits-1)).U + sink_resp_s3.bits.sourceId
  when (req_s3.fromA) {
    sink_resp_s3.bits := normalAction.response
  }.elsewhen (req_s3.fromB) {

    sink_resp_s3.bits.opcode := 0.U
    sink_resp_s3.bits.param := 0.U

    sink_resp_s3.bits.tgtID.foreach(_ := task_s3.bits.srcID.get)
    sink_resp_s3.bits.srcID.foreach(_ := task_s3.bits.tgtID.get) // TODO: srcID should be fixed. FIX THIS!!!
    sink_resp_s3.bits.txnID.foreach(_ := task_s3.bits.txnID.get)
    sink_resp_s3.bits.dbID.foreach(_ := 0.U)
    sink_resp_s3.bits.pCrdType.foreach(_ := 0.U) // TODO
    sink_resp_s3.bits.chiOpcode.foreach(_ := MuxLookup(
      Cat(doFwd, doRespData),
      SnpResp
    )(Seq(
      Cat(false.B, false.B) -> SnpResp,
      Cat(true.B, false.B)  -> SnpRespFwded,
      Cat(false.B, true.B)  -> SnpRespData, // ignore SnpRespDataPtl for now
      Cat(true.B, true.B)   -> SnpRespDataFwded
    )))
    sink_resp_s3.bits.resp.foreach(_ := setPD(respCacheState, respPassDirty && doRespData))
    sink_resp_s3.bits.fwdState.foreach(_ := setPD(fwdCacheState, fwdPassDirty))
    sink_resp_s3.bits.txChannel := Cat(doRespData, !doRespData, false.B) // TODO: parameterize this
    sink_resp_s3.bits.size := log2Ceil(blockBytes).U
    sink_resp_s3.bits.meta := sink_resp_s3_b_meta
    sink_resp_s3.bits.metaWen := sink_resp_s3_b_metaWen

  }.otherwise { // req_s3.fromC
    sink_resp_s3.bits.opcode := ReleaseAck
    sink_resp_s3.bits.param := 0.U // param of ReleaseAck must be 0
  }

  val source_req_s3 = Wire(new TaskBundle)
  source_req_s3 := Mux(sink_resp_s3.valid, sink_resp_s3.bits, req_s3)
  source_req_s3.isKeyword.foreach(_ := req_s3.isKeyword.getOrElse(false.B))

  /* ======== Interact with DS ======== */
  val useReleaseBufData_s3 = req_s3.useProbeData ||
    req_s3.fromB && req_s3.snpHitReleaseWithData
  val data_s3 = Mux(useReleaseBufData_s3 && s3Decision.bits.release.valid,
    s3Decision.bits.release.bits.data, s3Decision.bits.refill.bits.data.data)
  // Internal Directory-only tasks retain a response opcode but must never emit SourceD.
  val mergedAData_s3 = req_s3.mergeA && req_s3.aMergeTask.opcode(0)
  val externalRefillBufConsumer_s3 = !mshr_refill_only_s3 &&
    (mshr_grantdata_s3 || mshr_accessackdata_s3 || mergedAData_s3)
  val internalRefillBufConsumer_s3 = mshr_refill_only_s3 &&
    req_s3.dsWen && req_s3.readRefillData
  val mshrRefillToReleaseCopy_s3 = task_s3.valid && mshr_refill_to_release_copy_s3
  val mshrRefillBufConsumer_s3 = task_s3.valid && mshr_req_s3 &&
    !useReleaseBufData_s3 &&
    (externalRefillBufConsumer_s3 || internalRefillBufConsumer_s3 || mshrRefillToReleaseCopy_s3)
  when(mshrRefillBufConsumer_s3) {
    val expectedRefillId = Mux(req_s3.refillToReleaseCopy, req_s3.refillBufReadId, req_s3.mshrId)
    assert(
      s3Decision.bits.refill.valid && s3Decision.bits.refill.bits.id === expectedRefillId,
      "RefillBuf response must match the consuming MSHR"
    )
  }
  val c_releaseData_s3 = s3Decision.bits.cPayload.data.asUInt
  val hasData_s3_tl = source_req_s3.opcode(0) // whether to respond data to TileLink-side
  val hasData_s3_chi = source_req_s3.toTXDAT // whether to respond data to CHI-side
  val hasData_s3 = hasData_s3_tl || hasData_s3_chi

  val need_data_a = normalAction.readData
  val need_data_b = sinkB_req_s3 && (doRespData || doFwd || nestable_dirResult_s3.hit && nestable_meta_s3.state === TRUNK)
  val ren = need_data_a || need_data_b || mshr_replace_capture_s3 || grantVictimCap_s3

  val wen_c = s3Decision.bits.cWen
  io.toMSHRCtl.orphanReleaseData := task_s3.valid && s3Decision.bits.cOrphanReleaseData
  val wen_mshr = req_s3.dsWen && (
    mshr_snpRespX_s3 || mshr_snpRespDataX_s3 ||
    mshr_writeCleanFull_s3 || mshr_writeBackFull_s3 || 
    mshr_writeEvictFull_s3 || mshr_writeEvictOrEvict_s3 || mshr_evict_s3 ||
    mshr_refill_s3 && !need_repl && !retry && !mshr_defer_refill_write_s3
  )
  val wen = wen_c || wen_mshr
  io.toMSHRCtl.releaseDataDirectWrite.valid := task_s3.valid &&
    s3Decision.bits.cReleaseDataDirectWrite
  io.toMSHRCtl.releaseDataDirectWrite.bits := req_s3
  val normalRefillDSWrite = task_s3.valid && mshr_refill_s3 &&
    req_s3.mshrContext === 0.U && wen_mshr && !need_repl && !retry &&
    !mshr_defer_refill_write_s3
  val normalRefillOverlapsSnoopHold = s3Decision.bits.normalRefillOverlapsSnoopHold
  when (normalRefillDSWrite) {
    assert(!normalRefillOverlapsSnoopHold,
      "normal refill must not overwrite a DS way held for SnoopMSHR data")
  }
  val normalCommitApplied = Wire(Valid(new MainpipeLeaseDone))
  normalCommitApplied.valid := task_s3.valid && mshr_refill_s3 &&
    req_s3.mshrContext === 0.U && !need_repl && !retry && !mshr_defer_refill_write_s3
  normalCommitApplied.bits.mshrId := req_s3.mshrId
  normalCommitApplied.bits.mshrContext := 0.U
  normalCommitApplied.bits.token := leaseToken_s3
  io.toMSHRCtl.refillWriteDone := RegNext(normalCommitApplied, 0.U.asTypeOf(normalCommitApplied))
  io.toMSHRCtl.grantInstallDone.valid := grantMergeInstall_s3
  io.toMSHRCtl.grantInstallDone.bits := req_s3.mshrId

  io.toDS.en_s3 := task_s3.valid && normalActionAdmitted_s3 && (ren || wen)
  io.toDS.req_s3.valid := io.toDS.en_s3
  io.toDS.req_s3.bits.way := Mux(
    commitReplWay,
    s3Decision.bits.repl.bits.way,
    Mux(mshr_req_s3, req_s3.way, dirResult_s3.way)
  )
  io.toDS.req_s3.bits.set := req_s3.set
  io.toDS.req_s3.bits.wen := wen
  io.toDS.wdata_s3.data := Mux(
    !mshr_req_s3,
    c_releaseData_s3,
    Mux(
      req_s3.replaceTask || req_s3.useProbeData,
      s3Decision.bits.release.bits.data,
      s3Decision.bits.refill.bits.data.data
    )
  )

  /* ======== Read DS and store data in Buffer ======== */
  val need_write_releaseBuf = need_data_b && need_mshr_s3_b ||
    mshr_replace_capture_s3 ||
    mshr_refill_to_release_copy_s3 ||
    grantVictimCap_s3
  val need_write_refillBuf = normalActionAdmitted_s3 && normalAction.captureRefill

  /* ======== Write Directory ======== */
  // B, C: Requests from Channel B (RXSNP) and Channel C would only downgrade permission,
  //       so there is no need to use 'nestable_*'.
  val metaW_valid_s3_a = normalActionAdmitted_s3 && normalAction.metaWrite
  val invalidateNestedReleaseHit = req_s3.snpHitReleaseToInval
  val metaW_valid_s3_b = s3Decision.bits.bMetaWValid
  val metaW_valid_s3_c = task_s3.valid && s3Decision.bits.cMetaValid
  val metaW_valid_s3_mshr = mshr_req_s3 && (
    req_s3.metaWen && !(mshr_refill_s3 && (retry || mshr_defer_refill_write_s3)) ||
      grantMergeInstall_s3
  )
  require(clientBits == 1)

  val metaW_s3_a = normalAction.meta
  val metaW_s3_b = s3Decision.bits.bMeta
  val metaW_s3_c = s3Decision.bits.cMeta

  // use merge_meta if mergeA
  val metaW_s3_mshr = WireInit(Mux(req_s3.mergeA, req_s3.aMergeTask.meta, req_s3.meta))
  metaW_s3_mshr.tagErr := req_s3.denied
  metaW_s3_mshr.dataErr := req_s3.corrupt

  val metaW_wayOH = Mux(
    commitReplWay,
    replWayOH_s3,
    Mux(mshr_req_s3, reqWayOH_s3, s3Decision.bits.wayOH)
  )

  io.metaWReq.valid := !resetFinish || task_s3.valid && normalActionAdmitted_s3 && (
    metaW_valid_s3_a || metaW_valid_s3_b || metaW_valid_s3_c || metaW_valid_s3_mshr
  )
  io.metaWReq.bits.set := Mux(resetFinish, req_s3.set, resetIdx)
  io.metaWReq.bits.wayOH := Mux(resetFinish, metaW_wayOH, Fill(cacheParams.ways, true.B))
  io.metaWReq.bits.wmeta := Mux(
    resetFinish,
    ParallelPriorityMux(
      Seq(metaW_valid_s3_a, metaW_valid_s3_b, metaW_valid_s3_c, metaW_valid_s3_mshr),
      Seq(metaW_s3_a, metaW_s3_b, metaW_s3_c, metaW_s3_mshr)
    ),
    MetaEntry()
  )

  io.tagWReq.valid := task_s3.valid && (
    req_s3.tagWen && mshr_refill_s3 &&
      !retry && !mshr_defer_refill_write_s3 ||
      grantMergeInstall_s3
  )
  io.tagWReq.bits.set := req_s3.set
  io.tagWReq.bits.wayOH := Mux(commitReplWay, replWayOH_s3, reqWayOH_s3)
  io.tagWReq.bits.wtag := req_s3.tag

  sink_resp_s3_b_metaWen := metaW_valid_s3_b
  sink_resp_s3_b_meta := metaW_s3_b

  /* ======== Interact with Channels (SourceD/TXREQ/TXRSP/TXDAT) ======== */
  val chnl_fire_s3 = d_s3.fire || txreq_s3.fire || txrsp_s3.fire || txdat_s3.fire
  val req_drop_s3 = !need_write_releaseBuf && !need_write_refillBuf && (
    !mshr_req_s3 && need_mshr_s3 || chnl_fire_s3
  ) || mshr_refill_s3 && (retry || mshr_refill_only_s3)

  val data_unready_s3 = hasData_s3 && !mshr_req_s3
  val data_unready_s3_tl = hasData_s3_tl && !mshr_req_s3
  /**
    * The combinational logic path of
    *     Directory metaAll
    * ->  Directory response
    * ->  MainPipe judging whether to respond data
    * is too long. Therefore the sinkB response may be latched to s4 for better timing.
    */
  val d_s3_latch = true
  val txdat_s3_latch = true
  val isD_s3 = Mux(
    mshr_req_s3,
    mshr_cmoresp_s3 && !req_s3.cmoAll || mshr_refill_s3 && !retry && !mshr_refill_only_s3,
    req_s3.fromC || normalAction.directSourceD
  )
  val isD_s3_ready = Mux(
    mshr_req_s3,
    mshr_cmoresp_s3 && !req_s3.cmoAll || mshr_refill_s3 && !retry && !mshr_refill_only_s3,
    req_s3.fromC // normal A responses are latched (d_s3_latch == true)
  )
  val isTXRSP_s3 = Mux(
    mshr_req_s3,
    mshr_snpRespX_s3,
    req_s3.fromB && !need_mshr_s3_b && !hasData_s3
  )
  val isTXDAT_s3 = Mux(
    mshr_req_s3,
    mshr_snpRespDataX_s3 || mshr_cbWrData_s3 || mshr_dct_s3,
    req_s3.fromB && !need_mshr_s3_b && 
      (doRespData && (!data_unready_s3 || req_s3.snpHitRelease && req_s3.snpHitReleaseWithData))
  )
  val isTXDAT_s3_ready = Mux(
    mshr_req_s3,
    mshr_snpRespDataX_s3 || mshr_cbWrData_s3 || mshr_dct_s3,
    req_s3.fromB && !need_mshr_s3_b && !txdat_s3_latch.B &&
      (doRespData && (!data_unready_s3 || req_s3.snpHitRelease && req_s3.snpHitReleaseWithData))
  )
  val isTXREQ_s3 = mshr_req_s3 && (mshr_writeBackFull_s3 || mshr_writeCleanFull_s3 || 
     mshr_writeEvictFull_s3 || mshr_writeEvictOrEvict_s3 || mshr_evict_s3)

  txreq_s3.valid := task_s3.valid && isTXREQ_s3
  txrsp_s3.valid := task_s3.valid && isTXRSP_s3
  txdat_s3.valid := task_s3.valid && isTXDAT_s3_ready
  d_s3.valid := task_s3.valid && isD_s3_ready
  txreq_s3.bits := source_req_s3.toCHIREQBundle()
  txrsp_s3.bits := source_req_s3
  txdat_s3.bits.task := source_req_s3
  txdat_s3.bits.data.data := data_s3
  d_s3.bits.task := source_req_s3
  d_s3.bits.task.opcode := Mux(mshr_req_s3, req_s3.opcode, sink_resp_s3.bits.opcode)
  d_s3.bits.data.data := data_s3

  when (task_s3.valid && !mshr_refill_only_s3) {
    OneHot.checkOneHot(Seq(isTXREQ_s3, isTXRSP_s3, isTXDAT_s3, isD_s3))
  }

  /* ======== nested writeback ======== */
  io.nestedwb.set := req_s3.set
  io.nestedwb.tag := req_s3.tag
  // This serves as VALID signal
  // c_set_dirty is true iff Release has Data
  io.nestedwb.c_set_dirty := task_s3.valid && task_s3.bits.fromC && task_s3.bits.opcode === ReleaseData && task_s3.bits.param === TtoN
  io.nestedwb.c_set_tip := task_s3.valid && task_s3.bits.fromC && task_s3.bits.opcode === Release && task_s3.bits.param === TtoN
  /**
    * Snoop nesting happens when:
    * 1. snoop nests a copy-back request
    * 2. snoop nests a Read/MakeUnique request
    * 
    * *NOTICE: Never allow 'b_inv_dirty' on SnpStash*, SnpQuery and other future snoops that would
    *          leave cache line state untouched.
    *          Never allow 'b_inv_dirty' on SnpOnce* nesting WriteCleanFull, which would end with SC.
    */
  io.nestedwb.b_inv_dirty := task_s3.valid && task_s3.bits.fromB && source_req_s3.snpHitReleaseToInval &&
    !(isSnpStashX(req_s3.chiOpcode.get) || isSnpQuery(req_s3.chiOpcode.get))
  io.nestedwb.b_toB.foreach(_ :=
    task_s3.valid && task_s3.bits.fromB && source_req_s3.metaWen && source_req_s3.meta.state === BRANCH
  )
  io.nestedwb.b_toN.foreach(_ :=
    task_s3.valid && task_s3.bits.fromB && source_req_s3.metaWen && source_req_s3.meta.state === INVALID
  )
  io.nestedwb.b_toClean.foreach(_ :=
    task_s3.valid && task_s3.bits.fromB && source_req_s3.metaWen && !source_req_s3.meta.dirty
  )

  io.nestedwbData := c_releaseData_s3.asTypeOf(new DSBlock)
  
  // TODO: add nested writeback from Snoop

  /* ======== prefetch ======== */
  io.replaceRecord.foreach { u =>
    val refillVictim = task_s3.valid && mshr_req_s3 && mshr_refill_s3 && !retry && need_repl && req_s3.fromA
    u.valid := refillVictim && replResp_valid_hold
    u.bits.reqSource := req_s3.reqSource
    u.bits.victimPAddr := Cat(s3Decision.bits.repl.bits.tag, req_s3.set, 0.U(offsetBits.W))
    u.bits.victimPfSource := s3Decision.bits.repl.bits.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U)
  }

  io.prefetchTrain.foreach {
    train =>      
      // --------------------- CDP ---------------------
      // for VpnTable, we train on both hit & miss
      val cdp_vpn_train_valid = task_s3.valid && ((req_acquire_s3 || req_get_s3) && req_s3.needHint.getOrElse(false.B) || req_s3.mergeA)

      // for FilterTable, we train on hit & evict
      // TODO: narrow this down to train on hitting a CDP prefetched block
      // Hit a block
      val cdp_filter_train_hit = task_s3.valid && ((req_acquire_s3 || req_get_s3) && req_s3.needHint.getOrElse(false.B) && dirResult_s3.hit || req_s3.mergeA)

      // Evict a unused CDP prefetched block
      val cdp_filter_train_evict = task_s3.valid && mshr_refill_s3 && req_s3.replTask &&
        !s3Decision.bits.repl.bits.retry && !s3Decision.bits.repl.bits.meta.accessed &&
        s3Decision.bits.repl.bits.meta.prefetch.getOrElse(false.B) &&
        s3Decision.bits.repl.bits.meta.prefetchSrc.getOrElse(false.B) === PfSource.CDP.id.U

      if (hasCDP) {
        train.bits.cdp_vpn_train_valid.get  := cdp_vpn_train_valid
        train.bits.cdp_filter_train_hit.get := cdp_filter_train_hit
        train.bits.cdp_filter_train_evict.get := cdp_filter_train_evict
      }

      val is_cdp_train  = cdp_vpn_train_valid || cdp_filter_train_hit || cdp_filter_train_evict

      // --------------------- Other Prefetcher ---------------------
      // train on request(with needHint flag) miss or hit on prefetched block
      // trigger train also in a_merge here
      val is_other_train = task_s3.valid && 
        (
          (req_acquire_s3 || req_get_s3) && req_s3.needHint.getOrElse(false.B) && (!dirResult_s3.hit || metaOnHit_s3.prefetch.get) 
          || req_s3.mergeA
        )
      
      // --------------------- Train Detail ---------------------
      train.valid := is_other_train || (if (hasCDP) is_cdp_train else false.B)
      train.bits.tag := req_s3.tag
      train.bits.set := req_s3.set
      train.bits.needT := Mux(
        req_s3.mergeA,
        needT(req_s3.aMergeTask.opcode, req_s3.aMergeTask.param),
        req_needT_s3
      )
      train.bits.source := Mux(req_s3.mergeA, req_s3.aMergeTask.sourceId, req_s3.sourceId)
      train.bits.vaddr.foreach(_ := Mux(req_s3.mergeA, req_s3.aMergeTask.vaddr.getOrElse(0.U), req_s3.vaddr.getOrElse(0.U)))
      train.bits.pc.foreach { pc =>
        pc := Mux(req_s3.mergeA, req_s3.aMergeTask.pc.getOrElse(0.U), req_s3.pc.getOrElse(0.U))
      }
      train.bits.hit := Mux(req_s3.mergeA, true.B, dirResult_s3.hit)
      train.bits.prefetched := Mux(req_s3.mergeA, true.B, metaOnHit_s3.prefetch.getOrElse(false.B))
      train.bits.pfsource := Mux(req_s3.mergeA, req_s3.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U), metaOnHit_s3.prefetchSrc.getOrElse(PfSource.NoWhere.id.U)) // TODO
      train.bits.reqsource := req_s3.reqSource
      if (hasCDP){
        train.bits.is_cdp_train.get := is_cdp_train
        train.bits.is_other_train.get := is_other_train
      }

      if (hasCDP) {
        train.bits.evict_tag.get := s3Decision.bits.repl.bits.tag
        train.bits.evict_set.get := s3Decision.bits.repl.bits.set
      }
  }

  /* ======== Stage 4 ======== */
  val task_s4 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val data_unready_s4 = RegInit(false.B)
  val data_s4 = Reg(UInt((blockBytes * 8).W))
  val ren_s4 = RegInit(false.B)
  val need_write_releaseBuf_s4 = RegInit(false.B)
  val need_write_refillBuf_s4 = RegInit(false.B)
  val grantVictimCap_s4 = RegInit(false.B)
  val grantVictimSlot_s4 = Reg(UInt(mshrBits.W))
  val isD_s4, isTXREQ_s4, isTXRSP_s4, isTXDAT_s4 = RegInit(false.B)
  val tagError_s4 = RegInit(false.B)
  val dataError_s4 = RegInit(false.B)
  val l2Error_s4 = RegInit(false.B)
  val pendingTXDAT_s4 = task_s4.bits.fromB && !task_s4.bits.mshrTask && task_s4.bits.toTXDAT
  val pendingD_s4 = (task_s4.bits.fromA && !task_s4.bits.mshrTask && !need_write_releaseBuf_s4 && !need_write_refillBuf_s4 &&
    Seq(GrantData, Grant, AccessAckData).map(_ === task_s4.bits.opcode).reduce(_ || _))

  task_s4.valid := task_s3.valid && !req_drop_s3
  val leaseToken_s4 = RegEnable(leaseToken_s3, task_s3.valid && !req_drop_s3)
  val grantToken_s4 = RegEnable(s3Decision.bits.grantToken, task_s3.valid && !req_drop_s3)

  when (task_s3.valid && !req_drop_s3) {
    task_s4.bits := source_req_s3

    when (!task_s3.bits.mshrTask && need_mshr_s3) {
      task_s4.bits.mshrId := io.fromMSHRCtl.mshr_alloc_ptr
    }

    data_unready_s4 := data_unready_s3
    data_s4 := data_s3
    ren_s4 := ren
    need_write_releaseBuf_s4 := need_write_releaseBuf
    need_write_refillBuf_s4 := need_write_refillBuf
    grantVictimCap_s4 := grantVictimCap_s3
    grantVictimSlot_s4 := s3Decision.bits.grantInstallSlot
    isD_s4 := isD_s3
    isTXREQ_s4 := isTXREQ_s3
    isTXRSP_s4 := isTXRSP_s3
    isTXDAT_s4 := isTXDAT_s3
    tagError_s4 := tagError_s3
    dataError_s4 := dataError_s3
    l2Error_s4 := l2Error_s3
  }

  // for reqs that CANNOT give response in MainPipe, but needs to write releaseBuf/refillBuf
  // we cannot drop them at s3, we must let them go to s4/s5
  val chnl_fire_s4 = d_s4.fire || txreq_s4.fire || txrsp_s4.fire || txdat_s4.fire
  val req_drop_s4 = !need_write_releaseBuf_s4 && !need_write_refillBuf_s4 && chnl_fire_s4

  val chnl_valid_s4 = task_s4.valid && !RegNext(chnl_fire_s3, false.B)
  d_s4.valid := chnl_valid_s4 && isD_s4 &&
    !(task_s4.map(b => b.opcode === Grant && b.fromA && !b.mshrTask).bits &&
      !need_write_releaseBuf_s4 && !need_write_refillBuf_s4)
  txreq_s4.valid := chnl_valid_s4 && isTXREQ_s4
  txrsp_s4.valid := chnl_valid_s4 && isTXRSP_s4
  txdat_s4.valid := chnl_valid_s4 && isTXDAT_s4
  d_s4.bits.task := task_s4.bits
  d_s4.bits.data.data := data_s4
  txreq_s4.bits := task_s4.bits.toCHIREQBundle()
  txrsp_s4.bits := task_s4.bits
  txdat_s4.bits.task := task_s4.bits
  txdat_s4.bits.data.data := data_s4

  /* ======== Stage 5 ======== */
  val task_s5 = RegInit(0.U.asTypeOf(Valid(new TaskBundle())))
  val ren_s5 = RegInit(false.B)
  val data_s5 = Reg(UInt((blockBytes * 8).W))
  val need_write_releaseBuf_s5 = RegInit(false.B)
  val need_write_refillBuf_s5 = RegInit(false.B)
  val grantVictimCap_s5 = RegInit(false.B)
  val grantVictimSlot_s5 = Reg(UInt(mshrBits.W))
  val isD_s5, isTXREQ_s5, isTXRSP_s5, isTXDAT_s5 = RegInit(false.B)
  val tagError_s5 = RegInit(false.B)
  val dataMetaError_s5 = RegInit(false.B)
  val l2TagError_s5 = RegInit(false.B)

  task_s5.valid := task_s4.valid && !req_drop_s4
  val leaseToken_s5 = RegEnable(leaseToken_s4, task_s4.valid && !req_drop_s4)
  val grantToken_s5 = RegEnable(grantToken_s4, task_s4.valid && !req_drop_s4)
  io.setStatus.zip(Seq(task_s3_capture, task_s3_decide, task_s3, task_s4, task_s5))
    .foreach { case (status, task) =>
      status.valid := task.valid
      status.bits.set := task.bits.set
      status.bits.pendingSnoop := !task.bits.mshrTask && task.bits.fromB
    }
  val sourceDTransferred_s4 = RegNext(d_s3.fire, false.B)
  val sourceDTransferred_s5 = RegNext(sourceDTransferred_s4 || d_s4.fire, false.B)
  assert(!d_s4.fire || !sourceDTransferred_s4,
    "one task cannot transfer its SourceD reservation twice at S4")
  assert(!d_s5.fire || !sourceDTransferred_s5,
    "one task cannot transfer its SourceD reservation twice at S5")
  assert(!d_s5.valid || d_s5.ready,
    "a terminal SourceD task must own its reserved GrantBuffer capacity")
  val grantReservationEnds = Seq(
    (d_s3.fire || task_s3.valid && req_drop_s3, task_s3.bits, s3Decision.bits.grantToken),
    (d_s4.fire || task_s4.valid && req_drop_s4 && !sourceDTransferred_s4, task_s4.bits, grantToken_s4),
    (d_s5.fire || task_s5.valid && !d_s5.valid && !sourceDTransferred_s5, task_s5.bits, grantToken_s5)
  )
  val grantReservationReturn = Wire(Vec(3, Valid(new GrantReservationToken)))
  grantReservationReturn.zip(grantReservationEnds).foreach { case (event, (end, task, token)) =>
    event.valid := end && (task.fromA || task.fromC)
    event.bits := token.bits
    when(event.valid) {
      assert(token.valid && token.bits.channel === task.channel,
        "each GrantBuffer terminal event must return its own retained token")
    }
  }
  io.grantReservationReturn := RegNext(grantReservationReturn,
    0.U.asTypeOf(grantReservationReturn))
  // Return the source lease only after the final local task or buffer reference.
  val leaseEnds = Seq(
    (task_s3.valid && task_s3.bits.mshrTask && req_drop_s3, task_s3.bits, leaseToken_s3),
    (task_s4.valid && task_s4.bits.mshrTask && req_drop_s4, task_s4.bits, leaseToken_s4),
    (task_s5.valid && task_s5.bits.mshrTask, task_s5.bits, leaseToken_s5)
  )
  // Each pipeline stage can retire a different leased source in the same cycle.
  // Keep every descriptor and token independent through both existing receipt stages.
  val leaseDoneEvents = leaseEnds.map { case (end, task, token) =>
    val event = WireInit(0.U.asTypeOf(Valid(new MainpipeLeaseDone)))
    event.valid := end
    event.bits.mshrId := task.mshrId
    event.bits.mshrContext := task.mshrContext
    event.bits.token := token
    event
  }
  val terminalReceipts = leaseDoneEvents.map { event =>
    RegNext(event, 0.U.asTypeOf(event))
  }
  io.toMSHRCtl.mainpipeLeaseDone.zip(terminalReceipts).foreach { case (out, receipt) =>
    out := RegNext(receipt, 0.U.asTypeOf(receipt))
  }

  when (task_s4.valid && !req_drop_s4) {
    task_s5.bits := task_s4.bits
    ren_s5 := ren_s4
    data_s5 := data_s4
    need_write_releaseBuf_s5 := need_write_releaseBuf_s4
    need_write_refillBuf_s5 := need_write_refillBuf_s4
    grantVictimCap_s5 := grantVictimCap_s4
    grantVictimSlot_s5 := grantVictimSlot_s4
    isD_s5 := isD_s4 || pendingD_s4
    isTXREQ_s5 := isTXREQ_s4
    isTXRSP_s5 := isTXRSP_s4
    isTXDAT_s5 := isTXDAT_s4 || pendingTXDAT_s4
    tagError_s5 := tagError_s4
    dataMetaError_s5 := dataError_s4
    l2TagError_s5 := l2Error_s4
  }
  val rdata_s5 = io.toDS.rdata_s5.data
  val dataError_s5 = io.toDS.error_s5 || dataMetaError_s5
  val l2Error_s5 = l2TagError_s5 || io.toDS.error_s5
  val out_data_s5 = Mux(task_s5.bits.mshrTask || task_s5.bits.snpHitReleaseWithData, data_s5, rdata_s5)
  val chnl_fire_s5 = d_s5.fire || txreq_s5.fire || txrsp_s5.fire || txdat_s5.fire

  // TODO: check this
  val customL1Hint = Module(new CustomL1Hint)

  val mshrHintCapture = RegNext(io.mshrHintQInfo, 0.U.asTypeOf(io.mshrHintQInfo))
  val mshrHintEvent = RegNext(mshrHintCapture, 0.U.asTypeOf(mshrHintCapture))
  val sinkCHintCapture = RegNext(io.sinkCHintQInfo, 0.U.asTypeOf(io.sinkCHintQInfo))
  val sinkCHintEvent = RegNext(sinkCHintCapture, 0.U.asTypeOf(sinkCHintCapture))
  customL1Hint.io.mshrHintQInfo := mshrHintEvent
  customL1Hint.io.sinkCHintQInfo := sinkCHintEvent
  customL1Hint.io.retry_s2 := RegNext(RegNext(io.retryFastFwd_s2 && task_s2.valid, false.B), false.B)
  io.hint_s1QueueFull := customL1Hint.io.hint_s1QueueFull ||
    mshrHintCapture.valid || sinkCHintCapture.valid || mshrHintEvent.valid || sinkCHintEvent.valid

  customL1Hint.io.s3.task      := task_s3
  // overwrite opcode: if sinkReq can respond, use sink_resp_s3.bits.opcode = Grant/GrantData
  customL1Hint.io.s3.task.bits.opcode := Mux(sink_resp_s3.valid, sink_resp_s3.bits.opcode, task_s3.bits.opcode)
  customL1Hint.io.s3.need_mshr := need_mshr_s3_a

  customL1Hint.io.l1Hint <> io.l1Hint

  val releaseBufWriteData_s5 = Mux(task_s5.bits.refillToReleaseCopy, data_s5, rdata_s5)
  io.releaseBufWrite.valid := task_s5.valid && need_write_releaseBuf_s5
  io.releaseBufWrite.bits.id := Mux(grantVictimCap_s5, grantVictimSlot_s5, task_s5.bits.mshrId)
  io.releaseBufWrite.bits.data.data := releaseBufWriteData_s5
  io.releaseBufWrite.bits.beatMask := Fill(beatSize, true.B)
  io.refillBufWrite.valid := task_s5.valid && need_write_refillBuf_s5
  io.refillBufWrite.bits.id := task_s5.bits.mshrId
  io.refillBufWrite.bits.data.data := rdata_s5
  io.refillBufWrite.bits.beatMask := Fill(beatSize, true.B)
  io.toMSHRCtl.replaceCaptureWrite.valid := io.releaseBufWrite.valid &&
    (task_s5.bits.replaceCapture || grantVictimCap_s5)
  io.toMSHRCtl.replaceCaptureWrite.bits := Mux(grantVictimCap_s5, grantVictimSlot_s5, task_s5.bits.mshrId)
  io.toMSHRCtl.refillToReleaseCopyWrite.valid :=
    io.releaseBufWrite.valid && task_s5.bits.refillToReleaseCopy
  io.toMSHRCtl.refillToReleaseCopyWrite.bits := task_s5.bits.mshrId

  val chnl_valid_s5 = task_s5.valid && !RegNext(chnl_fire_s4, false.B) && !RegNextN(chnl_fire_s3, 2, Some(false.B))
  d_s5.valid := chnl_valid_s5 && isD_s5
  txreq_s5.valid := chnl_valid_s5 && isTXREQ_s5
  txrsp_s5.valid := chnl_valid_s5 && isTXRSP_s5
  txdat_s5.valid := chnl_valid_s5 && isTXDAT_s5
  d_s5.bits.task := task_s5.bits
  d_s5.bits.task.denied := Mux(task_s5.bits.mshrTask || task_s5.bits.snpHitReleaseWithData, task_s5.bits.denied, tagError_s5)
  d_s5.bits.task.corrupt := Mux(task_s5.bits.mshrTask || task_s5.bits.snpHitReleaseWithData, task_s5.bits.corrupt, dataError_s5)
  d_s5.bits.data.data := out_data_s5
  txreq_s5.bits := task_s5.bits.toCHIREQBundle()
  txrsp_s5.bits := task_s5.bits
  txrsp_s5.bits.denied := tagError_s5
  txdat_s5.bits.task := task_s5.bits
  txdat_s5.bits.task.denied := tagError_s5
  txdat_s5.bits.task.corrupt := task_s5.bits.corrupt || dataError_s5
  txdat_s5.bits.data.data := out_data_s5

  /* ======== s5 CDP Detect Trigger ======== */
  if (hasCDP){
    val dirResult_s5_hit      = RegNextN(dirResult_s3.hit, 2)
    val dirResult_s5_fromCDP  = RegNextN(dirResult_s3.meta.prefetchSrc.getOrElse(0.U) === PfSource.CDP.id.U, 2)
    val dirResult_s5_fromSMS  = RegNextN(dirResult_s3.meta.prefetchSrc.getOrElse(0.U) === PfSource.SMS.id.U, 2)
    val dirResult_s5_fromBOP  = RegNextN(dirResult_s3.meta.prefetchSrc.getOrElse(0.U) === PfSource.BOP.id.U, 2)
    val dirResult_s5_fromPBOP = RegNextN(dirResult_s3.meta.prefetchSrc.getOrElse(0.U) === PfSource.PBOP.id.U, 2)

    val req_s5_sinkA = !task_s5.bits.mshrTask && task_s5.bits.fromA
    val req_s5_acquire_block = req_s5_sinkA && task_s5.bits.opcode === GrantData

    val is_hit_trigger_s5     = task_s5.valid && dirResult_s5_hit && req_s5_acquire_block &&
      (dirResult_s5_fromCDP || dirResult_s5_fromSMS || dirResult_s5_fromBOP || dirResult_s5_fromPBOP)
    val hit_trigger_data_s5   = out_data_s5
    val hit_trigger_depth_s3  = dirResult_s3.meta.cdpPfDepth.getOrElse(0.U)
    val hit_trigger_pfSrc_s3  = Mux(dirResult_s3.meta.accessed, PfSource.NoWhere.id.U, dirResult_s3.meta.prefetchSrc.getOrElse(0.U))
    val hit_trigger_depth_s5  = RegNextN(hit_trigger_depth_s3, 2)
    val hit_trigger_pfsrc_s5  = RegNextN(hit_trigger_pfSrc_s3, 2)

    val is_refill_trigger_s3 = task_s3.valid && (mshr_grantdata_s3 || mshr_hintack_s3)
    val is_refill_trigger_s5 = RegNextN(is_refill_trigger_s3, 2)
    val refill_trigger_data_s3  = Mux(req_s3.useProbeData, s3Decision.bits.release.bits.data, s3Decision.bits.refill.bits.data.data)
    val refill_trigger_data_s5  = RegNextN(refill_trigger_data_s3, 2)
    val refill_trigger_depth_s5 = RegNextN(metaW_s3_mshr.cdpPfDepth.getOrElse(0.U), 2)
    val refill_trigger_pfsrc_s5 = RegNextN(metaW_s3_mshr.prefetchSrc.getOrElse(0.U), 2)

    val cdp_trigger = io.cdp_trigger.get
    cdp_trigger.valid := is_hit_trigger_s5 || is_refill_trigger_s5
    cdp_trigger.bits.data        := Mux(is_hit_trigger_s5, hit_trigger_data_s5, refill_trigger_data_s5)
    cdp_trigger.bits.pfDepth     := Mux(is_hit_trigger_s5, hit_trigger_depth_s5, refill_trigger_depth_s5)
    cdp_trigger.bits.pfSource    := Mux(is_hit_trigger_s5, hit_trigger_pfsrc_s5, refill_trigger_pfsrc_s5)
    cdp_trigger.bits.isHit       := is_hit_trigger_s5
  }
  io.blockRefillOnlyCommit := Seq(d_s3, d_s4, d_s5).map { d =>
    d.valid && (d.bits.task.opcode(0) ||
      d.bits.task.mergeA && d.bits.task.aMergeTask.opcode(0))
  }.reduce(_ || _)

  /* ======== BlockInfo ======== */
  // if s2/s3 might write Dir, we must block s1 sink entrance
  // TODO:[Check] it seems that s3 Dir write will naturally block all s1 by dirRead.ready
  //        (an even stronger blocking than set blocking)
  //         so we might not need s3 blocking here
  def s23Block(chn: Char, s: TaskBundle): Bool = {
    val s1 = io.fromReqArb.status_s1
    val s1_set = chn match {
      case 'a' => s1.a_set
      case 'b' => s1.b_set
      case 'c' => s1.c_set
      case 'g' => s1.g_set
    }
    s.set === s1_set && !(s.mshrTask && !s.metaWen) // if guaranteed not to write meta, no blocking needed
  }
  def bBlock(s: TaskBundle, tag: Boolean = false): Bool = {
    val s1 = io.fromReqArb.status_s1
    // tag true: compare tag + set
    s.set === s1.b_set && (if(tag) s.tag === s1.b_tag else true.B)
  }

  io.toReqBuf(0) := task_s2.valid && s23Block('a', task_s2.bits)
  io.toReqBuf(1) := task_s3_capture.valid && s23Block('a', task_s3_capture.bits)
  io.toReqBuf(2) := task_s3_decide.valid && s23Block('a', task_s3_decide.bits)
  io.toReqBuf(3) := task_s3.valid && s23Block('a', task_s3.bits)

  io.toReqArb.blockC_s1 := task_s2.valid && s23Block('c', task_s2.bits) ||
    preApplyTasks.map(task => task.valid && s23Block('c', task.bits)).reduce(_ || _)

  io.toReqArb.blockB_s1 :=
    task_s2.valid && bBlock(task_s2.bits) ||
    preApplyTasks.map(task => task.valid && bBlock(task.bits)).reduce(_ || _) ||
    task_s3.valid && bBlock(task_s3.bits) ||
    task_s4.valid && bBlock(task_s4.bits, tag = true) ||
    task_s5.valid && bBlock(task_s5.bits, tag = true)
  
  io.toReqArb.blockA_s1 := false.B

  io.toReqArb.blockG_s1 := task_s2.valid && s23Block('g', task_s2.bits) ||
    preApplyTasks.map(task => task.valid && s23Block('g', task.bits)).reduce(_ || _)
  io.toReqArb.blockAll_s1 := orphanApplyBlocked

  /* ======== Pipeline Status ======== */
  require(io.status_vec_toD.size == 5)
  io.status_vec_toD(0).valid := task_s3.valid && Mux(
    mshr_req_s3,
    mshr_refill_s3 && !retry && !mshr_refill_only_s3,
    true.B
    // TODO:
    // To consider grantBuffer capacity conflict, only " req_s3.fromC || req_s3.fromA && !need_mshr_s3 " is needed
    // But to consider mshrFull, all channel_reqs are needed
    // so maybe it is excessive for grantBuf capacity conflict
  )

  io.status_vec_toD(0).bits.channel := task_s3.bits.channel
  io.status_vec_toD(1).valid        := task_s4.valid && (isD_s4 || pendingD_s4)
  io.status_vec_toD(1).bits.channel := task_s4.bits.channel
  io.status_vec_toD(2).valid        := d_s5.valid
  io.status_vec_toD(2).bits.channel := task_s5.bits.channel
  io.status_vec_toD.drop(3).zip(preApplyTasks).foreach { case (status, task) =>
    status.valid := task.valid
    status.bits.channel := task.bits.channel
  }

  // capacity control of TX channels
  val tx_task_s3 = Wire(Valid(new TaskBundle))
  tx_task_s3.valid := task_s3.valid // TODO: review this
  tx_task_s3.bits := req_s3
  val tasks = preApplyTasks ++ Seq(tx_task_s3, task_s4, task_s5)
  io.status_vec_toTX.zip(tasks).foreach { case (status, task) =>
    status.valid := task.valid
    status.bits.channel := task.bits.channel
    // To optimize timing, we restrict the blocking condition of TXRSP and TXDAT.
    // This may be inaccurate, but it works.
    status.bits.txChannel := task.bits.txChannel
    status.bits.mshrTask := task.bits.mshrTask
  }

  /* ======== Other Signals Assignment ======== */
  // Initial state assignment
  // ! Caution: s_ and w_ are false-as-valid
  when(req_s3.fromA) { alloc_state := normalAction.allocation.state }

  when (req_s3.fromB) { alloc_state := s3Decision.bits.bAllocState }

  val d = Seq(d_s5, d_s4, d_s3)
  val txreq = Seq(txreq_s5, txreq_s4, txreq_s3)
  val txrsp = Seq(txrsp_s5, txrsp_s4, txrsp_s3)
  val txdat = Seq(txdat_s5, txdat_s4, txdat_s3)
  // DO NOT use TLArbiter because TLArbiter will send continuous beats for the same source
  arb(d, io.toSourceD, Some("toSourceD"))
  arb(txreq, io.toTXREQ, Some("toTXREQ"))
  arb(txrsp, io.toTXRSP, Some("toTXRSP"))
  arb(txdat, io.toTXDAT, Some("toTXDAT"))

  val sourceDTask = io.toSourceD.bits.task
  val grantHandoff = Wire(Valid(new MainpipeLeaseDone))
  grantHandoff.valid := io.toSourceD.fire && sourceDTask.mshrTask &&
    sourceDTask.mshrContext === 0.U && !sourceDTask.refillOnly &&
    sourceDTask.deferRefillWrite && !sourceDTask.cmoTask
  grantHandoff.bits.mshrId := sourceDTask.mshrId
  grantHandoff.bits.mshrContext := sourceDTask.mshrContext
  grantHandoff.bits.token := Mux1H(Seq(
    d_s5.fire -> leaseToken_s5, d_s4.fire -> leaseToken_s4, d_s3.fire -> leaseToken_s3))
  val grantReceipt = RegInit(0.U.asTypeOf(Valid(new MainpipeLeaseDone)))
  grantReceipt.valid := grantHandoff.valid
  when(grantHandoff.valid) { grantReceipt.bits := grantHandoff.bits }
  io.toMSHRCtl.grantSent := grantReceipt

  io.error.valid := task_s5.valid
  io.error.bits.valid := l2Error_s5 // if not enableECC, should be false
  io.error.bits.address := Cat(task_s5.bits.tag, task_s5.bits.set, task_s5.bits.off)

  val cmoLineDone = task_s3.valid && mshr_cmoresp_s3 && req_s3.cmoAll
  io.cmoLineDone.foreach { _ := RegNextN(cmoLineDone, 2, Some(false.B)) }

  /* ===== Performance counters ===== */
  // SinkA requests
  XSPerfAccumulate("acquireBlock", task_s3.valid && sinkA_req_s3 && req_s3.opcode === AcquireBlock)
  XSPerfAccumulate("acquirePerm", task_s3.valid && sinkA_req_s3 && req_s3.opcode === AcquirePerm)
  XSPerfAccumulate("prefetch", task_s3.valid && req_prefetch_s3)
  XSPerfAccumulate("get", task_s3.valid && req_get_s3)
  XSPerfAccumulate("cbo_clean", task_s3.valid && req_cbo_clean_s3)
  XSPerfAccumulate("cbo_flush", task_s3.valid && req_cbo_flush_s3)
  XSPerfAccumulate("cbo_inval", task_s3.valid && req_cbo_inval_s3)

  XSPerfAccumulate("dir_wen_tag_cnt", io.tagWReq.valid)
  XSPerfAccumulate("dir_wen_meta_a_hit_cnt", task_s3.valid && metaW_valid_s3_a)
  XSPerfAccumulate("dir_wen_meta_b_snoop_cnt", task_s3.valid && metaW_valid_s3_b)
  XSPerfAccumulate("dir_wen_meta_c_release_cnt", task_s3.valid && metaW_valid_s3_c)
  XSPerfAccumulate("dir_wen_meta_mshr_commit_cnt",
    task_s3.valid && metaW_valid_s3_mshr && req_s3.mshrContext === 0.U && req_s3.refillOnly)
  XSPerfAccumulate("dir_wen_meta_mshr_resp_cnt",
    task_s3.valid && metaW_valid_s3_mshr && (req_s3.mshrContext === 1.U || req_s3.fromB))
  XSPerfAccumulate("dir_wen_meta_mshr_sidecar_cnt",
    task_s3.valid && metaW_valid_s3_mshr && req_s3.mshrContext === 2.U)
  XSPerfAccumulate("dir_wen_meta_mshr_other_cnt",
    task_s3.valid && metaW_valid_s3_mshr && req_s3.mshrContext === 0.U && !req_s3.refillOnly && !req_s3.fromB)
  XSPerfAccumulate("dir_wen_meta_a_hit_nochange_cnt",
    task_s3.valid && metaW_valid_s3_a && metaW_s3_a.asUInt === metaOnHit_s3.asUInt)
  XSPerfAccumulate("dir_wen_meta_c_rel_nochange_cnt",
    task_s3.valid && metaW_valid_s3_c && metaW_s3_c.asUInt === metaOnHit_s3.asUInt)

  // num of mshr req
  XSPerfAccumulate("mshr_grant_req", task_s3.valid && mshr_grant_s3 && !retry)
  XSPerfAccumulate("mshr_grantdata_req", task_s3.valid && mshr_grantdata_s3 && !retry)
  XSPerfAccumulate("mshr_accessackdata_req", task_s3.valid && mshr_accessackdata_s3 && !retry)
  XSPerfAccumulate("mshr_hintack_req", task_s3.valid && mshr_hintack_s3 && !retry)
  // XSPerfAccumulate("mshr_probeack_req", task_s3.valid && mshr_probeack_s3)
  // XSPerfAccumulate("mshr_probeackdata_req", task_s3.valid && mshr_probeackdata_s3)
  // XSPerfAccumulate("mshr_release_req", task_s3.valid && mshr_release_s3)
  XSPerfAccumulate("mshr_snpResp_req", task_s3.valid && mshr_snpResp_s3)
  XSPerfAccumulate("mshr_snpRespFwded_req", task_s3.valid && mshr_snpRespFwded_s3)
  XSPerfAccumulate("mshr_snpRespData_req", task_s3.valid && mshr_snpRespData_s3)
  XSPerfAccumulate("mshr_snpRespDataPtl_req", task_s3.valid && mshr_snpRespDataPtl_s3)
  XSPerfAccumulate("mshr_snpRespDataFwded_req", task_s3.valid && mshr_snpRespDataFwded_s3)
  XSPerfAccumulate("mshr_writeBackFull", task_s3.valid && mshr_writeBackFull_s3)
  XSPerfAccumulate("mshr_writeEvictFull", task_s3.valid && mshr_writeEvictFull_s3)
  XSPerfAccumulate("mshr_writeEvictOrEvict", task_s3.valid && mshr_writeEvictOrEvict_s3)
  XSPerfAccumulate("mshr_evict_s3", task_s3.valid && mshr_evict_s3)
  
  // directory access result
  val hit_s3 = task_s3.valid && !mshr_req_s3 && dirResult_s3.hit
  val miss_s3 = task_s3.valid && !mshr_req_s3 && !dirResult_s3.hit
  XSPerfAccumulate("a_req_hit", hit_s3 && req_s3.fromA)
  XSPerfAccumulate("acquire_hit", hit_s3 && req_s3.fromA &&
    (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm))
  XSPerfAccumulate("get_hit", hit_s3 && req_s3.fromA && req_s3.opcode === Get)
  XSPerfAccumulate("retry", mshr_refill_s3 && retry)

  XSPerfAccumulate("a_req_miss", miss_s3 && req_s3.fromA)
  XSPerfAccumulate("acquire_miss", miss_s3 && req_s3.fromA &&
    (req_s3.opcode === AcquireBlock || req_s3.opcode === AcquirePerm))
  XSPerfAccumulate("get_miss", miss_s3 && req_s3.fromA && req_s3.opcode === Get)

  XSPerfAccumulate("a_need_acquire_on_hit", task_s3.valid && req_s3.fromA && dirResult_s3.hit && acquire_on_hit_s3)
  XSPerfAccumulate("a_need_acquire_on_miss", task_s3.valid && req_s3.fromA && !dirResult_s3.hit && acquire_on_miss_s3)
  XSPerfAccumulate("get_need_probe", task_s3.valid && need_probe_s3_a && req_get_s3)
  XSPerfAccumulate("acquire_need_probe_alias", task_s3.valid && cache_alias)

  XSPerfAccumulate("b_need_probe_snpStable", task_s3.valid && need_pprobe_s3_b_snpStable)
  XSPerfAccumulate("b_need_probe_snpToB", task_s3.valid && need_pprobe_s3_b_snpToB)
  XSPerfAccumulate("b_need_probe_snpToN", task_s3.valid && need_pprobe_s3_b_snpToN)
  XSPerfAccumulate("b_need_dct", task_s3.valid && need_dct_s3_b)

  XSPerfAccumulate("b_req_hit", hit_s3 && req_s3.fromB)
  XSPerfAccumulate("b_req_miss", miss_s3 && req_s3.fromB)

  XSPerfHistogram("a_req_access_way", perfCnt = dirResult_s3.way,
    enable = task_s3.valid && !mshr_req_s3 && req_s3.fromA, start = 0, stop = cacheParams.ways, step = 1)
  XSPerfHistogram("a_req_hit_way", perfCnt = dirResult_s3.way,
    enable = hit_s3 && req_s3.fromA, start = 0, stop = cacheParams.ways, step = 1)
  XSPerfHistogram("a_req_miss_way_choice", perfCnt = dirResult_s3.way,
    enable = miss_s3 && req_s3.fromA, start = 0, stop = cacheParams.ways, step = 1)
  
  XSPerfHistogram("a_req_access_set", perfCnt = task_s3.bits.set,
    enable = task_s3.valid && !mshr_req_s3 && req_s3.fromA,
    start = 0, stop = cacheParams.sets, step = cacheParams.sets / 64)
  XSPerfHistogram("a_req_hit_set", perfCnt = task_s3.bits.set,
    enable = hit_s3 && req_s3.fromA,
    start = 0, stop = cacheParams.sets, step = cacheParams.sets / 64)
  XSPerfHistogram("a_req_miss_set", perfCnt = task_s3.bits.set,
    enable = miss_s3 && req_s3.fromA,
    start = 0, stop = cacheParams.sets, step = cacheParams.sets / 64)

  // pipeline stages for TX and sourceD reqs
  val pipe_len = Seq(5.U, 4.U, 3.U)
  val sourceD_pipe_len = ParallelMux(d.map(_.fire), pipe_len)
  val txreq_pipe_len = ParallelMux(txreq.map(_.fire), pipe_len)
  val txrsp_pipe_len = ParallelMux(txrsp.map(_.fire), pipe_len)
  val txdat_pipe_len = ParallelMux(txdat.map(_.fire), pipe_len)
  XSPerfHistogram("sourceD_pipeline_stages", sourceD_pipe_len,
    enable = io.toSourceD.fire, start = 3, stop = 5+1, step = 1)
  XSPerfHistogram("txreq_pipeline_stages", txreq_pipe_len,
    enable = io.toTXREQ.fire, start = 3, stop = 5+1, step = 1)
  XSPerfHistogram("txrsp_pipeline_stages", txrsp_pipe_len,
    enable = io.toTXRSP.fire, start = 3, stop = 5+1, step = 1)
  XSPerfHistogram("txdat_pipeline_stages", txdat_pipe_len,
    enable = io.toTXDAT.fire, start = 3, stop = 5+1, step = 1)

  // XSPerfAccumulate("a_req_tigger_prefetch", io.prefetchTrain.)
  prefetchOpt.foreach {
    _ =>
      XSPerfAccumulate("a_req_trigger_prefetch", io.prefetchTrain.get.fire)
      XSPerfAccumulate("a_req_trigger_prefetch_not_ready", io.prefetchTrain.get.valid && !io.prefetchTrain.get.ready)
      XSPerfAccumulate("acquire_trigger_prefetch_on_miss", io.prefetchTrain.get.fire && req_acquire_s3 && !dirResult_s3.hit)
      XSPerfAccumulate("acquire_trigger_prefetch_on_hit_pft", io.prefetchTrain.get.fire && req_acquire_s3 && dirResult_s3.hit && meta_s3.prefetch.get)
      // TODO
      // XSPerfAccumulate("release_all", mshr_release_s3)
      // XSPerfAccumulate("release_prefetch_accessed", mshr_release_s3 && meta_s3.prefetch.get && meta_s3.accessed)
      // XSPerfAccumulate("release_prefetch_not_accessed", mshr_release_s3 && meta_s3.prefetch.get && !meta_s3.accessed)
      XSPerfAccumulate("get_trigger_prefetch_on_miss", io.prefetchTrain.get.fire && req_get_s3 && !dirResult_s3.hit)
      XSPerfAccumulate("get_trigger_prefetch_on_hit_pft", io.prefetchTrain.get.fire && req_get_s3 && dirResult_s3.hit && meta_s3.prefetch.get)
  }

  XSPerfAccumulate("early_prefetch", meta_s3.prefetch.getOrElse(false.B) && !meta_s3.accessed && !dirResult_s3.hit && task_s3.valid)

  /* ===== Hardware Performance Monitor ===== */
  val perfEvents = Seq(
    ("l2_cache_hit", hit_s3 && req_s3.fromA),
    ("l2_cache_miss", miss_s3 && req_s3.fromA),
    ("l2_cache_access", task_s3.valid && (sinkA_req_s3 && !req_prefetch_s3 || sinkC_req_s3)),
    ("l2_cache_l2wb", task_s3.valid && (mshr_cbWrData_s3 || mshr_snpRespDataX_s3)),
    ("l2_cache_l1wb", task_s3.valid && sinkC_req_s3 && (req_s3.opcode === ReleaseData)),
    ("l2_cache_wb_victim", task_s3.valid && mshr_cbWrData_s3),
    ("l2_cache_wb_cleaning_coh", task_s3.valid && mshr_snpRespDataX_s3),
    ("l2_cache_prefetch_access", task_s3.valid && sinkA_req_s3 && req_prefetch_s3),
    ("l2_cache_prefetch_miss", task_s3.valid && sinkA_req_s3 && req_prefetch_s3 && miss_s3),
    ("l2_cache_access_rd", task_s3.valid && sinkA_req_s3 && !req_prefetch_s3),
    ("l2_cache_access_wr", task_s3.valid && sinkC_req_s3),
    ("l2_cache_miss_rd", task_s3.valid && sinkA_req_s3 && !req_prefetch_s3 && miss_s3),
    //    ("l2_cache_miss_wr", Inclusive L2 always hit),
    ("l2_cache_inv", task_s3.valid && sinkB_req_s3 && (req_s3.param === toN))
  )
  generatePerfEvent()
}
