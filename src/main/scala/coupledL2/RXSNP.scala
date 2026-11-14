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
import org.chipsalliance.cde.config.Parameters
import scala.collection.View.Fill
import xscache.coupledL2.{TaskBundle, MSHRInfo, MetaEntry, MergeTaskBundle}
import xscache.coupledL2.MetaData._
import xscache.chi.CHISNP

class RXSNP(
  lCreditNum: Int = 4 // the number of L-Credits that a receiver can provide
)(implicit p: Parameters) extends CoupledL2Module {
  val io = IO(new Bundle() {
    val rxsnp = Flipped(DecoupledIO(new CHISNP()))
    val task = DecoupledIO(new TaskBundle())
    val admissionEnable = Input(Bool())
    val queued = Output(Bool())
    val pipeSetStatus = Flipped(Vec(8, ValidIO(new PipeSetStatus)))
    val admission = DecoupledIO(new TaskBundle)
    val replaceAdmissionIdle = Input(UInt(mshrsAll.W))
    val sidecarReserve = ValidIO(UInt(mshrBits.W))
    val refillInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
    val snoopInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
    val replaceInfo = Vec(mshrsAll, Flipped(ValidIO(new ReplaceMSHRInfo)))
    val refillVictimDelegated = Input(UInt(mshrsAll.W))
    val replaceAlloc = Vec(4, Flipped(ValidIO(new ReplacementAllocAddress)))
  })

  val rxsnp = Wire(io.rxsnp.cloneType)
  val queue = Module(new Queue(io.rxsnp.bits.cloneType, 2, pipe = false, flow = false))
  rxsnp <> queue.io.deq
  queue.io.enq <> io.rxsnp
  io.queued := queue.io.deq.valid
  val task = Wire(new TaskBundle)

  /**
    * When should an MSHR with Acquire address of X block/nest an incoming snoop with address X?
    * 
    * 1. Before MSHR receives the first beat of CompData, snoop should be **nested** because snoop has higher priority
    *    than request according to CHI spec.
    * 2. After MSHR receives the first beat of CompData, and before L2 receives GrantAck from L1, snoop of X should be
    *    **blocked**, because a slave should not issue a Probe if there is a pending GrantAck on the block according
    *    to TileLink spec.
    * 3. Before MSHR sends out WriteBackFull/Evict to write refilled data into DS, snoop should be **blocked**, Because
    *    the snooped block is still in RefillBuffer rather than DS.
    * 4. After MSHR sends out WriteBackFull/Evict and write refilled data into DS, snoop should be **nested**, still
    *    because snoop has higher priority than request.
    * 5. Before MSHR completes Probe to L1 for alias replacing, snoop should be **blocked*.
    */
  val reqBlockSnpMask = VecInit(io.refillInfo.map(s =>
      s.valid && s.bits.set === task.set && s.bits.reqTag === task.tag && (
      !s.bits.postGrantReleaseHold && (
        s.bits.grantPending ||
        ((s.bits.w_grantfirst || // See [2], [3] above with 's.bits.blockRefill'
          s.bits.aliasTask.getOrElse(false.B) && !s.bits.w_rprobeacklast) &&
          (s.bits.blockRefill || s.bits.w_releaseack ||
            (s.bits.s_refill)))
      ) ||
      s.bits.postGrantReleaseHold && s.bits.refillCommitPending
    )
  )).asUInt
  val reqBlockSnp = reqBlockSnpMask.orR

  /**
    * When should an MSHR that is going to replace cacheline Y block/nest an incoming snoop with address Y?
    * 
    * 1. After MSHR decides which way to replace but before MSHR finished all the rProbes, the incoming snoop of Y
    *    should be **blocked**, because Once the Probe is issued the slave should not issue further Probes on the block
    *    until it receives a ProbeAck.
    * 2. After MSHR receives all the ProbeAcks of rProbe, the snoop of Y should be nested.
    * 3. In CMO transactions (reusing some replacing datapathes), before MSHR finishing all the rProbes and sending
    *    release tasks to MainPipe (DS write was done in release tasks on MainPipe), the incoming snoop of Y should 
    *    be **blocked**.
    */
  val replaceBlockSnpMask = VecInit(io.refillInfo.zipWithIndex.map { case (s, i) =>
    !io.refillVictimDelegated(i) && s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && !s.bits.dirHit && isValid(s.bits.meta.state) &&
    s.bits.w_replResp && (!s.bits.w_rprobeacklast || s.bits.w_releaseack || !RegNext(s.bits.w_replResp))
  }).asUInt
  val replaceBlockSnp = replaceBlockSnpMask.orR
  val sidecarLineMask = VecInit(io.replaceInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.tag === task.tag
  )).asUInt

  // Service snoops while CopyBack is outstanding; only issued immutable CopyBackWrData blocks nesting.
  val sidecarNestReadyMask = VecInit(io.replaceInfo.zipWithIndex.map { case (s, i) =>
    s.bits.probeDone && s.bits.releaseBufReady && io.replaceAdmissionIdle(i) &&
      (!s.bits.releaseStarted || !s.bits.copyBackIssued)
  }).asUInt
  val sidecarNestSnpMask = sidecarLineMask & sidecarNestReadyMask

  // Capture the full snoop snapshot with its concrete S/R reservation; hold the queue owner until B acceptance.
  val admissionValid = RegInit(false.B)
  val admissionTask = RegInit(0.U.asTypeOf(new TaskBundle))
  val admissionOwnerGeneration = Reg(UInt((mshrBits + 1).W))


  val normalVictimAwaitingOwnerMask = VecInit(io.refillInfo.zipWithIndex.map { case (s, i) =>
    !io.refillVictimDelegated(i) && s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag &&
      !s.bits.dirHit && s.bits.meta.state =/= INVALID &&
      RegNext(s.bits.w_replResp) && s.bits.w_rprobeacklast && !s.bits.w_releaseack
  }).asUInt
  val normalVictimAwaitingOwner = normalVictimAwaitingOwnerMask.orR && !sidecarLineMask.orR
  val replaceNestSnpMask = sidecarNestSnpMask
  val releaseToInvalNestSnpMask = VecInit(io.replaceInfo.map(s =>
    !s.bits.releaseToClean)).asUInt & replaceNestSnpMask
  val releaseToCleanNestSnpMask = VecInit(io.replaceInfo.map(s =>
    s.bits.releaseToClean)).asUInt & replaceNestSnpMask
  val nestedSnoopDataMask = VecInit(io.replaceInfo.map(s =>
    s.bits.releaseBufReady && !s.bits.snoopInvalidated)).asUInt
  val sidecarAlreadyInvalidMask = VecInit(io.replaceInfo.map(s =>
    s.valid && s.bits.snoopInvalidated
  )).asUInt
  when (rxsnp.valid && sidecarNestSnpMask.orR && !(sidecarNestSnpMask & sidecarAlreadyInvalidMask).orR) {
    assert((sidecarNestSnpMask & nestedSnoopDataMask).orR,
      "a sidecar-nested snoop must source data from the victim ReleaseBuf")
  }

  val replaceNestSnpMeta = ParallelOR((0 until mshrsAll).map { i =>
    Mux(replaceNestSnpMask(i),
      Mux(io.replaceInfo(i).bits.snoopInvalidated, MetaEntry(), io.replaceInfo(i).bits.meta),
      MetaEntry())
  })

  assert(!rxsnp.valid || PopCount(replaceNestSnpMask) <= 1.U, "multiple replace nest snoop")

  task := fromSnpToTaskBundle(rxsnp.bits)

  val snoopContextBlock = VecInit(io.snoopInfo.map(s =>
    s.valid && s.bits.blocksSnoop && s.bits.set === task.set && s.bits.reqTag === task.tag
  )).asUInt.orR
  val allocatingSidecarMatch = VecInit(io.replaceAlloc.map(s =>
    s.valid && s.bits.set === task.set && s.bits.tag === task.tag
  )).asUInt.orR
  val allocatingSidecarBlockSnp = allocatingSidecarMatch && !sidecarNestSnpMask.orR
  val sidecarBlockSnp = (sidecarLineMask & ~sidecarNestSnpMask).orR ||
    allocatingSidecarBlockSnp
  val pipelineSetConflict = io.pipeSetStatus.map(status =>
    status.valid && status.bits.set === task.set).reduce(_ || _)
  val stall = reqBlockSnp || replaceBlockSnp || sidecarBlockSnp ||
    normalVictimAwaitingOwner || snoopContextBlock || pipelineSetConflict

  io.admission.valid := rxsnp.valid && io.admissionEnable && !admissionValid && !stall
  io.admission.bits := task
  val admissionCapture = io.admission.fire
  io.task.valid := admissionValid
  io.task.bits := admissionTask
  rxsnp.ready := io.task.fire

  val admissionOwnerOH = UIntToOH(task.snpHitReleaseIdx, mshrsAll)
  when(admissionCapture) {
    admissionValid := true.B
    admissionTask := task
    admissionOwnerGeneration := Mux1H(admissionOwnerOH,
      io.replaceInfo.map(s => Cat(s.bits.parentEpoch, s.bits.parentId)))
  }.elsewhen(io.task.fire) {
    admissionValid := false.B
  }
  io.sidecarReserve.valid := RegNext(admissionCapture && task.snpHitRelease, false.B)
  io.sidecarReserve.bits := RegEnable(task.snpHitReleaseIdx, admissionCapture)

  when(admissionValid) {
    assert(rxsnp.valid, "a retained snoop snapshot must keep its CHI queue head")
    assert(fromSnpToTaskBundle(rxsnp.bits).tag === admissionTask.tag &&
      fromSnpToTaskBundle(rxsnp.bits).set === admissionTask.set &&
      rxsnp.bits.txnID === admissionTask.txnID.get,
      "a retained snoop snapshot must keep the same CHI request")
  }

  val stallCnt = RegInit(0.U(64.W))
  when(rxsnp.fire) {
    stallCnt := 0.U
  }.elsewhen(rxsnp.valid && !rxsnp.ready) {
    stallCnt := stallCnt + 1.U
  }

  val STALL_CNT_MAX = 28000.U
  assert(stallCnt <= STALL_CNT_MAX,
    "stallCnt full! maybe there is a deadlock! addr => 0x%x req_opcode => %d txn_id => %d",
    rxsnp.bits.addr, rxsnp.bits.opcode, rxsnp.bits.txnID)

  when (allocatingSidecarBlockSnp) {
    assert(!sidecarNestSnpMask.orR,
      "a sidecar's own nested snoop must not be blocked by its allocation origin")
  }
  val allocBlockOnly = rxsnp.valid && allocatingSidecarBlockSnp &&
    !(reqBlockSnp || replaceBlockSnp || snoopContextBlock ||
      (sidecarLineMask & ~sidecarNestSnpMask).orR)
  val allocBlockCnt = RegInit(0.U(8.W))
  when (allocBlockOnly) {
    allocBlockCnt := allocBlockCnt + 1.U
  }.otherwise {
    allocBlockCnt := 0.U
  }
  val ALLOC_BLOCK_CNT_MAX = 255.U
  assert(allocBlockCnt =/= ALLOC_BLOCK_CNT_MAX,
    "a snoop blocked only by an allocating sidecar must eventually be admitted")

  when(io.task.fire && admissionTask.snpHitRelease) {
    val owner = Mux1H(UIntToOH(admissionTask.snpHitReleaseIdx, mshrsAll), io.replaceInfo)
    assert(owner.valid && owner.bits.set === admissionTask.set &&
      owner.bits.tag === admissionTask.tag &&
      Cat(owner.bits.parentEpoch, owner.bits.parentId) === admissionOwnerGeneration,
      "captured nested snoop must retain its reserved ReleaseBuf owner")
  }
  assert(!admissionCapture || !stall)

  def fromSnpToTaskBundle(snp: CHISNP): TaskBundle = {
    val task = WireInit(0.U.asTypeOf(new TaskBundle))
    task.channel := "b010".U
    // Addr in CHI SNP channel has 3 fewer bits than full address
    val snpFullAddr = Cat(snp.addr, 0.U(3.W))
    val snpAddress = parseAddress(snpFullAddr)
    task.tag := snpAddress._1
    task.set := snpAddress._2
    task.off := snpAddress._3
    task.alias.foreach(_ := 0.U)
    task.vaddr.foreach(_ := 0.U)
    task.isKeyword.foreach(_ := false.B)
    // task.opcode := snp.opcode
    task.param := 0.U
    task.size := log2Up(cacheParams.blockBytes).U
    task.sourceId := 0.U(sourceIdBits.W)
    task.bufIdx := 0.U(bufIdxBits.W)
    task.needProbeAckData := false.B
    task.mshrTask := false.B
    task.mshrId := 0.U(mshrBits.W)
    task.aliasTask.foreach(_ := false.B)
    task.useProbeData := false.B
    task.mshrRetry := false.B
    task.fromL2pft.foreach(_ := false.B)
    task.needHint.foreach(_ := false.B)
    task.dirty := false.B
    task.way := 0.U(wayBits.W)
    task.meta := 0.U.asTypeOf(new MetaEntry)
    task.metaWen := false.B
    task.tagWen := false.B
    task.dsWen := false.B
    task.wayMask := Fill(cacheParams.ways, "b1".U)
    task.reqSource := MemReqSource.NoWhere.id.U
    task.replTask := false.B
    task.mergeA := false.B
    task.aMergeTask := 0.U.asTypeOf(new MergeTaskBundle)
    task.snpHitRelease := replaceNestSnpMask.orR
    task.snpHitReleaseToInval := releaseToInvalNestSnpMask.orR
    task.snpHitReleaseToClean := releaseToCleanNestSnpMask.orR
    task.snpHitReleaseWithData := (replaceNestSnpMask & nestedSnoopDataMask).orR
    task.snpHitReleaseIdx := PriorityEncoder(replaceNestSnpMask)
    task.snpHitReleaseMeta := replaceNestSnpMeta
    task.tgtID.foreach(_ := 0.U) // TODO
    task.srcID.foreach(_ := snp.srcID)
    task.txnID.foreach(_ := snp.txnID)
    task.dbID.foreach(_ := 0.U)
    task.fwdNID.foreach(_ := snp.fwdNID)
    task.fwdTxnID.foreach(_ := snp.fwdTxnID)
    task.chiOpcode.foreach(_ := snp.opcode)
    task.pCrdType.foreach(_ := 0.U)
    task.retToSrc.foreach(_ := snp.retToSrc)
    task.traceTag.foreach(_ := snp.traceTag)
    task
  }

}
