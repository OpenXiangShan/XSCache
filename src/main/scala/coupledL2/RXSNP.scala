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
    // A normal request, a snoop context, and a replacement victim can all be
    // live concurrently. Keep their addresses independent.
    val refillInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
    val snoopInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo())))
    val replaceInfo = Vec(mshrsAll, Flipped(ValidIO(new ReplaceMSHRInfo)))
    val refillVictimDelegated = Input(UInt(mshrsAll.W))
    val replaceAlloc = Vec(4, Flipped(ValidIO(new ReplacementAllocAddress)))
  })

  val rxsnp = Wire(io.rxsnp.cloneType)
  val queue = Module(new Queue(io.rxsnp.bits.cloneType, 2, flow = false))
  rxsnp <> queue.io.deq
  queue.io.enq <> io.rxsnp
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
      s.valid && s.bits.set === task.set && s.bits.reqTag === task.tag && !s.bits.willFree && (
      // A SourceD Grant has already transferred ownership to L1, while the
      // normal refill commit may still be absent from Directory. Keep the
      // snoop in the queue until that commit is visible; otherwise the snoop
      // would take the transient directory-miss/no-client direct path.
      !s.bits.postGrantReleaseHold && (
        s.bits.grantPending ||
        ((s.bits.w_grantfirst || // See [2], [3] above with 's.bits.blockRefill'
          s.bits.aliasTask.getOrElse(false.B) && !s.bits.w_rprobeacklast) &&
          (s.bits.blockRefill || s.bits.w_releaseack ||
          // A Grant task can leave the MSHR several cycles before its refill
          // reaches DataStorage. During that interval the directory cannot serve
          // a same-line snoop with the newly granted data.
            (s.bits.s_refill && !s.bits.willFree)))
      ) ||
      // In the post-Grant ReleaseData window the paired-snoop fast path only
      // works if the paired context is allocated before the normal commit can
      // fire.  A snoop that is still queued cannot win that race (MainPipe
      // congestion may delay its allocation past the commit), and would then
      // answer a Directory-miss SnpResp while the commit re-installs the line
      // afterwards.  Hold it until the deferred commit is fully visible; the
      // commit never depends on a snoop that has not been allocated, so this
      // cannot deadlock.
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
  // A CMO in flight only conflicts with snoops for its OWN line: blocking
  // every snoop would deadlock against openLLC, whose snoop-unit occupancy
  // gates the entrance that must accept the CMO sidecar's writeback.
  // (observed: CMO waits releaseAck <- openLLC entrance blocked by full
  // snpUnit <- snoop responses blocked here -> ReqBuf Leak).
  // For the CMO's own line, a nestable sidecar services the snoop from its
  // ReleaseBuf snapshot, so yield the block whenever nesting applies.
  val cmoBlockSnpMask = VecInit(io.refillInfo.map(s =>
    s.valid && s.bits.dirHit && isValid(s.bits.meta.state) &&
    !s.bits.s_cmoresp && (!s.bits.s_release || !s.bits.w_rprobeacklast || !s.bits.s_cmometaw) &&
    !s.bits.willFree &&
    s.bits.set === task.set && s.bits.reqTag === task.tag
  )).asUInt
  val replaceBlockSnpMask = VecInit(io.refillInfo.zipWithIndex.map { case (s, i) =>
    !io.refillVictimDelegated(i) && s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && !s.bits.dirHit && isValid(s.bits.meta.state) &&
    s.bits.s_cmoresp && s.bits.w_replResp && (!s.bits.w_rprobeacklast || s.bits.w_releaseack || !RegNext(s.bits.w_replResp)) &&
    !s.bits.willFree
  }).asUInt
  val replaceBlockSnp = replaceBlockSnpMask.orR
  // Share the same-line sidecar compare between the nest and block paths.
  // Both feed the same-cycle RXSNP admission cone, so this is a pure
  // combinational factoring with no handshake or protocol latency change.
  val sidecarLineMask = VecInit(io.replaceInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.tag === task.tag
  )).asUInt

  // A sidecar owns the victim ReleaseBuf until lower transport completes.
  // CHI requires an RN to service snoops while a CopyBack is outstanding, so
  // releaseStarted is not a blocking boundary. Only a CopyBackWrData already
  // issued to MainPipe is immutable; wait for its final data beat in that
  // narrow interval.
  val sidecarNestReadyMask = VecInit(io.replaceInfo.map(s =>
    s.bits.probeDone && s.bits.releaseBufReady &&
      (!s.bits.releaseStarted || !s.bits.copyBackIssued)
  )).asUInt
  val sidecarNestSnpMask = sidecarLineMask & sidecarNestReadyMask

  // A same-line snoop during a CMO is serviced by the CMO sidecar's ReleaseBuf
  // snapshot whenever nesting applies, so the CMO block yields to nesting.
  val cmoBlockSnp = cmoBlockSnpMask.orR && !sidecarNestSnpMask.orR

  // Normal's legacy victim flags describe an unresolved handoff, not a
  // ReleaseBuf owner. All victim release traffic now belongs to ReplaceMSHR.
  // Before delegation, wait for that owner to register. Once delegation is
  // sticky, an absent child means its writeback has completed: do not revive
  // the old normal index or wait for a replacement child. Read Directory.
  val normalVictimAwaitingOwnerMask = VecInit(io.refillInfo.zipWithIndex.map { case (s, i) =>
    !io.refillVictimDelegated(i) && s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag &&
      (!s.bits.dirHit || !s.bits.s_cmoresp) && s.bits.meta.state =/= INVALID &&
      RegNext(s.bits.w_replResp) && s.bits.w_rprobeacklast && !s.bits.w_releaseack
  }).asUInt
  val normalVictimAwaitingOwner = normalVictimAwaitingOwnerMask.orR && !sidecarLineMask.orR
  // RXSNP.task.valid is low while unresolved, so RequestArb never captures
  // the B or holds a B reservation fence. Replacement/CMO owner allocation
  // therefore progresses independently of this waiting snoop.
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
  // Allocation-stage victims cannot nest yet. Compare each raw origin address
  // before its control mux and physical slot selection, then OR the block.
  // R1: the origin is advertised before slot acceptance, so yield the guard
  // once a same-line sidecar is nest-ready. The registered-sidecar term below
  // then owns the decision (block while not ready, nest when ready), so an
  // origin that stays valid after its allocation can never block the newly
  // allocated sidecar's own nested snoop.
  val allocatingSidecarMatch = VecInit(io.replaceAlloc.map(s =>
    s.valid && s.bits.set === task.set && s.bits.tag === task.tag
  )).asUInt.orR
  val allocatingSidecarBlockSnp = allocatingSidecarMatch && !sidecarNestSnpMask.orR
  val sidecarBlockSnp = (sidecarLineMask & ~sidecarNestSnpMask).orR ||
    allocatingSidecarBlockSnp
  val stall = reqBlockSnp || replaceBlockSnp || cmoBlockSnp || sidecarBlockSnp ||
    normalVictimAwaitingOwner || snoopContextBlock
  io.task.valid := rxsnp.valid && !stall
  io.task.bits := task
  rxsnp.ready := io.task.ready && !stall

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

  // A sidecar's nested snoop must never be blocked by the allocation origin
  // that created it: the guard above yields whenever nesting is possible.
  when (allocatingSidecarBlockSnp) {
    assert(!sidecarNestSnpMask.orR,
      "a sidecar's own nested snoop must not be blocked by its allocation origin")
  }
  // Liveness: while the origin is the only reason a snoop waits, the
  // allocation must register its sidecar and hand the line to the terms above.
  // The raw origin itself may outlive the allocation, so bound the
  // exclusively-allocating-blocked window rather than the origin pulse.
  val allocBlockOnly = rxsnp.valid && allocatingSidecarBlockSnp &&
    !(reqBlockSnp || replaceBlockSnp || cmoBlockSnp || snoopContextBlock ||
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

  when(io.task.fire && task.snpHitRelease) {
    assert(sidecarNestSnpMask.orR &&
      (UIntToOH(task.snpHitReleaseIdx, mshrsAll) & sidecarNestSnpMask).orR,
      "captured nested snoop must reference its registered ReleaseBuf owner")
  }
  assert(!(stall && rxsnp.fire))

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
