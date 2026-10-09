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
  val cmoBlockSnpMask = VecInit(io.refillInfo.map(s =>
    s.valid && s.bits.dirHit && isValid(s.bits.meta.state) &&
    !s.bits.s_cmoresp && (!s.bits.s_release || !s.bits.w_rprobeacklast || !s.bits.s_cmometaw) &&
    !s.bits.willFree
  )).asUInt
  val cmoBlockSnp = cmoBlockSnpMask.orR
  val replaceBlockSnpMask = VecInit(io.refillInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag && !s.bits.dirHit && isValid(s.bits.meta.state) &&
    s.bits.s_cmoresp && s.bits.w_replResp && (!s.bits.w_rprobeacklast || s.bits.w_releaseack || !RegNext(s.bits.w_replResp)) &&
    !s.bits.willFree
  )).asUInt
  val replaceBlockSnp = replaceBlockSnpMask.orR
  val sidecarBlockSnpMask = VecInit(io.replaceInfo.map(s =>
    s.valid && s.bits.set === task.set && s.bits.tag === task.tag
  )).asUInt

  // A sidecar owns the victim ReleaseBuf until lower transport completes.
  // CHI requires an RN to service snoops while a CopyBack is outstanding, so
  // releaseStarted is not a blocking boundary. Only a CopyBackWrData already
  // issued to MainPipe is immutable; wait for its final data beat in that
  // narrow interval.
  val sidecarNestSnpMask = VecInit(io.replaceInfo.map(s =>
      s.valid && s.bits.set === task.set && s.bits.tag === task.tag &&
      s.bits.probeDone && s.bits.releaseBufReady &&
      (!s.bits.releaseStarted || !s.bits.copyBackIssued)
  )).asUInt

  // '!s.bits.dirHit'     : Nesting a Cache Replacement subsequent release
  // '!s.bits.s_cmoresp'  : Nesting a CMO subsequent release
  val normalReplaceNestSnpMask = VecInit(io.refillInfo.map(s =>
      s.valid && s.bits.set === task.set && s.bits.metaTag === task.tag &&
      (!s.bits.dirHit || !s.bits.s_cmoresp) && s.bits.meta.state =/= INVALID &&
      RegNext(s.bits.w_replResp) && s.bits.w_rprobeacklast && !s.bits.w_releaseack
    )).asUInt
  // A sidecar is the sole ReleaseBuf owner after the parent has delegated a
  // victim. Do not count that same replacement through both representations.
  val replaceNestSnpMask = Mux(
    sidecarNestSnpMask.orR,
    sidecarNestSnpMask,
    normalReplaceNestSnpMask
  )
  val releaseToInvalNestSnpMask = VecInit((0 until mshrsAll).map { i =>
    replaceNestSnpMask(i) && Mux(sidecarNestSnpMask(i),
      !io.replaceInfo(i).bits.releaseToClean, !io.refillInfo(i).bits.releaseToClean)
  }).asUInt
  val releaseToCleanNestSnpMask = VecInit((0 until mshrsAll).map { i =>
    replaceNestSnpMask(i) && Mux(sidecarNestSnpMask(i),
      io.replaceInfo(i).bits.releaseToClean, io.refillInfo(i).bits.releaseToClean)
  }).asUInt
  // A sidecar's ReleaseBuf snapshot is the only legal source once its parent
  // can reuse the victim DS way. For an older normal-MSHR replacement path,
  // retain the existing data-bearing-release predicate.
  val nestedSnoopDataMask = VecInit((0 until mshrsAll).map { i =>
    Mux(sidecarNestSnpMask(i),
      io.replaceInfo(i).bits.releaseBufReady && !io.replaceInfo(i).bits.snoopInvalidated,
      io.refillInfo(i).bits.replaceData)
  }).asUInt
  val sidecarAlreadyInvalidMask = VecInit(io.replaceInfo.map(s =>
    s.valid && s.bits.snoopInvalidated
  )).asUInt
  when (rxsnp.valid && sidecarNestSnpMask.orR && !(sidecarNestSnpMask & sidecarAlreadyInvalidMask).orR) {
    assert((sidecarNestSnpMask & nestedSnoopDataMask).orR,
      "a sidecar-nested snoop must source data from the victim ReleaseBuf")
  }

  val replaceNestSnpMeta = ParallelOR((0 until mshrsAll).map { i =>
    Mux(replaceNestSnpMask(i),
      Mux(sidecarNestSnpMask(i),
        Mux(io.replaceInfo(i).bits.snoopInvalidated, MetaEntry(), io.replaceInfo(i).bits.meta),
        io.refillInfo(i).bits.meta),
      MetaEntry())
  })

  assert(!rxsnp.valid || PopCount(replaceNestSnpMask) <= 1.U, "multiple replace nest snoop")

  task := fromSnpToTaskBundle(rxsnp.bits)

  val snoopContextBlock = VecInit(io.snoopInfo.map(s =>
    s.valid && s.bits.blocksSnoop && s.bits.set === task.set && s.bits.reqTag === task.tag
  )).asUInt.orR
  val sidecarBlockSnp = (sidecarBlockSnpMask & ~sidecarNestSnpMask).orR
  val stall = reqBlockSnp || replaceBlockSnp || cmoBlockSnp || sidecarBlockSnp || snoopContextBlock
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

  assert(!(stall && rxsnp.fire))

  def fromSnpToTaskBundle(snp: CHISNP): TaskBundle = {
    val task = WireInit(0.U.asTypeOf(new TaskBundle))
    task.channel := "b010".U
    // Addr in CHI SNP channel has 3 fewer bits than full address
    val snpFullAddr = Cat(snp.addr, 0.U(3.W))
    task.tag := parseAddress(snpFullAddr)._1
    task.set := parseAddress(snpFullAddr)._2
    task.off := parseAddress(snpFullAddr)._3
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
