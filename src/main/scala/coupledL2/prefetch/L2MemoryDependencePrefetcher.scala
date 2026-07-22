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

package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.MultiPortQueue
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, GTimer, MemReqSource, SignExt, XSPerfAccumulate}
import xscache.coupledL2._

/** Parameters for the L2 memory-dependence prefetcher.
  *
  * L2 MDP is selected through L2Param.prefetch like BOP/TP/NL.  It consumes
  * hinted L2 refill values, directly calculates data-dependent virtual targets,
  * translates them, and emits ordinary PrefetchReqs.
  */
case class L2MdpParameters(
  refillQueueEntries: Int = 32,
  candidateQueueEntries: Int = 8,
  translationEntries: Int = 8,
  inflightEntries: Int = 16
) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true

  require(refillQueueEntries > 0)
  require(candidateQueueEntries > 0)
  require(translationEntries > 0 && isPow2(translationEntries))
}

trait HasL2MdpParameters extends HasPrefetchParameters {
  def l2MdpParams: L2MdpParameters = prefetchers.collectFirst {
    case params: L2MdpParameters => params
  }.get

}

/** MainPipe sends one of these records after an MDP-hinted L2 miss refills.
  * data is the correctly extended load value; the remaining fields came from
  * the original L1 TileLink A request and survived the L2 MSHR.
  */
class L2MdpRefill(implicit p: Parameters) extends PrefetchBundle {
  val data = UInt(64.W)
  val imm = UInt(12.W)
  val pc = UInt(64.W)
  val vaddr = UInt(64.W)
  val source = UInt(sourceIdBits.W)
}

class L2MdpCandidate(implicit p: Parameters) extends PrefetchBundle {
  val triggerPC = UInt(64.W)
  val triggerVaddr = UInt(64.W)
  val targetVaddr = UInt(fullVAddrBits.W)
  val source = UInt(sourceIdBits.W)
}

/** Queue simultaneous refill pulses from all L2 slices without a lossy mux. */
class L2MdpRefillFilter(entries: Int)(implicit p: Parameters)
  extends PrefetchModule with HasL2MdpParameters {
  private val banks = 1 << bankBits
  private val rows = (entries + banks - 1) / banks

  val io = IO(new Bundle {
    // One input comes from each slice MainPipe; the output feeds L2 MDP s0.
    val in = Flipped(Vec(banks, ValidIO(new L2MdpRefill)))
    val out = DecoupledIO(new L2MdpRefill)
  })

  val queue = Module(new MultiPortQueue(
    new L2MdpRefill,
    enq_lanes = banks,
    deq_lanes = 1,
    lanes = banks,
    rows = rows
  ))
  queue.io.enq.zip(io.in).foreach { case (enq, in) =>
    enq.valid := in.valid
    enq.bits := in.bits
  }
  io.out <> queue.io.deq.head

  // Performance counters are grouped at the end of this helper class.
  for (i <- 0 until banks) {
    XSPerfAccumulate(s"l2_mdp_refill_filter_drop_$i", io.in(i).valid && !queue.io.enq(i).ready)
  }
  XSPerfAccumulate("l2_mdp_refill_filter_enq", PopCount(queue.io.enq.map(_.fire)))
  XSPerfAccumulate("l2_mdp_refill_filter_deq", io.out.fire)
}

/** Translate and filter L2 MDP virtual candidates before sending L2 requests. */
class L2MdpPrefetchBuffer(entriesNum: Int)(implicit p: Parameters)
  extends PrefetchModule with HasL2MdpParameters {
  require(fullVAddrBits > offsetBits && fullVAddrBits <= 64)

  private val idxBits = log2Ceil(entriesNum)
  private val vlineBits = fullVAddrBits - offsetBits
  private val plineBits = fullAddressBits - offsetBits

  class Entry extends PrefetchBundle {
    val vline = UInt(vlineBits.W)
    val pline = UInt(plineBits.W)
    val paddrValid = Bool()
    // A miss keeps the entry valid; this bit marks subsequent TLB attempts.
    val tlbMiss = Bool()
    val triggerPC = UInt(64.W)
    val triggerVaddr = UInt(64.W)
    val source = UInt(sourceIdBits.W)

    def vaddr: UInt = Cat(vline, 0.U(offsetBits.W))
    def paddr: UInt = Cat(pline, 0.U(offsetBits.W))
  }

  val io = IO(new Bundle {
    // Candidate comes from L2 MDP s2; translated PrefetchReq goes to the
    // central L2 prefetcher source arbiter through the module's dedicated TLB.
    val candidate = Flipped(DecoupledIO(new L2MdpCandidate))
    val tlbReq = new L2ToL1TlbIO(nRespDups = 1)
    val req = DecoupledIO(new PrefetchReq)
    // Retain the source PC alongside the translated request for ChiselDB.
    val triggerPC = Output(UInt(64.W))
  })

  val entries = RegInit(VecInit(Seq.fill(entriesNum)(0.U.asTypeOf(new Entry))))
  val valids = RegInit(VecInit(Seq.fill(entriesNum)(false.B)))

  // Deduplicate exact virtual cache lines. New lines only consume invalid
  // entries, so temporary fullness backpressures the candidate queue instead
  // of silently evicting an in-flight translation.
  val candidateLine = io.candidate.bits.targetVaddr(fullVAddrBits - 1, offsetBits)
  val matchVec = VecInit((0 until entriesNum).map(i => valids(i) && entries(i).vline === candidateLine))
  val invalidVec = VecInit(valids.map(! _))
  val hasMatch = matchVec.asUInt.orR
  val hasInvalid = invalidVec.asUInt.orR
  val allocIdx = PriorityEncoder(invalidVec)
  io.candidate.ready := hasMatch || hasInvalid

  when(io.candidate.fire && !hasMatch) {
    entries(allocIdx).vline := candidateLine
    entries(allocIdx).pline := 0.U
    entries(allocIdx).paddrValid := false.B
    entries(allocIdx).tlbMiss := false.B
    entries(allocIdx).triggerPC := io.candidate.bits.triggerPC
    entries(allocIdx).triggerVaddr := io.candidate.bits.triggerVaddr
    entries(allocIdx).source := io.candidate.bits.source
    valids(allocIdx) := true.B
  }

  /* TLB s0: arbitrate one untranslated entry. */
  val tlbArb = Module(new RRArbiter(new L2TlbReq, entriesNum))
  val pfArb = Module(new Arbiter(UInt(idxBits.W), entriesNum))
  val s0_tlbFireOH = VecInit(tlbArb.io.in.map(_.fire))
  val s0_tlbRetry = VecInit((0 until entriesNum).map(i => s0_tlbFireOH(i) && entries(i).tlbMiss)).asUInt.orR
  val s1_tlbFireOH = RegNext(s0_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH))
  val s2_tlbFireOH = RegNext(s1_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH))
  val s3_tlbFireOH = RegNext(s2_tlbFireOH, 0.U.asTypeOf(s0_tlbFireOH))
  val notInFlight = VecInit((0 until entriesNum).map(i =>
    !s1_tlbFireOH(i) && !s2_tlbFireOH(i) && !s3_tlbFireOH(i)
  ))

  for (i <- 0 until entriesNum) {
    tlbArb.io.in(i).valid := valids(i) && !entries(i).paddrValid && notInFlight(i)
    tlbArb.io.in(i).bits := 0.U.asTypeOf(new L2TlbReq)
    tlbArb.io.in(i).bits.vaddr := entries(i).vaddr
    tlbArb.io.in(i).bits.cmd := TlbCmd.read
    tlbArb.io.in(i).bits.isPrefetch := true.B
    tlbArb.io.in(i).bits.size := 3.U
    tlbArb.io.in(i).bits.kill := false.B
    tlbArb.io.in(i).bits.no_translate := false.B
  }
  tlbArb.io.out.ready := true.B

  /* TLB s0 -> s1: register and send the selected request. */
  val s1_tlbReqValid = RegNext(tlbArb.io.out.valid, false.B)
  val s1_tlbReqBits = RegEnable(tlbArb.io.out.bits, tlbArb.io.out.valid)
  val s1_vaddr = RegEnable(tlbArb.io.out.bits.vaddr, tlbArb.io.out.valid)
  val s1_tlbRetry = RegNext(s0_tlbRetry, false.B)
  io.tlbReq.req.valid := s1_tlbReqValid
  io.tlbReq.req.bits := s1_tlbReqBits
  io.tlbReq.req_kill := false.B
  io.tlbReq.resp.ready := true.B

  /* TLB s2: receive the DTLB response associated with the s0 selection. */
  val s2_tlbRespValid = io.tlbReq.resp.valid
  val s2_tlbRespBits = io.tlbReq.resp.bits
  val s2_vaddr = RegEnable(s1_vaddr, s1_tlbReqValid)

  /* TLB s2 -> s3: register the response for PMP checking and entry update. */
  val s3_tlbRespValid = RegNext(s2_tlbRespValid, false.B)
  val s3_tlbRespBits = RegEnable(s2_tlbRespBits, s2_tlbRespValid)
  val s3_vaddr = RegEnable(s2_vaddr, s2_tlbRespValid)
  val s3_idx = OHToUInt(s3_tlbFireOH)
  val s3_vaddrMismatch = entries(s3_idx).vaddr =/= s3_vaddr
  val s3_responseMatches = s3_tlbRespValid && s3_tlbFireOH.asUInt.orR && !s3_vaddrMismatch
  val s3_miss = s3_responseMatches && s3_tlbRespBits.miss
  val s3_hit = s3_responseMatches && !s3_tlbRespBits.miss
  val s3_fault = s3_hit && (
    s3_tlbRespBits.excp.head.pf.ld || s3_tlbRespBits.excp.head.gpf.ld || s3_tlbRespBits.excp.head.af.ld ||
    io.tlbReq.pmp_resp.ld || io.tlbReq.pmp_resp.mmio || Pbmt.isUncache(s3_tlbRespBits.pbmt)
  )

  // Match MutiLevelPrefetchFilter semantics: retain a TLB miss and retry it
  // after the in-flight mask clears; only a real translation/access fault drops.
  when(s3_miss) {
    entries(s3_idx).tlbMiss := true.B
  }.elsewhen(s3_fault) {
    valids(s3_idx) := false.B
    entries(s3_idx).tlbMiss := false.B
  }.elsewhen(s3_hit) {
    entries(s3_idx).pline := s3_tlbRespBits.paddr.head(fullAddressBits - 1, offsetBits)
    entries(s3_idx).paddrValid := true.B
    entries(s3_idx).tlbMiss := false.B
  }

  // All translated entries compete for the ordinary L2 PrefetchReq interface.
  for (i <- 0 until entriesNum) {
    pfArb.io.in(i).valid := valids(i) && entries(i).paddrValid
    pfArb.io.in(i).bits := i.U
  }
  val pfIdx = pfArb.io.out.bits
  pfArb.io.out.ready := io.req.ready
  io.req.valid := pfArb.io.out.valid
  io.req.bits.tag := parseFullAddress(entries(pfIdx).paddr)._1
  io.req.bits.set := parseFullAddress(entries(pfIdx).paddr)._2
  io.req.bits.vaddr.foreach(_ := entries(pfIdx).vline)
  io.req.bits.needT := false.B
  io.req.bits.source := entries(pfIdx).source
  io.req.bits.pfSource := MemReqSource.Prefetch2L2MDP.id.U
  io.triggerPC := entries(pfIdx).triggerPC

  when(io.req.fire) {
    valids(pfIdx) := false.B
  }

  // This module owns a dedicated non-blocking DTLB requestor.  A response in
  // s2 must correspond to exactly one request selected in s0 and sent in s1.
  assert(PopCount(s0_tlbFireOH) <= 1.U, "L2 MDP TLB request selection must be one-hot")
  assert(!io.tlbReq.req.valid || io.tlbReq.req.ready, "L2 MDP non-blocking TLB request must not be blocked")
  assert(!io.tlbReq.resp.valid || s2_tlbFireOH.asUInt.orR, "L2 MDP TLB response has no matching request")
  assert(!s3_tlbRespValid || !s3_tlbFireOH.asUInt.orR || !s3_vaddrMismatch,
    "L2 MDP TLB response is associated with the wrong virtual address")
  val s4_tlbMiss = RegNext(s3_miss, false.B)
  val s4_tlbMissIdx = RegEnable(s3_idx, s3_miss)
  when(s4_tlbMiss) {
    assert(valids(s4_tlbMissIdx), "L2 MDP TLB miss must retain its prefetch entry for retry")
  }
  when(io.tlbReq.req.fire) {
    assert(io.tlbReq.req.bits.vaddr(offsetBits - 1, 0) === 0.U, "L2 MDP TLB request must be line-aligned")
    assert(io.tlbReq.req.bits.cmd === TlbCmd.read, "L2 MDP TLB request must use read command")
    assert(io.tlbReq.req.bits.isPrefetch, "L2 MDP TLB request must be marked as prefetch")
    assert(!io.tlbReq.req.bits.kill && !io.tlbReq.req.bits.no_translate,
      "L2 MDP TLB request must be translated and must not be killed")
  }

  // Performance counters are grouped at the end of this helper class.
  XSPerfAccumulate("l2_mdp_tlb_candidate", io.candidate.fire)
  XSPerfAccumulate("l2_mdp_tlb_candidate_merge", io.candidate.fire && hasMatch)
  XSPerfAccumulate("l2_mdp_tlb_req", io.tlbReq.req.fire)
  XSPerfAccumulate("l2_mdp_tlb_req_blocked", io.tlbReq.req.valid && !io.tlbReq.req.ready)
  XSPerfAccumulate("l2_mdp_tlb_resp", io.tlbReq.resp.fire)
  XSPerfAccumulate("l2_mdp_tlb_resp_without_req", io.tlbReq.resp.valid && !s2_tlbFireOH.asUInt.orR)
  XSPerfAccumulate("l2_mdp_tlb_req_without_resp", s2_tlbFireOH.asUInt.orR && !io.tlbReq.resp.valid)
  XSPerfAccumulate(
    "l2_mdp_tlb_resp_vaddr_mismatch",
    s3_tlbRespValid && s3_tlbFireOH.asUInt.orR && s3_vaddrMismatch
  )
  XSPerfAccumulate("l2_mdp_tlb_miss", s3_miss)
  XSPerfAccumulate("l2_mdp_tlb_miss_retry", io.tlbReq.req.fire && s1_tlbRetry)
  XSPerfAccumulate("l2_mdp_tlb_fault", s3_fault)
  XSPerfAccumulate("l2_mdp_prefetch_fire", io.req.fire)
}

class L2MemoryDependencePrefetcher(implicit p: Parameters)
  extends PrefetchModule with HasL2MdpParameters {
  require(fullVAddrBits > offsetBits && fullVAddrBits <= 64)

  private val banks = 1 << bankBits
  class L2MdpRefillDBEntry extends Bundle {
    val timeCnt = UInt(64.W)
    val pc = UInt(64.W)
    val vaddr = UInt(64.W)
    val data = UInt(64.W)
    val imm = UInt(12.W)
    val prefetchVaddr = UInt(fullVAddrBits.W)
  }

  class L2MdpPrefetchDBEntry extends Bundle {
    val timeCnt = UInt(64.W)
    // PC carried from the hinted L2 refill through translation to this fire.
    val triggerPC = UInt(64.W)
    val vaddr = UInt(fullVAddrBits.W)
    val paddr = UInt(fullAddressBits.W)
  }

  val io = IO(new Bundle {
    // Each slice MainPipe supplies completed hinted-refill data. The request
    // output joins the central L2 prefetch source list after DTLB/PMP checks.
    val refill = Flipped(Vec(banks, ValidIO(new L2MdpRefill)))
    val enable = Input(Bool())
    val tlbReq = new L2ToL1TlbIO(nRespDups = 1)
    val req = DecoupledIO(new PrefetchReq)
  })

  val refillFilter = Module(new L2MdpRefillFilter(l2MdpParams.refillQueueEntries))
  refillFilter.io.in.zip(io.refill).foreach { case (filterIn, in) =>
    filterIn.valid := in.valid && io.enable
    filterIn.bits := in.bits
  }

  val candidateQueue = Module(new Queue(
    new L2MdpCandidate,
    l2MdpParams.candidateQueueEntries,
    pipe = true,
    flow = false
  ))

  // Every filtered hinted refill directly produces data + signext(imm).  The
  // queue backpressures the refill filter, so no candidate is discarded while
  // the translation buffer is temporarily full.
  val refillTarget = (
    refillFilter.io.out.bits.data + SignExt(refillFilter.io.out.bits.imm, 64)
  )(fullVAddrBits - 1, 0)
  candidateQueue.io.enq.valid := refillFilter.io.out.valid
  candidateQueue.io.enq.bits.triggerPC := refillFilter.io.out.bits.pc
  candidateQueue.io.enq.bits.triggerVaddr := refillFilter.io.out.bits.vaddr
  candidateQueue.io.enq.bits.targetVaddr := refillTarget
  candidateQueue.io.enq.bits.source := refillFilter.io.out.bits.source
  refillFilter.io.out.ready := candidateQueue.io.enq.ready

  val pfBuffer = Module(new L2MdpPrefetchBuffer(l2MdpParams.translationEntries))
  pfBuffer.io.candidate <> candidateQueue.io.deq
  pfBuffer.io.tlbReq <> io.tlbReq
  // Disabling L2 prefetching stops emission without discarding buffered state;
  // re-enabling resumes from the same translated request.
  io.req.valid := pfBuffer.io.req.valid && io.enable
  io.req.bits := pfBuffer.io.req.bits
  pfBuffer.io.req.ready := io.req.ready && io.enable

  // ChiselDB instrumentation is grouped at the end of the class.
  val hartId = cacheParams.hartId
  val refillTable = ChiselDB.createTable(s"l2MdpRefill_hart$hartId", new L2MdpRefillDBEntry, basicDB = true)
  val prefetchTable = ChiselDB.createTable(s"l2MdpPrefetch_hart$hartId", new L2MdpPrefetchDBEntry, basicDB = true)

  val refillLog = Wire(new L2MdpRefillDBEntry)
  refillLog.timeCnt := GTimer()
  refillLog.pc := refillFilter.io.out.bits.pc
  refillLog.vaddr := refillFilter.io.out.bits.vaddr
  refillLog.data := refillFilter.io.out.bits.data
  refillLog.imm := refillFilter.io.out.bits.imm
  refillLog.prefetchVaddr := refillTarget
  refillTable.log(refillLog, refillFilter.io.out.fire, "l2mdp", clock, reset)

  val prefetchLog = Wire(new L2MdpPrefetchDBEntry)
  prefetchLog.timeCnt := GTimer()
  prefetchLog.triggerPC := pfBuffer.io.triggerPC
  prefetchLog.vaddr := pfBuffer.io.req.bits.vaddr.getOrElse(0.U) << offsetBits
  prefetchLog.paddr := pfBuffer.io.req.bits.addr
  prefetchTable.log(prefetchLog, pfBuffer.io.req.fire, "l2mdp", clock, reset)

  // Performance counters are grouped after the DB writers.
  XSPerfAccumulate("l2_mdp_refill", refillFilter.io.out.fire)
  XSPerfAccumulate("l2_mdp_candidate", candidateQueue.io.enq.fire)
  XSPerfAccumulate(
    "l2_mdp_candidate_queue_block",
    refillFilter.io.out.valid && !candidateQueue.io.enq.ready
  )
  XSPerfAccumulate("l2_mdp_req_fire", io.req.fire)
  XSPerfAccumulate("l2_mdp_req_block", io.req.valid && !io.req.ready)
}
