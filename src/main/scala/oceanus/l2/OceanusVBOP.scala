// Intent (audit-fix-1): register shared capacity, same-VA serialization and
// issue fairness deviations in the source ledger before updating it.
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
 *
 *
 * Acknowledgement
 *
 * This implementation is inspired by several key papers:
 * [1] Pierre Michaud. "[A Best-Offset Prefetcher.](https://inria.hal.science/hal-01165600/)" 2nd Data Prefetching
 * Championship (DPC). 2015.
 * [2] Pierre Michaud. "[Best-Offset Hardware Prefetching.](https://doi.org/10.1109/HPCA.2016.7446087)" IEEE
 * International Symposium on High Performance Computer Architecture (HPCA). 2016.
 * *************************************************************************************
 */


/*
 * Source ledger (3a): /data/zhuy/coupledL2/prefetch/BestOffsetPrefetch.scala
 * 41-117: parameters/width helpers; 122-350: RR and score; 603-672: delay;
 * 351-601: request buffer; 673-794: virtual BOP pipeline.
 * Structural adaptations: explicit constructor/key parameters instead of TL
 * negotiation; VA=50, PA from L2Params, block=64, page=4096, source=CCHI
 * SrcID+TxnID; standalone train/request bundles instead of coupledL2 bundles.
 * Constantin/ChiselDB/performance utilities retain their reference behavior.
 * RR/score/delay record names use oceanus_vbop to coexist with the independent
 * reference's vbop tables in one test circuit (ChiselDB has a global registry).
 * D1: fixed TLB fire-oh (:450-516) becomes arbitrary-latency handshakes
 * through L2PrefetchTlbAdapter; one miss replay/fault policy stays there.
 * D1 capacity: the 16-entry buffer additionally shares 8 translation slots
 * with CDP; slots remain reserved through client retirement, so congestion can
 * increase full-drop relative to reference per-entry translation state.
 * D1 same VA: distinct needT/source entries translate serially to preserve
 * address-only response ownership; the reference permits parallel entries.
 * D1 fairness: reference entry TwoLevelRRArbiter (:420-421,565-570) becomes
 * global eligible-slot RR in the shared adapter (VBOP still wins admission).
 * Offered TLB payload stays locked across backpressure; every eligible slot
 * is served within the adapter's parameterized available-service-cycle bound.
 * Training: needHint/trainEnable is filtered before TSHR and slice arbitration;
 * enabled-vs-enabled simultaneous pulses retain Oceanus's lossy semantics.
 * D2: needT (:716,750,384) retained for dedup, output always Shared; counter
 * increments on req.fire for a needT=1 request. Future candidate: map needT=1
 * to StashUnique when the base supports that internal prefetch path.
 * D3: optional same-page PA bypass (fork :1138-1189); default false matches
 * reference (all predictions translate). Enabled bypass omits TLB/PMP checks.
 * CCHI source identity substitutes TL sourceId, since oceanus has no TL edge.
 * Training origin is the existing global miss/prefetched-hit event; no mergeA
 * is invented (OceanusBOP ledger documents the base's queued demand behavior).
 */
package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import utility.{ChiselDB, Constantin, MemReqSource, ParallelPriorityMux, XSPerfAccumulate, TwoLevelRRArbiter, ArbPerf}
import utility.sram.SRAMTemplate
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.l2._
import xscache.oceanus.compactchi.CCHIParametersKey
case class OceanusVBOPParameters(
  virtualTrain: Boolean = true,
  rrTableEntries: Int = 256,
  rrTagBits:      Int = 12,
  scoreBits:      Int = 5,
  roundMax:       Int = 50,
  badScore:       Int = 2,
  tlbReplayCnt:   Int = 10,
  dQEntries: Int = 16,
  dQLatency: Int = 300,
  dQMaxLatency: Int = 1024,
  offsetList: Seq[Int] = Seq(
    -256, -250, -243, -240, -225, -216, -200,
    -192, -180, -162, -160, -150, -144, -135, -128,
    -125, -120, -108, -100, -96, -90, -81, -80,
    -75, -72, -64, -60, -54, -50, -48, -45,
    -40, -36, -32, -30, -27, -25, -24, -20,
    -18, -16, -15, -12, -10, -9, -8, -6,
    -5, -4, -3, -2, -1,
    1, 2, 3, 4, 5, 6, 8,
    9, 10, 12, 15, 16, 18, 20, 24,
    25, 27, 30, 32, 36, 40, 45, 48,
    50, 54, 60, 64, 72, 75, 80, 81,
    90, 96, 100, 108, 120, 125, 128, 135,
    144, 150, 160, 162, 180, 192, 200, 216,
    225, 240, 243, 250/*, 256*/
  ),
  samePagePA: Boolean = false
)
case object OceanusVBOPKey extends Field[OceanusVBOPParameters](OceanusVBOPParameters())
trait OceanusVHasBOPParams {
  implicit val p: Parameters
  def bopParams = p(OceanusVBOPKey)
  def fullVAddrBits = 50
  def fullAddressBits = p(L2ParamsKey).physicalAddrWidth
  def fullAddrBits = 50
  def offsetBits = 6
  def sourceIdBits = p(CCHIParametersKey).TxnID_Width + p(CCHIParametersKey).UpstreamNodeID_Width
  def noOffsetAddrBits = 44
  def REQ_FILTER_SIZE = 16
  def rrTableEntries = bopParams.rrTableEntries
  def rrIdxBits = log2Up(rrTableEntries)
  def rrTagBits = bopParams.rrTagBits
  def scoreBits = bopParams.scoreBits
  def roundMax = bopParams.roundMax
  def badScore = bopParams.badScore
  def offsetList = bopParams.offsetList
  def inflightEntries = 16
  def dQEntries = bopParams.dQEntries
  def dQLatency = bopParams.dQLatency
  def dQMaxLatency = bopParams.dQMaxLatency
  def scores = offsetList.length
  def offsetWidth = log2Up(offsetList.max) + 2
  def roundBits = log2Up(roundMax)
  def scoreMax = (1 << scoreBits) - 1
  def scoreTableIdxBits = log2Up(scores)
  def virtualTrain = true
  def get_block_addr(a: UInt) = a(a.getWidth-1, 6)
  def getPPN(a: UInt) = a(a.getWidth-1, 12)
  def parseFullAddress(a: UInt) = (a(a.getWidth-1,12), a(11,6))
  def signedExtend(x: UInt, width: Int): UInt =
    if(x.getWidth >= width) x else Cat(Fill(width-x.getWidth,x.head(1)),x)
  object cacheParams {
    def hartId = 0
    def hasMbist = p(L2ParamsKey).hasMbist
    def hasSramCtl = p(L2ParamsKey).hasSramCtl
  }
}
abstract class OceanusVBOPBundle(implicit val p: Parameters) extends Bundle with OceanusVHasBOPParams
abstract class OceanusVBOPModule(implicit val p: Parameters) extends Module with OceanusVHasBOPParams
class OceanusVScoreTableEntry(implicit p: Parameters) extends OceanusVBOPBundle {
  // val offset = UInt(offsetWidth.W)
  val score = UInt(scoreBits.W)

  def apply(score: UInt) = {
    val entry = Wire(this)
    // entry.offset := offset
    entry.score := score
    entry
  }
}

class OceanusVTestOffsetReq(implicit p: Parameters) extends OceanusVBOPBundle {
  // find whether (X-d) is in recent request table
  val addr = UInt(fullAddrBits.W)
  val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
}

class OceanusVTestOffsetResp(implicit p: Parameters) extends OceanusVBOPBundle {
  // val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIdxBits.W)
  val hit = Bool()
}

class OceanusVTestOffsetBundle(implicit p: Parameters) extends OceanusVBOPBundle {
  val req = DecoupledIO(new OceanusVTestOffsetReq)
  val resp = Flipped(ValidIO(new OceanusVTestOffsetResp))
}

class OceanusVRecentRequestTable(name: String)(implicit p: Parameters) extends OceanusVBOPModule {
  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val r = Flipped(new OceanusVTestOffsetBundle)
  })

  // RR table is direct mapped, accessed through a hash function, each entry holding a partial tag.
  //        +----------+---------------+---------------+----------------------+
  // paddr: |  ......  |  8-bit hash2  |  8-bit hash1  |  6-bit cache offset  |
  //        +----------+---------------+---------------+----------------------+
  //        +-------+------------------+---------------+----------------------+
  //    or: |  ...  |    12-bit tag    |  8-bit hash1  |  6-bit cache offset  |
  //        +-------+------------------+---------------+----------------------+
  def lineAddr(addr: UInt) = addr(fullAddrBits - 1, offsetBits)
  def hash1(addr:    UInt) = lineAddr(addr)(rrIdxBits - 1, 0)
  def hash2(addr:    UInt) = lineAddr(addr)(2 * rrIdxBits - 1, rrIdxBits)
  def idx(addr:      UInt) = hash1(addr) ^ hash2(addr)
  def tag(addr:      UInt) = lineAddr(addr)(rrTagBits + rrIdxBits - 1, rrIdxBits)
  def rrTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(rrTagBits.W)
  }

  val rrTable = Module(
    new SRAMTemplate(
      rrTableEntry(),
      set = rrTableEntries,
      way = 1,
      shouldReset = true,
      singlePort = true,
      hasMbist = cacheParams.hasMbist,
      hasSramCtl = cacheParams.hasSramCtl
    )
  )

  val wAddr = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := idx(wAddr)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(wAddr)

  val rAddr = io.r.req.bits.addr - signedExtend((io.r.req.bits.testOffset << offsetBits), fullAddrBits)
  val rData = Wire(rrTableEntry())
  rrTable.io.r.req.valid := io.r.req.fire
  rrTable.io.r.req.bits.setIdx := idx(rAddr)
  rData := rrTable.io.r.resp.data(0)

  assert(!RegNext(io.w.fire && io.r.req.fire), "single port SRAM should not read and write at the same time")

  /** s0: req handshake */
  val s0_valid = rrTable.io.r.req.fire
  /** s1: rrTable read result */
  val s1_valid = RegNext(s0_valid, false.B)
  val s1_ptr = RegNext(io.r.req.bits.ptr)
  val s1_hit = rData.valid && rData.tag === RegNext(tag(rAddr))
  /** s2: return resp to ScoreTable */
  val s2_valid = RegNext(s1_valid, false.B)

  io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
  io.r.req.ready := true.B
  io.r.resp.valid := s2_valid
  io.r.resp.bits.ptr := RegEnable(s1_ptr, s1_valid)
  io.r.resp.bits.hit := RegEnable(s1_hit, s1_valid)

  class WRRTEntry extends Bundle{
    val addr = UInt(fullAddrBits.W)
  }
  val wrrt = ChiselDB.createTable(name+"WriteRecentRequestTable", new WRRTEntry, basicDB = false)
  val e = Wire(new WRRTEntry)
  e.addr := wAddr
  wrrt.log(e, io.w.valid && !io.r.req.valid, site = "OceanusVRecentRequestTable", clock, reset)

}

class OceanusVOffsetScoreTable(name: String = "")(implicit p: Parameters) extends OceanusVBOPModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val prefetchDisable = Output(Bool())
    val test = new OceanusVTestOffsetBundle
  })

  private val badscoreConstant = Constantin.createRecord(name+"_badScore", bopParams.badScore)
  private val roundMaxConstant = Constantin.createRecord(name+"_roundMax", roundMax)

  val prefetchOffset = RegInit(2.U(offsetWidth.W))
  val prefetchDisable = RegInit(false.B)
  // score table
  // val st = RegInit(VecInit(offsetList.map(off => (new OceanusVScoreTableEntry).apply(off.U, 0.U))))
  val st = RegInit(VecInit(Seq.fill(scores)((new OceanusVScoreTableEntry).apply(0.U))))
  val offList = WireInit(VecInit(offsetList.map(off => off.S(offsetWidth.W).asUInt)))
  val ptr = RegInit(0.U(scoreTableIdxBits.W))
  val round = RegInit(0.U(roundBits.W))

  val bestOffset = RegInit(2.U(offsetWidth.W)) // the entry with the highest score while traversing
  val bestScore = RegInit(0.U)
  val testOffset = offList(ptr)
  // def winner(e1: OceanusVScoreTableEntry, e2: OceanusVScoreTableEntry): OceanusVScoreTableEntry = {
  //   val w = Wire(new OceanusVScoreTableEntry)
  //   w := Mux(e1.score > e2.score, e1, e2)
  //   w
  // }

  val s_idle :: s_learn :: Nil = Enum(2)
  val state = RegInit(s_idle)

  // 1. At the start of a learning phase
  // All the scores are reset to 0.
  // At the end of every learning phase, the prefetch offset is updated as the one with the highest score.
  val isBad = bestScore < badscoreConstant
  when(state === s_idle) {
    st.foreach(_.score := 0.U)
    ptr := 0.U
    round := 0.U
    bestScore := 0.U
    prefetchOffset := bestOffset
    prefetchDisable := isBad
    state := s_learn
  }

  // 2. During a learning phase
  // On every eligible L2 read access (miss or prefetched hit), we test an offset d_i from the list.
  // If X-d_i hits in the RR table, the score of offset d_i is incremented. During a round, each offset
  // in the list is test once. When all the offsets in the list have been tested, the current round is
  // finished, and a new round begins from offset d_1 again.
  // The current learning phase finishes at the end of a round when:
  // (1) one of the score equals SCOREMAX, or
  // (2) the number of rounds equals ROUNDMAX.
  when(state === s_learn) {
    when(io.test.req.fire) {
      val roundFinish = ptr === (scores - 1).U
      ptr := Mux(roundFinish, 0.U, ptr + 1.U)
      round := Mux(roundFinish, round + 1.U, round)
    }

    // (2) the number of rounds equals ROUNDMAX.
    when(round >= roundMaxConstant) {
      state := s_idle
    }

    when(io.test.resp.valid && io.test.resp.bits.hit) {
      val oldScore = st(io.test.resp.bits.ptr).score
      val newScore = oldScore + 1.U
      val offset = offList(io.test.resp.bits.ptr)
      st(io.test.resp.bits.ptr).score := newScore
      // bestOffset := winner((new OceanusVScoreTableEntry).apply(offset, newScore), bestOffset)
      val renewOffset = newScore > bestScore
      bestOffset := Mux(renewOffset, offset, bestOffset)
      bestScore := Mux(renewOffset, newScore, bestScore)
      // (1) one of the score equals SCOREMAX
      when(newScore >= scoreMax.U) {
        state := s_idle
      }
    }
  }

  io.req.ready := state === s_learn
  io.prefetchOffset := prefetchOffset
  io.prefetchDisable := prefetchDisable
  io.test.req.valid := state === s_learn && io.req.valid
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr

  XSPerfAccumulate("total_learn_phase", state === s_idle)
  XSPerfAccumulate("total_bop_disable", state === s_idle && isBad)
  XSPerfAccumulate("total_bop_high_confidence", state === s_idle && bestScore === scoreMax.U)

  for (off <- offsetList) {
    if (off < 0) {
      XSPerfAccumulate("best_offset_neg_" + (-off).toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.S(offsetWidth.W).asUInt).asUInt, 0.U))
    } else {
      XSPerfAccumulate("best_offset_pos_" + off.toString + "_learning_phases",
        Mux(state === s_idle, (bestOffset === off.U).asUInt, 0.U))
    }
  }

  // FIXME lyq: set basicDB false
  // TODO lyq: How to record the offset to the variable name so that the offset is not recorded separately
  class BopTrainEntry extends Bundle {
    val bestOffset = UInt(offsetWidth.W)
    val bestScore = UInt(scoreBits.W)
    val offsets = Vec(offsetList.length, UInt(offsetWidth.W))
    val scores = Vec(offsetList.length, UInt(scoreBits.W))
  }
  val l2BopTrainTable = ChiselDB.createTable(name+"OceanusVOffsetScoreTable", new BopTrainEntry, basicDB = false)
  val data = Wire(new BopTrainEntry)
  data.bestOffset := bestOffset
  data.bestScore := bestScore
  for (i <- 0 until offsetList.length){
    data.offsets(i) := offList(i)
    data.scores(i) := st(i).score
  }
  // l2BopTrainTable.log(data = data, en = (state === s_idle) && !isBad, site = name+"OceanusVOffsetScoreTable", clock, reset)
  l2BopTrainTable.log(data = data, en = (state === s_idle) && !isBad, site = name+"OceanusVOffsetScoreTable", clock, reset)

}

class OceanusVDelayQueue(name: String = "")(implicit p: Parameters) extends  OceanusVBOPModule{
  val io = IO(new Bundle(){
    val pfCtrlOfDelayLatency = Input(UInt(10.W))
    val in = Flipped(DecoupledIO(UInt(noOffsetAddrBits.W)))
    val out = DecoupledIO(UInt(fullAddrBits.W))
    // only record `fullAddrBits - offsetBits` bits
    // out.bits = Cat(record, 0.U(offsetBits))
  })

  /* Setting */
  val IdxWidth = log2Up(dQEntries)
  val LatencyWidth = log2Up(dQMaxLatency)
  class Entry extends Bundle{
    val addrNoOffset = UInt(noOffsetAddrBits.W)
    val cnt = UInt(LatencyWidth.W)
  }
  val queue = RegInit(VecInit(Seq.fill(dQEntries)(0.U.asTypeOf(new Entry))))
  val valids = RegInit(VecInit(Seq.fill(dQEntries)(false.B)))
  val head = RegInit(0.U(IdxWidth.W))
  val tail = RegInit(0.U(IdxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last
  val outValid = !empty && !queue(head).cnt.orR && valids(head)

  /* In & Out */
  var cstDqLatency = Constantin.createRecord(name+"_delayQueueLatency", dQLatency)
  val setDqLatency = Wire(UInt(10.W))
  when(io.pfCtrlOfDelayLatency =/= 0.U) {
    setDqLatency := io.pfCtrlOfDelayLatency
  }.otherwise{
    setDqLatency := cstDqLatency
  }
  when(io.in.valid && !full) {
    // if queue is full, we drop the new request
    queue(tail).addrNoOffset := io.in.bits
    queue(tail).cnt := setDqLatency // dQLatency.U
    valids(tail) := true.B
    tail := tail + 1.U

    /*
    // if full, drop the old request
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
    */
  }
  when(outValid && io.out.ready) {
    valids(head) := false.B
    head := head + 1.U
  }
  io.in.ready := true.B
  io.out.valid := outValid
  io.out.bits := Cat(queue(head).addrNoOffset, 0.U(offsetBits.W))

  /* Update */
  for(i <- 0 until dQEntries){
    when(queue(i).cnt.orR){
      queue(i).cnt := queue(i).cnt - 1.U
    }
  }

  /* Perf */
  XSPerfAccumulate("full", full)
  XSPerfAccumulate("empty", empty)
  XSPerfAccumulate("entryNumber", PopCount(valids.asUInt))
  XSPerfAccumulate("inNumber", io.in.valid)
  XSPerfAccumulate("outNumber", io.out.valid)

}


// Intent: keep reference (:440-508) one-cycle allocation, drop-new and
// VA+needT+source dedup. Translation ownership is explicit until terminal resp;
// identical VAs serialize so the address-only adapter result identifies a slot.
class OceanusVBOPBuffer(implicit p: Parameters) extends OceanusVBOPModule {
  class Entry extends Bundle {
    val va = UInt(50.W)
    val pa = UInt(fullAddressBits.W)
    val needT = Bool()
    val source = UInt(sourceIdBits.W)
    val translated = Bool()
    val waiting = Bool()
  }
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Entry))
    val tlb = Flipped(new L2PrefetchTranslationClientIO()(p))
    val out = Decoupled(new Entry)
  })
  val entries = Reg(Vec(16, new Entry))
  val valids = RegInit(VecInit(Seq.fill(16)(false.B)))
  val previous = RegNext(io.in.valid, false.B)
  val previousVA = RegNext(io.in.bits.va)
  val allocate = Wire(Vec(16, Bool()))
  val matches = VecInit((0 until 16).map(i => valids(i) &&
    entries(i).va === io.in.bits.va && entries(i).needT === io.in.bits.needT &&
    entries(i).source === io.in.bits.source)).asUInt.orR
  val free = VecInit((0 until 16).map(i => !valids(i) && !allocate(i)))
  val accepted = io.in.valid && !(previous && previousVA === io.in.bits.va) && !matches && free.asUInt.orR
  val allocValid = RegNext(accepted, false.B)
  val allocOH = RegNext(PriorityEncoderOH(free.asUInt))
  val incoming = RegNext(io.in.bits)
  val tlbArb = Module(new TwoLevelRRArbiter(UInt(4.W), 16))
  val pfArb = Module(new TwoLevelRRArbiter(UInt(4.W), 16))
  // Keep the selected request stable across arbitrary client backpressure.
  val locked = RegInit(false.B)
  val lockedSlot = Reg(UInt(4.W))
  val selected = Mux(locked, lockedSlot, tlbArb.io.out.bits)
  io.tlb.req.valid := locked || tlbArb.io.out.valid
  io.tlb.req.bits := entries(selected).va
  tlbArb.io.out.ready := io.tlb.req.ready && !locked
  when(io.tlb.req.valid && !io.tlb.req.ready) { locked := true.B; lockedSlot := selected }
  when(io.tlb.req.fire) { locked := false.B; entries(selected).waiting := true.B }
  io.tlb.resp.ready := true.B
  val responseOH = VecInit((0 until 16).map(i => valids(i) && entries(i).waiting &&
    entries(i).va === io.tlb.resp.bits.vaddr))
  when(io.tlb.resp.valid) { assert(PopCount(responseOH) === 1.U) }
  io.out.valid := pfArb.io.out.valid
  io.out.bits := entries(pfArb.io.out.bits)
  pfArb.io.out.ready := io.out.ready
  for (i <- 0 until 16) {
    allocate(i) := allocValid && allocOH(i)
    val sameVAWaiting = (0 until 16).map(j => valids(j) && entries(j).waiting &&
      entries(j).va === entries(i).va).reduce(_ || _)
    tlbArb.io.in(i).valid := valids(i) && !entries(i).translated && !entries(i).waiting && !sameVAWaiting
    tlbArb.io.in(i).bits := i.U
    pfArb.io.in(i).valid := valids(i) && entries(i).translated
    pfArb.io.in(i).bits := i.U
    when(io.tlb.resp.fire && responseOH(i)) {
      entries(i).waiting := false.B
      when(io.tlb.resp.bits.dropped) { valids(i) := false.B }
        .otherwise { entries(i).pa := io.tlb.resp.bits.paddr; entries(i).translated := true.B }
    }
    when(pfArb.io.in(i).fire) { valids(i) := false.B }
    when(allocate(i)) {
      valids(i) := true.B
      entries(i) := incoming
      entries(i).waiting := false.B
    }
  }
  val fullDrops = RegInit(0.U(64.W))
  when(io.in.valid && !matches && !(previous && previousVA === io.in.bits.va) && !free.asUInt.orR) {
    fullDrops := fullDrops + 1.U
  }
  dontTouch(fullDrops)
}

object OceanusVBOP {
  // Build the key override before the Module constructor, without capturing
  // a partially initialized Module in the configuration's partial function.
  private[l2] def configured(outer: Parameters, geometry: OceanusVBOPParameters): Parameters =
    outer.alterPartial { case OceanusVBOPKey => geometry }
}
class OceanusVBOP(val params: OceanusVBOPParameters = OceanusVBOPParameters())(implicit outer: Parameters)
  extends OceanusVBOPModule()(OceanusVBOP.configured(outer, params)) {
  require(params.virtualTrain, "OceanusVBOP is the VA algorithm")
  require(isPow2(params.rrTableEntries) && isPow2(params.dQEntries))
  require(params.tlbReplayCnt == 10, "the shared TLB adapter implements the reference default replay count")
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val delayLatency = Input(UInt(10.W))
    val train = Flipped(Valid(new L2PrefetchTrain(true)(p)))
    val tlb = Flipped(new L2PrefetchTranslationClientIO()(p))
    val req = Decoupled(new L2PrefetchAlgorithmRequest()(p))
    val requestVA = Output(UInt(50.W))
    val originalNeedT = Output(Bool())
    val issuedNeedT = Output(Bool())
  })
  // Reference Prefetcher.scala:332-340: 4 entries, never backpressure cache,
  // full drops the NEW training event. CSR enable gates issue, not this queue.
  val input = Module(new Queue(new L2PrefetchTrain(true)(p), 4))
  input.io.enq.valid := io.train.valid && io.train.bits.trainEnable.get
  input.io.enq.bits := io.train.bits
  val overwritten = RegInit(0.U(64.W)) // legacy observation name: counts drop-new
  when(input.io.enq.valid && !input.io.enq.ready) { overwritten := overwritten + 1.U }
  val delayQueue = Module(new OceanusVDelayQueue("oceanus_vbop")(p))
  val rrTable = Module(new OceanusVRecentRequestTable("oceanus_vbop")(p))
  val scoreTable = Module(new OceanusVOffsetScoreTable("oceanus_vbop")(p))
  val reqFilter = Module(new OceanusVBOPBuffer()(p))
  rrTable.io.r <> scoreTable.io.test
  rrTable.io.w <> delayQueue.io.out
  delayQueue.io.pfCtrlOfDelayLatency := io.delayLatency
  // Reference :710-713 deliberately drive valid rather than train.fire.
  // Preserve it, including the startup/phase-end behavior, for exact learning.
  delayQueue.io.in.valid := input.io.deq.valid
  delayQueue.io.in.bits := input.io.deq.bits.vaddr.get
  scoreTable.io.req.valid := input.io.deq.valid
  scoreTable.io.req.bits := Cat(input.io.deq.bits.vaddr.get, 0.U(6.W))
  input.io.deq.ready := delayQueue.io.in.ready && scoreTable.io.req.ready
  val cstEnable = Constantin.createRecord("vbop_enable0", initValue = 1)
  val va = Cat(input.io.deq.bits.vaddr.get, 0.U(6.W))
  val delta = signedExtend(scoreTable.io.prefetchOffset << 6, 50)
  val predicted = (va + delta)(49, 0)
  val samePage = predicted(49,12) === va(49,12)
  val offer = Wire(new reqFilter.Entry)
  offer.va := predicted
  offer.pa := (input.io.deq.bits.paddr + signedExtend(scoreTable.io.prefetchOffset << 6, fullAddressBits))(fullAddressBits-1,0)
  offer.needT := input.io.deq.bits.needT.get
  offer.source := input.io.deq.bits.sourceId.get
  offer.translated := params.samePagePA.B && samePage
  offer.waiting := false.B
  val generate = input.io.deq.fire && io.enable && cstEnable.orR && !scoreTable.io.prefetchDisable
  reqFilter.io.in.valid := RegNext(generate, false.B)
  reqFilter.io.in.bits := RegEnable(offer, input.io.deq.fire)
  io.tlb <> reqFilter.io.tlb
  io.req.valid := reqFilter.io.out.valid
  io.req.bits.paddr := Cat(reqFilter.io.out.bits.pa(fullAddressBits-1,6), 0.U(6.W))
  io.req.bits.depth := 0.U
  reqFilter.io.out.ready := io.req.ready
  io.requestVA := reqFilter.io.out.bits.va
  io.originalNeedT := reqFilter.io.out.bits.needT
  io.issuedNeedT := false.B
  val needTDowngraded = RegInit(0.U(64.W))
  when(io.req.fire && io.originalNeedT) { needTDowngraded := needTDowngraded + 1.U }
  Seq(overwritten, needTDowngraded).foreach(dontTouch(_))
}
