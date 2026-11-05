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
 * dfd3edcf / 300515bc: student, committed teacher phase results, split
 * offsets, atomic training, default same-page PA bypass and PBOP skid.
 * Training: needHint/trainEnable is filtered before TSHR and slice arbitration;
 * enabled-vs-enabled simultaneous pulses retain Oceanus's lossy semantics.
 * D2: needT (:716,750,384) retained for dedup, output always Shared; counter
 * increments on req.fire for a needT=1 request. Future candidate: map needT=1
 * to StashUnique when the base supports that internal prefetch path.
 * D3 superseded by upstream dfd3edcf: same-page PA bypass defaults true.
 * It uses the training PA and intentionally omits TLB/PMP for same-page targets.
 * CCHI source identity substitutes TL sourceId, since oceanus has no TL edge.
 * Training origin is the existing global miss/prefetched-hit event; no mergeA
 * is invented (OceanusBOP ledger documents the base's queued demand behavior).
 */
package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters, Field}
import utility.{ChiselDB, Constantin, MemReqSource, ParallelPriorityMux, XSPerfAccumulate, TwoLevelRRArbiter, ArbPerf, XSPerfReference, XSPerfHistogram, SelectByFn}
import utility.sram.{SRAMTemplate, SramBroadcastBundle}
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
  crossPage: Boolean = true,
  useDelayOut: Boolean = true,
  enableStudentCover: Boolean = true,
  studentPoolSize: Int = 4,
  studentFilterTableBits: Int = 4096,
  studentFilterEntryBits: Int = 64,
  studentHashMode: String = "pairs_low9",
  studentCovThreshold: Int = 0,
  issueGateEnable: Boolean = true,
  issueConfThreshold: Int = 14,
  offsetList: Seq[Int] = Seq(
              -117, -147, -91, 117, 147, 91,
              -256, -250, -243, -240, -225, -216, -200,
              -192, -180, -162, -160, -150, -144, -135, -128,
              -125, -120, -108, -100, -96, -90, -81, -80,
              -75, -72, -64, -60, -54, -50, -48, -45,
              -40, -36, 32, 36, 40, 45, 48,
              50, 54, 60, 64, 72, 75, 80, 81,
              90, 96, 100, 108, 120, 125, 128, 135,
              144, 150, 160, 162, 180, 192, 200, 216,
              225, 240, 243, 250 /*, 256*/
            ),
  samePagePA: Boolean = true
)
case object OceanusVBOPKey extends Field[OceanusVBOPParameters](OceanusVBOPParameters())
trait OceanusVHasBOPParams {
  implicit val p: Parameters
  def bopParams = p(OceanusVBOPKey)
  def fullVAddrBits = 50
  def fullAddressBits = p(L2ParamsKey).physicalAddrWidth
  def fullAddrBits = if (virtualTrain) fullVAddrBits else fullAddressBits
  def offsetBits = 6
  def sourceIdBits = p(CCHIParametersKey).TxnID_Width + p(CCHIParametersKey).UpstreamNodeID_Width
  def noOffsetAddrBits = fullAddrBits - offsetBits
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
  def crossPage = bopParams.crossPage
  def useDelayOut = bopParams.useDelayOut
  def enableStudentCover = bopParams.enableStudentCover
  def studentPoolSize = bopParams.studentPoolSize
  def studentFilterTableBits = bopParams.studentFilterTableBits
  def studentFilterEntryBits = bopParams.studentFilterEntryBits
  def studentFilterTableEntries = studentFilterTableBits / studentFilterEntryBits
  def studentFilterEntryOffsetBits = log2Ceil(studentFilterEntryBits)
  def studentFilterEntryIdxBits = log2Ceil(studentFilterTableEntries)
  def studentHashMode = bopParams.studentHashMode
  def studentCovThreshold = bopParams.studentCovThreshold
  def issueGateEnable = bopParams.issueGateEnable
  def issueConfThreshold = bopParams.issueConfThreshold

  def studentPoolIdxBits = log2Up(studentPoolSize)
  def studentFilterIdxBits = log2Up(studentFilterTableBits)
  // Signed confidence values are -1, 0, or +1.
  def studentConfBits = 2
  def studentPhaseTrainBits = log2Up(scores * (roundMax + 2) + 1)
  def studentPhaseTrainMax = scores * (roundMax + 2)
  def scores = offsetList.length
  def offsetWidth = log2Up(offsetList.max) + 2
  def roundBits = log2Up(roundMax)
  def scoreMax = (1 << scoreBits) - 1
  def scoreTableIdxBits = log2Up(scores)
  def virtualTrain = bopParams.virtualTrain
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
abstract class OceanusVBOPModule(implicit val p: Parameters) extends Module with OceanusVHasBOPParams {
  protected def emitOffsetDistCounters(prefix: String, offset: SInt, enable: Bool): Unit = {
    for (off <- offsetList) {
      val counterName =
        if (off < 0) prefix + "_neg_" + (-off).toString
        else prefix + "_pos_" + off.toString
      XSPerfAccumulate(counterName, enable && offset === off.S(offsetWidth.W))
    }
  }

  protected def issueConf(phaseEnd: Bool, teacherDisable: Bool): Bool = {
    val conf = RegInit(0.U(4.W))
    val confAfterDisable = Mux(conf >= 13.U, 15.U, conf + 2.U)
    val confAfterEnable = Mux(conf === 0.U, 0.U, conf - 1.U)
    val nextConf = Mux(teacherDisable, confAfterDisable, confAfterEnable)
    when(phaseEnd) {
      conf := nextConf
    }
    conf < issueConfThreshold.U
  }
}

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

  // [Implementation detail R06] Define the optional SRAM-template DFT inputs.
  // Original: utility exposes broadcast inputs when the RR SRAM has MBIST.
  // This implementation: use Directory's inactive local broadcast defaults.
  // Reason: standalone/target construction otherwise leaves those inputs uninitialized.
  // Status: verification results are recorded in the migration ledger.
  rrTable.io.broadcast.foreach(_ := 0.U.asTypeOf(new SramBroadcastBundle))

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
    val phaseEndPulse = Output(Bool())
    val phaseBestOffsetCommitted = Output(UInt(offsetWidth.W))
    val phaseIssueEnable = Output(Bool())
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
  val bestScore = RegInit(0.U(scoreBits.W))
  val testOffset = offList(ptr)
  val phaseEndCommit = WireDefault(false.B)
  val phaseBestOffset = RegInit(2.U(offsetWidth.W))
  val phaseIssueEnable = RegInit(false.B)
  val respHit = io.test.resp.valid && io.test.resp.bits.hit
  val respPtr = io.test.resp.bits.ptr
  val oldScore = st(respPtr).score
  val newScore = Mux(oldScore === scoreMax.U, oldScore, oldScore + 1.U)
  val respOffset = offList(respPtr)
  val renewOffset = respHit && newScore > bestScore
  val committedBestOffset = Mux(renewOffset, respOffset, bestOffset)
  val committedBestScore = Mux(renewOffset, newScore, bestScore)
  val committedIsBad = committedBestScore < badscoreConstant
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
      phaseEndCommit := true.B
    }

    when(respHit) {
      st(respPtr).score := newScore
      // bestOffset := winner((new OceanusVScoreTableEntry).apply(offset, newScore), bestOffset)
      bestOffset := committedBestOffset
      bestScore := Mux(renewOffset, newScore, bestScore)
      // (1) one of the score equals SCOREMAX
      when(newScore >= scoreMax.U) {
        state := s_idle
        phaseEndCommit := true.B
      }
    }
  }

  when(phaseEndCommit) {
    phaseBestOffset := committedBestOffset
    phaseIssueEnable := !committedIsBad
  }

  io.req.ready := state === s_learn
  io.prefetchOffset := prefetchOffset
  io.prefetchDisable := prefetchDisable
  io.phaseEndPulse := RegNext(phaseEndCommit, false.B)
  io.phaseBestOffsetCommitted := phaseBestOffset
  io.phaseIssueEnable := phaseIssueEnable
  io.test.req.valid := state === s_learn && io.req.valid
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := ptr

  XSPerfAccumulate("total_learn_phase", state === s_idle)
  XSPerfAccumulate("total_bop_disable", state === s_idle && isBad)
  XSPerfAccumulate("total_bop_high_confidence", state === s_idle && bestScore === scoreMax.U)
  emitOffsetDistCounters("teacher_best_offset", io.phaseBestOffsetCommitted.asSInt, io.phaseEndPulse)

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

class OceanusVStudentPoolEntry(implicit p: Parameters) extends OceanusVBOPBundle {
  val valid = Bool()
  val offset = SInt(offsetWidth.W)
  val absOffset = UInt(offsetWidth.W)
  val conf = SInt(studentConfBits.W)
  val lastPhaseCov = UInt(studentPhaseTrainBits.W)
  val curPhaseCov = UInt(studentPhaseTrainBits.W)
}

class OceanusVStudentFilterUpdateReq(implicit p: Parameters) extends OceanusVBOPBundle {
  val baseAddr = UInt(fullAddrBits.W)
  val offsets = Vec(studentPoolSize, SInt(offsetWidth.W))
  val valids = Vec(studentPoolSize, Bool())
}

class OceanusVStudentFilterWriteCmd(implicit p: Parameters) extends OceanusVBOPBundle {
  val setIdxs = Vec(studentPoolSize, UInt(studentFilterEntryIdxBits.W))
  val data = Vec(studentPoolSize, UInt(studentFilterEntryBits.W))
  val valids = Vec(studentPoolSize, Bool())
}

class OceanusVStudentFilterTable(name: String = "")(implicit p: Parameters) extends OceanusVBOPModule {
  require(studentHashMode == "bop_rr" ||
    (studentHashMode == "pairs_low9" && studentFilterTableBits >= 1024),
    s"$name studentHashMode must be bop_rr, or pairs_low9 with at least 1024 table bits")

  private def hashBitPairs(addr: UInt, hi: Int, lo: Int, step: Int = 2): UInt = {
    require(hi >= lo)
    require(step > 0)
    if (hi - lo + 1 <= step) {
      addr(hi, lo)
    } else {
      val remain = (hi - lo + 1) % step
      val xortmp = (lo to (hi - remain) by step).map(i => addr(i + step - 1, i)).reduce(_ ^ _)
      if (remain == 0) {
        xortmp
      } else {
        Cat(xortmp(step - 1, remain), xortmp(remain - 1, 0) ^ addr(hi, hi - remain + 1))
      }
    }
  }

  private def filterIndex(addr: UInt): UInt = {
    val lineAddr = addr(fullAddrBits - 1, offsetBits)
    studentHashMode match {
      case "bop_rr" => (lineAddr ^ (lineAddr >> studentFilterIdxBits))(studentFilterIdxBits - 1, 0)
      case "pairs_low9" =>
        val rawLowBits = 9
        val hashBits = studentFilterIdxBits - rawLowBits
        val hashStartBit = 6
        val foldedHigh = hashBitPairs(lineAddr, lineAddr.getWidth - 1, hashStartBit, hashBits)
        Cat(foldedHigh, lineAddr(rawLowBits - 1, 0))
    }
  }

  private def addOffset(addr: UInt, offset: SInt): UInt = {
    val offsetBytes = signedExtend(offset.asUInt << offsetBits, fullAddrBits)
    (addr + offsetBytes)(fullAddrBits - 1, 0)
  }

  val io = IO(new Bundle {
    val queryReq = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val queryResp = ValidIO(Vec(studentPoolSize, Bool()))
    val updateReq = Flipped(ValidIO(new OceanusVStudentFilterUpdateReq))
    val clear = Input(Bool())
  })

  val tables = Seq.tabulate(studentPoolSize) { i =>
    Module(new SRAMTemplate(
      UInt(studentFilterEntryBits.W),
      set = studentFilterTableEntries,
      way = 1,
      singlePort = true,
      useBitmask = true,
      hasMbist = cacheParams.hasMbist,
      hasSramCtl = cacheParams.hasSramCtl
    )).suggestName(s"table_$i")
  }
  tables.foreach(_.io.broadcast.foreach(_ := 0.U.asTypeOf(new SramBroadcastBundle)))
  val entryValids = RegInit(VecInit(Seq.fill(studentPoolSize)(0.U(studentFilterTableEntries.W))))

  val s0_queryIdx = filterIndex(io.queryReq.bits)
  val s0_queryEntryIdx = s0_queryIdx(studentFilterIdxBits - 1, studentFilterEntryOffsetBits)
  val s0_queryBitIdx = s0_queryIdx(studentFilterEntryOffsetBits - 1, 0)
  val s0_queryEntryValids = Wire(Vec(studentPoolSize, Bool()))
  val s0_queryFire = io.queryReq.fire
  for (i <- 0 until studentPoolSize) {
    s0_queryEntryValids(i) := entryValids(i)(s0_queryEntryIdx)
    tables(i).io.r.req.valid := s0_queryFire
    tables(i).io.r.req.bits.apply(setIdx = s0_queryEntryIdx)
  }

  val s1_queryValid = RegNext(s0_queryFire, false.B)
  val s1_queryBitIdx = RegEnable(s0_queryBitIdx, s0_queryFire)
  val s1_queryEntryValids = RegEnable(s0_queryEntryValids, s0_queryFire)
  io.queryResp.valid := s1_queryValid
  for (i <- 0 until studentPoolSize) {
    io.queryResp.bits(i) := s1_queryEntryValids(i) && tables(i).io.r.resp.data(0)(s1_queryBitIdx)
  }

  val s0_updateEnable = io.updateReq.valid && !io.clear
  val s0_predictedAddrs = Wire(Vec(studentPoolSize, UInt(fullAddrBits.W)))
  for (i <- 0 until studentPoolSize) {
    s0_predictedAddrs(i) := addOffset(io.updateReq.bits.baseAddr, io.updateReq.bits.offsets(i))
  }

  val s1_updateEnable = RegNext(s0_updateEnable, false.B)
  val s1_basePPN = RegEnable(getPPN(io.updateReq.bits.baseAddr), s0_updateEnable)
  val s1_predictedAddrs = RegEnable(s0_predictedAddrs, s0_updateEnable)
  val s1_poolValids = RegEnable(io.updateReq.bits.valids, s0_updateEnable)
  val s1_writeCmd = Wire(new OceanusVStudentFilterWriteCmd)
  for (i <- 0 until studentPoolSize) {
    val predictedAddr = s1_predictedAddrs(i)
    val samePage = getPPN(predictedAddr) === s1_basePPN
    val writeIdx = filterIndex(predictedAddr)
    val writeBitIdx = writeIdx(studentFilterEntryOffsetBits - 1, 0)
    s1_writeCmd.setIdxs(i) := writeIdx(studentFilterIdxBits - 1, studentFilterEntryOffsetBits)
    s1_writeCmd.data(i) := UIntToOH(writeBitIdx, studentFilterEntryBits)
    s1_writeCmd.valids(i) := s1_poolValids(i) && (crossPage.B || samePage)
  }

  val s1_writeEnable = s1_updateEnable && !io.clear
  val s2_writeEnable = RegNext(s1_writeEnable, false.B)
  val s2_writeCmd = RegEnable(s1_writeCmd, s1_writeEnable)
  for (i <- 0 until studentPoolSize) {
    val entryValid = entryValids(i)(s2_writeCmd.setIdxs(i))
    val bitMask = Mux(entryValid, s2_writeCmd.data(i), Fill(studentFilterEntryBits, true.B))
    tables(i).io.w.req.valid := !io.clear && s2_writeEnable && s2_writeCmd.valids(i)
    tables(i).io.w.req.bits.apply(
      data = s2_writeCmd.data(i),
      setIdx = s2_writeCmd.setIdxs(i),
      waymask = 1.U,
      bitmask = bitMask
    )
    when(tables(i).io.w.req.fire) {
      entryValids(i) := entryValids(i).bitSet(s2_writeCmd.setIdxs(i), true.B)
    }
    assert(!(tables(i).io.r.req.valid && tables(i).io.w.req.valid),
      s"$name table $i cannot read and write in the same cycle")
  }

  io.queryReq.ready := !io.clear && !s2_writeEnable

  when(io.clear) {
    entryValids.foreach(_ := 0.U)
  }
}

class OceanusVStudentCoverageLearner(name: String = "")(implicit p: Parameters) extends OceanusVBOPModule {
  require(studentCovThreshold >= 0 && studentCovThreshold <= 5, s"$name studentCovThreshold must be in [0, 5]")

  val io = IO(new Bundle {
    val train = Flipped(DecoupledIO(UInt(fullAddrBits.W)))
    val delayOut = Flipped(ValidIO(UInt(fullAddrBits.W)))
    val teacherPhaseEnd = Input(Bool())
    val teacherBestOffset = Input(UInt(offsetWidth.W))
    val teacherIssueEnable = Input(Bool())
    val selectedOffset = Output(UInt(offsetWidth.W))
    val selectedValid = Output(Bool())
    val selectedEnable = Output(Bool())
  })

  private def absOffset(offset: SInt): UInt = {
    Mux(offset < 0.S, (-offset).asUInt, offset.asUInt)
  }

  private def isBetter(lhs: OceanusVStudentPoolEntry, rhs: OceanusVStudentPoolEntry): Bool = {
    (lhs.curPhaseCov > rhs.curPhaseCov) ||
      (lhs.curPhaseCov === rhs.curPhaseCov && (
        (lhs.conf > rhs.conf) ||
          (lhs.conf === rhs.conf && (
            (lhs.absOffset < rhs.absOffset) ||
              (lhs.absOffset === rhs.absOffset && lhs.offset < rhs.offset)
          ))
      ))
  }

  private def isWorse(lhs: OceanusVStudentPoolEntry, rhs: OceanusVStudentPoolEntry): Bool = {
    (lhs.curPhaseCov < rhs.curPhaseCov) ||
      (lhs.curPhaseCov === rhs.curPhaseCov && (
        (lhs.conf < rhs.conf) ||
          (lhs.conf === rhs.conf && (
            (lhs.absOffset < rhs.absOffset) ||
              (lhs.absOffset === rhs.absOffset && lhs.offset < rhs.offset)
          ))
      ))
  }

  // Replacement is confidence-first; it is independent of phase winner/loser ranking.
  private def isVictim(lhs: OceanusVStudentPoolEntry, rhs: OceanusVStudentPoolEntry): Bool = {
    (lhs.conf < rhs.conf) ||
      (lhs.conf === rhs.conf && (
        (lhs.lastPhaseCov < rhs.lastPhaseCov) ||
          (lhs.lastPhaseCov === rhs.lastPhaseCov && (
            (lhs.absOffset > rhs.absOffset) ||
              (lhs.absOffset === rhs.absOffset && lhs.offset < rhs.offset)
          ))
      ))
  }

  private def covThresholdMet(selectedCov: UInt, trainCount: UInt): Bool = {
    if (studentCovThreshold == 0) {
      true.B
    } else {
      selectedCov >= (trainCount >> studentCovThreshold)
    }
  }

  val pool = RegInit(VecInit(Seq.fill(studentPoolSize)(0.U.asTypeOf(new OceanusVStudentPoolEntry))))
  val filterTable = Module(new OceanusVStudentFilterTable(name))
  val selectedOffset = RegInit(1.S(offsetWidth.W))
  val selectedValid = RegInit(false.B)
  val selectedEnable = RegInit(false.B)
  val phaseTrainCount = RegInit(0.U(studentPhaseTrainBits.W))
  val endHasMatch = RegInit(false.B)
  val endIssueEn = RegInit(false.B)
  val endHasFree = RegInit(false.B)
  val endFreeIdx = RegInit(0.U(studentPoolIdxBits.W))
  val endVictimIdx = RegInit(0.U(studentPoolIdxBits.W))
  val endWorstIdx = RegInit(0.U(studentPoolIdxBits.W))
  val endBestIdx = RegInit(0.U(studentPoolIdxBits.W))
  val endTrainCount = RegInit(0.U(studentPhaseTrainBits.W))
  val endTeacherOffset = RegInit(0.U(offsetWidth.W))

  val s_training :: s_phaseEnd :: Nil = Enum(2)
  val state = RegInit(s_training)
  filterTable.io.clear := io.teacherPhaseEnd || state === s_phaseEnd

  // Query one 64-bit entry from every per-offset filter table.
  val s0_queryEnable = state === s_training && !io.teacherPhaseEnd
  filterTable.io.queryReq.valid := io.train.valid && s0_queryEnable
  filterTable.io.queryReq.bits := io.train.bits
  val s0_queryReady = s0_queryEnable && filterTable.io.queryReq.ready
  val s0_queryFire = filterTable.io.queryReq.fire
  val s1_queryValid = filterTable.io.queryResp.valid
  val s1_hitMask = filterTable.io.queryResp.bits

  // Generate filter-table writes for pool offsets
  val s0_writeAddr = if (useDelayOut) io.delayOut.bits else io.train.bits
  val s0_writeSourceValid = if (useDelayOut) io.delayOut.valid else s0_queryFire
  val s0_writeEnable = s0_writeSourceValid && state === s_training && !io.teacherPhaseEnd
  filterTable.io.updateReq.valid := s0_writeEnable
  filterTable.io.updateReq.bits.baseAddr := s0_writeAddr
  for (i <- 0 until studentPoolSize) {
    filterTable.io.updateReq.bits.offsets(i) := pool(i).offset
    filterTable.io.updateReq.bits.valids(i) := pool(i).valid
  }

  val selectInputs = (0 until studentPoolSize).map { i =>
    val in = Wire(Valid(UInt(studentPoolIdxBits.W)))
    in.valid := pool(i).valid
    in.bits := i.U
    in
  }
  val bestIdx = SelectByFn(selectInputs, pool.toSeq, isBetter, Some("bestSelect"), pipe = true)
  val worstIdx = SelectByFn(selectInputs, pool.toSeq, isWorse, Some("worstSelect"), pipe = true)

  // Compute teacher match and phase-end injection inputs
  val victimIdx = SelectByFn(selectInputs, pool.toSeq, isVictim, Some("victimSelect"), pipe = true)
  val matchVec = VecInit(pool.map { t =>
    t.valid && io.teacherIssueEnable && (io.teacherBestOffset.asSInt === t.offset)
  }).asUInt
  val freeVec = VecInit(pool.map(t => !t.valid)).asUInt
  val hasMatch = matchVec.orR
  val hasFree = freeVec.orR
  val freeIdx = PriorityEncoder(freeVec)
  val endInjectIdx = Mux(endHasFree, endFreeIdx, endVictimIdx)

  val endSelectedValid = pool(endBestIdx).valid
  val endSelectedCov = MuxLookup(endBestIdx, 0.U(studentPhaseTrainBits.W))(
    pool.indices.map(i => i.U -> pool(i).curPhaseCov)
  )
  val endSelectedEnable = endSelectedValid && covThresholdMet(endSelectedCov, endTrainCount)
  val endSelectedOffset = pool(endBestIdx).offset
  val endWorstCov = MuxLookup(endWorstIdx, 0.U(studentPhaseTrainBits.W))(
    pool.indices.map(i => i.U -> pool(i).curPhaseCov)
  )

  when(state === s_training) {
    when(io.teacherPhaseEnd) {
      endHasMatch := hasMatch
      endIssueEn := io.teacherIssueEnable
      endHasFree := hasFree
      endFreeIdx := freeIdx
      endVictimIdx := victimIdx.bits
      endWorstIdx := worstIdx.bits
      endBestIdx := bestIdx.bits
      endTrainCount := phaseTrainCount
      endTeacherOffset := io.teacherBestOffset
      state := s_phaseEnd
    }.otherwise {
      when(s1_queryValid) {
        phaseTrainCount := phaseTrainCount + 1.U
        for (i <- 0 until studentPoolSize) {
          // Accumulate coverage after selecting the requested bit from the SRAM entry.
          when(pool(i).valid && s1_hitMask(i)) {
            pool(i).curPhaseCov := pool(i).curPhaseCov + 1.U
          }
        }
      }
    }
  }.otherwise {
    // Select the best entry for next phase
    selectedValid := endSelectedValid
    selectedOffset := endSelectedOffset
    selectedEnable := endSelectedEnable

    // Update PhaseCov counters
    pool.foreach { entry =>
      entry.lastPhaseCov := entry.curPhaseCov
      entry.curPhaseCov := 0.U
    }
    phaseTrainCount := 0.U

    // Confidence update: winner +1, loser -1, all others 0.
    // If winner and loser are the same entry, no confidence is updated.
    when (endBestIdx =/= endWorstIdx) {
      for (i <- 0 until studentPoolSize) {
        when (pool(i).valid) {
          val update = Mux(
            endBestIdx === i.U,
            1.S(studentConfBits.W),
            Mux(endWorstIdx === i.U, (-1).S(studentConfBits.W), 0.S(studentConfBits.W))
          )
          pool(i).conf := update
        }
      }
    }

    // Inject teacher offset
    when (endIssueEn && !endHasMatch) {
      pool(endInjectIdx).valid := true.B
      pool(endInjectIdx).offset := endTeacherOffset.asSInt
      pool(endInjectIdx).absOffset := absOffset(endTeacherOffset.asSInt)
      pool(endInjectIdx).conf := 0.S
      pool(endInjectIdx).curPhaseCov := 0.U
      pool(endInjectIdx).lastPhaseCov := 0.U
    }

    state := s_training
  }

  io.train.ready := s0_queryReady
  io.selectedOffset := selectedOffset.asUInt
  io.selectedValid := selectedValid
  io.selectedEnable := selectedEnable

  XSPerfAccumulate("student_phase_end", state === s_phaseEnd)
  XSPerfAccumulate("student_fallback", state === s_phaseEnd && !endSelectedEnable)
  val studentMetricEnable = state === s_phaseEnd && endSelectedValid
  emitOffsetDistCounters("student_best_offset", endSelectedOffset, studentMetricEnable)
  XSPerfHistogram("student_best_cov", endSelectedCov, studentMetricEnable, 0, 3072, 128)
  XSPerfHistogram("student_worst_cov", endWorstCov, studentMetricEnable, 0, 3072, 128)
  XSPerfHistogram("student_train_count", endTrainCount, state === s_phaseEnd, 1024, 4096, 256)
  XSPerfHistogram("student_best_worst_gap", endSelectedCov - endWorstCov, studentMetricEnable, 0, 768, 64)
}

class OceanusVDelayQueue(name: String = "")(implicit p: Parameters) extends  OceanusVBOPModule{
  val io = IO(new Bundle(){
    val fullDrop = Output(Bool())
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
  io.fullDrop := io.in.valid && full
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
    val issueOffset = SInt(offsetWidth.W)
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
  XSPerfAccumulate("request_buffer_full_drop", io.in.valid && !matches &&
    !(previous && previousVA === io.in.bits.va) && !free.asUInt.orR)
  XSPerfReference("request_buffer_pending", PopCount(valids))
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
    val trainAccepted = Output(Bool()) // R10: input FIFO admission, independent of issue enable.
  })
  // Reference Prefetcher.scala:332-340: 4 entries, never backpressure cache,
  // full drops the NEW training event. CSR enable gates issue, not this queue.
  val input = Module(new Queue(new L2PrefetchTrain(true)(p), 4))
  input.io.enq.valid := io.train.valid && io.train.bits.trainEnable.get
  input.io.enq.bits := io.train.bits
  io.trainAccepted := input.io.enq.fire
  XSPerfAccumulate("vbop_train_raw", input.io.enq.valid)
  XSPerfAccumulate("vbop_train_accept", input.io.enq.fire)
  XSPerfAccumulate("vbop_train_drop", input.io.enq.valid && !input.io.enq.ready)
  XSPerfReference("vbop_train_pending", input.io.count)
  val overwritten = RegInit(0.U(64.W)) // legacy observation name: counts drop-new
  when(input.io.enq.valid && !input.io.enq.ready) { overwritten := overwritten + 1.U }
  val delayQueue = Module(new OceanusVDelayQueue("oceanus_vbop")(p))
  val rrTable = Module(new OceanusVRecentRequestTable("oceanus_vbop")(p))
  val scoreTable = Module(new OceanusVOffsetScoreTable("oceanus_vbop")(p))
  val reqFilter = Module(new OceanusVBOPBuffer()(p))
  rrTable.io.r <> scoreTable.io.test
  rrTable.io.w <> delayQueue.io.out
  delayQueue.io.pfCtrlOfDelayLatency := io.delayLatency
  // Upstream dfd3edcf: all three learners consume the same accepted event.
  val student = if (enableStudentCover) Some(Module(new OceanusVStudentCoverageLearner("oceanus_vbop")(p))) else None
  val studentTrainReady = student.map(_.io.train.ready).getOrElse(true.B)
  val phaseEnd = scoreTable.io.phaseEndPulse
  val teacherOffset = scoreTable.io.prefetchOffset
  val teacherDisable = scoreTable.io.prefetchDisable
  val studentSelectedEnable = student.map(_.io.selectedEnable).getOrElse(false.B)
  val issueOffset = Mux(studentSelectedEnable, student.map(_.io.selectedOffset).getOrElse(teacherOffset), teacherOffset)
  val issueGate = if (issueGateEnable) issueConf(phaseEnd, teacherDisable) else true.B
  val issueEnable = (studentSelectedEnable || !teacherDisable) && issueGate
  delayQueue.io.in.valid := input.io.deq.valid && studentTrainReady && scoreTable.io.req.ready
  delayQueue.io.in.bits := input.io.deq.bits.vaddr.get
  scoreTable.io.req.valid := input.io.deq.valid && delayQueue.io.in.ready && studentTrainReady
  scoreTable.io.req.bits := Cat(input.io.deq.bits.vaddr.get, 0.U(6.W))
  input.io.deq.ready := delayQueue.io.in.ready && scoreTable.io.req.ready && studentTrainReady
  val va = Cat(input.io.deq.bits.vaddr.get, 0.U(6.W))
  student.foreach { learner =>
    learner.io.train.valid := input.io.deq.valid && delayQueue.io.in.ready && scoreTable.io.req.ready
    learner.io.train.bits := va
    learner.io.delayOut.valid := delayQueue.io.out.fire
    learner.io.delayOut.bits := delayQueue.io.out.bits
    learner.io.teacherPhaseEnd := phaseEnd
    learner.io.teacherBestOffset := scoreTable.io.phaseBestOffsetCommitted
    learner.io.teacherIssueEnable := scoreTable.io.phaseIssueEnable
  }
  val cstEnable = Constantin.createRecord("vbop_enable0", initValue = 1)
  val enable = io.enable && cstEnable.orR
  val delta = signedExtend(issueOffset << 6, 50)
  val predicted = (va + delta)(49, 0)
  val samePage = predicted(49,12) === va(49,12)
  val offer = Wire(new reqFilter.Entry)
  offer.va := predicted
  offer.pa := (input.io.deq.bits.paddr + signedExtend(issueOffset << 6, fullAddressBits))(fullAddressBits-1,0)
  offer.needT := input.io.deq.bits.needT.get
  offer.source := input.io.deq.bits.sourceId.get
  offer.translated := params.samePagePA.B && samePage
  offer.waiting := false.B
  offer.issueOffset := issueOffset.asSInt
  val generate = input.io.deq.fire && enable && issueEnable
  val s1Valid = RegNext(generate, false.B)
  reqFilter.io.in.valid := enable && s1Valid
  reqFilter.io.in.bits := RegEnable(offer, input.io.deq.fire)
  XSPerfAccumulate("bop_train", input.io.deq.fire)
  XSPerfAccumulate("bop_req", io.req.fire)
  XSPerfAccumulate("bop_student_takeover", input.io.deq.fire && studentSelectedEnable)
  XSPerfAccumulate("bop_teacher_selected", input.io.deq.fire && !studentSelectedEnable)
  XSPerfReference("bop_teacher_current_offset", teacherOffset.asSInt.pad(64).asUInt)
  XSPerfReference("bop_student_current_offset", student.map(_.io.selectedOffset.asSInt.pad(64).asUInt).getOrElse(teacherOffset.asSInt.pad(64).asUInt))
  XSPerfReference("bop_current_issue_offset", issueOffset.asSInt.pad(64).asUInt)
  XSPerfReference("bop_current_student_enable", studentSelectedEnable.asUInt)
  emitOffsetDistCounters("training_issue_offset", issueOffset.asSInt, input.io.deq.fire)
  XSPerfAccumulate("bop_drop_for_external_disable", input.io.deq.fire && !enable)
  XSPerfAccumulate("bop_drop_for_auto_disable", input.io.deq.fire && enable && !issueEnable)
  emitOffsetDistCounters("prefetch_sent_issue_offset", reqFilter.io.out.bits.issueOffset, io.req.fire)
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
