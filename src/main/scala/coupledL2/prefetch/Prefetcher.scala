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
import utility._
import org.chipsalliance.cde.config.Parameters
import utility.mbist.MbistPipeline
import xscache.coupledL2._
import xscache.coupledL2.utils._
import freechips.rocketchip.tilelink.LFSR64
import xscache.chi.{CHIREQ, CHIRSP, HasCHIOpcodes, MPAM, MemAttr, OrderEncodings, SAM}

/* virtual address */
trait HasPrefetcherHelper extends HasCircularQueuePtrHelper with HasCoupledL2Parameters with HasPrefetchParameters {
  // filter
  val TRAIN_FILTER_SIZE = 4
  val REQ_FILTER_SIZE = 16
  val TLB_REPLAY_CNT = 10

  // parameters
  val BLK_ADDR_RAW_WIDTH = 10
  val REGION_SIZE = 1024
  val PAGE_OFFSET = pageOffsetBits
  val VADDR_HASH_WIDTH = 5

  // vaddr:
  // |       tag               |     index     |    offset    |
  // |       block addr                        | block offset |
  // |       region addr       |        region offset         |
  val BLOCK_OFFSET = offsetBits
  val REGION_OFFSET = log2Up(REGION_SIZE)
  val REGION_BLKS = REGION_SIZE / blockBytes
  val INDEX_BITS = log2Up(REGION_BLKS)
  val TAG_BITS = fullVAddrBits - REGION_OFFSET
  val PTAG_BITS = fullAddressBits - REGION_OFFSET
  val BLOCK_ADDR_BITS = fullVAddrBits - BLOCK_OFFSET

  // hash related
  val HASH_TAG_WIDTH = VADDR_HASH_WIDTH + BLK_ADDR_RAW_WIDTH

  def get_tag(vaddr: UInt) = {
    require(vaddr.getWidth == fullVAddrBits)
    vaddr(vaddr.getWidth - 1, REGION_OFFSET)
  }

  def get_ptag(vaddr: UInt) = {
    require(vaddr.getWidth == fullAddressBits)
    vaddr(vaddr.getWidth - 1, REGION_OFFSET)
  }

  def get_index(addr: UInt) = {
    require(addr.getWidth >= REGION_OFFSET)
    addr(REGION_OFFSET - 1, BLOCK_OFFSET)
  }

  def get_index_oh(vaddr: UInt): UInt = {
    UIntToOH(get_index(vaddr))
  }

  def get_block_addr(addr: UInt): UInt = {
    addr(addr.getWidth - 1, BLOCK_OFFSET)
  }

  def _vaddr_hash(x: UInt): UInt = {
    val width = VADDR_HASH_WIDTH
    val low = x(width - 1, 0)
    val mid = x(2 * width - 1, width)
    val high = x(3 * width - 1, 2 * width)
    low ^ mid ^ high
  }

  def block_hash_tag(vaddr: UInt): UInt = {
    val blk_addr = get_block_addr(vaddr)
    val low = blk_addr(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = blk_addr(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = _vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_hash_tag(vaddr: UInt): UInt = {
    val region_tag = get_tag(vaddr)
    val low = region_tag(BLK_ADDR_RAW_WIDTH - 1, 0)
    val high = region_tag(BLK_ADDR_RAW_WIDTH - 1 + 3 * VADDR_HASH_WIDTH, BLK_ADDR_RAW_WIDTH)
    val high_hash = _vaddr_hash(high)
    Cat(high_hash, low)
  }

  def region_to_block_addr(tag: UInt, index: UInt): UInt = {
    Cat(tag, index)
  }

  def toBinary(n: Int): String = n match {
    case 0 | 1 => s"$n"
    case _ => s"${toBinary(n / 2)}${n % 2}"
  }
}

/* Per-engine prefetch confidence tier admission.
 *
 * The PrefetchController measures a 5-level accuracy tier per prefetch
 * engine (0 <25%, 1 <50%, 2 <75%, 3 <90%, 4 >=90%). The NoC pressure tier is
 * the CHI E.b CBusy[1:0] level of the target bank folded with the SN hint
 * (0 <50%, 1 <75%, 2 <90%, 3 >=90%). A packed per-NoC-tier minimum confidence
 * tier decides admission:
 *   nocTier 0 (<50%): admit every prefetch (minTier 0)
 *   nocTier 1 (<75%): admit accuracy >= 50% (minTier 2)
 *   nocTier 2 (<90%): admit accuracy >= 75% (minTier 3)
 *   nocTier 3 (>=90%): admit accuracy >= 90% (minTier 4)
 * L1 engines (Stream/Stride/Berti/SMS) and NL bypass the gate: their L2-level
 * useless accounting is biased, they are throttled by L1-side accuracy.
 */
object PfConfidence {
  val ENG_NUM = 7
  val TIER_MAX = 4

  def engIdx(pfSource: UInt): UInt = {
    MuxLookup(pfSource, ENG_NUM.U)(Seq(
      MemReqSource.Prefetch2L2Stream.id.U -> 0.U,
      MemReqSource.Prefetch2L2Stride.id.U -> 1.U,
      MemReqSource.Prefetch2L2Berti.id.U  -> 2.U,
      MemReqSource.Prefetch2L2SMS.id.U    -> 3.U,
      MemReqSource.Prefetch2L2BOP.id.U    -> 4.U,
      MemReqSource.Prefetch2L2PBOP.id.U   -> 5.U,
      MemReqSource.Prefetch2L2TP.id.U     -> 6.U
    ))
  }

  def minTierOf(nocTier: UInt, minTierPacked: UInt): UInt =
    (minTierPacked >> (nocTier * 3.U))(2, 0)

  // Admit by the confidence tier carried by the request itself. Ungated
  // engines bypass the matrix; a gated request needs its own tier to reach
  // the minimum tier of the current NoC busy level.
  def admitReq(
    pfSource: UInt,
    pfConf: UInt,
    nocTier: UInt,
    minTierPacked: UInt,
    gateMask: UInt,
    en: Bool
  ): Bool = {
    val idx = engIdx(pfSource)
    val tracked = idx < ENG_NUM.U && gateMask(idx)
    !en || !tracked || pfConf >= minTierOf(nocTier, minTierPacked)
  }

  // Map a BOP score (scoreBits wide) to a 5-level tier by four packed
  // thresholds t1|t2|t3|t4 (5 bits each, t4 at the top).
  def scoreToTier(score: UInt, threshPacked: UInt): UInt = {
    val t1 = threshPacked(4, 0)
    val t2 = threshPacked(9, 5)
    val t3 = threshPacked(14, 10)
    val t4 = threshPacked(19, 15)
    Mux(score >= t4, 4.U, Mux(score >= t3, 3.U,
      Mux(score >= t2, 2.U, Mux(score >= t1, 1.U, 0.U))))
  }

  def engReqSource(e: Int): UInt = e match {
    case 0 => MemReqSource.Prefetch2L2Stream.id.U
    case 1 => MemReqSource.Prefetch2L2Stride.id.U
    case 2 => MemReqSource.Prefetch2L2Berti.id.U
    case 3 => MemReqSource.Prefetch2L2SMS.id.U
    case 4 => MemReqSource.Prefetch2L2BOP.id.U
    case 5 => MemReqSource.Prefetch2L2PBOP.id.U
    case 6 => MemReqSource.Prefetch2L2TP.id.U
  }
}

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  // NOTE: the vaddr is the virtual address of prefetch paddr without offset bits.
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val pfSource = UInt(MemReqSource.reqSourceBits.W)
  // request-level confidence tier: 5 levels, same bands as the accuracy
  // tiers; generated by the issuing engine from its internal prior score
  val pfConf = UInt(3.W)
  val pfConfValid = Bool()
  // Frozen raw engine score used by the multi-history policy calibrator.
  val pfRawScore = UInt(PfPolicy.ScoreWidth.W)
  val pfEvent512Score = UInt(PfPolicy.ScoreWidth.W)
  val pfEvent512Valid = Bool()
  val pfPolicyScores = Vec(PfPolicy.PolicyCount, UInt(PfPolicy.ScoreWidth.W))

  // CDP
  val cdpPfDepth = if (hasCDP) Some(UInt(cdpPfDepthBits.get.W)) else None

  def addr: UInt = Cat(tag, set, 0.U(offsetBits.W))
  def setaddr: UInt = Cat(tag, set)
  def isBOP:Bool = pfSource === MemReqSource.Prefetch2L2BOP.id.U
  def isPBOP:Bool = pfSource === MemReqSource.Prefetch2L2PBOP.id.U
  def isSMS:Bool = pfSource === MemReqSource.Prefetch2L2SMS.id.U
  def isTP:Bool = pfSource === MemReqSource.Prefetch2L2TP.id.U
  def isNL:Bool = pfSource === MemReqSource.Prefetch2L2NL.id.U
  def isCDP:Bool = pfSource === MemReqSource.Prefetch2L2CDP.id.U
  def needAck:Bool = pfSource === MemReqSource.Prefetch2L2BOP.id.U || pfSource === MemReqSource.Prefetch2L2PBOP.id.U
  def fromL2:Bool =
    pfSource === MemReqSource.Prefetch2L2BOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2PBOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
      pfSource === MemReqSource.Prefetch2L2TP.id.U  ||
      pfSource === MemReqSource.Prefetch2L2NL.id.U  ||
      pfSource === MemReqSource.Prefetch2L2CDP.id.U
}

class PfPolicyQueuedReq(implicit p: Parameters) extends PrefetchBundle {
  val req = new PrefetchReq
  val event512Allow = Bool()
  val nocTier = UInt(2.W)
}

class PfPolicyDbIssue(implicit p: Parameters) extends PrefetchBundle {
  val cycle = UInt(64.W)
  val paddr = UInt(fullAddressBits.W)
  val pfSource = UInt(MemReqSource.reqSourceBits.W)
  val rawScore = UInt(PfPolicy.ScoreWidth.W)
  val policyScores = Vec(PfPolicy.PolicyCount, UInt(PfPolicy.ScoreWidth.W))
  val event512Allow = Bool()
  val nocTier = UInt(2.W)
  val eventType = UInt(2.W)
}

class PfPolicyDbOutcome(implicit p: Parameters) extends PrefetchBundle {
  val cycle = UInt(64.W)
  val paddr = UInt(fullAddressBits.W)
  val pfSource = UInt(MemReqSource.reqSourceBits.W)
  val rawScore = UInt(PfPolicy.ScoreWidth.W)
  val useful = Bool()
  val eventType = UInt(2.W)
}

class PfPolicyDbCalibration(implicit p: Parameters) extends PrefetchBundle {
  val cycle = UInt(64.W)
  val pfSource = UInt(MemReqSource.reqSourceBits.W)
  val rawScore = UInt(PfPolicy.ScoreWidth.W)
  val predictedScore = UInt(PfPolicy.ScoreWidth.W)
  val actualScore = UInt(PfPolicy.ScoreWidth.W)
  val windowUseful = UInt(16.W)
  val windowTerminal = UInt(16.W)
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
  val pfSource = UInt(MemReqSource.reqSourceBits.W)

  def addr = Cat(tag, set, 0.U(offsetBits.W))
  def isBOP: Bool = pfSource === MemReqSource.Prefetch2L2BOP.id.U
  def isPBOP: Bool = pfSource === MemReqSource.Prefetch2L2PBOP.id.U
  def isSMS: Bool = pfSource === MemReqSource.Prefetch2L2SMS.id.U
  def isTP: Bool = pfSource === MemReqSource.Prefetch2L2TP.id.U
  def isNL: Bool = pfSource === MemReqSource.Prefetch2L2NL.id.U
  def fromL2: Bool =
    pfSource === MemReqSource.Prefetch2L2BOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2PBOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
      pfSource === MemReqSource.Prefetch2L2TP.id.U  ||
      pfSource === MemReqSource.Prefetch2L2NL.id.U  ||
      pfSource === MemReqSource.Prefetch2L2CDP.id.U
}

class PrefetchTrain(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val vaddr = vaddrBitsOpt.map(_ => UInt(vaddrBitsOpt.get.W))
  val pc = pcBitOpt.map(_ => UInt(pcBitOpt.get.W))
  val hit = Bool()
  val prefetched = Bool()
  val pfsource = UInt(PfSource.pfSourceBits.W)
  val reqsource = UInt(MemReqSource.reqSourceBits.W)

  val evict_tag = Option.when(hasCDP) (UInt(fullTagBits.W))
  val evict_set = Option.when(hasCDP) (UInt(setBits.W))

  // this is for CDP: train CDP when valid address req comes to MainPipe
  val cdp_vpn_train_valid     = Option.when(hasCDP) (Bool())

  val cdp_filter_train_hit    = Option.when(hasCDP) (Bool())
  val cdp_filter_train_evict  = Option.when(hasCDP) (Bool())

  // clarify whether the train is for CDP or other prefetcher
  val is_cdp_train    = Option.when(hasCDP) (Bool())
  val is_other_train  = Option.when(hasCDP) (Bool())
  
  def addr: UInt = Cat(tag, set, 0.U(offsetBits.W))
  def evict_addr: UInt = Cat(evict_tag.getOrElse(0.U(fullTagBits.W)), evict_set.getOrElse(0.U(setBits.W)), 0.U(offsetBits.W))
}

class PrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Flipped(DecoupledIO(new PrefetchTrain))
  val tlb_req = new L2ToL1TlbIO(nRespDups= 1)
  val req = DecoupledIO(new PrefetchReq)
  val resp = Flipped(DecoupledIO(new PrefetchResp))
  val recv_addr = Flipped(ValidIO(new Bundle() {
    val addr = UInt(64.W)
    val pfSource = UInt(MemReqSource.reqSourceBits.W)
  }))
}

class PrefetchTopIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Vec(banks, Flipped(DecoupledIO(new PrefetchTrain)))
  val tlb_req = new L2ToL1TlbIO(nRespDups= 1)
  val req = Vec(banks, DecoupledIO(new PrefetchReq))
  val stash_txreq = DecoupledIO(new CHIREQ)
  val stash_rxrsp = Flipped(DecoupledIO(new CHIRSP))
  val hnCBusy = Input(Vec(1 << bankBits, UInt(3.W)))
  val snCBusy = Input(UInt(3.W))
  val txreqHardStall = Input(Bool())
  val l2PfqBusy = Output(Bool())
  // Per bank per engine: the minimum confidence tier admitted under the
  // current NoC busy tier of that bank; 0 for ungated engines so their
  // requests are never abandoned in MSHR.
  val pfMinTier = Output(Vec(1 << bankBits, Vec(7, UInt(3.W))))
  val resp = Vec(banks, Flipped(DecoupledIO(new PrefetchResp)))
  val recv_addr = Flipped(ValidIO(new Bundle() {
    val addr = UInt(64.W)
    val pfSource = UInt(MemReqSource.reqSourceBits.W)
  }))
  val l3_recv = Input(new PrefetchRecv)
}

class Prefetcher(implicit p: Parameters) extends PrefetchModule {
  override val banks = 1 << bankBits

  val io = IO(new PrefetchTopIO)
  val tpio = IO(new Bundle() {
    val tpmeta_port = if (hasTPPrefetcher) Some(new tpmetaPortIO(hartIdLen, fullAddressBits, offsetBits)) else None
  })
  val cdpio = IO(new Bundle {
    val cdp_trigger = if (hasCDP) Some(Vec(banks, Flipped(ValidIO(new CDPDetectTask(dataBits=blockBits))))) else None
    val pfStat = if (hasCDP) Some(Input(new PrefetchStat)) else None
  })
  val hartId = IO(Input(UInt(hartIdLen.W)))
  val pfCtrlFromCore = IO(Input(new PrefetchCtrlFromCore))
  val l2ToL1PfCtrl = IO(Output(new L2ToL1PfCtrl))
  val pfFeedbackVec = IO(Input(Vec(banks, new PrefetchFeedbackBundle())))

  val prefetchController = Module(new PrefetchController)
  prefetchController.io.pfFeedbackVec := pfFeedbackVec

  // l2 receive need 2 cycles to transmit from core
  val streamDegree = prefetchController.io.l2PfFbCtrl.streamDegree
  val strideDegree = prefetchController.io.l2PfFbCtrl.strideDegree
  val bertiDegree = prefetchController.io.l2PfFbCtrl.bertiDegree
  val smsDegree = prefetchController.io.l2PfFbCtrl.smsDegree
  val vbopDegree = prefetchController.io.l2PfFbCtrl.vbopDegree
  val pbopDegree = prefetchController.io.l2PfFbCtrl.pbopDegree
  val tpDegree = prefetchController.io.l2PfFbCtrl.tpDegree

  // NoC pressure tier per bank: CHI E.b CBusy[1:0] of the target bank HN
  // folded with the global SN hint (00 <50%, 01 >50%, 10 >75%, 11 >90%).
  val confTierVec = prefetchController.io.confTier
  val confHartId = cacheParams.hartId
  // Runtime throttle controls. Bits 0..8 select Stream, Stride, Berti, SMS,
  // NL, VBOP, PBOP, TP, and CDP respectively. Base builds keep the master
  // switch off so the instrumentation is behavior-neutral by default.
  val throttleEn = Constantin.createRecord(s"l2pf_throttleEn$confHartId", initValue = 0)
  val throttleMask = Constantin.createRecord(s"l2pf_throttleMask$confHartId", initValue = 511)
  // Packed 4x3-bit minimum confidence tier per NoC tier: 0, 2, 3, 4.
  val confMinTier = Constantin.createRecord(s"l2pf_confMinTier$confHartId", initValue = 2256)
  // Packed 4x5-bit minimum scores for normal/moderate/high/critical pressure:
  // 0, 12, 20, 28, matching the GEM5 HistoryEvent512 controller.
  val event512MinScore = Constantin.createRecord(s"l2pf_event512MinScore$confHartId", initValue = 737536)
  // Optional source-level SMS accuracy gate. The score uses the L2 SMS outcome
  // trainer and is independent of NoC pressure.
  val smsAccuracyEnable = Constantin.createRecord(s"l2pf_smsAccuracyEnable$confHartId", initValue = 0)
  val smsAccuracyMinScore = Constantin.createRecord(s"l2pf_smsAccuracyMinScore$confHartId", initValue = 8)
  // L2-native sources use Event512 after their source-level history is ready;
  // pfConf remains the cold-start fallback.
  val nocTier = RegInit(VecInit(Seq.fill(banks)(0.U(2.W))))
  for (i <- 0 until banks) {
    val hnTier = io.hnCBusy(i)(1, 0)
    val snTier = io.snCBusy(1, 0)
    nocTier(i) := Mux(hnTier > snTier, hnTier, snTier)
  }
  def throttleActive(pfSource: UInt): Bool =
    throttleEn =/= 0.U && PfPolicy.event512FilterSource(pfSource) &&
      ((throttleMask >> PfPolicy.sourceIndex(pfSource))(0) === 1.U)

  def confAdmit(pfSource: UInt, pfConf: UInt, pfConfValid: Bool, bank: Int): Bool = {
    val fallback = Mux(
      !pfConfValid,
      true.B,
      pfConf >= PfConfidence.minTierOf(nocTier(bank), confMinTier)
    )
    Mux(throttleActive(pfSource), fallback, true.B)
  }
  def event512Ready(pfSource: UInt, event512Valid: Bool): Bool =
    throttleActive(pfSource) && event512Valid

  def smsAccuracyActive(pfSource: UInt): Bool =
    smsAccuracyEnable =/= 0.U && pfSource === MemReqSource.Prefetch2L2SMS.id.U

  def smsAccuracyWouldDrop(pfSource: UInt, event512Score: UInt, event512Valid: Bool): Bool =
    smsAccuracyActive(pfSource) && event512Valid && event512Score < smsAccuracyMinScore
  def policyAdmit(
    pfSource: UInt,
    pfConf: UInt,
    pfConfValid: Bool,
    event512Score: UInt,
    event512Valid: Bool,
    bank: Int
  ): Bool = {
    val fallback = confAdmit(pfSource, pfConf, pfConfValid, bank)
    val calibrated = PfPolicy.event512Admit(
      pfSource, event512Score, event512Valid, nocTier(bank), event512MinScore, true.B
    )
    Mux(event512Ready(pfSource, event512Valid), calibrated, fallback)
  }
  for (i <- 0 until banks) {
    val legacyMask = Cat(
      throttleMask(7),
      throttleMask(6),
      throttleMask(5),
      throttleMask(3, 0)
    )
    io.pfMinTier(i) := VecInit((0 until PfConfidence.ENG_NUM).map { e =>
      Mux(
        throttleEn =/= 0.U && legacyMask(e),
        PfConfidence.minTierOf(nocTier(i), confMinTier),
        0.U
      )
    })
  }
  // max NoC busy tier across banks, fed back to L1 issue gating
  l2ToL1PfCtrl.nocTier := nocTier.reduce((a, b) => Mux(a > b, a, b))
  l2ToL1PfCtrl.streamDegree := streamDegree
  l2ToL1PfCtrl.strideDegree := strideDegree
  l2ToL1PfCtrl.bertiDegree := bertiDegree
  l2ToL1PfCtrl.smsDegree := smsDegree
  for (t <- 0 until 4) {
    XSPerfAccumulate(s"pftq_noctier$t", PopCount(nocTier.map(_ === t.U)))
  }

  val pfRcv_en = RegNextN(pfCtrlFromCore.l2_pf_master_en && pfCtrlFromCore.l2_pf_recv_en, 2, Some(true.B))
  val pbop_en = pfCtrlFromCore.l2_pf_master_en && pfCtrlFromCore.l2_pbop_en
  val vbop_en = pfCtrlFromCore.l2_pf_master_en && pfCtrlFromCore.l2_vbop_en
  val tp_en = pfCtrlFromCore.l2_pf_master_en && pfCtrlFromCore.l2_tp_en
  val cdp_en = false.B
  val delay_latency = pfCtrlFromCore.l2_pf_delay_latency

  /**
   * TLB request arbitration logic.
   *
   * WARNING:
   * 1. `req_kill` is always low in both BOP and CDP, so it is not considered here.
   * 2. The request → response → PMP response pipeline takes three cycles.
   *    Request-response pairing is guaranteed by the prefetchers.
   */
  val bop_tlb_req = WireInit(0.U.asTypeOf(new L2ToL1TlbIO(nRespDups= 1)))
  val cdp_tlb_req = WireInit(0.U.asTypeOf(new L2ToL1TlbIO(nRespDups= 1)))

  val tlbReqArb = Module(new Arbiter(new L2TlbReq, 2))
  tlbReqArb.io.in(0) <> bop_tlb_req.req
  tlbReqArb.io.in(1) <> cdp_tlb_req.req
  tlbReqArb.io.out <> io.tlb_req.req
  io.tlb_req.req_kill := false.B

  bop_tlb_req.resp.valid := io.tlb_req.resp.valid
  bop_tlb_req.resp.bits := io.tlb_req.resp.bits
  bop_tlb_req.pmp_resp := io.tlb_req.pmp_resp
  
  cdp_tlb_req.resp.valid := io.tlb_req.resp.valid
  cdp_tlb_req.resp.bits := io.tlb_req.resp.bits
  cdp_tlb_req.pmp_resp := io.tlb_req.pmp_resp

  io.tlb_req.resp.ready := true.B

  val stashPrefetcher = Module(new StashPrefetcher)
  // Stop L3 stash issue on the busy bank when CBusy[1:0] is >75% or TXREQ stays backpressured.
  val l2HardCnt = RegInit(0.U(3.W))
  when(io.txreqHardStall) {
    l2HardCnt := Mux(l2HardCnt === 7.U, l2HardCnt, l2HardCnt + 1.U)
  }.otherwise {
    l2HardCnt := 0.U
  }
  val l2Hard = l2HardCnt >= 2.U
  stashPrefetcher.io.recv := io.l3_recv
  stashPrefetcher.io.allow := VecInit(nocTier.map(t => t < 2.U && !l2Hard))
  XSPerfAccumulate("pftq_l2hard", l2Hard)
  io.stash_txreq <> stashPrefetcher.io.txreq
  stashPrefetcher.io.rxrsp <> io.stash_rxrsp

  // =================== Prefetchers =====================
  // TODO: consider separate VBOP and PBOP in prefetch param
  val pbop = if (hasBOP) Some(
    Module(new PBestOffsetPrefetch()(p.alterPartial({
      case L2ParamKey =>
        val l2Params = p(L2ParamKey)
        l2Params.copy(prefetch = Seq(BOPParameters(
            virtualTrain = false,
            badScore = 1,
            crossPage = false,
            useDelayOut = false,
            enableStudentCover = true,
            studentPoolSize = 8,
            studentFilterTableBits = 2048,
            studentHashMode = "bop_rr",
            studentCovThreshold = 5,
            issueGateEnable = true,
            issueConfThreshold = 14,
            offsetList = Seq(
              -32, -30, -27, -25, -24, -20, -18, -16, -15,
              -12, -10, -9, -8, -6, -5, -4, -3, -2, -1,
              1, 2, 3, 4, 5, 6, 8, 9, 10,
              12, 15, 16, 18, 20, 24, 25, 27, 30
            )
          )) ++ l2Params.prefetch.filterNot(_.isInstanceOf[BOPParameters])
        )
    })))
  ) else None

  val vbop = if (hasBOP) Some(
    Module(new VBestOffsetPrefetch()(p.alterPartial({
      case L2ParamKey =>
        val l2Params = p(L2ParamKey)
        l2Params.copy(prefetch = Seq(BOPParameters(
            badScore = 2,
            crossPage = true,
            useDelayOut = true,
            enableStudentCover = true,
            studentPoolSize = 4,
            studentFilterTableBits = 4096,
            studentHashMode = "pairs_low9",
            studentCovThreshold = 0,
            issueGateEnable = true,
            issueConfThreshold = 14,
            offsetList = Seq(
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
            )
          )) ++ l2Params.prefetch.filterNot(_.isInstanceOf[BOPParameters])
        )
    })))
  ) else None

  val tp = if (hasTPPrefetcher) Some(Module(new TemporalPrefetch())) else None
  // define Next-Line Prefetcher
  val nl = if (hasNLPrefetcher) Some(Module(new NextLinePrefetch())) else None

  val cdp = if (hasCDP) Some(Module(new CDPPrefetcher())) else None

  // prefetch from upper level
  val pfRcv = if (hasReceiver) Some(Module(new PrefetchReceiver())) else None

  val train = Wire(DecoupledIO(new PrefetchTrain))
  val resp = Wire(DecoupledIO(new PrefetchResp))
  fastArb(io.train, train, Some("prefetch_train"))
  fastArb(io.resp, resp, Some("prefetch_resp"))

  prefetchController.io.isDemandTrain := train.valid && (
    MemReqSource.isCPUReq(train.bits.reqsource) || MemReqSource.isL1Prefetch(train.bits.reqsource)
  )
  // =================== Connection for each Prefetcher =====================
  // Rcv > NL > VBOP > PBOP > TP > CDP
  train.ready := true.B
  if (hasBOP) {
    val vbop_train_buf = Module(new Queue(new PrefetchTrain, entries = 4))
    vbop_train_buf.io.enq.valid := train.valid && train.bits.is_other_train.getOrElse(true.B)
    vbop_train_buf.io.enq.bits  := train.bits
    XSPerfAccumulate("vbop_train_drop", vbop_train_buf.io.enq.valid && !vbop_train_buf.io.enq.ready)
    XSPerfAccumulate("vbop_train_accept", vbop_train_buf.io.enq.fire)

    vbop.get.io.enable := vbop_en
    vbop.get.io.fdbkDegree := vbopDegree
    vbop.get.io.pfCtrlOfDelayLatency := delay_latency
    vbop.get.io.train <> vbop_train_buf.io.deq
    vbop.get.io.resp <> resp
    vbop.get.io.resp.valid := resp.valid && resp.bits.isBOP
    vbop.get.io.tlb_req <> bop_tlb_req

    val pbop_train_buf = Module(new Queue(new PrefetchTrain, entries = 4))
    pbop_train_buf.io.enq.valid := train.valid && train.bits.is_other_train.getOrElse(true.B)
    pbop_train_buf.io.enq.bits  := train.bits
    XSPerfAccumulate("pbop_train_drop", pbop_train_buf.io.enq.valid && !pbop_train_buf.io.enq.ready)
    XSPerfAccumulate("pbop_train_accept", pbop_train_buf.io.enq.fire)

    pbop.get.io.enable := pbop_en
    pbop.get.io.fdbkDegree := pbopDegree
    pbop.get.io.pfCtrlOfDelayLatency := delay_latency
    pbop.get.io.train <> pbop_train_buf.io.deq
    pbop.get.io.resp <> resp
    pbop.get.io.resp.valid := resp.valid && resp.bits.isPBOP
  }
  if (hasReceiver) {
    pfRcv.get.io.enable := pfRcv_en
    pfRcv.get.io.recv_addr := ValidIODelay(io.recv_addr, 2)
    assert(!pfRcv.get.io.req.valid ||
      pfRcv.get.io.req.bits.pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
      pfRcv.get.io.req.bits.pfSource === MemReqSource.Prefetch2L2Stream.id.U ||
      pfRcv.get.io.req.bits.pfSource === MemReqSource.Prefetch2L2Stride.id.U ||
      pfRcv.get.io.req.bits.pfSource === MemReqSource.Prefetch2L2Berti.id.U
    )
  }

  if (hasNLPrefetcher) {
    val nl_train_buf = Module(new Queue(new PrefetchTrain, entries = 4))
    nl_train_buf.io.enq.valid := train.valid && (train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U) && train.bits.is_other_train.getOrElse(true.B)
    nl_train_buf.io.enq.bits  := train.bits
    XSPerfAccumulate("nl_train_drop", nl_train_buf.io.enq.valid && !nl_train_buf.io.enq.ready)
    XSPerfAccumulate("nl_train_accept", nl_train_buf.io.enq.fire)

    nl.get.io.enable := true.B
    nl.get.io.train <> nl_train_buf.io.deq
    nl.get.io.resp <> resp
  }

  if (hasTPPrefetcher) {
    val tp_train_buf = Module(new Queue(new PrefetchTrain, entries = 4))
    tp_train_buf.io.enq.valid := train.valid && (train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U) && train.bits.is_other_train.getOrElse(true.B)
    tp_train_buf.io.enq.bits  := train.bits
    XSPerfAccumulate("tp_train_drop", tp_train_buf.io.enq.valid && !tp_train_buf.io.enq.ready)
    XSPerfAccumulate("tp_train_accept", tp_train_buf.io.enq.fire)

    tp.get.io.enable := tp_en
    tp.get.io.fdbkDegree := tpDegree
    tp.get.io.train <> tp_train_buf.io.deq
    tp.get.io.resp <> resp
    tp.get.io.hartid := hartId

    tp.get.io.tpmeta_port <> tpio.tpmeta_port.get
  }
  if (hasCDP) {
    cdp.get.io.enable := cdp_en

    // Train
    cdp.get.io.vpnTrain.valid  := train.valid && train.bits.cdp_vpn_train_valid.get && train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U
    cdp.get.io.vpnTrain.bits   := train.bits

    cdp.get.io.filterTrain.valid := train.valid && (train.bits.cdp_filter_train_hit.get || train.bits.cdp_filter_train_evict.get) && train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U
    cdp.get.io.filterTrain.bits  := train.bits

    // Trigger
    cdp.get.io.l2DetectTriggers <> cdpio.cdp_trigger.get
    cdp.get.io.pfStat <> cdpio.pfStat.get

    // tlb req
    cdp.get.io.tlbReq <> cdp_tlb_req

  }
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(2, "MbistPipeL2Prefetcher", cacheParams.hasMbist && (hasBOP || hasTPPrefetcher || hasCDP))

  // =================== Connection of all Prefetchers =====================
  /* prefetchers -> pftQueue -> pipe -> Slices.SinkA */
  private val SRC_NUM = 6
  private val Seq(rcv_idx, nl_idx, vbop_idx, pbop_idx, tp_idx, cdp_idx) = (0 until SRC_NUM).toSeq
  val reqs = Seq(
    if (hasReceiver) Some(pfRcv.get.io.req) else None,
    if (hasNLPrefetcher) Some(nl.get.io.req) else None,
    if (hasBOP) Some(vbop.get.io.req) else None,
    if (hasBOP) Some(pbop.get.io.req) else None,
    if (hasTPPrefetcher) Some(tp.get.io.req) else None,
    if (hasCDP) Some(cdp.get.io.pftReq) else None
  )
  val reqsValid = reqs.map(_.map(_.valid).getOrElse(false.B))
  val reqsBits = reqs.map(_.map(_.bits).getOrElse(0.U.asTypeOf(new PrefetchReq)))
  val policySources = Seq(
    MemReqSource.Prefetch2L2Stream.id.U,
    MemReqSource.Prefetch2L2Stride.id.U,
    MemReqSource.Prefetch2L2Berti.id.U,
    MemReqSource.Prefetch2L2SMS.id.U,
    MemReqSource.Prefetch2L2NL.id.U,
    MemReqSource.Prefetch2L2BOP.id.U,
    MemReqSource.Prefetch2L2PBOP.id.U,
    MemReqSource.Prefetch2L2TP.id.U,
    MemReqSource.Prefetch2L2CDP.id.U
  )
  val policySourceNames = Seq("stream", "stride", "berti", "sms", "nl", "vbop", "pbop", "tp", "cdp")
  val policyLabelRawScores = Seq(
    0.U, 0.U, 0.U, 0.U, 0.U,
    reqsBits(vbop_idx).pfRawScore,
    reqsBits(pbop_idx).pfRawScore,
    0.U, 0.U
  )
  require(policySources.size == PfPolicy.SourceCount)
  val policyCalibrator = Module(new PfPolicyCalibrator(policySources.size, banks))
  val policyTimer = GTimer()
  val policyOutcomeDb = ChiselDB.createTable(s"L2PfPolicyOutcome_hart${cacheParams.hartId}", new PfPolicyDbOutcome, basicDB = true)
  val policyCalibrationDb = ChiselDB.createTable(s"L2PfPolicyCalibration_hart${cacheParams.hartId}", new PfPolicyDbCalibration, basicDB = true)
  for (bank <- 0 until banks) {
    val feedback = pfFeedbackVec(bank)
    val useful = feedback.dirResult.valid && feedback.dirResult.bits.meta.prefetch.getOrElse(false.B) &&
      !MemReqSource.isL2Prefetch(feedback.dirResult.bits.replacerInfo.reqSource)
    val useless = feedback.replaceRecord.valid && feedback.replaceRecord.bits.victimPfUseless
    policyCalibrator.io.outcomes(bank).valid := useful || useless
    policyCalibrator.io.outcomes(bank).pfSource := Mux(
      useful,
      PfPolicy.requestSourceOf(feedback.dirResult.bits.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U)),
      PfPolicy.requestSourceOf(feedback.replaceRecord.bits.victimPfSource)
    )
    policyCalibrator.io.outcomes(bank).rawScore := Mux(
      useful,
      feedback.dirResult.bits.meta.prefetchRawScore.getOrElse(0.U),
      feedback.replaceRecord.bits.victimPfRawScore
    )
    policyCalibrator.io.outcomes(bank).useful := useful

    val usefulSource = PfPolicy.requestSourceOf(feedback.dirResult.bits.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U))
    val usefulOutcome = Wire(new PfPolicyDbOutcome)
    usefulOutcome.cycle := policyTimer
    usefulOutcome.paddr := Cat(feedback.dirResult.bits.tag, feedback.dirResult.bits.set, 0.U(offsetBits.W))
    usefulOutcome.pfSource := usefulSource
    usefulOutcome.rawScore := feedback.dirResult.bits.meta.prefetchRawScore.getOrElse(0.U)
    usefulOutcome.useful := true.B
    usefulOutcome.eventType := 1.U
    policyOutcomeDb.log(
      data = usefulOutcome,
      en = useful && PfPolicy.trackedSource(usefulSource),
      site = s"L2PfPolicyOutcome_hart${cacheParams.hartId}",
      clock,
      reset
    )

    val uselessSource = PfPolicy.requestSourceOf(feedback.replaceRecord.bits.victimPfSource)
    val uselessOutcome = Wire(new PfPolicyDbOutcome)
    uselessOutcome.cycle := policyTimer
    uselessOutcome.paddr := feedback.replaceRecord.bits.victimPAddr
    uselessOutcome.pfSource := uselessSource
    uselessOutcome.rawScore := feedback.replaceRecord.bits.victimPfRawScore
    uselessOutcome.useful := false.B
    uselessOutcome.eventType := 2.U
    policyOutcomeDb.log(
      data = uselessOutcome,
      en = useless && PfPolicy.trackedSource(uselessSource),
      site = s"L2PfPolicyOutcome_hart${cacheParams.hartId}",
      clock,
      reset
    )
  }
  for (bank <- 0 until banks) {
    val calibration = Wire(new PfPolicyDbCalibration)
    calibration.cycle := policyTimer
    calibration.pfSource := policyCalibrator.io.calibration(bank).bits.pfSource
    calibration.rawScore := policyCalibrator.io.calibration(bank).bits.rawScore
    calibration.predictedScore := policyCalibrator.io.calibration(bank).bits.predictedScore
    calibration.actualScore := policyCalibrator.io.calibration(bank).bits.actualScore
    calibration.windowUseful := policyCalibrator.io.calibration(bank).bits.windowUseful
    calibration.windowTerminal := policyCalibrator.io.calibration(bank).bits.windowTerminal
    policyCalibrationDb.log(
      data = calibration,
      en = policyCalibrator.io.calibration(bank).valid,
      site = s"L2PfPolicyCalibration_hart${cacheParams.hartId}",
      clock,
      reset
    )
  }
  for (((source, rawScore), port) <- policySources.zip(policyLabelRawScores).zipWithIndex) {
    policyCalibrator.io.labelSource(port) := source
    policyCalibrator.io.labelRawScore(port) := rawScore
  }
  val policyReqsBits = Wire(Vec(SRC_NUM, new PrefetchReq))
  policyReqsBits := VecInit(reqsBits)
  val policyScores = Wire(Vec(SRC_NUM, Vec(PfPolicy.PolicyCount, UInt(PfPolicy.ScoreWidth.W))))
  policyScores := 0.U.asTypeOf(policyScores)
  for (req <- policyReqsBits) {
    req.pfPolicyScores := 0.U.asTypeOf(Vec(PfPolicy.PolicyCount, UInt(PfPolicy.ScoreWidth.W)))
    req.pfEvent512Score := 0.U
    req.pfEvent512Valid := false.B
  }
  val receiverLabel = policyCalibrator.io.label(
    PfPolicy.sourceIndex(policyReqsBits(rcv_idx).pfSource)
  )
  policyScores(rcv_idx) := receiverLabel.score
  policyReqsBits(rcv_idx).pfPolicyScores := receiverLabel.score
  policyReqsBits(rcv_idx).pfEvent512Score := receiverLabel.score(PfPolicy.HistoryEvent512)
  policyReqsBits(rcv_idx).pfEvent512Valid := receiverLabel.event512Valid

  val fixedPolicyReqs = Seq(
    nl_idx -> 4,
    vbop_idx -> 5,
    pbop_idx -> 6,
    tp_idx -> 7,
    cdp_idx -> 8
  )
  for ((reqIdx, port) <- fixedPolicyReqs) {
    policyScores(reqIdx) := policyCalibrator.io.label(port).score
    policyReqsBits(reqIdx).pfPolicyScores := policyCalibrator.io.label(port).score
    policyReqsBits(reqIdx).pfEvent512Score :=
      policyCalibrator.io.label(port).score(PfPolicy.HistoryEvent512)
    policyReqsBits(reqIdx).pfEvent512Valid :=
      policyCalibrator.io.label(port).event512Valid
  }
  val reqsSetAddr = policyReqsBits.map(_.setaddr)
  val pftQueue = Seq.tabulate(banks) { _ =>
    Module(new OverwriteQueue(
      gen = new PfPolicyQueuedReq,
      entries = inflightEntries,
      hasFlow = true
    ))
  }
  val pipe = Seq.tabulate(banks) { _ => Module(new Pipeline(new PfPolicyQueuedReq, 1)) }
  val select = Wire(Vec(banks, Vec(SRC_NUM, Bool())))
  val selectOH = Wire(Vec(banks, Vec(SRC_NUM, Bool())))
  val dropVecAll = Wire(Vec(banks, Vec(SRC_NUM, Bool())))
  io.l2PfqBusy := VecInit(pftQueue.map(_.io.full)).asUInt.orR

  for (i <- 0 until banks) {
    // A full pftQueue still overwrites the oldest queued prefetch.
    val reqsAllowed = Seq(
      true.B,
      true.B,
      vbopDegree.orR,
      pbopDegree.orR,
      tpDegree.orR,
      hasCDP.B
    )
    // Use Event512 once its source-level history is ready. During cold start,
    // fall back to the request confidence tier instead of double-throttling.
    val event512ReadyVec = policyReqsBits.map(b => event512Ready(b.pfSource, b.pfEvent512Valid))
    val event512AdmitVec = policyReqsBits.map(b => PfPolicy.event512Admit(
      b.pfSource, b.pfEvent512Score, b.pfEvent512Valid, nocTier(i), event512MinScore, true.B
    ))
    val confDropVec = policyReqsBits.map { b =>
      throttleActive(b.pfSource) && !event512Ready(b.pfSource, b.pfEvent512Valid) &&
        b.pfConfValid && b.pfConf < PfConfidence.minTierOf(nocTier(i), confMinTier)
    }
    val admitVec = policyReqsBits.map(b => policyAdmit(
      b.pfSource, b.pfConf, b.pfConfValid, b.pfEvent512Score, b.pfEvent512Valid, i
    ))
    select(i) := VecInit(reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(admitVec).map {
      case (((valid, addr), allowed), admit) => valid && allowed && admit && bank_eq(addr, i, bankBits)
    })
    dropVecAll(i) := VecInit(reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(admitVec).map {
      case (((valid, addr), allowed), admit) => valid && allowed && !admit && bank_eq(addr, i, bankBits)
    })
    val event512Drop = reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(event512ReadyVec).zip(event512AdmitVec).map {
      case ((((valid, addr), allowed), event512Ready), event512Admit) =>
        valid && allowed && event512Ready && !event512Admit && bank_eq(addr, i, bankBits)
    }
    val smsAccuracyDrop = reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(policyReqsBits).map {
      case (((valid, addr), allowed), req) =>
        valid && allowed && smsAccuracyActive(req.pfSource) && req.pfEvent512Valid &&
          smsAccuracyWouldDrop(req.pfSource, req.pfEvent512Score, req.pfEvent512Valid) &&
          bank_eq(addr, i, bankBits)
    }
    XSPerfAccumulate("pftq_sms_accuracy_valid_enq", PopCount(reqsValid.zip(policyReqsBits).map {
      case (valid, req) => valid && smsAccuracyActive(req.pfSource) && req.pfEvent512Valid
    }))
    XSPerfAccumulate("pftq_sms_accuracy_drop_enq", PopCount(smsAccuracyDrop))
    XSPerfHistogram("pftq_sms_accuracy_drop_score_enq", ParallelPriorityMux(
      policyReqsBits.map(_.pfSource === MemReqSource.Prefetch2L2SMS.id.U),
      policyReqsBits.map(_.pfEvent512Score)
    ), smsAccuracyDrop.reduce(_ || _), 0, 32, 1)
    for ((source, sourceName) <- policySources.zip(policySourceNames)) {
      val confDropForSource = reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(policyReqsBits).zip(confDropVec).map {
        case ((((valid, addr), allowed), req), drop) =>
          valid && allowed && drop && req.pfSource === source && bank_eq(addr, i, bankBits)
      }
      val eventDropForSource = reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(policyReqsBits).zip(event512Drop).map {
        case ((((valid, addr), allowed), req), drop) =>
          valid && allowed && drop && req.pfSource === source && bank_eq(addr, i, bankBits)
      }
      for (t <- 0 to PfConfidence.TIER_MAX) {
        XSPerfAccumulate(
          s"pftq_conf_drop_enq_${sourceName}_t${t}_bank${i}",
          PopCount(confDropForSource.zip(policyReqsBits).map { case (drop, req) => drop && req.pfConf === t.U })
        )
      }
      XSPerfAccumulate(s"pftq_event512_drop_enq_${sourceName}_bank$i", PopCount(eventDropForSource))
      val sourceEventScore = ParallelPriorityMux(
        policyReqsBits.map(_.pfSource === source),
        policyReqsBits.map(_.pfEvent512Score)
      )
      XSPerfHistogram(
        s"pftq_event512_drop_score_enq_${sourceName}_bank$i",
        sourceEventScore,
        eventDropForSource.reduce(_ || _),
        0,
        32,
        1
      )
    }
    selectOH(i) := VecInit(PriorityEncoderOH(select(i).asUInt).asBools)
    pftQueue(i).io.enq.valid := select(i).asUInt.orR
    pftQueue(i).io.enq.bits.req := ParallelPriorityMux(select(i).asUInt, policyReqsBits)
    pftQueue(i).io.enq.bits.event512Allow := ParallelPriorityMux(select(i).asUInt, admitVec)
    pftQueue(i).io.enq.bits.nocTier := nocTier(i)
    // Re-check the tier at dequeue and discard instead of holding: a queued
    // prefetch whose tier fell below the busy bank threshold is dropped fast
    // so it cannot keep occupying NoC/L2 resources. The head entry is staged
    // in a register so the admit decision never feeds back into deq.ready
    // combinationally (OverwriteQueue hasFlow makes deq.bits depend on
    // deq.ready, so gating deq.ready with deq.bits would form a loop).
    val headValid = RegInit(false.B)
    val headBits = Reg(chiselTypeOf(pftQueue(i).io.deq.bits))
    val headConfAdmit = confAdmit(headBits.req.pfSource, headBits.req.pfConf, headBits.req.pfConfValid, i)
    val headEvent512Ready = event512Ready(headBits.req.pfSource, headBits.req.pfEvent512Valid)
    val headEvent512Admit = PfPolicy.event512Admit(
      headBits.req.pfSource,
      headBits.req.pfEvent512Score,
      headBits.req.pfEvent512Valid,
      nocTier(i),
      event512MinScore,
      true.B
    )
    val headAdmit = policyAdmit(
      headBits.req.pfSource,
      headBits.req.pfConf,
      headBits.req.pfConfValid,
      headBits.req.pfEvent512Score,
      headBits.req.pfEvent512Valid,
      i
    )
    val headConfDrop = headValid && throttleActive(headBits.req.pfSource) &&
      !headEvent512Ready && headBits.req.pfConfValid &&
      headBits.req.pfConf < PfConfidence.minTierOf(nocTier(i), confMinTier)
    val headEventDrop = headValid && headEvent512Ready && !headEvent512Admit
    val headSmsAccuracyDrop = headValid && smsAccuracyActive(headBits.req.pfSource) &&
      headBits.req.pfEvent512Valid &&
      smsAccuracyWouldDrop(headBits.req.pfSource, headBits.req.pfEvent512Score, headBits.req.pfEvent512Valid)
    XSPerfAccumulate("pftq_sms_accuracy_valid_deq", headValid && smsAccuracyActive(headBits.req.pfSource) && headBits.req.pfEvent512Valid)
    XSPerfAccumulate("pftq_sms_accuracy_drop_deq", headSmsAccuracyDrop)
    XSPerfHistogram("pftq_sms_accuracy_drop_score_deq", headBits.req.pfEvent512Score, headSmsAccuracyDrop, 0, 32, 1)
    for ((source, sourceName) <- policySources.zip(policySourceNames)) {
      for (t <- 0 to PfConfidence.TIER_MAX) {
        XSPerfAccumulate(
          s"pftq_conf_drop_deq_${sourceName}_t${t}_bank${i}",
          headConfDrop && headBits.req.pfSource === source && headBits.req.pfConf === t.U
        )
      }
      XSPerfAccumulate(
        s"pftq_event512_drop_deq_${sourceName}_bank$i",
        headEventDrop && headBits.req.pfSource === source
      )
      XSPerfHistogram(
        s"pftq_event512_drop_score_deq_${sourceName}_bank$i",
        headBits.req.pfEvent512Score,
        headEventDrop && headBits.req.pfSource === source,
        0,
        32,
        1
      )
    }
    val headExplore = !headAdmit && LFSR64()(2, 0) === 0.U
    val headIssue = headAdmit || headExplore
    val headDone = headValid && (!headIssue || pipe(i).io.in.ready)
    pftQueue(i).io.deq.ready := !headValid || headDone
    pipe(i).io.in.valid := headValid && headIssue
    pipe(i).io.in.bits := headBits
    pipe(i).io.in.bits.event512Allow := headAdmit
    pipe(i).io.in.bits.nocTier := nocTier(i)
    when(pftQueue(i).io.deq.fire) {
      headValid := true.B
      headBits := pftQueue(i).io.deq.bits
    }.elsewhen(headDone) {
      headValid := false.B
    }
    io.req(i).valid := pipe(i).io.out.valid
    io.req(i).bits := pipe(i).io.out.bits.req
    pipe(i).io.out.ready := io.req(i).ready
    XSPerfAccumulate(s"pftq_enq_drop_bank$i", PopCount(dropVecAll(i)))
    XSPerfAccumulate(s"pftq_event512_enq_drop_bank$i", PopCount(event512Drop))
    XSPerfAccumulate(s"pftq_deq_drop_bank$i", headValid && !headIssue)
    XSPerfAccumulate(s"pftq_event512_deq_drop_bank$i", headValid && headEvent512Ready && !headEvent512Admit)
    XSPerfAccumulate(s"pftq_deq_explore_bank$i", headValid && headExplore && pipe(i).io.in.ready)
  }
  val pftqOverwrite = VecInit((0 until banks).map { i =>
    pftQueue(i).io.full && pftQueue(i).io.enq.fire && !pftQueue(i).io.deq.fire
  })
  val pftqIssueFire = VecInit((0 until banks).map { i =>
    pipe(i).io.in.fire
  })
  for (t <- 0 to PfConfidence.TIER_MAX) {
    XSPerfAccumulate(s"pftq_issue_vbop_t$t", PopCount((0 until banks).map(i =>
      pftQueue(i).io.enq.fire && pftQueue(i).io.enq.bits.req.isBOP &&
      pftQueue(i).io.enq.bits.req.pfConf === t.U)))
    XSPerfAccumulate(s"pftq_issue_pbop_t$t", PopCount((0 until banks).map(i =>
      pftQueue(i).io.enq.fire && pftQueue(i).io.enq.bits.req.isPBOP &&
      pftQueue(i).io.enq.bits.req.pfConf === t.U)))
  }
  XSPerfAccumulate("pftq_full", PopCount(VecInit(pftQueue.map(_.io.full))))
  XSPerfAccumulate("pftq_enq_fire", PopCount(VecInit(pftQueue.map(_.io.enq.fire))))
  XSPerfAccumulate("pftq_overwrite", PopCount(pftqOverwrite))
  XSPerfAccumulate("pftq_issue_fire", PopCount(pftqIssueFire))

  for ((sourceName, sourceId) <- Seq(
         "vbop" -> MemReqSource.Prefetch2L2BOP.id.U,
         "pbop" -> MemReqSource.Prefetch2L2PBOP.id.U
       ); policy <- 0 until PfPolicy.PolicyCount) {
    val issued = (0 until banks).map(i => io.req(i).fire && io.req(i).bits.pfSource === sourceId)
    val issuedScoreSum = issued.zipWithIndex.map { case (valid, i) =>
      Mux(valid, io.req(i).bits.pfPolicyScores(policy), 0.U)
    }.reduce(_ +& _)
    XSPerfAccumulate(s"pf_policy_${sourceName}_p${policy}_issued", PopCount(issued))
    XSPerfAccumulate(s"pf_policy_${sourceName}_p${policy}_issued_score_sum", issuedScoreSum)
  }

  val policyIssueDb = ChiselDB.createTable(s"L2PfPolicyIssue_hart${cacheParams.hartId}", new PfPolicyDbIssue, basicDB = true)
  for (i <- 0 until banks) {
    val issue = Wire(new PfPolicyDbIssue)
    issue.cycle := policyTimer
    issue.paddr := io.req(i).bits.addr
    issue.pfSource := io.req(i).bits.pfSource
    issue.rawScore := io.req(i).bits.pfRawScore
    issue.policyScores := io.req(i).bits.pfPolicyScores
    issue.event512Allow := pipe(i).io.out.bits.event512Allow
    issue.nocTier := pipe(i).io.out.bits.nocTier
    issue.eventType := 0.U
    policyIssueDb.log(
      data = issue,
      en = io.req(i).fire && PfPolicy.trackedSource(issue.pfSource),
      site = s"L2PfPolicyIssue_hart${cacheParams.hartId}",
      clock,
      reset
    )
  }

  for ((reqOpt, j) <- reqs.zipWithIndex) {
    reqOpt.foreach { req =>
      val enqAck = (0 until banks).map(i => selectOH(i)(j)).reduce(_ || _)
      val dropAck = (0 until banks).map(i => dropVecAll(i)(j)).reduce(_ || _)
      req.ready := enqAck || dropAck
    }
  }

  val reqsFire = reqs.map(_.map(_.fire).getOrElse(false.B))

  // Per-engine lifecycle at the prefetch arbitration boundary. `dropProxy`
  // means the engine presented a request but lost this cycle's selection; the
  // overwrite queue has no source-tagged drop output in this revision.
  val lifecycleNames = Seq("L1", "NextLine", "VBOP", "PBOP", "TP", "CDP")
  for ((name, i) <- lifecycleNames.zipWithIndex) {
    XSPerfAccumulate(s"l2prefetchReq$name", reqsValid(i))
    XSPerfAccumulate(s"l2prefetchPipeReq$name", reqsFire(i))
    XSPerfAccumulate(s"l2prefetchDropProxy$name", reqsValid(i) && !reqsFire(i))
  }

  XSPerfAccumulate("prefetch_train_valid", train.valid)
  XSPerfAccumulate("prefetch_train_in_valid", PopCount(io.train.map(_.valid)))
  XSPerfAccumulate("prefetch_resp_valid", resp.valid)
  XSPerfAccumulate("prefetch_resp_in_valid", PopCount(io.resp.map(_.valid)))
  XSPerfAccumulate("prefetch_req_fromL1", reqsValid(rcv_idx))
  XSPerfAccumulate("prefetch_req_fromVBOP", reqsValid(vbop_idx))
  XSPerfAccumulate("prefetch_req_fromPBOP", reqsValid(pbop_idx))
  XSPerfAccumulate("prefetch_req_fromBOP", reqsValid(vbop_idx) || reqsValid(pbop_idx))
  XSPerfAccumulate("prefetch_req_fromTP", reqsValid(tp_idx))
  XSPerfAccumulate("prefetch_req_fromNL", reqsValid(nl_idx))
  XSPerfAccumulate("prefetch_req_fromCDP", reqsValid(cdp_idx))

  XSPerfAccumulate("prefetch_req_selectL1", reqsFire(rcv_idx))
  XSPerfAccumulate("prefetch_req_selectVBOP", reqsFire(vbop_idx))
  XSPerfAccumulate("prefetch_req_selectPBOP", reqsFire(pbop_idx))
  XSPerfAccumulate("prefetch_req_selectBOP", reqsFire(vbop_idx) || reqsFire(pbop_idx))
  XSPerfAccumulate("prefetch_req_selectTP", reqsFire(tp_idx))
  XSPerfAccumulate("prefetch_req_selectNL", reqsFire(nl_idx))
  XSPerfAccumulate("prefetch_req_selectCDP", reqsFire(cdp_idx))
  XSPerfAccumulate("prefetch_req_SMS_other_overlapped",
    reqsValid(rcv_idx) &&
      (reqsValid(vbop_idx) || reqsValid(pbop_idx) || reqsValid(tp_idx) || reqsValid(nl_idx) || reqsValid(cdp_idx))
  )

  // NOTE: set basicDB false when debug over
  // TODO: change the enable signal to not target the BOP
  class TrainEntry extends Bundle{
    val paddr = UInt(fullAddressBits.W)
    val vaddr = UInt(fullVAddrBits.W)
    val needT = Bool()
    val hit = Bool()
    val prefetched = Bool()
    val source = UInt(sourceIdBits.W)
    val pfsource = UInt(PfSource.pfSourceBits.W)
    val reqsource = UInt(MemReqSource.reqSourceBits.W)
  }
  val trainTT = ChiselDB.createTable("L2PrefetchTrainTable", new TrainEntry, basicDB = false)
  val e1 = Wire(new TrainEntry)
  e1.paddr := train.bits.addr
  e1.vaddr := train.bits.vaddr.getOrElse(0.U) << offsetBits
  e1.needT := train.bits.needT
  e1.hit := train.bits.hit
  e1.prefetched := train.bits.prefetched
  e1.source := train.bits.source
  e1.pfsource := train.bits.pfsource
  e1.reqsource := train.bits.reqsource
  trainTT.log(
    data = e1,
    en = train.valid,
    site = "L2Train",
    clock, reset
  )

  class PrefetchEntry extends Bundle{
    val paddr = UInt(fullAddressBits.W)
    val needT = Bool()
    val pfsource = UInt(MemReqSource.reqSourceBits.W)
  }
  val pfTT = ChiselDB.createTable("L2PrefetchReqTable", new PrefetchEntry, basicDB = false)
  for (i <- 0 until banks) {
    val e2 = Wire(new PrefetchEntry)
    e2.paddr := io.req(i).bits.addr
    e2.needT := io.req(i).bits.needT
    e2.pfsource := io.req(i).bits.pfSource
    pfTT.log(
      data = e2,
      en = io.req(i).fire,
      site = "L2PrefetchReq",
      clock, reset
    )
  }
}
