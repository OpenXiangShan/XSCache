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
  val confThrottleEn = Constantin.createRecord(s"l2pf_confThrottle$confHartId", initValue = 1)
  // Packed 4x3-bit minimum confidence tier per NoC tier: 0, 2, 3, 4.
  val confMinTier = Constantin.createRecord(s"l2pf_confMinTier$confHartId", initValue = 2256)
  // Gate only L2 native engines (VBOP/PBOP/TP); L1 engines bypass.
  val confGateMask = Constantin.createRecord(s"l2pf_confGateMask$confHartId", initValue = 112)
  val nocTier = RegInit(VecInit(Seq.fill(banks)(0.U(2.W))))
  for (i <- 0 until banks) {
    val hnTier = io.hnCBusy(i)(1, 0)
    val snTier = io.snCBusy(1, 0)
    nocTier(i) := Mux(hnTier > snTier, hnTier, snTier)
  }
  def confAdmit(pfSource: UInt, pfConf: UInt, bank: Int): Bool = PfConfidence.admitReq(
    pfSource, pfConf, nocTier(bank), confMinTier, confGateMask, confThrottleEn =/= 0.U
  )
  for (i <- 0 until banks) {
    io.pfMinTier(i) := VecInit((0 until PfConfidence.ENG_NUM).map { e =>
      Mux(
        confThrottleEn =/= 0.U && confGateMask(e),
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
  val reqsSetAddr = reqsBits.map(_.setaddr)
  val pftQueue = Seq.tabulate(banks) { _ =>
    Module(new OverwriteQueue(
      gen = new PrefetchReq,
      entries = inflightEntries,
      hasFlow = true
    ))
  }
  val pipe = Seq.tabulate(banks) { _ => Module(new Pipeline(new PrefetchReq, 1)) }
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
      false.B
    )
    // Admit by confidence tier at enqueue. A blocked prefetch is acknowledged
    // and discarded so the source engine never holds it head-of-line; the
    // drop is counted as not served, not as useless.
    val admitVec = reqsBits.map(b => confAdmit(b.pfSource, b.pfConf, i))
    select(i) := VecInit(reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(admitVec).map {
      case (((valid, addr), allowed), admit) => valid && allowed && admit && bank_eq(addr, i, bankBits)
    })
    dropVecAll(i) := VecInit(reqsValid.zip(reqsSetAddr).zip(reqsAllowed).zip(admitVec).map {
      case (((valid, addr), allowed), admit) => valid && allowed && !admit && bank_eq(addr, i, bankBits)
    })
    selectOH(i) := VecInit(PriorityEncoderOH(select(i).asUInt).asBools)
    pftQueue(i).io.enq.valid := select(i).asUInt.orR
    pftQueue(i).io.enq.bits := ParallelPriorityMux(select(i).asUInt, reqsBits)
    // Re-check the tier at dequeue and discard instead of holding: a queued
    // prefetch whose tier fell below the busy bank threshold is dropped fast
    // so it cannot keep occupying NoC/L2 resources. The head entry is staged
    // in a register so the admit decision never feeds back into deq.ready
    // combinationally (OverwriteQueue hasFlow makes deq.bits depend on
    // deq.ready, so gating deq.ready with deq.bits would form a loop).
    val headValid = RegInit(false.B)
    val headBits = Reg(chiselTypeOf(pftQueue(i).io.deq.bits))
    val headAdmit = confAdmit(headBits.pfSource, headBits.pfConf, i)
    val headExplore = !headAdmit && LFSR64()(2, 0) === 0.U
    val headIssue = headAdmit || headExplore
    val headDone = headValid && (!headIssue || pipe(i).io.in.ready)
    pftQueue(i).io.deq.ready := !headValid || headDone
    pipe(i).io.in.valid := headValid && headIssue
    pipe(i).io.in.bits := headBits
    when(pftQueue(i).io.deq.fire) {
      headValid := true.B
      headBits := pftQueue(i).io.deq.bits
    }.elsewhen(headDone) {
      headValid := false.B
    }
    io.req(i) <> pipe(i).io.out
    XSPerfAccumulate(s"pftq_enq_drop_bank$i", PopCount(dropVecAll(i)))
    XSPerfAccumulate(s"pftq_deq_drop_bank$i", headValid && !headIssue)
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
      pftQueue(i).io.enq.fire && pftQueue(i).io.enq.bits.isBOP &&
      pftQueue(i).io.enq.bits.pfConf === t.U)))
    XSPerfAccumulate(s"pftq_issue_pbop_t$t", PopCount((0 until banks).map(i =>
      pftQueue(i).io.enq.fire && pftQueue(i).io.enq.bits.isPBOP &&
      pftQueue(i).io.enq.bits.pfConf === t.U)))
  }
  XSPerfAccumulate("pftq_full", PopCount(VecInit(pftQueue.map(_.io.full))))
  XSPerfAccumulate("pftq_enq_fire", PopCount(VecInit(pftQueue.map(_.io.enq.fire))))
  XSPerfAccumulate("pftq_overwrite", PopCount(pftqOverwrite))
  XSPerfAccumulate("pftq_issue_fire", PopCount(pftqIssueFire))

  for ((reqOpt, j) <- reqs.zipWithIndex) {
    reqOpt.foreach { req =>
      val enqAck = (0 until banks).map(i => selectOH(i)(j)).reduce(_ || _)
      val dropAck = (0 until banks).map(i => dropVecAll(i)(j)).reduce(_ || _)
      req.ready := enqAck || dropAck
    }
  }

  val reqsFire = reqs.map(_.map(_.fire).getOrElse(false.B))

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
