package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xscache.coupledL2.{HasCoupledL2Parameters, L2ToL1TlbIO}
import utility._
import utility.sram.SRAMTemplate
import xscache.coupledL2._

case class CDPParameters(
  useFilteredDetect:  Boolean = false,

  hotThreshold:   Int = 2,
  depthThreshold: Int = 1,

  detectPipeNum: Int = 4,

  reqFilterEntryNum: Int = 16,   // how many entries in the prefetch req filter?

  // Replacement
  replacer: Option[String] = Some("setplru"),

  // VPN Table Params
  vpnTableSetNum:       Int = 4,
  vpnTableWayNum:       Int = 4,
  vpnTableSubEntryNum:  Int = 4,
  vpnTableTagBits:      Int = 10,     // should be a val within (0, 18 - log2(vpnTableSubEntryNum)]
  useVpnTableHashIndex: Boolean = true,
  counterBits:          Int = 10,
  vpnResetPeriod:       Int = 128,    // Every $vpnResetPeriod visits, VPN entries will be reset
  entryBits:            Int = 21,     // Every SubEntry maintain for 2^$(entryBits) Bits

  // FilterTable Params
  filterTableSetNum:  Int = 64,
  filterTableWayNum:  Int = 4,
  filterEntryBlks:  Int = 64,         // 64 slots per entry
  filterEntryGranularity: Int = 4096, // 4KB per slot

  degree:   Int = 3,      // issue how many prefetch req?
  useDynamicDegree: Boolean = false,

  debug: Boolean = false,

  useFilterTable: Boolean = true

) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16  // ???
}

trait HasCDPParams extends HasPrefetcherHelper with HasCoupledL2Parameters {
  def cdpParams = prefetchers.find {
    case p: CDPParameters => true
    case _ => false
  }.get.asInstanceOf[CDPParameters]

  val banks = 1 << bankBits

  val debug = cdpParams.debug

  val useFilteredDetect = cdpParams.useFilteredDetect
  val useFilterTable    = cdpParams.useFilterTable

  val hotThreshold   = cdpParams.hotThreshold
  val depthThreshold = cdpParams.depthThreshold
  require(depthThreshold <= pfDepthMax, s"depthThreshold(${depthThreshold}) should be less than pfDepthMax(${pfDepthMax})")

  // helper function
  def get_folded_hash(originVal: UInt, resultBitWidth: Int): UInt = {    // fold $originVal length value into $resultBitWidth
    val totalBits = originVal.getWidth

    // Handle zero-width originVal to avoid reduce on an empty sequence
    if (totalBits == 0) {
      0.U(resultBitWidth.W)
    } else {
      val groupNum  = if (totalBits % resultBitWidth == 0) totalBits / resultBitWidth else totalBits / resultBitWidth + 1

      val paddedBits = groupNum * resultBitWidth
      val padWidth = paddedBits - totalBits
      val paddedVal =
        if (padWidth == 0) originVal
        else Cat(0.U(padWidth.W), originVal)

      val groups = Seq.tabulate(groupNum) { i =>
        val startBit = i * resultBitWidth
        val endBit = (i + 1) * resultBitWidth - 1
        paddedVal(endBit, startBit)
      }

      groups.reduce(_ ^ _)
    }
  }

  val detectPipeNum = cdpParams.detectPipeNum

  val degree            = cdpParams.degree
  val useDynamicDegree  = cdpParams.useDynamicDegree

  val replType  = cdpParams.replacer

  // VpnTable Params
  val counterBits   = cdpParams.counterBits

  def get_vpn2(addr: UInt): UInt = addr(38, 30)
  def get_vpn1(addr: UInt): UInt = addr(29, 21)
  def get_vpn0(addr: UInt): UInt = addr(20, 12)
  def get_offset(addr: UInt): UInt = addr(11, 0)

  val vpnTableSetNum      = cdpParams.vpnTableSetNum
  val vpnTableWayNum      = cdpParams.vpnTableWayNum
  val vpnTableSubEntryNum = cdpParams.vpnTableSubEntryNum

  val vpnResetPeriod      = cdpParams.vpnResetPeriod

  val entryBits     = cdpParams.entryBits
  val mainEntryBits = log2Ceil(vpnTableSetNum)
  val subEntryBits  = log2Ceil(vpnTableSubEntryNum)
  val vpnTabTagBits = cdpParams.vpnTableTagBits
  val useVpnTableHashIndex = cdpParams.useVpnTableHashIndex
  val vpnWayBits    = log2Ceil(vpnTableWayNum)

  // vaddr => [ Tag | MainEntryIdx | SubEntryIdx | entryBits(1M Space) ]
  // vpn_addr => [ Tag | MainEntryIdx | SubEntryIdx ]
  //
  // Hash the narrow index fields with upper VPN bits to avoid skewed set/sub-entry access.
  // Keep the main-entry hash independent of sub_idx so the four sub entries belonging to
  // the same main entry still share one tag/way and can use alloc_sub normally.

  def get_vpn_addr(addr: UInt) = addr(addr.getWidth - 1, entryBits)  // TODO: parameterize

  def get_vpntab_origin_tag(addr: UInt) = {
    val vpnAddr = get_vpn_addr(addr)
    vpnAddr(vpnAddr.getWidth - 1, subEntryBits + mainEntryBits) // TODO: parameterize
  }

  def get_vpntab_origin_main_idx(addr: UInt) = {
    val vpnAddr = get_vpn_addr(addr)
    vpnAddr(subEntryBits + mainEntryBits - 1, subEntryBits)
  }

  def get_vpntab_origin_sub_idx(addr: UInt) = {
    val vpnAddr = get_vpn_addr(addr)
    vpnAddr(subEntryBits - 1, 0)
  }

  def get_main_idx(addr: UInt) = {
    val originTag     = get_vpntab_origin_tag(addr)
    val originMainIdx = get_vpntab_origin_main_idx(addr)

    if (useVpnTableHashIndex) {
      originMainIdx ^ get_folded_hash(originTag, mainEntryBits)
    } else {
      originMainIdx
    }
  }

  def get_sub_idx(addr: UInt) = {
    val originTag     = get_vpntab_origin_tag(addr)
    val originMainIdx = get_vpntab_origin_main_idx(addr)
    val originSubIdx  = get_vpntab_origin_sub_idx(addr)

    if (useVpnTableHashIndex) {
      originSubIdx ^ get_folded_hash(Cat(originTag, originMainIdx), subEntryBits)
    } else {
      originSubIdx
    }
  }

  def get_vpntab_tag(addr: UInt) = {
    get_folded_hash(get_vpntab_origin_tag(addr), vpnTabTagBits)
  }

  // Filter Table Params
  val filterTableSetNum = cdpParams.filterTableSetNum
  val filterTableWayNum = cdpParams.filterTableWayNum
  val filterEntryBlks = cdpParams.filterEntryBlks
  val filterEntryGranularity = cdpParams.filterEntryGranularity

  val filterTableOffsetBits = log2Ceil(filterEntryBlks)
  val filterTableSetBits    = log2Ceil(filterTableSetNum)
  val filterTableTagBits    = fullAddressBits - log2Ceil(filterEntryGranularity) - filterTableSetBits - filterTableOffsetBits

  def get_filter_addr(addr: UInt) = {
    addr(addr.getWidth - 1, log2Ceil(filterEntryGranularity))
  }

  def get_filter_offset(addr: UInt) = {
    val filterAddr = get_filter_addr(addr)
    filterAddr(filterTableOffsetBits - 1, 0)
  }

  def get_filter_set(addr: UInt) = {
    val filterAddr = get_filter_addr(addr)
    filterAddr(filterTableSetBits + filterTableOffsetBits - 1, filterTableOffsetBits)
  }

  def get_filter_tag(addr: UInt) = {
    val filterAddr = get_filter_addr(addr)
    filterAddr(filterAddr.getWidth - 1, filterTableSetBits + filterTableOffsetBits)
  }

  // SentUnit Params
  val reqFilterEntryNum = cdpParams.reqFilterEntryNum
  val reqFilterVTagBits = fullVAddrBits - log2Ceil(blockBytes)
  val reqFilterPTagBits = fullAddressBits - log2Ceil(blockBytes)
}

abstract class CDPBundle(implicit val p: Parameters) extends Bundle with HasCDPParams
abstract class CDPModule(implicit val p: Parameters) extends Module with HasCDPParams

class CDPDetectTrigger(implicit p: Parameters) extends CDPBundle {
  /**
    * Raw detectTrigger from Hit/Refill
    * Data width == blockBytes
    */
  val cacheblock  = UInt(blockBits.W)
  val pfDepth     = UInt(pfDepthBits.W)
  val pfSource    = UInt(PfSource.pfSourceBits.W)
  val is_hit      = Bool()    // is this trigger from req hitting l2?
}

class CDPDetectEntry(implicit p: Parameters) extends CDPBundle {
  /**
    * Entry that records the split trigger
    * Saved in DetectTrigBuffer
    * Data width == half blockBytes
    */
  val half_cacheblock = UInt((blockBits / 2).W)
  val pfDepth         = UInt(pfDepthBits.W)
  val pfSource        = UInt(PfSource.pfSourceBits.W)
  val is_hit  = Bool()        // is this trigger from req hitting l2?
}

class VpnTableMetaInfo(implicit p: Parameters) extends CDPBundle {
  val valid = Bool()
  val hot   = Bool()    // indicate whether this 1MB page is frequently visited in the past period

  val prevRefCnt  = UInt(counterBits.W)
  val refCnt      = UInt(counterBits.W)
}

class vtQueryReq(implicit p: Parameters) extends CDPBundle {
  val main_idx  = UInt(mainEntryBits.W)
  val sub_idx   = UInt(subEntryBits.W)
}

class vtQueryRsp(implicit p: Parameters) extends CDPBundle {
  val tag_vec   = Vec(vpnTableWayNum, UInt(vpnTabTagBits.W))
  val meta_vec  = Vec(vpnTableWayNum, new VpnTableMetaInfo)
  val valid_vec = Vec(vpnTableWayNum, Bool())
}

class vtTrainReq(implicit p: Parameters) extends CDPBundle {
  val alloc_main  = Bool()    // allocate a new MainEntry (clear all the SubEntries)
  val alloc_sub   = Bool()    // allocate a new SubEntry (refCnt = 1)
  val target_way  = UInt(log2Ceil(vpnTableWayNum).W)

  val main_idx    = UInt(mainEntryBits.W)
  val sub_idx     = UInt(subEntryBits.W)

  val tag         = UInt(vpnTabTagBits.W)

  val is_hit_cdp  = Bool()  // train trigger from hitting a CDP prefetched block?
}

class VpnTable(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val query_req = Flipped(Vec(detectPipeNum + 1, ValidIO(new vtQueryReq)))   // +1 for train pipe
    val query_rsp = Vec(detectPipeNum + 1, ValidIO(new vtQueryRsp))
    val train_req = Flipped(ValidIO(new vtTrainReq))
  })

  val (query_req, query_rsp) = (io.query_req, io.query_rsp)
  val train_req = io.train_req

  val refreshCnt = RegInit(0.U(32.W))
  val is_refresh = Wire(Bool())

  // TODO: use SRAM ?
  // Tag Array
  val tag_array = RegInit(VecInit(Seq.fill(vpnTableSetNum)(
    VecInit(Seq.fill(vpnTableWayNum)(0.U(vpnTabTagBits.W)))
  )))

  // Valid Array
  val valid_array = RegInit(VecInit(Seq.fill(vpnTableSetNum)(
    VecInit(Seq.fill(vpnTableWayNum)(false.B))
  )))

  // Meta Info Array
  val meta_array = RegInit(VecInit(Seq.fill(vpnTableSetNum)(
    VecInit(Seq.fill(vpnTableWayNum)(
      VecInit(Seq.fill(vpnTableSubEntryNum)(0.U.asTypeOf(new VpnTableMetaInfo)))
    ))
  )))

  // Query Logic
  for (i <- 0 until detectPipeNum + 1) {
    val (req, rsp) = (query_req(i), query_rsp(i))

    rsp.valid := !reset.asBool

    val (main_idx, sub_idx) = (req.bits.main_idx, req.bits.sub_idx)

    for (j <- 0 until vpnTableWayNum) {
      rsp.bits.tag_vec(j)   := tag_array(main_idx)(j)
      rsp.bits.meta_vec(j)  := meta_array(main_idx)(j)(sub_idx)
      rsp.bits.valid_vec(j) := valid_array(main_idx)(j)
    }
  }

  // Train Logic
  when (train_req.valid && !is_refresh) {
    val (main_idx, sub_idx) = (train_req.bits.main_idx, train_req.bits.sub_idx)
    val target_way = train_req.bits.target_way
    val (alloc_main, alloc_sub) = (train_req.bits.alloc_main, train_req.bits.alloc_sub)
    val no_alloc = !alloc_main && !alloc_sub

    assert(!(alloc_main && alloc_sub), "TrainReq can't allocate both main entry and sub entry!")

    val incr_num  = Mux(train_req.bits.is_hit_cdp, 4.U, 1.U)

    when (alloc_main) {
      // use target_way for replacement
      val replace_way = target_way

      // Update Tag
      tag_array(main_idx)(replace_way)  := train_req.bits.tag

      // Update Valid
      valid_array(main_idx)(replace_way) := true.B

      // Update Meta
      for (i <- 0 until vpnTableSubEntryNum) {
        when (i.U === sub_idx) {
          meta_array(main_idx)(replace_way)(i)  := 0.U.asTypeOf(new VpnTableMetaInfo)
          meta_array(main_idx)(replace_way)(i).valid  := true.B
          meta_array(main_idx)(replace_way)(i).refCnt := incr_num
        }.otherwise {
          meta_array(main_idx)(replace_way)(i)  := 0.U.asTypeOf(new VpnTableMetaInfo)
        }
      }
    }

    when (alloc_sub) {
      // only update the meta of the target sub entry
      meta_array(main_idx)(target_way)(sub_idx).valid := true.B
      meta_array(main_idx)(target_way)(sub_idx).refCnt := incr_num
    }

    when (no_alloc) {
      // only update the refCnt of the target sub entry
      meta_array(main_idx)(target_way)(sub_idx).refCnt := meta_array(main_idx)(target_way)(sub_idx).refCnt + incr_num
    }
  }

  // Refresh Logic
  when (refreshCnt < vpnResetPeriod.U && train_req.valid){
    refreshCnt := refreshCnt + 1.U
  }

  is_refresh := refreshCnt >= vpnResetPeriod.U && !reset.asBool
  when (is_refresh) {
    refreshCnt := 0.U

    // go through every sub entry
    for (i <- 0 until vpnTableSetNum) {
      for (j <- 0 until vpnTableWayNum) {
        for (k <- 0 until vpnTableSubEntryNum) {
          val entry = meta_array(i)(j)(k)

          /**
           * Update Entry:
           *  prevRefCnt -> 0.8 * refCnt + 0.2 * prevRefCnt
           *  refCnt  -> 0
           *  hot -> prevRefCnt > hotThreshold ? 1 : 0
           * */
          // TODO: For better timing, maybe we should pipeline this.
          val nxt_prevRefCnt = ((entry.refCnt * 13.U) + (entry.prevRefCnt * 3.U)) >> 4.U

          entry.refCnt      := 0.U
          entry.prevRefCnt  := nxt_prevRefCnt
          entry.hot         := Mux(nxt_prevRefCnt > hotThreshold.U, true.B, false.B)
        }
      }
    }
  }

  // ------------------ Performance Counter ------------------
  XSPerfAccumulate("vt_refresh", is_refresh)
  XSPerfAccumulate("vt_alloc_main", train_req.valid && train_req.bits.alloc_main)
  XSPerfAccumulate("vt_alloc_sub", train_req.valid && train_req.bits.alloc_sub)
  XSPerfAccumulate("vt_no_alloc", train_req.valid && !train_req.bits.alloc_main && !train_req.bits.alloc_sub)

  // VpnTable train index distribution.
  for (i <- 0 until vpnTableSetNum) {
    XSPerfAccumulate(s"vt_train_main_idx_$i", train_req.valid && !is_refresh && train_req.bits.main_idx === i.U)
    XSPerfAccumulate(s"vt_alloc_main_idx_$i", train_req.valid && !is_refresh && train_req.bits.alloc_main && train_req.bits.main_idx === i.U)
    XSPerfAccumulate(s"vt_alloc_sub_main_idx_$i", train_req.valid && !is_refresh && train_req.bits.alloc_sub && train_req.bits.main_idx === i.U)
    XSPerfAccumulate(
      s"vt_no_alloc_main_idx_$i",
      train_req.valid && !is_refresh && !train_req.bits.alloc_main && !train_req.bits.alloc_sub && train_req.bits.main_idx === i.U
    )
  }
  for (i <- 0 until vpnTableSubEntryNum) {
    XSPerfAccumulate(s"vt_train_sub_idx_$i", train_req.valid && !is_refresh && train_req.bits.sub_idx === i.U)
    XSPerfAccumulate(s"vt_alloc_sub_sub_idx_$i", train_req.valid && !is_refresh && train_req.bits.alloc_sub && train_req.bits.sub_idx === i.U)
    XSPerfAccumulate(
      s"vt_no_alloc_sub_idx_$i",
      train_req.valid && !is_refresh && !train_req.bits.alloc_main && !train_req.bits.alloc_sub && train_req.bits.sub_idx === i.U
    )
  }
  for (i <- 0 until vpnTableSetNum) {
    for (j <- 0 until vpnTableSubEntryNum) {
      XSPerfAccumulate(
        s"vt_train_idx_${i}_$j",
        train_req.valid && !is_refresh && train_req.bits.main_idx === i.U && train_req.bits.sub_idx === j.U
      )
    }
  }

  // train trig data
  XSPerfAccumulate("in_train_trig_used", train_req.valid && !is_refresh)
  XSPerfAccumulate("in_train_trig_drop_by_refresh", train_req.valid && is_refresh)
}

class ftQueryReq(implicit p: Parameters) extends CDPBundle {
  val set_idx = UInt(filterTableSetBits.W)
}

class ftQueryRsp(implicit p: Parameters) extends CDPBundle {
  val valid_vec = Vec(filterTableWayNum, Bool())
  val tag_vec   = Vec(filterTableWayNum, UInt(filterTableTagBits.W))
  val sat_vec   = Vec(filterTableWayNum, Vec(filterEntryBlks, UInt(2.W)))
}

class ftTrainReq(implicit p: Parameters) extends CDPBundle {
  val set     = UInt(filterTableSetBits.W)
  val way     = UInt(log2Ceil(filterTableWayNum).W)
  val tag     = UInt(filterTableTagBits.W)
  val sat     = Vec(filterEntryBlks, UInt(2.W))
}

class FilterTable(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val query_req = Flipped(DecoupledIO(new ftQueryReq))
    val query_rsp = ValidIO(new ftQueryRsp)
    val train_req = Flipped(DecoupledIO(new ftTrainReq))
  })

  val (query_req, query_rsp) = (io.query_req, io.query_rsp)
  val train_req = io.train_req

  val SatBankNum = 2
  val SatBankBlks = filterEntryBlks / SatBankNum
  require(filterEntryBlks % SatBankNum == 0, "filterEntryBlks must be divisible by SatBankNum")

  def ftMetaEntry() = new Bundle {
    val valid = Bool()
    val tag   = UInt(filterTableTagBits.W)
  }

  val metaArray = Module(
    new SRAMTemplate(
      ftMetaEntry(),
      set = filterTableSetNum,
      way = filterTableWayNum,
      shouldReset = true,
      singlePort = true,
      hasMbist = cacheParams.hasMbist,
      hasSramCtl = cacheParams.hasSramCtl
    )
  )
  val satBanks = Seq.fill(SatBankNum) {
    Module(
      new SRAMTemplate(
        Vec(SatBankBlks, UInt(2.W)),
        set = filterTableSetNum,
        way = filterTableWayNum,
        singlePort = true,
        hasMbist = cacheParams.hasMbist,
        hasSramCtl = cacheParams.hasSramCtl
      )
    )
  }

  // query
  val queryReady = metaArray.io.r.req.ready && satBanks.map(_.io.r.req.ready).reduce(_ && _) && !train_req.valid
  query_req.ready := queryReady
  val queryFire = query_req.valid && queryReady

  metaArray.io.r.req.valid := queryFire
  metaArray.io.r.req.bits.setIdx := query_req.bits.set_idx
  satBanks.foreach { bank =>
    bank.io.r.req.valid := queryFire
    bank.io.r.req.bits.setIdx := query_req.bits.set_idx
  }

  query_rsp.valid := RegNext(queryFire, false.B)
  for (i <- 0 until filterTableWayNum) {
    query_rsp.bits.valid_vec(i) := metaArray.io.r.resp.data(i).valid
    query_rsp.bits.tag_vec(i)   := metaArray.io.r.resp.data(i).tag
    for (j <- 0 until filterEntryBlks) {
      query_rsp.bits.sat_vec(i)(j) := satBanks(j / SatBankBlks).io.r.resp.data(i)(j % SatBankBlks)
    }
  }

  // train
  val trainReady = metaArray.io.w.req.ready && satBanks.map(_.io.w.req.ready).reduce(_ && _)
  train_req.ready := trainReady
  val trainFire = train_req.valid && trainReady
  val trainWayOH = UIntToOH(train_req.bits.way, filterTableWayNum)

  val metaWData = WireInit(0.U.asTypeOf(ftMetaEntry()))
  metaWData.valid := true.B
  metaWData.tag   := train_req.bits.tag
  metaArray.io.w.req.valid := trainFire
  metaArray.io.w.req.bits.apply(metaWData, train_req.bits.set, trainWayOH)

  for (bankIdx <- 0 until SatBankNum) {
    val satWData = Wire(Vec(SatBankBlks, UInt(2.W)))
    for (blkIdx <- 0 until SatBankBlks) {
      satWData(blkIdx) := train_req.bits.sat(bankIdx * SatBankBlks + blkIdx)
    }
    satBanks(bankIdx).io.w.req.valid := trainFire
    satBanks(bankIdx).io.w.req.bits.apply(satWData, train_req.bits.set, trainWayOH)
  }
}

class CDPDetectReq(implicit p: Parameters) extends CDPBundle {
  /**
    * Request Task processed in DetectPipe
    * Data width == 8 byte
    */
  val data      = UInt(64.W)  // 8 byte
  val pfDepth   = UInt(pfDepthBits.W)
  val pfSource  = UInt(PfSource.pfSourceBits.W)
  val is_hit    = Bool()  // is this trigger from req hitting l2?
}

class CDPPrefetchReq(implicit p: Parameters) extends CDPBundle {
  /**
    * Internal prefetchReq
    * Used in DetectPipe <> degreeBuf <> SentUnit
    */
  val pfVAddr  = UInt(fullVAddrBits.W)
  val pfDepth = UInt(pfDepthBits.W)

  // Only for monitor
  val pfSource  = UInt(PfSource.pfSourceBits.W)
  val is_hit    = Bool()    // Req triggered by hitting l2
}

class ftTrainPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val train_trigger = Flipped(DecoupledIO(new PrefetchTrain))

    // to FilterTable
    val query_req = DecoupledIO(new ftQueryReq)
    val query_rsp = Input(new ftQueryRsp)
    val train_req = DecoupledIO(new ftTrainReq)
  })

  val (train_trigger, query_req, query_rsp, train_req) =
    (io.train_trigger, io.query_req, io.query_rsp, io.train_req)

  val replacer = ReplacementPolicy.fromString(replType, filterTableWayNum, filterTableSetNum)

  val s1_valid = RegInit(false.B)
  val s1_set_idx = Reg(UInt(filterTableSetBits.W))
  val s1_offset  = Reg(UInt(filterTableOffsetBits.W))
  val s1_tag     = Reg(UInt(filterTableTagBits.W))
  val s1_is_used = Reg(Bool())
  val s1_is_evict = Reg(Bool())

  val s2_valid = RegInit(false.B)
  val s2_set_idx = Reg(UInt(filterTableSetBits.W))
  val s2_offset  = Reg(UInt(filterTableOffsetBits.W))
  val s2_tag     = Reg(UInt(filterTableTagBits.W))
  val s2_is_used = Reg(Bool())
  val s2_is_evict = Reg(Bool())

  val s3_valid = Wire(Bool())
  val s3_set_idx = Wire(UInt(filterTableSetBits.W))
  val s3_offset  = Wire(UInt(filterTableOffsetBits.W))
  val s3_tag     = Wire(UInt(filterTableTagBits.W))
  val s3_is_used = Wire(Bool())
  val s3_is_evict = Wire(Bool())

  val same_vec = Wire(Vec(3, Bool()))
  val same_addr = same_vec.reduce(_ || _)
  val s1_ready = !s1_valid || query_req.ready && train_req.ready    // s1 ready to recv req from s0

  // ----------- s0: accept train trigger -----------
  val s0_valid = train_trigger.valid && !same_addr
  train_trigger.ready := !reset.asBool && s1_ready && !same_addr

  val train_paddr = Mux(
    train_trigger.bits.cdp_filter_train_evict,
    train_trigger.bits.evict_addr,
    train_trigger.bits.addr
  )

  val s0_set_idx = get_filter_set(train_paddr)
  val s0_offset  = get_filter_offset(train_paddr)
  val s0_tag     = get_filter_tag(train_paddr)
  val s0_is_used = train_trigger.bits.cdp_filter_train_hit
  val s0_is_evict = train_trigger.bits.cdp_filter_train_evict

  // ----------- s1: query FilterTable -----------
  when (s1_ready) {
    s1_valid   := s0_valid
    s1_set_idx := s0_set_idx
    s1_offset  := s0_offset
    s1_tag     := s0_tag
    s1_is_used := s0_is_used
    s1_is_evict := s0_is_evict
  }

  query_req.valid := s1_valid
  query_req.bits.set_idx := s1_set_idx

  same_vec(0) := s1_valid && s1_set_idx === s0_set_idx && s1_tag === s0_tag

  // ----------- s2: receive FilterTable and latch -----------
  when (query_req.fire) {
    s2_valid := s1_valid
    s2_set_idx := s1_set_idx
    s2_offset  := s1_offset
    s2_tag     := s1_tag
    s2_is_used := s1_is_used
    s2_is_evict := s1_is_evict
  }.otherwise {
    s2_valid := false.B
  }

  val ft_s2_rsp = query_rsp

  same_vec(1) := s2_valid && s2_set_idx === s0_set_idx && s2_tag === s0_tag

  // ----------- s3: chk hit and update -----------
  s3_valid := RegNext(s2_valid, false.B)
  s3_set_idx  := RegNext(s2_set_idx)
  s3_offset   := RegNext(s2_offset)
  s3_tag      := RegNext(s2_tag)
  s3_is_used  := RegNext(s2_is_used)
  s3_is_evict := RegNext(s2_is_evict)

  val ft_s3_rsp = RegNext(ft_s2_rsp)

  val s3_hit_vec = ft_s3_rsp.tag_vec.zip(ft_s3_rsp.valid_vec).map {
    case (tag, valid) => tag === s3_tag && valid
  }

  val s3_hit = s3_hit_vec.reduce(_ || _)
  val s3_hit_way = PriorityEncoder(s3_hit_vec)
  assert(PopCount(s3_hit_vec) < 2.U || !s3_valid, "FilterTable entry multiple hit!")

  val s3_repl_way = replacer.way(s3_set_idx)
  val s3_target_way = Mux(s3_hit, s3_hit_way, s3_repl_way)
  val s3_old_sat_vec = ft_s3_rsp.sat_vec(s3_target_way)
  val s3_need_update = s3_hit || s3_is_evict

  val s3_next_sat_vec = Wire(Vec(filterEntryBlks, UInt(2.W)))
  for (i <- 0 until filterEntryBlks) {
    val old_sat = s3_old_sat_vec(i)
    when (!s3_hit) {
      s3_next_sat_vec(i) := Mux(i.U === s3_offset, Mux(s3_is_used, 1.U, 3.U), 2.U)  // if not hit, set the accessed block to 1 or 3, others to 2
    }.otherwise {
      // if hit, update the accessed block's sat counter based on whether it is used or not
      s3_next_sat_vec(i) := Mux(i.U === s3_offset,
        Mux(s3_is_used, Mux(old_sat === 0.U, 0.U, old_sat - 1.U), Mux(old_sat === 3.U, 3.U, old_sat + 1.U)),
        old_sat
      )
    }
  }

  val s3_update_info = WireInit(0.U.asTypeOf(new ftTrainReq))
  s3_update_info.set := s3_set_idx
  s3_update_info.way := s3_target_way
  s3_update_info.tag := s3_tag
  s3_update_info.sat := s3_next_sat_vec

  same_vec(2) := s3_valid && s3_set_idx === s0_set_idx && s3_tag === s0_tag

  train_req.valid := s3_valid && s3_need_update
  train_req.bits  := s3_update_info

  assert(!train_req.valid || train_req.ready, "FilterTable train req should be ready when valid!")

  when (train_req.fire) {
    replacer.access(s3_update_info.set, s3_update_info.way)
  }
}

class vtTrainPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val train_trigger = Flipped(DecoupledIO(new PrefetchTrain))

    // to VPN Table
    val query_req = ValidIO(new vtQueryReq)
    val query_rsp = Flipped(ValidIO(new vtQueryRsp))
    val train_req = ValidIO(new vtTrainReq)

    // to DetectPipe
    val addr_state = new Bundle {
      val is_sv39 = Output(Bool())
      val is_sv48 = Output(Bool())
      val is_sv57 = Output(Bool())
    }
  })

  val (train_trigger, query_req, query_rsp, train_req) =
    (io.train_trigger, io.query_req, io.query_rsp, io.train_req)

  val replacer = ReplacementPolicy.fromString(replType, vpnTableWayNum, vpnTableSetNum)

  val stage_valid = Wire(Vec(5, Bool()))
  val same_vec = Wire(Vec(4, Bool()))

  val same_addr = same_vec.reduce(_ || _)

  // Record Max Addr Range
  val s_sv39::s_sv48::s_sv57::Nil = Enum(3)
  val addr_state = RegInit(s_sv39)

  // ----------- s0: accept train trigger -----------
  stage_valid(0) := train_trigger.valid && !same_addr
  train_trigger.ready := reset.asBool || !same_addr

  val train_vaddr = train_trigger.bits.vaddr.getOrElse(0.U) << log2Ceil(blockBytes)
  val s0_main_idx = get_main_idx(train_vaddr)
  val s0_sub_idx  = get_sub_idx(train_vaddr)
  val s0_tag      = get_vpntab_tag(train_vaddr)
  val s0_is_hit_cdp = train_trigger.bits.hit && train_trigger.bits.pfsource === PfSource.CDP.id.U

  // addr_state update
  val max_addr_width = if (train_trigger.bits.vaddr.isDefined) {
    train_trigger.bits.vaddr.getOrElse(0.U).getWidth + log2Ceil(blockBytes)
  } else {
    39  // default to sv39 if vaddr is not defined
  }

  val next_state = WireDefault(addr_state)
  switch (addr_state) {
    is (s_sv39) {
      if (39 < max_addr_width && max_addr_width <= 48) {
        when (train_trigger.fire) {
          next_state := Mux(train_vaddr(train_vaddr.getWidth - 1, 39) =/= 0.U, s_sv48, s_sv39)
        }
      }
      if (48 < max_addr_width && max_addr_width <= 57) {
        when (train_trigger.fire) {
          next_state := Mux(train_vaddr(train_vaddr.getWidth - 1, 48) =/= 0.U, 
            s_sv57, 
            Mux(train_vaddr(train_vaddr.getWidth - 1, 39) =/= 0.U, s_sv48, s_sv39)
          )
        }
      }
    }
    is (s_sv48) {
      if (48 < max_addr_width && max_addr_width <= 57) {
        when (train_trigger.fire) {
          next_state := Mux(train_vaddr(train_vaddr.getWidth - 1, 48) =/= 0.U, s_sv57, s_sv48)
        }
      }
    }
  }

  addr_state := next_state
  io.addr_state.is_sv39 := addr_state === s_sv39
  io.addr_state.is_sv48 := addr_state === s_sv48
  io.addr_state.is_sv57 := addr_state === s_sv57

  // ----------- s1: query VpnTable -----------
  stage_valid(1) := RegNext(stage_valid(0))
  val s1_main_idx = RegNext(s0_main_idx)
  val s1_sub_idx  = RegNext(s0_sub_idx)
  val s1_tag      = RegNext(s0_tag)
  val s1_is_hit_cdp = RegNext(s0_is_hit_cdp)
  val s1_tab_rsp = query_rsp.bits

  query_req.valid := stage_valid(1)
  query_req.bits.main_idx := s1_main_idx
  query_req.bits.sub_idx  := s1_sub_idx

  same_vec(0) := stage_valid(1) && s1_main_idx === s0_main_idx && s1_tag === s0_tag

  // ----------- s2: check VpnTable hit -----------
  stage_valid(2) := RegNext(stage_valid(1))
  val s2_main_idx = RegNext(s1_main_idx)
  val s2_sub_idx  = RegNext(s1_sub_idx)
  val s2_tag      = RegNext(s1_tag)
  val s2_is_hit_cdp = RegNext(s1_is_hit_cdp)
  val s2_tab_rsp = RegNext(s1_tab_rsp)

  val s2_hit_main_vec = s2_tab_rsp.tag_vec.zip(s2_tab_rsp.valid_vec).map {
    case (tag, valid) => tag === s2_tag && valid
  }
  val s2_hit_main_idx = PriorityEncoder(s2_hit_main_vec)
  val s2_hit_main = s2_hit_main_vec.reduce(_ || _)
  assert(PopCount(s2_hit_main_vec) < 2.U || !stage_valid(2), "Main entry multiple hit!")

  val s2_hit_sub_vec = s2_tab_rsp.tag_vec.zip(s2_tab_rsp.meta_vec).zip(s2_tab_rsp.valid_vec).map {
    case ((tag, meta), valid) => tag === s2_tag && meta.valid && valid
  }
  val s2_hit_sub = s2_hit_sub_vec.reduce(_ || _)

  same_vec(1) := stage_valid(2) && s2_main_idx === s0_main_idx && s2_tag === s0_tag

  // ----------- s3: build VpnTable update -----------
  stage_valid(3) := RegNext(stage_valid(2))
  val s3_main_idx = RegNext(s2_main_idx)
  val s3_sub_idx  = RegNext(s2_sub_idx)
  val s3_tag      = RegNext(s2_tag)
  val s3_is_hit_cdp = RegNext(s2_is_hit_cdp)
  val s3_hit_main = RegNext(s2_hit_main)
  val s3_hit_sub = RegNext(s2_hit_sub)
  val s3_hit_main_idx = RegNext(s2_hit_main_idx)

  val plru_way = replacer.way(s3_main_idx)
  val s3_update_info = WireInit(0.U.asTypeOf(new vtTrainReq))
  s3_update_info.tag := s3_tag
  s3_update_info.main_idx := s3_main_idx
  s3_update_info.sub_idx := s3_sub_idx
  s3_update_info.alloc_main := !s3_hit_main
  s3_update_info.alloc_sub := s3_hit_main && !s3_hit_sub
  s3_update_info.target_way := Mux(s3_hit_main, s3_hit_main_idx, plru_way)
  s3_update_info.is_hit_cdp := s3_is_hit_cdp

  same_vec(2) := stage_valid(3) && s3_main_idx === s0_main_idx && s3_tag === s0_tag

  // ----------- s4: drive VpnTable update -----------
  stage_valid(4) := RegNext(stage_valid(3))
  val s4_update_info = RegNext(s3_update_info)

  train_req.valid := stage_valid(4)
  train_req.bits := s4_update_info

  when (stage_valid(4)) {
    replacer.access(s4_update_info.main_idx, s4_update_info.target_way)
  }

  same_vec(3) := stage_valid(4) && s4_update_info.main_idx === s0_main_idx && s4_update_info.tag === s0_tag

  class trainTriggerEntry extends CDPBundle {
    val vaddr = UInt(fullVAddrBits.W)
    val main_idx = UInt(mainEntryBits.W)
    val sub_idx  = UInt(subEntryBits.W)
    val way      = UInt(vpnWayBits.W)
    val alloc_main  = Bool()
    val alloc_sub   = Bool()
    val no_alloc    = Bool()
  }

  val cdpTrainTriggerDB = ChiselDB.createTable("cdpTrain", new trainTriggerEntry, basicDB = debug)

  val train_trigger_entry = Wire(new trainTriggerEntry)
  train_trigger_entry.vaddr := RegNext(RegNext(RegNext(RegNext(train_vaddr))))
  train_trigger_entry.main_idx := s4_update_info.main_idx
  train_trigger_entry.sub_idx := s4_update_info.sub_idx
  train_trigger_entry.way := s4_update_info.target_way
  train_trigger_entry.alloc_main := s4_update_info.alloc_main
  train_trigger_entry.alloc_sub := s4_update_info.alloc_sub
  train_trigger_entry.no_alloc := !s4_update_info.alloc_main && !s4_update_info.alloc_sub

  cdpTrainTriggerDB.log(train_trigger_entry, stage_valid(4), "", clock, reset)
}

class DetectPipeline(name:String)(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val detect_req    = Flipped(ValidIO(new CDPDetectReq))

    // to Vpn Table
    val vt_query_req  = ValidIO(new vtQueryReq)
    val vt_query_rsp  = Flipped(ValidIO(new vtQueryRsp))

    // Prefetch Req
    val pft_req = ValidIO(new CDPPrefetchReq)

    // addr_state
    val addr_state = new Bundle {
      val is_sv39 = Input(Bool())
      val is_sv48 = Input(Bool())
      val is_sv57 = Input(Bool())
    }
  })

  val detect_req  = io.detect_req
  val pft_req     = io.pft_req

  val (vt_query_req, vt_query_rsp) = (io.vt_query_req, io.vt_query_rsp)

  // Pipeline Ctrl Signals
  val s0_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s1_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s2_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s3_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))
  val s4_req  = WireInit(0.U.asTypeOf(Valid(new CDPDetectReq)))

  // ------------------ s0 ------------------
  s0_req.valid  := detect_req.valid
  s0_req.bits   := detect_req.bits

  // ------------------ s1 ------------------
  // query VpnTable
  s1_req.valid  := RegNext(s0_req.valid)
  s1_req.bits   := RegNext(s0_req.bits)

  val s1_data     = s1_req.bits.data
  val s1_addr     = s1_data(fullVAddrBits - 1, 0)
  val s1_main_idx = get_main_idx(s1_addr)
  val s1_sub_idx  = get_sub_idx(s1_addr)

  vt_query_req.valid  := s1_req.valid
  vt_query_req.bits.main_idx  := s1_main_idx
  vt_query_req.bits.sub_idx   := s1_sub_idx

  val s1_vt_query_rsp = vt_query_rsp.bits

  // ------------------ s2 ------------------
  // check conditions
  s2_req.valid  := RegNext(s1_req.valid)
  s2_req.bits   := RegNext(s1_req.bits)

  val s2_data   = s2_req.bits.data
  val s2_addr   = s2_data(fullVAddrBits - 1, 0)
  val s2_depth  = s2_req.bits.pfDepth
  val s2_is_hit = s2_req.bits.is_hit
  
  val s2_vt_query_rsp = RegNext(s1_vt_query_rsp)

  val s2_tag  = get_vpntab_tag(s2_addr)
  val s2_vt_hit_vec = s2_vt_query_rsp.tag_vec.zip(s2_vt_query_rsp.meta_vec).map{
    case (t, m) =>
      t === s2_tag && m.valid
  }
  assert(PopCount(s2_vt_hit_vec) < 2.U || !s2_req.valid, "VpnTable multiple hit in DetectPipeline!")
  
  val s2_vt_hit     = s2_vt_hit_vec.reduce(_ || _)
  val s2_vt_hit_idx = PriorityEncoder(s2_vt_hit_vec)
  val s2_vt_hit_hot = s2_vt_query_rsp.meta_vec(s2_vt_hit_idx).hot

  val s2_vpn0 = get_vpn0(s2_addr)
  val s2_vpn0_is_nzero    = s2_vpn0 =/= 0.U

  val s2_low_bit  = s2_data(1, 0)
  val s2_low_bit_is_zero  = s2_low_bit === 0.U

  val s2_high_bit = Mux(
    io.addr_state.is_sv39, 
    s2_data(63, 39),
    Mux(io.addr_state.is_sv48, 
      s2_data(63, 48),
      Mux(io.addr_state.is_sv57, s2_data(63, 57), 0.U)
    )
  )
  val s2_high_bit_is_zero = s2_high_bit === 0.U

  // TODO: maybe we should move depth control totally to the entrance?
  val s2_can_pft  = s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero && s2_vt_hit && s2_vt_hit_hot

  // ------------------ s3 ------------------
  // generate prefetch req
  s3_req.valid  := RegNext(s2_req.valid && s2_can_pft)
  s3_req.bits   := RegNext(s2_req.bits)

  val s3_vt_hit     = RegNext(s2_vt_hit)
  val s3_vt_hit_idx = RegNext(s2_vt_hit_idx)
  val s3_can_pft    = RegNext(s2_can_pft)
  val s3_depth      = RegNext(Mux(
    s2_is_hit,
    1.U,      // hit a CDP prefetched block, reinforce
    Mux(s2_depth === 0.U, pfDepthMax.U, s2_depth + 1.U)
  ))

  val s3_data = s3_req.bits.data

  pft_req.valid := s3_req.valid
  pft_req.bits.pfVAddr  := s3_data(fullVAddrBits - 1, 0)
  pft_req.bits.pfDepth  := s3_depth
  pft_req.bits.pfSource := s3_req.bits.pfSource
  pft_req.bits.is_hit   := s3_req.bits.is_hit

  // ------------------ Performance Counter ------------------
  // Valid VpnTable hit/miss and distribution
  XSPerfAccumulate("valid_vt_hit", s2_req.valid && s2_vt_hit && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero)
  XSPerfAccumulate("valid_vt_miss", s2_req.valid && !s2_vt_hit && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero)

  // ----------- ChiselDB -----------
  class detectTriggerEntry extends CDPBundle {
    val vaddr     = UInt(64.W)
    val pfDepth   = UInt(pfDepthBits.W)
    val pfSource  = UInt(PfSource.pfSourceBits.W)
    val main_idx    = UInt(mainEntryBits.W)
    val sub_idx     = UInt(subEntryBits.W)
    val vt_hit      = Bool()
    val vt_hit_hot  = Bool()
    val canPft      = Bool()
  }

  val cdpDetectTriggerDB = ChiselDB.createTable(name + "_cdpDetect", new detectTriggerEntry, basicDB = debug)

  val detect_trigger_entry = Wire(new detectTriggerEntry)
  detect_trigger_entry.vaddr := s2_data
  detect_trigger_entry.pfDepth := s2_depth
  detect_trigger_entry.pfSource := s2_req.bits.pfSource
  detect_trigger_entry.main_idx := RegNext(s1_main_idx)
  detect_trigger_entry.sub_idx  := RegNext(s1_sub_idx)
  detect_trigger_entry.vt_hit := s2_vt_hit
  detect_trigger_entry.vt_hit_hot := s2_vt_hit_hot
  detect_trigger_entry.canPft := s2_can_pft

  val en = s2_req.valid && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero
  cdpDetectTriggerDB.log(detect_trigger_entry, en, "", clock, reset)
}

class PrefetchFilterEntry(implicit p: Parameters) extends CDPBundle {
  val paddr_valid = Bool()
  val pTag  = UInt(reqFilterPTagBits.W)   // paddr = [ pTag | blockOffset ]
  val vTag  = UInt(reqFilterVTagBits.W)   // vaddr = [ vTag | blockOffset ]
  val pfDepth = UInt(pfDepthBits.W)

  // for TLB retry
  val retry_en    = Bool()
  val retry_timer = UInt(4.W)
  
  // Only for monitor
  val pfSource = UInt(PfSource.pfSourceBits.W)
  val is_hit   = Bool() // is this trigger from req hitting l2?

  def toPrefetchReq(): PrefetchReq = {
    val req = Wire(new PrefetchReq)

    val full_addr = Cat(pTag, 0.U(log2Ceil(blockBytes).W))
    req := DontCare
    req.tag := parseFullAddress(full_addr)._1
    req.set := parseFullAddress(full_addr)._2
    req.pfSource  := MemReqSource.Prefetch2L2CDP.id.U
    req.pfDepth   := pfDepth

    req
  }
}

class SentUnit(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val in  = Flipped(DecoupledIO(new CDPPrefetchReq))
    val out = DecoupledIO(new PrefetchReq)

    // tlb
    val tlb_req = new L2ToL1TlbIO

    // filter table
    val ft_query_req = DecoupledIO(new ftQueryReq)
    val ft_query_rsp = Input(new ftQueryRsp)
  })

  def same_page(addr1: UInt, addr2: UInt): Bool = {
    addr1(fullVAddrBits - 1, pageOffsetBits) === addr2(fullVAddrBits - 1, pageOffsetBits)
  }

  val (in, out) = (io.in, io.out)
  val (ft_query_req, ft_query_rsp) = (io.ft_query_req, io.ft_query_rsp)

  val tlb_req = io.tlb_req.req
  val tlb_rsp = io.tlb_req.resp
  val pmp_rsp = io.tlb_req.pmp_resp
  io.tlb_req.req_kill := false.B
  tlb_rsp.ready := true.B

  // check same cacheline
  def block_addr(addr: UInt) = {
    require(addr.getWidth >= log2Ceil(blockBytes), "Address width is smaller than block size")
    addr(addr.getWidth - 1, log2Ceil(blockBytes))
  }

  // buffer
  val valids    = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(false.B)))
  val entries   = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(0.U.asTypeOf(new PrefetchFilterEntry))))
  
  val req_inflight  = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(false.B)))

  val tlb_arb = Module(new TwoLevelRRArbiter(new L2TlbReq, reqFilterEntryNum))
  val pft_arb = Module(new TwoLevelRRArbiter(new PrefetchReq, reqFilterEntryNum))
  ArbPerf(tlb_arb, "cdp_tlb_arb")
  ArbPerf(pft_arb, "cdp_pft_arb")

  // enq buf logic
  in.ready := true.B  // TODO: backpressure when buffer full

  val entry_hit_vec = entries.zip(valids).map{
    case (e, v) =>
      v && e.vTag === block_addr(in.bits.pfVAddr)
  }
  val entry_hit = entry_hit_vec.reduce(_ || _)

  val free_entry_vec = valids.map(!_)
  val has_free_entry = free_entry_vec.reduce(_ || _)
  val free_entry_idx = PriorityEncoder(free_entry_vec)

  val idx = free_entry_idx
  val entry = entries(idx)
  when (in.valid && !entry_hit && has_free_entry) {
    val alloc_entry = WireInit(0.U.asTypeOf(new PrefetchFilterEntry))
    alloc_entry.vTag := block_addr(in.bits.pfVAddr)
    alloc_entry.pfDepth   := in.bits.pfDepth
    alloc_entry.pfSource  := in.bits.pfSource
    alloc_entry.is_hit    := in.bits.is_hit

    entry := alloc_entry
    valids(idx) := true.B
  }

  // timer
  for (i <- 0 until reqFilterEntryNum) {
    when (entries(i).retry_en && entries(i).retry_timer < 10.U) {   // TODO: parameterize the interval value
      entries(i).retry_timer := entries(i).retry_timer + 1.U
    }
  }

  // --------------- tlb pipe ---------------
  val tlb_s1_valid = RegInit(false.B)
  val tlb_s2_valid = RegNext(tlb_req.fire, false.B)
  val tlb_s3_valid = RegNext(tlb_s2_valid, false.B)

  val tlb_s1_req = RegEnable(tlb_arb.io.out.bits, tlb_arb.io.out.fire)
  val tlb_s1_addr = Wire(UInt(fullVAddrBits.W))
  val tlb_s2_addr = RegEnable(tlb_s1_addr, tlb_req.fire)
  val tlb_s3_addr = RegEnable(tlb_s2_addr, tlb_s2_valid)

  // -------- tlb s0: arb tlb req from buffer --------
  for (i <- 0 until reqFilterEntryNum) {
    val entry = entries(i)
    val entry_tlb_req = tlb_arb.io.in(i)

    val entry_timer_ok = !entry.retry_en || entry.retry_timer >= 10.U

    val req_vaddr = Cat(entry.vTag, 0.U(log2Ceil(blockBytes).W))
    val s1_same_page = same_page(req_vaddr, tlb_s1_addr) && tlb_s1_valid
    val s2_same_page = same_page(req_vaddr, tlb_s2_addr) && tlb_s2_valid
    val s3_same_page = same_page(req_vaddr, tlb_s3_addr) && tlb_s3_valid
    val page_conflict = s1_same_page || s2_same_page || s3_same_page
    assert(
      PopCount(Seq(s1_same_page, s2_same_page, s3_same_page)) <= 1.U,
      "multiple inflight tlb reqs should not target the same page!"
    )

    entry_tlb_req.valid := valids(i) && !entry.paddr_valid && entry_timer_ok && !page_conflict
    entry_tlb_req.bits  := DontCare
    entry_tlb_req.bits.vaddr  := req_vaddr
    entry_tlb_req.bits.cmd    := TlbCmd.read
    entry_tlb_req.bits.isPrefetch := true.B
    entry_tlb_req.bits.size   := 3.U
    entry_tlb_req.bits.kill   := false.B
    entry_tlb_req.bits.no_translate := false.B
  }

  // -------- tlb s1: send tlb req --------
  val tlb_s1_ready = !tlb_s1_valid || tlb_req.ready
  tlb_arb.io.out.ready := tlb_s1_ready
  when (tlb_s1_ready) {
    tlb_s1_valid := tlb_arb.io.out.valid
  }

  tlb_req.valid := tlb_s1_valid
  tlb_req.bits := tlb_s1_req
  tlb_s1_addr := tlb_s1_req.vaddr

  // -------- tlb s2: recv tlb rsp --------
  // If miss, enable retry. If the retry also misses, drop the req.
  val tlb_s2_rsp = tlb_rsp

  // -------- tlb s3: recv pmp rsp --------
  val tlb_s3_pmp       = pmp_rsp
  val tlb_s3_rsp_valid = RegNext(tlb_s2_rsp.valid, false.B)
  val tlb_s3_rsp_bits  = RegNext(tlb_s2_rsp.bits)

  val s3_drop =
    // page/access fault
    tlb_s3_rsp_bits.excp.head.pf.ld || tlb_s3_rsp_bits.excp.head.gpf.ld || tlb_s3_rsp_bits.excp.head.af.ld ||
    // uncache
    tlb_s3_pmp.mmio || Pbmt.isUncache(tlb_s3_rsp_bits.pbmt) ||
    // pmp access fault
    tlb_s3_pmp.ld
  
  // gather info from s2 and s3
  for (i <- 0 until reqFilterEntryNum) {
    val entry = entries(i)
    val entry_addr = Cat(entries(i).vTag, 0.U(log2Ceil(blockBytes).W))
    
    // s2: if first miss, enable retry; if second miss, drop
    val s2_same_page = same_page(entry_addr, tlb_s2_addr) && tlb_s2_valid
    when (s2_same_page && tlb_s2_rsp.valid && tlb_s2_rsp.bits.miss && valids(i)) {
      when (entry.retry_en) {
        // second miss, drop the req
        valids(i) := false.B
      }.otherwise {
        // first miss, enable retry
        entries(i).retry_en := true.B
        entries(i).retry_timer := 0.U
      }
    }
    XSPerfAccumulate(
      s"s2_drop_tlb_miss_entry$i",
      s2_same_page && tlb_s2_rsp.valid && tlb_s2_rsp.bits.miss && valids(i) && entry.retry_en
    )

    // s3: check pf && pmp result, if fail, drop the req; otherwise, update the entry
    val s3_same_page = same_page(entry_addr, tlb_s3_addr) && tlb_s3_valid
    when (s3_same_page && tlb_s3_rsp_valid && !tlb_s3_rsp_bits.miss && valids(i)) {
      when (s3_drop) {
        valids(i) := false.B
      }.otherwise {
        entries(i).paddr_valid := true.B

        val page_num    = tlb_s3_rsp_bits.paddr.head(fullAddressBits - 1, pageOffsetBits)
        val page_offset = entry_addr(pageOffsetBits - 1, 0)
        entries(i).pTag := block_addr(Cat(page_num, page_offset))
      }
    }
    XSPerfAccumulate(
      s"s3_drop_entry$i",
      s3_same_page && tlb_s3_rsp_valid && !tlb_s3_rsp_bits.miss && valids(i) && s3_drop
    )
  }

  // --------------- prefetch req pipe ---------------
  val pft_s0_valid = Wire(Bool())
  val pft_s1_valid = RegNext(ft_query_req.fire, false.B)
  val pft_s2_valid = RegNext(pft_s1_valid, false.B)

  val pft_s0_chosen_idx = Wire(UInt(log2Ceil(reqFilterEntryNum).W))
  val pft_s1_chosen_idx = RegEnable(pft_s0_chosen_idx, ft_query_req.fire)
  val pft_s2_chosen_idx = RegEnable(pft_s1_chosen_idx, pft_s1_valid)

  val pft_s0_req = Wire(new PrefetchReq)
  val pft_s1_req = RegEnable(pft_s0_req, ft_query_req.fire)
  val pft_s2_req = RegEnable(pft_s1_req, pft_s1_valid)

  // --------- req s0: arb prefetch req & query filter table ---------
  for (i <- 0 until reqFilterEntryNum) {
    val entry = entries(i)
    val entry_pft_req = pft_arb.io.in(i)

    entry_pft_req.valid := valids(i) && entry.paddr_valid && !req_inflight(i)
    entry_pft_req.bits := entry.toPrefetchReq()

    when (entry_pft_req.fire) {
      req_inflight(i) := true.B
    }
  }

  pft_s0_valid := pft_arb.io.out.valid
  pft_arb.io.out.ready := ft_query_req.ready

  pft_s0_chosen_idx := pft_arb.io.chosen
  pft_s0_req := pft_arb.io.out.bits

  ft_query_req.valid := pft_s0_valid
  ft_query_req.bits  := 0.U.asTypeOf(new ftQueryReq)
  ft_query_req.bits.set_idx := get_filter_set(pft_s0_req.addr)

  // --------- req s1: recv filter table rsp ---------
  val ft_s1_rsp = ft_query_rsp

  // --------- req s2: chk hit & send req ---------
  val ft_s2_rsp = RegEnable(ft_s1_rsp, pft_s1_valid)

  val hit = Wire(Bool())
  val can_pft = Wire(Bool())

  if (useFilterTable) {
    val valid_vec = ft_s2_rsp.valid_vec
    val tag_vec   = ft_s2_rsp.tag_vec
    val hit_vec   = valid_vec.zip(tag_vec).map{
      case (v, t) =>
        v && t === get_filter_tag(pft_s2_req.addr)
    }

    val hit_idx = PriorityEncoder(hit_vec)
    val offset  = get_filter_offset(pft_s2_req.addr)

    val sat_vec = ft_s2_rsp.sat_vec
    hit := hit_vec.reduce(_ || _)
    can_pft := !hit || sat_vec(hit_idx)(offset) =/= 3.U

  } else {
    hit := false.B
    can_pft := true.B
  }

  // send req
  out.valid := pft_s2_valid && can_pft
  out.bits  := pft_s2_req

  when (out.fire || pft_s2_valid && !can_pft) {
    valids(pft_s2_chosen_idx) := false.B
  }

  when (pft_s2_valid) {
    req_inflight(pft_s2_chosen_idx) := false.B
  }

  // ----------------- Perf Counter -----------------
  XSPerfAccumulate("in_drop_by_hit", in.valid && entry_hit)
  XSPerfAccumulate("in_drop_by_full", in.valid && !entry_hit && !has_free_entry)

  XSPerfAccumulate("pf_req_drop_by_filter", pft_s2_valid && !can_pft)
  XSPerfAccumulate("filter_hit", pft_s2_valid && hit)
  XSPerfAccumulate("filter_miss", pft_s2_valid && !hit)

  val chosen_entry = entries(pft_s2_chosen_idx)
  XSPerfAccumulate("pf_req_fromCPU", out.fire && chosen_entry.pfSource === PfSource.NoWhere.id.U)
  XSPerfAccumulate("pf_req_fromBOP", out.fire && (chosen_entry.pfSource === PfSource.BOP.id.U || chosen_entry.pfSource === PfSource.PBOP.id.U))
  XSPerfAccumulate("pf_req_fromSMS", out.fire && chosen_entry.pfSource === PfSource.SMS.id.U)
  XSPerfAccumulate("pf_req_fromStream", out.fire && chosen_entry.pfSource === PfSource.Stream.id.U)
  XSPerfAccumulate("pf_req_fromStride", out.fire && chosen_entry.pfSource === PfSource.Stride.id.U)
  XSPerfAccumulate("pf_req_fromCDP", out.fire && chosen_entry.pfSource === PfSource.CDP.id.U)
}

class CDPPrefetcher(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    // detect
    val l2_detect_triggers = Flipped(Vec(banks, ValidIO(new CDPDetectTrigger)))

    // train
    val vpn_train     = Flipped(ValidIO(new PrefetchTrain))
    val filter_train  = Flipped(ValidIO(new PrefetchTrain))
    val pfStat        = Input(new PrefetchStat)

    // tlb?
    val tlb_req = new L2ToL1TlbIO

    // prefetch req?
    val pft_req = DecoupledIO(new PrefetchReq)
  })

  println(s"====== CDP Prefetcher Config (hart ${cacheParams.hartId}) ======")
  println(s"useFilteredDetect:  $useFilteredDetect")
  println(s"useFilterTable:     $useFilterTable")
  println(s"degree:             $degree")
  println(s"useDynamicDegree:   $useDynamicDegree")
  println(s"vpnTableTagBits:    $vpnTabTagBits")
  println(s"VpnSubEntryBits:    $entryBits")
  println(s"vpnResetPeriod:     $vpnResetPeriod")
  println(s"hotThreshold:       $hotThreshold")
  println(s"debug mode:         $debug")
  println(s"============================================")


  private val cstEnable = Constantin.createRecord("cdp_enable"+cacheParams.hartId.toString, initValue = 1)
  require(degree > 0, "CDP degree must be positive")

  val enable = io.enable & cstEnable.orR
  val (vpn_train, filter_train) = (io.vpn_train, io.filter_train)

  val l2_triggers = io.l2_detect_triggers

  val filter_table      = if (useFilterTable) Some(Module(new FilterTable)) else None
  val vpn_table         = Module(new VpnTable)
  val vt_train_pipe     = Module(new vtTrainPipeline)
  val ft_train_pipe     = if (useFilterTable) Some(Module(new ftTrainPipeline)) else None
  val detect_pipe_seq   = Seq.tabulate(detectPipeNum)(i => Module(new DetectPipeline(s"dp$i")))

  // TODO: ugly...
  // Detect Req
  val detect_trig_queue_seq = Seq.fill(banks)(Module(new MIMOQueue(new CDPDetectEntry, 8, 2, 1)))
  val detect_trig_arb = Module(new RRArbiterInit(new CDPDetectEntry, banks))

  // detect_trigs <> detect_trig_queue_seq <> detect_trig_arb <> detect_pipe_seq
  for (i <- 0 until banks) {
    val detect_trig_queue = detect_trig_queue_seq(i)

    val detect_trig = l2_triggers(i)

    /**
      * Check : Detection Condition
      * Hit Trigger:
          a) Hit a CDP prefetched block, pfDepth == 1 or pfDepthMax
          b) Hit a SMS/BOP prefetched block

      * Refill Trigger:
          a) Refill a true demanded block
          b) Refill a CDP required block
          c) Refill other prefetcher's block.
    */
    val detect_trig_fromCDP = detect_trig.bits.pfSource === PfSource.CDP.id.U
    val detect_trig_fromSMS = detect_trig.bits.pfSource === PfSource.SMS.id.U
    val detect_trig_fromBOP = detect_trig.bits.pfSource === PfSource.BOP.id.U || detect_trig.bits.pfSource === PfSource.PBOP.id.U
    val detect_trig_fromStream  = detect_trig.bits.pfSource === PfSource.Stream.id.U
    val detect_trig_fromStride  = detect_trig.bits.pfSource === PfSource.Stride.id.U
    val detect_trig_fromCPU     = detect_trig.bits.pfSource === PfSource.NoWhere.id.U

    // TODO: move depth check to MainPipe?
    val hit_trigger       = detect_trig.bits.is_hit  &&
      (
        if (useFilteredDetect) {
          detect_trig_fromCDP &&
            (detect_trig.bits.pfDepth === 1.U || detect_trig.bits.pfDepth === pfDepthMax.U) ||
            detect_trig_fromSMS || detect_trig_fromBOP
        }
        else {
          detect_trig_fromCDP &&
            (detect_trig.bits.pfDepth === 1.U || detect_trig.bits.pfDepth === pfDepthMax.U)
        }
      )
    val refill_trigger    = !detect_trig.bits.is_hit && detect_trig.bits.pfDepth < depthThreshold.U &&
      (
        if (useFilteredDetect) {
          detect_trig_fromCPU || detect_trig_fromCDP || detect_trig_fromStride || detect_trig_fromStream
        }
        else {
          true.B
        }
      )
    
    detect_trig_queue.io.flush := reset.asBool

    detect_trig_queue.io.enq(0).valid := detect_trig.valid && (hit_trigger || refill_trigger) && enable
    detect_trig_queue.io.enq(0).bits.half_cacheblock  := detect_trig.bits.cacheblock(blockBits / 2 - 1, 0)
    detect_trig_queue.io.enq(0).bits.pfDepth  := detect_trig.bits.pfDepth
    detect_trig_queue.io.enq(0).bits.pfSource := detect_trig.bits.pfSource
    detect_trig_queue.io.enq(0).bits.is_hit   := detect_trig.bits.is_hit

    detect_trig_queue.io.enq(1).valid := detect_trig.valid && (hit_trigger || refill_trigger) && enable
    detect_trig_queue.io.enq(1).bits.half_cacheblock  := detect_trig.bits.cacheblock(blockBits - 1, blockBits / 2)
    detect_trig_queue.io.enq(1).bits.pfDepth  := detect_trig.bits.pfDepth
    detect_trig_queue.io.enq(1).bits.pfSource := detect_trig.bits.pfSource
    detect_trig_queue.io.enq(1).bits.is_hit   := detect_trig.bits.is_hit

    detect_trig_arb.io.in(i) <> detect_trig_queue_seq(i).io.deq(0)

    XSPerfAccumulate(s"detect_trig_num_bank$i", detect_trig.valid && enable)
    XSPerfAccumulate(s"detect_trig_drop0_bank$i", detect_trig_queue.io.enq(0).valid && !detect_trig_queue.io.enq(0).ready && enable)
    XSPerfAccumulate(s"detect_trig_drop1_bank$i", detect_trig_queue.io.enq(1).valid && !detect_trig_queue.io.enq(1).ready && enable)

    XSPerfAccumulate(s"detect_trig_hit_fromCDP_bank$i", detect_trig.valid && detect_trig.bits.is_hit && detect_trig_fromCDP && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromSMS_bank$i", detect_trig.valid && detect_trig.bits.is_hit && detect_trig_fromSMS && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromBOP_bank$i", detect_trig.valid && detect_trig.bits.is_hit && detect_trig_fromBOP && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromStream_bank$i", detect_trig.valid && detect_trig.bits.is_hit && detect_trig_fromStream && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromStride_bank$i", detect_trig.valid && detect_trig.bits.is_hit && detect_trig_fromStride && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromCPU_bank$i", detect_trig.valid && detect_trig.bits.is_hit && detect_trig_fromCPU && enable)

    XSPerfAccumulate(s"detect_trig_refill_fromCDP_bank$i", detect_trig.valid && !detect_trig.bits.is_hit && detect_trig_fromCDP && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromSMS_bank$i", detect_trig.valid && !detect_trig.bits.is_hit && detect_trig_fromSMS && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromBOP_bank$i", detect_trig.valid && !detect_trig.bits.is_hit && detect_trig_fromBOP && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromStream_bank$i", detect_trig.valid && !detect_trig.bits.is_hit && detect_trig_fromStream && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromStride_bank$i", detect_trig.valid && !detect_trig.bits.is_hit && detect_trig_fromStride && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromCPU_bank$i", detect_trig.valid && !detect_trig.bits.is_hit && detect_trig_fromCPU && enable)
  }

  for (i <- 0 until detectPipeNum) {
    val detect_pipe = detect_pipe_seq(i)

    detect_pipe.io.detect_req.valid := detect_trig_arb.io.out.valid
    detect_pipe.io.detect_req.bits.data     := detect_trig_arb.io.out.bits.half_cacheblock((i + 1) * 64 - 1, i * 64)   // 8 Byte ==> 64 bit
    detect_pipe.io.detect_req.bits.pfDepth  := detect_trig_arb.io.out.bits.pfDepth
    detect_pipe.io.detect_req.bits.pfSource := detect_trig_arb.io.out.bits.pfSource
    detect_pipe.io.detect_req.bits.is_hit   := detect_trig_arb.io.out.bits.is_hit

    detect_pipe.io.addr_state <> vt_train_pipe.io.addr_state
  }
  detect_trig_arb.io.out.ready := true.B

  // VpnTable & FilterTable Train Trigger
  val vpn_train_reqBuf  = Module(new Queue(new PrefetchTrain, 8))
  vpn_train_reqBuf.io.enq.valid := vpn_train.valid && enable
  vpn_train_reqBuf.io.enq.bits  := vpn_train.bits
  vt_train_pipe.io.train_trigger <> vpn_train_reqBuf.io.deq
  XSPerfAccumulate("vpn_train_drop", vpn_train_reqBuf.io.enq.valid && !vpn_train_reqBuf.io.enq.ready)
  XSPerfAccumulate("vpn_train_accept", vpn_train_reqBuf.io.enq.fire)

  if (useFilterTable) {
    val filter_train_reqBuf = Module(new Queue(new PrefetchTrain, 8))
    filter_train_reqBuf.io.enq.valid := filter_train.valid && enable
    filter_train_reqBuf.io.enq.bits  := filter_train.bits
    ft_train_pipe.get.io.train_trigger <> filter_train_reqBuf.io.deq
    XSPerfAccumulate("filter_train_drop", filter_train_reqBuf.io.enq.valid && !filter_train_reqBuf.io.enq.ready)
    XSPerfAccumulate("filter_train_accept", filter_train_reqBuf.io.enq.fire)
  }

  val vpn_tab_query_req_seq = detect_pipe_seq.map(_.io.vt_query_req) ++ Seq(vt_train_pipe.io.query_req)
  val vpn_tab_query_rsp_seq = detect_pipe_seq.map(_.io.vt_query_rsp) ++ Seq(vt_train_pipe.io.query_rsp)
  require(vpn_tab_query_req_seq.size == vpn_table.io.query_req.size)
  vpn_table.io.query_req.zip(vpn_tab_query_req_seq).foreach{
    case (tab_req, pipe_req) =>
      tab_req.valid := pipe_req.valid
      tab_req.bits := pipe_req.bits
  }
  vpn_table.io.query_rsp.zip(vpn_tab_query_rsp_seq).foreach{
    case (tab_rsp, pipe_rsp) =>
      pipe_rsp.valid := tab_rsp.valid
      pipe_rsp.bits := tab_rsp.bits
  }
  vpn_table.io.train_req  <> vt_train_pipe.io.train_req

  val issueDegree =
    if (useDynamicDegree) {
      /**
        * EWMA Accuracy Control
        * Low accuracy will reduce the degree to avoid useless prefetches
        */

      val cdpPfSent = io.pfStat.pfSentVec(PfSource.CDP.id)
      val cdpPfHit = io.pfStat.pfHitVec(PfSource.CDP.id)

      val cdpPfSentPrev = RegInit(0.U(cdpPfSent.getWidth.W))
      val cdpPfHitPrev = RegInit(0.U(cdpPfHit.getWidth.W))
      val sentDelta = cdpPfSent - cdpPfSentPrev
      val hitDelta = cdpPfHit - cdpPfHitPrev
      cdpPfSentPrev := cdpPfSent
      cdpPfHitPrev := cdpPfHit

      val degreeEwmaShift = 9
      val sentEwma = RegInit(0.U(cdpPfSent.getWidth.W))
      val hitEwma = RegInit(0.U(cdpPfHit.getWidth.W))
      sentEwma := sentEwma - (sentEwma >> degreeEwmaShift) + sentDelta
      hitEwma := hitEwma - (hitEwma >> degreeEwmaShift) + hitDelta

      val sentLt100 = cdpPfSent < 100.U(cdpPfSent.getWidth.W)
      val accuracyGt5Pct = hitEwma * 100.U(7.W) > sentEwma * 5.U(3.W)
      Mux(sentLt100 || accuracyGt5Pct, degree.U, 1.U)
    } else {
      degree.U
    }

  // Degreed Buffer
  val degree_buf_seq = Seq.fill(detectPipeNum)(Module(new MIMOQueue(new CDPPrefetchReq, 8, degree, 1)))
  val degree_buf_arb = Module(new RRArbiterInit(new CDPPrefetchReq, detectPipeNum))
  for (i <- 0 until detectPipeNum) {
    val buf = degree_buf_seq(i)
    val req = detect_pipe_seq(i).io.pft_req
    buf.io.flush := reset.asBool
    for (j <- 0 until degree) {
      buf.io.enq(j).valid := req.valid && j.U < issueDegree
      buf.io.enq(j).bits  := req.bits

      if (j > 0) {
        buf.io.enq(j).bits.pfVAddr := req.bits.pfVAddr + (j * blockBytes).U
      }
    }

    degree_buf_arb.io.in(i) <> buf.io.deq(0)

    XSPerfAccumulate(s"drop_by_acc_pipe$i", Mux(req.valid, degree.U - issueDegree, 0.U))
  }

  // SendUnit
  val send_unit = Module(new SentUnit)
  send_unit.io.tlb_req <> io.tlb_req
  if (useFilterTable) {
    val ft_query_arb = Module(new RRArbiter(new ftQueryReq, 2))
    ft_query_arb.io.in(0) <> ft_train_pipe.get.io.query_req
    ft_query_arb.io.in(1) <> send_unit.io.ft_query_req
    filter_table.get.io.query_req <> ft_query_arb.io.out

    filter_table.get.io.train_req <> ft_train_pipe.get.io.train_req

    val ft_query_chosen = RegEnable(ft_query_arb.io.chosen, ft_query_arb.io.out.fire)
    ft_train_pipe.get.io.query_rsp := Mux(
      filter_table.get.io.query_rsp.valid && ft_query_chosen === 0.U,
      filter_table.get.io.query_rsp.bits,
      0.U.asTypeOf(new ftQueryRsp)
    )
    send_unit.io.ft_query_rsp := Mux(
      filter_table.get.io.query_rsp.valid && ft_query_chosen === 1.U,
      filter_table.get.io.query_rsp.bits,
      0.U.asTypeOf(new ftQueryRsp)
    )
  } else {
    send_unit.io.ft_query_req.ready := true.B
    send_unit.io.ft_query_rsp := 0.U.asTypeOf(new ftQueryRsp)
  }
  send_unit.io.in   <> degree_buf_arb.io.out
  send_unit.io.out  <> io.pft_req
}
