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

  reqFilterEntryNum: Int = 16,   // how many entries in the SentUnit?
  retryInterval: Int = 10,

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
  def getFoldedHash(originVal: UInt, resultBitWidth: Int): UInt = {    // fold $originVal length value into $resultBitWidth
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

  def getVpn0(addr: UInt): UInt = addr(20, 12)
  def getOffset(addr: UInt): UInt = addr(11, 0)

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
  // Keep the main-entry hash independent of subIdx so the four sub entries belonging to
  // the same main entry still share one tag/way and can use allocSub normally.

  def getVpnAddr(addr: UInt) = addr(addr.getWidth - 1, entryBits)  // TODO: parameterize

  def getVpnTableOriginTag(addr: UInt) = {
    val vpnAddr = getVpnAddr(addr)
    vpnAddr(vpnAddr.getWidth - 1, subEntryBits + mainEntryBits) // TODO: parameterize
  }

  def getVpnTableOriginMainIdx(addr: UInt) = {
    val vpnAddr = getVpnAddr(addr)
    vpnAddr(subEntryBits + mainEntryBits - 1, subEntryBits)
  }

  def getVpnTableOriginSubIdx(addr: UInt) = {
    val vpnAddr = getVpnAddr(addr)
    vpnAddr(subEntryBits - 1, 0)
  }

  def getMainIdx(addr: UInt) = {
    val originTag     = getVpnTableOriginTag(addr)
    val originMainIdx = getVpnTableOriginMainIdx(addr)

    if (useVpnTableHashIndex) {
      originMainIdx ^ getFoldedHash(originTag, mainEntryBits)
    } else {
      originMainIdx
    }
  }

  def getSubIdx(addr: UInt) = {
    val originTag     = getVpnTableOriginTag(addr)
    val originMainIdx = getVpnTableOriginMainIdx(addr)
    val originSubIdx  = getVpnTableOriginSubIdx(addr)

    if (useVpnTableHashIndex) {
      originSubIdx ^ getFoldedHash(Cat(originTag, originMainIdx), subEntryBits)
    } else {
      originSubIdx
    }
  }

  def getVpnTableTag(addr: UInt) = {
    getFoldedHash(getVpnTableOriginTag(addr), vpnTabTagBits)
  }

  // Filter Table Params
  val filterTableSetNum = cdpParams.filterTableSetNum
  val filterTableWayNum = cdpParams.filterTableWayNum
  val filterEntryBlks = cdpParams.filterEntryBlks
  val filterEntryGranularity = cdpParams.filterEntryGranularity

  val filterTableOffsetBits = log2Ceil(filterEntryBlks)
  val filterTableSetBits    = log2Ceil(filterTableSetNum)
  val filterTableTagBits    = fullAddressBits - log2Ceil(filterEntryGranularity) - filterTableSetBits - filterTableOffsetBits

  def getFilterAddr(addr: UInt) = {
    addr(addr.getWidth - 1, log2Ceil(filterEntryGranularity))
  }

  def getFilterOffset(addr: UInt) = {
    val filterAddr = getFilterAddr(addr)
    filterAddr(filterTableOffsetBits - 1, 0)
  }

  def getFilterSet(addr: UInt) = {
    val filterAddr = getFilterAddr(addr)
    filterAddr(filterTableSetBits + filterTableOffsetBits - 1, filterTableOffsetBits)
  }

  def getFilterTag(addr: UInt) = {
    val filterAddr = getFilterAddr(addr)
    filterAddr(filterAddr.getWidth - 1, filterTableSetBits + filterTableOffsetBits)
  }

  // SentUnit Params
  val reqFilterEntryNum = cdpParams.reqFilterEntryNum
  val reqFilterVTagBits = fullVAddrBits - log2Ceil(blockBytes)
  val reqFilterPTagBits = fullAddressBits - log2Ceil(blockBytes)
  val retryInterval = cdpParams.retryInterval
  require(retryInterval <= 15, "retryInterval should be less than 15, otherwise the retryTimer will overflow")
}

abstract class CDPBundle(implicit val p: Parameters) extends Bundle with HasCDPParams
abstract class CDPModule(implicit val p: Parameters) extends Module with HasCDPParams

class CDPDetectTask(val dataBits: Int)(implicit p: Parameters) extends CDPBundle {
  /**
   * dataBits specifies the data width used at different pipeline stages:
   * - blockBits: Full cacheline width. Used to receive detect triggers from MainPipe.
   * - blockBits / 2: Half cacheline width. Each cacheline is split into two halves and
   *        buffered in DetectTriggerQueue before entering DetectPipe.
   * - 64:  8-byte width. DetectPipe processes one 8-byte chunk per cycle.
   */
  require(dataBits == blockBits || dataBits == blockBits / 2 || dataBits == 64)

  val data = UInt(dataBits.W)
  val pfDepth = UInt(pfDepthBits.W)
  val pfSource = UInt(PfSource.pfSourceBits.W)
  val isHit = Bool() // is this trigger from req hitting l2?
}

class VpnTableMetaInfo(implicit p: Parameters) extends CDPBundle {
  val hot = Bool()    // indicate whether this page is frequently visited in the past period
  val prevRefCnt = UInt(counterBits.W)
  val refCnt = UInt(counterBits.W)
}

class VtQueryReq(implicit p: Parameters) extends CDPBundle {
  val mainIdx = UInt(mainEntryBits.W)
  val subIdx = UInt(subEntryBits.W)
}

class VtQueryRsp(implicit p: Parameters) extends CDPBundle {
  val tagVec = Vec(vpnTableWayNum, UInt(vpnTabTagBits.W))
  val metaVec = Vec(vpnTableWayNum, new VpnTableMetaInfo)

  val subValidVec = Vec(vpnTableWayNum, Bool())
  val mainValidVec = Vec(vpnTableWayNum, Bool())
}

class VtTrainReq(implicit p: Parameters) extends CDPBundle {
  val allocMain  = Bool()    // allocate a new MainEntry (clear all the SubEntries)
  val allocSub   = Bool()    // allocate a new SubEntry (refCnt = 1)
  val targetWay  = UInt(log2Ceil(vpnTableWayNum).W)

  val mainIdx    = UInt(mainEntryBits.W)
  val subIdx     = UInt(subEntryBits.W)

  val tag         = UInt(vpnTabTagBits.W)

  val isHitCdp  = Bool()  // train trigger from hitting a CDP prefetched block?
}

class VpnTable(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val queryReq = Flipped(Vec(detectPipeNum + 1, ValidIO(new VtQueryReq)))   // +1 for train pipe
    val queryRsp = Vec(detectPipeNum + 1, Output(new VtQueryRsp))
    val trainReq = Flipped(ValidIO(new VtTrainReq))
  })

  val (queryReq, queryRsp) = (io.queryReq, io.queryRsp)
  val trainReq = io.trainReq

  val refreshCnt = RegInit(0.U(32.W))
  val isRefresh = Wire(Bool())

  // Valid Array (for subEntry)
  val validArray = RegInit(VecInit(Seq.fill(vpnTableSetNum)(
    VecInit(Seq.fill(vpnTableWayNum)(
      VecInit(Seq.fill(vpnTableSubEntryNum)(false.B))
    ))
  )))

  // Tag Array
  val tagArray = Reg(
    Vec(vpnTableSetNum, Vec(vpnTableWayNum, UInt(vpnTabTagBits.W)))
  )

  // Meta Info Array
  val metaArray = Reg(
    Vec(vpnTableSetNum, Vec(vpnTableWayNum, Vec(vpnTableSubEntryNum, new VpnTableMetaInfo)))
  )

  // Query Logic
  for (i <- 0 until detectPipeNum + 1) {
    val (req, rsp) = (queryReq(i), queryRsp(i))

    val (mainIdx, subIdx) = (req.bits.mainIdx, req.bits.subIdx)

    for (j <- 0 until vpnTableWayNum) {
      rsp.tagVec(j) := tagArray(mainIdx)(j)
      rsp.metaVec(j) := metaArray(mainIdx)(j)(subIdx)
      rsp.subValidVec(j) := validArray(mainIdx)(j)(subIdx)
      rsp.mainValidVec(j) := validArray(mainIdx)(j).reduce(_ || _)
    }
  }

  // Train Logic
  when (trainReq.valid && !isRefresh) {
    val (mainIdx, subIdx) = (trainReq.bits.mainIdx, trainReq.bits.subIdx)
    val targetWay = trainReq.bits.targetWay
    val (allocMain, allocSub) = (trainReq.bits.allocMain, trainReq.bits.allocSub)
    val noAlloc = !allocMain && !allocSub

    assert(!(allocMain && allocSub), "TrainReq can't allocate both main entry and sub entry!")

    val incrNum  = Mux(trainReq.bits.isHitCdp, 4.U, 1.U)

    when (allocMain) {
      // use targetWay for replacement
      val replaceWay = targetWay

      // Update Tag
      tagArray(mainIdx)(replaceWay)  := trainReq.bits.tag

      // Update Meta & Valid
      for (i <- 0 until vpnTableSubEntryNum) {
        when (i.U === subIdx) {
          validArray(mainIdx)(replaceWay)(i) := true.B
          metaArray(mainIdx)(replaceWay)(i).refCnt := incrNum
          metaArray(mainIdx)(replaceWay)(i).hot    := false.B
          metaArray(mainIdx)(replaceWay)(i).prevRefCnt := 0.U
        }.otherwise {
          validArray(mainIdx)(replaceWay)(i) := false.B
        }
      }
    }

    when (allocSub) {
      // only update the meta of the target sub entry
      validArray(mainIdx)(targetWay)(subIdx) := true.B
      
      metaArray(mainIdx)(targetWay)(subIdx).refCnt := incrNum
      metaArray(mainIdx)(targetWay)(subIdx).hot := false.B
      metaArray(mainIdx)(targetWay)(subIdx).prevRefCnt := 0.U
    }

    when (noAlloc) {
      // only update the refCnt of the target sub entry
      metaArray(mainIdx)(targetWay)(subIdx).refCnt := metaArray(mainIdx)(targetWay)(subIdx).refCnt + incrNum
    }
  }

  // Refresh Logic
  when (refreshCnt < vpnResetPeriod.U && trainReq.valid){
    refreshCnt := refreshCnt + 1.U
  }

  isRefresh := refreshCnt >= vpnResetPeriod.U
  when (isRefresh) {
    refreshCnt := 0.U

    // go through every sub entry
    for (i <- 0 until vpnTableSetNum) {
      for (j <- 0 until vpnTableWayNum) {
        for (k <- 0 until vpnTableSubEntryNum) {
          val entry = metaArray(i)(j)(k)

          /**
           * Update Entry:
           *  prevRefCnt -> 0.8 * refCnt + 0.2 * prevRefCnt
           *  refCnt  -> 0
           *  hot -> prevRefCnt > hotThreshold ? 1 : 0
           * */
          // TODO: For better timing, maybe we should pipeline this.
          val nextPrevRefCnt = ((entry.refCnt * 13.U) + (entry.prevRefCnt * 3.U)) >> 4.U

          entry.refCnt      := 0.U
          entry.prevRefCnt  := nextPrevRefCnt
          entry.hot         := Mux(nextPrevRefCnt > hotThreshold.U, true.B, false.B)
        }
      }
    }
  }

  // ------------------ Performance Counter ------------------
  XSPerfAccumulate("vt_refresh", isRefresh)
  XSPerfAccumulate("vt_alloc_main", trainReq.valid && trainReq.bits.allocMain)
  XSPerfAccumulate("vt_alloc_sub", trainReq.valid && trainReq.bits.allocSub)
  XSPerfAccumulate("vt_no_alloc", trainReq.valid && !trainReq.bits.allocMain && !trainReq.bits.allocSub)

  // VpnTable train index distribution.
  for (i <- 0 until vpnTableSetNum) {
    XSPerfAccumulate(s"vt_train_main_idx_$i", trainReq.valid && !isRefresh && trainReq.bits.mainIdx === i.U)
    XSPerfAccumulate(s"vt_alloc_main_idx_$i", trainReq.valid && !isRefresh && trainReq.bits.allocMain && trainReq.bits.mainIdx === i.U)
    XSPerfAccumulate(s"vt_alloc_sub_main_idx_$i", trainReq.valid && !isRefresh && trainReq.bits.allocSub && trainReq.bits.mainIdx === i.U)
    XSPerfAccumulate(
      s"vt_no_alloc_main_idx_$i",
      trainReq.valid && !isRefresh && !trainReq.bits.allocMain && !trainReq.bits.allocSub && trainReq.bits.mainIdx === i.U
    )
  }
  for (i <- 0 until vpnTableSubEntryNum) {
    XSPerfAccumulate(s"vt_train_sub_idx_$i", trainReq.valid && !isRefresh && trainReq.bits.subIdx === i.U)
    XSPerfAccumulate(s"vt_alloc_sub_sub_idx_$i", trainReq.valid && !isRefresh && trainReq.bits.allocSub && trainReq.bits.subIdx === i.U)
    XSPerfAccumulate(
      s"vt_no_alloc_sub_idx_$i",
      trainReq.valid && !isRefresh && !trainReq.bits.allocMain && !trainReq.bits.allocSub && trainReq.bits.subIdx === i.U
    )
  }
  for (i <- 0 until vpnTableSetNum) {
    for (j <- 0 until vpnTableSubEntryNum) {
      XSPerfAccumulate(
        s"vt_train_idx_${i}_$j",
        trainReq.valid && !isRefresh && trainReq.bits.mainIdx === i.U && trainReq.bits.subIdx === j.U
      )
    }
  }

  // train trig data
  XSPerfAccumulate("in_train_trig_used", trainReq.valid && !isRefresh)
  XSPerfAccumulate("in_train_trig_drop_by_refresh", trainReq.valid && isRefresh)
}

class FtQueryReq(implicit p: Parameters) extends CDPBundle {
  val setIdx = UInt(filterTableSetBits.W)
}

class FtQueryRsp(implicit p: Parameters) extends CDPBundle {
  val validVec = Vec(filterTableWayNum, Bool())
  val tagVec   = Vec(filterTableWayNum, UInt(filterTableTagBits.W))
  val satVec   = Vec(filterTableWayNum, Vec(filterEntryBlks, UInt(2.W)))
}

class FtTrainReq(implicit p: Parameters) extends CDPBundle {
  val set     = UInt(filterTableSetBits.W)
  val way     = UInt(log2Ceil(filterTableWayNum).W)
  val tag     = UInt(filterTableTagBits.W)
  val sat     = Vec(filterEntryBlks, UInt(2.W))
}

class FilterTable(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val queryReq = Flipped(DecoupledIO(new FtQueryReq))
    val queryRsp = ValidIO(new FtQueryRsp)
    val trainReq = Flipped(DecoupledIO(new FtTrainReq))
  })

  val (queryReq, queryRsp) = (io.queryReq, io.queryRsp)
  val trainReq = io.trainReq

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
  val queryReady = metaArray.io.r.req.ready && satBanks.map(_.io.r.req.ready).reduce(_ && _) && !trainReq.valid
  queryReq.ready := queryReady
  val queryFire = queryReq.valid && queryReady

  metaArray.io.r.req.valid := queryFire
  metaArray.io.r.req.bits.setIdx := queryReq.bits.setIdx
  satBanks.foreach { bank =>
    bank.io.r.req.valid := queryFire
    bank.io.r.req.bits.setIdx := queryReq.bits.setIdx
  }

  queryRsp.valid := RegNext(queryFire, false.B)
  for (i <- 0 until filterTableWayNum) {
    queryRsp.bits.validVec(i) := metaArray.io.r.resp.data(i).valid
    queryRsp.bits.tagVec(i)   := metaArray.io.r.resp.data(i).tag
    for (j <- 0 until filterEntryBlks) {
      queryRsp.bits.satVec(i)(j) := satBanks(j / SatBankBlks).io.r.resp.data(i)(j % SatBankBlks)
    }
  }

  // train
  val trainReady = metaArray.io.w.req.ready && satBanks.map(_.io.w.req.ready).reduce(_ && _)
  trainReq.ready := trainReady
  val trainFire = trainReq.valid && trainReady
  val trainWayOH = UIntToOH(trainReq.bits.way, filterTableWayNum)

  val metaWData = WireInit(0.U.asTypeOf(ftMetaEntry()))
  metaWData.valid := true.B
  metaWData.tag   := trainReq.bits.tag
  metaArray.io.w.req.valid := trainFire
  metaArray.io.w.req.bits.apply(metaWData, trainReq.bits.set, trainWayOH)

  for (bankIdx <- 0 until SatBankNum) {
    val satWData = Wire(Vec(SatBankBlks, UInt(2.W)))
    for (blkIdx <- 0 until SatBankBlks) {
      satWData(blkIdx) := trainReq.bits.sat(bankIdx * SatBankBlks + blkIdx)
    }
    satBanks(bankIdx).io.w.req.valid := trainFire
    satBanks(bankIdx).io.w.req.bits.apply(satWData, trainReq.bits.set, trainWayOH)
  }
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
  val isHit    = Bool()    // Req triggered by hitting l2
}

class FtTrainPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val trainTrigger = Flipped(DecoupledIO(new PrefetchTrain))

    // to FilterTable
    val queryReq = DecoupledIO(new FtQueryReq)
    val queryRsp = Input(new FtQueryRsp)
    val trainReq = DecoupledIO(new FtTrainReq)
  })

  val (trainTrigger, queryReq, queryRsp, trainReq) =
    (io.trainTrigger, io.queryReq, io.queryRsp, io.trainReq)

  val replacer = ReplacementPolicy.fromString(replType, filterTableWayNum, filterTableSetNum)

  class FtTrainTask extends Bundle {
    val setIdx = UInt(filterTableSetBits.W)
    val offset = UInt(filterTableOffsetBits.W)
    val tag = UInt(filterTableTagBits.W)
    val isUsed = Bool()
    val isEvict = Bool()
  }

  val s0_valid = Wire(Bool())
  val s0_task = Wire(new FtTrainTask)

  val s1_valid = RegInit(false.B)
  val s1_task = Reg(new FtTrainTask)

  val s2_valid = RegInit(false.B)
  val s2_task = Reg(new FtTrainTask)

  val s3_valid = Wire(Bool())
  val s3_task = Wire(new FtTrainTask)

  val sameVec = Wire(Vec(3, Bool()))
  val sameAddr = sameVec.reduce(_ || _)
  val s1_ready = !s1_valid || queryReq.ready && trainReq.ready    // s1 ready to recv req from s0

  // ----------- s0: accept train trigger -----------
  s0_valid := trainTrigger.valid && !sameAddr
  trainTrigger.ready := !reset.asBool && s1_ready && !sameAddr

  val trainPaddr = Mux(
    trainTrigger.bits.cdp_filter_train_evict.get,
    trainTrigger.bits.evict_addr,
    trainTrigger.bits.addr
  )

  s0_task.setIdx := getFilterSet(trainPaddr)
  s0_task.offset  := getFilterOffset(trainPaddr)
  s0_task.tag     := getFilterTag(trainPaddr)
  s0_task.isUsed := trainTrigger.bits.cdp_filter_train_hit.get
  s0_task.isEvict := trainTrigger.bits.cdp_filter_train_evict.get

  // ----------- s1: query FilterTable -----------
  when (s1_ready) {
    s1_valid := s0_valid
    s1_task := s0_task
  }

  queryReq.valid := s1_valid
  queryReq.bits.setIdx := s1_task.setIdx

  sameVec(0) := s1_valid && s1_task.setIdx === s0_task.setIdx && s1_task.tag === s0_task.tag

  // ----------- s2: receive FilterTable and latch -----------
  when (queryReq.fire) {
    s2_valid := s1_valid
    s2_task := s1_task
  }.otherwise {
    s2_valid := false.B
  }

  val ft_s2_rsp = queryRsp

  sameVec(1) := s2_valid && s2_task.setIdx === s0_task.setIdx && s2_task.tag === s0_task.tag

  // ----------- s3: chk hit and update -----------
  s3_valid := RegNext(s2_valid, false.B)
  s3_task := RegEnable(s2_task, s2_valid)

  val ft_s3_rsp = RegNext(ft_s2_rsp)

  val s3_hit_vec = ft_s3_rsp.tagVec.zip(ft_s3_rsp.validVec).map {
    case (tag, valid) => tag === s3_task.tag && valid
  }

  val s3_hit = s3_hit_vec.reduce(_ || _)
  val s3_hit_way = PriorityEncoder(s3_hit_vec)
  assert(PopCount(s3_hit_vec) < 2.U || !s3_valid, "FilterTable entry multiple hit!")

  val s3_repl_way = replacer.way(s3_task.setIdx)
  val s3_target_way = Mux(s3_hit, s3_hit_way, s3_repl_way)
  val s3_old_sat_vec = ft_s3_rsp.satVec(s3_target_way)
  val s3_need_update = s3_hit || s3_task.isEvict

  val s3_next_sat_vec = Wire(Vec(filterEntryBlks, UInt(2.W)))
  for (i <- 0 until filterEntryBlks) {
    val oldSat = s3_old_sat_vec(i)
    when (!s3_hit) {
      s3_next_sat_vec(i) := Mux(i.U === s3_task.offset, Mux(s3_task.isUsed, 1.U, 3.U), 2.U)  // if not hit, set the accessed block to 1 or 3, others to 2
    }.otherwise {
      // if hit, update the accessed block's sat counter based on whether it is used or not
      s3_next_sat_vec(i) := Mux(i.U === s3_task.offset,
        Mux(s3_task.isUsed, Mux(oldSat === 0.U, 0.U, oldSat - 1.U), Mux(oldSat === 3.U, 3.U, oldSat + 1.U)),
        oldSat
      )
    }
  }

  val s3_update_info = WireInit(0.U.asTypeOf(new FtTrainReq))
  s3_update_info.set := s3_task.setIdx
  s3_update_info.way := s3_target_way
  s3_update_info.tag := s3_task.tag
  s3_update_info.sat := s3_next_sat_vec

  sameVec(2) := s3_valid && s3_task.setIdx === s0_task.setIdx && s3_task.tag === s0_task.tag

  trainReq.valid := s3_valid && s3_need_update
  trainReq.bits  := s3_update_info

  assert(!trainReq.valid || trainReq.ready, "FilterTable train req should be ready when valid!")

  when (trainReq.fire) {
    replacer.access(s3_update_info.set, s3_update_info.way)
  }
}

class VtTrainPipeline(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val trainTrigger = Flipped(DecoupledIO(new PrefetchTrain))

    // to VPN Table
    val queryReq = ValidIO(new VtQueryReq)
    val queryRsp = Input(new VtQueryRsp)
    val trainReq = ValidIO(new VtTrainReq)

    // to DetectPipe
    val addrState = new Bundle {
      val isSv39 = Output(Bool())
      val isSv48 = Output(Bool())
      val isSv57 = Output(Bool())
    }
  })

  val (trainTrigger, queryReq, queryRsp, trainReq) =
    (io.trainTrigger, io.queryReq, io.queryRsp, io.trainReq)

  val replacer = ReplacementPolicy.fromString(replType, vpnTableWayNum, vpnTableSetNum)

  val stage_valid = Wire(Vec(5, Bool()))
  val sameVec = Wire(Vec(4, Bool()))

  val sameAddr = sameVec.reduce(_ || _)

  // Record Max Addr Range
  val s_sv39::s_sv48::s_sv57::Nil = Enum(3)
  val addrState = RegInit(s_sv39)

  // ----------- s0: accept train trigger -----------
  stage_valid(0) := trainTrigger.valid && !sameAddr
  trainTrigger.ready := reset.asBool || !sameAddr

  val trainVaddr = trainTrigger.bits.vaddr.getOrElse(0.U) << log2Ceil(blockBytes)
  val s0_main_idx = getMainIdx(trainVaddr)
  val s0_sub_idx  = getSubIdx(trainVaddr)
  val s0_tag      = getVpnTableTag(trainVaddr)
  val s0_is_hit_cdp = trainTrigger.bits.hit && trainTrigger.bits.pfsource === PfSource.CDP.id.U

  // addrState update
  val maxAddrWidth = if (trainTrigger.bits.vaddr.isDefined) {
    trainTrigger.bits.vaddr.getOrElse(0.U).getWidth + log2Ceil(blockBytes)
  } else {
    39  // default to sv39 if vaddr is not defined
  }

  val nextState = WireDefault(addrState)
  switch (addrState) {
    is (s_sv39) {
      if (39 < maxAddrWidth && maxAddrWidth <= 48) {
        when (trainTrigger.fire) {
          nextState := Mux(trainVaddr(trainVaddr.getWidth - 1, 39) =/= 0.U, s_sv48, s_sv39)
        }
      }
      if (48 < maxAddrWidth && maxAddrWidth <= 57) {
        when (trainTrigger.fire) {
          nextState := Mux(trainVaddr(trainVaddr.getWidth - 1, 48) =/= 0.U,
            s_sv57, 
            Mux(trainVaddr(trainVaddr.getWidth - 1, 39) =/= 0.U, s_sv48, s_sv39)
          )
        }
      }
    }
    is (s_sv48) {
      if (48 < maxAddrWidth && maxAddrWidth <= 57) {
        when (trainTrigger.fire) {
          nextState := Mux(trainVaddr(trainVaddr.getWidth - 1, 48) =/= 0.U, s_sv57, s_sv48)
        }
      }
    }
  }

  addrState := nextState
  io.addrState.isSv39 := addrState === s_sv39
  io.addrState.isSv48 := addrState === s_sv48
  io.addrState.isSv57 := addrState === s_sv57

  // ----------- s1: query VpnTable -----------
  stage_valid(1) := RegNext(stage_valid(0))
  val s1_main_idx = RegNext(s0_main_idx)
  val s1_sub_idx  = RegNext(s0_sub_idx)
  val s1_tag      = RegNext(s0_tag)
  val s1_is_hit_cdp = RegNext(s0_is_hit_cdp)
  val s1_tab_rsp = queryRsp

  queryReq.valid := stage_valid(1)
  queryReq.bits.mainIdx := s1_main_idx
  queryReq.bits.subIdx  := s1_sub_idx

  sameVec(0) := stage_valid(1) && s1_main_idx === s0_main_idx && s1_tag === s0_tag

  // ----------- s2: check VpnTable hit -----------
  stage_valid(2) := RegNext(stage_valid(1))
  val s2_main_idx = RegNext(s1_main_idx)
  val s2_sub_idx  = RegNext(s1_sub_idx)
  val s2_tag      = RegNext(s1_tag)
  val s2_is_hit_cdp = RegNext(s1_is_hit_cdp)
  val s2_tab_rsp = RegNext(s1_tab_rsp)

  val s2_hit_main_vec = s2_tab_rsp.tagVec.zip(s2_tab_rsp.mainValidVec).map {
    case (tag, valid) => tag === s2_tag && valid
  }
  val s2_hit_main_idx = PriorityEncoder(s2_hit_main_vec)
  val s2_hit_main = s2_hit_main_vec.reduce(_ || _)
  assert(PopCount(s2_hit_main_vec) < 2.U || !stage_valid(2), "Main entry multiple hit!")

  val s2_hit_sub_vec = s2_tab_rsp.tagVec.zip(s2_tab_rsp.subValidVec).map {
    case (tag, valid) => tag === s2_tag  && valid
  }
  val s2_hit_sub = s2_hit_sub_vec.reduce(_ || _)

  sameVec(1) := stage_valid(2) && s2_main_idx === s0_main_idx && s2_tag === s0_tag

  // ----------- s3: build VpnTable update -----------
  stage_valid(3) := RegNext(stage_valid(2))
  val s3_main_idx = RegNext(s2_main_idx)
  val s3_sub_idx  = RegNext(s2_sub_idx)
  val s3_tag      = RegNext(s2_tag)
  val s3_is_hit_cdp = RegNext(s2_is_hit_cdp)
  val s3_hit_main = RegNext(s2_hit_main)
  val s3_hit_sub = RegNext(s2_hit_sub)
  val s3_hit_main_idx = RegNext(s2_hit_main_idx)

  val plruWay = replacer.way(s3_main_idx)
  val s3_update_info = WireInit(0.U.asTypeOf(new VtTrainReq))
  s3_update_info.tag := s3_tag
  s3_update_info.mainIdx := s3_main_idx
  s3_update_info.subIdx := s3_sub_idx
  s3_update_info.allocMain := !s3_hit_main
  s3_update_info.allocSub := s3_hit_main && !s3_hit_sub
  s3_update_info.targetWay := Mux(s3_hit_main, s3_hit_main_idx, plruWay)
  s3_update_info.isHitCdp := s3_is_hit_cdp

  sameVec(2) := stage_valid(3) && s3_main_idx === s0_main_idx && s3_tag === s0_tag

  // ----------- s4: drive VpnTable update -----------
  stage_valid(4) := RegNext(stage_valid(3))
  val s4_update_info = RegNext(s3_update_info)

  trainReq.valid := stage_valid(4)
  trainReq.bits := s4_update_info

  when (stage_valid(4)) {
    replacer.access(s4_update_info.mainIdx, s4_update_info.targetWay)
  }

  sameVec(3) := stage_valid(4) && s4_update_info.mainIdx === s0_main_idx && s4_update_info.tag === s0_tag

  class TrainTriggerEntry extends CDPBundle {
    val vaddr = UInt(fullVAddrBits.W)
    val mainIdx = UInt(mainEntryBits.W)
    val subIdx  = UInt(subEntryBits.W)
    val way      = UInt(vpnWayBits.W)
    val allocMain  = Bool()
    val allocSub   = Bool()
    val noAlloc    = Bool()
  }

  val cdpTrainTriggerDB = ChiselDB.createTable("cdpTrain", new TrainTriggerEntry, basicDB = debug)

  val trainTriggerEntry = Wire(new TrainTriggerEntry)
  trainTriggerEntry.vaddr := RegNext(RegNext(RegNext(RegNext(trainVaddr))))
  trainTriggerEntry.mainIdx := s4_update_info.mainIdx
  trainTriggerEntry.subIdx := s4_update_info.subIdx
  trainTriggerEntry.way := s4_update_info.targetWay
  trainTriggerEntry.allocMain := s4_update_info.allocMain
  trainTriggerEntry.allocSub := s4_update_info.allocSub
  trainTriggerEntry.noAlloc := !s4_update_info.allocMain && !s4_update_info.allocSub

  cdpTrainTriggerDB.log(trainTriggerEntry, stage_valid(4), "", clock, reset)
}

class DetectPipeline(name:String)(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val detectReq    = Flipped(ValidIO(new CDPDetectTask(dataBits=64)))

    // to Vpn Table
    val vtQueryReq  = ValidIO(new VtQueryReq)
    val vtQueryRsp  = Input(new VtQueryRsp)

    // Prefetch Req
    val pftReq = ValidIO(new CDPPrefetchReq)

    // addrState
    val addrState = new Bundle {
      val isSv39 = Input(Bool())
      val isSv48 = Input(Bool())
      val isSv57 = Input(Bool())
    }
  })

  val detectReq  = io.detectReq
  val pftReq     = io.pftReq

  val (vtQueryReq, vtQueryRsp) = (io.vtQueryReq, io.vtQueryRsp)

  // Pipeline Ctrl Signals
  val s0_req  = Wire(Valid(new CDPDetectTask(dataBits=64)))
  val s1_req  = Wire(Valid(new CDPDetectTask(dataBits=64)))
  val s2_req  = Wire(Valid(new CDPDetectTask(dataBits=64)))
  val s3_req  = Wire(Valid(new CDPDetectTask(dataBits=64)))

  // ------------------ s0 ------------------
  s0_req.valid  := detectReq.valid
  s0_req.bits   := detectReq.bits

  // ------------------ s1 ------------------
  // query VpnTable
  s1_req.valid  := RegNext(s0_req.valid)
  s1_req.bits   := RegEnable(s0_req.bits, s0_req.valid)

  val s1_data     = s1_req.bits.data
  val s1_addr     = s1_data(fullVAddrBits - 1, 0)
  val s1_main_idx = getMainIdx(s1_addr)
  val s1_sub_idx  = getSubIdx(s1_addr)

  vtQueryReq.valid  := s1_req.valid
  vtQueryReq.bits.mainIdx  := s1_main_idx
  vtQueryReq.bits.subIdx   := s1_sub_idx

  val s1_vt_query_rsp = vtQueryRsp

  // ------------------ s2 ------------------
  // check conditions
  s2_req.valid  := RegNext(s1_req.valid)
  s2_req.bits   := RegEnable(s1_req.bits, s1_req.valid)

  val s2_data   = s2_req.bits.data
  val s2_addr   = s2_data(fullVAddrBits - 1, 0)
  val s2_depth  = s2_req.bits.pfDepth
  val s2_is_hit = s2_req.bits.isHit
  
  val s2_vt_query_rsp = RegNext(s1_vt_query_rsp)

  val s2_tag  = getVpnTableTag(s2_addr)
  val s2_vt_hit_vec = s2_vt_query_rsp.tagVec.zip(s2_vt_query_rsp.subValidVec).map{
    case (t, v) =>
      t === s2_tag && v
  }
  assert(PopCount(s2_vt_hit_vec) < 2.U || !s2_req.valid, "VpnTable multiple hit in DetectPipeline!")
  
  val s2_vt_hit     = s2_vt_hit_vec.reduce(_ || _)
  val s2_vt_hit_idx = PriorityEncoder(s2_vt_hit_vec)
  val s2_vt_hit_hot = s2_vt_query_rsp.metaVec(s2_vt_hit_idx).hot

  val s2_vpn0 = getVpn0(s2_addr)
  val s2_vpn0_is_nzero    = s2_vpn0 =/= 0.U

  val s2_low_bit  = s2_data(1, 0)
  val s2_low_bit_is_zero  = s2_low_bit === 0.U

  val s2_high_bit = Mux(
    io.addrState.isSv39,
    s2_data(63, 39),
    Mux(io.addrState.isSv48,
      s2_data(63, 48),
      Mux(io.addrState.isSv57, s2_data(63, 57), 0.U)
    )
  )
  val s2_high_bit_is_zero = s2_high_bit === 0.U

  // TODO: maybe we should move depth control totally to the entrance?
  val s2_can_pft  = s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero && s2_vt_hit && s2_vt_hit_hot

  // ------------------ s3 ------------------
  // generate prefetch req
  s3_req.valid  := RegNext(s2_req.valid && s2_can_pft)
  s3_req.bits   := RegEnable(s2_req.bits, s2_req.valid && s2_can_pft)

  val s3_vt_hit     = RegNext(s2_vt_hit)
  val s3_vt_hit_idx = RegNext(s2_vt_hit_idx)
  val s3_can_pft    = RegNext(s2_can_pft)
  val s3_depth      = RegNext(Mux(
    s2_is_hit,
    1.U,      // hit a CDP prefetched block, reinforce
    Mux(s2_depth === 0.U, pfDepthMax.U, s2_depth + 1.U)
  ))

  val s3_data = s3_req.bits.data

  pftReq.valid := s3_req.valid
  pftReq.bits.pfVAddr  := s3_data(fullVAddrBits - 1, 0)
  pftReq.bits.pfDepth  := s3_depth
  pftReq.bits.pfSource := s3_req.bits.pfSource
  pftReq.bits.isHit   := s3_req.bits.isHit

  // ------------------ Performance Counter ------------------
  // Valid VpnTable hit/miss and distribution
  XSPerfAccumulate("valid_vt_hit", s2_req.valid && s2_vt_hit && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero)
  XSPerfAccumulate("valid_vt_miss", s2_req.valid && !s2_vt_hit && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero)

  // ----------- ChiselDB -----------
  class DetectTriggerEntry extends CDPBundle {
    val vaddr     = UInt(64.W)
    val pfDepth   = UInt(pfDepthBits.W)
    val pfSource  = UInt(PfSource.pfSourceBits.W)
    val mainIdx    = UInt(mainEntryBits.W)
    val subIdx     = UInt(subEntryBits.W)
    val vtHit      = Bool()
    val vtHitHot  = Bool()
    val canPft      = Bool()
  }

  val cdpDetectTriggerDB = ChiselDB.createTable(name + "_cdpDetect", new DetectTriggerEntry, basicDB = debug)

  val detectTriggerEntry = Wire(new DetectTriggerEntry)
  detectTriggerEntry.vaddr := s2_data
  detectTriggerEntry.pfDepth := s2_depth
  detectTriggerEntry.pfSource := s2_req.bits.pfSource
  detectTriggerEntry.mainIdx := RegNext(s1_main_idx)
  detectTriggerEntry.subIdx  := RegNext(s1_sub_idx)
  detectTriggerEntry.vtHit := s2_vt_hit
  detectTriggerEntry.vtHitHot := s2_vt_hit_hot
  detectTriggerEntry.canPft := s2_can_pft

  val en = s2_req.valid && s2_high_bit_is_zero && s2_low_bit_is_zero && s2_vpn0_is_nzero
  cdpDetectTriggerDB.log(detectTriggerEntry, en, "", clock, reset)
}

class PrefetchFilterEntry(implicit p: Parameters) extends CDPBundle {
  val paddrValid = Bool()
  val pTag  = UInt(reqFilterPTagBits.W)   // paddr = [ pTag | blockOffset ]
  val vTag  = UInt(reqFilterVTagBits.W)   // vaddr = [ vTag | blockOffset ]
  val pfDepth = UInt(pfDepthBits.W)

  // for TLB retry
  val retryEn    = Bool()
  val retryTimer = UInt(4.W)
  
  // Only for monitor
  val pfSource = UInt(PfSource.pfSourceBits.W)
  val isHit   = Bool() // is this trigger from req hitting l2?

  def toPrefetchReq(): PrefetchReq = {
    val req = Wire(new PrefetchReq)

    val fullAddr = Cat(pTag, 0.U(log2Ceil(blockBytes).W))
    req := DontCare
    req.tag := parseFullAddress(fullAddr)._1
    req.set := parseFullAddress(fullAddr)._2
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
    val tlbReq = new L2ToL1TlbIO

    // filter table
    val ftQueryReq = DecoupledIO(new FtQueryReq)
    val ftQueryRsp = Input(new FtQueryRsp)
  })

  def samePage(addr1: UInt, addr2: UInt): Bool = {
    addr1(fullVAddrBits - 1, pageOffsetBits) === addr2(fullVAddrBits - 1, pageOffsetBits)
  }

  val (in, out) = (io.in, io.out)
  val (ftQueryReq, ftQueryRsp) = (io.ftQueryReq, io.ftQueryRsp)

  val tlbReq = io.tlbReq.req
  val tlbRsp = io.tlbReq.resp
  val pmpRsp = io.tlbReq.pmp_resp
  io.tlbReq.req_kill := false.B
  tlbRsp.ready := true.B

  // check same cacheline
  def blockAddr(addr: UInt) = {
    require(addr.getWidth >= log2Ceil(blockBytes), "Address width is smaller than block size")
    addr(addr.getWidth - 1, log2Ceil(blockBytes))
  }

  // buffer
  val valids    = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(false.B)))
  val entries   = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(0.U.asTypeOf(new PrefetchFilterEntry))))
  
  val reqInflight  = RegInit(VecInit(Seq.fill(reqFilterEntryNum)(false.B)))

  val tlbArb = Module(new TwoLevelRRArbiter(new L2TlbReq, reqFilterEntryNum))
  val pftArb = Module(new TwoLevelRRArbiter(new PrefetchReq, reqFilterEntryNum))
  ArbPerf(tlbArb, "cdp_tlb_arb")
  ArbPerf(pftArb, "cdp_pft_arb")

  // enq buf logic
  in.ready := true.B  // TODO: backpressure when buffer full

  val entryHitVec = entries.zip(valids).map{
    case (e, v) =>
      v && e.vTag === blockAddr(in.bits.pfVAddr)
  }
  val entryHit = entryHitVec.reduce(_ || _)

  val freeEntryVec = valids.map(!_)
  val hasFreeEntry = freeEntryVec.reduce(_ || _)
  val freeEntryIdx = PriorityEncoder(freeEntryVec)

  val idx = freeEntryIdx
  val entry = entries(idx)
  when (in.valid && !entryHit && hasFreeEntry) {
    val allocEntry = WireInit(0.U.asTypeOf(new PrefetchFilterEntry))
    allocEntry.vTag := blockAddr(in.bits.pfVAddr)
    allocEntry.pfDepth   := in.bits.pfDepth
    allocEntry.pfSource  := in.bits.pfSource
    allocEntry.isHit    := in.bits.isHit

    entry := allocEntry
    valids(idx) := true.B
  }

  // timer
  for (i <- 0 until reqFilterEntryNum) {
    when (entries(i).retryEn && entries(i).retryTimer < retryInterval.U) {
      entries(i).retryTimer := entries(i).retryTimer + 1.U
    }
  }

  // --------------- tlb pipe ---------------
  val tlb_s1_valid = RegInit(false.B)
  val tlb_s2_valid = RegNext(tlbReq.fire, false.B)
  val tlb_s3_valid = RegNext(tlb_s2_valid, false.B)

  val tlb_s1_req = RegEnable(tlbArb.io.out.bits, tlbArb.io.out.fire)
  val tlb_s1_addr = Wire(UInt(fullVAddrBits.W))
  val tlb_s2_addr = RegEnable(tlb_s1_addr, tlbReq.fire)
  val tlb_s3_addr = RegEnable(tlb_s2_addr, tlb_s2_valid)

  // -------- tlb s0: arb tlb req from buffer --------
  for (i <- 0 until reqFilterEntryNum) {
    val entry = entries(i)
    val entryTlbReq = tlbArb.io.in(i)

    val entryTimerOk = !entry.retryEn || entry.retryTimer >= retryInterval.U

    val reqVaddr = Cat(entry.vTag, 0.U(log2Ceil(blockBytes).W))
    val s1_same_page = samePage(reqVaddr, tlb_s1_addr) && tlb_s1_valid
    val s2_same_page = samePage(reqVaddr, tlb_s2_addr) && tlb_s2_valid
    val s3_same_page = samePage(reqVaddr, tlb_s3_addr) && tlb_s3_valid
    val pageConflict = s1_same_page || s2_same_page || s3_same_page
    assert(
      PopCount(Seq(s1_same_page, s2_same_page, s3_same_page)) <= 1.U,
      "multiple inflight tlb reqs should not target the same page!"
    )

    entryTlbReq.valid := valids(i) && !entry.paddrValid && entryTimerOk && !pageConflict
    entryTlbReq.bits.vaddr  := reqVaddr
    entryTlbReq.bits.cmd    := TlbCmd.read
    entryTlbReq.bits.isPrefetch := true.B
    entryTlbReq.bits.size   := 3.U
    entryTlbReq.bits.kill   := false.B
    entryTlbReq.bits.no_translate := false.B
  }

  // -------- tlb s1: send tlb req --------
  val tlb_s1_ready = !tlb_s1_valid || tlbReq.ready
  tlbArb.io.out.ready := tlb_s1_ready
  when (tlb_s1_ready) {
    tlb_s1_valid := tlbArb.io.out.valid
  }

  tlbReq.valid := tlb_s1_valid
  tlbReq.bits := tlb_s1_req
  tlb_s1_addr := tlb_s1_req.vaddr

  // -------- tlb s2: recv tlb rsp --------
  // If miss, enable retry. If the retry also misses, drop the req.
  val tlb_s2_rsp = tlbRsp

  // -------- tlb s3: recv pmp rsp --------
  val tlb_s3_pmp       = pmpRsp
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
    val entryAddr = Cat(entries(i).vTag, 0.U(log2Ceil(blockBytes).W))
    
    // s2: if first miss, enable retry; if second miss, drop
    val s2_same_page = samePage(entryAddr, tlb_s2_addr) && tlb_s2_valid
    when (s2_same_page && tlb_s2_rsp.valid && tlb_s2_rsp.bits.miss && valids(i)) {
      when (entry.retryEn) {
        // second miss, drop the req
        valids(i) := false.B
      }.otherwise {
        // first miss, enable retry
        entries(i).retryEn := true.B
        entries(i).retryTimer := 0.U
      }
    }
    XSPerfAccumulate(
      s"s2_drop_tlb_miss_entry$i",
      s2_same_page && tlb_s2_rsp.valid && tlb_s2_rsp.bits.miss && valids(i) && entry.retryEn
    )

    // s3: check pf && pmp result, if fail, drop the req; otherwise, update the entry
    val s3_same_page = samePage(entryAddr, tlb_s3_addr) && tlb_s3_valid
    when (s3_same_page && tlb_s3_rsp_valid && !tlb_s3_rsp_bits.miss && valids(i)) {
      when (s3_drop) {
        valids(i) := false.B
      }.otherwise {
        entries(i).paddrValid := true.B

        val pageNum    = tlb_s3_rsp_bits.paddr.head(fullAddressBits - 1, pageOffsetBits)
        val pageOffset = entryAddr(pageOffsetBits - 1, 0)
        entries(i).pTag := blockAddr(Cat(pageNum, pageOffset))
      }
    }
    XSPerfAccumulate(
      s"s3_drop_entry$i",
      s3_same_page && tlb_s3_rsp_valid && !tlb_s3_rsp_bits.miss && valids(i) && s3_drop
    )
  }

  // --------------- prefetch req pipe ---------------
  val pft_s0_valid = Wire(Bool())
  val pft_s1_valid = RegNext(ftQueryReq.fire, false.B)
  val pft_s2_valid = RegNext(pft_s1_valid, false.B)

  val pft_s0_chosen_idx = Wire(UInt(log2Ceil(reqFilterEntryNum).W))
  val pft_s1_chosen_idx = RegEnable(pft_s0_chosen_idx, ftQueryReq.fire)
  val pft_s2_chosen_idx = RegEnable(pft_s1_chosen_idx, pft_s1_valid)

  val pft_s0_req = Wire(new PrefetchReq)
  val pft_s1_req = RegEnable(pft_s0_req, ftQueryReq.fire)
  val pft_s2_req = RegEnable(pft_s1_req, pft_s1_valid)

  // --------- req s0: arb prefetch req & query filter table ---------
  for (i <- 0 until reqFilterEntryNum) {
    val entry = entries(i)
    val entryPftReq = pftArb.io.in(i)

    entryPftReq.valid := valids(i) && entry.paddrValid && !reqInflight(i)
    entryPftReq.bits := entry.toPrefetchReq()

    when (entryPftReq.fire) {
      reqInflight(i) := true.B
    }
  }

  pft_s0_valid := pftArb.io.out.valid
  pftArb.io.out.ready := ftQueryReq.ready

  pft_s0_chosen_idx := pftArb.io.chosen
  pft_s0_req := pftArb.io.out.bits

  ftQueryReq.valid := pft_s0_valid
  ftQueryReq.bits  := 0.U.asTypeOf(new FtQueryReq)
  ftQueryReq.bits.setIdx := getFilterSet(pft_s0_req.addr)

  // --------- req s1: recv filter table rsp ---------
  val ft_s1_rsp = ftQueryRsp

  // --------- req s2: chk hit & send req ---------
  val ft_s2_rsp = RegEnable(ft_s1_rsp, pft_s1_valid)

  val hit = Wire(Bool())
  val canPft = Wire(Bool())

  if (useFilterTable) {
    val validVec = ft_s2_rsp.validVec
    val tagVec   = ft_s2_rsp.tagVec
    val hitVec   = validVec.zip(tagVec).map{
      case (v, t) =>
        v && t === getFilterTag(pft_s2_req.addr)
    }

    val hitIdx = PriorityEncoder(hitVec)
    val offset  = getFilterOffset(pft_s2_req.addr)

    val satVec = ft_s2_rsp.satVec
    hit := hitVec.reduce(_ || _)
    canPft := !hit || satVec(hitIdx)(offset) =/= 3.U

  } else {
    hit := false.B
    canPft := true.B
  }

  // send req
  out.valid := pft_s2_valid && canPft
  out.bits  := pft_s2_req

  when (out.fire || pft_s2_valid && !canPft) {
    valids(pft_s2_chosen_idx) := false.B
  }

  when (pft_s2_valid) {
    reqInflight(pft_s2_chosen_idx) := false.B
  }

  // ----------------- Perf Counter -----------------
  XSPerfAccumulate("in_drop_by_hit", in.valid && entryHit)
  XSPerfAccumulate("in_drop_by_full", in.valid && !entryHit && !hasFreeEntry)

  XSPerfAccumulate("pf_req_drop_by_filter", pft_s2_valid && !canPft)
  XSPerfAccumulate("filter_hit", pft_s2_valid && hit)
  XSPerfAccumulate("filter_miss", pft_s2_valid && !hit)

  val chosenEntry = entries(pft_s2_chosen_idx)
  XSPerfAccumulate("pf_req_fromCPU", out.fire && chosenEntry.pfSource === PfSource.NoWhere.id.U)
  XSPerfAccumulate("pf_req_fromBOP", out.fire && (chosenEntry.pfSource === PfSource.BOP.id.U || chosenEntry.pfSource === PfSource.PBOP.id.U))
  XSPerfAccumulate("pf_req_fromSMS", out.fire && chosenEntry.pfSource === PfSource.SMS.id.U)
  XSPerfAccumulate("pf_req_fromStream", out.fire && chosenEntry.pfSource === PfSource.Stream.id.U)
  XSPerfAccumulate("pf_req_fromStride", out.fire && chosenEntry.pfSource === PfSource.Stride.id.U)
  XSPerfAccumulate("pf_req_fromCDP", out.fire && chosenEntry.pfSource === PfSource.CDP.id.U)
}

class CDPPrefetcher(implicit p: Parameters) extends CDPModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())

    // detect
    val l2DetectTriggers = Flipped(Vec(banks, ValidIO(new CDPDetectTask(dataBits=blockBits))))

    // train
    val vpnTrain     = Flipped(ValidIO(new PrefetchTrain))
    val filterTrain  = Flipped(ValidIO(new PrefetchTrain))
    val pfStat        = Input(new PrefetchStat)

    // tlb?
    val tlbReq = new L2ToL1TlbIO

    // prefetch req?
    val pftReq = DecoupledIO(new PrefetchReq)
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

  /**
   * When disabled, no new detect or train triggers are accepted.
   * Requests already in the pipeline continue until they drain naturally.
   */
  val enable = io.enable & cstEnable.orR  

  val (vpnTrain, filterTrain) = (io.vpnTrain, io.filterTrain)

  val l2Triggers = io.l2DetectTriggers

  val filterTable      = if (useFilterTable) Some(Module(new FilterTable)) else None
  val vpnTable         = Module(new VpnTable)
  val vtTrainPipe     = Module(new VtTrainPipeline)
  val ftTrainPipe     = if (useFilterTable) Some(Module(new FtTrainPipeline)) else None
  val detectPipeSeq   = Seq.tabulate(detectPipeNum)(i => Module(new DetectPipeline(s"dp$i")))

  // TODO: ugly...
  // Detect Req
  val detectTrigQueueSeq = Seq.fill(banks)(Module(new MIMOQueue(new CDPDetectTask(dataBits=blockBits / 2), 8, 2, 1)))
  val detectTrigArb = Module(new RRArbiterInit(new CDPDetectTask(dataBits=blockBits / 2), banks))

  // detect_trigs <> detectTrigQueueSeq <> detectTrigArb <> detectPipeSeq
  for (i <- 0 until banks) {
    val detectTrigQueue = detectTrigQueueSeq(i)

    val detectTrig = l2Triggers(i)

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
    val detectTrigFromCDP = detectTrig.bits.pfSource === PfSource.CDP.id.U
    val detectTrigFromSMS = detectTrig.bits.pfSource === PfSource.SMS.id.U
    val detectTrigFromBOP = detectTrig.bits.pfSource === PfSource.BOP.id.U || detectTrig.bits.pfSource === PfSource.PBOP.id.U
    val detectTrigFromStream  = detectTrig.bits.pfSource === PfSource.Stream.id.U
    val detectTrigFromStride  = detectTrig.bits.pfSource === PfSource.Stride.id.U
    val detectTrigFromCPU     = detectTrig.bits.pfSource === PfSource.NoWhere.id.U

    // TODO: move depth check to MainPipe?
    val hitTrigger       = detectTrig.bits.isHit  &&
      (
        if (useFilteredDetect) {
          detectTrigFromCDP &&
            (detectTrig.bits.pfDepth === 1.U || detectTrig.bits.pfDepth === pfDepthMax.U) ||
            detectTrigFromSMS || detectTrigFromBOP
        }
        else {
          detectTrigFromCDP &&
            (detectTrig.bits.pfDepth === 1.U || detectTrig.bits.pfDepth === pfDepthMax.U)
        }
      )
    val refillTrigger    = !detectTrig.bits.isHit && detectTrig.bits.pfDepth < depthThreshold.U &&
      (
        if (useFilteredDetect) {
          detectTrigFromCPU || detectTrigFromCDP || detectTrigFromStride || detectTrigFromStream
        }
        else {
          true.B
        }
      )
    
    detectTrigQueue.io.flush := reset.asBool

    detectTrigQueue.io.enq(0).valid := detectTrig.valid && (hitTrigger || refillTrigger) && enable
    detectTrigQueue.io.enq(0).bits.data  := detectTrig.bits.data(blockBits / 2 - 1, 0)
    detectTrigQueue.io.enq(0).bits.pfDepth  := detectTrig.bits.pfDepth
    detectTrigQueue.io.enq(0).bits.pfSource := detectTrig.bits.pfSource
    detectTrigQueue.io.enq(0).bits.isHit   := detectTrig.bits.isHit

    detectTrigQueue.io.enq(1).valid := detectTrig.valid && (hitTrigger || refillTrigger) && enable
    detectTrigQueue.io.enq(1).bits.data  := detectTrig.bits.data(blockBits - 1, blockBits / 2)
    detectTrigQueue.io.enq(1).bits.pfDepth  := detectTrig.bits.pfDepth
    detectTrigQueue.io.enq(1).bits.pfSource := detectTrig.bits.pfSource
    detectTrigQueue.io.enq(1).bits.isHit   := detectTrig.bits.isHit

    detectTrigArb.io.in(i) <> detectTrigQueueSeq(i).io.deq(0)

    XSPerfAccumulate(s"detect_trig_num_bank$i", detectTrig.valid && enable)
    XSPerfAccumulate(s"detect_trig_drop0_bank$i", detectTrigQueue.io.enq(0).valid && !detectTrigQueue.io.enq(0).ready && enable)
    XSPerfAccumulate(s"detect_trig_drop1_bank$i", detectTrigQueue.io.enq(1).valid && !detectTrigQueue.io.enq(1).ready && enable)

    XSPerfAccumulate(s"detect_trig_hit_fromCDP_bank$i", detectTrig.valid && detectTrig.bits.isHit && detectTrigFromCDP && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromSMS_bank$i", detectTrig.valid && detectTrig.bits.isHit && detectTrigFromSMS && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromBOP_bank$i", detectTrig.valid && detectTrig.bits.isHit && detectTrigFromBOP && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromStream_bank$i", detectTrig.valid && detectTrig.bits.isHit && detectTrigFromStream && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromStride_bank$i", detectTrig.valid && detectTrig.bits.isHit && detectTrigFromStride && enable)
    XSPerfAccumulate(s"detect_trig_hit_fromCPU_bank$i", detectTrig.valid && detectTrig.bits.isHit && detectTrigFromCPU && enable)

    XSPerfAccumulate(s"detect_trig_refill_fromCDP_bank$i", detectTrig.valid && !detectTrig.bits.isHit && detectTrigFromCDP && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromSMS_bank$i", detectTrig.valid && !detectTrig.bits.isHit && detectTrigFromSMS && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromBOP_bank$i", detectTrig.valid && !detectTrig.bits.isHit && detectTrigFromBOP && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromStream_bank$i", detectTrig.valid && !detectTrig.bits.isHit && detectTrigFromStream && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromStride_bank$i", detectTrig.valid && !detectTrig.bits.isHit && detectTrigFromStride && enable)
    XSPerfAccumulate(s"detect_trig_refill_fromCPU_bank$i", detectTrig.valid && !detectTrig.bits.isHit && detectTrigFromCPU && enable)
  }

  for (i <- 0 until detectPipeNum) {
    val detectPipe = detectPipeSeq(i)

    detectPipe.io.detectReq.valid := detectTrigArb.io.out.valid
    detectPipe.io.detectReq.bits.data     := detectTrigArb.io.out.bits.data((i + 1) * 64 - 1, i * 64)   // 8 Byte ==> 64 bit
    detectPipe.io.detectReq.bits.pfDepth  := detectTrigArb.io.out.bits.pfDepth
    detectPipe.io.detectReq.bits.pfSource := detectTrigArb.io.out.bits.pfSource
    detectPipe.io.detectReq.bits.isHit   := detectTrigArb.io.out.bits.isHit

    detectPipe.io.addrState <> vtTrainPipe.io.addrState
  }
  detectTrigArb.io.out.ready := true.B

  // VpnTable & FilterTable Train Trigger
  val vpnTrainReqBuf  = Module(new Queue(new PrefetchTrain, 8))
  vpnTrainReqBuf.io.enq.valid := vpnTrain.valid && enable
  vpnTrainReqBuf.io.enq.bits  := vpnTrain.bits
  vtTrainPipe.io.trainTrigger <> vpnTrainReqBuf.io.deq
  XSPerfAccumulate("vpn_train_drop", vpnTrainReqBuf.io.enq.valid && !vpnTrainReqBuf.io.enq.ready)
  XSPerfAccumulate("vpn_train_accept", vpnTrainReqBuf.io.enq.fire)

  if (useFilterTable) {
    val filterTrainReqBuf = Module(new Queue(new PrefetchTrain, 8))
    filterTrainReqBuf.io.enq.valid := filterTrain.valid && enable
    filterTrainReqBuf.io.enq.bits  := filterTrain.bits
    ftTrainPipe.get.io.trainTrigger <> filterTrainReqBuf.io.deq
    XSPerfAccumulate("filter_train_drop", filterTrainReqBuf.io.enq.valid && !filterTrainReqBuf.io.enq.ready)
    XSPerfAccumulate("filter_train_accept", filterTrainReqBuf.io.enq.fire)
  }

  val vpnTabQueryReqSeq = detectPipeSeq.map(_.io.vtQueryReq) ++ Seq(vtTrainPipe.io.queryReq)
  val vpnTabQueryRspSeq = detectPipeSeq.map(_.io.vtQueryRsp) ++ Seq(vtTrainPipe.io.queryRsp)
  require(vpnTabQueryReqSeq.size == vpnTable.io.queryReq.size)
  vpnTable.io.queryReq.zip(vpnTabQueryReqSeq).foreach{
    case (tabReq, pipeReq) =>
      tabReq.valid := pipeReq.valid
      tabReq.bits := pipeReq.bits
  }
  vpnTable.io.queryRsp.zip(vpnTabQueryRspSeq).foreach{
    case (tabRsp, pipeRsp) =>
      pipeRsp := tabRsp
  }
  vpnTable.io.trainReq  <> vtTrainPipe.io.trainReq

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

      val DegreeEwmaShift = 9
      val sentEwma = RegInit(0.U(cdpPfSent.getWidth.W))
      val hitEwma = RegInit(0.U(cdpPfHit.getWidth.W))
      sentEwma := sentEwma - (sentEwma >> DegreeEwmaShift) + sentDelta
      hitEwma := hitEwma - (hitEwma >> DegreeEwmaShift) + hitDelta

      val sentLt100 = cdpPfSent < 100.U(cdpPfSent.getWidth.W)
      val accuracyGt5Pct = hitEwma * 100.U(7.W) > sentEwma * 5.U(3.W)
      Mux(sentLt100 || accuracyGt5Pct, degree.U, 1.U)
    } else {
      degree.U
    }

  // Degreed Buffer
  val degreeBufSeq = Seq.fill(detectPipeNum)(Module(new MIMOQueue(new CDPPrefetchReq, 8, degree, 1)))
  val degreeBufArb = Module(new RRArbiterInit(new CDPPrefetchReq, detectPipeNum))
  for (i <- 0 until detectPipeNum) {
    val buf = degreeBufSeq(i)
    val req = detectPipeSeq(i).io.pftReq
    buf.io.flush := reset.asBool
    for (j <- 0 until degree) {
      buf.io.enq(j).valid := req.valid && j.U < issueDegree
      buf.io.enq(j).bits  := req.bits

      if (j > 0) {
        buf.io.enq(j).bits.pfVAddr := req.bits.pfVAddr + (j * blockBytes).U
      }
    }

    degreeBufArb.io.in(i) <> buf.io.deq(0)

    XSPerfAccumulate(s"drop_by_acc_pipe$i", Mux(req.valid, degree.U - issueDegree, 0.U))
  }

  // SendUnit
  val sendUnit = Module(new SentUnit)
  sendUnit.io.tlbReq <> io.tlbReq
  if (useFilterTable) {
    val ftQueryArb = Module(new RRArbiter(new FtQueryReq, 2))
    ftQueryArb.io.in(0) <> ftTrainPipe.get.io.queryReq
    ftQueryArb.io.in(1) <> sendUnit.io.ftQueryReq
    filterTable.get.io.queryReq <> ftQueryArb.io.out

    filterTable.get.io.trainReq <> ftTrainPipe.get.io.trainReq

    val ftQueryChosen = RegEnable(ftQueryArb.io.chosen, ftQueryArb.io.out.fire)
    ftTrainPipe.get.io.queryRsp := Mux(
      filterTable.get.io.queryRsp.valid && ftQueryChosen === 0.U,
      filterTable.get.io.queryRsp.bits,
      0.U.asTypeOf(new FtQueryRsp)
    )
    sendUnit.io.ftQueryRsp := Mux(
      filterTable.get.io.queryRsp.valid && ftQueryChosen === 1.U,
      filterTable.get.io.queryRsp.bits,
      0.U.asTypeOf(new FtQueryRsp)
    )
  } else {
    sendUnit.io.ftQueryReq.ready := true.B
    sendUnit.io.ftQueryRsp := 0.U.asTypeOf(new FtQueryRsp)
  }
  sendUnit.io.in   <> degreeBufArb.io.out
  sendUnit.io.out  <> io.pftReq
}
