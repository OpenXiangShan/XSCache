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
 * [1] Robert Cooksey, Stephan Jourdan, and Dirk Grunwald. "[A stateless, content-directed data prefetching 
 * mechanism.](https://doi.org/10.1145/635508.605427)" ACM SIGOPS Operating System Review. 2002
 * *************************************************************************************
 */

/*
 * Intent/source ledger (C0), read before implementation:
 * C1 intent/source ledger (read before implementation):
 * CDP.scala:290-448 VpnTable; :695-867 VtTrainPipeline, expression-for-expression
 * except local type names, fixed block-VA field and the debug table name.
 * Preserve unreset invalid payload, wraparound, refresh priority/drop, PLRU
 * updates even on refresh, and original reset/stage-valid expressions.
 * Global event mapping: L2CDPTrain.vaddr -> train.vaddr (both VA[49:6]),
 * hit -> hit, old pfSource -> pfSource; hit && source9 gives +4, else +1.
 * prefetched/depth/PA do not gate that weight. :1471-1476 gives Queue(8).
 * Local admission uses enable/trainEnable/vpnValid and excludes L1DataPrefetch
 * (Prefetcher.scala:407); stop admission on disable but drain accepted work.
 * D-CDP-Get: MainPipe.scala:726/:731/:750-753 trains eligible Get+needHint;
 * current CCHI ReadOnce is excluded (L2VPipeREQ.scala:228-230/:248/:267).
 * Kernel ICache/PTW/data-uncache/instruction-uncache Get clients declare no
 * PrefetchField (ICache.scala:44-51, L2TLB.scala:43, Uncache.scala:196,
 * InstrUncache.scala:35-38); their present Get traffic is therefore ineligible.
 * MemBlock.scala:266-273 buffers the same instruction-uncache Get without
 * adding a hint. This does not remove the interface coverage deviation.
 * No cache/BOP edit.
 * CDP output remains inactive; only C1 training is instantiated, not C2-C6.
 * /data/zhuy/coupledL2/prefetch/CDP.scala:35-79 parameters; :81-237 address
 * helpers; :239-288/:450-465/:553-564/:1016-1042 interface/entry definitions.
 * Names are prefixed OceanusCDP; geometry is explicit VA50/PA48/line64/page4K,
 * internal source4 and depth2. No coupledL2 types or modules are instantiated.
 * :1319-1336 top-level interface is adapted to full PA training events and the
 * existing variable-latency translation client. This skeleton is not wired
 * into L2Top. C1 adds only VPN/VtTrain; C2-C5 fill and integrate later.
 * MainPipe.scala:731, CDP.scala:66/:212-228/:609-671: Filter used feedback is
 * by 4KB physical page, regardless of prefetch bit; do not implement :729 TODO.
 * mergeA maps to one demand hit after RBE wait (MainPipe.scala:726/:731/:770).
 * MainPipe.scala:734-737: unused CDP maps to actual capacity eviction valid
 * with old source9 and the unconsumed mark still present; never all CDP evicts.
 * MainPipe.scala:537: Get hit does not clear mark. Oceanus semantic counterpart
 * is CCHI ReadOnce (:62 Opcode.scala); currently neither clears nor trains
 * BOP/CDP (L2VPipeREQ.scala:220-230/:248/:267/:1354/:1465-1469). Report only;
 * this C0 does not change existing BOP/REQ behavior.
 * CDP.scala:609-619/:667-671: evict chooses victim PA; simultaneous used=true
 * still wins the decrement branch. Distinct events are not silently merged.
 * CDP.scala:1400-1434: filter WHOLE lines before slice arbitration, half queue
 * and cross-slice RR. Existing 1c-2 capture order differs; defer to C3/C5.
 * CDP.scala:68-69/:1501-1528: default static degree3 for C0-C5; C6 optional.
 * D-CDP-event: physical eviction timing, refiller reqSource and Get training
 * coverage differ. D-CDP-queue: local MIMOQueue partial-fire issue unresolved.
 * D1-CDP: variable TLB timing/shared admission and slot RR require C4 proof.
 * Filter mechanism relationship to Apple US9886385 awaits legal assessment.
 * PC + relative-offset quality filtering is a future performance experiment
 * candidate, dependent on a PC sideband. No patent-source reading is claimed.
 */
package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Field, Parameters}
import utility._

case class OceanusCDPParameters(
  pfDepthMax: Int = 3,
  pfDepthBits: Int = 2,

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

) {
  val hasPrefetchBit: Boolean = true
  val hasPrefetchSrc: Boolean = true
  val inflightEntries: Int = 16  // ???
}

case object OceanusCDPKey extends Field[OceanusCDPParameters](OceanusCDPParameters())
case object OceanusCDPBanksKey extends Field[Int](1)

trait OceanusCDPGeometry {
  implicit val p: Parameters
  def fullVAddrBits = 50
  def fullAddressBits = 48
  def blockBytes = 64
  def blockBits = 512
  def pageOffsetBits = 12
  def cdpPfDepthBits = Some(p(OceanusCDPKey).pfDepthBits)
  def cdpPfDepthMax = Some(p(OceanusCDPKey).pfDepthMax)
}
trait HasOceanusCDPParams extends OceanusCDPGeometry {
  def cdpParams = p(OceanusCDPKey)
  require(cdpParams.pfDepthMax < (1 << cdpParams.pfDepthBits), s"pfDepthMax(${cdpParams.pfDepthMax}) should be less than 2^pfDepthBits(${cdpParams.pfDepthBits})")

  val banks = p(OceanusCDPBanksKey)
  require(Set(1, 2, 4).contains(banks), "Oceanus CDP supports 1/2/4 slices")
  require(cdpParams.pfDepthBits == 2, "Oceanus Meta carries two depth bits")

  val debug = cdpParams.debug

  val useFilteredDetect = cdpParams.useFilteredDetect
  val useFilterTable    = cdpParams.useFilterTable

  val hotThreshold   = cdpParams.hotThreshold
  val depthThreshold = cdpParams.depthThreshold
  require(depthThreshold <= cdpPfDepthMax.get, s"depthThreshold(${depthThreshold}) should be less than pfDepthMax(${cdpPfDepthMax.get})")

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

  val degree = cdpParams.degree
  val useDynamicDegree  = cdpParams.useDynamicDegree

  val replType = cdpParams.replacer

  // VpnTable Params
  val counterBits = cdpParams.counterBits

  def getVpn0(addr: UInt): UInt = addr(20, 12)
  def getOffset(addr: UInt): UInt = addr(11, 0)

  val vpnTableSetNum = cdpParams.vpnTableSetNum
  val vpnTableWayNum = cdpParams.vpnTableWayNum
  val vpnTableSubEntryNum = cdpParams.vpnTableSubEntryNum

  val vpnResetPeriod = cdpParams.vpnResetPeriod

  val entryBits = cdpParams.entryBits
  val mainEntryBits = log2Ceil(vpnTableSetNum)
  val subEntryBits = log2Ceil(vpnTableSubEntryNum)
  val vpnTabTagBits = cdpParams.vpnTableTagBits
  val useVpnTableHashIndex = cdpParams.useVpnTableHashIndex
  val vpnWayBits = log2Ceil(vpnTableWayNum)

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
    val originTag = getVpnTableOriginTag(addr)
    val originMainIdx = getVpnTableOriginMainIdx(addr)

    if (useVpnTableHashIndex) {
      originMainIdx ^ getFoldedHash(originTag, mainEntryBits)
    } else {
      originMainIdx
    }
  }

  def getSubIdx(addr: UInt) = {
    val originTag = getVpnTableOriginTag(addr)
    val originMainIdx = getVpnTableOriginMainIdx(addr)
    val originSubIdx = getVpnTableOriginSubIdx(addr)

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
  val filterTableSetBits = log2Ceil(filterTableSetNum)
  val filterTableTagBits = fullAddressBits - log2Ceil(filterEntryGranularity) - filterTableSetBits - filterTableOffsetBits

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

abstract class OceanusCDPBundle(implicit val p: Parameters) extends Bundle with HasOceanusCDPParams
abstract class OceanusCDPModule(implicit val p: Parameters) extends Module with HasOceanusCDPParams

class OceanusCDPDetectTask(val dataBits: Int)(implicit p: Parameters) extends OceanusCDPBundle {
  /**
   * dataBits specifies the data width used at different pipeline stages:
   * - blockBits: Full cacheline width. Used to receive detect triggers from MainPipe.
   * - blockBits / 2: Half cacheline width. Each cacheline is split into two halves and
   *        buffered in DetectTriggerQueue before entering DetectPipe.
   * - 64:  8-byte width. DetectPipe processes one 8-byte chunk per cycle.
   */
  require(dataBits == blockBits || dataBits == blockBits / 2 || dataBits == 64)

  val data = UInt(dataBits.W)
  val pfDepth = UInt(cdpPfDepthBits.get.W)
  val pfSource = UInt(L2PfSource.Width.W)
  val isHit = Bool() // is this trigger from req hitting l2?
}

class OceanusCDPVpnMeta(implicit p: Parameters) extends OceanusCDPBundle {
  val hot = Bool()    // indicate whether this page is frequently visited in the past period
  val prevRefCnt = UInt(counterBits.W)
  val refCnt = UInt(counterBits.W)
}

class OceanusCDPVtQueryReq(implicit p: Parameters) extends OceanusCDPBundle {
  val mainIdx = UInt(mainEntryBits.W)
  val subIdx = UInt(subEntryBits.W)
}

class OceanusCDPVtQueryRsp(implicit p: Parameters) extends OceanusCDPBundle {
  val tagVec = Vec(vpnTableWayNum, UInt(vpnTabTagBits.W))
  val metaVec = Vec(vpnTableWayNum, new OceanusCDPVpnMeta)

  val subValidVec = Vec(vpnTableWayNum, Bool())
  val mainValidVec = Vec(vpnTableWayNum, Bool())
}

class OceanusCDPVtTrainReq(implicit p: Parameters) extends OceanusCDPBundle {
  val allocMain = Bool()    // allocate a new MainEntry (clear all the SubEntries)
  val allocSub = Bool()    // allocate a new SubEntry (refCnt = 1)
  val targetWay = UInt(log2Ceil(vpnTableWayNum).W)

  val mainIdx = UInt(mainEntryBits.W)
  val subIdx = UInt(subEntryBits.W)

  val tag = UInt(vpnTabTagBits.W)

  val isHitCdp = Bool()  // train trigger from hitting a CDP prefetched block?
}

class OceanusCDPFtQueryReq(implicit p: Parameters) extends OceanusCDPBundle {
  val setIdx = UInt(filterTableSetBits.W)
}

class OceanusCDPFtQueryRsp(implicit p: Parameters) extends OceanusCDPBundle {
  val validVec = Vec(filterTableWayNum, Bool())
  val tagVec = Vec(filterTableWayNum, UInt(filterTableTagBits.W))
  val satVec = Vec(filterTableWayNum, Vec(filterEntryBlks, UInt(2.W)))
}

class OceanusCDPFtTrainReq(implicit p: Parameters) extends OceanusCDPBundle {
  val set = UInt(filterTableSetBits.W)
  val way = UInt(log2Ceil(filterTableWayNum).W)
  val tag = UInt(filterTableTagBits.W)
  val sat = Vec(filterEntryBlks, UInt(2.W))
}

class OceanusCDPCandidate(implicit p: Parameters) extends OceanusCDPBundle {
  /**
    * Internal prefetchReq
    * Used in DetectPipe <> degreeBuf <> SentUnit
    */
  val pfVAddr = UInt(fullVAddrBits.W)
  val pfDepth = UInt(cdpPfDepthBits.get.W)

  // Only for monitor
  val pfSource = UInt(L2PfSource.Width.W)
  val isHit = Bool()    // Req triggered by hitting l2
}

class OceanusCDPEntry(implicit p: Parameters) extends OceanusCDPBundle {
  val paddrValid = Bool()
  val pTag = UInt(reqFilterPTagBits.W)   // paddr = [ pTag | blockOffset ]
  val vTag = UInt(reqFilterVTagBits.W)   // vaddr = [ vTag | blockOffset ]
  val pfDepth = UInt(cdpPfDepthBits.get.W)

  // for TLB retry
  val retryEn = Bool()
  val retryTimer = UInt(4.W)
  
  // Only for monitor
  val pfSource = UInt(L2PfSource.Width.W)
  val isHit = Bool() // is this trigger from req hitting l2?

  // Intent (C0): define the transport conversion, not the SentUnit algorithm.
  // Reference :1030-1039 defines PA/depth/request source; remaining fields were
  // DontCare. Shared is deterministic for the later CCHI adapter (D-CDP-fields).
  def toRequest(): OceanusCDPRequest = {
    val req = Wire(new OceanusCDPRequest)
    req.paddr := Cat(pTag, 0.U(6.W))
    req.pfDepth := pfDepth
    req.pfSource := L2PfSource.CDP.U
    req.reqSource := MemReqSource.Prefetch2L2CDP.id.U
    req.needT := false.B
    req
  }
}

// Intent (C0): preserve combined hit/evict flags and both physical addresses;
// C2 must select victim address before applying the isUsed priority.
class OceanusCDPTrain(implicit p: Parameters) extends OceanusCDPBundle {
  val paddr = UInt(fullAddressBits.W)
  val evictPaddr = UInt(fullAddressBits.W)
  val vaddr = UInt((fullVAddrBits - 6).W)
  val hit = Bool()
  val prefetched = Bool()
  val pfSource = UInt(4.W)
  val reqSource = UInt(5.W)
  val trainEnable = Bool()
  val vpnValid = Bool()
  val isUsed = Bool()
  val isEvict = Bool()
}

// Intent (C1): adapt the existing global demand event without connecting it
// to the cache. This is VPN-only: absent prefetch-bit and Filter/evict fields
// stay zero; VPN weighting reads hit and old pfSource, never prefetched/depth.
object OceanusCDPTrain {
  def fromVpnEvent(event: L2CDPTrain)(implicit p: Parameters): OceanusCDPTrain = {
    val train = WireInit(0.U.asTypeOf(new OceanusCDPTrain))
    train.paddr := event.paddr
    train.vaddr := event.vaddr
    train.hit := event.hit
    train.pfSource := event.pfSource
    train.reqSource := event.reqSource
    train.trainEnable := event.trainEnable
    train.vpnValid := true.B
    train
  }
}

class OceanusCDPRequest(implicit p: Parameters) extends OceanusCDPBundle {
  val paddr = UInt(fullAddressBits.W)
  val pfDepth = UInt(cdpPfDepthBits.get.W)
  val pfSource = UInt(4.W)
  val reqSource = UInt(5.W)
  val needT = Bool()
}

class OceanusCDPStat extends Bundle {
  val pfSentVec = Vec(10, UInt(64.W))
  val pfHitVec = Vec(10, UInt(64.W))
}

class OceanusCDPIO(implicit p: Parameters) extends OceanusCDPBundle {
  val enable = Input(Bool())
  val l2DetectTriggers = Flipped(Vec(banks, Valid(new OceanusCDPDetectTask(512))))
  val vpnTrain = Flipped(Valid(new OceanusCDPTrain))
  val filterTrain = Flipped(Valid(new OceanusCDPTrain))
  val pfStat = Input(new OceanusCDPStat)
  val tlb = Flipped(new L2PrefetchTranslationClientIO)
  val req = Decoupled(new OceanusCDPRequest)
}

// Intent (C1): retain reference :290-448 and :695-867 state/update timing.
// These modules have no coupledL2 dependency and remain outside the cache.
class OceanusCDPVpnTable(implicit p: Parameters) extends OceanusCDPModule {
  val io = IO(new Bundle {
    val queryReq = Flipped(Vec(detectPipeNum + 1, ValidIO(new OceanusCDPVtQueryReq)))   // +1 for train pipe
    val queryRsp = Vec(detectPipeNum + 1, Output(new OceanusCDPVtQueryRsp))
    val trainReq = Flipped(ValidIO(new OceanusCDPVtTrainReq))
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
    Vec(vpnTableSetNum, Vec(vpnTableWayNum, Vec(vpnTableSubEntryNum, new OceanusCDPVpnMeta)))
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

    val incrNum = Mux(trainReq.bits.isHitCdp, 4.U, 1.U)

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
  when (refreshCnt < vpnResetPeriod.U && trainReq.valid) {
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

          entry.refCnt := 0.U
          entry.prevRefCnt := nextPrevRefCnt
          entry.hot := Mux(nextPrevRefCnt > hotThreshold.U, true.B, false.B)
        }
      }
    }
  }

  // ------------------ Performance Counter ------------------
  XSPerfAccumulate("vt_refresh", isRefresh)
  XSPerfAccumulate("vt_alloc_main", trainReq.valid && trainReq.bits.allocMain)
  XSPerfAccumulate("vt_alloc_sub", trainReq.valid && trainReq.bits.allocSub)
  XSPerfAccumulate("vt_no_alloc", trainReq.valid && !trainReq.bits.allocMain && !trainReq.bits.allocSub)

  // OceanusCDPVpnTable train index distribution.
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

class OceanusCDPVtTrainPipeline(implicit p: Parameters) extends OceanusCDPModule {
  val io = IO(new Bundle {
    val trainTrigger = Flipped(DecoupledIO(new OceanusCDPTrain))

    // to VPN Table
    val queryReq = ValidIO(new OceanusCDPVtQueryReq)
    val queryRsp = Input(new OceanusCDPVtQueryRsp)
    val trainReq = ValidIO(new OceanusCDPVtTrainReq)

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

  val trainVaddr = trainTrigger.bits.vaddr << log2Ceil(blockBytes)
  val s0_main_idx = getMainIdx(trainVaddr)
  val s0_sub_idx = getSubIdx(trainVaddr)
  val s0_tag = getVpnTableTag(trainVaddr)
  val s0_is_hit_cdp = trainTrigger.bits.hit && trainTrigger.bits.pfSource === L2PfSource.CDP.U

  // addrState update
  // Intent (C1): the local event always carries block VA[49:6]; unlike
  // the reference optional field there is no absent-VA elaboration branch.
  val maxAddrWidth = trainTrigger.bits.vaddr.getWidth + log2Ceil(blockBytes)

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

  // ----------- s1: query OceanusCDPVpnTable -----------
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

  // ----------- s2: check OceanusCDPVpnTable hit -----------
  stage_valid(2) := RegNext(stage_valid(1))
  val s2_main_idx = RegNext(s1_main_idx)
  val s2_sub_idx  = RegNext(s1_sub_idx)
  val s2_tag = RegNext(s1_tag)
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

  // ----------- s3: build OceanusCDPVpnTable update -----------
  stage_valid(3) := RegNext(stage_valid(2))
  val s3_main_idx = RegNext(s2_main_idx)
  val s3_sub_idx  = RegNext(s2_sub_idx)
  val s3_tag = RegNext(s2_tag)
  val s3_is_hit_cdp = RegNext(s2_is_hit_cdp)
  val s3_hit_main = RegNext(s2_hit_main)
  val s3_hit_sub = RegNext(s2_hit_sub)
  val s3_hit_main_idx = RegNext(s2_hit_main_idx)

  val plruWay = replacer.way(s3_main_idx)
  val s3_update_info = WireInit(0.U.asTypeOf(new OceanusCDPVtTrainReq))
  s3_update_info.tag := s3_tag
  s3_update_info.mainIdx := s3_main_idx
  s3_update_info.subIdx := s3_sub_idx
  s3_update_info.allocMain := !s3_hit_main
  s3_update_info.allocSub := s3_hit_main && !s3_hit_sub
  s3_update_info.targetWay := Mux(s3_hit_main, s3_hit_main_idx, plruWay)
  s3_update_info.isHitCdp := s3_is_hit_cdp

  sameVec(2) := stage_valid(3) && s3_main_idx === s0_main_idx && s3_tag === s0_tag

  // ----------- s4: drive OceanusCDPVpnTable update -----------
  stage_valid(4) := RegNext(stage_valid(3))
  val s4_update_info = RegNext(s3_update_info)

  trainReq.valid := stage_valid(4)
  trainReq.bits := s4_update_info

  when (stage_valid(4)) {
    replacer.access(s4_update_info.mainIdx, s4_update_info.targetWay)
  }

  sameVec(3) := stage_valid(4) && s4_update_info.mainIdx === s0_main_idx && s4_update_info.tag === s0_tag

  class TrainTriggerEntry extends OceanusCDPBundle {
    val vaddr = UInt(fullVAddrBits.W)
    val mainIdx = UInt(mainEntryBits.W)
    val subIdx = UInt(subEntryBits.W)
    val way = UInt(vpnWayBits.W)
    val allocMain = Bool()
    val allocSub = Bool()
    val noAlloc = Bool()
  }

  val cdpTrainTriggerDB = ChiselDB.createTable("oceanusCDPTrain", new TrainTriggerEntry, basicDB = debug)

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

object OceanusCDP {
  def configured(config: OceanusCDPParameters, banks: Int, p: Parameters): Parameters =
    p.alterPartial {
      case OceanusCDPKey => config
      case OceanusCDPBanksKey => banks
    }
}

// Intent (C1): instantiate only VPN training behind the existing inactive
// interface. Cache integration and request generation belong to later slices.
class OceanusCDP(config: OceanusCDPParameters = OceanusCDPParameters(), bankCount: Int = 1)
    (implicit p: Parameters) extends OceanusCDPModule()(OceanusCDP.configured(config, bankCount, p)) {
  // Intent (C1): pass the instance's configured geometry explicitly to every
  // child; the constructor's implicit p is the caller's outer parameter set.
  private val instanceParams = OceanusCDP.configured(config, bankCount, p)
  val io = IO(new OceanusCDPIO()(instanceParams))
  val vpnTable = Module(new OceanusCDPVpnTable()(instanceParams))
  val vtTrainPipe = Module(new OceanusCDPVtTrainPipeline()(instanceParams))
  val vpnTrainReqBuf = Module(new Queue(new OceanusCDPTrain()(instanceParams), 8))
  vpnTrainReqBuf.io.enq.valid := io.vpnTrain.valid && io.enable &&
    io.vpnTrain.bits.vpnValid && io.vpnTrain.bits.trainEnable &&
    io.vpnTrain.bits.reqSource =/= MemReqSource.L1DataPrefetch.id.U
  vpnTrainReqBuf.io.enq.bits := io.vpnTrain.bits
  vtTrainPipe.io.trainTrigger <> vpnTrainReqBuf.io.deq
  vpnTable.io.queryReq(detectPipeNum) := vtTrainPipe.io.queryReq
  vtTrainPipe.io.queryRsp := vpnTable.io.queryRsp(detectPipeNum)
  vpnTable.io.trainReq := vtTrainPipe.io.trainReq
  for (i <- 0 until detectPipeNum) {
    vpnTable.io.queryReq(i).valid := false.B
    vpnTable.io.queryReq(i).bits := 0.U.asTypeOf(vpnTable.io.queryReq(i).bits)
  }
  XSPerfAccumulate("vpn_train_drop", vpnTrainReqBuf.io.enq.valid && !vpnTrainReqBuf.io.enq.ready)
  XSPerfAccumulate("vpn_train_accept", vpnTrainReqBuf.io.enq.fire)
  io.req.valid := false.B
  io.req.bits := 0.U.asTypeOf(io.req.bits)
  io.tlb.req.valid := false.B
  io.tlb.req.bits := 0.U
  io.tlb.resp.ready := true.B
}
