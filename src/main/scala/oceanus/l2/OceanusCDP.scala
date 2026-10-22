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
 * /data/zhuy/coupledL2/prefetch/CDP.scala:35-79 parameters; :81-237 address
 * helpers; :239-288/:450-465/:553-564/:1016-1042 interface/entry definitions.
 * Names are prefixed OceanusCDP; geometry is explicit VA50/PA48/line64/page4K,
 * internal source4 and depth2. No coupledL2 types or modules are instantiated.
 * :1319-1336 top-level interface is adapted to full PA training events and the
 * existing variable-latency translation client. This skeleton is not wired
 * into L2Top and contains no CDP algorithm; C1-C5 fill and integrate it later.
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
import utility.MemReqSource

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

object OceanusCDP {
  def configured(config: OceanusCDPParameters, banks: Int, p: Parameters): Parameters =
    p.alterPartial {
      case OceanusCDPKey => config
      case OceanusCDPBanksKey => banks
    }
}

// Intent (C0): a typechecked, elaboratable interface skeleton only. No table,
// detector, queue, training, translation state or feedback algorithm exists yet.
// It stays uninstantiated in L2Top until the later migration slices pass.
class OceanusCDP(config: OceanusCDPParameters = OceanusCDPParameters(), bankCount: Int = 1)
    (implicit p: Parameters) extends OceanusCDPModule()(OceanusCDP.configured(config, bankCount, p)) {
  val io = IO(new OceanusCDPIO)
  io.req.valid := false.B
  io.req.bits := 0.U.asTypeOf(io.req.bits)
  io.tlb.req.valid := false.B
  io.tlb.req.bits := 0.U
  io.tlb.resp.ready := true.B
}
