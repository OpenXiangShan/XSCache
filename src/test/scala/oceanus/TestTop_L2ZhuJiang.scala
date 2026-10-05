package oceanus

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import dongjiang.DJParam
import org.chipsalliance.cde.config._
import scala.collection.mutable.ArrayBuffer
import utility._
import oceanus.l2.{L2Configuration, L2Params, L2ParamsKey, L2Top, L2UpstreamPortType, L2UpstreamTable, L2UpstreamTableEntry}
import oceanus.chi.{CHIParameters, CHIParametersKey, EnumCHIIssue}
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import xijiang.{NodeParam, NodeType}
import xs.utils.debug.{HardwareAssertionKey, HwaParams}
import xs.utils.perf.{DebugOptions, DebugOptionsKey}
import xs.utils.perf.{LogUtilsOptions => ZJLogUtilsOptions, LogUtilsOptionsKey => ZJLogUtilsOptionsKey}
import xs.utils.perf.{PerfCounterOptions => ZJPerfCounterOptions, PerfCounterOptionsKey => ZJPerfCounterOptionsKey, XSPerfLevel => ZJXSPerfLevel}
import zhujiang.device.AxiDeviceParams
import zhujiang.{ZJParameters, ZJParametersKey, Zhujiang}

/*  Oceanus L2 x N behind the ZhuJiang CHI interconnect, as a cohestra_v3 DUT
 *  (plugged into the ARI Fuzz exactly like the Avocado DUT):
 *
 *       cchi_t1p{0..N-1}_*  (harness Taurus upstream nodes, CCHI NIDs 0..N-1)
 *                     |
 *                L2Top x N            (nodeId = the ZhuJiang CC node ID)
 *                     |  CHIRNFInterface (L-credit channels + link layer)
 *                     v
 *        OceanusZhuJiangBridge x N   (credits/link terminated internally)
 *                     |  SocketDevSide (sync socket, decoupled flits)
 *                     v
 *       ZhuJiang ring + HN-F banks + SN --ddrIO--> axi_m0_* (harness memory)
 *
 *  Boundary contract (identical naming to the cohestra_v3/avocado contract,
 *  auto-detected by the harness per present port index):
 *    - module clock/reset (high-active reset), no other control pins;
 *    - per L2 P: cchi_t1p{P}_{rxevt,rxreq,txsnp,txrsp,rxrsp,txdat,rxdat}_
 *      {valid,ready,bits_<Field>} — separate-field CCHI Type-1;
 *    - per L2 P: mon_chi_rn{P}_{txreq,txrsp,txdat,rxsnp,rxrsp,rxdat}_
 *      {flitpend,flitv,lcrdv[,flit][,flit_<Field>]} — passive monitor taps of
 *      the L2 <-> ZhuJiang link, on the L2 side of the bridge (full CHI-E
 *      flit geometry, identical to the Avocado monitor taps);
 *    - axi_m0_* — AXI4 master (id 24b / addr 48b / data 256b) towards the
 *      harness AXIMemory, driven by ZhuJiang's SN-side ddrIO.
 *
 *  Per-instance identities:
 *    --l2         number of L2 instances (1..4, default 4); each L2 takes the
 *                 node ID of its ZhuJiang CC node (see --print-topology output
 *                 / zj_topology.txt next to the RTL);
 *    --cchi-nids  upstream CCHI Type-1 node ID per L2 (one coherent client
 *                 per L2), default 0,1,2,3 — the harness upstream.type1.nodeid.
 *
 *  Known boundaries (same as TestTop_L2Avocado / TestTop_L2MultiCHI):
 *    - DnTXREQ.TgtID stays hardwired to 0 in L2VPipeREQ ("E-SAM only
 *      currently"); safe here because the CC node's RnRouter recomputes the
 *      completer TgtID from the address at ring injection.
 *    - CCHI NIDs must fit CCHIParameters.UpstreamNodeID_Width (default 4).
 */

class TestTop_L2ZhuJiang(
  val numL2: Int,
  val numSlices: Int,
  val cchiNIDs: Seq[Int],
  val chiMon: CHIMonitorMode = CHIMonitorMode.Packed
)(implicit p: Parameters) extends Module with HasOceanusZhuJiangBridge {

  import TestTop_CHIExport._

  private val ccNodes = p(ZJParametersKey).island.filter(_.nodeType == NodeType.CC)
  require(ccNodes.size >= numL2, s"ZhuJiang config has ${ccNodes.size} CC nodes, but $numL2 L2s are requested")

  // -- ZhuJiang interconnect (async-reset domain, as in TestTopZhuJiang)
  val zj = withClockAndReset(clock, reset.asAsyncReset) { Module(new Zhujiang) }
  zj.io.ci := 0.U
  zj.io.dft := DontCare
  zj.io.ramctl := DontCare
  require(zj.ccnIO.size >= numL2, s"ZhuJiang exposes ${zj.ccnIO.size} CCN IOs, but $numL2 L2s are requested")
  require(zj.ddrIO.nonEmpty, "TestTop_L2ZhuJiang expects at least one DDR AXI port")
  zj.ddrIO.drop(1).foreach(_ := DontCare)
  zj.cfgIO.foreach(_ := DontCare)
  zj.dmaIO.foreach(_ := DontCare)
  zj.hwaIO.foreach(_ := DontCare)

  val l2s = Seq.tabulate(numL2)(i => Module(new L2Top(new L2Configuration(
    nodeId = ccNodes(i).nodeId,
    eSAM = true,
    slices = 0 until numSlices,
    upstream = new L2UpstreamTable(Seq(
      L2UpstreamTableEntry(L2UpstreamPortType.Type1, cchiNIDs(i))))))))

  l2s.zipWithIndex.foreach { case (l2, i) =>
    // -- Exported upstream CCHI Type-1 port (cohestra contract: cchi_t1p{P}_*)
    l2.io.t1p.foreach { port =>
      exportChannel(s"cchi_t1p${i}_rxevt", port.UpEVT, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_rxreq", port.UpREQ, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_txsnp", port.DnSNP, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_txrsp", port.DnRSP, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_rxrsp", port.UpRSP, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_txdat", port.DnDAT, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_rxdat", port.UpDAT, dutDrives = false)
    }

    // -- Downstream CHI link into the ZhuJiang CC socket (credits and the
    //    link layer terminate inside the bridge; async-reset domain)
    withClockAndReset(clock, reset.asAsyncReset) {
      connectOceanusToZhuJiang(l2.io.chi, zj.ccnIO(i), ccNodes(i))
    }

    // -- CHI monitor taps (passive output-only; mon_chi_rn{P}_*) on the L2
    //    side of the bridge — full CHI-E flit geometry
    monitorCHIChannel(s"mon_chi_rn${i}_txreq", l2.io.chi.txreq, req(l2.io.chi.txreq.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_txrsp", l2.io.chi.txrsp, rsp(l2.io.chi.txrsp.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_txdat", l2.io.chi.txdat, dat(l2.io.chi.txdat.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_rxsnp", l2.io.chi.rxsnp, snp(l2.io.chi.rxsnp.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_rxrsp", l2.io.chi.rxrsp, rsp(l2.io.chi.rxrsp.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_rxdat", l2.io.chi.rxdat, dat(l2.io.chi.rxdat.flit), chiMon)
  }

  // -- DDR AXI master export (harness AXIMemory): axi_m0_* from ZhuJiang's
  //    SN-side ddrIO. Contract widths: id 24b, addr 48b, data 256b; AXI4
  //    subset (lock/cache/prot/qos/region/user stay inside the DUT).
  {
    val axi = zj.ddrIO.head
    def widen(sig: UInt, w: Int): UInt =
      if (sig.getWidth >= w) sig(w - 1, 0) else sig.pad(w)

    val axi_m0_awvalid = IO(Output(Bool())).suggestName("axi_m0_awvalid")
    val axi_m0_awid = IO(Output(UInt(24.W))).suggestName("axi_m0_awid")
    val axi_m0_awaddr = IO(Output(UInt(48.W))).suggestName("axi_m0_awaddr")
    val axi_m0_awlen = IO(Output(UInt(8.W))).suggestName("axi_m0_awlen")
    val axi_m0_awsize = IO(Output(UInt(3.W))).suggestName("axi_m0_awsize")
    val axi_m0_awburst = IO(Output(UInt(2.W))).suggestName("axi_m0_awburst")
    val axi_m0_awready = IO(Input(Bool())).suggestName("axi_m0_awready")
    axi_m0_awvalid := axi.awvalid
    axi_m0_awid := widen(axi.awid, 24)
    axi_m0_awaddr := widen(axi.awaddr, 48)
    axi_m0_awlen := widen(axi.awlen, 8)
    axi_m0_awsize := widen(axi.awsize, 3)
    axi_m0_awburst := widen(axi.awburst, 2)
    axi.awready := axi_m0_awready

    val axi_m0_wvalid = IO(Output(Bool())).suggestName("axi_m0_wvalid")
    val axi_m0_wdata = IO(Output(UInt(256.W))).suggestName("axi_m0_wdata")
    val axi_m0_wstrb = IO(Output(UInt(32.W))).suggestName("axi_m0_wstrb")
    val axi_m0_wlast = IO(Output(Bool())).suggestName("axi_m0_wlast")
    val axi_m0_wready = IO(Input(Bool())).suggestName("axi_m0_wready")
    axi_m0_wvalid := axi.wvalid
    axi_m0_wdata := axi.wdata
    axi_m0_wstrb := axi.wstrb
    axi_m0_wlast := axi.wlast
    axi.wready := axi_m0_wready

    val axi_m0_bvalid = IO(Input(Bool())).suggestName("axi_m0_bvalid")
    val axi_m0_bid = IO(Input(UInt(24.W))).suggestName("axi_m0_bid")
    val axi_m0_bresp = IO(Input(UInt(2.W))).suggestName("axi_m0_bresp")
    val axi_m0_bready = IO(Output(Bool())).suggestName("axi_m0_bready")
    axi.bvalid := axi_m0_bvalid
    axi.bid := widen(axi_m0_bid, axi.bid.getWidth)
    axi.bresp := axi_m0_bresp
    axi_m0_bready := axi.bready
    axi.buser := 0.U

    val axi_m0_arvalid = IO(Output(Bool())).suggestName("axi_m0_arvalid")
    val axi_m0_arid = IO(Output(UInt(24.W))).suggestName("axi_m0_arid")
    val axi_m0_araddr = IO(Output(UInt(48.W))).suggestName("axi_m0_araddr")
    val axi_m0_arlen = IO(Output(UInt(8.W))).suggestName("axi_m0_arlen")
    val axi_m0_arsize = IO(Output(UInt(3.W))).suggestName("axi_m0_arsize")
    val axi_m0_arburst = IO(Output(UInt(2.W))).suggestName("axi_m0_arburst")
    val axi_m0_arready = IO(Input(Bool())).suggestName("axi_m0_arready")
    axi_m0_arvalid := axi.arvalid
    axi_m0_arid := widen(axi.arid, 24)
    axi_m0_araddr := widen(axi.araddr, 48)
    axi_m0_arlen := widen(axi.arlen, 8)
    axi_m0_arsize := widen(axi.arsize, 3)
    axi_m0_arburst := widen(axi.arburst, 2)
    axi.arready := axi_m0_arready

    val axi_m0_rvalid = IO(Input(Bool())).suggestName("axi_m0_rvalid")
    val axi_m0_rid = IO(Input(UInt(24.W))).suggestName("axi_m0_rid")
    val axi_m0_rdata = IO(Input(UInt(256.W))).suggestName("axi_m0_rdata")
    val axi_m0_rresp = IO(Input(UInt(2.W))).suggestName("axi_m0_rresp")
    val axi_m0_rlast = IO(Input(Bool())).suggestName("axi_m0_rlast")
    val axi_m0_rready = IO(Output(Bool())).suggestName("axi_m0_rready")
    axi.rvalid := axi_m0_rvalid
    axi.rid := widen(axi_m0_rid, axi.rid.getWidth)
    axi.rdata := axi_m0_rdata
    axi.rresp := axi_m0_rresp
    axi.rlast := axi_m0_rlast
    axi_m0_rready := axi.rready
    axi.ruser := 0.U
  }

  // -- Logging
  val log = IO(new Bundle {
    val dump = Input(Bool())
    val clean = Input(Bool())
  })

  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U

  val timer = WireDefault(0.U(64.W))
  val logEnable = WireDefault(false.B)
  val clean = WireDefault(false.B)
  val dump = WireDefault(false.B)

  timer := cycle
  logEnable := true.B
  clean := log.clean
  dump := log.dump

  dontTouch(timer)
  dontTouch(logEnable)
  dontTouch(clean)
  dontTouch(dump)

  XSLog.collect(timer, logEnable, clean, dump)
}

object TestTop_L2ZhuJiangConfig {

  /* Ring template (12 slots): HF bank ports interleaved with CCs, exactly one
   * default HNI, one S (memory), one M, trailing P spacer. CC slots are
   * filled in order; numL2 < 4 drops the tail CCs. With nodeAidBits = 3 the
   * resulting node IDs are: HF 0x00/0x10/0x28/0x40, CC 0x08/0x18/0x30/0x48,
   * HNI 0x20, SNF 0x38, MN 0x50 (numL2 = 4). */
  private def nodeParams(numL2: Int): Seq[NodeParam] = {
    require(numL2 >= 1 && numL2 <= 4, s"TestTop_L2ZhuJiang supports 1..4 L2s, got $numL2")
    val cc = NodeParam(nodeType = NodeType.CC, socket = "sync")
    var ccIdx = 0
    Seq(
      NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 0),
      cc,
      NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 0),
      cc,
      NodeParam(nodeType = NodeType.HI, defaultHni = true,
        axiDevParams = Some(AxiDeviceParams(wrapper = "east", attr = "main"))),
      NodeParam(nodeType = NodeType.HF, bankId = 1, hfpId = 1),
      cc,
      NodeParam(nodeType = NodeType.S,
        axiDevParams = Some(AxiDeviceParams(wrapper = "south", attr = "mem_0"))),
      NodeParam(nodeType = NodeType.HF, bankId = 0, hfpId = 1),
      cc,
      NodeParam(nodeType = NodeType.M,
        axiDevParams = Some(AxiDeviceParams(wrapper = "misc", attr = "hwa"))),
      NodeParam(nodeType = NodeType.P)
    ).flatMap { np =>
      if (np.nodeType == NodeType.CC) {
        val keep = ccIdx < numL2
        ccIdx += 1
        Option.when(keep)(np)
      } else Some(np)
    }
  }

  def nocConfig(numL2: Int): ZJParameters = ZJParameters(
    nodeNidBits = 8,
    nodeAidBits = 3,
    nodeParams = nodeParams(numL2),
    djParamsOpt = Some(DJParam(llcSizeInB = 8 * 4 * 64, llcWays = 4, sfWays = 4)),
    tfbParams = None
  )

  /* Node-ID table for the cohestra CLog.B topology map, written next to the
   * RTL by the generator (zj_topology.txt). */
  def topologyLines(numL2: Int, aidBits: Int = 3): Seq[String] = {
    val ids = nodeParams(numL2).zipWithIndex.map { case (np, i) => (np, i << aidBits) }
    def idsOf(nt: Int) = ids.filter(_._1.nodeType == nt).map(_._2)
    Seq(
      "cc " + idsOf(NodeType.CC).mkString(" "),
      "hnf " + idsOf(NodeType.HF).mkString(" "),
      "snf " + idsOf(NodeType.S).mkString(" "),
      "hni " + idsOf(NodeType.HI).mkString(" "),
      "mn " + idsOf(NodeType.M).mkString(" ")
    )
  }
}

object TestTop_L2ZhuJiang extends App {

  val usage = """
Usage: TestTop_L2ZhuJiang [<--option> <values>]

      --l2 <n>                  number of L2 instances, 1..4, 4 by default;
                                each L2 takes the node ID of its ZhuJiang CC
                                node (see zj_topology.txt next to the RTL)
      --slices <slice_num>      specify the number of slices per L2, 2 by default;
                                external SAM supports 1 to 4 slices
      --noperf                  disable all DUT performance counters (drops the
                                LogPerfEndpoint bulk; much faster firtool/verilation)
      --cchi-nids <n,n,n,n>     upstream CCHI Type-1 node ID of each L2 (one
                                coherent client per L2); defaults to 0,1,2,3,
                                matching the harness's upstream.type1.nodeid
      --chi-mon <off|separate|packed|both>
                                CHI monitor port taps (mon_chi_rn{P}_*), packed
                                by default
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  var numL2 = 4
  var numSlices = 2
  var noPerf = false
  var cchiNIDsArg: Option[Seq[Int]] = None
  var chiMonArg: Option[String] = None

  def parseNIDList(value: String): Seq[Int] = value.split(",").map(_.trim.toInt).toIndexedSeq

  val varArgs = ArrayBuffer[String]()
  var i = 0
  while (i < args.length) {
    args(i) match {
      case "--l2"        => numL2 = args(i + 1).toInt; i += 2
      case "--slices"    => numSlices = args(i + 1).toInt; i += 2
      case "--noperf"    => noPerf = true; i += 1
      case "--cchi-nids" => cchiNIDsArg = Some(parseNIDList(args(i + 1))); i += 2
      case "--chi-mon"   => chiMonArg = Some(args(i + 1)); i += 2
      case other         => varArgs += other; i += 1
    }
  }
  varArgs.trimToSize()

  require(numL2 >= 1 && numL2 <= 4, s"Unsupported L2 count $numL2 (1..4)")
  require(numSlices >= 1 && numSlices <= 4, s"Unsupported slice count $numSlices under eSAM")

  val chiMon = chiMonArg match {
    case None | Some("packed")   => CHIMonitorMode.Packed
    case Some("off")             => CHIMonitorMode.Off
    case Some("separate")        => CHIMonitorMode.Separate
    case Some("both")            => CHIMonitorMode.Both
    case Some(other)             =>
      throw new IllegalArgumentException(s"Unknown --chi-mon '$other' (expected off|separate|packed|both)")
  }

  val cchiNIDs = cchiNIDsArg.getOrElse(Seq(0, 1, 2, 3))
  require(cchiNIDs.length >= numL2, s"--cchi-nids lists ${cchiNIDs.length} NIDs but $numL2 L2s are requested")

  // Keep in sync with the CHIParametersKey / CCHIParametersKey configs below.
  val chiNodeIdWidth = 11

  val cchiUpstreamNIDWidth = CCHIParameters().UpstreamNodeID_Width
  require(cchiNIDs.forall(n => n >= 0 && n < (1 << cchiUpstreamNIDWidth)),
    s"CCHI upstream NIDs must fit UpstreamNodeID_Width=$cchiUpstreamNIDWidth: $cchiNIDs")

  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params (
      physicalAddrWidth = 48,
      mshrSize = 16,
      ways = 4,
      sets = 32
    )
    case CHIParametersKey => CHIParameters (
      issue = EnumCHIIssue.E,
      nodeIdWidth = chiNodeIdWidth,
      reqAddrWidth = 48,
      reqRsvdcWidth = 4,
      datRsvdcWidth = 4,
      dataWidth = 256,
      dataCheckPresent = true,
      poisonPresent = true,
      mpamPresent = true
    )
    // CCHI parameters are the L2's own upstream contract and stay independent
    // of the AMBA CHI side.
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = !noPerf,
      fpgaPlatform = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions (
      enablePerfPrint = !noPerf,
      enablePerfDB = false,
      perfLevel = if (noPerf) XSPerfLevel.NORMAL else XSPerfLevel.VERBOSE,
      0
    )
    // ZhuJiang / xs.utils side (mirrors TestTopZhuJiangConfig)
    case ZJParametersKey => TestTop_L2ZhuJiangConfig.nocConfig(numL2)
    case HardwareAssertionKey => HwaParams(enable = false)
    case ZJLogUtilsOptionsKey => ZJLogUtilsOptions(
      enableDebug = false,
      enablePerf = !noPerf,
      fpgaPlatform = false
    )
    case ZJPerfCounterOptionsKey => ZJPerfCounterOptions(
      enablePerfPrint = !noPerf,
      enablePerfDB = false,
      perfLevel = if (noPerf) ZJXSPerfLevel.NORMAL else ZJXSPerfLevel.VERBOSE,
      perfDBHartID = 0
    )
    case DebugOptionsKey => DebugOptions(
      FPGAPlatform = false,
      EnableDifftest = false,
      AlwaysBasicDiff = false,
      EnableDebug = false,
      EnablePerfDebug = false,
      UseDRAMSim = false,
      EnableTopDown = false,
      EnableChiselDB = false,
      AlwaysBasicDB = false,
      EnableRollingDB = false,
      EnableHWMoniter = false
    )
  })

  (new ChiselStage).execute(varArgs.toArray,
    ChiselGeneratorAnnotation(() =>
      new TestTop_L2ZhuJiang(numL2, numSlices, cchiNIDs, chiMon)(config))
      +: TestTopFirtoolOptions())

  // Node-ID table for the cohestra CLog.B topology map, next to the RTL
  val tdIdx = varArgs.indexOf("-td")
  val td = if (tdIdx >= 0 && tdIdx + 1 < varArgs.length) varArgs(tdIdx + 1) else "./build"
  java.nio.file.Files.createDirectories(java.nio.file.Paths.get(td))
  java.nio.file.Files.write(
    java.nio.file.Paths.get(td, "zj_topology.txt"),
    (TestTop_L2ZhuJiangConfig.topologyLines(numL2).mkString("\n") + "\n").getBytes)
}
