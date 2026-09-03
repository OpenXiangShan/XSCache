package oceanus

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config._
import scala.collection.mutable.ArrayBuffer
import utility._
import oceanus.l2.{L2Configuration, L2Params, L2ParamsKey, L2Top}
import oceanus.chi.{CHIParameters, CHIParametersKey, EnumCHIChannel, EnumCHIIssue}
import oceanus.chi.bundle.{CHIBundleDAT, CHIBundleREQ, CHIBundleRSP, CHIBundleSNP}
import oceanus.chi.link.OceanusChannelAdapter
import xscache.chi.{ChannelIO, CHIIssue, HasCHIMsgParameters, Issue}
import xscache.coupledL2.L2Param
import xscache.openLLC.{OpenLLC, OpenLLCParam, OpenLLCParamKey, OpenNCB}
import cc.xiangshan.openncb.{EnumAXIMasterOrder, EnumCHIDataCheck, NCBParameters, NCBParametersKey}

/*  Oceanus L2 (CCHI upstream / CHI RN-F downstream) -> OpenLLC -> OpenNCB -> AXI4.
 *
 *       CCHI upstream ports (exported, one Type-1 + two Type-4 per L2)
 *                     |
 *                L2Top x numL2
 *                     |  CHIRNFInterface (oceanus.chi bundles)
 *           [OceanusChannelAdapter]  (flit-width matched configs, see below)
 *                     |  PortIO (xscache.chi legacy bundles)
 *                 OpenLLC rn(i)
 *                     |
 *                 OpenLLC sn -> OpenNCB -> AXI4 (exported as axi_m0_*)
 *
 *  The oceanus CHIParameters below is pinned so that every oceanus flit is
 *  bit-width-identical to its legacy Eb counterpart (REQ 162 / SNP 115 /
 *  RSP 73 / DAT 422); OceanusChannelAdapter re-slices the raw flits.
 *
 *  Top-level port naming follows the Cohestra V3 contract (CHIron
 *  cchi/cohestra/cohestra_v3), exported pin-by-pin via suggestName:
 *    cchi_t1p{P}_{rxevt,rxreq,txsnp,txrsp,rxrsp,txdat,rxdat}_{valid,ready,bits_*}
 *      - one Type-1 port per L2; DUT inputs on rx*, DUT outputs on tx*
 *    cchi_t4p{2i}/cchi_t4p{2i+1}_{rxreq,txdat}_*
 *      - the two Type-4 ports of L2 i (no cohestra concept yet; same scheme)
 *    axi_m0_{awvalid,awready,awid,awaddr,awlen,awsize,awburst,
 *            wvalid,wready,wdata,wstrb,wlast,
 *            bvalid,bready,bid,bresp,
 *            arvalid,arready,arid,araddr,arlen,arsize,arburst,
 *            rvalid,rready,rid,rdata,rresp,rlast}
 *      - flat AXI4 master port toward memory (DUT is the master)
 *    clock / reset (high-active)
 *  Internal CCHI channel names (UpEVT/DnSNP/...) intentionally do not appear
 *  in the emitted port names.
 */
class TestTop_L2OpenLLC(val numL2: Int, val numSlices: Int)(implicit p: Parameters) extends LazyModule
  with HasCHIMsgParameters {

  override lazy val desiredName: String = "TestTop_L2OpenLLC"

  val l3Bridge = LazyModule(new OpenNCB())

  val mem = new AXI4SlaveNode(Seq(new AXI4SlavePortParameters(
    slaves = Seq(new AXI4SlaveParameters(
      address = Seq(new AddressSet(0, 0xffff_ffffL)),
      supportsWrite = new TransferSizes(1, 64),
      supportsRead = new TransferSizes(1, 64)
    )),
    beatBytes = 32
  )))

  mem :=
    AXI4Xbar() :=
    l3Bridge.axi4node

  lazy val module = new LazyModuleImp(this) {

    val l2cfg = new L2Configuration(nodeId = 0, eSAM = true, slices = 0 until numSlices)

    val l2s = Seq.fill(numL2)(Module(new L2Top(l2cfg)))
    val l3 = Module(new OpenLLC())

    // -- Cohestra V3 pin-level export ------------------------------------------
    // Every external pin is an individual IO named via suggestName, so emitted
    // port names follow the cohestra_v3 contract (see header) regardless of how
    // the internal bundles/channels are named.

    /* Export a Decoupled channel as {prefix}_valid / {prefix}_ready /
     * {prefix}_bits_<field> pins. dutDrives = true for channels the DUT
     * sources (valid/bits are outputs, ready is an input). */
    def exportChannel[T <: Bundle](prefix: String, chan: ReadyValidIO[T], dutDrives: Boolean): Unit = {
      val vld = IO(if (dutDrives) Output(Bool()) else Input(Bool())).suggestName(s"${prefix}_valid")
      val rdy = IO(if (dutDrives) Input(Bool()) else Output(Bool())).suggestName(s"${prefix}_ready")
      if (dutDrives) { vld := chan.valid; chan.ready := rdy }
      else           { chan.valid := vld; rdy := chan.ready }
      chan.bits.elements.foreach { case (name, field) =>
        val pin = IO(if (dutDrives) Output(chiselTypeOf(field)) else Input(chiselTypeOf(field)))
          .suggestName(s"${prefix}_bits_${name}")
        if (dutDrives) pin := field else field := pin
      }
    }

    /* Export an AXI4 channel as flat axi_m0_<chan><sig> pins over a field
     * whitelist (lock/cache/prot/qos/user/echo sidebands stay internal). */
    def exportAXIChannel[T <: Bundle](chan: ReadyValidIO[T], chanName: String,
                                      fields: Seq[String], dutDrives: Boolean): Unit = {
      val vld = IO(if (dutDrives) Output(Bool()) else Input(Bool())).suggestName(s"axi_m0_${chanName}valid")
      val rdy = IO(if (dutDrives) Input(Bool()) else Output(Bool())).suggestName(s"axi_m0_${chanName}ready")
      if (dutDrives) { vld := chan.valid; chan.ready := rdy }
      else           { chan.valid := vld; rdy := chan.ready }
      fields.foreach { name =>
        val field = chan.bits.elements(name)
        val pin = IO(if (dutDrives) Output(chiselTypeOf(field)) else Input(chiselTypeOf(field)))
          .suggestName(s"axi_m0_${chanName}${name}")
        if (dutDrives) pin := field else field := pin
      }
    }

    // -- Exported upstream CCHI ports, one Type-1 + two Type-4 per L2
    l2s.zipWithIndex.foreach { case (l2, i) =>
      exportChannel(s"cchi_t1p${i}_rxevt", l2.io.t1p0.UpEVT, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_rxreq", l2.io.t1p0.UpREQ, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_txsnp", l2.io.t1p0.DnSNP, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_txrsp", l2.io.t1p0.DnRSP, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_rxrsp", l2.io.t1p0.UpRSP, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_txdat", l2.io.t1p0.DnDAT, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_rxdat", l2.io.t1p0.UpDAT, dutDrives = false)

      exportChannel(s"cchi_t4p${2*i}_rxreq",   l2.io.t4p0.UpREQ, dutDrives = false)
      exportChannel(s"cchi_t4p${2*i}_txdat",   l2.io.t4p0.DnDAT, dutDrives = true)
      exportChannel(s"cchi_t4p${2*i+1}_rxreq", l2.io.t4p1.UpREQ, dutDrives = false)
      exportChannel(s"cchi_t4p${2*i+1}_txdat", l2.io.t4p1.DnDAT, dutDrives = true)
    }

    // -- Downstream CHI: oceanus CHIRNFInterface <-> legacy PortIO via OceanusChannelAdapter
    l2s.zipWithIndex.foreach { case (l2, i) =>
      val rn = l3.io.rn(i)

      // L2 TX -> L3 (oceanus channel source -> ChannelIO sink)
      val adpTxReq = Wire(ChannelIO(new CHIBundleREQ))
      OceanusChannelAdapter.connectRX(l2.io.chi.txreq, adpTxReq, EnumCHIChannel.REQ)
      rn.tx.req.flitpend := adpTxReq.flitpend
      rn.tx.req.flitv    := adpTxReq.flitv
      rn.tx.req.flit     := adpTxReq.flit
      adpTxReq.lcrdv     := rn.tx.req.lcrdv

      val adpTxRsp = Wire(ChannelIO(new CHIBundleRSP))
      OceanusChannelAdapter.connectRX(l2.io.chi.txrsp, adpTxRsp, EnumCHIChannel.RSP)
      rn.tx.rsp.flitpend := adpTxRsp.flitpend
      rn.tx.rsp.flitv    := adpTxRsp.flitv
      rn.tx.rsp.flit     := adpTxRsp.flit
      adpTxRsp.lcrdv     := rn.tx.rsp.lcrdv

      val adpTxDat = Wire(ChannelIO(new CHIBundleDAT))
      OceanusChannelAdapter.connectRX(l2.io.chi.txdat, adpTxDat, EnumCHIChannel.DAT)
      rn.tx.dat.flitpend := adpTxDat.flitpend
      rn.tx.dat.flitv    := adpTxDat.flitv
      rn.tx.dat.flit     := adpTxDat.flit
      adpTxDat.lcrdv     := rn.tx.dat.lcrdv

      // L3 -> L2 RX (ChannelIO source -> oceanus channel sink)
      val adpRxSnp = Wire(ChannelIO(new CHIBundleSNP))
      adpRxSnp.flitpend := rn.rx.snp.flitpend
      adpRxSnp.flitv    := rn.rx.snp.flitv
      adpRxSnp.flit     := rn.rx.snp.flit
      rn.rx.snp.lcrdv   := adpRxSnp.lcrdv
      OceanusChannelAdapter.connectTX(adpRxSnp, l2.io.chi.rxsnp, EnumCHIChannel.SNP)

      val adpRxRsp = Wire(ChannelIO(new CHIBundleRSP))
      adpRxRsp.flitpend := rn.rx.rsp.flitpend
      adpRxRsp.flitv    := rn.rx.rsp.flitv
      adpRxRsp.flit     := rn.rx.rsp.flit
      rn.rx.rsp.lcrdv   := adpRxRsp.lcrdv
      OceanusChannelAdapter.connectTX(adpRxRsp, l2.io.chi.rxrsp, EnumCHIChannel.RSP)

      val adpRxDat = Wire(ChannelIO(new CHIBundleDAT))
      adpRxDat.flitpend := rn.rx.dat.flitpend
      adpRxDat.flitv    := rn.rx.dat.flitv
      adpRxDat.flit     := rn.rx.dat.flit
      rn.rx.dat.lcrdv   := adpRxDat.lcrdv
      OceanusChannelAdapter.connectTX(adpRxDat, l2.io.chi.rxdat, EnumCHIChannel.DAT)

      // link-active / sactive / sysco
      rn.tx.linkactivereq       := l2.io.chi.txlinkactivereq
      l2.io.chi.txlinkactiveack := rn.tx.linkactiveack
      l2.io.chi.rxlinkactivereq := rn.rx.linkactivereq
      rn.rx.linkactiveack       := l2.io.chi.rxlinkactiveack
      rn.txsactive              := l2.io.chi.txsactive
      l2.io.chi.rxsactive       := rn.rxsactive
      rn.syscoreq               := l2.io.chi.syscoreq
      l2.io.chi.syscoack        := rn.syscoack
    }

    // -- L3 downstream SN port -> OpenNCB -> AXI4
    l3.io.sn <> l3Bridge.module.io.chi
    l3.io.nodeID := numL2.U(NODEID_WIDTH.W)
    l3.io.debugTopDown := DontCare

    // -- AXI4 memory master, flat cohestra_v3 naming (axi_m0_*); DUT is the master
    val memAxi = mem.in.head._1
    exportAXIChannel(memAxi.aw, "aw", Seq("id", "addr", "len", "size", "burst"), dutDrives = true)
    exportAXIChannel(memAxi.w,  "w",  Seq("data", "strb", "last"),               dutDrives = true)
    exportAXIChannel(memAxi.b,  "b",  Seq("id", "resp"),                         dutDrives = false)
    exportAXIChannel(memAxi.ar, "ar", Seq("id", "addr", "len", "size", "burst"), dutDrives = true)
    exportAXIChannel(memAxi.r,  "r",  Seq("id", "data", "resp", "last"),         dutDrives = false)

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
}

object TestTop_L2OpenLLC extends App {

  val usage = """
Usage: TestTop_L2OpenLLC [<--option> <values>]

      --l2 <l2_num>             specify the number of Oceanus L2 instances, 1 by default
      --slices <slice_num>      specify the number of slices per L2, 2 by default;
                                external SAM supports 1 to 4 slices
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  var varArgs = ArrayBuffer(args.toIndexedSeq:_*)
  var varArgsDropped = 0

  var numL2 = 1
  var numSlices = 2

  val varArgsToDrop = args.sliding(2, 1).zipWithIndex.collect {
    case (Array("--l2", value), i) => (numL2 = value.toInt, i)
    case (Array("--slices", value), i) => (numSlices = value.toInt, i)
  }

  varArgsToDrop.map(_._2).foreach(i => {
    varArgs.remove(i - varArgsDropped, 2)
    varArgsDropped = varArgsDropped + 2
  })
  varArgs.trimToSize()

  require(numL2 >= 1, s"Unsupported L2 count $numL2")
  require(numSlices >= 1 && numSlices <= 4, s"Unsupported slice count $numSlices under eSAM")

  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params (
      physicalAddrWidth = 48,
      mshrSize = 16,
      ways = 4,
      sets = 32
    )
    case CHIParametersKey => CHIParameters (
      issue = EnumCHIIssue.E,
      nodeIdWidth = 11,
      reqAddrWidth = 48,
      reqRsvdcWidth = 4,
      datRsvdcWidth = 4,
      dataWidth = 256,
      dataCheckPresent = true,
      poisonPresent = true,
      mpamPresent = true
    )
    case CHIIssue => Issue.Eb
    // CHIAddrWidthKey / CHIDataCheckKey / CHIPoisonKey defaults (48 / oddparity / true)
    // already match the oceanus CHIParameters above.
    case OpenLLCParamKey => OpenLLCParam(
      ways = 4,
      sets = 64,
      banks = 1,
      clientCaches = Seq.fill(numL2)(L2Param(ways = 4, sets = 32)),
      fullAddressBits = 48,
      hartIds = 0 until numL2,
      enableRollingDB = false,
      enableCHILog = false
    )
    case NCBParametersKey => new NCBParameters(
      axiMasterOrder      = EnumAXIMasterOrder.WriteAddress,
      readCompDMT         = false,
      writeCancelable     = false,
      writeNoError        = true,
      axiBurstAlwaysIncr  = true,
      chiDataCheck        = EnumCHIDataCheck.OddParity
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = true,
      fpgaPlatform = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions (
      enablePerfPrint = true,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE,
      0
    )
  })

  val top = DisableMonitors(p => LazyModule(new TestTop_L2OpenLLC(numL2, numSlices)(p)))(config)

  (new ChiselStage).execute(varArgs.toArray,
    ChiselGeneratorAnnotation(() => top.module) +: TestTopFirtoolOptions())
}
