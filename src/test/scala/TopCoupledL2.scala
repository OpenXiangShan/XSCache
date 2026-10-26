package xscache.coupledL2

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.MaxHartIdBits
import xscache.coupledL2.prefetch._
import utility._
import utility.chiron._
import utility.sram.SramBroadcastBundle
import xscache.common.{AliasField, BankBitsKey, DirtyField, PrefetchField}
import xscache.chi._

/* Release-oriented generation top for CoupledL2, configured after the Kunminghu
 * V2 DefaultConfig cache/DFT parameters (XiangShan top/Configs.scala:
 * L2CacheConfig("2MB", banks=4) on top of BaseConfig):
 *  - 2MB L2 in 4 banks: 1024 sets x 8 ways x 64B per bank
 *  - 16 MSHRs (coupledL2 default)
 *  - client: 64KB 4-way L1D model (128 sets, 6 ways, 2 alias bits, 44-bit vaddr)
 *  - tag+data ECC (secded) on, CHI poison/oddparity data check on
 *  - prefetch: BOP + CDP + prefetch receiver
 *  - CHI downstream Issue E.b, 48-bit address
 *  - MBIST enabled (dft/dft_reset exposed at top, as in XiangShan's L2Top),
 *    SRAM ctl disabled
 *  - performance, debug, and trace instrumentation disabled
 * The L1-facing TileLink slave port (tl_in) and the CHI RN-F port (chi) are the
 * top-level interfaces.
 *
 * Firtool options follow the XiangShan release flow (XiangShan Makefile
 * MFC_ARGS), so the generated Verilog carries no location-info comments and no
 * shared port declarations, and is parseable by scripts/parser.py.
 */
class TopCoupledL2(banks: Int = 4)(implicit p: Parameters) extends LazyModule
  with HasCHIMsgParameters {

  override lazy val desiredName: String = "TopCoupledL2"
  val cacheParams = p(L2ParamKey)

  val l2 = LazyModule(new CoupledL2())

  // Boundary node for the L1-facing TL slave port: the client-node-at-boundary
  // idiom (rocket-chip CanHaveSlaveTLPort); makeIOs exposes it as a slave port.
  // Request user fields mirror the Kunminghu L1D channel (Prefetch/ReqSource/
  // Vaddr/Alias).
  val tlIn = TLClientNode(Seq(
    TLMasterPortParameters.v2(
      masters = Seq(
        TLMasterParameters.v1(
          name = "l1",
          sourceId = IdRange(0, 64),
          supportsProbe = TransferSizes(cacheParams.blockBytes)
        )
      ),
      channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
      minLatency = 1,
      echoFields = Nil,
      requestFields = Seq(PrefetchField(), ReqSourceField(), VaddrField(44), AliasField(2)),
      responseKeys = cacheParams.respKey
    )
  ))

  val l1xbar = TLXbar()
  l2.managerNode := TLXbar() :=* BankBinder(banks, 64) :*= l2.node :*= l1xbar
  l1xbar := tlIn

  // MMIO (uncached) access port, same boundary idiom
  val mmioIn = TLClientNode(Seq(
    TLMasterPortParameters.v1(
      clients = Seq(TLMasterParameters.v1(
        name = "uncache",
        sourceId = IdRange(0, 16)
      ))
    )
  ))
  l2.mmioBridge.mmioNode := mmioIn

  // Prefetch receiver ports (present under the Kunminghu prefetch config):
  // drive sink nodes from sources whose payloads become top-level inputs
  val pfRecvSrc = l2.pf_recv_node.map { sink =>
    val src = BundleBridgeSource(Some(() => new PrefetchRecv()))
    sink := src
    src
  }
  val l3PfRecvSrc = l2.l3_pf_recv_node.map { sink =>
    val src = BundleBridgeSource(Some(() => new PrefetchRecv()))
    sink := src
    src
  }

  lazy val module = new LazyModuleImp(this) {

    val tl_in = tlIn.makeIOs()(ValName("tl_in"))
    val mmio = mmioIn.makeIOs()(ValName("mmio_port"))
    val pf_recv = pfRecvSrc.map(_.makeIO()(ValName("pf_recv")))
    val l3_pf_recv = l3PfRecvSrc.map(_.makeIO()(ValName("l3_pf_recv")))

    val io = IO(new Bundle {
      val chi = new PortIO
      val nodeID = Input(UInt(NODEID_WIDTH.W))
      val dft = Input(new SramBroadcastBundle)
      val dft_reset = Input(new DFTResetSignals)
      val l2_hint = Output(chiselTypeOf(l2.module.io.l2_hint))
      val l2_fdbk_pf_ctrl = Output(chiselTypeOf(l2.module.io.l2_fdbk_pf_ctrl))
      val l2Miss = Output(Bool())
      val error = Output(chiselTypeOf(l2.module.io.error))
    })

    l2.module.io.lcreditCHI.get <> io.chi
    l2.module.io.nodeID := io.nodeID
    l2.module.io.dft.get := io.dft
    l2.module.io.dft_reset.get := io.dft_reset
    io.l2_hint <> l2.module.io.l2_hint
    io.l2_fdbk_pf_ctrl <> l2.module.io.l2_fdbk_pf_ctrl
    io.l2Miss := l2.module.io.l2Miss
    io.error <> l2.module.io.error

    l2.module.io.hartId := 0.U
    l2.module.io.pfCtrlFromCore := DontCare
    l2.module.io.debugTopDown := DontCare
    l2.module.io.l2_tlb_req := DontCare

    dontTouch(l2.module.io)
  }
}

object TopCoupledL2 extends App {

  val usage = """
Usage: TopCoupledL2 [<chisel/firtool options>]

  Generates the release RTL of CoupledL2 with the Kunminghu V2 DefaultConfig
  parameters (2MB, 4 banks, 8 ways, 16 MSHRs, MBIST on). No custom options.
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  // Kunminghu V2 DefaultConfig alignment, see the class comment above
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      name                = "L2",
      ways                = 8,
      sets                = 1024,
      blockBytes          = 64,
      clientCaches        = Seq(L1Param(
        name              = "dcache",
        sets              = 128,
        ways              = 6,
        aliasBitsOpt      = Some(2),
        vaddrBitsOpt      = Some(44)
      )),
      mshrs               = 16,
      echoField           = Seq(DirtyField()),
      reqField            = Seq(ReqSourceField()),
      tagECC              = Some("secded"),
      dataECC             = Some("secded"),
      enableTagECC        = true,
      enableDataECC       = true,
      dataCheck           = Some("oddparity"),
      enablePoison        = true,
      prefetch            = Seq(BOPParameters(), CDPParameters(), PrefetchReceiverParams()),
      enableL2Flush       = false,
      enablePerf          = false,
      enableRollingDB     = false,
      enableMonitor       = false,
      enableTLLog         = false,
      enableCHILog        = false,
      elaboratedTopDown   = false,
      hasMbist            = true,
      hasSramCtl          = false,
      FPGAPlatform        = false
    )
    case CHIIssue => "E.b"
    case BankBitsKey => 2
    case MaxHartIdBits => 0
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = false,
      fpgaPlatform = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE,
      0
    )
  })

  CLogB.init(false)
  ChiselDB.init(false)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(new TopCoupledL2()(p)))(config)

  // Same firtool options as the XiangShan release flow (XiangShan Makefile MFC_ARGS)
  val releaseFirtoolOptions = Seq(
    FirtoolOption("-O=release"),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")
  )

  (new ChiselStage).execute(args,
    ChiselGeneratorAnnotation(() => top.module) +: releaseFirtoolOptions)

  FileRegisters.write("./build")
}
