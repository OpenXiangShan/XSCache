package oceanus

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import utility._
import utility.sram.{SramBroadcastBundle, SramHelper}
import oceanus.l2._
import oceanus.chi._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable.ArrayBuffer

/* Release-oriented generation top for the Oceanus L2 (L2Top), configured after
 * the Kunminghu V2 DefaultConfig (XiangShan top/Configs.scala):
 *  - 2MB L2 in 4 slices (banks): 1024 sets x 8 ways x 64B per slice.
 *    The 8-way DRRIP configuration matches Kunminghu V2's L2 associativity
 *    and CoupledL2's default replacement policy.
 *  - mshrSize = 32
 *  - 48-bit physical address, CHI Issue E, 256-bit data width
 *  - data ECC (secded) enabled
 *  - MBIST enabled, SRAM ctl disabled (Kunminghu DFTOptions defaults)
 *  - performance, debug, and trace instrumentation disabled
 *
 * Firtool options follow the XiangShan release flow (XiangShan Makefile
 * MFC_ARGS), so the generated Verilog carries no location-info comments and no
 * shared port declarations, and is parseable by XiangShan scripts/parser.py.
 */
class Top_L2Top(val l2cfg: L2Configuration, val withPerf: Boolean = false)(implicit val p: Parameters) extends Module with HasL2Params {

  val module = Module(new L2Top(l2cfg))

  val io = IO(module.io.cloneType)

  io <> module.io

  // DFT broadcast for the mbist-enabled SRAMs: genRam marks every SRAM
  // broadcast bundle DontCare and queues it (SramHelper.scala:174-180);
  // collect and drive them all from this top-level input — the same
  // mechanism coupledL2/XiangShan use (their cache top calls
  // genBroadCastBundleTop and forwards io.dft).
  val dft = IO(Input(new SramBroadcastBundle))
  SramHelper.genBroadCastBundleTop() := dft

  // Performance instrumentation (--perf): same collect pattern as
  // TestTop_L2Top. The perf counter registers elaborate only when
  // enablePerfPrint is set, which the App ties to withPerf.
  if (withPerf) {
    val log = IO(new Bundle {
      val dump = Input(Bool())
      val clean = Input(Bool())
    })

    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    timer := 0.U
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

object Top_L2Top extends App {

  val usage = """
Usage: Top_L2Top [<--option> <values>]

      --slices <slice_num>      specify the number of L2 slices, 4 by default;
                                external SAM supports 1 to 4 slices
      --tshr <tshr_num>         specify the number of TSHRs (mshrSize), 32 by
                                default; must be >= 4, power of two recommended
      --l2-drrip-release-update <0|1>
                                enable DRRIP release updates through DirRel,
                                1 (on) by default
      --l2-drrip-use-sram <0|1> select the RRIP SRAM+CAM backend (1, default)
                                or the FF backend (0)
      --l2-origin-use-sram <0|1>
                                use origin SRAM (1, default) or origin FF (0)
      --l2-dirrel-arbitration <mode>
                                dirrel-high, rr (default), or dirrel-low
      --l2-dirrel-single-cycle <0|1>
                                enable single-cycle DirRel (0, default); requires
                                RRIP SRAM with origin FF
      --perf                    enable all performance counters (XSPerf) and
                                expose the log dump/clean pins; off by default
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  // --perf is a bare flag; drop it before the sliding two-arg option parse
  val genPerf = args.contains("--perf")
  val optArgs = if (genPerf) args.filterNot(_ == "--perf") else args

  var varArgs = ArrayBuffer(optArgs.toIndexedSeq:_*)
  var varArgsDropped = 0

  var numSlices = 4
  var numTshr = 32
  var drripUseSRAM = true
  var originUseSRAM = true
  var enableDRRIPReleaseUpdate = true
  var dirRelArbitration = "rr"
  var dirRelSingleCycle = false

  def parseBoolean(option: String, value: String): Boolean = {
    require(value == "0" || value == "1", s"$option expects 0 or 1, got $value")
    value == "1"
  }

  val varArgsToDrop = optArgs.sliding(2, 1).zipWithIndex.collect {
    case (Array("--slices", value), i) => (numSlices = value.toInt, i)
    case (Array("--tshr", value), i) => (numTshr = value.toInt, i)
    case (Array("--l2-drrip-release-update", value), i) =>
      (enableDRRIPReleaseUpdate = parseBoolean("--l2-drrip-release-update", value), i)
    case (Array("--l2-drrip-use-sram", value), i) =>
      (drripUseSRAM = parseBoolean("--l2-drrip-use-sram", value), i)
    case (Array("--l2-dirrel-arbitration", value), i) => (dirRelArbitration = value, i)
    case (Array("--l2-origin-use-sram", value), i) =>
      (originUseSRAM = parseBoolean("--l2-origin-use-sram", value), i)
    case (Array("--l2-dirrel-single-cycle", value), i) =>
      (dirRelSingleCycle = parseBoolean("--l2-dirrel-single-cycle", value), i)
  }

  varArgsToDrop.map(_._2).foreach(i => {
    varArgs.remove(i - varArgsDropped, 2)
    varArgsDropped = varArgsDropped + 2
  })
  varArgs.trimToSize()

  require(numSlices >= 1 && numSlices <= 4, s"Unsupported slice count $numSlices under eSAM")
  require(numTshr >= 4, s"TSHR count $numTshr too small (reservation table needs >= 4)")

  val l2cfg = new L2Configuration(
    nodeId = 0,
    eSAM = true,
    slices = 0 until numSlices
  )

  // Kunminghu V2 DefaultConfig alignment, see the class comment above
  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params (
      physicalAddrWidth = 48,
      mshrSize = numTshr,
      ways = 8,
      sets = 1024,
      replacement = "drrip",
      blockBytes = 64,
      dataSRAMSplit = 4,
      enableDataECC = true,
      hasMbist = true,
      hasSramCtl = false,
      dataECC = Some("secded"),
      drripUseSRAM = drripUseSRAM,
      originUseSRAM = originUseSRAM,
      enableDRRIPReleaseUpdate = enableDRRIPReleaseUpdate,
      dirRelArbitration = dirRelArbitration,
      dirRelSingleCycle = dirRelSingleCycle
    )
    case CHIParametersKey => CHIParameters (
      issue = EnumCHIIssue.E,
      nodeIdWidth = 11,
      reqAddrWidth = 48,
      dataWidth = 256
    )
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = genPerf,
      fpgaPlatform = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions (
      enablePerfPrint = genPerf,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE,
      0
    )
  })

  // Same firtool options as the XiangShan release flow (XiangShan Makefile MFC_ARGS)
  val releaseFirtoolOptions = Seq(
    FirtoolOption("-O=release"),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")
  )

  (new ChiselStage).execute(varArgs.toArray,
    ChiselGeneratorAnnotation(() => new Top_L2Top(l2cfg, genPerf)(config)) +: releaseFirtoolOptions)
}
