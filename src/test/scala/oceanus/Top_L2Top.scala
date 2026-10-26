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
class Top_L2Top(val l2cfg: L2Configuration)(implicit val p: Parameters) extends Module with HasL2Params {

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
}

object Top_L2Top extends App {

  val usage = """
Usage: Top_L2Top [<--option> <values>]

      --slices <slice_num>      specify the number of L2 slices, 4 by default;
                                external SAM supports 1 to 4 slices
      --tshr <tshr_num>         specify the number of TSHRs (mshrSize), 32 by
                                default; must be >= 4, power of two recommended
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  var varArgs = ArrayBuffer(args.toIndexedSeq:_*)
  var varArgsDropped = 0

  var numSlices = 4
  var numTshr = 32

  val varArgsToDrop = args.sliding(2, 1).zipWithIndex.collect {
    case (Array("--slices", value), i) => (numSlices = value.toInt, i)
    case (Array("--tshr", value), i) => (numTshr = value.toInt, i)
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
      dataSRAMSplit = 8,
      enableDataECC = true,
      hasMbist = true,
      hasSramCtl = false,
      dataECC = Some("secded")
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
      enablePerf = false,
      fpgaPlatform = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions (
      enablePerfPrint = false,
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
    ChiselGeneratorAnnotation(() => new Top_L2Top(l2cfg)(config)) +: releaseFirtoolOptions)
}
