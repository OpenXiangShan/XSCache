package oceanus

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import utility._
import oceanus.l2._
import oceanus.chi._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable.ArrayBuffer

object L2TopPhase1aPorts {
  def enabled: Boolean = sys.props.get("oceanus.phase1a.ports").contains("true")
}

// Phase 1a acceptance uses a test-only JVM property to elaborate both optional
// interfaces in every existing production-path harness. Inputs remain idle;
// test assertions check the exposed translation interface throughout each run.
class TestTop_L2Top(val l2cfg: L2Configuration)(implicit val p: Parameters) extends Module with HasL2Params {

  val module = Module(new L2Top(l2cfg))
  val observation = L2TopObservation.attach(module, nMSHR)

  val io = IO(module.io.cloneType)

  io <> module.io

  require(l2cfg.pfTlbEnable == L2TopPhase1aPorts.enabled)
  require(l2cfg.pfTrainEnable == L2TopPhase1aPorts.enabled)
  println(s"PHASE1A_PORTS tlb=${io.pfTlb.nonEmpty} train=${io.pfTrain.nonEmpty} type1=${io.t1p.size}")
  io.pfTlb.foreach { tlb =>
    assert(!tlb.req.valid && !tlb.req_kill && tlb.resp.ready,
      "Phase 1a TLB must remain idle and discard responses")
  }
  io.pfTrain.foreach { ports =>
    require(ports.size == io.t1p.size)
    ports.foreach { train =>
      require(train.vaddrBlock.getWidth == 44 && train.reqSource.getWidth == 5 && train.trainEnable.getWidth == 1)
    }
  }

  //
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

object TestTop_L2Top extends App {
  L2TopCases.registerResponseModel(L2TopBuiltInResponseModel)
  L2TopCases.dispatch(args)

  val usage = """
Usage: TestTop_L2Top [<--option> <values>]

      --slices <slice_num>      specify the number of L2 slices, 2 by default;
                                external SAM supports 1 to 4 slices
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  var varArgs = ArrayBuffer(args.toIndexedSeq:_*)
  var varArgsDropped = 0

  var numSlices = 2

  val varArgsToDrop = args.sliding(2, 1).zipWithIndex.collect {
    case (Array("--slices", value), i) => (numSlices = value.toInt, i)
  }

  varArgsToDrop.map(_._2).foreach(i => {
    varArgs.remove(i - varArgsDropped, 2)
    varArgsDropped = varArgsDropped + 2
  })
  varArgs.trimToSize()

  require(numSlices >= 1 && numSlices <= 4, s"Unsupported slice count $numSlices under eSAM")

  val l2cfg = new L2Configuration(
    nodeId = 0,
    eSAM = true,
    slices = 0 until numSlices,
    // This BOP observation harness preserves its enabled elaboration configuration.
    bopEnable = true,
    pfTlbEnable = L2TopPhase1aPorts.enabled,
    pfTrainEnable = L2TopPhase1aPorts.enabled
  )

  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params (
      physicalAddrWidth = 48,
      mshrSize = 8
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

  (new ChiselStage).execute(varArgs.toArray,
    ChiselGeneratorAnnotation(() => new TestTop_L2Top(l2cfg)(config)) +: TestTopFirtoolOptions())
}
