package oceanus

import chisel3._
import oceanus.l2._
import oceanus.chi._
import org.chipsalliance.cde.config._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import utility._

// Exercise production L2Top counters with both receiver routing modes and
// production queue depths, independently of the external F1/M9 fixtures.
object TestTop_L2QueueObservationElaboration extends App {
  require(args.length == 2, "usage: <stub|legacy> <output.fir|output.sv>")
  require(Set("stub", "legacy").contains(args(0)))
  Constantin.init(false)
  implicit val parameters: Parameters = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 4, ways = 8, sets = 16)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11,
      reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters(WayIndex_Width = 3)
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(true, false, XSPerfLevel.VERBOSE, 0)
  })
  val stub = args(0) == "stub"
  val config = new L2Configuration(8, true, 0 until 2, bopEnable = true,
    pfRecvEnable = true, pfCtrlEnable = true, pfTrainEnable = stub, pfTlbEnable = stub,
    vbopStubEnable = stub, cdpStubEnable = stub)
  class CheckedTop extends Module {
    val cache = Module(new L2Top(config))
    val io = IO(cache.io.cloneType)
    io <> cache.io
    val clean = IO(Input(Bool()))
    val dump = IO(Input(Bool()))
    XSLog.collect(0.U(64.W), true.B, clean, dump)
  }
  val output = java.nio.file.Paths.get(args(1))
  java.nio.file.Files.createDirectories(output.getParent)
  val emitted = if (args(1).endsWith(".fir")) _root_.circt.stage.ChiselStage.emitCHIRRTL(new CheckedTop)
    else _root_.circt.stage.ChiselStage.emitSystemVerilog(new CheckedTop,
      firtoolOpts = Array("--disable-all-randomization", "--strip-debug-info"))
  java.nio.file.Files.writeString(output, emitted)
  println(s"QOBS_ELAB pass=true mode=${args(0)} slices=2 depths=16,32")
}
