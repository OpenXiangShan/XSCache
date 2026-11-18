// Intent (zy-pf-obs): elaborate the read-only observer against real L2 modules
// with both switch settings, using the existing XSPerf endpoint and controls.
package oceanus

import chisel3._
import chisel3.util._
import oceanus.l2._
import oceanus.chi._
import org.chipsalliance.cde.config._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import utility._

object TestTop_L2PrefetchObservation extends App {
  require(args.length >= 2, "usage: <on|off|baseline> <output.sv> [sets] [slices] [tshrs]")
  val perf = args(0) == "on"
  val attach = args(0) != "baseline"
  val sets = args.lift(2).map(_.toInt).getOrElse(16)
  val banks = args.lift(3).map(_.toInt).getOrElse(1)
  val tshrs = args.lift(4).map(_.toInt).getOrElse(4)
  Constantin.init(false)
  implicit val parameters: Parameters = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = tshrs, ways = 8, sets = sets)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11,
      reqAddrWidth = 48, reqRsvdcWidth = 4, datRsvdcWidth = 4, dataWidth = 256,
      dataCheckPresent = true, poisonPresent = true, mpamPresent = true)
    case CCHIParametersKey => CCHIParameters(WayIndex_Width = 3)
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = perf, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = perf,
      enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  val upstream = new L2UpstreamTable(Seq(
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 0),
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 1),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 4),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 5)))
  val cfg = new L2Configuration(0, true, 0 until banks, upstream = upstream,
    pfSrcID = 2, bopEnable = true, pfRecvEnable = true, pfTrainEnable = true,
    pfCtrlEnable = true, pfTlbEnable = true, vbopStubEnable = true, cdpStubEnable = true)
  class CheckedTop extends Module {
    val cache = Module(new L2Top(cfg))
    val io = IO(cache.io.cloneType)
    io <> cache.io
    val arrival = IO(Input(Vec(2, Valid(new oceanus.compactchi.FlitREQ))))
    val arrivalSource = IO(Input(Vec(2, UInt(5.W))))
    val clean = IO(Input(Bool()))
    val dump = IO(Input(Bool()))
    // Intent (zy-pf-obs): elaborate M9's address-dependent allocator taps as
    // well. These external arrivals describe the source buffers in this top.
    if (attach) L2PrefetchObservation(cache, arrival.zip(arrivalSource).map { case (a, src) =>
      L2ObservationArrival(a.valid, a.bits, src)
    }, arrival.zipWithIndex.map { case (a, lane) =>
      L2ObservationFifo(a.valid, a.bits.Addr, io.t1p(lane).UpREQ.fire, upstream.type1(lane).nid)
    })
    XSLog.collect(GTimer(), true.B, clean, dump)
  }
  // Intent (zy-pf-obs): a small raw-CHIRRTL fixture catches GSIM grammar
  // compatibility before paying for another whole-core model generation.
  val sv = if (args(1).endsWith(".fir")) _root_.circt.stage.ChiselStage.emitCHIRRTL(new CheckedTop)
    else _root_.circt.stage.ChiselStage.emitSystemVerilog(new CheckedTop,
      firtoolOpts = Array("--disable-all-randomization", "--strip-debug-info"))
  val output = java.nio.file.Paths.get(args(1))
  java.nio.file.Files.createDirectories(output.getParent)
  java.nio.file.Files.writeString(output, sv)
  println(s"PFOBS_ELAB mode=${args(0)} sets=$sets slices=$banks tshrs=$tshrs")
}
