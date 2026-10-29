package oceanus

import chisel3._
import oceanus.l2._
import oceanus.chi._
import org.chipsalliance.cde.config._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import utility._

// R06: compile/elaborate one producer at a time with the actual core geometry.
// This checks construction and initialized DFT inputs; it does not claim MBIST scan coverage.
object TestTop_L2PrefetchKMH3Elaboration extends App {
  Constantin.init(false)
  require(args.length == 3, "usage: <bop|vbop|cdp> <mbist:true|false> <output.sv>")
  val source = args(0)
  require(Set("bop", "vbop", "cdp").contains(source))
  val mbist = args(1).toBoolean
  implicit val parameters: Parameters = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(physicalAddrWidth = 48, mshrSize = 32,
      ways = 8, sets = 1024, blockBytes = 64, dataSRAMSplit = 8,
      enableDataECC = true, dataECC = Some("secded"), hasMbist = mbist,
      hasSramCtl = false, replacement = "drrip",
      enableDRRIPReleaseUpdate = true, enableDRRIPEntranceReleaseUpdate = true)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11,
      reqAddrWidth = 48, reqRsvdcWidth = 4, datRsvdcWidth = 4, dataWidth = 256,
      dataCheckPresent = true, poisonPresent = true, mpamPresent = true)
    case CCHIParametersKey => CCHIParameters(WayIndex_Width = 3)
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false,
      enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  val upstream = new L2UpstreamTable(Seq(
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 0),
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 1),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 4),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 5)))
  val config = new L2Configuration(0, true, 0 until 4, upstream = upstream,
    pfSrcID = 2, bopEnable = source == "bop", prefetchIssueEnable = true,
    pfTrainEnable = true, pfCtrlEnable = true, pfTlbEnable = source != "bop",
    pfRecvEnable = false, vbopStubEnable = source == "vbop", cdpStubEnable = source == "cdp")
  class CheckedTop extends Module {
    val cache = Module(new L2Top(config)(parameters))
    val io = IO(cache.io.cloneType)
    io <> cache.io
    if (mbist) {
      val broadcast = _root_.utility.sram.SramHelper.genBroadCastBundleTop()
      broadcast := 0.U.asTypeOf(broadcast)
      val te = ClockGate.genTeSrc
      te.cgen := false.B
    }
    XSLog.collect(0.U(64.W), false.B, false.B, false.B)
  }
  val sv = _root_.circt.stage.ChiselStage.emitSystemVerilog(new CheckedTop,
    firtoolOpts = Array("--disable-all-randomization", "--strip-debug-info"))
  val output = java.nio.file.Paths.get(args(2))
  java.nio.file.Files.createDirectories(output.getParent)
  java.nio.file.Files.writeString(output, sv)
  println(s"KMH3_PREFETCH_ELAB pass=true source=$source mbist=$mbist slices=4 ways=8 sets=1024 tshrs=32 ecc=secded replacement=drrip dualType1=true pfSrcID=2")
}
