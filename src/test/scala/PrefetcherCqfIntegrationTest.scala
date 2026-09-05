package xscache.coupledL2

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.{ChiselStage, FirtoolOption}
import freechips.rocketchip.diplomacy.{AddressSet, DisableMonitors, LazyModule}
import org.chipsalliance.cde.config.Config
import utility.{ChiselDB, Constantin}
import utility.chiron.CLogB
import xscache.chi.CHIIssue
import xscache.coupledL2.prefetch.BOPParameters

object PrefetcherCqfIntegrationTest extends App {
  val config = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      ways = 4,
      sets = 128,
      clientCaches = Seq(L1Param(
        name = "dcache",
        aliasBitsOpt = Some(2),
        vaddrBitsOpt = Some(44),
        pcBitOpt = Some(50)
      )),
      prefetch = Seq(BOPParameters(enableCQF = true)),
      enablePerf = false,
      enableRollingDB = false,
      enableMonitor = false,
      enableTLLog = false,
      enableCHILog = false,
      elaboratedTopDown = false,
      sam = Seq(AddressSet.everything -> 0)
    )
    case CHIIssue => "B"
  })

  CLogB.init(false)
  ChiselDB.init(false)
  Constantin.init(false)

  val top = DisableMonitors(p => LazyModule(
    new TestTop_CHIL2(numCores = 1, numULAgents = 0, banks = 1)(p)
  ))(config)

  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => top.module),
    FirtoolOption("--disable-annotation-unknown"),
    FirtoolOption("--default-layer-specialization=enable")
  ))
}
