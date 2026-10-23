// Intent (C0): check hardware address helpers against independent host formulas,
// literal boundary/alias vectors and the untouched test reference. Elaborate
// every public bundle, the inactive production skeleton and full ReferenceCDP.
// This does not claim production algorithm differential coverage (C1-C5).
package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.{cdpreference => ref}
import org.chipsalliance.cde.config._
import scala.util.Random
import utility._

// Intent: constructor parameters must exist before inherited helper vals run.
class CDPProductionHelpers(implicit val p: Parameters) extends HasOceanusCDPParams
class CDPReferenceHelpers(implicit val p: Parameters) extends ref.HasCDPParams

class CDPHelperHarness(hash: Boolean)(implicit val p: Parameters) extends Module {
  val geometry = OceanusCDPParameters(useVpnTableHashIndex = hash)
  val helperParams = p.alterPartial { case OceanusCDPKey => geometry }
  val referenceParams = p.alterPartial { case ref.ReferenceCDPKey => ref.CDPParameters(useVpnTableHashIndex = hash) }
  val h = new CDPProductionHelpers()(helperParams)
  val r = new CDPReferenceHelpers()(referenceParams)
  val io = IO(new Bundle {
    val va = Input(UInt(50.W))
    val pa = Input(UInt(48.W))
    val fold = Input(UInt(25.W))
    val depth = Input(UInt(2.W))
    val actual = Output(Vec(11, UInt(50.W)))
    val reference = Output(Vec(11, UInt(50.W)))
    val request = Output(new OceanusCDPRequest()(helperParams))
    val referencePA = Output(UInt(48.W))
    val referenceDepth = Output(UInt(2.W))
    val referenceSource = Output(UInt(5.W))
    val widths = Output(Vec(12, UInt(16.W)))
  })
  def fields(g: HasOceanusCDPParams) = Seq(g.getMainIdx(io.va), g.getSubIdx(io.va),
    g.getVpnTableTag(io.va), g.getFilterSet(io.pa), g.getFilterTag(io.pa),
    g.getFilterOffset(io.pa), g.getVpn0(io.va), g.getOffset(io.va),
    g.getFoldedHash(io.fold, 10), g.getFoldedHash(0.U(0.W), 10), g.getVpnAddr(io.va))
  val expected = Seq(r.getMainIdx(io.va), r.getSubIdx(io.va), r.getVpnTableTag(io.va),
    r.getFilterSet(io.pa), r.getFilterTag(io.pa), r.getFilterOffset(io.pa),
    r.getVpn0(io.va), r.getOffset(io.va), r.getFoldedHash(io.fold, 10),
    r.getFoldedHash(0.U(0.W), 10), r.getVpnAddr(io.va))
  io.actual := VecInit(fields(h).map(_.pad(50)))
  io.reference := VecInit(expected.map(_.pad(50)))
  val entry = WireInit(0.U.asTypeOf(new OceanusCDPEntry()(helperParams)))
  entry.pTag := io.pa(47, 6)
  entry.pfDepth := io.depth
  io.request := entry.toRequest()
  val referenceEntry = WireInit(0.U.asTypeOf(new ref.PrefetchFilterEntry()(referenceParams)))
  referenceEntry.pTag := io.pa(47, 6)
  referenceEntry.pfDepth := io.depth
  val referenceRequest = referenceEntry.toPrefetchReq()
  io.referencePA := referenceRequest.addr
  io.referenceDepth := referenceRequest.cdpPfDepth.get
  io.referenceSource := referenceRequest.pfSource
  val definitions: Seq[Data] = Seq(new OceanusCDPDetectTask(512)(helperParams),
    new OceanusCDPDetectTask(256)(helperParams), new OceanusCDPDetectTask(64)(helperParams),
    new OceanusCDPVpnMeta()(helperParams), new OceanusCDPVtQueryReq()(helperParams),
    new OceanusCDPVtQueryRsp()(helperParams), new OceanusCDPVtTrainReq()(helperParams),
    new OceanusCDPFtQueryReq()(helperParams), new OceanusCDPFtQueryRsp()(helperParams),
    new OceanusCDPFtTrainReq()(helperParams), new OceanusCDPCandidate()(helperParams),
    new OceanusCDPEntry()(helperParams))
  io.widths := VecInit(definitions.map(_.getWidth.U(16.W)))
}

object TestTop_OceanusCDP extends App with ChiselSim {
  Constantin.init(false)
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params()
    case xscache.oceanus.compactchi.CCHIParametersKey => xscache.oceanus.compactchi.CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  val production = OceanusCDPParameters()
  val reference = ref.CDPParameters()
  assert(production.productElementNames.toSeq == reference.productElementNames.toSeq)
  assert(production.productIterator.toSeq == reference.productIterator.toSeq)
  production.productElementNames.zip(production.productIterator).foreach { case (name, value) =>
    println(s"CDP_C0_DEFAULT pass=true name=$name production=$value reference=$value")
  }
  assert(production.hasPrefetchBit == reference.hasPrefetchBit && production.hasPrefetchSrc == reference.hasPrefetchSrc &&
    production.inflightEntries == reference.inflightEntries)
  println("CDP_C0_DEFAULT pass=true name=hasPrefetchBit production=true reference=true")
  println("CDP_C0_DEFAULT pass=true name=hasPrefetchSrc production=true reference=true")
  println("CDP_C0_DEFAULT pass=true name=inflightEntries production=16 reference=16")

  // Literal default vectors encode independent bit-level expectations in the
  // order main/sub/tag/set/filterTag/slot/VPN0/offset/fold10/zeroFold/VPNaddr.
  val literals = Seq(
    (BigInt(0), BigInt(0), Seq[BigInt](0,0,0,0,0,0,0,0,0,0,0)),
    (BigInt("201004",16), BigInt("3ffff",16), Seq[BigInt](0,1,0,0,0,63,1,4,0,0,1)),
    (BigInt("2000000",16), BigInt("1000000",16), Seq[BigInt](1,1,1,0,1,0,0,0,0,0,16)),
    (BigInt("8000000",16), BigInt("ffffff",16), Seq[BigInt](1,1,4,63,0,63,0,0,0,0,64)),
    ((BigInt(1)<<50)-1, (BigInt(1)<<48)-1, Seq[BigInt](2,1,31,63,(BigInt(1)<<24)-1,63,511,4095,0,0,(BigInt(1)<<29)-1))
  )
  def xorBits(value: BigInt, shift: Int, width: Int): BigInt = {
    // Accumulate each set input bit into its independent destination column.
    var result = BigInt(0)
    for (bit <- shift until value.bitLength if value.testBit(bit)) result = result.flipBit((bit-shift) % width)
    result
  }
  var checked = 0
  for (hash <- Seq(true, false)) {
    simulate(new CDPHelperHarness(hash)(params)) { dut =>
      dut.io.va.poke(0.U); dut.io.pa.poke(0.U); dut.io.fold.poke(0.U); dut.io.depth.poke(0.U)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      assert(dut.io.widths.map(_.peek().litValue).toSeq == Seq[BigInt](519,263,71,21,4,132,19,6,612,160,57,99))
      val random = new Random(0xc0)
      val boundary = (0 until 50).flatMap(b => Seq((BigInt(1)<<b)-1, BigInt(1)<<b, (BigInt(1)<<b)+1))
      val alias = Seq(BigInt(0), (BigInt(1)<<25)|(BigInt(1)<<35),
        BigInt("201004",16), BigInt("201ffc",16))
      val values = literals.map(x => (x._1, x._2, BigInt(0))) ++
        (boundary ++ alias).map(a => (a, a & ((BigInt(1)<<48)-1), a & ((BigInt(1)<<25)-1))) ++
        (0 until 512).map(_ => (BigInt(50,random), BigInt(48,random), BigInt(25,random)))
      for (((va,pa,fold), index) <- values.zipWithIndex) {
        dut.io.va.poke(va.U); dut.io.pa.poke(pa.U); dut.io.fold.poke(fold.U)
        val depth = index % 4
        dut.io.depth.poke(depth.U)
        val tag = va >> 25
        val main = (va >> 23) & 3
        val sub = (va >> 21) & 3
        val golden = Seq(if(hash) main ^ xorBits(va,25,2) else main,
          if(hash) sub ^ xorBits(va,23,2) else sub, xorBits(va,25,10),
          (pa >> 18)&63, pa >> 24, (pa >> 12)&63, (va >> 12)&511, va&4095,
          xorBits(fold,0,10), BigInt(0), va >> 21)
        if(hash && index < literals.size) assert(golden == literals(index)._3, s"literal index=$index $golden")
        assert(dut.io.actual.map(_.peek().litValue).toSeq == golden, s"golden hash=$hash va=$va pa=$pa")
        assert(dut.io.reference.map(_.peek().litValue).toSeq == golden, s"reference hash=$hash va=$va pa=$pa")
        val aligned = (pa >> 6) << 6
        assert(dut.io.request.paddr.peek().litValue == aligned && dut.io.referencePA.peek().litValue == aligned)
        assert(dut.io.request.pfDepth.peek().litValue == depth && dut.io.referenceDepth.peek().litValue == depth)
        assert(dut.io.request.pfSource.peek().litValue == 9 && dut.io.request.reqSource.peek().litValue == 16)
        assert(dut.io.referenceSource.peek().litValue == 16 && !dut.io.request.needT.peek().litToBoolean)
        checked += 1
      }
      println(s"CDP_C0_HELPERS pass=true hash=$hash vectors=${values.size} literals=${if(hash) literals.size else 0} boundaries=${boundary.size} alias=${alias.size} random=512 widths=true")
    }
  }
  // Intent (C2): Filter SRAM definitions are cached by utility for one Chisel
  // context. Elaborate all production geometries and the original reference
  // as Definitions in one context, without reusing a prior circuit's SRAM.
  val chirrtl = _root_.circt.stage.ChiselStage.emitCHIRRTL(new Module {
    for (banks <- Seq(1,2,4)) {
      chisel3.experimental.hierarchy.Definition(new OceanusCDP(bankCount=banks)(params))
    }
    chisel3.experimental.hierarchy.Definition(new ref.ReferenceCDP()(params))
  })
  assert(chirrtl.contains("OceanusCDP") && chirrtl.contains("VpnTable") &&
    chirrtl.contains("FilterTable") && chirrtl.contains("SentUnit"))
  println(s"CDP_C0_RESULT pass=true vectors=$checked defaultFields=${production.productArity+3} skeletonBanks=1,2,4 referenceElaborated=true")
}
