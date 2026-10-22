package oceanus

// Intent (3a): identical timed training events enter independent reference and
// production RTL. Compare learning events, score vectors/rounds/offset commits,
// and emitted VA/PA/source sequences. needT has its own registered comparison.
// Reference TLB is always ready, resp=req.fire+1 and PMP=req.fire+2. A second
// mode varies production latency and backpressure to exercise deviation D1.
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.{vbopreference => ref}
import org.chipsalliance.cde.config._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import utility._
import scala.collection.mutable

object TestTop_OceanusVBOPOvernight extends App with ChiselSim {
  // Intent: use declared reference defaults in simulation, without the
  // platform's external Constantin DPI runtime/configuration service.
  Constantin.init(false)
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params()
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false, enablePerf=true, fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=true, enablePerfDB=false,
      perfLevel=XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  // Test the complete production offset list and RR geometry. Shorter phases
  // make all five trace families exercise repeated commits with bounded runtime.
  def opt(name: String, default: String): String = args.find(_.startsWith(name+"=")).map(_.split("=",2)(1)).getOrElse(default)
  val defaults = args.contains("--defaults")
  val latency = opt("--latency", "1").toInt
  val first = opt("--first", "1").toInt
  val last = opt("--last", "50").toInt
  val count = opt("--trains", if(defaults) "6200" else "1400").toInt
  require(Set(1,3,7)(latency))
  // Reference roundBits=log2Up(roundMax): use a non-power-of-two short limit
  // so the reference counter can reach it (4 wraps in two bits). Preserve
  // that original algorithm behavior; production defaults remain roundMax=50.
  val geometry = if(defaults) OceanusVBOPParameters() else
    OceanusVBOPParameters(scoreBits=3, roundMax=5, badScore=0, dQLatency=32)
  private val defaultsReference = ref.BOPParameters()
  require(OceanusVBOPParameters().offsetList == defaultsReference.offsetList)
  require(OceanusVBOPParameters().rrTableEntries == defaultsReference.rrTableEntries &&
    OceanusVBOPParameters().rrTagBits == defaultsReference.rrTagBits &&
    OceanusVBOPParameters().scoreBits == defaultsReference.scoreBits &&
    OceanusVBOPParameters().roundMax == defaultsReference.roundMax &&
    OceanusVBOPParameters().tlbReplayCnt == defaultsReference.tlbReplayCnt &&
    OceanusVBOPParameters().dQEntries == defaultsReference.dQEntries &&
    OceanusVBOPParameters().dQMaxLatency == defaultsReference.dQMaxLatency &&
    OceanusVBOPParameters().badScore == defaultsReference.badScore &&
    OceanusVBOPParameters().dQLatency == defaultsReference.dQLatency)
  val kinds = Seq(opt("--kind", "random"))
  simulate(new VBOPComparison(geometry)(params)) { dut =>
    def flag(x: Bool) = x.peek().litToBoolean
    def value(x: UInt) = x.peek().litValue
    var totalSequences = 0
    var totalIssued = 0
    for ((kind, k) <- kinds.zipWithIndex; seed <- first to last) {
      val random = new scala.util.Random(0x3a000L + k*100 + seed)
      val refLearning = mutable.ArrayBuffer.empty[BigInt]
      val dutLearning = mutable.ArrayBuffer.empty[BigInt]
      val refRequests = mutable.ArrayBuffer.empty[(BigInt,Boolean)]
      val dutRequests = mutable.ArrayBuffer.empty[(BigInt,Boolean,Boolean)]
      dut.io.train.valid.poke(false.B)
      dut.io.train.bits.poke(0.U.asTypeOf(dut.io.train.bits))
      dut.io.enable.poke(true.B); dut.io.ready.poke(true.B)
      dut.io.tlbReady.poke(true.B); dut.io.latency.poke(latency.U)
      dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B)
      def sample(): Unit = {
        if(flag(dut.io.refLearning.valid)) refLearning += value(dut.io.refLearning.bits)
        if(flag(dut.io.dutLearning.valid)) dutLearning += value(dut.io.dutLearning.bits)
        if(flag(dut.io.refRequest.valid)) refRequests += ((value(dut.io.refRequest.bits),flag(dut.io.refNeedT)))
        if(flag(dut.io.dutRequest.valid)) dutRequests += ((value(dut.io.dutRequest.bits),flag(dut.io.originalNeedT),flag(dut.io.actualNeedT)))
      }
      def tick(): Unit = { sample(); dut.clock.step() }
      for (_ <- 0 until 300) tick() // reference SRAM reset completes
      var line = BigInt(0x200000/64)

      val trace = mutable.ArrayBuffer.empty[String]
      for (i <- 0 until count) {
        line = kind match {
          case "in-page-stride" => BigInt(0x200000/64 + (i*2 + seed) % 60)
          case "cross-page-stride" => line + (seed%31+1)
          case "alternating-offsets" => line + (if(i%2==0) 1 else 4)
          case "noise" => if(random.nextInt(5)==0) BigInt(0x200000/64+random.nextInt(8192)) else line+2
          case "random" => BigInt(0x200000/64+random.nextInt(16384))
        }
        trace += s"$i,$line,${i%4096},${i%3==0},${i%19!=0}"
        dut.io.train.bits.vaddr.get.poke(line.U)
        dut.io.train.bits.paddr.poke(((line<<6)^BigInt("12345000",16)).U)
        dut.io.train.bits.reqSource.get.poke((i%32).U)
        dut.io.train.bits.sourceId.get.poke((i%4096).U)
        dut.io.train.bits.needT.get.poke((i%3==0).B)
        dut.io.train.bits.trainEnable.get.poke((i%19!=0).B)
        dut.io.train.valid.poke(true.B); tick(); dut.io.train.valid.poke(false.B)
        for(_ <- 0 until 15) tick()
      }
      for(_ <- 0 until 350) tick()
      val eventMismatch = refLearning.toSeq != dutLearning.toSeq || refRequests.map(_._1).toSeq != dutRequests.map(_._1).toSeq || refRequests.map(_._2).toSeq != dutRequests.map(_._2).toSeq
      if(eventMismatch) {
        val dir = java.nio.file.Paths.get(opt("--failures", "/data/zhuy/pf-framework/overnight-20261021/failures"))
        java.nio.file.Files.createDirectories(dir)
        val prefix = s"$kind-seed$seed-latency$latency-trains$count"
        java.nio.file.Files.writeString(dir.resolve(prefix+".csv"), "index,line,source,needT,trainEnable\n"+trace.mkString("\n"))
        java.nio.file.Files.writeString(dir.resolve(prefix+".events"), s"referenceLearning=${refLearning.mkString(",")}\ndutLearning=${dutLearning.mkString(",")}\nreferenceRequests=${refRequests.mkString(",")}\ndutRequests=${dutRequests.mkString(",")}\n")
        println(s"VBOP_DIFF_FAILURE kind=$kind seed=$seed latency=$latency latency=$latency trains=$count trace=${dir.resolve(prefix+".csv")}")
      }
      assert(refLearning.toSeq == dutLearning.toSeq, s"learning mismatch $kind/$seed index=${refLearning.zip(dutLearning).indexWhere{case(a,b)=>a!=b}} counts=${refLearning.size}/${dutLearning.size}")
      assert(refRequests.map(_._1).toSeq == dutRequests.map(_._1).toSeq, s"request order/address/source $kind/$seed counts=${refRequests.size}/${dutRequests.size}")
      assert((defaults || refRequests.nonEmpty) && refLearning.size > count, "vacuous learning trace")
      assert(refRequests.map(_._2).toSeq == dutRequests.map(_._2).toSeq, "original needT changed")
      assert(dutRequests.forall(!_._3), "issued needT must be zero")
      val downgraded = refRequests.count(_._2)
      assert((defaults || downgraded > 0) && value(dut.io.downgraded) == downgraded)
      assert(value(dut.io.dropped)==0)
      assert(value(dut.io.accepted) == count - (count+18)/19, "trainEnable acceptance count")
      assert(value(dut.io.refTranslations)==refRequests.size)
      assert(value(dut.io.dutTranslations)==dutRequests.size, "default must translate same-page predictions too")
      assert(value(dut.io.commits)>=2, s"no learned phase commit $kind/$seed")
      totalSequences += 1; totalIssued += refRequests.size
      def digest(values: Seq[BigInt]): String = java.security.MessageDigest.getInstance("SHA-256")
        .digest(values.mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)).map(b => f"${b&255}%02x").mkString
      println(s"VBOP_DIFF_SEQUENCE pass=true kind=$kind seed=$seed latency=$latency trains=$count learning=${refLearning.size} requests=${refRequests.size} commits=${value(dut.io.commits)} learningSHA256=${digest(refLearning.toSeq)} requestSHA256=${digest(refRequests.map(_._1).toSeq)}")
      println(s"VBOP_DIFF_NEEDT kind=$kind seed=$seed latency=$latency original_equal=${refRequests.size} actual_equal=${refRequests.size-downgraded} downgraded=$downgraded deviation=D2")
    }
    println(s"VBOP_DIFF_RESULT pass=true sequences=$totalSequences identical=$totalSequences requests=$totalIssued")
  }
}
