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

class VBOPComparison(val geometry: OceanusVBOPParameters)(implicit val p: Parameters) extends Module {
  // Intent: observe descendant registers through Chisel's read-only bore API;
  // direct references across module boundaries are not legal circuit operands.
  def read[T <: Data](signal: T): T = BoringUtils.bore(signal)
  val io = IO(new Bundle {
    val train = Flipped(Valid(new L2PrefetchTrain(true)))
    val enable = Input(Bool())
    val ready = Input(Bool())
    val tlbReady = Input(Bool())
    val latency = Input(UInt(3.W))
    val refLearning = Valid(UInt((geometry.offsetList.size * geometry.scoreBits + 128).W))
    val dutLearning = Valid(UInt((geometry.offsetList.size * geometry.scoreBits + 128).W))
    val commits = Output(UInt(32.W))
    val refRequest = Valid(UInt((50+48+12).W))
    val dutRequest = Valid(UInt((50+48+12).W))
    val refNeedT = Output(Bool())
    val originalNeedT = Output(Bool())
    val actualNeedT = Output(Bool())
    val downgraded = Output(UInt(64.W))
    val refTranslations = Output(UInt(64.W))
    val dutTranslations = Output(UInt(64.W))
    val accepted = Output(UInt(64.W))
    val dropped = Output(UInt(64.W))
  })
  val referenceGeometry = ref.BOPParameters(rrTableEntries=geometry.rrTableEntries,
    rrTagBits=geometry.rrTagBits, scoreBits=geometry.scoreBits, roundMax=geometry.roundMax,
    badScore=geometry.badScore, tlbReplayCnt=geometry.tlbReplayCnt,
    dQEntries=geometry.dQEntries, dQLatency=geometry.dQLatency,
    dQMaxLatency=geometry.dQMaxLatency, offsetList=geometry.offsetList)
  val reference = Module(new ref.VBestOffsetPrefetch()(p.alterPartial { case ref.VBOPKey => referenceGeometry }))
  val dut = Module(new OceanusVBOP(geometry))
  val adapter = Module(new L2PrefetchTlbAdapter)
  adapter.io.clients(0) <> dut.io.tlb
  adapter.io.clients(1).req.valid := false.B
  adapter.io.clients(1).req.bits := 0.U
  adapter.io.clients(1).resp.ready := true.B
  val referenceInput = Module(new Queue(new ref.PrefetchTrain, 4))
  referenceInput.io.enq.valid := io.train.valid && io.train.bits.trainEnable.get
  referenceInput.io.enq.bits.vaddr.get := io.train.bits.vaddr.get
  referenceInput.io.enq.bits.needT := io.train.bits.needT.get
  referenceInput.io.enq.bits.source := io.train.bits.sourceId.get
  referenceInput.io.enq.bits.paddr := io.train.bits.paddr
  reference.io.train <> referenceInput.io.deq
  reference.io.enable := io.enable
  reference.io.pbopCrossPage := true.B
  reference.io.pfCtrlOfDelayLatency := 0.U
  reference.io.resp.valid := false.B
  reference.io.resp.bits := 0.U.asTypeOf(reference.io.resp.bits)
  reference.io.req.ready := io.ready
  dut.io.train := io.train
  dut.io.enable := io.enable
  dut.io.delayLatency := 0.U
  dut.io.req.ready := io.ready
  def pa(va: UInt): UInt = (va ^ "h12345000".U)(47,0)
  val rt = reference.io.tlb_req
  rt.req.ready := true.B
  rt.resp.valid := RegNext(rt.req.fire, false.B)
  rt.resp.bits := 0.U.asTypeOf(rt.resp.bits)
  rt.resp.bits.paddr.head := RegNext(pa(rt.req.bits.vaddr))
  rt.pmp_resp := 0.U.asTypeOf(rt.pmp_resp)
  val dt = adapter.io.tlb
  dt.req.ready := io.tlbReady
  val pending = RegInit(VecInit(Seq.fill(7)(false.B)))
  val addresses = Reg(Vec(7, UInt(48.W)))
  pending(0) := dt.req.fire
  addresses(0) := pa(dt.req.bits.vaddr)
  for(i <- 1 until 7) { pending(i) := pending(i-1); addresses(i) := addresses(i-1) }
  dt.resp.valid := pending(io.latency-1.U)
  dt.resp.bits := 0.U.asTypeOf(dt.resp.bits)
  dt.resp.bits.paddr.head := addresses(io.latency-1.U)
  dt.pmp_resp := 0.U.asTypeOf(dt.pmp_resp)
  val refTranslations = RegInit(0.U(64.W))
  val dutTranslations = RegInit(0.U(64.W))
  when(rt.req.fire) { refTranslations := refTranslations + 1.U }
  when(dt.req.fire) { dutTranslations := dutTranslations + 1.U }
  io.refTranslations := refTranslations
  io.dutTranslations := dutTranslations
  val rs = reference.scoreTable
  val ds = dut.scoreTable
  val commits = RegInit(0.U(32.W))
  val rFire = read(rs.io.test.req.valid) && read(rs.io.test.req.ready)
  val dFire = read(ds.io.test.req.valid) && read(ds.io.test.req.ready)
  val rResp = read(rs.io.test.resp.valid)
  val dResp = read(ds.io.test.resp.valid)
  val rState = read(rs.state)
  val dState = read(ds.state)
  when(rState === 0.U) { commits := commits + 1.U }
  io.commits := commits
  io.refLearning.valid := rFire || rResp || rState === 0.U
  io.dutLearning.valid := dFire || dResp || dState === 0.U
  io.refLearning.bits := Cat(rFire, rResp, rState,
    read(rs.ptr), read(rs.round), read(rs.bestOffset), read(rs.bestScore), read(rs.prefetchOffset), read(rs.prefetchDisable),
    VecInit(rs.st.map(e => read(e.score))).asUInt)
  io.dutLearning.bits := Cat(dFire, dResp, dState,
    read(ds.ptr), read(ds.round), read(ds.bestOffset), read(ds.bestScore), read(ds.prefetchOffset), read(ds.prefetchDisable),
    VecInit(ds.st.map(e => read(e.score))).asUInt)
  io.refRequest.valid := reference.io.req.fire
  io.refRequest.bits := Cat(reference.io.req.bits.vaddr.get, 0.U(6.W),
    reference.io.req.bits.addr, reference.io.req.bits.source)
  io.dutRequest.valid := dut.io.req.fire
  io.dutRequest.bits := Cat(dut.io.requestVA, dut.io.req.bits.paddr, read(dut.reqFilter.io.out.bits.source))
  io.refNeedT := reference.io.req.bits.needT
  io.originalNeedT := dut.io.originalNeedT
  io.actualNeedT := dut.io.issuedNeedT
  io.downgraded := read(dut.needTDowngraded)
  val accepted = RegInit(0.U(64.W))
  when(read(dut.input.io.deq.valid) && read(dut.input.io.deq.ready)) { accepted := accepted + 1.U }
  io.accepted := accepted
  io.dropped := read(dut.overwritten)
}

object TestTop_OceanusVBOP extends App with ChiselSim {
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
  val defaults = args.contains("--defaults")
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
  val kinds = Seq("in-page-stride", "cross-page-stride", "alternating-offsets", "noise", "random")
  simulate(new VBOPComparison(geometry)(params)) { dut =>
    def flag(x: Bool) = x.peek().litToBoolean
    def value(x: UInt) = x.peek().litValue
    var totalSequences = 0
    var totalIssued = 0
    for ((kind, k) <- kinds.zipWithIndex; seed <- 1 to (if(defaults) 1 else 3)) {
      val random = new scala.util.Random(0x3a000L + k*100 + seed)
      val refLearning = mutable.ArrayBuffer.empty[BigInt]
      val dutLearning = mutable.ArrayBuffer.empty[BigInt]
      val refRequests = mutable.ArrayBuffer.empty[(BigInt,Boolean)]
      val dutRequests = mutable.ArrayBuffer.empty[(BigInt,Boolean,Boolean)]
      dut.io.train.valid.poke(false.B)
      dut.io.train.bits.poke(0.U.asTypeOf(dut.io.train.bits))
      dut.io.enable.poke(true.B); dut.io.ready.poke(true.B)
      dut.io.tlbReady.poke(true.B); dut.io.latency.poke(1.U)
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
      val count = if(defaults) 6200 else 1400
      for (i <- 0 until count) {
        line = kind match {
          case "in-page-stride" => BigInt(0x200000/64 + (i*2 + seed) % 60)
          case "cross-page-stride" => line + (seed+1)
          case "alternating-offsets" => line + (if(i%2==0) 1 else 4)
          case "noise" => if(random.nextInt(5)==0) BigInt(0x200000/64+random.nextInt(8192)) else line+2
          case "random" => BigInt(0x200000/64+random.nextInt(16384))
        }
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
      println(s"VBOP_DIFF_SEQUENCE pass=true kind=$kind seed=$seed trains=$count learning=${refLearning.size} requests=${refRequests.size} commits=${value(dut.io.commits)} learningSHA256=${digest(refLearning.toSeq)} requestSHA256=${digest(refRequests.map(_._1).toSeq)}")
      println(s"VBOP_DIFF_NEEDT kind=$kind seed=$seed original_equal=${refRequests.size} actual_equal=${refRequests.size-downgraded} downgraded=$downgraded deviation=D2")
    }
    kinds.foreach(k => println(s"VBOP_DIFF_CLASS kind=$k sequences=${if(defaults) 1 else 3} identical=${if(defaults) 1 else 3} defaults=$defaults"))
    if(!defaults) {
      dut.io.train.valid.poke(false.B); dut.io.ready.poke(true.B)
      dut.io.tlbReady.poke(true.B); dut.io.latency.poke(7.U)
      dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B); dut.clock.step(300)
      val actual = mutable.ArrayBuffer.empty[BigInt]
      val expected = mutable.ArrayBuffer.empty[BigInt]
      val refActual = mutable.ArrayBuffer.empty[BigInt]
      var cycle = 0
      def tick(): Unit = {
        dut.io.tlbReady.poke((cycle%5==0).B)
        dut.io.ready.poke((cycle%3==0).B)
        if(flag(dut.io.dutRequest.valid)) actual += value(dut.io.dutRequest.bits)
        if(flag(dut.io.refRequest.valid)) refActual += value(dut.io.refRequest.bits)
        dut.clock.step(); cycle += 1
      }
      // One request at a time isolates latency from documented RR issue order.
      for(i <- 0 until 64) {
        val line = BigInt(0x800000/64 + 4*i)
        dut.io.train.bits.vaddr.get.poke(line.U)
        dut.io.train.bits.paddr.poke(((line<<6)^BigInt("12345000",16)).U)
        dut.io.train.bits.sourceId.get.poke(i.U)
        dut.io.train.bits.trainEnable.get.poke(true.B)
        dut.io.train.bits.needT.get.poke(true.B)
        dut.io.train.valid.poke(true.B); tick(); dut.io.train.valid.poke(false.B)
        for(_ <- 0 until 63) tick()
      }
      for(_ <- 0 until 100) tick()
      assert(actual.toSeq == refActual.toSeq && actual.size == 64)
      assert(value(dut.io.downgraded)==64 && value(dut.io.dropped)==0)
      println("VBOP_HANDSHAKE pass=true latency=7 requests=64 identical=64 tlbStalls=true outputStalls=true")
    }
    println(s"VBOP_DIFF_RESULT pass=true sequences=$totalSequences identical=$totalSequences requests=$totalIssued")
  }
}
