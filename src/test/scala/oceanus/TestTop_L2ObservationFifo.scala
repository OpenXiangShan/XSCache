// Intent (zy-pf-obs): validate M9 against real two-entry source queues and two
// real slice allocators. Hold downstream data to fill the single shared slot;
// compare another free slice, the same slice, and two full slices. Test only.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}

class ObservationFifoTop(cfg: L2Configuration)(implicit p: Parameters) extends TestTop_L2Top(cfg) {
  // Intent: lazy IO is safe when the parent invokes the observation hook.
  lazy val fifoCheck = IO(Output(new Bundle {
    val count = UInt(2.W)
    val blocked = Bool()
  }))
  override protected def attachExtraObservations(): Unit = {
    val queue = Module(new Queue(new Bundle {
      val request = new oceanus.compactchi.FlitREQ
      val context = new L2PrefetchTrainSideband
    }, 2))
    val port = io.t1p(0)
    queue.io.enq.valid := port.UpREQ.valid
    queue.io.enq.bits.request := port.UpREQ.bits
    queue.io.enq.bits.context := io.pfTrain.get(0)
    port.UpREQ.ready := queue.io.enq.ready
    module.io.t1p(0).UpREQ.valid := queue.io.deq.valid
    module.io.t1p(0).UpREQ.bits := queue.io.deq.bits.request
    module.io.pfTrain.get(0) := queue.io.deq.bits.context
    queue.io.deq.ready := module.io.t1p(0).UpREQ.ready
    // Intent: expose physical queue state to the host oracle, independently
    // of the observer's private shadow queue and its M9 expression.
    fifoCheck.count := queue.io.count
    fifoCheck.blocked := queue.io.deq.valid && !queue.io.deq.ready
    L2PrefetchObservation(module,
      Seq(L2ObservationArrival(port.UpREQ.fire, port.UpREQ.bits, io.pfTrain.get(0).reqSource)),
      Seq(L2ObservationFifo(port.UpREQ.fire, port.UpREQ.bits.Addr, queue.io.deq.fire, cfg.upstream.type1(0).nid)))
  }
}

object TestTop_L2ObservationFifo extends App with ChiselSim {
  Constantin.init(false)
  val cfg = new L2Configuration(8, true, 0 until 2, pfSrcID = 3,
    bopEnable = false, pfTlbEnable = true, pfTrainEnable = true)
  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 4, sets = 16, ways = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11,
      reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters(WayIndex_Width = 3)
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  simulate(new ObservationFifoTop(cfg)(config)) { dut =>
    val spec = CaseSpec("pfobs-fifo", UpConfig(Seq(MasterId.t1p0),
      ExplicitRequestStream(Seq.empty, Seq.empty), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(12), FixedOutstanding(64), ImmediateCreditPolicy()), 20000, (0,20000), Seq.empty)
    def flag(x: Bool) = x.peek().litToBoolean
    // map2 is parity of the line address. All addresses below are distinct.
    val slice0 = Seq(0x30000, 0x30c00, 0x33000).map(BigInt(_))
    val slice1 = Seq(0x30400, 0x30800).map(BigInt(_))
    assert(slice0.forall(a => ((a >> 6).bitCount & 1) == 0))
    assert(slice1.forall(a => ((a >> 6).bitCount & 1) == 1))
    for ((name, secondSame, secondFull, expected) <- Seq(
      ("other_slice_free", false, false, 40),
      ("same_slice", true, false, 0),
      ("other_slice_full", false, true, 0))) {
      // A fresh protocol host accompanies each DUT reset; credits from the
      // previous plateau must not survive into the next CHI link startup.
      val up = new L2TopUpstreamModel(dut, spec)
      val dn = new L2TopDownstreamModel(dut, spec)
      var cycle = 0
      def tick(): Unit = { dn.stepWithCredits(cycle); cycle += 1 }
      up.initInputs(); dn.initInputs()
      dut.log.clean.poke(false.B); dut.log.dump.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B)
      dn.activateRxLink()
      for (_ <- 0 until 300) tick()
      var txn = 0
      def request(address: BigInt): Unit = {
        up.driveRead(UpRequestSpec(MasterId.t1p0, txn, 0, 0,
          CCHIOpcode.ReadShared.opcode, address, expCompData = true))
        dut.io.pfTrain.get(0).reqSource.poke(2.U)
        dut.io.pfTrain.get(0).trainEnable.poke(false.B)
        var wait = 0
        while (!flag(dut.io.t1p(0).UpREQ.ready)) { tick(); wait += 1; assert(wait < 100) }
        tick(); up.clearUpReq(); txn += 1
      }
      request(slice0(0)); for (_ <- 0 until 80) tick()
      if (secondFull) { request(slice1(0)); for (_ <- 0 until 80) tick() }
      request(slice0(1))
      request(if (secondSame) slice0(2) else slice1(1))
      for (_ <- 0 until 10) tick()
      // No downstream response is driven: the initial owners keep each
      // selected shared TSHR live for the entire measured plateau.
      assert(dut.fifoCheck.count.peek().litValue == 2)
      assert(flag(dut.fifoCheck.blocked))
      dut.log.clean.poke(true.B); tick(); dut.log.clean.poke(false.B)
      // Existing XSPerf prints next_counter, including the dump edge:
      // 39 ordinary edges plus that final edge are exactly 40 samples.
      for (_ <- 0 until 39) tick()
      println(s"PFOBS_FIFO_WINDOW name=$name expected=$expected")
      dut.log.dump.poke(true.B); tick(); dut.log.dump.poke(false.B)
    }
    println("PFOBS_FIFO_RESULT pass=true windows=3")
  }
}
