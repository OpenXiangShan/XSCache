// Intent (zy-pf-obs): validate M9/head_block/M9b against real source queues
// and real slice allocators. All four source IDs and I-cache's two stages are
// covered. Test-only queue wiring never changes the production source buffers.
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
  lazy val fifoCheck = IO(Output(Vec(4, new Bundle {
    val count = UInt(2.W)
    val blocked = Bool()
    val sourceBlocked = Bool()
    val headAddress = UInt(48.W)
  })))
  override protected def attachExtraObservations(): Unit = {
    val inputs = io.t1p.map(_.UpREQ).toSeq ++ io.t4p.map(_.UpREQ).toSeq
    val outputs = module.io.t1p.map(_.UpREQ).toSeq ++ module.io.t4p.map(_.UpREQ).toSeq
    val fifos = inputs.zip(outputs).zipWithIndex.map { case ((in, out), lane) =>
      val stages = if (lane == 2) 2 else 1
      val queues = Seq.fill(stages)(Module(new Queue(new Bundle {
        val request = new oceanus.compactchi.FlitREQ
        val context = new L2PrefetchTrainSideband
      }, 2)))
      queues.head.io.enq.valid := in.valid
      queues.head.io.enq.bits.request := in.bits
      queues.head.io.enq.bits.context := (if (lane < io.t1p.size) io.pfTrain.get(lane)
        else 0.U.asTypeOf(new L2PrefetchTrainSideband))
      in.ready := queues.head.io.enq.ready
      queues.zip(queues.drop(1)).foreach { case (a, b) => b.io.enq <> a.io.deq }
      val last = queues.last
      out.valid := last.io.deq.valid
      out.bits := last.io.deq.bits.request
      last.io.deq.ready := out.ready
      if (lane < io.t1p.size) module.io.pfTrain.get(lane) := last.io.deq.bits.context
      // Read the actual final Queue IO for a host-side oracle independent of
      // the observer's private stage counts and address records.
      fifoCheck(lane).count := last.io.count
      fifoCheck(lane).blocked := last.io.deq.valid && !last.io.deq.ready
      fifoCheck(lane).sourceBlocked := in.valid && !in.ready
      fifoCheck(lane).headAddress := last.io.deq.bits.request.Addr
      L2ObservationFifo(in.fire, in.bits.Addr, out.fire,
        cfg.upstream.entries(lane).nid, in.valid && !in.ready, stages)
    }
    L2PrefetchObservation(module, io.t1p.zipWithIndex.map { case (port, lane) =>
      L2ObservationArrival(port.UpREQ.fire, port.UpREQ.bits, io.pfTrain.get(lane).reqSource)
    }, fifos)
  }
}

object TestTop_L2ObservationFifo extends App with ChiselSim {
  Constantin.init(false)
  val upstream = new L2UpstreamTable(Seq(
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 0),
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 1),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 4),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 5)))
  val cfg = new L2Configuration(8, true, 0 until 2, upstream = upstream, pfSrcID = 3,
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
    val slice0 = Seq(0x30000, 0x30c00, 0x33000).map(BigInt(_))
    val slice1 = Seq(0x30400, 0x30800).map(BigInt(_))
    assert(slice0.forall(a => ((a >> 6).bitCount & 1) == 0))
    assert(slice1.forall(a => ((a >> 6).bitCount & 1) == 1))
    val masters = Seq(MasterId.T1P0, MasterId.T1P1, MasterId.T4P0, MasterId.T4P1)
    // Expected counts per edge: head_block, collateral, same_pa total,
    // third_same_pa, different_slice_ready. The latter two are disjoint
    // subsets of same_pa total, never assumed exhaustive.
    case class Scenario(name: String, resident: Seq[BigInt], pending: Option[BigInt],
      fillUpstream: Boolean, secondFull: Boolean, expected: Seq[Int])
    val cases = Seq(
      Scenario("empty", Seq.empty, None, false, false, Seq(0,0,0,0,0)),
      Scenario("head_only", Seq(slice0(1)), None, false, false, Seq(1,0,0,0,0)),
      Scenario("other_slice_free", Seq(slice0(1),slice1(1)), None, false, false, Seq(1,1,0,0,0)),
      Scenario("same_slice", Seq(slice0(1),slice0(2)), None, false, false, Seq(1,0,0,0,0)),
      Scenario("other_slice_full", Seq(slice0(1),slice1(1)), None, false, true, Seq(1,0,0,0,0)),
      Scenario("same_pa_no_pending", Seq.fill(2)(slice0(1)), None, false, false, Seq(1,0,0,0,0)),
      Scenario("third_same_pa", Seq.fill(2)(slice0(1)), Some(slice0(1)), true, false, Seq(1,0,1,1,0)),
      Scenario("different_slice_ready", Seq.fill(2)(slice0(1)), Some(slice1(1)), true, false, Seq(1,0,1,0,1)),
      Scenario("different_slice_full", Seq.fill(2)(slice0(1)), Some(slice1(1)), true, true, Seq(1,0,1,0,0)),
      Scenario("third_different_pa_same_slice", Seq.fill(2)(slice0(1)), Some(slice0(2)), true, false, Seq(1,0,1,0,0)),
      Scenario("different_resident_pa", Seq(slice0(1),slice0(2)), Some(slice1(1)), true, false, Seq(1,0,0,0,0)))
    var windows = 0
    for (lane <- masters.indices; scenario <- cases) {
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
      val port = if (lane < 2) dut.io.t1p(lane).UpREQ else dut.io.t4p(lane - 2).UpREQ
      def drive(address: BigInt): Unit = {
        up.driveRead(UpRequestSpec(masters(lane), txn, upstream.entries(lane).nid, 0,
          if (lane < 2) CCHIOpcode.ReadShared.opcode else CCHIOpcode.ReadOnce.opcode,
          address, expCompData = lane < 2))
        if (lane < 2) {
          dut.io.pfTrain.get(lane).reqSource.poke(2.U)
          dut.io.pfTrain.get(lane).trainEnable.poke(false.B)
        }
      }
      def request(address: BigInt): Unit = {
        drive(address)
        var wait = 0
        while (!flag(port.ready)) { tick(); wait += 1; assert(wait < 100) }
        tick(); up.clearUpReq(); txn += 1
      }
      if (scenario.resident.nonEmpty) {
        // No downstream response: these owners occupy the real shared TSHRs.
        request(slice0(0)); for (_ <- 0 until 80) tick()
        if (scenario.secondFull) { request(slice1(0)); for (_ <- 0 until 80) tick() }
        scenario.resident.foreach(request)
        for (_ <- 0 until 10) tick()
        if (lane == 2 && scenario.fillUpstream) {
          for (_ <- 0 until 2) request(slice0(2))
          for (_ <- 0 until 5) tick()
        }
        scenario.pending.foreach(drive)
      }
      val check = dut.fifoCheck(lane)
      assert(check.count.peek().litValue == scenario.resident.size,
        s"${scenario.name} source=${upstream.entries(lane).nid} final stage count")
      assert(flag(check.blocked) == scenario.resident.nonEmpty)
      assert(flag(check.sourceBlocked) == scenario.pending.nonEmpty)
      // Check real queue validity/blocking on every measured edge. Clean does
      // not discard FIFO contents; dump includes its edge in next_counter.
      def checkPlateau(): Unit = {
        assert(check.count.peek().litValue == scenario.resident.size)
        assert(flag(check.blocked) == (scenario.expected.head == 1))
        assert(flag(check.sourceBlocked) == scenario.pending.nonEmpty)
        if (scenario.resident.nonEmpty)
          assert(check.headAddress.peek().litValue == scenario.resident.head)
      }
      dut.log.clean.poke(true.B); tick(); dut.log.clean.poke(false.B)
      for (_ <- 0 until 39) { checkPlateau(); tick() }
      checkPlateau()
      println(s"PFOBS_FIFO_WINDOW name=${scenario.name} source=${upstream.entries(lane).nid} " +
        s"expected=${scenario.expected.map(_ * 40).mkString(",")}")
      dut.log.dump.poke(true.B); tick(); dut.log.dump.poke(false.B)
      windows += 1
      if (scenario.name == "empty") {
        dut.log.clean.poke(true.B); tick(); dut.log.clean.poke(false.B)
        request(slice1(1))
        // For two stages, allow exactly one inter-stage move, then observe
        // the head on its accepting edge. A valid dequeuing head is not blocked.
        if (lane == 2) tick()
        assert(check.count.peek().litValue == 1 && !flag(check.blocked))
        println(s"PFOBS_FIFO_WINDOW name=dequeue_edge source=${upstream.entries(lane).nid} expected=0,0,0,0,0")
        dut.log.dump.poke(true.B); tick(); dut.log.dump.poke(false.B)
        windows += 1
      }
      if (lane == 2 && scenario.name == "same_pa_no_pending") {
        dut.log.clean.poke(true.B); tick(); dut.log.clean.poke(false.B)
        drive(slice1(1))
        // Final stage is full, but the first stage can accept this source
        // request. M9b must remain zero on this edge, unlike true backpressure.
        assert(flag(port.ready) && !flag(check.sourceBlocked))
        println(s"PFOBS_FIFO_WINDOW name=upstream_can_accept source=4 expected=1,0,0,0,0")
        dut.log.dump.poke(true.B); tick(); dut.log.dump.poke(false.B)
        windows += 1
      }
    }
    println(s"PFOBS_FIFO_RESULT pass=true windows=$windows")
  }
}
