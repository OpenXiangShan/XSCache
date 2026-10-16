package oceanus

import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.compactchi.CCHIOpcode
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable

case class L2TopConcurrentPlan(name: String, slices: Int,
  batches: Seq[(Int, Seq[UpRequestSpec])], latency: Int = 80,
  bopParams: OceanusBOPParameters = OceanusBOPParameters(), watchdog: Int = 6000)
case class L2TopConcurrentTrace(
  accepts: Seq[(Int, UpRequestSpec)], trains: Seq[(Int, Int, BigInt)],
  received: Seq[(Int, BigInt)], enqueues: Seq[(Int, BigInt)], issues: Seq[(Int, Int, BigInt)],
  priorityDrops: Int, reuseFirstCollisions: Int, maxTshrs: Int, maxOutstanding: Int,
  blockedEnqueues: Seq[BigInt], retained: Seq[BigInt], overwrites: Int, blockedOverwrites: Int,
  releasedAt: Option[Int], checkedBeats: Int, completed: Int)

/** Production-path simulation service shared by the deterministic cases.
  * No forced signals or artificial queue blocking: external valid owns the
  * REQ arbiter, and all requests/refills/responses traverse production RTL.
  */
object L2TopConcurrentSimulation extends ChiselSim {
  def run(plan: L2TopConcurrentPlan)(verify: L2TopConcurrentTrace => Unit): Unit = {
    val cfg = new L2Configuration(8, true, 0 until plan.slices,
      bopEnable = true, bopParams = plan.bopParams)
    val params = new Config((_, _, _) => {
      case L2ParamsKey => L2Params(mshrSize = 8, ways = 4, sets = 128,
        blockBytes = 64, dataSRAMSplit = 8, enableDataECC = false,
        hasMbist = false, hasSramCtl = false, dataECC = Some("secded"))
      case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
      case CCHIParametersKey => CCHIParameters()
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
    })
    implicit val simulator: HasSimulator = HasSimulator.default
    val all = plan.batches.flatMap(_._2)
    val spec = CaseSpec(plan.name, UpConfig(Seq(MasterId.t1p0, MasterId.t4p0, MasterId.t4p1),
      ExplicitRequestStream(all, Seq.fill(all.size)(BigInt(CHIFieldResp.CompData_SC))), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(plan.latency), FixedOutstanding(64), ImmediateCreditPolicy()), plan.watchdog, (0, plan.watchdog), Seq.empty)
    simulate(new TestTop_L2Top(cfg)(params)) { dut =>
      val up = new L2TopUpstreamModel(dut, spec)
      val driver = new L2TopConcurrentDriver(dut, up)
      val dn = new L2TopDownstreamModel(dut, spec)
      val data = mutable.Queue.empty[PendingDnComp]
      val rsp = mutable.Queue.empty[PendingDnRsp]
      val dnLive = mutable.Set.empty[DnTxnID]
      val snoops = mutable.Queue.empty[(SnpTxnID, BigInt)]
      val trains = mutable.ArrayBuffer.empty[(Int, Int, BigInt)]
      val received = mutable.ArrayBuffer.empty[(Int, BigInt)]
      val enqueues = mutable.ArrayBuffer.empty[(Int, BigInt)]
      val issues = mutable.ArrayBuffer.empty[(Int, Int, BigInt)]
      val queueModel = Seq.fill(plan.slices)(mutable.Queue.empty[BigInt])
      val blockedEnqueues = mutable.ArrayBuffer.empty[BigInt]
      var retained = Seq.empty[BigInt]
      var releasedAt = Option.empty[Int]
      var overwrites = 0
      var blockedOverwrites = 0
      var actualOverwrites = 0
      var priorityDrops = 0
      var reuseFirstCollisions = 0
      var maxTshrs = 0
      // Track reuse at allocator admission until each RBE actually dequeues.
      val reused = mutable.Map.empty[(Int, Int), Boolean]
      up.initInputs(); dn.initInputs()
      dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
      dut.reset.poke(true.B)
      for (c <- -308 until -300) dn.stepWithCredits(c)
      dut.reset.poke(false.B); dn.activateRxLink()
      for (c <- -300 until 0) { dn.clearResponses(); up.clearResponses(); dn.stepWithCredits(c) }
      var cycle = 0
      var quiet = 0
      var finished = false
      while (!finished && cycle < plan.watchdog) {
        dn.clearResponses(); up.clearResponses()
        plan.batches.filter(_._1 == cycle).foreach(b => driver.enqueue(b._2))
        driver.drive(cycle)
        val external = dut.io.t1p(0).UpREQ.valid.peek().litToBoolean
        // Starvation has a single batch at cycle 0 on t1p0. The snapshot is
        // taken before the first edge without external valid, not from data
        // completion timing. Later in-flight trains are tracked separately.
        if (plan.slices == 1 && plan.batches.size == 1 && cycle > 0 && !external && releasedAt.isEmpty) {
          releasedAt = Some(cycle); retained = queueModel.head.toSeq
        }
        if (dut.io.chi.txreq.flitv.peek().litToBoolean) {
          val flit = dut.io.chi.txreq.flit
          val id = DnTxnID(flit.TxnID.get.peek().litValue)
          assert(!dnLive(id), s"downstream TxnID reused while live: $id")
          dnLive += id; dn.noteRequest(id)
          val src = flit.SrcID.get.peek().litValue
          val addr = flit.Addr.get.peek().litValue
          val op = flit.Opcode.get.peek().litValue
          if (dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id, src, cycle + plan.latency, dn.downstreamEvictRspOpcode(op)))
          else {
            data.enqueue(PendingDnComp(id, src, cycle + plan.latency, 0, CHIFieldResp.CompData_SC, addr))
            data.enqueue(PendingDnComp(id, src, cycle + plan.latency + 1, 1, CHIFieldResp.CompData_SC, addr))
          }
        }
        data.dequeueFirst(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
          dn.driveDnData(p); dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
          if (p.beat == 1) assert(dnLive.remove(p.dnTxnID))
        }
        rsp.dequeueFirst(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
          dn.driveDnRsp(p); dn.noteResponse(p.dnTxnID, true, cycle); assert(dnLive.remove(p.dnTxnID))
        }
        if (!dut.io.t1p(0).UpRSP.valid.peek().litToBoolean) {
          snoops.headOption.foreach { case (id, src) => up.driveUpSnpResp(id, src) }
          if (snoops.nonEmpty && dut.io.t1p(0).UpRSP.ready.peek().litToBoolean) snoops.dequeue()
        }
        if (dut.io.t1p(0).DnSNP.valid.peek().litToBoolean) {
          snoops.enqueue((SnpTxnID(dut.io.t1p(0).DnSNP.bits.TxnID.peek().litValue),
            dut.io.t1p(0).DnSNP.bits.SrcID.peek().litValue))
        }
        var activeTshrs = 0
        (0 until plan.slices).foreach { s =>
          if (dut.observation.prefetchQueueOverwrite(s).peek().litToBoolean) {
            actualOverwrites += 1
            if (plan.slices == 1 && external) blockedOverwrites += 1
          }
          val local = mutable.ArrayBuffer.empty[(Int, Boolean)]
          (0 until 8).foreach { t =>
            val p = dut.observation.dirProbeBySlice(s)(t)
            if (p.valid.peek().litToBoolean || p.reqActive.peek().litToBoolean) activeTshrs += 1
            if (p.reqAlloc.peek().litToBoolean || p.reqReuse.peek().litToBoolean)
              reused((s, t)) = p.reqReuse.peek().litToBoolean
            if (p.bopTrainValid.peek().litToBoolean) local += ((t, reused.getOrElse((s, t), false)))
          }
          priorityDrops += (local.size - 1).max(0)
          if (local.size > 1 && local.exists(_._2) && local.exists(!_._2)) reuseFirstCollisions += 1
          if (local.size > 1) println(s"PRIORITYMUX_COLLISION cycle=$cycle slice=$s tshrs=$local drops=${local.size - 1}")
          if (dut.observation.sliceTrainValid(s).peek().litToBoolean) {
            val addr = dut.observation.sliceTrainAddr(s).peek().litValue
            trains += ((cycle, s, addr))
            assert(local.nonEmpty && addr == dut.observation.dirProbeBySlice(s)(local.head._1).tshrPaddr.peek().litValue)
          }
          if (dut.observation.bopPrefetchIssueBySlice(s).peek().litToBoolean) {
            val addr = dut.observation.bopPrefetchIssueAddrBySlice(s).peek().litValue
            assert(queueModel(s).nonEmpty && queueModel(s).dequeue() == addr, s"queue order mismatch slice=$s addr=$addr")
            issues += ((cycle, s, addr))
            if (plan.slices == 1) assert(!external, s"prefetch entered during external traffic cycle=$cycle")
          }
        }
        maxTshrs = maxTshrs.max(activeTshrs)
        if (dut.observation.bopTrainFire.peek().litToBoolean) received += ((cycle, dut.observation.bopTrainAddr.peek().litValue))
        if (dut.observation.bopReqFire.peek().litToBoolean) {
          val addr = dut.observation.bopReqAddr.peek().litValue
          val s = L2TopBOPExpectations.slice(addr, plan.slices)
          enqueues += ((cycle, addr))
          if (plan.slices == 1 && external) blockedEnqueues += addr
          if (queueModel(s).size == 2) { queueModel(s).dequeue(); overwrites += 1 }
          queueModel(s).enqueue(addr)
        }
        driver.sample(cycle)
        val pending = driver.pending + data.size + rsp.size + dnLive.size + snoops.size + queueModel.map(_.size).sum
        val lastBatch = plan.batches.map(_._1).max
        if (cycle >= lastBatch && driver.completed == all.size && pending == 0 && activeTshrs == 0 &&
            !dut.observation.bopReqValid.peek().litToBoolean) quiet += 1 else quiet = 0
        dn.stepWithCredits(cycle)
        finished = quiet >= 20
        cycle += 1
      }
      assert(finished, s"watchdog case=${plan.name} cycle=$cycle completed=${driver.completed}/${all.size} driverPending=${driver.pending} dn=${dnLive.size} queues=${queueModel.map(_.size)}")
      assert(driver.checkedBeats == all.size * 2)
      assert(actualOverwrites == overwrites, s"queue overwrite mismatch actual=$actualOverwrites expected=$overwrites")
      val trace = L2TopConcurrentTrace(driver.accepted.toSeq, trains.toSeq, received.toSeq,
        enqueues.toSeq, issues.toSeq, priorityDrops, reuseFirstCollisions, maxTshrs, driver.maxOutstanding,
        blockedEnqueues.toSeq, retained, actualOverwrites, blockedOverwrites, releasedAt, driver.checkedBeats, driver.completed)
      verify(trace)
      println(s"CONCURRENT_RESULT case=${plan.name} pass=true completed=${driver.completed} checkedBeats=${driver.checkedBeats} maxOutstanding=${driver.maxOutstanding} maxTshrs=$maxTshrs train=${received.size} slicePulses=${trains.size} priorityMuxDrops=$priorityDrops reuseFirstCollisions=$reuseFirstCollisions pending=0 active=0 watchdog=false")
    }
  }
}
