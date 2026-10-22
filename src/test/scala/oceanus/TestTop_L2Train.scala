// Intent (audit-fix-1): require disabled-event coverage before arbitration.
package oceanus

import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable

// Phase 1b expectations before implementation: two Type-1 ports on distinct
// PAs accept together, and train together across slices. Both cold miss and
// first prefetched hit carry their own VA/source/enable. A demand submitted
// after real internal prefetch admission but before its response must queue
// in the same REQ RBE and train later with its own (different) VPN.
object TestTop_L2Train extends App with ChiselSim {
  require(L2TopPhase1aPorts.enabled && L2TopTrainStimulus.driven)
  val mixedEnable = args.contains("--mixed-enable")
  val count = args.collectFirst { case a if a.startsWith("--slices=") => a.drop(9).toInt }.getOrElse(2)
  require(Set(2, 4)(count))
  val upstream = new L2UpstreamTable(Seq(L2UpstreamTableEntry(L2UpstreamPortType.Type1, 0),
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 6)))
  val cfg = new L2Configuration(8, true, 0 until count, upstream = upstream, bopEnable = true,
    pfTrainEnable = true, pfTlbEnable = true,
    bopParams = OceanusBOPParameters(badScore = 0, roundMax = 100000))
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new TestTop_L2Top(cfg)(params)) { dut =>
    val spec = CaseSpec("phase1b-train", UpConfig(Seq(MasterId.t1p0),
      ExplicitRequestStream(Seq.empty, Seq.empty), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(100), FixedOutstanding(64), ImmediateCreditPolicy()), 6000, (0, 6000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec)
    val dn = new L2TopDownstreamModel(dut, spec)
    // Intent (1c-2): run independent line/FIFO/Meta checks on these real
    // simultaneous-slice requests and the demand queued behind a prefetch.
    val cdp = new L2TopCDPChecker(dut)
    val dataQ = mutable.Queue.empty[PendingDnComp]
    val rspQ = mutable.Queue.empty[PendingDnRsp]
    case class Req(addr: BigInt, txn: Int)
    case class Read(addr: BigInt, beats: mutable.Set[Int] = mutable.Set.empty)
    val active = mutable.Map.empty[(Int, Int), Read]
    val held = mutable.Map.empty[Int, Req]
    val acks = Seq.fill(2)(mutable.Queue.empty[(Int, BigInt, BigInt)])
    val snoops = Seq.fill(2)(mutable.Queue.empty[(BigInt, BigInt)])
    val accepted = mutable.ArrayBuffer.empty[(Int, Int, BigInt)]
    var completed = 0; var checkedBeats = 0; var cycle = 0
    def flag(x: Bool): Boolean = x.peek().litToBoolean
    def value(x: UInt): BigInt = x.peek().litValue
    up.initInputs(); dn.initInputs()
    dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
    val second = dut.io.t1p(1)
    second.UpREQ.valid.poke(false.B); second.UpREQ.bits.poke(0.U.asTypeOf(second.UpREQ.bits))
    second.UpEVT.valid.poke(false.B); second.UpEVT.bits.poke(0.U.asTypeOf(second.UpEVT.bits))
    second.UpRSP.valid.poke(false.B); second.UpRSP.bits.poke(0.U.asTypeOf(second.UpRSP.bits))
    second.UpDAT.valid.poke(false.B); second.UpDAT.bits.poke(0.U.asTypeOf(second.UpDAT.bits))
    second.DnSNP.ready.poke(true.B); second.DnRSP.ready.poke(true.B); second.DnDAT.ready.poke(true.B)
    dut.reset.poke(true.B)
    for (c <- -308 until -300) dn.stepWithCredits(c)
    dut.reset.poke(false.B); dn.activateRxLink()
    for (c <- -300 until 0) dn.stepWithCredits(c)
    def add(port: Int, addr: BigInt, txn: Int): Unit = {
      assert(!held.contains(port)); held(port) = Req(addr, txn)
    }
    def tick(): Unit = {
      assert(cycle < 6000, s"watchdog active=$active held=$held completed=$completed")
      dn.clearResponses()
      dut.io.t1p.zipWithIndex.foreach { case (port, i) =>
        port.UpREQ.valid.poke(false.B); port.UpRSP.valid.poke(false.B)
        // Poison inactive sideband immediately after acceptance. A broken
        // execution-time bypass would pick this unrelated VPN/source/enable.
        L2TopTrainStimulus.poke(dut.io.pfTrain.get(i), 0x777fc0, 7, cycle & 255)
        held.get(i).foreach { r =>
          val req = port.UpREQ
          req.valid.poke(true.B); req.bits.poke(0.U.asTypeOf(req.bits))
          req.bits.SrcID.poke((if (i == 0) 0 else 6).U); req.bits.TxnID.poke(r.txn.U)
          req.bits.Opcode.poke(CCHIOpcode.ReadShared.opcode.U); req.bits.Size.poke(6.U)
          req.bits.MemAttr.poke(15.U); req.bits.ExpCompData.poke(true.B); req.bits.Addr.poke(r.addr.U)
          L2TopTrainStimulus.poke(dut.io.pfTrain.get(i), r.addr, if (i == 0) 0 else 6, r.txn)
        }
        acks(i).headOption match {
          case Some((_, dbid, slice)) =>
            port.UpRSP.valid.poke(true.B); port.UpRSP.bits.poke(0.U.asTypeOf(port.UpRSP.bits))
            port.UpRSP.bits.Opcode.poke(CCHIOpcode.CompAck.opcode.U)
            port.UpRSP.bits.SrcID.poke((if (i == 0) 0 else 6).U)
            port.UpRSP.bits.TxnID.poke(dbid.U); port.UpRSP.bits.TgtID.poke(slice.U)
          case None => snoops(i).headOption.foreach { case (txn, src) =>
            port.UpRSP.valid.poke(true.B); port.UpRSP.bits.poke(0.U.asTypeOf(port.UpRSP.bits))
            port.UpRSP.bits.Opcode.poke(CCHIOpcode.SnpResp.opcode.U)
            port.UpRSP.bits.SrcID.poke((if (i == 0) 0 else 6).U)
            port.UpRSP.bits.TxnID.poke(txn.U); port.UpRSP.bits.TgtID.poke(src.U)
          }
        }
        if (flag(port.UpRSP.valid) && flag(port.UpRSP.ready)) {
          if (acks(i).nonEmpty) { val (txn, _, _) = acks(i).dequeue(); assert(active.remove((i, txn)).nonEmpty); completed += 1 }
          else snoops(i).dequeue()
        }
        if (flag(port.DnSNP.valid)) snoops(i).enqueue((value(port.DnSNP.bits.TxnID), value(port.DnSNP.bits.SrcID)))
        if (flag(port.UpREQ.valid) && flag(port.UpREQ.ready)) {
          val r = held.remove(i).get; assert(!active.contains((i, r.txn)))
          active((i, r.txn)) = Read(r.addr); accepted += ((cycle, i, r.addr))
        }
        if (flag(port.DnDAT.valid)) {
          val flit = port.DnDAT.bits; val txn = value(flit.TxnID).toInt
          val read = active((i, txn)); val beat = value(flit.DataID).toInt
          assert(!read.beats(beat)); read.beats += beat
          assert(value(flit.Data) == L2TopReferenceMemory.beat(read.addr, beat))
          checkedBeats += 1
          if (read.beats.size == 2) acks(i).enqueue((txn, value(flit.DBID), value(flit.SrcID)))
        }
      }
      if (flag(dut.io.chi.txreq.flitv)) {
        val flit = dut.io.chi.txreq.flit; val id = DnTxnID(value(flit.TxnID.get))
        val src = value(flit.SrcID.get); val op = value(flit.Opcode.get)
        dn.noteRequest(id)
        if (dn.isEvictReq(op)) rspQ.enqueue(PendingDnRsp(id, src, cycle + 100, dn.downstreamEvictRspOpcode(op)))
        else (0 until 2).foreach(b => dataQ.enqueue(PendingDnComp(id, src, cycle + 100 + b, b,
          CHIFieldResp.CompData_SC, value(flit.Addr.get))))
      }
      dataQ.dequeueFirst(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
        dn.driveDnData(p); dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
      }
      rspQ.dequeueFirst(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
        dn.driveDnRsp(p); dn.noteResponse(p.dnTxnID, true, cycle)
      }
      cdp.sample()
      dn.stepWithCredits(cycle); cycle += 1
    }
    def drain(): Unit = {
      var quiet = 0
      while (quiet < 25) {
        tick()
        val idle = held.isEmpty && active.isEmpty && dataQ.isEmpty && rspQ.isEmpty &&
          acks.forall(_.isEmpty) && snoops.forall(_.isEmpty) && dn.trainChecker.pending == 0 &&
          dut.observation.dirProbeBySlice.flatten.forall(p => !flag(p.valid) && !flag(p.reqActive)) &&
          !flag(dut.observation.bopReqValid)
        if (idle) quiet += 1 else quiet = 0
      }
    }
    // Pick distinct sets and hash destinations. Equal request timing gives
    // simultaneous cold-directory reads without any metadata manipulation.
    val cold = (0 until count).map { s =>
      (0 until 64).map(i => BigInt(0x20000 + i * 0x100)).find(a => L2TopBOPExpectations.slice(a, count) == s).get
    }
    cold.grouped(2).zipWithIndex.foreach { case (pair, group) =>
      pair.zipWithIndex.foreach { case (a, port) => add(port, a, group * 4) }
      drain()
      // First hit has a fresh VPN/reqSource; odd txn makes trainEnable=false
      // for these page-aligned-offset addresses. The existing lossy global
      // arbiter accepts one of the simultaneous cold trains: exactly one
      // target was prefetched, while the other target is a new demand miss.
      val before = dn.trainChecker.hits
      pair.zipWithIndex.foreach { case (a, port) => add(port, a + 0x80, group * 4 + 1) }
      drain(); assert(dn.trainChecker.hits - before == 1, s"one BOP target per simultaneous pair: hits=${dn.trainChecker.hits - before}")
    }
    // Intent (audit-fix-1): real requests execute simultaneously on two slices;
    // the lower slice is disabled, and the higher slice must train unchanged.
    if (mixedEnable) {
      val pair = (0 until 2).map { s =>
        (0 until 128).map(i => BigInt(0x50000+i*0x100)).find(a => L2TopBOPExpectations.slice(a,count)==s).get
      }
      val before = dn.trainChecker.acceptedByBop
      val mixedBefore = dn.trainChecker.mixedGlobal
      add(0,pair(0),31); add(1,pair(1),32)
      drain()
      assert(dn.trainChecker.mixedGlobal > mixedBefore, "mixed real execution did not coincide")
      assert(dn.trainChecker.acceptedByBop == before+1, "enabled peer lost to disabled train")
      println(s"TRAIN_MIXED_INTEGRATION pass=true slices=$count enabledAccepted=1 disabledInputs=1")
    }
    // A separate cold miss produces an actual in-flight BOP request. Submit
    // its target on the other port before the 100-cycle downstream response.
    val lateBase = BigInt(0x38000)
    add(0, lateBase, 20)
    var injected = false
    while (!injected) {
      val pulses = dut.observation.bopPrefetchIssueBySlice.zip(dut.observation.bopPrefetchIssueAddrBySlice)
      if (pulses.exists { case (v, a) => flag(v) && value(a) == lateBase + 0x80 }) injected = true
      tick()
    }
    add(1, lateBase + 0x80, 21)
    drain()
    val c = dn.trainChecker
    assert(c.misses >= count + 1 && c.hits >= count / 2 + 1 && c.disabled > 0 && c.internal > 0)
    assert(c.lateHits >= 1 && c.simultaneousAccepts > 0 && c.simultaneousTrains > 0)
    assert(c.pending == 0 && c.accepts == c.executions)
    assert(completed == 2 * count + 2 + (if(mixedEnable) 2 else 0) && checkedBeats == completed * 2)
    c.report()
    if (dut.cdpObservation.nonEmpty) {
      assert(cdp.lines > 0 && cdp.forwarded > 0 && cdp.hits > 0 && cdp.disabled > 0)
      cdp.report()
    }
    // Count disabled raw events before arbitration and enabled BOP accepts.
    assert(c.disabledBeforeArb > 0 && c.acceptedByBop > 0)
    println(s"TRAIN_GATE_RESULT pass=true enabled=${c.acceptedByBop} disabledInputs=${c.disabledBeforeArb} slices=$count")
    println(s"VA_DIRECTED_RESULT pass=true slices=$count completed=$completed beats=$checkedBeats miss=true firstHit=true lateOwnVA=true enableZero=true multiPort=true multiSlice=true internalZero=true")
  }
}
