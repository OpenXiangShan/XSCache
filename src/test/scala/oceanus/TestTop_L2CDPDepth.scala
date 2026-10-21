package oceanus

// Intent (1c-2): inject depth only through a test subclass's internal request
// encoder, then exercise real refill, directory SRAM readback, first demand,
// queued late demand and capacity eviction. Never force Meta or TSHR state.

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

object TestTop_L2CDPDepth extends App with ChiselSim {
  require(L2TopPhase1aPorts.enabled && sys.props.get("oceanus.phase1c2.depth").contains("true"))
  val cfg = new L2Configuration(8, true, 0 until 1, bopEnable = true,
    pfRecvEnable = true, pfCtrlEnable = true, pfTrainEnable = true, pfTlbEnable = true)
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new TestTop_L2Top(cfg)(params)) { dut =>
    val requests = (0 until 64).map(index => UpRequestSpec(MasterId.t1p0, index, 0, 0,
      CCHIOpcode.ReadShared.opcode, BigInt(0), expCompData = true))
    val spec = CaseSpec("cdp-depth", UpConfig(Seq(MasterId.t1p0),
      ExplicitRequestStream(requests, Seq.fill(requests.size)(BigInt(CHIFieldResp.CompData_SC))), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(60), FixedOutstanding(64), ImmediateCreditPolicy()), 16000, (0, 16000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec)
    val dn = new L2TopDownstreamModel(dut, spec)
    val cdp = new L2TopCDPChecker(dut)
    val dataQ = mutable.Queue.empty[PendingDnComp]
    val rspQ = mutable.Queue.empty[PendingDnRsp]
    val ackQ = mutable.Queue.empty[PendingUpAck]
    val snpQ = mutable.Queue.empty[PendingSnpRsp]
    val downstream = mutable.ArrayBuffer.empty[BigInt]
    val installations = mutable.Map.empty[BigInt, (BigInt, BigInt)]
    var cycle = 0
    var txn = 0
    var checkedBeats = 0
    var activeAddr = BigInt(0)
    def flag(signal: Bool): Boolean = signal.peek().litToBoolean
    def value(signal: UInt): BigInt = signal.peek().litValue
    def tick(): Unit = {
      up.clearResponses(); dn.clearResponses()
      ackQ.headOption.foreach { ack => up.driveUpCompAck(ack.upDBID, ack.sliceIdx); ackQ.dequeue() }
      if (!flag(dut.io.t1p(0).UpRSP.valid)) snpQ.headOption.foreach { snp =>
        up.driveUpSnpResp(snp.snpTxnID, snp.tgtId); snpQ.dequeue()
      }
      dataQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { response =>
        dn.driveDnData(response); dataQ.dequeue(); dn.noteResponse(response.dnTxnID, response.beat == 1, cycle)
      }
      rspQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxRsp).foreach { response =>
        dn.driveDnRsp(response); rspQ.dequeue(); dn.noteResponse(response.dnTxnID, true, cycle)
      }
      if (flag(dut.io.chi.txreq.flitv)) {
        val flit = dut.io.chi.txreq.flit
        val id = DnTxnID(value(flit.TxnID.get)); val src = value(flit.SrcID.get)
        val opcode = value(flit.Opcode.get); val addr = value(flit.Addr.get)
        dn.noteRequest(id)
        if (dn.isEvictReq(opcode)) rspQ.enqueue(PendingDnRsp(id, src, cycle + 60, dn.downstreamEvictRspOpcode(opcode)))
        else {
          downstream += addr
          (0 until 2).foreach(beat => dataQ.enqueue(PendingDnComp(id, src, cycle + 60 + beat, beat,
            BigInt(CHIFieldResp.CompData_SC), addr)))
        }
      }
      val port = dut.io.t1p(0)
      if (flag(port.DnDAT.valid)) {
        val flit = port.DnDAT.bits
        val beat = value(flit.DataID).toInt
        assert(value(flit.Data) == L2TopReferenceMemory.beat(activeAddr, beat))
        checkedBeats += 1
        if (beat == 1) ackQ.enqueue(PendingUpAck(0, UpDBID(value(flit.DBID)),
          UpTxnID(value(flit.TxnID)), cycle + 1, value(flit.SrcID).toInt))
      }
      if (flag(port.DnSNP.valid)) snpQ.enqueue(PendingSnpRsp(SnpTxnID(value(port.DnSNP.bits.TxnID)), value(port.DnSNP.bits.SrcID), cycle + 1))
      assert(!flag(port.DnRSP.valid))
      cdp.sample()
      dn.stepWithCredits(cycle); cycle += 1
      assert(cycle < 16000, "CDP depth watchdog")
    }
    def drain(): Unit = {
      var quiet = 0
      while (quiet < 25) {
        tick()
        val idle = dataQ.isEmpty && rspQ.isEmpty && ackQ.isEmpty && snpQ.isEmpty &&
          dut.observation.dirProbeBySlice.flatten.forall(probe => !flag(probe.valid) && !flag(probe.reqActive))
        quiet = if (idle) quiet + 1 else 0
      }
    }
    def prefetch(addr: BigInt, depth: Int, wait: Boolean = true): Unit = {
      val recv = dut.io.pfRecv.get
      dut.testDepth.get.poke(depth.U)
      recv.addr.poke(addr.U); recv.pf_source.poke(16.U)
      recv.addr_valid.poke(true.B); tick(); recv.addr_valid.poke(false.B)
      installations(addr) = (BigInt(9), BigInt(depth))
      if (wait) drain()
    }
    def request(addr: BigInt, stash: Boolean = false, disabled: Boolean = false): Unit = {
      activeAddr = addr
      up.driveRead(requests(txn).copy(addr = addr,
        opcode = if (stash) CCHIOpcode.StashShared.opcode else CCHIOpcode.ReadShared.opcode, expCompData = !stash))
      // Intent (1c-2): ordinary, already-consumed hits still qualify for CDP
      // training; verify disabled-event accounting includes these BOP non-events.
      dut.io.pfTrain.get.head.trainEnable.poke((!disabled).B)
      while (!flag(dut.io.t1p(0).UpREQ.ready)) tick()
      tick(); up.clearUpReq(); txn += 1
      drain()
    }
    def readback(addr: BigInt, source: Int, depth: Int): Unit = {
      val before = cdp.reads.size
      request(addr, stash = true)
      assert(cdp.reads.drop(before).toSeq == Seq((addr, BigInt(source), BigInt(depth))),
        s"depth SRAM readback addr=$addr got=${cdp.reads.drop(before)} expected=$source/$depth")
    }
    up.initInputs(); dn.initInputs()
    dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
    dut.testDepth.get.poke(0.U)
    dut.io.pfRecv.get.addr.poke(0.U); dut.io.pfRecv.get.pf_source.poke(16.U)
    dut.io.pfRecv.get.pf_en.poke(true.B); dut.io.pfRecv.get.addr_valid.poke(false.B)
    val ctrl = dut.io.pfCtrl.get
    ctrl.l2_pf_master_en.poke(true.B); ctrl.l2_pf_recv_en.poke(true.B); ctrl.l2_pbop_en.poke(false.B)
    ctrl.l2_vbop_en.poke(false.B); ctrl.l2_cdp_en.poke(false.B); ctrl.l2_tp_en.poke(false.B)
    ctrl.l2_pf_delay_latency.poke(0.U)
    dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B); dn.activateRxLink()
    for (warmup <- -300 until 0) dn.stepWithCredits(warmup)
    for (depth <- 0 until 4) {
      val addr = BigInt(0x20040 + depth * 0x100)
      prefetch(addr, depth)
      assert(cdp.writes.contains((addr, BigInt(9), BigInt(depth))))
      readback(addr, 9, depth)
      val before = downstream.size
      request(addr)
      assert(downstream.size == before)
      assert(cdp.trains.contains((addr, BigInt(9), BigInt(depth), true)))
      readback(addr, 0, 0)
      request(addr, disabled = true)
      assert(cdp.trains.last == ((addr, BigInt(0), BigInt(0), true)))
      println(s"CDP_DEPTH_PHASE pass=true depth=$depth SRAM=true oldMeta=true clear=true")
    }
    val lateAddr = BigInt(0x38040)
    prefetch(lateAddr, 3, wait = false)
    while (!downstream.contains(lateAddr)) tick()
    request(lateAddr)
    assert(cdp.trains.contains((lateAddr, BigInt(9), BigInt(3), true)))
    readback(lateAddr, 0, 0)
    val beforeEvict = cdp.evictions.size
    for (index <- 0 until 10) prefetch(BigInt(0x80000 + index * 0x2000), index % 3 + 1)
    val victims = cdp.evictions.drop(beforeEvict)
    assert(victims.size >= 6)
    victims.foreach { case (addr, source, depth) =>
      assert(installations.get(addr).contains((source, depth)) && source == 9 && depth != 0,
        s"victim PA/old Meta mismatch: $addr $source $depth")
    }
    cdp.report()
    assert(cdp.forwarded >= 15 && cdp.hits >= 5 && checkedBeats == 18 && cdp.disabled >= 4)
    println(s"CDP_DEPTH_RESULT pass=true depths=0,1,2,3 lateRBE=true ordinaryDisabledHits=4 victims=${victims.size} checkedBeats=$checkedBeats")
  }
}
