package oceanus

// Exercise real optional Recv/control ports through the production CCHI path.
// Check routing, CHI requests, committed Meta and fresh SRAM readback, first-use
// consumption, control drops, real Recv/BOP contention, and separate source ledgers.
// All expectations are literal source IDs or address-derived oracles; no RTL force.

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

object TestTop_L2Recv extends App with ChiselSim {
  val slices = args.collectFirst { case a if a.startsWith("--slices=") => a.drop(9).toInt }.getOrElse(1)
  require(Set(1, 2, 4)(slices))
  val selectedCase = args.collectFirst { case a if a.startsWith("--case=") => a.drop(7) }
  val recvCycles = args.collectFirst { case a if a.startsWith("--recv-cycles=") => a.drop(14).toInt }.getOrElse(200)
  require(recvCycles > 0, "--recv-cycles must be positive")
  val cfg = new L2Configuration(8, true, 0 until slices, bopEnable = true, pfSrcID = 3,
    bopParams = OceanusBOPParameters(badScore = 0), pfRecvEnable = true, pfCtrlEnable = true)
  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new TestTop_L2Top(cfg)(config)) { dut =>
    Seq("single", "disabled", "mixed", "resident", "dedup", "overflow", "sustained").filter(name => selectedCase.forall(_ == name)).foreach { name =>
      val requests = (0 until 64).map(i => UpRequestSpec(MasterId.t1p0, i, 0, 0,
        CCHIOpcode.ReadShared.opcode, BigInt(0), expCompData = true))
      val spec = CaseSpec("recv-" + name,
        UpConfig(Seq(MasterId.t1p0), ExplicitRequestStream(requests,
          Seq.fill(requests.size)(BigInt(CHIFieldResp.CompData_UC))), SnoopPolicy.SnpRespI),
        DnConfig(FixedLatency(12), FixedOutstanding(64), ImmediateCreditPolicy(),
          readNotSharedDirtyResp = BigInt(CHIFieldResp.CompData_SC)), 10000, (0, 10000), Seq.empty)
      val up = new L2TopUpstreamModel(dut, spec)
      val dn = new L2TopDownstreamModel(dut, spec)
      val dataQ = mutable.Queue.empty[PendingDnComp]
      val rspQ = mutable.Queue.empty[PendingDnRsp]
      val ackQ = mutable.Queue.empty[PendingUpAck]
      val snpQ = mutable.Queue.empty[PendingSnpRsp]
      val landed = mutable.Map.empty[BigInt, BigInt]
      val reads = mutable.ArrayBuffer.empty[(BigInt, Boolean, BigInt)]
      val downstream = mutable.ArrayBuffer.empty[(BigInt, BigInt)]
      val admissions = mutable.ArrayBuffer.empty[(BigInt, BigInt, Int)]
      val trains = mutable.ArrayBuffer.empty[BigInt]
      var cycle = 0
      var txn = 0
      var dataCount = 0
      var recvCount = 0
      val recvDirectEnteredBySlice = Array.fill(slices)(0)
      var collision = false
      def flag(b: Bool): Boolean = b.peek().litToBoolean
      def value(u: UInt): BigInt = u.peek().litValue
      def bank(addr: BigInt): Int = slices match {
        case 1 => 0
        case 2 => (addr >> 6).bitCount % 2
        case 4 =>
          val even = (0 until 48 by 2).count(addr.testBit) % 2
          val odd = (1 until 48 by 2).count(addr.testBit) % 2
          even + 2 * odd
      }
      def perf: Seq[BigInt] = L2BOPPerf.names.indices.map(i =>
        (0 until slices).map(s => value(dut.observation.bopPerfCountBySlice(s)(i))).sum)
      def quiet: Boolean = dataQ.isEmpty && rspQ.isEmpty && ackQ.isEmpty && snpQ.isEmpty &&
        !flag(dut.observation.bopReqValid) &&
        dut.observation.prefetchQueueValid.forall(q => !flag(q)) &&
        dut.observation.dirProbeBySlice.flatten.forall(d => !flag(d.valid) && !flag(d.reqActive))
      def tick(): Unit = {
        up.clearResponses(); dn.clearResponses()
        var rspDriven = false
        ackQ.headOption.filter(_.due <= cycle).foreach { a =>
          up.driveUpCompAck(a.upDBID, a.sliceIdx); ackQ.dequeue(); rspDriven = true
        }
        if (!rspDriven) snpQ.headOption.filter(_.due <= cycle).foreach { s =>
          up.driveUpSnpResp(s.snpTxnID, s.tgtId); snpQ.dequeue()
        }
        dataQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
          dn.driveDnData(p); dataQ.dequeue(); dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
        }
        rspQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
          dn.driveDnRsp(p); rspQ.dequeue(); dn.noteResponse(p.dnTxnID, finalBeat = true, cycle)
        }
        (0 until slices).foreach { s =>
          if (flag(dut.observation.sliceTrainValid(s))) trains += value(dut.observation.sliceTrainAddr(s))
          if (flag(dut.observation.recvIssue(s))) {
            recvCount += 1
            recvDirectEnteredBySlice(s) += 1
            val addr = value(dut.io.pfRecv.get.addr)
            val source = value(dut.observation.recvTxnID(s)) >> 4
            admissions += ((addr, source, s))
            assert(s == bank(addr), s"Recv eSAM routing addr=$addr slice=$s")
            if (flag(dut.observation.prefetchQueueValid(s))) {
              assert(!flag(dut.observation.prefetchQueueReady(s)), "Recv wins same-slice arbitration")
              collision = true
            }
          }
          dut.observation.dirProbeBySlice(s).foreach { d =>
            val addr = value(d.tshrPaddr)
            if (flag(d.dirWbLand)) landed(addr) = value(d.dirResultPfSource)
            if (flag(d.dirRdResp)) reads += ((addr, flag(d.fromDirMetaHit), value(d.fromDirMetaPfSource)))
            if (flag(d.reqActive) && flag(d.pRxReqIsPrefetch))
              assert(!flag(d.upTxRspFire) && !flag(d.upTxDatFire), "internal Recv/BOP has no upstream completion")
          }
        }
        if (flag(dut.io.chi.txreq.flitv)) {
          val id = DnTxnID(value(dut.io.chi.txreq.flit.TxnID.get))
          val src = value(dut.io.chi.txreq.flit.SrcID.get)
          val op = value(dut.io.chi.txreq.flit.Opcode.get)
          val addr = value(dut.io.chi.txreq.flit.Addr.get)
          downstream += ((addr, op)); dn.noteRequest(id)
          assert(!dn.isEvictReq(op), "directed addresses must avoid replacement")
          dataQ.enqueue(PendingDnComp(id, src, cycle + 12, 0, BigInt(CHIFieldResp.CompData_SC)),
            PendingDnComp(id, src, cycle + 13, 1, BigInt(CHIFieldResp.CompData_SC)))
        }
        if (flag(dut.io.t1p(0).DnDAT.valid) && flag(dut.io.t1p(0).DnDAT.ready)) {
          dataCount += 1
          if (value(dut.io.t1p(0).DnDAT.bits.DataID) == 1)
            ackQ.enqueue(PendingUpAck(0, UpDBID(value(dut.io.t1p(0).DnDAT.bits.DBID)),
              UpTxnID(value(dut.io.t1p(0).DnDAT.bits.TxnID)), cycle + 1,
              value(dut.io.t1p(0).DnDAT.bits.SrcID).toInt))
        }
        assert(!flag(dut.io.t1p(0).DnRSP.valid), "no dataless demand in these cases")
        if (flag(dut.io.t1p(0).DnSNP.valid) && flag(dut.io.t1p(0).DnSNP.ready))
          snpQ.enqueue(PendingSnpRsp(SnpTxnID(value(dut.io.t1p(0).DnSNP.bits.TxnID)),
            value(dut.io.t1p(0).DnSNP.bits.SrcID), cycle + 1))
        dn.stepWithCredits(cycle); cycle += 1
        assert(cycle < 10000L + recvCycles.toLong * 20, s"Recv watchdog case=$name")
      }
      def drain(): Unit = {
        var n = 0
        while (n < 24) { tick(); n = if (quiet) n + 1 else 0 }
        assert(dataQ.isEmpty && rspQ.isEmpty && ackQ.isEmpty && snpQ.isEmpty)
      }
      def recv(addr: BigInt, raw: Int = 10, enabled: Boolean = true): Unit = {
        dut.io.pfRecv.get.addr.poke(addr.U); dut.io.pfRecv.get.pf_source.poke(raw.U)
        dut.io.pfRecv.get.pf_en.poke(enabled.B); dut.io.pfRecv.get.addr_valid.poke(true.B)
        tick(); dut.io.pfRecv.get.addr_valid.poke(false.B)
      }
      def request(addr: BigInt, stash: Boolean = false, waitDrain: Boolean = true): Unit = {
        val r = requests(txn).copy(addr = addr,
          opcode = if (stash) CCHIOpcode.StashShared.opcode else CCHIOpcode.ReadShared.opcode,
          expCompData = !stash)
        // The model owns protocol defaults; replace only the request under test.
        up.driveRead(r)
        while (!flag(dut.io.t1p(0).UpREQ.ready)) tick()
        tick(); up.clearUpReq(); txn += 1
        if (waitDrain) drain()
      }
      def readback(addr: BigInt, source: Int): Unit = {
        reads.clear(); request(addr, stash = true)
        assert(reads.toSeq == Seq((addr, true, BigInt(source))), s"SRAM readback $reads expected=$source")
      }
      up.initInputs(); dn.initInputs()
      dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
      dut.io.pfRecv.get.addr.poke(0.U); dut.io.pfRecv.get.pf_source.poke(0.U)
      dut.io.pfRecv.get.pf_en.poke(true.B); dut.io.pfRecv.get.addr_valid.poke(false.B)
      val ctrl = dut.io.pfCtrl.get
      ctrl.l2_pf_master_en.poke(true.B); ctrl.l2_pf_recv_en.poke(true.B); ctrl.l2_pbop_en.poke(false.B)
      ctrl.l2_vbop_en.poke(false.B); ctrl.l2_tp_en.poke(false.B); ctrl.l2_cdp_en.poke(false.B)
      ctrl.l2_pf_delay_latency.poke(0.U)
      dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B); dn.activateRxLink()
      for (i <- 0 until 300) dn.stepWithCredits(i - 300)
      val target = BigInt("20040", 16)
      name match {
        case "single" =>
          // Global MemReqSource IDs -> fixed local IDs, including unknown fallback.
          Seq((8,2), (9,3), (10,1), (11,4), (12,5), (13,6), (14,7), (15,8), (16,9), (17,0)).zipWithIndex.foreach {
            case ((raw, mapped), i) =>
              val addr = target + i * 0x100
              val t = trains.size; val d = downstream.size; val p = perf
              recv(addr, raw); drain()
              assert(admissions.last._1 == addr && admissions.last._2 == mapped)
              assert(landed(addr) == mapped && trains.size == t)
              assert(downstream.drop(d).toSeq == Seq((addr, BigInt(L2TopDownstreamOpcodes.ReadNotSharedDirty))))
              readback(addr, mapped)
              request(addr)
              assert(landed(addr) == 0 && trains.size - t == (if (mapped == 0) 0 else 1))
              readback(addr, 0); request(addr)
              assert(trains.size - t == (if (mapped == 0) 0 else 1), "only first demand trains")
              val delta = perf.zip(p).map { case (a,b) => a-b }
              assert(delta == Seq(if (mapped != 0) 1 else 0,0,0,0,0,if (mapped != 0) 1 else 0,0,0).map(BigInt(_)))
              val sourceUseful = (0 until slices).flatMap(s =>
                (0 to L2PfSource.CDP).map(src => value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Useful))))
              assert(sourceUseful.sum == perf(L2BOPPerf.Useful), "useful source ledger must close")
              println(s"RECV_SOURCE_PHASE pass=true raw=$raw mapped=$mapped slices=$slices")
          }
        case "disabled" =>
          recv(target, enabled = false); drain()
          ctrl.l2_pf_recv_en.poke(false.B); recv(target + 0x100); drain()
          ctrl.l2_pf_recv_en.poke(true.B); ctrl.l2_pf_master_en.poke(false.B)
          recv(target + 0x200); drain()
          assert(recvCount == 0 && downstream.isEmpty && landed.isEmpty && trains.isEmpty)
          assert(perf.forall(_ == 0))
        case "mixed" =>
          ctrl.l2_pbop_en.poke(true.B)
          val demand = BigInt("10000", 16); val bopAddr = demand + 0x80
          request(demand, waitDrain = false)
          while (!dut.observation.prefetchQueueValid.exists(flag)) tick()
          // Same bank as the queued BOP request; separate set, no address hazard.
          val recvAddr = bopAddr + 0x1400
          recv(recvAddr); drain()
          assert(collision && recvCount == 1 && trains.toSeq == Seq(demand))
          assert(landed(recvAddr) == 1 && landed(bopAddr) == 3)
          assert(downstream.map(_._1).toSet == Set(demand, recvAddr, bopAddr) && downstream.size == 3)
          val issuedBySource = (0 to L2PfSource.CDP).map(src =>
            (0 until slices).map(s => value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.IssuedDownstream))).sum)
          assert(issuedBySource.sum == perf(L2BOPPerf.IssuedDownstream), s"issued source ledger=$issuedBySource perf=$perf")
          assert(issuedBySource(1) >= 1 && issuedBySource(3) >= 1, s"Recv/BOP source issue=$issuedBySource")
          ctrl.l2_pbop_en.poke(false.B)
          readback(recvAddr, 1); readback(bopAddr, 3)
          request(recvAddr); request(bopAddr)
          assert(trains.toSeq == Seq(demand, recvAddr, bopAddr))
          readback(recvAddr, 0); readback(bopAddr, 0)
          val usefulBySource = (0 to L2PfSource.CDP).map(src =>
            (0 until slices).map(s => value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Useful))).sum)
          assert(usefulBySource.sum == perf(L2BOPPerf.Useful), s"useful source ledger=$usefulBySource perf=$perf")
          assert(landed.values.forall(_ == 0))
          println("RECV_ACCOUNTING pass=true recv=1 recvUseful=1 recvResident=0 bopIssued=1 bopUseful=1 bopResident=0 pending=0")
        case "resident" =>
          request(target, stash = true)
          val d = downstream.size
          recv(target); drain(); readback(target, 0)
          assert(landed(target) == 0 && downstream.size == d && trains.isEmpty)
          recv(target + 0x100, raw = 11); drain(); readback(target + 0x100, 4)
          recv(target + 0x100, raw = 10); drain(); readback(target + 0x100, 4)
          assert(downstream.size == d + 1 && trains.isEmpty)
          val redundantBySource = (0 until slices).flatMap(s =>
            (0 to L2PfSource.CDP).map(src => value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Redundant))))
          assert(redundantBySource.sum == perf(L2BOPPerf.Redundant), s"redundant source ledger=$redundantBySource perf=$perf")
        case "dedup" =>
          ctrl.l2_pbop_en.poke(true.B)
          val demand = BigInt("30000", 16); val duplicate = demand + 0x80
          request(demand, waitDrain = false)
          while (!dut.observation.prefetchQueueValid.exists(flag)) tick()
          val before = downstream.size
          recv(duplicate, raw = 10); drain()
          assert(downstream.drop(before).count(_._1 == duplicate) == 1,
            s"cross-source duplicate issued more than once: ${downstream.drop(before)}")
          println("RECV_DEDUP_RESULT pass=true duplicateIssued=1")
        case "overflow" =>
          // Hold only the PBOP issue path while still allowing demand training;
          // this fills the 16-entry PBOP queue and exercises source-local
          // overwrite accounting without suppressing admission.
          ctrl.l2_pbop_en.poke(false.B)
          val base = BigInt("38000", 16)
          // Forty trains spread across four banks cannot fill a 16-entry
          // source queue. Keep this overflow fixture in one demand bank;
          // the initial +0x80 PBOP offset maps all hints to one bank too.
          val overflowAddresses = Iterator.from(0).map(i => base + i * 0x100)
            .filter(addr => bank(addr) == bank(base)).take(40).toVector
          overflowAddresses.foreach(addr => request(addr, waitDrain = false))
          (0 until 800).foreach(_ => tick())
          val held = (0 until slices).map(s => value(dut.observation.prefetchQueueOverwriteBySource(s)(3))).sum
          assert(held > 0, s"PBOP source queue overwrite counter did not rise: $held")
          ctrl.l2_pbop_en.poke(true.B); drain()
          println(s"RECV_OVERFLOW_RESULT pass=true source=3 overwrites=$held")
        case "sustained" =>
          // Build a real PBOP backlog with ordinary demand training, then
          // overlap continuous Recv with fresh demand refills in the window.
          // Snapshot pre-existing stock separately from window event deltas;
          // no training signal or production connection is overridden.
          val base = BigInt("48000", 16)
          (0 until 39).foreach(i => request(base + i * 0x100, waitDrain = false))
          (0 until 800).foreach(_ => tick())
          request(base + 39 * 0x100, waitDrain = false)
          while (!flag(dut.observation.bopTrainValid)) tick()
          val demandStopped = txn
          val trainBefore = value(dut.observation.bopTrainFireCount)
          val admissionBefore = recvCount
          def sum(counter: Vec[Vec[UInt]], source: Int): BigInt =
            (0 until slices).map(s => value(counter(s)(source))).sum
          def ledger(source: Int): Seq[BigInt] = Seq(
            sum(dut.observation.prefetchEnqueueBySource, source),
            sum(dut.observation.prefetchQueueOverwriteBySource, source),
            sum(dut.observation.prefetchEnterBySource, source),
            sum(dut.observation.prefetchDedupBySource, source),
            sum(dut.observation.prefetchPendingBySource, source))
          def checkLedger(): Unit = (0 until slices).foreach { s =>
            (0 to L2PfSource.CDP).foreach { source =>
              val enq = value(dut.observation.prefetchEnqueueBySource(s)(source))
              val overwrite = value(dut.observation.prefetchQueueOverwriteBySource(s)(source))
              val entered = value(dut.observation.prefetchEnterBySource(s)(source))
              val dedup = value(dut.observation.prefetchDedupBySource(s)(source))
              val pending = value(dut.observation.prefetchPendingBySource(s)(source))
              assert(enq == overwrite + entered + dedup + pending,
                s"queue ledger slice=$s source=$source enq=$enq overwrite=$overwrite entered=$entered dedup=$dedup pending=$pending")
            }
          }
          val before = ledger(3)
          val recvBefore = ledger(1)
          val acceptedBefore = sum(dut.observation.recvAcceptedBySource, 1)
          val enteredBefore = sum(dut.observation.prefetchSliceEnteredBySource, 1)
          val acceptedBeforeBySlice = (0 until slices).map(s => value(dut.observation.recvAcceptedBySource(s)(1)))
          val enteredBeforeBySlice = (0 until slices).map(s => value(dut.observation.prefetchSliceEnteredBySource(s)(1)))
          val directBeforeBySlice = recvDirectEnteredBySlice.clone()
          ctrl.l2_pbop_en.poke(true.B)
          // Keep Recv continuous while 24 fresh demand misses enter the same
          // production path. Their real refills/train pulses must create at
          // least twenty BOP queue admissions inside this measurement window.
          val windowDemands = (0 until 24).map(i => requests((demandStopped + i) % requests.size)
            .copy(addr = BigInt("70000", 16) + i * 0x100))
          var windowDemandIdx = 0
          (0 until recvCycles).foreach { i =>
            if (windowDemandIdx < windowDemands.size) up.driveRead(windowDemands(windowDemandIdx))
            dut.io.pfRecv.get.addr.poke((BigInt("60000", 16) + (i % 64) * 64).U)
            dut.io.pfRecv.get.pf_source.poke(10.U); dut.io.pfRecv.get.pf_en.poke(true.B)
            dut.io.pfRecv.get.addr_valid.poke(true.B)
            val demandFire = flag(dut.io.t1p(0).UpREQ.valid) && flag(dut.io.t1p(0).UpREQ.ready)
            tick()
            if (demandFire) { up.clearUpReq(); txn += 1; windowDemandIdx += 1 }
            dut.io.pfRecv.get.addr_valid.poke(false.B)
            checkLedger()
          }
          up.clearUpReq()
          val bopTotal = ledger(3); val bop = bopTotal.zip(before).map { case (now, old) => now - old }
          val receiverTotal = ledger(1); val receiver = receiverTotal.zip(recvBefore).map { case (now, old) => now - old }
          val trainDuring = value(dut.observation.bopTrainFireCount) - trainBefore
          val directEntered = recvCount - admissionBefore
          assert(receiver(0) + directEntered == recvCycles,
            s"continuous Recv acceptance must equal N=$recvCycles, got queue=${receiver(0)} direct=$directEntered")
          assert(bop(0) >= 20, s"BOP must enqueue at least 20 times in the Recv window, got ${bop(0)}")
          assert(trainDuring > 0, "real in-flight demand training must overlap Recv")
          // The accepted-input and slice-entry counters are independent probes
          // of the two ends of the production path, including the direct route.
          // This equality would fail on the old duplicate-injection wiring.
          val directBySlice = recvDirectEnteredBySlice.zip(directBeforeBySlice).map { case (now, old) => now - old }
          val accepted = sum(dut.observation.recvAcceptedBySource, 1) - acceptedBefore
          val entered = sum(dut.observation.prefetchSliceEnteredBySource, 1) - enteredBefore
          assert(accepted == recvCycles && accepted == directEntered + receiver(0))
          assert(accepted + recvBefore(4) == entered + receiver(3) + receiver(1) + receiverTotal(4),
            s"Recv conservation accepted=$accepted entered=$entered dropped=${receiver(3)} overwritten=${receiver(1)} pending=${receiverTotal(4)}")
          assert(before(4) + bop(0) == bop(1) + bop(2) + bop(3) + bopTotal(4))
          (0 until slices).foreach { s =>
            val direct = directBySlice(s)
            val enqueued = value(dut.observation.prefetchEnqueueBySource(s)(1))
            val overwritten = value(dut.observation.prefetchQueueOverwriteBySource(s)(1))
            val dropped = value(dut.observation.prefetchDedupBySource(s)(1))
            val pending = value(dut.observation.prefetchPendingBySource(s)(1))
            val inputCount = value(dut.observation.recvAcceptedBySource(s)(1)) - acceptedBeforeBySlice(s)
            val sliceCount = value(dut.observation.prefetchSliceEnteredBySource(s)(1)) - enteredBeforeBySlice(s)
            assert(inputCount == direct + enqueued)
            assert(inputCount == sliceCount + dropped + overwritten + pending,
              s"Recv conservation slice=$s accepted=$inputCount entered=$sliceCount dropped=$dropped overwritten=$overwritten pending=$pending")
          }
          println(s"RECV_SUSTAINED_RESULT pass=true slices=$slices N=$recvCycles bopEnqueued=${bop(0)} bopOverwritten=${bop(1)} bopEntered=${bop(2)} recvEntered=$entered " +
            s"bopPendingAtWindowStart=${before(4)} recvPendingAtWindowStart=${recvBefore(4)} bopDedup=${bop(3)} bopPendingAtWindowEnd=${bopTotal(4)} recvEnqueued=${receiver(0)} recvOverwritten=${receiver(1)} recvDedup=${receiver(3)} recvPending=${receiverTotal(4)} recvDirectEntered=$directEntered recvQueueEntered=${receiver(2)} trainDuring=$trainDuring windowDemands=$windowDemandIdx")
          println(s"RECV_CONSERVATION pass=true slices=$slices accepted=$accepted entered=$entered dropped=${receiver(3)} overwritten=${receiver(1)} pending=${receiverTotal(4)}")
          drain(); checkLedger()
          assert(ledger(3)(4) == 0 && ledger(1)(4) == 0)
          assert(ledger(3)(0) == value(dut.observation.bopTrainFireCount), "all accepted training enqueued in this address range")
          assert(ledger(1)(0) + (recvCount - admissionBefore) == recvCycles)
          val drainedEntered = sum(dut.observation.prefetchSliceEnteredBySource, 1) - enteredBefore
          assert(accepted == drainedEntered + ledger(1)(1) + ledger(1)(3))
          println(s"RECV_SUSTAINED_DRAIN pass=true slices=$slices bop=${ledger(3).mkString(",")} recv=${ledger(1).mkString(",")} recvDirectEntered=${recvCount-admissionBefore} order=enqueued,overwritten,entered,dedup,pending")
      }
      drain()
      L2BOPPerf.names.indices.foreach { metric =>
        val bySource = (0 until slices).flatMap(s =>
          (0 to L2PfSource.CDP).map(src => value(dut.observation.bopPerfCountBySliceSource(s)(src)(metric))))
        assert(bySource.sum == perf(metric), s"source ledger metric=${L2BOPPerf.names(metric)} bySource=$bySource total=${perf(metric)}")
      }
      assert(dataCount == (if (name == "single") 40 else if (name == "mixed") 6 else if (name == "dedup") 2 else if (name == "overflow") 80 else if (name == "sustained") 128 else 0))
      val queuePending = (0 until slices).flatMap(s => (0 to L2PfSource.CDP).map(source =>
        value(dut.observation.prefetchPendingBySource(s)(source)))).sum
      // Non-PBOP sources originate exclusively in Recv, so their end-to-end
      // input/entry balance also covers all mapping/control/dedup fixtures.
      // PBOP shares its source with the autonomous producer; its queue balance
      // is checked independently in the sustained fixture above.
      (0 until slices).foreach { s =>
        (0 to L2PfSource.CDP).filter(_ != L2PfSource.PBOP).foreach { source =>
          val accepted = value(dut.observation.recvAcceptedBySource(s)(source))
          val entered = value(dut.observation.prefetchSliceEnteredBySource(s)(source))
          val dropped = value(dut.observation.prefetchDedupBySource(s)(source))
          val overwritten = value(dut.observation.prefetchQueueOverwriteBySource(s)(source))
          val pending = value(dut.observation.prefetchPendingBySource(s)(source))
          assert(accepted == entered + dropped + overwritten + pending,
            s"Recv end-to-end balance case=$name slice=$s source=$source accepted=$accepted entered=$entered dropped=$dropped overwritten=$overwritten pending=$pending")
        }
      }
      println(s"RECV_RESULT pass=true case=$name slices=$slices admissions=$recvCount downstream=${downstream.size} train=${trains.size} pending=$queuePending")
    }
  }
}
