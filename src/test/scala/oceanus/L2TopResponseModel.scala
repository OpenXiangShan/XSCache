package oceanus

import chisel3._
import chisel3.util.log2Ceil
import _root_.circt.stage.ChiselStage
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.simulator.{ChiselSim, HasSimulator}
import utility._
import oceanus.l2._
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.compactchi.{CCHIOpcode, CCHIResp}
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.util.Try
import java.io.File
import svsim.verilator.Backend.CompilationSettings.TraceKind
import svsim.verilator.Backend.CompilationSettings.TraceStyle

object L2TopBuiltInResponseModel extends L2TopResponseModel with ChiselSim {
  def run(args: Array[String]): Unit = {
    val usage = """
  Usage: TestTop_L2Top [<--option> <values>]

        --slices <slice_num>      specify the number of L2 slices, 2 by default;
                                  external SAM supports 1 to 4 slices
        --mshr-size=<n>           specify the L2 MSHR count, 8 by default
        --case=<name>             select simulate case: load-miss or dirty-victim
        --policy-probe             run a multi-request downstream policy probe
        --dn-latency=<n>           fixed downstream response latency for policy probe
        --dn-max-outstanding=<n>   downstream response concurrency limit
        --dn-credit-return-delay=<n> delay TX credit return by n cycles
        --probe-requests=<n>       request count for policy probe
        --probe-cycles=<n>         watchdog cycles for policy probe
        --simulate                run the built-in response model
        --vcd                     dump VCD when used with --simulate
    """

    if (args.contains("--help"))
    {
      println(usage)
      System.exit(0)
    }

    val simulateMode = args.contains("--simulate")
    val dumpVcd = args.contains("--vcd")
    val caseArgPrefix = "--case="
    val mshrSizeArgPrefix = "--mshr-size="
    val dnLatencyArgPrefix = "--dn-latency="
    val dnOutstandingArgPrefix = "--dn-max-outstanding="
    val dnCreditDelayArgPrefix = "--dn-credit-return-delay="
    val probeRequestsArgPrefix = "--probe-requests="
    val probeCyclesArgPrefix = "--probe-cycles="
    val testMshrSize = args.collectFirst {
      case s if s.startsWith(mshrSizeArgPrefix) => s.stripPrefix(mshrSizeArgPrefix).toInt
    }.getOrElse(8)
    val selectedCase = args.collectFirst {
      case s if s.startsWith(caseArgPrefix) => s.stripPrefix(caseArgPrefix)
    }.getOrElse("load-miss")
    val bopClosedLoop = selectedCase == "bop-closed-loop" || selectedCase == "bop-closed-loop-uc"
    val bopCollision = selectedCase.startsWith("bop-collision-")
    val bopOverwriteQueue = selectedCase == "bop-overwrite-queue"
    val bopPrefetchSmoke = selectedCase == "bop-prefetch-smoke" || selectedCase == "bop-prefetch-smoke-uc"
    // The short offset=2 smoke isolates issue/fill routing from learning.
    // Startup idle assigns disable=(bestScore < badScore): default 0<1
    // disables the first learning phase. Smoke-only 0<0 is false, preserving
    // all eight expected requests; production badScore=1 remains unchanged.
    val bopParams = if (bopPrefetchSmoke) OceanusBOPParameters(badScore = 0)
                    else if (bopClosedLoop || bopOverwriteQueue || bopCollision) OceanusBOPParameters(delayQueueLatency = 300, roundMax = 4)
                    else OceanusBOPParameters()
    val closedLoopStageARequests = 160
    val closedLoopMeasuredRequests = 128
    val closedLoopTotalRequests = closedLoopStageARequests + closedLoopMeasuredRequests

    val policyProbe = args.contains("--policy-probe")
    // Issue off, training kept. The regression compares against the recorded
    // no-prefetch baseline, so a prefetch must not reach a slice.
    val noPrefetchIssue = args.contains("--no-prefetch-issue")
    // A/B gate: disable queue construction on both sides of the 2c RTL change.
    // Expected cold demand metrics are unchanged; training remains observable.
    val noBop = args.contains("--no-bop")
    val dnLatency = args.collectFirst { case s if s.startsWith(dnLatencyArgPrefix) => s.stripPrefix(dnLatencyArgPrefix).toInt }.getOrElse(12)
    val dnOutstanding = args.collectFirst { case s if s.startsWith(dnOutstandingArgPrefix) => s.stripPrefix(dnOutstandingArgPrefix).toInt }.getOrElse(64)
    val dnCreditDelay = args.collectFirst { case s if s.startsWith(dnCreditDelayArgPrefix) => s.stripPrefix(dnCreditDelayArgPrefix).toInt }.getOrElse(0)
    val probeRequests = args.collectFirst { case s if s.startsWith(probeRequestsArgPrefix) => s.stripPrefix(probeRequestsArgPrefix).toInt }.getOrElse(32)
    val probeCycles = args.collectFirst { case s if s.startsWith(probeCyclesArgPrefix) => s.stripPrefix(probeCyclesArgPrefix).toInt }.getOrElse(4000)
    require(dnLatency >= 0 && dnOutstanding >= 1 && dnCreditDelay >= 0 && probeRequests >= 1 && probeCycles >= 1)

    var varArgs = ArrayBuffer(args.toIndexedSeq:_*)
    var varArgsDropped = 0

    var numSlices = if (simulateMode) 1 else 2

    val varArgsToDrop = args.sliding(2, 1).zipWithIndex.collect {
      case (Array("--slices", value), i) => (numSlices = value.toInt, i)
    }

    varArgsToDrop.map(_._2).foreach(i => {
      varArgs.remove(i - varArgsDropped, 2)
      varArgsDropped = varArgsDropped + 2
    })
    varArgs -= "--simulate"
    varArgs -= "--vcd"
    varArgs --= args.filter(_.startsWith(caseArgPrefix))
    varArgs --= args.filter(_.startsWith(mshrSizeArgPrefix))
    varArgs -= "--policy-probe"
    varArgs --= args.filter(_.startsWith(dnLatencyArgPrefix))
    varArgs --= args.filter(_.startsWith(dnOutstandingArgPrefix))
    varArgs --= args.filter(_.startsWith(dnCreditDelayArgPrefix))
    varArgs --= args.filter(_.startsWith(probeRequestsArgPrefix))
    varArgs --= args.filter(_.startsWith(probeCyclesArgPrefix))
    varArgs -= "--no-prefetch-issue"
    varArgs -= "--no-bop"
    varArgs.trimToSize()

    val validCases = L2TopCases.names.toSet
    require(validCases.contains(selectedCase),
      s"unknown --case=$selectedCase, expected one of ${validCases.toSeq.sorted.mkString(",")}")
    val baseCaseSpec = L2TopCases(selectedCase)
    require(selectedCase != "prefetched-clear" && selectedCase != "prefetched-clear-readunique",
      "prefetched-clear cases are not supported on bop-on-base")
    val caseSpec = if (policyProbe) {
      val baseStream = baseCaseSpec.up.stream
      val stream = baseStream match {
        case fixed: FixedReadStream => fixed.copy(requestCount = probeRequests, opcode = CCHIOpcode.ReadShared.opcode, dirtyDataResp = false)
        case other => throw new IllegalArgumentException(s"policy probe requires FixedReadStream, got ${other.getClass.getSimpleName}")
      }
      baseCaseSpec.copy(
        up = baseCaseSpec.up.copy(stream = stream),
        dn = baseCaseSpec.dn.copy(
          latency = FixedLatency(dnLatency),
          maxOutstanding = FixedOutstanding(dnOutstanding),
          creditPolicy = ImmediateCreditPolicy(txCreditReturnDelay = dnCreditDelay),
          holdTxReqCreditUntilResponse = true
        ),
        cycles = probeCycles,
        checks = Seq.empty
      )
    } else if (selectedCase == "dirty-victim" && numSlices > 1) {
      // Multi-slice routing, derived from the hash before this run. The test
      // config assigns slice i the node ID i (L2TopResponseModel.scala:145), so
      // the mapped index is the slice index. Both hashes are byte-identical to
      // tag prefetch-bop-freeze-20260929 (its L2Top.scala:70-72 map2 and :82-88
      // map4, versus the current L2Top.scala:91-93 and :103-108). Its
      // prefetchTarget (freeze L2Top.scala:177-187) calls the same mapN, so the
      // prefetch demux and the demand SAM cannot disagree.
      //
      // map2 (2 slices) is the parity of addr>>6. map4 (4 slices) xors the even
      // and the odd bits of the raw byte address; shifting by 6 does not change
      // the parity class of a bit, so a line-aligned address gives the same
      // result either way (the map4 entry in CLAUDE.md).
      //
      //   addr      line    map2   map4
      //   0x8000    0x200   1      2
      //   0x1c000   0x700   1      2
      //   0x4c000   0x1300  1      2
      //   0x58000   0x1600  1      2
      //   0x10c000  0x4300  1      2
      //
      // All five land on one slice: slice 1 for 2 slices, slice 2 for 4 slices.
      // The set index is addr[12:6] (offsetBits=6, setBits=7, 128 sets), and
      // every address has those seven bits clear, so all five share set 0 of
      // that one slice. The first four fill empty ways and hit
      // evict_victim_invalid; only the fifth replaces a dirty line. Expected:
      // created/completed/pending/active = 1/1/0/0, with the single fire on that
      // slice and 0 on the others. The replacement phase counts only the fifth
      // request, so dnTxReq/upTxDat/dnTxDat = 2/2/2, the downstream opcode is
      // WriteBackFull 0x1B, and both beats match the preload with nonzero BE.
      // Same derivation as the 1-slice comment above passLoadMiss, since the
      // set occupancy is identical.
      val addresses = Seq("8000", "1c000", "4c000", "58000", "10c000").map(BigInt(_, 16))
      val stream = ExplicitRequestStream(
        addresses.zipWithIndex.map { case (addr, index) =>
          UpRequestSpec(MasterId.t1p0, BigInt(index), 0, 0, CCHIOpcode.ReadUnique.opcode, addr)
        },
        Seq.fill(addresses.size)(BigInt(CHIFieldResp.CompData_UD_PD))
      )
      baseCaseSpec.copy(up = baseCaseSpec.up.copy(stream = stream))
    } else baseCaseSpec
    require(numSlices >= 1 && numSlices <= 4, s"Unsupported slice count $numSlices under eSAM")
    require(!bopOverwriteQueue || numSlices == 1, "bop-overwrite-queue requires --slices 1")
    require(!bopCollision || numSlices == 1, "bop-collision requires --slices 1")
    require(!bopPrefetchSmoke || (!noPrefetchIssue && !noBop && Set(1, 2, 4).contains(numSlices)),
      "prefetch smoke requires issue enabled and 1/2/4 slices")
    require(testMshrSize >= 2, s"--mshr-size=$testMshrSize is too small for this test")

    val l2cfg = new L2Configuration(
      nodeId = 8,
      eSAM = true,
      slices = 0 until numSlices,
      // Explicit opt-in for BOP cases; --no-bop keeps the A/B queue-free baseline.
      bopEnable = !noBop,
      prefetchIssueEnable = !noPrefetchIssue,
      bopParams = bopParams
    )

    val config = new Config((_, _, _) => {
      case L2ParamsKey => L2Params (
        physicalAddrWidth = 48,
        mshrSize = testMshrSize,
        ways = 4,
        sets = 128,
        blockBytes = 64,
        dataSRAMSplit = 8,
        enableDataECC = false,
        hasMbist = false,
        hasSramCtl = false,
        dataECC = Some("secded")
      )
      case CHIParametersKey => CHIParameters (
        issue = EnumCHIIssue.E,
        nodeIdWidth = 11,
        reqAddrWidth = 48,
        dataWidth = 256
      )
      case CCHIParametersKey => CCHIParameters()
      case LogUtilsOptionsKey => LogUtilsOptions(
        enableDebug = false,
        enablePerf = true,
        fpgaPlatform = false
      )
      case PerfCounterOptionsKey => PerfCounterOptions (
        enablePerfPrint = true,
        enablePerfDB = false,
        perfLevel = XSPerfLevel.VERBOSE,
        0
      )
    })

    val fullChiCompDataOpcode = 0x04
    val fullChiCompOpcode = 0x04
    val fullChiCompDBIDRespOpcode = 0x05
    val fullChiCopyBackWrDataOpcode = 0x02
    val fullChiEvictOpcode = 0x0d
    val fullChiWriteBackFullOpcode = 0x1b
    val fullChiWriteEvictFullOpcode = 0x15
    val fullChiWriteEvictOrEvictOpcode = 0x42

    def pct(bucket: BigInt, cycles: BigInt): Double =
      if (cycles == 0) 0.0 else bucket.toDouble * 100.0 / cycles.toDouble

    def printBuckets(name: String, buckets: L2TopObservationBuckets): Unit = {
      val cycles = buckets.cycles.peek().litValue
      val grant = buckets.grant.peek().litValue
      val blocked = buckets.blocked.peek().litValue
      val starved = buckets.starved.peek().litValue
      val busy = buckets.busy.peek().litValue
      println(f"$name%-12s cycles=$cycles%5d grant=$grant%5d (${pct(grant, cycles)}%6.2f%%) " +
        f"blocked=$blocked%5d (${pct(blocked, cycles)}%6.2f%%) " +
        f"starved=$starved%5d (${pct(starved, cycles)}%6.2f%%) " +
        f"busy=$busy%5d (${pct(busy, cycles)}%6.2f%%)")
    }

    def seconds(ns: Long): Double = ns.toDouble / 1000000000.0

    def runSimulation(): Unit = {
      implicit val sim: chisel3.simulator.HasSimulator = if (dumpVcd) {
        HasSimulator.simulators.verilator(
          verilatorSettings = svsim.verilator.Backend.CompilationSettings(
            traceStyle = Some(TraceStyle(TraceKind.Vcd)),
            outputSplit = Some(30000),
            outputSplitCFuncs = Some(30000),
            disabledWarnings = Seq("STMTDLY", "WIDTH")
          )
        )
      } else {
        HasSimulator.default
      }

      val requestCount = caseSpec.up.stream.requestCount
      val responseDelay = caseSpec.dn.latency.delay(0, L2TopLatencyOp.CompData, 0)
      val directoryInitWaitCycles = 300
      val watchdogCycles = caseSpec.cycles
      val fixedAddr = BigInt("8000", 16)
      val dirtyVictimKnownFailReport = "/nfs/home/zhuy/handover/be-zero-dirty-evict-defect-20260902.md"
      val dirtyVictimKnownFailMessage = s"blocked on upstream BE defect, see $dirtyVictimKnownFailReport"
      val requestStrideBits = 13
      val totalStartNs = System.nanoTime()
      var simulationStartNs = totalStartNs
      var simulationEndNs = totalStartNs

      println(s"ELAB_BEGIN case=$selectedCase mshrSize=$testMshrSize slices=$numSlices requestCount=$requestCount watchdogCycles=$watchdogCycles badScore=${bopParams.badScore}")
      // Warmup advances the hardware block counter by 300 cycles before cycle 0.
      // With a 4500-cycle block, dequeue resumes at simulation cycle 4200;
      // the first BOP request is at cycle 3557 with this stimulus. Expect
      // 27 blocked enqueues: two retained (0xaf00, 0xaf40), 25 oldest overwritten;
      // enqueue-ready always 1, trainStalls=0, bopReqStalls=0, and drain in order.
      val blockPrefetchCycles = if (bopOverwriteQueue) 4500 else 0
      simulate(new TestTop_L2Top(l2cfg)(config)) { dut =>
        val totalTshrs = numSlices * testMshrSize
        def tshrIndex(sliceIdx: Int, tshrId: Int): Int = {
          assert(sliceIdx >= 0 && sliceIdx < numSlices, s"sliceIdx out of range: $sliceIdx numSlices=$numSlices")
          assert(tshrId >= 0 && tshrId < 8, s"tshrId out of range: $tshrId")
          assert(tshrId < testMshrSize, s"tshrId exceeds configured MSHR count: $tshrId mshr=$testMshrSize")
          sliceIdx * testMshrSize + tshrId
        }
        def decodeDnTxnID(dnTxnID: DnTxnID): (Int, Int) = {
          val sliceBits = log2Ceil(numSlices)
          val sliceMask = if (sliceBits == 0) 0 else (1 << sliceBits) - 1
          val sliceIdx = (dnTxnID.v.toInt & sliceMask)
          val tshrId = (dnTxnID.v >> sliceBits).toInt
          assert(tshrId >= 0 && tshrId < 8, s"decoded Dn TxnID has invalid TSHR: txn=${dnTxnID.v} tshr=$tshrId")
          assert(sliceIdx >= 0 && sliceIdx < numSlices, s"decoded Dn TxnID has invalid slice: txn=${dnTxnID.v} slice=$sliceIdx numSlices=$numSlices")
          (sliceIdx, tshrId)
        }
        def probe(globalIndex: Int) = {
          val sliceIdx = globalIndex / testMshrSize
          val tshrId = globalIndex % testMshrSize
          dut.observation.dirProbeBySlice(sliceIdx)(tshrId)
        }
        def sliceTshrProbe(sliceIdx: Int, tshrId: Int) =
          dut.observation.sliceTshrProbe(sliceIdx)(tshrId)
        def tshrDetail(globalIndex: Int) =
          sliceTshrProbe(globalIndex / testMshrSize, globalIndex % testMshrSize)
        case class GrantSnapshot(dirRd: BigInt, dirWb: BigInt, dirReplRd: BigInt, dsWb: BigInt, dsRd: BigInt, dsAheadRd: BigInt)
        case class HostSnapshot(dnTxReq: Int, upTxDat: Int, completed: Int, dnTxDat: Int)
        case class AttributedGrant(cycle: Int, tshr: Int, kind: String)
        val attrGrants = ArrayBuffer.empty[AttributedGrant]
        case class TshrLife(
          var allocCycle: Option[Int] = None,
          var deallocCycle: Option[Int] = None,
          var validCycles: Int = 0,
          var allocPaddr: BigInt = BigInt(0),
          var servicePaddr: BigInt = BigInt(0),
          var lastMask: BigInt = BigInt(-1),
          var evictbackSeen: Boolean = false,
          var entered: Boolean = false,
          states: ArrayBuffer[String] = ArrayBuffer.empty
        )

        val upstream = new L2TopUpstreamModel(dut, caseSpec)
        val downstream = new L2TopDownstreamModel(dut, caseSpec)

        def b(value: Bool): Int = if (value.peek().litToBoolean) 1 else 0
        def hex(value: BigInt): String = "0x" + value.toString(16)
        def dnDataValue(p: PendingDnComp): BigInt = downstream.dnDataValue(p)
        def requestAddr(index: Int): BigInt = upstream.requestAddr(index)
        def delta(now: BigInt, thenValue: BigInt): BigInt = now - thenValue

        def grants(): GrantSnapshot = GrantSnapshot(
          dut.observation.grants.dirRd.peek().litValue,
          dut.observation.grants.dirWb.peek().litValue,
          dut.observation.grants.dirReplRd.peek().litValue,
          dut.observation.grants.dsWb.peek().litValue,
          dut.observation.grants.dsRd.peek().litValue,
          dut.observation.grants.dsAheadRd.peek().litValue
        )

        def preAssertTraceLine(cycle: Int): String = downstream.preAssertTraceLine(cycle)
        def recordPreAssertTrace(cycle: Int): Unit = downstream.recordPreAssertTrace(cycle)
        def initInputs(): Unit = {
          upstream.initInputs()
          downstream.initInputs()
          dut.log.dump.poke(false.B)
          dut.log.clean.poke(false.B)
        }
        def clearUpReq(): Unit = upstream.clearUpReq()
        def clearResponses(): Unit = {
          downstream.clearResponses()
          upstream.clearResponses()
        }
        def activateRxLink(): Unit = downstream.activateRxLink()
        def driveRead(index: Int): Unit = upstream.driveRead(index)
        def driveDnData(p: PendingDnComp): Unit = downstream.driveDnData(p)
        def driveDnRsp(p: PendingDnRsp): Unit = downstream.driveDnRsp(p)
        def driveUpCompAck(upDBID: UpDBID, sliceIdx: Int): Unit = upstream.driveUpCompAck(upDBID, sliceIdx)
        def driveUpSnpResp(snpTxnID: SnpTxnID, tgtId: BigInt): Unit = upstream.driveUpSnpResp(snpTxnID, tgtId)
        def driveUpSnpRespData(p: PendingSnpData): Unit = upstream.driveUpSnpRespData(p)
        def isEvictReq(opcode: BigInt): Boolean = downstream.isEvictReq(opcode)
        def downstreamEvictRspOpcode(opcode: BigInt): BigInt = downstream.downstreamEvictRspOpcode(opcode)
        def advanceTxLCredits(cycle: Int): Unit = downstream.advanceTxLCredits(cycle)
        def advanceRxLCredits(cycle: Int): Unit = downstream.advanceRxLCredits(cycle)
        def stepWithCredits(cycle: Int): Unit = downstream.stepWithCredits(cycle)
        def canDriveRxDat: Boolean = downstream.canDriveRxDat
        def canDriveRxRsp: Boolean = downstream.canDriveRxRsp
        val warmupPendingDnComp = scala.collection.mutable.Queue.empty[PendingDnComp]
        val warmupPendingDnRsp = scala.collection.mutable.Queue.empty[PendingDnRsp]
        var warmupDnTxReqCount = 0
        def serviceWarmupDownstream(cycle: Int): Unit = {
          val dnTxFire = dut.io.chi.txreq.flitv.peek().litToBoolean
          if (dnTxFire) {
            val dnTxnID = DnTxnID(dut.io.chi.txreq.flit.TxnID.get.peek().litValue)
            downstream.noteRequest(dnTxnID)
            val srcId = dut.io.chi.txreq.flit.SrcID.get.peek().litValue
            val opcode = dut.io.chi.txreq.flit.Opcode.get.peek().litValue
            if (isEvictReq(opcode)) {
              warmupPendingDnRsp.enqueue(PendingDnRsp(dnTxnID, srcId, cycle + responseDelay, downstreamEvictRspOpcode(opcode)))
            } else {
              warmupPendingDnComp.enqueue(PendingDnComp(dnTxnID, srcId, cycle + responseDelay, 0, BigInt(CHIFieldResp.CompData_SC)))
              warmupPendingDnComp.enqueue(PendingDnComp(dnTxnID, srcId, cycle + responseDelay + 1, 1, BigInt(CHIFieldResp.CompData_SC)))
            }
            warmupDnTxReqCount += 1
            println(s"WARMUP_DN_TXREQ cycle=$cycle dnTxn=${dnTxnID.v} opcode=$opcode enqueueDue=${cycle + responseDelay}")
          }
          warmupPendingDnComp.headOption.filter(p => p.due <= cycle && canDriveRxDat && downstream.canDeliverResponse).foreach { p =>
            driveDnData(p)
            warmupPendingDnComp.dequeue()
            downstream.noteResponse(p.dnTxnID, p.beat == 1, cycle)
            println(s"WARMUP_DRIVE_DN_COMPDATA cycle=$cycle dnTxn=${p.dnTxnID.v} beat=${p.beat}")
          }
          warmupPendingDnRsp.headOption.filter(p => p.due <= cycle && canDriveRxRsp && downstream.canDeliverResponse).foreach { p =>
            driveDnRsp(p)
            warmupPendingDnRsp.dequeue()
            downstream.noteResponse(p.dnTxnID, finalBeat = true, cycle)
            println(s"WARMUP_DRIVE_DN_RSP cycle=$cycle dnTxn=${p.dnTxnID.v}")
          }
        }
        def printResponseModelReport(cycle: Int, issued: Int, completed: Int, dnTxReqCount: Int, upTxDatCount: Int, dnTxDatCount: Int, baseGrant: GrantSnapshot): Unit = {
          val g = grants()
          println(s"RESPONSE_MODEL_DONE cycle=$cycle issued=$issued completed=$completed dnTxReq=$dnTxReqCount upTxDat=$upTxDatCount dnTxDat=$dnTxDatCount")
          printBuckets("directory", dut.observation.directory)
          printBuckets("dataStorage", dut.observation.dataStorage)
          printBuckets("upRXREQ", dut.observation.upRXREQ)
          printBuckets("upTXDAT", dut.observation.upTXDAT)
          println(s"RESPONSE_MODEL_GRANTS dirRd=${delta(g.dirRd, baseGrant.dirRd)} dirWb=${delta(g.dirWb, baseGrant.dirWb)} " +
            s"dirReplRd=${delta(g.dirReplRd, baseGrant.dirReplRd)} dsWb=${delta(g.dsWb, baseGrant.dsWb)} " +
            s"dsRd=${delta(g.dsRd, baseGrant.dsRd)} dsAheadRd=${delta(g.dsAheadRd, baseGrant.dsAheadRd)}")
        }

        def host(dnTxReqCount: Int, upTxDatCount: Int, completed: Int, dnTxDatCount: Int): HostSnapshot =
          HostSnapshot(dnTxReqCount, upTxDatCount, completed, dnTxDatCount)

        def printState(prefix: String, cycle: Int, issued: Int, completed: Int): Unit = {
          println(s"$prefix cycle=$cycle issued=$issued completed=$completed")
          printBuckets("directory", dut.observation.directory)
          printBuckets("dataStorage", dut.observation.dataStorage)
          printBuckets("upRXREQ", dut.observation.upRXREQ)
          printBuckets("upTXDAT", dut.observation.upTXDAT)
        }

        def printCaseReport(
          name: String,
          cycle: Int,
          issued: Int,
          hostBase: HostSnapshot,
          grantBase: GrantSnapshot,
          dnTxReqCount: Int,
          upTxDatCount: Int,
          dnTxDatCount: Int,
          completed: Int,
          ok: Boolean
        ): Unit = {
          val g = grants()
          val dnTxReqDelta = dnTxReqCount - hostBase.dnTxReq
          val upTxDatDelta = upTxDatCount - hostBase.upTxDat
          val completedDelta = completed - hostBase.completed
          val dnTxDatDelta = dnTxDatCount - hostBase.dnTxDat
          val dsReadDelta = delta(g.dsRd, grantBase.dsRd) + delta(g.dsAheadRd, grantBase.dsAheadRd)
          println(s"CASE $name cycle=$cycle issued=$issued completed=$completed completedDelta=$completedDelta pass=$ok")
          printBuckets("directory", dut.observation.directory)
          printBuckets("dataStorage", dut.observation.dataStorage)
          printBuckets("upRXREQ", dut.observation.upRXREQ)
          printBuckets("upTXDAT", dut.observation.upTXDAT)
          println(s"CASE $name actual dnTxReqDelta=$dnTxReqDelta upTxDatDelta=$upTxDatDelta dnTxDatDelta=$dnTxDatDelta " +
            s"dirRdGrantDelta=${delta(g.dirRd, grantBase.dirRd)} dirWbGrantDelta=${delta(g.dirWb, grantBase.dirWb)} " +
            s"dirReplRdGrantDelta=${delta(g.dirReplRd, grantBase.dirReplRd)} dsWbGrantDelta=${delta(g.dsWb, grantBase.dsWb)} " +
            s"dsRdGrantDelta=${delta(g.dsRd, grantBase.dsRd)} dsAheadRdGrantDelta=${delta(g.dsAheadRd, grantBase.dsAheadRd)} " +
            s"dsReadGrantDelta=$dsReadDelta")
        }

        // Base semantics, the frozen prefetch-bop-freeze-20260929 expectations do
        // not apply here. 22e5ef4 ("fix: bypassing EvictBack on ReplRd returning I
        // state in meta") added evict_victim_invalid at L2VPipeREQ.scala:1608 and
        // gates s_evict_up_evict on !evict_not_needed at :1618-1620. The freeze tag
        // only skips evict_self_alias (its L2VPipeREQ.scala:1320,1326), so an
        // Invalid victim still produced an EvictBack there. Both cases below are
        // derived from the current base, not measured and back-filled.
        //
        // load-miss: one cold ReadShared (L2TopCases.scala:163). The replacer's
        // victim reads Invalid, so L2VPipeREQ.scala:1608,1620 suppresses the
        // EvictBack: created/completed/pending/active = 0/0/0/0. The demand itself
        // is unchanged, so dnTxReqDelta=1, upTxDatDelta=2, completed=1 stay.
        //
        // dirty-victim: five ReadUnique with dirtyDataResp (L2TopCases.scala:176)
        // into a 4-way set (L2TopResponseModel.scala:155). The first four allocate
        // empty ways and hit evict_victim_invalid, producing no EvictBack; only
        // the fifth replaces a dirty, non-Invalid line, so exactly one EvictBack
        // is created and its target TSHR deallocates: 1/1/0/0. Deltas are taken
        // against the snapshot at issued == completed == 4 (below), so they count
        // only the fifth request: its own ReadUnique miss is one downstream read
        // and the EvictBack's WriteBackFull is the second, dnTxReqDelta=2; the
        // miss returns two CompData beats, upTxDatDelta=2; the writeback sends two
        // CopyBackWrData beats, dnTxDatDelta=2. That victim's dirResult.dirty
        // selects CHI_WriteBackFull = 0x1B (L2VPipeREQ.scala:603-609,
        // CHIRNFOpcodesREQ.scala:32), and the two beats (DataID 0 and 2) carry the
        // preload CompData of upTxnID < 4 (L2TopResponseModel.scala:1247-1248)
        // with nonzero BE because the victim state is not I (L2VPipeREQ.scala:1009).
        def passLoadMiss(baseHost: HostSnapshot, baseGrant: GrantSnapshot, dnTxReqCount: Int, upTxDatCount: Int, completed: Int): Boolean = {
          (dnTxReqCount - baseHost.dnTxReq) == 1 &&
          (upTxDatCount - baseHost.upTxDat) == 2 &&
          completed == 1 &&
          Seq("DirRd", "DirWb", "DSWb").forall(kind => attrGrants.exists(_.kind == kind))
        }

        val reqStateNames = Seq(
          "w_snpresp0", "w_snpresp2", "s_snpcompack", "s_dn_txreq", "w_dn_pcrdgrant", "w_ds_resp",
          "w_s_rd_dn_compack", "s_rd_dn_compack", "w_rd_dn_data0", "w_rd_dn_data2", "w_rd_dn_comp", "w_rd_up_compack",
          "w_s_rd_up_compdata0", "s_rd_up_compdata0", "w_s_rd_up_compdata2", "s_rd_up_compdata2", "w_s_rd_up_comp", "s_rd_up_comp",
          "w_unlock_dir", "w_unlock_ds", "w_s_repl", "s_repl", "w_s_evict_up_evict", "s_evict_up_evict", "w_evict_s_dn_txreq",
          "w_evict_dn_comp", "w_evict_dn_compdbid", "w_s_evict_dn_cbwrdata0", "w_s_evict_dn_cbwrdata2", "s_evict_dn_cbwrdata0",
          "s_evict_dn_cbwrdata2", "s_evict_dn_compack", "w_evict_peer_unlock_ds"
        )
        def reqStateText(mask: BigInt): String = {
          val names = reqStateNames.zipWithIndex.collect { case (name, bit) if ((mask >> bit) & 1) == 1 => name }
          if (names.isEmpty) "free" else names.mkString("|")
        }

        def printTshr0Probe(tag: String, cycle: Int, upTxReqFire: Int): Unit = {
          val t = dut.observation.tshr0
          val c = dut.observation.tshr0Counts
          val r = dut.observation.rawReqCounts
          println(s"$tag tshr0 cycle=$cycle " +
            s"w_unlock_ds=${b(t.wUnlockDs)} w_unlock_dir=${b(t.wUnlockDir)} lock_ds=${b(t.lockDs)} " +
            s"expect_replace=${b(t.expectReplace)} self_unlock_ds=${b(t.selfUnlockDs)} " +
            s"unlock_self_evictback=${b(t.unlockSelfEvictback)} w_s_repl=${b(t.wSRepl)} " +
            s"trigger_replace=${b(t.triggerReplace)} toDir_ReplRd=${b(t.toDirReplRd)} s_evict=${b(t.sEvict)} " +
            s"state_dirRead={PreArb=${b(t.stateDirReadPreArb)} ReplPreArb=${b(t.stateDirReadReplPreArb)} PostArb=${b(t.stateDirReadPostArb)}} " +
            s"dirResult={state=${t.dirResultState.peek().litValue} dirty=${b(t.dirResultDirty)} hit=${b(t.dirResultHit)} way=${t.dirResultWay.peek().litValue}} " +
            s"dirRdResp={valid=${b(t.dirRdResp)} state=${t.dirRdRespState.peek().litValue} dirty=${b(t.dirRdRespDirty)} hit=${b(t.dirRdRespHit)} way=${t.dirRdRespWay.peek().litValue}} " +
            s"replRdResp={valid=${b(t.replRdResp)} state=${t.replRdRespState.peek().litValue} dirty=${b(t.replRdRespDirty)} way=${t.replRdRespWay.peek().litValue} replWay=${t.replWay.peek().litValue} replPaddr=0x${t.replPaddr.peek().litValue.toString(16)}} " +
            s"fromDir={META_state=${t.fromDirMetaState.peek().litValue} META_dirty=${b(t.fromDirMetaDirty)} META_hit=${b(t.fromDirMetaHit)} META_way=${t.fromDirMetaWay.peek().litValue} REPL_way=${t.fromDirReplWay.peek().litValue} REPL_paddr=0x${t.fromDirReplPaddr.peek().litValue.toString(16)}} " +
            s"rawReqCounts={replRdVec=${r.replRdVec.peek().litValue} dirRdVec=${r.dirRdVec.peek().litValue} dirWbVec=${r.dirWbVec.peek().litValue} wbVec=${r.wbVec.peek().litValue} rdVec=${r.rdVec.peek().litValue}} " +
            s"tshr0Counts={w_unlock_ds=${c.wUnlockDs.peek().litValue} w_unlock_dir=${c.wUnlockDir.peek().litValue} lock_ds=${c.lockDs.peek().litValue} " +
            s"expect_replace=${c.expectReplace.peek().litValue} self_unlock_ds=${c.selfUnlockDs.peek().litValue} unlock_self_evictback=${c.unlockSelfEvictback.peek().litValue} " +
            s"w_s_repl=${c.wSRepl.peek().litValue} trigger_replace=${c.triggerReplace.peek().litValue} toDir_ReplRd=${c.toDirReplRd.peek().litValue} s_evict=${c.sEvict.peek().litValue}} " +
            s"upTxReqFire=$upTxReqFire")
        }

        def beStageLine(name: String, stage: L2TopBEStageProbe): String =
          s"$name={v=${b(stage.valid)} r=${b(stage.ready)} op=${stage.opcode.peek().litValue} dataID=${stage.dataId.peek().litValue} BE=0x${stage.be.peek().litValue.toString(16)}}"

        def beSourceActive(i: Int): Boolean = {
          val p = dut.observation.beProbe
          p.vpipeReqAll(i).valid.peek().litToBoolean || p.tshrAll(i).valid.peek().litToBoolean ||
            p.sEvictCbWrData0All(i).peek().litToBoolean || p.sEvictCbWrData2All(i).peek().litToBoolean
        }

        def beSourceLine(i: Int): String = {
          val p = dut.observation.beProbe
          s"src$i={reqDir={state=${p.reqDirStateAll(i).peek().litValue} dirty=${b(p.reqDirDirtyAll(i))} hit=${b(p.reqDirHitAll(i))}} " +
            s"exprItems={stateIsI=${b(p.reqStateIsIAll(i))} stateNotI=${b(p.reqStateNotIAll(i))}} " +
            s"reqBEExpr=0x${p.reqBEExprAll(i).peek().litValue.toString(16)} " +
            s"sEvictCbWrData0=${b(p.sEvictCbWrData0All(i))} sEvictCbWrData2=${b(p.sEvictCbWrData2All(i))} " +
            beStageLine("vpipeReq", p.vpipeReqAll(i)) + " " + beStageLine("tshr", p.tshrAll(i)) + "}"
        }

        def printBETrace(cycle: Int): Unit = {
          val p = dut.observation.beProbe
          val sourceText = (0 until testMshrSize).filter(beSourceActive).map(beSourceLine).mkString(" ")
          val sourceSuffix = if (sourceText.nonEmpty) s" sources=[$sourceText]" else ""
          println(s"BE_TRACE cycle=$cycle " +
            s"reqDir={state=${p.reqDirState.peek().litValue} dirty=${b(p.reqDirDirty)} hit=${b(p.reqDirHit)}} " +
            s"exprItems={stateIsI=${b(p.reqStateIsI)} stateNotI=${b(p.reqStateNotI)}} " +
            s"reqBEEnable=${b(p.reqBEEnable)} reqBEExpr=0x${p.reqBEExpr.peek().litValue.toString(16)} " +
            s"sEvictCbWrData0=${b(p.sEvictCbWrData0)} sEvictCbWrData2=${b(p.sEvictCbWrData2)} " +
            beStageLine("vpipeReq", p.vpipeReq) + " " + beStageLine("tshr", p.tshr) + " " +
            beStageLine("slice", p.slice) + " " + beStageLine("chiLink", p.chiLink) + " " + beStageLine("top", p.top) + sourceSuffix)
        }

        def beStageHasCopyBack(stage: L2TopBEStageProbe): Boolean =
          stage.valid.peek().litToBoolean && stage.opcode.peek().litValue == fullChiCopyBackWrDataOpcode

        def nonTopBETraceActive(): Boolean = {
          val p = dut.observation.beProbe
          val sourceHasCopyBack = (0 until testMshrSize).exists(i =>
            beStageHasCopyBack(p.vpipeReqAll(i)) || beStageHasCopyBack(p.tshrAll(i)))
          (sourceHasCopyBack || beStageHasCopyBack(p.slice) || beStageHasCopyBack(p.chiLink)) && !beStageHasCopyBack(p.top)
        }

        def dirTimingLine(label: String, cycle: Int, i: Int): Unit = {
          val d = probe(i)
          println(s"DIR_RACE_$label cycle=$cycle tshr=$i paddr=0x${d.paddr.peek().litValue.toString(16)} " +
            s"dirRdReq=${b(d.dirRdReq)} dirRdArbComp=${b(d.dirRdArbComp)} dirRdResp=${b(d.dirRdResp)} " +
            s"dirWbReq=${b(d.dirWbReq)} dirWbLand=${b(d.dirWbLand)} rdDone=${b(d.rdDone)} " +
            s"fromDirMETA={state=${d.fromDirMetaState.peek().litValue} dirty=${b(d.fromDirMetaDirty)} hit=${b(d.fromDirMetaHit)} way=${d.fromDirMetaWay.peek().litValue}} " +
            s"dirResultReg={state=${d.dirResultState.peek().litValue} dirty=${b(d.dirResultDirty)} hit=${b(d.dirResultHit)} way=${d.dirResultWay.peek().litValue}}")
        }

        def printDSEvictbackLine(cycle: Int, phase: String, i: Int): Unit = {
          val d = probe(i)
          val data = d.dsRdData.peek().litValue
          val data0 = data & ((BigInt(1) << 256) - 1)
          val data2 = data >> 256
          println(s"DS_EVICTBACK_$phase cycle=$cycle tshr=$i " +
            s"rdEvictback=${b(d.dsRdEvictback)} rdEn=${b(d.dsRdEn)} rdDone=${b(d.dsRdDone)} " +
            s"reqRd=${b(d.dsReqRd)} arbComp=${b(d.dsRdArbComp)} resp=${b(d.dsRdResp)} " +
            s"dataID0=${hex(data0)} dataID2=${hex(data2)}")
        }

        simulationStartNs = System.nanoTime()
        val head = scala.sys.process.Process(Seq("git", "rev-parse", "--short", "HEAD")).!!.trim
        val dirty = scala.sys.process.Process(Seq("git", "status", "--short")).!!.trim
        println(s"FINGERPRINT head=$head dirty=${if (dirty.isEmpty) "clean" else "dirty"}")
        println(f"SIM_BEGIN case=$selectedCase elaboration_sec=${seconds(simulationStartNs - totalStartNs)}%.3f")

        initInputs()
        dut.reset.poke(true.B)
        for (warmup <- 0 until 8) {
          stepWithCredits(warmup - 8 - directoryInitWaitCycles)
        }
        dut.reset.poke(false.B)
        activateRxLink()
        for (warmup <- 0 until directoryInitWaitCycles) {
          clearResponses()
          serviceWarmupDownstream(warmup)
          stepWithCredits(warmup - directoryInitWaitCycles)
        }
        println(s"WARMUP_DN_SUMMARY dnTxReq=$warmupDnTxReqCount pendingComp=${warmupPendingDnComp.size} pendingRsp=${warmupPendingDnRsp.size}")

        val pendingL1Reqs = scala.collection.mutable.Queue.empty[PendingL1Req]
        val dnTxnTable = scala.collection.mutable.Map.empty[DnTxnID, DnTxnInfo]
        val completedInternalDnTxn = scala.collection.mutable.Set.empty[DnTxnID]
        val pendingDnComp = scala.collection.mutable.Queue.empty[PendingDnComp]
        val pendingDnRsp = scala.collection.mutable.Queue.empty[PendingDnRsp]
        val collisionAddress = BigInt("8000", 16) + (BigInt(170) << 6)
        var collisionBaseline: Option[(Int, Int, Int, Int, Int)] = None
        var heldDnTxn: Option[DnTxnID] = None
        var heldDnReleased = false
        var releaseHeldAt: Option[Int] = None
        var targetPrefetchedLast: Option[Boolean] = None
        var directoryReadbackPrefetched: Option[Boolean] = None
        var collisionPrefetchIssueCycle: Option[Int] = None
        var collisionDnReqs = ArrayBuffer.empty[(BigInt, BigInt, Boolean)]
        val collisionBopReqAddresses = ArrayBuffer.empty[BigInt]
        val collisionPrefetchIssueAddresses = ArrayBuffer.empty[BigInt]
        var collisionBaseDrops: Option[(Int, Int)] = None
        var secondAcceptCycle: Option[Int] = None
        var readbackAcceptCycle: Option[Int] = None
        var readbackAllocator: Option[(Int, BigInt, BigInt, BigInt)] = None
        var firstCollisionDnCycle: Option[Int] = None
        val pendingUpAck = scala.collection.mutable.Map.empty[UpDBID, PendingUpAck]
        val pendingSnpRsp = scala.collection.mutable.Queue.empty[PendingSnpRsp]
        val pendingSnpData = scala.collection.mutable.Queue.empty[PendingSnpData]
        val preloadData = scala.collection.mutable.Map.empty[(Int, Int), BigInt]
        var issued = 0
        var completed = 0
        var nextClosedLoopCycle = 0
        var closedLoopTrainEvents = 0
        var closedLoopPrefetchIssues = 0
        var closedLoopTrainStalls = 0
        var closedLoopCrossPageDrops = 0
        var closedLoopDisabledDrops = 0
        var closedLoopFullDrops = 0
        var closedLoopBopReqStalls = 0
        var closedLoopBopReqFirstValidCycle: Option[Int] = None
        var closedLoopPrefetchedHitTrains = 0
        var closedLoopBopQueueOverwrites = 0
        var overwriteReadyViolations = 0
        var overwriteObservedOverwrites = 0
        val overwriteBlockedEnqueues = ArrayBuffer.empty[BigInt]
        val overwriteBlockedDequeues = ArrayBuffer.empty[BigInt]
        val overwriteAfterBlockDequeues = ArrayBuffer.empty[BigInt]
        val overwriteExpectedQueue = scala.collection.mutable.Queue.empty[BigInt]
        val overwriteDequeued = scala.collection.mutable.ArrayBuffer.empty[BigInt]
        var overwriteOrderMismatch: Option[String] = None
        var bopReqComparePreviousValid = false
        var bopReqCompareWindowRemaining = 0
        var bopReqCompareWindowsSeen = 0
        var bopReqProbePreviousValid = false
        var bopReqProbeWindowRemaining = 0
        var bopReqProbeWindowsSeen = 0
        var closedLoopMeasuredOffset: Option[BigInt] = None
        var closedLoopLastTrainAddr: Option[BigInt] = None
        val closedLoopStageATrainCycles = scala.collection.mutable.ArrayBuffer.empty[Int]
        val closedLoopTrainLines = scala.collection.mutable.Set.empty[BigInt]
        val closedLoopTrainCounts = scala.collection.mutable.Map.empty[BigInt, Int].withDefaultValue(0)
        val closedLoopDemandAcceptCycles = scala.collection.mutable.Map.empty[BigInt, Int]
        val closedLoopPrefetchIssueCycles = scala.collection.mutable.Map.empty[BigInt, Int]
        val closedLoopBopReqAddresses = scala.collection.mutable.Set.empty[BigInt]
        val closedLoopCrossPageAddresses = scala.collection.mutable.Set.empty[BigInt]
        val closedLoopDisabledAddresses = scala.collection.mutable.Set.empty[BigInt]
        val closedLoopPrefetchedHitAddresses = scala.collection.mutable.Set.empty[BigInt]
        val closedLoopQueueIssues = scala.collection.mutable.Map.empty[BigInt, Int].withDefaultValue(0)
        val closedLoopInternalTags = scala.collection.mutable.Set.empty[BigInt]
        val collisionTargetAllocations = ArrayBuffer.empty[String]
        val closedLoopInternalFills = scala.collection.mutable.Map.empty[BigInt, (BigInt, Boolean, Boolean)]
        val expectedBOP = L2TopBOPExpectations
        val expectedFillState = if (caseSpec.dn.readNotSharedDirtyResp == CHIFieldResp.CompData_UC) BigInt(3) else BigInt(1)
        def expectedFills(addresses: Set[BigInt]): Map[BigInt, (BigInt, Boolean, Boolean)] =
          addresses.map(_ -> ((expectedFillState, false, true))).toMap
        // Fixed offset +2; expected addresses and mapN results were derived in
        // L2TopCases before running. Track every lease from allocation through
        // committed directory fill and deallocation, rather than infer it from
        // a downstream transaction alone. Reusing the same TSHR later is legal.
        val smokeDemandAddresses = (0 until 8).map(k => BigInt("10000", 16) + k * 0x100)
        val smokeExpectedAddresses = smokeDemandAddresses.map(_ + 0x80)
        val smokeExpectedSlices = numSlices match {
          case 1 => Seq.fill(8)(0)
          case 2 => Seq(0, 1, 1, 0, 1, 0, 0, 1)
          case 4 => Seq(3, 2, 1, 0, 2, 3, 0, 1)
          case _ => Seq.empty
        }
        val smokeExpectedState = if (selectedCase == "bop-prefetch-smoke-uc") BigInt(3) else BigInt(1)
        case class SmokePrefetch(addr: BigInt, var tshr: Option[Int] = None,
          var allocations: Int = 0, var issues: Int = 0, var requestSeen: Boolean = false,
          var srcID: Option[BigInt] = None, var reqOpcode: Option[BigInt] = None,
          var dnOpcode: Option[BigInt] = None, var dnRequests: Int = 0,
          var fillState: Option[BigInt] = None, var landedState: Option[BigInt] = None,
          var fillDirty: Option[Boolean] = None, var deallocated: Boolean = false,
          var upResponse: Boolean = false, var trained: Boolean = false)
        val smokePrefetches = scala.collection.mutable.ArrayBuffer.empty[SmokePrefetch]
        var cycle = 0
        def smokePending: Int = pendingL1Reqs.size + pendingDnComp.size + pendingDnRsp.size +
          pendingUpAck.size + pendingSnpRsp.size + pendingSnpData.size +
          dnTxnTable.keys.count(txn => !completedInternalDnTxn.contains(txn))
        def smokeActive: Int = (0 until totalTshrs).count { i =>
          probe(i).valid.peek().litToBoolean || probe(i).reqActive.peek().litToBoolean
        }
        def smokeDone: Boolean = issued == requestCount && completed == requestCount &&
          smokePrefetches.size == 8 && smokePrefetches.forall(_.deallocated) &&
          smokePending == 0 && smokeActive == 0 && cycle >= nextClosedLoopCycle + 33
        var dnTxReqCount = 0
        var dnTxDatCount = 0
        var dnTxRspCount = 0
        var upTxReqCount = 0
        var upTxSnpCount = 0
        var upTxDatCount = 0
        var upTxReqRiseCount = 0
        var sEvictRiseCount = 0
        val sEvictRiseByTshr = Array.fill(totalTshrs)(0)
        val prevSEvictByTshr = Array.fill(totalTshrs)(false)
        var prevUpTxReqFire = false
        var nextHandle = 1L
        var rootHandle: Option[Long] = None
        var rootTshr: Option[Int] = None
        val tshrHandles = Array.fill[Option[Long]](totalTshrs)(None)
        val unassignedRootHandles = scala.collection.mutable.Queue.empty[Long]
        val handleParents = scala.collection.mutable.Map.empty[Long, Option[Long]]
        val handleSEvictRises = scala.collection.mutable.Map.empty[Long, Int].withDefaultValue(0)
        val handleSEvictCycle = scala.collection.mutable.Map.empty[Long, Int]
        val handleChildCount = scala.collection.mutable.Map.empty[Long, Int].withDefaultValue(0)
        val evictBackHandles = scala.collection.mutable.Set.empty[Long]
        val pendingEvictHandleBySource = scala.collection.mutable.Map.empty[Int, Long]
        val activeEvictHandleByTshr = scala.collection.mutable.Map.empty[Int, Long]
        val completedEvictBackHandles = scala.collection.mutable.Set.empty[Long]
        var firstCasePrinted = false
        var knownFailMatched = false
        var victimBaseSet = false
        var victimBaseHost = host(0, 0, 0, 0)
        var victimBaseGrant = grants()
        var victimDirRdState = BigInt(-1)
        var victimDirRdDirty = -1
        var victimDirRdHit = -1
        var victimReplState = BigInt(-1)
        var victimReplDirty = -1
        var victimReplWay = BigInt(-1)
        var victimReplPaddr = BigInt(-1)
        var victimBE = BigInt(-1)
        var victimBeat0BE = BigInt(-1)
        var victimBeat2BE = BigInt(-1)
        var victimBeat0Data = BigInt(-1)
        var victimBeat2Data = BigInt(-1)
        var victimDnTxDat = 0
        val victimDnOpcodes = ArrayBuffer.empty[BigInt]
        var victimDnTxDatBeat0 = false
        var victimDnTxDatBeat2 = false
        var tshr1CompDBIDRespPrinted = false
        var dsEvictbackRdPrinted = false
        var dsEvictbackGrantPrinted = false
        var dsEvictbackRespPrinted = false
        var victimATshr = 0
        var victimBTshr = 1
        var failed = false
        var finished = false
        var failReason = s"watchdog fired after $watchdogCycles cycles"
        val initialHost = host(0, 0, 0, 0)
        val initialGrant = grants()
        val tshrLives = Array.fill(totalTshrs)(TshrLife())
        var loadMissBTshr: Option[Int] = None
        var hostAllocBlockedCycles = 0
        var hostAllocFirstBlockedCycle: Option[Int] = None
        var evictbackLoopFirstValidCycle: Option[Int] = None
        var evictbackLoopFireCycle: Option[Int] = None
        var evictbackLoopBlockedCycles = 0
        var sawReqDirtyWrite = false
        var sawSnoopDataPassDirtyBeat0 = false
        var sawSnoopDataPassDirtyBeat2 = false

        def passSnoopDataPassDirty: Boolean =
          sawReqDirtyWrite && sawSnoopDataPassDirtyBeat0 && sawSnoopDataPassDirtyBeat2

        def victimDataSummary: String = {
          val expected0 = preloadData.get((0, 0))
          val expected2 = preloadData.get((0, 2))
          val match0 = expected0.contains(victimBeat0Data)
          val match2 = expected2.contains(victimBeat2Data)
          s" beat0Data=${hex(victimBeat0Data)} beat0Expected=${expected0.map(hex).getOrElse("<unknown>")} beat0DataMatch=${if (match0) 1 else 0}" +
            s" beat2Data=${hex(victimBeat2Data)} beat2Expected=${expected2.map(hex).getOrElse("<unknown>")} beat2DataMatch=${if (match2) 1 else 0}"
        }

        def trace(handle: Long, parentHandle: Option[Long], cycle: Int, channel: String, direction: String,
                  opcode: String, protocolId: String, phase: String): Unit = {
          println(s"TRACE handle=$handle parent=${parentHandle.map(_.toString).getOrElse("0")} cycle=$cycle " +
            s"ch=$channel dir=$direction op=$opcode proto=$protocolId phase=$phase")
        }

        def insertDnTxn(dnTxnID: DnTxnID, info: DnTxnInfo, cycle: Int): Unit = {
          assert(info.dnTxnID == dnTxnID,
            s"DnTxnInfo ID mismatch at insert: cycle=$cycle key=${dnTxnID.v} info=${info.dnTxnID.v}")
          if (dnTxnTable.get(dnTxnID).exists(_.origin == InternalOrigin) && completedInternalDnTxn.contains(dnTxnID)) {
            dnTxnTable -= dnTxnID
            completedInternalDnTxn -= dnTxnID
          }
          assert(!dnTxnTable.contains(dnTxnID),
            s"dnTxnTable slot is not empty at insert: cycle=$cycle dnTxn=${dnTxnID.v} old=${dnTxnTable.get(dnTxnID)} new=$info")
          dnTxnTable(dnTxnID) = info
        }

        def recordAttribution(cycle: Int, evictBackTargets: Set[Int]): Unit = {
          for (i <- 0 until totalTshrs) {
            val d = probe(i)
            val life = tshrLives(i)
            if (d.valid.peek().litToBoolean) {
              life.validCycles += 1
            }
            if (d.alloc.peek().litToBoolean || d.reuse.peek().litToBoolean) {
              if (!evictBackTargets.contains(i) && rootTshr.contains(i)) {
                unassignedRootHandles.headOption.foreach { handle =>
                  unassignedRootHandles.dequeue()
                  tshrHandles(i) = Some(handle)
                  rootHandle = Some(handle)
                  println(s"HANDLE_BIND cycle=$cycle handle=$handle parent=0 tshr=$i evictback=0 reqOpcode=${d.pRxReqOpcode.peek().litValue}")
                }
              }
            }
            if (d.alloc.peek().litToBoolean || d.reuse.peek().litToBoolean) {
              life.allocCycle = Some(cycle)
              life.deallocCycle = None
              life.allocPaddr = d.tshrPaddr.peek().litValue
              life.entered = true
              println(s"TSHR_LIFE cycle=$cycle tshr=$i event=${if (d.alloc.peek().litToBoolean) "alloc" else "reuse"} " +
                s"enter={REQ=${b(d.enterREQ)} EVT=${b(d.enterEVT)} SNP=${b(d.enterSNP)}} paddr=0x${life.allocPaddr.toString(16)}")
            }
            if (d.dealloc.peek().litToBoolean) {
              val sliceIdx = i / testMshrSize
              val tshrId = i % testMshrSize
              val dnTxnID = DnTxnID(BigInt(tshrId << log2Ceil(numSlices) | sliceIdx))
              dnTxnTable.get(dnTxnID) match {
                case Some(info) if info.origin == InternalOrigin && completedInternalDnTxn.contains(dnTxnID) =>
                  dnTxnTable -= dnTxnID
                  completedInternalDnTxn -= dnTxnID
                case Some(info) if info.origin == InternalOrigin =>
                  throw new AssertionError(s"internal Dn transaction deallocated before response: cycle=$cycle dnTxn=${dnTxnID.v} info=$info")
                case Some(info) if info.origin.isInstanceOf[L1Origin] => ()
                case Some(info) =>
                  throw new AssertionError(s"unexpected Dn transaction origin at dealloc: cycle=$cycle dnTxn=${dnTxnID.v} info=$info")
                case None => ()
              }
              if (life.entered) {
                life.deallocCycle = Some(cycle)
                life.entered = false
                println(s"TSHR_LIFE cycle=$cycle tshr=$i event=dealloc paddr=0x${d.tshrPaddr.peek().litValue.toString(16)}")
              }
              if (!d.alloc.peek().litToBoolean && !d.reuse.peek().litToBoolean) {
                tshrHandles(i) = None
              }
            }
            if (d.pRxReqEvictback.peek().litToBoolean) {
              life.evictbackSeen = true
            }
            if (selectedCase == "snoop-data-passdirty" && d.metaWrDirtySASet.peek().litToBoolean) {
              sawReqDirtyWrite = true
              println(s"SNOOP_PD_REQ_META_WRITE cycle=$cycle tshr=$i paddr=0x${d.tshrPaddr.peek().litValue.toString(16)}")
            }

            val mask = d.reqStateMask.peek().litValue
            if (mask != life.lastMask) {
              val text = reqStateText(mask)
              if (mask != 0 || life.lastMask != BigInt(-1)) {
                life.states += text
                if (selectedCase == "load-miss" && (i == 0 || tshrHandles(i).nonEmpty)) {
                  println(s"STATE_TRACE case=load-miss cycle=$cycle tshr=$i states=$text mask=0x${mask.toString(16)}")
                }
              }
              life.lastMask = mask
            }

            def addGrant(kind: String): Unit = {
              attrGrants += AttributedGrant(cycle, i, kind)
              if (kind == "DirRd") {
                life.servicePaddr = d.paddr.peek().litValue
              }
              println(s"GRANT_TRACE case=$selectedCase cycle=$cycle tshr=$i kind=$kind paddr=0x${d.paddr.peek().litValue.toString(16)} " +
                s"reqState=${reqStateText(mask)} pRxReqOpcode=${d.pRxReqOpcode.peek().litValue} evictback=${b(d.pRxReqEvictback)}")
            }
            if (d.dirRdArbComp.peek().litToBoolean) addGrant("DirRd")
            if (d.replRdArbComp.peek().litToBoolean) addGrant("ReplRd")
            if (d.dirWbLand.peek().litToBoolean) addGrant("DirWb")
            if (d.dsWbArbComp.peek().litToBoolean) addGrant("DSWb")
            if (d.dsRdArbComp.peek().litToBoolean) addGrant("DSRd")
            if (d.dsAheadRdArbComp.peek().litToBoolean) addGrant("DSAheadRd")
            if (d.dirRdResp.peek().litToBoolean) {
              life.states += s"DirRdResp(fromDirState=${d.fromDirMetaState.peek().litValue},dirty=${b(d.fromDirMetaDirty)},hit=${b(d.fromDirMetaHit)})"
              println(s"DIR_RESP_TRACE case=$selectedCase cycle=$cycle tshr=$i kind=DirRdResp " +
                s"fromDirState=${d.fromDirMetaState.peek().litValue} fromDirDirty=${b(d.fromDirMetaDirty)} fromDirHit=${b(d.fromDirMetaHit)} fromDirWay=${d.fromDirMetaWay.peek().litValue} " +
                s"dirResultRegState=${d.dirResultState.peek().litValue} dirResultRegDirty=${b(d.dirResultDirty)} dirResultRegHit=${b(d.dirResultHit)} dirResultRegWay=${d.dirResultWay.peek().litValue} evictback=${b(d.pRxReqEvictback)}")
            }
            if (d.replRdResp.peek().litToBoolean) {
              life.states += s"ReplRdResp(fromDirState=${d.fromDirMetaState.peek().litValue},dirty=${b(d.fromDirMetaDirty)},hit=${b(d.fromDirMetaHit)})"
              println(s"DIR_RESP_TRACE case=$selectedCase cycle=$cycle tshr=$i kind=ReplRdResp " +
                s"fromDirState=${d.fromDirMetaState.peek().litValue} fromDirDirty=${b(d.fromDirMetaDirty)} fromDirHit=${b(d.fromDirMetaHit)} fromDirWay=${d.fromDirMetaWay.peek().litValue} " +
                s"dirResultRegState=${d.dirResultState.peek().litValue} dirResultRegDirty=${b(d.dirResultDirty)} dirResultRegHit=${b(d.dirResultHit)} dirResultRegWay=${d.dirResultWay.peek().litValue} evictback=${b(d.pRxReqEvictback)}")
            }
            if (d.upTxRspFire.peek().litToBoolean) {
              life.states += s"UpTXRSP(op=${d.upTxRspOpcode.peek().litValue})"
              println(s"RSP_TRACE case=$selectedCase cycle=$cycle tshr=$i kind=UpTXRSP opcode=${d.upTxRspOpcode.peek().litValue} evictback=${b(d.pRxReqEvictback)}")
            }
            if (d.dnTxRspFire.peek().litToBoolean) {
              life.states += s"DnTXRSP(op=${d.dnTxRspOpcode.peek().litValue})"
              println(s"RSP_TRACE case=$selectedCase cycle=$cycle tshr=$i kind=DnTXRSP opcode=${d.dnTxRspOpcode.peek().litValue} evictback=${b(d.pRxReqEvictback)}")
            }
          }
        }

        def grantDeltaInLife(grants: Iterable[AttributedGrant], tshr: Int, kind: String, life: TshrLife): Int =
          grants.count { g =>
            g.tshr == tshr && g.kind == kind &&
              life.allocCycle.exists(a => g.cycle >= a) &&
              life.deallocCycle.forall(d => g.cycle <= d)
          }

        def printTshrAttribution(label: String, tshr: Int): Unit = {
          val life = tshrLives(tshr)
          val allocText = life.allocCycle.map(_.toString).getOrElse("<none>")
          val deallocText = life.deallocCycle.map(_.toString).getOrElse("<none>")
          val occupancyText = (for (a <- life.allocCycle; d <- life.deallocCycle) yield (d - a).toString).getOrElse("<open>")
          val stateText = life.states.foldLeft(ArrayBuffer.empty[String]) { (acc, s) =>
            if (acc.lastOption.contains(s)) acc else acc += s
          }.mkString(" -> ")
          val dirRd = grantDeltaInLife(attrGrants, tshr, "DirRd", life)
          val replRd = grantDeltaInLife(attrGrants, tshr, "ReplRd", life)
          val dirWb = grantDeltaInLife(attrGrants, tshr, "DirWb", life)
          val dsWb = grantDeltaInLife(attrGrants, tshr, "DSWb", life)
          val dsRd = grantDeltaInLife(attrGrants, tshr, "DSRd", life)
          val dsAheadRd = grantDeltaInLife(attrGrants, tshr, "DSAheadRd", life)
          val dsTotal = dsWb + dsRd + dsAheadRd
          println(s"ATTR_SUMMARY role=$label tshr=$tshr allocCycle=$allocText deallocCycle=$deallocText " +
            s"occupancyCycles=$occupancyText validSampledCycles=${life.validCycles} allocPaddr=0x${life.allocPaddr.toString(16)} servicePaddr=0x${life.servicePaddr.toString(16)} evictbackSeen=${if (life.evictbackSeen) 1 else 0}")
          println(s"ATTR_GRANTS role=$label tshr=$tshr " +
            s"DirRd=$dirRd ReplRd=$replRd DirWb=$dirWb DSWb=$dsWb DSRd=$dsRd DSAheadRd=$dsAheadRd DSTotal=$dsTotal")
          println(s"ATTR_STATES role=$label tshr=$tshr path=${if (stateText.nonEmpty) stateText else "<none>"}")
        }

        def printLoadMissAttribution(cycle: Int): Unit = {
          println(s"ATTR_CASE case=load-miss cycle=$cycle base=8157ab7 branch=align-8157ab7 slices=$numSlices mshrSize=$testMshrSize nWays=4 note=single-case-internal-tshr-attribution")
          println(s"ATTR_ALLOC_BLOCK case=load-miss role=A_refill kind=initial-UpRXREQ blockedCycles=$hostAllocBlockedCycles " +
            s"firstBlockedCycle=${hostAllocFirstBlockedCycle.map(_.toString).getOrElse("<none>")} issued=$issued")
          println(s"ATTR_EVICTBACK_LOOP case=load-miss role=B_empty_evict firstValidCycle=${evictbackLoopFirstValidCycle.map(_.toString).getOrElse("<none>")} " +
            s"fireCycle=${evictbackLoopFireCycle.map(_.toString).getOrElse("<none>")} " +
            s"allocBlockedCycles=$evictbackLoopBlockedCycles bAllocated=${if (loadMissBTshr.nonEmpty) 1 else 0}")
          rootTshr match {
            case Some(aTshr) => printTshrAttribution("A_refill", aTshr)
            case None => println("ATTR_SUMMARY role=A_refill tshr=<none> allocCycle=<none> deallocCycle=<none>")
          }
          loadMissBTshr match {
            case Some(bTshr) => printTshrAttribution("B_empty_evict", bTshr)
            case None =>
              println(s"ATTR_SUMMARY role=B_empty_evict tshr=<none> allocCycle=<none> deallocCycle=<none> occupancyCycles=<none> validSampledCycles=0 allocPaddr=0x0 servicePaddr=0x0 evictbackSeen=0")
              println(s"ATTR_GRANTS role=B_empty_evict tshr=<none> DirRd=0 ReplRd=0 DirWb=0 DSWb=0 DSRd=0 DSAheadRd=0 DSTotal=0")
              println(s"ATTR_STATES role=B_empty_evict tshr=<none> path=<none>")
          }
        }

        while (!failed && !finished && cycle < watchdogCycles) {
          clearResponses()
          var droveUpRsp = false

          if (!bopCollision && cycle > 0 && cycle % 100 == 0) {
            println(s"SIM_PROGRESS case=$selectedCase cycle=$cycle issued=$issued completed=$completed " +
              s"dnTxReq=$dnTxReqCount upTxDat=$upTxDatCount upTxReqFire=$upTxReqCount")
            printBuckets("directory", dut.observation.directory)
            printBuckets("dataStorage", dut.observation.dataStorage)
            printBuckets("upRXREQ", dut.observation.upRXREQ)
            printBuckets("upTXDAT", dut.observation.upTXDAT)
            printTshr0Probe("SIM_PROGRESS", cycle, upTxReqCount)
          }

          if (selectedCase == "dirty-victim" && !victimBaseSet && issued == 4 && completed == 4) {
            victimBaseHost = host(dnTxReqCount, upTxDatCount, completed, dnTxDatCount)
            victimBaseGrant = grants()
            victimBaseSet = true
            println(s"CASE dirty-victim preload-complete cycle=$cycle issued=$issued completed=$completed")
            printTshr0Probe("DIRTY_PRELOAD_DONE", cycle, upTxReqCount)
          }

          if (selectedCase == "dirty-victim" && victimBaseSet && victimDirRdState < 0 && tshrDetail(victimATshr).dirRdResp.peek().litToBoolean) {
            val aDir = tshrDetail(victimATshr)
            victimDirRdState = aDir.dirRdRespState.peek().litValue
            victimDirRdDirty = b(aDir.dirRdRespDirty)
            victimDirRdHit = b(aDir.dirRdRespHit)
            println(s"VICTIM_DIRRD cycle=$cycle state=$victimDirRdState dirty=$victimDirRdDirty hit=$victimDirRdHit way=${aDir.dirRdRespWay.peek().litValue}")
          }
          if (selectedCase == "dirty-victim" && victimBaseSet && victimReplState < 0 && tshrDetail(victimATshr).replRdResp.peek().litToBoolean) {
            val aDir = tshrDetail(victimATshr)
            victimReplState = aDir.replRdRespState.peek().litValue
            victimReplDirty = b(aDir.replRdRespDirty)
            victimReplWay = aDir.replRdRespWay.peek().litValue
            victimReplPaddr = aDir.replPaddr.peek().litValue
            println(s"VICTIM_REPLRD cycle=$cycle state=$victimReplState dirty=$victimReplDirty way=$victimReplWay replPaddr=0x${victimReplPaddr.toString(16)} " +
              s"fromDirMETA={state=${aDir.fromDirMetaState.peek().litValue} dirty=${b(aDir.fromDirMetaDirty)} hit=${b(aDir.fromDirMetaHit)} way=${aDir.fromDirMetaWay.peek().litValue}} " +
              s"fromDirREPL={way=${aDir.fromDirReplWay.peek().litValue} paddr=0x${aDir.fromDirReplPaddr.peek().litValue.toString(16)}}")
          }
          if (selectedCase == "dirty-victim" && victimBaseSet) {
            val aDir = probe(victimATshr)
            val bDir = probe(victimBTshr)
            if (bDir.dirRdReq.peek().litToBoolean && victimDirRdState < 0) {
              dirTimingLine("B_DIRRD_REQ", cycle, victimBTshr)
            }
            if (!tshr1CompDBIDRespPrinted && bDir.dnRxrspCompdbidresp.peek().litToBoolean) {
              tshr1CompDBIDRespPrinted = true
              dirTimingLine("B_COMPDBIDRESP", cycle, victimBTshr)
            }
            if (!dsEvictbackRdPrinted && bDir.dsRdEvictback.peek().litToBoolean) {
              dsEvictbackRdPrinted = true
              printDSEvictbackLine(cycle, "RD_EN", victimBTshr)
            }
            if (!dsEvictbackGrantPrinted && bDir.dsRdArbComp.peek().litToBoolean) {
              dsEvictbackGrantPrinted = true
              printDSEvictbackLine(cycle, "RD_GRANT", victimBTshr)
            }
            if (!dsEvictbackRespPrinted && bDir.dsRdResp.peek().litToBoolean) {
              dsEvictbackRespPrinted = true
              printDSEvictbackLine(cycle, "RD_RESP", victimBTshr)
            }
            if (aDir.dirWbLand.peek().litToBoolean) {
              dirTimingLine("A_DIRWB_LAND", cycle, victimATshr)
            }
          }

          // No EvictBack dependency: a cold miss's victim is Invalid, so base never
          // creates one (see the expectation comment above passLoadMiss).
          val loadMissDone = !policyProbe && passLoadMiss(initialHost, initialGrant, dnTxReqCount, upTxDatCount, completed)
          if (selectedCase == "load-miss" && loadMissDone) {
            printCaseReport("load-miss", cycle, issued, initialHost, initialGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = true)
            firstCasePrinted = true
            failed = false
          }

          val done = selectedCase match {
            case "bop-prefetch-smoke" | "bop-prefetch-smoke-uc" => smokeDone
            case "bop-closed-loop" | "bop-closed-loop-uc" => completed >= closedLoopTotalRequests && cycle >= nextClosedLoopCycle + 500
            case name if name.startsWith("bop-collision-") =>
              issued == requestCount && completed == requestCount && heldDnReleased &&
                cycle >= secondAcceptCycle.getOrElse(cycle) + 500
            case "bop-overwrite-queue" => cycle >= 5000 && overwriteExpectedQueue.isEmpty
            case "prefetched-clear" | "prefetched-clear-readunique" => false
            case "load-miss" if policyProbe => false
            case "load-miss" => firstCasePrinted
            case "dirty-victim" => victimBaseSet && completed >= 5 && victimDnTxDatBeat0 && victimDnTxDatBeat2 && victimBeat0BE != 0 && victimBeat2BE != 0
            case "snoop-data-passdirty" => passSnoopDataPassDirty
          }

          if (done) {
            clearUpReq()
          } else if (bopCollision && issued == closedLoopStageARequests + 2 && completed == issued &&
              secondAcceptCycle.exists(cycle >= _ + 350)) {
            driveRead(issued)
          } else if (bopCollision && issued >= closedLoopStageARequests && issued < closedLoopStageARequests + 2 && collisionBaseline.nonEmpty &&
              (issued == closedLoopStageARequests || (if (selectedCase.endsWith("c3")) firstCollisionDnCycle.nonEmpty else heldDnTxn.nonEmpty))) {
            driveRead(issued)
          } else if (bopPrefetchSmoke && issued < requestCount && issued == completed &&
              smokePending == 0 && smokeActive == 0 && cycle >= nextClosedLoopCycle) {
            driveRead(issued)
            nextClosedLoopCycle = cycle + 33
          } else if ((bopClosedLoop || bopOverwriteQueue || (bopCollision && issued < closedLoopStageARequests)) &&
              issued < requestCount && issued == completed && cycle >= nextClosedLoopCycle) {
            driveRead(issued)
            nextClosedLoopCycle = cycle + (if (bopOverwriteQueue) 5 else 33)
          } else if (selectedCase == "dirty-victim" && issued < requestCount && issued == completed) {
            driveRead(issued)
          } else if (selectedCase == "snoop-data-passdirty" && issued < requestCount && issued == completed && (issued == 0 || cycle >= 80)) {
            driveRead(issued)
          } else if (selectedCase == "load-miss" && issued < requestCount) {
            driveRead(issued)
          } else {
            clearUpReq()
          }

          val upReqFire = issued < requestCount &&
            dut.io.t1p(0).UpREQ.valid.peek().litToBoolean &&
            dut.io.t1p(0).UpREQ.ready.peek().litToBoolean
          if (upReqFire) {
            val handle = nextHandle
            nextHandle += 1
            val upTxnID = UpTxnID(dut.io.t1p(0).UpREQ.bits.TxnID.peek().litValue)
            val upOpcode = dut.io.t1p(0).UpREQ.bits.Opcode.peek().litValue
            val expectsCompAck = dut.io.t1p(0).UpREQ.bits.ExpCompData.peek().litToBoolean
            rootHandle = rootHandle.orElse(Some(handle))
            handleParents(handle) = None
            val upAddr = dut.io.t1p(0).UpREQ.bits.Addr.peek().litValue
            pendingL1Reqs.enqueue(PendingL1Req(handle, upTxnID, upOpcode, expectsCompAck, upAddr))
            if (bopClosedLoop || bopCollision) closedLoopDemandAcceptCycles(upAddr) = cycle
            unassignedRootHandles.enqueue(handle)
            println(s"HANDLE_ALLOC cycle=$cycle handle=$handle parent=0 ch=UpRXREQ proto=TxnID:${upTxnID.v}")
            trace(handle, None, cycle, "UpRXREQ", "In", upOpcode.toString, s"TxnID:${upTxnID.v}", "accept")
            issued += 1
            if (bopCollision && issued == closedLoopStageARequests + 2) secondAcceptCycle = Some(cycle)
            if (bopCollision && issued == requestCount) readbackAcceptCycle = Some(cycle)
          }

          if (bopClosedLoop || bopOverwriteQueue || bopCollision || bopPrefetchSmoke) {
            if (dut.observation.bopTrainFire.peek().litToBoolean) {
              closedLoopLastTrainAddr = Some(dut.observation.bopTrainAddr.peek().litValue)
              val line = dut.observation.bopTrainAddr.peek().litValue >> 6
              closedLoopTrainEvents += 1
              if (closedLoopTrainEvents <= expectedBOP.phaseTrains) closedLoopStageATrainCycles += cycle
              closedLoopTrainLines += line
              closedLoopTrainCounts(line) = closedLoopTrainCounts(line) + 1
            }
            if (dut.observation.bopTrainStall.peek().litToBoolean) closedLoopTrainStalls += 1
            if (dut.observation.bopCrossPageDrop.peek().litToBoolean) {
              closedLoopCrossPageDrops += 1
              closedLoopCrossPageAddresses += dut.observation.bopDropAddr.peek().litValue
            }
            if (dut.observation.bopDisabledDrop.peek().litToBoolean) {
              closedLoopDisabledDrops += 1
              closedLoopDisabledAddresses += dut.observation.bopDropAddr.peek().litValue
            }
            if (dut.observation.bopDelayQueueFullDrop.peek().litToBoolean) closedLoopFullDrops += 1
            val bopReqValid = dut.observation.bopReqValid.peek().litToBoolean
            val bopReqReady = dut.observation.bopReqReady.peek().litToBoolean
            val bopReqEnqReady = dut.observation.bopReqEnqReady.peek().litToBoolean
            val bopTrainValid = dut.observation.bopTrainValid.peek().litToBoolean
            val bopTrainReady = dut.observation.bopTrainReady.peek().litToBoolean
            val bopTrainStall = dut.observation.bopTrainStall.peek().litToBoolean
            val bopReqFire = bopReqValid && bopReqReady
            if (bopReqValid && !bopReqProbePreviousValid && bopReqProbeWindowsSeen < 3) {
              bopReqProbeWindowsSeen += 1
              bopReqProbeWindowRemaining = 3
            }
            if (bopReqProbeWindowRemaining > 0) {
              println(s"BOP_SAME_CYCLE cycle=$cycle reqValid=${if (bopReqValid) 1 else 0} " +
                s"reqReady=${if (bopReqReady) 1 else 0} reqEnqReady=${if (bopReqEnqReady) 1 else 0} " +
                s"reqFire=${if (bopReqFire) 1 else 0} trainValid=${if (bopTrainValid) 1 else 0} " +
                s"trainReady=${if (bopTrainReady) 1 else 0} trainStall=${if (bopTrainStall) 1 else 0}")
              bopReqProbeWindowRemaining -= 1
            }
            bopReqProbePreviousValid = bopReqValid
            if (bopReqValid) {
              val firstValidCycle = closedLoopBopReqFirstValidCycle.getOrElse(cycle)
              closedLoopBopReqFirstValidCycle = Some(firstValidCycle)
              if (bopReqFire) {
                closedLoopBopReqStalls += cycle - firstValidCycle
                closedLoopBopReqFirstValidCycle = None
              }
            } else {
              closedLoopBopReqFirstValidCycle = None
            }
            if (bopReqFire) {
              closedLoopBopReqAddresses += dut.observation.bopReqAddr.peek().litValue
              if (bopCollision && collisionBaseline.nonEmpty) {
                collisionBopReqAddresses += dut.observation.bopReqAddr.peek().litValue
              }
              if (bopPrefetchSmoke) {
                val addr = dut.observation.bopReqAddr.peek().litValue
                val k = smokePrefetches.size
                assert(k < 8 && addr == smokeExpectedAddresses(k),
                  s"prefetch $k must use offset +2: actual=${hex(addr)} expected=${smokeExpectedAddresses.lift(k).map(hex)}")
                smokePrefetches += SmokePrefetch(addr)
              }
            }
            val prefetchedHitTrainCount = dut.observation.prefetchedHitTrainCount.peek().litValue.toInt
            if ((bopClosedLoop || bopCollision) && prefetchedHitTrainCount > 0) {
              closedLoopPrefetchedHitTrains += prefetchedHitTrainCount
              (0 until numSlices).foreach { sliceIdx =>
                if (dut.observation.prefetchedHitTrainValidBySlice(sliceIdx).peek().litToBoolean) {
                  closedLoopPrefetchedHitAddresses += dut.observation.prefetchedHitTrainAddrBySlice(sliceIdx).peek().litValue
                }
              }
            }
            if ((bopClosedLoop || bopOverwriteQueue) && dut.observation.bopQueueOverwrite.peek().litToBoolean) {
              closedLoopBopQueueOverwrites += 1
            }
            if (bopOverwriteQueue) {
              if (!bopReqEnqReady || (bopReqValid && !bopReqReady)) overwriteReadyViolations += 1
              val deq = dut.observation.bopPrefetchIssue.peek().litToBoolean
              if (deq) {
                val actual = dut.observation.bopPrefetchIssueAddr.peek().litValue
                overwriteDequeued += actual
                if (cycle < blockPrefetchCycles - directoryInitWaitCycles) overwriteBlockedDequeues += actual
                else overwriteAfterBlockDequeues += actual
                if (overwriteExpectedQueue.isEmpty) overwriteOrderMismatch = Some(s"unexpected dequeue addr=0x${actual.toString(16)} cycle=$cycle")
                else {
                  val expected = overwriteExpectedQueue.dequeue()
                  if (expected != actual) overwriteOrderMismatch = Some(s"dequeue order expected=0x${expected.toString(16)} actual=0x${actual.toString(16)} cycle=$cycle")
                }
              }
              if (bopReqFire) {
                val addr = dut.observation.bopReqAddr.peek().litValue
                if (cycle < blockPrefetchCycles - directoryInitWaitCycles) overwriteBlockedEnqueues += addr
                if (overwriteExpectedQueue.size >= 2 && !deq) {
                  overwriteExpectedQueue.dequeue()
                  overwriteObservedOverwrites += 1
                }
                overwriteExpectedQueue.enqueue(addr)
              }
            }
            if (bopReqFire && closedLoopMeasuredOffset.isEmpty &&
                closedLoopTrainEvents >= closedLoopStageARequests && closedLoopLastTrainAddr.nonEmpty) {
              closedLoopMeasuredOffset = Some(
                dut.observation.bopReqAddr.peek().litValue - closedLoopLastTrainAddr.get
              )
            }
            if (bopCollision && collisionBaseline.nonEmpty && dut.observation.bopPrefetchIssue.peek().litToBoolean) {
              val issueAddress = dut.observation.bopPrefetchIssueAddr.peek().litValue
              collisionPrefetchIssueAddresses += issueAddress
              if (issueAddress == collisionAddress) {
                collisionPrefetchIssueCycle = Some(cycle)
                if (selectedCase.endsWith("c3")) releaseHeldAt = Some(cycle + 20)
              }
            }
          }

          if (bopClosedLoop || bopCollision) {
            (0 until numSlices).foreach { slice =>
              if (dut.observation.bopPrefetchIssueBySlice(slice).peek().litToBoolean) {
                val addr = dut.observation.bopPrefetchIssueAddrBySlice(slice).peek().litValue
                assert(slice == expectedBOP.slice(addr, numSlices), "production queue issue slice differs from mapN")
                closedLoopQueueIssues(addr) += 1
              }
            }
            (0 until totalTshrs).foreach { i =>
              val d = probe(i)
              val addr = d.tshrPaddr.peek().litValue
              if (bopCollision && d.reqAlloc.peek().litToBoolean &&
                  d.reqAllocPaddr.peek().litValue == collisionAddress) collisionTargetAllocations += "alloc"
              if (bopCollision && d.reqReuse.peek().litToBoolean &&
                  addr == collisionAddress) collisionTargetAllocations += "reuse"
              if (d.tagWrite.peek().litToBoolean && d.pRxReqIsPrefetch.peek().litToBoolean)
                closedLoopInternalTags += addr
              if (d.dirWbLand.peek().litToBoolean && closedLoopInternalTags.contains(addr) &&
                  !closedLoopInternalFills.contains(addr)) {
                closedLoopInternalFills(addr) = (d.dirResultState.peek().litValue,
                  d.dirResultDirty.peek().litToBoolean, d.dirResultPrefetched.peek().litToBoolean)
              }
            }
          }

          if (bopPrefetchSmoke) {
            (0 until numSlices).foreach { slice =>
              if (dut.observation.bopPrefetchIssueBySlice(slice).peek().litToBoolean) {
                val addr = dut.observation.bopPrefetchIssueAddrBySlice(slice).peek().litValue
                val k = smokeExpectedAddresses.indexOf(addr)
                assert(k >= 0 && slice == smokeExpectedSlices(k),
                  s"prefetch route mismatch: addr=${hex(addr)} slice=$slice expected=${smokeExpectedSlices.lift(k)}")
                val pf = smokePrefetches.find(_.addr == addr).getOrElse(
                  throw new AssertionError(s"issued prefetch without BOP request: ${hex(addr)}"))
                pf.issues += 1
              }
            }
            (0 until totalTshrs).foreach { i =>
              val d = probe(i)
              if (d.reqAlloc.peek().litToBoolean) {
                val addr = d.reqAllocPaddr.peek().litValue
                smokePrefetches.find(_.addr == addr).foreach { pf =>
                  val k = smokeExpectedAddresses.indexOf(addr)
                  assert(i / testMshrSize == smokeExpectedSlices(k))
                  pf.allocations += 1
                  assert(pf.allocations == 1, s"prefetch ${hex(addr)} allocated more than once")
                  pf.tshr = Some(i)
                }
              }
              if (d.reqReuse.peek().litToBoolean) {
                assert(!smokeExpectedAddresses.contains(d.tshrPaddr.peek().litValue),
                  "cold smoke prefetch must allocate, never reuse")
              }
            }
            smokePrefetches.filterNot(_.deallocated).foreach { pf =>
              pf.tshr.foreach { i =>
                val d = probe(i)
                if (d.valid.peek().litToBoolean && d.tshrPaddr.peek().litValue == pf.addr) {
                  pf.upResponse ||= d.upTxRspFire.peek().litToBoolean || d.upTxDatFire.peek().litToBoolean
                  pf.trained ||= d.bopTrainValid.peek().litToBoolean
                  if (d.reqActive.peek().litToBoolean && d.pRxReqIsPrefetch.peek().litToBoolean) {
                    pf.requestSeen = true
                    pf.srcID = Some(d.pRxReqSrcID.peek().litValue)
                    pf.reqOpcode = Some(d.pRxReqOpcode.peek().litValue)
                    assert(pf.srcID.contains(BigInt(l2cfg.pfSrcID)) &&
                      pf.reqOpcode.contains(CCHIOpcode.StashShared.opcode) &&
                      !d.pRxReqExpCompData.peek().litToBoolean && d.pRxReqSize.peek().litValue == 6)
                  }
                  if (d.metaWrReqState.peek().litToBoolean) {
                    pf.fillState = Some(d.metaWrReqMetaState.peek().litValue)
                  }
                  if (d.dirWbLand.peek().litToBoolean) {
                    pf.landedState = Some(d.dirResultState.peek().litValue)
                    pf.fillDirty = Some(d.dirResultDirty.peek().litToBoolean)
                  }
                  if (d.dealloc.peek().litToBoolean) pf.deallocated = true
                }
              }
            }
          }

          if (bopCollision && collisionBaseline.isEmpty && issued == closedLoopStageARequests &&
              completed == closedLoopStageARequests && cycle >= nextClosedLoopCycle + 500) {
            assert(closedLoopMeasuredOffset.contains(BigInt(10 * 64)),
              s"Stage-A must learn +10 before collision: measured=$closedLoopMeasuredOffset")
            collisionBaseline = Some((closedLoopTrainEvents, closedLoopPrefetchedHitTrains,
              dnTxReqCount, completed, evictBackHandles.size))
            collisionBaseDrops = Some((closedLoopCrossPageDrops, closedLoopDisabledDrops))
            // Base Stage-A derivation: D0..159 plus targets D158..169 has
            // <=2 distinct tags per (slice,set), so no valid victim replacement.
            // Invalid victims skip EvictBack. Do not reuse frozen 160-2=158.
            assert(closedLoopTrainEvents == closedLoopStageARequests &&
              closedLoopTrainCounts.toMap == expectedBOP.trains(160).map(a => (a >> 6) -> 1).toMap &&
              closedLoopPrefetchedHitTrains == expectedBOP.hits(160).size &&
              closedLoopPrefetchedHitAddresses.toSet == expectedBOP.hits(160) &&
              closedLoopPrefetchIssues == expectedBOP.issues(160).size &&
              closedLoopBopReqAddresses.toSet == expectedBOP.issues(160) &&
              closedLoopQueueIssues.toMap == expectedBOP.issues(160).map(_ -> 1).toMap &&
              closedLoopCrossPageAddresses.toSet == expectedBOP.crossPage(160) &&
              closedLoopDisabledAddresses.toSet == expectedBOP.disabled(160) &&
              closedLoopCrossPageDrops == 4 && closedLoopDisabledDrops == 144 &&
              dnTxReqCount == 158 && closedLoopFullDrops == 0 &&
              expectedBOP.validReplacements(expectedBOP.trains(160) ++ expectedBOP.issues(160), numSlices) == 0 &&
              evictBackHandles.isEmpty && completedEvictBackHandles.isEmpty &&
              pendingEvictHandleBySource.isEmpty && activeEvictHandleByTshr.isEmpty,
              "base Stage-A counts/address sets/valid-victim replacements differ from independent expectations")
            println(s"BOP_COLLISION_BASE case=$selectedCase cycle=$cycle offset=+10 " +
              s"train=$closedLoopTrainEvents prefetchedHit=$closedLoopPrefetchedHitTrains dnTxReq=$dnTxReqCount completed=$completed " +
              s"evict=${evictBackHandles.size}/${completedEvictBackHandles.size}/${pendingEvictHandleBySource.size}/${activeEvictHandleByTshr.size}")
          }
          if (bopCollision) {
            val allocator = dut.observation.allocator
            if (readbackAcceptCycle.contains(cycle) && allocator.reqValid.peek().litToBoolean &&
                allocator.reqPaddr.peek().litValue == collisionAddress) {
              readbackAllocator = Some((cycle, allocator.allocVec.peek().litValue,
                allocator.reuseVec.peek().litValue, allocator.paddrHitVec.peek().litValue))
              println(s"BOP_COLLISION_READBACK_ALLOC case=$selectedCase cycle=$cycle " +
                s"paddr=0x${collisionAddress.toString(16)} alloc=0x${allocator.allocVec.peek().litValue.toString(16)} " +
                s"reuse=0x${allocator.reuseVec.peek().litValue.toString(16)} " +
                s"paddrHit=0x${allocator.paddrHitVec.peek().litValue.toString(16)}")
            }
            (0 until totalTshrs).foreach { index =>
              val target = probe(index)
              if (target.valid.peek().litToBoolean && target.tshrPaddr.peek().litValue == collisionAddress) {
                targetPrefetchedLast = Some(dut.observation.dirProbe(index).dirResultPrefetched.peek().litToBoolean)
                if (readbackAcceptCycle.nonEmpty && target.dirRdResp.peek().litToBoolean) {
                  assert(target.fromDirMetaHit.peek().litToBoolean, "readback must find the committed line")
                  directoryReadbackPrefetched = Some(target.fromDirMetaPrefetched.peek().litToBoolean)
                }
              }
            }
            if (!heldDnReleased && releaseHeldAt.exists(_ <= cycle)) {
              assert(heldDnTxn.nonEmpty, "release requires a held downstream TxnID")
              heldDnReleased = true
              println(s"BOP_COLLISION_RELEASE case=$selectedCase cycle=$cycle txn=${heldDnTxn.get.v}")
            }
          }

          val dnTxFire = dut.io.chi.txreq.flitv.peek().litToBoolean
          if (dnTxFire) {
            val dnTxnID = DnTxnID(dut.io.chi.txreq.flit.TxnID.get.peek().litValue)
            downstream.noteRequest(dnTxnID)
            val srcId = dut.io.chi.txreq.flit.SrcID.get.peek().litValue
            val opcode = dut.io.chi.txreq.flit.Opcode.get.peek().litValue
            val (sliceIdx, tshrId) = decodeDnTxnID(dnTxnID)
            val globalTshr = tshrIndex(sliceIdx, tshrId)
            val internalPrefetch = probe(globalTshr).pRxReqIsPrefetch.peek().litToBoolean
            if (bopPrefetchSmoke) {
              val dnAddr = dut.io.chi.txreq.flit.Addr.get.peek().litValue
              smokePrefetches.find(_.addr == dnAddr).foreach { pf =>
                assert(internalPrefetch && pf.tshr.contains(globalTshr),
                  s"prefetch ${hex(dnAddr)} downstream source does not match its allocation")
                pf.dnOpcode = Some(opcode)
                pf.dnRequests += 1
                assert(pf.dnRequests == 1 && opcode == L2TopDownstreamOpcodes.ReadNotSharedDirty)
              }
            }
            if (bopCollision && collisionBaseline.nonEmpty) {
              collisionDnReqs += ((dut.io.chi.txreq.flit.Addr.get.peek().litValue, opcode, internalPrefetch))
              val holdThis = dut.io.chi.txreq.flit.Addr.get.peek().litValue == collisionAddress &&
                (if (selectedCase.endsWith("c3")) !internalPrefetch else internalPrefetch)
              if (holdThis && heldDnTxn.isEmpty) {
                heldDnTxn = Some(dnTxnID)
                firstCollisionDnCycle = Some(cycle)
                println(s"BOP_COLLISION_HOLD case=$selectedCase cycle=$cycle txn=${dnTxnID.v} internal=$internalPrefetch")
              }
            }
            if ((bopClosedLoop || bopCollision) && internalPrefetch) {
              closedLoopPrefetchIssues += 1
              closedLoopPrefetchIssueCycles.getOrElseUpdate(dut.io.chi.txreq.flit.Addr.get.peek().litValue, cycle)
            }
            if (isEvictReq(opcode)) {
              if (selectedCase == "dirty-victim" && victimBaseSet) victimDnOpcodes += opcode
              val handle = nextHandle
              nextHandle += 1
              val parent = tshrHandles(globalTshr)
              handleParents(handle) = parent
              insertDnTxn(dnTxnID, DnTxnInfo(dnTxnID, sliceIdx, tshrId, handle, parent, InternalOrigin, opcode), cycle)
              val rspOpcode = downstreamEvictRspOpcode(opcode)
              trace(handle, parent, cycle, "DnTXREQ", "Out", opcode.toString, s"TxnID:${dnTxnID.v}", "issue")
              println(s"DN_TXREQ cycle=$cycle dnTxn=${dnTxnID.v} origin=internal opcode=$opcode enqueueRxRspOpcode=$rspOpcode due=${cycle + responseDelay}")
              pendingDnRsp.enqueue(PendingDnRsp(dnTxnID, srcId, cycle + responseDelay, rspOpcode))
            } else {
              val dnAddr = dut.io.chi.txreq.flit.Addr.get.peek().litValue
              val request = if (internalPrefetch) None else pendingL1Reqs.dequeueFirst(_.addr == dnAddr)
              assert(internalPrefetch || request.nonEmpty,
                s"DnTXREQ has neither matching demand nor internal-prefetch source: cycle=$cycle dnTxn=${dnTxnID.v} slice=$sliceIdx tshr=$tshrId addr=0x${dnAddr.toString(16)}")
              val origin = if (request.nonEmpty) {
                val req = request.get
                L1Origin(req.handle, req.upTxnID, req.upOpcode, req.expectsCompAck)
              } else {
                InternalOrigin
              }
              val handle = origin match {
                case L1Origin(value, _, _, _) => value
                case InternalOrigin => rootHandle.getOrElse(0L)
              }
              val parent = handleParents.getOrElse(handle, None)
              if (!internalPrefetch && !isEvictReq(opcode)) {
                tshrHandles(globalTshr) = Some(handle)
                rootTshr = Some(globalTshr)
                origin match {
                  case L1Origin(_, upTxnID, _, _) if selectedCase == "dirty-victim" && upTxnID.v == 4 =>
                    victimATshr = globalTshr
                  case _ => ()
                }
                handleParents(handle) = None
                unassignedRootHandles.dequeueFirst(_ == handle)
                println(s"HANDLE_BIND_DNTX cycle=$cycle handle=$handle parent=0 tshr=$tshrId dnTxn=${dnTxnID.v}")
              }
              insertDnTxn(dnTxnID, DnTxnInfo(dnTxnID, sliceIdx, tshrId, handle, parent, origin, opcode), cycle)
              trace(handle, parent, cycle, "DnTXREQ", "Out", opcode.toString, s"TxnID:${dnTxnID.v}", "issue")
              val dnResp = origin match {
                case L1Origin(_, _, upOpcode, _) if bopCollision && upOpcode == CCHIOpcode.ReadUnique.opcode =>
                  BigInt(CHIFieldResp.CompData_UC)
                case _ if opcode == L2TopDownstreamOpcodes.ReadNotSharedDirty =>
                  caseSpec.dn.readNotSharedDirtyResp
                case L1Origin(_, upTxnID, _, _) => caseSpec.up.stream.downstreamDataResp(upTxnID.v.toInt, opcode)
                case InternalOrigin => BigInt(CHIFieldResp.CompData_SC)
              }
              val originText = origin match {
                case L1Origin(_, upTxnID, _, expectsCompAck) => s"upTxn=${upTxnID.v} expectUpCompAck=${if (expectsCompAck) 1 else 0}"
                case InternalOrigin => "internal"
              }
              println(s"DN_TXREQ cycle=$cycle dnTxn=${dnTxnID.v} slice=$sliceIdx tshr=$tshrId origin=$originText opcode=$opcode enqueueRxDatDue=${cycle + responseDelay}/${cycle + responseDelay + 1}")
              pendingDnComp.enqueue(PendingDnComp(dnTxnID, srcId, cycle + responseDelay, 0, dnResp))
              pendingDnComp.enqueue(PendingDnComp(dnTxnID, srcId, cycle + responseDelay + 1, 1, dnResp))
            }
            if (!internalPrefetch) {
              dnTxReqCount += 1
            }
          }

          pendingDnComp.dequeueFirst(p => p.due <= cycle && canDriveRxDat && downstream.canDeliverResponse &&
            (!heldDnTxn.contains(p.dnTxnID) || heldDnReleased)).foreach { p =>
            val data = dnDataValue(p)
            driveDnData(p)
            val resp = p.resp
            val dataId = if (p.beat == 0) 0 else 2
            val info = dnTxnTable.getOrElse(p.dnTxnID,
              throw new AssertionError(s"DnRXDAT for unknown transaction: cycle=$cycle dnTxn=${p.dnTxnID.v} table=$dnTxnTable"))
            trace(info.handle, info.parentHandle, cycle, "DnRXDAT", "In", fullChiCompDataOpcode.toString, s"TxnID:${p.dnTxnID.v}", s"beat${p.beat}")
            info.origin match {
              case L1Origin(_, upTxnID, _, _) if selectedCase == "dirty-victim" && upTxnID.v < 4 =>
                preloadData((upTxnID.v.toInt, dataId)) = data
              case _ => ()
            }
            println(s"DRIVE_DN_COMPDATA cycle=$cycle dnTxn=${p.dnTxnID.v} opcode=$fullChiCompDataOpcode resp=$resp dataID=$dataId dirtyResp=${if (resp == CHIFieldResp.CompData_UD_PD) 1 else 0} data=${hex(data)}")
            downstream.noteResponse(p.dnTxnID, p.beat == 1, cycle)
            if (p.beat == 1 && dnTxnTable.get(p.dnTxnID).exists(_.origin == InternalOrigin)) {
              completedInternalDnTxn += p.dnTxnID
            }
          }
          pendingDnRsp.dequeueFirst(p => p.due <= cycle && canDriveRxRsp && downstream.canDeliverResponse &&
            (!heldDnTxn.contains(p.dnTxnID) || heldDnReleased)).foreach { p =>
            driveDnRsp(p)
            val info = dnTxnTable.getOrElse(p.dnTxnID,
              throw new AssertionError(s"DnRXRSP for unknown transaction: cycle=$cycle dnTxn=${p.dnTxnID.v} table=$dnTxnTable"))
            trace(info.handle, info.parentHandle, cycle, "DnRXRSP", "In", p.opcode.toString, s"TxnID:${p.dnTxnID.v}", "response")
            println(s"DRIVE_DN_RSP cycle=$cycle dnTxn=${p.dnTxnID.v} opcode=${p.opcode}")
            downstream.noteResponse(p.dnTxnID, finalBeat = true, cycle)
            if (dnTxnTable.get(p.dnTxnID).exists(_.origin == InternalOrigin)) {
              completedInternalDnTxn += p.dnTxnID
            }
          }

          val upTxDatFire = dut.io.t1p(0).DnDAT.valid.peek().litToBoolean &&
            dut.io.t1p(0).DnDAT.ready.peek().litToBoolean
          if (upTxDatFire) {
            val upTxnID = UpTxnID(dut.io.t1p(0).DnDAT.bits.TxnID.peek().litValue)
            val upDBID = UpDBID(dut.io.t1p(0).DnDAT.bits.DBID.peek().litValue)
            val responseSliceIdx = dut.io.t1p(0).DnDAT.bits.SrcID.peek().litValue.toInt
            assert(responseSliceIdx >= 0 && responseSliceIdx < numSlices,
              s"UpTXDAT has invalid source slice: cycle=$cycle slice=$responseSliceIdx numSlices=$numSlices")
            val opcode = dut.io.t1p(0).DnDAT.bits.Opcode.peek().litValue
            val dataId = dut.io.t1p(0).DnDAT.bits.DataID.peek().litValue
            upTxDatCount += 1
            if (opcode == CCHIOpcode.CompData.opcode && (dataId == 1 || dataId == 2)) {
              val matches = dnTxnTable.values.filter(info =>
                info.tshrId == upDBID.v && (info.origin match {
                  case L1Origin(_, `upTxnID`, _, true) => true
                  case _ => false
                })).toSeq
              assert(matches.size <= 1,
                s"ambiguous demand UpTXDAT source: cycle=$cycle upTxn=${upTxnID.v} upDBID=${upDBID.v} matches=$matches")
              matches.headOption match {
                case Some(info) =>
                  assert(info.sliceIdx == responseSliceIdx,
                    s"UpTXDAT slice mismatch: cycle=$cycle src=$responseSliceIdx info=$info")
                  pendingUpAck(upDBID) = PendingUpAck(info.handle, upDBID, upTxnID, cycle + 1, responseSliceIdx, Some(info.dnTxnID))
                  trace(info.handle, info.parentHandle, cycle, "UpTXDAT", "Out", opcode.toString, s"DBID:${upDBID.v}", "data")
                case None =>
                  pendingL1Reqs.dequeueFirst(_.upTxnID == upTxnID).fold {
                    throw new AssertionError(s"demand UpTXDAT has neither matching L1 transaction nor pending demand: cycle=$cycle upTxn=${upTxnID.v} upDBID=${upDBID.v} table=$dnTxnTable")
                  } { req =>
                    pendingUpAck(upDBID) = PendingUpAck(req.handle, upDBID, upTxnID, cycle + 1, responseSliceIdx)
                    trace(req.handle, handleParents.getOrElse(req.handle, None), cycle, "UpTXDAT", "Out", opcode.toString, s"DBID:${upDBID.v}", "demand-hit")
                  }
              }
            }
          }

          pendingUpAck.values.find(_.due <= cycle).foreach { p =>
            driveUpCompAck(p.upDBID, p.sliceIdx)
            trace(p.handle, handleParents.getOrElse(p.handle, None), cycle, "UpRXRSP", "In", CCHIOpcode.CompAck.opcode.toString, s"TxnID:${p.upDBID.v}", "complete")
            pendingUpAck -= p.upDBID
            p.dnTxnID.foreach { dnTxnID =>
              val info = dnTxnTable.remove(dnTxnID).getOrElse(
                throw new AssertionError(s"CompAck has no matching Dn transaction: cycle=$cycle dnTxn=${dnTxnID.v}"))
              assert(info.handle == p.handle && info.origin.isInstanceOf[L1Origin],
                s"CompAck matched unexpected Dn transaction: cycle=$cycle ack=$p info=$info")
            }
            completed += 1
            droveUpRsp = true
          }

          pendingSnpData.headOption.filter(_.due <= cycle).foreach { p =>
            driveUpSnpRespData(p)
            println(s"DRIVE_UP_SNPDATA cycle=$cycle snpTxn=${p.snpTxnID.v} opcode=${CCHIOpcode.SnpRespData.opcode} resp=${p.resp} dataID=${if (p.beat == 0) 0 else 1}")
            if (selectedCase == "snoop-data-passdirty" && p.resp == CCHIResp.I_PD.value) {
              if (p.beat == 0) {
                sawSnoopDataPassDirtyBeat0 = true
              } else {
                sawSnoopDataPassDirtyBeat2 = true
              }
            }
            pendingSnpData.dequeue()
          }

          pendingSnpRsp.headOption.filter(_.due <= cycle && !droveUpRsp).foreach { p =>
            driveUpSnpResp(p.snpTxnID, p.tgtId)
            println(s"DRIVE_UP_SNPRESP cycle=$cycle snpTxn=${p.snpTxnID.v} opcode=${CCHIOpcode.SnpResp.opcode} resp=${CCHIResp.I.value} respName=${CCHIResp.I.name}")
            pendingSnpRsp.dequeue()
            droveUpRsp = true
          }

          val sEvictNow = dut.observation.tshr0.sEvict.peek().litToBoolean
          // EvictBack created = source TSHR UpTXEVB.fire. sliceEvictBackFire is the
          // arbiter output (L2TSHRCtrl.scala:143); the per-TSHR source is
          // sliceSourceFireVec, read below.
          val localReqFires = (0 until numSlices).filter(sliceIdx =>
            dut.observation.sliceEvictBackFire(sliceIdx).peek().litToBoolean)
          val upTxReqFireNow = localReqFires.nonEmpty
          val upTxReqFromPrefetch = localReqFires.exists { sliceIdx =>
            val mask = dut.observation.sliceSourceFireVec(sliceIdx).peek().litValue
            assert(mask.bitCount == 1, s"local UpTXREQ has invalid source: cycle=$cycle slice=$sliceIdx mask=0x${mask.toString(16)}")
            sliceTshrProbe(sliceIdx, mask.lowestSetBit).pRxReqIsPrefetch.peek().litToBoolean
          }

          if (selectedCase == "load-miss" &&
              dut.io.t1p(0).UpREQ.valid.peek().litToBoolean &&
              !dut.io.t1p(0).UpREQ.ready.peek().litToBoolean) {
            hostAllocBlockedCycles += 1
            if (hostAllocFirstBlockedCycle.isEmpty) {
              hostAllocFirstBlockedCycle = Some(cycle)
            }
          }
          if (selectedCase == "load-miss" && sEvictNow) {
            if (evictbackLoopFirstValidCycle.isEmpty) {
              evictbackLoopFirstValidCycle = Some(cycle)
            }
            if (evictbackLoopFireCycle.isEmpty && !upTxReqFireNow) {
              evictbackLoopBlockedCycles += 1
            }
          }
          // Fire is the arbiter output, not a separate UpTXREQ valid/ready pair:
          // base carries EvictBack on UpTXEVB and that pair no longer exists.
          if (selectedCase == "load-miss" && sEvictNow && upTxReqFireNow && evictbackLoopFireCycle.isEmpty) {
            evictbackLoopFireCycle = Some(cycle)
          }

          if (upTxReqFireNow && !upTxReqFromPrefetch) {
            upTxReqCount += 1
          }
          if (dut.io.t1p(0).DnSNP.valid.peek().litToBoolean && dut.io.t1p(0).DnSNP.ready.peek().litToBoolean) {
            val snpTxnID = SnpTxnID(dut.io.t1p(0).DnSNP.bits.TxnID.peek().litValue)
            val snpTgtID = dut.io.t1p(0).DnSNP.bits.SrcID.peek().litValue
            caseSpec.up.snoopPolicy match {
              case SnoopPolicy.SnpRespDataI_PD =>
                println(s"OBS_DNSNP cycle=$cycle snpTxn=${snpTxnID.v} opcode=${dut.io.t1p(0).DnSNP.bits.Opcode.peek().litValue} addr=0x${dut.io.t1p(0).DnSNP.bits.Addr.peek().litValue.toString(16)} alias=${dut.io.t1p(0).DnSNP.bits.alias.peek().litValue} responder=SnpRespDataI_PD")
                pendingSnpData.enqueue(PendingSnpData(snpTxnID, snpTgtID, cycle + 1, 0, CCHIResp.I_PD.value))
                pendingSnpData.enqueue(PendingSnpData(snpTxnID, snpTgtID, cycle + 2, 1, CCHIResp.I_PD.value))
              case SnoopPolicy.SnpRespI =>
                println(s"OBS_DNSNP cycle=$cycle snpTxn=${snpTxnID.v} opcode=${dut.io.t1p(0).DnSNP.bits.Opcode.peek().litValue} addr=0x${dut.io.t1p(0).DnSNP.bits.Addr.peek().litValue.toString(16)} alias=${dut.io.t1p(0).DnSNP.bits.alias.peek().litValue} responder=no-data-SnpRespI")
                pendingSnpRsp.enqueue(PendingSnpRsp(snpTxnID, snpTgtID, cycle + 1))
            }
            upTxSnpCount += 1
          }

          if (dut.io.chi.txrsp.flitv.peek().litToBoolean) {
            dnTxRspCount += 1
          }
          if (selectedCase == "dirty-victim" && victimBaseSet && nonTopBETraceActive()) {
            printBETrace(cycle)
          }
          if (dut.io.chi.txdat.flitv.peek().litToBoolean) {
            dnTxDatCount += 1
            if (selectedCase == "dirty-victim" && victimBaseSet) {
              victimDnTxDat += 1
              val dataId = dut.io.chi.txdat.flit.DataID.get.peek().litValue
              victimBE = dut.io.chi.txdat.flit.BE.get.peek().litValue
              val data = dut.io.chi.txdat.flit.Data.get.peek().litValue
              val expectedDataOpt = if (dataId == 0 || dataId == 2) preloadData.get((0, dataId.toInt)) else None
              val dataMatches = expectedDataOpt.contains(data)
              val expectedData = expectedDataOpt.map(hex).getOrElse("<unknown>")
              if (dataId == 0) {
                victimDnTxDatBeat0 = true
                victimBeat0BE = victimBE
                victimBeat0Data = data
              }
              if (dataId == 2) {
                victimDnTxDatBeat2 = true
                victimBeat2BE = victimBE
                victimBeat2Data = data
              }
              println(s"VICTIM_DNTXDAT cycle=$cycle opcode=${dut.io.chi.txdat.flit.Opcode.get.peek().litValue} txn=${dut.io.chi.txdat.flit.TxnID.get.peek().litValue} " +
                s"beat=$dataId resp=${dut.io.chi.txdat.flit.Resp.get.peek().litValue} BE=0x${victimBE.toString(16)} " +
                s"data=${hex(data)} expected=$expectedData dataMatchesExpected=${if (dataMatches) 1 else 0}")
              printBETrace(cycle)
            }
          }

          val allocator = dut.observation.allocator
          if (allocator.reqValid.peek().litToBoolean) {
            println(s"ALLOCATOR_TRACE cycle=$cycle reqPaddr=0x${allocator.reqPaddr.peek().litValue.toString(16)} " +
              s"paddr_hit_vec=0x${allocator.paddrHitVec.peek().litValue.toString(16)} " +
              s"can_reuse_vec=0x${allocator.canReuseVec.peek().litValue.toString(16)} " +
              s"alloc_vec=0x${allocator.allocVec.peek().litValue.toString(16)} " +
              s"reuse_vec=0x${allocator.reuseVec.peek().litValue.toString(16)} " +
              s"alloc_any=${b(allocator.allocAny)} reuse_any=${b(allocator.reuseAny)} " +
              s"postcluster.valid=${b(allocator.postclusterValid)} postcluster.ready=${b(allocator.postclusterReady)} " +
              s"rxreq.valid=${b(allocator.reqValid)} rxreq.ready=${b(allocator.reqReady)} fire=${if (upTxReqFireNow) 1 else 0}")
          }

          val currentSEvictHandles = ArrayBuffer.empty[(Int, Long, Long)]
          for (i <- 0 until totalTshrs) {
            val d = probe(i)
            val sEvictByTshr = ((d.reqStateMask.peek().litValue >> 23) & 1) == 1
            val internalPrefetch = d.pRxReqIsPrefetch.peek().litToBoolean
            if (sEvictByTshr && !prevSEvictByTshr(i) && !internalPrefetch) {
              sEvictRiseByTshr(i) += 1
              sEvictRiseCount += 1
              val handle = tshrHandles(i).getOrElse(0L)
              val parent = tshrHandles(i).flatMap(handleParents.get).flatten
              val riseCount = handleSEvictRises(handle) + 1
              handleSEvictRises(handle) = riseCount
              assert(handle != 0L, s"s_evict rise has no transaction handle: cycle=$cycle tshr=$i")
              assert(riseCount <= 1, s"handle=$handle has multiple s_evict rises: count=$riseCount cycle=$cycle tshr=$i")
              assert(parent.isEmpty, s"derived handle=$handle parent=${parent.get} produced s_evict: cycle=$cycle tshr=$i")
              handleSEvictCycle(handle) = cycle
              assert(!pendingEvictHandleBySource.contains(i),
                s"source TSHR $i already has a pending EvictBack handle at cycle=$cycle")
              assert(handleChildCount(handle) == 0,
                s"parent handle=$handle has multiple EvictBack children")
              val childHandle = nextHandle
              nextHandle += 1
              handleParents(childHandle) = Some(handle)
              handleChildCount(handle) += 1
              evictBackHandles += childHandle
              pendingEvictHandleBySource(i) = childHandle
              currentSEvictHandles += ((i, handle, childHandle))
              println(s"HANDLE_CREATE cycle=$cycle handle=$childHandle parent=$handle sourceTshr=$i targetTshr=pending " +
                s"evictback=1 reqOpcode=${CCHIOpcode.EvictBack.opcode} trigger=s_evict_rise")
              trace(childHandle, Some(handle), cycle, "Internal", "Derived", "EvictBack", s"SourceTSHR:$i", "create")
              println(s"SEVICT_TRACE cycle=$cycle tshr=$i handle=$handle " +
                s"parent=${parent.map(_.toString).getOrElse("0")} " +
                s"evictback=${b(d.pRxReqEvictback)} reqOpcode=${d.pRxReqOpcode.peek().litValue} " +
                s"upTxReqFire=${if (upTxReqFireNow) 1 else 0} bHandle=$childHandle")
            }
            prevSEvictByTshr(i) = sEvictByTshr
          }

          val evictBackTargets = scala.collection.mutable.Set.empty[Int]
          localReqFires.foreach { sliceIdx =>
            val sourceMask = dut.observation.sliceSourceFireVec(sliceIdx).peek().litValue
            val allocMask = dut.observation.sliceAllocVec(sliceIdx).peek().litValue
            val reuseMask = dut.observation.sliceReuseVec(sliceIdx).peek().litValue
            val targetMask = allocMask | reuseMask
            assert(sourceMask.bitCount == 1,
              s"EvictBack fire must have exactly one source TSHR: cycle=$cycle sourceMask=0x${sourceMask.toString(16)}")
            assert(targetMask.bitCount == 1,
              s"EvictBack fire must have exactly one allocation/reuse target: cycle=$cycle targetMask=0x${targetMask.toString(16)}")
            val sourceTshr = tshrIndex(sliceIdx, sourceMask.lowestSetBit)
            val targetTshr = tshrIndex(sliceIdx, targetMask.lowestSetBit)
            if (!sliceTshrProbe(sliceIdx, sourceMask.lowestSetBit).pRxReqIsPrefetch.peek().litToBoolean) {
              val childHandle = pendingEvictHandleBySource.remove(sourceTshr).getOrElse(
                throw new AssertionError(s"EvictBack fire has no handle created at s_evict rise: cycle=$cycle sourceTshr=$sourceTshr"))
              val parentHandle = handleParents(childHandle).get
              tshrHandles(targetTshr) = Some(childHandle)
              activeEvictHandleByTshr(targetTshr) = childHandle
              evictBackTargets += targetTshr
              loadMissBTshr = loadMissBTshr.orElse(Some(targetTshr))
              if (selectedCase == "dirty-victim" && dnTxnTable.values.exists(info =>
                  info.handle == parentHandle && (info.origin match {
                    case L1Origin(_, upTxnID, _, _) => upTxnID.v == 4
                    case _ => false
                  }))) victimBTshr = targetTshr
              val allocationKind = if (((allocMask >> (targetTshr % testMshrSize)) & 1) == 1) "alloc" else "reuse"
              println(s"HANDLE_BIND_EVICTBACK cycle=$cycle handle=$childHandle parent=$parentHandle sourceTshr=$sourceTshr " +
                s"targetTshr=$targetTshr event=$allocationKind reqPaddr=0x${allocator.reqPaddr.peek().litValue.toString(16)}")
              trace(childHandle, Some(parentHandle), cycle, "Internal", "Derived", "EvictBack", s"TSHR:$targetTshr", allocationKind)
            }
          }

          val completedTargets = ArrayBuffer.empty[Int]
          activeEvictHandleByTshr.toSeq.sortBy(_._1).foreach { case (i, childHandle) =>
            val d = probe(i)
            val parentHandle = handleParents(childHandle).get
            if (d.dealloc.peek().litToBoolean || d.rxReqFire.peek().litToBoolean ||
                d.upTxRspFire.peek().litToBoolean || d.dnTxRspFire.peek().litToBoolean) {
              println(s"LOOPBACK_EVENT cycle=$cycle handle=$childHandle parent=$parentHandle tshr=$i " +
                s"valid=${b(d.valid)} alloc=${b(d.alloc)} reuse=${b(d.reuse)} dealloc=${b(d.dealloc)} " +
                s"enterREQ=${b(d.enterREQ)} rxReqFire=${b(d.rxReqFire)} pRxReqOpcode=${d.pRxReqOpcode.peek().litValue} " +
                s"pRxReqEvictback=${b(d.pRxReqEvictback)} reqActive=${b(d.reqActive)} " +
                s"reqStateMask=0x${d.reqStateMask.peek().litValue.toString(16)} upTxRspFire=${b(d.upTxRspFire)} dnTxRspFire=${b(d.dnTxRspFire)}")
            }
            if (d.dealloc.peek().litToBoolean) {
              assert(!completedEvictBackHandles.contains(childHandle),
                s"EvictBack handle=$childHandle completed more than once")
              completedEvictBackHandles += childHandle
              completedTargets += i
              trace(childHandle, Some(parentHandle), cycle, "Internal", "Derived", "EvictBack", s"TSHR:$i", "complete")
            }
          }
          completedTargets.foreach(activeEvictHandleByTshr.remove)

          recordAttribution(cycle, evictBackTargets.toSet)
          currentSEvictHandles.foreach { case (_, parentHandle, childHandle) =>
            assert(handleChildCount(parentHandle) == 1,
              s"s_evict parent=$parentHandle must have exactly one B handle at cycle=$cycle")
            assert(evictBackHandles.contains(childHandle),
              s"s_evict parent=$parentHandle lost B handle=$childHandle at cycle=$cycle")
            println(s"SEVICT_PAIR cycle=$cycle parent=$parentHandle child=$childHandle " +
              s"upTxReqFire=${if (upTxReqFireNow) 1 else 0} bCreated=1 relation=created-at-s_evict-rise")
          }
          assert(evictBackHandles.size == sEvictRiseCount,
            s"Every s_evict rise must create exactly one B handle: rises=$sEvictRiseCount handles=${evictBackHandles.size} cycle=$cycle")
          if (upTxReqFireNow && !prevUpTxReqFire) {
            upTxReqRiseCount += 1
          }
          prevUpTxReqFire = upTxReqFireNow
          stepWithCredits(cycle)

          if (bopClosedLoop) {
            val compareValid = dut.observation.bopReqValid.peek().litToBoolean
            val compareReady = dut.observation.bopReqReady.peek().litToBoolean
            val compareEnqReady = dut.observation.bopReqEnqReady.peek().litToBoolean
            val compareFire = dut.observation.bopReqFire.peek().litToBoolean
            if (compareValid && !bopReqComparePreviousValid && bopReqCompareWindowsSeen < 3) {
              bopReqCompareWindowsSeen += 1
              bopReqCompareWindowRemaining = 3
            }
            if (bopReqCompareWindowRemaining > 0) {
              println(s"BOP_REQ_COMPARE cycle=${cycle + 1} " +
                s"valid=${if (compareValid) 1 else 0} ready=${if (compareReady) 1 else 0} " +
                s"enqReady=${if (compareEnqReady) 1 else 0} fire=${if (compareFire) 1 else 0}")
              bopReqCompareWindowRemaining -= 1
            }
            bopReqComparePreviousValid = compareValid
          }

          val dirtyVictimBadBE = selectedCase == "dirty-victim" && victimBaseSet &&
            victimDnTxDatBeat0 && victimDnTxDatBeat2 && (victimBeat0BE == 0 || victimBeat2BE == 0)
          val dirtyVictimDataMatches =
            preloadData.get((0, 0)).contains(victimBeat0Data) && preloadData.get((0, 2)).contains(victimBeat2Data)
          val dirtyVictimDSReadSeen = grants().dsRd - victimBaseGrant.dsRd > 0
          val dirtyVictimKnownBEFailure = dirtyVictimBadBE && dirtyVictimDataMatches && dirtyVictimDSReadSeen
          val evictBacksComplete = pendingEvictHandleBySource.isEmpty && activeEvictHandleByTshr.isEmpty &&
            completedEvictBackHandles.size == evictBackHandles.size
          if (bopCollision && heldDnTxn.nonEmpty && !selectedCase.endsWith("c3") &&
              secondAcceptCycle.nonEmpty && releaseHeldAt.isEmpty) releaseHeldAt = secondAcceptCycle.map(_ + 20)
          val postDone = selectedCase match {
            case "bop-prefetch-smoke" | "bop-prefetch-smoke-uc" => smokeDone
            case "bop-closed-loop" | "bop-closed-loop-uc" => completed >= closedLoopTotalRequests && cycle >= nextClosedLoopCycle + 500
            case name if name.startsWith("bop-collision-") =>
              issued == requestCount && completed == requestCount && heldDnReleased &&
                cycle >= secondAcceptCycle.getOrElse(cycle) + 500
            case "bop-overwrite-queue" => cycle >= 5000 && overwriteExpectedQueue.isEmpty
            case "prefetched-clear" | "prefetched-clear-readunique" => false
            case "load-miss" => firstCasePrinted
            case "dirty-victim" => victimBaseSet && completed >= 5 && victimDnTxDatBeat0 && victimDnTxDatBeat2 &&
              victimBeat0BE != 0 && victimBeat2BE != 0 && evictBacksComplete
            case "snoop-data-passdirty" => passSnoopDataPassDirty
          }

          if (dirtyVictimBadBE) {
            failed = true
            knownFailMatched = dirtyVictimKnownBEFailure
            failReason = s"dirty-victim CopyBackWrData BE must be nonzero, beat0BE=0x${victimBeat0BE.toString(16)} beat2BE=0x${victimBeat2BE.toString(16)}"
            cycle += 1
            printCaseReport("dirty-victim", cycle, issued, victimBaseHost, victimBaseGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = false)
            println(s"VICTIM_RESULT BE=0x${victimBE.toString(16)} dnTxDatDelta=${dnTxDatCount - victimBaseHost.dnTxDat} victimDnTxDat=$victimDnTxDat " +
              s"beat0=${if (victimDnTxDatBeat0) 1 else 0} beat2=${if (victimDnTxDatBeat2) 1 else 0} beat0BE=0x${victimBeat0BE.toString(16)} beat2BE=0x${victimBeat2BE.toString(16)} " +
              s"dsRdGrantDelta=${grants().dsRd - victimBaseGrant.dsRd} dsAheadRdGrantDelta=${grants().dsAheadRd - victimBaseGrant.dsAheadRd} " +
              s"dirRdState=$victimDirRdState dirRdDirty=$victimDirRdDirty dirRdHit=$victimDirRdHit " +
              s"replRdState=$victimReplState replRdDirty=$victimReplDirty replRdWay=$victimReplWay replPaddr=0x${victimReplPaddr.toString(16)}" +
              victimDataSummary)
            printTshr0Probe("SIM_FAIL", cycle, upTxReqCount)
            simulationEndNs = System.nanoTime()
            println(s"COUNTS total dnTxReq=$dnTxReqCount upTxDat=$upTxDatCount completed=$completed " +
              s"dnTxDat=$dnTxDatCount dnTxRsp=$dnTxRspCount upTxReq=$upTxReqCount upTxSnp=$upTxSnpCount " +
              s"upTxReqRise=$upTxReqRiseCount sEvictRise=$sEvictRiseCount")
            println(s"SEVICT_COUNTS scope=per-tshr counts=${sEvictRiseByTshr.mkString(",")} total=${sEvictRiseByTshr.sum} " +
              "totalScope=all-tshrs rising-edges state=s_evict_up_evict")
            println(s"EVICTBACK_COUNTS created=${evictBackHandles.size} completed=${completedEvictBackHandles.size} " +
              s"pending=${pendingEvictHandleBySource.size} active=${activeEvictHandleByTshr.size}")
            println(f"TIMING case=$selectedCase elaboration_sec=${seconds(simulationStartNs - totalStartNs)}%.3f " +
              f"simulation_sec=${seconds(simulationEndNs - simulationStartNs)}%.3f total_sec=${seconds(simulationEndNs - totalStartNs)}%.3f")
            if (knownFailMatched) {
              println(s"KNOWN_FAIL case=dirty-victim reason=\"$dirtyVictimKnownFailMessage\" failReason=\"$failReason\"")
            }
          } else if (postDone) {
            finished = true
            cycle += 1
            if (selectedCase == "dirty-victim") {
              // 1/1/0/0, not the frozen 5/5/0/0: only the fifth ReadUnique replaces a
              // dirty line; the first four hit evict_victim_invalid. completed counts
              // the target TSHR's dealloc (below), and the downstream request must be
              // WriteBackFull with both CopyBackWrData beats matching the preload.
              assert(evictBackHandles.size == 1 && completedEvictBackHandles.size == 1 &&
                pendingEvictHandleBySource.isEmpty && activeEvictHandleByTshr.isEmpty,
                s"dirty-victim EvictBack must be 1/1/0/0: created=${evictBackHandles.size} " +
                  s"completed=${completedEvictBackHandles.size} pending=${pendingEvictHandleBySource.size} " +
                  s"active=${activeEvictHandleByTshr.size}")
              assert(victimDnOpcodes == Seq(fullChiWriteBackFullOpcode),
                s"dirty-victim downstream eviction must be WriteBackFull 0x1B: opcodes=$victimDnOpcodes")
              assert(dnTxReqCount - victimBaseHost.dnTxReq == 2 && upTxDatCount - victimBaseHost.upTxDat == 2,
                s"dirty-victim replacement must issue one downstream read and return two beats: " +
                  s"dnTxReqDelta=${dnTxReqCount - victimBaseHost.dnTxReq} upTxDatDelta=${upTxDatCount - victimBaseHost.upTxDat}")
              assert(dirtyVictimDataMatches && dnTxDatCount - victimBaseHost.dnTxDat == 2 &&
                victimBeat0BE != 0 && victimBeat2BE != 0,
                s"dirty-victim must write two matching nonzero-BE victim data beats: matches=$dirtyVictimDataMatches " +
                  s"dnTxDatDelta=${dnTxDatCount - victimBaseHost.dnTxDat} " +
                  s"beat0BE=0x${victimBeat0BE.toString(16)} beat2BE=0x${victimBeat2BE.toString(16)}")
              printCaseReport("dirty-victim", cycle, issued, victimBaseHost, victimBaseGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = true)
              println(s"VICTIM_RESULT BE=0x${victimBE.toString(16)} dnTxDatDelta=${dnTxDatCount - victimBaseHost.dnTxDat} victimDnTxDat=$victimDnTxDat " +
                s"beat0=${if (victimDnTxDatBeat0) 1 else 0} beat2=${if (victimDnTxDatBeat2) 1 else 0} beat0BE=0x${victimBeat0BE.toString(16)} beat2BE=0x${victimBeat2BE.toString(16)} " +
                s"dsRdGrantDelta=${grants().dsRd - victimBaseGrant.dsRd} dsAheadRdGrantDelta=${grants().dsAheadRd - victimBaseGrant.dsAheadRd} " +
                s"dirRdState=$victimDirRdState dirRdDirty=$victimDirRdDirty dirRdHit=$victimDirRdHit " +
                s"replRdState=$victimReplState replRdDirty=$victimReplDirty replRdWay=$victimReplWay replPaddr=0x${victimReplPaddr.toString(16)}" +
                victimDataSummary)
            } else if (selectedCase == "snoop-data-passdirty") {
              printCaseReport("snoop-data-passdirty", cycle, issued, initialHost, initialGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = true)
              println(s"SNOOP_PD_RESULT sawReqDirtyWrite=${if (sawReqDirtyWrite) 1 else 0} completed=$completed upTxSnp=$upTxSnpCount")
            } else if (bopPrefetchSmoke) {
              assert(smokePrefetches.map(_.addr).toSeq == smokeExpectedAddresses)
              smokePrefetches.zipWithIndex.foreach { case (pf, k) =>
                val pass = pf.allocations == 1 && pf.issues == 1 && pf.requestSeen &&
                  pf.tshr.exists(_ / testMshrSize == smokeExpectedSlices(k)) &&
                  pf.srcID.contains(BigInt(l2cfg.pfSrcID)) &&
                  pf.reqOpcode.contains(CCHIOpcode.StashShared.opcode) &&
                  pf.dnRequests == 1 && pf.dnOpcode.contains(L2TopDownstreamOpcodes.ReadNotSharedDirty) &&
                  pf.fillState.contains(smokeExpectedState) && pf.landedState.contains(smokeExpectedState) &&
                  pf.fillDirty.contains(false) && pf.deallocated && !pf.upResponse && !pf.trained
                println(s"BOP_SMOKE_PREFETCH k=$k pass=$pass addr=${hex(pf.addr)} " +
                  s"slice=${pf.tshr.map(_ / testMshrSize)} tshr=${pf.tshr} alloc=${pf.allocations} issue=${pf.issues} " +
                  s"srcID=${pf.srcID} opcode=${pf.reqOpcode} dnRequests=${pf.dnRequests} dnOpcode=${pf.dnOpcode} " +
                  s"fillState=${pf.fillState} landedState=${pf.landedState} dirty=${pf.fillDirty} " +
                  s"deallocated=${pf.deallocated} upResponse=${pf.upResponse} trained=${pf.trained}")
                assert(pass, s"prefetch $k failed its lifecycle checks")
              }
              val pass = issued == 8 && completed == 8 && dnTxReqCount == 8 &&
                upTxDatCount == 16 && dnTxDatCount == 0 &&
                dut.observation.bopTrainFireCount.peek().litValue == 8 && closedLoopTrainEvents == 8 &&
                closedLoopTrainCounts.toMap == smokeDemandAddresses.map(_ >> 6).map(_ -> 1).toMap &&
                closedLoopCrossPageDrops == 0 && closedLoopDisabledDrops == 0 &&
                closedLoopFullDrops == 0 && evictBackHandles.isEmpty && completedEvictBackHandles.isEmpty &&
                pendingEvictHandleBySource.isEmpty && activeEvictHandleByTshr.isEmpty &&
                smokePending == 0 && smokeActive == 0
              println(s"BOP_PREFETCH_SMOKE_RESULT pass=$pass case=$selectedCase slices=$numSlices " +
                s"prefetches=${smokePrefetches.size} prefetchDn=${smokePrefetches.map(_.dnRequests).sum} " +
                s"demandDn=$dnTxReqCount upTxDat=$upTxDatCount completed=$completed TRAIN_FIRE=$closedLoopTrainEvents " +
                s"crossPageDrops=$closedLoopCrossPageDrops evict=0/0/0/0 pending=$smokePending active=$smokeActive")
              assert(pass, "BOP prefetch smoke aggregate checks failed")
            } else if (bopCollision) {
              val (baseTrain, basePrefetchedHit, baseDnReq, baseCompleted, baseEvict) = collisionBaseline.getOrElse(
                throw new AssertionError("collision completed without Stage-A baseline"))
              val caseC2 = selectedCase.contains("-c2")
              val caseC3 = selectedCase.endsWith("c3")
              // Base C1-C3 SC/UC reasoning and address sets were written
              // before simulation in L2TopBOPExpectations. SC S->RU requires
              // 0x07 (MakeReadUnique disabled); UC UU->RU needs no downstream
              // upgrade. D0..170 plus D180 still has <=2 tags per set, so every
              // case has zero valid replacements (including C3's cold D170).
              val fillUC = expectedFillState == 3
              val expectedDnDelta = if ((caseC2 && !fillUC) || caseC3) 1 else 0
              val expectedHitDelta = if (caseC3) 1 else 2
              val expectedEvictDelta = 0
              val targetDn = collisionDnReqs.filter(_._1 == collisionAddress)
              val expectedTargetDn = if (caseC2 && !fillUC) Seq((BigInt(0x26), true), (BigInt(0x07), false))
                else if (caseC3) Seq((BigInt(0x26), false)) else Seq((BigInt(0x26), true))
              val targetDnActual = targetDn.map { case (_, opcode, internal) => (opcode, internal) }.toSeq
              val expectedPrefetchAddresses = Seq(collisionAddress, collisionAddress + 10 * 64).sorted
              val prefetchDn = collisionDnReqs.collect { case (address, _, true) => address }
              val expectedPrefetchDn = if (caseC3) Seq(collisionAddress + 10 * 64) else expectedPrefetchAddresses
              val (baseCrossPage, baseDisabled) = collisionBaseDrops.getOrElse(
                throw new AssertionError("collision completed without BOP drop baseline"))
              val readbackAllocOk = readbackAllocator.exists { case (_, alloc, reuse, paddrHit) =>
                alloc.bitCount == 1 && reuse == 0 && paddrHit == 0
              }
              val evictTuple = (evictBackHandles.size, completedEvictBackHandles.size,
                pendingEvictHandleBySource.size, activeEvictHandleByTshr.size)
              val pass = closedLoopTrainEvents - baseTrain == 2 &&
                closedLoopPrefetchedHitTrains - basePrefetchedHit == expectedHitDelta &&
                dnTxReqCount - baseDnReq == expectedDnDelta && completed - baseCompleted == 3 &&
                targetDnActual == expectedTargetDn && collisionPrefetchIssueCycle.nonEmpty &&
                collisionBopReqAddresses.sorted.toSeq == expectedPrefetchAddresses &&
                collisionPrefetchIssueAddresses.sorted.toSeq == expectedPrefetchAddresses &&
                prefetchDn.sorted.toSeq == expectedPrefetchDn &&
                closedLoopCrossPageDrops - baseCrossPage == 0 &&
                closedLoopDisabledDrops - baseDisabled == 0 && readbackAllocOk &&
                firstCollisionDnCycle.nonEmpty && secondAcceptCycle.nonEmpty &&
                (if (caseC3) collisionPrefetchIssueCycle.get >= secondAcceptCycle.get
                  else secondAcceptCycle.get > firstCollisionDnCycle.get) &&
                releaseHeldAt.exists(_ > (if (caseC3) collisionPrefetchIssueCycle.get else secondAcceptCycle.get)) &&
                targetPrefetchedLast.contains(false) && directoryReadbackPrefetched.contains(false) &&
                heldDnReleased && evictBacksComplete &&
                evictBackHandles.size - baseEvict == expectedEvictDelta &&
                pendingDnComp.isEmpty && pendingDnRsp.isEmpty && smokePending == 0 && smokeActive == 0 &&
                closedLoopTrainCounts.toMap == (expectedBOP.trains(160) ++
                  Set(expectedBOP.address(160), collisionAddress)).map(a => (a >> 6) -> 1).toMap &&
                closedLoopPrefetchedHitAddresses.toSet == (expectedBOP.hits(160) ++
                  (if (caseC3) Set(expectedBOP.address(160)) else Set(expectedBOP.address(160), collisionAddress))) &&
                closedLoopBopReqAddresses.toSet == (expectedBOP.issues(160) ++ expectedPrefetchAddresses.toSet) &&
                closedLoopQueueIssues.toMap == (expectedBOP.issues(160) ++ expectedPrefetchAddresses.toSet).map(_ -> 1).toMap &&
                closedLoopInternalFills.toMap == expectedFills(expectedBOP.issues(160) ++ expectedPrefetchDn.toSet) &&
                closedLoopCrossPageAddresses.toSet == expectedBOP.crossPage(160) &&
                closedLoopDisabledAddresses.toSet == expectedBOP.disabled(160) &&
                closedLoopFullDrops == 0 && closedLoopTrainStalls == 0 && closedLoopBopReqStalls == 0 &&
                evictBackHandles.isEmpty && completedEvictBackHandles.isEmpty &&
                collisionTargetAllocations.toSeq == Seq("alloc", "reuse", "alloc")
              if (!pass) println(s"BOP_COLLISION_DIFFERENCE targetAllocations=$collisionTargetAllocations " +
                s"expectedTargetAllocations=alloc,reuse,alloc fills=${closedLoopInternalFills.toMap} " +
                s"expectedFills=${expectedFills(expectedBOP.issues(160) ++ expectedPrefetchDn.toSet)} " +
                s"queueIssues=${closedLoopQueueIssues.toMap} trainCounts=${closedLoopTrainCounts.toMap} " +
                s"hitAddresses=${closedLoopPrefetchedHitAddresses.toSeq.sorted.map(hex).mkString(",")}")
              val sliceTrainPulses = dut.observation.sliceTrainPulseCount.map(_.peek().litValue)
              val bopTrainReceived = dut.observation.bopTrainReceiveCount.peek().litValue
              println(s"BOP_COLLISION_TRAIN_OBS case=$selectedCase " +
                s"sliceTrainPulses=${sliceTrainPulses.mkString(",")} bopTrainReceived=$bopTrainReceived")
              println(s"BOP_COLLISION_EXPECTED case=$selectedCase trainDelta=2 prefetchedHitDelta=$expectedHitDelta " +
                s"dnTxReqDelta=$expectedDnDelta completedDelta=3 targetDn=$expectedTargetDn " +
                s"bopReq=2 bopPrefetchIssue=2 prefetchDn=${expectedPrefetchDn.size} " +
                s"queuedTargetPrefetchDn=${if (caseC3) 0 else 1} " +
                s"bopAddresses=${expectedPrefetchAddresses.map(hex).mkString(",")} " +
                s"prefetchDnAddresses=${expectedPrefetchDn.map(hex).mkString(",")} " +
                s"crossPageDelta=0 disabledDelta=0 readbackAlloc=1 readbackReuse=0 " +
                s"finalLocalPrefetched=0 directoryReadbackPrefetched=0 evictCreatedDelta=$expectedEvictDelta pending=0 active=0")
              println(s"BOP_COLLISION_RESULT case=$selectedCase pass=$pass cycle=$cycle " +
                s"trainDelta=${closedLoopTrainEvents - baseTrain} prefetchedHitDelta=${closedLoopPrefetchedHitTrains - basePrefetchedHit} " +
                s"dnTxReqDelta=${dnTxReqCount - baseDnReq} completedDelta=${completed - baseCompleted} " +
                s"targetDn=$targetDnActual bopReq=${collisionBopReqAddresses.size} " +
                s"bopAddresses=${collisionBopReqAddresses.map(hex).mkString(",")} " +
                s"bopPrefetchIssue=${collisionPrefetchIssueAddresses.size} " +
                s"prefetchDn=${prefetchDn.size} prefetchDnAddresses=${prefetchDn.map(hex).mkString(",")} " +
                s"queuedTargetPrefetchDn=${prefetchDn.count(_ == collisionAddress)} " +
                s"crossPageDelta=${closedLoopCrossPageDrops - baseCrossPage} disabledDelta=${closedLoopDisabledDrops - baseDisabled} " +
                s"readbackAllocator=$readbackAllocator finalLocalPrefetched=$targetPrefetchedLast directoryReadbackPrefetched=$directoryReadbackPrefetched " +
                s"evict=${evictTuple._1}/${evictTuple._2}/${evictTuple._3}/${evictTuple._4} " +
                s"evictCreatedDelta=${evictBackHandles.size - baseEvict} heldTxn=${heldDnTxn.map(_.v)} " +
                s"prefetchIssue=$collisionPrefetchIssueCycle")
              assert(pass, "BOP concurrent collision checks failed")
            } else if (bopClosedLoop) {
              println(s"EVICTBACK_COUNTS created=${evictBackHandles.size} completed=${completedEvictBackHandles.size} " +
                s"pending=${pendingEvictHandleBySource.size} active=${activeEvictHandleByTshr.size}")
              // Independent stimulus-derived sets; measured issue/demand timing
              // is diagnostic only and must never supply an expected address.
              val expectedPrefetchedHitAddresses = expectedBOP.hits(288)
              val expectedPrefetchedHitTrains = expectedPrefetchedHitAddresses.size
              val expectedPrefetchAddressSet = expectedBOP.issues(288)
              val maxTrainPerLine = if (closedLoopTrainCounts.isEmpty) 0 else closedLoopTrainCounts.values.max
              val stageATrainPeriods = closedLoopStageATrainCycles.iterator
                .zip(closedLoopStageATrainCycles.iterator.drop(1))
                .map { case (previous, current) => current - previous }
                .toSeq
              val measuredT = stageATrainPeriods.groupBy(identity).maxByOption { case (_, periods) => periods.size }.map(_._1)
              val predictedOffset = measuredT.flatMap { period =>
                bopParams.offsetList.filter(offset => offset > 0 && offset * period > bopParams.delayQueueLatency + 1).minOption
              }
              val expectedTrainCounts = expectedBOP.trains(288).map(a => (a >> 6) -> 1).toMap
              val expectedEvicts = expectedBOP.validReplacements(expectedBOP.trains(288) ++ expectedPrefetchAddressSet, numSlices)
              val expectedHitsBySlice = expectedBOP.bySlice(expectedPrefetchedHitAddresses, numSlices)
              val actualHitsBySlice = expectedBOP.bySlice(closedLoopPrefetchedHitAddresses.toSet, numSlices)
              val expectedTrainBySlice = expectedBOP.bySlice(expectedBOP.trains(288), numSlices)
              val actualTrainBySlice = dut.observation.sliceTrainPulseCount.map(_.peek().litValue.toInt).toSeq
              val closedLoopPass = completed == closedLoopTotalRequests && issued == closedLoopTotalRequests &&
                closedLoopFullDrops == 0 && closedLoopTrainStalls == 0 &&
                closedLoopTrainEvents == 288 && closedLoopTrainCounts.toMap == expectedTrainCounts &&
                closedLoopPrefetchIssues == expectedPrefetchAddressSet.size &&
                closedLoopPrefetchIssueCycles.keySet.toSet == expectedPrefetchAddressSet &&
                closedLoopBopReqAddresses.toSet == expectedPrefetchAddressSet &&
                closedLoopQueueIssues.toMap == expectedPrefetchAddressSet.map(_ -> 1).toMap &&
                closedLoopInternalFills.toMap == expectedFills(expectedPrefetchAddressSet) &&
                closedLoopCrossPageDrops == expectedBOP.crossPage(288).size &&
                closedLoopCrossPageAddresses.toSet == expectedBOP.crossPage(288) &&
                closedLoopDisabledDrops == expectedBOP.disabled(288).size &&
                closedLoopDisabledAddresses.toSet == expectedBOP.disabled(288) &&
                maxTrainPerLine == 1 && measuredT.contains(expectedBOP.period) && predictedOffset.contains(expectedBOP.offset) &&
                closedLoopMeasuredOffset.contains(BigInt(expectedBOP.offset * 64)) &&
                closedLoopBopReqStalls == 0 && closedLoopBopQueueOverwrites == 0 &&
                closedLoopPrefetchedHitTrains == expectedPrefetchedHitTrains &&
                closedLoopPrefetchedHitAddresses.toSet == expectedPrefetchedHitAddresses &&
                actualHitsBySlice == expectedHitsBySlice && actualTrainBySlice == expectedTrainBySlice &&
                dnTxReqCount == 288 - expectedPrefetchedHitTrains && upTxDatCount == 2 * 288 && dnTxDatCount == 0 &&
                expectedEvicts == 0 && evictBackHandles.isEmpty && completedEvictBackHandles.isEmpty &&
                pendingEvictHandleBySource.isEmpty && activeEvictHandleByTshr.isEmpty &&
                dut.observation.sliceEvictBackFireCount.forall(_.peek().litValue == 0) &&
                smokePending == 0 && smokeActive == 0
              if (!closedLoopPass) println(s"BOP_CLOSED_LOOP_DIFFERENCE " +
                s"missingIssues=${(expectedPrefetchAddressSet -- closedLoopPrefetchIssueCycles.keySet).toSeq.sorted.map(hex)} " +
                s"extraIssues=${(closedLoopPrefetchIssueCycles.keySet.toSet -- expectedPrefetchAddressSet).toSeq.sorted.map(hex)} " +
                s"missingHits=${(expectedPrefetchedHitAddresses -- closedLoopPrefetchedHitAddresses).toSeq.sorted.map(hex)} " +
                s"extraHits=${(closedLoopPrefetchedHitAddresses.toSet -- expectedPrefetchedHitAddresses).toSeq.sorted.map(hex)} " +
                s"fillDifferences=${closedLoopInternalFills.filter { case (a, value) => !expectedFills(expectedPrefetchAddressSet).get(a).contains(value) }} " +
                s"missingFills=${expectedPrefetchAddressSet -- closedLoopInternalFills.keySet} " +
                s"actualTrainBySlice=$actualTrainBySlice actualHitBySlice=$actualHitsBySlice")
              println(s"BOP_CLOSED_LOOP_SETS train=${expectedBOP.trains(288).toSeq.sorted.map(hex).mkString(",")} " +
                s"prefetch=${expectedPrefetchAddressSet.toSeq.sorted.map(hex).mkString(",")} " +
                s"crossPage=${expectedBOP.crossPage(288).toSeq.sorted.map(hex).mkString(",")} " +
                s"disabled=${expectedBOP.disabled(288).toSeq.sorted.map(hex).mkString(",")}")
              println(s"BOP_CLOSED_LOOP_BASE_EXPECTED case=$selectedCase slices=$numSlices " +
                s"trainBySlice=$expectedTrainBySlice issueBySlice=${expectedBOP.bySlice(expectedPrefetchAddressSet, numSlices)} " +
                s"hitBySlice=$expectedHitsBySlice fills=${closedLoopInternalFills.size}/${expectedPrefetchAddressSet.size} " +
                s"fillState=$expectedFillState maxSetOccupancy=${expectedBOP.occupancy(expectedBOP.trains(288) ++ expectedPrefetchAddressSet, numSlices).values.max} " +
                s"evict=$expectedEvicts/0/0/0 pending=$smokePending active=$smokeActive")
              println(s"BOP_CLOSED_LOOP_EXPECTED prefetchAddressSet=${expectedPrefetchAddressSet.size} " +
                s"bopReqAddresses=${closedLoopBopReqAddresses.size} crossPageDrops=${closedLoopCrossPageAddresses.size} " +
                s"disabledDrops=${closedLoopDisabledAddresses.size} timelyPrefetchedHits=$expectedPrefetchedHitTrains " +
                s"timelyHitAddresses=${expectedPrefetchedHitAddresses.toSeq.sorted.map(a => s"0x${a.toString(16)}").mkString(",")}")
              println(s"BOP_CLOSED_LOOP_RESULT pass=$closedLoopPass cycle=$cycle completed=$completed " +
                s"stageARequests=$closedLoopStageARequests measuredM=$closedLoopMeasuredRequests " +
                s"trainEvents=$closedLoopTrainEvents uniqueTrainLines=${closedLoopTrainLines.size} " +
                s"maxTrainPerLine=$maxTrainPerLine prefetchIssues=$closedLoopPrefetchIssues " +
                s"crossPageDrops=$closedLoopCrossPageDrops disabledDrops=$closedLoopDisabledDrops " +
                s"fullDrops=$closedLoopFullDrops trainStalls=$closedLoopTrainStalls " +
                s"bopReqStalls=$closedLoopBopReqStalls bopQueueOverwrites=$closedLoopBopQueueOverwrites " +
                s"prefetchedHitTrains=$closedLoopPrefetchedHitTrains " +
                s"predictedOffset=${predictedOffset.map(v => s"+$v").getOrElse("none")} " +
                s"measuredOffset=${closedLoopMeasuredOffset.map(_ / 64).map(v => s"+$v").getOrElse("none")} " +
                s"measuredT=${measuredT.map(_.toString).getOrElse("none")}")
              assert(closedLoopPass, "BOP closed-loop checks failed")
            } else if (bopOverwriteQueue) {
              // Independent pre-run arithmetic: depth=2, no dequeue during block.
              // For N blocked enqueues, exactly max(0,N-2) oldest entries are
              // overwritten; the retained drain order is blockedEnqueues.takeRight(2).
              val expectedOverwrites = (overwriteBlockedEnqueues.size - 2).max(0)
              val expectedRetained = overwriteBlockedEnqueues.takeRight(2).toSeq
              val overwritePass = overwriteReadyViolations == 0 &&
                overwriteBlockedEnqueues.size == 27 && overwriteBlockedDequeues.isEmpty &&
                expectedRetained == Seq(BigInt("af00", 16), BigInt("af40", 16)) &&
                overwriteObservedOverwrites == expectedOverwrites &&
                expectedOverwrites == 25 && closedLoopBopQueueOverwrites == expectedOverwrites &&
                closedLoopTrainStalls == 0 && closedLoopBopReqStalls == 0 &&
                overwriteOrderMismatch.isEmpty && overwriteExpectedQueue.isEmpty &&
                overwriteAfterBlockDequeues.take(2).toSeq == expectedRetained
              println(s"BOP_OVERWRITE_QUEUE_EXPECTED depth=2 blockedEnqueues=${overwriteBlockedEnqueues.size} " +
                s"overwriteCount=$expectedOverwrites retainedAfterDrain=${expectedRetained.map(hex).mkString(",")}")
              println(s"BOP_OVERWRITE_QUEUE_RESULT pass=$overwritePass cycle=$cycle " +
                s"enqReadyViolations=$overwriteReadyViolations bopQueueOverwrites=$closedLoopBopQueueOverwrites " +
                s"modelOverwrites=$overwriteObservedOverwrites blockedDequeues=${overwriteBlockedDequeues.size} " +
                s"trainStalls=$closedLoopTrainStalls bopReqStalls=$closedLoopBopReqStalls " +
                s"firstDequeued=${overwriteAfterBlockDequeues.take(2).map(hex).mkString(",")} " +
                s"orderMismatch=${overwriteOrderMismatch.getOrElse("none")}")
              assert(overwritePass, "BOP overwrite-queue checks failed")
            } else if (policyProbe) {
              val g = grants()
              println(s"POLICY_RESULT_FINAL latency=$dnLatency maxOutstanding=$dnOutstanding creditReturnDelay=$dnCreditDelay requests=$requestCount cycle=$cycle " +
                s"issued=$issued completed=$completed upRXREQ_blocked=${dut.observation.upRXREQ.blocked.peek().litValue} " +
                s"upRXREQ_grant=${dut.observation.upRXREQ.grant.peek().litValue} upTXDAT_blocked=${dut.observation.upTXDAT.blocked.peek().litValue} " +
                s"dir_grant=${delta(g.dirRd, initialGrant.dirRd) + delta(g.dirWb, initialGrant.dirWb) + delta(g.dirReplRd, initialGrant.dirReplRd)} " +
                s"ds_grant=${delta(g.dsWb, initialGrant.dsWb) + delta(g.dsRd, initialGrant.dsRd) + delta(g.dsAheadRd, initialGrant.dsAheadRd)}")
            } else {
              printCaseReport("load-miss", cycle, issued, initialHost, initialGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = true)
              printLoadMissAttribution(cycle)
            }
            printTshr0Probe("SIM_DONE", cycle, upTxReqCount)
            simulationEndNs = System.nanoTime()
            println(s"COUNTS total dnTxReq=$dnTxReqCount upTxDat=$upTxDatCount completed=$completed " +
              s"dnTxDat=$dnTxDatCount dnTxRsp=$dnTxRspCount upTxReq=$upTxReqCount upTxSnp=$upTxSnpCount " +
              s"upTxReqRise=$upTxReqRiseCount sEvictRise=$sEvictRiseCount")
            // bop.io.train is the post-arbitration aggregate (L2Top.scala:138), so
            // one fire is one accepted training event. Expected from the demand
            // count: load-miss trains its single miss once, dirty-victim trains
            // each of its five ReadUnique once.
            println(s"TRAIN_FIRE count=${dut.observation.bopTrainFireCount.peek().litValue}")
            // Step 10, derived before running: issue-off drains the queues, so
            // only demands enter REQ. load-miss has one cold read: 1/2/0,
            // completed=1, TRAIN_FIRE=1, EvictBack=0/0/0/0. dirty-victim has
            // five cold ReadUnique lines in one 4-way set: TRAIN_FIRE=5;
            // the fifth replaces once (EvictBack=1/1/0/0), with replacement
            // deltas 2/2/2, WriteBackFull 0x1B and both full-BE matching beats.
            // Routing changes which slice owns the set, not these counts.
            if ((noPrefetchIssue || noBop) && !policyProbe &&
                (selectedCase == "load-miss" || selectedCase == "dirty-victim")) {
              val dirty = selectedCase == "dirty-victim"
              assert(dut.observation.bopTrainFireCount.peek().litValue == (if (dirty) 5 else 1))
              assert(completed == (if (dirty) 5 else 1))
              assert(evictBackHandles.size == (if (dirty) 1 else 0) &&
                completedEvictBackHandles.size == (if (dirty) 1 else 0) &&
                pendingEvictHandleBySource.isEmpty && activeEvictHandleByTshr.isEmpty)
              if (dirty) {
                assert(victimBeat0BE == BigInt("ffffffff", 16) && victimBeat2BE == BigInt("ffffffff", 16))
              } else {
                assert(dnTxReqCount == 1 && upTxDatCount == 2 && dnTxDatCount == 0)
              }
              println(s"BOP_ISSUE_OFF_BASELINE pass=true case=$selectedCase slices=$numSlices")
            }
            println(s"SLICE_EVICTBACK_FIRE counts=${dut.observation.sliceEvictBackFireCount.map(_.peek().litValue).mkString(",")}")
            println(s"SEVICT_COUNTS scope=per-tshr counts=${sEvictRiseByTshr.mkString(",")} total=${sEvictRiseByTshr.sum} " +
              "totalScope=all-tshrs rising-edges state=s_evict_up_evict")
            if (!policyProbe) {
              assert(evictBacksComplete,
                s"All EvictBack handles must complete: created=${evictBackHandles.size} completed=${completedEvictBackHandles.size} " +
                  s"pending=${pendingEvictHandleBySource.size} active=${activeEvictHandleByTshr.size}")
            }
            println(s"EVICTBACK_COUNTS created=${evictBackHandles.size} completed=${completedEvictBackHandles.size} " +
              s"pending=${pendingEvictHandleBySource.size} active=${activeEvictHandleByTshr.size}")
            println(f"TIMING case=$selectedCase elaboration_sec=${seconds(simulationStartNs - totalStartNs)}%.3f " +
              f"simulation_sec=${seconds(simulationEndNs - simulationStartNs)}%.3f total_sec=${seconds(simulationEndNs - totalStartNs)}%.3f")
            if (selectedCase == "dirty-victim") {
              println(s"KNOWN_FAIL_RESOLVED case=dirty-victim report=$dirtyVictimKnownFailReport")
            }
          } else {
            cycle += 1
          }
        }

        if (!finished && !failed) {
          failed = !policyProbe
          finished = policyProbe
          failReason = s"watchdog fired after $watchdogCycles cycles"
          printState("WATCHDOG", cycle, issued, completed)
          printTshr0Probe("WATCHDOG", cycle, upTxReqCount)
          if (policyProbe) {
            val g = grants()
            println(s"POLICY_RESULT latency=$dnLatency maxOutstanding=$dnOutstanding creditReturnDelay=$dnCreditDelay requests=$requestCount cycle=$cycle " +
              s"issued=$issued completed=$completed upRXREQ_blocked=${dut.observation.upRXREQ.blocked.peek().litValue} " +
              s"upRXREQ_grant=${dut.observation.upRXREQ.grant.peek().litValue} upTXDAT_grant=${dut.observation.upTXDAT.grant.peek().litValue} " +
              s"upTXDAT_blocked=${dut.observation.upTXDAT.blocked.peek().litValue} " +
              s"dir_grant=${delta(g.dirRd, initialGrant.dirRd) + delta(g.dirWb, initialGrant.dirWb) + delta(g.dirReplRd, initialGrant.dirReplRd)} " +
              s"ds_grant=${delta(g.dsWb, initialGrant.dsWb) + delta(g.dsRd, initialGrant.dsRd) + delta(g.dsAheadRd, initialGrant.dsAheadRd)}")
          } else if (selectedCase == "dirty-victim" && victimBaseSet) {
            printCaseReport("dirty-victim", cycle, issued, victimBaseHost, victimBaseGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = false)
            println(s"VICTIM_RESULT BE=0x${victimBE.toString(16)} dnTxDatDelta=${dnTxDatCount - victimBaseHost.dnTxDat} victimDnTxDat=$victimDnTxDat " +
              s"beat0=${if (victimDnTxDatBeat0) 1 else 0} beat2=${if (victimDnTxDatBeat2) 1 else 0} beat0BE=0x${victimBeat0BE.toString(16)} beat2BE=0x${victimBeat2BE.toString(16)} " +
              s"dsRdGrantDelta=${grants().dsRd - victimBaseGrant.dsRd} dsAheadRdGrantDelta=${grants().dsAheadRd - victimBaseGrant.dsAheadRd} " +
              s"dirRdState=$victimDirRdState dirRdDirty=$victimDirRdDirty dirRdHit=$victimDirRdHit " +
              s"replRdState=$victimReplState replRdDirty=$victimReplDirty replRdWay=$victimReplWay replPaddr=0x${victimReplPaddr.toString(16)}" +
              victimDataSummary)
          } else if (selectedCase == "snoop-data-passdirty") {
            printCaseReport("snoop-data-passdirty", cycle, issued, initialHost, initialGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = false)
            println(s"SNOOP_PD_RESULT sawReqDirtyWrite=${if (sawReqDirtyWrite) 1 else 0} completed=$completed upTxSnp=$upTxSnpCount")
          } else if (bopPrefetchSmoke) {
            println(s"BOP_PREFETCH_SMOKE_RESULT pass=false reason=watchdog case=$selectedCase slices=$numSlices " +
              s"prefetches=${smokePrefetches.size} prefetchDn=${smokePrefetches.map(_.dnRequests).sum} " +
              s"demandDn=$dnTxReqCount upTxDat=$upTxDatCount completed=$completed " +
              s"TRAIN_FIRE=${dut.observation.bopTrainFireCount.peek().litValue} " +
              s"crossPageDrops=$closedLoopCrossPageDrops disabledDrops=$closedLoopDisabledDrops " +
              s"pending=$smokePending active=$smokeActive")
            smokePrefetches.zipWithIndex.foreach { case (pf, k) =>
              println(s"BOP_SMOKE_INCOMPLETE k=$k details=$pf")
            }
          } else {
            printCaseReport("load-miss", cycle, issued, initialHost, initialGrant, dnTxReqCount, upTxDatCount, dnTxDatCount, completed, ok = false)
            printLoadMissAttribution(cycle)
          }
          simulationEndNs = System.nanoTime()
          println(s"COUNTS total dnTxReq=$dnTxReqCount upTxDat=$upTxDatCount completed=$completed " +
            s"dnTxDat=$dnTxDatCount dnTxRsp=$dnTxRspCount upTxReq=$upTxReqCount upTxSnp=$upTxSnpCount " +
            s"upTxReqRise=$upTxReqRiseCount sEvictRise=$sEvictRiseCount")
          println(s"SEVICT_COUNTS scope=per-tshr counts=${sEvictRiseByTshr.mkString(",")} total=${sEvictRiseByTshr.sum} " +
            "totalScope=all-tshrs rising-edges state=s_evict_up_evict")
          println(s"EVICTBACK_COUNTS created=${evictBackHandles.size} completed=${completedEvictBackHandles.size} " +
            s"pending=${pendingEvictHandleBySource.size} active=${activeEvictHandleByTshr.size}")
          println(f"TIMING case=$selectedCase elaboration_sec=${seconds(simulationStartNs - totalStartNs)}%.3f " +
            f"simulation_sec=${seconds(simulationEndNs - simulationStartNs)}%.3f total_sec=${seconds(simulationEndNs - totalStartNs)}%.3f")
        }

        assert(!failed || knownFailMatched, s"TestTop_L2Top simulation failed: $failReason")
      }
    }

    if (simulateMode) {
      runSimulation()
    } else {
      (new ChiselStage).execute(varArgs.toArray,
        ChiselGeneratorAnnotation(() => new TestTop_L2Top(l2cfg)(config)) +: TestTopFirtoolOptions())
    }
  }
}
