package xscache.coupledL2.prefetch

import chisel3._
import chisel3.simulator.ChiselSim
import org.chipsalliance.cde.config.{Config, Parameters}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, MemReqSource, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xscache.coupledL2.{L1Param, L2Param, L2ParamKey}
import xscache.common.BankBitsKey

import scala.collection.mutable

class CqfPcSourceValidHarness extends Module {
  val io = IO(new Bundle {
    val reqSource = Input(UInt(MemReqSource.reqSourceBits.W))
    val pcValid = Output(Bool())
  })

  io.pcValid := CqfParameters.pcSourceValid(io.reqSource)
}

object CompactQualityFeedbackTest extends App with ChiselSim {
  private val lineMask = (1L << CqfParameters.LineBits) - 1L

  private def signExtend(value: Long, width: Int): Long = {
    val mask = (1L << width) - 1L
    val truncated = value & mask
    if ((truncated & (1L << (width - 1))) != 0L) truncated | ~mask else truncated
  }

  private def mix64(value: Long): Long = {
    var x = value
    x ^= x >>> 30
    x *= 0xBF58476D1CE4E5B9L
    x ^= x >>> 27
    x *= 0x94D049BB133111EBL
    x ^ (x >>> 31)
  }

  private def sampleLarge(pc: Long, triggerLine: Long, salt: Long, mask: Long): Boolean = {
    val pcSignature = mix64((signExtend(pc, 50) >>> 1) ^ 0x9E3779B97F4A7C15L)
    val triggerBlockAddress = signExtend(triggerLine << 6, 48)
    (mix64(pcSignature ^ triggerBlockAddress ^ salt) & mask) == 0L
  }

  private def sampledLarge(pc: Long, triggerLine: Long): Boolean =
    sampleLarge(pc, triggerLine, 0x0B5EL, 0xfL)

  private def feedbackKey(line: Long): Long = {
    var x = line & lineMask
    x ^= x >>> 17
    x ^= (x << 13) & lineMask
    x ^= x >>> 6
    x ^= (x << 7) & lineMask
    (x ^ (x >>> 11)) & lineMask
  }

  private val config: Parameters = new Config((_, _, _) => {
    case L2ParamKey => L2Param(
      clientCaches = Seq(L1Param(
        vaddrBitsOpt = Some(44),
        pcBitOpt = Some(50)
      )),
      prefetch = Seq(BOPParameters(enableCQF = true)),
      enablePerf = false,
      enableRollingDB = false,
      enableMonitor = false,
      enableTLLog = false,
      enableCHILog = false,
      elaboratedTopDown = false
    )
    case BankBitsKey => 0
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  })

  simulate(new CqfPcSourceValidHarness) { dut =>
    Seq(
      MemReqSource.CPULoadData -> true,
      MemReqSource.CPUStoreData -> false,
      MemReqSource.CPUAtomicData -> false,
      MemReqSource.L1InstPrefetch -> false,
      MemReqSource.L1DataPrefetch -> false,
      MemReqSource.Prefetch2L2BOP -> false
    ).foreach { case (source, expected) =>
      dut.io.reqSource.poke(source.id.U)
      dut.io.pcValid.expect(expected.B)
    }
  }

  simulate(new CompactQualityFeedback()(config)) { dut =>
    dut.io.enable.poke(true.B)
    dut.io.demand.valid.poke(false.B)
    dut.io.demand.bits.poke(0.U)
    for (kind <- 0 until 2) {
      dut.io.candidate(kind).valid.poke(false.B)
      dut.io.candidate(kind).bits.poke(0.U.asTypeOf(dut.io.candidate(kind).bits))
      dut.io.decision(kind).ready.poke(true.B)
    }
    dut.reset.poke(true.B)
    dut.clock.step(2)
    dut.reset.poke(false.B)
    dut.clock.step(1)

    def pokeCandidate(kind: Int, pc: Long, pcValid: Boolean, trigger: Long, candidate: Long): Unit = {
      dut.io.candidate(kind).bits.pc.poke(BigInt(pc & Long.MaxValue).U)
      dut.io.candidate(kind).bits.pcValid.poke(pcValid.B)
      dut.io.candidate(kind).bits.kind.poke((kind == 0).B)
      dut.io.candidate(kind).bits.triggerLine.poke(BigInt(trigger & lineMask).U)
      dut.io.candidate(kind).bits.candidateLine.poke(BigInt(candidate & lineMask).U)
    }

    def awaitDecision(kind: Int): (Boolean, Boolean, Boolean) = {
      var cycles = 0
      while (!dut.io.decision(kind).valid.peek().litToBoolean && cycles < 8) {
        dut.clock.step(1)
        cycles += 1
      }
      assert(cycles < 8, s"CQF kind $kind did not return a decision")
      val result = (
        dut.io.decision(kind).bits.allow.peek().litToBoolean,
        dut.io.decision(kind).bits.sampled.peek().litToBoolean,
        dut.io.decision(kind).bits.feedbackInserted.peek().litToBoolean
      )
      dut.clock.step(1)
      result
    }

    def issue(kind: Int, pc: Long, pcValid: Boolean, trigger: Long, candidate: Long):
        (Boolean, Boolean, Boolean) = {
      pokeCandidate(kind, pc, pcValid, trigger, candidate)
      dut.io.candidate(kind).valid.poke(true.B)
      dut.io.candidate(kind).ready.expect(true.B)
      dut.clock.step(1)
      dut.io.candidate(kind).valid.poke(false.B)
      awaitDecision(kind)
    }

    // Both ports must capture in the same cycle. Large is serviced first,
    // but Small remains associated with its own response port.
    pokeCandidate(0, 0x1000L, pcValid = false, 0x10L, 0x20L)
    pokeCandidate(1, 0x2000L, pcValid = false, 0x30L, 0x40L)
    dut.io.candidate(0).valid.poke(true.B)
    dut.io.candidate(1).valid.poke(true.B)
    dut.io.candidate(0).ready.expect(true.B)
    dut.io.candidate(1).ready.expect(true.B)
    dut.clock.step(1)
    dut.io.candidate(0).valid.poke(false.B)
    dut.io.candidate(1).valid.poke(false.B)
    assert(awaitDecision(0) == ((true, false, false)))
    assert(awaitDecision(1) == ((true, false, false)))

    // Demand traffic can occupy the service cycle, but it must not turn CQF
    // capacity into upstream backpressure. The second pulse is visibly
    // rejected while the first Large ingress slot is full.
    dut.io.demand.valid.poke(true.B)
    dut.io.demand.bits.poke(0x55.U)
    pokeCandidate(0, 0x3000L, pcValid = false, 0x50L, 0x60L)
    dut.io.candidate(0).valid.poke(true.B)
    dut.io.candidate(0).ready.expect(true.B)
    dut.clock.step(1)
    pokeCandidate(0, 0x3002L, pcValid = false, 0x51L, 0x61L)
    dut.io.candidate(0).ready.expect(false.B)
    dut.io.demandAccept.expect(false.B)
    dut.clock.step(1)
    dut.io.candidate(0).valid.poke(false.B)
    dut.io.demand.valid.poke(false.B)
    assert(awaitDecision(0) == ((true, false, false)))

    // Disabling CQF prevents new ingress but cannot strand a previously
    // captured transaction.
    dut.io.demand.valid.poke(true.B)
    pokeCandidate(0, 0x3500L, pcValid = false, 0x52L, 0x62L)
    dut.io.candidate(0).valid.poke(true.B)
    dut.io.candidate(0).ready.expect(true.B)
    dut.clock.step(1)
    dut.io.candidate(0).valid.poke(false.B)
    dut.io.demand.valid.poke(false.B)
    dut.io.enable.poke(false.B)
    assert(awaitDecision(0) == ((true, false, false)))
    dut.io.enable.poke(true.B)

    val qualityPc = 0x4000L
    val sampledTrigger = (0L until 4096L).find(sampledLarge(qualityPc, _)).get
    val targetSet = (feedbackKey(0L) & 0x3fL).toInt
    val seenTags = mutable.HashSet.empty[Long]
    val sameSetLines = mutable.ArrayBuffer.empty[Long]
    var line = 0L
    while (sameSetLines.length < 4) {
      val key = feedbackKey(line)
      val tag = (key >>> 6) & 0x3fffL
      if ((key & 0x3fL).toInt == targetSet && seenTags.add(tag)) {
        sameSetLines += line
      }
      line += 1
    }

    // Fill all four Feedback ways. Resolving the fourth line specifically
    // guards against confusing way 3 with the miss sentinel.
    sameSetLines.foreach { candidateLine =>
      assert(issue(0, qualityPc, pcValid = true, sampledTrigger, candidateLine) ==
        ((true, true, true)))
    }
    dut.io.demand.bits.poke(sameSetLines.last.U)
    dut.io.demand.valid.poke(true.B)
    dut.io.demandAccept.expect(true.B)
    dut.clock.step(1)
    dut.io.demand.valid.poke(false.B)
    assert(issue(0, qualityPc, pcValid = true, sampledTrigger, sameSetLines.last) ==
      ((true, true, true)))

    // High-half canonical Sv48 values must be sign-extended before sampling,
    // matching GEM5's 64-bit Addr representation rather than zero-extension.
    val highPc = 0x3800000001000L
    val highTriggerBase = 0x20000000000L
    val highSampledTrigger = (highTriggerBase until (highTriggerBase + 4096L))
      .find(sampledLarge(highPc, _)).get
    assert(issue(0, highPc, pcValid = true, highSampledTrigger, 0x20000001000L) ==
      ((true, true, true)))

    // A duplicate sampled fingerprint is coalesced and does not allocate a
    // second Feedback entry.
    assert(issue(0, qualityPc, pcValid = true, sampledTrigger, sameSetLines.last) ==
      ((true, true, false)))

    // End-to-end state-machine exercise: collect 32 sampled candidates for a
    // fresh Quality key, expire them as unused with E6/T30, observe BLOCK,
    // then resolve enough probes useful to cross the reopen hysteresis.
    val blockPc = 0x8000L
    val observeTriggers = (0L until 100000L).filter(sampledLarge(blockPc, _)).take(32)
    val feedbackIds = mutable.HashSet.empty[Long]
    val blockCandidateLines = mutable.ArrayBuffer.empty[Long]
    var blockLine = 0x10000L
    while (blockCandidateLines.length < 32) {
      val key = feedbackKey(blockLine) & 0xfffffL
      if (feedbackIds.add(key)) {
        blockCandidateLines += blockLine
      }
      blockLine += 1
    }
    observeTriggers.zip(blockCandidateLines).foreach { case (trigger, candidateLine) =>
      assert(issue(0, blockPc, pcValid = true, trigger, candidateLine) ==
        ((true, true, true)))
    }

    dut.io.demand.valid.poke(true.B)
    dut.io.demand.bits.poke(0x3ffffffffffL.U)
    dut.clock.step(2176)
    dut.io.demand.valid.poke(false.B)

    val suppressedTrigger = (0L until 100000L)
      .find(trigger => !sampleLarge(blockPc, trigger, 0xB10CL, 0x3fL)).get
    assert(issue(0, blockPc, pcValid = true, suppressedTrigger, 0x20000L) ==
      ((false, false, false)))

    val strictProbeTriggers = (0L until 200000L)
      .filter(sampleLarge(blockPc, _, 0xB10CL, 0x3fL)).take(2)
    val borderlineProbeTriggers = (0L until 200000L)
      .filter(sampleLarge(blockPc, _, 0xB10CL, 0x7L)).drop(2).take(2)
    (strictProbeTriggers ++ borderlineProbeTriggers).zipWithIndex.foreach {
      case (trigger, index) =>
        val candidateLine = 0x30000L + index
        val probe = issue(0, blockPc, pcValid = true, trigger, candidateLine)
        assert(probe == ((true, true, true)), s"unexpected BLOCK probe decision: $probe")
        dut.io.demand.valid.poke(true.B)
        dut.io.demand.bits.poke(candidateLine.U)
        dut.clock.step(1)
        dut.io.demand.valid.poke(false.B)
    }

    val openUnsampledTrigger = (0L until 100000L)
      .find(trigger => !sampleLarge(blockPc, trigger, 0x5A6DL, 0xfL)).get
    assert(issue(0, blockPc, pcValid = true, openUnsampledTrigger, 0x40000L) ==
      ((true, false, false)))
  }
}
