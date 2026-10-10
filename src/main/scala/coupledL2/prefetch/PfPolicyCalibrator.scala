package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{MemReqSource, XSPerfAccumulate}
import xscache.coupledL2._

object PfPolicy {
  val ScoreWidth = 5
  val ScoreMax = (1 << ScoreWidth) - 1
  // Sources without an engine-specific raw score use a source-level bin.
  // BOP/PBOP retain their raw-score bins.
  val SourceCount = 9
  val ScoreBins = 1 << ScoreWidth
  val BinCount = SourceCount * ScoreBins

  val Raw = 0
  val PreviousWindow = 1
  val HistoryLong = 2
  val HistoryShort = 3
  val HistoryShortLong = 4
  val HistoryMultiScale = 5
  val HistoryEventTriggered = 6
  val HistoryEvent128 = 7
  val HistoryEvent256 = 8
  val HistoryEvent512 = 9
  val HistoryEvent64Slow = 10
  val Adaptive = 11
  val PolicyCount = 12

  val Event512TerminalThreshold = 512
  val Event512DecayNumerator = 9
  val Event512DecayDenominator = 10

  def sourceIndex(pfSource: UInt): UInt = MuxLookup(pfSource, 0.U)(Seq(
    MemReqSource.Prefetch2L2Stream.id.U -> 0.U,
    MemReqSource.Prefetch2L2Stride.id.U -> 1.U,
    MemReqSource.Prefetch2L2Berti.id.U -> 2.U,
    MemReqSource.Prefetch2L2SMS.id.U -> 3.U,
    MemReqSource.Prefetch2L2NL.id.U -> 4.U,
    MemReqSource.Prefetch2L2BOP.id.U -> 5.U,
    MemReqSource.Prefetch2L2PBOP.id.U -> 6.U,
    MemReqSource.Prefetch2L2TP.id.U -> 7.U,
    MemReqSource.Prefetch2L2CDP.id.U -> 8.U
  ))

  def trackedSource(pfSource: UInt): Bool =
    pfSource === MemReqSource.Prefetch2L2Stream.id.U ||
      pfSource === MemReqSource.Prefetch2L2Stride.id.U ||
      pfSource === MemReqSource.Prefetch2L2Berti.id.U ||
      pfSource === MemReqSource.Prefetch2L2SMS.id.U ||
      pfSource === MemReqSource.Prefetch2L2NL.id.U ||
      pfSource === MemReqSource.Prefetch2L2BOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2PBOP.id.U ||
      pfSource === MemReqSource.Prefetch2L2TP.id.U ||
      pfSource === MemReqSource.Prefetch2L2CDP.id.U

  def event512FilterSource(pfSource: UInt): Bool =
    trackedSource(pfSource)

  def requestSourceOf(pfSource: UInt): UInt = MuxLookup(pfSource, 0.U)(Seq(
    PfSource.Stream.id.U -> MemReqSource.Prefetch2L2Stream.id.U,
    PfSource.Stride.id.U -> MemReqSource.Prefetch2L2Stride.id.U,
    PfSource.Berti.id.U -> MemReqSource.Prefetch2L2Berti.id.U,
    PfSource.SMS.id.U -> MemReqSource.Prefetch2L2SMS.id.U,
    PfSource.NL.id.U -> MemReqSource.Prefetch2L2NL.id.U,
    PfSource.BOP.id.U -> MemReqSource.Prefetch2L2BOP.id.U,
    PfSource.PBOP.id.U -> MemReqSource.Prefetch2L2PBOP.id.U,
    PfSource.TP.id.U -> MemReqSource.Prefetch2L2TP.id.U,
    PfSource.CDP.id.U -> MemReqSource.Prefetch2L2CDP.id.U
  ))

  def binIndex(pfSource: UInt, rawScore: UInt): UInt =
    Cat(sourceIndex(pfSource), rawScore)

  def minScoreOf(nocTier: UInt, minScorePacked: UInt): UInt =
    (minScorePacked >> (nocTier * ScoreWidth.U))(ScoreWidth - 1, 0)

  def event512Admit(
    pfSource: UInt,
    event512Score: UInt,
    event512Valid: Bool,
    nocTier: UInt,
    minScorePacked: UInt,
    enable: Bool
  ): Bool = {
    !enable || !event512FilterSource(pfSource) || !event512Valid ||
      event512Score >= minScoreOf(nocTier, minScorePacked)
  }
}

class PfPolicyLabel(implicit p: Parameters) extends PrefetchBundle {
  val rawScore = UInt(PfPolicy.ScoreWidth.W)
  val score = Vec(PfPolicy.PolicyCount, UInt(PfPolicy.ScoreWidth.W))
  val event512Valid = Bool()
}

class PfPolicyOutcome(implicit p: Parameters) extends PrefetchBundle {
  val valid = Bool()
  val pfSource = UInt(MemReqSource.reqSourceBits.W)
  val rawScore = UInt(PfPolicy.ScoreWidth.W)
  val useful = Bool()
}

class PfPolicyCalibration(implicit p: Parameters) extends PrefetchBundle {
  val pfSource = UInt(MemReqSource.reqSourceBits.W)
  val rawScore = UInt(PfPolicy.ScoreWidth.W)
  val predictedScore = UInt(PfPolicy.ScoreWidth.W)
  val actualScore = UInt(PfPolicy.ScoreWidth.W)
  val windowUseful = UInt(16.W)
  val windowTerminal = UInt(16.W)
}

class PfPolicyTrack extends Bundle {
  val historyUseful = UInt(16.W)
  val historyTerminal = UInt(16.W)
  val windowUseful = UInt(10.W)
  val windowTerminal = UInt(10.W)
  val previousScore = UInt(PfPolicy.ScoreWidth.W)
  val previousValid = Bool()
}

class PfPolicyCalibrator(labelPorts: Int, outcomePorts: Int)(implicit p: Parameters) extends PrefetchModule {
  require(labelPorts > 0)
  require(outcomePorts > 0)

  val io = IO(new Bundle {
    val labelSource = Input(Vec(labelPorts, UInt(MemReqSource.reqSourceBits.W)))
    val labelRawScore = Input(Vec(labelPorts, UInt(PfPolicy.ScoreWidth.W)))
    val label = Output(Vec(labelPorts, new PfPolicyLabel))
    val outcomes = Input(Vec(outcomePorts, new PfPolicyOutcome))
    val calibration = Output(Vec(outcomePorts, Valid(new PfPolicyCalibration)))
  })

  private def emptyTracks = VecInit(Seq.fill(PfPolicy.BinCount)(0.U.asTypeOf(new PfPolicyTrack)))

  val longTracks = RegInit(emptyTracks)
  val shortTracks = RegInit(emptyTracks)
  val eventTracks = RegInit(emptyTracks)
  val event128Tracks = RegInit(emptyTracks)
  val event256Tracks = RegInit(emptyTracks)
  val event512Tracks = RegInit(emptyTracks)
  val event64SlowTracks = RegInit(emptyTracks)
  val scale128Tracks = RegInit(emptyTracks)
  val scale256Tracks = RegInit(emptyTracks)
  val scale512Tracks = RegInit(emptyTracks)
  val scale1024Tracks = RegInit(emptyTracks)

  private def saturatingAdd(value: UInt, increment: UInt, width: Int): UInt = {
    val sum = value +& increment
    Mux(sum(width), Fill(width, 1.U(1.W)), sum(width - 1, 0))
  }

  private def scoreFromCounts(useful: UInt, terminal: UInt): UInt = {
    // Round the useful ratio to the five-bit score domain, then saturate before
    // narrowing.  Without the clamp, a 100% useful bin rounds to 32 and the
    // low five bits wrap it to zero.
    val safeTerminal = Mux(terminal === 0.U, 1.U, terminal)
    val numerator = (useful << PfPolicy.ScoreWidth) + (terminal >> 1)
    val estimated = numerator / safeTerminal
    val bounded = Mux(estimated > PfPolicy.ScoreMax.U, PfPolicy.ScoreMax.U, estimated)
    bounded(PfPolicy.ScoreWidth - 1, 0)
  }

  private def scoreOf(track: PfPolicyTrack, fallback: UInt): UInt = {
    val estimated = scoreFromCounts(track.historyUseful, track.historyTerminal)
    Mux(track.historyTerminal === 0.U, fallback, estimated)
  }

  for (port <- 0 until outcomePorts) {
    io.calibration(port).valid := false.B
    io.calibration(port).bits := 0.U.asTypeOf(new PfPolicyCalibration)
  }

  // Emit one record for each distinct Event512 bin that closes this cycle.
  // The predicted score is the score visible before this window was merged;
  // actualScore is computed from the just-completed window.
  for (port <- 0 until outcomePorts) {
    val outcome = io.outcomes(port)
    val outcomeBin = PfPolicy.binIndex(outcome.pfSource, outcome.rawScore)
    val matching = io.outcomes.map(other =>
      other.valid && PfPolicy.trackedSource(other.pfSource) &&
        PfPolicy.binIndex(other.pfSource, other.rawScore) === outcomeBin
    )
    val terminalAdd = PopCount(matching)
    val usefulAdd = PopCount(matching.zip(io.outcomes).map { case (hit, other) => hit && other.useful })
    val old = event512Tracks(outcomeBin)
    val windowTerminal = old.windowTerminal +& terminalAdd
    val windowUseful = old.windowUseful +& usefulAdd
    val duplicate = io.outcomes.take(port).map(previous =>
      previous.valid && PfPolicy.trackedSource(previous.pfSource) &&
        PfPolicy.binIndex(previous.pfSource, previous.rawScore) === outcomeBin
    ).foldLeft(false.B)(_ || _)
    val closes = outcome.valid && PfPolicy.trackedSource(outcome.pfSource) &&
      !duplicate && terminalAdd.orR &&
      windowTerminal >= PfPolicy.Event512TerminalThreshold.U
    when(closes) {
      io.calibration(port).valid := true.B
      io.calibration(port).bits.pfSource := outcome.pfSource
      io.calibration(port).bits.rawScore := outcome.rawScore
      io.calibration(port).bits.predictedScore := scoreOf(old, outcome.rawScore)
      io.calibration(port).bits.actualScore := scoreFromCounts(windowUseful, windowTerminal)
      io.calibration(port).bits.windowUseful := windowUseful
      io.calibration(port).bits.windowTerminal := windowTerminal
    }
  }

  private def updateTrack(tracks: Vec[PfPolicyTrack], threshold: Int, decayNumerator: Int, decayDenominator: Int): Unit = {
    for (bin <- 0 until PfPolicy.BinCount) {
      val matching = io.outcomes.map(outcome =>
        outcome.valid && PfPolicy.trackedSource(outcome.pfSource) && PfPolicy.binIndex(outcome.pfSource, outcome.rawScore) === bin.U
      )
      val terminalAdd = PopCount(matching)
      val usefulAdd = PopCount(matching.zip(io.outcomes).map { case (hit, outcome) => hit && outcome.useful })
      val old = tracks(bin)
      val next = WireInit(old)
      val windowTerminal = old.windowTerminal +& terminalAdd
      val windowUseful = old.windowUseful +& usefulAdd
      val update = terminalAdd.orR && windowTerminal >= threshold.U
      val decayedUseful = (old.historyUseful * decayNumerator.U) / decayDenominator.U
      val decayedTerminal = (old.historyTerminal * decayNumerator.U) / decayDenominator.U
      val previous = scoreFromCounts(windowUseful, windowTerminal)

      when(terminalAdd.orR) {
        when(update) {
          next.historyUseful := saturatingAdd(decayedUseful, windowUseful, 16)
          next.historyTerminal := saturatingAdd(decayedTerminal, windowTerminal, 16)
          next.windowUseful := 0.U
          next.windowTerminal := 0.U
          next.previousScore := previous
          next.previousValid := true.B
        }.otherwise {
          next.windowUseful := saturatingAdd(old.windowUseful, usefulAdd, 10)
          next.windowTerminal := saturatingAdd(old.windowTerminal, terminalAdd, 10)
        }
      }
      tracks(bin) := next
    }
  }

  // All policies are trained from the same frozen source/raw-score outcome.
  updateTrack(longTracks, 1024, 9, 10)
  updateTrack(shortTracks, 128, 7, 10)
  updateTrack(eventTracks, 64, 9, 10)
  updateTrack(event128Tracks, 128, 9, 10)
  updateTrack(event256Tracks, 256, 9, 10)
  updateTrack(event512Tracks, PfPolicy.Event512TerminalThreshold, PfPolicy.Event512DecayNumerator, PfPolicy.Event512DecayDenominator)
  updateTrack(event64SlowTracks, 64, 31, 32)
  updateTrack(scale128Tracks, 128, 7, 10)
  updateTrack(scale256Tracks, 256, 3, 4)
  updateTrack(scale512Tracks, 512, 41, 50)
  updateTrack(scale1024Tracks, 1024, 9, 10)

  for (port <- 0 until labelPorts) {
    val raw = io.labelRawScore(port)
    val index = PfPolicy.binIndex(io.labelSource(port), raw)
    val longScore = scoreOf(longTracks(index), raw)
    val shortScore = scoreOf(shortTracks(index), raw)
    val eventScore = scoreOf(eventTracks(index), raw)
    val event128Score = scoreOf(event128Tracks(index), raw)
    val event256Score = scoreOf(event256Tracks(index), raw)
    val event512Score = scoreOf(event512Tracks(index), raw)
    val event512Valid = event512Tracks(index).historyTerminal >= PfPolicy.Event512TerminalThreshold.U
    val event64SlowScore = scoreOf(event64SlowTracks(index), raw)
    val scaleScores = Seq(scale128Tracks, scale256Tracks, scale512Tracks, scale1024Tracks).map(tracks => scoreOf(tracks(index), raw))
    val shortReliability = Mux(shortTracks(index).historyTerminal >= 64.U, 4.U, shortTracks(index).historyTerminal >> 4)
    val shortLongScore = ((shortScore * shortReliability) + (longScore * (4.U - shortReliability))) >> 2
    val multiScaleScore = scaleScores.reduce(_ +& _)(6, 2)
    val adaptiveScore = (longScore +& event512Score +& event64SlowScore)(6, 2)

    io.label(port).rawScore := raw
    io.label(port).score(PfPolicy.Raw) := raw
    io.label(port).score(PfPolicy.PreviousWindow) := Mux(longTracks(index).previousValid, longTracks(index).previousScore, raw)
    io.label(port).score(PfPolicy.HistoryLong) := longScore
    io.label(port).score(PfPolicy.HistoryShort) := shortScore
    io.label(port).score(PfPolicy.HistoryShortLong) := shortLongScore
    io.label(port).score(PfPolicy.HistoryMultiScale) := multiScaleScore
    io.label(port).score(PfPolicy.HistoryEventTriggered) := eventScore
    io.label(port).score(PfPolicy.HistoryEvent128) := event128Score
    io.label(port).score(PfPolicy.HistoryEvent256) := event256Score
    io.label(port).score(PfPolicy.HistoryEvent512) := event512Score
    io.label(port).score(PfPolicy.HistoryEvent64Slow) := event64SlowScore
    io.label(port).score(PfPolicy.Adaptive) := adaptiveScore
    io.label(port).event512Valid := event512Valid
  }

  for (policy <- 0 until PfPolicy.PolicyCount) {
    XSPerfAccumulate(s"pf_policy_label_p$policy", PopCount(io.label.map(label => label.score(policy) =/= 0.U)))
  }
  XSPerfAccumulate("pf_policy_event512_outcome", PopCount(io.outcomes.map(outcome => outcome.valid && PfPolicy.trackedSource(outcome.pfSource))))
  XSPerfAccumulate("pf_policy_event512_calibration", PopCount(io.calibration.map(_.valid)))
}
