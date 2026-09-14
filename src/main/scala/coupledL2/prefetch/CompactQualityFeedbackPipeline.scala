package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, XSPerfAccumulate, XSPerfMax}
import utility.sram.SRAMTemplate

private object CqfPipelineQualityPhase {
  val Width = 3
  val Waiting = 0.U(Width.W)
  val CandidateRead = 1.U(Width.W)
  val UsefulRead = 2.U(Width.W)
  val UnusedRead = 3.U(Width.W)
  val Done = 4.U(Width.W)
}

private object CqfPipelineFeedbackPhase {
  val Width = 3
  val Waiting = 0.U(Width.W)
  val CandidateRead = 1.U(Width.W)
  val DemandMatchRead = 2.U(Width.W)
  val DemandSweepRead = 3.U(Width.W)
  val DemandWrite1 = 4.U(Width.W)
  val Done = 5.U(Width.W)
}

private class CqfPipelineQualityEntry extends Bundle {
  val valid = Bool()
  val kind = Bool()
  val tag = UInt(8.W)
  val state = UInt(2.W)
  val useful = UInt(7.W)
  val unused = UInt(7.W)
  val resolved = UInt(6.W)
}

private class CqfPipelineFeedbackEntry extends Bundle {
  val valid = Bool()
  val tag = UInt(14.W)
  val ownerSet = UInt(6.W)
  val ownerTag = UInt(8.W)
  val ownerKind = Bool()
  val issueEpoch = UInt(6.W)
}

private class CqfPipelineContext(implicit p: Parameters) extends PrefetchBundle {
  val active = Bool()
  val sequence = UInt(32.W)
  val allocatedCycle = UInt(32.W)
  val isDemand = Bool()
  val port = UInt(1.W)
  val demandLine = UInt(CqfParameters.LineBits.W)
  val candidate = new CqfCandidate

  val qualityPhase = UInt(CqfPipelineQualityPhase.Width.W)
  val feedbackPhase = UInt(CqfPipelineFeedbackPhase.Width.W)
  val allow = Bool()
  val sampled = Bool()
  val feedbackInserted = Bool()

  val demandMatchSet = UInt(6.W)
  val demandMatchTag = UInt(14.W)
  val demandSweepIndex = UInt(8.W)
  val demandNextEpoch = UInt(6.W)

  val usefulPending = Bool()
  val usefulOwnerSet = UInt(6.W)
  val usefulOwnerTag = UInt(8.W)
  val usefulOwnerKind = Bool()
  val unusedPending = Bool()
  val unusedOwnerSet = UInt(6.W)
  val unusedOwnerTag = UInt(8.W)
  val unusedOwnerKind = Bool()
}

/**
  * Table-centric CQF implementation.
  *
  * Sixteen register contexts replace the serialized event FIFO/current-event
  * pair. Quality and Feedback each retain exactly one 1RW SRAM port and at
  * most one outstanding read. Different events may occupy the two table
  * pipelines concurrently. Within each table, the oldest logical event is a
  * conservative ordering barrier; this avoids stale owner updates and a
  * demand overtaking an older candidate whose Feedback sampling decision is
  * not known yet.
  */
class CompactQualityFeedbackPipeline(implicit p: Parameters) extends PrefetchModule {
  private val contextEntries = CqfParameters.PipelineContextEntries
  private val contextBits = log2Ceil(contextEntries)
  private val qualityWays = 4
  private val qualitySets = 64
  private val feedbackWays = 4
  private val feedbackSets = 64
  private val lineBits = CqfParameters.LineBits

  require(isPow2(contextEntries) && contextEntries >= 3)
  require(offsetBits == 6,
    s"CQF's Sv48 line fingerprint requires 64-byte blocks, got ${1 << offsetBits} bytes")

  val io = IO(new CompactQualityFeedbackIO)

  private val stateObserve = 1.U(2.W)
  private val stateOpen = 2.U(2.W)
  private val stateBlock = 3.U(2.W)

  private val qualityTable = Module(new SRAMTemplate(
    new CqfPipelineQualityEntry,
    set = qualitySets,
    way = qualityWays,
    singlePort = true,
    shouldReset = true,
    hasMbist = cacheParams.hasMbist,
    hasSramCtl = cacheParams.hasSramCtl
  ))
  private val feedbackTable = Module(new SRAMTemplate(
    new CqfPipelineFeedbackEntry,
    set = feedbackSets,
    way = feedbackWays,
    singlePort = true,
    shouldReset = true,
    hasMbist = cacheParams.hasMbist,
    hasSramCtl = cacheParams.hasSramCtl
  ))

  private val contexts = RegInit(VecInit(Seq.fill(contextEntries)(
    0.U.asTypeOf(new CqfPipelineContext))))
  private val qPlru = RegInit(VecInit(Seq.fill(qualitySets)(0.U(3.W))))
  private val fVictim = RegInit(VecInit(Seq.fill(feedbackSets)(0.U(2.W))))
  private val feedbackOccupancy = RegInit(0.U(9.W))
  private val demandAge = RegInit(0.U(12.W))
  private val sweepPtr = RegInit(0.U(8.W))
  private val nextSequence = RegInit(0.U(32.W))
  private val candidateRoundRobin = RegInit(0.U(1.W))
  private val cycleCounter = RegInit(0.U(64.W))
  cycleCounter := cycleCounter + 1.U

  private def lowMul(a: UInt, b: UInt): UInt = {
    val product = a * b
    product(63, 0)
  }

  private def mix64(value: UInt): UInt = {
    var x = value.pad(64)
    x = x ^ (x >> 30)
    x = lowMul(x, "hBF58476D1CE4E5B9".U(64.W))
    x = x ^ (x >> 27)
    x = lowMul(x, "h94D049BB133111EB".U(64.W))
    x ^ (x >> 31)
  }

  private def signExtendTo64(value: UInt): UInt = {
    require(value.getWidth <= 64)
    if (value.getWidth == 64) value
    else Cat(Fill(64 - value.getWidth, value(value.getWidth - 1)), value)
  }

  private def qualityHash(pc: UInt, kind: Bool): UInt = {
    val kindMix = Mux(kind,
      "h9E3779B97F4A7C15".U(64.W),
      "h3C6EF372FE94F82A".U(64.W))
    var x = signExtendTo64(pc) >> 1
    x = x ^ (x >> 7)
    x = x ^ (x >> 13)
    x = x ^ (x >> 27)
    x = x ^ kindMix
    x = x ^ (x >> 11)
    x ^ (x >> 23)
  }

  private def samplingHash(pc: UInt, kind: Bool, triggerLine: UInt, salt: UInt): UInt = {
    val pcSig = mix64((signExtendTo64(pc) >> 1) ^ Mux(kind,
      "h9E3779B97F4A7C15".U(64.W),
      "h3C6EF372FE94F82A".U(64.W)))
    val triggerBlockAddress = signExtendTo64(Cat(triggerLine, 0.U(offsetBits.W)))
    mix64(pcSig ^ triggerBlockAddress ^ salt.pad(64))
  }

  private def feedbackHash(line: UInt): UInt = {
    val mask = ((BigInt(1) << lineBits) - 1).U(lineBits.W)
    var x = line
    x = x ^ (x >> 17)
    x = x ^ ((x << 13) & mask)
    x = x ^ (x >> 6)
    x = x ^ ((x << 7) & mask)
    x ^ (x >> 11)
  }

  private def plruNext(state: UInt, way: UInt): UInt =
    MuxLookup(way, Cat(0.U(1.W), state(1), 0.U(1.W)))(Seq(
      0.U -> Cat(state(2), 1.U(1.W), 1.U(1.W)),
      1.U -> Cat(state(2), 0.U(1.W), 1.U(1.W)),
      2.U -> Cat(1.U(1.W), state(1), 0.U(1.W))
    ))

  private def plruVictim(state: UInt): UInt =
    Mux(!state(0), Mux(!state(1), 0.U, 1.U), Mux(!state(2), 2.U, 3.U))

  private def findQualityWay(
    row: Vec[CqfPipelineQualityEntry],
    tag: UInt,
    kind: Bool
  ): UInt = {
    val hits = VecInit(row.map(e => e.valid && e.tag === tag && e.kind === kind))
    Mux(hits.asUInt.orR, OHToUInt(PriorityEncoderOH(hits.asUInt)), qualityWays.U)
  }

  private def selectQualityWay(
    row: Vec[CqfPipelineQualityEntry],
    tag: UInt,
    kind: Bool,
    plru: UInt
  ): UInt = {
    val hitWay = findQualityWay(row, tag, kind)
    val invalid = VecInit(row.map(e => !e.valid))
    Mux(hitWay =/= qualityWays.U, hitWay,
      Mux(invalid.asUInt.orR, OHToUInt(PriorityEncoderOH(invalid.asUInt)), plruVictim(plru)))
  }

  private def findFeedbackWay(row: Vec[CqfPipelineFeedbackEntry], tag: UInt): UInt = {
    val hits = VecInit(row.map(e => e.valid && e.tag === tag))
    Mux(hits.asUInt.orR, OHToUInt(PriorityEncoderOH(hits.asUInt)), feedbackWays.U)
  }

  private def stateAfterEvidence(state: UInt, useful: UInt, unused: UInt): UInt = {
    val samples = useful +& unused
    val blockLimit = useful * 10.U + 4.U
    val reopenLimit = useful * 10.U
    val shouldBlock = unused >= blockLimit
    val meetsReopen = reopenLimit >= 4.U && unused <= reopenLimit - 4.U
    Mux(state === stateObserve && samples < 32.U, stateObserve,
      Mux(state === stateBlock,
        Mux(meetsReopen, stateOpen, stateBlock),
        Mux(shouldBlock, stateBlock, stateOpen)))
  }

  private def applyOutcome(entry: CqfPipelineQualityEntry, isUseful: Bool):
      (UInt, UInt, UInt, UInt, UInt, Bool) = {
    val usefulNext = entry.useful + Mux(isUseful, 1.U, 0.U)
    val unusedNext = entry.unused + Mux(isUseful, 0.U, 1.U)
    val resolvedNext = entry.resolved +& 1.U
    val stateBeforeDecay = stateAfterEvidence(entry.state, usefulNext, unusedNext)
    val decay = resolvedNext >= 64.U
    val usefulAfterDecay = (usefulNext >> 1).pad(7)
    val unusedAfterDecay = (unusedNext >> 1).pad(7)
    val finalUseful = Mux(decay, usefulAfterDecay, usefulNext)
    val finalUnused = Mux(decay, unusedAfterDecay, unusedNext)
    val finalResolved = Mux(decay, 0.U, resolvedNext(5, 0))
    val finalState = Mux(decay,
      stateAfterEvidence(stateBeforeDecay, usefulAfterDecay, unusedAfterDecay),
      stateBeforeDecay)
    (finalUseful, finalUnused, finalResolved, finalState, stateBeforeDecay, decay)
  }

  private def oldestOH(valid: Seq[Bool], sequence: Seq[UInt]): UInt = {
    require(valid.length == sequence.length && valid.nonEmpty)
    VecInit(valid.indices.map { i =>
      val olderExists = valid.indices.filter(_ != i).map { j =>
        // Modular comparison is safe because at most 16 sequences are live;
        // their distance is always far below half of the 32-bit number space.
        valid(j) && (sequence(j) - sequence(i))(31)
      }.reduce(_ || _)
      valid(i) && !olderExists
    }).asUInt
  }

  private val tablesReady = qualityTable.io.resetDone && feedbackTable.io.resetDone
  private val activeVec = VecInit(contexts.map(_.active))
  private val contextOccupancy = PopCount(activeVec)

  // --------------------------------------------------------------------------
  // Completion and per-port in-order decision commit
  // --------------------------------------------------------------------------
  private val demandComplete = VecInit(contexts.map(c =>
    c.active && c.isDemand &&
      c.qualityPhase === CqfPipelineQualityPhase.Done &&
      c.feedbackPhase === CqfPipelineFeedbackPhase.Done))
  private val candidateActive = (0 until 2).map { port =>
    VecInit(contexts.map(c =>
      c.active && !c.isDemand && c.port === port.U))
  }
  private val candidateComplete = (0 until 2).map { port =>
    VecInit(contexts.map(c =>
      c.active && !c.isDemand && c.port === port.U &&
        c.qualityPhase === CqfPipelineQualityPhase.Done &&
        c.feedbackPhase === CqfPipelineFeedbackPhase.Done))
  }
  private val decisionOH = (0 until 2).map { port =>
    // Select the oldest active candidate first. A completed younger context
    // must wait behind it because CqfRequestGate pairs decisions with a FIFO
    // of native BOP requests and carries no transaction ID.
    oldestOH(candidateActive(port), contexts.map(_.sequence))
  }
  private val decisionIndex = decisionOH.map(OHToUInt(_))

  for (port <- 0 until 2) {
    io.decision(port).valid := decisionOH(port).orR &&
      candidateComplete(port)(decisionIndex(port))
    io.decision(port).bits.allow := contexts(decisionIndex(port)).allow
    io.decision(port).bits.sampled := contexts(decisionIndex(port)).sampled
    io.decision(port).bits.feedbackInserted := contexts(decisionIndex(port)).feedbackInserted
  }

  private val decisionFire = VecInit(io.decision.map(_.fire))
  private val contextRetire = VecInit(contexts.indices.map { i =>
    val candidateRetire = (0 until 2).map { port =>
      decisionFire(port) && decisionOH(port)(i)
    }.reduce(_ || _)
    demandComplete(i) || candidateRetire
  })
  for (i <- 0 until contextEntries) {
    when (contextRetire(i)) {
      contexts(i).active := false.B
    }
  }

  // --------------------------------------------------------------------------
  // Three-way non-backpressuring admission: demand first, then RR candidates
  // --------------------------------------------------------------------------
  private val demandInput = io.enable && io.demand.valid
  private val candidateInputs = VecInit(io.candidate.map(c => io.enable && c.valid))
  private val bothCandidates = candidateInputs.asUInt.andR
  private val firstPort = Mux(bothCandidates, candidateRoundRobin,
    Mux(candidateInputs(0), 0.U, 1.U))
  private val secondPort = ~firstPort
  private val firstValid = Mux(firstPort === 0.U, candidateInputs(0), candidateInputs(1))
  private val secondValid = bothCandidates

  // A completed demand or handshaken decision can be replaced in the same
  // cycle. Allocation assignments appear after retirement assignments and
  // therefore own the reused entry's complete next state.
  private val initiallyFree = ((~activeVec.asUInt) | contextRetire.asUInt)(
    contextEntries - 1, 0)
  private val demandAllocOH = PriorityEncoderOH(initiallyFree)
  private val freeAfterDemand = initiallyFree & ~Mux(demandInput, demandAllocOH, 0.U)
  private val firstAllocOH = PriorityEncoderOH(freeAfterDemand)
  private val freeAfterFirst = freeAfterDemand & ~Mux(firstValid, firstAllocOH, 0.U)
  private val secondAllocOH = PriorityEncoderOH(freeAfterFirst)

  io.demandAccept := demandInput && demandAllocOH.orR
  private val firstReady = firstValid && firstAllocOH.orR
  private val secondReady = secondValid && secondAllocOH.orR
  io.candidate(0).ready := Mux(firstPort === 0.U, firstReady, secondReady)
  io.candidate(1).ready := Mux(firstPort === 0.U, secondReady, firstReady)

  private val candidateAccepted = VecInit(io.candidate.map(_.fire))
  private val firstAccepted = Mux(firstPort === 0.U, candidateAccepted(0), candidateAccepted(1))
  private val secondAccepted = Mux(firstPort === 0.U, candidateAccepted(1), candidateAccepted(0))
  private val admittedCount = io.demandAccept.asUInt +& firstAccepted.asUInt +& secondAccepted.asUInt

  private val demandContext = WireInit(0.U.asTypeOf(new CqfPipelineContext))
  demandContext.active := true.B
  demandContext.sequence := nextSequence
  demandContext.allocatedCycle := cycleCounter(31, 0)
  demandContext.isDemand := true.B
  demandContext.demandLine := io.demand.bits
  demandContext.qualityPhase := CqfPipelineQualityPhase.Waiting
  demandContext.feedbackPhase := CqfPipelineFeedbackPhase.DemandMatchRead
  demandContext.allow := true.B

  private val firstCandidateBits = Mux(firstPort === 0.U,
    io.candidate(0).bits, io.candidate(1).bits)
  private val secondCandidateBits = Mux(firstPort === 0.U,
    io.candidate(1).bits, io.candidate(0).bits)
  private val firstContext = WireInit(0.U.asTypeOf(new CqfPipelineContext))
  firstContext.active := true.B
  firstContext.sequence := nextSequence + io.demandAccept.asUInt
  firstContext.allocatedCycle := cycleCounter(31, 0)
  firstContext.isDemand := false.B
  firstContext.port := firstPort
  firstContext.candidate := firstCandidateBits
  firstContext.qualityPhase := CqfPipelineQualityPhase.CandidateRead
  firstContext.feedbackPhase := CqfPipelineFeedbackPhase.Waiting
  firstContext.allow := true.B

  private val secondContext = WireInit(0.U.asTypeOf(new CqfPipelineContext))
  secondContext.active := true.B
  secondContext.sequence := nextSequence + io.demandAccept.asUInt +& firstAccepted.asUInt
  secondContext.allocatedCycle := cycleCounter(31, 0)
  secondContext.isDemand := false.B
  secondContext.port := secondPort
  secondContext.candidate := secondCandidateBits
  secondContext.qualityPhase := CqfPipelineQualityPhase.CandidateRead
  secondContext.feedbackPhase := CqfPipelineFeedbackPhase.Waiting
  secondContext.allow := true.B

  when (io.demandAccept) {
    contexts(OHToUInt(demandAllocOH)) := demandContext
  }
  when (firstAccepted) {
    contexts(OHToUInt(firstAllocOH)) := firstContext
  }
  when (secondAccepted) {
    contexts(OHToUInt(secondAllocOH)) := secondContext
  }
  when (admittedCount =/= 0.U) {
    nextSequence := nextSequence + admittedCount
  }
  when (candidateAccepted.asUInt.xorR) {
    candidateRoundRobin := ~Mux(candidateAccepted(1), 1.U, 0.U)
  }

  // --------------------------------------------------------------------------
  // Quality 1RW pipeline
  // --------------------------------------------------------------------------
  private val qualityReadResponseValid = RegInit(false.B)
  private val qualityReadContext = Reg(UInt(contextBits.W))
  private val qualityReadPhase = Reg(UInt(CqfPipelineQualityPhase.Width.W))
  qualityReadResponseValid := false.B

  private val qualityWriteValid = WireDefault(false.B)
  private val qualityWriteSet = WireDefault(0.U(6.W))
  private val qualityWriteRow = Wire(Vec(qualityWays, new CqfPipelineQualityEntry))
  qualityWriteRow := 0.U.asTypeOf(qualityWriteRow)
  private val qualityHitPulse = WireDefault(false.B)
  private val qualityAllocatePulse = WireDefault(false.B)
  private val qualityReplacePulse = WireDefault(false.B)
  private val qualityStateTransitionCount = WireDefault(0.U(2.W))
  private val observeToOpenCount = WireDefault(0.U(2.W))
  private val observeToBlockCount = WireDefault(0.U(2.W))
  private val openToBlockCount = WireDefault(0.U(2.W))
  private val blockToOpenCount = WireDefault(0.U(2.W))
  private val feedbackUsefulPulse = WireDefault(false.B)
  private val feedbackUnusedPulse = WireDefault(false.B)
  private val feedbackOwnerMissPulse = WireDefault(false.B)
  private val qualityToFeedbackValid = WireDefault(false.B)
  private val qualityToFeedbackContext = WireDefault(0.U(contextBits.W))

  private val qResponseContext = contexts(qualityReadContext)
  private val qResponseRow = qualityTable.io.r.resp.data
  private val qResponseUsefulThenUnused = qualityReadResponseValid &&
    qualityReadPhase === CqfPipelineQualityPhase.UsefulRead &&
    qResponseContext.unusedPending

  when (qualityReadResponseValid) {
    when (qualityReadPhase === CqfPipelineQualityPhase.CandidateRead) {
      val hash = qualityHash(qResponseContext.candidate.pc, qResponseContext.candidate.kind)
      val set = hash(5, 0)
      val tag = hash(13, 6)
      val hitWay = findQualityWay(qResponseRow, tag, qResponseContext.candidate.kind)
      val hit = qResponseContext.candidate.pcValid && hitWay =/= qualityWays.U
      val selectedWay = selectQualityWay(
        qResponseRow, tag, qResponseContext.candidate.kind, qPlru(set))
      val oldEntry = qResponseRow(selectedWay(1, 0))
      val oldState = Mux(hit, oldEntry.state, stateObserve)
      val useful = Mux(hit, oldEntry.useful, 0.U)
      val unused = Mux(hit, oldEntry.unused, 0.U)
      val strict = unused >= useful * 20.U + 4.U
      val observeSample = samplingHash(
        qResponseContext.candidate.pc, qResponseContext.candidate.kind,
        qResponseContext.candidate.triggerLine, "h0B5E".U)(3, 0) === 0.U
      val openSample = samplingHash(
        qResponseContext.candidate.pc, qResponseContext.candidate.kind,
        qResponseContext.candidate.triggerLine, "h5A6D".U)(3, 0) === 0.U
      val borderlineProbe = samplingHash(
        qResponseContext.candidate.pc, qResponseContext.candidate.kind,
        qResponseContext.candidate.triggerLine, "hB10C".U)(2, 0) === 0.U
      val strictProbe = samplingHash(
        qResponseContext.candidate.pc, qResponseContext.candidate.kind,
        qResponseContext.candidate.triggerLine, "hB10C".U)(5, 0) === 0.U
      val sampled = qResponseContext.candidate.pcValid && Mux(oldState === stateBlock,
        Mux(strict, strictProbe, borderlineProbe),
        Mux(oldState === stateOpen, openSample, observeSample))
      val allowed = !qResponseContext.candidate.pcValid || !hit ||
        oldState =/= stateBlock || sampled

      contexts(qualityReadContext).allow := allowed
      contexts(qualityReadContext).sampled := sampled
      contexts(qualityReadContext).qualityPhase := CqfPipelineQualityPhase.Done
      contexts(qualityReadContext).feedbackPhase := Mux(sampled,
        CqfPipelineFeedbackPhase.CandidateRead,
        CqfPipelineFeedbackPhase.Done)
      qualityToFeedbackValid := sampled
      qualityToFeedbackContext := qualityReadContext

      when (qResponseContext.candidate.pcValid) {
        val updatedRow = WireInit(qResponseRow)
        qualityHitPulse := hit
        qualityAllocatePulse := !hit
        qualityReplacePulse := !hit && oldEntry.valid
        qPlru(set) := plruNext(qPlru(set), selectedWay)
        when (!hit) {
          updatedRow(selectedWay(1, 0)).valid := true.B
          updatedRow(selectedWay(1, 0)).kind := qResponseContext.candidate.kind
          updatedRow(selectedWay(1, 0)).tag := tag
          updatedRow(selectedWay(1, 0)).state := stateObserve
          updatedRow(selectedWay(1, 0)).useful := 0.U
          updatedRow(selectedWay(1, 0)).unused := 0.U
          updatedRow(selectedWay(1, 0)).resolved := 0.U
          qualityWriteValid := true.B
          qualityWriteSet := set
          qualityWriteRow := updatedRow
        }
      }
    }.otherwise {
      val usefulOutcome = qualityReadPhase === CqfPipelineQualityPhase.UsefulRead
      val ownerTag = Mux(usefulOutcome,
        qResponseContext.usefulOwnerTag, qResponseContext.unusedOwnerTag)
      val ownerKind = Mux(usefulOutcome,
        qResponseContext.usefulOwnerKind, qResponseContext.unusedOwnerKind)
      val hitWay = findQualityWay(qResponseRow, ownerTag, ownerKind)
      val ownerHit = hitWay =/= qualityWays.U
      val oldEntry = qResponseRow(hitWay(1, 0))
      val outcome = applyOutcome(oldEntry, usefulOutcome)
      val updatedRow = WireInit(qResponseRow)
      updatedRow(hitWay(1, 0)).useful := outcome._1
      updatedRow(hitWay(1, 0)).unused := outcome._2
      updatedRow(hitWay(1, 0)).resolved := outcome._3
      updatedRow(hitWay(1, 0)).state := outcome._4

      feedbackUsefulPulse := ownerHit && usefulOutcome
      feedbackUnusedPulse := ownerHit && !usefulOutcome
      feedbackOwnerMissPulse := !ownerHit
      when (ownerHit) {
        qualityWriteValid := true.B
        qualityWriteSet := Mux(usefulOutcome,
          qResponseContext.usefulOwnerSet, qResponseContext.unusedOwnerSet)
        qualityWriteRow := updatedRow
        qualityStateTransitionCount := PopCount(VecInit(Seq(
          oldEntry.state =/= outcome._5,
          outcome._6 && outcome._5 =/= outcome._4)))
        observeToOpenCount := PopCount(VecInit(Seq(
          oldEntry.state === stateObserve && outcome._5 === stateOpen,
          outcome._6 && outcome._5 === stateObserve && outcome._4 === stateOpen)))
        observeToBlockCount := PopCount(VecInit(Seq(
          oldEntry.state === stateObserve && outcome._5 === stateBlock,
          outcome._6 && outcome._5 === stateObserve && outcome._4 === stateBlock)))
        openToBlockCount := PopCount(VecInit(Seq(
          oldEntry.state === stateOpen && outcome._5 === stateBlock,
          outcome._6 && outcome._5 === stateOpen && outcome._4 === stateBlock)))
        blockToOpenCount := PopCount(VecInit(Seq(
          oldEntry.state === stateBlock && outcome._5 === stateOpen,
          outcome._6 && outcome._5 === stateBlock && outcome._4 === stateOpen)))
      }
      contexts(qualityReadContext).qualityPhase := Mux(
        usefulOutcome && qResponseContext.unusedPending,
        CqfPipelineQualityPhase.UnusedRead,
        CqfPipelineQualityPhase.Done)
    }
  }

  private val qualityPotential = contexts.indices.map { i =>
    contexts(i).active && contexts(i).qualityPhase =/= CqfPipelineQualityPhase.Done &&
      !(qualityReadResponseValid && qualityReadContext === i.U)
  }
  private val qualitySelectOH = oldestOH(qualityPotential, contexts.map(_.sequence))
  private val qualitySelectIndex = OHToUInt(qualitySelectOH)
  private val qualitySelectPhase = contexts(qualitySelectIndex).qualityPhase
  private val qualitySelectReady = qualitySelectOH.orR &&
    qualitySelectPhase =/= CqfPipelineQualityPhase.Waiting
  private val qualityReadIssueValid = tablesReady && !qualityWriteValid &&
    !qResponseUsefulThenUnused && qualitySelectReady
  private val qualityReadIssueSet = MuxLookup(qualitySelectPhase, 0.U)(Seq(
    CqfPipelineQualityPhase.CandidateRead ->
      qualityHash(contexts(qualitySelectIndex).candidate.pc,
        contexts(qualitySelectIndex).candidate.kind)(5, 0),
    CqfPipelineQualityPhase.UsefulRead -> contexts(qualitySelectIndex).usefulOwnerSet,
    CqfPipelineQualityPhase.UnusedRead -> contexts(qualitySelectIndex).unusedOwnerSet
  ))

  qualityTable.io.r.req.valid := qualityReadIssueValid
  qualityTable.io.r.req.bits.setIdx := qualityReadIssueSet
  qualityTable.io.w.req.valid := qualityWriteValid
  qualityTable.io.w.req.bits.setIdx := qualityWriteSet
  qualityTable.io.w.req.bits.data := qualityWriteRow
  qualityTable.io.w.req.bits.waymask.foreach(_ := Fill(qualityWays, 1.U(1.W)))
  private val qualityReadFire = qualityTable.io.r.req.fire
  private val qualityWriteFire = qualityTable.io.w.req.fire
  when (qualityReadFire) {
    qualityReadResponseValid := true.B
    qualityReadContext := qualitySelectIndex
    qualityReadPhase := qualitySelectPhase
  }

  // --------------------------------------------------------------------------
  // Feedback 1RW pipeline
  // --------------------------------------------------------------------------
  private val feedbackReadResponseValid = RegInit(false.B)
  private val feedbackReadContext = Reg(UInt(contextBits.W))
  private val feedbackReadPhase = Reg(UInt(CqfPipelineFeedbackPhase.Width.W))
  // Strict Feedback ordering guarantees that only the oldest demand can be
  // between match/sweep or waiting for write1. Keep these wide rows in the
  // table pipeline instead of replicating them in all 16 contexts.
  private val feedbackMatchScratch = Reg(Vec(feedbackWays, new CqfPipelineFeedbackEntry))
  private val feedbackWrite1Set = Reg(UInt(6.W))
  private val feedbackWrite1Row = Reg(Vec(feedbackWays, new CqfPipelineFeedbackEntry))
  feedbackReadResponseValid := false.B

  private val feedbackResponseWriteValid = WireDefault(false.B)
  private val feedbackResponseWriteSet = WireDefault(0.U(6.W))
  private val feedbackResponseWriteRow = Wire(Vec(feedbackWays, new CqfPipelineFeedbackEntry))
  feedbackResponseWriteRow := 0.U.asTypeOf(feedbackResponseWriteRow)
  private val feedbackSelectedPulse = WireDefault(false.B)
  private val feedbackInsertPulse = WireDefault(false.B)
  private val feedbackCoalescePulse = WireDefault(false.B)
  private val feedbackReplacePulse = WireDefault(false.B)
  private val feedbackRetireCount = WireDefault(0.U(2.W))
  private val feedbackExpiryPulse = WireDefault(false.B)
  private val feedbackOccupancyIncrement = WireDefault(false.B)

  private val fResponseContext = contexts(feedbackReadContext)
  private val fResponseRow = feedbackTable.io.r.resp.data
  private val feedbackResponseContinues = feedbackReadResponseValid &&
    feedbackReadPhase === CqfPipelineFeedbackPhase.DemandMatchRead &&
    fResponseContext.demandMatchSet =/= fResponseContext.demandSweepIndex(7, 2)

  when (feedbackReadResponseValid) {
    when (feedbackReadPhase === CqfPipelineFeedbackPhase.CandidateRead) {
      val qualityKey = qualityHash(
        fResponseContext.candidate.pc, fResponseContext.candidate.kind)
      val key = feedbackHash(fResponseContext.candidate.candidateLine)
      val set = key(5, 0)
      val tag = key(19, 6)
      val hitWay = findFeedbackWay(fResponseRow, tag)
      val hit = hitWay =/= feedbackWays.U
      val invalid = VecInit(fResponseRow.map(e => !e.valid))
      val hasInvalid = invalid.asUInt.orR
      val victim = Mux(hasInvalid,
        OHToUInt(PriorityEncoderOH(invalid.asUInt)), fVictim(set))
      val updatedRow = WireInit(fResponseRow)

      feedbackSelectedPulse := true.B
      feedbackCoalescePulse := hit
      contexts(feedbackReadContext).feedbackPhase := CqfPipelineFeedbackPhase.Done
      when (!hit) {
        updatedRow(victim).valid := true.B
        updatedRow(victim).tag := tag
        updatedRow(victim).ownerSet := qualityKey(5, 0)
        updatedRow(victim).ownerTag := qualityKey(13, 6)
        updatedRow(victim).ownerKind := fResponseContext.candidate.kind
        updatedRow(victim).issueEpoch := demandAge(11, 6)
        feedbackResponseWriteValid := true.B
        feedbackResponseWriteSet := set
        feedbackResponseWriteRow := updatedRow
        feedbackInsertPulse := true.B
        feedbackReplacePulse := !hasInvalid
        feedbackOccupancyIncrement := hasInvalid
        contexts(feedbackReadContext).feedbackInserted := true.B
        when (!hasInvalid) {
          fVictim(set) := fVictim(set) + 1.U
        }
      }
    }.otherwise {
      val matchResponse = feedbackReadPhase === CqfPipelineFeedbackPhase.DemandMatchRead
      val matchRow = Wire(Vec(feedbackWays, new CqfPipelineFeedbackEntry))
      matchRow := Mux(matchResponse, fResponseRow, feedbackMatchScratch)
      val sameSet = fResponseContext.demandMatchSet === fResponseContext.demandSweepIndex(7, 2)
      when (matchResponse && !sameSet) {
        feedbackMatchScratch := fResponseRow
        contexts(feedbackReadContext).feedbackPhase :=
          CqfPipelineFeedbackPhase.DemandSweepRead
      }.otherwise {
        val sweepRow = fResponseRow
        val hitWay = findFeedbackWay(matchRow, fResponseContext.demandMatchTag)
        val useful = hitWay =/= feedbackWays.U
        val usefulEntry = matchRow(hitWay(1, 0))
        val sweepWay = fResponseContext.demandSweepIndex(1, 0)
        val sweepEntry = sweepRow(sweepWay)
        val expired = sweepEntry.valid &&
          (fResponseContext.demandNextEpoch - sweepEntry.issueEpoch) >= 30.U
        val usefulWinsSweep = useful && sameSet && hitWay(1, 0) === sweepWay
        val unused = expired && !usefulWinsSweep
        val matchCleared = WireInit(matchRow)
        val sweepCleared = WireInit(sweepRow)
        val combinedCleared = WireInit(matchRow)
        when (useful) {
          matchCleared(hitWay(1, 0)).valid := false.B
          combinedCleared(hitWay(1, 0)).valid := false.B
        }
        when (unused) {
          sweepCleared(sweepWay).valid := false.B
          combinedCleared(sweepWay).valid := false.B
        }

        contexts(feedbackReadContext).usefulPending := useful
        contexts(feedbackReadContext).usefulOwnerSet := usefulEntry.ownerSet
        contexts(feedbackReadContext).usefulOwnerTag := usefulEntry.ownerTag
        contexts(feedbackReadContext).usefulOwnerKind := usefulEntry.ownerKind
        contexts(feedbackReadContext).unusedPending := unused
        contexts(feedbackReadContext).unusedOwnerSet := sweepEntry.ownerSet
        contexts(feedbackReadContext).unusedOwnerTag := sweepEntry.ownerTag
        contexts(feedbackReadContext).unusedOwnerKind := sweepEntry.ownerKind
        contexts(feedbackReadContext).qualityPhase := Mux(useful,
          CqfPipelineQualityPhase.UsefulRead,
          Mux(unused, CqfPipelineQualityPhase.UnusedRead,
            CqfPipelineQualityPhase.Done))

        feedbackRetireCount := PopCount(VecInit(Seq(useful, unused)))
        feedbackExpiryPulse := unused
        when (sameSet && (useful || unused)) {
          feedbackResponseWriteValid := true.B
          feedbackResponseWriteSet := fResponseContext.demandMatchSet
          feedbackResponseWriteRow := combinedCleared
          contexts(feedbackReadContext).feedbackPhase := CqfPipelineFeedbackPhase.Done
        }.elsewhen (!sameSet && useful) {
          feedbackResponseWriteValid := true.B
          feedbackResponseWriteSet := fResponseContext.demandMatchSet
          feedbackResponseWriteRow := matchCleared
          when (unused) {
            feedbackWrite1Set := fResponseContext.demandSweepIndex(7, 2)
            feedbackWrite1Row := sweepCleared
            contexts(feedbackReadContext).feedbackPhase :=
              CqfPipelineFeedbackPhase.DemandWrite1
          }.otherwise {
            contexts(feedbackReadContext).feedbackPhase := CqfPipelineFeedbackPhase.Done
          }
        }.elsewhen (!sameSet && unused) {
          feedbackResponseWriteValid := true.B
          feedbackResponseWriteSet := fResponseContext.demandSweepIndex(7, 2)
          feedbackResponseWriteRow := sweepCleared
          contexts(feedbackReadContext).feedbackPhase := CqfPipelineFeedbackPhase.Done
        }.otherwise {
          contexts(feedbackReadContext).feedbackPhase := CqfPipelineFeedbackPhase.Done
        }
      }
    }
  }

  // Build a post-response view for scheduling. A sampled candidate discovered
  // by the Quality response can launch its Feedback lookup in this same cycle.
  private val feedbackPotential = contexts.indices.map { i =>
    val existing = contexts(i).active &&
      contexts(i).feedbackPhase =/= CqfPipelineFeedbackPhase.Done &&
      !(feedbackReadResponseValid && feedbackReadContext === i.U)
    val qualityHandoff = qualityToFeedbackValid && qualityToFeedbackContext === i.U
    existing || qualityHandoff
  }
  private val feedbackPhaseView = contexts.indices.map { i =>
    Mux(qualityToFeedbackValid && qualityToFeedbackContext === i.U,
      CqfPipelineFeedbackPhase.CandidateRead, contexts(i).feedbackPhase)
  }
  private val feedbackSelectOH = oldestOH(feedbackPotential, contexts.map(_.sequence))
  private val feedbackSelectIndex = OHToUInt(feedbackSelectOH)
  private val feedbackSelectPhase = Mux1H(feedbackSelectOH, feedbackPhaseView)
  private val feedbackSelectReady = feedbackSelectOH.orR &&
    feedbackSelectPhase =/= CqfPipelineFeedbackPhase.Waiting
  private val feedbackSelectContext = contexts(feedbackSelectIndex)
  private val feedbackPendingWrite = feedbackSelectReady &&
    feedbackSelectPhase === CqfPipelineFeedbackPhase.DemandWrite1
  private val feedbackPendingWriteGranted = tablesReady &&
    !feedbackResponseWriteValid && !feedbackResponseContinues && feedbackPendingWrite
  private val feedbackWriteValid = feedbackResponseWriteValid || feedbackPendingWriteGranted
  private val feedbackWriteSet = Mux(feedbackResponseWriteValid,
    feedbackResponseWriteSet, feedbackWrite1Set)
  private val feedbackWriteRow = Mux(feedbackResponseWriteValid,
    feedbackResponseWriteRow, feedbackWrite1Row)

  private val feedbackReadIssueValid = tablesReady && !feedbackWriteValid &&
    !feedbackResponseContinues && feedbackSelectReady && !feedbackPendingWrite
  private val feedbackCandidateKey = feedbackHash(feedbackSelectContext.candidate.candidateLine)
  private val feedbackDemandKey = feedbackHash(feedbackSelectContext.demandLine)
  private val feedbackReadIssueSet = MuxLookup(feedbackSelectPhase, 0.U)(Seq(
    CqfPipelineFeedbackPhase.CandidateRead -> feedbackCandidateKey(5, 0),
    CqfPipelineFeedbackPhase.DemandMatchRead -> feedbackDemandKey(5, 0),
    CqfPipelineFeedbackPhase.DemandSweepRead -> feedbackSelectContext.demandSweepIndex(7, 2)
  ))

  feedbackTable.io.r.req.valid := feedbackReadIssueValid
  feedbackTable.io.r.req.bits.setIdx := feedbackReadIssueSet
  feedbackTable.io.w.req.valid := feedbackWriteValid
  feedbackTable.io.w.req.bits.setIdx := feedbackWriteSet
  feedbackTable.io.w.req.bits.data := feedbackWriteRow
  feedbackTable.io.w.req.bits.waymask.foreach(_ := Fill(feedbackWays, 1.U(1.W)))
  private val feedbackReadFire = feedbackTable.io.r.req.fire
  private val feedbackWriteFire = feedbackTable.io.w.req.fire

  when (feedbackReadFire) {
    feedbackReadResponseValid := true.B
    feedbackReadContext := feedbackSelectIndex
    feedbackReadPhase := feedbackSelectPhase
    when (feedbackSelectPhase === CqfPipelineFeedbackPhase.DemandMatchRead) {
      val nextAge = demandAge + 1.U
      demandAge := nextAge
      sweepPtr := sweepPtr + 1.U
      contexts(feedbackSelectIndex).demandMatchSet := feedbackDemandKey(5, 0)
      contexts(feedbackSelectIndex).demandMatchTag := feedbackDemandKey(19, 6)
      contexts(feedbackSelectIndex).demandSweepIndex := sweepPtr
      contexts(feedbackSelectIndex).demandNextEpoch := nextAge(11, 6)
    }
  }
  when (feedbackWriteFire && feedbackPendingWriteGranted) {
    contexts(feedbackSelectIndex).feedbackPhase := CqfPipelineFeedbackPhase.Done
  }

  when (feedbackOccupancyIncrement || feedbackRetireCount.orR) {
    feedbackOccupancy := feedbackOccupancy + feedbackOccupancyIncrement.asUInt - feedbackRetireCount
  }

  // --------------------------------------------------------------------------
  // Sparse replay trace, shared schema with the serialized implementation
  // --------------------------------------------------------------------------
  private val replayInputTable = ChiselDB.createTable(
    "L2BopCqfReplayInput", new CqfReplayInputEntry, basicDB = true)
  private val replayDecisionTable = ChiselDB.createTable(
    "L2BopCqfReplayDecision", new CqfReplayDecisionEntry, basicDB = true)
  private val replayReadyTable = ChiselDB.createTable(
    "L2BopCqfReplayReady", new CqfReplayReadyEntry, basicDB = true)
  private val replayControlTable = ChiselDB.createTable(
    "L2BopCqfReplayControl", new CqfReplayControlEntry, basicDB = true)

  private val replayDemandInput = WireInit(0.U.asTypeOf(new CqfReplayInputEntry))
  replayDemandInput.cycle := cycleCounter
  replayDemandInput.hartId := cacheParams.hartId.U
  replayDemandInput.eventType := CqfReplayEventType.Demand.U
  replayDemandInput.port := 0.U
  replayDemandInput.enable := io.enable
  replayDemandInput.valid := io.demand.valid
  replayDemandInput.fire := io.demand.valid && io.demandAccept
  replayDemandInput.accepted := io.demandAccept
  replayDemandInput.demandLine := io.demand.bits
  replayInputTable.log(replayDemandInput, en = io.demand.valid,
    site = "demand", clock, reset)

  for (port <- 0 until 2) {
    val replayCandidateInput = WireInit(0.U.asTypeOf(new CqfReplayInputEntry))
    replayCandidateInput.cycle := cycleCounter
    replayCandidateInput.hartId := cacheParams.hartId.U
    replayCandidateInput.eventType := CqfReplayEventType.Candidate.U
    replayCandidateInput.port := port.U
    replayCandidateInput.enable := io.enable
    replayCandidateInput.valid := io.candidate(port).valid
    replayCandidateInput.ready := io.candidate(port).ready
    replayCandidateInput.fire := io.candidate(port).fire
    replayCandidateInput.accepted := io.candidate(port).fire
    replayCandidateInput.candidate := io.candidate(port).bits
    replayInputTable.log(replayCandidateInput, en = io.candidate(port).valid,
      site = s"candidate$port", clock, reset)

    val replayDecision = WireInit(0.U.asTypeOf(new CqfReplayDecisionEntry))
    replayDecision.cycle := cycleCounter
    replayDecision.hartId := cacheParams.hartId.U
    replayDecision.port := port.U
    replayDecision.valid := io.decision(port).valid
    replayDecision.ready := io.decision(port).ready
    replayDecision.fire := io.decision(port).fire
    replayDecision.allow := io.decision(port).bits.allow
    replayDecision.sampled := io.decision(port).bits.sampled
    replayDecision.feedbackInserted := io.decision(port).bits.feedbackInserted
    replayDecisionTable.log(replayDecision, en = io.decision(port).fire,
      site = s"decision$port", clock, reset)

    val previousReady = RegNext(io.decision(port).ready, false.B)
    val replayReady = WireInit(0.U.asTypeOf(new CqfReplayReadyEntry))
    replayReady.cycle := cycleCounter
    replayReady.hartId := cacheParams.hartId.U
    replayReady.port := port.U
    replayReady.ready := io.decision(port).ready
    replayReadyTable.log(replayReady,
      en = !reset.asBool && io.decision(port).ready =/= previousReady,
      site = s"decision_ready$port", clock, reset)
  }

  private val previousEnable = RegNext(io.enable, false.B)
  private val replayControl = WireInit(0.U.asTypeOf(new CqfReplayControlEntry))
  replayControl.cycle := cycleCounter
  replayControl.hartId := cacheParams.hartId.U
  replayControl.enable := io.enable
  replayControlTable.log(replayControl,
    en = !reset.asBool && io.enable =/= previousEnable,
    site = "enable", clock, reset)

  // --------------------------------------------------------------------------
  // Alignment counters. Existing names keep result parsers compatible; new
  // context counters expose the actual capacity pressure of this engine.
  // --------------------------------------------------------------------------
  private val candidateInputCount = PopCount(candidateInputs)
  private val candidateAcceptCount = PopCount(candidateAccepted)
  private val candidateNotAdmitted = VecInit(io.candidate.zip(candidateAccepted).map {
    case (candidate, accepted) => io.enable && candidate.valid && !accepted
  })
  private val candidateNotAdmittedCount = PopCount(candidateNotAdmitted)
  private val candidateCompleteCount = PopCount(decisionFire)
  private val candidateAllowCount = PopCount((0 until 2).map(port =>
    decisionFire(port) && io.decision(port).bits.allow))
  private val candidateSuppressCount = candidateCompleteCount - candidateAllowCount
  private val candidateSampleCount = PopCount((0 until 2).map(port =>
    decisionFire(port) && io.decision(port).bits.sampled))
  private val candidateFeedbackInsertCount = PopCount((0 until 2).map(port =>
    decisionFire(port) && io.decision(port).bits.feedbackInserted))
  private val demandCompleteCount = PopCount(demandComplete)
  private val candidateLatencySum = (0 until 2).map { port =>
    Mux(decisionFire(port),
      cycleCounter(31, 0) - contexts(decisionIndex(port)).allocatedCycle, 0.U)
  }.reduce(_ +& _)
  private val demandLatencySum = contexts.indices.map { i =>
    Mux(demandComplete(i), cycleCounter(31, 0) - contexts(i).allocatedCycle, 0.U)
  }.reduce(_ +& _)
  private val maxCandidateLatency = Mux(decisionFire.asUInt.orR,
    Mux(decisionFire.asUInt.andR,
      Mux(cycleCounter(31, 0) - contexts(decisionIndex(0)).allocatedCycle >=
        cycleCounter(31, 0) - contexts(decisionIndex(1)).allocatedCycle,
        cycleCounter(31, 0) - contexts(decisionIndex(0)).allocatedCycle,
        cycleCounter(31, 0) - contexts(decisionIndex(1)).allocatedCycle),
      Mux(decisionFire(0),
        cycleCounter(31, 0) - contexts(decisionIndex(0)).allocatedCycle,
        cycleCounter(31, 0) - contexts(decisionIndex(1)).allocatedCycle)), 0.U)
  private val maxDemandLatency = contexts.indices.map { i =>
    Mux(demandComplete(i), cycleCounter(31, 0) - contexts(i).allocatedCycle, 0.U)
  }.reduce((a, b) => Mux(a >= b, a, b))
  private val qualitySramAccess = qualityReadFire || qualityWriteFire
  private val feedbackSramAccess = feedbackReadFire || feedbackWriteFire

  XSPerfAccumulate("cqf_demand_input", demandInput)
  XSPerfAccumulate("cqf_demand_enqueue", io.demandAccept)
  XSPerfAccumulate("cqf_demand_accept", io.demandAccept)
  XSPerfAccumulate("cqf_demand_processed", demandCompleteCount)
  XSPerfAccumulate("cqf_demand", io.demandAccept)
  XSPerfAccumulate("cqf_demand_drop", demandInput && !io.demandAccept)
  XSPerfAccumulate("cqf_demand_overflow_drop", demandInput && !io.demandAccept)
  XSPerfAccumulate("cqf_candidate_large_input", candidateInputs(0))
  XSPerfAccumulate("cqf_candidate_small_input", candidateInputs(1))
  XSPerfAccumulate("cqf_candidate_input", candidateInputCount)
  XSPerfAccumulate("cqf_candidate_eligible", candidateInputCount)
  XSPerfAccumulate("cqf_candidate_large_accept", candidateAccepted(0))
  XSPerfAccumulate("cqf_candidate_small_accept", candidateAccepted(1))
  XSPerfAccumulate("cqf_candidate_accept", candidateAcceptCount)
  XSPerfAccumulate("cqf_candidate_admit", candidateAcceptCount)
  XSPerfAccumulate("cqf_candidate_large_capacity_bypass", candidateNotAdmitted(0))
  XSPerfAccumulate("cqf_candidate_small_capacity_bypass", candidateNotAdmitted(1))
  XSPerfAccumulate("cqf_candidate_capacity_bypass", candidateNotAdmittedCount)
  XSPerfAccumulate("cqf_candidate_not_admitted", candidateNotAdmittedCount)
  XSPerfAccumulate("cqf_candidate_large_drop", false.B)
  XSPerfAccumulate("cqf_candidate_small_drop", false.B)
  XSPerfAccumulate("cqf_candidate_drop", false.B)
  XSPerfAccumulate("cqf_candidate_dual_input", candidateInputs.asUInt.andR)
  XSPerfAccumulate("cqf_candidate_dual_capture", candidateAccepted.asUInt.andR)
  XSPerfAccumulate("cqf_candidate_dual_partial_drop", false.B)
  XSPerfAccumulate("cqf_candidate_dual_full_drop", false.B)
  XSPerfAccumulate("cqf_candidate_dual_partial_bypass",
    candidateInputs.asUInt.andR && candidateAccepted.asUInt.xorR)
  XSPerfAccumulate("cqf_candidate_dual_full_bypass",
    candidateInputs.asUInt.andR && !candidateAccepted.asUInt.orR)
  XSPerfAccumulate("cqf_candidate_service", candidateCompleteCount)
  XSPerfAccumulate("cqf_candidate_allow", candidateAllowCount)
  XSPerfAccumulate("cqf_candidate_suppress", candidateSuppressCount)
  XSPerfAccumulate("cqf_candidate_policy_allow", candidateAllowCount)
  XSPerfAccumulate("cqf_candidate_policy_suppress", candidateSuppressCount)
  XSPerfAccumulate("cqf_candidate_sampled", candidateSampleCount)
  XSPerfAccumulate("cqf_quality_hit", qualityHitPulse)
  XSPerfAccumulate("cqf_quality_allocate", qualityAllocatePulse)
  XSPerfAccumulate("cqf_quality_replace", qualityReplacePulse)
  XSPerfAccumulate("cqf_feedback_selected", feedbackSelectedPulse)
  XSPerfAccumulate("cqf_feedback_insert", feedbackInsertPulse)
  XSPerfAccumulate("cqf_feedback_inserted_decision", candidateFeedbackInsertCount)
  XSPerfAccumulate("cqf_feedback_coalesce", feedbackCoalescePulse)
  XSPerfAccumulate("cqf_feedback_replace", feedbackReplacePulse)
  XSPerfAccumulate("cqf_feedback_conflict", feedbackReplacePulse)
  XSPerfAccumulate("cqf_feedback_useful", feedbackUsefulPulse)
  XSPerfAccumulate("cqf_feedback_unused", feedbackUnusedPulse)
  XSPerfAccumulate("cqf_feedback_expiry", feedbackExpiryPulse)
  XSPerfAccumulate("cqf_feedback_expiry_unused", feedbackUnusedPulse)
  XSPerfAccumulate("cqf_feedback_expiry_owner_miss",
    feedbackOwnerMissPulse && qualityReadPhase === CqfPipelineQualityPhase.UnusedRead)
  XSPerfAccumulate("cqf_feedback_retire", feedbackRetireCount)
  XSPerfAccumulate("cqf_feedback_owner_miss", feedbackOwnerMissPulse)
  XSPerfAccumulate("cqf_feedback_orphan_outcome", feedbackOwnerMissPulse)
  XSPerfAccumulate("cqf_feedback_unknown", feedbackReplacePulse.asUInt +& feedbackOwnerMissPulse.asUInt)
  XSPerfAccumulate("cqf_state_transition", qualityStateTransitionCount)
  XSPerfAccumulate("cqf_observe_to_open", observeToOpenCount)
  XSPerfAccumulate("cqf_observe_to_block", observeToBlockCount)
  XSPerfAccumulate("cqf_open_to_block", openToBlockCount)
  XSPerfAccumulate("cqf_block_to_open", blockToOpenCount)
  XSPerfAccumulate("cqf_feedback_occupancy_sum", feedbackOccupancy)
  XSPerfMax("cqf_feedback_occupancy", feedbackOccupancy, true.B)
  XSPerfAccumulate("cqf_engine_busy_cycles", contextOccupancy =/= 0.U)
  XSPerfAccumulate("cqf_event_fifo_occupancy_sum", contextOccupancy)
  XSPerfMax("cqf_event_fifo_occupancy", contextOccupancy, true.B)
  XSPerfMax("cqf_event_fifo_occupancy_max", contextOccupancy, true.B)
  XSPerfAccumulate("cqf_context_occupancy_sum", contextOccupancy)
  XSPerfMax("cqf_context_occupancy", contextOccupancy, true.B)
  XSPerfMax("cqf_context_occupancy_max", contextOccupancy, true.B)
  XSPerfAccumulate("cqf_context_full_cycles", contextOccupancy === contextEntries.U)
  XSPerfAccumulate("cqf_quality_sram_read", qualityReadFire)
  XSPerfAccumulate("cqf_quality_sram_write", qualityWriteFire)
  XSPerfAccumulate("cqf_feedback_sram_read", feedbackReadFire)
  XSPerfAccumulate("cqf_feedback_sram_write", feedbackWriteFire)
  XSPerfAccumulate("cqf_dual_sram_access", qualitySramAccess && feedbackSramAccess)
  XSPerfAccumulate("cqf_candidate_latency_sum", candidateLatencySum)
  XSPerfMax("cqf_candidate_latency", maxCandidateLatency, decisionFire.asUInt.orR)
  XSPerfMax("cqf_candidate_latency_max", maxCandidateLatency, decisionFire.asUInt.orR)
  XSPerfAccumulate("cqf_demand_latency_sum", demandLatencySum)
  XSPerfMax("cqf_demand_latency", maxDemandLatency, demandCompleteCount.orR)
  XSPerfMax("cqf_demand_latency_max", maxDemandLatency, demandCompleteCount.orR)

  when (!reset.asBool) {
    assert(demandInput === (io.demandAccept || (demandInput && !io.demandAccept)),
      "CQF pipeline demand must be accepted or explicitly dropped")
    for (port <- 0 until 2) {
      assert(candidateInputs(port) ===
        (candidateAccepted(port) || candidateNotAdmitted(port)),
        "CQF pipeline candidate must be admitted or fail open")
    }
    assert(candidateInputCount === candidateAcceptCount +& candidateNotAdmittedCount,
      "CQF pipeline candidate admission partition mismatch")
    assert(!(qualityReadFire && qualityWriteFire),
      "CQF pipeline Quality 1RW SRAM accessed twice")
    assert(!(feedbackReadFire && feedbackWriteFire),
      "CQF pipeline Feedback 1RW SRAM accessed twice")
    assert(!qualityWriteValid || qualityWriteFire,
      "CQF pipeline Quality write unexpectedly backpressured")
    assert(!feedbackWriteValid || feedbackWriteFire,
      "CQF pipeline Feedback write unexpectedly backpressured")
    assert(feedbackRetireCount <= feedbackOccupancy +& feedbackOccupancyIncrement.asUInt,
      "CQF pipeline Feedback occupancy underflow")
    assert(feedbackOccupancy <= (feedbackSets * feedbackWays).U,
      "CQF pipeline Feedback occupancy overflow")
    assert(contextOccupancy <= contextEntries.U,
      "CQF pipeline context occupancy overflow")
  }
}

/** Selectable wrapper used by XiangShan and the standalone replay top. */
class CompactQualityFeedback(
  pipelined: Boolean = CqfParameters.PipelinedEngine
)(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new CompactQualityFeedbackIO)

  if (pipelined) {
    val impl = Module(new CompactQualityFeedbackPipeline)
    impl.io.enable := io.enable
    impl.io.demand <> io.demand
    io.demandAccept := impl.io.demandAccept
    for (port <- 0 until 2) {
      impl.io.candidate(port) <> io.candidate(port)
      io.decision(port) <> impl.io.decision(port)
    }
  } else {
    val impl = Module(new CompactQualityFeedbackSerial)
    impl.io.enable := io.enable
    impl.io.demand <> io.demand
    io.demandAccept := impl.io.demandAccept
    for (port <- 0 until 2) {
      impl.io.candidate(port) <> io.candidate(port)
      io.decision(port) <> impl.io.decision(port)
    }
  }
}
