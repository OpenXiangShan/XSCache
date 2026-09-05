package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{MemReqSource, XSPerfAccumulate, XSPerfMax}

object CqfParameters {
  // The fixed profile fingerprints canonical Sv48 byte addresses without the
  // six cache-block offset bits, independent of wider Sv48x4 transport wires.
  val LineBits = 42

  /**
    * Sources for which the current L1-to-L2 request path guarantees a real PC.
    *
    * CPUStoreData and CPUAtomicData can originate from MainPipe, which writes
    * zero into the PC field. L1DataPrefetch also reaches MissQueue through
    * MainPipe without a trigger PC. A zero-valued PC cannot itself indicate
    * invalidity because zero is a legal PC bit pattern.
    */
  def pcSourceValid(reqSource: UInt): Bool =
    reqSource === MemReqSource.CPULoadData.id.U
}

/** Raw BOP candidate presented to the shared compact quality controller. */
class CqfCandidate(implicit p: Parameters) extends PrefetchBundle {
  val pc = UInt(pcBitOpt.getOrElse(fullVAddrBits).W)
  val pcValid = Bool()
  val kind = Bool() // true: VBOP/Large, false: PBOP/Small
  val triggerLine = UInt(CqfParameters.LineBits.W)
  val candidateLine = UInt(CqfParameters.LineBits.W)
}

class CqfDecision extends Bundle {
  val allow = Bool()
  val sampled = Bool()
  val feedbackInserted = Bool()
}

/**
  * Compact Quality Feedback controller.
  *
  * The first RTL implementation intentionally uses register arrays.  The
  * logical organization is still fixed to 256 entries and four ways, so the
  * array can be replaced with an SRAM wrapper after event-level validation.
  * Demand input is never backpressured. Candidate ingress is independent per
  * BOP kind and retains one event per kind while the controller is servicing
  * the other kind.
  */
class CompactQualityFeedback(implicit p: Parameters) extends PrefetchModule {
  private val qualityEntries = 256
  private val qualityWays = 4
  private val qualitySets = qualityEntries / qualityWays
  private val feedbackEntries = 256
  private val feedbackWays = 4
  private val feedbackSets = feedbackEntries / feedbackWays
  private val qualityTagBits = 8
  private val feedbackTagBits = 14
  private val lineBits = CqfParameters.LineBits

  require(fullVAddrBits >= offsetBits,
    s"CQF requires a virtual address wider than the cache-line offset, got $fullVAddrBits")
  require(offsetBits == 6,
    s"CQF's Sv48 line fingerprint requires 64-byte blocks, got ${1 << offsetBits} bytes")

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val demand = Flipped(ValidIO(UInt(lineBits.W)))
    val demandAccept = Output(Bool())
    val candidate = Vec(2, Flipped(DecoupledIO(new CqfCandidate)))
    val decision = Vec(2, DecoupledIO(new CqfDecision))
  })

  private val kindLarge = 0
  private val kindSmall = 1
  private val stateObserve = 1.U(2.W)
  private val stateOpen = 2.U(2.W)
  private val stateBlock = 3.U(2.W)

  private val qValid = RegInit(VecInit(Seq.fill(qualityEntries)(false.B)))
  private val qKind = RegInit(VecInit(Seq.fill(qualityEntries)(false.B)))
  private val qTag = RegInit(VecInit(Seq.fill(qualityEntries)(0.U(qualityTagBits.W))))
  private val qState = RegInit(VecInit(Seq.fill(qualityEntries)(0.U(2.W))))
  private val qUseful = RegInit(VecInit(Seq.fill(qualityEntries)(0.U(7.W))))
  private val qUnused = RegInit(VecInit(Seq.fill(qualityEntries)(0.U(7.W))))
  private val qResolved = RegInit(VecInit(Seq.fill(qualityEntries)(0.U(6.W))))
  private val qPlru = RegInit(VecInit(Seq.fill(qualitySets)(0.U(3.W))))

  private val fValid = RegInit(VecInit(Seq.fill(feedbackEntries)(false.B)))
  private val fTag = RegInit(VecInit(Seq.fill(feedbackEntries)(0.U(feedbackTagBits.W))))
  private val fOwnerSet = RegInit(VecInit(Seq.fill(feedbackEntries)(0.U(6.W))))
  private val fOwnerTag = RegInit(VecInit(Seq.fill(feedbackEntries)(0.U(qualityTagBits.W))))
  private val fOwnerKind = RegInit(VecInit(Seq.fill(feedbackEntries)(false.B)))
  private val fIssueEpoch = RegInit(VecInit(Seq.fill(feedbackEntries)(0.U(6.W))))
  private val fVictim = RegInit(VecInit(Seq.fill(feedbackSets)(0.U(2.W))))

  private val ingressValid = RegInit(VecInit(Seq.fill(2)(false.B)))
  private val ingressBits = Reg(Vec(2, new CqfCandidate))
  private val responseValid = RegInit(VecInit(Seq.fill(2)(false.B)))
  private val responseBits = Reg(Vec(2, new CqfDecision))

  private val demandAge = RegInit(0.U(12.W))
  private val sweepPtr = RegInit(0.U(8.W))

  val candidateAccepted = Wire(Vec(2, Bool()))
  for (k <- 0 until 2) {
    io.candidate(k).ready := io.enable && !ingressValid(k)
    candidateAccepted(k) := io.candidate(k).fire
    io.decision(k).valid := responseValid(k)
    io.decision(k).bits := responseBits(k)
    when (io.decision(k).fire) {
      responseValid(k) := false.B
    }
    when (io.candidate(k).fire) {
      ingressBits(k) := io.candidate(k).bits
      ingressValid(k) := true.B
    }
  }

  // A pending candidate owns the single state-update service slot.  A
  // simultaneous demand is dropped for CQF only and never backpressures the
  // shared PrefetchTrain stream. This bounds candidate decision latency under
  // a continuous demand stream.
  private val candidatePending = ingressValid.asUInt.orR
  io.demandAccept := io.enable && io.demand.valid && !candidatePending

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
    require(value.getWidth <= 64,
      s"CQF hashes support values up to 64 bits, got ${value.getWidth}")
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
    // GEM5 samples with blockAddress(addr): byte-address bits are retained and
    // the six block-offset bits are zero. The RTL interface transports addr[47:6],
    // so reconstruct that block-aligned byte address before hashing.
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

  private def plruNext(state: UInt, way: UInt): UInt = {
    val next = Wire(UInt(3.W))
    next := state
    when (way === 0.U) {
      next := Cat(state(2), 1.U(1.W), 1.U(1.W))
    }.elsewhen (way === 1.U) {
      next := Cat(state(2), 0.U(1.W), 1.U(1.W))
    }.elsewhen (way === 2.U) {
      next := Cat(1.U(1.W), state(1), 0.U(1.W))
    }.otherwise {
      next := Cat(0.U(1.W), state(1), 0.U(1.W))
    }
    next
  }

  private def plruVictim(state: UInt): UInt = {
    Mux(!state(0), Mux(!state(1), 0.U, 1.U), Mux(!state(2), 2.U, 3.U))
  }

  private def selectQualityWay(set: UInt, tag: UInt, kind: Bool): UInt = {
    val hits = Wire(Vec(qualityWays, Bool()))
    val invalids = Wire(Vec(qualityWays, Bool()))
    for (w <- 0 until qualityWays) {
      val idx = Cat(set, w.U(2.W))
      hits(w) := qValid(idx) && qTag(idx) === tag && qKind(idx) === kind
      invalids(w) := !qValid(idx)
    }
    Mux(hits.asUInt.orR,
      OHToUInt(PriorityEncoderOH(hits.asUInt)),
      Mux(invalids.asUInt.orR,
        OHToUInt(PriorityEncoderOH(invalids.asUInt)),
        plruVictim(qPlru(set))))
  }

  private def stateAfterEvidence(state: UInt, useful: UInt, unused: UInt): UInt = {
    val samples = useful +& unused
    val blockLimit = useful * 10.U + 4.U
    val reopenLimit = useful * 10.U
    val shouldBlock = unused >= blockLimit
    val meetsReopen = reopenLimit >= 4.U && unused <= reopenLimit - 4.U
    Mux(state === stateObserve && samples < 32.U,
      stateObserve,
      Mux(state === stateBlock,
        Mux(meetsReopen, stateOpen, stateBlock),
        Mux(shouldBlock, stateBlock, stateOpen)))
  }

  private def applyOutcome(
    useful: UInt,
    unused: UInt,
    resolved: UInt,
    state: UInt,
    isUseful: Bool
  ): (UInt, UInt, UInt, UInt, UInt, Bool) = {
    val usefulNext = useful + Mux(isUseful, 1.U, 0.U)
    val unusedNext = unused + Mux(isUseful, 0.U, 1.U)
    val resolvedNext = resolved +& 1.U
    val stateBeforeDecay = stateAfterEvidence(state, usefulNext, unusedNext)
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

  private def findFeedbackWay(set: UInt, tag: UInt): UInt = {
    val hits = Wire(Vec(feedbackWays, Bool()))
    for (w <- 0 until feedbackWays) {
      val idx = Cat(set, w.U(2.W))
      hits(w) := fValid(idx) && fTag(idx) === tag
    }
    // Four ways need a fifth value as the miss sentinel.  Using way 3 as
    // the sentinel would make a real hit in the last way unresolvable.
    Mux(hits.asUInt.orR, OHToUInt(PriorityEncoderOH(hits.asUInt)), feedbackWays.U)
  }

  private val demandHash = feedbackHash(io.demand.bits)
  private val demandSet = demandHash(5, 0)
  private val demandTag = demandHash(19, 6)
  private val demandHitWay = findFeedbackWay(demandSet, demandTag)
  private val demandHitIdx = Cat(demandSet, demandHitWay(1, 0))
  private val nextDemandAge = demandAge + 1.U
  private val currentEpoch = demandAge(11, 6)
  private val nextDemandEpoch = nextDemandAge(11, 6)
  private val sweepIdx = sweepPtr
  private val sweepExpired = fValid(sweepIdx) &&
    ((nextDemandEpoch - fIssueEpoch(sweepIdx)) >= 30.U)
  private val usefulWinsSweep = io.demandAccept &&
    demandHitWay =/= feedbackWays.U && demandHitIdx === sweepIdx

  private val usefulFeedback = io.demandAccept &&
    demandHitWay =/= feedbackWays.U
  private val usefulOwnerSet = fOwnerSet(demandHitIdx)
  private val usefulOwnerTag = fOwnerTag(demandHitIdx)
  private val usefulOwnerKind = fOwnerKind(demandHitIdx)
  private val usefulOwnerWay = selectQualityWay(usefulOwnerSet, usefulOwnerTag, usefulOwnerKind)
  private val usefulOwnerIdx = Cat(usefulOwnerSet, usefulOwnerWay)
  private val usefulOwnerMatch = usefulFeedback && qValid(usefulOwnerIdx) &&
    qTag(usefulOwnerIdx) === usefulOwnerTag && qKind(usefulOwnerIdx) === usefulOwnerKind

  private val unusedFeedback = io.demandAccept && sweepExpired && !usefulWinsSweep
  private val unusedOwnerSet = fOwnerSet(sweepIdx)
  private val unusedOwnerTag = fOwnerTag(sweepIdx)
  private val unusedOwnerKind = fOwnerKind(sweepIdx)
  private val unusedOwnerWay = selectQualityWay(unusedOwnerSet, unusedOwnerTag, unusedOwnerKind)
  private val unusedOwnerIdx = Cat(unusedOwnerSet, unusedOwnerWay)
  private val unusedOwnerMatch = unusedFeedback && qValid(unusedOwnerIdx) &&
    qTag(unusedOwnerIdx) === unusedOwnerTag && qKind(unusedOwnerIdx) === unusedOwnerKind

  private val afterUseful = applyOutcome(
    qUseful(usefulOwnerIdx), qUnused(usefulOwnerIdx),
    qResolved(usefulOwnerIdx), qState(usefulOwnerIdx), true.B)
  private val outcomesShareOwner = usefulOwnerMatch && unusedOwnerMatch &&
    unusedOwnerIdx === usefulOwnerIdx
  private val unusedStateBefore = Mux(
    outcomesShareOwner, afterUseful._4, qState(unusedOwnerIdx))
  private val afterUnused = applyOutcome(
    Mux(outcomesShareOwner, afterUseful._1, qUseful(unusedOwnerIdx)),
    Mux(outcomesShareOwner, afterUseful._2, qUnused(unusedOwnerIdx)),
    Mux(outcomesShareOwner, afterUseful._3, qResolved(unusedOwnerIdx)),
    unusedStateBefore,
    false.B)

  private def outcomeTransitionCount(
    valid: Bool,
    oldState: UInt,
    outcome: (UInt, UInt, UInt, UInt, UInt, Bool)
  ): UInt = PopCount(VecInit(Seq(
    valid && oldState =/= outcome._5,
    valid && outcome._6 && outcome._5 =/= outcome._4
  )))

  private def outcomeTransitionCount(
    valid: Bool,
    oldState: UInt,
    outcome: (UInt, UInt, UInt, UInt, UInt, Bool),
    from: UInt,
    to: UInt
  ): UInt = PopCount(VecInit(Seq(
    valid && oldState === from && outcome._5 === to,
    valid && outcome._6 && outcome._5 === from && outcome._4 === to
  )))

  private def transitionCount(from: UInt, to: UInt): UInt =
    outcomeTransitionCount(usefulOwnerMatch, qState(usefulOwnerIdx), afterUseful, from, to) +&
      outcomeTransitionCount(unusedOwnerMatch, unusedStateBefore, afterUnused, from, to)

  private val observeToOpenCount = transitionCount(stateObserve, stateOpen)
  private val observeToBlockCount = transitionCount(stateObserve, stateBlock)
  private val openToBlockCount = transitionCount(stateOpen, stateBlock)
  private val blockToOpenCount = transitionCount(stateBlock, stateOpen)
  private val stateTransitionCount =
    outcomeTransitionCount(usefulOwnerMatch, qState(usefulOwnerIdx), afterUseful) +&
      outcomeTransitionCount(unusedOwnerMatch, unusedStateBefore, afterUnused)
  private val classifiedStateTransitionCount = observeToOpenCount +&
    observeToBlockCount +& openToBlockCount +& blockToOpenCount

  // Demand lookup is serviced before candidate ingress. This also ensures a
  // candidate derived from the current train cannot match itself.
  when (io.demandAccept) {
    demandAge := nextDemandAge
    sweepPtr := sweepPtr + 1.U

    when (usefulFeedback) {
      fValid(demandHitIdx) := false.B
    }

    when (unusedFeedback) {
      fValid(sweepIdx) := false.B
    }

    when (usefulOwnerMatch) {
      when (outcomesShareOwner) {
        qUseful(usefulOwnerIdx) := afterUnused._1
        qUnused(usefulOwnerIdx) := afterUnused._2
        qResolved(usefulOwnerIdx) := afterUnused._3
        qState(usefulOwnerIdx) := afterUnused._4
      }.otherwise {
        qUseful(usefulOwnerIdx) := afterUseful._1
        qUnused(usefulOwnerIdx) := afterUseful._2
        qResolved(usefulOwnerIdx) := afterUseful._3
        qState(usefulOwnerIdx) := afterUseful._4
      }
    }

    when (unusedOwnerMatch && !outcomesShareOwner) {
      qUseful(unusedOwnerIdx) := afterUnused._1
      qUnused(unusedOwnerIdx) := afterUnused._2
      qResolved(unusedOwnerIdx) := afterUnused._3
      qState(unusedOwnerIdx) := afterUnused._4
    }
  }

  val serviceKind = WireDefault(1.U(1.W))
  when (ingressValid(kindLarge) && !responseValid(kindLarge)) {
    serviceKind := 0.U
  }
  // Enable gates new ingress only. Once captured, a transaction must drain
  // even if software disables both BOPs before the decision is returned.
  val serviceValid = ingressValid(serviceKind) && !responseValid(serviceKind)
  val service = ingressBits(serviceKind)
  val serviceHash = qualityHash(service.pc, service.kind)
  val serviceSet = serviceHash(5, 0)
  val serviceTag = serviceHash(13, 6)
  val serviceWay = selectQualityWay(serviceSet, serviceTag, service.kind)
  val serviceIdx = Cat(serviceSet, serviceWay)
  val serviceOldValid = qValid(serviceIdx)
  val serviceOldHit = serviceOldValid && qTag(serviceIdx) === serviceTag && qKind(serviceIdx) === service.kind
  val serviceOldState = Mux(serviceOldHit, qState(serviceIdx), stateObserve)
  val serviceUseful = Mux(serviceOldHit, qUseful(serviceIdx), 0.U(7.W))
  val serviceUnused = Mux(serviceOldHit, qUnused(serviceIdx), 0.U(7.W))
  val serviceStrict = serviceUnused >= (serviceUseful * 20.U + 4.U)
  val sampleHash = samplingHash(service.pc, service.kind, service.triggerLine, "h0B5E".U)
  val observeSample = sampleHash(3, 0) === 0.U
  val openSample = samplingHash(service.pc, service.kind, service.triggerLine, "h5A6D".U)(3, 0) === 0.U
  val borderlineProbe = samplingHash(service.pc, service.kind, service.triggerLine, "hB10C".U)(2, 0) === 0.U
  val strictProbe = samplingHash(service.pc, service.kind, service.triggerLine, "hB10C".U)(5, 0) === 0.U
  val serviceSampled = Mux(serviceOldState === stateBlock,
    Mux(serviceStrict, strictProbe, borderlineProbe),
    Mux(serviceOldState === stateOpen, openSample, observeSample))
  val serviceAllowed = !serviceOldHit || serviceOldState =/= stateBlock || serviceSampled
  val serviceFbHash = feedbackHash(service.candidateLine)
  val serviceFbSet = serviceFbHash(5, 0)
  val serviceFbTag = serviceFbHash(19, 6)
  val serviceFbHitWay = findFeedbackWay(serviceFbSet, serviceFbTag)
  val serviceFbHit = serviceFbHitWay =/= feedbackWays.U
  val serviceFbInvalids = VecInit((0 until feedbackWays).map(w =>
    !fValid(Cat(serviceFbSet, w.U(2.W)))))
  val serviceFbHasInvalid = serviceFbInvalids.asUInt.orR
  val serviceFbVictim = Mux(
    serviceFbHasInvalid,
    OHToUInt(PriorityEncoderOH(serviceFbInvalids.asUInt)),
    fVictim(serviceFbSet))
  val serviceFbIdx = Cat(serviceFbSet, serviceFbVictim)

  when (serviceValid) {
    ingressValid(serviceKind) := false.B
    responseValid(serviceKind) := true.B
    responseBits(serviceKind).allow := Mux(service.pcValid, serviceAllowed, true.B)
    responseBits(serviceKind).sampled := service.pcValid && serviceSampled
    responseBits(serviceKind).feedbackInserted := false.B

    when (service.pcValid) {
      when (!serviceOldHit) {
        qValid(serviceIdx) := true.B
        qTag(serviceIdx) := serviceTag
        qKind(serviceIdx) := service.kind
        qState(serviceIdx) := stateObserve
        qUseful(serviceIdx) := 0.U
        qUnused(serviceIdx) := 0.U
        qResolved(serviceIdx) := 0.U
      }
      qPlru(serviceSet) := plruNext(qPlru(serviceSet), serviceWay)

      when (serviceSampled && !serviceFbHit) {
        fValid(serviceFbIdx) := true.B
        fTag(serviceFbIdx) := serviceFbTag
        fOwnerSet(serviceFbIdx) := serviceSet
        fOwnerTag(serviceFbIdx) := serviceTag
        fOwnerKind(serviceFbIdx) := service.kind
        fIssueEpoch(serviceFbIdx) := currentEpoch
        when (!serviceFbHasInvalid) {
          fVictim(serviceFbSet) := fVictim(serviceFbSet) + 1.U
        }
        responseBits(serviceKind).feedbackInserted := true.B
      }
    }
  }

  private val demandInput = io.enable && io.demand.valid
  private val demandDrop = demandInput && !io.demandAccept
  private val candidateInputCount = PopCount(VecInit(io.candidate.map(_.valid)))
  private val candidateAcceptCount = PopCount(candidateAccepted)
  private val candidateDrop = VecInit(io.candidate.zip(candidateAccepted).map {
    case (candidate, accepted) => candidate.valid && !accepted
  })
  private val candidateDropCount = PopCount(candidateDrop)
  private val dualCandidateInput = VecInit(io.candidate.map(_.valid)).asUInt.andR
  private val feedbackSelected = serviceValid && service.pcValid && serviceSampled
  private val feedbackInsert = feedbackSelected && !serviceFbHit
  private val feedbackCoalesce = feedbackSelected && serviceFbHit
  private val feedbackReplace = feedbackInsert && !serviceFbHasInvalid
  private val usefulOwnerMiss = usefulFeedback && !usefulOwnerMatch
  private val expiryOwnerMiss = unusedFeedback && !unusedOwnerMatch
  private val ownerMissCount = PopCount(VecInit(Seq(usefulOwnerMiss, expiryOwnerMiss)))
  private val feedbackUnknownCount = feedbackReplace.asUInt +& ownerMissCount
  private val candidateAllow = serviceValid && Mux(service.pcValid, serviceAllowed, true.B)
  private val candidateSuppress = serviceValid && service.pcValid && !serviceAllowed
  private val feedbackAllocate = feedbackInsert && serviceFbHasInvalid
  private val feedbackRetireCount = PopCount(VecInit(Seq(usefulFeedback, unusedFeedback)))
  private val feedbackOccupancy = RegInit(0.U(9.W))
  private val feedbackOccupancyNext = WireDefault(feedbackOccupancy)
  when (feedbackAllocate) {
    feedbackOccupancyNext := feedbackOccupancy + 1.U
  }.elsewhen (feedbackRetireCount =/= 0.U) {
    assert(feedbackOccupancy >= feedbackRetireCount,
      "CQF Feedback occupancy underflow")
    feedbackOccupancyNext := feedbackOccupancy - feedbackRetireCount
  }
  feedbackOccupancy := feedbackOccupancyNext

  XSPerfAccumulate("cqf_demand_input", demandInput)
  XSPerfAccumulate("cqf_demand_accept", io.demandAccept)
  // Compatibility alias retained for existing RTL result parsers.
  XSPerfAccumulate("cqf_demand", io.demandAccept)
  XSPerfAccumulate("cqf_demand_drop", demandDrop)
  XSPerfAccumulate("cqf_candidate_large_input", io.candidate(0).valid)
  XSPerfAccumulate("cqf_candidate_small_input", io.candidate(1).valid)
  XSPerfAccumulate("cqf_candidate_input", candidateInputCount)
  XSPerfAccumulate("cqf_candidate_large_accept", candidateAccepted(0))
  XSPerfAccumulate("cqf_candidate_small_accept", candidateAccepted(1))
  XSPerfAccumulate("cqf_candidate_accept", candidateAcceptCount)
  XSPerfAccumulate("cqf_candidate_large_drop", candidateDrop(0))
  XSPerfAccumulate("cqf_candidate_small_drop", candidateDrop(1))
  XSPerfAccumulate("cqf_candidate_drop", candidateDropCount)
  XSPerfAccumulate("cqf_candidate_dual_input", dualCandidateInput)
  XSPerfAccumulate("cqf_candidate_dual_capture", candidateAccepted.asUInt.andR)
  XSPerfAccumulate("cqf_candidate_dual_partial_drop",
    dualCandidateInput && candidateAccepted.asUInt.xorR)
  XSPerfAccumulate("cqf_candidate_dual_full_drop",
    dualCandidateInput && !candidateAccepted.asUInt.orR)
  XSPerfAccumulate("cqf_candidate_service", serviceValid)
  XSPerfAccumulate("cqf_candidate_allow", candidateAllow)
  XSPerfAccumulate("cqf_candidate_suppress", candidateSuppress)
  XSPerfAccumulate("cqf_quality_hit", serviceValid && service.pcValid && serviceOldHit)
  XSPerfAccumulate("cqf_quality_allocate", serviceValid && service.pcValid && !serviceOldHit)
  XSPerfAccumulate("cqf_quality_replace",
    serviceValid && service.pcValid && !serviceOldHit && serviceOldValid)
  XSPerfAccumulate("cqf_feedback_selected", feedbackSelected)
  XSPerfAccumulate("cqf_feedback_insert", feedbackInsert)
  XSPerfAccumulate("cqf_feedback_coalesce", feedbackCoalesce)
  XSPerfAccumulate("cqf_feedback_replace", feedbackReplace)
  // The fixed profile defines every full-set conflict as a replacement.
  XSPerfAccumulate("cqf_feedback_conflict", feedbackReplace)
  XSPerfAccumulate("cqf_feedback_useful", usefulOwnerMatch)
  XSPerfAccumulate("cqf_feedback_unused", unusedOwnerMatch)
  XSPerfAccumulate("cqf_feedback_expiry", unusedFeedback)
  XSPerfAccumulate("cqf_feedback_expiry_unused", unusedOwnerMatch)
  XSPerfAccumulate("cqf_feedback_expiry_owner_miss", expiryOwnerMiss)
  XSPerfAccumulate("cqf_feedback_retire", feedbackRetireCount)
  XSPerfAccumulate("cqf_feedback_owner_miss", ownerMissCount)
  XSPerfAccumulate("cqf_feedback_orphan_outcome", ownerMissCount)
  XSPerfAccumulate("cqf_feedback_unknown", feedbackUnknownCount)
  XSPerfAccumulate("cqf_state_transition", stateTransitionCount)
  XSPerfAccumulate("cqf_observe_to_open", observeToOpenCount)
  XSPerfAccumulate("cqf_observe_to_block", observeToBlockCount)
  XSPerfAccumulate("cqf_open_to_block", openToBlockCount)
  XSPerfAccumulate("cqf_block_to_open", blockToOpenCount)
  // The sum counter enables average occupancy calculation using elapsed cycles.
  XSPerfAccumulate("cqf_feedback_occupancy_sum", feedbackOccupancy)
  XSPerfMax("cqf_feedback_occupancy", feedbackOccupancyNext, true.B)

  when (!reset.asBool) {
    assert(demandInput === (io.demandAccept || demandDrop),
      "CQF demand input must be either accepted or dropped")
    for (k <- 0 until 2) {
      assert(io.candidate(k).valid === (candidateAccepted(k) || candidateDrop(k)),
        "CQF candidate input must be either accepted or dropped")
    }
    assert(candidateInputCount === candidateAcceptCount +& candidateDropCount,
      "CQF candidate totals do not match the per-port event partition")
    assert(serviceValid === (candidateAllow || candidateSuppress),
      "CQF serviced candidate must be either allowed or suppressed")
    assert(feedbackSelected === (feedbackInsert || feedbackCoalesce),
      "CQF selected feedback must be either inserted or coalesced")
    assert(unusedFeedback === (unusedOwnerMatch || expiryOwnerMiss),
      "CQF expiry must resolve as unused or owner-miss")
    assert(stateTransitionCount === classifiedStateTransitionCount,
      "CQF state transition is not part of the fixed-profile state machine")
    assert(feedbackOccupancy === PopCount(fValid),
      "CQF Feedback occupancy does not match valid entries")
  }

  val dualCapture = candidateAccepted.asUInt.andR
  val previousDualCapture = RegNext(dualCapture, false.B)
  when (previousDualCapture) {
    assert(ingressValid.asUInt.andR,
      "CQF must retain both Large and Small candidates after a dual capture")
  }
}
