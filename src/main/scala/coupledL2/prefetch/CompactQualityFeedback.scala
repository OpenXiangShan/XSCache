package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, MemReqSource, XSPerfAccumulate, XSPerfMax}
import utility.sram.SRAMTemplate

object CqfParameters {
  // The fixed profile fingerprints canonical Sv48 byte addresses without the
  // six cache-block offset bits, independent of wider Sv48x4 transport wires.
  val LineBits = 42
  val EventQueueEntries = 8
  val GatePendingEntries = 8
  val GateOutputEntries = 4
  // Keep the serialized engine available for trace A/B and regression tests.
  // Normal XiangShan integration uses the table-centric pipeline.
  val PipelinedEngine = true
  val PipelineContextEntries = 16

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

  def lineFromByteAddress(byteAddress: UInt, blockOffsetBits: Int): UInt = {
    require(byteAddress.getWidth > blockOffsetBits)
    byteAddress(byteAddress.getWidth - 1, blockOffsetBits)
      .pad(LineBits)(LineBits - 1, 0)
  }

  def addLineOffset(line: UInt, offset: SInt): UInt =
    (line.asSInt + offset).asUInt.pad(LineBits)(LineBits - 1, 0)

  def subtractLineOffset(line: UInt, offset: SInt): UInt =
    addLineOffset(line, -offset)
}

/** Raw BOP candidate presented to the shared compact quality controller. */
class CqfCandidate(implicit p: Parameters) extends PrefetchBundle {
  val pc = UInt(pcBitOpt.getOrElse(fullVAddrBits).W)
  val pcValid = Bool()
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
  val kind = Bool() // true: VBOP/Large, false: PBOP/Small
  val triggerLine = UInt(CqfParameters.LineBits.W)
  val candidateLine = UInt(CqfParameters.LineBits.W)
}

object CqfTraceOutcome {
  val Width = 3
  val Allow = 0
  val Suppress = 1
  val InvalidPcBypass = 2
  val DisabledBypass = 3
  // Kept for trace-schema compatibility. The new fail-open gate never emits it.
  val CapacityDrop = 4
  val CapacityBypass = 5
  val OutputOverflowDrop = 6
}

/** Sparse event kinds used by the CQF black-box replay trace. */
object CqfReplayEventType {
  val Demand = 0
  val Candidate = 1
}

/**
  * One input event observed at the CompactQualityFeedback black-box boundary.
  *
  * Rows are emitted only when an input is offered (`valid` is high), rather
  * than once per clock.  `accepted` records the corresponding handshake so a
  * replay can distinguish an event captured by CQF from one rejected by its
  * event-FIFO capacity.
  */
class CqfReplayInputEntry(implicit p: Parameters) extends PrefetchBundle {
  val cycle = UInt(64.W)
  val hartId = UInt(16.W)
  val eventType = UInt(1.W)
  val port = UInt(1.W)
  val enable = Bool()
  val valid = Bool()
  val ready = Bool()
  val fire = Bool()
  val accepted = Bool()
  val demandLine = UInt(CqfParameters.LineBits.W)
  val candidate = new CqfCandidate
}

/** One sparse decision handshake emitted by the CQF black box. */
class CqfReplayDecisionEntry(implicit p: Parameters) extends PrefetchBundle {
  val cycle = UInt(64.W)
  val hartId = UInt(16.W)
  val port = UInt(1.W)
  val valid = Bool()
  val ready = Bool()
  val fire = Bool()
  val allow = Bool()
  val sampled = Bool()
  val feedbackInserted = Bool()
}

/** A sparse record of decision-ready backpressure changes. */
class CqfReplayReadyEntry(implicit p: Parameters) extends PrefetchBundle {
  val cycle = UInt(64.W)
  val hartId = UInt(16.W)
  val port = UInt(1.W)
  val ready = Bool()
}

/** A sparse record of runtime CQF enable changes. */
class CqfReplayControlEntry(implicit p: Parameters) extends PrefetchBundle {
  val cycle = UInt(64.W)
  val hartId = UInt(16.W)
  val enable = Bool()
}

/** One resolved outcome for every native BOP request presented to CQF. */
class CqfTraceEntry(implicit p: Parameters) extends PrefetchBundle {
  val pc = UInt(pcBitOpt.getOrElse(fullVAddrBits).W)
  val pcValid = Bool()
  val kind = Bool()
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
  val paddr = UInt(fullAddressBits.W)
  val triggerLine = UInt(CqfParameters.LineBits.W)
  val candidateLine = UInt(CqfParameters.LineBits.W)
  val outcome = UInt(CqfTraceOutcome.Width.W)
  val allowed = Bool()
  val suppressed = Bool()
  val sampled = Bool()
  val feedbackInserted = Bool()
}

class CqfDecision extends Bundle {
  val allow = Bool()
  val sampled = Bool()
  val feedbackInserted = Bool()
}

class CompactQualityFeedbackIO(implicit p: Parameters) extends PrefetchBundle {
  val enable = Input(Bool())
  val demand = Flipped(ValidIO(UInt(CqfParameters.LineBits.W)))
  val demandAccept = Output(Bool())
  val candidate = Vec(2, Flipped(DecoupledIO(new CqfCandidate)))
  val decision = Vec(2, DecoupledIO(new CqfDecision))
}

private class CqfPendingRequest(implicit p: Parameters) extends PrefetchBundle {
  // CQF is deliberately placed before PrefetchReqBuffer/TLB.  Keep the
  // native BOP request here so admission does not depend on a translated or
  // deduplicated request already existing.
  val req = new BopReqBundle
  val meta = new CqfCandidate
}

private class CqfOutputRequest(implicit p: Parameters) extends PrefetchBundle {
  val req = new BopReqBundle
  val outcome = UInt(CqfTraceOutcome.Width.W)
}

/** A four-entry register queue with two enqueue ports and one dequeue port. */
private class CqfOutputQueue(entries: Int)(implicit p: Parameters) extends PrefetchModule {
  require(isPow2(entries) && entries >= 2)

  val io = IO(new Bundle {
    val enq = Vec(2, Flipped(DecoupledIO(new CqfOutputRequest)))
    val deq = DecoupledIO(new CqfOutputRequest)
    val count = Output(UInt(log2Ceil(entries + 1).W))
  })

  private val ptrBits = log2Ceil(entries)
  private val storage = Reg(Vec(entries, new CqfOutputRequest))
  private val head = RegInit(0.U(ptrBits.W))
  private val tail = RegInit(0.U(ptrBits.W))
  private val count = RegInit(0.U(log2Ceil(entries + 1).W))

  io.deq.valid := count =/= 0.U
  io.deq.bits := storage(head)
  io.count := count

  private val dequeue = io.deq.fire
  private val freeAfterDequeue = entries.U - count + dequeue.asUInt
  io.enq(0).ready := freeAfterDequeue >= 1.U
  io.enq(1).ready := freeAfterDequeue >= (1.U +& io.enq(0).fire.asUInt)

  private val enqueueCount = PopCount(io.enq.map(_.fire))
  private val secondTail = (tail + io.enq(0).fire.asUInt)(ptrBits - 1, 0)

  when (io.enq(0).fire) {
    storage(tail) := io.enq(0).bits
  }
  when (io.enq(1).fire) {
    storage(secondTail) := io.enq(1).bits
  }
  when (enqueueCount =/= 0.U) {
    tail := tail + enqueueCount
  }
  when (dequeue) {
    head := head + 1.U
  }
  count := count + enqueueCount - dequeue.asUInt

  when (!reset.asBool) {
    assert(count <= entries.U, "CQF output queue occupancy overflow")
    assert(enqueueCount <= freeAfterDequeue,
      "CQF output queue accepted more requests than available slots")
  }
}

/**
  * External fail-open admission gate for one native BOP output stream.
  *
  * Requests admitted by CQF are retained in-order until their decisions return.
  * Requests that cannot enter CQF, have no valid PC, or arrive while CQF is
  * disabled bypass policy through the same output queue. No backpressure is
  * propagated into native BOP. A request is actually lost only when this small
  * output queue cannot retain an immediate bypass; that case has a distinct
  * OutputOverflowDrop outcome and is never counted as policy suppression.
  */
class CqfRequestGate(counterPrefix: String)(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    // This boundary is the outermost BOP control point: the request has
    // already selected its teacher/student offset and passed issueGate, but
    // has not entered request filtering or TLB translation yet.
    val in = Flipped(DecoupledIO(new BopReqBundle))
    val inMeta = Flipped(ValidIO(new CqfCandidate))
    val out = DecoupledIO(new BopReqBundle)
    val candidate = DecoupledIO(new CqfCandidate)
    val decision = Flipped(DecoupledIO(new CqfDecision))
    val candidateEligible = Output(Bool())
    val candidateAdmit = Output(Bool())
    val capacityBypass = Output(Bool())
  })

  private val pending = Module(new Queue(
    new CqfPendingRequest,
    entries = CqfParameters.GatePendingEntries,
    pipe = false,
    flow = false
  ))
  private val output = Module(new CqfOutputQueue(CqfParameters.GateOutputEntries))

  io.in.ready := true.B
  io.out.valid := output.io.deq.valid
  io.out.bits := output.io.deq.bits.req
  output.io.deq.ready := io.out.ready

  private val candidateEligible = io.enable && io.inMeta.valid && io.inMeta.bits.pcValid
  private val invalidPc = io.enable && io.inMeta.valid && !io.inMeta.bits.pcValid
  private val disabled = !io.enable

  // Only advertise a candidate when the matching native request can also be
  // retained. If CQF does not grant it in this cycle, the request fails open.
  private val pendingCapacityAvailable = pending.io.count < CqfParameters.GatePendingEntries.U
  io.candidate.valid := io.in.valid && candidateEligible && pendingCapacityAvailable
  io.candidate.bits := io.inMeta.bits
  private val candidateAdmit = io.in.fire && io.candidate.fire
  io.candidateEligible := io.in.fire && candidateEligible
  io.candidateAdmit := candidateAdmit

  pending.io.enq.valid := candidateAdmit
  pending.io.enq.bits.req := io.in.bits
  pending.io.enq.bits.meta := io.inMeta.bits

  private val immediateValid = io.in.fire && !candidateAdmit
  private val capacityBypass = immediateValid && candidateEligible
  io.capacityBypass := capacityBypass
  private val immediateBaseOutcome = Mux(
    capacityBypass,
    CqfTraceOutcome.CapacityBypass.U,
    Mux(invalidPc, CqfTraceOutcome.InvalidPcBypass.U, CqfTraceOutcome.DisabledBypass.U)
  )

  // Suppression decisions never need output capacity. Allowed decisions wait
  // for output space, preserving sampled Feedback ownership for requests that
  // will really leave the gate. Immediate fail-open traffic has enqueue
  // priority because a CQF decision can safely wait while native BOP cannot.
  output.io.enq(0).valid := immediateValid
  output.io.enq(0).bits.req := io.in.bits
  output.io.enq(0).bits.outcome := immediateBaseOutcome
  output.io.enq(1).valid := io.decision.valid && pending.io.deq.valid && io.decision.bits.allow
  output.io.enq(1).bits.req := pending.io.deq.bits.req
  output.io.enq(1).bits.outcome := CqfTraceOutcome.Allow.U
  io.decision.ready := pending.io.deq.valid &&
    (!io.decision.bits.allow || output.io.enq(1).ready)
  pending.io.deq.ready := io.decision.fire

  private val immediateEnqueue = output.io.enq(0).fire
  private val outputOverflowDrop = immediateValid && !output.io.enq(0).ready

  private def makeTraceEntry(
    req: BopReqBundle,
    meta: CqfCandidate,
    outcome: UInt,
    allowed: Bool,
    suppressed: Bool,
    sampled: Bool,
    feedbackInserted: Bool
  ): CqfTraceEntry = {
    val entry = Wire(new CqfTraceEntry)
    entry.pc := meta.pc
    entry.pcValid := meta.pcValid
    entry.kind := meta.kind
    entry.reqSource := meta.reqSource
    // Translation has not happened at this point.  Preserve the same-page
    // physical address when BOP already knows it; otherwise leave the trace
    // address zero rather than recording a virtual address as a paddr.
    entry.paddr := Mux(req.samePagePaddrValid, req.samePagePaddr, 0.U)
    entry.triggerLine := meta.triggerLine
    entry.candidateLine := meta.candidateLine
    entry.outcome := outcome
    entry.allowed := allowed
    entry.suppressed := suppressed
    entry.sampled := sampled
    entry.feedbackInserted := feedbackInserted
    entry
  }

  private val traceTable = ChiselDB.createTable("L2BopCqfTrace", new CqfTraceEntry, basicDB = true)
  private val decisionTrace = makeTraceEntry(
    pending.io.deq.bits.req,
    pending.io.deq.bits.meta,
    Mux(io.decision.bits.allow, CqfTraceOutcome.Allow.U, CqfTraceOutcome.Suppress.U),
    io.decision.bits.allow,
    !io.decision.bits.allow,
    io.decision.bits.sampled,
    io.decision.bits.feedbackInserted
  )
  traceTable.log(
    data = decisionTrace,
    en = io.decision.fire,
    site = s"${counterPrefix}_decision",
    clock,
    reset
  )

  private val immediateTrace = makeTraceEntry(
    io.in.bits,
    io.inMeta.bits,
    Mux(outputOverflowDrop, CqfTraceOutcome.OutputOverflowDrop.U, immediateBaseOutcome),
    immediateEnqueue,
    false.B,
    false.B,
    false.B
  )
  traceTable.log(
    data = immediateTrace,
    en = immediateValid,
    site = s"${counterPrefix}_immediate",
    clock,
    reset
  )

  private val eligibleOverflowDrop = outputOverflowDrop && candidateEligible
  private val outputAllowSent = io.out.fire &&
    output.io.deq.bits.outcome === CqfTraceOutcome.Allow.U
  private val outputCapacityBypassSent = io.out.fire &&
    output.io.deq.bits.outcome === CqfTraceOutcome.CapacityBypass.U

  XSPerfAccumulate(s"${counterPrefix}_cqf_candidate", io.in.fire && candidateEligible)
  XSPerfAccumulate(s"${counterPrefix}_cqf_candidate_eligible", io.in.fire && candidateEligible)
  XSPerfAccumulate(s"${counterPrefix}_cqf_candidate_accept", candidateAdmit)
  XSPerfAccumulate(s"${counterPrefix}_cqf_candidate_admit", candidateAdmit)
  XSPerfAccumulate(s"${counterPrefix}_cqf_candidate_capacity_bypass", capacityBypass)
  // Compatibility name now counts only a real final-output loss.
  XSPerfAccumulate(s"${counterPrefix}_cqf_candidate_drop", eligibleOverflowDrop)
  XSPerfAccumulate(s"${counterPrefix}_cqf_invalid_pc_bypass", immediateValid && invalidPc)
  XSPerfAccumulate(s"${counterPrefix}_cqf_disabled_bypass", immediateValid && disabled)
  XSPerfAccumulate(s"${counterPrefix}_cqf_invalid_pc_store_bypass",
    immediateValid && invalidPc &&
      io.inMeta.bits.reqSource === MemReqSource.CPUStoreData.id.U)
  XSPerfAccumulate(s"${counterPrefix}_cqf_invalid_pc_atomic_bypass",
    immediateValid && invalidPc &&
      io.inMeta.bits.reqSource === MemReqSource.CPUAtomicData.id.U)
  XSPerfAccumulate(s"${counterPrefix}_cqf_invalid_pc_l1_prefetch_bypass",
    immediateValid && invalidPc && MemReqSource.isL1Prefetch(io.inMeta.bits.reqSource))
  XSPerfAccumulate(s"${counterPrefix}_cqf_invalid_pc_other_bypass",
    immediateValid && invalidPc &&
      io.inMeta.bits.reqSource =/= MemReqSource.CPUStoreData.id.U &&
      io.inMeta.bits.reqSource =/= MemReqSource.CPUAtomicData.id.U &&
      !MemReqSource.isL1Prefetch(io.inMeta.bits.reqSource))
  // Legacy capacity-drop event is intentionally eliminated by fail-open bypass.
  XSPerfAccumulate(s"${counterPrefix}_cqf_capacity_drop", false.B)
  XSPerfAccumulate(s"${counterPrefix}_cqf_bypass_enqueue",
    immediateEnqueue && immediateBaseOutcome =/= CqfTraceOutcome.Allow.U)
  XSPerfAccumulate(s"${counterPrefix}_cqf_output_overflow_drop", outputOverflowDrop)
  XSPerfAccumulate(s"${counterPrefix}_cqf_allow", io.decision.fire && io.decision.bits.allow)
  XSPerfAccumulate(s"${counterPrefix}_cqf_suppress", io.decision.fire && !io.decision.bits.allow)
  XSPerfAccumulate(s"${counterPrefix}_cqf_policy_allow",
    io.decision.fire && io.decision.bits.allow)
  XSPerfAccumulate(s"${counterPrefix}_cqf_policy_suppress",
    io.decision.fire && !io.decision.bits.allow)
  XSPerfAccumulate(s"${counterPrefix}_cqf_allow_sent", outputAllowSent)
  XSPerfAccumulate(s"${counterPrefix}_cqf_capacity_bypass_sent", outputCapacityBypassSent)
  XSPerfAccumulate(s"${counterPrefix}_cqf_req_out", io.out.fire)
  XSPerfAccumulate(s"${counterPrefix}_cqf_output_occupancy_sum", output.io.count)
  XSPerfMax(s"${counterPrefix}_cqf_output_occupancy", output.io.count, true.B)
  XSPerfMax(s"${counterPrefix}_cqf_output_occupancy_max", output.io.count, true.B)
  XSPerfAccumulate(s"${counterPrefix}_cqf_pending_occupancy_sum", pending.io.count)
  XSPerfMax(s"${counterPrefix}_cqf_pending_occupancy", pending.io.count, true.B)
  XSPerfMax(s"${counterPrefix}_cqf_pending_occupancy_max", pending.io.count, true.B)

  when (!reset.asBool) {
    assert(!io.in.valid || io.inMeta.valid,
      "CQF gate metadata must be valid with every native BOP request")
    assert(!io.decision.valid || pending.io.deq.valid,
      "CQF returned a decision without a matching native BOP request")
    assert(!candidateAdmit || pending.io.enq.fire,
      "CQF-admitted candidate must retain its native BOP request")
    assert(!immediateValid || immediateEnqueue || outputOverflowDrop,
      "CQF immediate bypass must be queued or explicitly dropped")
    assert(io.in.fire === (candidateAdmit || immediateEnqueue || outputOverflowDrop),
      "CQF native request must be admitted, bypass-enqueued, or explicitly dropped")
    assert(!(io.decision.fire && !io.decision.bits.allow) || !output.io.enq(1).fire,
      "CQF policy suppression must not enter the output queue")
  }
}

private class CqfQualityEntry extends Bundle {
  val valid = Bool()
  val kind = Bool()
  val tag = UInt(8.W)
  val state = UInt(2.W)
  val useful = UInt(7.W)
  val unused = UInt(7.W)
  val resolved = UInt(6.W)
}

private class CqfFeedbackEntry extends Bundle {
  val valid = Bool()
  val tag = UInt(14.W)
  val ownerSet = UInt(6.W)
  val ownerTag = UInt(8.W)
  val ownerKind = Bool()
  val issueEpoch = UInt(6.W)
}

private class CqfEvent(implicit p: Parameters) extends PrefetchBundle {
  val isDemand = Bool()
  val port = UInt(1.W)
  val demandLine = UInt(CqfParameters.LineBits.W)
  val candidate = new CqfCandidate
  val enqueueCycle = UInt(32.W)
}

/**
  * Multi-enqueue event FIFO for the serialized CQF engine.
  *
  * The front end can capture one demand and up to two BOP candidates in the
  * same cycle, while the engine still dequeues at most one event per cycle.
  * Enqueue port 0 is demand; ports 1 and 2 are the caller-selected candidate
  * order.
  */
private class CqfEventQueue(entries: Int)(implicit p: Parameters) extends PrefetchModule {
  require(isPow2(entries) && entries >= 2)

  private val enqueuePorts = 3
  private val ptrBits = log2Ceil(entries)
  private val countBits = log2Ceil(entries + 1)

  val io = IO(new Bundle {
    val enq = Vec(enqueuePorts, Flipped(DecoupledIO(new CqfEvent)))
    val deq = DecoupledIO(new CqfEvent)
    val count = Output(UInt(countBits.W))
    val free = Output(UInt(countBits.W))
  })

  private val storage = Reg(Vec(entries, new CqfEvent))
  private val head = RegInit(0.U(ptrBits.W))
  private val tail = RegInit(0.U(ptrBits.W))
  private val count = RegInit(0.U(countBits.W))

  io.deq.valid := count =/= 0.U
  io.deq.bits := storage(head)
  io.count := count

  private val dequeue = io.deq.fire
  private val freeAfterDequeue = entries.U(countBits.W) - count + dequeue.asUInt
  io.free := freeAfterDequeue

  // Earlier enqueue ports have priority and consume capacity before later
  // ports. This ordering is demand first, then the selected candidate order.
  for (i <- 0 until enqueuePorts) {
    val prefixFire = if (i == 0) {
      0.U(countBits.W)
    } else {
      PopCount(io.enq.take(i).map(_.fire)).asUInt
    }
    io.enq(i).ready := freeAfterDequeue >= (prefixFire + 1.U)
  }

  private val enqueueCount = PopCount(io.enq.map(_.fire))
  for (i <- 0 until enqueuePorts) {
    val offset = if (i == 0) {
      0.U(countBits.W)
    } else {
      PopCount(io.enq.take(i).map(_.fire)).asUInt
    }
    val writePtr = (tail + offset)(ptrBits - 1, 0)
    when (io.enq(i).fire) {
      storage(writePtr) := io.enq(i).bits
    }
  }

  when (enqueueCount =/= 0.U) {
    tail := tail + enqueueCount
  }
  when (dequeue) {
    head := head + 1.U
  }
  count := count + enqueueCount - dequeue.asUInt

  when (!reset.asBool) {
    assert(count <= entries.U, "CQF event FIFO occupancy overflow")
    assert(enqueueCount <= freeAfterDequeue,
      "CQF event FIFO accepted more events than available slots")
  }
}

/**
  * SRAM-oriented Compact Quality Feedback controller.
  *
  * Quality and Feedback are independent 64-set, four-way, synchronous 1RW
  * SRAMs. A single multi-cycle engine serializes all logical events. Each SRAM
  * accepts at most one read or write per cycle, while independent Quality and
  * Feedback operations may proceed in parallel. An eight-entry event FIFO
  * absorbs bursts; its last slot is reserved for demand feedback. Demand is
  * never backpressured and is dropped only when that FIFO is actually full.
  * The front end can admit both VBOP and PBOP candidates in one cycle; the
  * serialized engine still processes only one event at a time. A candidate
  * that is not admitted is handled fail-open by its external CqfRequestGate.
  */
class CompactQualityFeedbackSerial(implicit p: Parameters) extends PrefetchModule {
  private val qualityWays = 4
  private val qualitySets = 64
  private val feedbackWays = 4
  private val feedbackSets = 64
  private val qualityTagBits = 8
  private val feedbackTagBits = 14
  private val lineBits = CqfParameters.LineBits

  require(fullVAddrBits >= offsetBits,
    s"CQF requires a virtual address wider than the cache-line offset, got $fullVAddrBits")
  require(offsetBits == 6,
    s"CQF's Sv48 line fingerprint requires 64-byte blocks, got ${1 << offsetBits} bytes")

  val io = IO(new CompactQualityFeedbackIO)

  private val stateObserve = 1.U(2.W)
  private val stateOpen = 2.U(2.W)
  private val stateBlock = 3.U(2.W)

  private val qualityTable = Module(new SRAMTemplate(
    new CqfQualityEntry,
    set = qualitySets,
    way = qualityWays,
    singlePort = true,
    shouldReset = true,
    hasMbist = cacheParams.hasMbist,
    hasSramCtl = cacheParams.hasSramCtl
  ))
  private val feedbackTable = Module(new SRAMTemplate(
    new CqfFeedbackEntry,
    set = feedbackSets,
    way = feedbackWays,
    singlePort = true,
    shouldReset = true,
    hasMbist = cacheParams.hasMbist,
    hasSramCtl = cacheParams.hasSramCtl
  ))

  private val qPlru = RegInit(VecInit(Seq.fill(qualitySets)(0.U(3.W))))
  private val fVictim = RegInit(VecInit(Seq.fill(feedbackSets)(0.U(2.W))))
  private val feedbackOccupancy = RegInit(0.U(9.W))
  private val demandAge = RegInit(0.U(12.W))
  private val sweepPtr = RegInit(0.U(8.W))
  // Keep replay timestamps unambiguous for long online runs. Existing
  // latency counters intentionally retain their 32-bit output contract.
  private val cycleCounter = RegInit(0.U(64.W))
  cycleCounter := cycleCounter + 1.U

  private val tablesReady = qualityTable.io.resetDone && feedbackTable.io.resetDone
  private val eventQueue = Module(new CqfEventQueue(CqfParameters.EventQueueEntries))
  private val candidateRoundRobin = RegInit(0.U(1.W))

  private val demandInput = io.enable && io.demand.valid
  private val candidateInputs = VecInit(io.candidate.map(c => io.enable && c.valid))

  // The queue's free count includes a same-cycle dequeue. A demand consumes
  // the first available slot. Candidates may use later slots in that same
  // cycle, while one final slot remains reserved for a future demand.
  private val candidateReserveAvailable = eventQueue.io.free >=
    (2.U +& demandInput.asUInt)
  private val candidateDualAvailable = eventQueue.io.free >=
    (3.U +& demandInput.asUInt)
  private val bothCandidates = candidateInputs.asUInt.andR
  private val firstIsCandidate0 = Mux(
    bothCandidates,
    candidateRoundRobin === 0.U,
    candidateInputs(0)
  )
  private val firstCandidateValid = Mux(
    firstIsCandidate0,
    candidateInputs(0),
    candidateInputs(1)
  )
  private val secondCandidateValid = bothCandidates
  private val firstCandidateBits = Mux(
    firstIsCandidate0,
    io.candidate(0).bits,
    io.candidate(1).bits
  )
  private val secondCandidateBits = Mux(
    firstIsCandidate0,
    io.candidate(1).bits,
    io.candidate(0).bits
  )
  private val firstCandidateEnqueue = candidateReserveAvailable && firstCandidateValid
  private val secondCandidateEnqueue = candidateDualAvailable && secondCandidateValid

  eventQueue.io.enq(1).valid := firstCandidateEnqueue
  eventQueue.io.enq(1).bits.isDemand := false.B
  eventQueue.io.enq(1).bits.port := Mux(firstIsCandidate0, 0.U, 1.U)
  eventQueue.io.enq(1).bits.demandLine := 0.U
  eventQueue.io.enq(1).bits.candidate := firstCandidateBits
  eventQueue.io.enq(1).bits.enqueueCycle := cycleCounter(31, 0)

  eventQueue.io.enq(2).valid := secondCandidateEnqueue
  eventQueue.io.enq(2).bits.isDemand := false.B
  eventQueue.io.enq(2).bits.port := Mux(firstIsCandidate0, 1.U, 0.U)
  eventQueue.io.enq(2).bits.demandLine := 0.U
  eventQueue.io.enq(2).bits.candidate := secondCandidateBits
  eventQueue.io.enq(2).bits.enqueueCycle := cycleCounter(31, 0)

  private val firstCandidateReady = firstCandidateEnqueue && eventQueue.io.enq(1).ready
  private val secondCandidateReady = secondCandidateEnqueue && eventQueue.io.enq(2).ready
  io.candidate(0).ready := Mux(firstIsCandidate0, firstCandidateReady, secondCandidateReady)
  io.candidate(1).ready := Mux(firstIsCandidate0, secondCandidateReady, firstCandidateReady)

  private val candidateAccepted = VecInit(io.candidate.map(_.fire))
  private val anyCandidateAccepted = candidateAccepted.asUInt.orR
  private val acceptedCandidatePort = Mux(candidateAccepted(1), 1.U, 0.U)
  when (anyCandidateAccepted && !candidateAccepted.asUInt.andR) {
    candidateRoundRobin := ~acceptedCandidatePort
  }

  // Events may queue while resettable SRAMs perform their 64-set clear sweep.
  // Consequently every enabled demand rejection means the FIFO is truly full.
  eventQueue.io.enq(0).valid := demandInput
  eventQueue.io.enq(0).bits.isDemand := true.B
  eventQueue.io.enq(0).bits.port := 0.U
  eventQueue.io.enq(0).bits.demandLine := io.demand.bits
  eventQueue.io.enq(0).bits.candidate := 0.U.asTypeOf(new CqfCandidate)
  eventQueue.io.enq(0).bits.enqueueCycle := cycleCounter(31, 0)

  io.demandAccept := eventQueue.io.enq(0).fire
  private val demandEnqueue = io.demandAccept
  private val demandOverflowDrop = demandInput && !io.demandAccept

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
    MuxLookup(way, Cat(0.U(1.W), state(1), 0.U(1.W)))(Seq(
      0.U -> Cat(state(2), 1.U(1.W), 1.U(1.W)),
      1.U -> Cat(state(2), 0.U(1.W), 1.U(1.W)),
      2.U -> Cat(1.U(1.W), state(1), 0.U(1.W))
    ))
  }

  private def plruVictim(state: UInt): UInt =
    Mux(!state(0), Mux(!state(1), 0.U, 1.U), Mux(!state(2), 2.U, 3.U))

  private def findQualityWay(row: Vec[CqfQualityEntry], tag: UInt, kind: Bool): UInt = {
    val hits = VecInit(row.map(e => e.valid && e.tag === tag && e.kind === kind))
    Mux(hits.asUInt.orR, OHToUInt(PriorityEncoderOH(hits.asUInt)), qualityWays.U)
  }

  private def selectQualityWay(
    row: Vec[CqfQualityEntry],
    tag: UInt,
    kind: Bool,
    plru: UInt
  ): UInt = {
    val hitWay = findQualityWay(row, tag, kind)
    val invalids = VecInit(row.map(e => !e.valid))
    Mux(hitWay =/= qualityWays.U,
      hitWay,
      Mux(invalids.asUInt.orR,
        OHToUInt(PriorityEncoderOH(invalids.asUInt)),
        plruVictim(plru)))
  }

  private def findFeedbackWay(row: Vec[CqfFeedbackEntry], tag: UInt): UInt = {
    val hits = VecInit(row.map(e => e.valid && e.tag === tag))
    Mux(hits.asUInt.orR, OHToUInt(PriorityEncoderOH(hits.asUInt)), feedbackWays.U)
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

  private def applyOutcome(entry: CqfQualityEntry, isUseful: Bool):
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

  private val Seq(
    sIdle,
    sCandidateQualityRead, sCandidateQualityEval, sCandidateQualityWrite,
    sCandidateFeedbackRead, sCandidateFeedbackEval, sCandidateFeedbackWrite,
    sCandidateResponse,
    sDemandMatchRead, sDemandMatchCapture, sDemandSweepRead, sDemandSweepCapture,
    sDemandFeedbackEval, sDemandFeedbackWrite0, sDemandFeedbackWrite1,
    sDemandUsefulQualityRead, sDemandUsefulQualityEval, sDemandUsefulQualityWrite,
    sDemandUnusedQualityRead, sDemandUnusedQualityEval, sDemandUnusedQualityWrite
  ) = Enum(21)
  private val state = RegInit(sIdle)
  private val currentEvent = Reg(new CqfEvent)

  private val responseValid = RegInit(false.B)
  private val responsePort = Reg(UInt(1.W))
  private val responseBits = Reg(new CqfDecision)
  for (k <- 0 until 2) {
    io.decision(k).valid := responseValid && responsePort === k.U
    io.decision(k).bits := responseBits
  }
  private val responseReady = Mux(responsePort === 0.U,
    io.decision(0).ready, io.decision(1).ready)
  private val responseFire = responseValid && responseReady

  eventQueue.io.deq.ready := state === sIdle && tablesReady && !responseValid

  private val qualityWriteSet = Reg(UInt(6.W))
  private val qualityWriteRow = Reg(Vec(qualityWays, new CqfQualityEntry))
  private val feedbackWriteSet = Reg(UInt(6.W))
  private val feedbackWriteRow = Reg(Vec(feedbackWays, new CqfFeedbackEntry))

  private val qualityReadValid = WireDefault(false.B)
  private val qualityReadSet = WireDefault(0.U(6.W))
  private val qualityWriteValid = WireDefault(false.B)
  private val feedbackReadValid = WireDefault(false.B)
  private val feedbackReadSet = WireDefault(0.U(6.W))
  private val feedbackWriteValid = WireDefault(false.B)

  qualityTable.io.r.req.valid := qualityReadValid
  qualityTable.io.r.req.bits.setIdx := qualityReadSet
  qualityTable.io.w.req.valid := qualityWriteValid
  qualityTable.io.w.req.bits.data := qualityWriteRow
  qualityTable.io.w.req.bits.setIdx := qualityWriteSet
  qualityTable.io.w.req.bits.waymask.foreach(_ := Fill(qualityWays, 1.U(1.W)))

  feedbackTable.io.r.req.valid := feedbackReadValid
  feedbackTable.io.r.req.bits.setIdx := feedbackReadSet
  feedbackTable.io.w.req.valid := feedbackWriteValid
  feedbackTable.io.w.req.bits.data := feedbackWriteRow
  feedbackTable.io.w.req.bits.setIdx := feedbackWriteSet
  feedbackTable.io.w.req.bits.waymask.foreach(_ := Fill(feedbackWays, 1.U(1.W)))

  private val qualityReadFire = qualityTable.io.r.req.fire
  private val qualityWriteFire = qualityTable.io.w.req.fire
  private val feedbackReadFire = feedbackTable.io.r.req.fire
  private val feedbackWriteFire = feedbackTable.io.w.req.fire

  private val candidateAllowedReg = Reg(Bool())
  private val candidateSampledReg = Reg(Bool())

  private val demandMatchSet = Reg(UInt(6.W))
  private val demandMatchTag = Reg(UInt(feedbackTagBits.W))
  private val demandSweepIndex = Reg(UInt(8.W))
  private val demandNextEpoch = Reg(UInt(6.W))
  private val demandMatchRow = Reg(Vec(feedbackWays, new CqfFeedbackEntry))
  private val demandSweepRow = Reg(Vec(feedbackWays, new CqfFeedbackEntry))
  private val demandFeedbackWrite0Valid = RegInit(false.B)
  private val demandFeedbackWrite0Set = Reg(UInt(6.W))
  private val demandFeedbackWrite0Row = Reg(Vec(feedbackWays, new CqfFeedbackEntry))
  private val demandFeedbackWrite1Valid = RegInit(false.B)
  private val demandFeedbackWrite1Set = Reg(UInt(6.W))
  private val demandFeedbackWrite1Row = Reg(Vec(feedbackWays, new CqfFeedbackEntry))
  private val demandUsefulPending = RegInit(false.B)
  private val demandUsefulOwnerSet = Reg(UInt(6.W))
  private val demandUsefulOwnerTag = Reg(UInt(qualityTagBits.W))
  private val demandUsefulOwnerKind = Reg(Bool())
  private val demandUnusedPending = RegInit(false.B)
  private val demandUnusedOwnerSet = Reg(UInt(6.W))
  private val demandUnusedOwnerTag = Reg(UInt(qualityTagBits.W))
  private val demandUnusedOwnerKind = Reg(Bool())
  private val demandPrefetchedQualityRow = Reg(Vec(qualityWays, new CqfQualityEntry))
  private val demandQualityRowPrefetched = RegInit(false.B)

  private val demandProcessedPulse = WireDefault(false.B)
  private val candidateCompletePulse = WireDefault(false.B)
  private val candidateAllowPulse = WireDefault(false.B)
  private val candidateSuppressPulse = WireDefault(false.B)
  private val qualityHitPulse = WireDefault(false.B)
  private val qualityAllocatePulse = WireDefault(false.B)
  private val qualityReplacePulse = WireDefault(false.B)
  private val feedbackSelectedPulse = WireDefault(false.B)
  private val feedbackInsertPulse = WireDefault(false.B)
  private val feedbackCoalescePulse = WireDefault(false.B)
  private val feedbackReplacePulse = WireDefault(false.B)
  private val feedbackUsefulPulse = WireDefault(false.B)
  private val feedbackUnusedPulse = WireDefault(false.B)
  private val feedbackExpiryPulse = WireDefault(false.B)
  private val feedbackOwnerMissPulse = WireDefault(false.B)
  private val feedbackRetireCount = WireDefault(0.U(2.W))
  private val feedbackOccupancyIncrement = WireDefault(false.B)
  private val feedbackOccupancyDecrement = WireDefault(0.U(2.W))
  private val stateTransitionCount = WireDefault(0.U(2.W))
  private val observeToOpenCount = WireDefault(0.U(2.W))
  private val observeToBlockCount = WireDefault(0.U(2.W))
  private val openToBlockCount = WireDefault(0.U(2.W))
  private val blockToOpenCount = WireDefault(0.U(2.W))
  private val candidateLatency = WireDefault(0.U(32.W))
  private val demandLatency = WireDefault(0.U(32.W))

  private def completeCandidate(allow: Bool, sampled: Bool, feedbackInserted: Bool): Unit = {
    responseValid := true.B
    responsePort := currentEvent.port
    responseBits.allow := allow
    responseBits.sampled := sampled
    responseBits.feedbackInserted := feedbackInserted
    state := sCandidateResponse
    candidateCompletePulse := true.B
    candidateAllowPulse := allow
    candidateSuppressPulse := !allow
    candidateLatency := cycleCounter(31, 0) - currentEvent.enqueueCycle
  }

  private def completeDemand(): Unit = {
    state := sIdle
    demandProcessedPulse := true.B
    demandLatency := cycleCounter(31, 0) - currentEvent.enqueueCycle
  }

  private def routeAfterFeedbackWrites(): Unit = {
    when (demandUsefulPending) {
      state := sDemandUsefulQualityRead
    }.elsewhen (demandUnusedPending) {
      state := sDemandUnusedQualityRead
    }.otherwise {
      completeDemand()
    }
  }

  private def routeAfterUseful(): Unit = {
    when (demandUnusedPending) {
      state := sDemandUnusedQualityRead
    }.otherwise {
      completeDemand()
    }
  }

  when (feedbackOccupancyIncrement) {
    feedbackOccupancy := feedbackOccupancy + 1.U
  }.elsewhen (feedbackOccupancyDecrement =/= 0.U) {
    assert(feedbackOccupancy >= feedbackOccupancyDecrement,
      "CQF Feedback occupancy underflow")
    feedbackOccupancy := feedbackOccupancy - feedbackOccupancyDecrement
  }

  when (responseFire) {
    responseValid := false.B
  }

  switch(state) {
    is(sIdle) {
      when (eventQueue.io.deq.fire) {
        currentEvent := eventQueue.io.deq.bits
        when (eventQueue.io.deq.bits.isDemand) {
          val nextAge = demandAge + 1.U
          val demandHash = feedbackHash(eventQueue.io.deq.bits.demandLine)
          demandQualityRowPrefetched := false.B
          demandAge := nextAge
          demandMatchSet := demandHash(5, 0)
          demandMatchTag := demandHash(19, 6)
          demandSweepIndex := sweepPtr
          demandNextEpoch := nextAge(11, 6)
          sweepPtr := sweepPtr + 1.U
          state := sDemandMatchRead
        }.otherwise {
          state := sCandidateQualityRead
        }
      }
    }

    is(sCandidateQualityRead) {
      val hash = qualityHash(currentEvent.candidate.pc, currentEvent.candidate.kind)
      qualityReadValid := true.B
      qualityReadSet := hash(5, 0)
      when (qualityReadFire) {
        state := sCandidateQualityEval
      }
    }

    is(sCandidateQualityEval) {
      val hash = qualityHash(currentEvent.candidate.pc, currentEvent.candidate.kind)
      val set = hash(5, 0)
      val tag = hash(13, 6)
      val row = qualityTable.io.r.resp.data
      val hitWay = findQualityWay(row, tag, currentEvent.candidate.kind)
      val hit = currentEvent.candidate.pcValid && hitWay =/= qualityWays.U
      val selectedWay = selectQualityWay(row, tag, currentEvent.candidate.kind, qPlru(set))
      val oldEntry = row(selectedWay(1, 0))
      val oldState = Mux(hit, oldEntry.state, stateObserve)
      val useful = Mux(hit, oldEntry.useful, 0.U(7.W))
      val unused = Mux(hit, oldEntry.unused, 0.U(7.W))
      val strict = unused >= (useful * 20.U + 4.U)
      val observeSample = samplingHash(
        currentEvent.candidate.pc,
        currentEvent.candidate.kind,
        currentEvent.candidate.triggerLine,
        "h0B5E".U
      )(3, 0) === 0.U
      val openSample = samplingHash(
        currentEvent.candidate.pc,
        currentEvent.candidate.kind,
        currentEvent.candidate.triggerLine,
        "h5A6D".U
      )(3, 0) === 0.U
      val borderlineProbe = samplingHash(
        currentEvent.candidate.pc,
        currentEvent.candidate.kind,
        currentEvent.candidate.triggerLine,
        "hB10C".U
      )(2, 0) === 0.U
      val strictProbe = samplingHash(
        currentEvent.candidate.pc,
        currentEvent.candidate.kind,
        currentEvent.candidate.triggerLine,
        "hB10C".U
      )(5, 0) === 0.U
      val sampled = currentEvent.candidate.pcValid && Mux(oldState === stateBlock,
        Mux(strict, strictProbe, borderlineProbe),
        Mux(oldState === stateOpen, openSample, observeSample))
      val allowed = !currentEvent.candidate.pcValid || !hit || oldState =/= stateBlock || sampled
      val updatedRow = WireInit(row)

      candidateAllowedReg := allowed
      candidateSampledReg := sampled

      when (currentEvent.candidate.pcValid) {
        qualityHitPulse := hit
        qualityAllocatePulse := !hit
        qualityReplacePulse := !hit && oldEntry.valid
        qPlru(set) := plruNext(qPlru(set), selectedWay)
        when (!hit) {
          updatedRow(selectedWay(1, 0)).valid := true.B
          updatedRow(selectedWay(1, 0)).kind := currentEvent.candidate.kind
          updatedRow(selectedWay(1, 0)).tag := tag
          updatedRow(selectedWay(1, 0)).state := stateObserve
          updatedRow(selectedWay(1, 0)).useful := 0.U
          updatedRow(selectedWay(1, 0)).unused := 0.U
          updatedRow(selectedWay(1, 0)).resolved := 0.U
          qualityWriteSet := set
          qualityWriteRow := updatedRow
          state := sCandidateQualityWrite
        }.elsewhen (sampled) {
          val feedbackKey = feedbackHash(currentEvent.candidate.candidateLine)
          feedbackReadValid := true.B
          feedbackReadSet := feedbackKey(5, 0)
          feedbackSelectedPulse := true.B
          when (feedbackReadFire) {
            state := sCandidateFeedbackEval
          }
        }.otherwise {
          completeCandidate(allowed, sampled, false.B)
        }
      }.otherwise {
        completeCandidate(true.B, false.B, false.B)
      }
    }

    is(sCandidateQualityWrite) {
      when (candidateSampledReg) {
        val feedbackKey = feedbackHash(currentEvent.candidate.candidateLine)
        // The tables are independent 1RW SRAMs. Both are known ready after the
        // reset sweep, so issue the allocation write and Feedback lookup in
        // parallel without coupling one SRAM's ready into the other's valid.
        qualityWriteValid := true.B
        feedbackReadValid := true.B
        feedbackReadSet := feedbackKey(5, 0)
        feedbackSelectedPulse := feedbackReadFire
        when (qualityWriteFire && feedbackReadFire) {
          state := sCandidateFeedbackEval
        }
      }.otherwise {
        qualityWriteValid := true.B
        when (qualityWriteFire) {
          completeCandidate(candidateAllowedReg, candidateSampledReg, false.B)
        }
      }
    }

    is(sCandidateFeedbackRead) {
      val hash = feedbackHash(currentEvent.candidate.candidateLine)
      feedbackReadValid := true.B
      feedbackReadSet := hash(5, 0)
      feedbackSelectedPulse := true.B
      when (feedbackReadFire) {
        state := sCandidateFeedbackEval
      }
    }

    is(sCandidateFeedbackEval) {
      val qualityKey = qualityHash(currentEvent.candidate.pc, currentEvent.candidate.kind)
      val feedbackKey = feedbackHash(currentEvent.candidate.candidateLine)
      val set = feedbackKey(5, 0)
      val tag = feedbackKey(19, 6)
      val row = feedbackTable.io.r.resp.data
      val hitWay = findFeedbackWay(row, tag)
      val hit = hitWay =/= feedbackWays.U
      val invalids = VecInit(row.map(e => !e.valid))
      val hasInvalid = invalids.asUInt.orR
      val victim = Mux(hasInvalid,
        OHToUInt(PriorityEncoderOH(invalids.asUInt)),
        fVictim(set))
      val updatedRow = WireInit(row)

      feedbackCoalescePulse := hit
      when (hit) {
        completeCandidate(candidateAllowedReg, candidateSampledReg, false.B)
      }.otherwise {
        feedbackInsertPulse := true.B
        feedbackReplacePulse := !hasInvalid
        feedbackOccupancyIncrement := hasInvalid
        updatedRow(victim).valid := true.B
        updatedRow(victim).tag := tag
        updatedRow(victim).ownerSet := qualityKey(5, 0)
        updatedRow(victim).ownerTag := qualityKey(13, 6)
        updatedRow(victim).ownerKind := currentEvent.candidate.kind
        updatedRow(victim).issueEpoch := demandAge(11, 6)
        feedbackWriteSet := set
        feedbackWriteRow := updatedRow
        when (!hasInvalid) {
          fVictim(set) := fVictim(set) + 1.U
        }
        state := sCandidateFeedbackWrite
      }
    }

    is(sCandidateFeedbackWrite) {
      feedbackWriteValid := true.B
      when (feedbackWriteFire) {
        completeCandidate(candidateAllowedReg, candidateSampledReg, true.B)
      }
    }

    is(sCandidateResponse) {
      when (responseFire) {
        state := sIdle
      }
    }

    is(sDemandMatchRead) {
      feedbackReadValid := true.B
      feedbackReadSet := demandMatchSet
      when (feedbackReadFire) {
        state := sDemandMatchCapture
      }
    }

    is(sDemandMatchCapture) {
      demandMatchRow := feedbackTable.io.r.resp.data
      when (demandMatchSet === demandSweepIndex(7, 2)) {
        demandSweepRow := feedbackTable.io.r.resp.data
        state := sDemandFeedbackEval
      }.otherwise {
        state := sDemandSweepRead
      }
    }

    is(sDemandSweepRead) {
      feedbackReadValid := true.B
      feedbackReadSet := demandSweepIndex(7, 2)
      when (feedbackReadFire) {
        state := sDemandSweepCapture
      }
    }

    is(sDemandSweepCapture) {
      demandSweepRow := feedbackTable.io.r.resp.data
      state := sDemandFeedbackEval
    }

    is(sDemandFeedbackEval) {
      val hitWay = findFeedbackWay(demandMatchRow, demandMatchTag)
      val useful = hitWay =/= feedbackWays.U
      val usefulEntry = demandMatchRow(hitWay(1, 0))
      val sweepWay = demandSweepIndex(1, 0)
      val sweepEntry = demandSweepRow(sweepWay)
      val expired = sweepEntry.valid &&
        ((demandNextEpoch - sweepEntry.issueEpoch) >= 30.U)
      val sameSet = demandMatchSet === demandSweepIndex(7, 2)
      val usefulWinsSweep = useful && sameSet && hitWay(1, 0) === sweepWay
      val unused = expired && !usefulWinsSweep
      val matchCleared = WireInit(demandMatchRow)
      val sweepCleared = WireInit(demandSweepRow)
      val combinedCleared = WireInit(demandMatchRow)

      when (useful) {
        matchCleared(hitWay(1, 0)).valid := false.B
        combinedCleared(hitWay(1, 0)).valid := false.B
      }
      when (unused) {
        sweepCleared(sweepWay).valid := false.B
        combinedCleared(sweepWay).valid := false.B
      }

      demandUsefulPending := useful
      demandUsefulOwnerSet := usefulEntry.ownerSet
      demandUsefulOwnerTag := usefulEntry.ownerTag
      demandUsefulOwnerKind := usefulEntry.ownerKind
      demandUnusedPending := unused
      demandUnusedOwnerSet := sweepEntry.ownerSet
      demandUnusedOwnerTag := sweepEntry.ownerTag
      demandUnusedOwnerKind := sweepEntry.ownerKind

      when (sameSet) {
        demandFeedbackWrite0Valid := useful || unused
        demandFeedbackWrite0Set := demandMatchSet
        demandFeedbackWrite0Row := combinedCleared
        demandFeedbackWrite1Valid := false.B
      }.otherwise {
        demandFeedbackWrite0Valid := useful
        demandFeedbackWrite0Set := demandMatchSet
        demandFeedbackWrite0Row := matchCleared
        demandFeedbackWrite1Valid := unused
        demandFeedbackWrite1Set := demandSweepIndex(7, 2)
        demandFeedbackWrite1Row := sweepCleared
      }

      val retireCount = PopCount(VecInit(Seq(useful, unused)))
      feedbackExpiryPulse := unused
      feedbackRetireCount := retireCount
      feedbackOccupancyDecrement := retireCount
      when (useful || unused) {
        state := sDemandFeedbackWrite0
      }.otherwise {
        completeDemand()
      }
    }

    is(sDemandFeedbackWrite0) {
      val qualityReadNeeded = demandUsefulPending || demandUnusedPending
      val feedbackWriteNeeded = demandFeedbackWrite0Valid
      qualityReadValid := qualityReadNeeded
      qualityReadSet := Mux(
        demandUsefulPending,
        demandUsefulOwnerSet,
        demandUnusedOwnerSet
      )
      feedbackWriteValid := feedbackWriteNeeded
      feedbackTable.io.w.req.bits.setIdx := demandFeedbackWrite0Set
      feedbackTable.io.w.req.bits.data := demandFeedbackWrite0Row
      val qualityReadDone = !qualityReadNeeded || qualityReadFire
      val feedbackWriteDone = !feedbackWriteNeeded || feedbackWriteFire
      when (qualityReadDone && feedbackWriteDone) {
        state := sDemandFeedbackWrite1
      }
    }

    is(sDemandFeedbackWrite1) {
      when (demandFeedbackWrite1Valid) {
        feedbackWriteValid := true.B
        feedbackTable.io.w.req.bits.setIdx := demandFeedbackWrite1Set
        feedbackTable.io.w.req.bits.data := demandFeedbackWrite1Row
        when (feedbackWriteFire) {
          demandPrefetchedQualityRow := qualityTable.io.r.resp.data
          demandQualityRowPrefetched := demandUsefulPending || demandUnusedPending
          when (demandUsefulPending) {
            state := sDemandUsefulQualityEval
          }.elsewhen (demandUnusedPending) {
            state := sDemandUnusedQualityEval
          }.otherwise {
            completeDemand()
          }
        }
      }.otherwise {
        demandPrefetchedQualityRow := qualityTable.io.r.resp.data
        demandQualityRowPrefetched := demandUsefulPending || demandUnusedPending
        when (demandUsefulPending) {
          state := sDemandUsefulQualityEval
        }.elsewhen (demandUnusedPending) {
          state := sDemandUnusedQualityEval
        }.otherwise {
          completeDemand()
        }
      }
    }

    is(sDemandUsefulQualityRead) {
      qualityReadValid := true.B
      qualityReadSet := demandUsefulOwnerSet
      when (qualityReadFire) {
        state := sDemandUsefulQualityEval
      }
    }

    is(sDemandUsefulQualityEval) {
      val row = Mux(
        demandQualityRowPrefetched,
        demandPrefetchedQualityRow,
        qualityTable.io.r.resp.data
      )
      val hitWay = findQualityWay(row, demandUsefulOwnerTag, demandUsefulOwnerKind)
      val ownerHit = hitWay =/= qualityWays.U
      val oldEntry = row(hitWay(1, 0))
      val outcome = applyOutcome(oldEntry, true.B)
      val updatedRow = WireInit(row)
      updatedRow(hitWay(1, 0)).useful := outcome._1
      updatedRow(hitWay(1, 0)).unused := outcome._2
      updatedRow(hitWay(1, 0)).resolved := outcome._3
      updatedRow(hitWay(1, 0)).state := outcome._4

      feedbackUsefulPulse := ownerHit
      feedbackOwnerMissPulse := !ownerHit
      demandQualityRowPrefetched := false.B
      when (ownerHit) {
        stateTransitionCount := outcomeTransitionCount(true.B, oldEntry.state, outcome)
        observeToOpenCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateObserve, stateOpen)
        observeToBlockCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateObserve, stateBlock)
        openToBlockCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateOpen, stateBlock)
        blockToOpenCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateBlock, stateOpen)
        qualityWriteSet := demandUsefulOwnerSet
        qualityWriteRow := updatedRow
        state := sDemandUsefulQualityWrite
      }.otherwise {
        routeAfterUseful()
      }
    }

    is(sDemandUsefulQualityWrite) {
      qualityWriteValid := true.B
      when (qualityWriteFire) {
        routeAfterUseful()
      }
    }

    is(sDemandUnusedQualityRead) {
      qualityReadValid := true.B
      qualityReadSet := demandUnusedOwnerSet
      when (qualityReadFire) {
        state := sDemandUnusedQualityEval
      }
    }

    is(sDemandUnusedQualityEval) {
      val row = Mux(
        demandQualityRowPrefetched,
        demandPrefetchedQualityRow,
        qualityTable.io.r.resp.data
      )
      val hitWay = findQualityWay(row, demandUnusedOwnerTag, demandUnusedOwnerKind)
      val ownerHit = hitWay =/= qualityWays.U
      val oldEntry = row(hitWay(1, 0))
      val outcome = applyOutcome(oldEntry, false.B)
      val updatedRow = WireInit(row)
      updatedRow(hitWay(1, 0)).useful := outcome._1
      updatedRow(hitWay(1, 0)).unused := outcome._2
      updatedRow(hitWay(1, 0)).resolved := outcome._3
      updatedRow(hitWay(1, 0)).state := outcome._4

      feedbackUnusedPulse := ownerHit
      feedbackOwnerMissPulse := !ownerHit
      demandQualityRowPrefetched := false.B
      when (ownerHit) {
        stateTransitionCount := outcomeTransitionCount(true.B, oldEntry.state, outcome)
        observeToOpenCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateObserve, stateOpen)
        observeToBlockCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateObserve, stateBlock)
        openToBlockCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateOpen, stateBlock)
        blockToOpenCount := outcomeTransitionCount(
          true.B, oldEntry.state, outcome, stateBlock, stateOpen)
        qualityWriteSet := demandUnusedOwnerSet
        qualityWriteRow := updatedRow
        state := sDemandUnusedQualityWrite
      }.otherwise {
        completeDemand()
      }
    }

    is(sDemandUnusedQualityWrite) {
      qualityWriteValid := true.B
      when (qualityWriteFire) {
        completeDemand()
      }
    }
  }

  private val candidateInputCount = PopCount(candidateInputs)
  private val candidateAcceptCount = PopCount(candidateAccepted)
  private val candidateNotAdmitted = VecInit(io.candidate.zip(candidateAccepted).map {
    case (candidate, accepted) => io.enable && candidate.valid && !accepted
  })
  private val candidateNotAdmittedCount = PopCount(candidateNotAdmitted)
  private val dualCandidateInput = candidateInputs.asUInt.andR
  private val candidateDualCapture = candidateAccepted.asUInt.andR
  private val engineBusy = state =/= sIdle || responseValid
  private val qualitySramAccess = qualityReadFire || qualityWriteFire
  private val feedbackSramAccess = feedbackReadFire || feedbackWriteFire
  private val feedbackUnknownCount = feedbackReplacePulse.asUInt +& feedbackOwnerMissPulse.asUInt

  // CQF replay traces are intentionally sparse.  Inputs are logged only when
  // an event is offered, decisions only when they handshake, and ready/enable
  // only when those control values change.  No trace signal participates in a
  // ready/valid path or in the CQF state machine.
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
  replayDemandInput.ready := false.B
  replayDemandInput.fire := io.demand.valid && io.demandAccept
  replayDemandInput.accepted := io.demandAccept
  replayDemandInput.demandLine := io.demand.bits
  replayInputTable.log(
    replayDemandInput,
    en = io.demand.valid,
    site = "demand",
    clock,
    reset
  )

  for (k <- 0 until 2) {
    val replayCandidateInput = WireInit(0.U.asTypeOf(new CqfReplayInputEntry))
    replayCandidateInput.cycle := cycleCounter
    replayCandidateInput.hartId := cacheParams.hartId.U
    replayCandidateInput.eventType := CqfReplayEventType.Candidate.U
    replayCandidateInput.port := k.U
    replayCandidateInput.enable := io.enable
    replayCandidateInput.valid := io.candidate(k).valid
    replayCandidateInput.ready := io.candidate(k).ready
    replayCandidateInput.fire := io.candidate(k).fire
    replayCandidateInput.accepted := io.candidate(k).fire
    replayCandidateInput.candidate := io.candidate(k).bits
    replayInputTable.log(
      replayCandidateInput,
      en = io.candidate(k).valid,
      site = s"candidate$k",
      clock,
      reset
    )

    val replayDecision = WireInit(0.U.asTypeOf(new CqfReplayDecisionEntry))
    replayDecision.cycle := cycleCounter
    replayDecision.hartId := cacheParams.hartId.U
    replayDecision.port := k.U
    replayDecision.valid := io.decision(k).valid
    replayDecision.ready := io.decision(k).ready
    replayDecision.fire := io.decision(k).fire
    replayDecision.allow := io.decision(k).bits.allow
    replayDecision.sampled := io.decision(k).bits.sampled
    replayDecision.feedbackInserted := io.decision(k).bits.feedbackInserted
    replayDecisionTable.log(
      replayDecision,
      en = io.decision(k).fire,
      site = s"decision$k",
      clock,
      reset
    )

    val previousDecisionReady = RegNext(io.decision(k).ready, false.B)
    val replayReady = WireInit(0.U.asTypeOf(new CqfReplayReadyEntry))
    replayReady.cycle := cycleCounter
    replayReady.hartId := cacheParams.hartId.U
    replayReady.port := k.U
    replayReady.ready := io.decision(k).ready
    replayReadyTable.log(
      replayReady,
      en = !reset.asBool && io.decision(k).ready =/= previousDecisionReady,
      site = s"decision_ready$k",
      clock,
      reset
    )
  }

  private val previousEnable = RegNext(io.enable, false.B)
  private val replayControl = WireInit(0.U.asTypeOf(new CqfReplayControlEntry))
  replayControl.cycle := cycleCounter
  replayControl.hartId := cacheParams.hartId.U
  replayControl.enable := io.enable
  replayControlTable.log(
    replayControl,
    en = !reset.asBool && io.enable =/= previousEnable,
    site = "enable",
    clock,
    reset
  )

  XSPerfAccumulate("cqf_demand_input", demandInput)
  XSPerfAccumulate("cqf_demand_enqueue", demandEnqueue)
  XSPerfAccumulate("cqf_demand_accept", demandEnqueue)
  XSPerfAccumulate("cqf_demand_processed", demandProcessedPulse)
  // Compatibility aliases retained for existing RTL result parsers.
  XSPerfAccumulate("cqf_demand", demandEnqueue)
  XSPerfAccumulate("cqf_demand_drop", demandOverflowDrop)
  XSPerfAccumulate("cqf_demand_overflow_drop", demandOverflowDrop)
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
  // No native request is dropped at CQF admission; gates count final overflow.
  XSPerfAccumulate("cqf_candidate_large_drop", false.B)
  XSPerfAccumulate("cqf_candidate_small_drop", false.B)
  XSPerfAccumulate("cqf_candidate_drop", false.B)
  XSPerfAccumulate("cqf_candidate_dual_input", dualCandidateInput)
  XSPerfAccumulate("cqf_candidate_dual_capture", candidateDualCapture)
  // Compatibility drop names now mean real loss, which admission never causes.
  XSPerfAccumulate("cqf_candidate_dual_partial_drop", false.B)
  XSPerfAccumulate("cqf_candidate_dual_full_drop", false.B)
  XSPerfAccumulate("cqf_candidate_dual_partial_bypass",
    dualCandidateInput && candidateAccepted.asUInt.xorR)
  XSPerfAccumulate("cqf_candidate_dual_full_bypass",
    dualCandidateInput && !candidateAccepted.asUInt.orR)
  XSPerfAccumulate("cqf_candidate_service", candidateCompletePulse)
  XSPerfAccumulate("cqf_candidate_allow", candidateAllowPulse)
  XSPerfAccumulate("cqf_candidate_suppress", candidateSuppressPulse)
  XSPerfAccumulate("cqf_candidate_policy_allow", candidateAllowPulse)
  XSPerfAccumulate("cqf_candidate_policy_suppress", candidateSuppressPulse)
  XSPerfAccumulate("cqf_quality_hit", qualityHitPulse)
  XSPerfAccumulate("cqf_quality_allocate", qualityAllocatePulse)
  XSPerfAccumulate("cqf_quality_replace", qualityReplacePulse)
  XSPerfAccumulate("cqf_feedback_selected", feedbackSelectedPulse)
  XSPerfAccumulate("cqf_feedback_insert", feedbackInsertPulse)
  XSPerfAccumulate("cqf_feedback_coalesce", feedbackCoalescePulse)
  XSPerfAccumulate("cqf_feedback_replace", feedbackReplacePulse)
  XSPerfAccumulate("cqf_feedback_conflict", feedbackReplacePulse)
  XSPerfAccumulate("cqf_feedback_useful", feedbackUsefulPulse)
  XSPerfAccumulate("cqf_feedback_unused", feedbackUnusedPulse)
  XSPerfAccumulate("cqf_feedback_expiry", feedbackExpiryPulse)
  XSPerfAccumulate("cqf_feedback_expiry_unused", feedbackUnusedPulse)
  XSPerfAccumulate("cqf_feedback_expiry_owner_miss",
    feedbackOwnerMissPulse && state === sDemandUnusedQualityEval)
  XSPerfAccumulate("cqf_feedback_retire", feedbackRetireCount)
  XSPerfAccumulate("cqf_feedback_owner_miss", feedbackOwnerMissPulse)
  XSPerfAccumulate("cqf_feedback_orphan_outcome", feedbackOwnerMissPulse)
  XSPerfAccumulate("cqf_feedback_unknown", feedbackUnknownCount)
  XSPerfAccumulate("cqf_state_transition", stateTransitionCount)
  XSPerfAccumulate("cqf_observe_to_open", observeToOpenCount)
  XSPerfAccumulate("cqf_observe_to_block", observeToBlockCount)
  XSPerfAccumulate("cqf_open_to_block", openToBlockCount)
  XSPerfAccumulate("cqf_block_to_open", blockToOpenCount)
  XSPerfAccumulate("cqf_feedback_occupancy_sum", feedbackOccupancy)
  XSPerfMax("cqf_feedback_occupancy", feedbackOccupancy, true.B)
  XSPerfAccumulate("cqf_engine_busy_cycles", engineBusy)
  XSPerfAccumulate("cqf_event_fifo_occupancy_sum", eventQueue.io.count)
  XSPerfMax("cqf_event_fifo_occupancy", eventQueue.io.count, true.B)
  XSPerfMax("cqf_event_fifo_occupancy_max", eventQueue.io.count, true.B)
  XSPerfAccumulate("cqf_quality_sram_read", qualityReadFire)
  XSPerfAccumulate("cqf_quality_sram_write", qualityWriteFire)
  XSPerfAccumulate("cqf_feedback_sram_read", feedbackReadFire)
  XSPerfAccumulate("cqf_feedback_sram_write", feedbackWriteFire)
  XSPerfAccumulate("cqf_dual_sram_access", qualitySramAccess && feedbackSramAccess)
  XSPerfAccumulate("cqf_candidate_latency_sum", Mux(candidateCompletePulse, candidateLatency, 0.U))
  XSPerfMax("cqf_candidate_latency", candidateLatency, candidateCompletePulse)
  XSPerfMax("cqf_candidate_latency_max", candidateLatency, candidateCompletePulse)
  XSPerfAccumulate("cqf_demand_latency_sum", Mux(demandProcessedPulse, demandLatency, 0.U))
  XSPerfMax("cqf_demand_latency", demandLatency, demandProcessedPulse)
  XSPerfMax("cqf_demand_latency_max", demandLatency, demandProcessedPulse)

  when (!reset.asBool) {
    assert(demandInput === (demandEnqueue || demandOverflowDrop),
      "CQF demand input must be either enqueued or explicitly dropped")
    for (k <- 0 until 2) {
      assert(candidateInputs(k) === (candidateAccepted(k) || candidateNotAdmitted(k)),
        "CQF candidate input must be admitted or handled by gate fail-open")
    }
    assert(candidateInputCount === candidateAcceptCount +& candidateNotAdmittedCount,
      "CQF candidate totals do not match admission partition")
    assert(candidateCompletePulse === (candidateAllowPulse || candidateSuppressPulse),
      "CQF completed candidate must be policy-allowed or policy-suppressed")
    assert(!(candidateAllowPulse && candidateSuppressPulse),
      "CQF candidate cannot be both policy-allowed and policy-suppressed")
    assert(!(qualityReadFire && qualityWriteFire),
      "CQF Quality 1RW SRAM cannot read and write in one cycle")
    assert(!(feedbackReadFire && feedbackWriteFire),
      "CQF Feedback 1RW SRAM cannot read and write in one cycle")
    assert(!(feedbackOccupancyIncrement && feedbackOccupancyDecrement.orR),
      "CQF serialized engine cannot allocate and retire Feedback together")
    assert(feedbackOccupancy <= (feedbackSets * feedbackWays).U,
      "CQF Feedback occupancy overflow")
  }
}
