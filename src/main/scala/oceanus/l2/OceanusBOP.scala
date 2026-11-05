/*
 * Semantic ledger
 *
 * This module is the oceanus implementation of BOP (Michaud, HPCA '16).
 * Every difference from the original algorithm semantics must be recorded here
 * in the following fixed four-line format:
 *   [Deviation N] One-sentence summary
 *   Original:       What the paper does
 *   This implementation: What this implementation does
 *   Reason:         Why the difference is necessary
 *   Status:         Verified / pending verification (method) / carried to a later step
 *
 * 1. Anything not recorded here is considered consistent with the original.
 * 2. Record a deviation only after reading the corresponding passage in the
 *    paper; mark recollections as "not checked against the paper."
 * 3. Moving an entry to "Verified" requires concrete observed quantities, not
 *    merely that a test ran successfully.
 * 4. If original semantics are restored, retain the history as
 *    [Resolved N] and state the step in which it was resolved.
 *
 * [Implementation detail 1 — not a deviation] One global instance receives the complete
 *                 physical-address training stream after per-slice arbitration.
 * Original:       One BOP instance observes the complete address stream.
 * This implementation: Each slice emits a training pulse and L2Top merges those pulses
 *                 into one global BOP instance; BOP learns the allocator-captured physical address.
 * Reason:         This is the same global-instance topology as the coupledL2 reference:
 *                 CoupledL2.scala:491-498 creates one Prefetcher and connects its
 *                 banked train ports, while CoupledL2.scala:597-604 connects each
 *                 slice's pipelined train stream to one bank. MainPipe.scala:757
 *                 produces one scalar train.valid per slice MainPipe each cycle.
 *                 XOR-hashed slicing is why keeping the stream global is important.
 * Status:         Verified by temporary commit 2b16ca1: dirty-victim produced five
 *                 train.fire addresses 0x8000/0xa000/0xc000/0xe000/0x10000, each
 *                 matching the corresponding request physical address. The original
 *                 step-2a evidence also remains valid
 *                 (bop-2a-dirty-victim-20260917.log:43-252).
 *
 * [Implementation detail 2 — not a deviation] RR writes come from the DelayQueue.
 * Reference:      coupledL2 PBOP connects delayQueue.io.out directly to rrTable.io.w
 *                 (BestOffsetPrefetch.scala:820-821); io.resp only drives the RR
 *                 write-path ready signal (:847).
 * This implementation: Training events likewise enter the DelayQueue and write the
 *                 RR table when their delay expires.
 * Reason:         This is structurally consistent with the coupledL2 PBOP body.
 * Note:           The BOP paper describes an enabled-state path in which prefetch
 *                 completion updates the RR table. That paper-level difference is
 *                 retained here as a note and has not been checked against the
 *                 original paper in this step.
 * Status:         Confirmed structurally equivalent to coupledL2 PBOP. Step 2d no
 *                 longer requires a refill-completion upload path for RR writes.
 *
 * [Implementation detail 3 — not a deviation] Per-slice arbitration, finite buffering, and silent training loss are structurally consistent with the coupledL2 reference implementation.
 * Reference:      coupledL2 has one global prefetcher. Per-slice training ports are merged into its single training port through utility/FastArbiter.scala.
 * This implementation: L2Top instantiates one utility/FastArbiter per global BOP input
 *                 set. FastArbiter records unselected valid inputs in pendingMask and
 *                 uses pending-aware round-robin selection after each output fire
 *                 (utility/FastArbiter.scala:35-51). MainPipe.scala:722 generates
 *                 train.valid without consulting train.ready, and s3-to-s4 advancement
 *                 likewise does not depend on train.ready. Events whose valid pulse ends
 *                 before a pending grant are silently lost; Oceanus can also drop samples
 *                 at the DelayQueue input.
 * Reason:         Oceanus XOR-hashed slicing makes loss approximately uniform downsampling, while coupledL2 set-bit banking can bias loss toward contiguous address ranges. This depends on XOR hashing and must be revisited if slice mapping changes.
 * Status:         Confirmed structurally equivalent to the reference implementation; no step-2d fullDrops-versus-bestScore measurement is required.
 *
 * [Deviation 4] Concurrent training arbitration is only verified at the cross-slice boundary.
 * Original:       The reference implementation merges per-slice training streams through
 *                 FastArbiter, recording unselected valid inputs in pendingMask and
 *                 serving them in pending-aware round-robin order.
 * This implementation: L2Top connects every slice's bopTrain.valid/bits to one
 *                 FastArbiter input and connects its output to the single BOP train port
 *                 (L2Top.scala:128-133). However, each L2TSHRCtrl first collapses all
 *                 TSHR pulses in that slice with PriorityMux (L2TSHRCtrl.scala:79-81);
 *                 when two TSHRs assert together, only the lowest-index payload survives.
 *                 The production path therefore matches the reference only at the
 *                 slice-to-global-BOP boundary, not at the intra-slice boundary.
 * Reason:         Each TSHR's RBE independently gates its dequeue into its REQ vPipe
 *                 (L2TSHR.scala:1291-1309, 1357-1358; L2RBE.scala:40-67), and the
 *                 training pulse is generated from that vPipe's rxreq_fire, not from
 *                 allocator fire (L2TSHR.scala:462-464; L2VPipeREQ.scala:150-168).
 *                 There is no shared arbiter among TSHR RBEs; first dequeue is limited
 *                 by the directory response, while reuse dequeue is not. Thus a same-
 *                 slice PriorityMux collision is theoretically possible and can lose
 *                 one training pulse.
 * Status:         Serial and C1-C3 cases had candidateCycles=0; not observed. Impact is
 *                 an occasional lost training sample. No directed use case constructed.
 *
 * [Resolved 5] Training intake is no longer gated by prefetch request issue.
 * Reference:      coupledL2 PBOP uses the same (!req_valid || io.req.ready) term in
 *                 io.train.ready (BestOffsetPrefetch.scala:846).
 * Previous implementation: The same term gated canAcceptTrain, so output-side
 *                 backpressure could reject training.
 * Reason:         The coupledL2 output-side OverwriteQueue keeps enq.ready high and
 *                 prevents request-path occupancy from throttling PBOP training.
 * Resolution:     Commit b88f53d observes the selected OverwriteQueue enqueue-ready;
 *                 its overwrite queue keeps enq.ready high and drops the oldest item.
 * Status:         Resolved by the closed-loop and overwrite-queue tests.
 *
 * [Implementation detail 6 — not a deviation] roundBits is log2Ceil(roundMax + 1).
 * Reference:      coupledL2 PBOP uses log2Up(roundMax).
 * This implementation: One extra representable value is retained when roundMax is a
 *                 power of two; for roundMax=4, a 2-bit counter cannot satisfy round>=4.
 * Reason:         Prevent round counter overflow from disabling roundMax termination.
 * Status:         Intentional implementation difference.
 *
 * [Deviation 7] RR SRAM reset backpressure is propagated to training intake.
 * Reference:      coupledL2 RecentRequestTable leaves io.r.req.ready asserted while
 *                 its 256-set SRAMTemplate is clearing after reset.
 * This implementation: io.r.req.ready follows the RR SRAM read-port ready, so BOP
 *                 accepts no training during the 256-cycle RR clear window.
 * Reason:         Oceanus Directory initialization takes 128 cycles
 *                 (L2Directory.scala:286/305/323), shorter than the 256-cycle RR reset.
 *                 Training events can reach BOP during the system-level cycle 128-256
 *                 window, so preserving RR reset backpressure is required. PBOP does
 *                 not need this hardening because other coupledL2 SRAM resets last
 *                 longer; that premise does not hold in Oceanus. The 300-cycle L2Top
 *                 test warmup masks this window.
 * Status:         Verified by TestTop_OceanusBOPResetReady: resetCycles=256,
 *                 accepted=0, and train.ready recovers after the clear window.
 *
 * [Resolved 8] Demand hit now clears the prefetched bit after training.
 * Reference:      coupledL2 trains on the first demand Acquire hit using the old
 *                 prefetch bit (MainPipe.scala:750/771), then rewrites meta with
 *                 prefetch=false and accessed=true (MainPipe.scala:537/556/562,
 *                 Directory.scala:56).
 * Previous implementation: L2VPipeREQ only set prefetched=true on internal
 *                 prefetch refill; demand hits never cleared it, so every later demand
 *                 hit trained again until eviction.
 * Resolution:     Commit beb0090 trains from the old directory result, then writes
 *                 prefetched=false for a demand ReadShared/ReadUnique hit; internal
 *                 prefetch refill remains the only path that writes true.
 * Status:         Verified by prefetched-clear-before-fix-20260921.log:48
 *                 (baseline=0, afterSecond=2, delta=2) and
 *                 prefetched-clear-after-fix-20260921.log:45
 *                 (baseline=0, afterSecond=1, delta=1).
 *
 * [Implementation detail 9 — relative to frozen e2dc7b6, paper not checked]
 *                 Training covers MakeUnique (训练覆盖 MakeUnique): miss and prefetched hit.
 * Reference:      coupledL2 MainPipe:173 includes AcquirePerm; :759-766 trains
 *                 needHint Acquire/Get misses or prefetched hits, plus mergeA.
 *                 Its direct A-hit MetaEntry defaults prefetch=false (:538,557;
 *                 Directory:56). MSHR hit refill instead retains meta_pft (MSHR:795).
 * This implementation: RS/RU/MU demand hits train using the old prefetched bit
 *                 and clear it at the same edge. Frozen e2dc7b6 cleared only RS/RU.
 *                 RS/RU/MU misses train as well; frozen e2dc7b6 omitted MU miss/hit.
 *                 AcquirePerm is explicitly included in the reference miss predicate.
 * Reason:         MU corresponds to AcquirePerm. Base has no corresponding MSHR
 *                 pipeline; hits requiring permission upgrades also consume the bit.
 * Status:         Verified by TestTop_L2Prefetched SC/UC RS/RU/MU: first hit
 *                 reads SRAM prefetched=1 with trainDelta=1; second hit reads
 *                 0 with trainDelta=0. External Stash train=0; replacement
 *                 checks victim bit=1, new-tag mask=1/data=0, SRAM readback=0.
 *                 MU miss SC/UC: cold miss trainDelta=1/internalFills=1, subsequent
 *                 ordinary hit trainDelta=0. A/B six groups have identical counts.
 *
 * [Resolved D4] PBOP now samples the delay_latency CSR on queue admission.
 * Reference:      BestOffsetPrefetch.scala:628-637 (test ReferenceVBOP.scala:670)
 *                 selects nonzero CSR, otherwise configured latency (default 300).
 * Previous implementation: PBOP used only delayQueueLatency; VBOP has consumed
 *                 the CSR since f2478aa8. The slice-3 deferred-control note was stale.
 * Resolution:     L2Top connects the same CSR to PBOP and VBOP; zero preserves
 *                 the constructor fallback, including accelerated test settings.
 * Evidence:       L1 VBOP same-value differential 600/600; CSR=0 falls back to 300.
 *                 TestTop_OceanusBOPDelay compares 0/300/37 against reference.
 *
 * [Implementation detail — not a deviation] The coupledL2 PBOP structure is flattened.
 * Reference:      coupledL2 separates RecentRequestTable, OffsetScoreTable, and
 *                 DelayQueue modules, uses Constantin controls, and retains ChiselDB
 *                 logging and performance counters.
 * This implementation: Those modules are inlined, Constantin controls are constructor
 *                 parameters, logging and counters are omitted, and train/req IO carries
 *                 complete physical-address UInt values.
 * Reason:         Oceanus uses a global full-address topology described by Deviation 1.
 * Status:         Intentional structural difference.
 */
package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, XSPerfAccumulate}
import utility.sram.{SRAMTemplate, SramBroadcastBundle}

case class OceanusBOPParameters(
  rrTableEntries: Int = 256,
  rrTagBits: Int = 12,
  scoreBits: Int = 5,
  roundMax: Int = 50,
  badScore: Int = 1,
  delayQueueEntries: Int = 16,
  delayQueueLatency: Int = 300,
  delayQueueMaxLatency: Int = 1024,
  enableStudentCover: Boolean = true,
  studentPoolSize: Int = 8,
  studentFilterTableBits: Int = 2048,
  studentFilterEntryBits: Int = 64,
  studentHashMode: String = "bop_rr",
  studentCovThreshold: Int = 5,
  issueGateEnable: Boolean = true,
  issueConfThreshold: Int = 14,
  offsetList: Seq[Int] = Seq(
    -32, -30, -27, -25, -24, -20, -18, -16, -15,
    -12, -10, -9, -8, -6, -5, -4, -3, -2, -1,
    1, 2, 3, 4, 5, 6, 8, 9, 10,
    12, 15, 16, 18, 20, 24, 25, 27, 30
  )
)

object OceanusBOPParameters {
  private[l2] def geometry(b: OceanusBOPParameters): OceanusVBOPParameters = OceanusVBOPParameters(
    virtualTrain=false, rrTableEntries=b.rrTableEntries, rrTagBits=b.rrTagBits,
    scoreBits=b.scoreBits, roundMax=b.roundMax, badScore=b.badScore,
    dQEntries=b.delayQueueEntries, dQLatency=b.delayQueueLatency, dQMaxLatency=b.delayQueueMaxLatency,
    crossPage=false, useDelayOut=false, enableStudentCover=b.enableStudentCover,
    studentPoolSize=b.studentPoolSize, studentFilterTableBits=b.studentFilterTableBits,
    studentFilterEntryBits=b.studentFilterEntryBits, studentHashMode=b.studentHashMode,
    studentCovThreshold=b.studentCovThreshold, issueGateEnable=b.issueGateEnable,
    issueConfThreshold=b.issueConfThreshold, offsetList=b.offsetList)
}

// Upstream dfd3edcf: PA learner and one-entry flow-through output skid.
// Shared learner implementation is parameterized by PA rather than VA geometry.
class OceanusBOP(val params: OceanusBOPParameters = OceanusBOPParameters())(implicit outer: Parameters)
  extends OceanusVBOPModule()(OceanusVBOP.configured(outer, OceanusBOPParameters.geometry(params))) {
  val io = IO(new Bundle {
    val delayLatency = Input(UInt(10.W))
    val train = Flipped(Decoupled(UInt(fullAddressBits.W)))
    val req = Decoupled(UInt(fullAddressBits.W))
    val crossPageDrop = Output(Bool())
    val disabledDrop = Output(Bool())
    val delayQueueFullDrop = Output(Bool())
  })
  val delayQueue = Module(new OceanusVDelayQueue("oceanus_pbop"))
  val recentRequestTable = Module(new OceanusVRecentRequestTable("oceanus_pbop"))
  val scoreTable = Module(new OceanusVOffsetScoreTable("oceanus_pbop"))
  val student = if (enableStudentCover) Some(Module(new OceanusVStudentCoverageLearner("oceanus_pbop"))) else None
  val studentTrainReady = student.map(_.io.train.ready).getOrElse(true.B)
  val teacherOffset = scoreTable.io.prefetchOffset
  val teacherDisable = scoreTable.io.prefetchDisable
  val studentSelectedEnable = student.map(_.io.selectedEnable).getOrElse(false.B)
  val issueOffset = Mux(studentSelectedEnable, student.map(_.io.selectedOffset).getOrElse(teacherOffset), teacherOffset)
  val issueGate = if (issueGateEnable) issueConf(scoreTable.io.phaseEndPulse, teacherDisable) else true.B
  val issueEnable = (studentSelectedEnable || !teacherDisable) && issueGate
  val cstEnable = Constantin.createRecord("pbop_enable0", initValue=1)
  val enable = cstEnable.orR
  val newAddress = io.train.bits + signedExtend(issueOffset << offsetBits, fullAddressBits)
  val crossPageNow = getPPN(newAddress) =/= getPPN(io.train.bits)
  class Request extends Bundle {
    val address = UInt(fullAddressBits.W)
    val issueOffset = SInt(offsetWidth.W)
  }
  val request = Reg(new Request)
  val requestValid = RegInit(false.B)
  val skid = Module(new Queue(new Request, 1, pipe=false, flow=true))
  val s1Ready = skid.io.enq.ready || !requestValid
  io.train.ready := delayQueue.io.in.ready && scoreTable.io.req.ready && studentTrainReady && s1Ready
  recentRequestTable.io.r <> scoreTable.io.test
  recentRequestTable.io.w <> delayQueue.io.out
  delayQueue.io.pfCtrlOfDelayLatency := io.delayLatency
  delayQueue.io.in.valid := io.train.valid && scoreTable.io.req.ready && studentTrainReady && s1Ready
  delayQueue.io.in.bits := get_block_addr(io.train.bits)
  scoreTable.io.req.valid := io.train.valid && delayQueue.io.in.ready && studentTrainReady && s1Ready
  scoreTable.io.req.bits := io.train.bits
  student.foreach { learner =>
    learner.io.train.valid := io.train.valid && delayQueue.io.in.ready && scoreTable.io.req.ready && s1Ready
    learner.io.train.bits := io.train.bits
    learner.io.delayOut.valid := delayQueue.io.out.fire
    learner.io.delayOut.bits := delayQueue.io.out.bits
    learner.io.teacherPhaseEnd := scoreTable.io.phaseEndPulse
    learner.io.teacherBestOffset := scoreTable.io.phaseBestOffsetCommitted
    learner.io.teacherIssueEnable := scoreTable.io.phaseIssueEnable
  }
  when(io.train.fire) {
    request.address := newAddress
    request.issueOffset := issueOffset.asSInt
    requestValid := enable && !crossPageNow && issueEnable
  }.elsewhen(s1Ready && requestValid) { requestValid := false.B }
  skid.io.enq.valid := requestValid
  skid.io.enq.bits := request
  io.req.valid := enable && skid.io.deq.valid
  io.req.bits := skid.io.deq.bits.address
  skid.io.deq.ready := io.req.ready
  io.crossPageDrop := io.train.fire && crossPageNow
  io.disabledDrop := io.train.fire && !crossPageNow && !issueEnable
  io.delayQueueFullDrop := delayQueue.io.in.valid && delayQueue.full
  XSPerfAccumulate("bop_train", io.train.fire)
  XSPerfAccumulate("bop_req", io.req.fire)
  XSPerfAccumulate("bop_student_takeover", io.train.fire && studentSelectedEnable)
  emitOffsetDistCounters("prefetch_sent_issue_offset", skid.io.deq.bits.issueOffset, io.req.fire)
}

private[l2] class OceanusBOPDelayQueue(name: String, params: OceanusBOPParameters)(implicit outer: Parameters)
  extends OceanusVBOPModule()(OceanusVBOP.configured(outer, OceanusBOPParameters.geometry(params))) {
  val io = IO(new Bundle {
    val delayLatency = Input(UInt(10.W))
    val in = Flipped(Decoupled(UInt(noOffsetAddrBits.W)))
    val out = Decoupled(UInt(fullAddrBits.W))
    val fullDrop = Output(Bool())
  })
  val queue = Module(new OceanusVDelayQueue(name))
  queue.io.pfCtrlOfDelayLatency := io.delayLatency
  queue.io.in <> io.in
  io.out <> queue.io.out
  io.fullDrop := queue.io.in.valid && queue.full
}
