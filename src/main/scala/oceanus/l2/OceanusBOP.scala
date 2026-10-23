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
import utility.{ChiselDB, XSPerfAccumulate}
import utility.sram.SRAMTemplate

case class OceanusBOPParameters(
  rrTableEntries: Int = 256,
  rrTagBits: Int = 12,
  scoreBits: Int = 5,
  roundMax: Int = 50,
  badScore: Int = 1,
  delayQueueEntries: Int = 16,
  delayQueueLatency: Int = 300,
  delayQueueMaxLatency: Int = 1024,
  offsetList: Seq[Int] = Seq(
    -32, -30, -27, -25, -24, -20, -18, -16, -15,
    -12, -10, -9, -8, -6, -5, -4, -3, -2, -1,
    1, 2, 3, 4, 5, 6, 8, 9, 10,
    12, 15, 16, 18, 20, 24, 25, 27, 30
  )
)

private abstract class OceanusBOPModule(val bopParams: OceanusBOPParameters)(implicit val p: Parameters)
  extends Module with HasL2Params {
  protected val addressWidth = paramL2.physicalAddrWidth
  protected val noOffsetAddressWidth = addressWidth - offsetBits
  protected val rrIndexBits = log2Ceil(bopParams.rrTableEntries)
  protected val offsetWidth = log2Ceil(bopParams.offsetList.max) + 2
  protected val scoreTableIndexBits = log2Ceil(bopParams.offsetList.size)
  protected val roundBits = log2Ceil(bopParams.roundMax + 1)
  protected val scoreMax = (1 << bopParams.scoreBits) - 1
}

private class OceanusBOPTestOffsetReq(
  addressWidth: Int,
  offsetWidth: Int,
  scoreTableIndexBits: Int
) extends Bundle {
  val addr = UInt(addressWidth.W)
  val testOffset = UInt(offsetWidth.W)
  val ptr = UInt(scoreTableIndexBits.W)
}

private class OceanusBOPTestOffsetResp(scoreTableIndexBits: Int) extends Bundle {
  val ptr = UInt(scoreTableIndexBits.W)
  val hit = Bool()
}

private class OceanusBOPTestOffsetBundle(
  addressWidth: Int,
  offsetWidth: Int,
  scoreTableIndexBits: Int
) extends Bundle {
  val req = DecoupledIO(new OceanusBOPTestOffsetReq(addressWidth, offsetWidth, scoreTableIndexBits))
  val resp = Flipped(ValidIO(new OceanusBOPTestOffsetResp(scoreTableIndexBits)))
}

private class OceanusBOPRecentRequestTable(name: String, params: OceanusBOPParameters)(implicit p: Parameters)
  extends OceanusBOPModule(params) {
  require(isPow2(bopParams.rrTableEntries) && bopParams.rrTableEntries >= 2)

  val io = IO(new Bundle {
    val w = Flipped(DecoupledIO(UInt(addressWidth.W)))
    val r = Flipped(new OceanusBOPTestOffsetBundle(addressWidth, offsetWidth, scoreTableIndexBits))
  })

  private def lineAddress(address: UInt): UInt = address(addressWidth - 1, offsetBits)
  private def hash1(address: UInt): UInt = lineAddress(address)(rrIndexBits - 1, 0)
  private def hash2(address: UInt): UInt = lineAddress(address)(2 * rrIndexBits - 1, rrIndexBits)
  private def index(address: UInt): UInt = hash1(address) ^ hash2(address)
  private def tag(address: UInt): UInt = lineAddress(address)(bopParams.rrTagBits + rrIndexBits - 1, rrIndexBits)
  private def signedExtend(value: UInt): UInt =
    Cat(Fill(addressWidth - value.getWidth, value(value.getWidth - 1)), value)

  private class RecentRequestEntry extends Bundle {
    val valid = Bool()
    val tag = UInt(bopParams.rrTagBits.W)
  }

  private val rrTable = Module(new SRAMTemplate(
    gen = new RecentRequestEntry,
    set = bopParams.rrTableEntries,
    way = 1,
    shouldReset = true,
    singlePort = true,
    hasMbist = paramL2.hasMbist,
    hasSramCtl = paramL2.hasSramCtl
  ))

  private val writeAddress = io.w.bits
  rrTable.io.w.req.valid := io.w.valid && !io.r.req.valid
  rrTable.io.w.req.bits.setIdx := index(writeAddress)
  rrTable.io.w.req.bits.data(0).valid := true.B
  rrTable.io.w.req.bits.data(0).tag := tag(writeAddress)

  private val readAddress = io.r.req.bits.addr - signedExtend(io.r.req.bits.testOffset << offsetBits)
  private val readData = Wire(new RecentRequestEntry)
  rrTable.io.r.req.valid := io.r.req.fire
  rrTable.io.r.req.bits.setIdx := index(readAddress)
  readData := rrTable.io.r.resp.data(0)

  assert(!RegNext(io.w.fire && io.r.req.fire), "single port SRAM should not read and write at the same time")

  private val stage0Valid = rrTable.io.r.req.fire
  private val stage1Valid = RegNext(stage0Valid, false.B)
  private val stage1Pointer = RegNext(io.r.req.bits.ptr)
  private val stage1Hit = readData.valid && readData.tag === RegNext(tag(readAddress))
  private val stage2Valid = RegNext(stage1Valid, false.B)

  io.w.ready := rrTable.io.w.req.ready && !io.r.req.valid
  io.r.req.ready := rrTable.io.r.req.ready
  io.r.resp.valid := stage2Valid
  io.r.resp.bits.ptr := RegEnable(stage1Pointer, stage1Valid)
  io.r.resp.bits.hit := RegEnable(stage1Hit, stage1Valid)

  private class WriteRecentRequestEntry extends Bundle {
    val addr = UInt(addressWidth.W)
  }
  private val writeRecentRequestTable = ChiselDB.createTable(
    name + "WriteRecentRequestTable",
    new WriteRecentRequestEntry,
    basicDB = false
  )
  private val writeLogEntry = Wire(new WriteRecentRequestEntry)
  writeLogEntry.addr := writeAddress
  writeRecentRequestTable.log(
    writeLogEntry,
    io.w.valid && !io.r.req.valid,
    site = "RecentRequestTable",
    clock,
    reset
  )
}

private class OceanusBOPOffsetScoreTable(name: String, params: OceanusBOPParameters)(implicit p: Parameters)
  extends OceanusBOPModule(params) {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(UInt(addressWidth.W)))
    val requestAvailable = Input(Bool())
    val prefetchOffset = Output(UInt(offsetWidth.W))
    val prefetchDisable = Output(Bool())
    val test = new OceanusBOPTestOffsetBundle(addressWidth, offsetWidth, scoreTableIndexBits)
  })

  private class ScoreTableEntry extends Bundle {
    val score = UInt(bopParams.scoreBits.W)
  }

  private val prefetchOffset = RegInit(2.U(offsetWidth.W))
  private val prefetchDisable = RegInit(false.B)
  private val scoreTable = RegInit(VecInit(Seq.fill(bopParams.offsetList.size)(0.U.asTypeOf(new ScoreTableEntry))))
  private val offsetList = WireInit(VecInit(bopParams.offsetList.map(_.S(offsetWidth.W).asUInt)))
  private val pointer = RegInit(0.U(scoreTableIndexBits.W))
  private val round = RegInit(0.U(roundBits.W))
  private val bestOffset = RegInit(2.U(offsetWidth.W))
  private val bestScore = RegInit(0.U(bopParams.scoreBits.W))
  private val testOffset = offsetList(pointer)

  private val idle :: learn :: Nil = Enum(2)
  private val state = RegInit(idle)
  private val isBad = bestScore < bopParams.badScore.U

  // Startup is identical to coupledL2 PBOP: BestOffsetPrefetch.scala:238,
  // 247,256,261-269 resets disable=false, bestScore=0, state=idle, then
  // assigns disable=(bestScore < badScore) at the first non-reset edge.
  // Its production PBOP overrides badScore=1 (Prefetcher.scala:274), as we
  // do by default: 0<1 disables issue during the first learning phase.
  // Reset disable=false does NOT mean every initial train issues a prefetch.
  // A test-only badScore=0 keeps startup enabled because 0<0 is false;
  // the comparison is strict <, not <=. No production startup exception.
  when(state === idle) {
    scoreTable.foreach(_.score := 0.U)
    pointer := 0.U
    round := 0.U
    bestScore := 0.U
    prefetchOffset := bestOffset
    prefetchDisable := isBad
    state := learn
  }

  when(state === learn) {
    when(io.test.req.fire) {
      val roundFinished = pointer === (bopParams.offsetList.size - 1).U
      pointer := Mux(roundFinished, 0.U, pointer + 1.U)
      round := Mux(roundFinished, round + 1.U, round)
    }

    when(round >= bopParams.roundMax.U) {
      state := idle
    }

    when(io.test.resp.valid && io.test.resp.bits.hit) {
      val oldScore = scoreTable(io.test.resp.bits.ptr).score
      val newScore = oldScore + 1.U
      val offset = offsetList(io.test.resp.bits.ptr)
      val renewOffset = newScore > bestScore
      scoreTable(io.test.resp.bits.ptr).score := newScore
      bestOffset := Mux(renewOffset, offset, bestOffset)
      bestScore := Mux(renewOffset, newScore, bestScore)
      when(newScore >= scoreMax.U) {
        state := idle
      }
    }
  }

  io.req.ready := state === learn && io.test.req.ready
  io.prefetchOffset := prefetchOffset
  io.prefetchDisable := prefetchDisable
  io.test.req.valid := state === learn && io.req.valid && io.requestAvailable
  io.test.req.bits.addr := io.req.bits
  io.test.req.bits.testOffset := testOffset
  io.test.req.bits.ptr := pointer

  XSPerfAccumulate("total_learn_phase", state === idle)
  XSPerfAccumulate("total_bop_disable", state === idle && isBad)
  XSPerfAccumulate("total_bop_high_confidence", state === idle && bestScore === scoreMax.U)

  for (offset <- bopParams.offsetList) {
    if (offset < 0) {
      XSPerfAccumulate(
        "best_offset_neg_" + (-offset).toString + "_learning_phases",
        Mux(state === idle, (bestOffset === offset.S(offsetWidth.W).asUInt).asUInt, 0.U)
      )
    } else {
      XSPerfAccumulate(
        "best_offset_pos_" + offset.toString + "_learning_phases",
        Mux(state === idle, (bestOffset === offset.U).asUInt, 0.U)
      )
    }
  }

  private class BopTrainEntry extends Bundle {
    val bestOffset = UInt(offsetWidth.W)
    val bestScore = UInt(bopParams.scoreBits.W)
    val offsets = Vec(bopParams.offsetList.size, UInt(offsetWidth.W))
    val scores = Vec(bopParams.offsetList.size, UInt(bopParams.scoreBits.W))
  }
  private val bopTrainTable = ChiselDB.createTable(name + "OffsetScoreTable", new BopTrainEntry, basicDB = false)
  private val trainLogEntry = Wire(new BopTrainEntry)
  trainLogEntry.bestOffset := bestOffset
  trainLogEntry.bestScore := bestScore
  for (index <- bopParams.offsetList.indices) {
    trainLogEntry.offsets(index) := offsetList(index)
    trainLogEntry.scores(index) := scoreTable(index).score
  }
  bopTrainTable.log(
    data = trainLogEntry,
    en = state === idle && !isBad,
    site = name + "OffsetScoreTable",
    clock,
    reset
  )
}

private class OceanusBOPDelayQueue(name: String, params: OceanusBOPParameters)(implicit p: Parameters)
  extends OceanusBOPModule(params) {
  require(isPow2(bopParams.delayQueueEntries) && bopParams.delayQueueEntries >= 2)
  require(bopParams.delayQueueLatency >= 0 && bopParams.delayQueueLatency < bopParams.delayQueueMaxLatency)

  val io = IO(new Bundle {
    val delayLatency = Input(UInt(10.W))
    val in = Flipped(DecoupledIO(UInt(noOffsetAddressWidth.W)))
    val out = DecoupledIO(UInt(addressWidth.W))
    val fullDrop = Output(Bool())
  })

  private val indexWidth = log2Ceil(bopParams.delayQueueEntries)
  private val latencyWidth = log2Ceil(bopParams.delayQueueMaxLatency)
  private class Entry extends Bundle {
    val addressNoOffset = UInt(noOffsetAddressWidth.W)
    val count = UInt(latencyWidth.W)
  }

  private val queue = RegInit(VecInit(Seq.fill(bopParams.delayQueueEntries)(0.U.asTypeOf(new Entry))))
  private val valid = RegInit(VecInit(Seq.fill(bopParams.delayQueueEntries)(false.B)))
  private val head = RegInit(0.U(indexWidth.W))
  private val tail = RegInit(0.U(indexWidth.W))
  private val empty = head === tail && !valid.last
  private val full = head === tail && valid.last
  private val outValid = !empty && !queue(head).count.orR && valid(head)

  when(io.in.valid && !full) {
    queue(tail).addressNoOffset := io.in.bits
    queue(tail).count := Mux(io.delayLatency.orR, io.delayLatency, bopParams.delayQueueLatency.U)
    valid(tail) := true.B
    tail := tail + 1.U
  }
  when(outValid && io.out.ready) {
    valid(head) := false.B
    head := head + 1.U
  }

  io.in.ready := true.B
  io.out.valid := outValid
  io.out.bits := Cat(queue(head).addressNoOffset, 0.U(offsetBits.W))
  io.fullDrop := io.in.valid && full

  for (index <- 0 until bopParams.delayQueueEntries) {
    when(queue(index).count.orR) {
      queue(index).count := queue(index).count - 1.U
    }
  }

  XSPerfAccumulate("full", full)
  XSPerfAccumulate("empty", empty)
  XSPerfAccumulate("entryNumber", PopCount(valid.asUInt))
  XSPerfAccumulate("inNumber", io.in.valid)
  XSPerfAccumulate("outNumber", io.out.valid)
}

class OceanusBOP(val bopParams: OceanusBOPParameters = OceanusBOPParameters())(implicit val p: Parameters)
  extends Module with HasL2Params {
  require(bopParams.roundMax >= 1)
  require(bopParams.badScore >= 0 && bopParams.badScore < (1 << bopParams.scoreBits))
  require(bopParams.offsetList.nonEmpty)

  private val addressWidth = paramL2.physicalAddrWidth
  private val offsetWidth = log2Ceil(bopParams.offsetList.max) + 2
  private val pageOffsetBits = 12

  val io = IO(new Bundle {
    val delayLatency = Input(UInt(10.W))
    val train = Flipped(Decoupled(UInt(addressWidth.W)))
    val req = Decoupled(UInt(addressWidth.W))
    val crossPageDrop = Output(Bool())
    val disabledDrop = Output(Bool())
    val delayQueueFullDrop = Output(Bool())
  })

  private def signedExtend(value: UInt): UInt =
    Cat(Fill(addressWidth - value.getWidth, value(value.getWidth - 1)), value)

  private val request = Reg(UInt(addressWidth.W))
  private val requestValid = RegInit(false.B)

  private val delayQueue = Module(new OceanusBOPDelayQueue("pbop", bopParams))
  delayQueue.io.delayLatency := io.delayLatency
  private val recentRequestTable = Module(new OceanusBOPRecentRequestTable("pbop", bopParams))
  private val scoreTable = Module(new OceanusBOPOffsetScoreTable("pbop", bopParams))

  private val prefetchOffset = scoreTable.io.prefetchOffset
  private val prefetchDisable = scoreTable.io.prefetchDisable
  private val oldAddress = io.train.bits
  private val oldAddressNoOffset = oldAddress(addressWidth - 1, offsetBits)
  private val newAddress = oldAddress + signedExtend(prefetchOffset << offsetBits)

  recentRequestTable.io.r <> scoreTable.io.test
  recentRequestTable.io.w <> delayQueue.io.out
  delayQueue.io.in.valid := io.train.valid
  delayQueue.io.in.bits := oldAddressNoOffset
  scoreTable.io.req.valid := io.train.valid
  scoreTable.io.req.bits := oldAddress
  scoreTable.io.requestAvailable := !requestValid || io.req.ready

  private val crossPage = newAddress(addressWidth - 1, pageOffsetBits) =/=
    oldAddress(addressWidth - 1, pageOffsetBits)

  when(io.req.fire) {
    requestValid := false.B
  }
  when(scoreTable.io.req.fire) {
    request := newAddress
    requestValid := !crossPage && !prefetchDisable
  }

  io.req.valid := requestValid
  io.req.bits := request
  io.train.ready := delayQueue.io.in.ready && scoreTable.io.req.ready && (!requestValid || io.req.ready)
  io.crossPageDrop := scoreTable.io.req.fire && crossPage
  io.disabledDrop := scoreTable.io.req.fire && !crossPage && prefetchDisable
  io.delayQueueFullDrop := delayQueue.io.fullDrop

  for (offset <- bopParams.offsetList) {
    if (offset < 0) {
      XSPerfAccumulate("best_offset_neg_" + (-offset).toString, prefetchOffset === offset.S(offsetWidth.W).asUInt)
    } else {
      XSPerfAccumulate("best_offset_pos_" + offset.toString, prefetchOffset === offset.U)
    }
  }
  XSPerfAccumulate("bop_req", io.req.fire)
  XSPerfAccumulate("bop_train", io.train.fire)
  XSPerfAccumulate("bop_train_stall_for_st_not_ready", io.train.valid && !scoreTable.io.req.ready)
  XSPerfAccumulate("bop_drop_for_cross_page", scoreTable.io.req.fire && crossPage)
  XSPerfAccumulate("bop_drop_for_auto_disable", scoreTable.io.req.fire && prefetchDisable)
}
