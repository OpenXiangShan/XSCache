package oceanus.l2

import chisel3._
import chisel3.util._

// Metadata only. Both data beats remain in the owning TSHR until consumed.
class L2RefillHint(txnBits: Int, nodeBits: Int) extends Bundle {
  val sourceId = UInt(txnBits.W)
  val targetId = UInt(nodeBits.W)
  val isKeyword = Bool()
}

// Enqueue is the externally visible hint event. Every entry reserves two
// output beats starting three cycles later. The pointer stays until beat 1
// actually fires, including under unexpected downstream backpressure.
class L2HintPointerQueue(pointerBits: Int, entries: Int = 4, ahead: Int = 3) extends Module {
  require(entries >= 3 && ahead == 3)
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(pointerBits.W)))
    val out = Decoupled(UInt(pointerBits.W))
    val second = Output(Bool())
    val full = Output(Bool())
  })
  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U
  val pointers = Module(new Queue(new Bundle {
    val pointer = UInt(pointerBits.W)
    val due = UInt(64.W)
  }, entries))
  val cooldown = RegNext(io.enq.fire, false.B)
  io.full := !pointers.io.enq.ready
  io.enq.ready := pointers.io.enq.ready && !cooldown
  pointers.io.enq.valid := io.enq.valid && !cooldown
  pointers.io.enq.bits.pointer := io.enq.bits
  pointers.io.enq.bits.due := cycle + ahead.U
  val second = RegInit(false.B)
  io.out.valid := pointers.io.deq.valid && cycle >= pointers.io.deq.bits.due
  io.out.bits := pointers.io.deq.bits.pointer
  io.second := second
  pointers.io.deq.ready := io.out.fire && second
  when(io.out.fire) { second := !second }
}

// Observation only: none of these signals feed hint admission or DAT scheduling.
// skip_* is a disjoint first-failure histogram in this order; condition_* may
// overlap. Both are charged once, at the first actual L1D DAT handshake.
object L2HintCoverage {
  val pipeReasons = Seq(
    "directory_not_hit", "opcode_not_read_shared_unique", "waiting_data0_missing",
    "waiting_data2_missing", "data0_already_scheduled", "data2_already_scheduled",
    "not_waiting_ds", "waiting_snoop0", "waiting_snoop2",
    "waiting_downstream0", "waiting_downstream2", "l1evt_active",
    "ds_read_cancelled"
  )
  val tshrReasons = Seq(
    "tshr_invalid", "evt_pipe_busy", "snp_pipe_busy",
    "nested_enter", "evt_rbe_valid", "snp_rbe_valid",
    "evb_rbe_valid", "req_rbe_valid", "rxdat_write0",
    "rxdat_write2"
  )
  val admissionReasons = Seq(
    "source_out_of_range", "local_other_owner", "local_reservation_active",
    "local_fifo_full", "local_fifo_cooldown", "other_slice_active",
    "channel_fifo_full", "channel_fifo_cooldown", "same_cycle_arb_lost",
    "downstream_not_ready", "hint_disabled", "no_ds_response",
    "no_recorded_reason"
  )
  val reasons = pipeReasons ++ tshrReasons ++ admissionReasons
}
class L2HitHintObservation extends Bundle {
  val request = Bool()
  val hit = Bool()
  val dsResponse = Bool()
  val failures = Vec(L2HintCoverage.pipeReasons.size + L2HintCoverage.tshrReasons.size, Bool())
}
