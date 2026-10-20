package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate

class OverwriteQueue[T <: Data](gen: T, entries: Int)(implicit p: Parameters) extends Module {
  require(entries > 1, "OverwriteQueue requires at least two entries")
  require((entries & (entries - 1)) == 0, "OverwriteQueue entries must be a power of two")

  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
    // Expose the exact overwrite pulse for per-source accounting.  The
    // queue's ready/flow semantics remain unchanged.
    val overwrite = Output(Bool())
  })

  private def wrapInc(ptr: UInt): UInt = Mux(ptr === (entries - 1).U, 0.U, ptr + 1.U)

  val data = Reg(Vec(entries, gen))
  val head = RegInit(0.U(log2Ceil(entries).W))
  val tail = RegInit(0.U(log2Ceil(entries).W))
  val maybeFull = RegInit(false.B)

  val full = head === tail && maybeFull
  val empty = head === tail && !maybeFull
  val dequeue = io.deq.ready && !empty
  val enqueue = io.enq.valid
  val overwrite = full && enqueue && !dequeue

  io.enq.ready := true.B
  io.deq.valid := !empty
  io.deq.bits := data(head)
  io.overwrite := overwrite
  XSPerfAccumulate("overwrite", overwrite)

  when (enqueue) {
    data(tail) := io.enq.bits
    tail := wrapInc(tail)
  }

  when (dequeue || overwrite) {
    head := wrapInc(head)
  }

  when (overwrite) {
    maybeFull := true.B
  }.elsewhen (enqueue =/= dequeue) {
    maybeFull := enqueue
  }
}
