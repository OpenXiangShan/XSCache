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
