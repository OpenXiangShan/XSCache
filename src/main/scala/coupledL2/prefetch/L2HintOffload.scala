package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._

/** GEM5 l2-stream-offload-default policy (8f30f6a).
  * Despite its name, that branch selects SStride and SPht, NOT SStream.
  * Inputs are cumulative L2 dequeue/useful counts, not L1 accuracy or
  * request-generation counts. The registered decision keeps the wide ratio
  * comparisons off the hint routing path. No division, sampling, or epochs.
  */
class L2HintOffloadPolicy extends Module {
  val io = IO(new Bundle {
    val total = Input(UInt(64.W))
    val cdp = Input(UInt(64.W))
    val sent = Input(Vec(2, UInt(64.W))) // Stride, SMS/PHT
    val useful = Input(Vec(2, UInt(64.W)))
    val offload = Output(Vec(2, Bool()))
  })

  // Multiplication grows the result width; do not truncate before comparing.
  val cdpDominates = io.total =/= 0.U && io.cdp * 5.U > io.total
  for (i <- 0 until 2) {
    // Zero-denominator sources stay in L2, matching GEM5's NaN comparison.
    io.offload(i) := RegNext(cdpDominates && io.sent(i) =/= 0.U &&
      io.useful(i) * 5.U < io.sent(i) * 4.U, false.B)
  }
}

/** Lossless queue for accepted offloads, fairly sharing the stash allocator
  * with the existing native-L3 queue. Full queues backpressure the router.
  */
class L2HintOffloadQueue[T <: Data](gen: T, entries: Int) extends Module {
  val io = IO(new Bundle {
    val native = Flipped(Decoupled(gen))
    val offload = Flipped(Decoupled(gen))
    val out = Decoupled(gen)
    val offloadAllocated = Output(Bool())
  })
  val queue = Module(new Queue(gen, entries))
  queue.io.enq <> io.offload
  val arb = Module(new RRArbiter(gen, 2))
  arb.io.in(0) <> io.native
  arb.io.in(1) <> queue.io.deq
  io.out <> arb.io.out
  io.offloadAllocated := queue.io.deq.fire
}
