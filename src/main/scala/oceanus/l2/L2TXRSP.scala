package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.chi.bundle.CHIBundleRSP
import org.chipsalliance.cde.config.Parameters

class L2TXRSP(entries: Int = 2)(implicit val p: Parameters) extends Module with HasL2Params {
  require(entries > 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CHIBundleRSP))
    val out = Decoupled(new CHIBundleRSP)
  })

  val queue = Module(new Queue(new CHIBundleRSP, entries = entries, pipe = false, flow = false))
  queue.io.enq <> io.in
  io.out <> queue.io.deq

  when (io.out.fire) {
    assert(io.out.bits.Opcode.get =/= 0.U, "TXRSP must not contain an L-credit return flit")
  }
}
