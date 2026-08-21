package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.chi.bundle.CHIBundleREQ
import org.chipsalliance.cde.config.Parameters

class L2TXREQ(entries: Int = 2)(implicit val p: Parameters) extends Module with HasL2Params {
  require(entries > 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CHIBundleREQ))
    val out = Decoupled(new CHIBundleREQ)
  })

  val queue = Module(new Queue(new CHIBundleREQ, entries = entries, pipe = false, flow = false))
  queue.io.enq <> io.in
  io.out <> queue.io.deq

  when (io.out.fire) {
    assert(io.out.bits.Opcode.get =/= 0.U, "TXREQ must not contain an L-credit return flit")
  }
}
