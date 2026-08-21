package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.chi.bundle.CHIBundleDAT
import org.chipsalliance.cde.config.Parameters

class L2TXDAT(entries: Int = 2)(implicit val p: Parameters) extends Module with HasL2Params {
  require(entries > 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CHIBundleDAT))
    val out = Decoupled(new CHIBundleDAT)
  })

  val queue = Module(new Queue(new CHIBundleDAT, entries = entries, pipe = false, flow = false))
  queue.io.enq <> io.in
  io.out <> queue.io.deq

  when (io.in.fire) {
    assert(io.in.bits.Opcode.get =/= 0.U, "TXDAT must not contain an L-credit return flit")
    assert(
      io.in.bits.DataID.get === 0.U || io.in.bits.DataID.get === 2.U,
      "TXDAT DataID must be 0 or 2"
    )
  }
}
