package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.chi.bundle.CHIBundleSNP
import org.chipsalliance.cde.config.Parameters

class L2RXSNP(entries: Int = 2)(implicit val p: Parameters) extends Module with HasL2Params {
  require(entries > 0)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CHIBundleSNP))
    val out = Decoupled(new CHIBundleSNP)
  })

  val queue = Module(new Queue(new CHIBundleSNP, entries = entries, pipe = false, flow = false))
  queue.io.enq <> io.in
  io.out <> queue.io.deq

  when (io.in.fire) {
    assert(io.in.bits.Opcode.get =/= 0.U, "RXSNP must not contain an L-credit return flit")
  }
}
