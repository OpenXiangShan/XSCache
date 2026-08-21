package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.chi.bundle.CHIBundleDAT
import org.chipsalliance.cde.config.Parameters

class L2RXDAT(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new CHIBundleDAT))
    val out = Valid(new CHIBundleDAT)
  })

  io.out := io.in

  when (io.in.valid) {
    assert(io.in.bits.TxnID.get < paramL2.mshrSize.U, "RXDAT TxnID is outside the local TSHR range")
    assert(
      io.in.bits.DataID.get === 0.U || io.in.bits.DataID.get === 2.U,
      "RXDAT DataID must be 0 or 2"
    )
  }
}
