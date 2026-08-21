package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.chi.bundle.CHIBundleRSP
import oceanus.chi.opcode.CHIRNFOpcodesRSP
import org.chipsalliance.cde.config.Parameters

class L2RXRSP(implicit val p: Parameters)
    extends Module
    with HasL2Params
    with CHIRNFOpcodesRSP {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new CHIBundleRSP))
    val out = Valid(new CHIBundleRSP)
    val pCrdGrant = Valid(new L2PCreditPool.Entry)
  })

  val isPCrdGrant = io.in.bits.Opcode.get === CHI_PCrdGrant.asUIntForRSP

  io.out.valid := io.in.valid && !isPCrdGrant
  io.out.bits := io.in.bits

  io.pCrdGrant.valid := io.in.valid && isPCrdGrant
  io.pCrdGrant.bits.srcId := io.in.bits.SrcID.get
  io.pCrdGrant.bits.pCrdType := io.in.bits.PCrdType.get

  when (io.out.valid) {
    assert(io.out.bits.TxnID.get < paramL2.mshrSize.U, "RXRSP TxnID is outside the local TSHR range")
  }
  assert(!(io.out.valid && io.pCrdGrant.valid))
}
