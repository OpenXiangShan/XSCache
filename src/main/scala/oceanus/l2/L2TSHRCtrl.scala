package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.l2._
import oceanus.l2.L2Common._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.SeqBoolBitwiseOps
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.compactchi.CCHIOpcode._


class L2TSHRCtrl(implicit val p: Parameters) extends Module with HasL2Params {

  val io = IO(new Bundle {
    val RXEVT = Flipped(Decoupled(new FlitEVT))
    val RXSNP = Flipped(Decoupled(new CHIBundleSNP))
    val RXREQ = Flipped(Decoupled(new FlitREQ))

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))
    val UpRXDAT = Flipped(Valid(new FlitUpDAT))

    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))
  })

  def getTSHRIdFromTxnID(txnId: UInt) = txnId

  //
  val tshrs = Seq.tabulate(paramL2.mshrSize)(i => Module(new L2TSHR(i)))

  tshrs.foreach { case t => 
    t.io.RXEVT := io.RXEVT.bits
    t.io.RXSNP := io.RXSNP.bits
    t.io.RXREQ := io.RXREQ.bits

    t.io.UpRXRSP.valid := io.UpRXRSP.valid && getTSHRIdFromTxnID(io.UpRXRSP.bits.TxnID) === t.id.U 
    t.io.UpRXRSP.bits := io.UpRXRSP.bits

    t.io.UpRXDAT.valid := io.UpRXDAT.valid && getTSHRIdFromTxnID(io.UpRXDAT.bits.TxnID) === t.id.U
    t.io.UpRXDAT.bits := io.UpRXDAT.bits

    t.io.DnRXRSP.valid := io.DnRXRSP.valid && getTSHRIdFromTxnID(io.DnRXRSP.bits.TxnID.get) === t.id.U
    t.io.DnRXRSP.bits := io.DnRXRSP.bits

    t.io.DnRXDAT.valid := io.DnRXDAT.valid && getTSHRIdFromTxnID(io.DnRXDAT.bits.TxnID.get) === t.id.U
    t.io.DnRXDAT.bits := io.DnRXDAT.bits
  }

  assert(!io.UpRXRSP.valid || (PopCount(tshrs.map(_.io.UpRXRSP.valid)) =/= 0.U), "UpRXRSP valid but no TSHR selected, flit lost")
  assert(!io.UpRXDAT.valid || (PopCount(tshrs.map(_.io.UpRXDAT.valid)) =/= 0.U), "UpRXDAT valid but no TSHR selected, flit lost")
  assert(!io.DnRXRSP.valid || (PopCount(tshrs.map(_.io.DnRXRSP.valid)) =/= 0.U), "DnRXRSP valid but no TSHR selected, flit lost")
  assert(!io.DnRXDAT.valid || (PopCount(tshrs.map(_.io.DnRXDAT.valid)) =/= 0.U), "DnRXDAT valid but no TSHR selected, flit lost")

  //
  val tshrAlloc = Module(new L2TSHRAlloc(new L2TSHRAllocConfig(
    cluster = Seq(Seq(L2TSHRAllocTarget.EVT), Seq(L2TSHRAllocTarget.SNP, L2TSHRAllocTarget.REQ)), // TODO: parameterize clustering
    resv = Seq(
      (paramL2.mshrSize - 1, L2TSHRResvTarget.L2EVT),
      (paramL2.mshrSize - 2, L2TSHRResvTarget.L1EVT),
      (paramL2.mshrSize - 3, L2TSHRResvTarget.L3SNP)) // TODO: parameterize reservation
  )))

  tshrAlloc.io.fromTSHRCtrl.RXEVT <> io.RXEVT
  tshrAlloc.io.fromTSHRCtrl.RXSNP <> io.RXSNP
  tshrAlloc.io.fromTSHRCtrl.RXREQ <> io.RXREQ

  tshrs.foreach { case t => 
    tshrAlloc.io.fromTSHR(t.id).bits := t.io.toAlloc
    tshrAlloc.io.fromTSHR(t.id).valid := t.io.valid
    t.io.fromAlloc := tshrAlloc.io.toTSHR(t.id)
  }

}
