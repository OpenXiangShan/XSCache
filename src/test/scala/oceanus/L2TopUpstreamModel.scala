package oceanus

import chisel3._
import chisel3.simulator.ChiselSim
import oceanus.compactchi.{CCHIOpcode, CCHIResp}

case class UpTxnID(v: BigInt) extends AnyVal
case class UpDBID(v: BigInt) extends AnyVal
case class SnpTxnID(v: BigInt) extends AnyVal

sealed trait UpstreamOrigin
case class L1Origin(handle: Long, upTxnID: UpTxnID, upOpcode: BigInt, expectsCompAck: Boolean) extends UpstreamOrigin
case object InternalOrigin extends UpstreamOrigin

case class PendingL1Req(handle: Long, upTxnID: UpTxnID, upOpcode: BigInt, expectsCompAck: Boolean, addr: BigInt)
case class PendingUpAck(handle: Long, upDBID: UpDBID, upTxnID: UpTxnID, due: Int, sliceIdx: Int, dnTxnID: Option[DnTxnID] = None)
case class PendingSnpRsp(snpTxnID: SnpTxnID, tgtId: BigInt, due: Int)
case class PendingSnpData(snpTxnID: SnpTxnID, tgtId: BigInt, due: Int, beat: Int, resp: BigInt)

class L2TopUpstreamModel(dut: TestTop_L2Top, spec: CaseSpec) extends ChiselSim {
  def initInputs(): Unit = {
    dut.io.t1p(0).UpEVT.valid.poke(false.B)
    dut.io.t1p(0).UpEVT.bits.poke(0.U.asTypeOf(dut.io.t1p(0).UpEVT.bits))
    dut.io.t1p(0).UpREQ.valid.poke(false.B)
    dut.io.t1p(0).UpREQ.bits.poke(0.U.asTypeOf(dut.io.t1p(0).UpREQ.bits))
    dut.io.t1p(0).UpRSP.valid.poke(false.B)
    dut.io.t1p(0).UpRSP.bits.poke(0.U.asTypeOf(dut.io.t1p(0).UpRSP.bits))
    dut.io.t1p(0).UpDAT.valid.poke(false.B)
    dut.io.t1p(0).UpDAT.bits.poke(0.U.asTypeOf(dut.io.t1p(0).UpDAT.bits))
    dut.io.t1p(0).DnSNP.ready.poke(true.B)
    dut.io.t1p(0).DnRSP.ready.poke(true.B)
    dut.io.t1p(0).DnDAT.ready.poke(true.B)

    dut.io.t4p(0).UpREQ.valid.poke(false.B)
    dut.io.t4p(0).UpREQ.bits.poke(0.U.asTypeOf(dut.io.t4p(0).UpREQ.bits))
    dut.io.t4p(0).DnDAT.ready.poke(true.B)
    dut.io.t4p(1).UpREQ.valid.poke(false.B)
    dut.io.t4p(1).UpREQ.bits.poke(0.U.asTypeOf(dut.io.t4p(1).UpREQ.bits))
    dut.io.t4p(1).DnDAT.ready.poke(true.B)
  }

  def clearUpReq(): Unit = {
    dut.io.t1p(0).UpREQ.valid.poke(false.B)
  }

  def clearResponses(): Unit = {
    dut.io.t1p(0).UpRSP.valid.poke(false.B)
  }

  def requestAddr(index: Int): BigInt = spec.up.stream.request(index).addr

  def driveRead(index: Int): Unit = {
    val req = spec.up.stream.request(index)
    require(req.master == MasterId.t1p0, s"only t1p0 is wired in the current directed skeleton, got ${req.master.name}")
    dut.io.t1p(0).UpREQ.valid.poke(true.B)
    dut.io.t1p(0).UpREQ.bits.TxnID.poke(req.txnId.U)
    dut.io.t1p(0).UpREQ.bits.SrcID.poke(req.srcId.U)
    dut.io.t1p(0).UpREQ.bits.TgtID.poke(req.tgtId.U)
    dut.io.t1p(0).UpREQ.bits.Opcode.poke(req.opcode.U)
    dut.io.t1p(0).UpREQ.bits.Size.poke(req.size.U)
    dut.io.t1p(0).UpREQ.bits.NS.poke(req.ns.B)
    dut.io.t1p(0).UpREQ.bits.Order.poke(req.order.U)
    dut.io.t1p(0).UpREQ.bits.MemAttr.poke(req.memAttr.U)
    dut.io.t1p(0).UpREQ.bits.Excl.poke(req.excl.B)
    dut.io.t1p(0).UpREQ.bits.ExpCompData.poke(req.expCompData.B)
    dut.io.t1p(0).UpREQ.bits.Addr.poke(req.addr.U)
  }

  def driveUpCompAck(upDBID: UpDBID, sliceIdx: Int = 0): Unit = {
    dut.io.t1p(0).UpRSP.valid.poke(true.B)
    dut.io.t1p(0).UpRSP.bits.poke(0.U.asTypeOf(dut.io.t1p(0).UpRSP.bits))
    dut.io.t1p(0).UpRSP.bits.TxnID.poke(upDBID.v.U)
    dut.io.t1p(0).UpRSP.bits.SrcID.poke(0.U)
    dut.io.t1p(0).UpRSP.bits.TgtID.poke(sliceIdx.U)
    dut.io.t1p(0).UpRSP.bits.Opcode.poke(CCHIOpcode.CompAck.U)
  }

  def driveUpSnpResp(snpTxnID: SnpTxnID, tgtId: BigInt = BigInt(0)): Unit = {
    dut.io.t1p(0).UpRSP.valid.poke(true.B)
    dut.io.t1p(0).UpRSP.bits.poke(0.U.asTypeOf(dut.io.t1p(0).UpRSP.bits))
    dut.io.t1p(0).UpRSP.bits.TxnID.poke(snpTxnID.v.U)
    dut.io.t1p(0).UpRSP.bits.SrcID.poke(0.U)
    dut.io.t1p(0).UpRSP.bits.TgtID.poke(tgtId.U)
    dut.io.t1p(0).UpRSP.bits.Opcode.poke(CCHIOpcode.SnpResp.U)
    dut.io.t1p(0).UpRSP.bits.Resp.poke(CCHIResp.I.U)
  }

  def driveUpSnpRespData(p: PendingSnpData): Unit = {
    dut.io.t1p(0).UpDAT.valid.poke(true.B)
    dut.io.t1p(0).UpDAT.bits.poke(0.U.asTypeOf(dut.io.t1p(0).UpDAT.bits))
    dut.io.t1p(0).UpDAT.bits.TxnID.poke(p.snpTxnID.v.U)
    dut.io.t1p(0).UpDAT.bits.SrcID.poke(0.U)
    dut.io.t1p(0).UpDAT.bits.TgtID.poke(p.tgtId.U)
    dut.io.t1p(0).UpDAT.bits.Opcode.poke(CCHIOpcode.SnpRespData.U)
    dut.io.t1p(0).UpDAT.bits.RespErr.poke(0.U)
    dut.io.t1p(0).UpDAT.bits.Resp.poke(p.resp.U)
    dut.io.t1p(0).UpDAT.bits.DataID.poke((if (p.beat == 0) 0 else 1).U)
    dut.io.t1p(0).UpDAT.bits.Data.poke((BigInt("20000000000000000000000000000000", 16) + p.snpTxnID.v + p.beat).U)
    dut.io.t1p(0).UpDAT.bits.BE.poke(BigInt("ffffffff", 16).U)
  }
}
