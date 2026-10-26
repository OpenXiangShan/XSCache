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

/** Test-side REQ legality, checked before asserting valid or queuing work. */
object L2TopUpstreamLegality {
  // CHIron cchi/spec/cchi_protocol_encoding.hpp:175-187 (Type4),
  // pinned by test/CHIron at 6f2e59ce. ReadShared is not in this list.
  private val type4Opcodes = Set[BigInt](CCHIOpcode.StashShared.opcode,
    CCHIOpcode.StashUnique.opcode, CCHIOpcode.ReadNoSnp.opcode, CCHIOpcode.ReadOnce.opcode)
  def check(req: UpRequestSpec): Unit = {
    if (req.master == MasterId.T4P0 || req.master == MasterId.T4P1)
      require(type4Opcodes.contains(req.opcode),
        s"CCHI_TYPE4_OPCODE_VIOLATION master=${req.master.name} opcode=0x${req.opcode.toString(16)} addr=0x${req.addr.toString(16)} allowed=0x00,0x01,0x02,0x03")
  }
}

class L2TopUpstreamModel(dut: TestTop_L2Top, spec: CaseSpec) extends ChiselSim {
  // Phase 1a optional ports have no upstream producer yet. Initialize their
  // inputs to deterministic idle values for both acceptance configurations.
  def initInputs(): Unit = {
    dut.io.pfTlb.foreach { tlb =>
      tlb.req.ready.poke(false.B)
      tlb.resp.valid.poke(false.B)
      tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits))
      tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
    }
    // Intent (1c-1): idle VA/source remain zero, but tie trainEnable HIGH to
    // preserve legacy enabled training. An all-zero sideband now disables BOP;
    // explicitly driven zero-enable cases are covered by TestTop_L2Train.
    dut.io.pfTrain.foreach(_.foreach { train =>
      train.poke(0.U.asTypeOf(train))
      train.trainEnable.poke(true.B)
    })
    dut.io.t1p.foreach { port =>
      port.UpEVT.valid.poke(false.B)
      port.UpEVT.bits.poke(0.U.asTypeOf(port.UpEVT.bits))
      port.UpREQ.valid.poke(false.B)
      port.UpREQ.bits.poke(0.U.asTypeOf(port.UpREQ.bits))
      port.UpRSP.valid.poke(false.B)
      port.UpRSP.bits.poke(0.U.asTypeOf(port.UpRSP.bits))
      port.UpDAT.valid.poke(false.B)
      port.UpDAT.bits.poke(0.U.asTypeOf(port.UpDAT.bits))
      port.DnSNP.ready.poke(true.B)
      port.DnRSP.ready.poke(true.B)
      port.DnDAT.ready.poke(true.B)
    }

    dut.io.t4p.zipWithIndex.foreach { case (port, _) =>
      port.UpREQ.valid.poke(false.B)
      port.UpREQ.bits.poke(0.U.asTypeOf(port.UpREQ.bits))
      port.DnDAT.ready.poke(true.B)
    }
  }

  def clearUpReq(): Unit = {
    dut.io.t1p.foreach(_.UpREQ.valid.poke(false.B))
    dut.io.t4p.foreach(_.UpREQ.valid.poke(false.B))
  }

  def clearResponses(): Unit = {
    dut.io.t1p.foreach { port =>
      port.UpRSP.valid.poke(false.B)
      port.UpDAT.valid.poke(false.B)
    }
  }

  def requestAddr(index: Int): BigInt = spec.up.stream.request(index).addr

  def driveRead(index: Int): Unit = {
    val req = spec.up.stream.request(index)
    driveRead(req)
  }

  /** Drive one request on its owning port. TxnID belongs to that port. */
  def driveRead(req: UpRequestSpec): Unit = {
    L2TopUpstreamLegality.check(req)
    val port = req.master match {
      case MasterId.T1P0 => dut.io.t1p(0).UpREQ
      case MasterId.T1P1 => dut.io.t1p(1).UpREQ
      case MasterId.T4P0 => dut.io.t4p(0).UpREQ
      case MasterId.T4P1 => dut.io.t4p(1).UpREQ
    }
    port.valid.poke(true.B)
    port.bits.TxnID.poke(req.txnId.U)
    port.bits.SrcID.poke(req.srcId.U)
    port.bits.TgtID.poke(req.tgtId.U)
    port.bits.Opcode.poke(req.opcode.U)
    port.bits.Size.poke(req.size.U)
    port.bits.NS.poke(req.ns.B)
    port.bits.Order.poke(req.order.U)
    port.bits.MemAttr.poke(req.memAttr.U)
    port.bits.Excl.poke(req.excl.B)
    port.bits.ExpCompData.poke(req.expCompData.B)
    port.bits.Addr.poke(req.addr.U)
    // Phase 1b driven regression: vary VPN/source/enable per live request while
    // preserving all existing requests and timing. Default remains idle.
    if (L2TopTrainStimulus.driven && req.master == MasterId.T1P0)
      dut.io.pfTrain.foreach(p => L2TopTrainStimulus.poke(p(0), req.addr, req.srcId, req.txnId))
  }

  def driveUpCompAck(upDBID: UpDBID, sliceIdx: Int = 0, portIdx: Int = 0): Unit = {
    val port = dut.io.t1p(portIdx).UpRSP
    port.valid.poke(true.B)
    port.bits.poke(0.U.asTypeOf(port.bits))
    port.bits.TxnID.poke(upDBID.v.U)
    port.bits.SrcID.poke(dut.l2cfg.upstream.type1(portIdx).nid.U)
    port.bits.TgtID.poke(sliceIdx.U)
    port.bits.Opcode.poke(CCHIOpcode.CompAck.U)
  }

  def driveUpSnpResp(snpTxnID: SnpTxnID, tgtId: BigInt = BigInt(0), portIdx: Int = 0): Unit = {
    dut.io.t1p(portIdx).UpRSP.valid.poke(true.B)
    dut.io.t1p(portIdx).UpRSP.bits.poke(0.U.asTypeOf(dut.io.t1p(portIdx).UpRSP.bits))
    dut.io.t1p(portIdx).UpRSP.bits.TxnID.poke(snpTxnID.v.U)
    dut.io.t1p(portIdx).UpRSP.bits.SrcID.poke(dut.l2cfg.upstream.type1(portIdx).nid.U)
    dut.io.t1p(portIdx).UpRSP.bits.TgtID.poke(tgtId.U)
    dut.io.t1p(portIdx).UpRSP.bits.Opcode.poke(CCHIOpcode.SnpResp.U)
    dut.io.t1p(portIdx).UpRSP.bits.Resp.poke(CCHIResp.I.U)
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
