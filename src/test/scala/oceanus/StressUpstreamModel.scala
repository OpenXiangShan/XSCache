package oceanus
import chisel3._
import chisel3.simulator.ChiselSim
import oceanus.compactchi.{CCHIOpcode,CCHIResp}
// Intent: the public upstream protocol driver, typed for the lean stress top.
class StressUpstreamModel(dut: StressTopBase, spec: CaseSpec) extends ChiselSim {
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

    dut.io.t4p.zipWithIndex.foreach { case (port, _) =>
      port.UpREQ.valid.poke(false.B)
      port.UpREQ.bits.poke(0.U.asTypeOf(port.UpREQ.bits))
      port.DnDAT.ready.poke(true.B)
    }
  }

  def clearUpReq(): Unit = {
    dut.io.t1p(0).UpREQ.valid.poke(false.B)
    dut.io.t4p.foreach(_.UpREQ.valid.poke(false.B))
  }

  def clearResponses(): Unit = {
    dut.io.t1p(0).UpRSP.valid.poke(false.B)
    dut.io.t1p(0).UpDAT.valid.poke(false.B)
  }

  def requestAddr(index: Int): BigInt = spec.up.stream.request(index).addr

  def driveRead(index: Int): Unit = {
    val req = spec.up.stream.request(index)
    driveRead(req)
  }

  /** Drive one request on its owning port. TxnID belongs to that port. */
  def driveRead(req: UpRequestSpec): Unit = {
    val port = req.master match {
      case MasterId.T1P0 => dut.io.t1p(0).UpREQ
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
