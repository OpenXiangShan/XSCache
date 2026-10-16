package oceanus

import chisel3._
import chisel3.simulator.ChiselSim
import oceanus.chi.field.CHIFieldResp

case class DnTxnID(v: BigInt) extends AnyVal
case class DnTxnInfo(
  dnTxnID: DnTxnID,
  sliceIdx: Int,
  tshrId: Int,
  handle: Long,
  parentHandle: Option[Long],
  origin: UpstreamOrigin,
  dnOpcode: BigInt
)
case class PendingDnComp(dnTxnID: DnTxnID, srcId: BigInt, due: Int, beat: Int, resp: BigInt, addr: BigInt = BigInt(-1))
case class PendingDnRsp(dnTxnID: DnTxnID, srcId: BigInt, due: Int, opcode: BigInt)

object L2TopDownstreamOpcodes {
  val CompData = 0x04
  val Comp = 0x04
  val CompDBIDResp = 0x05
  val CopyBackWrData = 0x02
  val Evict = 0x0d
  val WriteBackFull = 0x1b
  val WriteEvictFull = 0x15
  val WriteEvictOrEvict = 0x42
  val ReadNotSharedDirty = 0x26
}

class L2TopDownstreamModel(dut: TestTop_L2Top, spec: CaseSpec) extends ChiselSim {
  private val credit = spec.dn.creditPolicy
  private val maxTxLCredits = credit.maxTxLCredits
  private val maxRxRspLCredits = credit.maxRxRspLCredits
  private val maxRxDatLCredits = credit.maxRxDatLCredits
  private val maxRxSnpLCredits = credit.maxRxSnpLCredits
  private val txCreditReturnDelay = credit.txCreditReturnDelay
  private var hnLinkActiveAck = false
  private var hnTxLinkRun = false
  private var hnRxLinkRun = false
  private class ReceiverCreditTracker(name: String, maxLCredits: Int, recycleOnFlit: Boolean = true) {
    var balance = 0
    private var initialLCredits = maxLCredits
    private val returnCreditDue = scala.collection.mutable.Queue.empty[Int]

    def hasGrant(cycle: Int): Boolean = initialLCredits > 0 || returnCreditDue.headOption.exists(_ <= cycle)

    def advance(linkRun: Boolean, linkReq: Boolean, linkAck: Boolean, grant: Boolean, flit: Boolean, cycle: Int): Unit = {
      assert(!grant || linkRun,
        s"$name lcrdv before TX LINKACTIVE RUN at cycle=$cycle txlinkreq=${if (linkReq) 1 else 0} txlinkack=${if (linkAck) 1 else 0}")
      assert(!flit || linkRun,
        s"$name flit before TX LINKACTIVE RUN at cycle=$cycle txlinkreq=${if (linkReq) 1 else 0} txlinkack=${if (linkAck) 1 else 0}")
      assert(!flit || balance > 0,
        s"$name flit without available lcredit at cycle=$cycle balance=$balance grant=${if (grant) 1 else 0}")

      if (grant) {
        if (initialLCredits > 0) {
          initialLCredits -= 1
        } else {
          assert(returnCreditDue.headOption.exists(_ <= cycle), s"$name lcrdv grant without pending credit at cycle=$cycle")
          returnCreditDue.dequeue()
        }
      }

      balance += (if (grant) 1 else 0) - (if (flit) 1 else 0)
      assert(balance >= 0 && balance <= maxLCredits,
        s"$name credit balance out of range at cycle=$cycle balance=$balance grant=${if (grant) 1 else 0} flit=${if (flit) 1 else 0}")

      if (flit && recycleOnFlit) {
        returnCreditDue.enqueue(cycle + txCreditReturnDelay + 1)
      }
    }

    def returnCredit(cycle: Int): Unit = {
      returnCreditDue.enqueue(cycle + txCreditReturnDelay + 1)
      assert(returnCreditDue.size <= maxLCredits, s"$name returned too many credits: pending=${returnCreditDue.size} max=$maxLCredits")
    }
  }

  private class SenderCreditTracker(name: String, maxLCredits: Int) {
    var balance = 0

    def canSend(linkRun: Boolean): Boolean = linkRun && balance > 0

    def advance(linkRun: Boolean, linkReq: Boolean, linkAck: Boolean, grant: Boolean, flit: Boolean, cycle: Int): Unit = {
      assert(!grant || linkRun,
        s"$name lcrdv before RX LINKACTIVE RUN at cycle=$cycle rxlinkreq=${if (linkReq) 1 else 0} rxlinkack=${if (linkAck) 1 else 0}")
      assert(!flit || linkRun,
        s"$name flit before RX LINKACTIVE RUN at cycle=$cycle rxlinkreq=${if (linkReq) 1 else 0} rxlinkack=${if (linkAck) 1 else 0}")
      assert(!flit || balance > 0,
        s"$name flit without available lcredit at cycle=$cycle balance=$balance grant=${if (grant) 1 else 0}")

      balance += (if (grant) 1 else 0) - (if (flit) 1 else 0)
      assert(balance >= 0 && balance <= maxLCredits,
        s"$name credit balance out of range at cycle=$cycle balance=$balance grant=${if (grant) 1 else 0} flit=${if (flit) 1 else 0}")
    }
  }

  private val holdTxReqCredit = spec.dn.holdTxReqCreditUntilResponse
  private val txreqCreditLimit = if (holdTxReqCredit) math.min(maxTxLCredits, spec.dn.maxOutstanding.limit(0)) else maxTxLCredits
  private val txreqCredits = new ReceiverCreditTracker("txreq", txreqCreditLimit, recycleOnFlit = !holdTxReqCredit)
  private val txrspCredits = new ReceiverCreditTracker("txrsp", maxTxLCredits)
  private val txdatCredits = new ReceiverCreditTracker("txdat", maxTxLCredits)
  private val rxrspCredits = new SenderCreditTracker("rxrsp", maxRxRspLCredits)
  private val rxdatCredits = new SenderCreditTracker("rxdat", maxRxDatLCredits)
  private val rxsnpCredits = new SenderCreditTracker("rxsnp", maxRxSnpLCredits)
  private val preAssertTrace = scala.collection.mutable.Queue.empty[String]
  private var rxLinkActiveReq = false
  private var outstandingDn = 0

  def noteRequest(txnId: DnTxnID): Unit = {
    if (holdTxReqCredit) {
      outstandingDn += 1
      assert(outstandingDn <= spec.dn.maxOutstanding.limit(0),
        s"downstream outstanding overflow: txn=${txnId.v} outstanding=$outstandingDn limit=${spec.dn.maxOutstanding.limit(0)}")
    }
  }

  def canDeliverResponse: Boolean = true

  def noteResponse(txnId: DnTxnID, finalBeat: Boolean, cycle: Int): Unit = {
    if (holdTxReqCredit && finalBeat) {
      assert(outstandingDn > 0, s"downstream response without outstanding request: txn=${txnId.v}")
      outstandingDn -= 1
      txreqCredits.returnCredit(cycle)
    }
  }

  def initInputs(): Unit = {
    dut.io.chi.rxrsp.flitv.poke(false.B)
    dut.io.chi.rxrsp.flitpend.poke(false.B)
    dut.io.chi.rxrsp.flit.poke(0.U.asTypeOf(dut.io.chi.rxrsp.flit))
    dut.io.chi.rxdat.flitv.poke(false.B)
    dut.io.chi.rxdat.flitpend.poke(false.B)
    dut.io.chi.rxdat.flit.poke(0.U.asTypeOf(dut.io.chi.rxdat.flit))
    dut.io.chi.rxsnp.flitv.poke(false.B)
    dut.io.chi.rxsnp.flitpend.poke(false.B)
    dut.io.chi.rxsnp.flit.poke(0.U.asTypeOf(dut.io.chi.rxsnp.flit))
    dut.io.chi.txreq.lcrdv.poke(false.B)
    dut.io.chi.txrsp.lcrdv.poke(false.B)
    dut.io.chi.txdat.lcrdv.poke(false.B)
    dut.io.chi.rxlinkactivereq.poke(false.B)
    dut.io.chi.txlinkactiveack.poke(false.B)
    dut.io.chi.rxsactive.poke(true.B)
    dut.io.chi.syscoack.poke(false.B)
  }

  def clearResponses(): Unit = {
    dut.io.chi.rxdat.flitv.poke(false.B)
    dut.io.chi.rxdat.flitpend.poke(false.B)
    dut.io.chi.rxrsp.flitv.poke(false.B)
    dut.io.chi.rxrsp.flitpend.poke(false.B)
    dut.io.chi.rxsnp.flitv.poke(false.B)
    dut.io.chi.rxsnp.flitpend.poke(false.B)
  }

  // Concurrent directed cases use an immutable line-addressed reference
  // memory.  Legacy cases retain the transaction-id payload so their frozen
  // eviction-data assertions remain byte-for-byte compatible.
  def referenceData(addr: BigInt, beat: Int): BigInt =
    L2TopReferenceMemory.beat(addr, beat)

  def dnDataValue(p: PendingDnComp): BigInt =
    if (p.addr >= 0) referenceData(p.addr, p.beat)
    else BigInt("10000000000000000000000000000000", 16) + p.dnTxnID.v + p.beat

  def driveDnData(p: PendingDnComp): Unit = {
    val data = dnDataValue(p)
    dut.io.chi.rxdat.flitv.poke(true.B)
    dut.io.chi.rxdat.flitpend.poke(true.B)
    dut.io.chi.rxdat.flit.poke(0.U.asTypeOf(dut.io.chi.rxdat.flit))
    dut.io.chi.rxdat.flit.TxnID.get.poke(p.dnTxnID.v.U)
    dut.io.chi.rxdat.flit.SrcID.get.poke(0.U)
    dut.io.chi.rxdat.flit.TgtID.get.poke(p.srcId.U)
    dut.io.chi.rxdat.flit.HomeNID.get.poke(0.U)
    dut.io.chi.rxdat.flit.Opcode.get.poke(L2TopDownstreamOpcodes.CompData.U)
    dut.io.chi.rxdat.flit.RespErr.get.poke(0.U)
    dut.io.chi.rxdat.flit.Resp.get.poke(p.resp.U)
    dut.io.chi.rxdat.flit.DBID.get.poke(p.dnTxnID.v.U)
    dut.io.chi.rxdat.flit.DataID.get.poke((if (p.beat == 0) 0 else 2).U)
    dut.io.chi.rxdat.flit.Data.get.poke(data.U)
  }

  def driveDnRsp(p: PendingDnRsp): Unit = {
    dut.io.chi.rxrsp.flitv.poke(true.B)
    dut.io.chi.rxrsp.flitpend.poke(true.B)
    dut.io.chi.rxrsp.flit.poke(0.U.asTypeOf(dut.io.chi.rxrsp.flit))
    dut.io.chi.rxrsp.flit.TxnID.get.poke(p.dnTxnID.v.U)
    dut.io.chi.rxrsp.flit.SrcID.get.poke(0.U)
    dut.io.chi.rxrsp.flit.TgtID.get.poke(p.srcId.U)
    dut.io.chi.rxrsp.flit.Opcode.get.poke(p.opcode.U)
    dut.io.chi.rxrsp.flit.RespErr.get.poke(0.U)
    dut.io.chi.rxrsp.flit.Resp.get.poke(0.U)
    dut.io.chi.rxrsp.flit.DBID_PGroupID_StashGroupID_TagGroupID.get.poke(p.dnTxnID.v.U)
    dut.io.chi.rxrsp.flit.PCrdType.get.poke(0.U)
  }

  def isEvictReq(opcode: BigInt): Boolean =
    opcode == L2TopDownstreamOpcodes.Evict ||
    opcode == L2TopDownstreamOpcodes.WriteBackFull ||
    opcode == L2TopDownstreamOpcodes.WriteEvictFull ||
    opcode == L2TopDownstreamOpcodes.WriteEvictOrEvict

  def downstreamEvictRspOpcode(opcode: BigInt): BigInt =
    if (opcode == L2TopDownstreamOpcodes.Evict) L2TopDownstreamOpcodes.Comp else L2TopDownstreamOpcodes.CompDBIDResp

  def preAssertTraceLine(cycle: Int): String = {
    val txReqValid = dut.io.chi.txreq.flitv.peek().litToBoolean
    val txReqOpcode = if (txReqValid) dut.io.chi.txreq.flit.Opcode.get.peek().litValue else BigInt(-1)
    val txReqTxnId = if (txReqValid) dut.io.chi.txreq.flit.TxnID.get.peek().litValue else BigInt(-1)
    val rxDatValid = dut.io.chi.rxdat.flitv.peek().litToBoolean
    val rxRspValid = dut.io.chi.rxrsp.flitv.peek().litToBoolean
    s"PRE_ASSERT cycle=$cycle txreqValid=${if (txReqValid) 1 else 0} txreqOpcode=$txReqOpcode txreqTxn=$txReqTxnId " +
      s"txLinkRun=${if (hnTxLinkRun) 1 else 0} rxLinkRun=${if (hnRxLinkRun) 1 else 0} " +
      s"txreqCredit=${txreqCredits.balance} txrspCredit=${txrspCredits.balance} txdatCredit=${txdatCredits.balance} " +
      s"rxrspCredit=${rxrspCredits.balance} rxdatCredit=${rxdatCredits.balance} rxsnpCredit=${rxsnpCredits.balance} " +
      s"rxrspValid=${if (rxRspValid) 1 else 0} rxdatValid=${if (rxDatValid) 1 else 0}"
  }

  def recordPreAssertTrace(cycle: Int): Unit = {
    preAssertTrace.enqueue(preAssertTraceLine(cycle))
    while (preAssertTrace.size > 24) {
      preAssertTrace.dequeue()
    }
  }

  def activateRxLink(): Unit = {
    rxLinkActiveReq = true
  }

  def driveTxLCredits(cycle: Int): Unit = {
    val txLinkReq = dut.io.chi.txlinkactivereq.peek().litToBoolean
    val txLinkAck = hnLinkActiveAck || txLinkReq
    dut.io.chi.txlinkactiveack.poke(txLinkAck.B)
    dut.io.chi.rxlinkactivereq.poke(rxLinkActiveReq.B)

    dut.io.chi.txreq.lcrdv.poke((hnTxLinkRun && txreqCredits.hasGrant(cycle)).B)
    dut.io.chi.txrsp.lcrdv.poke((hnTxLinkRun && txrspCredits.hasGrant(cycle)).B)
    dut.io.chi.txdat.lcrdv.poke((hnTxLinkRun && txdatCredits.hasGrant(cycle)).B)
  }

  def advanceTxLCredits(cycle: Int): Unit = {
    val txLinkReq = dut.io.chi.txlinkactivereq.peek().litToBoolean
    val txLinkAck = dut.io.chi.txlinkactiveack.peek().litToBoolean
    val txLinkRun = txLinkReq && txLinkAck
    val txreqGrant = dut.io.chi.txreq.lcrdv.peek().litToBoolean
    val txrspGrant = dut.io.chi.txrsp.lcrdv.peek().litToBoolean
    val txdatGrant = dut.io.chi.txdat.lcrdv.peek().litToBoolean
    val txreqFlit = dut.io.chi.txreq.flitv.peek().litToBoolean
    val txrspFlit = dut.io.chi.txrsp.flitv.peek().litToBoolean
    val txdatFlit = dut.io.chi.txdat.flitv.peek().litToBoolean

    txreqCredits.advance(txLinkRun, txLinkReq, txLinkAck, txreqGrant, txreqFlit, cycle)
    txrspCredits.advance(txLinkRun, txLinkReq, txLinkAck, txrspGrant, txrspFlit, cycle)
    txdatCredits.advance(txLinkRun, txLinkReq, txLinkAck, txdatGrant, txdatFlit, cycle)

    hnLinkActiveAck = txLinkAck
    hnTxLinkRun = txLinkRun
  }

  def advanceRxLCredits(cycle: Int): Unit = {
    val rxLinkReq = dut.io.chi.rxlinkactivereq.peek().litToBoolean
    val rxLinkAck = dut.io.chi.rxlinkactiveack.peek().litToBoolean
    val rxLinkRun = rxLinkReq && rxLinkAck
    val rxrspGrant = dut.io.chi.rxrsp.lcrdv.peek().litToBoolean
    val rxdatGrant = dut.io.chi.rxdat.lcrdv.peek().litToBoolean
    val rxsnpGrant = dut.io.chi.rxsnp.lcrdv.peek().litToBoolean
    val rxrspFlit = dut.io.chi.rxrsp.flitv.peek().litToBoolean
    val rxdatFlit = dut.io.chi.rxdat.flitv.peek().litToBoolean
    val rxsnpFlit = dut.io.chi.rxsnp.flitv.peek().litToBoolean

    rxrspCredits.advance(rxLinkRun, rxLinkReq, rxLinkAck, rxrspGrant, rxrspFlit, cycle)
    rxdatCredits.advance(rxLinkRun, rxLinkReq, rxLinkAck, rxdatGrant, rxdatFlit, cycle)
    rxsnpCredits.advance(rxLinkRun, rxLinkReq, rxLinkAck, rxsnpGrant, rxsnpFlit, cycle)
    hnRxLinkRun = rxLinkRun
  }

  def stepWithCredits(cycle: Int): Unit = {
    driveTxLCredits(cycle)
    recordPreAssertTrace(cycle)
    try {
      advanceTxLCredits(cycle)
      advanceRxLCredits(cycle)
      dut.clock.step(1)
    } catch {
      case t: Throwable =>
        println(s"SIM_EXCEPTION cycle=$cycle class=${t.getClass.getName} message=${Option(t.getMessage).getOrElse("<none>")}")
        println("TRACE_WINDOW_BEGIN")
        preAssertTrace.foreach(println)
        println("TRACE_WINDOW_END")
        throw t
    }
  }

  def canDriveRxSnp: Boolean = rxsnpCredits.canSend(hnRxLinkRun)
  def canDriveRxDat: Boolean = rxdatCredits.canSend(hnRxLinkRun)
  def canDriveRxRsp: Boolean = rxrspCredits.canSend(hnRxLinkRun)
}
