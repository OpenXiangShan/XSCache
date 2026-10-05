package oceanus

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.bundle.{CHIBundleDAT, CHIBundleREQ, CHIBundleRSP, CHIBundleSNP}
import oceanus.chi.intf.CHIRNFInterface
import xijiang.{Node, NodeType}
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit, SnoopFlit}
import zhujiang.device.socket.{SocketDevSide, SocketIcnSideBundle}

/*  Oceanus L2 (CHIRNFInterface: L-credit channels + link activation/SYSCO) to
 *  ZhuJiang CC socket (plain Decoupled flit channels — xijiang has no credit
 *  or link-layer concept) bridge.
 *
 *  - Link layer: same policy as TestTop_L2Avocado — link always up, always
 *    active, coherency always granted (the L2's link/sysco pins never leave
 *    the DUT).
 *  - TX channels (L2 -> ZhuJiang, REQ/RSP/DAT): L-credit sink. A per-channel
 *    queue (depth 4) plus a granted-not-consumed credit counter guarantees
 *    `count + inflight <= depth`, so any flit the L2 sends on a held credit
 *    is always accepted (asserted below).
 *  - RX channels (ZhuJiang -> L2, SNP/RSP/DAT): credit source. Socket flits
 *    queue up; a flit is presented (flitv) only while holding an unconsumed
 *    credit granted by the L2's lcrdv. flitpend tracks the queue non-empty
 *    level (asserted at least as early as flitv, never later).
 *  - Fields ZhuJiang does not transport (DataCheck/Poison/MPAM/RSVDC/
 *    TraceTag/...) are dropped on TX and re-terminated on RX; DataCheck is
 *    re-synthesized as per-byte XOR parity so the L2 keeps its pinned
 *    CHI-E configuration (identical to the Avocado DUT build).
 */
trait HasOceanusZhuJiangBridge {

  // -- width-fit assign: zero-extend when the sink is wider, truncate
  //    otherwise (used only where truncation is provably safe). -----------
  protected def fitAssign(dst: UInt, src: UInt): Unit = {
    if (dst.getWidth >= src.getWidth) dst := src
    else dst := src(dst.getWidth - 1, 0)
  }

  // -- canonical-name -> UInt view of an oceanus flit (absent fields dropped)
  protected def viewMap(views: Seq[(String, Option[UInt])]): Map[String, UInt] =
    views.collect { case (n, Some(u)) => n -> u }.toMap

  // -- zero every flit field not listed (zero-width fields included: Chisel
  //    still requires a driver for them) ----------------------------------
  protected def zeroExcept(dst: Bundle, keep: Set[String]): Unit =
    dst.elements.foreach { case (n, f) =>
      if (!keep(n)) f := 0.U(f.getWidth.W)
    }

  // -- drive an oceanus flit from a ZhuJiang flit: assignments are keyed by
  //    canonical field name and land on the physical union carrier (via the
  //    primary-alias map); every unlisted field is tied to zero. -----------
  protected def driveOceanus(dst: Bundle, assigns: Map[String, UInt]): Unit =
    dst.elements.foreach { case (phys, f) =>
      assigns.get(TestTop_CHIExport.primaryOf(phys)) match {
        case Some(v) => fitAssign(f.asInstanceOf[UInt], v)
        case None    => f := 0.U(f.getWidth.W)
      }
    }

  // ------------------------------------------------------------------------
  // oceanus -> ZhuJiang field maps (TX direction)
  // ------------------------------------------------------------------------
  protected def mapZJReq(dst: ReqFlit, src: CHIBundleREQ): Unit = {
    val v = viewMap(TestTop_CHIExport.req(src))
    fitAssign(dst.QoS,        v("QoS"))
    fitAssign(dst.TgtID,      v("TgtID"))
    fitAssign(dst.SrcID,      v("SrcID"))
    fitAssign(dst.TxnID,      v("TxnID"))
    fitAssign(dst.Opcode,     v("Opcode"))
    fitAssign(dst.Size,       v("Size"))
    fitAssign(dst.Addr,       v("Addr"))
    fitAssign(dst.Order,      v("Order"))
    fitAssign(dst.MemAttr,    v("MemAttr"))
    fitAssign(dst.SnpAttr,    v("SnpAttr"))
    fitAssign(dst.Excl,       v("Excl"))
    fitAssign(dst.ExpCompAck, v("ExpCompAck"))
    dst.ReturnNID.foreach(r => if (r.getWidth > 0) r := 0.U)
    dst.ReturnTxnID.foreach(r => if (r.getWidth > 0) r := 0.U)
    zeroExcept(dst, Set("QoS", "TgtID", "SrcID", "TxnID", "Opcode", "Size", "Addr",
      "Order", "MemAttr", "SnpAttr", "Excl", "ExpCompAck", "ReturnNID", "ReturnTxnID"))
  }

  protected def mapZJRsp(dst: RespFlit, src: CHIBundleRSP): Unit = {
    val v = viewMap(TestTop_CHIExport.rsp(src))
    fitAssign(dst.QoS,      v("QoS"))
    fitAssign(dst.TgtID,    v("TgtID"))
    fitAssign(dst.SrcID,    v("SrcID"))
    fitAssign(dst.TxnID,    v("TxnID"))
    fitAssign(dst.Opcode,   v("Opcode"))
    fitAssign(dst.RespErr,  v("RespErr"))
    fitAssign(dst.Resp,     v("Resp"))
    fitAssign(dst.FwdState, v("FwdState"))
    fitAssign(dst.CBusy,    v("CBusy"))
    fitAssign(dst.DBID,     v("DBID"))
    zeroExcept(dst, Set("QoS", "TgtID", "SrcID", "TxnID", "Opcode", "RespErr",
      "Resp", "FwdState", "CBusy", "DBID"))
  }

  protected def mapZJDat(dst: DataFlit, src: CHIBundleDAT): Unit = {
    val v = viewMap(TestTop_CHIExport.dat(src))
    fitAssign(dst.QoS,        v("QoS"))
    fitAssign(dst.TgtID,      v("TgtID"))
    fitAssign(dst.SrcID,      v("SrcID"))
    fitAssign(dst.TxnID,      v("TxnID"))
    fitAssign(dst.HomeNID,    v("HomeNID"))
    fitAssign(dst.Opcode,     v("Opcode"))
    fitAssign(dst.RespErr,    v("RespErr"))
    fitAssign(dst.Resp,       v("Resp"))
    fitAssign(dst.DataSource, v("DataSource"))
    fitAssign(dst.CBusy,      v("CBusy"))
    fitAssign(dst.DBID,       v("DBID"))
    fitAssign(dst.DataID,     v("DataID"))
    fitAssign(dst.Data,       v("Data"))
    fitAssign(dst.BE,         v("BE"))
    zeroExcept(dst, Set("QoS", "TgtID", "SrcID", "TxnID", "HomeNID", "Opcode",
      "RespErr", "Resp", "DataSource", "CBusy", "DBID", "DataID", "Data", "BE"))
  }

  // ------------------------------------------------------------------------
  // ZhuJiang -> oceanus field maps (RX direction)
  // ------------------------------------------------------------------------
  protected def mapOceanusRsp(dst: CHIBundleRSP, src: RespFlit): Unit =
    driveOceanus(dst, Map(
      "QoS" -> src.QoS, "TgtID" -> src.TgtID, "SrcID" -> src.SrcID, "TxnID" -> src.TxnID,
      "Opcode" -> src.Opcode, "RespErr" -> src.RespErr, "Resp" -> src.Resp,
      "FwdState" -> src.FwdState, "CBusy" -> src.CBusy, "DBID" -> src.DBID))

  protected def mapOceanusDat(dst: CHIBundleDAT, src: DataFlit): Unit = {
    // Re-synthesize CHI DataCheck (one parity bit per data byte) and clear
    // Poison: ZhuJiang transports neither, and the L2 keeps DataCheck/Poison
    // present in its pinned CHI-E configuration.
    val dataCheck = Cat((0 until 32).map(i => src.Data(i * 8 + 7, i * 8).xorR).reverse)
    driveOceanus(dst, Map(
      "QoS" -> src.QoS, "TgtID" -> src.TgtID, "SrcID" -> src.SrcID, "TxnID" -> src.TxnID,
      "HomeNID" -> src.HomeNID, "Opcode" -> src.Opcode, "RespErr" -> src.RespErr,
      "Resp" -> src.Resp, "FwdState" -> src.DataSource, "CBusy" -> src.CBusy,
      "DBID" -> src.DBID, "DataID" -> src.DataID, "Data" -> src.Data, "BE" -> src.BE,
      "DataCheck" -> dataCheck, "Poison" -> 0.U(1.W)))
  }

  protected def mapOceanusSnp(dst: CHIBundleSNP, src: SnoopFlit): Unit =
    driveOceanus(dst, Map(
      "QoS" -> src.QoS, "SrcID" -> src.SrcID, "TxnID" -> src.TxnID,
      "FwdNID" -> src.FwdNID, "FwdTxnID" -> src.FwdTxnID, "Opcode" -> src.Opcode,
      "Addr" -> src.Addr, "DoNotGoToSD" -> src.DoNotGoToSD, "RetToSrc" -> src.RetToSrc,
      "NS" -> 0.U(1.W), "TraceTag" -> 0.U(1.W), "MPAM" -> 0.U(1.W)))

  // ------------------------------------------------------------------------
  // Channel terminators
  // ------------------------------------------------------------------------

  /* TX credit sink: consumes `flitv` into a queue and grants L-credits
   * (`lcrdv`) such that a credited flit always has a queue slot. Returns the
   * enqueue side — the caller drives `.bits` with the mapped ZhuJiang flit.
   * The dequeue side is connected to `dst`. */
  protected def zjTxTerm[Z <: Bundle](dst: DecoupledIO[Z], gen: Z, depth: Int,
                                      flitv: Bool, lcrdv: Bool): DecoupledIO[Z] = {
    val q = Module(new Queue(gen, depth))
    val used = RegInit(0.U(8.W)) // queue occupancy (enq fires minus deq fires)
    when(q.io.enq.fire && !q.io.deq.fire) { used := used + 1.U }
    when(!q.io.enq.fire && q.io.deq.fire) { used := used - 1.U }
    val inflight = RegInit(0.U(8.W)) // credits granted to the L2, not yet consumed
    val grant = (used + inflight) < depth.U
    lcrdv := grant
    when(grant && !flitv) { inflight := inflight + 1.U }
    when(!grant && flitv) { inflight := inflight - 1.U }
    q.io.enq.valid := flitv
    assert(!flitv || q.io.enq.ready, "OceanusZhuJiangBridge: credited TX flit dropped")
    dst.valid := q.io.deq.valid
    dst.bits := q.io.deq.bits
    q.io.deq.ready := dst.ready
    q.io.enq
  }

  /* RX credit source: queues socket flits and presents one (`flitv`) only
   * while holding an unconsumed L-credit granted by the L2 (`lcrdv`).
   * Returns (flitpend, flitv, flit-bits) for the L2-facing channel. */
  protected def zjRxTerm[Z <: Bundle](src: DecoupledIO[Z], gen: Z, depth: Int,
                                      lcrdv: Bool): (Bool, Bool, Z) = {
    val q = Module(new Queue(gen, depth))
    q.io.enq.valid := src.valid
    q.io.enq.bits := src.bits
    src.ready := q.io.enq.ready
    val held = RegInit(0.U(8.W)) // credits granted by the L2, not yet consumed
    val send = q.io.deq.valid && held =/= 0.U
    when(lcrdv && !send) { held := held + 1.U }
    when(!lcrdv && send) { held := held - 1.U }
    q.io.deq.ready := send
    (q.io.deq.valid, send, q.io.deq.bits)
  }

  // ------------------------------------------------------------------------
  // Top-level connection: oceanus CHIRNFInterface <-> ZhuJiang CC socket
  // ------------------------------------------------------------------------
  protected def connectOceanusToZhuJiang(chi: CHIRNFInterface,
                                         socketIO: SocketIcnSideBundle,
                                         ccNode: Node)(implicit p: Parameters): Unit = {
    require(ccNode.nodeType == NodeType.CC, "ZhuJiang bridge requires a CC node")
    require(ccNode.socket == "sync", "ZhuJiang bridge requires a sync-socket CC node")

    val socket = Module(new SocketDevSide(ccNode))
    socketIO <> socket.io.socket

    // -- Link-active / SACTIVE / SYSCO: identical policy to TestTop_L2Avocado.
    //    Link always up (peer always requests and instantly acks), always
    //    active, coherency always granted. L2 output pins (rxlinkactiveack,
    //    txsactive, syscoreq) are intentionally left open.
    chi.rxlinkactivereq := true.B
    chi.txlinkactiveack := chi.txlinkactivereq
    chi.rxsactive := true.B
    chi.syscoack := true.B

    // -- TXREQ / TXRSP / TXDAT: L2 -> ZhuJiang (L-credit sinks)
    val txreqEnq = zjTxTerm(socket.io.icn.rx.req.get.asInstanceOf[DecoupledIO[ReqFlit]],
      new ReqFlit(), 4, chi.txreq.flitv, chi.txreq.lcrdv)
    mapZJReq(txreqEnq.bits, chi.txreq.flit)

    val txrspEnq = zjTxTerm(socket.io.icn.rx.resp.get.asInstanceOf[DecoupledIO[RespFlit]],
      new RespFlit(), 4, chi.txrsp.flitv, chi.txrsp.lcrdv)
    mapZJRsp(txrspEnq.bits, chi.txrsp.flit)

    val txdatEnq = zjTxTerm(socket.io.icn.rx.data.get.asInstanceOf[DecoupledIO[DataFlit]],
      new DataFlit(), 4, chi.txdat.flitv, chi.txdat.lcrdv)
    mapZJDat(txdatEnq.bits, chi.txdat.flit)

    // -- Unused socket channels: never injected, never drained
    socket.io.icn.rx.hpr.foreach { h => h.valid := false.B; h.bits := 0.U.asTypeOf(h.bits) }
    socket.io.icn.rx.snoop.foreach { h => h.valid := false.B; h.bits := 0.U.asTypeOf(h.bits) }
    socket.io.icn.rx.debug.foreach { h => h.valid := false.B; h.bits := 0.U.asTypeOf(h.bits) }
    socket.io.icn.tx.req.foreach(_.ready := false.B)
    socket.io.icn.tx.hpr.foreach(_.ready := false.B)
    socket.io.icn.tx.debug.foreach(_.ready := false.B)

    // -- RXRSP / RXDAT / RXSNP: ZhuJiang -> L2 (credit sources)
    val (rxrspPend, rxrspV, rxrspBits) = zjRxTerm(socket.io.icn.tx.resp.get.asInstanceOf[DecoupledIO[RespFlit]],
      new RespFlit(), 4, chi.rxrsp.lcrdv)
    mapOceanusRsp(chi.rxrsp.flit, rxrspBits)
    chi.rxrsp.flitpend := rxrspPend
    chi.rxrsp.flitv := rxrspV

    val (rxdatPend, rxdatV, rxdatBits) = zjRxTerm(socket.io.icn.tx.data.get.asInstanceOf[DecoupledIO[DataFlit]],
      new DataFlit(), 4, chi.rxdat.lcrdv)
    mapOceanusDat(chi.rxdat.flit, rxdatBits)
    chi.rxdat.flitpend := rxdatPend
    chi.rxdat.flitv := rxdatV

    val (rxsnpPend, rxsnpV, rxsnpBits) = zjRxTerm(socket.io.icn.tx.snoop.get.asInstanceOf[DecoupledIO[SnoopFlit]],
      new SnoopFlit(), 4, chi.rxsnp.lcrdv)
    mapOceanusSnp(chi.rxsnp.flit, rxsnpBits)
    chi.rxsnp.flitpend := rxsnpPend
    chi.rxsnp.flitv := rxsnpV
  }
}
