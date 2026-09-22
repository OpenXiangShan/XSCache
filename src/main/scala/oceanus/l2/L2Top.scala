package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
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
import oceanus.chi.intf.CHIRNFInterface
import oceanus.chi.intf.CHIRNFRawInterface


sealed trait L2UpstreamPortType
object L2UpstreamPortType {
  case object Type1 extends L2UpstreamPortType
  case object Type4 extends L2UpstreamPortType
}

case class L2UpstreamTableEntry(portType: L2UpstreamPortType, nid: Int)

/* Upstream CCHI port table of an Oceanus L2: which upstream ports are exported
 * and which node ID each of them takes.
 *
 * Multiple Type-1 ports are taken as different upstream sources of the SAME
 * coherent client: they share the single client bit in the Directory (only one
 * Type-1 client is supported for now), and all their node IDs are passed to
 * L2ClientTable translated as the same client.
 *
 * NOTE: with multiple Type-1 ports, the user must guarantee that they have no
 * operational address overlapping between each other. This is a documented
 * contract only — intentionally NOT checked by any assertion. */
class L2UpstreamTable(val entries: Seq[L2UpstreamTableEntry]) {
  require(entries.nonEmpty, "L2UpstreamTable must contain at least one entry")
  require(entries.map(_.nid).distinct.size == entries.size,
    s"Duplicated node ID in L2UpstreamTable: ${entries.map(_.nid).mkString(", ")}")
  require(entries.exists(_.portType == L2UpstreamPortType.Type1),
    "L2UpstreamTable must contain at least one Type-1 port (the coherent upstream client)")

  val type1 = entries.filter(_.portType == L2UpstreamPortType.Type1)
  val type4 = entries.filter(_.portType == L2UpstreamPortType.Type4)
}

object L2UpstreamTable {
  def defaults = new L2UpstreamTable(Seq(
    L2UpstreamTableEntry(L2UpstreamPortType.Type1, 0),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 4),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4, 5)
  ))
}

class L2Configuration(
  val nodeId: Int,
  val eSAM: Boolean,
  val slices: Seq[Int],
  val upstream: L2UpstreamTable = L2UpstreamTable.defaults
) {
  def sliceNum = slices.size
}

class L2Top(val config: L2Configuration)(implicit val p: Parameters) extends Module with HasL2Params {

  val t1NIDs = config.upstream.type1.map(_.nid)
  val t4NIDs = config.upstream.type4.map(_.nid)

  val io = IO(new Bundle {
    val t1p = Vec(t1NIDs.size, new CCHIInterfaceType1)
    val t4p = Vec(t4NIDs.size, new CCHIInterfaceType4)

    val chi = new CHIRNFInterface
  })

  // ----------------------------------------------------------------

  //
  val postSAM_t1p = Wire(Vec(t1NIDs.size, new CCHIInterfaceType1))
  val postSAM_t4p = Wire(Vec(t4NIDs.size, new CCHIInterfaceType4))

  postSAM_t1p.zip(io.t1p).foreach { case (post, port) => post <> port }
  postSAM_t4p.zip(io.t4p).foreach { case (post, port) => post <> port }

  // -- Upstream External SAM (L1 has no internal sam, slice mapped at L2)
  def map1(addr: UInt): UInt = config.slices.head.U

  def map2(addr: UInt): UInt = {
    VecInit(config.slices.map(_.U))((addr >> 6).xorR)
  }

  def map3(addr: UInt): UInt = {
    val mappingAddr = addr >> 6
    val mappingTable = WireInit(VecInit((0 until 49).map(_ % 3).map(_.U(2.W))))
    val mappedIndex = mappingAddr(2, 0) + mappingAddr(5, 3) + mappingAddr(8, 6) + mappingAddr(10, 9) +
                      mappingAddr(13, 11) + mappingAddr(16, 14) + mappingAddr(19, 17) + mappingAddr(21, 20)
    VecInit(config.slices.map(_.U))(mappingTable(mappedIndex))
  }

  def map4(addr: UInt): UInt = {
    val mappingAddr = addr >> 6
    val mappedIndex0 = addr.asBools.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).xorR
    val mappedIndex1 = addr.asBools.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).xorR
    VecInit(config.slices.map(_.U))(Cat(mappedIndex1, mappedIndex0))
  }

  if (config.eSAM) {
    require(config.sliceNum >= 1 && config.sliceNum <= 4,
      s"Unsupported slice count ${config.sliceNum} under eSAM")
    val samMap: UInt => UInt = config.sliceNum match {
      case 1 => (addr: UInt) => map1(addr)
      case 2 => (addr: UInt) => map2(addr)
      case 3 => (addr: UInt) => map3(addr)
      case 4 => (addr: UInt) => map4(addr)
    }
    io.t1p.zip(postSAM_t1p).foreach { case (port, post) =>
      post.UpEVT.bits.TgtID := samMap(port.UpEVT.bits.Addr)
      post.UpREQ.bits.TgtID := samMap(port.UpREQ.bits.Addr)
    }
    io.t4p.zip(postSAM_t4p).foreach { case (port, post) =>
      post.UpREQ.bits.TgtID := samMap(port.UpREQ.bits.Addr)
    }
  }
  // ----------------------------------------------------------------

  //
  val slices = Seq.tabulate(config.sliceNum)(i => Module(new L2TSHRCtrl(config.sliceNum, i, config.slices(i), config.nodeId)))

  slices.zipWithIndex.foreach { case (slice, i) =>
    slice.io.consts.sliceIdx := i.U
    slice.io.consts.sliceNID := config.slices(i).U
    slice.io.consts.nodeId := config.nodeId.U
    // Snoops target the first listed Type-1 port: multiple Type-1 ports are
    // sources of the same single coherent client (see L2UpstreamTable).
    slice.io.consts.clientNID := t1NIDs.head.U
  }

  // - Upstream RXEVT routing
  val postSAM_UpEVTs = postSAM_t1p.map(_.UpEVT).toSeq
  val preArb_UpEVTs = Wire(Vec(config.sliceNum, Vec(postSAM_UpEVTs.size, Decoupled(new FlitEVT))))

  postSAM_UpEVTs.zip(preArb_UpEVTs.transpose).foreach { case (sink, sources) => {
    sink.ready := sources.map(_.fire).orR
  }}

  slices.zipWithIndex.foreach { case (slice, i) => {
    postSAM_UpEVTs.zipWithIndex.foreach { case (postSAM_UpEVT, j) => {
      preArb_UpEVTs(i)(j).bits := postSAM_UpEVT.bits
      preArb_UpEVTs(i)(j).valid := postSAM_UpEVT.valid && postSAM_UpEVT.bits.TgtID === config.slices(i).U
    }}
    fastArb(preArb_UpEVTs(i), slice.io.UpRXEVT, Some(s"UpRXEVT_$i"))
  }}

  // - Upstream RXREQ routing (Including local loop-back)
  val postSAM_UpREQs = postSAM_t1p.map(_.UpREQ).toSeq ++ postSAM_t4p.map(_.UpREQ).toSeq
  val preArb_UpREQs = Wire(Vec(config.sliceNum, Vec(postSAM_UpREQs.size, Decoupled(new FlitREQ))))

  postSAM_UpREQs.zip(preArb_UpREQs.transpose).foreach { case (sink, sources) => {
    sink.ready := sources.map(_.fire).orR
  }}

  slices.zipWithIndex.foreach { case (slice, i) => {
    postSAM_UpREQs.zipWithIndex.foreach { case (postSAM_UpREQ, j) => {
      preArb_UpREQs(i)(j).bits := postSAM_UpREQ.bits
      preArb_UpREQs(i)(j).valid := postSAM_UpREQ.valid && postSAM_UpREQ.bits.TgtID === config.slices(i).U
    }}
    val slice_io_UpRXREQ_remote = Wire(Decoupled(new FlitREQ))
    fastArb(preArb_UpREQs(i), slice_io_UpRXREQ_remote, Some(s"UpRXREQ_remote_$i"))
    arb(Seq(slice_io_UpRXREQ_remote), slice.io.UpRXREQ, Some(s"UpRXREQ_$i"))
  }}

  // - Upstream RXRSP routing
  val postSAM_UpRSPs = postSAM_t1p.map(_.UpRSP).toSeq
  val preArb_UpRSPs = Wire(Vec(config.sliceNum, Vec(postSAM_UpRSPs.size, Decoupled(new FlitUpRSP))))

  postSAM_UpRSPs.zip(preArb_UpRSPs.transpose).foreach { case (sink, sources) => {
    sink.ready := sources.map(_.fire).orR
  }}

  slices.zipWithIndex.foreach { case (slice, i) => {
    postSAM_UpRSPs.zipWithIndex.foreach { case (postSAM_UpRSP, j) => {
      preArb_UpRSPs(i)(j).bits := postSAM_UpRSP.bits
      preArb_UpRSPs(i)(j).valid := postSAM_UpRSP.valid && postSAM_UpRSP.bits.TgtID === config.slices(i).U
    }}
    val slice_io_UpRXRSP_decoupled = Wire(Decoupled(new FlitUpRSP))
    slice.io.UpRXRSP.bits := slice_io_UpRXRSP_decoupled.bits
    slice.io.UpRXRSP.valid := slice_io_UpRXRSP_decoupled.valid
    slice_io_UpRXRSP_decoupled.ready := true.B
    fastArb(preArb_UpRSPs(i), slice_io_UpRXRSP_decoupled, Some(s"UpRXRSP_$i"))
  }}

  // - Upstream RXDAT routing
  val postSAM_UpDATs = postSAM_t1p.map(_.UpDAT).toSeq
  val preArb_UpDATs = Wire(Vec(config.sliceNum, Vec(postSAM_UpDATs.size, Decoupled(new FlitUpDAT))))

  postSAM_UpDATs.zip(preArb_UpDATs.transpose).foreach { case (sink, sources) => {
    sink.ready := sources.map(_.fire).orR
  }}

  slices.zipWithIndex.foreach { case (slice, i) => {
    postSAM_UpDATs.zipWithIndex.foreach { case (postSAM_UpDATs, j) => {
      preArb_UpDATs(i)(j).bits := postSAM_UpDATs.bits
      preArb_UpDATs(i)(j).valid := postSAM_UpDATs.valid && postSAM_UpDATs.bits.TgtID === config.slices(i).U
    }}
    val slice_io_UpRXDAT_decoupled = Wire(Decoupled(new FlitUpDAT))
    slice.io.UpRXDAT.bits := slice_io_UpRXDAT_decoupled.bits
    slice.io.UpRXDAT.valid := slice_io_UpRXDAT_decoupled.valid
    slice_io_UpRXDAT_decoupled.ready := true.B
    fastArb(preArb_UpDATs(i), slice_io_UpRXDAT_decoupled, Some(s"UpRXDAT_$i"))
  }}

  // - Upstream TXSNP routing
  // Snoops are generated only for the first listed Type-1 NID (single coherent
  // client, see L2UpstreamTable); the demux still routes by TgtID.
  val port_DnSNPs = postSAM_t1p.map(_.DnSNP).toSeq.zip(t1NIDs)

  val postSAM_DnSNPs = slices.map(_.io.UpTXSNP)
  val preArb_DnSNPs = Wire(Vec(port_DnSNPs.size, Vec(postSAM_DnSNPs.size, Decoupled(new FlitSNP))))

  postSAM_DnSNPs.zip(preArb_DnSNPs.transpose).foreach { case (sink, sources) => {
    sink.ready := sources.map(_.fire).orR
  }}

  port_DnSNPs.zipWithIndex.foreach { case (port, i) => {
    postSAM_DnSNPs.zipWithIndex.foreach { case (postSAM_DnSNP, j) => {
      preArb_DnSNPs(i)(j).bits := postSAM_DnSNP.bits
      preArb_DnSNPs(i)(j).valid := postSAM_DnSNP.valid && postSAM_DnSNP.bits.TgtID === port._2.U
    }}
    fastArb(preArb_DnSNPs(i), port._1, Some(s"DnSNP_$i"))
  }}

  // - Upstream TXRSP routing
  val port_DnRSPs = postSAM_t1p.map(_.DnRSP).toSeq.zip(t1NIDs)

  val postSAM_DnRSPs = slices.map(_.io.UpTXRSP)
  val preArb_DnRSPs = Wire(Vec(port_DnRSPs.size, Vec(postSAM_DnRSPs.size, Decoupled(new FlitDnRSP))))

  postSAM_DnRSPs.zip(preArb_DnRSPs.transpose).foreach { case (sink, sources) => {
    sink.ready := sources.map(_.fire).orR
  }}

  port_DnRSPs.zipWithIndex.foreach { case (port, i) => {
    postSAM_DnRSPs.zipWithIndex.foreach { case (postSAM_DnRSP, j) => {
      preArb_DnRSPs(i)(j).bits := postSAM_DnRSP.bits
      preArb_DnRSPs(i)(j).valid := postSAM_DnRSP.valid && postSAM_DnRSP.bits.TgtID === port._2.U
    }}
    fastArb(preArb_DnRSPs(i), port._1, Some(s"DnRSP_$i"))
  }}

  // - Upstream TXDAT routing
  val port_DnDATs = postSAM_t1p.map(_.DnDAT).toSeq.zip(t1NIDs) ++
                    postSAM_t4p.map(_.DnDAT).toSeq.zip(t4NIDs)

  val postSAM_DnDATs = slices.map(_.io.UpTXDAT)
  val preArb_DnDATs = Wire(Vec(port_DnDATs.size, Vec(postSAM_DnDATs.size, Decoupled(new FlitDnDAT))))

  postSAM_DnDATs.zip(preArb_DnDATs.transpose).foreach { case (sink, sources) => {
    sink.ready := sources.map(_.fire).orR
  }}

  port_DnDATs.zipWithIndex.foreach { case (port, i) => {
    postSAM_DnDATs.zipWithIndex.foreach { case (postSAM_DnDAT, j) => {
      preArb_DnDATs(i)(j).bits := postSAM_DnDAT.bits
      preArb_DnDATs(i)(j).valid := postSAM_DnDAT.valid && postSAM_DnDAT.bits.TgtID === port._2.U
    }}
    fastArb(preArb_DnDATs(i), port._1, Some(s"DnDAT_$i"))
  }}


  // - Downstream CHI Link
  val chiLink = Module(new L2DownstreamCHI())

  fastArb(slices.map(_.io.DnTXREQ), chiLink.io.txreq, Some("CHI_TXREQ"))
  fastArb(slices.map(_.io.DnTXRSP), chiLink.io.txrsp, Some("CHI_TXRSP"))
  fastArb(slices.map(_.io.DnTXDAT), chiLink.io.txdat, Some("CHI_TXDAT"))

  val rxsnpSliceId = Wire(UInt(log2Ceil(config.sliceNum).W))

  if (config.eSAM) {
    if (config.sliceNum == 1) {
      rxsnpSliceId := 0.U
    } else if (config.sliceNum == 2) {
      rxsnpSliceId := map2(chiLink.io.rxsnp.bits.Addr.get << 3)
    } else if (config.sliceNum == 3) {
      rxsnpSliceId := map3(chiLink.io.rxsnp.bits.Addr.get << 3)
    } else if (config.sliceNum == 4) {
      rxsnpSliceId := map4(chiLink.io.rxsnp.bits.Addr.get << 3)
    } else {
      require(false, s"Unsupported slice count ${config.sliceNum} under eSAM")
    }
  } else {
    require(false, "L1 internal SAM not supported yet for Downstream CHI RXSNP link")
  }

  slices.zipWithIndex.foreach { case (slice, i) => {
    slice.io.DnRXSNP.bits := chiLink.io.rxsnp.bits
    slice.io.DnRXSNP.valid := chiLink.io.rxsnp.valid && rxsnpSliceId === i.U
  }}
  chiLink.io.rxsnp.ready := slices.map(_.io.DnRXSNP.ready)(rxsnpSliceId)

  slices.foreach { case (slice) => {
    slice.io.DnRXRSP.bits := chiLink.io.rxrsp.bits
    slice.io.DnRXRSP.valid := chiLink.io.rxrsp.valid && slice.routeSliceFromTxnID(chiLink.io.rxrsp.bits.TxnID.get)
  }}

  slices.foreach { case (slice) => {
    slice.io.DnRXDAT.bits := chiLink.io.rxdat.bits
    slice.io.DnRXDAT.valid := chiLink.io.rxdat.valid && slice.routeSliceFromTxnID(chiLink.io.rxdat.bits.TxnID.get)
  }}

  chiLink.io.exitco.foreach(_ := false.B) // TODO: Export when implement L2 Power Down

  io.chi <> chiLink.io.out

  //
  val clientTable = Module(new L2ClientTable(config.sliceNum, t1NIDs))

  clientTable.io.queryEVT.zip(slices.map(_.io.toClientTableEVT)).foreach { case (sinks, sources) => {
    sinks.zip(sources).foreach { case (sink, source) => sink := source }
  }}

  clientTable.io.queryREQ.zip(slices.map(_.io.toClientTableREQ)).foreach { case (sinks, sources) => {
    sinks.zip(sources).foreach { case (sink, source) => sink := source }
  }}

  clientTable.io.clientsEVT.zip(slices.map(_.io.fromClientTableEVT)).foreach { case (sources, sinks) => {
    sinks.zip(sources).foreach { case (sink, source) => sink := source }
  }}

  clientTable.io.clientsREQ.zip(slices.map(_.io.fromClientTableREQ)).foreach { case (sources, sinks) => {
    sinks.zip(sources).foreach { case (sink, source) => sink := source }
  }}

  //
  val pCreditPool = Module(new L2PCreditPool(config.sliceNum)) 

  pCreditPool.io.pCrdGrant := chiLink.io.pCrdGrant

  pCreditPool.io.mshrQuery.zip(slices.map(_.io.toPCreditPool)).foreach { case (sinks, sources) => {
    sinks.zip(sources).foreach { case (sink, source) => sink := source }
  }}

  pCreditPool.io.mshrGrant.zip(slices.map(_.io.fromPCreditPool)).foreach { case (sources, sinks) => {
    sinks.zip(sources).foreach { case (sink, source) => sink := source }
  }}
}
