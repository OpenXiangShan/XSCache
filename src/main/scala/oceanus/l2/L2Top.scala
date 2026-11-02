// Intent (audit-fix-1): reject disabled training before each lossy arbitration;
// generate CDP observation/storage only when training observation is enabled.
package oceanus.l2

// Intent (3a): replace the optional VBOP stub with a reference-derived VBOP;
// copy global event pulses into its four-entry drop-new queue. Retain the
// legacy vbopStubEnable name as the generation switch for compatibility.

// Intent (phase2): optional deterministic VBOP/CDP producers exercise VA/TLB,
// source queues and recursive Meta depth. With neither generated, retain the
// existing routing/timing. In stub mode isolate receiver fallback traffic so
// receiver priority is independent of the provenance encoded in its TxnID.

// Intent (1c-2): RR one CDP half-line globally into an observable empty sink;
// expose parallel event vectors and integration counters without an algorithm.

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
import xscache.coupledL2.{PrefetchCtrlFromCore, PrefetchRecv, L2TlbReq, L2TlbResp, PMPRespBundle}


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

// Sibling of each Type-1 UpREQ, with the same valid/ready/fire. TL user.vaddr
// is a block VA (VA[49:6]); user.reqSource is the 5-bit MemReqSource enum;
// user.PrefetchKey is trainEnable (coupledL2 SinkA.scala:76-86). These fields
// describe demand training, not the 4-bit directory prefetch-source encoding.
class L2PrefetchTrainSideband extends Bundle {
  val vaddrBlock = UInt(44.W)
  val reqSource = UInt(5.W)
  val trainEnable = Bool()
}

// The fork derives TLB widths from a negotiated TileLink edge. Oceanus has
// CCHI instead: reuse the fork field definitions, overriding only their width
// accessors (full VA50 and configured physical address width), without a TL node.
class L2PrefetchTlbIO(implicit val p: Parameters) extends Bundle {
  val req = Decoupled(new L2TlbReq {
    override def fullVAddrBits: Int = 50
  })
  val req_kill = Output(Bool())
  val resp = Flipped(Decoupled(new L2TlbResp {
    override def fullAddressBits: Int = p(L2ParamsKey).physicalAddrWidth
  }))
  val pmp_resp = Flipped(new PMPRespBundle)
}

// Optional fork-compatible TLB/PMP and per-Type-1 training ports. Phase 1c-1
// reserves tracked translation clients for the later VBOP/CDP integration.
class L2Configuration(
  val nodeId: Int,
  val eSAM: Boolean,
  val slices: Seq[Int],
  val upstream: L2UpstreamTable = L2UpstreamTable.defaults,
  // Static BOP generation switch. Other producers retain their own switches.
  val bopEnable: Boolean = false,
  // Issue switch, independent of bopEnable. Off keeps training and the queues
  // but forces every queue output invalid, so nothing reaches a slice.
  val prefetchIssueEnable: Boolean = true,
  // SrcID stamped on an injected prefetch. It must sit in the upstream CCHI
  // namespace and outside the upstream NID set: the test config uses nodeId=0,
  // which collides with upstream NID 0, so the downstream CHI nodeId cannot be
  // reused. 1 is the smallest value the 4-bit SrcID can hold that the default
  // upstream table (0/4/5) leaves free.
  val pfSrcID: Int = 1,
  // Width of the upstream SrcID pfSrcID is stamped into. Passed explicitly
  // because L2Configuration is not a Module and cannot see paramCCHI; the
  // default matches CCHIParameters.UpstreamNodeID_Width.
  srcIdWidth: Int = 4,
  // Defaults are the production OceanusBOPParameters; tests opt in and may
  // lower roundMax or delayQueueLatency without changing production geometry.
  val bopParams: OceanusBOPParameters = OceanusBOPParameters(),
  // Optional L1-to-L2 receiver and control ports. Keeping both absent by
  // default preserves the existing BOP-only top-level interface.
  val pfRecvEnable: Boolean = false,
  val pfCtrlEnable: Boolean = false,
  // CoupledL2 defaults: PBOP/BOP output buffering is 16 entries while L1
  // receiver sources use 32.  These are source queues, so the values remain
  // explicit configuration knobs for later VBOP/CDP integration.
  val pfBopQueueEntries: Int = 16,
  val pfRecvQueueEntries: Int = 32,
  val pfTlbEnable: Boolean = false,
  val pfTrainEnable: Boolean = false,
  val vbopStubEnable: Boolean = false,
  val cdpStubEnable: Boolean = false,
  val vbopParams: OceanusVBOPParameters = OceanusVBOPParameters(),
  val cdpParams: OceanusCDPParameters = OceanusCDPParameters()
) {
  def sliceNum = slices.size
  // Training enables cache event observation even without an algorithm sink.
  val cdpObserveEnable = pfTrainEnable || cdpStubEnable
  val stubMode = vbopStubEnable || cdpStubEnable
  require(!stubMode || (pfTlbEnable && pfTrainEnable), "VA stubs require TLB and training sideband")

  require(pfSrcID >= 0 && pfSrcID < (1 << srcIdWidth),
    s"pfSrcID=$pfSrcID is outside the $srcIdWidth-bit upstream SrcID range")
  require(!upstream.entries.map(_.nid).contains(pfSrcID),
    s"pfSrcID=$pfSrcID collides with an upstream NID: ${upstream.entries.map(_.nid).mkString(",")}")
  require(pfBopQueueEntries > 1 && (pfBopQueueEntries & (pfBopQueueEntries - 1)) == 0)
  require(pfRecvQueueEntries > 1 && (pfRecvQueueEntries & (pfRecvQueueEntries - 1)) == 0)
}

class L2Top(val config: L2Configuration)(implicit val p: Parameters) extends Module with HasL2Params {

  // Intent (phase2): a test subclass may pause issue while real producers fill
  // their queues; production uses only the existing static issue parameter.
  def prefetchIssueAllowed: Bool = config.prefetchIssueEnable.B

  val t1NIDs = config.upstream.type1.map(_.nid)
  val t4NIDs = config.upstream.type4.map(_.nid)

  val io = IO(new Bundle {
    val t1p = Vec(t1NIDs.size, new CCHIInterfaceType1)
    val t4p = Vec(t4NIDs.size, new CCHIInterfaceType4)

    val pfRecv = if (config.pfRecvEnable) Some(Input(new PrefetchRecv)) else None
    val pfCtrl = if (config.pfCtrlEnable) Some(Input(new PrefetchCtrlFromCore)) else None

    val pfTlb = if (config.pfTlbEnable) Some(new L2PrefetchTlbIO) else None
    val pfTrain = if (config.pfTrainEnable) Some(Input(Vec(t1NIDs.size, new L2PrefetchTrainSideband))) else None

    val chi = new CHIRNFInterface
  })

  // Intent (1c-1): connect the tracked adapter; future VBOP/CDP clients stay
  // idle here until phase 3, while harnesses exercise the same adapter directly.
  val pfTlbAdapter = if (config.pfTlbEnable) Some(Module(new L2PrefetchTlbAdapter)) else None
  val pfTlbClients = pfTlbAdapter.map(_.io.clients)
  pfTlbAdapter.foreach { adapter =>
    io.pfTlb.get <> adapter.io.tlb
    adapter.io.clients.foreach { client =>
      client.req.valid := false.B
      client.req.bits := 0.U
      client.resp.ready := true.B
    }
  }
  // Phase 1b: select the context from the same fired remote input as REQ;
  // the destination RBE samples it on that handshake without a port register.
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
  val slices = Seq.tabulate(config.sliceNum)(i => Module(new L2TSHRCtrl(config.sliceNum, i, config.slices(i), config.nodeId, config.pfTrainEnable)(
    p.alterPartial { case L2CDPObserveEnableKey => config.cdpObserveEnable })))

  // Source controls gate issue. delay_latency drives both BOP delay queues;
  // zero selects each algorithm's configured default (reference DelayQueue).
  val pfMasterEnable = if (config.pfCtrlEnable) io.pfCtrl.get.l2_pf_master_en else true.B
  val recvControlEnable = if (config.pfCtrlEnable) {
    pfMasterEnable && io.pfCtrl.get.l2_pf_recv_en
  } else true.B
  val bopControlEnable = if (config.pfCtrlEnable) {
    pfMasterEnable && io.pfCtrl.get.l2_pbop_en
  } else true.B
  // Intent (1c-1): expose master-qualified controls for future VBOP/CDP modules.
  val vbopControlEnable = WireDefault(if (config.pfCtrlEnable) {
    pfMasterEnable && io.pfCtrl.get.l2_vbop_en
  } else true.B)
  val cdpControlEnable = WireDefault(if (config.pfCtrlEnable) {
    pfMasterEnable && io.pfCtrl.get.l2_cdp_en
  } else true.B)
  dontTouch(vbopControlEnable)
  dontTouch(cdpControlEnable)

  val cdpData = WireDefault(0.U.asTypeOf(Valid(new L2CDPHalf)))
  val cdpDataSlice = WireDefault(0.U(log2Ceil(config.sliceNum).max(1).W))
  if (config.cdpObserveEnable) {
    val cdpDataArb = Module(new RRArbiter(new L2CDPHalf, config.sliceNum))
    cdpDataArb.suggestName("cdpDataArb")
    cdpDataArb.io.in.zip(slices).foreach { case (in, slice) => in <> slice.io.cdpData }
    cdpData.valid := cdpDataArb.io.out.valid
    cdpData.bits := cdpDataArb.io.out.bits
    cdpDataArb.io.out.ready := true.B
    cdpDataSlice := cdpDataArb.io.chosen
  } else slices.foreach(_.io.cdpData.ready := true.B)
  val cdpTrain = VecInit(slices.map(_.io.cdpTrain))
  val cdpEvict = VecInit(slices.map(_.io.cdpEvict))
  val vbopStub = if (config.vbopStubEnable) Some(Module(new OceanusVBOP(config.vbopParams))) else None
  // C5: keep the public generation knob compatible, but construct the actual
  // C1-C4 algorithm. Capture queues/RR remain at the 1c-2 boundary; no second
  // half queue is inserted. All new event logic disappears when the knob is off.
  val cdp = if (config.cdpStubEnable) Some(Module(new OceanusCDP(
    config.cdpParams, config.sliceNum, withSend = true, capturedHalves = true))) else None
  val cdpEvents = if (config.cdpStubEnable) Some(Module(new L2CDPCacheEvents(config.sliceNum * nMSHR))) else None
  vbopStub.foreach { stub =>
    stub.io.enable := vbopControlEnable
    stub.io.tlb <> pfTlbClients.get(0)
    stub.io.req.ready := true.B // downstream overwrite queues always accept
  }
  cdp.foreach { algorithm =>
    val events = cdpEvents.get
    events.io.enable := cdpControlEnable
    events.io.train := VecInit(slices.flatMap(_.io.cdpTrain.toSeq))
    events.io.evict := VecInit(slices.flatMap(_.io.cdpEvict.toSeq))
    algorithm.io.enable := cdpControlEnable
    algorithm.io.vpnTrain := events.io.vpn
    algorithm.io.filterTrain := events.io.filter
    algorithm.io.l2DetectTriggers := 0.U.asTypeOf(algorithm.io.l2DetectTriggers)
    algorithm.io.capturedHalf.get.valid := cdpData.valid
    algorithm.io.capturedHalf.get.bits.data := cdpData.bits.data
    algorithm.io.capturedHalf.get.bits.pfDepth := cdpData.bits.pfDepth
    algorithm.io.capturedHalf.get.bits.pfSource := cdpData.bits.pfSource
    algorithm.io.capturedHalf.get.bits.isHit := cdpData.bits.isHit
    algorithm.io.capturedHalf.get.bits.slice := cdpDataSlice
    // Static degree3 does not consume dynamic sent/hit counters (C6 deferred).
    algorithm.io.pfStat := 0.U.asTypeOf(algorithm.io.pfStat)
    val client = pfTlbClients.get(1)
    client.req.valid := algorithm.io.tlb.req.valid && cdpControlEnable
    client.req.bits := algorithm.io.tlb.req.bits
    algorithm.io.tlb.req.ready := client.req.ready && cdpControlEnable
    algorithm.io.tlb.resp <> client.resp
    algorithm.io.req.ready := cdpControlEnable
  }
  // Normalize once at the algorithm/source-queue boundary. The overwrite
  // queues accept every enabled producer fire; per-slice routing is unchanged.
  val cdpReq = cdp.map { algorithm =>
    val r = Wire(Decoupled(new L2PrefetchAlgorithmRequest))
    r.valid := algorithm.io.req.valid && cdpControlEnable
    r.bits.paddr := algorithm.io.req.bits.paddr
    r.bits.depth := algorithm.io.req.bits.pfDepth
    r.ready := true.B
    r
  }
  val cdpHalfDrops = VecInit(slices.map(_.io.cdpHalfDrops))
  val cdpCollisionDrops = VecInit(slices.map(_.io.cdpCollisionDrops))
  val trainEnableZeroCount = WireDefault(slices.map(_.io.trainDisabled).reduce(_ + _))
  Seq(cdpData, cdpDataSlice, cdpTrain, cdpEvict, cdpHalfDrops,
    cdpCollisionDrops, trainEnableZeroCount).foreach(x => if (config.cdpObserveEnable) dontTouch(x))
  // dontTouch requires a signal even when the optional control port is absent.
  val delayLatency = WireDefault(if (config.pfCtrlEnable) io.pfCtrl.get.l2_pf_delay_latency else 0.U(10.W))
  dontTouch(delayLatency)
  vbopStub.foreach(_.io.delayLatency := delayLatency)

  val pfTxnWidth = (new FlitREQ).TxnID.getWidth
  val pfSeqWidth = L2PrefetchTxnID.sequenceWidth(pfTxnWidth)
  val pfSequence = RegInit(0.U((pfSeqWidth max 1).W))

  // Recv is a one-cycle, fire-and-forget request. The request is offered to the
  // slice directly first; only a request that the slice arbiter does not fire
  // may enter its source queue, so one Recv flit cannot be injected twice.
  val pfRecvReqs: Seq[DecoupledIO[FlitREQ]] = if (config.pfRecvEnable) {
    val recvAddr = io.pfRecv.get.addr(47, 0)
    val recvTarget = config.sliceNum match {
      case 1 => map1(recvAddr)
      case 2 => map2(recvAddr)
      case 3 => map3(recvAddr)
      case _ => map4(recvAddr)
    }
    val recvSource = L2PfSource.fromMemReqSource(io.pfRecv.get.pf_source)
    val recvReq = WireInit(0.U.asTypeOf(new FlitREQ))
    recvReq.Opcode := CCHIOpcode.StashShared.U
    recvReq.Addr := recvAddr
    recvReq.TgtID := recvTarget
    recvReq.SrcID := config.pfSrcID.U
    recvReq.TxnID := L2PrefetchTxnID.encode(recvSource, pfSequence, pfTxnWidth)
    recvReq.ExpCompData := false.B
    recvReq.Size := CCHISize.B64.U
    recvReq.TagAlias := 0.U
    val recvValid = io.pfRecv.get.addr_valid && io.pfRecv.get.pf_en && recvControlEnable
    Seq.tabulate(config.sliceNum) { i =>
      val req = Wire(Decoupled(new FlitREQ))
      req.bits := recvReq
      req.valid := recvValid && recvTarget === config.slices(i).U
      req
    }
  } else Nil
  val recvReqFire = pfRecvReqs.map(_.fire).reduceOption(_ || _).getOrElse(false.B)

  val bop = Option.when(config.bopEnable)(Module(new OceanusBOP(config.bopParams)))
  bop.foreach(_.io.delayLatency := delayLatency)
  val bopReqValid = bop.map(_.io.req.valid).getOrElse(false.B)
  val bopReqBits = bop.map(_.io.req.bits).getOrElse(0.U(paramL2.physicalAddrWidth.W))
  val bopTrainArb = Module(new L2PrefetchTrainArb(config.sliceNum, config.pfTrainEnable))
  bopTrainArb.io.in.zip(slices).foreach { case (sink, source) =>
    sink.valid := source.io.bopTrain.valid
    sink.bits := source.io.bopTrain.bits
  }
  // Reference MainPipe.scala:750-759 filters needHint before slice arbitration.
  // Disabled events cannot consume a choice or displace another slice's pulse.
  bop.foreach { learner =>
    learner.io.train.valid := bopTrainArb.io.out.valid
    learner.io.train.bits := bopTrainArb.io.out.bits.paddr
  }
  bopTrainArb.io.out.ready := bop.map(_.io.train.ready).getOrElse(true.B)
  vbopStub.foreach { stub =>
    // Intent (3a refinement): slice training is a lossy Valid event pulse;
    // copy it even when PBOP's independent learner is momentarily not ready.
    // It cannot be replayed by the cache, and VBOP owns its own drop-new queue.
    stub.io.train.valid := bopTrainArb.io.out.valid
    stub.io.train.bits := bopTrainArb.io.out.bits
  }

  // R10 observation: the origin is the physical upstream SrcID, not reqSource
  // or the provenance source of an old prefetched line. Evictions have their
  // own bucket because their origin request no longer belongs to this event.
  val trainAccounting = if (config.pfTrainEnable) Some(Module(new L2PrefetchTrainCounters(
    config.upstream.entries.size + 1, 2 * config.sliceNum * nMSHR))) else None
  trainAccounting.foreach { counts =>
    val origins = VecInit(slices.flatMap(_.io.trainSourceNID.get.toSeq))
    val bopRaw = slices.flatMap(_.io.bopTrainRaw.get.toSeq)
    val cdpRaw = slices.flatMap(_.io.cdpTrain.toSeq)
    val evictRaw = slices.flatMap(_.io.cdpEvict.toSeq)
    counts.io.total := 0.U.asTypeOf(counts.io.total)
    counts.io.accepted := 0.U.asTypeOf(counts.io.accepted)
    val selectedOrigin = bopTrainArb.io.out.bits.sourceId.get >> p(xscache.oceanus.compactchi.CCHIParametersKey).TxnID_Width
    config.upstream.entries.zipWithIndex.foreach { case (entry,source) =>
      val rawBop = PopCount(bopRaw.zipWithIndex.map { case (v,i) => v && origins(i) === entry.nid.U })
      val rawCDP = PopCount(cdpRaw.zipWithIndex.map { case (t,i) => t.valid && origins(i) === entry.nid.U })
      counts.io.total(0)(source) := rawBop
      counts.io.total(1)(source) := rawBop
      counts.io.total(2)(source) := rawCDP
      counts.io.total(3)(source) := rawCDP
      counts.io.accepted(0)(source) := bop.map(_.io.train.fire).getOrElse(false.B) && selectedOrigin === entry.nid.U
      vbopStub.foreach { learner =>
        counts.io.accepted(1)(source) := learner.io.trainAccepted && selectedOrigin === entry.nid.U
      }
      cdp.foreach { learner =>
        val events = cdpEvents.get
        counts.io.accepted(2)(source) := learner.io.vpnTrainAccepted && origins(events.io.vpnChoice) === entry.nid.U
        counts.io.accepted(3)(source) := learner.io.filterTrainAccepted &&
          events.io.filterChoice < cdpRaw.size.U && origins(events.io.filterChoice) === entry.nid.U
      }
    }
    val victimBucket = config.upstream.entries.size
    counts.io.total(3)(victimBucket) := PopCount(evictRaw.map(_.valid))
    cdp.foreach { learner =>
      counts.io.accepted(3)(victimBucket) := learner.io.filterTrainAccepted &&
        cdpEvents.get.io.filterChoice >= cdpRaw.size.U
    }
  }

  // Internal Stash has no upstream completion; downstream IDs remain TSHR/slice IDs.
  // Source order is the policy boundary: receiver encodings precede PBOP/BOP,
  // and remaining slots leave room for VBOP/CDP. A later aging policy can
  // replace this order without changing queue or slice wiring.
  val pfSourceSlots = Seq(L2PfSource.SMS, L2PfSource.BOP, L2PfSource.PBOP,
    L2PfSource.Stream, L2PfSource.Stride, L2PfSource.TP, L2PfSource.Berti,
    L2PfSource.NL, L2PfSource.CDP)
  val pfSourcePriority = Seq(L2PfSource.SMS, L2PfSource.Stream, L2PfSource.Stride,
    L2PfSource.Berti, L2PfSource.BOP, L2PfSource.PBOP, L2PfSource.TP,
    L2PfSource.NL, L2PfSource.CDP)
  // Internal observation hook: one overwrite pulse per slice/source. It is
  // intentionally not part of the CCHI-facing IO bundle.
  val pfQueueOverwriteBySource = Wire(Vec(config.sliceNum, Vec(pfSourceSlots.size, Bool())))
  pfQueueOverwriteBySource.foreach(_.foreach(_ := false.B))
  def pfTarget(addr: UInt): UInt = config.sliceNum match {
    case 1 => map1(addr)
    case 2 => map2(addr)
    case 3 => map3(addr)
    case _ => map4(addr)
  }
  val pfQueues: Seq[DecoupledIO[FlitREQ]] = if (!config.bopEnable && !config.pfRecvEnable && !config.stubMode) {
    bop.foreach(_.io.req.ready := true.B)
    Nil
  } else {
    val prefetchTarget = config.sliceNum match {
      case 1 => map1(bopReqBits)
      case 2 => map2(bopReqBits)
      case 3 => map3(bopReqBits)
      case _ => map4(bopReqBits)
    }
    val queues = Seq.tabulate(config.sliceNum) { i =>
      val sourceQueues = pfSourceSlots.zipWithIndex.map { case (source, sourceIdx) =>
        val entries = if (source == L2PfSource.BOP || source == L2PfSource.PBOP)
          config.pfBopQueueEntries else config.pfRecvQueueEntries
        val queue = Module(new OverwriteQueue(new FlitREQ, entries = entries))
        queue.suggestName(s"pfQueue_${i}_src${source}")
        pfQueueOverwriteBySource(i)(sourceIdx) := queue.io.overwrite
        val fromBop = (source == L2PfSource.PBOP).B && bopReqValid && prefetchTarget === config.slices(i).U &&
          config.bopEnable.B && (if (config.stubMode) bopControlEnable else true.B)
        val fromRecv = if (config.pfRecvEnable && !config.stubMode) {
          // pfRecvReqs.ready is driven by the slice REQ arbiter below. A direct
          // fire owns the flit; queue admission is the fallback route only.
          pfRecvReqs(i).valid && !pfRecvReqs(i).ready &&
            L2PfSource.fromMemReqSource(io.pfRecv.get.pf_source) === source.U
        } else false.B
        val stub = if (source == L2PfSource.BOP) vbopStub.map(_.io.req)
          else if (source == L2PfSource.CDP) cdpReq else None
        val fromStub = stub.map(r => r.valid && pfTarget(r.bits.paddr) === config.slices(i).U).getOrElse(false.B)
        queue.io.enq.valid := fromBop || fromRecv || fromStub
        val bopBits = WireInit(0.U.asTypeOf(new FlitREQ))
        bopBits.Opcode := CCHIOpcode.StashShared.U
        bopBits.Addr := bopReqBits
        bopBits.TgtID := config.slices(i).U
        bopBits.SrcID := config.pfSrcID.U
        bopBits.TxnID := L2PrefetchTxnID.encode(
          L2PfSource.PBOP.U(L2PfSource.Width.W), pfSequence, pfTxnWidth)
        bopBits.ExpCompData := false.B
        bopBits.Size := CCHISize.B64.U
        bopBits.TagAlias := 0.U
        val recvBits = if (config.pfRecvEnable) pfRecvReqs(i).bits else 0.U.asTypeOf(new FlitREQ)
        val stubBits = WireInit(bopBits)
        stub.foreach { r =>
          stubBits.Addr := r.bits.paddr
          stubBits.TxnID := L2PrefetchTxnID.encode(source.U(L2PfSource.Width.W), r.bits.depth, pfSequence, pfTxnWidth)
        }
        queue.io.enq.bits := Mux(fromStub, stubBits, Mux(fromBop, bopBits, recvBits))
        (source, queue)
      }
      // Boundary history compares only lastIssuedAddr, not an outstanding CAM.
      // Duplicates missed here are serialized by the slice's paddr hit check
      // (L2TSHRAlloc.scala:207-212) and counted as redundant at REQ execution.
      // This limited history costs efficiency, not correctness.
      val lastIssuedValid = RegInit(false.B)
      val lastIssuedAddr = Reg(UInt(paramL2.physicalAddrWidth.W))
      def sourceEnabled(source: Int): Bool = if (!config.stubMode) true.B else source match {
        case L2PfSource.BOP => vbopControlEnable
        case L2PfSource.PBOP => bopControlEnable
        case L2PfSource.CDP => cdpControlEnable
        case _ => recvControlEnable
      }
      val candidates = pfSourcePriority.map { source =>
        val q = sourceQueues.find(_._1 == source).get._2
        q.io.deq.valid && sourceEnabled(source) && !(lastIssuedValid && q.io.deq.bits.Addr === lastIssuedAddr)
      }
      val chosen = PriorityEncoderOH(VecInit(candidates).asUInt)
      val issued = Wire(Decoupled(new FlitREQ))
      issued.valid := candidates.reduce(_ || _) && prefetchIssueAllowed &&
        (if (config.stubMode) true.B else if (config.bopEnable) bopControlEnable else recvControlEnable)
      issued.bits := Mux1H(pfSourcePriority.zip(chosen.asBools).map { case (source, hit) =>
        hit -> sourceQueues.find(_._1 == source).get._2.io.deq.bits
      })
      val chosenFire = issued.valid && issued.ready
      sourceQueues.foreach { case (source, queue) =>
        val idx = pfSourcePriority.indexOf(source)
        val isChosen = if (idx >= 0) chosen.asBools(idx) else false.B
        val stale = queue.io.deq.valid && lastIssuedValid && queue.io.deq.bits.Addr === lastIssuedAddr
        val sameAsChosen = queue.io.deq.valid && chosenFire && issued.bits.Addr === queue.io.deq.bits.Addr && !isChosen
        // A static issue-off configuration drains for backward-compatible
        // training-only runs; a runtime control gate holds entries so queue
        // overflow remains observable per source.
        queue.io.deq.ready := !config.prefetchIssueEnable.B || stale ||
          (isChosen && chosenFire) || sameAsChosen
      }
      when (chosenFire) {
        lastIssuedValid := true.B
        lastIssuedAddr := issued.bits.Addr
      }
      issued
    }
    when(bopReqValid) {
      assert(PopCount(VecInit(config.slices.map(nid => prefetchTarget === nid.U))) === 1.U,
        "BOP request must demux to exactly one prefetch queue")
    }
    // Ready is the selected source queue's enqueue ready, not dequeue ready:
    // dequeue backpressure must not reject training. OverwriteQueue itself is
    // always ready, so this remains a source-local admission boundary.
    bop.foreach(_.io.req.ready := true.B)
    queues
  }

  // Receiver fallback remains a separate producer in stub mode: its pfSource
  // describes origin, and must not lower its arbitration priority to CDP's.
  val recvFallback = if (config.stubMode && config.pfRecvEnable) {
    Seq.tabulate(config.sliceNum) { i =>
      val q = Module(new OverwriteQueue(new FlitREQ, config.pfRecvQueueEntries))
      q.suggestName(s"recvFallback_$i")
      q.io.enq.valid := pfRecvReqs(i).valid && !pfRecvReqs(i).ready
      q.io.enq.bits := pfRecvReqs(i).bits
      q
    }
  } else Nil

  when (bop.map(_.io.req.fire).getOrElse(false.B) || recvReqFire || vbopStub.map(_.io.req.fire).getOrElse(false.B) ||
    cdp.map(_.io.req.fire).getOrElse(false.B)) {
    pfSequence := pfSequence + 1.U
  }

  bop.foreach { learner =>
    XSPerfAccumulate("oceanus_bop_cross_page_drop", learner.io.crossPageDrop)
    XSPerfAccumulate("oceanus_bop_disabled_drop", learner.io.disabledDrop)
    XSPerfAccumulate("oceanus_bop_delay_queue_full_drop", learner.io.delayQueueFullDrop)
  }

  slices.zipWithIndex.foreach { case (slice, i) =>
    slice.io.consts.sliceIdx := i.U
    slice.io.consts.sliceNID := config.slices(i).U
    slice.io.consts.nodeId := config.nodeId.U
    // Snoops target the first listed Type-1 port: multiple Type-1 ports are
    // sources of the same single coherent client (see L2UpstreamTable).
    slice.io.consts.clientNID := t1NIDs.head.U
    slice.io.consts.pfSrcID := config.pfSrcID.U
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

  // - Upstream RXREQ routing. Local loop-back goes through the EVB channel,
  // not this arbiter. External demand wins; among prefetch sources Recv wins BOP.
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
    // Recv is input 1 and therefore wins over queued sources when the slice
    // accepts it. Its ready signal also selects the mutually exclusive queue
    // fallback in the source-queue logic above.
    val fallback = recvFallback.lift(i).map { q =>
      val out = Wire(Decoupled(new FlitREQ))
      out.bits := q.io.deq.bits
      out.valid := q.io.deq.valid && recvControlEnable && prefetchIssueAllowed
      q.io.deq.ready := out.ready && recvControlEnable && prefetchIssueAllowed
      out
    }.toSeq
    val reqInputs = slice_io_UpRXREQ_remote +: (pfRecvReqs.lift(i).toSeq ++ fallback ++ pfQueues.lift(i).toSeq)
    arb(reqInputs, slice.io.UpRXREQ, Some(s"UpRXREQ_$i"))
    slice.io.UpRXREQTrain.foreach { context =>
      // fire includes both routing arbiters and the allocator acceptance.
      // Internal Recv/BOP and Type-4 inputs have no training VA/context.
      val candidates = io.pfTrain.get.zipWithIndex.map { case (train, j) =>
        val compact = Wire(new L2PrefetchTrainContext)
        compact.vpn := train.vaddrBlock(43, 6)
        compact.reqSource := train.reqSource
        compact.trainEnable := train.trainEnable
        preArb_UpREQs(i)(j).fire -> compact
      }
      // A one-input Mux1H may collapse to its payload; explicitly zero the
      // entire context when no Type-1 request fires (including internal hints).
      context := Mux(candidates.map(_._1).reduce(_ || _), Mux1H(candidates),
        0.U.asTypeOf(context))
    }
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

  // Assert: an upstream RXRSP flit must target a configured slice NID. An
  // unmatched TgtID never fires any demux entry and the flit stalls at the
  // demux forever (sink.ready above is the OR of entry fires).
  postSAM_UpRSPs.zipWithIndex.foreach { case (flit, j) => {
    assert(!flit.valid || config.slices.map(n => flit.bits.TgtID === n.U).reduce(_ || _),
      s"UpRXRSP flit from upstream port $j with TgtID matching no slice NID @ %m")
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

  // Assert: an upstream RXDAT flit must target a configured slice NID (same
  // stall-forever failure mode as RXRSP above).
  postSAM_UpDATs.zipWithIndex.foreach { case (flit, j) => {
    assert(!flit.valid || config.slices.map(n => flit.bits.TgtID === n.U).reduce(_ || _),
      s"UpRXDAT flit from upstream port $j with TgtID matching no slice NID @ %m")
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
      when(postSAM_DnSNP.valid) {
        assert(postSAM_DnSNP.bits.TgtID =/= config.pfSrcID.U,
          "slice snoop TgtID must not be the prefetch SrcID")
      }
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
      when(postSAM_DnRSP.valid) {
        assert(postSAM_DnRSP.bits.TgtID =/= config.pfSrcID.U,
          "slice response TgtID must not be the prefetch SrcID")
      }
    }}
    fastArb(preArb_DnRSPs(i), port._1, Some(s"DnRSP_$i"))
  }}

  // Assert: a slice-emitted UpTXRSP flit must target a configured upstream
  // Type-1 port NID (its TgtID is the requester SrcID recorded at allocation).
  // An unmatched TgtID never fires any demux entry and the flit stalls at the
  // demux forever (sink.ready above is the OR of entry fires).
  postSAM_DnRSPs.zipWithIndex.foreach { case (flit, j) => {
    assert(!flit.valid || t1NIDs.map(n => flit.bits.TgtID === n.U).reduce(_ || _),
      s"UpTXRSP flit from slice $j with TgtID matching no upstream Type-1 port NID @ %m")
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
      when(postSAM_DnDAT.valid) {
        assert(postSAM_DnDAT.bits.TgtID =/= config.pfSrcID.U,
          "slice data TgtID must not be the prefetch SrcID")
      }
    }}
    fastArb(preArb_DnDATs(i), port._1, Some(s"DnDAT_$i"))
  }}

  // Assert: a slice-emitted UpTXDAT flit must target a configured upstream port
  // NID (Type-1 or Type-4). Same stall-forever failure mode as UpTXRSP above.
  postSAM_DnDATs.zipWithIndex.foreach { case (flit, j) => {
    assert(!flit.valid || (t1NIDs ++ t4NIDs).map(n => flit.bits.TgtID === n.U).reduce(_ || _),
      s"UpTXDAT flit from slice $j with TgtID matching no upstream port NID @ %m")
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
