package xscache.coupledL2

import chisel3._
import chisel3.util._
import chisel3.reflect.DataMirror
import chisel3.ActualDirection
import chisel3.experimental.UnlocatableSourceInfo
import chisel3.simulator.ChiselSim
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.MaxHartIdBits
import org.chipsalliance.cde.config.Parameters
import utility._
import xscache.chi._
import xscache.common.BankBitsKey

object PairMiterParameters {
  val base = Parameters.empty
  val client = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "dcache", sourceId = IdRange(0, 16), supportsProbe = TransferSizes(64))))
  val manager = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0, (BigInt(1) << 48) - 1)),
    regionType = RegionType.CACHED,
    supportsAcquireB = TransferSizes(64), supportsAcquireT = TransferSizes(64),
    supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1, 64))),
    beatBytes = 32, endSinkId = 16)
  implicit val p: Parameters = base.alterPartial {
    case EdgeInKey => new TLEdgeIn(client, manager, base, UnlocatableSourceInfo)
    case EdgeOutKey => new TLEdgeOut(client, manager, base, UnlocatableSourceInfo)
    case L2ParamKey => L2Param(mshrs = 4, clientCaches = Seq(L1Param()),
      enablePerf = false, enableRollingDB = false, elaboratedTopDown = false)
    case BankBitsKey => 0
    case MaxHartIdBits => 1
    case CHIIssue => Issue.Eb
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  }
}

// Only adds observation ports; all state and transitions belong to SnoopMSHR.
class ObservableSnoop(implicit p: Parameters) extends SnoopMSHR {
  val observed = IO(Output(new Bundle {
    val pending = Bool(); val payload = Bool(); val probe = Bool()
    val pairValid = Bool(); val pair = UInt(8.W)
    val copy = Bool(); val sourceB = Bool(); val buffer = Bool()
  }))
  observed.pending := normalPostGrantReleasePendingLatched
  observed.payload := normalPostGrantReleasePayloadLatched
  observed.probe := normalProbePayloadLatched
  observed.pairValid := pairedNormalIdValid
  observed.pair := pairedNormalId
  observed.copy := io.tasks.mainpipe.valid && io.tasks.mainpipe.bits.refillToReleaseCopy
  observed.sourceB := io.tasks.source_b.valid
  observed.buffer := releaseBufPayloadReady
}

class ReplacementAdmissionPairMiter(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val request = Input(UInt(4.W)); val free = Input(UInt(4.W)); val reservation = Input(UInt(4.W))
    val parentHasChild = Input(Bool()); val registered = Input(UInt(4.W))
    val origins = Input(Vec(4, UInt(42.W))); val slots = Input(Vec(4, UInt(42.W)))
    val query = Input(UInt(42.W)); val originMask = Output(UInt(4.W))
    val accepted = Output(Bool()); val oldBlock = Output(Bool()); val admit = Output(Bool())
    val allocate = Input(Bool()); val allocPending = Input(Bool()); val allocPayload = Input(Bool())
    val allocPairValid = Input(Bool()); val allocPair = Input(UInt(8.W))
    val livePending = Input(Bool()); val livePayload = Input(Bool()); val liveProbe = Input(Bool())
    val livePairValid = Input(Bool()); val livePair = Input(UInt(8.W))
    val copyDone = Input(Bool())
    val pending = Output(Bool()); val payload = Output(Bool()); val probe = Output(Bool())
    val pairValid = Output(Bool()); val pair = Output(UInt(8.W))
    val copy = Output(Bool()); val sourceB = Output(Bool()); val buffer = Output(Bool())
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val rx = Module(new RXSNP)
  val snoop = Module(new ObservableSnoop)
  zeroInputs(rx.io); zeroInputs(snoop.io)
  // Independent old overlay: last writer wins, followed by the physical slot mux.
  val victim = WireDefault(io.origins(0))
  for (i <- 1 until 4) { when(io.request(i)) { victim := io.origins(i) } }
  val selected = Mux(io.request(1), io.reservation, PriorityEncoderOH(io.free))
  val accepted = io.request.orR && selected.orR && (io.request(3) || !io.parentHasChild)
  io.accepted := accepted
  val qualified = ReplacementAllocAddress.acceptedOrigins(io.request.asBools, accepted)
  io.originMask := VecInit(qualified).asUInt
  val oldBlocks = (0 until 4).map { i =>
    val allocating = accepted && selected(i)
    val active = allocating || io.registered(i)
    active && Mux(allocating, victim, io.slots(i)) === io.query
  }
  io.oldBlock := oldBlocks.reduce(_ || _)
  for (i <- 0 until 4) {
    rx.io.replaceAlloc(i).valid := qualified(i)
    rx.io.replaceAlloc(i).bits.set := io.origins(i)(6, 0)
    rx.io.replaceAlloc(i).bits.tag := io.origins(i)(41, 7)
    rx.io.replaceInfo(i).valid := io.registered(i)
    rx.io.replaceInfo(i).bits.set := io.slots(i)(6, 0)
    rx.io.replaceInfo(i).bits.tag := io.slots(i)(41, 7)
  }
  rx.io.rxsnp.valid := true.B
  rx.io.rxsnp.bits.addr := io.query << 3
  rx.io.rxsnp.bits.opcode := 7.U
  rx.io.task.ready := false.B
  io.admit := rx.io.task.valid
  snoop.io.alloc.valid := io.allocate
  snoop.io.alloc.bits.task.channel := 2.U
  snoop.io.alloc.bits.task.chiOpcode.get := 7.U
  // Probe remains outstanding while no task is accepted. Reallocation checks
  // register reset without needing to manufacture unrelated transaction traffic.
  snoop.io.alloc.bits.state := 0.U.asTypeOf(snoop.io.alloc.bits.state)
  snoop.io.allocPostGrantReleasePending := io.allocPending
  snoop.io.allocPostGrantReleasePayload := io.allocPayload
  snoop.io.allocPairedNormalId.valid := io.allocPairValid
  snoop.io.allocPairedNormalId.bits := io.allocPair
  snoop.io.normalRefillPostGrantReleasePending := io.livePending
  snoop.io.normalRefillPayloadFromPostGrantRelease := io.livePayload
  snoop.io.normalRefillPayloadFromProbeAck := io.liveProbe
  snoop.io.pairedNormalId.valid := io.livePairValid
  snoop.io.pairedNormalId.bits := io.livePair
  snoop.io.releaseBufCopyDone := io.copyDone
  io.pending := snoop.observed.pending; io.payload := snoop.observed.payload
  io.probe := snoop.observed.probe; io.pairValid := snoop.observed.pairValid
  io.pair := snoop.observed.pair; io.copy := snoop.observed.copy
  io.sourceB := snoop.observed.sourceB; io.buffer := snoop.observed.buffer
}

object TestReplacementAdmissionPairMiter extends App with ChiselSim {
  import PairMiterParameters._
  simulate(new ReplacementAdmissionPairMiter) { d =>
    val random = new scala.util.Random(3003)
    val query = BigInt(0x12345)
    d.io.query.poke(query.U); d.io.request.poke(0.U); d.io.free.poke(0.U)
    d.io.reservation.poke(0.U); d.io.parentHasChild.poke(false.B); d.io.registered.poke(0.U)
    (0 until 4).foreach { i => d.io.origins(i).poke((query+i).U); d.io.slots(i).poke((query+8+i).U) }
    def pairInputs(a: Boolean, ap: Boolean, ad: Boolean, av: Boolean, ai: Int,
                   lp: Boolean, ld: Boolean, probe: Boolean, lv: Boolean, li: Int, done: Boolean): Unit = {
      d.io.allocate.poke(a.B); d.io.allocPending.poke(ap.B); d.io.allocPayload.poke(ad.B)
      d.io.allocPairValid.poke(av.B); d.io.allocPair.poke(ai.U)
      d.io.livePending.poke(lp.B); d.io.livePayload.poke(ld.B); d.io.liveProbe.poke(probe.B)
      d.io.livePairValid.poke(lv.B); d.io.livePair.poke(li.U); d.io.copyDone.poke(done.B)
    }
    pairInputs(false,false,false,false,0,false,false,false,false,0,false)
    d.clock.step(3)
    var admissionChecks = 0
    // Every request combination including multi-origin, every no-slot/reserved
    // state, parent reuse and all origin aliases of the queued query.
    for (req <- 0 until 16; free <- Seq(0,1,2,4,8,15); reservation <- Seq(0,1,4);
         child <- Seq(false,true); hit <- 0 until 5) {
      d.io.request.poke(req.U); d.io.free.poke(free.U); d.io.reservation.poke(reservation.U)
      d.io.parentHasChild.poke(child.B)
      val selected = if ((req & 2) != 0) reservation else free & -free
      val reg = (15 ^ selected) & random.nextInt(16)
      d.io.registered.poke(reg.U)
      for (i <- 0 until 4) {
        d.io.origins(i).poke((if (hit == i) query else query+i+1).U)
        d.io.slots(i).poke((if (hit == 4 && i == 2) query else query+16+i).U)
      }
      val winner = (0 until 4).filter(i => (req & (1 << i)) != 0).lastOption
      val accepted = req != 0 && selected != 0 && ((req & 8) != 0 || !child)
      val expectedMask = if (accepted) 1 << winner.get else 0
      d.io.originMask.expect(expectedMask.U); d.io.accepted.expect(accepted.B)
      val oldBlock = d.io.oldBlock.peek().litToBoolean
      d.io.admit.expect((!oldBlock).B)
      admissionChecks += 1
    }
    // Independent software state: allocation overrides live updates, later
    // live events latch, and the next allocation clears all prior provenance.
    var pending=false; var payload=false; var probe=false; var pv=false; var id=0; var buf=false
    var pairChecks=0
    for (cycle <- 0 until 2000) {
      val alloc = cycle % 11 == 0
      val ap=random.nextBoolean(); val ad=ap && random.nextBoolean()
      val av=ap; val ai=random.nextInt(4)
      val lp=random.nextBoolean(); val ld=lp && random.nextBoolean()
      val pr=random.nextBoolean(); val lv=lp; val li=random.nextInt(4)
      val done = !alloc && random.nextInt(9)==0
      pairInputs(alloc,ap,ad,av,ai,lp,ld,pr,lv,li,done)
      d.clock.step()
      if (alloc) { pending=ap; payload=ad; probe=false; pv=av; id=ai; buf=false }
      else { pending ||= lp; payload ||= ld; probe ||= pr; if(lv){pv=true;id=li}; buf ||= done }
      d.io.pending.expect(pending.B); d.io.payload.expect(payload.B); d.io.probe.expect(probe.B)
      d.io.pairValid.expect(pv.B); d.io.pair.expect(id.U); d.io.buffer.expect(buf.B)
      d.io.sourceB.expect((!(pending || lp)).B)
      d.io.copy.expect(((pending || lp) && (payload || ld) && !buf && pv).B)
      pairChecks += 1
    }
    println(s"PASS: actual RXSNP admission + production priority qualifier: $admissionChecks old/new overlay checks; actual SnoopMSHR registers/SourceB/copy: $pairChecks cycles")
  }
}
