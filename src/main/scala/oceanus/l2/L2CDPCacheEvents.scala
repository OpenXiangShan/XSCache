package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.MemReqSource

// C5 event ledger: reference Prefetcher.scala:323-330/:407-411 has one shared
// train arbiter. Cache observations are Valid pulses, so select eligible VPN
// and Filter events independently and count every unselected pulse. Neither
// arbiter can stall the cache. Used and victim PA events are never combined.
// MainPipe.scala:731 accepts ordinary demand hits; :734-737 accepts only
// unused CDP victims. Here first demand consumption clears the source mark.
// Victim observations have no refiller reqSource/needHint: use deterministic
// CDP reqSource and trainEnable=true (registered C5 event-contract deviation).
class L2CDPCacheEvents(ports: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val train = Input(Vec(ports, Valid(new L2CDPTrain)))
    val evict = Input(Vec(ports, Valid(new L2CDPEvict)))
    val vpn = Valid(new OceanusCDPTrain)
    val filter = Valid(new OceanusCDPTrain)
  })
  val vpnArb = Module(new RRArbiter(new OceanusCDPTrain, ports))
  val filterArb = Module(new RRArbiter(new OceanusCDPTrain, 2 * ports))
  for (i <- 0 until ports) {
    val t = io.train(i)
    val eligible = io.enable && t.valid && t.bits.trainEnable &&
      t.bits.reqSource =/= MemReqSource.L1DataPrefetch.id.U
    val mapped = OceanusCDPTrain.fromVpnEvent(t.bits)
    mapped.prefetched := t.bits.pfSource =/= L2PfSource.NoWhere.U
    mapped.isUsed := t.bits.hit
    vpnArb.io.in(i).valid := eligible
    vpnArb.io.in(i).bits := mapped
    filterArb.io.in(i).valid := eligible && t.bits.hit
    filterArb.io.in(i).bits := mapped
    val e = io.evict(i)
    val victim = filterArb.io.in(i + ports)
    victim.valid := io.enable && e.valid && e.bits.pfSource === L2PfSource.CDP.U
    victim.bits := 0.U.asTypeOf(victim.bits)
    victim.bits.evictPaddr := e.bits.paddr
    victim.bits.pfSource := e.bits.pfSource
    victim.bits.prefetched := true.B
    victim.bits.reqSource := MemReqSource.Prefetch2L2CDP.id.U
    victim.bits.trainEnable := true.B
    victim.bits.isEvict := true.B
  }
  vpnArb.io.out.ready := true.B
  filterArb.io.out.ready := true.B
  io.vpn.valid := vpnArb.io.out.valid
  io.vpn.bits := vpnArb.io.out.bits
  io.filter.valid := filterArb.io.out.valid
  io.filter.bits := filterArb.io.out.bits
  val vpnCollisions = RegInit(0.U(64.W))
  val filterCollisions = RegInit(0.U(64.W))
  vpnCollisions := vpnCollisions + PopCount(vpnArb.io.in.map(_.valid)) - vpnArb.io.out.fire
  filterCollisions := filterCollisions + PopCount(filterArb.io.in.map(_.valid)) - filterArb.io.out.fire
  val evictions = RegInit(0.U(64.W))
  evictions := evictions + PopCount(io.evict.map(_.valid))
  Seq(vpnCollisions, filterCollisions, evictions).foreach(dontTouch(_))
}
