// Intent (audit-fix-1): retain raw disabled observations, but require local
// and global arbitration to select only enabled candidates.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.ChiselSim
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import scala.collection.mutable

// Phase 1b intent: observe actual upstream acceptance and actual RBE dequeue,
// then compare against a host transaction scoreboard. Never force RTL state.
class L2TrainProbe(implicit p: Parameters) extends Bundle {
  // Intent (3a): independently verify permission/source identity from the
  // accepted CCHI opcode, alongside the existing VA/source/enable scoreboard.
  val inOpcode = UInt(6.W)
  val opcode = UInt(6.W)
  val accept = Bool()
  val inAddr = UInt(48.W)
  val inSrc = UInt(4.W)
  val inTxn = UInt(8.W)
  val inContext = new L2PrefetchTrainContext
  val execute = Bool()
  val addr = UInt(48.W)
  val src = UInt(4.W)
  val txn = UInt(8.W)
  val context = new L2PrefetchTrainContext
  val activePrefetch = Bool()
  val hit = Bool()
  val prefetched = Bool()
  val train = Bool()
  val event = new L2PrefetchTrain(true)
}
class L2TrainObservation(slices: Int, entries: Int)(implicit p: Parameters) extends Bundle {
  val req = Output(Vec(slices, Vec(entries, new L2TrainProbe)))
  val sliceValid = Output(Vec(slices, Bool()))
  val sliceEvent = Output(Vec(slices, new L2PrefetchTrain(true)))
  val globalValid = Output(Bool())
  val globalEvent = Output(new L2PrefetchTrain(true))
}
object L2TopTrainObservation {
  def attach(top: L2Top, entries: Int)(implicit p: Parameters): Option[L2TrainObservation] = {
    if (!top.config.pfTrainEnable) {
      require(top.slices.forall(s => s.io.UpRXREQTrain.isEmpty && s.io.bopTrain.bits.vaddr.isEmpty &&
        s.tshrs.forall(t => t.rbeREQ.io.in.bits.train.isEmpty && t.vPipeREQ.io.UpRXREQTrain.isEmpty)))
      None
    } else {
      def read[T <: Data](signal: T): T = BoringUtils.bore(signal)
      val obs = IO(new L2TrainObservation(top.config.sliceNum, entries))
      top.slices.zipWithIndex.foreach { case (slice, s) =>
        obs.sliceValid(s) := read(slice.io.bopTrain.valid)
        obs.sliceEvent(s) := read(slice.io.bopTrain.bits)
        slice.tshrs.zipWithIndex.foreach { case (t, i) =>
          val probe = obs.req(s)(i)
          val q = t.rbeREQ
          val v = t.vPipeREQ
          require(q.io.in.bits.train.get.getWidth == 44)
          probe.accept := read(q.io.in.valid) && read(q.io.in.ready)
          probe.inAddr := read(t.io.UpRXREQ.Addr)
          probe.inSrc := read(t.io.UpRXREQ.SrcID)
          probe.inTxn := read(t.io.UpRXREQ.TxnID)
          probe.inOpcode := read(t.io.UpRXREQ.Opcode)
          probe.inContext := read(q.io.in.bits.train.get)
          probe.execute := read(v.io.UpRXREQ.valid)
          probe.addr := read(v.io.tshr_paddr)
          probe.src := read(v.io.UpRXREQ.bits.SrcID)
          probe.txn := read(v.io.UpRXREQ.bits.TxnID)
          probe.opcode := read(v.io.UpRXREQ.bits.Opcode)
          probe.context := read(v.io.UpRXREQTrain.get)
          probe.activePrefetch := read(v.active) && read(v.p_rxreq.SrcID) === top.config.pfSrcID.U
          probe.hit := read(v.io.tshr_dirResult.hit)
          probe.prefetched := read(v.io.tshr_dirResult.pfSource) =/= 0.U
          probe.train := read(v.io.bopTrain.valid)
          probe.event := read(v.io.bopTrain.bits)
        }
      }
      obs.globalValid := read(top.bopTrainArb.io.out.valid)
      obs.globalEvent := read(top.bopTrainArb.io.out.bits)
      Some(obs)
    }
  }
}

object L2TopTrainStimulus extends ChiselSim {
  def driven: Boolean = sys.props.get("oceanus.phase1b.drive").contains("true")
  // VA page offsets must agree with PA; VPN deliberately differs and includes
  // bit 37. Different source/transaction IDs generate different request VAs.
  def fields(addr: BigInt, src: BigInt, txn: BigInt): (BigInt, BigInt, Boolean) = {
    val vpn = ((BigInt(1) << 37) | ((addr >> 12) ^ (src << 17) ^ (txn << 23) ^ 0x13579)) & ((BigInt(1) << 38) - 1)
    ((vpn << 6) | ((addr >> 6) & 63), (txn + src * 7 + (addr >> 6)) & 31, ((txn ^ (addr >> 6)) & 1) == 0)
  }
  def poke(train: L2PrefetchTrainSideband, addr: BigInt, src: BigInt, txn: BigInt): Unit = {
    val (va, source, enable) = fields(addr, src, txn)
    train.vaddrBlock.poke(va.U); train.reqSource.poke(source.U); train.trainEnable.poke(enable.B)
  }
}

// Expectations originate at the exported port's handshake, not from any
// internal context. Queue residence is tracked by transaction identity and PA.
class L2TopTrainChecker(dut: TestTop_L2Top) extends ChiselSim {
  case class Expected(addr: BigInt, src: BigInt, txn: BigInt, vpn: BigInt, source: BigInt, enable: Boolean, at: Int, late: Boolean, opcode: BigInt)
  private val waiting = mutable.Map.empty[(Int, Int), Expected]
  var accepts = 0; var executions = 0; var misses = 0; var hits = 0
  var disabled = 0; var internal = 0; var lateHits = 0; var simultaneousAccepts = 0; var simultaneousTrains = 0
  // Count disabled raw inputs before arbitration, and actual enabled BOP fires.
  var disabledBeforeArb = 0; var acceptedByBop = 0
  var mixedLocal = 0; var mixedGlobal = 0
  private def value(x: UInt): BigInt = x.peek().litValue
  private def flag(x: Bool): Boolean = x.peek().litToBoolean
  private def context(c: L2PrefetchTrainContext): (BigInt, BigInt, Boolean) = (value(c.vpn), value(c.reqSource), flag(c.trainEnable))
  private def event(e: L2PrefetchTrain): (BigInt, BigInt, BigInt, Boolean) =
    (value(e.paddr), value(e.vaddr.get), value(e.reqSource.get), flag(e.trainEnable.get))
  def pending: Int = waiting.size
  def sample(cycle: Int): Unit = dut.trainObservation.foreach { obs =>
    if (dut.reset.peek().litValue != 0) waiting.clear()
    else {
      val accepted = dut.io.t1p.zipWithIndex.flatMap { case (port, index) =>
        if (flag(port.UpREQ.valid) && flag(port.UpREQ.ready)) {
          val train = dut.io.pfTrain.get(index)
          Some((value(port.UpREQ.bits.SrcID), value(port.UpREQ.bits.TxnID), value(port.UpREQ.bits.Addr)) ->
            (value(train.vaddrBlock) >> 6, value(train.reqSource), flag(train.trainEnable)))
        } else None
      }.toMap
      val acceptedOpcodes = dut.io.t1p.flatMap { port =>
        if(flag(port.UpREQ.valid) && flag(port.UpREQ.ready))
          Some((value(port.UpREQ.bits.SrcID), value(port.UpREQ.bits.TxnID), value(port.UpREQ.bits.Addr)) -> value(port.UpREQ.bits.Opcode))
        else None
      }.toMap
      if (accepted.size > 1) simultaneousAccepts += 1
      val local = mutable.ArrayBuffer.empty[(BigInt, BigInt, BigInt, Boolean)]
      obs.req.zipWithIndex.foreach { case (row, s) =>
        val trained = mutable.ArrayBuffer.empty[(BigInt, BigInt, BigInt, Boolean)]
        row.zipWithIndex.foreach { case (r, t) =>
          if (flag(r.execute)) {
            val e = waiting.remove((s, t)).getOrElse(throw new AssertionError(s"unmatched REQ execution slice=$s tshr=$t cycle=$cycle"))
            assert((value(r.addr), value(r.src), value(r.txn)) == ((e.addr & ~BigInt(63)), e.src, e.txn))
            assert(value(r.opcode) == e.opcode, "CCHI opcode changed while buffered")
            assert(context(r.context) == ((e.vpn, e.source, e.enable)), s"context changed in RBE: $e cycle=$cycle")
            val unique = Set(oceanus.compactchi.CCHIOpcode.ReadUnique.opcode,
              oceanus.compactchi.CCHIOpcode.MakeUnique.opcode)(e.opcode.toInt)
            assert(flag(r.event.needT.get) == unique)
            assert(value(r.event.sourceId.get) == ((e.src << 8) | e.txn))
            executions += 1
            if (e.src == dut.l2cfg.pfSrcID) {
              assert(context(r.context) == ((BigInt(0), BigInt(0), false)))
              assert(!flag(r.train) && event(r.event) == ((value(r.addr), BigInt(0), BigInt(0), false)))
              internal += 1
            }
            if (flag(r.train)) {
              val expected = (e.addr & ~BigInt(63), (e.vpn << 6) | ((e.addr >> 6) & 63), e.source, e.enable)
              assert(event(r.event) == expected, s"training fields differ: $e actual=${event(r.event)}")
              trained += expected
              if (!e.enable) disabled += 1
              if (flag(r.hit)) { assert(flag(r.prefetched)); hits += 1; if (e.late) lateHits += 1 }
              else misses += 1
            }
          }
          if (flag(r.accept)) {
            val addr = value(r.inAddr); val src = value(r.inSrc); val txn = value(r.inTxn)
            val expected = accepted.getOrElse((src, txn, addr), (BigInt(0), BigInt(0), false))
            acceptedOpcodes.get((src, txn, addr)).foreach(op => assert(value(r.inOpcode) == op))
            assert(context(r.inContext) == expected, s"upstream-to-RBE mismatch src=$src txn=$txn addr=$addr cycle=$cycle")
            assert(!waiting.contains((s, t)), s"occupied REQ RBE overwritten slice=$s tshr=$t")
            waiting((s, t)) = Expected(addr, src, txn, expected._1, expected._2, expected._3, cycle, flag(r.activePrefetch) && src != dut.l2cfg.pfSrcID, value(r.inOpcode))
            accepts += 1
          }
        }
        val enabled = trained.filter(_._4)
        disabledBeforeArb += trained.count(e => !e._4)
        if (enabled.nonEmpty && enabled.size != trained.size) mixedLocal += 1
        assert(flag(obs.sliceValid(s)) == enabled.nonEmpty)
        if (enabled.nonEmpty) { assert(event(obs.sliceEvent(s)) == enabled.head); local += enabled.head }
      }
      if (local.size > 1) simultaneousTrains += 1
      val raw = obs.req.flatten.filter(r => flag(r.train))
      if (raw.exists(r => !flag(r.event.trainEnable.get)) && local.nonEmpty) mixedGlobal += 1
      assert(flag(obs.globalValid) == local.nonEmpty)
      if (local.nonEmpty) assert(local.contains(event(obs.globalEvent)), s"global arbiter mixed payload ${event(obs.globalEvent)} from $local")
      // Intent (1c-1): check the actual BOP boundary against the independently
      // checked global event; disabled events never reach scoring/delay/RR.
      val expectedBopValid = flag(obs.globalValid) && flag(obs.globalEvent.trainEnable.get)
      assert(flag(dut.observation.bopTrainValid) == expectedBopValid, s"trainEnable gate mismatch cycle=$cycle")
      if (!expectedBopValid) assert(!flag(dut.observation.bopTrainFire))
      if (flag(obs.globalValid)) assert(flag(obs.globalEvent.trainEnable.get), "disabled global training")
      if (flag(dut.observation.bopTrainFire)) acceptedByBop += 1
    }
  }
  def report(): Unit = println(s"VA_CHECK_RESULT pass=true accepts=$accepts executions=$executions miss=$misses prefetchedHit=$hits disabled=$disabled internal=$internal lateHit=$lateHits simultaneousAccept=$simultaneousAccepts simultaneousTrain=$simultaneousTrains pending=$pending")
}
