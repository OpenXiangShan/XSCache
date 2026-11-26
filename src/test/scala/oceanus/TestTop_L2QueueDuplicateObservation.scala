package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.FlitREQ
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}

class QueueDuplicateObservationTop(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(2, Decoupled(new FlitREQ)))
    val out = Vec(2, Decoupled(new FlitREQ))
    val pf = Output(new L2QueueDuplicateObservation.Snapshot(4))
    val fallback = Output(new L2QueueDuplicateObservation.Snapshot(8))
    val clean = Input(Bool())
    val dump = Input(Bool())
    val history = Input(Bool())
    val same = Input(Bool())
    val issueOff = Input(Bool())
  })
  for ((entries, lane) <- Seq(4, 8).zipWithIndex) {
    val queue = Module(new OverwriteQueue(new FlitREQ, entries))
    val baseline = Module(new QueueObserveFrozenBaseline(new FlitREQ, entries))
    queue.io.enq <> io.in(lane)
    io.out(lane) <> queue.io.deq
    baseline.io.enq.valid := io.in(lane).valid
    baseline.io.enq.bits := io.in(lane).bits
    baseline.io.deq.ready := io.out(lane).ready
    assert(queue.io.enq.ready === baseline.io.enq.ready)
    assert(queue.io.deq.valid === baseline.io.deq.valid)
    assert(queue.io.overwrite === baseline.io.overwrite)
    when(queue.io.deq.valid) { assert(queue.io.deq.bits.asUInt === baseline.io.deq.bits.asUInt) }
    val snap = L2QueueDuplicateObservation.snapshot(queue)
    if (lane == 0) io.pf := snap else io.fallback := snap
    L2QueueDuplicateObservation.count(snap, s"pfobs_Q_test$lane", entries,
      mixedSources = lane == 1, stale = io.history, sameAsChosen = io.same, issueOff = io.issueOff)
  }
  XSLog.collect(0.U(64.W), true.B, io.clean, io.dump)
}

object TestTop_L2QueueDuplicateObservation extends App with ChiselSim {
  Constantin.init(false)
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params()
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(true, false, XSPerfLevel.VERBOSE, 0)
  })
  simulate(new QueueDuplicateObservationTop()(params)) { dut =>
    type Request = Map[String, BigInt]
    val sizes = Seq(4, 8)
    val queues = Array(Vector.empty[Request], Vector.empty[Request])
    val counts = scala.collection.mutable.Map.empty[String, BigInt].withDefaultValue(BigInt(0))
    val sourceShift = dut.io.in(0).bits.TxnID.getWidth - 4
    val depthShift = sourceShift - 2
    val fields = dut.io.in(0).bits.elements.keys.toSeq
    def req(pa: BigInt, source: Int = 1, depth: Int = 0, sequence: Int = 0,
      opcode: Int = 0x20): Request = fields.map(_ -> BigInt(0)).toMap ++ Map(
        "Addr" -> pa, "TxnID" -> ((BigInt(source) << sourceShift) |
          (BigInt(depth) << depthShift) | (BigInt(sequence) & ((BigInt(1) << depthShift) - 1))),
        "Opcode" -> BigInt(opcode), "Size" -> BigInt(6), "SrcID" -> BigInt(1))
    def source(r: Request): Int = (r("TxnID") >> sourceShift).toInt
    def key(r: Request) = (r("Addr") >> 6, r("TxnID") >> depthShift,
      r -- Seq("Addr", "TxnID"))
    def poke(r: Request, b: FlitREQ): Unit = b.elements.foreach {
      case (name, value: Bool) => value.poke((r(name) != 0).B)
      case (name, value: UInt) => value.poke(r(name).U)
      case (name, _) => sys.error(s"unexpected field $name")
    }
    def add(prefix: String, name: String, value: BigInt): Unit = counts(prefix + "_" + name) += value
    var cycles = 0
    var warmup = true
    def step(inputs: Seq[Option[Request]], ready: Boolean, clean: Boolean = false,
      history: Boolean = false, same: Boolean = false, issueOff: Boolean = false): Unit = {
      dut.io.clean.poke(clean.B); dut.io.dump.poke(false.B)
      dut.io.history.poke(history.B); dut.io.same.poke(same.B); dut.io.issueOff.poke(issueOff.B)
      for (lane <- sizes.indices) {
        val r = inputs(lane).getOrElse(req(0x1000))
        poke(r, dut.io.in(lane).bits)
        dut.io.in(lane).valid.poke(inputs(lane).nonEmpty.B)
        dut.io.out(lane).ready.poke(ready.B)
        val pending = queues(lane)
        val enq = inputs(lane).nonEmpty
        val deq = pending.nonEmpty && ready
        val overwrite = pending.size == sizes(lane) && enq && !deq
        val pa = enq && pending.exists(x => x("Addr") >> 6 == r("Addr") >> 6)
        val eq = enq && pending.exists(x => key(x) == key(r))
        val s = if (lane == 0) dut.io.pf else dut.io.fallback
        dut.io.in(lane).ready.expect(true.B)
        dut.io.out(lane).valid.expect(pending.nonEmpty.B)
        pending.headOption.foreach { head => dut.io.out(lane).bits.elements.foreach {
          case (name, x: Bool) => x.expect((head(name) != 0).B)
          case (name, x: UInt) => x.expect(head(name).U)
          case _ => ()
        }}
        s.offered.expect(enq.B); s.inserted.expect(enq.B)
        s.paHit.expect(pa.B); s.equivalentHit.expect(eq.B)
        s.overwrite.expect(overwrite.B); s.dequeued.expect(deq.B)
        s.dedupDrop.expect(false.B); s.occupancy.expect(pending.size.U)
        val prefix = s"pfobs_Q_test$lane"
        val events = Seq("offered" -> enq, "pa_hit" -> pa, "equivalent_hit" -> eq,
          "inserted" -> enq, "overwrite" -> overwrite, "dequeued" -> deq, "dedup_drop" -> false)
        if (!warmup && !clean) {
          events.foreach { case (name, enabled) => add(prefix, name, if (enabled) 1 else 0) }
          if (lane == 1) for (src <- 0 until 16; (name, enabled) <- events) {
            val from = if (name == "dequeued") pending.headOption.exists(source(_) == src) else source(r) == src
            add(s"${prefix}_src$src", name, if (enabled && from) 1 else 0)
          }
          add(prefix, "occupancy_samples", 1); add(prefix, "occupancy_sum", pending.size)
          for (n <- 0 to sizes(lane)) add(prefix, s"occupancy_$n", if (pending.size == n) 1 else 0)
          add(prefix, "filtered_history", if (deq && history) 1 else 0)
          add(prefix, "filtered_same_as_chosen", if (deq && !history && same) 1 else 0)
          add(prefix, "drained_issue_off", if (deq && !history && !same && issueOff) 1 else 0)
        }
        val removed = if (deq || overwrite) pending.tail else pending
        queues(lane) = if (enq) removed :+ r else removed
      }
      dut.clock.step(); cycles += 1
      if (clean) { counts.clear(); warmup = false }
    }
    def both(r: Request, ready: Boolean = false): Unit = step(Seq(Some(r), Some(r)), ready)
    dut.io.clean.poke(false.B); dut.io.dump.poke(false.B)
    dut.io.history.poke(false.B); dut.io.same.poke(false.B); dut.io.issueOff.poke(false.B)
    for (lane <- sizes.indices) { dut.io.in(lane).valid.poke(false.B); poke(req(0), dut.io.in(lane).bits); dut.io.out(lane).ready.poke(false.B) }
    dut.reset.poke(true.B); dut.clock.step(3); dut.reset.poke(false.B)
    // Warmup requests survive clean as pending items; measurements do not.
    both(req(0x1000)); both(req(0x1040))
    step(Seq(None, None), ready = false, clean = true)
    both(req(0x1007, sequence = 1)) // same line/attributes, different sequence
    both(req(0x1001, source = 9))  // PA hit, different provenance
    both(req(0x1002, depth = 2))   // PA hit, different CDP depth
    both(req(0x1003, opcode = 1))  // PA hit, different opcode
    // Repeated matches count once per offered request, including full queues.
    for (n <- 0 until 12) both(req(0x1000, sequence = n))
    both(req(0x1000), ready = true) // full enqueue+dequeue, no overwrite
    for (_ <- 0 until 10) step(Seq(None, None), ready = true)
    // Empty queue has old matching register data but no valid pending match.
    both(req(0x1000), ready = true)
    step(Seq(None, None), ready = true, history = true)
    step(Seq(None, None), ready = true)
    for (n <- 0 until 24) {
      both(req(0x2000 + 64 * n))
      step(Seq(None, None), ready = true, same = n % 2 == 0, issueOff = n % 2 != 0)
    }
    // All remaining attributes must participate in conservative equivalence.
    for (field <- fields.filterNot(Set("Addr", "TxnID"))) {
      for (_ <- 0 until 8) step(Seq(None, None), ready = true)
      val base = req(0x3000); both(base); both(base.updated(field, base(field) ^ 1))
    }
    val random = new scala.util.Random(0x26dedL)
    for (n <- 0 until 1200) {
      val inputs = sizes.indices.map { lane =>
        if (random.nextInt(4) == 0) None else Some(req(0x4000 + 64 * random.nextInt(5) + random.nextInt(64),
          source = if (lane == 0) 1 else random.nextInt(16), depth = random.nextInt(4), sequence = n,
          opcode = if (random.nextBoolean()) 0x20 else 1))
      }
      step(inputs, random.nextBoolean(), history = n % 7 == 0, same = n % 11 == 0, issueOff = n % 13 == 0)
    }
    for (_ <- 0 until 9) step(Seq(None, None), ready = true)
    counts.toSeq.sortBy(_._1).foreach { case (name, value) => println(s"QOBS_EXPECT $name $value") }
    // Dump includes the current cycle in XSPerfAccumulate; hold inputs idle
    // and emit the extra empty occupancy sample in the independent oracle.
    for (lane <- sizes.indices) {
      val prefix = s"pfobs_Q_test$lane"
      println(s"QOBS_DUMP_EXTRA ${prefix}_occupancy_samples 1")
      println(s"QOBS_DUMP_EXTRA ${prefix}_occupancy_0 1")
    }
    dut.io.clean.poke(false.B); dut.io.dump.poke(true.B); dut.clock.step()
    println(s"QOBS_RESULT pass=true cycles=$cycles baseline_cycle_equivalence=true clean_with_pending=true")
  }
}
