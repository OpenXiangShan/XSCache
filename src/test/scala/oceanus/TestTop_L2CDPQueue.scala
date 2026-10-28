package oceanus

// Intent (1c-2): stress the actual eight-entry dual-enqueue queues with a
// host FIFO oracle, including one-slot acceptance, simultaneous dequeue/full
// enqueue, pointer wrap and bounded RR service for every backlogged slice.
// Check all literal eight-bit TxnID encodings, independently of the decoder.

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.chi._
import org.chipsalliance.cde.config._
import scala.collection.mutable
import scala.util.Random

class L2CDPQueueHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(4, Valid(new L2CDPLine)))
    val out = Decoupled(new L2CDPHalf)
    val chosen = Output(UInt(2.W))
    val drops = Output(Vec(4, UInt(64.W)))
    val occupancy = Output(Vec(4, UInt(4.W)))
    val heads = Output(Vec(4, UInt(3.W)))
    val tails = Output(Vec(4, UInt(3.W)))
    val source = Input(UInt(4.W))
    val depth = Input(UInt(2.W))
    val sequence = Input(UInt(2.W))
    val encoded = Output(UInt(8.W))
    val decodedSource = Output(UInt(4.W))
    val decodedDepth = Output(UInt(2.W))
    val legacy = Output(UInt(8.W))
  })
  val arb = Module(new RRArbiter(new L2CDPHalf, 4))
  for (bank <- 0 until 4) {
    val queue = Module(new L2CDPHalfQueue)
    queue.io.in := io.in(bank)
    arb.io.in(bank) <> queue.io.out
    io.drops(bank) := queue.io.dropped
    io.occupancy(bank) := queue.io.occupancy
    io.heads(bank) := chisel3.util.experimental.BoringUtils.bore(queue.head)
    io.tails(bank) := chisel3.util.experimental.BoringUtils.bore(queue.tail)
  }
  io.out <> arb.io.out
  io.chosen := arb.io.chosen
  io.encoded := L2PrefetchTxnID.encode(io.source, io.depth, io.sequence, 8)
  io.decodedSource := L2PrefetchTxnID.decode(io.encoded)
  io.decodedDepth := L2PrefetchTxnID.depth(io.encoded)
  io.legacy := L2PrefetchTxnID.encode(io.source, io.sequence, 8)
}

object TestTop_L2CDPQueue extends App with ChiselSim {
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params()
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
  })
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  simulate(new L2CDPQueueHarness()(params)) { dut =>
    case class Half(addr: BigInt, data: BigInt, source: Int, depth: Int, hit: Boolean, high: Boolean)
    val fifos = Seq.fill(4)(mutable.Queue.empty[Half])
    val drops = Array.fill[BigInt](4)(0)
    val served = Array.fill(4)(0)
    val enqueued = Array.fill(4)(0)
    val waits = Array.fill(4)(0)
    val random = new Random(6882)
    var partial = 0
    var simultaneous = 0
    var cycle = 0
    def flag(signal: Bool): Boolean = signal.peek().litToBoolean
    def value(signal: UInt): BigInt = signal.peek().litValue
    dut.io.in.foreach(_.valid.poke(false.B))
    dut.io.out.ready.poke(false.B)
    dut.io.source.poke(0.U); dut.io.depth.poke(0.U); dut.io.sequence.poke(0.U)
    dut.reset.poke(true.B); dut.clock.step(4); dut.reset.poke(false.B)
    for (source <- 0 until 16; depth <- 0 until 4; sequence <- 0 until 4) {
      dut.io.source.poke(source.U); dut.io.depth.poke(depth.U); dut.io.sequence.poke(sequence.U)
      assert(value(dut.io.encoded) == ((source << 4) | (depth << 2) | sequence))
      assert(value(dut.io.decodedSource) == source && value(dut.io.decodedDepth) == depth)
      assert(value(dut.io.legacy) == ((source << 4) | sequence))
    }
    def tick(incoming: Set[Int], ready: Boolean): Unit = {
      dut.io.out.ready.poke(ready.B)
      val payloads = (0 until 4).map { bank =>
        val addr = BigInt((cycle * 4 + bank + 1) * 64)
        val source = (cycle + bank) % 16
        val depth = (cycle + bank) % 4
        val hit = (cycle & 1) != 0
        val halves = (0 until 2).map { half =>
          Half(addr, (BigInt(cycle + 1) << (128 + half)) | (bank * 2 + half + 1), source, depth, hit, half == 1)
        }
        val port = dut.io.in(bank)
        port.valid.poke(incoming(bank).B)
        port.bits.paddr.poke(addr.U); port.bits.pfSource.poke(source.U); port.bits.pfDepth.poke(depth.U)
        port.bits.isHit.poke(hit.B); port.bits.eventType.poke((if (hit) 1 else 0).U)
        port.bits.data.poke((halves(0).data | (halves(1).data << 256)).U)
        halves
      }
      val backlogged = fifos.indices.filter(fifos(_).nonEmpty)
      assert(flag(dut.io.out.valid) == backlogged.nonEmpty)
      for (bank <- 0 until 4) {
        assert(value(dut.io.drops(bank)) == drops(bank))
        assert(value(dut.io.occupancy(bank)) == fifos(bank).size)
        assert(value(dut.io.heads(bank)) == served(bank)%8 && value(dut.io.tails(bank)) == enqueued(bank)%8, "half queue modulo pointer reference")
      }
      if (ready && backlogged.nonEmpty) {
        val bank = value(dut.io.chosen).toInt
        val expected = fifos(bank).dequeue()
        val actual = dut.io.out.bits
        assert(Half(value(actual.paddr), value(actual.data), value(actual.pfSource).toInt,
          value(actual.pfDepth).toInt, flag(actual.isHit), flag(actual.high)) == expected)
        assert(value(actual.eventType) == (if (expected.hit) 1 else 0))
        served(bank) += 1
        for (other <- 0 until 4) {
          waits(other) = if (other == bank || !backlogged.contains(other)) 0 else waits(other) + 1
          assert(waits(other) < 4, s"RR starved slice=$other")
        }
      }
      if (incoming.size > 1) simultaneous += 1
      incoming.foreach { bank =>
        val accepted = math.min(2, 8 - fifos(bank).size)
        if (accepted == 1) partial += 1
        fifos(bank).enqueueAll(payloads(bank).take(accepted))
        enqueued(bank) += accepted
        drops(bank) += 2 - accepted
      }
      dut.clock.step(); cycle += 1
    }
    for (_ <- 0 until 6) tick(Set(0, 1, 2, 3), false)
    for (_ <- 0 until 48) tick(Set(0, 1, 2, 3), true)
    for (_ <- 0 until 4000) tick((0 until 4).filter(_ => random.nextBoolean()).toSet, random.nextInt(4) != 0)
    while (fifos.exists(_.nonEmpty)) tick(Set.empty, true)
    tick(Set.empty, true)
    assert(partial > 0 && simultaneous > 0 && drops.forall(_ > 0) && served.forall(_ > 100))
    for(bank <- 0 until 4) {
      assert(served(bank)/8>=50 && enqueued(bank)/8>=50)
      println(s"CDP_HALF_QUEUE_WRAP pass=true bank=$bank headWraps=${served(bank)/8} tailWraps=${enqueued(bank)/8} residual=${fifos(bank).size}")
    }
    println(s"CDP_QUEUE_RESULT pass=true cycles=$cycle partial=$partial simultaneous=$simultaneous drops=${drops.mkString(",")} served=${served.mkString(",")} rrBound=4")
    println("CDP_TXN_RESULT pass=true encodings=256 sources=16 depths=4 sequences=4 legacyDepthZero=true")
  }
}
