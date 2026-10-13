package xscache.coupledL2.prefetch

import chisel3._
import chisel3.simulator.ChiselSim

/** Run with mill -i XSCache.offloadTest.runMain
  * xscache.coupledL2.prefetch.L2HintOffloadTest (requires Verilator).
  */
object L2HintOffloadTest extends App with ChiselSim {
  simulate(new L2HintOffloadPolicy) { dut =>
    dut.io.total.poke(0.U)
    dut.io.cdp.poke(0.U)
    dut.io.sent.foreach(_.poke(0.U))
    dut.io.useful.foreach(_.poke(0.U))
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.io.offload.foreach(_.expect(false.B))
    dut.reset.poke(false.B)

    def check(total: BigInt, cdp: BigInt, sent: BigInt, useful: BigInt): Unit = {
      dut.io.total.poke(total.U)
      dut.io.cdp.poke(cdp.U)
      dut.io.sent(0).poke(sent.U)
      dut.io.useful(0).poke(useful.U)
      // An accurate second source must not inherit the first source's policy.
      dut.io.sent(1).poke(10.U)
      dut.io.useful(1).poke(8.U)
      dut.clock.step()
      val expected = total > 0 && sent > 0 && cdp * 5 > total && useful * 5 < sent * 4
      dut.io.offload(0).expect(expected.B)
      dut.io.offload(1).expect(false.B)
    }
    check(0, 0, 0, 0)
    check(100, 21, 0, 0)
    check(100, 20, 10, 7) // strict CDP boundary
    check(100, 21, 10, 8) // strict accuracy boundary
    check(100, 21, 10, 7)
    check(100, 21, 10, 11) // delayed useful observations may exceed sent
    val max = (BigInt(1) << 64) - 1
    check(max, max / 5 + 1, max, max * 4 / 5)
    check(max, max / 5, max, 0)
    val rng = new scala.util.Random(20261012)
    for (_ <- 0 until 256) {
      val total = BigInt(64, rng)
      check(total, BigInt(64, rng) % (total + 1), BigInt(64, rng), BigInt(64, rng))
    }
    // Reset clears a previously asserted registered decision.
    check(100, 21, 10, 0)
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.io.offload.foreach(_.expect(false.B))
  }

  simulate(new L2HintOffloadQueue(UInt(32.W), entries = 4)) { dut =>
    dut.io.native.valid.poke(false.B)
    dut.io.native.bits.poke(0.U)
    dut.io.offload.valid.poke(false.B)
    dut.io.offload.bits.poke(0.U)
    dut.io.out.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step()
    dut.reset.poke(false.B)

    // Fill the offload queue under downstream backpressure.
    for (i <- 1 to 4) {
      dut.io.offload.valid.poke(true.B)
      dut.io.offload.bits.poke(i.U)
      dut.io.offload.ready.expect(true.B)
      dut.clock.step()
    }
    dut.io.offload.bits.poke(999.U)
    dut.io.offload.ready.expect(false.B)
    dut.clock.step(8) // A blocked request must not overwrite accepted entries.
    dut.io.offload.valid.poke(false.B)
    dut.io.native.valid.poke(true.B)
    dut.io.native.bits.poke(1000.U)
    dut.io.out.ready.poke(true.B)
    var offloads = Vector.empty[BigInt]
    var natives = 0
    for (_ <- 0 until 8) {
      dut.io.out.valid.expect(true.B)
      val data = dut.io.out.bits.peek().litValue
      if (data == 1000) natives += 1 else offloads :+= data
      dut.clock.step()
    }
    assert(offloads == Vector(1, 2, 3, 4).map(BigInt(_)))
    assert(natives == 4, "Both continuously eligible sources must make progress")

    // Random backpressure and concurrent arrivals; check every accepted word
    // exactly once and in order within each source, then drain completely.
    dut.io.native.valid.poke(false.B)
    val expected = Array.fill(2)(scala.collection.mutable.Queue.empty[BigInt])
    val rng = new scala.util.Random(42)
    var serial = 2000
    for (cycle <- 0 until 600) {
      val nativeValid = cycle < 500 && rng.nextBoolean()
      val offloadValid = cycle < 500 && rng.nextBoolean()
      val ready = cycle >= 500 || rng.nextBoolean()
      dut.io.native.valid.poke(nativeValid.B)
      dut.io.offload.valid.poke(offloadValid.B)
      dut.io.native.bits.poke((serial * 2).U)
      dut.io.offload.bits.poke((serial * 2 + 1).U)
      dut.io.out.ready.poke(ready.B)
      if (nativeValid && dut.io.native.ready.peek().litToBoolean) expected(0).enqueue(serial * 2)
      if (offloadValid && dut.io.offload.ready.peek().litToBoolean) expected(1).enqueue(serial * 2 + 1)
      if (ready && dut.io.out.valid.peek().litToBoolean) {
        val data = dut.io.out.bits.peek().litValue
        val src = (data % 2).toInt
        assert(expected(src).nonEmpty && expected(src).dequeue() == data)
      }
      dut.clock.step()
      serial += 1
    }
    assert(expected.forall(_.isEmpty))
    dut.io.out.valid.expect(false.B)
  }
  println("L2HintOffloadTest: policy boundaries, full-width arithmetic, reset, queue retention and fairness passed")
}

/** Elaborate the actual four-bank L2 including receiver, CDP, statistics,
  * offload routing, stash entries and CHI wiring (not only the policy unit).
  */
object L2HintOffloadElaborate extends App {
  import xscache.coupledL2._
  TestTopCHIHelper.gen(
    p => new TestTop_CHIL2(numCores = 1, banks = 4)(p.alterPartial {
      case L2ParamKey => p(L2ParamKey).copy(
        enableL2HintOffload = true,
        prefetch = Seq(BOPParameters(), CDPParameters(), PrefetchReceiverParams())
      )
    }),
    "B", false, false, false, false
  )(args)
}
