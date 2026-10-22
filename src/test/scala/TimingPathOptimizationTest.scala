package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import chisel3.simulator.ChiselSim
import utility.{FastArbiter, FastArbiterBase, LatchFastArbiter, LowerMaskPrefix, MaskToOH, SECDEDCode}

class TimingPathOptimizationHarness extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(50.W))
    val offset = Input(SInt(10.W))
    val raw = Input(UInt(137.W))
    val data = Input(UInt(128.W))
    val flipMask = Input(UInt(137.W))
    val encoded = Input(Bool())
    val equal = Output(Bool())
    val errors = Output(Vec(4, Bool()))
  })

  val addressChecks = Seq((16, 6), (48, 6), (50, 6), (49, 5)).map { case (width, offBits) =>
    val addr = io.addr(width - 1, 0)
    val offsetBytes = (io.offset.pad(width).asUInt << offBits)(width - 1, 0)
    BOPAddressAdd(addr, io.offset, offBits) === (addr + offsetBytes)(width - 1, 0)
  }
  val eccChecks = Seq(1, 8, 64, 128).zipWithIndex.map { case (bits, i) =>
    val code = new SECDEDCode
    val width = code.width(bits)
    val word = Mux(io.encoded,
      code.encode(io.data(bits - 1, 0)) ^ io.flipMask(width - 1, 0),
      io.raw(width - 1, 0))
    val decoded = code.decode(word)
    val legacyError = decoded.correctable || decoded.uncorrectable
    io.errors(i) := decoded.error
    decoded.error === legacyError
  }
  io.equal := (addressChecks ++ eccChecks).reduce(_ && _)
}

/** Combinational equivalence plus errors in every data/check-bit position. */
object TestTimingPathOptimizations extends App with ChiselSim {
  simulate(new TimingPathOptimizationHarness) { dut =>
    val random = new scala.util.Random(1000)
    val addrMask = (BigInt(1) << 50) - 1
    dut.io.addr.poke(0.U)
    dut.io.offset.poke(0.S)
    dut.io.data.poke(0.U)
    dut.io.flipMask.poke(0.U)
    dut.io.encoded.poke(false.B)
    dut.io.raw.poke(0.U)

    // Exercise carry/borrow through every address bit, signed extrema, page
    // boundaries, address wrap, and nonzero byte offsets.
    val addresses = (Seq(BigInt(0), addrMask) ++ (0 until 50).flatMap { bit =>
      val boundary = BigInt(1) << bit
      Seq(boundary, boundary - 1, (addrMask ^ (boundary - 1)))
    }).distinct
    for (addr <- addresses; offset <- Seq(-512, -511, -256, -1, 0, 1, 255, 510, 511)) {
      dut.io.addr.poke(addr.U)
      dut.io.offset.poke(offset.S)
      dut.io.equal.expect(true.B)
    }

    // Includes exhaustive raw codewords for the shortened 8-data-bit SECDED.
    for (word <- 0 until 8192) {
      dut.io.raw.poke(word.U)
      dut.io.equal.expect(true.B)
    }
    for (_ <- 0 until 2000) {
      dut.io.addr.poke(BigInt(50, random).U)
      dut.io.offset.poke((random.nextInt(1024) - 512).S)
      dut.io.raw.poke(BigInt(137, random).U)
      dut.io.equal.expect(true.B)
    }

    dut.io.encoded.poke(true.B)
    for (data <- Seq(BigInt(0), (BigInt(1) << 128) - 1, BigInt(128, random))) {
      dut.io.data.poke(data.U)
      dut.io.flipMask.poke(0.U)
      dut.io.equal.expect(true.B)
      (0 until 4).foreach(i => dut.io.errors(i).expect(false.B))
      for (i <- 0 until 137) {
        dut.io.flipMask.poke((BigInt(1) << i).U)
        dut.io.equal.expect(true.B)
        dut.io.errors(3).expect(true.B)
      }
      for (i <- 0 until 137; j <- 0 until i) {
        dut.io.flipMask.poke(((BigInt(1) << i) | (BigInt(1) << j)).U)
        dut.io.equal.expect(true.B)
        dut.io.errors(3).expect(true.B)
      }
    }
    println("PASS: BOP address equivalence (4 configurations), SECDED legacy equivalence, 137 single-bit and 9316 double-bit injections on 3 payloads")
  }
}

/** Frozen pre-optimization arbitration, including pending-request withdrawal
  * and the latch variant's capture/accept boundary.
  */
class LegacyTimingArbiter(n: Int, latch: Boolean) extends FastArbiterBase(UInt(16.W), n) {
  def first(mask: UInt): UInt = VecInit((0 until n).map { i =>
    mask(i) && (if (i == 0) true.B else !mask(i - 1, 0).orR)
  }).asUInt
  val chosenOH = Wire(UInt(n.W))
  val valids = VecInit(io.in.map(_.valid)).asUInt
  val outValid = RegInit(false.B)
  val outBits = RegInit(0.U(16.W))
  val chosenReg = RegInit(0.U(n.W))
  val update = if (latch) valids.orR && !outValid else io.out.fire
  val pending = RegEnable(valids & ~chosenOH, 0.U(n.W), update)
  val grantMask = RegEnable(VecInit((0 until n).map { i =>
    if (i == 0) false.B else chosenOH(i - 1, 0).orR
  }).asUInt, 0.U(n.W), update)
  val rr = first(grantMask & pending)
  chosenOH := Mux((rr & valids).orR, rr, first(valids))
  if (latch) {
    when(update) {
      outValid := true.B
      outBits := Mux1H(chosenOH, io.in.map(_.bits))
      chosenReg := chosenOH
    }
    when(io.out.fire) { outValid := false.B }
    io.out.valid := outValid && valids(OHToUInt(chosenReg))
    io.out.bits := outBits
    io.chosenOH := chosenReg
    io.chosen := OHToUInt(chosenReg)
    io.in.zipWithIndex.foreach { case (in, i) => in.ready := chosenReg(i) && outValid && io.out.ready }
  } else {
    io.out.valid := valids.orR
    io.out.bits := Mux1H(chosenOH, io.in.map(_.bits))
    io.chosenOH := chosenOH
    io.chosen := OHToUInt(chosenOH)
    io.in.zipWithIndex.foreach { case (in, i) => in.ready := chosenOH(i) && io.out.ready }
  }
}

class TimingArbiterEquivalenceHarness extends Module {
  val io = IO(new Bundle {
    val mask = Input(UInt(80.W))
    val ready = Input(Bool())
    val data = Input(UInt(16.W))
    val equal = Output(Bool())
  })
  val checks = Seq(1, 3, 8, 24, 40, 48, 80).flatMap { n =>
    val mask = io.mask(n - 1, 0)
    val oldPrefix = VecInit((0 until n).map { i =>
      if (i == 0) false.B else mask(i - 1, 0).orR
    }).asUInt
    val prefixCheck = LowerMaskPrefix(mask) === oldPrefix
    val oneHotCheck = MaskToOH(mask) === (mask & ~oldPrefix)
    val sequential = Seq(false, true).map { latch =>
      val dut = if (latch) Module(new LatchFastArbiter(UInt(16.W), n)) else Module(new FastArbiter(UInt(16.W), n))
      val ref = Module(new LegacyTimingArbiter(n, latch))
      Seq(dut, ref).foreach { arb =>
        arb.io.out.ready := io.ready
        arb.io.in.zipWithIndex.foreach { case (in, i) =>
          in.valid := mask(i)
          in.bits := io.data ^ i.U
        }
      }
      dut.io.out.valid === ref.io.out.valid && dut.io.out.bits === ref.io.out.bits &&
        dut.io.chosenOH === ref.io.chosenOH && dut.io.chosen === ref.io.chosen &&
        VecInit(dut.io.in.map(_.ready)).asUInt === VecInit(ref.io.in.map(_.ready)).asUInt
    }
    Seq(prefixCheck, oneHotCheck) ++ sequential
  }
  io.equal := checks.reduce(_ && _)
}

object TestTimingArbiterEquivalence extends App with ChiselSim {
  simulate(new TimingArbiterEquivalenceHarness) { dut =>
    val random = new scala.util.Random(1000)
    val all = (BigInt(1) << 80) - 1
    def step(mask: BigInt, ready: Boolean, data: Int): Unit = {
      dut.io.mask.poke(mask.U)
      dut.io.ready.poke(ready.B)
      dut.io.data.poke((data & 65535).U)
      dut.io.equal.expect(true.B)
      dut.clock.step()
      dut.io.equal.expect(true.B)
    }
    // Exhaust every 8-bit request set through backpressure and withdrawals.
    for (mask <- 0 until 256; ready <- Seq(false, true)) step(BigInt(mask), ready, mask)
    // Sweep grant positions and wrap at all seven parameter widths.
    for (i <- 0 until 80) {
      step(all, true, i)
      step(BigInt(1) << i, false, i + 1)
      step(0, true, i + 2)
      step(all ^ (BigInt(1) << i), true, i + 3)
    }
    for (cycle <- 0 until 5000) {
      val mask = cycle % 8 match {
        case 0 => all
        case 1 => BigInt(0)
        case 2 => BigInt(1) << random.nextInt(80)
        case _ => BigInt(80, random)
      }
      step(mask, random.nextBoolean(), random.nextInt(65536))
    }
    println("PASS: legacy FastArbiter/LatchFastArbiter cycle equivalence at 1/3/8/24/40/48/80 inputs, prefix/one-hot equivalence, stalls and withdrawals")
  }
}
