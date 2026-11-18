// Intent (Joseph 2026-11-18): exercise the same 32-bit timestamp
// arithmetic used by F3, including former coarse-clock overflow and wrap.
package oceanus

import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2.L2PrefetchObservation

class L2ObservationTimeTop extends Module {
  val io = IO(new Bundle {
    val begin = Input(UInt(32.W))
    val end = Input(UInt(32.W))
    val elapsed = Output(UInt(32.W))
    val ordered = Output(Bool())
    val ageOverflow = Output(Bool())
    val leadOverflow = Output(Bool())
    val leadNegative = Output(Bool())
  })
  io.elapsed := L2PrefetchObservation.delta(io.end, io.begin)
  io.ordered := L2PrefetchObservation.beforeOrEqual(io.begin, io.end)
  io.ageOverflow := io.elapsed >= 262144.U
  io.leadOverflow := io.elapsed.asSInt >= 32768.S
  io.leadNegative := io.elapsed.asSInt < 0.S
}
object TestTop_L2ObservationTime extends App with ChiselSim {
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  simulate(new L2ObservationTimeTop) { dut =>
    val mask = (BigInt(1) << 32) - 1
    val cases = Seq((BigInt(0), BigInt(524288)),
      (BigInt(0), BigInt(1100000)), (BigInt(37), BigInt(2000000)),
      (mask - 100, BigInt(23)), (BigInt(600000), BigInt(599900)),
      (BigInt(12345), BigInt(12345)))
    for ((begin, end) <- cases) {
      val elapsed = (end - begin) & mask
      val signed = if (elapsed.testBit(31)) elapsed - (BigInt(1) << 32) else elapsed
      dut.io.begin.poke(begin.U); dut.io.end.poke(end.U)
      dut.io.elapsed.expect(elapsed.U)
      dut.io.ordered.expect((signed >= 0).B)
      dut.io.ageOverflow.expect((elapsed >= 262144).B)
      dut.io.leadOverflow.expect((signed >= 32768).B)
      dut.io.leadNegative.expect((signed < 0).B)
    }
    println(s"PFOBS_TIME32_RESULT pass=true vectors=${cases.size}")
  }
}
