package oceanus

import oceanus.compactchi.CCHIOpcode

/** A single allowed Type-4 read, with a BOP-off control for RTL triage.
  * Preserve RTL assertions: a failing response must remain a failing test.
  */
object TestType4ReadOnce extends App {
  val bop = !args.contains("--bop-off")
  val request = UpRequestSpec(MasterId.t4p0, 0, 4, 0,
    CCHIOpcode.ReadOnce.opcode, BigInt("20100", 16), expCompData = false)
  L2TopUpstreamLegality.check(request)
  println(s"TYPE4_READONCE_REQUEST src=4 opcode=0x03 addr=0x20100 ExpCompData=0 bop=$bop")
  L2TopConcurrentSimulation.run(L2TopConcurrentPlan("type4-single-readonce", 1,
    Seq(0 -> Seq(request)), bopEnable = bop)) { trace =>
    assert(trace.completed == 1 && trace.checkedBeats == 2)
    assert(trace.trains.isEmpty && trace.received.isEmpty)
  }
}
