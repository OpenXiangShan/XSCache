// Intent (pf-v2): simultaneous 4-slice/local train competition with pre-filtered payloads.
// Intent (audit-fix-1): exercise the exact production local selection and
// global arbitration with concurrent one-cycle training pulses and all enable
// combinations; compare complete selected payloads and downstream acceptance.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import org.chipsalliance.cde.config._

class ConflictTrainHarness()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(4, Vec(2, Valid(new L2PrefetchTrain(true)))))
    val out = Decoupled(new L2PrefetchTrain(true))
    val local = Output(Vec(4, Valid(new L2PrefetchTrain(true))))
  })
  val arb = Module(new L2PrefetchTrainArb(4, true))
  for(s <- 0 until 4) {
    io.local(s) := L2PrefetchTrainSelect.local(io.in(s).toSeq)
    arb.io.in(s) := io.local(s)
  }
  io.out <> arb.io.out
}

object TestTop_L2ConflictTrain extends App with ChiselSim {
  implicit val simulator: HasSimulator = HasSimulator.default
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case xscache.oceanus.compactchi.CCHIParametersKey => xscache.oceanus.compactchi.CCHIParameters()
  })
  simulate(new ConflictTrainHarness()(params)) { dut =>
    def flag(b: Bool) = b.peek().litToBoolean
    def drive(valid: Int, enable: Int): Unit = {
      for(s <- 0 until 4; t <- 0 until 2) {
        val i=s*2+t; val in=dut.io.in(s)(t)
        in.valid.poke(((valid & (1<<i))!=0).B)
        in.bits.poke(0.U.asTypeOf(in.bits))
        in.bits.trainEnable.get.poke(((enable & (1<<i))!=0).B)
        in.bits.paddr.poke((0x1000+i*64).U)
        in.bits.vaddr.get.poke((0x123400+i).U)
        in.bits.sourceId.get.poke((0x100+i).U)
        in.bits.needT.get.poke((i%2==1).B)
        in.bits.reqSource.get.poke((10+i).U)
      }
    }
    drive(0,0); dut.io.out.ready.poke(true.B)
    dut.reset.poke(true.B); dut.clock.step(5); dut.reset.poke(false.B)
    // Exhaust all valid/enable patterns, including each possible placement of
    // the sole enabled pulse among three disabled competitors.
    for(valid <- 0 until 256; enable <- 0 until 256) {
      drive(valid,enable)
      val eligible=(0 until 8).filter(i => (valid & enable & (1<<i))!=0)
      for(s <- 0 until 4) {
        val choices=eligible.filter(_/2==s)
        assert(flag(dut.io.local(s).valid)==choices.nonEmpty)
        choices.headOption.foreach { i =>
          assert(dut.io.local(s).bits.peek().litValue==dut.io.in(s)(i%2).bits.peek().litValue)
        }
      }
      assert(flag(dut.io.out.valid)==eligible.nonEmpty)
      if(eligible.nonEmpty) {
        val selected=dut.io.out.bits.peek().litValue
        assert((0 until 4).filter(s => flag(dut.io.local(s).valid)).exists(s => dut.io.local(s).bits.peek().litValue==selected))
        assert(flag(dut.io.out.bits.trainEnable.get))
      }
      dut.clock.step()
      drive(0,0); assert(!flag(dut.io.out.valid)); dut.clock.step()
    }
    println("TRAIN_ARB_CASE same-slice-disabled-enabled pass=true")
    println("TRAIN_ARB_CASE cross-slice-disabled-enabled pass=true")
    println("TRAIN_ARB_CASE mixed-enable-exhaustive pass=true patterns=65536 slices=4 payload=true pulses=true")
  }
}
