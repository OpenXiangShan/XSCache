// D4: original DelayQueue RTL is the oracle; also check an independent timer.
package oceanus.l2
import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.{vbopreference => ref}
import org.chipsalliance.cde.config._
import utility._

class BOPDelayComparison(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val delay = Input(UInt(10.W))
    val in = Flipped(Valid(UInt(42.W)))
    val ready = Input(Bool())
    val out = Valid(UInt(48.W))
  })
  private val dut = Module(new OceanusBOPDelayQueue("d4", OceanusBOPParameters()))
  val reference = Module(new ref.DelayQueue("d4_reference"))
  val vbop = Module(new OceanusVDelayQueue("d4_vbop"))
  dut.io.delayLatency := io.delay
  dut.io.in.valid := io.in.valid; dut.io.in.bits := io.in.bits
  dut.io.out.ready := io.ready
  reference.io.pfCtrlOfDelayLatency := io.delay
  reference.io.in.valid := io.in.valid; reference.io.in.bits := io.in.bits
  reference.io.out.ready := io.ready
  vbop.io.pfCtrlOfDelayLatency := io.delay
  vbop.io.in.valid := io.in.valid; vbop.io.in.bits := io.in.bits
  vbop.io.out.ready := io.ready
  io.out.valid := dut.io.out.valid; io.out.bits := dut.io.out.bits
  when(!reset.asBool) {
    assert(dut.io.in.ready === reference.io.in.ready)
    assert(dut.io.out.valid === reference.io.out.valid)
    assert(vbop.io.out.valid === reference.io.out.valid)
    when(reference.io.out.valid) {
      assert(dut.io.out.bits === reference.io.out.bits)
      assert(vbop.io.out.bits === reference.io.out.bits)
    }
  }
}
object TestTop_OceanusBOPDelay extends App with ChiselSim {
  Constantin.init(false)
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case oceanus.chi.CHIParametersKey => oceanus.chi.CHIParameters(issue=oceanus.chi.EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case xscache.oceanus.compactchi.CCHIParametersKey => xscache.oceanus.compactchi.CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new BOPDelayComparison()(params)) { dut =>
    for(delay <- Seq(0,300,37)) {
      dut.io.in.valid.poke(false); dut.io.in.bits.poke(0); dut.io.ready.poke(true); dut.io.delay.poke(delay)
      dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false);dut.clock.step()
      dut.io.in.valid.poke(true);dut.io.in.bits.poke(0x1234)
      dut.clock.step();dut.io.in.valid.poke(false)
      val expected=if(delay==0)300 else delay
      // The loaded count decrements on subsequent clocks; first valid at +expected.
      for(cycle <- 0 until expected) {dut.io.out.valid.expect(false);dut.clock.step()}
      dut.io.out.valid.expect(true);dut.io.out.bits.expect(BigInt(0x1234)<<6)
      dut.clock.step();dut.io.out.valid.expect(false)
      for(i <- 0 until 600) {
        dut.io.delay.poke(if(i<300)delay else 19)
        dut.io.in.valid.poke(i%7==0);dut.io.in.bits.poke(i+8192)
        dut.io.ready.poke(i%11!=0);dut.clock.step()
      }
      dut.io.in.valid.poke(false);dut.io.ready.poke(true);dut.clock.step(1100)
      println(s"BOP_DELAY_RESULT delay=$delay fallback=$expected differential=600/600 pass=true")
    }
  }
}
