package oceanus

// Accelerated boundary-only tests for 64-bit observation counters. Explicit
// test-only preload of state; real production adders and event qualification
// then execute unchanged. These are not claimed as natural 2^64-cycle wraps.
import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.chi._
import org.chipsalliance.cde.config._
import utility.MemReqSource

class CDPHalfObservationWrap(implicit p: Parameters) extends L2CDPHalfQueue {
  val preload = IO(Flipped(Valid(UInt(64.W))))
  when(preload.valid) { dropped := preload.bits }
}
class CDPEventsObservationWrap(implicit p: Parameters) extends L2CDPCacheEvents(2) {
  val preload = IO(Flipped(Valid(UInt(64.W))))
  val counters = IO(Output(Vec(3, UInt(64.W))))
  when(preload.valid) { vpnCollisions := preload.bits; filterCollisions := preload.bits; evictions := preload.bits }
  counters := VecInit(vpnCollisions, filterCollisions, evictions)
}
object TestTop_CDPObservationWrap extends App with ChiselSim {
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case CHIParametersKey => CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
  })
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  val modulus = BigInt(1) << 64
  var cycles = 0
  simulate(new CDPHalfObservationWrap()(params)) { dut =>
    dut.io.in.valid.poke(false); dut.io.in.bits.poke(0.U.asTypeOf(dut.io.in.bits));dut.io.out.ready.poke(false)
    dut.preload.valid.poke(false);dut.preload.bits.poke(0)
    dut.reset.poke(true);dut.clock.step(4);dut.reset.poke(false)
    dut.io.in.valid.poke(true);dut.clock.step(4);cycles+=4
    dut.io.occupancy.expect(8)
    for(round <- 0 until 50) {
      dut.io.in.valid.poke(false);dut.preload.valid.poke(true);dut.preload.bits.poke(modulus-5)
      dut.clock.step();cycles+=1;dut.preload.valid.poke(false)
      var expected=modulus-5;var wraps=0
      for(step <- 0 until 8) {
        dut.io.dropped.expect(expected);dut.io.occupancy.expect(8)
        val enabled=step%3!=0;dut.io.in.valid.poke(enabled)
        val sum=expected+(if(enabled)2 else 0);if(sum>=modulus)wraps+=1
        expected=sum%modulus;dut.clock.step();cycles+=1
      }
      dut.io.dropped.expect(expected);assert(wraps==1)
    }
    println(s"CDP_OBSERVATION_WRAP pass=true counter=halfDropped width=64 wraps=50 testPreload=true cycles=$cycles")
  }
  simulate(new CDPEventsObservationWrap()(params)) { dut =>
    dut.io.enable.poke(false);dut.io.train.poke(0.U.asTypeOf(dut.io.train));dut.io.evict.poke(0.U.asTypeOf(dut.io.evict))
    dut.preload.valid.poke(false);dut.preload.bits.poke(0)
    dut.reset.poke(true);dut.clock.step(4);dut.reset.poke(false)
    for(i <- 0 until 2) {
      dut.io.train(i).valid.poke(true);dut.io.train(i).bits.trainEnable.poke(true)
      dut.io.train(i).bits.reqSource.poke(MemReqSource.CPULoadData.id);dut.io.train(i).bits.hit.poke(true)
      dut.io.evict(i).valid.poke(true);dut.io.evict(i).bits.pfSource.poke(L2PfSource.CDP)
    }
    for(round <- 0 until 50) {
      dut.io.enable.poke(false);dut.preload.valid.poke(true);dut.preload.bits.poke(modulus-5)
      dut.clock.step();cycles+=1;dut.preload.valid.poke(false)
      val expected=Array.fill[BigInt](3)(modulus-5);val wraps=Array.fill(3)(0)
      for(step <- 0 until 12) {
        for(i <- 0 until 3)dut.counters(i).expect(expected(i))
        val enabled=step%3!=0;dut.io.enable.poke(enabled)
        val increment=Seq(if(enabled)1 else 0,if(enabled)3 else 0,2)
        for(i <- 0 until 3) {
          val sum=expected(i)+increment(i);if(sum>=modulus)wraps(i)+=1;expected(i)=sum%modulus
        }
        dut.clock.step();cycles+=1
      }
      for(i <- 0 until 3){dut.counters(i).expect(expected(i));assert(wraps(i)==1)}
    }
    for(name <- Seq("vpnCollisions","filterCollisions","evictions"))
      println(s"CDP_OBSERVATION_WRAP pass=true counter=$name width=64 wraps=50 testPreload=true cycles=$cycles")
  }
  println(s"CDP_OBSERVATION_WRAP_RESULT pass=true counters=4 wrapsEach=50 testPreload=true cycles=$cycles")
}
