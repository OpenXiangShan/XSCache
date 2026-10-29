package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._

// Directed source pulses at the production training arbitration boundary.
// This fixture does not substitute for the real CCHI/RBE integration checks.
class TrainAccountingHarness(implicit p: Parameters) extends Module {
  val ports = 4*32
  val io = IO(new Bundle {
    val in = Input(Vec(ports, Valid(new L2PrefetchTrain(true))))
    val ready = Input(Vec(4,Bool()))
    val total = Output(Vec(4,Vec(3,UInt(64.W))))
    val accepted = Output(Vec(4,Vec(3,UInt(64.W))))
    val dropped = Output(Vec(4,Vec(3,UInt(64.W))))
    val chosen = Output(Vec(4,Valid(UInt(1.W))))
  })
  val arb = Module(new L2PrefetchTrainArb(4,true))
  for(s <- 0 until 4) arb.io.in(s) := L2PrefetchTrainSelect.local(io.in.slice(s*32,(s+1)*32))
  arb.io.out.ready := io.ready(0)
  val events = Module(new L2CDPCacheEvents(ports))
  events.io.enable := true.B
  events.io.evict := 0.U.asTypeOf(events.io.evict)
  for(i <- 0 until ports) {
    val t = events.io.train(i)
    t.valid := io.in(i).valid
    t.bits := 0.U.asTypeOf(t.bits)
    t.bits.paddr := io.in(i).bits.paddr
    t.bits.vaddr := io.in(i).bits.vaddr.get
    t.bits.trainEnable := io.in(i).bits.trainEnable.get
    t.bits.reqSource := io.in(i).bits.reqSource.get
    t.bits.hit := true.B
  }
  val count = Module(new L2PrefetchTrainCounters(3,2*ports))
  count.io.total := 0.U.asTypeOf(count.io.total)
  count.io.accepted := 0.U.asTypeOf(count.io.accepted)
  val origins = VecInit(io.in.map(_.bits.sourceId.get >> 8))
  for(k <- 0 until 4) {
    io.chosen(k).valid := (if(k<2) arb.io.out.valid else if(k==2) events.io.vpn.valid else events.io.filter.valid) && io.ready(k)
    io.chosen(k).bits := (if(k<2) arb.io.out.bits.sourceId.get >> 8
      else origins(if(k==2) events.io.vpnChoice else events.io.filterChoice))
    for(source <- 0 until 2) {
      count.io.total(k)(source) := PopCount(io.in.zipWithIndex.map {case(t,i)=>t.valid && origins(i) === source.U})
      count.io.accepted(k)(source) := io.chosen(k).valid && io.chosen(k).bits === source.U
    }
  }
  io.total := count.io.totals
  io.accepted := count.io.accepts
  io.dropped := count.io.drops
}

object TestTop_L2TrainAccounting extends App with ChiselSim {
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  val p = new Config((_,_,_) => {
    case L2ParamsKey => L2Params(mshrSize=32,ways=8,sets=1024)
    case oceanus.chi.CHIParametersKey => oceanus.chi.CHIParameters(issue=oceanus.chi.EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case xscache.oceanus.compactchi.CCHIParametersKey => xscache.oceanus.compactchi.CCHIParameters()
  })
  simulate(new TrainAccountingHarness()(p)) { dut =>
    val total=Array.fill(4,3)(BigInt(0));val accept=Array.fill(4,3)(BigInt(0))
    def zero(): Unit = {dut.io.in.poke(0.U.asTypeOf(dut.io.in));dut.io.ready.foreach(_.poke(true.B))}
    zero();dut.reset.poke(true.B);dut.clock.step(5);dut.reset.poke(false.B)
    var cycles=0
    def tick(pulses: Seq[(Int,Int,Boolean)], ready: Int = 15): Unit = {
      zero()
      for(k <- 0 until 4) dut.io.ready(k).poke(((ready & (1<<k))!=0).B)
      pulses.foreach {case(index,source,en)=>
        val t=dut.io.in(index);t.valid.poke(true.B)
        t.bits.sourceId.get.poke(((source<<8)|(cycles&255)).U)
        t.bits.trainEnable.get.poke(en.B)
        t.bits.paddr.poke((0x10000+index*64).U)
        t.bits.vaddr.get.poke((0x80000+index).U)
      }
      for(k <- 0 until 4) {
        for(source <- 0 until 2) total(k)(source)+=pulses.count(_._2==source)
        if(dut.io.chosen(k).valid.peek().litToBoolean) {
          val source=dut.io.chosen(k).bits.peek().litValue.toInt
          assert(pulses.exists(p=>p._2==source && p._3), "disabled/nonexistent source selected")
          accept(k)(source)+=1
        }
      }
      dut.clock.step();cycles+=1
      for(k <- 0 until 4; source <- 0 until 3) {
        assert(dut.io.total(k)(source).peek().litValue==total(k)(source))
        assert(dut.io.accepted(k)(source).peek().litValue==accept(k)(source))
        assert(dut.io.dropped(k)(source).peek().litValue==total(k)(source)-accept(k)(source))
      }
    }
    tick(Seq((0,0,true),(32,1,true)))
    tick(Seq((0,0,true)));tick(Seq((32,1,true)))
    for(i <- 0 until 16) tick(Seq((if(i%2==0) 0 else 32,i%2,true)))
    tick(Seq((0,0,true),(1,1,true))) // same slice local loss
    tick(Seq((0,0,false),(32,1,false)))
    tick(Seq((0,0,false),(32,1,true)))
    tick(Seq((0,0,true),(32,1,true)),ready=0)
    val random=new scala.util.Random(0x523130)
    for(_ <- 0 until 256) {
      val pulses=random.shuffle((0 until 128).toList).take(random.nextInt(8)).map(i=>(i,i%2,random.nextBoolean()))
      tick(pulses,random.nextInt(16))
    }
    tick(Seq.empty)
    for(k <- 0 until 4;source <- 0 until 2) {
      assert(total(k)(source)>0 && accept(k)(source)>0 && total(k)(source)>accept(k)(source))
      println(s"TRAIN_R10 sink=$k SrcID=$source total=${total(k)(source)} accepted=${accept(k)(source)} dropped=${total(k)(source)-accept(k)(source)} conservation=true")
    }
    println(s"TRAIN_R10_RESULT pass=true slices=4 tshrs=32 sameCycle=true adjacent=true alternating=true trainEnableZero=true backpressure=true cycles=$cycles")
  }
}
