package oceanus
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim,HasSimulator}
import utility._
import oceanus.l2._
import scala.collection.mutable
import scala.util.Random

class CDPC3QueueHarness(inputs:Int) extends Module {
  val io=IO(new Bundle {
    val valid=Input(UInt(inputs.W));val data=Input(Vec(inputs,UInt(16.W)));val ready=Input(Bool())
    val accepted=Output(UInt(inputs.W));val out=Output(UInt(17.W));val count=Output(UInt(4.W))
    val rawAccepted=Output(UInt(inputs.W));val rawOut=Output(UInt(17.W));val rawCount=Output(UInt(4.W))
  })
  val prod=Module(new OceanusCDPQueue(UInt(16.W),inputs))
  val adapter=Module(new CDPC3ReferenceQueue(UInt(16.W),inputs))
  val raw=Module(new MIMOQueue(UInt(16.W),8,inputs,1))
  adapter.io.flush:=reset.asBool;raw.io.flush:=reset.asBool
  for(i<-0 until inputs) {
    prod.io.enq(i).valid:=io.valid(i);prod.io.enq(i).bits:=io.data(i)
    adapter.io.enq(i).valid:=io.valid(i);adapter.io.enq(i).bits:=io.data(i)
    raw.io.enq(i).valid:=io.valid(i);raw.io.enq(i).bits:=io.data(i)
    when(!reset.asBool){assert(prod.io.enq(i).ready===adapter.io.enq(i).ready)}
  }
  prod.io.deq.ready:=io.ready;adapter.io.deq(0).ready:=io.ready;raw.io.deq(0).ready:=io.ready
  io.accepted:=VecInit(prod.io.enq.map(_.fire)).asUInt;io.rawAccepted:=VecInit(raw.io.enq.map(_.fire)).asUInt
  io.out:=Cat(prod.io.deq.valid,Mux(prod.io.deq.valid,prod.io.deq.bits,0.U))
  io.rawOut:=Cat(raw.io.deq(0).valid,Mux(raw.io.deq(0).valid,raw.io.deq(0).bits,0.U))
  io.count:=prod.io.count
  def read[T<:Data](d:T):T=BoringUtils.bore(d)
  io.rawCount:=read(raw.enq_ptr)-read(raw.deq_ptr)
  when(!reset.asBool) {
    assert(prod.io.deq.valid===adapter.io.deq(0).valid)
    when(prod.io.deq.valid){assert(prod.io.deq.bits===adapter.io.deq(0).bits)}
  }
}
object TestTop_OceanusCDPC3Queue extends App with ChiselSim {
  implicit val simulator:HasSimulator=HasSimulator.default
  var checks=0L
  for(inputs<-Seq(2,3))simulate(new CDPC3QueueHarness(inputs)){dut=>
    val fifo=mutable.Queue.empty[Int];var rawFires=0;var rawPops=0;var data=1
    def tick(valid:Int,ready:Boolean):Unit={
      dut.io.valid.poke(valid);dut.io.ready.poke(ready)
      for(i<-0 until inputs)dut.io.data(i).poke(data+i)
      val expected=fifo.headOption.map(x=>BigInt(65536+x)).getOrElse(BigInt(0))
      assert(dut.io.out.peek().litValue==expected);assert(dut.io.count.peek().litValue==fifo.size)
      val free=8-fifo.size
      val add=(0 until inputs).filter(i=>((valid>>i)&1)!=0).take(free)
      val mask=add.map(1<<_).sum
      assert(dut.io.accepted.peek().litValue==mask)
      if(ready && fifo.nonEmpty)fifo.dequeue()
      add.foreach(i=>fifo.enqueue(data+i))
      rawFires+=dut.io.rawAccepted.peek().litValue.bitCount
      if(ready && dut.io.rawOut.peek().litValue.testBit(16))rawPops+=1
      data+=inputs;dut.clock.step();checks+=1
    }
    dut.io.valid.poke(0);dut.io.ready.poke(false);dut.io.data.foreach(_.poke(0))
    dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false)
    if(inputs==2) {
      for(_<-0 until 3)tick(3,false) // six
      tick(0,true) // five
      tick(3,false) // seven
      tick(3,false) // one accepted, raw total pointer unchanged
      dut.io.count.expect(8);dut.io.rawCount.expect(7)
    } else {
      tick(7,false);tick(7,false) // six
      tick(7,false) // two accepted, raw pointer unchanged
      dut.io.count.expect(8);dut.io.rawCount.expect(6)
    }
    for(_<-0 until 10)tick(0,true)
    val lost=rawFires-rawPops
    require(lost==(if(inputs==2)1 else 2))
    println(s"CDP_C3_RAW_DEPENDENCY inputs=$inputs accepted=$rawFires drained=$rawPops lost=$lost reproduced=true")
    // Explicit same-clock dual arrival and adjacent-clock low/high arrival.
    tick(3,false);tick(0,true);tick(0,true)
    tick(1,false);tick(2,false);tick(0,true);tick(0,true)
    println(s"CDP_C3_HALF_ARRIVAL inputs=$inputs simultaneous=1 adjacent=1 payload_checked=true")
    // Independent per-port timing, incl. low-only then high-only arrivals,
    // wraparound, simultaneous read/write and full-cycle drop.
    val random=new Random(0xc300+inputs)
    for(_<-0 until 4000)tick(random.nextInt(1<<inputs),random.nextBoolean())
    for(_<-0 until 12)tick(0,true)
    assert(fifo.isEmpty)
  }
  println(s"CDP_C3_QUEUE_RESULT pass=true checks=$checks raw_dependency_counterexamples=2")
}
