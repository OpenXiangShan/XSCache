package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.chi._
import org.chipsalliance.cde.config._
import scala.collection.mutable

class CDPAdapterWrapProbe(implicit p: Parameters) extends L2PrefetchTlbAdapter(8) {
  val pointer = IO(Output(UInt(3.W)));pointer := nextIssue
}
object TestTop_CDPAdapterWrap extends App with ChiselSim {
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case CHIParametersKey => CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
  })
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  var cycle=0;var wraps=0;var transactions=0;var requests=0;var timerCompletions=0
  simulate(new CDPAdapterWrapProbe()(params)) { dut =>
    def flag(b:Bool)=b.peek().litToBoolean
    def value(b:UInt)=b.peek().litValue
    val replies=mutable.Queue.empty[(Int,BigInt,Boolean)]
    val attempts=mutable.Map.empty[BigInt,Int].withDefaultValue(0)
    val missCycles=mutable.Map.empty[BigInt,Int]
    val accepted=mutable.Queue.empty[BigInt]
    var pointer=0;var issueOrdinal=0;var completed=0
    var base=BigInt(0)
    dut.io.clients.foreach { c => c.req.valid.poke(false);c.req.bits.poke(0);c.resp.ready.poke(false) }
    dut.io.tlb.req.ready.poke(false);dut.io.tlb.resp.valid.poke(false)
    dut.io.tlb.resp.bits.poke(0.U.asTypeOf(dut.io.tlb.resp.bits));dut.io.tlb.pmp_resp.poke(0.U.asTypeOf(dut.io.tlb.pmp_resp))
    dut.reset.poke(true);dut.clock.step(4);dut.reset.poke(false)
    def tick(offered:Option[BigInt]=None,external:Boolean=false,retire:Boolean=false):Unit={
      dut.io.clients(0).req.valid.poke(false)
      dut.io.clients(1).req.valid.poke(offered.nonEmpty);dut.io.clients(1).req.bits.poke(offered.getOrElse(BigInt(0)))
      dut.io.clients.foreach(_.resp.ready.poke(retire))
      dut.io.tlb.req.ready.poke(external);dut.io.tlb.resp.valid.poke(false)
      dut.io.tlb.resp.bits.poke(0.U.asTypeOf(dut.io.tlb.resp.bits));dut.io.tlb.pmp_resp.poke(0.U.asTypeOf(dut.io.tlb.pmp_resp))
      replies.headOption.filter(_._1<=cycle).foreach { case(_,va,miss)=>
        replies.dequeue();dut.io.tlb.resp.valid.poke(true)
        dut.io.tlb.resp.bits.paddr.head.poke(va+0x1000000);dut.io.tlb.resp.bits.miss.poke(miss)
        if(miss)missCycles(va)=cycle
      }
      dut.pointer.expect(pointer)
      if(offered.nonEmpty) {assert(flag(dut.io.clients(1).req.ready));accepted.enqueue(offered.get);transactions+=1}
      if(external && flag(dut.io.tlb.req.valid)) {
        val va=value(dut.io.tlb.req.bits.vaddr);val slot=issueOrdinal%8
        assert(va==base+slot*4096,s"round robin issue ordinal=$issueOrdinal cycle=$cycle")
        attempts(va)+=1
        if(attempts(va)==2) { assert(cycle-missCycles(va)>=10);timerCompletions+=1 }
        else assert(attempts(va)==1)
        pointer=(slot+1)%8;if(pointer==0)wraps+=1
        replies.enqueue((cycle+1,va,attempts(va)==1));issueOrdinal+=1;requests+=1
      }
      assert(!flag(dut.io.clients(0).resp.valid),"CDP terminal delivered to VBOP")
      if(retire && flag(dut.io.clients(1).resp.valid)) {
        val result=dut.io.clients(1).resp.bits;val va=accepted.dequeue()
        result.vaddr.expect(va);result.paddr.expect(va+0x1000000)
        result.retried.expect(true);result.miss.expect(false);result.dropped.expect(false)
        completed+=1
      }
      dut.clock.step();cycle+=1
    }
    for(round <- 0 until 50) {
      base=BigInt(0x100000)+BigInt(round)*0x10000;issueOrdinal=0
      for(i <- 0 until 8)tick(Some(base+i*4096))
      assert(!flag(dut.io.clients(1).req.ready),"full reservations did not backpressure")
      val start=completed
      while(completed-start<8) {assert(cycle<10000);tick(external=true,retire=true)}
      tick();assert(accepted.isEmpty && replies.isEmpty && pointer==0)
    }
    assert(wraps>=50 && timerCompletions>=50 && transactions==completed)
  }
  println(s"CDP_ADAPTER_WRAP_RESULT pass=true batches=50 pointerWraps=$wraps retryCountdowns=$timerCompletions transactions=$transactions externalRequests=$requests residual=0 cycles=$cycle reference=independent-host-contract")
}
