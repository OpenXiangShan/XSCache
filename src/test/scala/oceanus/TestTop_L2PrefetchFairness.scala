// Intent (audit-fix-1): fill the default eight slots before enabling service,
// continually refill retired low slots, and check each initial slot as well as
// every later request. Repeat with mixed owners and first-miss retries.
package oceanus

import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import org.chipsalliance.cde.config._
import scala.collection.mutable

class FairTlbHarness()(implicit p: Parameters) extends L2PrefetchTlbAdapter {
  val issuedSlot = IO(Output(UInt(indexBits.W)))
  issuedSlot := issue
}
object TestTop_L2PrefetchFairness extends App with ChiselSim {
  val params = new Config((_,_,_) => { case L2ParamsKey => L2Params() })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new FairTlbHarness()(params)) { dut =>
    def flag(b: Bool)=b.peek().litToBoolean
    def value(b: UInt)=b.peek().litValue
    for(mixed <- Seq(false,true); retry <- Seq(false,true)) {
      dut.io.clients.foreach { c => c.req.valid.poke(false.B); c.req.bits.poke(0.U); c.resp.ready.poke(true.B) }
      dut.io.tlb.req.ready.poke(false.B); dut.io.tlb.resp.valid.poke(false.B)
      dut.io.tlb.resp.bits.poke(0.U.asTypeOf(dut.io.tlb.resp.bits))
      dut.io.tlb.pmp_resp.poke(0.U.asTypeOf(dut.io.tlb.pmp_resp))
      dut.reset.poke(true.B); dut.clock.step(5); dut.reset.poke(false.B)
      case class Req(client: Int, born: Int, initial: Boolean)
      val live=mutable.Map.empty[BigInt,Req]
      val issued=mutable.Map.empty[BigInt,Int].withDefaultValue(0)
      val last=mutable.Map.empty[BigInt,Int]
      val replies=mutable.Queue.empty[(BigInt,Int,Int)]
      val slots=mutable.Set.empty[Int]
      val completed=mutable.Set.empty[BigInt]
      var next=0; var cycle=0; var initialRetired=0
      var held: Option[(Int,BigInt)]=None
      def tick(feed: Boolean, ready: Boolean): Unit = {
        if(feed && held.isEmpty) { held=Some((if(mixed && next<8) next%2 else 0,BigInt(0x10000+next*64))); next+=1 }
        dut.io.clients.zipWithIndex.foreach { case(c,i) =>
          c.req.valid.poke(held.exists(_._1==i).B)
          c.req.bits.poke(held.filter(_._1==i).map(_._2).getOrElse(BigInt(0)).U)
        }
        dut.io.tlb.req.ready.poke(ready.B)
        dut.io.tlb.resp.valid.poke(false.B)
        dut.io.tlb.resp.bits.poke(0.U.asTypeOf(dut.io.tlb.resp.bits))
        replies.headOption.filter(_._3==cycle).foreach { case(va,attempt,_) =>
          replies.dequeue(); dut.io.tlb.resp.valid.poke(true.B)
          dut.io.tlb.resp.bits.paddr.head.poke(va.U)
          dut.io.tlb.resp.bits.miss.poke((retry && attempt==1 && live(va).initial).B)
        }
        for(c <- 0 until 2) {
          val io=dut.io.clients(c)
          if(flag(io.resp.valid)) {
            val va=value(io.resp.bits.vaddr); val r=live.remove(va).get
            assert(r.client==c && completed.add(va) && !flag(io.resp.bits.dropped))
            assert(flag(io.resp.bits.retried)==(retry && r.initial))
            if(r.initial) initialRetired+=1
          }
          if(flag(io.req.valid) && flag(io.req.ready)) {
            val va=value(io.req.bits); live(va)=Req(c,cycle,va<0x10200); held=None
          }
        }
        if(flag(dut.io.tlb.req.valid) && ready) {
          val va=value(dut.io.tlb.req.bits.vaddr); val r=live(va)
          issued(va)+=1
          if(issued(va)==1) {
            assert(cycle-math.max(8,r.born+1)<8,s"unfair VA=$va born=${r.born} cycle=$cycle")
            if(r.initial) slots+=value(dut.issuedSlot).toInt
          } else {
            val minimum=if(r.client==0) 12 else 11
            assert(issued(va)==2 && cycle-last(va)>=minimum+1 && cycle-last(va)<=minimum+8,
              s"retry service VA=$va delta=${cycle-last(va)}")
          }
          last(va)=cycle; replies.enqueue((va,issued(va),cycle+1))
        }
        dut.clock.step();cycle+=1
      }
      for(_ <- 0 until 8) tick(true,false)
      assert(live.size==8,"default capacity not filled")
      for(_ <- 0 until 256) tick(true,true)
      var timeout=0
      while(live.nonEmpty || held.nonEmpty || replies.nonEmpty) { tick(false,true);timeout+=1;assert(timeout<100) }
      assert(slots.toSet==(0 until 8).toSet && initialRetired==8 && completed.size>100)
      println(s"TLB_FAIR_CASE pass=true entries=8 mixed=$mixed retry=$retry initialRetired=$initialRetired slots=${slots.toSeq.sorted.mkString(",")} completed=${completed.size}")
    }
  }
}
