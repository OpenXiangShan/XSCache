package oceanus

import chisel3._
import chisel3.util._
import java.nio.file.{Files,Path}
import scala.collection.mutable

// Test-only observations. Qualification is reconstructed from raw terminal
// transactions; query selection and completion order are checked by a host RR
// and pipeline model, independently of DUT chosen IDs and output order.
class CDPC4OrderingHarness(checkFixed: Boolean = true)(implicit p: org.chipsalliance.cde.config.Parameters)
  extends CDPC4Harness(checkFixed) {
  val orderFields=mutable.ArrayBuffer.empty[(String,UInt)]
  def observe(n:String,d:Data):Unit=orderFields += n -> read(d).asUInt
  for((s,n)<-Seq(prod->"d")) {
    observe(n+".valid",s.valids);observe(n+".pa",VecInit((0 until 16).map(i=>read(s.valids)(i) && read(s.entries)(i).paddrValid)))
    observe(n+".eligible",VecInit((0 until 16).map(i=>read(s.pftArb.io.in(i).valid))))
    observe(n+".inflight",s.reqInflight);observe(n+".query",s.io.ftQueryReq.fire)
    observe(n+".queryIndex",s.pftArb.io.chosen);observe(n+".queryReady",s.io.ftQueryReq.ready)
    observe(n+".drop",s.translationDrop);observe(n+".filterDrop",s.filterDrop);observe(n+".filterIndex",s.idx2)
    observe(n+".out",s.io.out.valid);observe(n+".outIndex",s.output.io.deq.bits.index)
    observe(n+".outPA",s.io.out.bits.paddr);observe(n+".outDepth",s.io.out.bits.pfDepth)
    observe(n+".request",s.io.tlb.req.fire);observe(n+".requestVA",s.io.tlb.req.bits)
    observe(n+".terminal",s.io.tlb.resp.fire);observe(n+".terminalVA",s.io.tlb.resp.bits.vaddr)
    observe(n+".terminalDrop",s.io.tlb.resp.bits.dropped);observe(n+".queue",s.output.io.count)
  }
  val rs=reference
  observe("r.valid",rs.valids);observe("r.pa",VecInit((0 until 16).map(i=>read(rs.valids)(i) && read(rs.entries)(i).paddrValid)))
  observe("r.eligible",VecInit((0 until 16).map(i=>read(rs.pftArb.io.in(i).valid))))
  observe("r.inflight",rs.reqInflight);observe("r.query",rs.io.ftQueryReq.fire)
  observe("r.queryIndex",rs.pftArb.io.chosen);observe("r.queryReady",rs.io.ftQueryReq.ready)
  observe("r.drop",refDrops);observe("r.filterDrop",read(rs.pft_s2_valid) && !read(rs.canPft))
  observe("r.filterIndex",rs.pft_s2_chosen_idx)
  observe("r.out",rs.io.out.valid);observe("r.outIndex",rs.pft_s2_chosen_idx)
  observe("r.outPA",rs.io.out.bits.addr);observe("r.outDepth",rs.io.out.bits.cdpPfDepth.get)
  observe("r.request",rs.io.tlbReq.req.fire);observe("r.requestVA",rs.io.tlbReq.req.bits.vaddr)
  observe("r.terminal",read(rs.tlb_s3_valid) && read(rs.tlb_s3_rsp_valid) && !read(rs.tlb_s3_rsp_bits).miss)
  observe("r.terminalVA",rs.tlb_s3_addr);observe("r.terminalDrop",rs.s3_drop)
  val order=IO(Output(UInt(orderFields.map(_._2.getWidth).sum.W)))
  order:=Cat(orderFields.map(_._2).reverse.toSeq)
  val orderWidths=orderFields.map{case(n,d)=>n->d.getWidth}.toVector
}

// Exact software transcription of the existing two-level arbitration rule.
// No chosen-index observations are used to advance this model.
class CDPOrderRR {
  private val pending=Array(0,0);private val grant=Array(0,0);private var low=false
  private def first(x:Int):Int=x & -x
  def choose(mask:Int,fire:Boolean):Int={
    val v=Array(mask&255,(mask>>8)&255)
    val c=(0 to 1).map{i=>val r=first(pending(i)&grant(i));if((r&v(i))!=0)r else first(v(i))}.toArray
    val selectLow=if(v(0)!=0 && v(1)!=0)low else v(0)!=0
    val side=if(selectLow)0 else 1;val bit=c(side)
    val result=if(bit==0)-1 else Integer.numberOfTrailingZeros(bit)+side*8
    if(fire){
      assert(result>=0)
      pending(side)=v(side)& ~bit;grant(side)=(255 ^ ((bit<<1)-1))&255
      low=if(selectLow)v(1)==0 else v(0)!=0
    }
    result
  }
}

class CDPOrderingTracker(dut:CDPC4OrderingHarness,dir:Path) extends chisel3.simulator.ChiselSim {
  case class Item(id:Int,va:BigInt,depth:Int,source:Int,allocated:Int,slot:Int)
  case class Query(due:Int,item:Item,allow:Boolean)
  class Side {
    val live=mutable.Map.empty[Int,Item];val pa=mutable.Set.empty[Int];val inflight=mutable.Set.empty[Int]
    val pipeline=mutable.Queue.empty[Query];val completed=mutable.Queue.empty[Item];val rr=new CDPOrderRR
    val predicted=mutable.ArrayBuffer.empty[String];val observed=mutable.ArrayBuffer.empty[String];val records=mutable.Map.empty[Int,ujson.Obj]
    val requests=mutable.Map.empty[BigInt,Int].withDefaultValue(0)
  }
  var sides=Array(new Side,new Side);var cycle=0;var nextID=0;var label=""
  val environment=mutable.ArrayBuffer.empty[ujson.Value]
  def reset(n:String):Unit={sides=Array(new Side,new Side);cycle=0;nextID=0;label=n;environment.clear()}
  private def mask(xs:IterableOnce[Int]):BigInt=xs.iterator.foldLeft(BigInt(0))((m,i)=>m.setBit(i))
  private def physical(va:BigInt):BigInt=(((va>>12)^BigInt("123456",16))<<12)|(va&4032)
  def sample(encoded:BigInt,ready:Boolean,filter:Boolean,query:Boolean,allowOverride:Option[BigInt=>Boolean]=None):Unit={
    var bits=dut.order.peek().litValue
    val observed=dut.orderWidths.map{case(n,w)=>val v=bits&((BigInt(1)<<w)-1);bits>>=w;n->v}.toMap
    val offered=(encoded>>57)!=0;val va=(encoded>>7)&((BigInt(1)<<50)-1);val depth=((encoded>>5)&3).toInt
    val source=((encoded>>1)&15).toInt
    for((s,n)<-sides.zip(Seq("d","r"))) {
      def v(k:String)=observed(n+"."+k)
      def yes(k:String)=v(k)!=0
      def need(ok:Boolean,what:String):Unit=assert(ok,s"ordering $label cycle=$cycle side=$n $what")
      need(v("valid")==mask(s.live.keys),"live identity")
      need(v("pa")==mask(s.pa),"qualification not explained by a terminal page transaction")
      need(v("inflight")==mask(s.inflight),"inflight lifecycle")
      val eligible=s.live.keys.filter(i=>s.pa(i) && !s.inflight(i)).toVector
      need(v("eligible")==mask(eligible),"qualification/inflight eligibility")
      val queryReady=yes("queryReady");need(queryReady==query,"query gate");val qfire=eligible.nonEmpty && queryReady
      need(yes("query")==qfire,"query fire")
      val chosen=s.rr.choose(mask(eligible).toInt,qfire)
      if(qfire)need(v("queryIndex")==chosen,"independent RR selection")
      val due=if(s.pipeline.headOption.exists(_.due==cycle))Some(s.pipeline.dequeue())else None
      val accept=due.filter(_.allow);val reject=due.filterNot(_.allow)
      if(n=="d")accept.foreach(q=>s.completed.enqueue(q.item))
      val output=if(n=="d")s.completed.headOption else accept.map(_.item)
      need(yes("out")==output.nonEmpty,"output schedule")
      output.foreach{it=>need(v("outIndex")==it.slot && v("outPA")==physical(it.va) && v("outDepth")==it.depth,"output identity/order")}
      need(yes("filterDrop")==reject.nonEmpty,"Filter decision snapshot")
      reject.foreach(q=>need(v("filterIndex")==q.item.slot,"Filter retired identity"))
      val retired=mutable.Set.empty[Int]
      reject.foreach{q=>retired += q.item.slot;s.records(q.item.id)("filterDropCycle")=ujson.Num(cycle)}
      output.filter(_=>ready).foreach {it=>
        s.predicted += s"${it.id}:${physical(it.va)}:${it.depth}"
        val observedItem=s.live(v("outIndex").toInt);s.observed += s"${observedItem.id}:${v("outPA")}:${v("outDepth")}"
        s.records(it.id)("outputCycle")=ujson.Num(cycle);retired += it.slot
        if(n=="d")s.completed.dequeue()
      }
      if(n=="r")due.foreach(q=>s.inflight -= q.item.slot)
      if(qfire){
        val it=s.live(chosen);val allow=allowOverride.map(_(it.va)).getOrElse(!filter || (((physical(it.va)>>12)&63).toInt%5!=0))
        s.pipeline.enqueue(Query(cycle+2,it,allow));s.inflight += chosen
        s.records(it.id)("queries").arr += ujson.Arr(cycle,allow,ready)
      }
      if(yes("request")){val page=v("requestVA")>>12;s.requests(page)+=1}
      // A terminal covers exactly the pre-edge same-page waiting set. New
      // allocations on that edge deliberately miss this transaction fanout.
      if(yes("terminal") && !yes("terminalDrop")){
        val page=v("terminalVA")>>12
        for((slot,it)<-s.live if !s.pa(slot) && (it.va>>12)==page){
          s.pa += slot
          s.records(it.id)("qualificationCycle")=ujson.Num(cycle+1)
          s.records(it.id)("pageTransaction")=ujson.Arr(page.toString(16),s.requests(page),cycle)
        }
      }
      for(i<-0 until 16 if v("drop").testBit(i)){
        need(s.live.contains(i),"translation drop without owner");retired += i
      }
      val duplicate=s.live.values.exists(_.va==(va & ~BigInt(63)))
      val free=(0 until 16).find(i=> !s.live.contains(i))
      val allocated=offered && !duplicate && free.nonEmpty
      retired.foreach{i=>s.live.remove(i);s.pa -= i;s.inflight -= i}
      if(allocated){
        val i=free.get;val it=Item(nextID,va & ~BigInt(63),depth,source,cycle,i);s.live(i)=it;s.pa -= i;s.inflight -= i
        s.records(it.id)=ujson.Obj("id"->it.id,"va"->it.va.toString(16),"depth"->depth,"source"->source,
          "inputCycle"->cycle,"slot"->i,"page"->(it.va>>12).toString(16),"queries"->ujson.Arr())
      }
    }
    environment += ujson.Arr(cycle,ready,filter,query)
    if(offered)nextID+=1
    cycle+=1
  }
  def finish(actual:Seq[Seq[String]],compareSet:Boolean,save:Boolean=false,requireSourceOrder:Boolean=false):Unit={
    for(i<-0 to 1) {
      assert(actual(i)==sides(i).predicted.toSeq,s"ordering $label side=$i output permutation outside the independent schedule")
      assert(sides(i).live.isEmpty && sides(i).pipeline.isEmpty && sides(i).completed.isEmpty,
        s"ordering unfinished label=$label side=$i live=${sides(i).live.toVector} pa=${sides(i).pa} inflight=${sides(i).inflight} pipe=${sides(i).pipeline.toVector} completed=${sides(i).completed.toVector} lastQueries=${sides(i).live.values.toVector.map(it=>it.id -> sides(i).records(it.id)("queries").arr.takeRight(5))}")
      // Mutation check: exchanging a same-source pair keeps its multiset,
      // yet must violate the independently predicted order.
      if(actual(i).size>=2){val mutated=actual(i).updated(0,actual(i)(1)).updated(1,actual(i)(0));assert(mutated.sorted==actual(i).sorted && mutated!=sides(i).predicted.toSeq)}
    }
    if(compareSet){
      assert(actual(0).sorted==actual(1).sorted,"complete output multiset")
      val outputIDs=actual(0).map(_.takeWhile(_!=':').toInt)
      for(id<-outputIDs){
        val d=sides(0).records(id);val r=sides(1).records(id)
        for(k<-Seq("id","va","depth","source","inputCycle","page"))assert(d(k)==r(k),s"reorder changed input identity $label id=$id key=$k")
        for(record<-Seq(d,r))assert(record.obj.contains("pageTransaction") && record.obj.contains("qualificationCycle"),"reorder lacks a page qualification witness")
      }
      if(requireSourceOrder){
        for(source<-outputIDs.map(i=>sides(0).records(i)("source").num.toInt).distinct){
          val bySource=actual.map(_.filter(v=>sides(0).records(v.takeWhile(_!=':').toInt)("source").num.toInt==source))
          assert(bySource(0)==bySource(1),s"same-source contract requires a D5 decision label=$label source=$source")
        }
      }
    }
    if(compareSet || save){
      val dest=dir.resolve(label+".order.json")
      Files.writeString(dest,ujson.Obj("head"->sys.props.getOrElse("oceanus.c4.head","unknown"),"label"->label,
        "completeMultisetEqual"->(actual(0).sorted==actual(1).sorted),"cycles"->cycle,"environment"->ujson.Arr.from(environment),
        "dut"->ujson.Obj("predicted"->ujson.Arr.from(sides(0).predicted),"items"->ujson.Arr.from(sides(0).records.toVector.sortBy(_._1).map(_._2))),
        "reference"->ujson.Obj("predicted"->ujson.Arr.from(sides(1).predicted),"items"->ujson.Arr.from(sides(1).records.toVector.sortBy(_._1).map(_._2))),
        "allowedSwapRule"->"same source/input identity/page; qualification explained by raw page terminal; both orders must match independent RR/completion replay",
        "independentOrderChecked"->true,"mutationRejected"->true,"policyApproval"->"D5-pending").render())
    }
  }
}
