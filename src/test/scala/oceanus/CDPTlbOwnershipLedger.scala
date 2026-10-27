package oceanus

import scala.collection.mutable

// Independent finite IDs bind adapter allocation -> retry -> external FIFO ->
// result -> client retirement. CSR generations never reset these live maps.
class CDPTlbOwnershipLedger {
  case class Item(id:Long,va:BigInt,owner:Int,generation:Long,var issued:Int=0,
    var result:Option[(BigInt,Boolean,Boolean)]=None)
  private val slots=mutable.Map.empty[Int,Item]
  private val inflight=mutable.Queue.empty[(Int,Long)]
  private var next=0L;private var retired=0L;private var oldRetired=0L
  private var latestGeneration=0L;private var retryWitness=0L
  def counters:Map[String,Long]=Map("allocated"->next,"retired"->retired,"live"->slots.size.toLong,
    "oldRetired"->oldRetired,"retries"->retryWitness,"inflight"->inflight.size.toLong)
  def closeGeneration(g:Long):Unit={assert(g==latestGeneration)}
  def sample(r:Map[String,BigInt],cycle:Int,generation:Long):Unit={
    latestGeneration=generation
    def b(n:String)=r(n)!=0
    def check(ok:Boolean,why:String):Unit=assert(ok,s"M4 TLB ownership cycle=$cycle $why slots=$slots inflight=$inflight")
    for(i<-0 until 8) {
      val state=r(s"adapterSlot$i.state")
      check((state!=0)==slots.contains(i),s"hardware/live slot=$i state=$state")
      slots.get(i).foreach { x=>check(r(s"adapterSlot$i.va")==x.va && r(s"adapterSlot$i.owner")==x.owner,s"slot identity changed id=${x.id}") }
    }
    for((event,owner)<-Seq("adapterClient0Resp"->0,"terminal"->1) if b(event)) {
      val va=r(event+".va")
      val found=slots.toSeq.filter{case(_,x)=>x.owner==owner&&x.va==va}
      check(found.size==1,s"ambiguous client retirement owner=$owner VA=$va")
      val (index,x)=found.head
      val expected=x.result.getOrElse(throw new AssertionError(s"M4 client result without external response id=${x.id}"))
      check(r(event+".pa")==expected._1 && b(event+".retry")==expected._2 && b(event+".miss")==expected._3,s"result belongs to different identity id=${x.id}")
      if(x.generation<generation)oldRetired+=1
      slots.remove(index);retired+=1
    }
    if(b("externalResponse")) {
      check(inflight.nonEmpty,"orphan external response")
      val(index,id)=inflight.dequeue();val x=slots.getOrElse(index,throw new AssertionError("M4 response reused slot"))
      check(x.id==id,"stale response assigned to new allocation")
      val miss=b("externalResponse.miss")
      if(!miss || x.issued==2)x.result=Some((r("externalResponse.pa"),x.issued==2,miss))
    }
    if(b("externalReq")) {
      val index=r("externalReq.index").toInt
      val x=slots.getOrElse(index,throw new AssertionError(s"M4 external req has no accepted client slot=$index"))
      check(x.va==r("externalReq.va") && x.owner==r("externalReq.owner"),s"external request wrong allocation id=${x.id}")
      check(x.issued<2,s"excess retries id=${x.id}")
      x.issued+=1;if(x.issued==2)retryWitness+=1
      inflight.enqueue(index->x.id)
    }
    for((event,owner)<-Seq("adapterClient0Req"->0,"tlbRequest"->1) if b(event)) {
      val index=r("adapterAlloc").toInt
      check(!slots.contains(index),s"allocation reused live slot=$index")
      slots(index)=Item(next,r(event+".va"),owner,generation);next+=1
    }
    check(next==retired+slots.size,"adapter identity conservation")
  }
  def finish():Unit={assert(slots.isEmpty&&inflight.isEmpty,s"M4 TLB residual $counters");println(s"CDP_TLB_OWNERSHIP pass=true counters=$counters")}
}
