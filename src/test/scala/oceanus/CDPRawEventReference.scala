package oceanus

import chisel3._
import chisel3.simulator.ChiselSim
import oceanus.compactchi.CCHIOpcode
import oceanus.l2._
import scala.collection.mutable
import utility.MemReqSource

// M2: upstream identities + directory responses/updates + independent immutable
// memory establish raw events. No DUT train/evict/capture output feeds prediction.
// Losing pulses disappear at the two separate RR arbiters, before admission.
class CDPRawEventReference(slices: Int, memory: (BigInt,Int)=>BigInt, tshrs: Int = 8, sets: Int = 128) extends ChiselSim {
  case class Meta(hit: Boolean=false,state: Int=0,source: Int=0,depth: Int=0)
  case class Train(pa: BigInt,va: BigInt,source: Int,depth: Int,hit: Boolean,req: Int,en: Boolean)
  case class Line(pa: BigInt,source: Int,depth: Int,hit: Boolean)
  case class Half(line: Line,high: Boolean)
  case class Selected(pa: BigInt=0,va: BigInt=0,victim: BigInt=0,source: Int=0,hit: Boolean=false,
    prefetched: Boolean=false,req: Int=0,en: Boolean=false,vpn: Boolean=false,used: Boolean=false,evict: Boolean=false)
  val coverage=mutable.Map.empty[String,Long].withDefaultValue(0L)
  def hit(name: String,n: Long=1): Unit=coverage(name)+=n
  def f(x: Bool)=x.litOption.getOrElse(x.peek().litValue)!=0
  def v(x: UInt)=x.litOption.getOrElse(x.peek().litValue)
  private val metas=Array.fill(slices,tshrs)(Meta())
  private val pending=Array.fill[Option[Line]](slices,tshrs)(None)
  private val full=Array.fill(slices,tshrs)(false)
  private val mergedWait=Array.fill(slices,tshrs)(false)
  private val queues=Seq.fill(slices)(mutable.Queue.empty[Half])
  private val drop=Array.fill[BigInt](slices)(0);private val collision=Array.fill[BigInt](slices)(0)
  private var vpnLast=0;private var filterLast=0;private var halfLast=0
  private var vpnCollisions=BigInt(0);private var filterCollisions=BigInt(0)
  private var filterCount=0;private var vpnCount=0
  private val requests=mutable.Map.empty[(BigInt,BigInt),mutable.Queue[(BigInt,BigInt,Int,Boolean)]]
  val directory=mutable.Map.empty[(Int,Int,Int),(BigInt,Meta)]
  // Directory ways can be reassigned before the old EvictBack host finishes.
  // Keep the committed line lifecycle through that host's local invalidation;
  // tag reuse must not erase the victim's still-live provenance.
  private val residentLines=mutable.Map.empty[BigInt,Meta]
  private val residentHistory=mutable.Queue.empty[String]
  private def history(s:String):Unit={residentHistory.enqueue(s);if(residentHistory.size>80)residentHistory.dequeue()}
  def upstream(source: BigInt,txn: BigInt,pa: BigInt,va: BigInt,req: Int,en: Boolean): Unit =
    requests.getOrElseUpdate((source,txn),mutable.Queue.empty).enqueue((pa,va,req,en))
  private def rr(indices: Seq[Int],last: Int): Option[Int]=indices.find(_>last).orElse(indices.headOption)
  def sample(dut: TestTopCDPRawAudit,row: Map[String,BigInt],cycle: Int): Unit = {
    def num(path:String):BigInt=row(path)
    def bit(path:String):Boolean=row(path)!=0
    val observePrefetch=dut.module.config.cdpObserveEnable
    val enabled=row("enable")!=0
    val train=mutable.ArrayBuffer.empty[(Int,Train)]
    val evict=mutable.ArrayBuffer.empty[(Int,Meta,BigInt)]
    val lines=Seq.fill(slices)(mutable.ArrayBuffer.empty[(Int,Line)])
    for(bank<-0 until slices;i<-0 until tshrs) {
      val p=s"raw$bank.$i";val addr=num(p+".addr");val old=metas(bank)(i)
      val exec=bit(p+".exec");val req=p+".req"
      val demand=num(req+".SrcID")!=dut.module.config.pfSrcID
      val opcode=num(req+".Opcode").toInt
      val read=Set(CCHIOpcode.ReadShared.opcode,CCHIOpcode.ReadUnique.opcode,CCHIOpcode.MakeUnique.opcode)(opcode)
      var start: Option[Line]=None
      if(exec) {
        hit("executed")
        if(demand) {
          val k=(num(req+".SrcID"),num(req+".TxnID"))
          val q=requests.getOrElse(k,throw new AssertionError(s"M2 execution without upstream $k cycle=$cycle"))
          assert(q.nonEmpty);val (pa,va,rs,en)=q.dequeue();if(q.isEmpty) requests.remove(k)
          assert(pa==addr && (!observePrefetch || ((va>>6)==num(p+".context.vpn")*64+(addr>>6&63) && rs==num(p+".context.reqSource") && en==bit(p+".context.trainEnable"))),s"M2 upstream context cycle=$cycle key=$k")
          if(opcode==CCHIOpcode.ReadOnce.opcode) hit("ReadOnce")
        }
        if(observePrefetch && demand && read) train+=((bank*tshrs+i,Train(addr,(num(p+".context.vpn")<<6)|(addr>>6&63),if(old.hit) old.source else 0,if(old.hit) old.depth else 0,old.hit,num(p+".context.reqSource").toInt,bit(p+".context.trainEnable"))))
        val isHit=demand && old.hit && Set(9,L2PfSource.SMS,2,3)(old.source) &&
          Set(CCHIOpcode.ReadShared.opcode,CCHIOpcode.ReadUnique.opcode)(opcode) && bit(req+".ExpCompData")
        val refill= !old.hit && Set(CCHIOpcode.ReadShared.opcode,CCHIOpcode.ReadUnique.opcode,CCHIOpcode.StashShared.opcode,CCHIOpcode.StashUnique.opcode)(opcode)
        if(observePrefetch && (isHit || refill)) {
          val pf= !demand && (num(req+".TxnID")>>4)!=0
          start=Some(Line(addr,if(isHit) old.source else if(pf) (num(req+".TxnID")>>4).toInt else 0,
            if(isHit) old.depth else if(pf) ((num(req+".TxnID")>>2)&3).toInt else 0,isHit))
        }
      }
      // M5: a data demand accepted into a live CDP prefetch RBE owns a
      // default-meta refill scan when it executes. Training keeps old meta.
      // Derive this from allocator acceptance and the host's pending identity.
      val mergeEnter=bit(p+".enterREQ") && bit(p+".reuseREQ") &&
        num(p+".incomingSource")!=num(p+".prefetchSourceID") &&
        Set(CCHIOpcode.ReadShared.opcode,CCHIOpcode.ReadUnique.opcode)(num(p+".incomingOpcode").toInt) &&
        bit(p+".incomingData") && bit(p+".prefetchActive") &&
        pending(bank)(i).exists(x=>x.source==9 && !x.hit)
      val mergeWaiting=mergedWait(bank)(i) || mergeEnter
      val mergedStart=mergedWait(bank)(i) && start.exists(_.hit)
      if(mergedStart) { start=start.map(x=>x.copy(source=0,depth=0,hit=false));hit("mergedScans") }
      val expectedTrain=train.lastOption.filter(_._1==bank*tshrs+i).map(_._2)
      assert(bit(p+".train.valid")==expectedTrain.nonEmpty,s"M2 raw train valid bank=$bank tshr=$i cycle=$cycle")
      expectedTrain.foreach { t=>
        val b=p+".train.bits"
        assert(num(b+".paddr")==t.pa && num(b+".vaddr")==t.va && num(b+".pfSource")==t.source && num(b+".pfDepth")==t.depth && bit(b+".hit")==t.hit && num(b+".reqSource")==t.req && bit(b+".trainEnable")==t.en,s"M2 raw train fields cycle=$cycle")
        if(t.req==MemReqSource.L1DataPrefetch.id) hit("L1DataPrefetch")
        if(!t.en) hit("trainEnable0")
      }
      val e=bit(p+".evictState") && bit(p+".metaMask.state") && num(p+".metaValue.state")==0 && old.hit && old.state!=0
      assert(bit(p+".evict.valid")== (observePrefetch && e),s"M2 eviction lifecycle cycle=$cycle bank=$bank tshr=$i old=$old")
      if(e) {
        if (observePrefetch) assert(num(p+".evict.bits.paddr")==addr && num(p+".evict.bits.pfSource")==old.source && num(p+".evict.bits.pfDepth")==old.depth)
        val resident=residentLines.get(addr)
        assert(resident.exists(m=>m.state!=0 && m.source==old.source && m.depth==old.depth),
          s"M2 victim lifecycle PA=$addr old=$old resident=$resident history=${residentHistory.mkString(";")}")
        history(s"evict cycle=$cycle bank=$bank tshr=$i addr=$addr old=$old");residentLines.remove(addr)
        evict+=((bank*tshrs+i,old,addr));hit("evictions")
      }
      val ctx=start.orElse(if(!bit(p+".reqValid")) pending(bank)(i) else None)
      val expectedLine=ctx.filter { x=>
        val cdpRefill=x.source==9 && !x.hit
        val complete=bit(p+".last") || bit(p+".ds") ||
          ((x.hit || cdpRefill || mergedStart) && full(bank)(i) && !bit(p+".rx"))
        complete && (!bit(p+".dealloc") || (cdpRefill && full(bank)(i) && pending(bank)(i).nonEmpty)) &&
          (!mergeWaiting || mergedStart) && !(cdpRefill && bit(p+".prefetchActive"))
      }
      assert(bit(p+".line.valid")==expectedLine.nonEmpty,s"M2 capture lifecycle cycle=$cycle bank=$bank tshr=$i context=$ctx last=${bit(p+".last")} ds=${bit(p+".ds")}")
      expectedLine.foreach { l=>
        val b=p+".line.bits"
        assert(num(b+".paddr")==l.pa && num(b+".pfSource")==l.source && num(b+".pfDepth")==l.depth && bit(b+".isHit")==l.hit && num(b+".eventType")==(if(l.hit) 1 else 0))
        assert(num(b+".data")==(memory(l.pa,0)|(memory(l.pa,1)<<256)),s"M2 independent capture memory cycle=$cycle PA=${l.pa}")
        lines(bank)+=((i,l));hit("captureLines")
      }
      if(bit(p+".reqValid")||bit(p+".dealloc")) { pending(bank)(i)=None;mergedWait(bank)(i)=false }
      if(mergeEnter) mergedWait(bank)(i)=true
      start.foreach(x=>pending(bank)(i)=Some(x))
      if(expectedLine.nonEmpty) pending(bank)(i)=None
      if(bit(p+".rx")||bit(p+".dealloc")) full(bank)(i)=false
      if(bit(p+".last")||bit(p+".ds")) full(bank)(i)=true
      if(bit(p+".dirRead")) { val m=p+".dirMeta";metas(bank)(i)=Meta(bit(m+".hit"),num(m+".state").toInt,num(m+".pfSource").toInt,num(m+".cdpPfDepth").toInt) }
      if(exec && demand && read && old.hit) residentLines.get(addr).foreach(m=>residentLines(addr)=m.copy(source=0,depth=0))
      // Local masked writes change the old-meta view before directory commit.
      var m=metas(bank)(i)
      if(bit(p+".metaMask.state")) m=m.copy(state=num(p+".metaValue.state").toInt,hit=num(p+".metaValue.state")!=0)
      if(bit(p+".metaMask.pfSource")) m=m.copy(source=num(p+".metaValue.pfSource").toInt,depth=num(p+".metaValue.cdpPfDepth").toInt)
      metas(bank)(i)=m
      if(bit(p+".dirWrite")) {
        val w=p+".write";val key=(bank,((num(w+".PADDR")>>6)&(sets-1)).toInt,num(w+".WAY").toInt)
        val previous=directory.getOrElse(key,(num(w+".PADDR"),Meta()))
        var dm=previous._2
        if(bit(w+".META_WEN.state")) dm=dm.copy(state=num(w+".META.state").toInt,hit=num(w+".META.state")!=0)
        if(bit(w+".META_WEN.pfSource")) dm=dm.copy(source=num(w+".META.pfSource").toInt,depth=num(w+".META.cdpPfDepth").toInt)
        val line=if(bit(w+".TAG_WEN")) num(w+".PADDR") else previous._1
        directory(key)=(line,dm)
        history(s"write cycle=$cycle bank=$bank tshr=$i addr=$line pa=${num(w+".PADDR")} tag=${bit(w+".TAG_WEN")} way=${num(w+".WAY")} state=$dm")
        if(dm.state!=0)residentLines(line)=dm
        else residentLines.remove(line)
      }
    }
    if (observePrefetch) checkAdapters(row,cycle,train.toSeq,evict.toSeq,lines.map(_.toSeq),dut.selectedVpn,dut.selectedFilter,dut.halfOut,dut.halfBank)
    else assert(train.isEmpty && lines.forall(_.isEmpty), "structural-off predicted prefetch events")
  }
  // The same M2 RR/admission/capture oracle is also exercised at the adapter
  // interface. Those directed witnesses are reported separately from system
  // reachability; they never replace cache lifecycle evidence above.
  def checkAdapters(row:Map[String,BigInt],cycle:Int,train:Seq[(Int,Train)],evict:Seq[(Int,Meta,BigInt)],
      lines:Seq[Seq[(Int,Line)]],selectedVpn:chisel3.util.Valid[OceanusCDPTrain],
      selectedFilter:chisel3.util.Valid[OceanusCDPTrain],halfOut:chisel3.util.Valid[L2CDPHalf],halfBank:UInt):Unit={
    val enabled=row("enable")!=0
    if(train.size>1) {hit("multiTrain");if(train.map(_._1/tshrs).distinct.size>1) hit("crossSliceTrain");if(train.groupBy(_._1/tshrs).values.exists(_.size>1)) hit("crossTSHRTrain")}
    if(evict.size>1) {hit("multiEvict");if(evict.map(_._1/tshrs).distinct.size>1) hit("crossSliceEvict");if(evict.groupBy(_._1/tshrs).values.exists(_.size>1)) hit("crossTSHREvict")}
    if(train.nonEmpty && evict.nonEmpty) hit("trainEvictSameCycle")
    val eligible=train.filter(t=>enabled && t._2.en && t._2.req!=MemReqSource.L1DataPrefetch.id)
    def mapped(t: Train)=Selected(t.pa,t.va,source=t.source,hit=t.hit,prefetched=t.source!=0,req=t.req,en=t.en,vpn=true,used=t.hit)
    val vs=eligible.map(t=>t._1->mapped(t._2)).toMap
    val fs=(eligible.filter(_._2.hit).map(t=>t._1->mapped(t._2))++evict.filter(e=>enabled && e._2.source==9).map { e=>
      (e._1+slices*tshrs)->Selected(victim=e._3,source=9,prefetched=true,req=MemReqSource.Prefetch2L2CDP.id,en=true,evict=true)
    }).toMap
    val vi=rr(vs.keys.toSeq.sorted,vpnLast);val fi=rr(fs.keys.toSeq.sorted,filterLast)
    def compare(p: chisel3.util.Valid[OceanusCDPTrain],e: Option[Selected]): Unit = {
      assert(f(p.valid)==e.nonEmpty,s"M2 selected valid cycle=$cycle")
      e.foreach { t=>val b=p.bits
        assert(v(b.paddr)==t.pa && v(b.vaddr)==t.va && v(b.evictPaddr)==t.victim && v(b.pfSource)==t.source && f(b.hit)==t.hit && f(b.prefetched)==t.prefetched && v(b.reqSource)==t.req && f(b.trainEnable)==t.en && f(b.vpnValid)==t.vpn && f(b.isUsed)==t.used && f(b.isEvict)==t.evict,s"M2 RR selected payload cycle=$cycle expected=$t actualPA=${v(b.paddr)} actualVA=${v(b.vaddr)} victim=${v(b.evictPaddr)} source=${v(b.pfSource)} hit=${f(b.hit)} prefetched=${f(b.prefetched)} req=${v(b.reqSource)} en=${f(b.trainEnable)} vpn=${f(b.vpnValid)} used=${f(b.isUsed)} evict=${f(b.isEvict)}")
      }
    }
    compare(selectedVpn,vi.map(vs));compare(selectedFilter,fi.map(fs))
    if(vi.nonEmpty && fi.nonEmpty && vi!=fi) hit("vpnFilterDifferent")
    if(eligible.exists(_._2.hit) && evict.exists(_._2.source==9)) hit("usedEvictConflict")
    assert(row("vpnCollisions")==vpnCollisions && row("filterCollisions")==filterCollisions)
    vpnCollisions+=vs.size-vi.size;filterCollisions+=fs.size-fi.size
    vi.foreach(vpnLast=_);fi.foreach(filterLast=_)
    assert(row("filterQueueCount")==filterCount && row("vpnQueueCount")==vpnCount,s"M2 admission occupancy cycle=$cycle")
    val fa=fi.nonEmpty && filterCount<8;val fd=fi.nonEmpty && !fa
    val va=vi.nonEmpty && vpnCount<8;val vd=vi.nonEmpty && !va
    assert(row("filterAccepted")== (if(fa) 1 else 0) && row("filterDropped")== (if(fd) 1 else 0),s"M2 Filter accept/drop cycle=$cycle")
    assert(row("vpnAccepted")== (if(va) 1 else 0) && row("vpnDropped")== (if(vd) 1 else 0))
    if(fa) hit("filterAccepted");if(fd) hit("filterDropped")
    filterCount+=(if(fa) 1 else 0)-row("filterDequeue").toInt
    vpnCount+=(if(va) 1 else 0)-row("vpnDequeue").toInt
    val chosen=rr(queues.indices.filter(queues(_).nonEmpty),halfLast)
    assert(f(halfOut.valid)==chosen.nonEmpty,s"M2 half RR cycle=$cycle")
    chosen.foreach { bank=>
      assert(v(halfBank)==bank)
      val h=queues(bank).dequeue();val b=halfOut.bits;val l=h.line
      assert(v(b.paddr)==l.pa && f(b.high)==h.high && v(b.data)==memory(l.pa,if(h.high) 1 else 0) && v(b.pfSource)==l.source && v(b.pfDepth)==l.depth && f(b.isHit)==l.hit)
      halfLast=bank;hit("halves")
    }
    for(bank<-0 until slices) {
      assert(row(s"captureDrops$bank")==drop(bank) && row(s"collisionDrops$bank")==collision(bank))
      if(lines(bank).nonEmpty) {
        val before=queues(bank).size;val n=math.min(2,8-before)
        if(n==1) hit("captureOneSlot");if(n==0) hit("captureFull")
        if(chosen.contains(bank)) hit("captureSimultaneous")
        val l=lines(bank).head._2;queues(bank).enqueueAll((0 until n).map(i=>Half(l,i==1)))
        drop(bank)+=2-n;collision(bank)+=(lines(bank).size-1)*2
        if(lines(bank).size>1) hit("captureCollision")
      }
    }
  }
  def requireCoverage(names: Seq[String]): Unit=names.foreach { name=>
    assert(coverage(name)>0,s"M3 required coverage missing: $name all=$coverage")
  }
  def finish(): Unit={
    assert(requests.isEmpty && queues.forall(_.isEmpty),s"M2 residual requests=$requests halves=$queues")
    println("CDP_RAW_REFERENCE_RESULT pass=true "+coverage.toSeq.sortBy(_._1).map {case(k,v)=>s"$k=$v"}.mkString(" "))
  }
}
