package oceanus

import chisel3._
import chisel3.simulator.ChiselSim
import scala.collection.mutable

// Test-only, pre-edge observations. Capacity sums include physically present
// slots; fullFraction is the fraction of resource-slot cycles spent full.
class StressPressureQuality(dut:TestTopStress,caseId:String,seed:Int,slices:Int,
  minimum:Int,trace:Boolean) extends ChiselSim {
  val sums=Array.fill(dut.metrics.size)(0L)
  val peaks=Array.fill(dut.metrics.size)(0)
  val full=Array.fill(dut.metrics.size)(0L)
  val groups=Seq("TSHR","RBE","prefetch","TLB").map { name=>
    val indices=dut.metrics.indices.filter {i=>
      val n=dut.metrics(i)._1
      name match {
        case "TSHR"=>n.matches("slice[0-9]+.active")
        case "RBE"=>n.contains("/L2RBE") && n.endsWith(".count")
        case "prefetch"=>n.contains("/OverwriteQueue") || n.startsWith("vbop.") || n.startsWith("pbop.") || n=="cdp.waiting" || n.contains("/L2CDPHalfQueue")
        case "TLB"=>n=="tlb.occupied"
      }
    }
    name->indices
  }.toMap
  val aggregatePeaks=mutable.Map.empty[String,Int].withDefaultValue(0)
  val allFull=mutable.Map.empty[String,Long].withDefaultValue(0L)
  val blocked=mutable.Map.empty[String,Long].withDefaultValue(0L)
  val offered=mutable.Map.empty[String,Long].withDefaultValue(0L)
  val unique=mutable.Set.empty[BigInt]
  val demandUnique=mutable.Set.empty[BigInt]
  val sets=mutable.Set.empty[(Int,Int)]
  var sameSetCycles=0L;var sameSetTSHRSum=0L;var downstreamEvictions=0L
  var sampled=0L;var maxSameSet=0;var maxSameSetLines=0;var evictions=0L
  var snoops=0L;var overlap=0L;var sameCycleLast=0L;var lastDrop=0L
  val pending=mutable.Map.empty[BigInt,Int].withDefaultValue(0)
  val lastRefill=mutable.Map.empty[BigInt,Int]
  val lastSnoop=mutable.Map.empty[BigInt,Int]
  val windowOverlap=mutable.Set.empty[(BigInt,Int,Int)]
  var arbitrationBoth=0L;var reqBypassed=0L;var snoopWins=0L;var snoopFire=0L
  var reported=false
  def bank(pa:BigInt):Int=if(slices==1) 0 else if(slices==2) ((pa>>6).bitCount&1) else {
    val lo=(6 until 48 by 2).count(pa.testBit(_))&1
    val hi=(7 until 48 by 2).count(pa.testBit(_))&1
    lo|(hi<<1)
  }
  def address(pa:BigInt):Unit={unique+=pa;sets+=((bank(pa),((pa>>6)&127).toInt))}
  def event(kind:String,cycle:Int,fields:String):Unit=if(trace) println(s"CONFLICT_EVENT $kind cycle=$cycle $fields")
  def hostChannel(cycle:Int,name:String,valid:Boolean,ready:Boolean):Unit=if(cycle<minimum && valid) {
    offered(name)+=1
    if(!ready) blocked(name)+=1
  }
  def downstream(cycle:Int,pa:BigInt,evict:Boolean):Unit={
    if(cycle<minimum) {address(pa);if(evict) downstreamEvictions+=1}
    if(!evict) pending(pa)+=1
  }
  def refill(cycle:Int,pa:BigInt,beat:Int):Unit={
    event("DN_DAT",cycle,s"pa=0x${pa.toString(16)} beat=$beat")
    if(beat==1) {
      if(pending(pa)>1) pending(pa)-=1 else pending.remove(pa)
      lastRefill(pa)=cycle
      lastSnoop.get(pa).filter(c=>cycle-c<=16).foreach {c=>windowOverlap+=((pa,c,cycle))}
    }
  }
  def snoop(cycle:Int,pa:BigInt):Unit={
    if(cycle<minimum) {
      snoops+=1;address(pa)
      if(pending(pa)>0) overlap+=1
      if(lastRefill.get(pa).contains(cycle)) sameCycleLast+=1
      lastRefill.get(pa).filter(c=>cycle-c<=16).foreach {c=>windowOverlap+=((pa,cycle,c))}
    }
    lastSnoop(pa)=cycle
    event("DN_SNP",cycle,s"pa=0x${pa.toString(16)}")
  }
  def sample(cycle:Int,state:BigInt):Unit={
    if(cycle<minimum) {
      sampled+=1
      for(i<-dut.metrics.indices) {
        val n=((state>>(32*i))&0xffffffffL).toInt
        sums(i)+=n;peaks(i)=peaks(i).max(n)
        if(n==dut.metrics(i)._3) full(i)+=1
      }
      for((name,indices)<-groups) {
        val occupancy=indices.map(i=>((state>>(32*i))&0xffffffffL).toInt).sum
        val cap=indices.map(i=>dut.metrics(i)._3).sum
        aggregatePeaks(name)=aggregatePeaks(name).max(occupancy)
        if(cap>0 && occupancy==cap) allFull(name)+=1
      }
      val req=dut.io.t1p.head.UpREQ
      if(req.valid.peek().litToBoolean && req.ready.peek().litToBoolean) {
        val pa=req.bits.Addr.peek().litValue
        address(pa);demandUnique+=pa
      }
    }
    var bits=dut.qualityPacked.peek().litValue
    val values=dut.qualitySignals.zip(dut.qualityWidths).map {case((name,_),width)=>
      val n=bits&((BigInt(1)<<width)-1);bits=bits>>width;name->n
    }.toMap
    def num(name:String)=values(name)
    def flag(name:String)=num(name)!=0
    if(cycle<minimum) for(name<-values.keys.filter(_.endsWith(".valid"))) {
      val prefix=name.stripSuffix(".valid")
      if(values.contains(prefix+".ready") && flag(name)) {
        offered(prefix)+=1
        if(!flag(prefix+".ready")) blocked(prefix)+=1
      }
    }
    if(cycle<minimum) for(b<-0 until slices) {
      if(flag(s"arb$b.req") && flag(s"arb$b.snp")) arbitrationBoth+=1
      if(flag(s"arb$b.req") && flag(s"arb$b.snpWins")) {
        snoopWins+=1
        if(!flag(s"arb$b.reqReady")) reqBypassed+=1
        if(flag(s"arb$b.snpFire")) snoopFire+=1
      }
    }
    val perSet=mutable.Map.empty[(Int,Int),mutable.ArrayBuffer[BigInt]]
    for(b<-0 until slices;t<-0 until 8) {
      val p=s"s$b.t$t."
      val pa=num(p+"pa");val hex=s"slice=$b tshr=$t pa=0x${pa.toString(16)}"
      if(cycle<minimum && flag(p+"valid")) {
        perSet.getOrElseUpdate((b,((pa>>6)&127).toInt),mutable.ArrayBuffer.empty)+=pa
      }
      if(cycle<minimum && flag(p+"evict")) evictions+=1
      if(flag(p+"drop") && lastRefill.get(pa).contains(cycle)) lastDrop+=1
      if(trace) {
        if(flag(p+"dirRead")) event("DIR_READ",cycle,hex+s" hit=${flag(p+"hit")} way=${num(p+"way")}")
        if(flag(p+"dirGrant")) event("DIR_WRITE",cycle,hex+s" tagWrite=${flag(p+"tagMask")} way=${num(p+"way")} state=${num(p+"dirState")}")
        if(flag(p+"dsRead")) event("DS_READ",cycle,hex+s" way=${num(p+"way")}")
        if(flag(p+"dsWrite")) event("DS_WRITE",cycle,hex+s" way=${num(p+"way")}")
        if(Seq("drop","dirGrant","reqWrite","snpWrite","dsGrant").exists(k=>flag(p+k)))
          event("META_FLOW",cycle,hex+s" directoryGrant=${flag(p+"dirGrant")} stateMask=${flag(p+"stateMask")} tagMask=${flag(p+"tagMask")} localState=${num(p+"localState")} directoryState=${num(p+"dirState")} reqWrite=${flag(p+"reqWrite")} reqState=${num(p+"reqState")} snpWrite=${flag(p+"snpWrite")} snpState=${num(p+"snpState")} bufferDrop=${flag(p+"drop")} dsGrant=${flag(p+"dsGrant")} bufferModified=${flag(p+"modified")}")
      }
    }
    if(perSet.nonEmpty) {
      if(cycle<minimum) {
        val maximum=perSet.values.map(_.size).max
        sameSetTSHRSum+=maximum
        if(maximum>=2) sameSetCycles+=1
      }
      maxSameSet=maxSameSet.max(perSet.values.map(_.size).max)
      maxSameSetLines=maxSameSetLines.max(perSet.values.map(_.distinct.size).max)
    }
  }
  def report(cycle:Int,finished:Boolean):Unit=if(!reported) {
    reported=true
    for((name,indices)<-groups.toSeq.sortBy(_._1)) {
      val capacity=indices.map(i=>dut.metrics(i)._3).sum
      val sum=indices.map(sums(_)).sum
      val fullCount=indices.map(full(_)).sum
      println(s"QUALITY_RESOURCE caseId=$caseId seed=$seed resource=$name samples=$sampled capacity=$capacity occupancySum=$sum peak=${aggregatePeaks(name)} resourceSlots=${indices.size} fullSlotCycles=$fullCount allFullCycles=${allFull(name)}")
      indices.foreach {i=>println(s"QUALITY_SLOT caseId=$caseId seed=$seed resource=$name name=${dut.metrics(i)._1} cap=${dut.metrics(i)._3} samples=$sampled sum=${sums(i)} peak=${peaks(i)} full=${full(i)}")}
    }
    for(name<-valuesChannelNames) println(s"QUALITY_CHANNEL caseId=$caseId seed=$seed channel=$name samples=$sampled offered=${offered(name)} blocked=${blocked(name)}")
    println(s"QUALITY_ADDRESS caseId=$caseId seed=$seed samples=$sampled uniqueLines=${unique.size} demandUniqueLines=${demandUnique.size} sliceSets=${sets.size} sets=${sets.map(_._2).size} maxSameSetTSHR=$maxSameSet maxSameSetLines=$maxSameSetLines evictions=$downstreamEvictions cdpEvictPulseCycles=$evictions sameSetCycles=$sameSetCycles maxSameSetTSHRSum=$sameSetTSHRSum")
    println(s"QUALITY_ARB caseId=$caseId seed=$seed samples=$sampled slices=$slices bothValid=$arbitrationBoth reqBlockedBySnp=$reqBypassed snpWinsWhileReq=$snoopWins snpFireWhileReq=$snoopFire")
    println(s"QUALITY_SNOOP caseId=$caseId seed=$seed samples=$sampled snoops=$snoops inflightOverlap=$overlap sameCycleLastBeat=$sameCycleLast window16Overlap=${windowOverlap.size} lastBeatDrop=$lastDrop finished=$finished cycle=$cycle")
  }
  def valuesChannelNames:Seq[String]=dut.qualitySignals.map(_._1).filter(_.endsWith(".ready")).map(_.stripSuffix(".ready")).toSeq++Seq("hn.rxdat","hn.rxrsp","hn.rxsnp","hn.snpOrder","hn.dataOrder","hn.rspOrder")
}
