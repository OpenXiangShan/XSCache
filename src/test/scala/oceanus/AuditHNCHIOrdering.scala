package oceanus

import scala.collection.mutable

// Exact pf-v2 ordering checker; field checks are sampled by L2TopDownstreamModel.
class AuditHNCHIOrdering {
  private case class Read(line:BigInt,expAck:Boolean,var started:Boolean=false,
    var acked:Boolean=false,var beats:Set[Int]=Set.empty)
  private val reads=mutable.Map.empty[(BigInt,BigInt),Read]
  private val snoops=mutable.Map.empty[(BigInt,BigInt),(BigInt,BigInt)]
  private val evictions=mutable.Map.empty[(BigInt,BigInt),BigInt]
  private case class CopyBack(line:BigInt,var beats:Set[Int]=Set.empty)
  private val copybacks=mutable.Map.empty[(BigInt,BigInt),CopyBack]
  private def line(pa:BigInt)=pa>>6
  private def check(ok:Boolean,detail: =>String):Unit=
    if(!ok) throw new IllegalStateException("HN_CHI_ORDER_VIOLATION "+detail)
  def canData(rn:BigInt,pa:BigInt):Boolean= !snoops.values.exists(_==((rn,line(pa))))
  def canSnoop(rn:BigInt,pa:BigInt):Boolean=
    !reads.exists{case((src,_),r)=>src==rn&&r.line==line(pa)&&r.started&&
      (r.beats.size<2||(r.expAck&& !r.acked))} &&
    !copybacks.exists{case((src,_),w)=>src==rn&&w.line==line(pa)}
  def request(rn:BigInt,txn:BigInt,pa:BigInt,expAck:Boolean):Unit={
    check(!reads.contains((rn,txn)),s"read identity reused rn=$rn txn=$txn")
    reads((rn,txn))=Read(line(pa),expAck)
  }
  def eviction(rn:BigInt,txn:BigInt,pa:BigInt):Unit={
    check(!evictions.contains((rn,txn)),s"eviction identity reused rn=$rn txn=$txn")
    evictions((rn,txn))=line(pa)
  }
  def completion(rn:BigInt,txn:BigInt,dbid:BigInt,opcode:Int,cycle:Int):Unit={
    val key=(rn,txn)
    check(evictions.contains(key),s"unmatched eviction completion cycle=$cycle rn=$rn txn=$txn")
    val pa=evictions(key)<<6
    check(canData(rn,pa),s"eviction completion while SnpResp pending cycle=$cycle rn=$rn txn=$txn line=0x${pa.toString(16)}")
    check(Set(4,5)(opcode),s"unexpected eviction response opcode=$opcode")
    if(opcode==5) {
      check(!copybacks.contains((rn,dbid)),s"CopyBack DBID reused rn=$rn dbid=$dbid")
      copybacks((rn,dbid))=CopyBack(line(pa))
    }
    evictions.remove(key)
  }
  def writeData(rn:BigInt,dbid:BigInt,beat:Int,cycle:Int):Unit={
    val key=(rn,dbid)
    check(copybacks.contains(key),s"unmatched CopyBackWrData cycle=$cycle rn=$rn dbid=$dbid")
    val w=copybacks(key)
    check(Set(0,2)(beat)&& !w.beats(beat),s"invalid CopyBackWrData beat cycle=$cycle rn=$rn dbid=$dbid beat=$beat")
    w.beats+=beat
    if(w.beats.size==2) copybacks.remove(key)
  }
  private def retire(key:(BigInt,BigInt)):Unit={
    val r=reads(key)
    if(r.beats.size==2&&(!r.expAck||r.acked)) reads.remove(key)
  }
  def data(rn:BigInt,txn:BigInt,beat:Int,cycle:Int):Unit={
    val key=(rn,txn)
    check(reads.contains(key),s"unmatched CompData cycle=$cycle rn=$rn txn=$txn")
    val r=reads(key)
    check(canData(rn,r.line<<6),s"CompData while SnpResp pending cycle=$cycle rn=$rn txn=$txn line=0x${(r.line<<6).toString(16)}")
    check(Set(0,2)(beat)&& !r.beats(beat),s"invalid CompData beat cycle=$cycle rn=$rn txn=$txn beat=$beat")
    r.started=true;r.beats+=beat;retire(key)
  }
  def snoop(rn:BigInt,hn:BigInt,txn:BigInt,pa:BigInt,cycle:Int):Unit={
    check(canSnoop(rn,pa),s"SNP before CompAck/WriteData cycle=$cycle rn=$rn txn=$txn line=0x${(line(pa)<<6).toString(16)}")
    check(!snoops.contains((hn,txn)),s"snoop identity reused hn=$hn txn=$txn")
    snoops((hn,txn))=(rn,line(pa))
  }
  def response(rn:BigInt,hn:BigInt,txn:BigInt,opcode:Int,cycle:Int):Unit=opcode match {
    case 1 =>
      check(snoops.get((hn,txn)).exists(_._1==rn),s"unmatched SnpResp cycle=$cycle rn=$rn hn=$hn txn=$txn")
      snoops.remove((hn,txn))
    case 2 =>
      val key=(rn,txn)
      check(reads.get(key).exists(r=>r.expAck&&r.started&& !r.acked),s"unmatched CompAck cycle=$cycle rn=$rn txn=$txn")
      reads(key).acked=true;retire(key)
    case _ => ()
  }
  def empty:Boolean=reads.isEmpty&&snoops.isEmpty&&evictions.isEmpty&&copybacks.isEmpty
}

