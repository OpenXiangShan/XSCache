package oceanus

import scala.collection.mutable

// Host-only CHI model, shared by v2/v3 adapters. No dependency on either
// conflict harness or RTL hierarchy. IHI0050E.b is the normative issue.
case class HNExtConfig(retryPercent:Int=30, splitPercent:Int=50, dctPercent:Int=50,
  seed:Long=1L, maxGrantDelay:Int=2048, maxSplitGap:Int=512, forwardEnable:Boolean=true, directedCopyBackRetry:Boolean=false) {
  require(Set(0,10,30,50)(retryPercent))
  require(0<=splitPercent && splitPercent<=100 && 0<=dctPercent && dctPercent<=100)
  require(maxGrantDelay>=1 && maxGrantDelay<=2048 && maxSplitGap>=0 && maxSplitGap<=512)
}
case class HNExtReq(fields:Map[String,BigInt]) {
  def apply(k:String):BigInt=fields.getOrElse(k,BigInt(0))
  def rn:BigInt=apply("SrcID")
  def txn:BigInt=apply("TxnID")
  def addr:BigInt=apply("Addr")
  def op:Int=apply("Opcode").toInt
  def key:(BigInt,BigInt)=(rn,txn)
}
case class HNExtRsp(rn:BigInt,txn:BigInt,op:Int,src:BigInt=0,dbid:BigInt=0,
  resp:Int=0,credit:Int=0)
case class HNExtDat(rn:BigInt,txn:BigInt,op:Int,src:BigInt,home:BigInt,
  dbid:BigInt,resp:Int,beat:Int,addr:BigInt)
case class HNExtSnp(rn:BigInt,hn:BigInt,txn:BigInt,addr:BigInt,op:Int,
  ret:Int,dnSD:Int,fwd:BigInt,fwdTxn:BigInt)
case class HNExtTargetAck(src:BigInt,tgt:BigInt,txn:BigInt)
case class HNExtDrive(rsp:Option[HNExtRsp],dat:Option[HNExtDat],snp:Option[HNExtSnp],
  targetAcks:Seq[HNExtTargetAck])

class L2HNExtensionModel(val config:HNExtConfig, val memory:(BigInt,Int)=>BigInt,
  val trace:String=>Unit=(_:String)=>()) {
  import L2HNExtensionModel._
  private val rng=new scala.util.Random(config.seed)
  val counts=mutable.Map.empty[String,Long].withDefaultValue(0L)
  val overlapCycles=mutable.Map.empty[String,Long].withDefaultValue(0L)
  val overlapWindows=mutable.Map.empty[String,Long].withDefaultValue(0L)
  private val recent=mutable.Map.empty[BigInt,mutable.Queue[(Int,String)]]
  private val previousOverlap=mutable.Set.empty[(BigInt,String)]
  val issuedSnoops=mutable.ArrayBuffer.empty[HNExtSnp]
  val acceptedRequests=mutable.ArrayBuffer.empty[(Int,HNExtReq)]
  val completedRequests=mutable.ArrayBuffer.empty[(Int,HNExtReq)]
  val dataEvents=mutable.ArrayBuffer.empty[(Int,HNExtDat)]
  private case class Read(req:HNExtReq,home:BigInt,src:BigInt,dbid:BigInt,resp:Int,split:Boolean,
    var response:Boolean=false,var ack:Boolean=false,var beats:Set[Int]=Set.empty,
    var firstData:Int = -1,var responseCycle:Int = -1)
  private case class Write(req:HNExtReq,var grant:Boolean=false,var comp:Boolean=false,
    var beats:Set[Int]=Set.empty)
  private case class Retry(req:HNExtReq,credit:Int,var ack:Boolean=false,var invalidated:Boolean=false)
  private case class Snoop(s:HNExtSnp,var home:Boolean=false,var forwarded:Boolean=false,
    var fwdState:Int = -1,var homeBeats:Set[Int]=Set.empty,var beats:Set[Int]=Set.empty,
    var ack:Boolean=false,var ackDue:Int=Int.MaxValue,var fallback:Boolean=false)
  private case class SR(due:Int,r:HNExtRsp)
  private case class SD(due:Int,d:HNExtDat)
  private val reads=mutable.Map.empty[(BigInt,BigInt),Read]
  private val writes=mutable.Map.empty[(BigInt,BigInt),Write]
  private val retries=mutable.Map.empty[(BigInt,BigInt),Retry]
  private val snoops=mutable.Map.empty[(BigInt,BigInt),Snoop]
  private val credits=mutable.Map.empty[(BigInt,BigInt,Int),Int].withDefaultValue(0)
  private val responses=mutable.ArrayBuffer.empty[SR]
  private val data=mutable.ArrayBuffer.empty[SD]
  private val offers=mutable.Queue.empty[(BigInt,BigInt,Option[Int])]
  private var snpSeq=0
  private var retrySeq=0
  private var dbidSeq=512
  private var lastDriveCycle = -1
  def check(ok:Boolean,why: =>String):Unit=
    if(!ok) throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION "+why)
  private def event(name:String,pa:BigInt,c:Int):Unit={
    counts(name)+=1
    trace(s"HN_EXT_EVENT cycle=$c name=$name pa=0x${pa.toString(16)}")
    if(category(name).nonEmpty) {
      val q=recent.getOrElseUpdate(pa>>6,mutable.Queue.empty[(Int,String)])
      q.enqueue((c,name))
    }
  }
  // Called only after the actual source-9 slice request acceptance. This is
  // an observational event; it never offers traffic or changes HN scheduling.
  def observeCDPPrefetch(pa:BigInt,c:Int):Unit=event("cdpPrefetch",pa,c)
  private def category(n:String):Option[String]=n match {
    case "cdpPrefetch"=>Some("cdpPrefetch")
    case "retryAck"|"retryReissue"=>Some("retry")
    case "fwdSnoop"=>Some("forward")
    case "dctForwardComplete"=>Some("actualForward")
    case "requesterDCT"=>Some("requesterDCT")
    case "splitRead"=>Some("splitRead")
    case "stashSnoop"=>Some("stash")
    case "normalSnoop"=>Some("normalSnoop")
    case "copyBack"=>Some("copyBack")
    case _=>None
  }
  def recordOverlaps(c:Int):Unit={
    val now=mutable.Set.empty[(BigInt,String)]
    recent.keys.toSeq.foreach{line=>
      val q=recent(line);while(q.headOption.exists(_._1<=c-512))q.dequeue()
      if(q.isEmpty)recent.remove(line)
      else {
        val names=q.flatMap(x=>category(x._2)).toSet.toSeq.sorted
        for(n<-2 to names.size;combo<-names.combinations(n)) {
          val name=combo.mkString("+");val key=(line,name)
          overlapCycles(name)+=1;now+=key
          if(!previousOverlap(key))overlapWindows(name)+=1
        }
      }
    }
    previousOverlap.clear();previousOverlap++=now
    val active=mutable.Map.empty[BigInt,Set[String]].withDefaultValue(Set.empty)
    def tag(pa:BigInt,s:String):Unit={active(pa>>6)=active(pa>>6)+s}
    retries.values.foreach(x=>tag(x.req.addr,"retryWait"))
    reads.values.filter(x=>x.response||x.beats.nonEmpty).foreach(x=>tag(x.req.addr,if(x.split)"splitRead" else if(x.src!=x.home)"requesterDCT" else "compData"))
    snoops.values.foreach(x=>tag(x.s.addr,if(isFwd(x.s.op))"forward" else if(isStash(x.s.op))"stash" else "normalSnoop"))
    active.values.filter(_.size>=2).foreach(ns=>counts("activeSameLine:"+ns.toSeq.sorted.mkString("+"))+=1)
  }
  def canData(rn:BigInt,pa:BigInt):Boolean=
    !snoops.values.exists(x=>x.s.rn==rn&&(x.s.addr>>6)==(pa>>6))
  def canSnoop(rn:BigInt,pa:BigInt):Boolean={
    val line=pa>>6
    !reads.values.exists(x=>x.req.rn==rn&&(x.req.addr>>6)==line&&(x.response||x.beats.nonEmpty)&&
      (x.beats.size<2||(x.req("ExpCompAck")!=0 && !x.ack))) &&
    !writes.values.exists(x=>x.req.rn==rn&&(x.req.addr>>6)==line&&x.grant&&x.beats.size<2) &&
    !snoops.values.exists(x=>x.s.fwd==rn&&(x.s.addr>>6)==line&&
      x.beats.nonEmpty&&(!x.ack||x.beats.size<2))
  }
  def offerSnoop(rn:BigInt,pa:BigInt,op:Option[Int]=None):Unit=offers.enqueue((rn,pa,op))
  def outstanding:Int=reads.size+writes.size+retries.size+snoops.size
  def pendingRetries:Int=retries.size
  def pendingSnoops:Int=snoops.size
  def creditBalance:Int=credits.values.sum
  def empty:Boolean=outstanding==0&&responses.isEmpty&&data.isEmpty&&offers.isEmpty&&creditBalance==0
  def pendingSummary:String=s"reads=${reads.keys.toSeq} writes=${writes.keys.toSeq} retries=${retries.keys.toSeq} snps=${snoops.keys.toSeq} rsp=${responses.size} dat=${data.size} offers=${offers.size} credit=$creditBalance"
  def activeReadRequests:Seq[HNExtReq]=reads.values.map(_.req).toSeq
  def upstreamData(pa:BigInt,beat:Int,c:Int):Unit={
    val pending=reads.values.filter(x=>(x.req.addr>>6)==(pa>>6)).toSeq
    check(!pending.exists(x=> !x.beats(beat*2)),s"upstream beat before corresponding refill data cycle=$c pa=$pa beat=$beat")
    if(pending.exists(_.beats.size<2))counts("upstreamStreamingBeforeAllData")+=1
    counts("upstreamDataBeatsChecked")+=1
  }
  private def allocateDBID(home:BigInt,rn:BigInt):BigInt={
    var tries=0
    while(reads.values.exists(r=>r.home==home&&r.req.rn==rn&&r.dbid==dbidSeq)) {
      dbidSeq=512+(dbidSeq-511)%3000;tries+=1;check(tries<3000,"DBID space exhausted")
    }
    val id=BigInt(dbidSeq);dbidSeq=512+(dbidSeq-511)%3000;id
  }
  def request(req:HNExtReq,c:Int):Unit={
    counts("requestFlits")+=1
    if(req.op==5) { // PCrdReturn; cancelled CopyBack retry (§2.6.6, §2.11.1)
      val t=req("PCrdType").toInt;val key=(req("TgtID"),req.rn,t)
      check(req("AllowRetry")==0&&credits(key)>0,s"credit return without credit cycle=$c req=$req")
      if(req.txn!=0) {
        counts("dutProtocolErrors")+=1
        trace(s"HN_EXT_DUT_PROTOCOL_ERROR cycle=$c channel=TXREQ rule=2.6.6 PCrdReturnTxnID=${req.txn} expected=0")
      }
      // PCrdReturn carries no transaction identity. Credits are fungible within
      // (Completer, Requester, PCrdType); never associate it by TxnID zero.
      val pending=retries.values.find(r=>r.req.rn==req.rn&&r.credit==t&&r.ack&&
        r.req("TgtID")==req("TgtID")&&isWrite(r.req.op)&&r.invalidated)
      check(pending.nonEmpty,s"unmatched/non-invalidated CopyBack PCrdReturn $req (§2.11)")
      credits(key)-=1;retries.remove(pending.get.req.key)
      event("creditReturn",pending.get.req.addr,c);completedRequests+=((c,pending.get.req));return
    }
    check(!reads.contains(req.key)&& !writes.contains(req.key),s"request identity reused cycle=$c req=$req")
    if(req("AllowRetry")==0) {
      val variableFields=Seq("QoS","TgtID","TxnID",
        "SLCRepHint","AllowRetry","PCrdType","TraceTag","RSVDC")
      def invariant(q:HNExtReq)=q.fields -- variableFields
      val retry=retries.get(req.key).filter(r=>r.credit==req("PCrdType")&&invariant(r.req)==invariant(req))
        .orElse(retries.values.find(r=>r.req.rn==req.rn&&r.credit==req("PCrdType")&&invariant(r.req)==invariant(req)))
        .getOrElse(throw new IllegalStateException(s"HN_CHI_EXTENSION_VIOLATION reissue without matching RetryAck cycle=$c req=$req"))
      val ck=(BigInt(0),req.rn,retry.credit)
      check(retry.ack&&req("PCrdType")==retry.credit&&credits(ck)>0,s"reissue credit mismatch cycle=$c req=$req")
      val a=invariant(retry.req)
      val b=invariant(req)
      check(a==b,s"reissue changed fields cycle=$c changed=${(a.keySet++b.keySet).filter(k=>a.get(k)!=b.get(k))}")
      credits(ck)-=1;retries.remove(retry.req.key);event("retryReissue",req.addr,c)
      accept(req,c)
    } else {
      check(req("PCrdType")==0,s"initial PCrdType nonzero cycle=$c")
      check(!retries.contains(req.key),s"retry identity reused cycle=$c")
      if(rng.nextInt(100)<config.retryPercent||(config.directedCopyBackRetry&&isWrite(req.op))) {
        val directed=config.directedCopyBackRetry&&isWrite(req.op)
        val t=if(directed)1 else retrySeq%16;retrySeq+=1
        val grantDelay=if(directed)config.maxGrantDelay else if(retrySeq%4==1) 1 else if(retrySeq%4==2) config.maxGrantDelay else 1+rng.nextInt(config.maxGrantDelay)
        val early= !directed&&retrySeq%2==1
        val ackDelay=if(early) grantDelay+1+rng.nextInt(32) else 1
        val actualGrant=if(early)grantDelay else math.max(2,grantDelay)
        retries(req.key)=Retry(req,t)
        responses+=SR(c+ackDelay,HNExtRsp(req.rn,req.txn,3,credit=t))
        responses+=SR(c+actualGrant,HNExtRsp(req.rn,0,7,credit=t))
        counts("grantDelayMin")=if(counts("grantDelayMin")==0)actualGrant else counts("grantDelayMin").min(actualGrant)
        counts("grantDelayMax")=counts("grantDelayMax").max(actualGrant)
        counts("retryPCrdType:"+t)+=1
        counts("maxWaitingRetries")=counts("maxWaitingRetries").max(retries.size)
        counts("maxWaitingTypes")=counts("maxWaitingTypes").max(retries.values.map(_.credit).toSet.size)
        event("retrySelected",req.addr,c)
      } else accept(req,c)
    }
  }
  private def accept(req:HNExtReq,c:Int):Unit={
    acceptedRequests+=((c,req));counts("accepted")+=1
    val due=c+12+rng.nextInt(33)
    if(isWrite(req.op)) {
      writes(req.key)=Write(req)
      responses+=SR(due,HNExtRsp(req.rn,req.txn,if(req.op==13)4 else 5,dbid=req.txn))
    } else {
      val split=rng.nextInt(100)<config.splitPercent
      val dct= !split&&rng.nextInt(100)<config.dctPercent
      val home=BigInt(0) // One fixed Home; only the DCT data source differs.
      val src=if(dct)BigInt(0x31) else home
      val resp=if(Set(7,0x4c)(req.op))2 else 1
      val dbid=allocateDBID(home,req.rn)
      reads(req.key)=Read(req,home,src,dbid,resp,split)
      if(split) {
        val n=counts("splitScheduled").toInt;counts("splitScheduled")+=1
        val gap=if(n%4==0)0 else if(n%4==1)config.maxSplitGap else rng.nextInt(config.maxSplitGap+1)
        val responseFirst=n%2==0
        val rspDue=due+(if(responseFirst)0 else gap)
        val datDue=due+(if(responseFirst)gap else 0)
        responses+=SR(rspDue,HNExtRsp(req.rn,req.txn,11,home,dbid,resp))
        for(beat<-Seq(0,2))data+=SD(datDue+(if(beat==0)0 else 1+rng.nextInt(17)),HNExtDat(req.rn,req.txn,11,src,home,dbid,resp,beat,req.addr))
        counts("splitGapMin")=if(n==0)gap else counts("splitGapMin").min(gap)
        counts("splitGapMax")=counts("splitGapMax").max(gap)
      } else {
        for(beat<-Seq(0,2))data+=SD(due+(if(beat==0)0 else 1+rng.nextInt(17)),HNExtDat(req.rn,req.txn,4,src,home,dbid,resp,beat,req.addr))
      }
    }
  }
  // Drive only chooses messages. Actual observed flits must be reported below.
  def drive(c:Int,rspReady:Boolean,datReady:Boolean,snpReady:Boolean,
    paused:BigInt=>Boolean=(_:BigInt)=>false):HNExtDrive={
    check(c>lastDriveCycle,s"drive repeated cycle=$c");lastDriveCycle=c
    // A failed forwarding attempt still belongs to an original virtual Read.
    // Complete that Read from memory instead of discarding its requester.
    snoops.values.filter(x=>isFwd(x.s.op)&&x.home&& !x.forwarded&& !x.fallback).foreach{x=>
      x.fallback=true;x.beats=Set(0);x.ackDue=c+2
      trace(s"HN_EXT_VIRTUAL_FALLBACK cycle=$c SrcID=${x.s.hn} TgtID=${x.s.fwd} TxnID=${x.s.fwdTxn} HomeNID=${x.s.hn} DBID=${x.s.txn} DataID=0 Resp=${expectedFwd(x.s.op)} pa=0x${x.s.addr.toString(16)}")
    }
    snoops.values.filter(x=>x.fallback&&x.beats==Set(0)&&x.ackDue<=c+1).foreach{x=>
      x.beats+=2
      trace(s"HN_EXT_VIRTUAL_FALLBACK cycle=$c SrcID=${x.s.hn} TgtID=${x.s.fwd} TxnID=${x.s.fwdTxn} HomeNID=${x.s.hn} DBID=${x.s.txn} DataID=2 Resp=${expectedFwd(x.s.op)} pa=0x${x.s.addr.toString(16)}")
      counts("fallbackReadCompleted")+=1
    }
    val acks=snoops.values.filter(x=>(x.forwarded||x.fallback)&&x.beats.size==2&& !x.ack&&x.ackDue<=c)
      .map(x=>HNExtTargetAck(x.s.fwd,x.s.hn,x.s.txn)).toSeq
    acks.foreach(a=>targetAck(a,c))
    def rspLegal(r:HNExtRsp)=r.op==3||r.op==7||{
      val pa=reads.get((r.rn,r.txn)).map(_.req.addr).orElse(writes.get((r.rn,r.txn)).map(_.req.addr))
      check(pa.nonEmpty,s"response without request $r");canData(r.rn,pa.get)
    }
    if(responses.exists(x=>x.due<=c&& !rspLegal(x.r)))counts("responseOrderStallCycles")+=1
    val ri=if(rspReady)responses.indexWhere(x=>x.due<=c&&rspLegal(x.r)) else -1
    val rsp=if(ri>=0)Some(responses.remove(ri).r) else None
    if(data.exists(x=>x.due<=c&& !canData(x.d.rn,x.d.addr)))counts("dataOrderStallCycles")+=1
    val di=if(datReady&&rng.nextInt(5)!=0)data.indexWhere(x=>x.due<=c&& !paused(x.d.addr)&&canData(x.d.rn,x.d.addr)) else -1
    val dat=if(di>=0)Some(data.remove(di).d) else None
    val snp=offers.headOption.flatMap{case(rn,pa,requested)=>
      val sameNow=dat.exists(x=>x.rn==rn&&(x.addr>>6)==(pa>>6)) ||
        rsp.exists(x=>x.rn==rn&&Set(4,5,11)(x.op)&&reads.get((x.rn,x.txn)).map(_.req.addr>>6).orElse(writes.get((x.rn,x.txn)).map(_.req.addr>>6)).contains(pa>>6))
      if(!canSnoop(rn,pa)||sameNow){counts("snoopOrderStallCycles")+=1;None}
      else if(!snpReady)None
      else {
        check(snpSeq<4000,"snoop TxnID range exhausted")
        val sequence=snpSeq;snpSeq+=1
        val selectedOp=requested.getOrElse(if(sequence%10<6)fwdOps((sequence%10+sequence/10)%fwdOps.size)
          else if(sequence%10<8)stashOps((sequence/10+sequence)%stashOps.size)
          else normalOps((sequence/10+sequence)%normalOps.size))
        // Disabling DCT also replaces forwarding snoops with their non-forwarding
        // equivalents. Preserve the sequence and address selection for ablation.
        val op=if(config.forwardEnable)selectedOp else Map(0x11->1,0x12->2,0x13->3,
          0x14->4,0x16->0x15,0x17->7).getOrElse(selectedOp,selectedOp)
        val ret=if(Set(1,2,3,4,0x11,0x12,0x14)(op))sequence%2 else 0
        val sd=if(op==0x10)0 else if(Set(1,2,3,4,0x11,0x12,0x13,0x14,0x15,0x16)(op))sequence%2 else 1
        offers.dequeue();Some(HNExtSnp(rn,0,BigInt(snpSeq),pa,op,ret,sd,
          if(isFwd(op))BigInt(0x100+sequence%4) else BigInt(0),if(isFwd(op))BigInt(0x800+sequence%1024) else BigInt(0)))
      }
    }
    HNExtDrive(rsp,dat,snp,acks)
  }
  def rxRsp(r:HNExtRsp,c:Int):Unit=r.op match {
    case 3 =>
      val t=retries.getOrElse((r.rn,r.txn),throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION unknown RetryAck"))
      check(!t.ack&&t.credit==r.credit&&r.src==0,"duplicate/mismatched RetryAck")
      t.ack=true;event("retryAck",t.req.addr,c)
      if(credits((r.src,r.rn,r.credit))>0)counts("grantBeforeRetryAck")+=1
    case 7 =>
      check(r.txn==0&&r.dbid==0&&r.src==0,"PCrdGrant identifiers (§2.6.5)")
      val key=(r.src,r.rn,r.credit);credits(key)+=1;counts("creditGranted")+=1
    case 11 =>
      val x=reads.getOrElse((r.rn,r.txn),throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION unknown RespSepData"))
      check(x.split&& !x.response&&r.src==x.home&&r.dbid==x.dbid&&r.resp==x.resp,"RespSepData identity/duplicate")
      check(canData(r.rn,x.req.addr),"RespSepData while SnpResp pending (§4.11.2)")
      x.response=true;x.responseCycle=c
      event("splitRead",x.req.addr,c);counts(if(x.beats.isEmpty)"splitResponseFirst" else "splitDataFirst")+=1
      retireRead((r.rn,r.txn),c)
    case 4|5 =>
      val x=writes.getOrElse((r.rn,r.txn),throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION unknown write completion"))
      check(canData(r.rn,x.req.addr),"CopyBack completion while SnpResp pending (§4.11.2)")
      check(!x.comp&&r.dbid==x.req.txn&&r.src==0,"write completion mismatch")
      x.comp=true
      if(r.op==5){x.grant=true;event("copyBack",x.req.addr,c)}
      else {check(x.req.op==13,"Comp without CopyBack DBID unsupported");writes.remove(x.req.key);completedRequests+=((c,x.req))}
    case _=>check(false,s"unsupported rxrsp $r")
  }
  def rxDat(d:HNExtDat,c:Int):Unit={
    val x=reads.getOrElse((d.rn,d.txn),throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION unmatched RXDAT"))
    check(canData(d.rn,d.addr),"RXDAT while SnpResp pending (§4.11.2)")
    check(d.home==x.home&&d.src==x.src&&d.dbid==x.dbid&&d.resp==x.resp,"RXDAT routing/response mismatch")
    check(d.op==(if(x.split)11 else 4)&&Set(0,2)(d.beat)&& !x.beats(d.beat),"RXDAT opcode/beat/duplicate")
    if(x.firstData<0){x.firstData=c;if(d.src!=d.home)event("requesterDCT",d.addr,c)}
    x.beats+=d.beat
    if(!x.split)x.response=true
    if(x.beats.size==2&&c-x.firstData>1)counts("dataBeatGap")+=1
    dataEvents+=((c,d));retireRead((d.rn,d.txn),c)
  }
  private def retireRead(key:(BigInt,BigInt),c:Int):Unit={
    reads.get(key).foreach{x=>if(x.beats.size==2&&x.response&&(x.req("ExpCompAck")==0||x.ack)){
      completedRequests+=((c,x.req));reads.remove(key);counts("readCompleted")+=1
    }}
  }
  def compAck(rn:BigInt,home:BigInt,dbid:BigInt,c:Int):Unit={
    val matches=reads.toSeq.filter{case(_,x)=>x.req.rn==rn&&x.home==home&&x.dbid==dbid}
    check(matches.size==1,s"CompAck routing/identity cycle=$c rn=$rn home=$home dbid=$dbid (§2.6.1/§2.8.3)")
    val(key,x)=matches.head
    check(x.req("ExpCompAck")!=0&&x.response&& !x.ack,"early/duplicate CompAck (§2.8.3)")
    if(x.split&&x.beats.size<2)counts("ackBeforeAllSplitData")+=1
    if(x.src!=x.home)counts("dctAckHomeChecked")+=1
    x.ack=true;counts("compAck")+=1;retireRead(key,c)
  }
  def rxSnp(s:HNExtSnp,c:Int):Unit={
    check(canSnoop(s.rn,s.addr),"SNP during completion/targetAck/CopyBack window (§4.11.2)")
    check(!snoops.contains((s.hn,s.txn)),"snoop TxnID reused")
    check((fwdOps++stashOps++normalOps).contains(s.op),"unsupported SNP opcode")
    check(Set(0,1)(s.ret)&&Set(0,1)(s.dnSD),"SNP modifier encoding")
    check(!Set(5,6,8,9,10,11,12,0x10,0x13,0x17)(s.op)||s.ret==0,
      "RetToSrc must be zero (§4.9)")
    check(!Set(5,6,7,8,9,10,11,12,0x17)(s.op)||s.dnSD==1,
      "DoNotGoToSD must be one (§13.10.34)")
    check(s.op!=0x10||s.dnSD==0,"SnpQuery DoNotGoToSD must be zero (§13.10.34)")
    check(isFwd(s.op)||(s.fwd==0&&s.fwdTxn==0),"non-Fwd SNP forwarding fields must be zero (§2.5.7–8)")
    if(isFwd(s.op)) {
      check(!snoops.values.exists(x=>isFwd(x.s.op)&&x.s.fwd==s.fwd&&x.s.fwdTxn==s.fwdTxn),
        "virtual requester TxnID reused (§2.5.2)")
      trace(s"HN_EXT_VIRTUAL_REQUEST cycle=$c SrcID=${s.fwd} TgtID=${s.hn} TxnID=${s.fwdTxn} Opcode=${Map(0x11->1,0x12->2,0x13->3,0x14->0x26,0x16->0x4c,0x17->7)(s.op)} ExpCompAck=1 pa=0x${s.addr.toString(16)}")
    }
    snoops((s.hn,s.txn))=Snoop(s);issuedSnoops+=s
    counts("snoopTotal")+=1;counts("snpOpcode:"+s.op)+=1
    if(retries.values.exists(x=>x.req.rn==s.rn&&(x.req.addr>>6)==(s.addr>>6)))counts("snoopDuringRetrySameLine")+=1
    if(retries.values.exists(x=>x.ack&&x.req.rn==s.rn&&(x.req.addr>>6)==(s.addr>>6)))counts("snoopAfterRetryAckSameLine")+=1
    event(if(isFwd(s.op))"fwdSnoop" else if(isStash(s.op))"stashSnoop" else "normalSnoop",s.addr,c)
  }
  private def expectedFwd(op:Int):Int=if(op==0x13)0 else if(Set(0x16,0x17)(op))2 else 1
  def snoopResponse(rn:BigInt,home:BigInt,txn:BigInt,op:Int,resp:Int,fwdState:Int,
    beat:Option[Int],payload:Option[BigInt],c:Int):Unit={
    val key=(home,txn);val x=snoops.getOrElse(key,throw new IllegalStateException(s"HN_CHI_EXTENSION_VIOLATION unmatched SnpResp $key"))
    check(x.s.rn==rn&& !x.home,"duplicate/wrong RN snoop response")
    val forwarded=op==9||op==6
    check(if(beat.nonEmpty)Set(1,6)(op) else Set(1,9)(op),s"unexpected snoop response opcode=$op")
    check(!forwarded||isFwd(x.s.op),"forwarded response for non-Fwd SNP")
    if(forwarded)check(fwdState==expectedFwd(x.s.op),"SnpRespFwded FwdState mismatch")
    else check(fwdState==0,"non-Fwd response FwdState nonzero")
    check(x.beats.isEmpty||forwarded,"non-forwarded Home response after DCT data")
    check(x.fwdState<0||x.fwdState==fwdState,"FwdState disagrees across Home/data channels")
    if(isStash(x.s.op))check(!forwarded,"Stash cannot forward")
    check(Set(0,1,2,3,4,5,6)(resp),"invalid snoop Resp")
    x.forwarded=forwarded;x.fwdState=fwdState
    beat match {
      case Some(b)=>
        check(Set(0,2)(b)&& !x.homeBeats(b),"invalid home snoop data beat")
        check(payload.contains(memory(x.s.addr,b/2)),"home snoop data mismatch")
        x.homeBeats+=b;x.home=x.homeBeats.size==2
      case None=>x.home=true
    }
    if(x.home){
      if((resp&3)==0)retries.values.filter(r=>r.req.rn==rn&&(r.req.addr>>6)==(x.s.addr>>6)&&isWrite(r.req.op)).foreach(_.invalidated=true)
      counts(if(forwarded)"homeFwdResponse" else "homeSnoopResponse")+=1;retireSnoop(key,c)
    }
  }
  def forwardedData(rn:BigInt,target:BigInt,txn:BigInt,home:BigInt,dbid:BigInt,
    resp:Int,beat:Int,payload:BigInt,c:Int):Unit={
    val key=(home,dbid);val x=snoops.getOrElse(key,throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION DCT unknown DBID"))
    check(isFwd(x.s.op)&&x.s.rn==rn&&x.s.fwd==target&&x.s.fwdTxn==txn,"DCT TgtID/TxnID/HomeNID/DBID mismatch (§2.6.1)")
    check(!x.home||x.forwarded,"DCT after non-forwarded Home response")
    check((x.fwdState<0||resp==x.fwdState)&&resp==expectedFwd(x.s.op),"DCT Resp/FwdState mismatch")
    check(Set(0,2)(beat)&& !x.beats(beat),"duplicate/invalid DCT beat")
    check(payload==memory(x.s.addr,beat/2),"DCT forwarded data mismatch")
    x.forwarded=true;x.fwdState=resp
    x.beats+=beat;counts("forwardDataBeats")+=1
    check(!canSnoop(target,x.s.addr),"target completion guard missing (§4.11.2)")
    counts("targetSnoopBlocked")+=1 // actual virtual HN offer to the target
    if(x.beats.size==2){x.ackDue=c+1+rng.nextInt(64);event("dctForwardComplete",x.s.addr,c)}
  }
  def targetAck(a:HNExtTargetAck,c:Int):Unit={
    val key=(a.tgt,a.txn);val x=snoops.getOrElse(key,throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION unmatched target CompAck"))
    check(x.s.fwd==a.src&&(x.forwarded||x.fallback)&&x.beats.size==2&& !x.ack,"target CompAck before all DCT data/duplicate (§2.8.3)")
    x.ack=true;counts(if(x.forwarded)"targetCompAck" else "fallbackCompAck")+=1;trace(s"HN_EXT_TARGET_COMP_ACK cycle=$c SrcID=${a.src} TgtID=${a.tgt} TxnID=${a.txn}")
    retireSnoop(key,c)
  }
  private def retireSnoop(key:(BigInt,BigInt),c:Int):Unit={
    val x=snoops(key)
    if(x.home&&(!isFwd(x.s.op)||(x.beats.size==2&&x.ack))) {
      snoops.remove(key);counts("snoopCompleted")+=1
      if(x.forwarded)counts("forwardTransactionCompleted")+=1
    } else if(x.home&&x.forwarded)counts("homeResponseAwaitTargetAck")+=1
  }
  def copyBackData(rn:BigInt,home:BigInt,dbid:BigInt,resp:Int,beat:Int,payload:BigInt,c:Int):Unit={
    val x=writes.getOrElse((rn,dbid),throw new IllegalStateException("HN_CHI_EXTENSION_VIOLATION unmatched CopyBack data"))
    check(x.grant&&x.comp&&home==0&&Set(0,2)(beat)&& !x.beats(beat),"write data before DBID/duplicate")
    if(resp!=0)check(payload==memory(x.req.addr,beat/2),"CopyBack data mismatch")
    x.beats+=beat;counts("copyBackBeats")+=1
    if(x.beats.size==2){writes.remove(x.req.key);completedRequests+=((c,x.req));counts("copyBackCompleted")+=1}
  }
  def finish():Unit={
    check(empty,s"model did not drain: $pendingSummary")
    check(counts("retrySelected")==counts("retryReissue")+counts("creditReturn"),"retried requests not completed/returned")
    check(counts("creditGranted")==counts("retryReissue")+counts("creditReturn"),"credit lost/reused")
    check(counts("accepted")==completedRequests.size-counts("creditReturn"),"accepted request closure")
    check(counts("snoopTotal")==counts("snoopCompleted"),"snoop closure")
    check(counts("targetCompAck")==counts("forwardTransactionCompleted"),"forward completion/target CompAck closure")
    check(counts("fallbackReadCompleted")==counts("fallbackCompAck"),"fallback Read/CompAck closure")
    if(counts("dutProtocolErrors")!=0)throw new IllegalStateException(
      "L2_CHI_PROTOCOL_VIOLATION DUT protocol field violation; see HN_EXT_DUT_PROTOCOL_ERROR")
  }
}
object L2HNExtensionModel {
  val fwdOps=Seq(0x11,0x12,0x13,0x14,0x17,0x16)
  val stashOps=Seq(5,6,11,12)
  val normalOps=Seq(1,2,3,4,7,0x15,8,9,10,0x10)
  def isFwd(op:Int):Boolean=fwdOps.contains(op)
  def isStash(op:Int):Boolean=stashOps.contains(op)
  def isWrite(op:Int):Boolean=Set(13,0x1b,0x15,0x42)(op)
}
