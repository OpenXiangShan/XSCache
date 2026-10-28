package oceanus

import scala.collection.mutable

// Host checker negative tests. They do not count as RTL functional coverage.
object TestHNExtensionModel extends App {
  val rn=BigInt(8);val pa=BigInt(0x220040)
  def q(id:Int=0,a:BigInt=pa)=HNExtReq(Map("SrcID"->rn,"TgtID"->BigInt(0),"TxnID"->BigInt(id),
    "Addr"->a,"Opcode"->BigInt(1),"Size"->BigInt(6),"ExpCompAck"->BigInt(1),
    "AllowRetry"->BigInt(1),"PCrdType"->BigInt(0),"MemAttr"->BigInt(15)))
  // Same line (including byte offsets), inclusive 512-cycle boundary,
  // and no accidental overlap with an adjacent cacheline.
  val windowModel=new L2HNExtensionModel(HNExtConfig(0,0,0),OvernightMemory.beat _)
  windowModel.observeCDPPrefetch(pa+7,0)
  windowModel.rxSnp(HNExtSnp(rn,0,0x91,pa,7,0,1,0,0),511)
  windowModel.recordOverlaps(511)
  assert(windowModel.overlapWindows("cdpPrefetch+normalSnoop")==1)
  windowModel.recordOverlaps(512)
  assert(windowModel.overlapCycles("cdpPrefetch+normalSnoop")==1)
  windowModel.observeCDPPrefetch(pa+64,512)
  windowModel.recordOverlaps(512)
  assert(windowModel.overlapWindows("cdpPrefetch+normalSnoop")==1)
  windowModel.observeCDPPrefetch(pa,513)
  windowModel.recordOverlaps(513)
  assert(windowModel.overlapWindows("cdpPrefetch+normalSnoop")==2)
  windowModel.snoopResponse(rn,0,0x91,1,0,0,None,None,514)
  windowModel.finish()
  println("HN_EXT_UNIT_CDP_WINDOW pass=true boundary=512 adjacentLineExcluded=true")
  var negatives=0
  def rejects(body: =>Unit):Unit={
    var rejected=false
    try body catch {case e:IllegalStateException=>
      assert(e.getMessage.startsWith("HN_CHI_EXTENSION_VIOLATION"));rejected=true}
    assert(rejected,"negative failed to stop");negatives+=1
  }
  def mk(split:Int=0,dct:Int=0,retry:Int=0)=new L2HNExtensionModel(HNExtConfig(retry,split,dct,seed=37),OvernightMemory.beat _)
  // RespSepData opens the Ack window before any data. All data must still
  // arrive, even after an early Ack; data-first must not justify an Ack.
  val sep=mk(split=100);sep.request(q(),0)
  rejects(sep.compAck(rn,0,512,1))
  var ack=false
  for(c<-1 to 1500){
    val d=sep.drive(c,true,true,true)
    d.rsp.foreach{r=>sep.rxRsp(r,c);sep.compAck(rn,r.src,r.dbid,c+1);ack=true
      rejects(sep.compAck(rn,r.src,r.dbid,c+2))}
    d.dat.foreach(sep.rxDat(_,c))
  }
  assert(ack);sep.finish()
  val dataFirst=mk(split=100)
  // The second split transaction schedules data before response.
  dataFirst.request(q(),0);dataFirst.request(q(1,pa+64),0)
  var denied=false
  for(c<-1 to 1800){val d=dataFirst.drive(c,true,true,true)
    d.dat.foreach{x=>dataFirst.rxDat(x,c)
      if(x.txn==1&& !denied){rejects(dataFirst.compAck(rn,x.home,x.dbid,c));denied=true}}
    d.rsp.foreach{r=>dataFirst.rxRsp(r,c);dataFirst.compAck(rn,r.src,r.dbid,c+1)}
  }
  assert(denied);dataFirst.finish()
  // DCT requester: SrcID differs from HomeNID. Wrong destination/DBID rejected.
  val dct=mk(dct=100);dct.request(q(),0);var dctAck=false
  for(c<-1 to 300){val d=dct.drive(c,true,true,true);d.dat.foreach{x=>
    dct.rxDat(x,c)
    if(!dctAck){assert(x.src!=x.home);rejects(dct.compAck(rn,x.src,x.dbid,c))
      rejects(dct.compAck(rn,x.home,x.dbid+1,c));dct.compAck(rn,x.home,x.dbid,c);dctAck=true}
  }}
  dct.finish()
  // Forward target ordering and two-part Home completion.
  val fwd=mk();val s=HNExtSnp(rn,0,7,pa,0x11,0,1,0x101,0x801)
  fwd.rxSnp(s,0)
  rejects(fwd.targetAck(HNExtTargetAck(s.fwd,s.hn,s.txn),1))
  rejects(fwd.snoopResponse(rn,0,7,9,1,2,None,None,1))
  fwd.snoopResponse(rn,0,7,9,1,1,None,None,2)
  assert(!fwd.empty&&fwd.pendingSnoops==1)
  rejects(fwd.forwardedData(rn,s.fwd+1,s.fwdTxn,0,7,1,0,OvernightMemory.beat(pa,0),3))
  rejects(fwd.forwardedData(rn,s.fwd,s.fwdTxn,0,7,1,0,BigInt(0),3))
  fwd.forwardedData(rn,s.fwd,s.fwdTxn,0,7,1,0,OvernightMemory.beat(pa,0),4)
  rejects(fwd.rxSnp(s.copy(rn=s.fwd,txn=8),5))
  rejects(fwd.forwardedData(rn,s.fwd,s.fwdTxn,0,7,1,0,OvernightMemory.beat(pa,0),5))
  rejects(fwd.targetAck(HNExtTargetAck(s.fwd,0,7),5))
  fwd.forwardedData(rn,s.fwd,s.fwdTxn,0,7,1,2,OvernightMemory.beat(pa,1),6)
  assert(fwd.pendingSnoops==1)
  fwd.targetAck(HNExtTargetAck(s.fwd,0,7),7);fwd.finish()
  rejects(fwd.targetAck(HNExtTargetAck(s.fwd,0,7),8))
  // A completed two-beat transfer counts as actual DCT; an invalid-line
  // forwarding attempt alone must not create an actualForward overlap.
  fwd.rxSnp(s.copy(op=7,txn=8,fwd=0,fwdTxn=0),8)
  fwd.recordOverlaps(8)
  assert(fwd.overlapWindows("actualForward+normalSnoop")==1)
  fwd.recordOverlaps(517)
  assert(fwd.overlapCycles("actualForward+normalSnoop")==2)
  fwd.recordOverlaps(518) // event at cycle 6 is outside [7,518]
  assert(fwd.overlapCycles("actualForward+normalSnoop")==2)

  val dataBeforeHome=mk();dataBeforeHome.rxSnp(s,0)
  dataBeforeHome.forwardedData(rn,s.fwd,s.fwdTxn,0,7,1,0,OvernightMemory.beat(pa,0),1)
  dataBeforeHome.forwardedData(rn,s.fwd,s.fwdTxn,0,7,1,2,OvernightMemory.beat(pa,1),2)
  dataBeforeHome.targetAck(HNExtTargetAck(s.fwd,0,7),3)
  assert(dataBeforeHome.pendingSnoops==1)
  dataBeforeHome.snoopResponse(rn,0,7,9,1,1,None,None,4);dataBeforeHome.finish()
  // Pending SNP forbids completion; the scheduler must retain blocked data.
  val blocked=mk();blocked.request(q(),0);blocked.rxSnp(s.copy(op=7,fwd=0,fwdTxn=0),1)
  assert(!blocked.canData(rn,pa))
  rejects(blocked.rxDat(HNExtDat(rn,0,4,0,0,512,1,0,pa),2))
  blocked.snoopResponse(rn,0,7,1,0,0,None,None,3)
  var acked=false
  for(c<-4 to 300){val d=blocked.drive(c,true,true,true);d.dat.foreach{x=>blocked.rxDat(x,c)
    if(!acked){blocked.compAck(rn,x.home,x.dbid,c);acked=true}}}
  blocked.finish()
  // Real credit ledger simulation: all sixteen types, grants before RetryAck,
  // concurrent waits, field invariance, and no consumption before both events.
  for(rate<-Seq(0,10,30,50)) {
    val m=mk(retry=rate,split=50,dct=100)
    val originals=(0 until 160).map(i=>q(i,pa+i*64)).map(x=>x.key->x).toMap
    originals.values.foreach(m.request(_,0))
    val retryAck=mutable.Map.empty[(BigInt,BigInt),Int]
    val credit=mutable.Map.empty[Int,Int].withDefaultValue(0)
    val ackQueue=mutable.Queue.empty[(Int,BigInt,BigInt,BigInt)]
    for(c<-1 to 5000) {
      while(ackQueue.headOption.exists(_._1<=c)){val(_,r,h,id)=ackQueue.dequeue();m.compAck(r,h,id,c)}
      val d=m.drive(c,true,true,true)
      d.rsp.foreach{r=>m.rxRsp(r,c);r.op match {
        case 3=>retryAck((r.rn,r.txn))=r.credit
        case 7=>credit(r.credit)+=1
        case 11=>ackQueue.enqueue((c+1,r.rn,r.src,r.dbid))
        case _=>sys.error("unexpected response")
      }}
      d.dat.foreach{x=>m.rxDat(x,c)
        if(x.op==4&&x.beat==0)ackQueue.enqueue((c+1,x.rn,x.home,x.dbid))}
      retryAck.toSeq.foreach{case(key,t)=>if(credit(t)>0){
        val original=originals(key);val re=original.copy(fields=original.fields++Map("AllowRetry"->BigInt(0),"PCrdType"->BigInt(t),
          "TxnID"->(original.txn+1000),"QoS"->BigInt(5)))
        rejects(m.request(re.copy(fields=re.fields.updated("MemAttr",BigInt(0))),c))
        m.request(re,c);credit(t)-=1;retryAck.remove(key)
      }}
    }
    m.finish();assert(m.completedRequests.size==160)
    if(rate==0)assert(m.counts("retrySelected")==0)
    else assert(m.counts("retrySelected")>0&&m.counts("grantBeforeRetryAck")>0)
    if(rate==50)assert((0 until 16).forall(t=>m.counts("retryPCrdType:"+t)>0))
    println(s"HN_EXT_UNIT_CREDIT rate=$rate retries=${m.counts("retrySelected")} grantFirst=${m.counts("grantBeforeRetryAck")} pass=true")
  }
  // Snoop modifier rules and a failed-forwarding Read must be checked beyond
  // the original data-forwarding happy path (§4.9, §13.10.34, §2.3.1 Alt 5c).
  rejects(mk().rxSnp(s.copy(op=0x13,ret=1),0))
  rejects(mk().rxSnp(s.copy(op=0x17,dnSD=0),0))
  rejects(mk().rxSnp(s.copy(op=7,fwdTxn=0),0))
  val fallback=mk();fallback.rxSnp(s,0)
  fallback.snoopResponse(rn,0,7,1,0,0,None,None,1)
  assert(!fallback.empty,"failed forwarding must not discard its original Read")
  for(c<-2 to 5)fallback.drive(c,true,true,true)
  fallback.finish();assert(fallback.counts("fallbackReadCompleted")==1&&fallback.counts("fallbackCompAck")==1)
  // Directed CopyBack cancellation uses a fungible P-Credit, never TxnID of
  // PCrdReturn. Its protocol-required zero cannot identify request TxnID 7.
  val cancelModel=new L2HNExtensionModel(HNExtConfig(0,0,0,seed=37,maxGrantDelay=64,
    forwardEnable=false,directedCopyBackRetry=true),OvernightMemory.beat _)
  val copy=q(7).copy(fields=q(7).fields.updated("Opcode",BigInt(0x42)))
  cancelModel.request(copy,0);var invalidated=false;var returned=false
  for(c<-1 to 100){val d=cancelModel.drive(c,true,true,true)
    d.rsp.foreach{r=>cancelModel.rxRsp(r,c)
      if(r.op==3){cancelModel.rxSnp(s.copy(op=7,fwd=0,fwdTxn=0),c)
        cancelModel.snoopResponse(rn,0,7,1,0,0,None,None,c+1);invalidated=true}
      if(r.op==7){assert(invalidated)
        val ret=HNExtReq(Map("SrcID"->rn,"TgtID"->BigInt(0),"TxnID"->BigInt(0),
          "Opcode"->BigInt(5),"AllowRetry"->BigInt(0),"PCrdType"->BigInt(r.credit)))
        cancelModel.request(ret,c+1);returned=true}
    }
  }
  assert(returned);cancelModel.finish()
  println(s"HN_EXT_UNIT pass=true negatives=$negatives forwardTarget=true requesterDCT=true splitRead=true retry=true")
}
