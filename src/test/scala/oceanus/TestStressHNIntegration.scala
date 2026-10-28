package oceanus

// Host-only integration boundaries; these are not RTL coverage.
object TestStressHNIntegration extends App {
  val rn=BigInt(8);val pa=BigInt(0x220000)
  val m=new L2HNExtensionModel(HNExtConfig(retryPercent=0),OvernightMemory.beat _)
  var first=BigInt(0);var reused=false
  // More than 4000 lifetime transactions, while one old ID remains live.
  // Reuse must avoid that live ID and keep the Home's outstanding identity unique.
  m.offerSnoop(rn,pa,Some(7))
  val held=m.drive(0,true,true,true).snp.get;m.rxSnp(held,0)
  for(c<-1 to 8200) {
    m.offerSnoop(rn,pa+64,Some(7))
    val s=m.drive(c,true,true,true).snp.get
    assert(s.txn!=held.txn)
    if(c==1)first=s.txn
    if(c>4000&&s.txn==first)reused=true
    m.rxSnp(s,c)
    m.snoopResponse(rn,s.hn,s.txn,1,0,0,None,None,c)
  }
  assert(reused && m.pendingSnoops==1)
  m.snoopResponse(rn,held.hn,held.txn,1,0,0,None,None,8201);m.finish()

  val q=HNExtReq(Map("SrcID"->rn,"TgtID"->BigInt(0),"TxnID"->BigInt(1),"Addr"->pa,
    "Opcode"->BigInt(1),"AllowRetry"->BigInt(1),"PCrdType"->BigInt(0),"ExpCompAck"->BigInt(1)))
  val timing=new L2HNExtensionModel(HNExtConfig(retryPercent=0,splitPercent=0,dctPercent=0,
    requestLatencyMin=200,requestLatencyMax=200,minBeatGap=256),OvernightMemory.beat _)
  timing.request(q,0);var firstData = -1;var secondData = -1
  for(c<-1 to 800) {
    val d=timing.drive(c,true,true,true)
    d.dat.foreach {x=>
      assert(c>=200)
      if(x.beat==0)firstData=c else {assert(firstData>=0&&c-firstData>=256);secondData=c}
      timing.rxDat(x,c)
      if(x.beat==0)timing.compAck(rn,x.home,x.dbid,c)
    }
  }
  assert(firstData>=200&&secondData>=firstData+256);timing.finish()

  val overlap=new StressHNCDPOverlap
  overlap.event(0,pa,"cdpEmitted");overlap.event(0,pa,"cdpIssued");overlap.tick(0)
  overlap.event(511,pa+1,"requesterDCT");overlap.tick(511)
  overlap.tick(512)
  overlap.event(512,pa,"splitRead");overlap.tick(512)
  // Boundary=511 overlaps; boundary=512 does not. Every mask is also checked
  // by the independent interval scanner in report().
  overlap.report("host-boundary",0,true,513)
  println("STRESS_HN_INTEGRATION_UNIT pass=true lifetimeSnoops=8201 liveIdAvoidance=true latency=true actualBeatGap=true windowAudit=true")
}
