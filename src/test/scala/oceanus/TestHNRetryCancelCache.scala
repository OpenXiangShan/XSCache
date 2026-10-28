package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters,CCHIParametersKey}
import scala.collection.mutable

object HNRetryCacheReflect {
  def field[T](obj:AnyRef,name:String):T={
    var c:Class[_]=obj.getClass
    while(c!=null){try{val f=c.getDeclaredField(name);f.setAccessible(true);return f.get(obj).asInstanceOf[T]}
      catch{case _:NoSuchFieldException=>c=c.getSuperclass}}
    throw new NoSuchFieldException(name)
  }
}
class HNRetryCacheTop(cfg:L2Configuration)(implicit p:Parameters) extends TestTop_L2Top(cfg) {
  val occupancy=IO(Output(UInt(8.W)))
  val locks=IO(Output(Vec(8,UInt(6.W))))
  val addresses=IO(Output(Vec(8,UInt(48.W))))
  val ts=HNRetryCacheReflect.field[Seq[L2TSHR]](module.slices.head,"tshrs")
  occupancy:=Cat(ts.reverse.map(t=>BoringUtils.bore(t.io.valid)))
  for((t,i)<-ts.zipWithIndex){
    val v=HNRetryCacheReflect.field[oceanus.l2.tshr.L2VPipeREQ](t,"vPipeREQ")
    def b(n:String)=BoringUtils.bore(HNRetryCacheReflect.field[Bool](v,n))
    locks(i):=Cat(b("w_evict_peer_commit_dir"),b("w_evict_dn_compdbid"),b("w_unlock_ds"),b("w_unlock_dir"),b("w_dn_pcrdgrant"),b("s_dn_txreq"))
    addresses(i):=BoringUtils.bore(v.io.tshr_paddr)
  }
}
object TestHNRetryCancelCache extends App with ChiselSim {
  System.setProperty("oceanus.phase1a.ports","false")
  Constantin.init(false)
  val cancel= !args.contains("--control")
  val expectBaseIssue=args.contains("--expect-base-issue")
  require(!expectBaseIssue||cancel,"known-base mode requires the cancel case")
  val cfg=new L2Configuration(8,true,0 until 1)
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params(mshrSize=8,ways=4,sets=16)
    case CHIParametersKey=>CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case CCHIParametersKey=>CCHIParameters()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=HasSimulator.default
  simulate(new HNRetryCacheTop(cfg)(params)){d=>
    def f(x:Bool)=x.peek().litToBoolean
    def v(x:UInt)=x.peek().litValue
    val dummy=UpRequestSpec(MasterId.t1p0,0,0,0,CCHIOpcode.StashShared.opcode,0x80000,expCompData=false)
    val spec=CaseSpec("retry-cancel-cache",UpConfig(Seq(MasterId.t1p0),ExplicitRequestStream(Seq(dummy),Seq(BigInt(1))),SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(25),FixedOutstanding(64),ImmediateCreditPolicy()),50000,(0,50000),Seq.empty)
    val dn=new L2TopDownstreamModel(d,spec)
    d.io.t1p.foreach{p=>
      p.UpEVT.valid.poke(false.B);p.UpEVT.bits.poke(0.U.asTypeOf(p.UpEVT.bits))
      p.UpREQ.valid.poke(false.B);p.UpREQ.bits.poke(0.U.asTypeOf(p.UpREQ.bits))
      p.UpRSP.valid.poke(false.B);p.UpRSP.bits.poke(0.U.asTypeOf(p.UpRSP.bits))
      p.UpDAT.valid.poke(false.B);p.UpDAT.bits.poke(0.U.asTypeOf(p.UpDAT.bits))
      p.DnSNP.ready.poke(true.B);p.DnRSP.ready.poke(true.B);p.DnDAT.ready.poke(true.B)
    }
    d.io.t4p.foreach{p=>p.UpREQ.valid.poke(false.B);p.UpREQ.bits.poke(0.U.asTypeOf(p.UpREQ.bits));p.DnDAT.ready.poke(true.B)}
    d.log.clean.poke(false.B);d.log.dump.poke(false.B);dn.initInputs()
    d.reset.poke(true.B);d.clock.step(8);d.reset.poke(false.B);dn.activateRxLink()
    for(c<- -100 until 0)dn.stepWithCredits(c)
    var cycle=0;var target:Option[BigInt]=None;var snoopQueued=false;var wantsSnoop=false
    var returned = -1;var selected=0L;var acked=false;var upstreamAccepted=0
    val snps=mutable.Queue.empty[(BigInt,BigInt)]
    val memory:(BigInt,Int)=>BigInt=L2TopReferenceMemory.beat _
    val model=new L2HNExtensionModel(HNExtConfig(0,0,0,seed=37,maxGrantDelay=128,forwardEnable=false,directedCopyBackRetry=true),memory,
      line=>{
        println("CACHE_EVENT "+line)
        if(line.contains("name=retryAck ")&&target.exists(a=>line.endsWith("pa=0x"+a.toString(16))))wantsSnoop=true
        if(line.contains("name=creditReturn "))returned=cycle
      })
    val pins=new L2HNExtensionPins(d,dn,model,8,(q,c)=>{
      if(model.counts("retrySelected")>selected&&L2HNExtensionModel.isWrite(q.op)&&target.isEmpty){
        target=Some(q.addr);println(s"CACHE_TARGET cycle=$c pa=0x${q.addr.toString(16)} txn=${q.txn}")
      }
      selected=model.counts("retrySelected")
    })
    def tick():Unit={
      assert(cycle<50000,"cache minimal absolute watchdog")
      d.io.t1p.head.UpRSP.valid.poke(false.B)
      snps.headOption.foreach{case(id,src)=>val p=d.io.t1p.head.UpRSP
        p.valid.poke(true.B);p.bits.poke(0.U.asTypeOf(p.bits));p.bits.TxnID.poke(id.U);p.bits.TgtID.poke(src.U);p.bits.Opcode.poke(CCHIOpcode.SnpResp.opcode.U)
        if(f(p.ready))snps.dequeue()
      }
      dn.clearResponses();pins.drive(cycle,_=>false)
      if(f(d.io.t1p.head.DnSNP.valid))snps.enqueue((v(d.io.t1p.head.DnSNP.bits.TxnID),v(d.io.t1p.head.DnSNP.bits.SrcID)))
      if(f(d.io.t1p.head.UpREQ.valid)&&f(d.io.t1p.head.UpREQ.ready))upstreamAccepted+=1
      assert(!f(d.io.t1p.head.DnRSP.valid),"silent Stash unexpectedly returned a response")
      pins.sample(cycle)
      if(target.nonEmpty) {
        val state=(0 until 8).map(i=>s"$i:${v(d.addresses(i)).toString(16)}:${v(d.locks(i))}").mkString(",")
        println(s"CACHE_CYCLE cycle=$cycle occupancy=${v(d.occupancy)} locks=$state")
      }
      if(cancel&&wantsSnoop&& !snoopQueued){pins.offer(target.get,Some(7));snoopQueued=true;println(s"CACHE_SNOOP_OFFER cycle=$cycle pa=0x${target.get.toString(16)}")}
      dn.stepWithCredits(cycle);cycle+=1
    }
    def stash(i:Int):Unit={
      val p=d.io.t1p.head.UpREQ;p.bits.poke(0.U.asTypeOf(p.bits));p.bits.Addr.poke((0x80000+i*1024).U)
      p.bits.TxnID.poke(i.U);p.bits.Opcode.poke(CCHIOpcode.StashShared.opcode.U);p.bits.Size.poke(6.U);p.bits.MemAttr.poke(15.U);p.valid.poke(true.B)
      val before=upstreamAccepted
      while(upstreamAccepted==before&&returned<0)tick()
      p.valid.poke(false.B)
      var quiet=0
      while(quiet<32&&returned<0){tick();quiet=if(model.empty&&v(d.occupancy)==0)quiet+1 else 0}
    }
    var issued=0
    while(issued<32&&returned<0&& !(target.nonEmpty&& !cancel&&model.empty&&v(d.occupancy)==0)){stash(issued);issued+=1}
    assert(target.nonEmpty,"no directed long-delay CopyBack retry selected")
    if(cancel){assert(returned>=0,"SnpUnique did not produce PCrdReturn");for(i<-0 until 1024)tick()}
    else {for(i<-0 until 80)tick()}
    val remaining=(0 until 8).filter(i=>v(d.locks(i))!=0).map(i=>s"$i:${v(d.addresses(i)).toString(16)}:${v(d.locks(i))}").mkString(",")
    println(s"CACHE_CANCEL_RESULT cancel=$cancel cycle=$cycle stashRequests=$issued HNEmpty=${model.empty} creditReturns=${model.counts("creditReturn")} occupancy=${v(d.occupancy)} remainingLocks=$remaining")
    if(expectBaseIssue) {
      // Only these independently observed signatures are expected. Model CHI
      // checks remain enabled, and an unrelated exception still fails the run.
      val stuck=(0 until 8).exists(i=>v(d.addresses(i))==target.get&&v(d.locks(i))==12)
      assert(model.empty&&model.counts("creditReturn")==1&&issued==5,
        "known-base cancellation did not close the expected CHI sequence")
      assert(v(d.occupancy)==2&&stuck,"known-base lock signature changed")
      assert(model.counts("dutProtocolErrors")==1,"known-base TxnID signature changed")
      try {model.finish();throw new AssertionError("expected PCrdReturn TxnID violation")}
      catch {case e:IllegalStateException if e.getMessage.startsWith("L2_CHI_PROTOCOL_VIOLATION")=>()}
      println("RETRY_BASE_KNOWN status=XFAIL issues=locks,PCrdReturn.TxnID base 已知问题(见 retry-base-note.md)")
    } else {
      model.finish()
      assert(v(d.occupancy)==0,s"cache retains TSHR occupancy after CHI closure: $remaining")
    }
  }
}
