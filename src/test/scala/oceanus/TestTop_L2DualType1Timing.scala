package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters,CCHIParametersKey}
import scala.collection.mutable

class DualTimingTop(cfg:L2Configuration)(implicit p:Parameters) extends TestTop_L2Top(cfg) {
  def read[T <: Data](x:T):T=BoringUtils.bore(x)
  val counts=IO(Output(Vec(3,Vec(4,Vec(5,UInt(64.W))))))
  val ledger=module.trainAccounting.get
  counts(0):=read(ledger.io.totals)
  counts(1):=read(ledger.io.accepts)
  counts(2):=read(ledger.io.drops)
  if(p(L2ParamsKey).hasMbist) {
    val broadcast=_root_.utility.sram.SramHelper.genBroadCastBundleTop()
    broadcast:=0.U.asTypeOf(broadcast)
    val te=ClockGate.genTeSrc;te.cgen:=false.B
  }
}
object TestTop_L2DualType1Timing extends App with ChiselSim {
  Constantin.init(false)
  require(L2TopPhase1aPorts.enabled)
  val upstream=new L2UpstreamTable(Seq(
    L2UpstreamTableEntry(L2UpstreamPortType.Type1,0),L2UpstreamTableEntry(L2UpstreamPortType.Type1,1),
    L2UpstreamTableEntry(L2UpstreamPortType.Type4,4),L2UpstreamTableEntry(L2UpstreamPortType.Type4,5)))
  val cfg=new L2Configuration(0,true,0 until 4,upstream=upstream,pfSrcID=2,bopEnable=false,
    prefetchIssueEnable=false,pfTrainEnable=true,pfTlbEnable=true,pfCtrlEnable=true,
    vbopStubEnable=true,cdpStubEnable=true)
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params(mshrSize=32,ways=8,sets=1024,hasMbist=true,enableDataECC=true)
    case CHIParametersKey=>CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,
      reqRsvdcWidth=4,datRsvdcWidth=4,dataWidth=256,dataCheckPresent=true,poisonPresent=true,mpamPresent=true)
    case CCHIParametersKey=>CCHIParameters()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=CDPAuditSimulator.bounded
  simulate(new DualTimingTop(cfg)(params)){dut=>
    val spec=CaseSpec("dual-type1-timing",UpConfig(Seq(MasterId.t1p0,MasterId.t1p1),
      ExplicitRequestStream(Seq.empty,Seq.empty),SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(30),FixedOutstanding(64),ImmediateCreditPolicy()),10000,(0,10000),Seq.empty)
    val up=new L2TopUpstreamModel(dut,spec);val dn=new L2TopDownstreamModel(dut,spec)
    case class Request(pa:BigInt,txn:Int,en:Boolean)
    val held=mutable.Map.empty[Int,Request]
    val active=mutable.Map.empty[(Int,Int),Request]
    val beatMasks=mutable.Map.empty[(Int,Int),Set[Int]].withDefaultValue(Set.empty)
    val acks=Seq.fill(2)(mutable.Queue.empty[(Int,BigInt,BigInt)])
    val data=mutable.Queue.empty[PendingDnComp];val rsp=mutable.Queue.empty[PendingDnRsp]
    val accepted=mutable.ArrayBuffer.empty[(Int,Int,Boolean)]
    val trained=mutable.ArrayBuffer.empty[(Int,Int,Boolean)]
    var cycle=0;var nextTxn=0;var completed=0;var checkedBeats=0
    def flag(x:Bool)=x.peek().litToBoolean
    def value(x:UInt)=x.peek().litValue
    up.initInputs();dn.initInputs();dut.log.dump.poke(false.B);dut.log.clean.poke(false.B)
    dut.holdIssue.get.poke(true.B)
    dut.io.pfCtrl.get.poke(0.U.asTypeOf(dut.io.pfCtrl.get))
    dut.io.pfCtrl.get.l2_pf_master_en.poke(true.B)
    dut.io.pfCtrl.get.l2_vbop_en.poke(true.B);dut.io.pfCtrl.get.l2_cdp_en.poke(true.B)
    dut.reset.poke(true.B);dut.clock.step(8);dut.reset.poke(false.B);dn.activateRxLink()
    for(c<- -300 until 0)dn.stepWithCredits(c)
    def enqueue(port:Int,pa:BigInt,en:Boolean):Unit={assert(!held.contains(port));held(port)=Request(pa,nextTxn,en);nextTxn+=1}
    def tick():Unit={
      assert(cycle<10000,s"dual timing watchdog held=$held active=$active")
      dn.clearResponses();up.clearResponses();up.clearUpReq()
      for(port<-0 until 2){
        val ctx=dut.io.pfTrain.get(port)
        ctx.vaddrBlock.poke((BigInt(0x555000)+cycle*64).U);ctx.reqSource.poke(31.U);ctx.trainEnable.poke(false.B)
        held.get(port).foreach{r=>
          val q=dut.io.t1p(port).UpREQ;q.valid.poke(true.B);q.bits.poke(0.U.asTypeOf(q.bits))
          q.bits.SrcID.poke(port.U);q.bits.TxnID.poke(r.txn.U);q.bits.Opcode.poke(CCHIOpcode.ReadShared.opcode.U)
          q.bits.Size.poke(6.U);q.bits.MemAttr.poke(5.U);q.bits.ExpCompData.poke(true.B);q.bits.Addr.poke(r.pa.U)
          ctx.vaddrBlock.poke(((r.pa+0x2000000)>>6).U);ctx.reqSource.poke(port.U);ctx.trainEnable.poke(r.en.B)
        }
        acks(port).headOption.foreach{case(_,dbid,slice)=>up.driveUpCompAck(UpDBID(dbid),slice.toInt,port)}
        val q=dut.io.t1p(port).UpREQ
        if(flag(q.valid)&&flag(q.ready)){
          val r=held.remove(port).get;active(port->r.txn)=r;accepted+=((cycle,port,r.en))
        }
        val ack=dut.io.t1p(port).UpRSP
        if(flag(ack.valid)&&flag(ack.ready)){
          val (id,_,_)=acks(port).dequeue();assert(active.remove(port->id).nonEmpty);beatMasks.remove(port->id);completed+=1
        }
        val d=dut.io.t1p(port).DnDAT
        if(flag(d.valid)&&flag(d.ready)){
          val key=port->value(d.bits.TxnID).toInt;val r=active(key);val beat=value(d.bits.DataID).toInt
          assert(Set(0,1)(beat)&& !beatMasks(key)(beat));assert(value(d.bits.Data)==L2TopReferenceMemory.beat(r.pa,beat))
          beatMasks(key)=beatMasks(key)+beat;checkedBeats+=1
          if(beatMasks(key).size==2)acks(port).enqueue((key._2,value(d.bits.DBID),value(d.bits.SrcID)))
        }
      }
      for(bank<-dut.trainObservation.get.req;t<-bank if flag(t.train))
        trained+=((cycle,value(t.src).toInt,flag(t.event.trainEnable.get)))
      if(flag(dut.io.chi.txreq.flitv)){
        val f=dut.io.chi.txreq.flit;val id=DnTxnID(value(f.TxnID.get));val src=value(f.SrcID.get);val op=value(f.Opcode.get)
        dn.noteRequest(id)
        if(dn.isEvictReq(op))rsp.enqueue(PendingDnRsp(id,src,cycle+30,dn.downstreamEvictRspOpcode(op)))
        else for(b<-0 until 2)data.enqueue(PendingDnComp(id,src,cycle+30+b,b,CHIFieldResp.CompData_SC,value(f.Addr.get)))
      }
      data.dequeueFirst(d=>d.due<=cycle&&dn.canDriveRxDat).foreach{d=>dn.driveDnData(d);dn.noteResponse(d.dnTxnID,d.beat==1,cycle)}
      rsp.dequeueFirst(r=>r.due<=cycle&&dn.canDriveRxRsp).foreach{r=>dn.driveDnRsp(r);dn.noteResponse(r.dnTxnID,true,cycle)}
      dn.stepWithCredits(cycle);cycle+=1
    }
    def drain():Unit={var quiet=0;while(quiet<20){tick();if(held.isEmpty&&active.isEmpty&&data.isEmpty&&rsp.isEmpty&&dn.trainChecker.pending==0)quiet+=1 else quiet=0}}
    def pairAddresses(base:BigInt)=Seq(0,1).map(s=>(0 until 256).map(i=>base+i*64).find(a=>L2TopBOPExpectations.slice(a,4)==s).get)
    def counts=Array.tabulate(3,4,5)((kind,sink,source)=>value(dut.counts(kind)(sink)(source)))
    for((name,gap,enabled)<-Seq(("same",0,true),("adjacent",1,true),("alternating",2,true),("disabled",0,false))){
      val before=counts;val a0=accepted.size;val t0=trained.size;val addresses=pairAddresses(BigInt(0x10000+nextTxn*0x1000))
      enqueue(0,addresses(0),enabled)
      if(gap==0)enqueue(1,addresses(1),enabled)
      tick()
      if(gap>0){for(_<-1 until gap)tick();enqueue(1,addresses(1),enabled)}
      drain()
      val got=accepted.drop(a0);val events=trained.drop(t0)
      assert(got.size==2&&events.size==2);assert(got(1)._1-got(0)._1==gap,s"$name acceptance gap=$got")
      assert(events(1)._1-events(0)._1==gap,s"$name training gap=$events")
      assert(events.forall(_._3==enabled))
      val after=counts
      for(sink<-0 until 4;src<-0 until 2){
        assert(after(0)(sink)(src)-before(0)(sink)(src)==1)
        val took=after(1)(sink)(src)-before(1)(sink)(src)
        val drop=after(2)(sink)(src)-before(2)(sink)(src)
        assert(took+drop==1);if(!enabled)assert(took==0&&drop==1)
      }
      println(s"DUAL_TYPE1_TIMING pass=true mode=$name gap=$gap enabled=$enabled accepted=$got trained=$events accounting=true")
    }
    assert(completed==8&&checkedBeats==16&&active.isEmpty)
    println(s"DUAL_TYPE1_TIMING_RESULT pass=true completed=$completed checkedBeats=$checkedBeats slices=4 tshrs=32 ways=8 sets=1024 mbist=true ecc=true")
  }
}
