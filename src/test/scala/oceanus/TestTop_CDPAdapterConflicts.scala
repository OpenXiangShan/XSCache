package oceanus

// Directed adapter-interface witnesses supplement the system raw lifecycle
// test. Inputs are legal Valid pulses at the published cache-event boundary;
// this harness does not force signals or claim end-to-end cache reachability.
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import scala.collection.mutable
import utility._

class CDPAdapterConflictHarness(implicit p:Parameters) extends Module {
  val io=IO(new Bundle {
    val train=Input(Vec(16,Valid(new L2CDPTrain)))
    val evict=Input(Vec(16,Valid(new L2CDPEvict)))
    val line=Input(Vec(2,Vec(8,Valid(new L2CDPLine))))
    val enable=Input(Bool())
  })
  val events=Module(new L2CDPCacheEvents(16))
  events.io.train:=io.train;events.io.evict:=io.evict;events.io.enable:=io.enable
  val algorithm=Module(new OceanusCDP(bankCount=2,capturedHalves=true))
  algorithm.io.enable:=io.enable;algorithm.io.vpnTrain:=events.io.vpn;algorithm.io.filterTrain:=events.io.filter
  algorithm.io.l2DetectTriggers:=0.U.asTypeOf(algorithm.io.l2DetectTriggers)
  algorithm.io.pfStat:=0.U.asTypeOf(algorithm.io.pfStat)
  algorithm.io.req.ready:=true.B;algorithm.io.tlb.req.ready:=true.B
  algorithm.io.tlb.resp.valid:=false.B;algorithm.io.tlb.resp.bits:=0.U.asTypeOf(algorithm.io.tlb.resp.bits)
  val arb=Module(new RRArbiter(new L2CDPHalf,2))
  val fields=mutable.ArrayBuffer.empty[(String,UInt)]
  def read[T<:Data](d:T):T=BoringUtils.bore(d)
  def add(n:String,d:Data):Unit=fields+=n->read(d).asUInt
  def fire(d:ReadyValidIO[_]):Bool=read(d.valid) && read(d.ready)
  for(bank<-0 until 2) {
    val q=Module(new L2CDPHalfQueue)
    q.io.in.valid:=io.line(bank).map(_.valid).reduce(_||_)
    q.io.in.bits:=PriorityMux(io.line(bank).map(x=>x.valid->x.bits))
    arb.io.in(bank)<>q.io.out
    val dropped=RegInit(0.U(64.W));val n=PopCount(io.line(bank).map(_.valid))
    when(n>1.U){dropped:=dropped+((n-1.U)<<1)}
    add(s"captureDrops$bank",q.io.dropped);add(s"collisionDrops$bank",dropped)
  }
  arb.io.out.ready:=true.B
  algorithm.io.capturedHalf.get.valid:=arb.io.out.valid
  algorithm.io.capturedHalf.get.bits.data:=arb.io.out.bits.data
  algorithm.io.capturedHalf.get.bits.pfSource:=arb.io.out.bits.pfSource
  algorithm.io.capturedHalf.get.bits.pfDepth:=arb.io.out.bits.pfDepth
  algorithm.io.capturedHalf.get.bits.isHit:=arb.io.out.bits.isHit
  algorithm.io.capturedHalf.get.bits.slice:=arb.io.chosen
  val vpn=IO(Output(Valid(new OceanusCDPTrain)));vpn:=events.io.vpn
  val filter=IO(Output(Valid(new OceanusCDPTrain)));filter:=events.io.filter
  val half=IO(Output(Valid(new L2CDPHalf)));half.valid:=arb.io.out.valid;half.bits:=arb.io.out.bits
  val halfBank=IO(Output(UInt(1.W)));halfBank:=arb.io.chosen
  val ft=algorithm.filterTraining.get;val vq=algorithm.vpnTrainReqBuf
  add("enable",io.enable);add("vpnCollisions",events.vpnCollisions);add("filterCollisions",events.filterCollisions)
  add("filterQueueCount",ft.queue.io.count);add("vpnQueueCount",vq.io.count)
  add("filterAccepted",fire(ft.queue.io.enq));add("filterDropped",read(ft.queue.io.enq.valid) && !read(ft.queue.io.enq.ready))
  add("vpnAccepted",fire(vq.io.enq));add("vpnDropped",read(vq.io.enq.valid) && !read(vq.io.enq.ready))
  add("filterDequeue",fire(ft.queue.io.deq));add("vpnDequeue",fire(vq.io.deq))
  val boundary=IO(Output(UInt(fields.map(_._2.getWidth).sum.W)))
  boundary:=Cat(fields.map(_._2).reverse.toSeq)
  val widths=fields.map{case(n,v)=>n->v.getWidth}.toVector
}
object TestTop_CDPAdapterConflicts extends App with ChiselSim {
  Constantin.init(false)
  implicit val simulator:HasSimulator=CDPAuditSimulator.bounded
  simulate(new CDPAdapterConflictHarness()(CDPC5Params.apply)) { dut=>
    val model=new CDPRawEventReference(2,(_,_)=>BigInt(0))
    import model._
    var cycle=0
    def tick(ts:Seq[(Int,Train)]=Seq.empty,es:Seq[(Int,Meta,BigInt)]=Seq.empty,
        ls:Seq[(Int,Int,Line)]=Seq.empty,enabled:Boolean=true):Unit={
      dut.io.enable.poke(enabled)
      dut.io.train.foreach(_.poke(0.U.asTypeOf(dut.io.train.head)))
      dut.io.evict.foreach(_.poke(0.U.asTypeOf(dut.io.evict.head)))
      dut.io.line.foreach(_.foreach(_.poke(0.U.asTypeOf(dut.io.line.head.head))))
      for((i,t)<-ts){val x=dut.io.train(i);x.valid.poke(true);x.bits.paddr.poke(t.pa);x.bits.vaddr.poke(t.va)
        x.bits.pfSource.poke(t.source);x.bits.pfDepth.poke(t.depth);x.bits.hit.poke(t.hit)
        x.bits.reqSource.poke(t.req);x.bits.trainEnable.poke(t.en)}
      for((i,m,pa)<-es){val x=dut.io.evict(i);x.valid.poke(true);x.bits.paddr.poke(pa)
        x.bits.pfSource.poke(m.source);x.bits.pfDepth.poke(m.depth)}
      for((b,i,l)<-ls){val x=dut.io.line(b)(i);x.valid.poke(true);x.bits.paddr.poke(l.pa)
        x.bits.pfSource.poke(l.source);x.bits.pfDepth.poke(l.depth);x.bits.isHit.poke(l.hit)
        x.bits.eventType.poke(if(l.hit)1 else 0);x.bits.data.poke(0)}
      var bits=dut.boundary.peek().litValue
      val row=dut.widths.map{case(n,w)=>val v=bits&((BigInt(1)<<w)-1);bits>>=w;n->v}.toMap
      val lines=(0 until 2).map(b=>ls.filter(_._1==b).map{case(_,i,l)=>i->l})
      model.checkAdapters(row,cycle,ts,es,lines,dut.vpn,dut.filter,dut.half,dut.halfBank)
      dut.clock.step();cycle+=1
    }
    dut.io.enable.poke(false);dut.io.train.foreach(_.poke(0.U.asTypeOf(dut.io.train.head)))
    dut.io.evict.foreach(_.poke(0.U.asTypeOf(dut.io.evict.head)))
    dut.io.line.foreach(_.foreach(_.poke(0.U.asTypeOf(dut.io.line.head.head))))
    dut.reset.poke(true);dut.clock.step(8);dut.reset.poke(false)
    // Chisel RRArbiter has no reset for lastGrant. A singleton transfer
    // establishes its known history before testing simultaneous choices.
    tick(Seq(0->Train(0x60000,0x8000,9,1,true,0,true)),
      ls=Seq((0,0,Line(0xa0000,0,0,false))))
    for(_<-0 until 30)tick()
    for(c<-0 until 80){
      val trains=Seq(0,1,8,9).map(i=>i->Train(BigInt(0x60000+i*64),BigInt(0x8000+i),if(i==0)0 else 9,1,i!=0,0,true))
      val evicts=Seq(2,3,10).map(i=>(i,Meta(true,1,9,1),BigInt(0x90000+i*64)))
      val lines=(0 until 2).flatMap(b=>Seq((b,0,Line(BigInt(0xa0000+b*64),0,0,false)),(b,1,Line(BigInt(0xb0000+b*64),0,0,false))))
      tick(trains,evicts,lines)
    }
    tick(Seq(0->Train(0x60000,0x8000,9,1,true,MemReqSource.L1DataPrefetch.id,true)))
    tick(Seq(1->Train(0x60040,0x8001,9,1,true,0,false)))
    tick(Seq(0->Train(0x60000,0x8000,9,1,true,0,true)),enabled=false)
    for(_<-0 until 200)tick()
    model.requireCoverage(Seq("crossTSHRTrain","crossSliceTrain","crossTSHREvict","crossSliceEvict",
      "vpnFilterDifferent","trainEvictSameCycle","usedEvictConflict","captureOneSlot","captureFull","captureSimultaneous","captureCollision"))
    model.finish()
    println("CDP_M3_ADAPTER_RESULT pass=true scope=cache-event-interface cycles="+cycle+" "+model.coverage.toSeq.sortBy(_._1).map{case(k,v)=>s"$k=$v"}.mkString(" "))
  }
}
