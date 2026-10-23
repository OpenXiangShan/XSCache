// C3: original DetectPipeline is never edited. Its queue dependency adaptation
// is explicit below; raw utility MIMOQueue gets a separate counterexample test.
package oceanus
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.{cdpreference => ref}
import oceanus.chi._
import org.chipsalliance.cde.config._
import utility._
import java.nio.file.{Files,Paths}
import java.io.PrintWriter
import scala.collection.mutable
import scala.util.Random

// Test-only dependency adapter: original MIMOQueue, with just total accepted
// pointer advancement overridden. No ReferenceCDP algorithm statement changes.
class CDPC3ReferenceQueue[T <: Data](gen: T, inputs: Int) extends MIMOQueue(gen,8,inputs,1) {
  when(!io.flush) { enq_ptr := enq_ptr + PopCount(io.enq.map(_.fire)) }
}
class CDPC3ReferenceDetection extends Bundle {
  val pfVAddr = UInt(50.W); val pfDepth = UInt(2.W)
  val pfSource = UInt(4.W); val isHit = Bool(); val slice = UInt(2.W)
}
class CDPC3ReferenceFrontend(filtered: Boolean, threshold: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val triggers = Flipped(Vec(4, Valid(new ref.CDPDetectTask(512))))
    val queryReq = Vec(4, Valid(new ref.VtQueryReq))
    val queryRsp = Input(Vec(4,new ref.VtQueryRsp))
    val addrState = Input(new Bundle {val isSv39=Bool();val isSv48=Bool();val isSv57=Bool()})
    val out = Decoupled(new CDPC3ReferenceDetection)
  })
  val detectPipeSeq = Seq.tabulate(4)(i=>Module(new ref.DetectPipeline(s"reference_dp$i")))
  val detectTrigQueueSeq = Seq.fill(4)(Module(new CDPC3ReferenceQueue(new ref.CDPDetectTask(256),2)))
  val detectTrigArb = Module(new RRArbiterInit(new ref.CDPDetectTask(256),4))
  for(i<-0 until 4) {
    val detectTrig=io.triggers(i);val detectTrigQueue=detectTrigQueueSeq(i)
    val detectTrigFromCDP=detectTrig.bits.pfSource===ref.PfSource.CDP.id.U
    val detectTrigFromSMS=detectTrig.bits.pfSource===ref.PfSource.SMS.id.U
    val detectTrigFromBOP=detectTrig.bits.pfSource===ref.PfSource.BOP.id.U || detectTrig.bits.pfSource===ref.PfSource.PBOP.id.U
    val detectTrigFromCPU=detectTrig.bits.pfSource===ref.PfSource.NoWhere.id.U
    val detectTrigFromStride=detectTrig.bits.pfSource===ref.PfSource.Stride.id.U
    val detectTrigFromStream=detectTrig.bits.pfSource===ref.PfSource.Stream.id.U
    val hitTrigger=detectTrig.bits.isHit && (if(filtered) {
      detectTrigFromCDP && (detectTrig.bits.pfDepth===1.U || detectTrig.bits.pfDepth===3.U) || detectTrigFromSMS || detectTrigFromBOP
    } else detectTrigFromCDP && (detectTrig.bits.pfDepth===1.U || detectTrig.bits.pfDepth===3.U))
    val refillTrigger= !detectTrig.bits.isHit && detectTrig.bits.pfDepth<threshold.U &&
      (if(filtered) detectTrigFromCPU || detectTrigFromCDP || detectTrigFromStride || detectTrigFromStream else true.B)
    detectTrigQueue.io.flush:=reset.asBool
    for(j<-0 until 2) {
      detectTrigQueue.io.enq(j).valid:=detectTrig.valid && (hitTrigger || refillTrigger) && io.enable
      detectTrigQueue.io.enq(j).bits.data:=detectTrig.bits.data((j+1)*256-1,j*256)
      detectTrigQueue.io.enq(j).bits.pfDepth:=detectTrig.bits.pfDepth
      detectTrigQueue.io.enq(j).bits.pfSource:=detectTrig.bits.pfSource
      detectTrigQueue.io.enq(j).bits.isHit:=detectTrig.bits.isHit
    }
    detectTrigArb.io.in(i)<>detectTrigQueue.io.deq(0)
  }
  detectTrigArb.io.out.ready:=true.B
  val slice1=RegEnable(detectTrigArb.io.chosen,detectTrigArb.io.out.valid)
  val v1=RegNext(detectTrigArb.io.out.valid,false.B);val v2=RegNext(v1,false.B)
  val slice2=RegEnable(slice1,v1);val slice3=RegEnable(slice2,v2)
  val raw=Wire(Vec(4,Valid(new CDPC3ReferenceDetection)))
  val degreeBufSeq=Seq.fill(4)(Module(new CDPC3ReferenceQueue(new CDPC3ReferenceDetection,3)))
  val degreeBufArb=Module(new RRArbiterInit(new CDPC3ReferenceDetection,4))
  for(i<-0 until 4) {
    val pipe=detectPipeSeq(i)
    pipe.io.detectReq.valid:=detectTrigArb.io.out.valid
    pipe.io.detectReq.bits.data:=detectTrigArb.io.out.bits.data((i+1)*64-1,i*64)
    pipe.io.detectReq.bits.pfDepth:=detectTrigArb.io.out.bits.pfDepth
    pipe.io.detectReq.bits.pfSource:=detectTrigArb.io.out.bits.pfSource
    pipe.io.detectReq.bits.isHit:=detectTrigArb.io.out.bits.isHit
    pipe.io.addrState:=io.addrState
    io.queryReq(i):=pipe.io.vtQueryReq;pipe.io.vtQueryRsp:=io.queryRsp(i)
    raw(i).valid:=pipe.io.pftReq.valid
    raw(i).bits.pfVAddr:=pipe.io.pftReq.bits.pfVAddr
    raw(i).bits.pfDepth:=pipe.io.pftReq.bits.pfDepth
    raw(i).bits.pfSource:=pipe.io.pftReq.bits.pfSource
    raw(i).bits.isHit:=pipe.io.pftReq.bits.isHit;raw(i).bits.slice:=slice3
    for(j<-0 until 3) {
      degreeBufSeq(i).io.enq(j).valid:=pipe.io.pftReq.valid
      degreeBufSeq(i).io.enq(j).bits:=raw(i).bits
      if(j>0)degreeBufSeq(i).io.enq(j).bits.pfVAddr:=pipe.io.pftReq.bits.pfVAddr+(j*64).U
    }
    degreeBufSeq(i).io.flush:=reset.asBool
    degreeBufArb.io.in(i)<>degreeBufSeq(i).io.deq(0)
  }
  io.out<>degreeBufArb.io.out
}
class CDPC3Harness(filtered:Boolean,threshold:Int)(implicit p:Parameters) extends Module {
  val pp=p.alterPartial{case OceanusCDPKey=>OceanusCDPParameters(useFilteredDetect=filtered,depthThreshold=threshold);case OceanusCDPBanksKey=>4}
  val rp=p.alterPartial{case ref.ReferenceCDPKey=>ref.CDPParameters(useFilteredDetect=filtered,depthThreshold=threshold);case ref.ReferenceCDPBankBitsKey=>2}
  val io=IO(new Bundle {
    val events=Input(Vec(4,UInt(520.W))) // data512, depth2, source4, hit, valid
    val enable=Input(Bool());val ready=Input(Bool());val mode=Input(UInt(2.W))
    val rsp=Input(Vec(4,UInt(16.W))) // tag10, valid4, hot2 (way0/1)
    val half=Output(UInt(266.W)) // valid, chosen2, task263
    val raw=Output(Vec(4,UInt(60.W))) // valid, slice2, addr50,depth2,source4,hit
    val out=Output(UInt(60.W))
    val counts=Output(Vec(4,UInt(4.W)))
    val degreeCounts=Output(Vec(4,UInt(4.W)))
    val accepted=Output(Vec(4,UInt(2.W)));val dropped=Output(Vec(4,UInt(2.W)))
    val degreeAccepted=Output(Vec(4,UInt(2.W)))
    val query=Output(Vec(4,UInt(5.W))) // valid/main2/sub2
    val reasons=Output(Vec(4,UInt(7.W)))
    val packed=Output(UInt(670.W))
  })
  // One simulator read per clock; these are precisely the existing probes.
  val observations=Seq(io.half)++io.raw.toSeq++Seq(io.out)++io.counts.toSeq++io.degreeCounts.toSeq++
    io.accepted.toSeq++io.dropped.toSeq++io.degreeAccepted.toSeq++io.query.toSeq++io.reasons.toSeq
  require(observations.map(_.getWidth).sum==670)
  io.packed:=Cat(observations.reverse)
  val prod=Module(new OceanusCDPDetectFrontend()(pp))
  val reference=Module(new CDPC3ReferenceFrontend(filtered,threshold)(rp))
  prod.io.enable:=io.enable;reference.io.enable:=io.enable
  prod.io.out.ready:=io.ready;reference.io.out.ready:=io.ready
  prod.io.addrState.isSv39:=io.mode===0.U;prod.io.addrState.isSv48:=io.mode===1.U;prod.io.addrState.isSv57:=io.mode===2.U
  reference.io.addrState:=prod.io.addrState
  def read[T<:Data](s:T):T=BoringUtils.bore(s)
  def packed(d:OceanusCDPDetection)=Cat(d.slice,d.pfVAddr,d.pfDepth,d.pfSource,d.isHit)
  def rpacked(d:CDPC3ReferenceDetection)=Cat(d.slice,d.pfVAddr,d.pfDepth,d.pfSource,d.isHit)
  for(i<-0 until 4) {
    val event=io.events(i)
    prod.io.triggers(i).valid:=event(519);prod.io.triggers(i).bits.data:=event(511,0)
    prod.io.triggers(i).bits.pfDepth:=event(513,512);prod.io.triggers(i).bits.pfSource:=event(517,514);prod.io.triggers(i).bits.isHit:=event(518)
    reference.io.triggers(i).valid:=event(519);reference.io.triggers(i).bits:=prod.io.triggers(i).bits.asTypeOf(reference.io.triggers(i).bits)
    val rsp=WireInit(0.U.asTypeOf(new OceanusCDPVtQueryRsp()(pp)))
    for(w<-0 until 4) {
      rsp.subValidVec(w):=io.rsp(i)(w+10);rsp.mainValidVec(w):=io.rsp(i)(w+10)
      rsp.tagVec(w):=io.rsp(i)(9,0)+w.U
      rsp.metaVec(w).hot:= (if(w<2)io.rsp(i)(w+14) else false.B)
    }
    prod.io.queryRsp(i):=rsp;reference.io.queryRsp(i):=rsp.asTypeOf(reference.io.queryRsp(i))
    io.counts(i):=read(prod.queues(i).io.count);io.degreeCounts(i):=read(prod.degreeQueues(i).io.count)
    io.accepted(i):=read(prod.accepted)(i);io.dropped(i):=read(prod.dropped)(i)
    io.degreeAccepted(i):=PopCount((0 until 3).map(j=>read(prod.degreeQueues(i).io.enq(j).valid)&&read(prod.degreeQueues(i).io.enq(j).ready)))
    val raw=read(prod.raw)(i);val rr=read(reference.raw)(i)
    io.raw(i):=Cat(raw.valid,Mux(raw.valid,packed(raw.bits),0.U))
    val query=prod.io.queryReq(i)
    io.query(i):=Cat(query.valid,Mux(query.valid,query.bits.asUInt,0.U))
    val lane=prod.lanes(i)
    val s2v=read(lane.s2_req.valid)
    io.reasons(i):=Cat(s2v,Mux(s2v,Cat(read(lane.s2_high_bit_is_zero),read(lane.s2_low_bit_is_zero),read(lane.s2_vpn0_is_nzero),read(lane.s2_vt_hit),read(lane.s2_vt_hit_hot),read(lane.s2_can_pft)),0.U))
    when(!reset.asBool) {
      assert(raw.valid===rr.valid,s"C3 lane $i valid mismatch")
      when(raw.valid){assert(packed(raw.bits)===rpacked(rr.bits),s"C3 lane $i candidate mismatch")}
      assert(query.valid===reference.io.queryReq(i).valid)
      when(query.valid){assert(query.bits.asUInt===reference.io.queryReq(i).bits.asUInt)}
      for(j<-0 until 2)assert(read(prod.queues(i).io.enq(j).ready)===read(reference.detectTrigQueueSeq(i).io.enq(j).ready))
      for(j<-0 until 3)assert(read(prod.degreeQueues(i).io.enq(j).ready)===read(reference.degreeBufSeq(i).io.enq(j).ready))
    }
  }
  val hv=read(prod.halfArb.io.out.valid)
  io.half:=Cat(hv,Mux(hv,read(prod.halfArb.io.chosen),0.U),Mux(hv,read(prod.halfArb.io.out.bits).asUInt,0.U))
  io.out:=Cat(prod.io.out.valid,Mux(prod.io.out.valid,packed(prod.io.out.bits),0.U))
  when(!reset.asBool) {
    assert(prod.io.out.valid===reference.io.out.valid,"C3 degree valid mismatch")
    when(prod.io.out.valid){assert(packed(prod.io.out.bits)===rpacked(reference.io.out.bits),"C3 degree output mismatch")}
  }
}

object TestTop_OceanusCDPC3 extends App with ChiselSim {
  Constantin.init(false)
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params()
    case CHIParametersKey=>CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case xscache.oceanus.compactchi.CCHIParametersKey=>xscache.oceanus.compactchi.CCHIParameters()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=HasSimulator.default
  def option(key:String,default:Int)=args.find(_.startsWith(key+"=")).map(_.split("=")(1).toInt).getOrElse(default)
  val seeds=option("--seeds",1000);val randomCycles=option("--cycles",512)
  val traceRoot=Paths.get(sys.props.getOrElse("oceanus.c3.trace","/data/zhuy/pf-framework/c3/traces"))
  Files.createDirectories(traceRoot)
  val head=sys.props.getOrElse("oceanus.c3.head",sys.error("HEAD required"))
  val total=mutable.Map.empty[String,Long].withDefaultValue(0L)
  def cover(name:String,n:Long=1):Unit={total(name)+=n}
  val mask50=(BigInt(1)<<50)-1;val mask256=(BigInt(1)<<256)-1
  case class Half(data:BigInt,depth:Int,source:Int,hit:Boolean,slice:Int) {
    def words=Vector.tabulate(4)(i=>(data>>(i*64))&((BigInt(1)<<64)-1))
    def bits=(data<<7)|(BigInt(depth)<<5)|(BigInt(source)<<1)|(if(hit)BigInt(1)else BigInt(0))
  }
  case class Result(addr:BigInt,depth:Int,source:Int,hit:Boolean,slice:Int) {
    def bits=(BigInt(slice)<<57)|((addr&mask50)<<7)|(BigInt(depth)<<5)|(BigInt(source)<<1)|(if(hit)BigInt(1)else BigInt(0))
  }
  case class Event(words:Vector[BigInt],depth:Int=0,source:Int=0,hit:Boolean=false,valid:Boolean=true) {
    def data=words.zipWithIndex.map{case(w,i)=>w<<(64*i)}.sum
    def bits=data|(BigInt(depth)<<512)|(BigInt(source)<<514)|(if(hit)BigInt(1)<<518 else BigInt(0))|(if(valid)BigInt(1)<<519 else BigInt(0))
  }
  val empty=Event(Vector.fill(8)(BigInt(0)),valid=false)
  def fold(a:BigInt,width:Int):Int={var x=a;var out=BigInt(0);while(x>0){out^=x&((BigInt(1)<<width)-1);x>>=width};out.toInt}
  def tag(a:BigInt)=fold((a&mask50)>>25,10)
  def indices(a:BigInt):(Int,Int)={val x=a&mask50;val high=x>>25;((((x>>23)&3).toInt^fold(high,2)),(((x>>21)&3).toInt^fold(x>>23,2)))}
  def eligible(e:Event,filtered:Boolean,threshold:Int):Boolean= {
    if(e.hit)(e.source==9 && (e.depth==1||e.depth==3)) || (filtered && Set(1,2,3)(e.source))
    else e.depth<threshold && (!filtered || Set(0,9,4,5)(e.source))
  }
  def choose(valid:Seq[Boolean],last:Int):Option[Int]=(1 to valid.size).map(i=>(last+i)%valid.size).find(valid)
  def validResult(h:Half,word:BigInt,rsp:Int,mode:Int):(Option[Result],Int)={
    val a=word&mask50;val width=Vector(39,48,57)(mode)
    val high=(word>>width)==0;val low=(word&3)==0;val vpn=((word>>12)&511)!=0
    val hits=(0 until 4).filter(w=>((rsp>>(10+w))&1)==1 && ((rsp&1023)+w)%1024==tag(a))
    assert(hits.size<=1)
    val hit=hits.nonEmpty;val hot=hits.headOption.exists(w=>w<2 && ((rsp>>(14+w))&1)==1)
    val can=high&&low&&vpn&&hit&&hot
    val reasons=(if(high)32 else 0)|(if(low)16 else 0)|(if(vpn)8 else 0)|(if(hit)4 else 0)|(if(hot)2 else 0)|(if(can)1 else 0)
    if(high&&low&&vpn&&hit&&hot){
      if((word&4)!=0)cover("bit2-one")
      if(mode==0)cover("Sv39")else if(mode==1)cover("Sv48")else cover("Sv57")
      if(mode==0 && word.testBit(38))cover("Sv39-limit-accept")
      if(mode==1 && word.testBit(47))cover("Sv48-limit-accept")
      if(mode==2 && word.testBit(56))cover("Sv57-limit-accept-and-truncate")
    }
    if(!high)cover("reject-high");if(!low)cover("reject-low");if(!vpn)cover("reject-vpn0");if(!hit)cover("reject-miss");if(hit && !hot)cover("reject-cold")
    val depth=if(h.hit)1 else if(h.depth==0)3 else (h.depth+1)&3
    if(can && depth==3)cover("depth-max");if(can && h.hit)cover("hit-depth1");if(can && !h.hit && h.depth>0)cover("depth-recursion")
    (if(can)Some(Result(a,depth,h.source,h.hit,h.slice))else None,reasons)
  }
  var checkedCycles=0L
  // Default formal run: 1000 independent seeds. Additional configuration has
  // directed recursion and filtered source qualification, without claiming C6.
  for((filtered,threshold,sceneSeeds)<-Seq((false,1,seeds),(true,3,3))) {
    simulate(new CDPC3Harness(filtered,threshold)(params)) {dut=>
      val observations=Seq(dut.io.half)++dut.io.raw.toSeq++Seq(dut.io.out)++dut.io.counts.toSeq++dut.io.degreeCounts.toSeq++
        dut.io.accepted.toSeq++dut.io.dropped.toSeq++dut.io.degreeAccepted.toSeq++dut.io.query.toSeq++dut.io.reasons.toSeq
      val offsets=observations.scanLeft(0)((n,d)=>n+d.getWidth).dropRight(1)
      val slices=observations.zip(offsets).map{case(d,n)=>d->(n,(BigInt(1)<<d.getWidth)-1)}.toMap
      var snapshot=BigInt(0)
      def value(x:UInt):BigInt={val(offset,mask)=slices(x);(snapshot>>offset)&mask}
      def scenario(name:String)(body:((Vector[Event],Boolean,Boolean,Int,Int)=>Unit)=>Unit):Unit={
        val trace=new PrintWriter(traceRoot.resolve((if(filtered)"filtered-"else "default-")+name+".jsonl").toFile)
        trace.println(s"{\"head\":\"$head\",\"filtered\":$filtered,\"threshold\":$threshold,\"scene\":\"$name\"}")
        val halves=Vector.fill(4)(mutable.Queue.empty[Half]);val degrees=Vector.fill(4)(mutable.Queue.empty[Result])
        var lastHalf=0;var lastDegree=0;var s1:Option[Half]=None;var s2:Option[(Half,Vector[Int])]=None
        var s3=Vector.fill[Option[Result]](4)(None)
        val accepted=Array.fill[Long](4)(0);val dropped=Array.fill[Long](4)(0);val scanned=Array.fill[Long](4)(0)
        val offeredDegree=Array.fill[Long](4)(0);val droppedDegree=Array.fill[Long](4)(0);val sentDegree=Array.fill[Long](4)(0)
        val waits=Array.fill(4)(0);var cycle=0
        dut.io.events.foreach(_.poke(0));dut.io.enable.poke(false);dut.io.ready.poke(true);dut.io.mode.poke(0);dut.io.rsp.foreach(_.poke(0))
        dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false)
        def tick(events:Vector[Event],enable:Boolean,ready:Boolean,mode:Int,rspStyle:Int):Unit={
          val rsps=Vector.tabulate(4){i=>s1.map{h=>
            val t=tag(h.words(i));val way=if(rspStyle==4)1 else 0
            val base=(t-way+1024)%1024
            val v=if(rspStyle==1)0 else 1<<way
            val hot=if(rspStyle==2)0 else 1<<way
            base|(v<<10)|(hot<<14)
          }.getOrElse(0)}
          events.zipWithIndex.foreach{case(e,i)=>dut.io.events(i).poke(e.bits)}
          dut.io.enable.poke(enable);dut.io.ready.poke(ready);dut.io.mode.poke(mode)
          rsps.zipWithIndex.foreach{case(r,i)=>dut.io.rsp(i).poke(r)}
          trace.println(s"{\"input_cycle\":$cycle,\"events\":[${events.map(e=>"\""+e.bits.toString(16)+"\"").mkString(",")}],\"enable\":$enable,\"ready\":$ready,\"mode\":$mode,\"rsp\":[${rsps.mkString(",")}]}")
          snapshot=dut.io.packed.peek().litValue
          // Independently verify the packed transport against every raw probe
          // at reset/pipeline startup and regular checkpoints, not just valid data.
          if(cycle<8 || cycle%512==0) {
            observations.foreach(d=>assert(value(d)==d.peek().litValue,s"probe unpack $name/$cycle"))
            cover("probe-unpack-checkpoints")
          }
          val chosenHalf=choose(halves.map(_.nonEmpty),lastHalf)
          val current=chosenHalf.map(i=>halves(i).front)
          val halfBits=current.map(h=>(BigInt(1)<<265)|(BigInt(h.slice)<<263)|h.bits).getOrElse(BigInt(0))
          assert(value(dut.io.half)==halfBits,s"$name cycle=$cycle half got=${value(dut.io.half).toString(16)} expected=${halfBits.toString(16)}")
          for(i<-0 until 4) {
            assert(value(dut.io.counts(i))==halves(i).size)
            assert(value(dut.io.degreeCounts(i))==degrees(i).size)
            val expected=s3(i).map(r=>(BigInt(1)<<59)|r.bits).getOrElse(BigInt(0))
            assert(value(dut.io.raw(i))==expected,s"$name cycle=$cycle lane=$i got=${value(dut.io.raw(i)).toString(16)} expected=${expected.toString(16)}")
            val expectedQuery=s1.map(h=>{val(m,s)=indices(h.words(i));BigInt(16|(m<<2)|s)}).getOrElse(BigInt(0))
            assert(value(dut.io.query(i))==expectedQuery,s"$name cycle=$cycle query lane$i")
          }
          val nextResults=Vector.tabulate(4){i=>s2.map{case(h,r)=>val(out,flags)=validResult(h,h.words(i),r(i),mode)
            assert(value(dut.io.reasons(i))==(64|flags),s"$name cycle=$cycle reasons lane$i");out
          }.getOrElse{assert(value(dut.io.reasons(i))==0);None}}
          val chosenDegree=choose(degrees.map(_.nonEmpty),lastDegree)
          val degreeOut=chosenDegree.map(i=>degrees(i).front)
          val expectedOut=degreeOut.map(r=>(BigInt(1)<<59)|r.bits).getOrElse(BigInt(0))
          assert(value(dut.io.out)==expectedOut,s"$name cycle=$cycle degree got=${value(dut.io.out).toString(16)} expected=${expectedOut.toString(16)}")
          val freeHalf=halves.map(q=>8-q.size);val freeDegree=degrees.map(q=>8-q.size)
          for(i<-0 until 4) {
            if(halves(i).nonEmpty){waits(i)+=1;assert(waits(i)<=4,s"RR unfair slice$i")}
          }
          chosenHalf.foreach{i=>halves(i).dequeue();scanned(i)+=1;lastHalf=i;waits(i)=0;cover("slice"+i)}
          if(ready)chosenDegree.foreach{i=>degrees(i).dequeue();sentDegree(i)+=1;lastDegree=i}
          for(i<-0 until 4) {
            val e=events(i);val admit=e.valid&&enable&&eligible(e,filtered,threshold)
            val added=if(admit)math.min(2,freeHalf(i))else 0
            assert(value(dut.io.accepted(i))==added)
            assert(value(dut.io.dropped(i))==(if(admit)2-added else 0))
            if(admit) {
              for(j<-0 until added)halves(i).enqueue(Half((e.data>>(j*256))&mask256,e.depth,e.source,e.hit,i))
              accepted(i)+=added;dropped(i)+=2-added
              cover("half-accepted",added);cover("half-drop",2-added)
              if(added==1)cover("half-partial")
            } else if(e.valid)cover(if(enable)"entry-filtered"else "entry-disabled")
            val degreeAdded=if(s3(i).nonEmpty)math.min(3,freeDegree(i))else 0
            assert(value(dut.io.degreeAccepted(i))==degreeAdded)
            s3(i).foreach{r=>
              offeredDegree(i)+=3;droppedDegree(i)+=3-degreeAdded
              for(j<-0 until degreeAdded){val a=(r.addr+j*64)&mask50;degrees(i).enqueue(r.copy(addr=a));if((a>>12)!=(r.addr>>12))cover("degree-cross-page")}
              cover("degree-drop",3-degreeAdded);if(degreeAdded==1||degreeAdded==2)cover("degree-partial")
              cover("raw-candidate")
            }
          }
          trace.println(s"{\"cycle\":$cycle,\"events\":[${events.map(e=>"\""+e.bits.toString(16)+"\"").mkString(",")}],\"enable\":$enable,\"ready\":$ready,\"mode\":$mode,\"rsp\":[${rsps.mkString(",")}],\"half\":\"${halfBits.toString(16)}\",\"out\":\"${expectedOut.toString(16)}\",\"raw\":[${dut.io.raw.map(value).mkString(",")}],\"query\":[${dut.io.query.map(value).mkString(",")}]}")
          dut.clock.step();s2=s1.map(h=>(h,rsps));s1=current;s3=nextResults
          cycle+=1;checkedCycles+=1
        }
        try {
          body(tick)
          for(_<-0 until 96)tick(Vector.fill(4)(empty),false,true,2,0)
          assert(halves.forall(_.isEmpty)&&degrees.forall(_.isEmpty)&&s1.isEmpty&&s2.isEmpty&&s3.forall(_.isEmpty))
          for(i<-0 until 4){assert(accepted(i)==scanned(i));assert(offeredDegree(i)==droppedDegree(i)+sentDegree(i))}
          cover("scenes");trace.println(s"{\"closed\":true,\"cycles\":$cycle}")
        } finally trace.close()
      }
      val pointers=Vector.tabulate(8)(i=>BigInt(0x12fc4)+i*0x1000)
      scenario("directed"){tick=>
        // All lanes incl. second half, bit2=1, degree crosses a 4K boundary.
        tick(Vector(Event(pointers),empty,empty,empty),true,true,0,0)
        for(_<-0 until 24)tick(Vector.fill(4)(empty),true,true,0,0)
        // Saturate every bank and every degree queue; one-free-slot dual input.
        for(_<-0 until 40)tick(Vector.fill(4)(Event(pointers)),true,false,0,0)
        for(_<-0 until 80)tick(Vector.fill(4)(empty),false,true,0,0)
        for(style<-Seq(1,2,4);mode<-0 until 3) {
          tick(Vector(Event(pointers),empty,empty,empty),true,true,mode,style)
          for(_<-0 until 12)tick(Vector.fill(4)(empty),true,true,mode,style)
        }
        val boundaries=Vector(BigInt(0),BigInt(0x200000),BigInt(0x12345),BigInt(0x12004),
          (BigInt(1)<<39)|0x12000,(BigInt(1)<<48)|0x12000,(BigInt(1)<<57)|0x12000,(BigInt(1)<<63)|0x12000)
        for(mode<-0 until 3;depth<-0 to 3;source<-Seq(0,1,2,3,4,5,9);hit<-Seq(false,true)) {
          tick(Vector(Event(boundaries,depth,source,hit),empty,empty,empty),true,true,mode,0)
          for(_<-0 until 5)tick(Vector.fill(4)(empty),true,true,mode,0)
        }
        val exactLimits=Vector(38,39,47,48,56,57,63).map(bit=>(BigInt(1)<<bit)|BigInt(0x12fc4)) :+ BigInt(0x12fc4)
        for(mode<-0 until 3) {
          tick(Vector(Event(exactLimits),empty,empty,empty),true,true,mode,0)
          for(_<-0 until 24)tick(Vector.fill(4)(empty),true,true,mode,0)
        }
        // Consecutive line arrivals preserve high-half data and old metadata.
        for(i<-0 until 8)tick(Vector(Event(pointers.map(_+i*64),if(i%2==0)0 else 3,if(i%2==0)0 else 9,i%2!=0),empty,empty,empty),true,true,0,0)
        for(_<-0 until 60)tick(Vector.fill(4)(empty),false,true,0,0)
      }
      for(seed<-0 until sceneSeeds)scenario("seed-"+seed){tick=>
        val r=new Random(seed.toLong+0xc300)
        for(_<-0 until randomCycles) {
          val events=Vector.tabulate(4){_=>
            val ws=Vector.tabulate(8){_=>
              val base=BigInt(0x12000+r.nextInt(32)*4096+r.nextInt(1024)*4)
              r.nextInt(12) match {
                case 0=>base|1;case 1=>base& ~BigInt(0x1ff000)
                case 2=>base|(BigInt(1)<<39);case 3=>base|(BigInt(1)<<48)
                case 4=>base|(BigInt(1)<<57);case 5=>base|(BigInt(1)<<63)
                case _=>base
              }
            }
            Event(ws,if(r.nextInt(3)==0)r.nextInt(4)else 0,if(r.nextBoolean())9 else r.nextInt(10),r.nextInt(4)==0,r.nextInt(3)!=0)
          }
          tick(events,r.nextInt(12)!=0,r.nextInt(4)!=0,r.nextInt(3),r.nextInt(5))
        }
        cover("random-seeds");cover("random-input-cycles",randomCycles)
      }
    }
  }
  for(name<-Seq("half-drop","half-partial","degree-drop","degree-partial","degree-cross-page","bit2-one","depth-max","hit-depth1","depth-recursion","reject-high","reject-low","reject-vpn0","reject-miss","reject-cold","Sv39","Sv48","Sv57","Sv39-limit-accept","Sv48-limit-accept","Sv57-limit-accept-and-truncate","slice0","slice1","slice2","slice3"))require(total(name)>0,s"missing $name")
  total.toSeq.sortBy(_._1).foreach{case(k,v)=>println(s"CDP_C3_COVER $k=$v")}
  println(s"CDP_C3_RESULT pass=true seeds=$seeds cycles=$checkedCycles mismatches=0 dependency=explicit-accepted-count-adapter")
}
