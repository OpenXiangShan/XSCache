package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.l2._
import oceanus.{cdpreference=>ref}
import org.chipsalliance.cde.config._
import utility._
import java.nio.file.{Files,Paths}

// Test-only real Filter tables receive the same committed train write. The
// production sender uses reference requery; ReferenceCDP remains verbatim.
class CDPH1Harness(implicit p:Parameters) extends CDPC4OrderingHarness(false) {
  val training=IO(new Bundle {
    val live=Input(Bool());val valid=Input(Bool());val saturation=Input(UInt(2.W))
    val set=Input(UInt(6.W));val tag=Input(UInt(24.W));val offset=Input(UInt(6.W));val ready=Output(Bool())
  })
  val actualFilter=Module(new OceanusCDPFilterTable)
  val referenceFilter=Module(new ref.FilterTable)
  actualFilter.io.queryReq.valid:=training.live && prod.io.ftQueryReq.valid && io.queryReady
  actualFilter.io.queryReq.bits:=prod.io.ftQueryReq.bits
  referenceFilter.io.queryReq.valid:=training.live && reference.io.ftQueryReq.valid && io.queryReady
  referenceFilter.io.queryReq.bits:=reference.io.ftQueryReq.bits
  actualFilter.io.trainReq.valid:=training.live && training.valid
  actualFilter.io.trainReq.bits:=0.U.asTypeOf(actualFilter.io.trainReq.bits)
  actualFilter.io.trainReq.bits.set:=training.set;actualFilter.io.trainReq.bits.tag:=training.tag
  // A legal FtTrain hit changes only its page offset, preserving all other
  // counters. The setup writes correspond to miss-unused(3), then used(2).
  actualFilter.io.trainReq.bits.sat.zipWithIndex.foreach {case(counter,i)=>
    counter:=Mux(training.offset===i.U,training.saturation,2.U)
  }
  referenceFilter.io.trainReq.valid:=actualFilter.io.trainReq.valid
  referenceFilter.io.trainReq.bits:=actualFilter.io.trainReq.bits.asTypeOf(referenceFilter.io.trainReq.bits)
  training.ready:=actualFilter.io.trainReq.ready && referenceFilter.io.trainReq.ready
  when(training.live) {
    prod.io.ftQueryReq.ready:=actualFilter.io.queryReq.ready && io.queryReady
    prod.io.ftQueryRsp:=actualFilter.io.queryRsp.bits
    reference.io.ftQueryReq.ready:=referenceFilter.io.queryReq.ready && io.queryReady
    reference.io.ftQueryRsp:=referenceFilter.io.queryRsp.bits
    assert(actualFilter.io.trainReq.ready===referenceFilter.io.trainReq.ready)
  }
}
object TestTop_OceanusCDPH1 extends App with ChiselSim {
  Constantin.init(false)
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=HasSimulator.default
  val dest=Paths.get(sys.props("oceanus.h1.trace"));Files.createDirectories(dest)
  simulate(new CDPH1Harness()(params)) { dut=>
    val model=new CDPOrderingTracker(dut,dest)
    def initialise(name:String,fixed:Boolean,live:Boolean=false):Unit={
      dut.io.fixed.poke(fixed);dut.io.candidate.poke(0);dut.io.gate.poke(false);dut.io.delay.poke(1)
      dut.io.ready.poke(false);dut.io.queryReady.poke(false);dut.io.filter.poke(false)
      dut.io.vbop.poke(false);dut.io.vbopVA.poke(0)
      dut.training.live.poke(live);dut.training.valid.poke(false);dut.training.saturation.poke(2)
      val physical=(BigInt(0x30)^BigInt("123456",16))<<12
      dut.training.set.poke((physical>>18)&63);dut.training.tag.poke(physical>>24);dut.training.offset.poke((physical>>12)&63)
      dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false);dut.clock.step(192)
      if(live){
        dut.training.saturation.poke(3);dut.training.valid.poke(true)
        assert(dut.training.ready.peek().litToBoolean);dut.clock.step();dut.training.valid.poke(false);dut.clock.step(8)
        dut.training.saturation.poke(2);dut.training.valid.poke(true)
        assert(dut.training.ready.peek().litToBoolean);dut.clock.step();dut.training.valid.poke(false)
        println("CDP_H1_FILTER_SETUP legalSingleOffsetWrites=true allocationUnused=3 used=2 concurrentUnused=3")
      }
      model.reset(name)
    }
    def tick(c:Int,va:Option[BigInt],depth:Int,gate:Boolean,delay:Int,ready:Boolean,live:Boolean=false):Unit={
      val candidate=va.map(a=>(BigInt(1)<<57)|(a<<7)|(BigInt(depth)<<5)|(BigInt(9)<<1)).getOrElse(BigInt(0))
      dut.io.candidate.poke(candidate);dut.io.gate.poke(gate);dut.io.delay.poke(delay)
      dut.io.ready.poke(ready);dut.io.queryReady.poke(true)
      val train=live && c==70
      dut.training.valid.poke(train);dut.training.saturation.poke(if(live && c>=70)3 else 2)
      if(train)assert(dut.training.ready.peek().litToBoolean)
      model.sample(candidate,ready,false,!train,if(live)Some((_:BigInt)=>c<=70)else None)
      dut.clock.step()
    }
    def finish(name:String,expectedEqual:Boolean=true):Boolean={
      val actual=model.sides.map(_.observed.toVector).toVector
      val equal=actual(0).sorted==actual(1).sorted
      model.finish(actual,compareSet=expectedEqual,save=true)
      assert(equal==expectedEqual)
      val different=actual(0)!=actual(1)
      println(s"CDP_H1_DIRECTED scene=$name independentOrderChecked=true multisetEqual=$equal orderDifferent=$different strictComparatorPassed=${equal && !different} candidateOrderModelPassed=true contractApproval=${if(equal)"D5-accepted-reordering"else "dynamic-filter-pending"}")
      different
    }
    // Fixed translation timing, common static Filter, only output backpressure.
    // End with continuous ready: reference requery repeats every three cycles
    // and can otherwise phase-lock against a period-three ready pattern.
    initialise("h1-output-backpressure",fixed=true)
    for(c<-0 until 256)tick(c,if(c<16)Some(BigInt(0x30000)+c*64)else None,c%4,c>=32,1,c>=64 && c%3!=0)
    assert(model.sides.forall(s=>s.live.size==1 && s.live.contains(4)),
      "period-three prefix no longer demonstrates the reference requery phase lock")
    val stalled=model.sides(1).live(4)
    assert(model.sides(1).records(stalled.id)("queries").arr.takeRight(5).forall(q=>(q(0).num.toInt+2)%3==0))
    println("CDP_H1_PHASE_LOCK observed=true originalPrefixCycles=256 dutLive=1 referenceLive=1 tailReadyCycles=32")
    for(c<-256 until 288)tick(c,None,0,true,1,true)
    assert(!finish("output-backpressure"),"shared requery mechanism reordered fixed-translation outputs")
    for((id,d)<-model.sides(0).records)assert(d("qualificationCycle")==model.sides(1).records(id)("qualificationCycle"),"backpressure fixture changed translation timing")
    initialise("h1-fixed-ready-control",fixed=true)
    for(c<-0 until 256)tick(c,if(c<16)Some(BigInt(0x30000)+c*64)else None,c%4,c>=32,1,true)
    assert(!finish("fixed-ready-control"),"fixed ready control reordered outputs")
    // Output is always ready; only the DUT translation response latency varies.
    var translationDifferences=0
    for(d<-1 to 7){
      initialise(s"h1-translation-$d",fixed=false)
      val pages=Vector(0x30,0x3b,0x3c,0x3d,0x3e,0x3f,0x40,0x4b)
      for(c<-0 until 256)tick(c,if(c<16)Some((BigInt(pages(c%8))<<12)+(c/8)*64)else None,c%4,c>=32,1+(c*d)%7,true)
      if(finish(s"translation-$d"))translationDifferences+=1
    }
    assert(translationDifferences>0)
    // Concurrent real table train, same fixed translation, common ready. A
    // saturated write after queries but before output fire must be re-queried
    // by both senders while stalled, producing the same four rejections.
    initialise("h1-filter-training",fixed=true,live=true)
    for(c<-0 until 256)tick(c,if(c<4)Some(BigInt(0x30000)+c*64)else None,c%4,c>=32,1,c>=110,live=true)
    finish("filter-training")
    assert(model.sides.forall(_.observed.isEmpty))
    assert(model.sides(0).records.values.count(_.obj.contains("filterDropCycle"))==4)
    assert(model.sides(1).records.values.count(_.obj.contains("filterDropCycle"))==4)
    for((id,d)<-model.sides(0).records)assert(d("qualificationCycle")==model.sides(1).records(id)("qualificationCycle"))
    // Random concurrent legal committed Filter writes. Counts use input IDs,
    // including PA/depth, so timing/reordering alone cannot create a difference.
    assert(scala.util.Try(CDPD5Policy.cause(false,false)).isFailure)
    assert(CDPD5Policy.cause(true,false)=="translation")
    assert(scala.util.Try(CDPD5Policy.cause(false,true)).isFailure)
    assert(scala.util.Try(CDPD5Policy.cause(true,true)).isFailure)
    val randomSeeds=args.find(_.startsWith("--filter-seeds=")).map(_.split("=")(1).toInt).getOrElse(1000)
    var dutTotal=0L;var refTotal=0L;var unionTotal=0L;var extraTotal=0L;var missingTotal=0L;var changed=0
    def measure(name:String,seed:Int):ujson.Obj={
      val outs=model.sides.map(_.observed.toVector)
      val extra=outs(0).diff(outs(1));val missing=outs(1).diff(outs(0));val union=outs(0).size+missing.size
      ujson.Obj("label"->name,"seed"->seed,"dutOutputs"->outs(0).size,"referenceOutputs"->outs(1).size,
        "unionOutputs"->union,"dutExtra"->extra.size,"dutMissing"->missing.size,
        "differentFractionOfUnion"->(if(union==0)0.0 else (extra.size+missing.size).toDouble/union),
        "extraIdentities"->ujson.Arr.from(extra),"missingIdentities"->ujson.Arr.from(missing))
    }
    val directed=measure("filter-training",-1)
    Files.writeString(dest.resolve("filter-directed-summary.json"),directed.render(2))
    val randomRows=ujson.Arr()
    for(seed<-0 until randomSeeds){
      // Mix adjacent seed IDs before the first bounded draw.
      val random=new scala.util.Random(new scala.util.Random(0xD50000L+seed).nextLong())
      val count=1+random.nextInt(16);val gate=24+random.nextInt(17);val delay=1+random.nextInt(7)
      val train=gate+2+random.nextInt(55);val release=random.nextInt(120);val fixed=random.nextBoolean()
      initialise(s"h1-filter-random-$seed",fixed=fixed,live=true)
      for(c<-0 until 256){
        val va=if(c<count)Some(BigInt(0x30000)+c*64)else None
        val encoded=va.map(a=>(BigInt(1)<<57)|(a<<7)|(BigInt(c%4)<<5)|(BigInt(9)<<1)).getOrElse(BigInt(0))
        val ready=c>=128 || (c>=release && random.nextInt(4)!=0)
        dut.io.candidate.poke(encoded);dut.io.gate.poke(c>=gate);dut.io.delay.poke(delay)
        dut.io.ready.poke(ready);dut.io.queryReady.poke(true)
        dut.training.valid.poke(c==train);dut.training.saturation.poke(if(c>=train)3 else 2)
        if(c==train)assert(dut.training.ready.peek().litToBoolean)
        model.sample(encoded,ready,false,c!=train,Some((_:BigInt)=>c<=train))
        dut.clock.step()
      }
      val actual=model.sides.map(_.observed.toVector).toVector
      model.finish(actual,compareSet=false,save=true)
      for(i<-0 to 1){
        val filtered=model.sides(i).records.values.count(_.obj.contains("filterDropCycle"))
        assert(model.sides(i).records.size==count && actual(i).size+filtered==count,"dynamic Filter identity/count conservation")
      }
      val row=measure(s"h1-filter-random-$seed",seed)
      row("fixedTranslation")=ujson.Bool(fixed);row("candidateCount")=ujson.Num(count)
      row("gateCycle")=ujson.Num(gate);row("trainCycle")=ujson.Num(train);row("releaseCycle")=ujson.Num(release)
      randomRows.arr+=row;dutTotal+=actual(0).size;refTotal+=actual(1).size
      unionTotal+=row("unionOutputs").num.toLong;extraTotal+=row("dutExtra").num.toLong;missingTotal+=row("dutMissing").num.toLong
      if(row("dutExtra").num+row("dutMissing").num>0)changed+=1
      if(seed%100==99)println(s"CDP_H1_FILTER_PROGRESS seeds=${seed+1} dut=$dutTotal reference=$refTotal extra=$extraTotal missing=$missingTotal")
    }
    val fraction=if(unionTotal==0)0.0 else (extraTotal+missingTotal).toDouble/unionTotal
    val summary=ujson.Obj("seeds"->randomSeeds,"changedScenarios"->changed,"dutOutputs"->ujson.Num(dutTotal.toDouble),"referenceOutputs"->ujson.Num(refTotal.toDouble),
      "unionOutputs"->ujson.Num(unionTotal.toDouble),"dutExtra"->ujson.Num(extraTotal.toDouble),"dutMissing"->ujson.Num(missingTotal.toDouble),"differentFractionOfUnion"->fraction,
      "differentFractionOfBothOutputCounts"->(if(dutTotal+refTotal==0)0.0 else (extraTotal+missingTotal).toDouble/(dutTotal+refTotal)),
      "threshold"->0.01,"thresholdExceeded"->(fraction>0.01 || directed("differentFractionOfUnion").num>0.01),
      "directed"->directed,"random"->randomRows,"RTLchanged"->true,"comparisonKey"->"inputID:PA:depth; multiset difference",
      "stimulusScope"->"real FilterTable concurrent committed legal single-offset used/unused writes; FtTrain upstream not instantiated")
    Files.writeString(dest.resolve("filter-quantification.json"),summary.render(2))
    println(s"CDP_H1_FILTER_QUANTIFIED seeds=$randomSeeds dut=$dutTotal reference=$refTotal extra=$extraTotal missing=$missingTotal union=$unionTotal fraction=$fraction")
    println(s"CDP_H1_OBSERVATIONS verified=true groups=3 translationCases=7 translationDifferences=$translationDifferences filterDutOutputs=0 filterReferenceOutputs=0 RTLchanged=true D5translationOnly=true residualDispositionsRequireExplanation=true")
  }
}
