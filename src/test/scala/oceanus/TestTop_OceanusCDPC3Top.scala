package oceanus
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.l2._
import oceanus.chi._
import oceanus.{cdpreference=>ref}
import org.chipsalliance.cde.config._
import utility._

class CDPC3TopHarness(implicit p:Parameters) extends Module {
  val io=IO(new Bundle {
    val train=Input(Bool());val va=Input(UInt(50.W));val trigger=Input(UInt(520.W));val bank=Input(UInt(2.W))
    val outputs=Output(UInt(32.W));val sent=Output(UInt(32.W))
  })
  val rp=p.alterPartial{case ref.ReferenceCDPKey=>ref.CDPParameters(useFilterTable=false);case ref.ReferenceCDPBankBitsKey=>2}
  val prod=Module(new OceanusCDP(OceanusCDPParameters(useFilterTable=false),4))
  val reference=Module(new ref.CDPPrefetcher()(rp))
  prod.io.enable:=true.B;reference.io.enable:=true.B
  prod.io.vpnTrain.valid:=io.train;prod.io.vpnTrain.bits:=0.U.asTypeOf(prod.io.vpnTrain.bits)
  prod.io.vpnTrain.bits.vaddr:=io.va(49,6);prod.io.vpnTrain.bits.vpnValid:=true.B;prod.io.vpnTrain.bits.trainEnable:=true.B
  reference.io.vpnTrain.valid:=io.train;reference.io.vpnTrain.bits:=0.U.asTypeOf(reference.io.vpnTrain.bits)
  reference.io.vpnTrain.bits.vaddr.get:=io.va(49,6)
  prod.io.filterTrain.valid:=false.B;prod.io.filterTrain.bits:=0.U.asTypeOf(prod.io.filterTrain.bits)
  reference.io.filterTrain.valid:=false.B;reference.io.filterTrain.bits:=0.U.asTypeOf(reference.io.filterTrain.bits)
  prod.io.pfStat:=0.U.asTypeOf(prod.io.pfStat);reference.io.pfStat:=0.U.asTypeOf(reference.io.pfStat)
  prod.io.tlb.req.ready:=true.B;prod.io.tlb.resp.valid:=false.B;prod.io.tlb.resp.bits:=0.U.asTypeOf(prod.io.tlb.resp.bits)
  reference.io.tlbReq.req.ready:=true.B;reference.io.tlbReq.resp.valid:=false.B
  reference.io.tlbReq.resp.bits:=0.U.asTypeOf(reference.io.tlbReq.resp.bits);reference.io.tlbReq.pmp_resp:=0.U.asTypeOf(reference.io.tlbReq.pmp_resp)
  prod.io.req.ready:=true.B;reference.io.pftReq.ready:=true.B
  for(i<-0 until 4) {
    val t=prod.io.l2DetectTriggers(i)
    t.valid:=io.trigger(519)&&io.bank===i.U
    t.bits.data:=io.trigger(511,0);t.bits.pfDepth:=io.trigger(513,512)
    t.bits.pfSource:=io.trigger(517,514);t.bits.isHit:=io.trigger(518)
    reference.io.l2DetectTriggers(i).valid:=t.valid
    reference.io.l2DetectTriggers(i).bits:=t.bits.asTypeOf(reference.io.l2DetectTriggers(i).bits)
  }
  def read[T<:Data](d:T):T=BoringUtils.bore(d)
  val count=RegInit(0.U(32.W));val sent=RegInit(0.U(32.W))
  val rv=read(reference.detectTrigArb.io.out.valid)
  val v1=RegNext(rv,false.B);val v2=RegNext(v1,false.B)
  val s1=RegEnable(read(reference.detectTrigArb.io.chosen),rv)
  val s2=RegEnable(s1,v1);val s3=RegEnable(s2,v2)
  val active=Wire(Vec(4,Bool()))
  for(i<-0 until 4) {
    val d=read(prod.detectFrontend.raw)(i);val r=read(reference.detectPipeSeq(i).io.pftReq)
    active(i):=d.valid
    when(!reset.asBool) {
      assert(d.valid===r.valid,"C3 actual top vs unadapted ReferenceCDP detection valid")
      when(d.valid) {
        assert(Cat(d.bits.pfVAddr,d.bits.pfDepth,d.bits.pfSource,d.bits.isHit)===r.bits.asUInt)
        assert(d.bits.slice===s3)
      }
    }
  }
  val doutValid=read(prod.c4Input.valid);val doutBits=read(prod.c4Input.bits)
  val routValid=read(reference.degreeBufArb.io.out.valid);val routBits=read(reference.degreeBufArb.io.out.bits)
  when(!reset.asBool) {
    assert(!prod.io.req.valid && !prod.io.tlb.req.valid,"C3 must leave external request/TLB inactive")
    assert(doutValid===routValid)
    when(doutValid){assert(Cat(doutBits.pfVAddr,doutBits.pfDepth,doutBits.pfSource,doutBits.isHit)===routBits.asUInt)}
  }
  count:=count+PopCount(active)
  when(doutValid && read(prod.c4Input.ready)){sent:=sent+1.U}
  io.outputs:=count;io.sent:=sent
}
object TestTop_OceanusCDPC3Top extends App with ChiselSim {
  Constantin.init(false)
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params()
    case CHIParametersKey=>CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case xscache.oceanus.compactchi.CCHIParametersKey=>xscache.oceanus.compactchi.CCHIParameters()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=HasSimulator.default
  simulate(new CDPC3TopHarness()(params)){dut=>
    dut.io.train.poke(false);dut.io.va.poke(0x12000);dut.io.trigger.poke(0);dut.io.bank.poke(0)
    dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false);dut.clock.step(4)
    // True training builds a hot VPN, rather than forcing table state.
    for(_<-0 until 130){dut.io.train.poke(true);dut.clock.step();dut.io.train.poke(false);dut.clock.step(6)}
    dut.clock.step(32)
    val words=Vector.tabulate(8)(i=>BigInt(0x12fc4)+i*0x1000)
    val data=words.zipWithIndex.map{case(w,i)=>w<<(64*i)}.sum
    for(bank<-0 until 4;hit<-Seq(false,true)) {
      dut.io.bank.poke(bank)
      val e=data|(if(hit)(BigInt(3)<<512)|(BigInt(9)<<514)|(BigInt(1)<<518)else BigInt(0))|(BigInt(1)<<519)
      dut.io.trigger.poke(e);dut.clock.step();dut.io.trigger.poke(0);dut.clock.step(40)
    }
    dut.io.outputs.expect(64);dut.io.sent.expect(192)
    println("CDP_C3_TOP_RESULT pass=true raw_candidates=64 c4_inputs=192 raw_reference=unmodified hot_training=130 slices=4")
  }
}
