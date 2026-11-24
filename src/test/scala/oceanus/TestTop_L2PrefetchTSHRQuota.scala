package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.l2._
import oceanus.compactchi.{FlitREQ,CCHIOpcode}
import org.chipsalliance.cde.config._
import utility._

// Real production demand-priority entrance and allocator, with an explicit
// host model of TSHR lifetime/REQ availability. Refills are intentionally held.
class PrefetchQuotaHarness(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val demand = Flipped(Decoupled(new FlitREQ))
    val prefetch = Flipped(Decoupled(new FlitREQ))
    val slots = Input(Vec(nMSHR,Valid(new L2TSHRAlloc.PathFromTSHR)))
    val allocation = Output(Vec(nMSHR,new L2TSHRAlloc.PathToTSHR))
  })
  val allocator = Module(new L2TSHRAlloc(new L2TSHRAllocConfig(
    Seq(Seq(L2TSHRAllocTarget.EVT),Seq(L2TSHRAllocTarget.SNP,L2TSHRAllocTarget.EVB,L2TSHRAllocTarget.REQ)),
    Seq((nMSHR-1,L2TSHRResvTarget.L2EVB),(nMSHR-2,L2TSHRResvTarget.L1EVT),(nMSHR-3,L2TSHRResvTarget.L3SNP)))))
  val entrance = Module(new Arbiter(new FlitREQ,2))
  entrance.io.in(0) <> io.demand;entrance.io.in(1) <> io.prefetch
  allocator.io.fromTSHRCtrl.RXREQ <> entrance.io.out
  allocator.io.pfSrcID.foreach(_ := 2.U)
  allocator.io.fromTSHRCtrl.RXEVT.valid := false.B
  allocator.io.fromTSHRCtrl.RXEVT.bits := 0.U.asTypeOf(allocator.io.fromTSHRCtrl.RXEVT.bits)
  allocator.io.fromTSHRCtrl.RXSNP.valid := false.B
  allocator.io.fromTSHRCtrl.RXSNP.bits := 0.U.asTypeOf(allocator.io.fromTSHRCtrl.RXSNP.bits)
  allocator.io.fromTSHRCtrl.RXEVB.valid := false.B
  allocator.io.fromTSHRCtrl.RXEVB.bits := 0.U.asTypeOf(allocator.io.fromTSHRCtrl.RXEVB.bits)
  allocator.io.fromTSHR := io.slots
  io.allocation := allocator.io.toTSHR
  XSLog.collect(0.U(64.W),false.B,false.B,false.B)
}
object TestTop_L2PrefetchTSHRQuota extends App with ChiselSim {
  Constantin.init(false)
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  val limit = args.headOption.map(_.toInt).getOrElse(4)
  val tshrs = args.lift(1).map(_.toInt).getOrElse(32)
  val shared = tshrs - 3
  require(shared > 0)
  val cap = if(limit<0) None else Some(limit)
  val p=new Config((_,_,_)=>{
    case L2ParamsKey => L2Params(mshrSize=tshrs,ways=8,sets=1024,prefetchMaxSharedTSHRs=cap)
    case oceanus.chi.CHIParametersKey => oceanus.chi.CHIParameters(issue=oceanus.chi.EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case xscache.oceanus.compactchi.CCHIParametersKey => xscache.oceanus.compactchi.CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  simulate(new PrefetchQuotaHarness()(p)) {dut=>
    val addresses=Array.fill[Option[BigInt]](tshrs)(None)
    def flag(x:Bool)=x.peek().litToBoolean
    def idle():Unit={
      dut.io.demand.valid.poke(false.B);dut.io.prefetch.valid.poke(false.B)
      dut.io.demand.bits.poke(0.U.asTypeOf(dut.io.demand.bits));dut.io.prefetch.bits.poke(0.U.asTypeOf(dut.io.prefetch.bits))
      for(i<-0 until tshrs) {dut.io.slots(i).valid.poke(addresses(i).nonEmpty.B);dut.io.slots(i).bits.poke(0.U.asTypeOf(dut.io.slots(i).bits));dut.io.slots(i).bits.paddr.poke(addresses(i).getOrElse(BigInt(0)).U)}
    }
    def drive(port:DecoupledIO[FlitREQ],pa:BigInt,pf:Boolean):Unit={
      port.valid.poke(true.B);port.bits.Addr.poke(pa.U);port.bits.SrcID.poke((if(pf)2 else 0).U)
      port.bits.TxnID.poke((if(pf)0x90 else 1).U);port.bits.Opcode.poke((if(pf)CCHIOpcode.StashShared.opcode else CCHIOpcode.ReadShared.opcode).U)
    }
    def step(pf:Option[BigInt]=None,demand:Option[BigInt]=None,expectedPF:Boolean=false,expectedDemand:Boolean=false):Option[Int]={
      idle();pf.foreach(drive(dut.io.prefetch,_,true));demand.foreach(drive(dut.io.demand,_,false))
      assert((flag(dut.io.prefetch.ready)&&pf.nonEmpty)==expectedPF,s"PF quota ready limit=$limit pf=$pf")
      assert((flag(dut.io.demand.ready)&&demand.nonEmpty)==expectedDemand,s"demand progress limit=$limit")
      val allocated=(0 until tshrs).filter(i=>flag(dut.io.allocation(i).alloc.REQ))
      val reused=(0 until tshrs).filter(i=>flag(dut.io.allocation(i).reuse.REQ))
      assert(allocated.size+reused.size==(if(expectedPF||expectedDemand)1 else 0))
      assert((allocated++reused).forall(_<shared),"REQ used reserved TSHR")
      val pa=if(expectedDemand)demand else if(expectedPF)pf else None
      dut.clock.step();allocated.foreach(i=>addresses(i)=pa);(allocated++reused).headOption
    }
    idle();dut.reset.poke(true.B);dut.clock.step(5);dut.reset.poke(false.B)
    val budget=cap.getOrElse(shared)
    val slots=(0 until budget).map(i=>step(pf=Some(BigInt(0x10000+i*64)),expectedPF=true).get)
    step(pf=Some(0x20000))
    if(budget<shared) {
      val demandSlot=step(pf=Some(0x20000),demand=Some(0x30000),expectedDemand=true).get
      // A new prefetch must not bypass the cap by reusing a demand-origin slot.
      step(pf=Some(0x30000))
      if(budget>0) {
        step(pf=Some(0x10000),expectedPF=true) // owned same-PA reuse costs zero
        step(demand=Some(0x10000),expectedDemand=true)
        addresses(slots.head)=None
        step(pf=Some(0x30000),expectedPF=true) // reuse consumes the freed quota
        step(pf=Some(0x20000))
        addresses(demandSlot)=None
        step(pf=Some(0x20000),expectedPF=true)
      }
    }
    if(cap.isEmpty) {
      assert(slots == (0 until shared))
      step(demand=Some(0x30000))
      assert(step(pf=Some(0x10000),expectedPF=true).contains(slots.head))
      assert(step(demand=Some(0x10000),expectedDemand=true).contains(slots.head))
      idle();dut.io.slots(slots.head).bits.busy.REQ.poke(true.B)
      drive(dut.io.demand,0x10000,false)
      assert(!flag(dut.io.demand.ready))
      assert(!dut.io.allocation.exists(slot=>flag(slot.alloc.REQ)||flag(slot.reuse.REQ)))
      dut.clock.step()
      for(reserved<-shared until tshrs) {
        val pa=BigInt(0x40000+reserved*64)
        addresses(reserved)=Some(pa)
        step(pf=Some(pa))
        step(demand=Some(pa))
        addresses(reserved)=None
      }
      addresses(slots.last)=None
      assert(step(demand=Some(0x30000),expectedDemand=true).contains(slots.last))
    }
    println(s"PREFETCH_TSHR_QUOTA_RESULT pass=true limit=$limit tshrs=$tshrs shared=$shared pfHeld=$budget demandProgress=${budget<shared} ownedReuse=${budget>0 && budget<shared} demandReuseCounted=${budget>0 && budget<shared} defaultUnlimited=${cap.isEmpty} capacityReuseReservedChecks=${cap.isEmpty}")
  }
}
