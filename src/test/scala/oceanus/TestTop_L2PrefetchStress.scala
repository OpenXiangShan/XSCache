package oceanus

// Overnight-only test code: real requests, refills, training and source queues.
// No forced algorithm output, directory metadata or RTL changes.
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.{CCHIOpcode, FlitREQ}
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable

object StressMemory {
  var vaMask: BigInt = 0
  // Read-only byte pattern with a canonical, nonzero pointer in word zero.
  // The same expected function is used by the demand scoreboard, including hits.
  def beat(addr: BigInt, beat: Int): BigInt = {
    val data = L2TopReferenceMemory.beat(addr, beat)
    if(beat == 0) (data >> 64 << 64) | (((addr + 0x1000) ^ vaMask) & ((BigInt(1)<<48)-1)) else data
  }
}

// Intent: census all real queues, retain per-item FIFO identities and observe
// terminal state without resetting, flushing or forcing the production RTL.
class StressQueueProbe(width: Int) extends Bundle {
  val enq = Valid(UInt(width.W)); val deq = Valid(UInt(width.W))
  val overwrite = Bool(); val occupancy = UInt(16.W)
}
class TestTopStress(cfg: L2Configuration)(implicit p: Parameters) extends StressTopEvents(cfg) {
  def children(m: chisel3.experimental.BaseModule): Seq[chisel3.experimental.BaseModule] =
    m.getClass.getMethods.find(_.getName == "getIds").get.invoke(m)
      .asInstanceOf[scala.collection.Seq[AnyRef]].collect {
        case child: chisel3.experimental.BaseModule => child
      }.toSeq
  val metrics = mutable.ArrayBuffer.empty[(String, UInt, Int)]
  def add(name: String, signal: UInt, cap: Int = 1): Unit = metrics += ((name,read(signal),cap))
  val fifos = mutable.ArrayBuffer.empty[(String, Int, StressQueueProbe)]
  def census(m: chisel3.experimental.BaseModule, path: String): Unit = {
    m match {
      case q: Queue[_] => add(path+".count", q.io.count, q.entries)
      case q: OverwriteQueue[_] =>
        val cap=q.data.length
        val obs=IO(Output(new StressQueueProbe(q.io.enq.bits.getWidth))).suggestName(s"fifoProbe${fifos.size}")
        obs.enq.valid := read(q.io.enq.valid) && read(q.io.enq.ready)
        obs.enq.bits := read(q.io.enq.bits).asUInt
        obs.deq.valid := read(q.io.deq.valid) && read(q.io.deq.ready)
        obs.deq.bits := read(q.io.deq.bits).asUInt
        obs.overwrite := read(q.io.overwrite)
        val h=read(q.head); val t=read(q.tail)
        obs.occupancy := Mux(read(q.full),cap.U,(t-h)(h.getWidth-1,0))
        fifos += ((path,cap,obs)); add(path+".count",obs.occupancy,cap)
      case q: OceanusCDPQueue[_] => add(path+".count",q.io.count,8)
      case q: L2CDPHalfQueue => add(path+".count",q.io.occupancy,8)
      case _ =>
    }
    children(m).zipWithIndex.foreach { case(c,i)=>census(c,path+"/"+c.getClass.getSimpleName+i) }
  }
  census(module,"L2Top")
  val a=module.pfTlbAdapter.get
  // Independent host service-age model sees eligibility and actual space.
  val tlbServicePacked=IO(Output(UInt(13.W)))
  tlbServicePacked := Cat(read(a.io.tlb.req.valid) && read(a.io.tlb.req.ready),
    read(a.issue), read(a.inflight.io.enq.ready) && read(a.io.tlb.req.ready), read(a.eligible).asUInt)
  add("tlb.occupied",PopCount(read(a.state).map(_ =/= 0.U)),8)
  add("tlb.locked",a.locked); add("tlb.pmpPair",a.paired)
  val v=module.vbopStub.get
  add("vbop.buffer",PopCount(read(v.reqFilter.valids)),16)
  add("vbop.allocPipeline",v.reqFilter.allocValid)
  add("vbop.generatePipeline",v.reqFilter.io.in.valid)
  add("vbop.locked",v.reqFilter.locked)
  add("vbop.delay",PopCount(read(v.delayQueue.valids)),16)
  // PBOP delayQueue is private in the frozen producer; fail if it disappears.
  def field[T](owner: AnyRef,name: String): T = {
    val f=owner.getClass.getDeclaredField(name); f.setAccessible(true); f.get(owner).asInstanceOf[T]
  }
  add("pbop.delay",PopCount(read(field[Vec[Bool]](field[AnyRef](module.bop,"delayQueue"),"valid"))),16)
  add("pbop.output",module.bop.io.req.valid)
  val cdp=module.cdp.get;val send=cdp.sentUnit.get
  add("cdp.entries",PopCount(read(send.valids)),16)
  add("cdp.tlbHeld",send.tlbHeld);add("cdp.query1",send.pftS1);add("cdp.query2",send.pftS2)
  add("cdp.vtPipe",PopCount(read(cdp.vtTrainPipe.stage_valid)),5)
  cdp.filterTraining.foreach(f=>Seq(f.pipe.s0_valid,f.pipe.s1_valid,f.pipe.s2_valid,f.pipe.s3_valid).zipWithIndex.foreach{case(v,i)=>add("cdp.filterPipe"+i,v)})
  cdp.detectFrontend.lanes.zipWithIndex.foreach {case(l,i)=>Seq(l.s1_req,l.s2_req,l.s3_req).zipWithIndex.foreach{case(v,j)=>add("cdp.detect"+i+"stage"+j,v.valid)}}
  val cdpCounters=mutable.ArrayBuffer.empty[(String,UInt)]
  def cc(name:String,n:UInt):Unit={val x=RegInit(0.U(64.W));x:=x+read(n);cdpCounters+=name->x}
  cc("emitted",(read(cdp.io.req.valid)&&read(cdp.io.req.ready)).asUInt)
  cc("candidate",read(send.io.in.valid).asUInt);cc("allocated",send.allocate.asUInt)
  cc("duplicate",send.duplicate.asUInt);cc("fullDrop",send.fullDrop.asUInt)
  cc("filterDrop",send.filterDrop.asUInt);cc("translationDrop",PopCount(read(send.translationDrop)))
  cc("halfAccepted",PopCount(read(cdp.detectFrontend.admitted)))
  cc("halfRejected",read(cdp.detectFrontend.dropped).reduce(_+&_))
  cc("degreeDrop",VecInit(cdp.detectFrontend.degreeQueues.map(q=>Mux(read(q.io.enq.head.valid),3.U-PopCount(read(q.accepted)),0.U))).reduce(_+&_))
  cc("vpnTrainDrop",(read(cdp.vpnTrainReqBuf.io.enq.valid)&& !read(cdp.vpnTrainReqBuf.io.enq.ready)).asUInt)
  cdp.filterTraining.foreach(f=>cc("filterTrainDrop",(read(f.queue.io.enq.valid)&& !read(f.queue.io.enq.ready)).asUInt))
  // Read-only S12 diagnostics. Slot occupancy is the union of active CDP
  // REQ execution and a queued CDP REQ, qualified by physical TSHR valid.
  // Origin-lifetime is reported separately: it also includes later demand/SNP
  // work in the same CDP-allocated slot, so it is not the primary metric.
  val analysisCounters=mutable.ArrayBuffer.empty[(String,UInt)]
  def ac(name:String,n:UInt):Unit={val x=RegInit(0.U(64.W));x:=x+n;analysisCounters+=name->x}
  val cdpSlots=module.slices.flatMap(_.tshrs).map {t=>
    val pendingCDP=RegInit(false.B)
    when(read(t.tshr_enter_REQ)) {
      pendingCDP:=read(t.io.UpRXREQ.SrcID)===cfg.pfSrcID.U &&
        L2PrefetchTxnID.decode(read(t.io.UpRXREQ.TxnID))===L2PfSource.CDP.U
    }
    read(t.tshr_valid) && ((read(t.vPipeREQ.io.bopPerf.activePrefetch) &&
      read(t.vPipeREQ.io.bopPerf.activePrefetchSource)===L2PfSource.CDP.U) ||
      (read(t.rbeREQ.io.valid) && pendingCDP))
  }
  val originSlots=module.slices.flatMap(_.tshrs).map {t=>
    val cdpOrigin=RegInit(false.B)
    when(read(t.io.fromAlloc.alloc.REQ)) {
      cdpOrigin:=read(t.io.UpRXREQ.SrcID)===cfg.pfSrcID.U &&
        L2PrefetchTxnID.decode(read(t.io.UpRXREQ.TxnID))===L2PfSource.CDP.U
    }
    when(read(t.tshr_dealloc)){cdpOrigin:=false.B}
    read(t.tshr_valid) && cdpOrigin
  }
  ac("cdpSlotCycles",PopCount(cdpSlots));ac("cdpAnyCycles",VecInit(cdpSlots).asUInt.orR.asUInt)
  ac("cdpOriginSlotCycles",PopCount(originSlots))
  ac("allSlotCycles",PopCount(module.slices.flatMap(_.tshrs).map(t=>read(t.tshr_valid))))
  for((name,idx)<-L2BOPPerf.names.zipWithIndex) {
    ac(name,PopCount(module.slices.flatMap(_.tshrs).map(t=>
      read(t.io.bopPerf(idx)) && read(t.io.bopPerfSource(idx))===L2PfSource.CDP.U)))
  }
  ac("cdpNewAlloc",PopCount(module.slices.flatMap(_.tshrs).map(t=>
    read(t.io.fromAlloc.alloc.REQ) && read(t.io.UpRXREQ.SrcID)===cfg.pfSrcID.U &&
      L2PrefetchTxnID.decode(read(t.io.UpRXREQ.TxnID))===L2PfSource.CDP.U)))
  cdp.filterTraining.foreach {f=>
    val ft=f.pipe
    ac("filterTrainAccepted",(read(f.queue.io.enq.valid)&&read(f.queue.io.enq.ready)).asUInt)
    ac("filterUpdates",(read(ft.io.trainReq.valid)&&read(ft.io.trainReq.ready)).asUInt)
    ac("filterUpdatesUsed",(read(ft.io.trainReq.valid)&&read(ft.io.trainReq.ready)&&read(ft.s3_task.isUsed)).asUInt)
    ac("filterUpdatesEvict",(read(ft.io.trainReq.valid)&&read(ft.io.trainReq.ready)&&read(ft.s3_task.isEvict)).asUInt)
    for(v<-0 to 3) ac("filterUpdatedSat"+v,PopCount((0 until cdp.cdpParams.filterEntryBlks).map(i=>
      read(ft.io.trainReq.valid)&&read(ft.io.trainReq.ready)&&read(ft.io.trainReq.bits.sat)(i)===v.U)))
  }
  ac("filterSenderQuery",read(send.pftS2).asUInt)
  val senderFilterHit=VecInit(send.hitVec.map(v=>read(v))).asUInt.orR
  ac("filterSenderHit",(read(send.pftS2)&&senderFilterHit).asUInt)
  for(v<-0 to 3) ac("filterSenderSat"+v,(read(send.pftS2)&&senderFilterHit &&
    read(send.rsp2.satVec)(PriorityEncoder(VecInit(send.hitVec.map(v=>read(v)))))(send.getFilterOffset(read(send.req2.paddr)))===v.U).asUInt)
  val analysisPacked=IO(Output(UInt((64*analysisCounters.size).W)))
  analysisPacked:=Cat(analysisCounters.reverse.map(_._2).toSeq)
  val cdpEntryProbe=IO(Output(Vec(16,UInt(88.W))))
  for(i<-0 until 16) cdpEntryProbe(i):=Cat(read(send.valids)(i),read(send.entries)(i).paddrValid,read(send.entries)(i).vTag,read(send.entries)(i).pTag)
  // Requery output fires directly from query stage 2; idx2 identifies the
  // retiring slot. A stalled allowed query keeps its existing live ledger.
  val cdpSendEvents=IO(Output(UInt(128.W)))
  cdpSendEvents:=Cat(read(send.allocate),read(send.freeEntryIdx),read(send.io.in.bits.pfVAddr),read(send.filterDrop),read(send.idx2),read(send.io.out.valid)&&read(send.io.out.ready),read(send.idx2),read(send.translationDrop).asUInt)
  val cdpCountPacked=IO(Output(UInt((64*cdpCounters.size).W)));cdpCountPacked:=Cat(cdpCounters.reverse.map(_._2).toSeq)
  module.slices.zipWithIndex.foreach { case(s,bank)=>
    add(s"slice$bank.active",PopCount(s.tshrs.map(t=>read(t.tshr_valid))),nMSHR)
    s.tshrs.zipWithIndex.foreach { case(t,i)=>add(s"slice$bank.tshr$i.cdpPending",t.cdpPending) }
  }
  // Read-only TSHR/RBE/reservation census, kept separate from drain predicates.
  val diagnosticSignals=mutable.ArrayBuffer.empty[(String,UInt)]
  def diagnostic(name:String,signal:Data):Unit=diagnosticSignals+=((name,read(signal).asUInt))
  module.slices.zipWithIndex.foreach {case(slice,b)=>
    slice.tshrs.zipWithIndex.foreach {case(t,i)=>
      val prefix=s"slice$b.tshr$i."
      diagnostic(prefix+"valid",t.tshr_valid); diagnostic(prefix+"paddr",t.tshr_paddr)
      diagnostic(prefix+"busy",t.io.toAlloc.busy)
      diagnostic(prefix+"reqFree",t.vPipeREQ.io.free); diagnostic(prefix+"snpFree",t.vPipeSNP.io.free)
      diagnostic(prefix+"evtFree",t.vPipeEVT.io.free)
      diagnostic(prefix+"reqBlock",t.vPipeREQ.io.blockRBE); diagnostic(prefix+"snpBlock",t.vPipeSNP.io.blockRBE)
      diagnostic(prefix+"sa",t.snoopAgent.io.dbg)
      def rbe[T <: Bundle](name:String,r:oceanus.l2.tshr.L2RBE[T]):Unit={
        diagnostic(prefix+"rbe"+name+".valid",r.io.valid)
        diagnostic(prefix+"rbe"+name+".blockTSHR",r.io.blockFromTSHR)
        diagnostic(prefix+"rbe"+name+".blockVPipe",r.io.blockFromVPipe)
        diagnostic(prefix+"rbe"+name+".blockDirectory",r.blockByDirectory)
        diagnostic(prefix+"rbe"+name+".out",r.io.out.valid)
      }
      rbe("EVT",t.rbeEVT);rbe("SNP",t.rbeSNP);rbe("EVB",t.rbeEVB);rbe("REQ",t.rbeREQ)
    }
    val alloc=slice.tshrAlloc
    diagnostic(s"slice$b.alloc.postValid",VecInit(alloc.postcluster.map(p=>read(p.valid))).asUInt)
    alloc.postcluster.indices.foreach {c=>
      diagnostic(s"slice$b.alloc.cluster$c.target",alloc.postcluster(c).bits.target)
      diagnostic(s"slice$b.alloc.cluster$c.paddr",alloc.postcluster(c).bits.paddr)
      diagnostic(s"slice$b.alloc.cluster$c.resvAllow",VecInit(alloc.resv_allow(c).map(x=>if(x.litOption.nonEmpty) x else read(x))).asUInt)
      diagnostic(s"slice$b.alloc.cluster$c.canAlloc",VecInit(alloc.can_alloc_vec(c).map(x=>read(x))).asUInt)
      diagnostic(s"slice$b.alloc.cluster$c.canReuse",VecInit(alloc.can_reuse_vec(c).map(x=>read(x))).asUInt)
    }
  }
  // Pressure-quality probes read production signals without changing drain state.
  val qualitySignals=mutable.ArrayBuffer.empty[(String,UInt)]
  def quality(name:String,x:Data):Unit=qualitySignals+=((name,read(x).asUInt))
  def channel(name:String,valid:Bool,ready:Bool):Unit={quality(name+".valid",valid);quality(name+".ready",ready)}
  channel("up.req",io.t1p.head.UpREQ.valid,io.t1p.head.UpREQ.ready)
  channel("up.rsp",io.t1p.head.UpRSP.valid,io.t1p.head.UpRSP.ready)
  channel("up.dat",io.t1p.head.DnDAT.valid,io.t1p.head.DnDAT.ready)
  channel("up.snp",io.t1p.head.DnSNP.valid,io.t1p.head.DnSNP.ready)
  channel("up.evt",io.t1p.head.UpEVT.valid,io.t1p.head.UpEVT.ready)
  channel("up.writeData",io.t1p.head.UpDAT.valid,io.t1p.head.UpDAT.ready)
  channel("up.downRsp",io.t1p.head.DnRSP.valid,io.t1p.head.DnRSP.ready)
  channel("tlb.req",io.pfTlb.get.req.valid,io.pfTlb.get.req.ready)
  channel("tlb.resp",io.pfTlb.get.resp.valid,io.pfTlb.get.resp.ready)
  io.t4p.zipWithIndex.foreach {case(port,i)=>
    channel(s"type4.$i.req",port.UpREQ.valid,port.UpREQ.ready)
    channel(s"type4.$i.dat",port.DnDAT.valid,port.DnDAT.ready)
  }
  module.slices.zipWithIndex.foreach {case(slice,b)=>
    val alloc=slice.tshrAlloc
    quality(s"arb$b.req",alloc.preclusterRXREQ.valid)
    quality(s"arb$b.snp",alloc.preclusterRXSNP.valid)
    quality(s"arb$b.reqReady",alloc.preclusterRXREQ.ready)
    val postValid=read(alloc.postcluster(1).valid)
    val postSnp=read(alloc.postcluster(1).bits.target.SNP)
    val postReady=read(alloc.postcluster(1).ready)
    quality(s"arb$b.snpWins",postValid && postSnp)
    quality(s"arb$b.snpFire",postValid && postSnp && postReady)
    channel(s"slice$b.req",slice.io.UpRXREQ.valid,slice.io.UpRXREQ.ready)
    channel(s"slice$b.dnreq",slice.io.DnTXREQ.valid,slice.io.DnTXREQ.ready)
    channel(s"slice$b.dnrsp",slice.io.DnTXRSP.valid,slice.io.DnTXRSP.ready)
    channel(s"slice$b.dndat",slice.io.DnTXDAT.valid,slice.io.DnTXDAT.ready)
    channel(s"slice$b.snp",slice.io.DnRXSNP.valid,slice.io.DnRXSNP.ready)
    slice.tshrs.zipWithIndex.foreach {case(t,i)=>
      val prefix=s"s$b.t$i."
      quality(prefix+"valid",t.tshr_valid);quality(prefix+"pa",t.tshr_paddr)
      quality(prefix+"dirRead",t.proxyDir.fromDir_DirRdResp)
      quality(prefix+"hit",t.io.fromDir.META.hit);quality(prefix+"way",t.dirResult.way)
      quality(prefix+"dirGrant",t.proxyDir.io.wb_accept)
      quality(prefix+"dirState",t.proxyDir.io.toDir.META.state)
      quality(prefix+"tagMask",t.tag_modified);quality(prefix+"stateMask",t.meta_modified.state)
      quality(prefix+"localState",t.meta.state)
      quality(prefix+"reqWrite",t.meta_write_REQ_mask.state)
      quality(prefix+"snpWrite",t.meta_write_SNP_mask.state)
      quality(prefix+"reqState",t.meta_write_REQ_meta.state)
      quality(prefix+"snpState",t.meta_write_SNP_meta.state)
      quality(prefix+"drop",t.tshr_buffer_drop);quality(prefix+"modified",t.tshr_buffer_modified)
      quality(prefix+"dsGrant",t.proxyDS.io.wb_accept)
      quality(prefix+"dsRead",t.tshr_buffer_wen_DS)
      quality(prefix+"dsWrite",t.io.toDS.DSBufWb)
      quality(prefix+"evict",t.io.cdpEvict.valid)
    }
  }
  val qualityWidths=qualitySignals.map(_._2.getWidth).toSeq
  val qualityPacked=IO(Output(UInt(qualityWidths.sum.W)))
  qualityPacked:=Cat(qualitySignals.reverse.map(_._2).toSeq)
  val diagnosticWidths=diagnosticSignals.map(_._2.getWidth).toSeq
  val diagnosticPacked=IO(Output(UInt(diagnosticWidths.sum.W)))
  diagnosticPacked:=Cat(diagnosticSignals.reverse.map(_._2).toSeq)
  val hnPrefetchSource=IO(Output(Vec(cfg.sliceNum,Vec(nMSHR,UInt(L2PfSource.Width.W)))))
  val internal=IO(Output(Vec(cfg.sliceNum,Vec(nMSHR,Bool()))))
  module.slices.zipWithIndex.foreach {case(s,b)=>s.tshrs.zipWithIndex.foreach {case(t,i)=>
    internal(b)(i) := read(t.vPipeREQ.p_rxreq.SrcID)===cfg.pfSrcID.U
    hnPrefetchSource(b)(i):=Mux(internal(b)(i),L2PrefetchTxnID.decode(read(t.vPipeREQ.p_rxreq.TxnID)),0.U)
  }}
  val stateCounts=IO(Output(Vec(metrics.size,UInt(32.W))))
  metrics.zipWithIndex.foreach {case((_,s,_),i)=>stateCounts(i):=s}
  val stateEmpty=IO(Output(Bool())); stateEmpty := !stateCounts.asUInt.orR
  // A separate model compares the cumulative effect counters against live
  // valid->I transitions and first demand consumes, not the perf pulses.
  val snpInvalid=IO(Output(Vec(cfg.sliceNum,Vec(nMSHR,Valid(UInt(4.W))))))
  module.slices.zipWithIndex.foreach {case(s,b)=>s.tshrs.zipWithIndex.foreach {case(t,i)=>
    val mask=read(t.meta_write_SNP_mask); val meta=read(t.meta_write_SNP_meta)
    val old=read(t.vPipeSNP.io.tshr_dirResult)
    snpInvalid(b)(i).valid := mask.state && meta.state===L2Directory.MetaState.I &&
      old.hit && old.state=/=L2Directory.MetaState.I && old.pfSource=/=0.U
    snpInvalid(b)(i).bits := old.pfSource

  }}
  val statePacked=IO(Output(UInt((32*metrics.size).W))); statePacked:=stateCounts.asUInt
  val fifoWidths=fifos.map(_._3.enq.bits.getWidth).toSeq
  val fifoPacked=IO(Output(UInt(fifos.map(x=>2*x._3.enq.bits.getWidth+19).sum.W)))
  fifoPacked:=Cat(fifos.reverse.map {case(_,_,p)=>Cat(p.enq.valid,p.enq.bits,p.deq.valid,p.deq.bits,p.overwrite,p.occupancy)}.toSeq)
  val snpPacked=IO(Output(UInt((cfg.sliceNum*nMSHR*5).W))); snpPacked:=snpInvalid.asUInt
  println("STRESS_CENSUS "+metrics.zipWithIndex.map {case((n,_,c),i)=>s"$i=$n:capacity=$c"}.mkString(","))
}

class StressDemandDriver(dut: StressTopBase, upstream: StressUpstreamModel) extends ChiselSim {
  val masters = Seq(MasterId.t1p0) // Type-4 is never stimulated
  private def reqPort(master: MasterId) = master match {
    case MasterId.T1P0 => dut.io.t1p(0).UpREQ
    case MasterId.T4P0 => dut.io.t4p(0).UpREQ
    case MasterId.T4P1 => dut.io.t4p(1).UpREQ
  }
  private def dataPort(master: MasterId) = master match {
    case MasterId.T1P0 => dut.io.t1p(0).DnDAT
    case MasterId.T4P0 => dut.io.t4p(0).DnDAT
    case MasterId.T4P1 => dut.io.t4p(1).DnDAT
  }
  private val queues = masters.map(_ -> mutable.Queue.empty[UpRequestSpec]).toMap
  private val nextID = mutable.Map(masters.map(_ -> 0): _*)
  private case class Read(req: UpRequestSpec, born: Int, beats: mutable.Set[Int] = mutable.Set.empty)
  private val active = mutable.Map.empty[(MasterId, BigInt), Read]
  private val held = mutable.Map.empty[MasterId, UpRequestSpec]
  private case class Ack(key: (MasterId, BigInt), dbid: BigInt, slice: BigInt)
  private val acks = mutable.Queue.empty[Ack]
  val accepted = mutable.ArrayBuffer.empty[(Int, UpRequestSpec)]
  var latencySum=0L; var latencyMax=0; var latencyCount=0
  var activeLatencySum=0L; var activeLatencyMax=0; var activeLatencyCount=0
  var validateData:(UpRequestSpec,Int,Int)=>Unit=(_:UpRequestSpec,_:Int,_:Int)=>()
  var stimulusEnd=Int.MaxValue
  private def finish(read: Read,cycle: Int): Unit = {
    val lat=cycle-read.born; latencySum+=lat; latencyMax=latencyMax.max(lat); latencyCount+=1
    if(cycle<stimulusEnd) { activeLatencySum+=lat; activeLatencyMax=activeLatencyMax.max(lat); activeLatencyCount+=1 }
  }
  var completed = 0
  var checkedBeats = 0
  var maxOutstanding = 0
  def outstanding: Int = active.size
  def pending: Int = active.size + held.size + queues.values.map(_.size).sum + acks.size
  def diagnostic: String = s"active=${active.toSeq.map {case(k,r)=>s"$k:addr=${r.req.addr.toString(16)}:born=${r.born}:beats=${r.beats.mkString(",")}"}.mkString(";")} held=$held queued=${queues.values.map(_.size).sum} acks=$acks"
  def queued: Int = held.size + queues.values.map(_.size).sum
  def enqueue(requests: Seq[UpRequestSpec]): Unit = requests.foreach(r => queues(r.master).enqueue(r))
  def drive(cycle: Int): Unit = {
    upstream.clearUpReq()
    masters.foreach { master =>
      if (!held.contains(master) && queues(master).nonEmpty) {
        val count = 1 << reqPort(master).bits.TxnID.getWidth
        (0 until count).map(i => (nextID(master) + i) % count)
          .find(id => !active.contains((master, BigInt(id)))).foreach { id =>
            val src = master match {
              case MasterId.T1P0 => 0
              case MasterId.T4P0 => 4
              case MasterId.T4P1 => 5
            }
            held(master) = queues(master).dequeue().copy(txnId = id, srcId = src,
              expCompData = master == MasterId.T1P0)
          }
      }
      held.get(master).foreach(upstream.driveRead)
    }
    // Existing acks only: a data response observed below cannot be acked at
    // the same edge, and DBID is always qualified by its source slice.
    acks.headOption.foreach(a => upstream.driveUpCompAck(UpDBID(a.dbid), a.slice.toInt))
  }
  def sample(cycle: Int): Unit = {
    if (acks.nonEmpty && dut.io.t1p(0).UpRSP.valid.peek().litToBoolean &&
        dut.io.t1p(0).UpRSP.ready.peek().litToBoolean) {
      val ack = acks.dequeue()
      finish(active.remove(ack.key).get,cycle)
      completed += 1
    }
    masters.foreach { master =>
      val port = reqPort(master)
      if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
        val request = held.remove(master).get
        val key = (master, request.txnId)
        assert(!active.contains(key), s"TxnID reused while active: $key")
        active(key) = Read(request,cycle)
        nextID(master) = (request.txnId.toInt + 1) % (1 << port.bits.TxnID.getWidth)
        accepted += ((cycle, request))
        maxOutstanding = maxOutstanding.max(active.size)

      }
      val data = dataPort(master)
      if (data.valid.peek().litToBoolean && data.ready.peek().litToBoolean) {
        val id = data.bits.TxnID.peek().litValue
        val key = (master, id)
        val read = active.getOrElse(key, throw new AssertionError(s"unmatched upstream response: $key"))
        assert(data.bits.Opcode.peek().litValue == CCHIOpcode.CompData.opcode)
        val beat = data.bits.DataID.peek().litValue.toInt
        assert(Set(0, 1).contains(beat) && !read.beats(beat), s"duplicate/invalid beat: $key beat=$beat")
        validateData(read.req,beat,cycle)
        val actual = data.bits.Data.peek().litValue
        val expected = StressMemory.beat(read.req.addr, beat)
        assert(actual == expected, s"data mismatch cycle=$cycle $key addr=0x${read.req.addr.toString(16)} beat=$beat actual=0x${actual.toString(16)} expected=0x${expected.toString(16)}")
        read.beats += beat
        checkedBeats += 1
        if (read.beats.size == 2) {
          if (master == MasterId.T1P0) {
            acks.enqueue(Ack(key, data.bits.DBID.peek().litValue, data.bits.SrcID.peek().litValue))
          } else {
            finish(active.remove(key).get,cycle)
            completed += 1
          }
        }
      }
    }
  }
}

object TestTop_L2PrefetchStress extends App with ChiselSim {
  Constantin.init(false)
  def opt(n: String, d: String): String = args.find(_.startsWith(n+"=")).map(_.split("=",2)(1)).getOrElse(d)
  val slices=opt("--slices","1").toInt
  val fast=args.contains("--fast")
  StressMemory.vaMask=if(fast) BigInt(0x1000000) else BigInt(0)
  def physical(va: BigInt)=va ^ StressMemory.vaMask
  def bankOf(addr: BigInt): Int = if(slices==1) 0 else if(slices==2) ((addr>>6).bitCount&1) else {
    val lo=(6 until 48 by 2).count(addr.testBit(_))&1
    val hi=(7 until 48 by 2).count(addr.testBit(_))&1; lo|(hi<<1)
  }
  require(Set(1,2,4)(slices))
  val cfg=new L2Configuration(8,true,0 until slices,bopEnable=true,pfRecvEnable=true,
    pfCtrlEnable=true,pfTrainEnable=true,pfTlbEnable=true,vbopStubEnable=true,cdpStubEnable=true,
    bopParams=OceanusBOPParameters(badScore=0,roundMax=100000),
    vbopParams=OceanusVBOPParameters(badScore=0,roundMax=100000,samePagePA=fast))
  val params=new Config((_,_,_)=>{
    case L2ParamsKey => L2Params(mshrSize=8,ways=4,sets=128)
    case CHIParametersKey => CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false,enablePerf=true,fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=true,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  // Bound compiler jobs explicitly; Verilator's default -j0 ignores MAKEFLAGS.
  implicit val simulator: HasSimulator=HasSimulator.simulators.verilator(
    compilationSettings=svsim.CommonCompilationSettings(availableParallelism=svsim.CommonCompilationSettings.AvailableParallelism.UpTo(1)),
    verilatorSettings=svsim.verilator.Backend.CompilationSettings.default.withParallelism(
      Some(svsim.verilator.Backend.CompilationSettings.Parallelism.Uniform.default.withNum(1))))
  implicit val testingDirectory: chisel3.testing.HasTestingDirectory =
    new chisel3.testing.HasTestingDirectory {
      override def getDirectory = java.nio.file.Paths.get(
        sys.props.getOrElse("pf.stress.workspace", "build/chiselsim/hn-"+java.util.UUID.randomUUID.toString))
    }
  val runFile=opt("--runs","")
  val runArguments: Seq[Array[String]]=if(runFile.isEmpty) Seq(args) else {
    val file=scala.io.Source.fromFile(runFile)
    try file.getLines().filter(_.trim.nonEmpty).map(_.trim.split(" +")).toVector finally file.close()
  }
  var suiteHadFailures=false
  simulate(new TestTopStress(cfg)(params)) { dut =>
    // Host-only pressure changes reuse this compiled DUT. Each seed still gets
    // a fresh reset, all-new independent ledgers and a full final drain.
    for(runArgs <- runArguments) {
    def opt(n: String,d: String): String=runArgs.find(_.startsWith(n+"=")).map(_.split("=",2)(1)).getOrElse(d)
  val firstSeed=opt("--seed","1").toInt
  val lastSeed=opt("--last-seed",firstSeed.toString).toInt
  val minimum=opt("--cycles","5000").toInt
  val demandPeriod=opt("--demand-period","32").toInt
  val item=opt("--item","smoke")
  val level=opt("--level","0").toInt
  val pattern=opt("--pattern","random")
  val recvPct=opt("--recv-pct","3.125").toDouble
  val recvBurst=opt("--recv-burst","0").toInt
  val recvHot=opt("--recv-hot","0").toInt
  val memMin=opt("--mem-min","16").toInt
  val memMax=opt("--mem-max",memMin.toString).toInt
  val beatGap=opt("--beat-gap","1").toInt
  val dataPeriod=opt("--data-period","1").toInt
  val creditHold=opt("--credit-hold","0").toInt
  val creditRandom=opt("--credit-random","0").toInt
  val missPct=opt("--tlb-miss","0").toInt
  val faultPct=opt("--tlb-fault","0").toInt
  val tlbMin=opt("--tlb-min","1").toInt
  val tlbMax=opt("--tlb-max",tlbMin.toString).toInt
  val tlbHold=opt("--tlb-hold","0").toInt
  val issueHold=opt("--issue-hold","0").toInt
  val budget=opt("--budget","64").toInt
  val snpPeriod=opt("--snoop-period","0").toInt
  val snoopRate=opt("--snoop-rate","0").toDouble
  val snoopTargets=opt("--snoop-addresses","").split(",").filter(_.nonEmpty).map(x=>BigInt(x,16)).toSeq
  require(snoopRate>=0 && snoopRate<=1)
  val shrinkAddresses=opt("--shrink-addresses","false").toBoolean
  val snoopStop=opt("--snoop-stop",minimum.toString).toInt
  val retryPct=opt("--retry-pct","0").toInt
  val retryDelay=opt("--retry-delay","16").toInt
  val hnExtension=opt("--hn-extension","false").toBoolean
  val hnDct=opt("--hn-dct","true").toBoolean
  val hnSplit=opt("--hn-split","true").toBoolean
  val qualityEnabled=opt("--quality","false").toBoolean
  val qualityTrace=opt("--quality-trace","false").toBoolean
  val contentionSets=opt("--contention-sets","8").toInt
  val contentionLines=opt("--contention-lines","8").toInt
  val snoopLead=opt("--snoop-lead","0").toInt
  def poolAddress(index:Int):BigInt=BigInt(0x200000)+((index%contentionSets)+(index/contentionSets%contentionLines)*128)*64
  val traceWindow=opt("--trace-window","0").toInt
  val prefetchMode=opt("--prefetch-mode","all")
  require(Set("all","none","bop","vbop","recv","cdp")(prefetchMode))
  val cdpEnabled=opt("--cdp-enable","true").toBoolean
  val analysis=opt("--cdp-analysis","false").toBoolean
  def enabled(source:String):Boolean=(prefetchMode=="all" || prefetchMode==source) && (source!="cdp" || cdpEnabled)
  val recordTape=opt("--record-tape","false").toBoolean
  val replayPath=opt("--replay-tape","")
  val snapshotCycles=opt("--snapshot-cycles","").split(",").filter(_.nonEmpty).map(_.toInt).toSet
  case class Tape(demand:BigInt,recv:Boolean,recvAddr:BigInt,snoop:BigInt)
  val replayTape:Map[Int,Tape]=if(replayPath.isEmpty) Map.empty else {
    val file=scala.io.Source.fromFile(replayPath)
    try file.getLines().filter(_.startsWith("STRESS_TAPE ")).map {line=>
      val f=line.split(" ").drop(1).map(_.split("=",2)).map(x=>x(0)->x(1)).toMap
      f("cycle").toInt->Tape(BigInt(f("demand"),16),f("recv")=="1",BigInt(f("recvAddr"),16),BigInt(f("snoop"),16))
    }.toMap finally file.close()
  }
  println(s"STRESS_ISOLATION mode=$prefetchMode replay=$replayPath recordTape=$recordTape seed=$firstSeed slices=$slices")
  val harsh=missPct>0 || faultPct>0
    val caseId=opt("--case-id","single")
    val stopAtMs=opt("--stop-at-ms",Long.MaxValue.toString).toLong
    for(seed <- firstSeed to lastSeed if System.currentTimeMillis()<stopAtMs) {
    var reportHNFailure:()=>Unit=()=>()
    try {
    println(s"STRESS_BEGIN caseId=$caseId item=$item level=$level seed=$seed slices=$slices cycles=$minimum demandPeriod=$demandPeriod recvPct=$recvPct mem=$memMin..$memMax beatGap=$beatGap creditHold=$creditHold tlb=$missPct/$tlbMin..$tlbMax snoopPeriod=$snpPeriod snoopRate=$snoopRate shrink=$shrinkAddresses sets=$contentionSets lines=$contentionLines retryPct=$retryPct pattern=$pattern budget=$budget")
    val seedStart=System.nanoTime()
    val random=new scala.util.Random(0x20261021L+seed*100+slices)
    val dummy=UpRequestSpec(MasterId.t1p0,0,0,0,CCHIOpcode.ReadShared.opcode,BigInt(0),expCompData=true)
    val spec=CaseSpec("overnight",UpConfig(Seq(MasterId.t1p0),ExplicitRequestStream(Seq(dummy),Seq(BigInt(CHIFieldResp.CompData_SC))),SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(25),FixedOutstanding(64),ImmediateCreditPolicy()),30000,(0,30000),Seq.empty)
    val up=new StressUpstreamModel(dut,spec)
    val driver=new StressDemandDriver(dut,up)
    val dn=new StressDownstreamModel(dut,spec)
    require(retryPct==0,"The reused HN oracle has no RetryAck cancellation API; this suite covers S01-S13.")
    val hnOrdering=new ConflictHNOrdering
    val hnRequester=BigInt(cfg.nodeId)
    val responseAddresses=mutable.Map.empty[DnTxnID,BigInt]
    var dataOrderStalls=0L;var snoopOrderStalls=0L;var responseOrderStalls=0L
    var orderingSamples=0L
    var rateAccumulator=0L
    var snoopOffers=0L
    var activeNoCompletion=0;var activeNoCompletionRun=0
    val rateUnits=math.round(snoopRate*1000000)
    val snoopRandom=new scala.util.Random(0x484eL+seed*100+slices)
    val data=mutable.Queue.empty[PendingDnComp]
    val firstBeatSent=mutable.Map.empty[DnTxnID,Int]
    val rsp=mutable.Queue.empty[PendingDnRsp]
    val live=mutable.Set.empty[DnTxnID]
    val snoops=mutable.Queue.empty[(SnpTxnID,BigInt)]
    case class Translation(client: Int,va: BigInt,due: Int,miss: Boolean,fault: Boolean,attempt: Int)
    val transactions=mutable.Map.empty[(Int,BigInt),Int]
    val terminals=mutable.Map.empty[(Int,BigInt),Translation]
    val cdpExpected=mutable.Map.empty[Int,(BigInt,Option[BigInt])]
    val allowed=Seq.fill(2)(mutable.Map.empty[BigInt,Int].withDefaultValue(0))
    var modelRetries=0; var modelDrops=0; var modelDone=0
    var actualRetries=0; var actualDrops=0; var actualDone=0
    var injectedMisses=0; var injectedFaults=0; var fastAllocations=0; var fastEmits=0
    val replies=mutable.Queue.empty[Translation]
    val sources=Seq(1,2,3,9)
    val produced=Array.fill(slices,10)(BigInt(0))
    val entered=Array.fill(slices,10)(BigInt(0))
    var cycle=0; var submitted=0; var recvInputs=0; var translations=0
    var lastProgress=0; var lastCompleted=0
    var maxDemandNoCompletion=0;var noCompletionRun=0;var maxUpReqBlocked=0;var upReqBlockedRun=0
    var lastDemandCompleteCycle = -1;var stimulusSnoops=0
    driver.stimulusEnd=minimum
    val serviceAges=Array.fill(8)(0); var servicePeak=0; var serviceCycles=0
    val qualityModel=new StressPressureQuality(dut,caseId,seed,slices,minimum,qualityTrace)
    val peaks=Array.fill(dut.metrics.size)(0); val fullCycles=Array.fill(dut.metrics.size)(0L)
    val queueModels=dut.fifos.map(_=>mutable.Queue.empty[BigInt]).toSeq
    val enqs=Array.fill(dut.fifos.size)(0L); val deqs=Array.fill(dut.fifos.size)(0L); val overs=Array.fill(dut.fifos.size)(0L)
    val uselessSnoop=Array.fill(slices,10)(BigInt(0))
    var snpSent=0; var snpDone=0; val snpLive=mutable.Set.empty[BigInt]
    val knownPrefetch=mutable.ArrayBuffer.empty[BigInt]
    val offeredSnoops=mutable.Queue.empty[BigInt]
    var replaySnoopOffered=0;var replaySnoopSent=0
    val overlap=new StressHNCDPOverlap
    val hnEvents=mutable.ArrayBuffer.empty[(Int,BigInt,String)]
    val hnTrace=mutable.Queue.empty[String]
    var hnLastSample = -1
    val extension=if(hnExtension) Some(new L2HNExtensionModel(
      HNExtConfig(retryPercent=0,splitPercent=if(hnSplit)50 else 0,dctPercent=if(hnDct)50 else 0,
        seed=0x484e4558L+seed*100+slices,forwardEnable=hnDct,
        requestLatencyMin=memMin,requestLatencyMax=memMax,minBeatGap=beatGap),StressMemory.beat _,line=>{
        hnTrace.enqueue(line);while(hnTrace.size>512)hnTrace.dequeue()
        if(line.startsWith("HN_EXT_EVENT ")) {
          val f=line.split(" ").drop(1).map(_.split("=",2)).map(a=>a(0)->a(1)).toMap
          val event=f("name");val c=f("cycle").toInt;val pa=BigInt(f("pa").drop(2),16)
          if(Set("dctForwardComplete","requesterDCT","splitRead","normalSnoop","stashSnoop")(event)) hnEvents+=((c,pa,event))
        } else if(line.startsWith("HN_EXT_DUT_PROTOCOL_ERROR")) println(line)
      })) else None
    var extCompletedIndex=0
    val extPins=extension.map { m=>
      driver.validateData=(q,b,c)=>m.upstreamData(q.addr,b,c)
      new StressHNExtensionPins(dut,dn,m,hnRequester,dataPeriod,(q,c)=>{
        val id=DnTxnID(q.txn);assert(!live(id),s"downstream live TxnID reuse $id")
        live+=id;dn.noteRequest(id)
        if(qualityEnabled) qualityModel.downstream(c,q.addr,L2HNExtensionModel.isWrite(q.op))
        if(!L2HNExtensionModel.isWrite(q.op)) {
          val tid=id.v.toInt>>log2Ceil(slices);val bank=id.v.toInt & (slices-1)
          if(dut.internal(bank)(tid).peek().litToBoolean)knownPrefetch+=q.addr
          if(dut.hnPrefetchSource(bank)(tid).peek().litValue==9)overlap.event(c,q.addr,"cdpIssued")
        }
      },(d,c)=>if(qualityEnabled)qualityModel.refill(c,d.addr,d.beat/2))
    }
    def hnReport(closed:Boolean):Unit=extension.foreach { m=>
      println(s"STRESS_HN_EXT caseId=$caseId seed=$seed closed=$closed retryPercent=0 dct=$hnDct split=$hnSplit violations=${m.counts("dutProtocolErrors")} "+m.counts.toSeq.sortBy(_._1).map{case(k,v)=>s"$k=$v"}.mkString(" "))
      overlap.report(caseId,seed,closed,hnLastSample+1)
      if(!closed)hnTrace.foreach(println)
    }
    reportHNFailure=()=>hnReport(false)
    def cdpSnapshot(closed:Boolean):Unit={
      val raw=dut.cdpCountPacked.peek().litValue
      println(s"STRESS_CDP_SNAPSHOT caseId=$caseId seed=$seed cycle=$cycle closed=$closed " + dut.cdpCounters.indices.map(i=>dut.cdpCounters(i)._1+"="+((raw>>(64*i))&((BigInt(1)<<64)-1))).mkString(" "))
    }
    val analysisMask=(BigInt(1)<<64)-1
    var analysisLast=Map.empty[String,BigInt];var analysisLastCompleted=0;var analysisStart=0
    def analysisCounts():Map[String,BigInt]={
      val raw=dut.analysisPacked.peek().litValue;val c=dut.cdpCountPacked.peek().litValue
      dut.analysisCounters.indices.map(i=>dut.analysisCounters(i)._1->((raw>>(64*i))&analysisMask)).toMap ++
        dut.cdpCounters.indices.map(i=>("cdp_"+dut.cdpCounters(i)._1)->((c>>(64*i))&analysisMask)).toMap
    }
    def analysisWindow(end:Int):Unit=if(analysis && end>analysisStart) {
      val now=analysisCounts();val delta=now.toSeq.sortBy(_._1).map{case(k,v)=>k+"="+(v-analysisLast.getOrElse(k,BigInt(0)))}.mkString(" ")
      println(s"STRESS_CDP_WINDOW caseId=$caseId seed=$seed enabled=$cdpEnabled start=$analysisStart end=$end samples=${end-analysisStart} completed=${driver.completed-analysisLastCompleted} $delta")
      analysisLast=now;analysisLastCompleted=driver.completed;analysisStart=end
    }
    def qos():Unit={cdpSnapshot(false);println(s"STRESS_QOS caseId=$caseId seed=$seed cycle=$cycle completed=${driver.completed} submitted=$submitted maxDemandNoCompletion=$maxDemandNoCompletion maxUpReqBlocked=$maxUpReqBlocked lastDemandCompleteCycle=$lastDemandCompleteCycle stimulusSnoops=$stimulusSnoops snpSent=$snpSent snpDone=$snpDone maxActiveNoCompletion=$activeNoCompletion snoopOffers=$snoopOffers pendingOffers=${offeredSnoops.size}")}
    val retryLive=mutable.Map.empty[DnTxnID,(BigInt,BigInt,BigInt)]
    var retryDemand=0; var retryPrefetch=0; var reissues=0; var pcrdSent=0
    var offered=0L; var blocked=0L; var activeCompleted=0
    val lastEnter=Array.fill(10)(0); val maxEnterGap=Array.fill(10)(0)
    val eventTrace=mutable.Queue.empty[String]
    def trace(message: => String): Unit = if(traceWindow>0) {
      eventTrace.enqueue(s"STRESS_TRACE cycle=$cycle $message")
      while(eventTrace.size>traceWindow) eventTrace.dequeue()
    }
    def dumpStall(): Unit = {
      println(s"STRESS_STALL caseId=$caseId seed=$seed cycle=$cycle lastProgress=$lastProgress completed=${driver.completed} pending=${driver.pending} data=$data rsp=$rsp live=$live snoops=$snoops snpLive=$snpLive retryLive=$retryLive replies=$replies")
      println("STRESS_DRIVER "+driver.diagnostic)
      val packed=dut.statePacked.peek().litValue
      println("STRESS_NONEMPTY "+dut.metrics.indices.filter(i=>((packed>>(32*i))&0xffffffffL)!=0).map(i=>s"${dut.metrics(i)._1}=${(packed>>(32*i))&0xffffffffL}").mkString(","))
      println(dn.preAssertTraceLine(cycle))
      var bits=dut.diagnosticPacked.peek().litValue
      dut.diagnosticSignals.zip(dut.diagnosticWidths).foreach {case((name,_),width)=>
        println(s"STRESS_STATE caseId=$caseId cycle=$cycle name=$name value=${bits&((BigInt(1)<<width)-1)}")
        bits=bits>>width
      }
      eventTrace.foreach(println)
    }
    var withholding=false
    def sampleCensus(): Unit = {
      val service=value(dut.tlbServicePacked)
      val hasService=service.testBit(8); val issued=service.testBit(12)
      val issueSlot=((service>>9)&7).toInt
      if(hasService) serviceCycles+=1
      for(i<-0 until 8) {
        if(!service.testBit(i)) serviceAges(i)=0
        else if(issued && issueSlot==i) {
          servicePeak=servicePeak.max(serviceAges(i)+1)
          assert(serviceAges(i)+1<=8,s"TLB service bound slot=$i age=${serviceAges(i)+1}")
          serviceAges(i)=0
        } else if(hasService) {
          serviceAges(i)+=1
          assert(serviceAges(i)<8,s"TLB service bound bypass slot=$i age=${serviceAges(i)}")
        }
      }
      val stateBits=value(dut.statePacked)
      if(qualityEnabled) qualityModel.sample(cycle,stateBits)
      for(i<-dut.metrics.indices) {
        val n=((stateBits>>(32*i)) & 0xffffffffL).toInt; peaks(i)=peaks(i).max(n)
        assert(n<=dut.metrics(i)._3,s"capacity exceeded metric=${dut.metrics(i)._1} count=$n")
        if(cycle<minimum && n==dut.metrics(i)._3) fullCycles(i)+=1
      }
      var packed=value(dut.fifoPacked)
      dut.fifos.zipWithIndex.foreach {case((name,capacity,p),i)=>
        val q=queueModels(i); val width=dut.fifoWidths(i); val mask=(BigInt(1)<<width)-1
        val occupancy=(packed & 65535).toInt; packed=packed>>16
        val overwrite=packed.testBit(0); packed=packed>>1
        val out=packed & mask; packed=packed>>width
        val pop=packed.testBit(0); packed=packed>>1
        val in=packed & mask; packed=packed>>width
        val push=packed.testBit(0); packed=packed>>1
        assert(occupancy==q.size,s"queue count $name got=$occupancy model=${q.size} cycle=$cycle")
        if(pop) { assert(q.nonEmpty && q.dequeue()==out,s"FIFO payload $name cycle=$cycle"); deqs(i)+=1 }
        if(overwrite) { assert(q.nonEmpty,s"overwrite empty $name"); q.dequeue(); overs(i)+=1 }
        if(push) { q.enqueue(in); enqs(i)+=1 }
        assert(q.size<=capacity && enqs(i)==deqs(i)+overs(i)+q.size,s"item closure $name")
      }
      if(cycle%64==0) for(s<-0 until slices; src<-0 to 9)
        assert(value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.UselessSnoop))==uselessSnoop(s)(src),s"useless_snoop mismatch slice=$s source=$src")
      var snps=value(dut.snpPacked)
      for(s<-0 until slices; t<-0 until 8) {
        val source=(snps & 15).toInt; val valid=snps.testBit(4); snps=snps>>5
        if(valid) uselessSnoop(s)(source)+=1
      }
    }
    def flag(x: Bool)=x.peek().litToBoolean
    def value(x: UInt)=x.peek().litValue
    def count(v: Vec[Vec[UInt]],s: Int,src: Int)=value(v(s)(src))
    def ledger(s: Int,src: Int): Seq[BigInt] = {
      val o=dut.observation
      Seq(count(o.prefetchEnqueueBySource,s,src), count(o.prefetchQueueOverwriteBySource,s,src),
        count(o.prefetchEnterBySource,s,src), count(o.prefetchDedupBySource,s,src),count(o.prefetchPendingBySource,s,src))
    }
    def check(): Unit = {
      for(s <- 0 until slices; src <- 0 to 9) {
        val Seq(enq,over,issue,dedup,pending)=ledger(s,src)
        assert(enq==over+issue+dedup+pending,s"queue conservation cycle=$cycle slice=$s source=$src ledger=${ledger(s,src)}")
        assert(count(dut.observation.prefetchSliceEnteredBySource,s,src)==entered(s)(src),s"independent entry counter slice=$s source=$src")
        assert(produced(s)(src)==entered(s)(src)+over+dedup+pending,
          s"producer/slice conservation cycle=$cycle slice=$s source=$src produced=${produced(s)(src)} entered=${entered(s)(src)} ledger=${ledger(s,src)}")
        if(src==1) assert(count(dut.observation.recvAcceptedBySource,s,src)==produced(s)(src),"Recv accepted counter")
      }
      assert(produced.map(_(1)).sum==recvInputs,"Recv external input conservation")
      for(s <- 0 until slices; m <- L2BOPPerf.names.indices) {
        val total=value(dut.observation.bopPerfCountBySlice(s)(m))
        val sum=(0 to 9).map(src=>value(dut.observation.bopPerfCountBySliceSource(s)(src)(m))).sum
        assert(total==sum,s"effect conservation slice=$s metric=$m total=$total sum=$sum")
      }
    }
    up.initInputs(); dn.initInputs()
    dut.holdIssue.get.poke(false.B)
    dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get))
    dut.io.pfRecv.get.pf_en.poke(true.B); dut.io.pfRecv.get.pf_source.poke(10.U)
    val ctrl=dut.io.pfCtrl.get
    ctrl.poke(0.U.asTypeOf(ctrl)); ctrl.l2_pf_master_en.poke((prefetchMode!="none").B)
    ctrl.l2_pf_recv_en.poke(enabled("recv").B); ctrl.l2_pbop_en.poke(enabled("bop").B)
    ctrl.l2_vbop_en.poke(enabled("vbop").B); ctrl.l2_cdp_en.poke(enabled("cdp").B)
    dut.io.pfTlb.get.req.ready.poke(true.B); dut.io.pfTlb.get.resp.valid.poke(false.B)
    dut.io.pfTlb.get.resp.bits.poke(0.U.asTypeOf(dut.io.pfTlb.get.resp.bits))
    dut.io.pfTlb.get.pmp_resp.poke(0.U.asTypeOf(dut.io.pfTlb.get.pmp_resp))
    dut.io.pfTrain.get.foreach(t=>t.poke(0.U.asTypeOf(t)))
    dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B); dn.activateRxLink()
    for(c <- -300 until 0) dn.stepWithCredits(c)
    var quiet=0; var stimulus=true; var activeCycles=0
    while(quiet<64 && cycle<minimum+1000000) {
      if(analysis && cycle<=minimum && cycle>0 && (cycle%1000==0 || cycle==minimum)) analysisWindow(cycle)
      up.clearResponses(); dn.clearResponses()
      stimulus=cycle<minimum
      if(stimulus) activeCycles+=1
      var tapeDemand=BigInt(-1);var tapeSnoop=BigInt(-1)
      val tape=replayTape.getOrElse(cycle,Tape(-1,false,0,-1))
      if(replayPath.nonEmpty && stimulus && tape.demand>=0) {
        driver.enqueue(Seq(dummy.copy(addr=tape.demand))); submitted+=1
      }
      if(replayPath.isEmpty && stimulus && cycle%demandPeriod==0 && driver.pending<(if(item=="S08") budget*slices else budget)) {
        val line=pattern match {
          case "stream" => submitted%4096
          case "hot1" => 0
          case "hot2" => submitted%2
          case "hot4" => submitted%4
          case "hot-slice" => submitted%4096
          case "all-max" => if(cycle%4096<2048) submitted%4 else submitted%4096
          case "hot" => submitted%4
          case "rotate" => (cycle/4096)%3 match {case 0=>submitted%4096; case 1=>random.nextInt(1024); case _=>submitted%4}
          case _ => if(random.nextInt(4)==0) random.nextInt(64) else random.nextInt(1024)
        }
        val pool=(0 until contentionSets*contentionLines).map(poolAddress)
        val selectedPool=if(pattern=="hot-slice") pool.filter(a=>bankOf(a)==0) else pool
        var addr=if(pattern=="contention") pool(random.nextInt(pool.size))
          else if(shrinkAddresses) selectedPool(line%selectedPool.size)
          else BigInt(0x200000+line*64)
        if(pattern=="hot-slice") while(bankOf(addr)!=0) addr+=64
        tapeDemand=addr
        driver.enqueue(Seq(dummy.copy(addr=addr)))
        submitted+=1
      }
      driver.drive(cycle)
      val train=dut.io.pfTrain.get.head
      train.vaddrBlock.poke(((value(dut.io.t1p.head.UpREQ.bits.Addr)^StressMemory.vaMask)>>6).U)
      train.trainEnable.poke(((value(dut.io.t1p.head.UpREQ.bits.Addr)>>6)%3!=0).B); train.reqSource.poke(((value(dut.io.t1p.head.UpREQ.bits.Addr)>>6)&31).U)
      // Burst Recv, with occasional issue pauses to build source queues.
      dut.holdIssue.get.poke((stimulus && issueHold>0 && cycle%(issueHold+64)<issueHold).B)
      val generatedRecv=stimulus && (if(recvBurst>0) cycle%(recvBurst+64)<recvBurst else random.nextDouble()*100<recvPct)
      val recv=if(replayPath.nonEmpty) stimulus && tape.recv else generatedRecv
      dut.io.pfRecv.get.addr_valid.poke(recv.B)
      val recvLine=if(random.nextInt(100)<recvHot) random.nextInt(512)*slices else random.nextInt(512)
      // 1-slice is trivially hot; multi-slice targets are chosen by host SAM.
      def hostBank(addr: BigInt): Int = if(slices==1) 0 else if(slices==2) ((addr>>6).bitCount&1) else {
        val lo=(6 until 48 by 2).count(addr.testBit(_))&1
        val hi=(7 until 48 by 2).count(addr.testBit(_))&1; lo|(hi<<1)
      }
      var recvAddr=BigInt(0x210000)+recvLine*64
      if(recvHot>0 && random.nextInt(100)<recvHot) while(hostBank(recvAddr)!=0) recvAddr+=64
      if(pattern=="contention" || shrinkAddresses) {
        val pool=(0 until contentionSets*contentionLines).map(poolAddress)
        val selectedPool=if(recvHot>0 && random.nextInt(100)<recvHot) pool.filter(a=>hostBank(a)==0) else pool
        recvAddr=selectedPool(random.nextInt(selectedPool.size))
      }
      if(replayPath.nonEmpty) recvAddr=tape.recvAddr
      dut.io.pfRecv.get.addr.poke(recvAddr.U)
      val tlb=dut.io.pfTlb.get
      tlb.req.ready.poke((!stimulus || tlbHold==0 || cycle%(tlbHold+64)>=tlbHold).B)
      tlb.resp.valid.poke(false.B); tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits))
      tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      replies.headOption.filter(_.due<=cycle).foreach { t =>
        replies.dequeue(); tlb.resp.valid.poke(true.B); tlb.resp.bits.paddr.head.poke(physical(t.va).U)
        tlb.resp.bits.miss.poke(t.miss.B); tlb.resp.bits.excp.head.pf.ld.poke(t.fault.B)
        if(!t.miss || t.attempt==2) {
          terminals((t.client,t.va))=t
          modelDone+=1
          if(t.miss || t.fault) modelDrops+=1
          else if(t.client==0) allowed(0)(physical(t.va))+=1
        } else modelRetries+=1
      }
      for(i<-0 until 16) {
        val e=value(dut.cdpEntryProbe(i));val valid=e.testBit(87);val pvalid=e.testBit(86)
        assert(valid==cdpExpected.contains(i),s"CDP slot identity cycle=$cycle index=$i")
        if(valid) {val(va,pa)=cdpExpected(i);assert(((e>>42)&((BigInt(1)<<44)-1))==(va>>6),"CDP VA identity");assert(pvalid==pa.nonEmpty,"CDP translated flag");pa.foreach(p=>assert((e&((BigInt(1)<<42)-1))==(p>>6),"CDP PA mapping"))}
      }
      // Compare adapter retirements to an independent request/response model.
      for(c <- 0 until 2) {
        val r=dut.probe.clientResult(c)
        if(flag(r.valid)) {
          val va=value(r.bits.vaddr); val key=(c,va)
          val t=terminals.remove(key).getOrElse(throw new AssertionError(s"unexpected adapter completion $key"))
          val retried=transactions.remove(key).get==2
          assert(flag(r.bits.retried)==retried && flag(r.bits.miss)==t.miss && flag(r.bits.fault)==(!t.miss && t.fault))
          assert(flag(r.bits.dropped)==(t.miss || t.fault) && value(r.bits.paddr)==physical(va),s"translation result mismatch $key")
          if(c==1) for((i,(line,pa))<-cdpExpected.toSeq if pa.isEmpty && (line>>12)==(va>>12)) {
            if(flag(r.bits.dropped)) cdpExpected.remove(i)
            else cdpExpected(i)=(line,Some(physical(line)))
          }
          actualDone+=1; if(retried) actualRetries+=1
          if(flag(r.bits.dropped)) actualDrops+=1
        }
      }
      for(c <- 0 until 2) {
        val r=dut.probe.clientReq(c)
        if(flag(r.valid)) {
          val key=(c,value(r.bits))
          assert(!transactions.contains(key),s"overlapping client/VA ownership $key")
          transactions(key)=0
        }
      }
      if(flag(dut.probe.fastAllocate.valid)) {
        val bits=value(dut.probe.fastAllocate.bits)
        val va=bits>>48; val pa=bits & ((BigInt(1)<<48)-1)
        assert(fast && physical(va)==pa,s"D3 PA incorrect VA=$va PA=$pa")
        fastAllocations+=1; allowed(0)(pa)+=1
      }
      assert(!flag(dut.probe.fastTranslatedAtTlb),"D3 translated slot requested TLB")
      if(flag(tlb.req.valid)&&flag(tlb.req.ready)) {
        val c=if(flag(dut.probe.tlbOwner)) 1 else 0
        val va=value(tlb.req.bits.vaddr); val key=(c,va)
        assert(transactions.contains(key),s"external TLB without client request $key")
        transactions(key)+=1; val attempt=transactions(key)
        assert(attempt<=2 && flag(dut.probe.tlbRetry)==(attempt==2),s"retry mismatch $key attempt=$attempt")
        val miss=random.nextInt(100)<missPct
        val fault=random.nextInt(100)<faultPct
        if(miss) injectedMisses+=1
        if(fault) injectedFaults+=1
        replies.enqueue(Translation(c,va,cycle+tlbMin+random.nextInt(tlbMax-tlbMin+1),miss,fault,attempt))
        translations+=1
      }
      val sev=value(dut.cdpSendEvents)
      val outIndex=((sev>>16)&15).toInt;val outFire=sev.testBit(20);val filterIndex=((sev>>21)&15).toInt;val filterDrop=sev.testBit(25)
      val allocatedVA=(sev>>26)&((BigInt(1)<<50)-1);val allocatedIndex=((sev>>76)&15).toInt;val allocated=sev.testBit(80)
      if(outFire) {val e=cdpExpected.remove(outIndex).getOrElse(throw new AssertionError("CDP output without live slot"));assert(e._2.contains(value(dut.probe.algorithm(1).bits)),"CDP output without matching successful page translation")}
      if(filterDrop) {val e=cdpExpected.remove(filterIndex).getOrElse(throw new AssertionError("CDP filter drop missing slot"));assert(e._2.nonEmpty,"CDP filter before translation")}
      if(allocated) {assert(!cdpExpected.contains(allocatedIndex));cdpExpected(allocatedIndex)=(allocatedVA,None)}
      for(c <- 0 until 2) {
        val a=dut.probe.algorithm(c)
        if(flag(a.valid)) {
          val pa=value(a.bits)
          if(c==0) {assert(allowed(c)(pa)>0,s"algorithm issued without successful translation/fast path client=$c PA=$pa");allowed(c)(pa)-=1}
          if(fast && c==0) fastEmits+=1
        }
      }
      var chosenData:Option[PendingDnComp]=None
      var chosenResponsePA:Option[BigInt]=None
      def dataLegal(d:PendingDnComp)=hnOrdering.canData(d.srcId,d.addr)
      if(!hnExtension) {
      if(flag(dut.io.chi.txreq.flitv)) {
        val f=dut.io.chi.txreq.flit
        val id=DnTxnID(value(f.TxnID.get)); val dst=value(f.SrcID.get)
        val op=value(f.Opcode.get); val addr=value(f.Addr.get)
        trace(s"DN_REQ id=$id op=$op addr=${addr.toString(16)}")
        responseAddresses(id)=addr
        val allowRetry=value(f.AllowRetry.get)!=0
        val isReissue=retryLive.contains(id)
        if(isReissue) {
          val old=retryLive.remove(id).get
          assert(old==(addr,op,dst) && !allowRetry && value(f.PCrdType.get)==0,s"reissue identity $id")
          reissues+=1
        } else {
          assert(!live(id),s"downstream live TxnID reuse $id"); live+=id; dn.noteRequest(id)
        }
        if(qualityEnabled) qualityModel.downstream(cycle,addr,dn.isEvictReq(op))
        val delay=if(item=="S15" && cycle%2048<1024) 1024 else memMin+random.nextInt(memMax-memMin+1)
        val internal=(addr>=0x210000) // actual classification replaced by observed TSHR context below
        val tid=id.v.toInt>>log2Ceil(slices)
        val bank=id.v.toInt & (slices-1)
        val pf=flag(dut.internal(bank)(tid))
        if(!isReissue && allowRetry && random.nextInt(100)<retryPct && op!=0x05 && !dn.isEvictReq(op)) {
          retryLive(id)=(addr,op,dst)
          rsp.enqueue(PendingDnRsp(id,dst,cycle+4,3))
          rsp.enqueue(PendingDnRsp(id,dst,cycle+4+retryDelay,7))
          if(pf) retryPrefetch+=1 else retryDemand+=1
        } else if(dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id,dst,cycle+delay,dn.downstreamEvictRspOpcode(op)))
        else if(op==0x05) { assert(live.remove(id)); dn.noteResponse(id,true,cycle) }
        else {
          if(pf) knownPrefetch+=addr
          val resp=if(op==0x07) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC
          for(b <- 0 until 2) data.enqueue(PendingDnComp(id,dst,cycle+delay+b*beatGap,b,resp,addr))
        }
      }
      if(data.exists(d=>d.due<=cycle && !dataLegal(d))) dataOrderStalls+=1
      // Host CHI channels use link credits instead of ready. Measure only
      // a due item waiting for credit, separately from its configured delay.
      if(qualityEnabled) {
        qualityModel.hostChannel(cycle,"hn.rxdat",data.exists(d=>d.due<=cycle && (d.beat==0 || firstBeatSent.get(d.dnTxnID).exists(t=>cycle-t>=beatGap))),dn.canDriveRxDat)
        qualityModel.hostChannel(cycle,"hn.rxrsp",rsp.exists(_.due<=cycle),dn.canDriveRxRsp)
        qualityModel.hostChannel(cycle,"hn.rxsnp",offeredSnoops.nonEmpty,dn.canDriveRxSnp)
      }
      // Keep beat order per transaction, randomize gaps and inter-transaction latency.
      if(cycle%dataPeriod==0) data.dequeueFirst(d=>d.due<=cycle && dn.canDriveRxDat && dataLegal(d) && (d.beat==0 || firstBeatSent.get(d.dnTxnID).exists(t=>cycle-t>=beatGap))).foreach { d =>
        chosenData=Some(d)
        trace(s"DN_DATA id=${d.dnTxnID} beat=${d.beat} addr=${d.addr.toString(16)}")
        if(qualityEnabled) qualityModel.refill(cycle,d.addr,d.beat)
        dn.driveDnData(d); dut.io.chi.rxdat.flit.Data.get.poke(StressMemory.beat(d.addr,d.beat).U)
        dn.noteResponse(d.dnTxnID,d.beat==1,cycle)
        if(d.beat==0) { assert(!firstBeatSent.contains(d.dnTxnID)); firstBeatSent(d.dnTxnID)=cycle }
        else { assert(cycle-firstBeatSent.remove(d.dnTxnID).get>=beatGap) }
        if(d.beat==1) assert(live.remove(d.dnTxnID))
      }
      if(rsp.exists(r=>r.due<=cycle && !hnOrdering.canData(r.srcId,responseAddresses(r.dnTxnID)))) responseOrderStalls+=1
      rsp.dequeueFirst(r=>r.due<=cycle && dn.canDriveRxRsp && hnOrdering.canData(r.srcId,responseAddresses(r.dnTxnID))).foreach { r =>
        chosenResponsePA=Some(responseAddresses(r.dnTxnID))
        dn.driveDnRsp(r)
        if(r.opcode==7) pcrdSent+=1
        else if(r.opcode!=3) { dn.noteResponse(r.dnTxnID,true,cycle); assert(live.remove(r.dnTxnID)) }
      }
      }
      // Preserve every offer across credit/order stalls, as in pf-v2.
      if(replayPath.nonEmpty && stimulus && tape.snoop>=0) {
        offeredSnoops.enqueue(tape.snoop);replaySnoopOffered+=1;snoopOffers+=1
      }
      var generateSnoop=false
      if(replayPath.isEmpty && stimulus && cycle<snoopStop) {
        if(rateUnits>0) {
          rateAccumulator+=rateUnits
          if(rateAccumulator>=1000000) {rateAccumulator-=1000000;generateSnoop=true}
        } else generateSnoop=snpPeriod>0 && cycle%snpPeriod==0
      }
      if(generateSnoop && (snoopTargets.nonEmpty || shrinkAddresses || pattern=="contention" || knownPrefetch.nonEmpty)) {
        val candidate=if(snoopTargets.nonEmpty) snoopTargets(snoopRandom.nextInt(snoopTargets.size)) else if(shrinkAddresses || pattern=="contention") {
          extension.flatMap(_.activeReadRequests.headOption.map(_.addr)).orElse(data.headOption.map(_.addr)).getOrElse(poolAddress(snoopRandom.nextInt(contentionSets*contentionLines)))
        } else if(knownPrefetch.nonEmpty) knownPrefetch(snoopRandom.nextInt(knownPrefetch.size))
        else BigInt(0x200000)
        offeredSnoops.enqueue(candidate);snoopOffers+=1;tapeSnoop=candidate
      }
      if(hnExtension) {
        while(offeredSnoops.nonEmpty)extPins.get.offer(offeredSnoops.dequeue())
        extPins.get.drive(cycle,_=>false)
        if(flag(dut.io.chi.rxsnp.flitv)) {
          snpSent+=1;if(stimulus)stimulusSnoops+=1
          if(qualityEnabled)qualityModel.snoop(cycle,value(dut.io.chi.rxsnp.flit.Addr.get)<<3)
        }
      } else {
      // These are the exact pf-v2 three guards: old scoreboard state,
      // same-edge chosen CompData, and same-edge chosen completion.
      def snoopLegal(addr:BigInt)=hnOrdering.canSnoop(hnRequester,addr)&&
        !chosenData.exists(d=>(d.addr>>6)==(addr>>6)&&d.srcId==hnRequester)&&
        !chosenResponsePA.exists(pa=>(pa>>6)==(addr>>6))
      if(offeredSnoops.headOption.exists(a=> !snoopLegal(a))) snoopOrderStalls+=1
      if(qualityEnabled) {
        qualityModel.hostChannel(cycle,"hn.snpOrder",offeredSnoops.nonEmpty,offeredSnoops.headOption.forall(snoopLegal))
        qualityModel.hostChannel(cycle,"hn.dataOrder",data.exists(_.due<=cycle),data.filter(_.due<=cycle).forall(dataLegal))
        qualityModel.hostChannel(cycle,"hn.rspOrder",rsp.exists(_.due<=cycle),rsp.filter(_.due<=cycle).forall(r=>hnOrdering.canData(r.srcId,responseAddresses(r.dnTxnID))))
      }
      offeredSnoops.headOption.filter(addr=>dn.canDriveRxSnp&&snoopLegal(addr)).foreach {addr=>
        val width=dut.io.chi.rxsnp.flit.TxnID.get.getWidth
        (0 until (1<<width)).find(i=> !snpLive(BigInt(i))).foreach { id=>
          offeredSnoops.dequeue()
          if(replayPath.nonEmpty) replaySnoopSent+=1
          if(qualityEnabled) qualityModel.snoop(cycle,addr)
          dut.io.chi.rxsnp.flitv.poke(true.B); dut.io.chi.rxsnp.flitpend.poke(true.B)
          dut.io.chi.rxsnp.flit.poke(0.U.asTypeOf(dut.io.chi.rxsnp.flit))
          dut.io.chi.rxsnp.flit.Opcode.get.poke(0x07.U)
          dut.io.chi.rxsnp.flit.DoNotGoToSD_DoNotDataPull.get.poke(1.U)
          dut.io.chi.rxsnp.flit.Addr.get.poke((addr>>3).U)
          dut.io.chi.rxsnp.flit.TxnID.get.poke(id.U)
          trace(s"DN_SNP id=$id addr=${addr.toString(16)}")
          snpLive+=BigInt(id); snpSent+=1
          if(stimulus) stimulusSnoops+=1
        }
      }
      if(flag(dut.io.chi.txrsp.flitv) && value(dut.io.chi.txrsp.flit.Opcode.get)==1) {
        val id=value(dut.io.chi.txrsp.flit.TxnID.get)
        trace(s"DN_SNP_RESP id=$id")
        assert(snpLive.remove(id),s"unmatched snoop response $id"); snpDone+=1
      }
      }
      if(!flag(dut.io.t1p.head.UpRSP.valid)) snoops.headOption.foreach { case(id,src)=>
        up.driveUpSnpResp(id,src)
        if(flag(dut.io.t1p.head.UpRSP.ready)) snoops.dequeue()
      }
      if(flag(dut.io.t1p.head.DnSNP.valid)) snoops.enqueue((SnpTxnID(value(dut.io.t1p.head.DnSNP.bits.TxnID)),value(dut.io.t1p.head.DnSNP.bits.SrcID)))
      if(cycle%16384==0) println(s"STRESS_PROGRESS cycle=$cycle seed=$seed slices=$slices submitted=$submitted completed=${driver.completed} sources=${sources.map(src=>src.toString+":"+entered.map(_(src)).sum).mkString(",")} produced=${sources.map(src=>src.toString+":"+produced.map(_(src)).sum).mkString(",")}")
      if(cycle%64==0) check() // pre-edge counters equal events observed through previous edge
      if(recv && enabled("recv")) recvInputs+=1
      for(s <- 0 until slices) {
        val mask=value(dut.probe.produced(s)).toInt
        sources.zipWithIndex.foreach { case(src,b)=>if((mask&(1<<b))!=0) produced(s)(src)+=1 }
        if(flag(dut.probe.entered(s).valid)) {
          val source=value(dut.probe.entered(s).bits).toInt
          entered(s)(source)+=1
          if(stimulus) { maxEnterGap(source)=maxEnterGap(source).max(cycle-lastEnter(source)); lastEnter(source)=cycle }
        }
      }

    if(hnExtension) {
      extPins.get.sample(cycle)
      val m=extension.get
      while(extCompletedIndex<m.completedRequests.size) {
        val (_,q)=m.completedRequests(extCompletedIndex);extCompletedIndex+=1
        val id=DnTxnID(q.txn);assert(live.remove(id));dn.noteResponse(id,true,cycle)
      }
      snpDone=m.counts("snoopCompleted").toInt
      hnEvents.foreach{case(c,pa,name)=>overlap.event(c,pa,name)};hnEvents.clear()
      if(flag(dut.probe.algorithm(1).valid))overlap.event(cycle,value(dut.probe.algorithm(1).bits),"cdpEmitted")
      overlap.tick(cycle);hnLastSample=cycle
    } else {
    // Independently sample the actual pins, rather than trusting queue choices.
    // Outgoing messages are checked before incoming responses at this edge:
    // a response observed now only releases an ordering barrier next cycle.
    if(flag(dut.io.chi.txreq.flitv)) {
      val f=dut.io.chi.txreq.flit
      if(dn.isEvictReq(value(f.Opcode.get)))
        hnOrdering.eviction(value(f.SrcID.get),value(f.TxnID.get),value(f.Addr.get))
      else
        hnOrdering.request(value(f.SrcID.get),value(f.TxnID.get),value(f.Addr.get),value(f.ExpCompAck.get)!=0)
    }
    if(flag(dut.io.chi.rxdat.flitv)) {
      val f=dut.io.chi.rxdat.flit
      hnOrdering.data(value(f.TgtID.get),value(f.TxnID.get),value(f.DataID.get).toInt,cycle)
    }
    if(flag(dut.io.chi.rxrsp.flitv)) {
      val f=dut.io.chi.rxrsp.flit
      hnOrdering.completion(value(f.TgtID.get),value(f.TxnID.get),value(f.DBID_PGroupID_StashGroupID_TagGroupID.get),value(f.Opcode.get).toInt,cycle)
    }
    if(flag(dut.io.chi.rxsnp.flitv)) {
      val f=dut.io.chi.rxsnp.flit
      hnOrdering.snoop(hnRequester,value(f.SrcID.get),value(f.TxnID.get),value(f.Addr.get)<<3,cycle)
    }
    if(flag(dut.io.chi.txrsp.flitv)) {
      val f=dut.io.chi.txrsp.flit;val op=value(f.Opcode.get).toInt
      hnOrdering.response(value(f.SrcID.get),value(f.TgtID.get),value(f.TxnID.get),op,cycle)
      if(op==2) trace(s"COMP_ACK cycle=$cycle rn=${value(f.SrcID.get)} txn=${value(f.TxnID.get)}")
    }
    if(flag(dut.io.chi.txdat.flitv)&&value(dut.io.chi.txdat.flit.Opcode.get)==L2TopDownstreamOpcodes.CopyBackWrData) {
      val f=dut.io.chi.txdat.flit
      hnOrdering.writeData(value(f.SrcID.get),value(f.TxnID.get),value(f.DataID.get).toInt,cycle)
      trace(s"COPYBACK_DATA cycle=$cycle rn=${value(f.SrcID.get)} dbid=${value(f.TxnID.get)} beat=${value(f.DataID.get)}")
    }
    }
      orderingSamples+=1
      sampleCensus()
      if(stimulus && flag(dut.io.t1p.head.UpREQ.valid)) {
        offered+=1; if(!flag(dut.io.t1p.head.UpREQ.ready)) blocked+=1
      }
      if(traceWindow>0) {
        val port=dut.io.t1p.head
        if(flag(port.UpRSP.valid)&&flag(port.UpRSP.ready)) trace(s"UP_RSP op=${value(port.UpRSP.bits.Opcode)} id=${value(port.UpRSP.bits.TxnID)} tgt=${value(port.UpRSP.bits.TgtID)}")
        if(flag(port.DnSNP.valid)&&flag(port.DnSNP.ready)) trace(s"UP_SNP id=${value(port.DnSNP.bits.TxnID)} src=${value(port.DnSNP.bits.SrcID)}")
        if(flag(port.DnDAT.valid)&&flag(port.DnDAT.ready)) trace(s"UP_DATA id=${value(port.DnDAT.bits.TxnID)} beat=${value(port.DnDAT.bits.DataID)}")
      }
      try driver.sample(cycle) catch {case t:Throwable=>
        if(qualityEnabled) qualityModel.report(cycle,false)
        throw t
      }
      if(stimulus) activeCompleted=driver.completed
      if(driver.completed!=lastCompleted) {
        lastProgress=cycle;lastCompleted=driver.completed;lastDemandCompleteCycle=cycle;noCompletionRun=0;activeNoCompletionRun=0
      } else if(driver.pending>0) {
        noCompletionRun+=1;maxDemandNoCompletion=maxDemandNoCompletion.max(noCompletionRun)
        if(stimulus) {activeNoCompletionRun+=1;activeNoCompletion=activeNoCompletion.max(activeNoCompletionRun)}
      } else {noCompletionRun=0;activeNoCompletionRun=0}
      if(flag(dut.io.t1p.head.UpREQ.valid)&& !flag(dut.io.t1p.head.UpREQ.ready)) {
        upReqBlockedRun+=1;maxUpReqBlocked=maxUpReqBlocked.max(upReqBlockedRun)
      } else upReqBlockedRun=0
      if(recordTape) println(s"STRESS_TAPE cycle=$cycle demand=${tapeDemand.toString(16)} recv=${if(recv) 1 else 0} recvAddr=${recvAddr.toString(16)} snoop=${tapeSnoop.toString(16)}")
      if(snapshotCycles(cycle)) {qos();dumpStall()}
      if(driver.pending!=0 && cycle-lastProgress>=20000) {
        qos();dumpStall();hnReport(false)
        if(qualityEnabled) qualityModel.report(cycle,false)
        println(s"HN_CHI_ORDER_CHECK caseId=$caseId seed=$seed pass=true closed=false violations=0 samples=$orderingSamples dataStallCycles=$dataOrderStalls snoopStallCycles=$snoopOrderStalls responseStallCycles=$responseOrderStalls")
      }
      assert(driver.pending==0 || cycle-lastProgress<20000,s"progress watchdog seed=$seed slices=$slices cycle=$cycle pending=${driver.pending}")
      val active= !flag(dut.stateEmpty)
      val pendingQueues=(0 until slices).flatMap(s=>(0 to 9).map(src=>count(dut.observation.prefetchPendingBySource,s,src))).sum
      val idle= !stimulus && driver.pending==0 && data.isEmpty && rsp.isEmpty && live.isEmpty && snoops.isEmpty && replies.isEmpty && pendingQueues==0 && !active
      val fullyIdle=idle && transactions.isEmpty && terminals.isEmpty && cdpExpected.isEmpty &&
        allowed(0).values.forall(_==0) && flag(dut.stateEmpty) && snpLive.isEmpty && retryLive.isEmpty && firstBeatSent.isEmpty && offeredSnoops.isEmpty && hnOrdering.empty && extension.forall(_.empty)
      quiet=if(fullyIdle) quiet+1 else 0
      withholding=stimulus && ((creditHold>0 && cycle%(creditHold+64)<creditHold) || random.nextInt(100)<creditRandom)
      dn.withholdRequestCredit=withholding
      try dn.stepWithCredits(cycle) catch {case t:Throwable=>
        if(qualityEnabled) qualityModel.report(cycle,false)
        throw t
      }
      cycle+=1
    }
    if(analysis) {
      val totals=analysisCounts().toSeq.sortBy(_._1).map{case(k,v)=>k+"="+v}.mkString(" ")
      println(s"STRESS_CDP_ANALYSIS caseId=$caseId seed=$seed enabled=$cdpEnabled cycle=$cycle $totals")
    }
    val cpacked=value(dut.cdpCountPacked)
    val ccvals=dut.cdpCounters.indices.map(i=>dut.cdpCounters(i)._1->((cpacked>>(64*i))&((BigInt(1)<<64)-1))).toMap
    assert(ccvals("candidate")==ccvals("allocated")+ccvals("duplicate")+ccvals("fullDrop"),"CDP candidate closure")
    assert(ccvals("allocated")==ccvals("emitted")+ccvals("filterDrop")+ccvals("translationDrop"),"CDP allocated/terminal closure")
    assert(ccvals("emitted")==produced.map(_(9)).sum,"CDP output/source closure")
    println(s"STRESS_CDP caseId=$caseId seed=$seed " + ccvals.toSeq.sortBy(_._1).map{case(k,v)=>s"$k=$v"}.mkString(" "))
    if(qualityEnabled) qualityModel.report(cycle,true)
    check()
    assert(cdpExpected.isEmpty,"CDP slots not retired")
    assert(allowed(0).values.forall(_==0),"successful translation/fast allocation did not emit exactly once")
    assert(transactions.isEmpty && terminals.isEmpty && cdpExpected.isEmpty && modelDone==actualDone && modelDrops==actualDrops && modelRetries==actualRetries,
      s"TLB ledger done=$modelDone/$actualDone retries=$modelRetries/$actualRetries drops=$modelDrops/$actualDrops live=$transactions terminals=$terminals")
    println(s"STRESS_TLB caseId=$caseId servicePeak=$servicePeak serviceCycles=$serviceCycles pass=true slices=$slices seed=$seed harsh=$harsh requests=$translations injectedMisses=$injectedMisses injectedFaults=$injectedFaults modelRetries=$modelRetries actualRetries=$actualRetries modelDrops=$modelDrops actualDrops=$actualDrops completed=$actualDone fast=$fast fastAllocations=$fastAllocations vbopEmits=$fastEmits")

    if(fast) assert(fastAllocations>0 && fastEmits>0,"D3 vacuous")
    qos()
    assert(quiet>=64,s"absolute watchdog seed=$seed slices=$slices cycle=$cycle")
    assert(activeCycles==minimum,"minimum active cycles")
    assert(driver.completed==submitted && driver.checkedBeats==submitted*2 && driver.pending==0,"scoreboard closure")
    for(i<-dut.fifos.indices) {
      assert(queueModels(i).isEmpty,s"queue not drained ${dut.fifos(i)._1}")
      println(s"STRESS_FIFO caseId=$caseId seed=$seed name=${dut.fifos(i)._1} cap=${dut.fifos(i)._2} enq=${enqs(i)} deq=${deqs(i)} overwritten=${overs(i)} pending=${queueModels(i).size}")
    }
    for(i<-dut.metrics.indices) println(s"STRESS_RESOURCE caseId=$caseId seed=$seed name=${dut.metrics(i)._1} cap=${dut.metrics(i)._3} peak=${peaks(i)} fullCycles=${fullCycles(i)}")
    println(s"STRESS_RESULT caseId=$caseId pass=true item=$item level=$level pattern=$pattern slices=$slices seed=$seed activeCycles=$activeCycles cycles=$cycle drainCycles=${cycle-minimum} demands=$submitted completed=${driver.completed} beats=${driver.checkedBeats} offeredCycles=$offered blockedCycles=$blocked activeCompleted=$activeCompleted latencyN=${driver.latencyCount} latencySum=${driver.latencySum} latencyMax=${driver.latencyMax} activeLatencyN=${driver.activeLatencyCount} activeLatencySum=${driver.activeLatencySum} activeLatencyMax=${driver.activeLatencyMax} retryDemand=$retryDemand retryPrefetch=$retryPrefetch reissues=$reissues pcrdSent=$pcrdSent snpSent=$snpSent snpDone=$snpDone uselessSnoop=${uselessSnoop.map(_.sum).sum} elapsedSeconds=${(System.nanoTime()-seedStart)/1e9}")
    assert(snpSent==snpDone && retryDemand+retryPrefetch==reissues && pcrdSent==reissues,"snoop/retry closure")
    println(s"STRESS_REPLAY caseId=$caseId mode=$prefetchMode offered=$replaySnoopOffered sent=$replaySnoopSent pending=${offeredSnoops.size} snoopDone=$snpDone")
    for(src <- sources) {
      val led=(0 until slices).map(s=>ledger(s,src)).transpose.map(_.sum)
      val useful=(0 until slices).map(s=>value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Useful))).sum
      val redundant=(0 until slices).map(s=>value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Redundant))).sum
      maxEnterGap(src)=maxEnterGap(src).max(minimum-lastEnter(src))
      println(s"STRESS_SOURCE caseId=$caseId maxEnterGap=${maxEnterGap(src)} pass=true slices=$slices seed=$seed source=$src produced=${produced.map(_(src)).sum} enqueued=${led(0)} overwritten=${led(1)} entered=${entered.map(_(src)).sum} queueEntered=${led(2)} dedup=${led(3)} pending=${led(4)} useful=$useful redundant=$redundant")
    }
    assert(hnOrdering.empty,"HN ordering closure")
    extension.foreach(_.finish());hnReport(true)
    qos()
    println(s"HN_CHI_ORDER_CHECK caseId=$caseId seed=$seed pass=true closed=true violations=0 samples=$orderingSamples dataStallCycles=$dataOrderStalls snoopStallCycles=$snoopOrderStalls responseStallCycles=$responseOrderStalls")
    println(s"STRESS_SOAK caseId=$caseId item=$item level=$level pass=true slices=$slices seed=$seed cycles=$cycle activeCycles=$activeCycles demands=$submitted completed=${driver.completed} checkedBeats=${driver.checkedBeats} translations=$translations recvInputs=$recvInputs watchdog=false scoreboard=true sourceClosure=true totalConservation=true recvConservation=true")
    } catch {
      case e:AssertionError=>
        reportHNFailure()
        suiteHadFailures=true
        println(s"STRESS_FAIL caseId=$caseId seed=$seed slices=$slices error=${e.getMessage.replace('\n',' ')}")
      case e:IllegalStateException if Option(e.getMessage).exists(m=>
        m.startsWith("HN_CHI_EXTENSION_VIOLATION")||m.startsWith("L2_CHI_PROTOCOL_VIOLATION"))=>
        // Preserve the original protocol failure even if its partial event
        // sample cannot be used for an overlap audit.
        try reportHNFailure() catch {case scala.util.control.NonFatal(d)=>println("STRESS_HN_DIAGNOSTIC_FAILURE "+d.getMessage)}
        suiteHadFailures=true
        println(s"STRESS_FAIL caseId=$caseId seed=$seed slices=$slices error=${e.getMessage.replace('\n',' ')}")
    }
    }
  }
  }
  assert(!suiteHadFailures,"one or more recorded stress seed failures")
}
