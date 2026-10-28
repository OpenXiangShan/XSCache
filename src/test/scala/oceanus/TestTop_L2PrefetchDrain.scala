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

// Read-only drain census. Missing private fields fail elaboration, as in
// L2TopObservation; no production signals are forced or rewritten.
class TestTopDrain(cfg: L2Configuration)(implicit p: Parameters) extends TestTopOvernight(cfg) {
  def field[T](owner: AnyRef, name: String): T = {
    var cls: Class[_] = owner.getClass
    while (cls != null) {
      try {
        val f = cls.getDeclaredField(name)
        f.setAccessible(true)
        return f.get(owner).asInstanceOf[T]
      } catch { case _: NoSuchFieldException => cls = cls.getSuperclass }
    }
    throw new NoSuchFieldException(s"${owner.getClass.getName}.$name")
  }
  def children(m: chisel3.experimental.BaseModule): Seq[chisel3.experimental.BaseModule] =
    m.getClass.getMethods.find(_.getName == "getIds").get.invoke(m)
      .asInstanceOf[scala.collection.Seq[AnyRef]].collect {
        case child: chisel3.experimental.BaseModule => child
      }.toSeq
  val metrics = mutable.ArrayBuffer.empty[(String, UInt)]
  def add(name: String, signal: UInt): Unit = metrics += name -> read(signal)
  def census(m: chisel3.experimental.BaseModule, path: String): Unit = {
    m match {
      case q: Queue[_] => add(path + ".count", q.io.count)
      case q: OverwriteQueue[_] => add(path + ".nonempty", !read(q.empty))
      case q: L2CDPHalfQueue => add(path + ".count", q.io.occupancy)
      case q: OceanusCDPQueue[_] => add(path + ".count", q.io.count)
      case _ =>
    }
    children(m).zipWithIndex.foreach { case (child, i) =>
      census(child, path + "/" + child.getClass.getSimpleName + i)
    }
  }
  census(module, "L2Top") // Includes every RBE, TLB inflight/completion FIFO,
  // Recv fallback, source, CDP input/half-line, VBOP input and CHI queue.
  // Read the real PBOP source head and its retirement next to the selected
  // merged output. This distinguishes entry to the slice from dedup removal.
  val sourceQueues = children(module).collect {
    case q: OverwriteQueue[_] => q.asInstanceOf[OverwriteQueue[FlitREQ]]
  }
  require(cfg.sliceNum == 1 && sourceQueues.size == module.pfSourceSlots.size + 1)
  val pbopQueue = sourceQueues(module.pfSourceSlots.indexOf(L2PfSource.PBOP))
  val merged = module.pfQueues.head
  val pbopRetire = IO(Output(Bool()))
  val pbopAddr = IO(Output(UInt(48.W)))
  val mergedFire = IO(Output(Bool()))
  val mergedAddr = IO(Output(UInt(48.W)))
  val mergedSource = IO(Output(UInt(4.W)))
  pbopRetire := read(pbopQueue.io.deq.valid) && read(pbopQueue.io.deq.ready)
  pbopAddr := read(pbopQueue.io.deq.bits.Addr)
  mergedFire := read(merged.valid) && read(merged.ready)
  mergedAddr := read(merged.bits.Addr)
  mergedSource := read(merged.bits.TxnID)(7,4)
  val starved = Seq(L2PfSource.PBOP, L2PfSource.CDP).map(src => sourceQueues(module.pfSourceSlots.indexOf(src)))
  val starvedEnq = IO(Output(Vec(2, Valid(UInt(48.W)))))
  val starvedDeq = IO(Output(Vec(2, Valid(UInt(48.W)))))
  val starvedOverwrite = IO(Output(Vec(2, Bool())))
  starved.zipWithIndex.foreach { case (q, i) =>
    starvedEnq(i).valid := read(q.io.enq.valid) && read(q.io.enq.ready)
    starvedEnq(i).bits := read(q.io.enq.bits.Addr)
    starvedDeq(i).valid := read(q.io.deq.valid) && read(q.io.deq.ready)
    starvedDeq(i).bits := read(q.io.deq.bits.Addr)
    starvedOverwrite(i) := read(q.io.overwrite)
  }
  val a = module.pfTlbAdapter.get
  for (i <- 0 until a.entries) add(s"tlb.slot$i", a.state(i))
  add("tlb.retry", PopCount(read(a.state).map(_ === 3.U)))
  add("tlb.locked", a.locked); add("tlb.pmpPair", a.paired)
  val v = module.vbopStub.get
  add("vbop.buffer", PopCount(read(v.reqFilter.valids)))
  add("vbop.allocPipeline", v.reqFilter.allocValid)
  add("vbop.generatePipeline", v.reqFilter.io.in.valid)
  add("vbop.locked", v.reqFilter.locked)
  add("vbop.waiting", PopCount(read(v.reqFilter.entries).zip(read(v.reqFilter.valids)).map {
    case (e, valid) => valid && e.waiting
  }))
  add("vbop.delay", PopCount(read(v.delayQueue.valids)))
  add("pbop.delay", PopCount(read(field[Vec[Bool]](field[AnyRef](module.bop, "delayQueue"), "valid"))))
  add("pbop.output", module.bop.io.req.valid)
  val cdp = module.cdp.get
  val send = cdp.sentUnit.get
  add("cdp.entries", PopCount(read(send.valids)))
  add("cdp.tlbHeld", send.tlbHeld)
  add("cdp.filterQuery1", send.pftS1); add("cdp.filterQuery2", send.pftS2)
  cdp.filterTraining.foreach { f =>
    Seq(f.pipe.s0_valid, f.pipe.s1_valid, f.pipe.s2_valid, f.pipe.s3_valid).zipWithIndex.foreach {
      case (valid, i) => add(s"cdp.filterTrain$i", valid)
    }
  }
  // C5: census includes every detection/degree and train pipeline valid,
  // not just queues and the old stub's single waiting bit.
  add("cdp.vtPipeline", PopCount(read(cdp.vtTrainPipe.stage_valid)))
  for ((lane, i) <- cdp.detectFrontend.lanes.zipWithIndex)
    for (stage <- Seq(lane.s1_req, lane.s2_req, lane.s3_req).zipWithIndex)
      add(s"cdp.detect$i.${stage._2}", stage._1.valid)
  module.slices.zipWithIndex.foreach { case (slice, s) =>
    field[Seq[L2TSHR]](slice, "tshrs").zipWithIndex.foreach { case (t, i) =>
      add(s"slice$s.tshr$i.valid", t.tshr_valid)
      add(s"slice$s.tshr$i.cdpPending", t.cdpPending)
    }
  }
  val drainCounts = IO(Output(Vec(metrics.size, UInt(32.W))))
  metrics.zipWithIndex.foreach { case ((_, signal), i) => drainCounts(i) := signal }
  val drainEmpty = IO(Output(Bool()))
  drainEmpty := !drainCounts.asUInt.orR
  println("DRAIN_CENSUS " + metrics.zipWithIndex.map { case ((name, _), i) => s"$i=$name" }.mkString(","))
}

object TestTop_L2PrefetchDrain extends App with ChiselSim {
  Constantin.init(false)
  def opt(n: String, d: String): String = args.find(_.startsWith(n+"=")).map(_.split("=",2)(1)).getOrElse(d)
  val slices=opt("--slices","1").toInt
  val firstSeed=opt("--seed","1").toInt
  val lastSeed=opt("--last-seed",firstSeed.toString).toInt
  val minimum=opt("--cycles","5000").toInt
  val demandPeriod=opt("--demand-period","32").toInt
  val harsh=args.contains("--harsh-tlb")
  val fast=args.contains("--fast")
  OvernightMemory.vaMask=if(fast) BigInt(0x1000000) else BigInt(0)
  def physical(va: BigInt)=va ^ OvernightMemory.vaMask
  val soak = args.contains("--soak")
  require(slices == 1 && (if(soak) firstSeed >= 1 && lastSeed >= firstSeed else Set(1, 6, 11, 16)(firstSeed) && lastSeed == firstSeed))
  require(minimum == 5000 && demandPeriod == 32 && !harsh && !fast)
  val stopCycle = minimum + 30000
  val drainLimit = stopCycle + 20000
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
  implicit val simulator: HasSimulator=CDPAuditSimulator.bounded
  simulate(new TestTopDrain(cfg)(params)) { dut =>
    for(seed <- firstSeed to lastSeed) {
    println(s"OVERNIGHT_BEGIN seed=$seed slices=$slices cycles=$minimum harsh=$harsh fast=$fast")
    val random=new scala.util.Random(0x20261021L+seed*100+slices)
    val dummy=UpRequestSpec(MasterId.t1p0,0,0,0,CCHIOpcode.ReadShared.opcode,BigInt(0),expCompData=true)
    val spec=CaseSpec("overnight",UpConfig(Seq(MasterId.t1p0),ExplicitRequestStream(Seq(dummy),Seq(BigInt(CHIFieldResp.CompData_SC))),SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(25),FixedOutstanding(64),ImmediateCreditPolicy()),30000,(0,30000),Seq.empty)
    val up=new L2TopUpstreamModel(dut,spec)
    val driver=new OvernightDemandDriver(dut,up)
    val dn=new L2TopDownstreamModel(dut,spec)
    val data=mutable.Queue.empty[PendingDnComp]
    val rsp=mutable.Queue.empty[PendingDnRsp]
    val live=mutable.Set.empty[DnTxnID]
    val snoops=mutable.Queue.empty[(SnpTxnID,BigInt)]
    case class Translation(client: Int,va: BigInt,due: Int,miss: Boolean,fault: Boolean,attempt: Int)
    val transactions=mutable.Map.empty[(Int,BigInt),Int]
    val terminals=mutable.Map.empty[(Int,BigInt),Translation]
    val allowed=Seq.fill(1)(mutable.Map.empty[BigInt,Int].withDefaultValue(0))
    val cdpLedger=new CDPCandidateLedger
    var modelRetries=0; var modelDrops=0; var modelDone=0
    var actualRetries=0; var actualDrops=0; var actualDone=0
    var injectedMisses=0; var injectedFaults=0; var fastAllocations=0; var fastEmits=0
    val replies=mutable.Queue.empty[Translation]
    val sources=Seq(1,2,3,9)
    val produced=Array.fill(slices,10)(BigInt(0))
    val entered=Array.fill(slices,10)(BigInt(0))
    var cycle=0; var withdrawn=0; var submitted=0; var recvInputs=0; var translations=0
    var lastProgress=0; var lastCompleted=0
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
    up.initInputs(); dn.initInputs(); dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
    dut.holdIssue.get.poke(false.B)
    dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get))
    dut.io.pfRecv.get.pf_en.poke(true.B); dut.io.pfRecv.get.pf_source.poke(10.U)
    val ctrl=dut.io.pfCtrl.get
    ctrl.poke(0.U.asTypeOf(ctrl)); ctrl.l2_pf_master_en.poke(true.B)
    ctrl.l2_pf_recv_en.poke(true.B); ctrl.l2_pbop_en.poke(true.B)
    ctrl.l2_vbop_en.poke(true.B); ctrl.l2_cdp_en.poke(true.B)
    dut.io.pfTlb.get.req.ready.poke(true.B); dut.io.pfTlb.get.resp.valid.poke(false.B)
    dut.io.pfTlb.get.resp.bits.poke(0.U.asTypeOf(dut.io.pfTlb.get.resp.bits))
    dut.io.pfTlb.get.pmp_resp.poke(0.U.asTypeOf(dut.io.pfTlb.get.pmp_resp))
    dut.io.pfTrain.get.foreach(t=>t.poke(0.U.asTypeOf(t)))
    dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B); dn.activateRxLink()
    for(c <- -300 until 0) dn.stepWithCredits(c)
    var quiet=0; var stimulus=true; var activeCycles=0
    var firstEmpty = -1
    // Intent (audit-fix-1): account for every resident starved item through
    // entry, same-address dedup or overwrite; entry alone is not progress.
    // Record PBOP generation/retirement addresses to distinguish repeated
    // generation from a historical-address filter (which this merge lacks).
    val stopEntered = Array.fill(slices, 10)(BigInt(0))
    case class Item(id: Int, addr: BigInt)
    val residents = Seq.fill(2)(mutable.Queue.empty[Item])
    val stopped = Seq.fill(2)(mutable.Set.empty[Int])
    val resolved = Seq.fill(2)(mutable.Map.empty[Int,String])
    var nextItem = 0
    val birth = mutable.Map.empty[(Int, BigInt), Int]
    def snapshot(tag: String): Unit = {
      val hardware = dut.metrics.indices.map(i => dut.metrics(i)._1 + "=" + value(dut.drainCounts(i))).mkString(",")
      println(s"DRAIN_STATE tag=$tag cycle=$cycle seed=$seed submitted=$submitted accepted=${driver.accepted.size} completed=${driver.completed} demandPending=${driver.pending} demandQueued=${driver.queued} data=${data.size} rsp=${rsp.size} dn=${live.size} snoops=${snoops.size} replies=${replies.size} transactions=$transactions terminals=$terminals allowed=${allowed.map(_.values.sum)} hardware=$hardware")
      for (src <- 0 to 9) println(s"DRAIN_LEDGER tag=$tag cycle=$cycle source=$src produced=${produced.map(_(src)).sum} entered=${entered.map(_(src)).sum} ledger=${ledger(0,src).mkString(",")}")
    }
    while(quiet<40 && cycle<drainLimit) {
      up.clearResponses(); dn.clearResponses()
      if(cycle == stopCycle) {
        check(); snapshot("original-stop")
        // A held valid is still external, unaccepted traffic. Withdraw it
        // at the cut together with unsent model work; never delete active reads
        // or completion ACKs. Keep planned/withdrawn/accepted counts separate.
        withdrawn = driver.queued
        dut.field[mutable.Map[MasterId, UpRequestSpec]](driver, "held").clear()
        dut.field[Map[MasterId, mutable.Queue[UpRequestSpec]]](driver, "queues").values.foreach(_.clear())
        assert(driver.queued == 0 && driver.accepted.size == submitted-withdrawn)
        println(s"DRAIN_DEMAND_CUT seed=$seed cycle=$cycle planned=$submitted withdrawn=$withdrawn accepted=${driver.accepted.size}")
        for(s <- 0 until slices; src <- 0 to 9) stopEntered(s)(src) = entered(s)(src)
        for(i <- 0 until 2) {
          assert(residents(i).size == ledger(0,Seq(3,9)(i))(4).toInt)
          stopped(i) ++= residents(i).map(_.id)
          println(s"DRAIN_RESIDENT seed=$seed source=${Seq(3,9)(i)} items=${residents(i).mkString(",")} uniqueAddresses=${residents(i).map(_.addr).distinct.size}")
        }
        // Reproduce the original end-of-loop assertion ordering before draining.
        val reason = if(transactions.nonEmpty || terminals.nonEmpty) "TLB-ledger" else "absolute-watchdog"
        println(s"DRAIN_STOP seed=$seed cycle=$cycle reason=$reason")
        // C5 changes CDP occupancy/TLB traffic; the historical stub's exact
        // seed16 transaction at this cut is no longer an expected invariant.
      }
      stimulus=cycle<stopCycle && (cycle<minimum || !sources.forall(src=>entered.map(_(src)).sum>0))
      if(stimulus) activeCycles+=1
      if(stimulus && cycle%demandPeriod==0 && driver.pending<20 && random.nextInt(3)!=0) {
        // Repeated working set plus fresh lines exercises hits and refills.
        val line=if(random.nextInt(4)==0) random.nextInt(64) else random.nextInt(1024)
        driver.enqueue(Seq(dummy.copy(addr=BigInt(0x200000+line*64))))
        submitted+=1
      }
      driver.drive(cycle)
      val train=dut.io.pfTrain.get.head
      train.vaddrBlock.poke(((value(dut.io.t1p.head.UpREQ.bits.Addr)^OvernightMemory.vaMask)>>6).U)
      train.trainEnable.poke(true.B); train.reqSource.poke((cycle%32).U)
      // Burst Recv, with occasional issue pauses to build source queues.
      dut.holdIssue.get.poke((stimulus && cycle%311<75).B)
      val recv=stimulus && (cycle%257<48 || random.nextInt(29)==0)
      dut.io.pfRecv.get.addr_valid.poke(recv.B)
      dut.io.pfRecv.get.addr.poke((BigInt(0x210000)+random.nextInt(512)*64).U)
      val tlb=dut.io.pfTlb.get
      tlb.req.ready.poke((random.nextInt(5)!=0).B)
      tlb.resp.valid.poke(false.B); tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits))
      tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      replies.headOption.filter(_.due<=cycle).foreach { t =>
        replies.dequeue()
        if(cycle >= stopCycle || (seed == 16 && (t.client,t.va) == (0,BigInt(2161024))))
          println(s"DRAIN_TLB event=external-response seed=$seed cycle=$cycle client=${t.client} va=${t.va} attempt=${t.attempt}")
        tlb.resp.valid.poke(true.B); tlb.resp.bits.paddr.head.poke(physical(t.va).U)
        tlb.resp.bits.miss.poke(t.miss.B); tlb.resp.bits.excp.head.pf.ld.poke(t.fault.B)
        if(!t.miss || t.attempt==2) {
          terminals((t.client,t.va))=t
          modelDone+=1
          if(t.miss || t.fault) modelDrops+=1
          else if(t.client==0) allowed(0)(physical(t.va))+=1
        } else modelRetries+=1
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
          if(cycle >= stopCycle || (seed == 16 && va == BigInt(2161024)))
            println(s"DRAIN_TLB event=client-complete seed=$seed cycle=$cycle client=$c va=$va accepted=${birth(key)} retried=$retried")
          birth.remove(key)
          actualDone+=1; if(retried) actualRetries+=1
          if(flag(r.bits.dropped)) actualDrops+=1
        }
      }
      for(c <- 0 until 2) {
        val r=dut.probe.clientReq(c)
        if(flag(r.valid)) {
          val key=(c,value(r.bits))
          assert(!transactions.contains(key),s"overlapping client/VA ownership $key")
          transactions(key)=0; birth(key)=cycle
          if(cycle >= stopCycle || (seed == 16 && key == (0,BigInt(2161024))))
            println(s"DRAIN_TLB event=client-accept seed=$seed cycle=$cycle client=$c va=${key._2}")
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
        val miss=harsh && random.nextInt(100)<30
        val fault=harsh && random.nextInt(100)<5
        if(miss) injectedMisses+=1
        if(fault) injectedFaults+=1
        replies.enqueue(Translation(c,va,cycle+(if(harsh) 1+random.nextInt(7) else Seq(1,3,7)(random.nextInt(3))),miss,fault,attempt))
        if(cycle >= stopCycle || (seed == 16 && key == (0,BigInt(2161024))))
          println(s"DRAIN_TLB event=external-request seed=$seed cycle=$cycle client=$c va=$va attempt=$attempt due=${replies.last.due}")
        translations+=1
      }
      cdpLedger.sample(dut.cdpAudit, cycle)
      for(c <- 0 until 2) {
        val a=dut.probe.algorithm(c)
        if(flag(a.valid)) {
          val pa=value(a.bits)
          if(c==0) {
            assert(allowed(0)(pa)>0,s"VBOP issued without successful translation/fast path PA=$pa")
            allowed(0)(pa)-=1
          }
          if(fast && c==0) fastEmits+=1
        }
      }
      if(flag(dut.io.chi.txreq.flitv)) {
        val f=dut.io.chi.txreq.flit
        val id=DnTxnID(value(f.TxnID.get)); val src=value(f.SrcID.get)
        val op=value(f.Opcode.get); val addr=value(f.Addr.get)
        assert(!live(id),s"downstream live TxnID reuse $id"); live+=id; dn.noteRequest(id)
        val delay=12+random.nextInt(48)
        if(dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id,src,cycle+delay,dn.downstreamEvictRspOpcode(op)))
        else {
          val resp=if(op==0x07) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC
          for(b <- 0 until 2) data.enqueue(PendingDnComp(id,src,cycle+delay+b,b,resp,addr))
        }
      }
      // Keep beat order per transaction, randomize gaps and inter-transaction latency.
      if(random.nextInt(5)!=0) data.dequeueFirst(d=>d.due<=cycle && dn.canDriveRxDat).foreach { d =>
        dn.driveDnData(d); dut.io.chi.rxdat.flit.Data.get.poke(OvernightMemory.beat(d.addr,d.beat).U)
        dn.noteResponse(d.dnTxnID,d.beat==1,cycle)
        if(d.beat==1) assert(live.remove(d.dnTxnID))
      }
      rsp.dequeueFirst(r=>r.due<=cycle && dn.canDriveRxRsp).foreach { r =>
        dn.driveDnRsp(r); dn.noteResponse(r.dnTxnID,true,cycle); assert(live.remove(r.dnTxnID))
      }
      if(!flag(dut.io.t1p.head.UpRSP.valid)) snoops.headOption.foreach { case(id,src)=>
        up.driveUpSnpResp(id,src)
        if(flag(dut.io.t1p.head.UpRSP.ready)) snoops.dequeue()
      }
      if(flag(dut.io.t1p.head.DnSNP.valid)) snoops.enqueue((SnpTxnID(value(dut.io.t1p.head.DnSNP.bits.TxnID)),value(dut.io.t1p.head.DnSNP.bits.SrcID)))
      if(cycle%1024==0) println(s"OVERNIGHT_PROGRESS cycle=$cycle seed=$seed slices=$slices submitted=$submitted completed=${driver.completed} sources=${sources.map(src=>src.toString+":"+entered.map(_(src)).sum).mkString(",")} produced=${sources.map(src=>src.toString+":"+produced.map(_(src)).sum).mkString(",")}")
      if(cycle%64==0) check() // pre-edge counters equal events observed through previous edge
      if(recv) recvInputs+=1
      for(s <- 0 until slices) {
        val mask=value(dut.probe.produced(s)).toInt
        sources.zipWithIndex.foreach { case(src,b)=>if((mask&(1<<b))!=0) produced(s)(src)+=1 }
        if(flag(dut.probe.entered(s).valid)) {
          val src=value(dut.probe.entered(s).bits).toInt
          if(cycle >= stopCycle && Set(3,9)(src) && entered(s)(src) == stopEntered(s)(src))
            println(s"DRAIN_STARVED_ENTER seed=$seed cycle=$cycle source=$src")
          entered(s)(src)+=1
        }
      }
      for(i <- 0 until 2) {
        def retire(reason: String): Unit = {
          val item = residents(i).dequeue()
          if(reason != "overwrite") assert(item.addr == value(dut.starvedDeq(i).bits))
          if(stopped(i)(item.id)) {
            assert(!resolved(i).contains(item.id)); resolved(i)(item.id)=reason
            println(s"DRAIN_ITEM seed=$seed cycle=$cycle source=${Seq(3,9)(i)} id=${item.id} addr=${item.addr} reason=$reason")
          }
        }
        if(flag(dut.starvedDeq(i).valid)) retire(if(flag(dut.mergedFire) && value(dut.mergedSource)==Seq(3,9)(i)) "entered" else "dedup")
        if(flag(dut.starvedOverwrite(i))) retire("overwrite")
        if(flag(dut.starvedEnq(i).valid)) { residents(i).enqueue(Item(nextItem,value(dut.starvedEnq(i).bits))); nextItem+=1 }
      }
      if(cycle >= stopCycle && flag(dut.pbopRetire))
        println(s"DRAIN_PBOP_RETIRE seed=$seed cycle=$cycle addr=${value(dut.pbopAddr)} mergedFire=${flag(dut.mergedFire)} mergedSource=${value(dut.mergedSource)} mergedAddr=${value(dut.mergedAddr)}")
      driver.sample(cycle)
      if(driver.completed!=lastCompleted) { lastProgress=cycle; lastCompleted=driver.completed }
      assert(cycle>=stopCycle || cycle-lastProgress<4000,s"progress watchdog seed=$seed slices=$slices cycle=$cycle pending=${driver.pending}")
      val active=dut.observation.dirProbeBySlice.flatten.exists(p=>flag(p.valid)||flag(p.reqActive))
      val pendingQueues=(0 until slices).flatMap(s=>(0 to 9).map(src=>count(dut.observation.prefetchPendingBySource,s,src))).sum
      val idle= !stimulus && driver.pending==0 && data.isEmpty && rsp.isEmpty && live.isEmpty && snoops.isEmpty && replies.isEmpty && pendingQueues==0 && !active
      val fullyIdle=idle && transactions.isEmpty && terminals.isEmpty && replies.isEmpty &&
        allowed(0).values.forall(_==0) && cdpLedger.pending==0 && flag(dut.drainEmpty)
      if(cycle >= stopCycle && fullyIdle && firstEmpty < 0) firstEmpty=cycle
      quiet=if(cycle >= stopCycle && fullyIdle) quiet+1 else 0
      if(cycle >= stopCycle && (cycle % 1024 == 0 || quiet == 40)) snapshot("drain")
      dn.stepWithCredits(cycle); cycle+=1
    }
    check(); snapshot("final")
    println(s"DRAIN_RESULT seed=$seed pass=${quiet>=40} stop=$stopCycle firstEmpty=$firstEmpty end=$cycle drainedIn=${if(firstEmpty<0) -1 else firstEmpty-stopCycle} quiet=$quiet")
    assert(quiet>=40 && flag(dut.drainEmpty), "drain timeout: see final state")
    cdpLedger.finish(s"slices=$slices seed=$seed")
    assert(allowed(0).values.forall(_==0),"successful translation/fast allocation did not emit exactly once")
    assert(transactions.isEmpty && terminals.isEmpty && modelDone==actualDone && modelDrops==actualDrops && modelRetries==actualRetries,
      s"TLB ledger done=$modelDone/$actualDone retries=$modelRetries/$actualRetries drops=$modelDrops/$actualDrops live=$transactions terminals=$terminals")
    println(s"OVERNIGHT_TLB pass=true slices=$slices seed=$seed harsh=$harsh requests=$translations injectedMisses=$injectedMisses injectedFaults=$injectedFaults modelRetries=$modelRetries actualRetries=$actualRetries modelDrops=$modelDrops actualDrops=$actualDrops completed=$actualDone fast=$fast fastAllocations=$fastAllocations vbopEmits=$fastEmits")
    if(harsh) assert(injectedMisses>0 && injectedFaults>0 && modelDrops>0 && modelRetries>0,"harsh TLB vacuous")
    if(fast) assert(fastAllocations>0 && fastEmits>0,"D3 vacuous")
    assert(quiet>=40,s"absolute watchdog seed=$seed slices=$slices cycle=$cycle")
    assert(activeCycles>=minimum,"minimum active cycles")
    assert(driver.completed==submitted-withdrawn && driver.checkedBeats==(submitted-withdrawn)*2 && driver.pending==0,"scoreboard closure")
    for(src <- sources) {
      val led=(0 until slices).map(s=>ledger(s,src)).transpose.map(_.sum)
      val useful=(0 until slices).map(s=>value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Useful))).sum
      val redundant=(0 until slices).map(s=>value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Redundant))).sum
      println(s"OVERNIGHT_SOURCE pass=true slices=$slices seed=$seed source=$src produced=${produced.map(_(src)).sum} enqueued=${led(0)} overwritten=${led(1)} entered=${entered.map(_(src)).sum} queueEntered=${led(2)} dedup=${led(3)} pending=${led(4)} useful=$useful redundant=$redundant")
    }
    println(s"OVERNIGHT_SOAK pass=true slices=$slices seed=$seed cycles=$cycle activeCycles=$activeCycles demands=${submitted-withdrawn} planned=$submitted withdrawn=$withdrawn completed=${driver.completed} checkedBeats=${driver.checkedBeats} translations=$translations recvInputs=$recvInputs watchdog=false scoreboard=true sourceClosure=true totalConservation=true recvConservation=true")
    for(i <- 0 until 2) {
      assert(stopped(i).nonEmpty && resolved(i).keySet == stopped(i) && residents(i).isEmpty,
        s"starved source ${Seq(3,9)(i)} item ledger did not close")
      println(s"DRAIN_DISPOSITION seed=$seed source=${Seq(3,9)(i)} pendingAtStop=${stopped(i).size} entered=${resolved(i).values.count(_=="entered")} dedup=${resolved(i).values.count(_=="dedup")} overwritten=${resolved(i).values.count(_=="overwrite")} pending=0")
    }
    println(s"DRAIN_ACCEPTANCE seed=$seed drain=true ledger=true itemClosure=true")
    }
  }
}
