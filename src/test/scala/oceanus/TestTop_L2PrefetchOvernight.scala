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

object OvernightMemory {
  var vaMask: BigInt = 0
  // Read-only byte pattern with a canonical, nonzero pointer in word zero.
  // The same expected function is used by the demand scoreboard, including hits.
  def beat(addr: BigInt, beat: Int): BigInt = {
    val data = L2TopReferenceMemory.beat(addr, beat)
    if(beat == 0) (data >> 64 << 64) | (((addr + 0x1000) ^ vaMask) & ((BigInt(1)<<48)-1)) else data
  }
}

class OvernightProbe(slices: Int)(implicit val p: Parameters) extends Bundle {
  val tlbOwner = Bool()
  val tlbRetry = Bool()
  val clientReq = Vec(2, Valid(UInt(50.W)))
  val clientResult = Vec(2, Valid(new L2PrefetchTranslationResult))
  val algorithm = Vec(2, Valid(UInt(48.W)))
  val fastAllocate = Valid(UInt(98.W))
  val fastTranslatedAtTlb = Bool()
  val produced = Vec(slices, UInt(4.W)) // Recv, VBOP, PBOP, CDP; producer boundary
  val entered = Vec(slices, Valid(UInt(4.W))) // actual slice REQ handshake
}
class TestTopOvernight(cfg: L2Configuration)(implicit p: Parameters) extends TestTop_L2Top(cfg) {
  def read[T <: Data](x: T): T = BoringUtils.bore(x)
  val probe = IO(Output(new OvernightProbe(cfg.sliceNum)))
  val adapter=module.pfTlbAdapter.get
  probe.tlbOwner := read(adapter.owner)(read(adapter.issue))
  probe.tlbRetry := read(adapter.retried)(read(adapter.issue))
  for(c <- 0 until 2) {
    val client=adapter.io.clients(c)
    probe.clientReq(c).valid := read(client.req.valid) && read(client.req.ready)
    probe.clientReq(c).bits := read(client.req.bits)
    probe.clientResult(c).valid := read(client.resp.valid) && read(client.resp.ready)
    probe.clientResult(c).bits := read(client.resp.bits)
  }
  val vf=module.vbopStub.get.reqFilter
  probe.fastAllocate.valid := read(vf.accepted) && read(vf.io.in.bits.translated)
  probe.fastAllocate.bits := Cat(read(vf.io.in.bits.va),read(vf.io.in.bits.pa))
  probe.fastTranslatedAtTlb := read(vf.io.tlb.req.valid) && read(vf.entries)(read(vf.selected)).translated
  Seq((read(module.vbopStub.get.io.req.valid) && read(module.vbopStub.get.io.req.ready), read(module.vbopStub.get.io.req.bits.paddr)),
    (read(module.cdp.get.io.req.valid) && read(module.cdp.get.io.req.ready), read(module.cdp.get.io.req.bits.paddr))).zipWithIndex.foreach { case((fire,pa),i)=>
    probe.algorithm(i).valid := read(fire)
    probe.algorithm(i).bits := read(pa)
  }
  def bank(pa: UInt): UInt = cfg.sliceNum match {
    case 1 => 0.U
    case 2 => pa(47,6).xorR
    case 4 => Cat(VecInit((7 until 48 by 2).map(pa(_))).asUInt.xorR, VecInit((6 until 48 by 2).map(pa(_))).asUInt.xorR)
  }
  for(s <- 0 until cfg.sliceNum) {
    val v = module.vbopStub.get.io.req
    val b = module.bop.io.req
    val c = module.cdp.get.io.req
    probe.produced(s) := Cat(
      read(c.valid) && read(c.ready) && bank(read(c.bits.paddr))===s.U,
      read(b.valid) && read(module.bopControlEnable) && bank(read(b.bits))===s.U,
      read(v.valid) && read(v.ready) && bank(read(v.bits.paddr))===s.U,
      read(module.pfRecvReqs(s).valid))
    val req=module.slices(s).io.UpRXREQ
    probe.entered(s).valid := read(req.valid) && read(req.ready) && read(req.bits.SrcID)===cfg.pfSrcID.U
    probe.entered(s).bits := read(req.bits.TxnID)(7,4)
  }
}

class OvernightDemandDriver(dut: TestTop_L2Top, upstream: L2TopUpstreamModel) extends ChiselSim {
  val masters = Seq(MasterId.t1p0, MasterId.t4p0, MasterId.t4p1)
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
  private case class Read(req: UpRequestSpec, beats: mutable.Set[Int] = mutable.Set.empty)
  private val active = mutable.Map.empty[(MasterId, BigInt), Read]
  private val held = mutable.Map.empty[MasterId, UpRequestSpec]
  private case class Ack(key: (MasterId, BigInt), dbid: BigInt, slice: BigInt)
  private val acks = mutable.Queue.empty[Ack]
  val accepted = mutable.ArrayBuffer.empty[(Int, UpRequestSpec)]
  var completed = 0
  var checkedBeats = 0
  var maxOutstanding = 0
  def outstanding: Int = active.size
  def pending: Int = active.size + held.size + queues.values.map(_.size).sum + acks.size
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
      assert(active.remove(ack.key).nonEmpty)
      completed += 1
    }
    masters.foreach { master =>
      val port = reqPort(master)
      if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
        val request = held.remove(master).get
        val key = (master, request.txnId)
        assert(!active.contains(key), s"TxnID reused while active: $key")
        active(key) = Read(request)
        nextID(master) = (request.txnId.toInt + 1) % (1 << port.bits.TxnID.getWidth)
        accepted += ((cycle, request))
        maxOutstanding = maxOutstanding.max(active.size)
        println(s"CONCURRENT_ACCEPT cycle=$cycle master=${master.name} txn=${request.txnId} addr=0x${request.addr.toString(16)}")
      }
      val data = dataPort(master)
      if (data.valid.peek().litToBoolean && data.ready.peek().litToBoolean) {
        val id = data.bits.TxnID.peek().litValue
        val key = (master, id)
        val read = active.getOrElse(key, throw new AssertionError(s"unmatched upstream response: $key"))
        assert(data.bits.Opcode.peek().litValue == CCHIOpcode.CompData.opcode)
        val beat = data.bits.DataID.peek().litValue.toInt
        assert(Set(0, 1).contains(beat) && !read.beats(beat), s"duplicate/invalid beat: $key beat=$beat")
        val actual = data.bits.Data.peek().litValue
        val expected = OvernightMemory.beat(read.req.addr, beat)
        assert(actual == expected, s"data mismatch $key addr=0x${read.req.addr.toString(16)} beat=$beat actual=0x${actual.toString(16)} expected=0x${expected.toString(16)}")
        read.beats += beat
        checkedBeats += 1
        if (read.beats.size == 2) {
          if (master == MasterId.T1P0) {
            acks.enqueue(Ack(key, data.bits.DBID.peek().litValue, data.bits.SrcID.peek().litValue))
          } else {
            active.remove(key)
            completed += 1
          }
        }
      }
    }
  }
}

object TestTop_L2PrefetchOvernight extends App with ChiselSim {
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
  implicit val simulator: HasSimulator=HasSimulator.default
  simulate(new TestTopOvernight(cfg)(params)) { dut =>
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
    while(quiet<40 && cycle<minimum+30000) {
      up.clearResponses(); dn.clearResponses()
      stimulus=cycle<minimum || !sources.forall(src=>entered.map(_(src)).sum>0)
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
      train.trainEnable.poke(((value(dut.io.t1p.head.UpREQ.bits.Addr)>>6)%3!=0).B); train.reqSource.poke((cycle%32).U)
      // Burst Recv, with occasional issue pauses to build source queues.
      dut.holdIssue.get.poke((stimulus && cycle%311<75).B)
      val recv=stimulus && (cycle%257<8 || random.nextInt(97)==0)
      dut.io.pfRecv.get.addr_valid.poke(recv.B)
      dut.io.pfRecv.get.addr.poke((BigInt(0x210000)+random.nextInt(512)*64).U)
      val tlb=dut.io.pfTlb.get
      tlb.req.ready.poke((random.nextInt(5)!=0).B)
      tlb.resp.valid.poke(false.B); tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits))
      tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      replies.headOption.filter(_.due<=cycle).foreach { t =>
        replies.dequeue(); tlb.resp.valid.poke(true.B); tlb.resp.bits.paddr.head.poke(physical(t.va).U)
        tlb.resp.bits.miss.poke(t.miss.B); tlb.resp.bits.excp.head.pf.ld.poke(t.fault.B)
        if(!t.miss || t.attempt==2) {
          terminals((t.client,t.va))=t
          modelDone+=1
          if(t.miss || t.fault) modelDrops+=1
          else allowed(t.client)(if(t.client==1) physical(t.va)>>12 else physical(t.va))+=1
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
        val miss=harsh && random.nextInt(100)<30
        val fault=harsh && random.nextInt(100)<5
        if(miss) injectedMisses+=1
        if(fault) injectedFaults+=1
        replies.enqueue(Translation(c,va,cycle+(if(harsh) 1+random.nextInt(7) else Seq(1,3,7)(random.nextInt(3))),miss,fault,attempt))
        translations+=1
      }
      for(c <- 0 until 2) {
        val a=dut.probe.algorithm(c)
        if(flag(a.valid)) {
          val pa=value(a.bits)
          // C5 D1: a successful CDP page translation permits each waiting
          // line offset, and Filter may suppress a translated entry. Keep
          // page permission separately from VBOP one-line retirement credits.
          val permission=if(c==1) pa>>12 else pa
          assert(allowed(c)(permission)>0,s"algorithm issued without successful translation/fast path client=$c PA=$pa")
          if(c==0) allowed(c)(pa)-=1
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
        if(flag(dut.probe.entered(s).valid)) entered(s)(value(dut.probe.entered(s).bits).toInt)+=1
      }
      driver.sample(cycle)
      if(driver.completed!=lastCompleted) { lastProgress=cycle; lastCompleted=driver.completed }
      assert(cycle-lastProgress<4000,s"progress watchdog seed=$seed slices=$slices cycle=$cycle pending=${driver.pending}")
      val active=dut.observation.dirProbeBySlice.flatten.exists(p=>flag(p.valid)||flag(p.reqActive))
      val pendingQueues=(0 until slices).flatMap(s=>(0 to 9).map(src=>count(dut.observation.prefetchPendingBySource,s,src))).sum
      val idle= !stimulus && driver.pending==0 && data.isEmpty && rsp.isEmpty && live.isEmpty && snoops.isEmpty && replies.isEmpty && pendingQueues==0 && !active
      quiet=if(idle) quiet+1 else 0
      dn.stepWithCredits(cycle); cycle+=1
    }
    check()
    assert(allowed(0).values.forall(_==0),"successful translation/fast allocation did not emit exactly once")
    assert(transactions.isEmpty && terminals.isEmpty && modelDone==actualDone && modelDrops==actualDrops && modelRetries==actualRetries,
      s"TLB ledger done=$modelDone/$actualDone retries=$modelRetries/$actualRetries drops=$modelDrops/$actualDrops live=$transactions terminals=$terminals")
    println(s"OVERNIGHT_TLB pass=true slices=$slices seed=$seed harsh=$harsh requests=$translations injectedMisses=$injectedMisses injectedFaults=$injectedFaults modelRetries=$modelRetries actualRetries=$actualRetries modelDrops=$modelDrops actualDrops=$actualDrops completed=$actualDone fast=$fast fastAllocations=$fastAllocations vbopEmits=$fastEmits")
    if(harsh) assert(injectedMisses>0 && injectedFaults>0 && modelDrops>0 && modelRetries>0,"harsh TLB vacuous")
    if(fast) assert(fastAllocations>0 && fastEmits>0,"D3 vacuous")
    assert(quiet>=40,s"absolute watchdog seed=$seed slices=$slices cycle=$cycle")
    assert(activeCycles>=minimum && sources.forall(src=>entered.map(_(src)).sum>0),"source coverage / minimum cycles")
    assert(driver.completed==submitted && driver.checkedBeats==submitted*2 && driver.pending==0,"scoreboard closure")
    for(src <- sources) {
      val led=(0 until slices).map(s=>ledger(s,src)).transpose.map(_.sum)
      val useful=(0 until slices).map(s=>value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Useful))).sum
      val redundant=(0 until slices).map(s=>value(dut.observation.bopPerfCountBySliceSource(s)(src)(L2BOPPerf.Redundant))).sum
      println(s"OVERNIGHT_SOURCE pass=true slices=$slices seed=$seed source=$src produced=${produced.map(_(src)).sum} enqueued=${led(0)} overwritten=${led(1)} entered=${entered.map(_(src)).sum} queueEntered=${led(2)} dedup=${led(3)} pending=${led(4)} useful=$useful redundant=$redundant")
    }
    println(s"OVERNIGHT_SOAK pass=true slices=$slices seed=$seed cycles=$cycle activeCycles=$activeCycles demands=$submitted completed=${driver.completed} checkedBeats=${driver.checkedBeats} translations=$translations recvInputs=$recvInputs watchdog=false scoreboard=true sourceClosure=true totalConservation=true recvConservation=true")
    }
  }
}
