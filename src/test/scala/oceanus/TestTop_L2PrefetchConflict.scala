// Intent (pf-v2): independent conflict tests; existing tests and all RTL stay frozen.
// Prove each overlap at the actual RBE/training/source/translation boundary.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.compactchi.{CCHIOpcode,FlitREQ}
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters,CCHIParametersKey}
import scala.collection.mutable

// Recorded host stimulus is independent of the responses seen in a source
// ablation. This preserves the exact demand enqueue cycles and addresses.
case class ConflictFrame(cycle:Int,hold:Boolean,train:Boolean,recv:Option[BigInt],snp:Option[BigInt],
  ready:Boolean,dataID:Int,dataPA:BigInt,dataBeat:Int,dnDelay:Int,tlbDelay:Int,
  demands:Seq[(BigInt,Boolean,Boolean)]) {
  def encode:String=Seq(cycle.toString,hold.toString,train.toString,
    recv.map(_.toString(16)).getOrElse("-"),snp.map(_.toString(16)).getOrElse("-"),
    ready.toString,dataID.toString,dataPA.toString(16),dataBeat.toString,dnDelay.toString,tlbDelay.toString,
    demands.map{case(pa,en,stash)=>s"${pa.toString(16)}:$en:$stash"}.mkString(",")).mkString("\t")
}
object ConflictFrame {
  def parse(line:String):ConflictFrame={
    val a=line.split("\t",-1)
    def pa(s:String)=if(s=="-") None else Some(BigInt(s,16))
    ConflictFrame(a(0).toInt,a(1).toBoolean,a(2).toBoolean,pa(a(3)),pa(a(4)),a(5).toBoolean,
      a(6).toInt,BigInt(a(7),16),a(8).toInt,a(9).toInt,a(10).toInt,
      if(a(11).isEmpty) Seq.empty else a(11).split(",").toSeq.map{s=>val b=s.split(":");(BigInt(b(0),16),b(1).toBoolean,b(2).toBoolean)})
  }
}

class ConflictTshrProbe(implicit val p: Parameters) extends Bundle {
  val requestWaits=UInt(6.W)
  val flags=UInt(12.W)
  val pa=UInt(48.W)
  val inputPA=UInt(48.W)
  val inputSource=UInt(4.W)
  val queuedSource=UInt(4.W)
  val inputOpcode=UInt(8.W)
  val train=new L2CDPTrain
  val evict=new L2CDPEvict
  val line=new L2CDPLine
  val source=UInt(4.W)
  val depth=UInt(2.W)
  val dsWrite=Bool()
  val dsSet=UInt(32.W)
  val dsWay=UInt(32.W)
  val dsData=UInt(512.W)
  val bufferData=UInt(512.W)
  val bufferFromDS=Bool()
  val metaFlags=UInt(8.W)
  val localState=UInt(2.W)
  val directoryState=UInt(2.W)
  val requestState=UInt(2.W)
  val snoopState=UInt(2.W)
}
class ConflictStorageProbe extends Bundle {
  val flags=UInt(5.W)
  val owner=UInt(3.W)
  val set=UInt(32.W)
  val way=UInt(32.W)
  val data=UInt(512.W)
  val response=UInt(512.W)
}
class ConflictQueueProbe extends Bundle {
  val flags=UInt(4.W)
  val put=UInt(48.W)
  val head=UInt(48.W)
  val putPacked=UInt(256.W)
  val headPacked=UInt(256.W)
}
class TestTopConflict(cfg: L2Configuration)(implicit p: Parameters) extends TestTopOvernight(cfg) {
  val hnNodeID=cfg.nodeId
  val ct=IO(Output(Vec(cfg.sliceNum,Vec(nMSHR,new ConflictTshrProbe))))
  val cq=IO(Output(Vec(cfg.sliceNum*10,new ConflictQueueProbe)))
  val enterPA=IO(Output(Vec(cfg.sliceNum,UInt(48.W))))
  val storage=IO(Output(Vec(cfg.sliceNum,new ConflictStorageProbe)))
  for(s<-0 until cfg.sliceNum) {
    val ds=module.slices(s).dataStorage
    val o=storage(s)
    o.flags:=Cat(read(ds.datStop_r),read(ds.grantAheadRd),read(ds.grantRd),read(ds.grantWb),read(ds.anyGrant))
    o.owner:=read(ds.winIdx);o.set:=read(ds.issueReq.set);o.way:=read(ds.issueReq.way)
    o.data:=read(ds.issueReq.wdata);o.response:=read(ds.sram.io.rdata.data)
  }
  for(s<-0 until cfg.sliceNum) enterPA(s):=read(module.slices(s).io.UpRXREQ.bits.Addr)
  val tf=IO(Output(UInt((cfg.sliceNum*nMSHR*12).W)))
  val qf=IO(Output(UInt((cfg.sliceNum*10*4).W)))
  // Final CDP replaces the legacy single-request stub. Drain every accepted
  // training/detection/candidate stage, including Filter re-query pipelines.
  val cdp=module.cdp.get
  val frontend=cdp.detectFrontend
  val send=cdp.sentUnit.get
  val cdpBusy=read(cdp.vpnTrainReqBuf.io.deq.valid) || read(cdp.vtTrainPipe.stage_valid).asUInt.orR ||
    read(frontend.v1) || read(frontend.v2) ||
    frontend.lanes.map(l=>read(l.s1_req.valid)||read(l.s2_req.valid)||read(l.s3_req.valid)).reduce(_||_) ||
    frontend.degreeQueues.map(q=>read(q.io.deq.valid)).reduce(_||_) ||
    read(send.valids).asUInt.orR || read(send.tlbHeld) || read(send.pftS1) || read(send.pftS2) ||
    cdp.filterTraining.map(f=>read(f.queue.io.deq.valid)||read(f.pipe.s1_valid)||
      read(f.pipe.s2_valid)||read(f.pipe.s3_valid)).getOrElse(false.B)
  val algorithmBusy=IO(Output(Bool()))
  algorithmBusy:=read(module.vbopStub.get.input.io.deq.valid)||read(module.vbopStub.get.reqFilter.valids).asUInt.orR||
    read(module.vbopStub.get.reqFilter.allocValid)||read(module.vbopStub.get.delayQueue.valids).asUInt.orR||
    cdpBusy||
    module.slices.map(s=>read(s.io.cdpData.valid)).reduce(_||_)
  val moduleEvictions=IO(Output(UInt(64.W)))
  moduleEvictions:=read(module.cdpEvents.get.evictions)
  val clientValid=IO(Output(UInt(2.W)))
  val slots=IO(Output(UInt(24.W)))
  val slotAges=IO(Output(UInt(32.W)))
  val tlbService=IO(Output(Bool()))
  for((s,si)<-module.slices.zipWithIndex;(t,ti)<-s.tshrs.zipWithIndex) {
    val o=ct(si)(ti)
    val tr=t.io.cdpTrain; val r=t.rbeREQ.io
    val en=t.vPipeREQ.io.tshr_meta_write_en.pfSource
    val flags=Seq(read(r.in.valid)&&read(r.in.ready),read(r.valid),read(r.out.valid),read(tr.valid),read(tr.bits.hit),read(tr.bits.trainEnable),
      read(t.rbeSNP.io.in.valid)&&read(t.rbeSNP.io.in.ready),read(t.rbeSNP.io.valid),read(t.rbeSNP.io.out.valid),
      read(t.io.cdpData.valid),read(t.io.cdpEvict.valid),read(en))
    o.flags:=Cat(flags.reverse)
    o.requestWaits:=Cat(read(t.vPipeREQ.w_evict_peer_commit_dir),read(t.vPipeREQ.w_evict_dn_compdbid),
      read(t.vPipeREQ.w_unlock_ds),read(t.vPipeREQ.w_unlock_dir),read(t.vPipeREQ.w_dn_pcrdgrant),read(t.vPipeREQ.s_dn_txreq))
    o.pa:=read(t.vPipeREQ.io.tshr_paddr)
    o.inputPA:=read(module.slices(si).io.UpRXREQ.bits.Addr)
    o.inputSource:=read(r.in.bits.req.SrcID)
    o.inputOpcode:=read(r.in.bits.req.Opcode)
    o.queuedSource:=read(r.out.bits.req.SrcID)
    o.train:=read(tr.bits);o.evict:=read(t.io.cdpEvict.bits);o.line:=read(t.io.cdpData.bits)
    o.source:=read(t.vPipeREQ.io.tshr_meta_write_meta.pfSource)
    o.depth:=read(t.vPipeREQ.io.tshr_meta_write_meta.cdpPfDepth)
    o.dsWrite:=read(t.io.toDS.DSBufWb)
    o.dsSet:=read(t.io.toDS.SET);o.dsWay:=read(t.io.toDS.WAY);o.dsData:=read(t.io.toDS.DATA)
    o.bufferData:=Cat(read(t.tshr_buffer_2),read(t.tshr_buffer_0))
    o.bufferFromDS:=read(t.tshr_buffer_wen_DS)
    o.metaFlags:=Cat(read(t.proxyDir.io.wb_accept),read(t.meta_modified.state),read(t.tag_modified),
      read(t.meta_write_SNP_mask.state),read(t.meta_write_REQ_mask.state),read(t.tshr_buffer_drop),
      read(t.proxyDS.io.wb_accept),read(t.tshr_buffer_modified))
    o.localState:=read(t.meta.state);o.directoryState:=read(t.proxyDir.io.toDir.META.state)
    o.requestState:=read(t.meta_write_REQ_meta.state);o.snoopState:=read(t.meta_write_SNP_meta.state)
  }
  val ids=module.getClass.getMethods.find(_.getName=="getIds").get.invoke(module)
    .asInstanceOf[scala.collection.Seq[AnyRef]]
  val queues=ids.collect {case q:OverwriteQueue[_]=>q.asInstanceOf[OverwriteQueue[FlitREQ]]}
  require(queues.size==cfg.sliceNum*10)
  for((q,i)<-queues.zipWithIndex) {
    val o=cq(i)
    o.flags:=Cat(read(q.full),read(q.io.overwrite),(read(q.io.deq.valid)&&read(q.io.deq.ready)),read(q.io.enq.valid))
    o.put:=read(q.io.enq.bits.Addr);o.head:=read(q.io.deq.bits.Addr)
    o.putPacked:=read(q.io.enq.bits).asUInt;o.headPacked:=read(q.io.deq.bits).asUInt
  }
  tf:=ct.flatten.map(_.flags).reverse.reduce(Cat(_,_))
  qf:=cq.map(_.flags).reverse.reduce(Cat(_,_))
  clientValid:=Cat(adapter.io.clients.reverse.map(c=>read(c.req.valid)))
  slots:=read(adapter.state).asUInt
  slotAges:=read(adapter.serviceAge).asUInt
  tlbService:=read(adapter.io.tlb.req.ready)&&read(adapter.inflight.io.enq.ready)
}

class ConflictDemandDriver(dut: TestTop_L2Top, upstream: L2TopUpstreamModel) extends ChiselSim {
  val masters = Seq(MasterId.t1p0, MasterId.t4p0, MasterId.t4p1)
  private def reqPort(master: MasterId) = master match {
    case MasterId.T1P0 => dut.io.t1p(0).UpREQ
    case MasterId.T1P1 => dut.io.t1p(1).UpREQ
    case MasterId.T4P0 => dut.io.t4p(0).UpREQ
    case MasterId.T4P1 => dut.io.t4p(1).UpREQ
  }
  private def dataPort(master: MasterId) = master match {
    case MasterId.T1P0 => dut.io.t1p(0).DnDAT
    case MasterId.T1P1 => dut.io.t1p(1).DnDAT
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
  var stashes = 0
  var checkedBeats = 0
  var maxOutstanding = 0
  var trace: String => Unit = _ => ()
  var validateData:(UpRequestSpec,Int,Int)=>Unit=(_:UpRequestSpec,_:Int,_:Int)=>()
  var dumpTrace: () => Unit = () => ()
  var demandOfferCycles = 0L
  var demandBlockedCycles = 0L
  def outstanding: Int = active.size
  def pending: Int = active.size + held.size + queues.values.map(_.size).sum + acks.size
  def queued: Int = held.size + queues.values.map(_.size).sum
  def pendingSummary:String=s"active=${active.toSeq.map{case(k,r)=>(k,r.req.addr.toString(16),r.beats.toSeq)}} held=$held queues=${queues.toSeq.map{case(k,q)=>(k,q.size)}} acks=$acks"
  def enqueue(requests: Seq[UpRequestSpec]): Unit = requests.foreach(r => queues(r.master).enqueue(r))
  def drive(cycle: Int): Unit = {
    upstream.clearUpReq()
    masters.foreach { master =>
      if (!held.contains(master) && queues(master).nonEmpty) {
        val count = 1 << reqPort(master).bits.TxnID.getWidth
        (0 until count).map(i => (nextID(master) + i) % count)
          .find(id => !active.contains((master, BigInt(id)))).foreach { id =>
            val src = master match {
              case MasterId.T1P0 => dut.l2cfg.upstream.type1.head.nid
              case MasterId.T1P1 => dut.l2cfg.upstream.type1(1).nid
              case MasterId.T4P0 => 4
              case MasterId.T4P1 => 5
            }
            held(master) = queues(master).dequeue().copy(txnId = id, srcId = src,
              expCompData = master == MasterId.T1P0)
          }
      }
      held.get(master).foreach { r =>
        val stash=Set(CCHIOpcode.StashShared.opcode,CCHIOpcode.StashUnique.opcode)(r.opcode.toInt)
        upstream.driveRead(if(stash) r.copy(expCompData=false) else r)
      }
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
      if(port.valid.peek().litToBoolean && held.get(master).exists(r =>
          !Set(CCHIOpcode.StashShared.opcode,CCHIOpcode.StashUnique.opcode)(r.opcode.toInt))) {
        demandOfferCycles+=1
        if(!port.ready.peek().litToBoolean) demandBlockedCycles+=1
      }
      if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
        val request = held.remove(master).get
        val key = (master, request.txnId)
        assert(!active.contains(key), s"TxnID reused while active: $key")
        if(Set(CCHIOpcode.StashShared.opcode,CCHIOpcode.StashUnique.opcode)(request.opcode.toInt)) stashes+=1
        else active(key) = Read(request)
        nextID(master) = (request.txnId.toInt + 1) % (1 << port.bits.TxnID.getWidth)
        accepted += ((cycle, request))
        trace(s"UP_REQ cycle=$cycle master=$master txn=${request.txnId} pa=0x${request.addr.toString(16)} opcode=${request.opcode}")
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
        val expected = OvernightMemory.beat(read.req.addr, beat)
        trace(s"UP_DAT cycle=$cycle master=$master txn=$id dbid=${data.bits.DBID.peek().litValue} slice=${data.bits.SrcID.peek().litValue} pa=0x${read.req.addr.toString(16)} beat=$beat actual=0x${actual.toString(16)} expected=0x${expected.toString(16)}")
        if(actual!=expected) dumpTrace()
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


// HN ordering at the external CHI interface (IHI0050E.b 4.11.2 / 2.8.3).
// Keep completion state through CompAck, even after the data queue is empty.
// IllegalStateException deliberately bypasses the random seed AssertionError
// recovery: an illegal stimulus must stop the entire simulation immediately.
// ConflictHNOrdering is shared with the stress harness in ConflictHNOrdering.scala.


// Host models use literal transaction histories/FIFOs, not DUT accounting as
// their expected values. All counters below are sampled before the same edge.
class ConflictRun(val dut: TestTopConflict,val seed: Int,val slices: Int,val randomMode: Boolean) extends ChiselSim {
  def flag(x:Bool)=x.peek().litToBoolean
  def value(x:UInt)=x.peek().litValue
  val random=new scala.util.Random(0x20261022L+seed*100+slices)
  val dummy=UpRequestSpec(MasterId.t1p0,0,0,0,CCHIOpcode.ReadShared.opcode,BigInt(0),expCompData=true)
  val spec=CaseSpec("pf-v2-conflict",UpConfig(Seq(MasterId.t1p0),ExplicitRequestStream(Seq(dummy),Seq(BigInt(CHIFieldResp.CompData_SC))),SnoopPolicy.SnpRespI),
    DnConfig(FixedLatency(25),FixedOutstanding(64),ImmediateCreditPolicy()),200000,(0,200000),Seq.empty)
  val up=new L2TopUpstreamModel(dut,spec)
  val dn=new L2TopDownstreamModel(dut,spec)
  val driver=new ConflictDemandDriver(dut,up)
  var extension:Option[L2HNExtensionPins]=None
  def enableHNExtension(config:HNExtConfig):Unit={
    val model=new L2HNExtensionModel(config,OvernightMemory.beat _,record _)
    driver.validateData=(q,b,c)=>model.upstreamData(q.addr,b,c)
    extension=Some(new L2HNExtensionPins(dut,dn,model,hnRequester,
      (req,c)=>{
        val id=DnTxnID(req.txn)
        if(req.op!=5) {
          live(id)=req.addr
          if(dut.observation.dirProbeBySlice.flatten.exists(p=>flag(p.reqActive)&&flag(p.pRxReqIsPrefetch)&&value(p.tshrPaddr)==req.addr))livePrefetch+=id
          if(!L2HNExtensionModel.isWrite(req.op))readRequests+=((c,req.addr))
        }
      },(d,c)=>{
        val id=DnTxnID(d.txn);receivedData(id)|=(1<<(d.beat/2))
        if(receivedData(id)==3){live.remove(id);livePrefetch-=id;receivedData-=id}
      }))
  }
  var sourceMode="all"
  var cdpEnable=true
  var fullHistory=false
  var hnFlitsOnly=false
  var tapeWriter:Option[java.io.PrintWriter]=None
  var replayFrames=Map.empty[Int,ConflictFrame]
  val tapeDemands=mutable.ArrayBuffer.empty[(BigInt,Boolean,Boolean)]
  val eventHistory=mutable.Queue.empty[String]
  def record(s:String):Unit={eventHistory.enqueue(s);if(eventHistory.size>512) eventHistory.dequeue();if(fullHistory||(hnFlitsOnly&&s.startsWith("HN_EXT"))) println("CONFLICT_EVENT "+s)}
  driver.trace=record _
  driver.dumpTrace=()=>{println(s"CONFLICT_FAILURE seed=$seed slices=$slices cycle=$cycle");eventHistory.foreach(s=>println("CONFLICT_HISTORY "+s));println(s"CONFLICT_FAILURE_STATE live=$live data=$data evicts=${evicts.takeRight(16)}")}
  val data=mutable.Queue.empty[PendingDnComp]
  val rsp=mutable.Queue.empty[PendingDnRsp]
  val live=mutable.Map.empty[DnTxnID,BigInt]
  val livePrefetch=mutable.Set.empty[DnTxnID]
  def inFlightPrefetch(pa:BigInt)=live.exists{case(id,addr)=>addr==pa&&livePrefetch(id)}
  val snps=mutable.Queue.empty[(SnpTxnID,BigInt)]
  val sentSnps=mutable.Map.empty[BigInt,BigInt]
  val hnOrdering=new ConflictHNOrdering
  // This CHI link addresses one RN, shared by all slices.
  val hnRequester=BigInt(dut.hnNodeID)
  val snoopOffers=mutable.Queue.empty[BigInt]
  var dataOrderStalls=0L;var snoopOrderStalls=0L;var responseOrderStalls=0L
  val lateDemands=mutable.Set.empty[BigInt]
  val receivedData=mutable.Map.empty[DnTxnID,Int].withDefaultValue(0)
  val paused=mutable.Set.empty[BigInt]
  val readRequests=mutable.ArrayBuffer.empty[(Int,BigInt)]
  val entries=mutable.ArrayBuffer.empty[(Int,BigInt,Int)]
  val writes=mutable.Map.empty[BigInt,(Int,Int)]
  val trains=mutable.ArrayBuffer.empty[(Int,BigInt,Int,Int,Boolean)]
  val replacements=mutable.Map.empty[(Int,BigInt),(BigInt,Int)]
  val evicts=mutable.ArrayBuffer.empty[(Int,BigInt,Int,Int)]
  val evidence=mutable.Map.empty[String,Long].withDefaultValue(0L)
  val firstEvidence=mutable.Map.empty[String,String]
  val episodes=mutable.Map.empty[String,Long].withDefaultValue(0L)
  val lastHit=mutable.Map.empty[String,Int].withDefaultValue(-2)
  val sources=Seq(1,2,3,9)
  val produced=Array.fill(slices,10)(BigInt(0))
  val entered=Array.fill(slices,10)(BigInt(0))
  val qModels=Seq.fill(slices*10)(mutable.Queue.empty[(BigInt,BigInt)])
  val maxWait=Array.fill(slices,10)(0)
  val waitAge=Array.fill(slices,10)(0)
  val pairNames=mutable.Map.empty[String,Long].withDefaultValue(0L)
  case class Translation(owner:Int,va:BigInt,due:Int,attempt:Int,miss:Boolean,fault:Boolean)
  val translations=mutable.Map.empty[(Int,BigInt),Int]
  val terminals=mutable.Map.empty[(Int,BigInt),Translation]
  val replies=mutable.Queue.empty[Translation]
  // VBOP emits one line per translation. Final CDP shares one page result
  // among candidates and retires by Filter/drop/output, checked by its ledger.
  val permits=Seq.fill(1)(mutable.Map.empty[BigInt,Int].withDefaultValue(0))
  val cdpLedger=new CDPCandidateLedger
  var modelRetry=0;var modelDrop=0;var retired=0;var actualRetry=0;var actualDrop=0
  var tlbRequests=0;var recvInputs=0;var submitted=0;var cycle=0;var snoopID=1;var snoopDone=0
  var latency=35;var hold=false;var trainEnable=true;var tlbReady=true;var harsh=false
  var recvNext:Option[BigInt]=None;var snpNext:Option[BigInt]=None
  var hitSnoop:Option[BigInt]=None
  var lastProgress=0;var lastCompleted=0
  var previousTlb:Option[BigInt]=None
  var lastFlags=BigInt(0)
  def hit(name:String,detail:String):Unit={
    evidence(name)+=1
    if(lastHit(name)<cycle-1) episodes(name)+=1
    lastHit(name)=cycle
    if(!firstEvidence.contains(name)) {firstEvidence(name)=s"cycle=$cycle $detail";println(s"CONFLICT_EVIDENCE seed=$seed slices=$slices name=$name cycle=$cycle $detail")}
  }
  def queueIndex(s:Int,source:Int):Int=if(source==1) 9*slices+s else s*9+Seq(1,2,3,4,5,6,7,8,9).indexOf(source)
  def controls(b:Boolean=true,v:Boolean=true,c:Boolean=true):Unit={
    val ctrl=dut.io.pfCtrl.get
    ctrl.poke(0.U.asTypeOf(ctrl));ctrl.l2_pf_master_en.poke((sourceMode!="none").B)
    ctrl.l2_pf_recv_en.poke((sourceMode=="all"||sourceMode=="recv").B)
    ctrl.l2_pbop_en.poke((b&&(sourceMode=="all"||sourceMode=="bop")).B)
    ctrl.l2_vbop_en.poke((v&&(sourceMode=="all"||sourceMode=="vbop")).B)
    ctrl.l2_cdp_en.poke((c&&cdpEnable&&(sourceMode=="all"||sourceMode=="cdp")).B)
  }
  def enqueue(pa:BigInt,enabled:Boolean=true,stash:Boolean=false):Unit={
    trainEnable=enabled
    tapeDemands+=((pa,enabled,stash))
    driver.enqueue(Seq(dummy.copy(addr=pa,opcode=if(stash) CCHIOpcode.StashShared.opcode else CCHIOpcode.ReadShared.opcode)))
    if(!stash) submitted+=1
  }
  def recv(pa:BigInt):Unit={recvNext=Some(pa);tick()}
  def sendSnoop(pa:BigInt):Unit={snpNext=Some(pa);while(snpNext.nonEmpty||snoopOffers.nonEmpty) tick()}
  def bit(f:Int,n:Int)=(f&(1<<n))!=0
  def ledgerCheck():Unit={
    val o=dut.observation
    for(s<-0 until slices;src<-0 to 9) {
      val enq=value(o.prefetchEnqueueBySource(s)(src));val over=value(o.prefetchQueueOverwriteBySource(s)(src))
      val issue=value(o.prefetchEnterBySource(s)(src));val dedup=value(o.prefetchDedupBySource(s)(src));val pend=value(o.prefetchPendingBySource(s)(src))
      assert(enq==over+issue+dedup+pend,s"queue closure cycle=$cycle slice=$s source=$src")
      assert(produced(s)(src)==entered(s)(src)+over+dedup+pend,s"producer closure cycle=$cycle slice=$s source=$src produced=${produced(s)(src)} entered=${entered(s)(src)} overwritten=$over dedup=$dedup pending=$pend")
      assert(value(o.prefetchSliceEnteredBySource(s)(src))==entered(s)(src))
      if(src==1) assert(value(o.recvAcceptedBySource(s)(src))==produced(s)(src))
    }
    assert(produced.map(_(1)).sum==recvInputs,"Recv direct/fallback conservation")
    for(s<-0 until slices;m<-L2BOPPerf.names.indices)
      assert(value(o.bopPerfCountBySlice(s)(m))==(0 to 9).map(src=>value(o.bopPerfCountBySliceSource(s)(src)(m))).sum,"effect sum")
  }
  def pendingTshrs:Boolean={
    dut.observation.dirProbeBySlice.flatten.exists(p=>flag(p.valid)||flag(p.reqActive))
  }
  def tick():Unit={
    assert(cycle<200000,s"absolute watchdog seed=$seed pending=${driver.pending} live=$live q=${qModels.map(_.size)}")
    val frame=replayFrames.get(cycle)
    frame.foreach{f=>f.demands.foreach{case(pa,en,stash)=>enqueue(pa,en,stash)};hold=f.hold;trainEnable=f.train;recvNext=f.recv;snpNext=f.snp}
    val rawRecv=recvNext;val rawSnp=snpNext
    if(sourceMode!="all"&&sourceMode!="recv") recvNext=None
    val frameHold=hold;val frameTrain=trainEnable
    var chosenData:Option[PendingDnComp]=None;var chosenResponsePA:Option[BigInt]=None
    var chosenDelay=0;var chosenTlbDelay=1
    up.clearResponses();dn.clearResponses();driver.drive(cycle)
    dut.holdIssue.get.poke(hold.B)
    val train=dut.io.pfTrain.get.head;val pa=value(dut.io.t1p.head.UpREQ.bits.Addr)
    train.vaddrBlock.poke((pa>>6).U);train.trainEnable.poke((trainEnable && (!randomMode || (pa>>6)%3!=0)).B);train.reqSource.poke((cycle%32).U)
    val recv=dut.io.pfRecv.get;recv.addr_valid.poke(recvNext.nonEmpty.B)
    recv.addr.poke(recvNext.getOrElse(BigInt(0)).U)
    val tlb=dut.io.pfTlb.get
    val readyNow=frame.map(_.ready).getOrElse(tlbReady && (!randomMode || random.nextInt(5)!=0))
    tlb.req.ready.poke(readyNow.B)
    tlb.resp.valid.poke(false.B);tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits));tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
    replies.headOption.filter(_.due<=cycle).foreach{t=>
      replies.dequeue();tlb.resp.valid.poke(true.B);tlb.resp.bits.paddr.head.poke(t.va.U)
      tlb.resp.bits.miss.poke(t.miss.B);tlb.resp.bits.excp.head.pf.ld.poke(t.fault.B)
      if(!t.miss || t.attempt==2) {terminals((t.owner,t.va))=t;if(t.miss||t.fault) modelDrop+=1 else if(t.owner==0) permits(0)(t.va)+=1}
      else modelRetry+=1
    }
    // Complete inbound channel stimulus before observing combinational
    // metadata/eviction/line events. Sampling before RXRSP missed same-cycle
    // victim invalidations even though the production counter saw the edge.
    if(extension.nonEmpty)extension.get.drive(cycle,paused.contains _)
    else {
    def dataLegal(d:PendingDnComp)=hnOrdering.canData(d.srcId,d.addr)
    if(data.exists(d=>d.due<=cycle&& !dataLegal(d))) dataOrderStalls+=1
    val selected=frame match {
      case Some(f) if f.dataID<0 => None
      case Some(f) =>
        def eligible(d:PendingDnComp)=d.due<=cycle && !paused(d.addr) && dn.canDriveRxDat && dataLegal(d)
        data.dequeueFirst(d=>eligible(d)&&d.dnTxnID.v==f.dataID&&d.addr==f.dataPA&&d.beat==f.dataBeat)
          .orElse(data.dequeueFirst(eligible))
      case None =>data.dequeueFirst(d=>d.due<=cycle && !paused(d.addr) && dn.canDriveRxDat && dataLegal(d) && (!randomMode||random.nextInt(5)!=0))
    }
    selected.foreach{d=>
      chosenData=Some(d)
      record(s"DN_DAT cycle=$cycle txn=${d.dnTxnID.v} pa=0x${d.addr.toString(16)} beat=${d.beat}");assert(live.get(d.dnTxnID).contains(d.addr),"RXDAT address/identity mismatch");assert((receivedData(d.dnTxnID)&(1<<d.beat))==0,"duplicate RXDAT beat");receivedData(d.dnTxnID)|=(1<<d.beat);dn.driveDnData(d);dut.io.chi.rxdat.flit.Data.get.poke(OvernightMemory.beat(d.addr,d.beat).U)
      val finalBeat=receivedData(d.dnTxnID)==3;dn.noteResponse(d.dnTxnID,finalBeat,cycle);if(finalBeat) {assert(live.remove(d.dnTxnID).nonEmpty);livePrefetch-=d.dnTxnID;receivedData-=d.dnTxnID}
    }
    // 4.11.2 also forbids CopyBack completion while SnpResp is pending.
    // Waiting only on CompData leaves the replacement/SNP hazard illegal.
    if(rsp.exists(r=>r.due<=cycle&& !hnOrdering.canData(r.srcId,live(r.dnTxnID)))) responseOrderStalls+=1
    rsp.dequeueFirst(r=>r.due<=cycle&&dn.canDriveRxRsp&&hnOrdering.canData(r.srcId,live(r.dnTxnID))).foreach{r=>
      chosenResponsePA=Some(live(r.dnTxnID))
      record(s"DN_RSP cycle=$cycle rn=${r.srcId} txn=${r.dnTxnID.v} pa=0x${live(r.dnTxnID).toString(16)} opcode=${r.opcode}")
      dn.driveDnRsp(r);dn.noteResponse(r.dnTxnID,true,cycle);assert(live.remove(r.dnTxnID).nonEmpty);livePrefetch-=r.dnTxnID
    }
    }
    if(!flag(dut.io.t1p.head.UpRSP.valid)) snps.headOption.foreach{case(id,src)=>up.driveUpSnpResp(id,src);if(flag(dut.io.t1p.head.UpRSP.ready)) snps.dequeue()}
    if(flag(dut.io.t1p.head.DnSNP.valid)) snps.enqueue((SnpTxnID(value(dut.io.t1p.head.DnSNP.bits.TxnID)),value(dut.io.t1p.head.DnSNP.bits.SrcID)))
    // Read event flags once per cycle; fields are sampled only for real events.
    for(s<-0 until slices;x<-dut.observation.dirProbeBySlice(s)) {
      if(flag(x.replRdResp)&&flag(x.pRxReqIsPrefetch)&&value(x.fromDirMetaState)!=0) {
        val victim=value(x.fromDirReplPaddr);val refiller=value(x.tshrPaddr)
        replacements((s,victim))=(refiller,cycle)
        if(!randomMode) println(s"CONFLICT_REPL_SELECTED cycle=$cycle slice=$s victim=0x${victim.toString(16)} refiller=0x${refiller.toString(16)}")
      }
    }
    val flags=value(dut.tf);lastFlags=flags
    var trainSlices=Set.empty[Int];var disabled=false
    val hitThisCycle=mutable.Map.empty[BigInt,Int]
    for(s<-0 until slices;t<-0 until dut.nMSHR) {
      val f=((flags>>((s*dut.nMSHR+t)*12))&4095).toInt
      val p=dut.ct(s)(t)
      if(bit(f,0)) {
        val inPA=value(p.inputPA);val src=value(p.inputSource)
        if(src!=dut.l2cfg.pfSrcID && value(p.inputOpcode)!=CCHIOpcode.StashShared.opcode && inFlightPrefetch(inPA)) {lateDemands+=inPA;hit("C01_enter",s"slice=$s tshr=$t pa=0x${inPA.toString(16)} liveDownstream=true src=$src")}
        if(src!=dut.l2cfg.pfSrcID && value(p.inputOpcode)==CCHIOpcode.StashShared.opcode && inFlightPrefetch(inPA)) hit("C05",s"slice=$s tshr=$t pa=0x${inPA.toString(16)} externalStash=true")
      }
      if(bit(f,1) && !bit(f,2) && value(p.queuedSource)!=dut.l2cfg.pfSrcID && inFlightPrefetch(value(p.pa))) hit("C01_wait",s"slice=$s tshr=$t pa=0x${value(p.pa).toString(16)} rbeValid=true rbeOut=false")
      if(bit(f,3)) {
        val tr=p.train;val addr=value(tr.paddr);val src=value(tr.pfSource).toInt;val depth=value(tr.pfDepth).toInt;val ishit=flag(tr.hit)
        trains+=((cycle,addr,src,depth,ishit))
        record(s"TRAIN cycle=$cycle slice=$s tshr=$t pa=0x${addr.toString(16)} hit=$ishit source=$src depth=$depth")
        if(flag(tr.trainEnable)) trainSlices+=s else disabled=true
        if(ishit && src!=0) {
          hitThisCycle(addr)=src
          if(lateDemands.remove(addr)) hit("C01",s"pa=0x${addr.toString(16)} oldSource=$src priorInFlightDemand=true hit=true")
          hit("C01_hit",s"slice=$s tshr=$t pa=0x${addr.toString(16)} source=$src depth=$depth")
          if(hitSnoop.contains(addr) && dn.canDriveRxSnp) {snpNext=Some(addr);hitSnoop=None}
        }
      }
      if(bit(f,6)) hit("SNP_RBE_ENTER",s"slice=$s tshr=$t pa=0x${value(p.pa).toString(16)}")
      if(bit(f,8)) hit("SNP_RBE_EXEC",s"slice=$s tshr=$t pa=0x${value(p.pa).toString(16)}")
      if(bit(f,10)) {
        val e=p.evict;val addr=value(e.paddr);val src=value(e.pfSource).toInt;val d=value(e.pfDepth).toInt
        evicts+=((cycle,addr,src,d));record(s"EVICT cycle=$cycle slice=$s tshr=$t pa=0x${addr.toString(16)} source=$src depth=$d")
        writes.get(addr).foreach{case(a,b)=>assert(a==src && b==d,s"victim history pa=$addr expected=$a/$b actual=$src/$d")}
        if(!randomMode) println(s"CONFLICT_EVICT cycle=$cycle victim=0x${addr.toString(16)} source=$src depth=$d peers=${dut.observation.dirProbeBySlice(s).map(x=>s"${flag(x.reqActive)}:${flag(x.pRxReqIsPrefetch)}:${value(x.tshrPaddr).toString(16)}").mkString(",")}")
        replacements.remove((s,addr)).foreach{case(refiller,selectedAt)=>
          assert(refiller!=addr&&((refiller>>6)&(dut.p(L2ParamsKey).sets-1))==((addr>>6)&(dut.p(L2ParamsKey).sets-1)),"replacement set/tag mismatch")
          hit("C04",s"slice=$s victim=0x${addr.toString(16)} refiller=0x${refiller.toString(16)} source=$src depth=$d replSelectedAt=$selectedAt actualInvalidation=true")
        }
        writes.remove(addr)
      }
      if(bit(f,11)) {writes(value(p.pa))=(value(p.source).toInt,value(p.depth).toInt);record(s"META cycle=$cycle slice=$s tshr=$t pa=0x${value(p.pa).toString(16)} source=${value(p.source)} depth=${value(p.depth)}")}
      if(fullHistory) {
        val d=dut.observation.dirProbeBySlice(s)(t)
        if(flag(d.dirRdResp)) record(s"DIR_READ cycle=$cycle slice=$s tshr=$t pa=0x${value(d.tshrPaddr).toString(16)} hit=${flag(d.fromDirMetaHit)} way=${value(d.fromDirMetaWay)} state=${value(d.fromDirMetaState)} source=${value(d.fromDirMetaPfSource)}")
        if(flag(d.dirWbLand)||flag(d.tagWrite)) record(s"DIR_WRITE cycle=$cycle slice=$s tshr=$t pa=0x${value(d.tshrPaddr).toString(16)} tagWrite=${flag(d.tagWrite)} way=${value(d.dirResultWay)} state=${value(d.metaWrReqMetaState)}")
        if(flag(d.dsRdResp)) record(s"DS_READ cycle=$cycle slice=$s tshr=$t pa=0x${value(d.tshrPaddr).toString(16)} way=${value(d.dirResultWay)} data=0x${value(d.dsRdData).toString(16)}")
        if(flag(p.dsWrite)) record(s"DS_WRITE cycle=$cycle slice=$s tshr=$t pa=0x${value(p.pa).toString(16)} set=${value(p.dsSet)} way=${value(p.dsWay)} data=0x${value(p.dsData).toString(16)}")
        if(flag(d.dsAheadRdResp)||flag(d.dsRdResp)) record(s"DS_RESPONSE cycle=$cycle slice=$s tshr=$t pa=0x${value(p.pa).toString(16)} ahead=${flag(d.dsAheadRdResp)} regular=${flag(d.dsRdResp)} capture=${flag(p.bufferFromDS)} data=0x${value(d.dsRdData).toString(16)} buffer=0x${value(p.bufferData).toString(16)} done=${flag(d.dsRdDone)}")
        val mf=value(p.metaFlags).toInt
        if((mf&158)!=0) record(s"META_FLOW cycle=$cycle slice=$s tshr=$t pa=0x${value(p.pa).toString(16)} directoryGrant=${bit(mf,7)} stateMask=${bit(mf,6)} tagMask=${bit(mf,5)} localState=${value(p.localState)} directoryState=${value(p.directoryState)} reqWrite=${bit(mf,3)} reqState=${value(p.requestState)} snpWrite=${bit(mf,4)} snpState=${value(p.snoopState)} bufferDrop=${bit(mf,2)} dsGrant=${bit(mf,1)} bufferModified=${bit(mf,0)}")
      }
    }
    if(fullHistory) for(s<-0 until slices) {
      val p=dut.storage(s);val f=value(p.flags).toInt
      if(bit(f,0)) record(s"DS_GRANT cycle=$cycle slice=$s tshr=${value(p.owner)} set=${value(p.set)} way=${value(p.way)} write=${bit(f,1)} read=${bit(f,2)} ahead=${bit(f,3)} data=0x${value(p.data).toString(16)}")
    }
    if(trainSlices.size>=2) hit("C06",s"trainSlices=${trainSlices.toSeq.sorted.mkString(",")}")
    if(trainSlices.nonEmpty && disabled) hit("C06_mixed",s"enabledSlices=${trainSlices.toSeq.sorted.mkString(",")} disabledRaw=true")
    // Exact-credit snoop injection, after observing demand execution this cycle.
    if(extension.nonEmpty) {
      snpNext.foreach(extension.get.offer(_));snpNext=None
    } else {
    snpNext.foreach(snoopOffers.enqueue(_));snpNext=None
    def snoopLegal(addr:BigInt)=hnOrdering.canSnoop(hnRequester,addr)&&
      !chosenData.exists(d=>(d.addr>>6)==(addr>>6)&&d.srcId==hnRequester)&&
      !chosenResponsePA.exists(pa=>(pa>>6)==(addr>>6))
    if(snoopOffers.headOption.exists(a=> !snoopLegal(a))) snoopOrderStalls+=1
    snoopOffers.headOption.filter(addr=>dn.canDriveRxSnp&&snoopLegal(addr)).foreach{addr=>
      val io=dut.io.chi.rxsnp
      io.flitv.poke(true.B);io.flitpend.poke(true.B);io.flit.poke(0.U.asTypeOf(io.flit))
      io.flit.Opcode.get.poke(7.U);io.flit.DoNotGoToSD_DoNotDataPull.get.poke(1.U)
      io.flit.Addr.get.poke((addr>>3).U);io.flit.TxnID.get.poke(snoopID.U);io.flit.SrcID.get.poke(0.U)
      hitThisCycle.get(addr).foreach(src=>hit("C03c",s"pa=0x${addr.toString(16)} demandHit=true oldSource=$src snoopCredit=true"))
      sentSnps(snoopID)=addr;snoopID+=1;snoopOffers.dequeue()
      record(s"SNP_SEND cycle=$cycle pa=0x${addr.toString(16)} id=${snoopID-1}")
      val noData=live.exists{case(id,pa)=>pa==addr&&livePrefetch(id)&&receivedData(id)==0}
      if(inFlightPrefetch(addr) && noData) hit("C03a",s"pa=0x${addr.toString(16)} downstreamOutstanding=true rxdatNotReturned=true")
      if(writes.get(addr).exists(_._1!=0) && !inFlightPrefetch(addr)) hit("C03b",s"pa=0x${addr.toString(16)} residentSource=${writes(addr)._1} unused=true")
    }
    if(flag(dut.io.chi.txrsp.flitv) && value(dut.io.chi.txrsp.flit.Opcode.get)==1) {
      val id=value(dut.io.chi.txrsp.flit.TxnID.get)
      val addr=sentSnps.remove(id).getOrElse(throw new AssertionError(s"unexpected snoop response $id"));writes.remove(addr);snoopDone+=1
      record(s"SNP_RESP cycle=$cycle pa=0x${addr.toString(16)} id=$id")
    }
    }
    if(cycle%64==0) ledgerCheck()
    for(c<-0 until 2) {
      val r=dut.probe.clientResult(c)
      if(flag(r.valid)) {
        val va=value(r.bits.vaddr);val key=(c,va);val tr=terminals.remove(key).get
        val attempts=translations.remove(key).get
        assert(flag(r.bits.retried)==(attempts==2) && flag(r.bits.miss)==tr.miss && flag(r.bits.fault)==(!tr.miss&&tr.fault))
        assert(flag(r.bits.dropped)==(tr.miss||tr.fault)&&value(r.bits.paddr)==va)
        retired+=1;if(attempts==2) actualRetry+=1;if(tr.miss||tr.fault) actualDrop+=1
      }
      val req=dut.probe.clientReq(c)
      if(flag(req.valid)) {val key=(c,value(req.bits));assert(!translations.contains(key));translations(key)=0}
    }
    if(value(dut.clientValid)==3) hit("C07",s"bothReqValid=true service=${flag(dut.tlbService)}")
    val slots=value(dut.slots);val states=(0 until 8).map(i=>((slots>>(i*3))&7).toInt)
    if(states.forall(_!=0)) hit("C10",s"nonFree=8 states=${states.mkString(",")} clientValid=${value(dut.clientValid)}")
    previousTlb.foreach(x=>assert(flag(tlb.req.valid)&&value(tlb.req.bits.vaddr)==x,"stalled TLB offer changed"))
    previousTlb=if(flag(tlb.req.valid)&& !flag(tlb.req.ready)) Some(value(tlb.req.bits.vaddr)) else None
    if(flag(tlb.req.valid)&&flag(tlb.req.ready)) {
      val owner=if(flag(dut.probe.tlbOwner)) 1 else 0;val va=value(tlb.req.bits.vaddr);val key=(owner,va)
      assert(translations.contains(key));translations(key)+=1;val attempt=translations(key);assert(attempt<=2)
      val miss=harsh&&random.nextInt(100)<20;val fault=harsh&&random.nextInt(100)<3
      chosenTlbDelay=frame.map(_.tlbDelay).getOrElse(Seq(1,3,7)(random.nextInt(3)))
      replies.enqueue(Translation(owner,va,cycle+chosenTlbDelay,attempt,miss,fault));tlbRequests+=1
    }
    cdpLedger.sample(dut.cdpAudit,cycle)
    val vbop=dut.probe.algorithm(0)
    if(flag(vbop.valid)) {
      val pa=value(vbop.bits)
      assert(permits(0)(pa)>0,s"unlicensed VBOP emit pa=$pa");permits(0)(pa)-=1
    }
    val qflags=value(dut.qf)
    for(s<-0 until slices if !hold && sources.forall(src=>qModels(queueIndex(s,src)).nonEmpty))
      hit("C08_release",s"slice=$s fourSourcesValid=true issueAllowed=true")
    for(s<-0 until slices) {
      val pending=sources.filter(src=>qModels(queueIndex(s,src)).nonEmpty)
      val queued=pending.map(src=>src->qModels(queueIndex(s,src)).head._1)
      for(a<-queued.indices;b<-a+1 until queued.size if queued(a)._2==queued(b)._2) {
        val key=s"${queued(a)._1}-${queued(b)._1}";pairNames(key)+=1
        hit("C02",s"slice=$s pair=$key pa=0x${queued(a)._2.toString(16)} simultaneousHeads=true")
      }
      if(sources.forall(pending.contains)) hit("C08",s"slice=$s queuedSources=1,2,3,9 hold=$hold")
      sources.foreach{src=>waitAge(s)(src)=if(pending.contains(src)) waitAge(s)(src)+1 else 0;maxWait(s)(src)=maxWait(s)(src).max(waitAge(s)(src))}
    }
    for(i<-qModels.indices) {
      val q=dut.cq(i);val f=((qflags>>(i*4))&15).toInt;val model=qModels(i)
      val capacity=if(i<9*slices && Set(1,2)(i%9)) 16 else 32
      assert(bit(f,3)==(model.size==capacity),s"queue full oracle cycle=$cycle queue=$i size=${model.size} flags=$f")
      val take=bit(f,1);val put=bit(f,0);val over=put&&model.size==capacity&& !take
      assert(bit(f,2)==over,s"queue overwrite cycle=$cycle queue=$i")
      if(take) {assert(model.nonEmpty);val (addr,bits)=model.dequeue();assert(value(q.head)==addr&&value(q.headPacked)==bits,s"FIFO payload queue=$i")}
      if(over) {model.dequeue();hit("C09",s"queue=$i full=true put=true take=false overwritten=true")}
      if(put) model.enqueue((value(q.put),value(q.putPacked)))
      if(put&&take) hit("C09_simultaneous",s"queue=$i put=true take=true full=${bit(f,3)}")
    }
    for(s<-0 until slices) {
      val mask=value(dut.probe.produced(s)).toInt
      sources.zipWithIndex.foreach{case(src,b)=>if((mask&(1<<b))!=0) produced(s)(src)+=1}
      if(flag(dut.probe.entered(s).valid)) {
        val src=value(dut.probe.entered(s).bits).toInt;entered(s)(src)+=1
        val addr=value(dut.enterPA(s))
        entries+=((cycle,addr,src))
        if(src==9) extension.foreach(_.model.observeCDPPrefetch(addr,cycle))
        record(s"PF_ENTER cycle=$cycle slice=$s pa=0x${addr.toString(16)} source=$src")
      }
    }
    if(recvNext.nonEmpty) recvInputs+=1
    recvNext=None
    if(extension.nonEmpty) {
      extension.get.sample(cycle)
      val m=extension.get.model
      if(m.empty){live.clear();livePrefetch.clear()}
    } else {
    if(flag(dut.io.chi.txreq.flitv)) {
      val f=dut.io.chi.txreq.flit;val id=DnTxnID(value(f.TxnID.get));val src=value(f.SrcID.get);val op=value(f.Opcode.get);val addr=value(f.Addr.get)
      assert(!live.contains(id));live(id)=addr
      if(dut.observation.dirProbeBySlice.flatten.exists(p=>flag(p.reqActive)&&flag(p.pRxReqIsPrefetch)&&value(p.tshrPaddr)==addr)) livePrefetch+=id
      dn.noteRequest(id)
      record(s"DN_REQ cycle=$cycle txn=${id.v} pa=0x${addr.toString(16)} opcode=$op")
      chosenDelay=frame.map(_.dnDelay).getOrElse(if(randomMode) random.nextInt(32) else 0)
      val delay=latency+chosenDelay
      if(dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id,src,cycle+delay,dn.downstreamEvictRspOpcode(op)))
      else {readRequests+=((cycle,addr));for(b<-0 until 2) data.enqueue(PendingDnComp(id,src,cycle+delay+b,b,if(op==7) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC,addr))}
    }
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
      if(op==2) record(s"COMP_ACK cycle=$cycle rn=${value(f.SrcID.get)} txn=${value(f.TxnID.get)}")
    }
    if(flag(dut.io.chi.txdat.flitv)&&value(dut.io.chi.txdat.flit.Opcode.get)==L2TopDownstreamOpcodes.CopyBackWrData) {
      val f=dut.io.chi.txdat.flit
      hnOrdering.writeData(value(f.SrcID.get),value(f.TxnID.get),value(f.DataID.get).toInt,cycle)
      record(s"COPYBACK_DATA cycle=$cycle rn=${value(f.SrcID.get)} dbid=${value(f.TxnID.get)} beat=${value(f.DataID.get)}")
    }
    }
    assert(!flag(dut.io.t1p.head.DnRSP.valid),"unexpected response (external Stash is silent)")
    tapeWriter.foreach{out=>
      out.println(ConflictFrame(cycle,frameHold,frameTrain,rawRecv,rawSnp,readyNow,
        chosenData.map(_.dnTxnID.v.toInt).getOrElse(-1),chosenData.map(_.addr).getOrElse(BigInt(0)),
        chosenData.map(_.beat).getOrElse(-1),chosenDelay,chosenTlbDelay,tapeDemands.toSeq).encode)
      out.flush()
    }
    tapeDemands.clear()
    driver.sample(cycle)
    if(driver.completed!=lastCompleted || flag(dut.io.chi.txreq.flitv) || flag(dut.io.chi.rxdat.flitv)||flag(dut.io.chi.rxrsp.flitv)) {lastProgress=cycle;lastCompleted=driver.completed}
    assert(driver.pending==0 || cycle-lastProgress<6000,s"demand no-progress seed=$seed cycle=$cycle pending=${driver.pending}")
    dn.stepWithCredits(cycle);cycle+=1
  }
  def until(test: =>Boolean,limit:Int=4000):Unit={val start=cycle;while(!test){assert(cycle-start<limit,s"condition watchdog seed=$seed cycle=$cycle");tick()}}
  def drain(queues:Boolean=true):Unit={
    var quiet=0;val start=cycle
    while(quiet<80) {
      assert(cycle-start<40000,s"drain watchdog seed=$seed cycle=$cycle live=$live slots=${value(dut.slots)} tlb=$translations")
      tick()
      val empty=driver.pending==0 && data.isEmpty && rsp.isEmpty && live.isEmpty && snps.isEmpty && replies.isEmpty && sentSnps.isEmpty &&
        snoopOffers.isEmpty && snpNext.isEmpty && hnOrdering.empty && extension.forall(_.model.empty) &&
        !pendingTshrs && (if(queues) qModels.forall(_.isEmpty)&&value(dut.slots)==0&&translations.isEmpty&& !flag(dut.algorithmBusy) else true)
      quiet=if(empty) quiet+1 else 0
    }
  }
  def init():Unit={
    up.initInputs();dn.initInputs();dut.log.dump.poke(false.B);dut.log.clean.poke(false.B)
    dut.holdIssue.get.poke(false.B);dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get))
    dut.io.pfRecv.get.pf_en.poke(true.B);dut.io.pfRecv.get.pf_source.poke(10.U);controls()
    dut.io.pfTlb.get.req.ready.poke(true.B);dut.io.pfTlb.get.resp.valid.poke(false.B)
    dut.io.pfTlb.get.resp.bits.poke(0.U.asTypeOf(dut.io.pfTlb.get.resp.bits))
    dut.io.pfTlb.get.pmp_resp.poke(0.U.asTypeOf(dut.io.pfTlb.get.pmp_resp))
    dut.reset.poke(true.B);dut.clock.step(8);dut.reset.poke(false.B);dn.activateRxLink()
    for(c<- -300 until 0) dn.stepWithCredits(c)
  }
  def dumpFailure():Unit={
    println(s"CONFLICT_FAILURE_STATE cycle=$cycle driver=${driver.pendingSummary} live=$live receivedData=$receivedData snps=$snps sentSnps=$sentSnps hold=$hold pendingTshrs=$pendingTshrs algorithmBusy=${flag(dut.algorithmBusy)} queues=${qModels.map(_.size)} slots=${value(dut.slots)} translations=$translations replies=$replies hnOrderingEmpty=${hnOrdering.empty}")
    extension.foreach{e=>
      println(s"HN_EXT_FAILURE_STATE cycle=$cycle ${e.model.pendingSummary}")
      e.model.counts.toSeq.sortBy(_._1).foreach{case(k,n)=>println(s"HN_EXT_FAILURE_COVER seed=$seed slices=$slices cycle=$cycle name=$k count=$n")}
      e.model.overlapWindows.toSeq.sortBy(_._1).foreach{case(k,n)=>println(s"HN_EXT_FAILURE_OVERLAP seed=$seed slices=$slices name=$k windows=$n cycles=${e.model.overlapCycles(k)} windowLength=512")}
    }
    for(s<-0 until slices;t<-0 until dut.nMSHR){val p=dut.ct(s)(t)
      println(s"CONFLICT_FAILURE_TSHR slice=$s tshr=$t flags=${value(p.flags)} requestWaits=${value(p.requestWaits)} pa=${value(p.pa).toString(16)} metaFlags=${value(p.metaFlags)} localState=${value(p.localState)} directoryState=${value(p.directoryState)} requestState=${value(p.requestState)} snoopState=${value(p.snoopState)}")
    }
    eventHistory.foreach(x=>println("CONFLICT_HISTORY "+x))
  }
  def finish():Unit={
    hold=false;tlbReady=true;paused.clear();drain();ledgerCheck()
    assert(driver.pending==0&&driver.completed==submitted&&driver.checkedBeats==submitted*2,"data scoreboard closure")
    assert(translations.isEmpty&&terminals.isEmpty&&permits.forall(_.values.forall(_==0)),"TLB permits/inflight closure")
    cdpLedger.finish(s"conflict seed=$seed slices=$slices")
    assert(modelRetry==actualRetry&&modelDrop==actualDrop,"TLB retry/drop closure")
    assert(sentSnps.isEmpty&&receivedData.isEmpty&&value(dut.slots)==0&&qModels.forall(_.isEmpty),"final occupancy zero")
    assert(hnOrdering.empty&&snoopOffers.isEmpty,"HN ordering closure")
    extension.foreach{e=>
      e.model.finish()
      e.model.counts.toSeq.sortBy(_._1).foreach{case(k,n)=>println(s"HN_EXT_COVER seed=$seed slices=$slices name=$k count=$n")}
      e.model.overlapWindows.toSeq.sortBy(_._1).foreach{case(k,n)=>println(s"HN_EXT_OVERLAP seed=$seed slices=$slices name=$k windows=$n cycles=${e.model.overlapCycles(k)} windowLength=512")}
      println(s"HN_EXT_CLOSURE pass=true seed=$seed slices=$slices pending=0 credits=0")
    }
    println(s"HN_CHI_ORDER_CHECK pass=true seed=$seed pending=0 dataStallCycles=$dataOrderStalls snoopStallCycles=$snoopOrderStalls responseStallCycles=$responseOrderStalls")
    println(s"CONFLICT_CLOSURE pass=true seed=$seed slices=$slices cycles=$cycle demands=$submitted checkedBeats=${driver.checkedBeats} stashes=${driver.stashes} recv=$recvInputs tlbRequests=$tlbRequests tlbRetired=$retired retries=$actualRetry drops=$actualDrop snoops=$snoopDone pending=0")
    Seq("C01","C01_enter","C01_wait","C01_hit","C02","C03a","C03b","C03c","C04","C05","C06","C06_mixed","C07","C08","C08_release","C09","C09_simultaneous","C10").foreach(k=>if(!evidence.contains(k)) evidence(k)=0)
    evidence.toSeq.sortBy(_._1).foreach{case(k,n)=>println(s"CONFLICT_COVER seed=$seed slices=$slices name=$k count=$n")}
    pairNames.toSeq.sortBy(_._1).foreach{case(k,n)=>println(s"CONFLICT_PAIR seed=$seed slices=$slices pair=$k count=$n")}
    sources.foreach(src=>println(s"CONFLICT_STARVATION source=$src maxQueueWait=${maxWait.map(_(src)).max} entered=${entered.map(_(src)).sum} expectedFixedPriority=true"))
  }
}

object TestTop_L2PrefetchConflict extends App with ChiselSim {
  Constantin.init(false)
  def opt(n:String,d:String)=args.find(_.startsWith(n+"=")).map(_.split("=",2)(1)).getOrElse(d)
  val slices=opt("--slices","1").toInt
  val mode=opt("--mode","directed")
  val selectedScenarios=opt("--cases",opt("--case","lifecycle")).split(",").toSeq
  val level=opt("--level","light")
  val first=opt("--seed","1").toInt;val last=opt("--last-seed",first.toString).toInt
  val selectedSeeds=opt("--seeds","") match {
    case "" => (first to last).toSeq
    case seeds => seeds.split(",").map(_.toInt).toSeq
  }
  require(selectedSeeds.nonEmpty && selectedSeeds.distinct.size==selectedSeeds.size)
  val cycles=opt("--cycles","20000").toInt
  val sourceMode=opt("--sources","all")
  require(Set("all","none","bop","vbop","recv","cdp")(sourceMode))
  val sourceModes=opt("--source-modes",sourceMode).split(",").toSeq
  require(sourceModes.forall(Set("all","none","bop","vbop","recv","cdp")))
  val ablations=opt("--ablation-modes","baseline").split(",").toSeq
  require(ablations.forall(Set("baseline","no-dct","no-retry","no-split","all-off")))
  val seedAblations=opt("--seed-ablation-plan","") match {
    case "" => selectedSeeds.flatMap(seed=>ablations.map(ablation=>(seed,ablation)))
    case plan => plan.split(",").toSeq.map{x=>val a=x.split(":",2);(a(0).toInt,a(1))}
  }
  require(seedAblations.nonEmpty&&seedAblations.forall{case(seed,a)=>selectedSeeds.contains(seed)&&
    Set("baseline","no-dct","no-retry","no-split","all-off")(a)})
  val requestedRecordTape=opt("--record-tape","")
  val requestedReplayTape=opt("--replay-tape","")
  val autoIsolation=opt("--auto-isolate-tape","")
  val phases=if(autoIsolation.nonEmpty) Seq("record","all","none","bop","vbop","recv","cdp") else sourceModes
  val fullHistory=opt("--full-history","false").toBoolean
  val cfg=new L2Configuration(8,true,0 until slices,bopEnable=true,pfRecvEnable=true,pfCtrlEnable=true,
    pfTrainEnable=true,pfTlbEnable=true,vbopStubEnable=true,cdpStubEnable=true,
    bopParams=OceanusBOPParameters(badScore=0,roundMax=100000),
    vbopParams=OceanusVBOPParameters(badScore=0,roundMax=100000))
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params(mshrSize=8,ways=4,sets=128)
    case CHIParametersKey=>CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case CCHIParametersKey=>CCHIParameters()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=true,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=true,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=HasSimulator.default
  simulate(new TestTopConflict(cfg)(params)){dut=>
    var hostFailures=0
    for((seed,ablation)<-seedAblations;scenario<-selectedScenarios;phase<-phases) {
      val activeMode=if(phase=="record") "all" else phase
      val recordTape=if(autoIsolation.nonEmpty && phase=="record") autoIsolation else requestedRecordTape
      val replayTape=if(autoIsolation.nonEmpty && phase!="record") autoIsolation else requestedReplayTape
      println(s"CONFLICT_BEGIN seed=$seed slices=$slices mode=$mode case=$scenario level=$level cycles=$cycles ablation=$ablation")
      println(s"CONFLICT_ISOLATION_BEGIN seed=$seed slices=$slices level=$level sources=$activeMode phase=$phase")
      val r=new ConflictRun(dut,seed,slices,mode=="random")
      r.cdpEnable=opt("--cdp-enable","true").toBoolean
      r.sourceMode=activeMode;r.fullHistory=fullHistory;r.hnFlitsOnly=opt("--hn-flits-only","false").toBoolean
      if(opt("--hn-extension","false").toBoolean) {
        require(requestedRecordTape.isEmpty&&requestedReplayTape.isEmpty&&autoIsolation.isEmpty,"HN extension uses event trace instead of legacy-only tape")
        val noDct=Set("no-dct","all-off")(ablation)
        val noRetry=Set("no-retry","all-off")(ablation)
        val noSplit=Set("no-split","all-off")(ablation)
        r.enableHNExtension(HNExtConfig(retryPercent=if(noRetry)0 else opt("--retry-percent","0").toInt,
          splitPercent=if(noSplit)0 else opt("--split-percent","50").toInt,
          dctPercent=if(noDct)0 else opt("--dct-percent","50").toInt,
          seed=0x484e0000L+seed*100+slices,forwardEnable= !noDct && opt("--forward-enable","true").toBoolean))
      }
      if(recordTape.nonEmpty) {
        val path=java.nio.file.Paths.get(recordTape.replace("{seed}",seed.toString))
        require(!java.nio.file.Files.exists(path),s"refuse overwrite tape $path")
        r.tapeWriter=Some(new java.io.PrintWriter(java.nio.file.Files.newBufferedWriter(path)))
        r.tapeWriter.get.println(s"# HEAD=${sys.env.getOrElse("PF_VALIDATION_HEAD","unrecorded")} seed=$seed slices=$slices level=$level")
      }
      if(replayTape.nonEmpty) {
        val input=scala.io.Source.fromFile(replayTape.replace("{seed}",seed.toString))
        try r.replayFrames=input.getLines().filterNot(_.startsWith("#")).map(ConflictFrame.parse).map(f=>f.cycle->f).toMap
        finally input.close()
        require(r.replayFrames.nonEmpty)
        println(s"CONFLICT_REPLAY seed=$seed sources=$activeMode tape=$replayTape frames=${r.replayFrames.size} demandOffers=${r.replayFrames.values.map(_.demands.count(!_._3)).sum}")
      }
      r.init()
      try {
      if(replayTape.nonEmpty) {
        val lastCycle=r.replayFrames.keys.max
        while(r.cycle<=lastCycle) r.tick()
        r.replayFrames=Map.empty
      } else if(mode=="directed") {
        scenario match {
          case "demand-snoop-refill" | "demand-refill" =>
            // Regression for the former illegal pending-SNP/CompData overlap.
            // The HN must receive SnpResp before returning the refill.
            // A baseline demand miss, a same-line invalidating snoop before
            // RXDAT, and a second read after both original transactions retire.
            // The memory is read-only, so either miss/refetch or a hit must
            // return the same independently computed bytes.
            // Distinct sets keep a preceding control case from seeding the
            // SRAM slot: reset invalidates Directory, but does not clear DS.
            val a=BigInt(if(scenario=="demand-refill") 0x2069c0 else 0x206980)
            r.paused+=a;r.enqueue(a,enabled=false)
            r.until(r.readRequests.exists(_._2==a))
            val issued=r.readRequests.find(_._2==a).get._1
            r.until(r.cycle>=issued+30)
            if(scenario=="demand-snoop-refill") r.sendSnoop(a) else r.tick()
            r.paused-=a;r.drain()
            println(s"CONFLICT_MINIMAL_PHASE firstReadComplete=true pa=0x${a.toString(16)} cycle=${r.cycle} sources=$activeMode")
            r.enqueue(a,enabled=false);r.drain()
          case "hn-extension" =>
            require(r.extension.nonEmpty)
            val a=BigInt(0x220040)
            for(op<-L2HNExtensionModel.fwdOps++L2HNExtensionModel.stashOps++L2HNExtensionModel.normalOps) {
              r.enqueue(a,enabled=false);r.drain()
              r.extension.get.offer(a,Some(op));r.drain()
              r.enqueue(a,enabled=false);r.drain()
            }
            for(batch<-0 until 12) {
              for(i<-0 until 12)r.enqueue(BigInt(0x400000+(batch*16+i)*64),enabled=false)
              for(t<-0 until 400) {
                if(t%19==0&&r.live.nonEmpty)r.extension.get.offer(r.live.values.head)
                r.tick()
              }
              r.drain()
            }
          case "lifecycle" =>
            // Recv is a genuine internal prefetch source. No data are returned
            // before the queued demand has demonstrated actual RBE waiting.
            val a=BigInt(0x220040);r.paused+=a;r.recv(a)
            r.until(r.readRequests.exists(_._2==a));r.enqueue(a,enabled=false)
            r.until(r.evidence("C01_wait")>0 && r.evidence("C01_enter")>0)
            r.paused-=a;r.drain()
            assert(r.trains.exists(t=>t._2==a&&t._3==1&&t._5),"late demand did not hit old source")
            assert(r.readRequests.count(_._2==a)==1,"late demand duplicated read")
            // a: prefetch issued, no RXDAT yet.
            val b=BigInt(0x240040);r.paused+=b;r.recv(b);r.until(r.readRequests.exists(_._2==b))
            r.sendSnoop(b);r.paused-=b;r.drain();r.enqueue(b,enabled=false);r.drain()
            // b: refill/Meta source nonzero; demand has not consumed it.
            val c=BigInt(0x260040);r.recv(c);r.drain();assert(r.writes.get(c).exists(_._1==1))
            r.sendSnoop(c);r.drain();r.enqueue(c,enabled=false);r.drain()
            // c: react to the ACTUAL hit event, not a guessed pipeline delay.
            val d=BigInt(0x280040);r.recv(d);r.drain();r.hitSnoop=Some(d);r.enqueue(d,enabled=false)
            r.until(r.evidence("C03c")>0);r.drain();r.enqueue(d,enabled=false);r.drain()
            // External Stash, silent in this CCHI base, same-line RBE overlap.
            val e=BigInt(0x2a0040);r.paused+=e;r.recv(e);r.until(r.readRequests.exists(_._2==e))
            r.enqueue(e,enabled=false,stash=true);r.until(r.evidence("C05")>0)
            r.paused-=e;r.drain();r.enqueue(e,enabled=false);r.drain()
            assert(Seq("C01","C01_enter","C01_wait","C01_hit","C03a","C03b","C03c","C05").forall(r.evidence(_)>0))
            assert(r.snoopDone==3,"three snoop windows must each retire")
          case "sources" =>
            // Separate real training streams give independent source heads.
            r.hold=true;r.controls(b=true,v=false,c=false);r.enqueue(0x300040);r.drain(queues=false)
            r.controls(b=false,v=true,c=true);r.enqueue(0x340040);r.drain(queues=false)
            r.controls();r.enqueue(0x380040,enabled=false)
            r.recv(0x3a0040);r.drain(queues=false)
            r.until(Seq(1,2,3,9).forall(src=>r.qModels(r.queueIndex(0,src)).nonEmpty),500)
            r.tick();val start=r.entries.size;r.hold=false;r.drain()
            val order=r.entries.drop(start).map(_._3).distinct.toSeq
            assert(order.take(4)==Seq(1,2,3,9),s"four-source order=$order")
            assert(r.evidence("C08_release")>0)
            // Same physical target from VBOP/PBOP's initial +2 prediction.
            r.hold=true;r.enqueue(0x400040);r.drain(queues=false)
            r.until(r.evidence("C02")>0,300);r.hold=false;r.drain()
            // Force overflow using legal finite Recv bursts under real demand
            // pressure; keep all controls enabled during this stress segment.
            r.hold=true
            for(i<-0 until 100) {r.enqueue(BigInt(0x500000+i*64),enabled=true);r.recv(BigInt(0x600000+i*64))}
            r.drain(queues=false);r.hold=false;r.drain()
            assert(r.evidence("C09")>0,"queue full/overwrite never occurred")
          case "pairs" =>
            for((pair,index)<-Seq(Seq(1,2),Seq(1,3),Seq(1,9),Seq(2,3),Seq(2,9),Seq(3,9)).zipWithIndex) {
              val target=BigInt(0x900080+index*0x20000)
              r.hold=true
              if(pair.contains(2)||pair.contains(3)) {
                r.controls(b=pair.contains(3),v=pair.contains(2),c=false)
                r.enqueue(target-128);r.drain(queues=false)
              }
              if(pair.contains(9)) {
                r.controls(b=false,v=false,c=true);r.enqueue(target-4096);r.drain(queues=false)
              }
              if(pair.contains(1)) {
                r.controls(b=false,v=false,c=false);r.enqueue(target+0x800,enabled=false);r.recv(target);r.drain(queues=false)
              }
              r.controls()
              assert(pair.forall(src=>r.qModels(r.queueIndex(0,src)).headOption.exists(_._1==target)),s"pair heads not constructed pair=$pair target=$target")
              r.tick();val key=pair.mkString("-");assert(r.pairNames(key)>0,s"pair overlap unobserved $key")
              r.hold=false;r.drain()
              println(s"CONFLICT_DIRECTED_PAIR pass=true pair=$key target=0x${target.toString(16)} count=${r.pairNames(key)}")
            }
          case "replacement" =>
            // Same actual set: sets=128, line=64 => stride 8192.
            r.controls(b=false,v=false,c=false)
            for(i<-0 until 12) {
              val pa=BigInt(0x80000+i*dut.p(L2ParamsKey).sets*64);r.recv(pa);r.drain()
              println(s"CONFLICT_REPL_PHASE i=$i pa=0x${pa.toString(16)} entered=${r.entries.size} reads=${r.readRequests.size} writes=${r.writes.toSeq.sortBy(_._1).map{case(a,(s,d))=>s"${a.toString(16)}:$s:$d"}.mkString(",")} evictions=${r.evicts.size} rtlEvictions=${r.value(dut.moduleEvictions)}")
            }
            assert(r.evicts.size>=8 && r.evidence("C04")>0,s"actual replacement overlap missing evictions=${r.evicts.size} overlap=${r.evidence("C04")}")
            for(i<-0 until 12) {r.enqueue(BigInt(0x80000+i*dut.p(L2ParamsKey).sets*64),enabled=false);r.drain()}
          case other=>sys.error("unknown case "+other)
        }
      } else {
        // Warmup is explicitly outside the measured random window. All four
        // source enables stay high, including idle-demand cells.
        for(i<-0 until 24) {r.enqueue(BigInt(0x200000+i*128));r.recv(BigInt(0x210000+i*64))}
        r.drain();r.evidence.clear();r.episodes.clear();r.lastHit.clear();r.firstEvidence.clear();r.pairNames.clear();r.maxWait.foreach(a=>java.util.Arrays.fill(a,0));r.waitAge.foreach(a=>java.util.Arrays.fill(a,0))
        val begin=r.cycle
        val offerBegin=r.driver.demandOfferCycles;val blockedBegin=r.driver.demandBlockedCycles
        val acceptedBegin=r.driver.accepted.size
        val period=Map("idle"->Int.MaxValue,"light"->32,"medium"->8,"heavy"->2,"saturated"->1)(level)
        var offered=0
        for(t<-0 until cycles) {
          if(level!="idle" && t%period==0 && r.driver.queued<24) {
            val line=if(r.random.nextInt(4)==0) r.random.nextInt(64) else r.random.nextInt(1024)
            val addr=if(r.random.nextInt(4)==0 && r.live.nonEmpty) r.live.values.head else BigInt(0x200000+line*64)
            r.enqueue(addr);offered+=1
          }
          if(level!="idle" && t%613==0 && r.livePrefetch.nonEmpty && r.driver.queued<24)
            r.enqueue(r.live(r.livePrefetch.head),enabled=true,stash=true)
          r.hold=t%311<75
          if(t%257<8 || r.random.nextInt(97)==0) {
            val addr=if(r.random.nextInt(3)==0&&r.live.nonEmpty) r.live.values.head else BigInt(0x210000+r.random.nextInt(512)*64)
            r.recvNext=Some(addr)
          }
          if(r.extension.nonEmpty&&t%11==0) {
            val target=if(r.live.nonEmpty)r.live.values.head else BigInt(0x210000+r.random.nextInt(512)*64)
            r.extension.get.offer(target)
          }
          // Sparse precision snoops, never a storm; one outstanding at most.
          if(t%991==0 && r.sentSnps.isEmpty && r.live.nonEmpty) r.snpNext=Some(r.live.values.head)
          r.tick()
          if(t%2048==0) println(s"CONFLICT_PROGRESS seed=$seed slices=$slices level=$level stimulus=$t completed=${r.driver.completed} pending=${r.driver.pending}")
        }
        assert(r.cycle-begin==cycles)
        Seq("C01","C01_enter","C01_wait","C01_hit","C02","C03a","C03b","C03c","C04","C05","C06","C06_mixed","C07","C08","C08_release","C09","C09_simultaneous","C10").foreach(k=>println(s"CONFLICT_RANDOM_COVER seed=$seed slices=$slices level=$level name=$k count=${r.evidence(k)} episodes=${r.episodes(k)} applicable=${!(slices==1&&k.startsWith("C06"))}"))
        val accepted=r.driver.accepted.drop(acceptedBegin).count{case(_,q)=>q.opcode==CCHIOpcode.ReadShared.opcode}
        println(s"CONFLICT_STIMULUS seed=$seed slices=$slices level=$level cycles=$cycles offered=$offered accepted=$accepted validCycles=${r.driver.demandOfferCycles-offerBegin} blockedCycles=${r.driver.demandBlockedCycles-blockedBegin} maxOutstanding=${r.driver.maxOutstanding}")
      }
      r.finish()
      println(s"CONFLICT_RESULT pass=true seed=$seed slices=$slices mode=$mode case=$scenario level=$level sources=$activeMode ablation=$ablation")
      } catch {
        // A host scoreboard failure leaves the simulator alive. Preserve its
        // evidence and reset before the next seed; RTL assertion failures still
        // terminate this JVM and are resumed by the external matrix runner.
        case e: java.lang.AssertionError if mode=="random" =>
          hostFailures+=1
          r.dumpFailure()
          println(s"CONFLICT_RESULT pass=false seed=$seed slices=$slices mode=$mode case=$scenario level=$level sources=$activeMode ablation=$ablation cycle=${r.cycle} error=${e.getMessage}")
      } finally r.tapeWriter.foreach(_.close())
    }
    assert(hostFailures==0,s"$hostFailures random seeds failed host scoreboards; see pass=false records")
  }
}
