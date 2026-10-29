package oceanus

// C5: record the real cache -> CDP boundary, including every input bus and
// accepted/discarded request identity. Nothing in the production cache is
// forced. A separate simulator replays these records into ReferenceCDP.
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
import java.nio.file.{Files, Paths}
import java.io.{PrintWriter, FileOutputStream}
import java.util.zip.GZIPOutputStream

class TestTopCDPRawAudit(cfg: L2Configuration)(implicit p: Parameters) extends TestTop_L2Top(cfg) {
  def read[T <: Data](x: T): T = BoringUtils.bore(x)
  val c = module.cdp.get
  val s = c.sentUnit.get
  val a = module.pfTlbAdapter.get
  val fields = mutable.ArrayBuffer.empty[(String, UInt)]
  def add(name: String, value: Data): Unit = fields += name -> read(value).asUInt
  def event(name: String, valid: Bool, fields: (String, Data)*): Unit = {
    add(name, valid)
    fields.foreach { case (n, b) => add(name + "." + n, Mux(read(valid), read(b).asUInt, 0.U(b.getWidth.W))) }
  }
  def fire(d: ReadyValidIO[_]): Bool = read(d.valid) && read(d.ready)
  val entries = read(s.entries)
  add("enable", module.cdpControlEnable)
  for ((name, t) <- Seq("vpn" -> c.io.vpnTrain, "filter" -> c.io.filterTrain))
    event(name, t.valid, "pa" -> t.bits.paddr, "victim" -> t.bits.evictPaddr,
      "va" -> t.bits.vaddr, "hit" -> t.bits.hit, "prefetched" -> t.bits.prefetched,
      "source" -> t.bits.pfSource, "reqSource" -> t.bits.reqSource,
      "trainEnable" -> t.bits.trainEnable, "vpnValid" -> t.bits.vpnValid,
      "used" -> t.bits.isUsed, "evict" -> t.bits.isEvict)
  val h = c.io.capturedHalf.get
  event("half", h.valid, "data" -> h.bits.data, "source" -> h.bits.pfSource,
    "depth" -> h.bits.pfDepth, "hit" -> h.bits.isHit, "slice" -> h.bits.slice)
  add("halfPA", Mux(read(module.cdpData.valid), read(module.cdpData.bits.paddr), 0.U))
  add("halfHigh", read(module.cdpData.bits.high) && read(module.cdpData.valid))
  add("halfAccepted", PopCount(read(c.detectFrontend.admitted)))
  add("halfRejected", read(c.detectFrontend.dropped).reduce(_ +& _))
  add("vpnAccepted", fire(c.vpnTrainReqBuf.io.enq))
  add("vpnDropped", read(c.vpnTrainReqBuf.io.enq.valid) && !read(c.vpnTrainReqBuf.io.enq.ready))
  c.filterTraining.foreach { f =>
    add("filterAccepted", fire(f.queue.io.enq))
    add("filterDropped", read(f.queue.io.enq.valid) && !read(f.queue.io.enq.ready))
  }
  event("candidate", s.io.in.valid, "va" -> s.io.in.bits.pfVAddr, "depth" -> s.io.in.bits.pfDepth)
  add("allocated", s.allocate); add("duplicate", s.duplicate); add("full", s.fullDrop)
  for (i <- 0 until 16) {
    event(s"translationDrop$i", s.translationDrop(i), "va" -> Cat(entries(i).vTag, 0.U(6.W)), "depth" -> s.entries(i).pfDepth)
  }
  event("filterDrop", s.filterDrop, "va" -> Cat(entries(read(s.idx2)).vTag, 0.U(6.W)), "depth" -> s.req2.pfDepth)
  event("out", fire(c.io.req), "pa" -> c.io.req.bits.paddr, "depth" -> c.io.req.bits.pfDepth)
  event("tlbRequest", fire(c.io.tlb.req), "va" -> c.io.tlb.req.bits)
  val result = c.io.tlb.resp
  event("terminal", fire(result), "va" -> result.bits.vaddr, "pa" -> result.bits.paddr,
    "retry" -> result.bits.retried, "miss" -> result.bits.miss, "fault" -> result.bits.fault,
    "mmio" -> result.bits.mmio, "uncache" -> result.bits.uncache, "drop" -> result.bits.dropped)
  event("externalReq", fire(a.io.tlb.req), "va" -> a.io.tlb.req.bits.vaddr,
    "owner" -> read(a.owner)(read(a.issue)), "retry" -> read(a.retried)(read(a.issue)))
  event("adapterClient0Req",fire(a.io.clients(0).req),"va"->a.io.clients(0).req.bits)
  event("adapterClient0Resp",fire(a.io.clients(0).resp),"va"->a.io.clients(0).resp.bits.vaddr,"pa"->a.io.clients(0).resp.bits.paddr,
    "retry"->a.io.clients(0).resp.bits.retried,"miss"->a.io.clients(0).resp.bits.miss,"drop"->a.io.clients(0).resp.bits.dropped)
  add("adapterAlloc",a.alloc)
  add("externalReq.index",a.issue)
  add("externalResponse.pa",a.io.tlb.resp.bits.paddr.head)
  add("externalResponse.miss",a.io.tlb.resp.bits.miss)
  for(i<-0 until 8) {
    add(s"adapterSlot$i.state",a.state(i));add(s"adapterSlot$i.va",a.address(i));add(s"adapterSlot$i.owner",a.owner(i))
  }
  add("externalResponse", a.io.tlb.resp.valid)
  add("externalResponseBits", a.io.tlb.resp.bits)
  add("externalPMP", a.io.tlb.pmp_resp)
  add("vbopBlockedCDP", read(a.io.clients(0).req.valid) && read(a.io.clients(1).req.valid))
  when(read(a.io.clients(0).req.valid) && read(a.io.clients(1).req.valid)) {
    assert(!read(a.io.clients(1).req.ready), "VBOP must win shared TLB slot admission")
  }
  for ((slice, bank) <- module.slices.zipWithIndex) {
    event(s"enter$bank", fire(slice.io.UpRXREQ) && read(slice.io.UpRXREQ.bits.SrcID) === cfg.pfSrcID.U,
      "pa" -> slice.io.UpRXREQ.bits.Addr, "txn" -> slice.io.UpRXREQ.bits.TxnID)
    add(s"captureDrops$bank", slice.io.cdpHalfDrops)
    add(s"collisionDrops$bank", slice.io.cdpCollisionDrops)
    for (i <- 0 until nMSHR) {
      val t = slice.io.cdpTrain(i); val e = slice.io.cdpEvict(i)
      event(s"train$bank.$i", t.valid, "bits" -> t.bits)
      event(s"evict$bank.$i", e.valid, "pa" -> e.bits.paddr, "source" -> e.bits.pfSource, "depth" -> e.bits.pfDepth)
    }
  }
  add("vpnCollisions", module.cdpEvents.get.vpnCollisions)
  add("filterCollisions", module.cdpEvents.get.filterCollisions)
  add("resident", PopCount(read(s.valids)))
  add("candidatePending", c.c4Input.valid)
  add("candidate.source",s.io.in.bits.pfSource)
  add("candidate.hit",s.io.in.bits.isHit)
  val ft=c.filterTraining.get
  add("filterPLRU",read(ft.pipe.replacer.asInstanceOf[freechips.rocketchip.util.SetAssocLRU].state_vec))
  event("filterTrainWrite",fire(ft.table.io.trainReq),"bits"->ft.table.io.trainReq.bits)

  // M4 read-only transient census; source queue ledgers are already exported
  // by the existing observation harness.
  add("tlbSlots",PopCount(read(a.state).map(_ =/= 0.U)))
  add("tlbRetry",PopCount(read(a.state).map(_ === 3.U)))
  add("tlbWaiting",PopCount(read(a.state).map(_ === 2.U)))
  add("tlbDone",PopCount(read(a.state).map(_ === 4.U)))
  // Filterfix retires directly from query stage two; there is no output FIFO.
  // Count the result stage for the overlapping transient census only.
  add("outputPending",read(s.pftS2).asUInt)
  add("outputValid",c.io.req.valid)
  add("outputReady",c.io.req.ready)
  add("vpnValids",PopCount(read(c.vpnTable.validArray).asUInt))
  val vpnValid=read(c.vpnTable.validArray);val vpnTags=read(c.vpnTable.tagArray);val vpnMeta=read(c.vpnTable.metaArray)
  fields += "vpnSnapshot"->Cat(vpnValid.asUInt,
    VecInit((0 until 4).flatMap(s=>(0 until 4).map(w=>Mux(vpnValid(s)(w).asUInt.orR,vpnTags(s)(w),0.U)))).asUInt,
    VecInit((0 until 4).flatMap(s=>(0 until 4).flatMap(w=>(0 until 4).map(u=>Mux(vpnValid(s)(w)(u),vpnMeta(s)(w)(u).asUInt,0.U))))).asUInt)
  val memories=Seq(c.filterTraining.get.table.metaArray)++c.filterTraining.get.table.satBanks
  fields += "filterMemoryWrites"->PopCount(memories.map(m=>read(m.io.w.req.valid) && read(m.io.w.req.ready)))
  add("filterWrite",fire(c.filterTraining.get.table.io.trainReq))
  event("filterQuery",fire(c.filterTraining.get.table.io.queryReq),"set"->c.filterTraining.get.table.io.queryReq.bits.setIdx)
  val filterRspValid=read(c.filterTraining.get.table.io.queryRsp.valid)
  fields += "filterResponse"->filterRspValid.asUInt
  fields += "filterResponse.masked"->Mux(filterRspValid,
    CDPC2Mask(read(c.filterTraining.get.table.io.queryRsp.bits.validVec),read(c.filterTraining.get.table.io.queryRsp.bits.tagVec),read(c.filterTraining.get.table.io.queryRsp.bits.satVec)),0.U)
  val halfCounts=module.slices.zipWithIndex.map { case (slice,bank)=>
    val children=slice.getClass.getMethods.find(_.getName=="getIds").get.invoke(slice)
      .asInstanceOf[scala.collection.Seq[AnyRef]]
    val q=children.collectFirst { case x:L2CDPHalfQueue=>x }.get
    add(s"halfCount$bank",q.io.occupancy)
  }
  val topChildren=module.getClass.getMethods.find(_.getName=="getIds").get.invoke(module)
    .asInstanceOf[scala.collection.Seq[AnyRef]]
  val topQueues=topChildren.collect {case q:OverwriteQueue[_]=>q}
  for(b<-0 until cfg.sliceNum) {
    val q=topQueues(b*9+8).asInstanceOf[OverwriteQueue[oceanus.compactchi.FlitREQ]]
    event(s"source9enq$b",fire(q.io.enq),"pa"->q.io.enq.bits.Addr,"txn"->q.io.enq.bits.TxnID)
    event(s"source9deq$b",fire(q.io.deq),"pa"->q.io.deq.bits.Addr,"txn"->q.io.deq.bits.TxnID)
    add(s"source9overwrite$b",q.io.overwrite)
  }
  val raw=IO(Output(Vec(cfg.sliceNum,Vec(nMSHR,new CDPRawLifecycle))))
  for ((slice,bank)<-module.slices.zipWithIndex; (t,i)<-CDPAuditReflect.field[Seq[L2TSHR]](slice,"tshrs").zipWithIndex) {
    val o=raw(bank)(i);val v=CDPAuditReflect.field[oceanus.l2.tshr.L2VPipeREQ](t,"vPipeREQ")
    o.exec:=read(CDPAuditReflect.field[Bool](v,"rxreq_fire"));o.req:=read(v.io.UpRXREQ.bits);o.context:=read(v.io.UpRXREQTrain.get)
    o.reqValid:=read(v.io.UpRXREQ.valid);o.addr:=read(t.io.toAlloc.paddr)
    o.dirRead:=read(t.io.fromDir.DirRdResp);o.dirMeta:=read(t.io.fromDir.META)
    o.dirWrite:=read(t.io.fromDir.DirWbArbComp);o.write:=read(t.io.toDir)
    o.metaMask:=read(v.io.tshr_meta_write_en);o.metaValue:=read(v.io.tshr_meta_write_meta)
    o.evictState:=read(CDPAuditReflect.field[Bool](v,"meta_wr_state_evictback_I"))
    o.dealloc:=read(CDPAuditReflect.field[Bool](t,"tshr_dealloc"))
    o.last:=read(CDPAuditReflect.field[Bool](t,"tshr_buffer_wen_last"))
    o.ds:=read(CDPAuditReflect.field[Bool](t,"tshr_buffer_wen_DS"))
    o.prefetchActive:=read(v.io.bopPerf.activePrefetch)
    o.enterREQ:=read(t.tshr_enter_REQ);o.reuseREQ:=read(t.io.fromAlloc.reuse.REQ)
    o.incomingSource:=read(t.io.UpRXREQ.SrcID);o.prefetchSourceID:=read(t.io.consts.pfSrcID)
    o.incomingOpcode:=read(t.io.UpRXREQ.Opcode);o.incomingData:=read(t.io.UpRXREQ.ExpCompData)
    o.rx:=read(CDPAuditReflect.field[Bool](t,"tshr_buffer_wen_RXDAT_0")) || read(CDPAuditReflect.field[Bool](t,"tshr_buffer_wen_RXDAT_2"))
    o.train:=read(t.io.cdpTrain);o.evict:=read(t.io.cdpEvict);o.line:=read(t.io.cdpData)
  }
  val selectedVpn=IO(Output(Valid(new OceanusCDPTrain)));selectedVpn:=read(c.io.vpnTrain)
  val selectedFilter=IO(Output(Valid(new OceanusCDPTrain)));selectedFilter:=read(c.io.filterTrain)
  val halfOut=IO(Output(Valid(new L2CDPHalf)));halfOut:=read(module.cdpData)
  val halfBank=IO(Output(UInt(2.W)));halfBank:=read(module.cdpDataSlice)
  val filterReady=IO(Output(Bool()));filterReady:=read(c.filterTraining.get.queue.io.enq.ready)
  val vpnReady=IO(Output(Bool()));vpnReady:=read(c.vpnTrainReqBuf.io.enq.ready)
  val audit=IO(Output(new CDPAuditProbe))
  CDPAuditProbe.connect(audit,c,read _)
  add("out.source",c.io.req.bits.pfSource);add("out.reqSource",c.io.req.bits.reqSource);add("out.needT",c.io.req.bits.needT)
  add("filterQueueCount",c.filterTraining.get.queue.io.count)
  add("filterDequeue",fire(c.filterTraining.get.queue.io.deq))
  add("vpnQueueCount",c.vpnTrainReqBuf.io.count)
  add("vpnDequeue",fire(c.vpnTrainReqBuf.io.deq))
  // One host peek per cycle for all raw lifecycle leaves. Avoid hundreds of
  // simulator RPCs while keeping named, independently decoded observations.
  def flattenRaw(prefix:String,d:Data):Unit=d match {
    case b:Bundle=>b.elements.toSeq.sortBy(_._1).foreach {case(k,v)=>flattenRaw(prefix+"."+k,v)}
    case v:Vec[_]=>v.zipWithIndex.foreach {case(x,i)=>flattenRaw(prefix+"."+i,x)}
    case x=>fields+=prefix->x.asUInt
  }
  for(bank<-0 until cfg.sliceNum;i<-0 until nMSHR)flattenRaw(s"raw$bank.$i",raw(bank)(i))
  val boundary = IO(Output(UInt(fields.map(_._2.getWidth).sum.W)))
  boundary := Cat(fields.map(_._2).reverse.toSeq)
  val widths = fields.map { case (n, v) => (n, v.getWidth) }.toVector
}


object CDPAuditReflect {
  def field[T](owner: AnyRef,name: String): T = {
    var cls: Class[_]=owner.getClass
    while(cls!=null) {
      try { val f=cls.getDeclaredField(name);f.setAccessible(true);return f.get(owner).asInstanceOf[T] }
      catch { case _: NoSuchFieldException=>cls=cls.getSuperclass }
    }
    throw new NoSuchFieldException(name)
  }
}

class CDPRawLifecycle(implicit val p: Parameters) extends Bundle {
  val exec=Bool();val req=new oceanus.compactchi.FlitREQStripped
  val context=new L2PrefetchTrainContext;val reqValid=Bool();val addr=UInt(48.W)
  val dirRead=Bool();val dirMeta=new L2Directory.MetaReadResult
  val dirWrite=Bool();val write=new L2Directory.PathToDirectory
  val metaMask=new L2Directory.MetaWriteMask;val metaValue=new L2Directory.Meta
  val evictState=Bool();val dealloc=Bool();val last=Bool();val ds=Bool();val rx=Bool()
  // Raw allocator/protocol observations; the host derives merged scan state.
  val prefetchActive=Bool();val enterREQ=Bool();val reuseREQ=Bool()
  val incomingSource=UInt(16.W);val prefetchSourceID=UInt(16.W)
  val incomingOpcode=UInt(8.W);val incomingData=Bool()
  val train=Valid(new L2CDPTrain);val evict=Valid(new L2CDPEvict);val line=Valid(new L2CDPLine)
}
