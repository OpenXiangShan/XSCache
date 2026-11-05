package oceanus

// Intent: retain only the stress counters and full-state observations. The
// full legacy timing/BE/debug probe hierarchy is not needed by this test.
// Production RTL, queue behavior and every host scoreboard remain unchanged.
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import oceanus.compactchi.FlitREQ
import oceanus.l2._
import utility._
import org.chipsalliance.cde.config.Parameters

class StressAccounting(slices: Int) extends Bundle {
  def counters = Vec(slices,Vec(10,UInt(64.W)))
  val recvAcceptedBySource=counters
  val prefetchSliceEnteredBySource=counters
  val prefetchQueueOverwriteBySource=counters
  val prefetchEnqueueBySource=counters
  val prefetchDedupBySource=counters
  val prefetchEnterBySource=counters
  val prefetchPendingBySource=counters
  val bopPerfCountBySlice=Vec(slices,Vec(L2BOPPerf.names.size,UInt(64.W)))
  val bopPerfCountBySliceSource=Vec(slices,Vec(10,Vec(L2BOPPerf.names.size,UInt(64.W))))
}
class StressTopBase(val l2cfg: L2Configuration)(implicit val p: Parameters) extends Module with HasL2Params {
  val module=Module(new L2TopPhase2Harness(l2cfg))
  val io=IO(module.io.cloneType); io<>module.io
  val holdIssue=Some(IO(Input(Bool())))
  module.holdIssue:=holdIssue.get
  val observation=IO(Output(new StressAccounting(l2cfg.sliceNum)))
  val slices=module.slices
  val sliceTshrs=slices.map(_.tshrs)
  val pfQueues=module.pfQueues
  val pfQueueOverwriteBySource=module.pfQueueOverwriteBySource
  val moduleIds=module.getClass.getMethods.find(_.getName=="getIds").get.invoke(module)
    .asInstanceOf[scala.collection.Seq[AnyRef]]
  val productionPfQueues=moduleIds.collect {case q: OverwriteQueue[_]=>q}
  require(productionPfQueues.size==l2cfg.sliceNum*module.pfSourceSlots.size+module.recvFallback.size)
  def boreRead[T <: Data](d:T):T=BoringUtils.bore(d)
  def privateField[T](owner:AnyRef,name:String):T={
    val f=owner.getClass.getDeclaredField(name);f.setAccessible(true);f.get(owner).asInstanceOf[T]
  }
  def privateBool(owner:AnyRef,name:String):Bool=privateField[Bool](owner,name)
  def accumulate(cond:Bool):UInt={val c=RegInit(0.U(64.W));when(cond){c:=c+1.U};c}
  XSLog.collect(0.U(64.W), true.B, false.B, false.B)
  val obs=observation
  slices.zipWithIndex.foreach { case (slice, s) =>
    val pulses = boreRead(slice.bopPerf)
    L2BOPPerf.names.indices.foreach { i =>
      val count = RegInit(0.U(64.W))
      count := count + pulses(i)
      obs.bopPerfCountBySlice(s)(i) := count
    }
    val sourcePulses = sliceTshrs(s)
    (0 to L2PfSource.CDP).foreach { source =>
      L2BOPPerf.names.indices.foreach { metric =>
        val count = RegInit(0.U(64.W))
        val pulse = sourcePulses.map { tshr =>
          val perf = boreRead(tshr.io.bopPerf(metric))
          val pfSource = boreRead(tshr.io.bopPerfSource(metric))
          perf && pfSource === source.U
        }.reduce(_ || _)
        // Count all matching TSHRs, including simultaneous completions.
        val increment = PopCount(sourcePulses.map { tshr =>
          val perf = boreRead(tshr.io.bopPerf(metric))
          val pfSource = boreRead(tshr.io.bopPerfSource(metric))
          perf && pfSource === source.U
        })
        count := count + increment
        obs.bopPerfCountBySliceSource(s)(source)(metric) := count
      }
    }
  }


  slices.indices.foreach {i=>
    val sourceQueueStart=i*module.pfSourceSlots.size
    (0 to L2PfSource.CDP).foreach { source =>
      val fallback = module.recvFallback.lift(i)
      val fallbackEnq = fallback.map(q => boreRead(q.io.enq.valid) &&
        L2PrefetchTxnID.decode(boreRead(q.io.enq.bits.TxnID)) === source.U).getOrElse(false.B)
      val fallbackTake = fallback.map(q => boreRead(q.io.deq.valid) && boreRead(q.io.deq.ready) &&
        L2PrefetchTxnID.decode(boreRead(q.io.deq.bits.TxnID)) === source.U).getOrElse(false.B)
      val fallbackOverwrite = fallback.map(q => boreRead(q.io.overwrite) &&
        L2PrefetchTxnID.decode(boreRead(q.io.deq.bits.TxnID)) === source.U).getOrElse(false.B)
      val recvAccepted = module.pfRecvReqs.lift(i).map(q => boreRead(q.valid) &&
        L2PrefetchTxnID.decode(boreRead(q.bits.TxnID)) === source.U).getOrElse(false.B)
      obs.recvAcceptedBySource(i)(source) := accumulate(recvAccepted)
      val sliceReq = slices(i).io.UpRXREQ
      obs.prefetchSliceEnteredBySource(i)(source) := accumulate(
        boreRead(sliceReq.valid) && boreRead(sliceReq.ready) &&
          boreRead(sliceReq.bits.SrcID) === module.config.pfSrcID.U &&
          L2PrefetchTxnID.decode(boreRead(sliceReq.bits.TxnID)) === source.U)
      val queueIdx = Seq(L2PfSource.SMS, L2PfSource.BOP, L2PfSource.PBOP,
        L2PfSource.Stream, L2PfSource.Stride, L2PfSource.TP, L2PfSource.Berti,
        L2PfSource.NL, L2PfSource.CDP).indexOf(source)
      val count = RegInit(0.U(64.W))
      val pulse = if (queueIdx >= 0) boreRead(pfQueueOverwriteBySource(i)(queueIdx)) else false.B
      count := count + pulse + fallbackOverwrite
      obs.prefetchQueueOverwriteBySource(i)(source) := count
      val accepted = pfQueues.lift(i).map(q => boreRead(q.valid) && boreRead(q.ready) &&
        L2PrefetchTxnID.decode(boreRead(q.bits.TxnID)) === source.U).getOrElse(false.B)
      val sourceQueue = if (queueIdx >= 0) productionPfQueues.lift(sourceQueueStart + queueIdx) else None
      val enqueued = sourceQueue.map(q => boreRead(q.io.enq.valid) && boreRead(q.io.enq.ready)).getOrElse(false.B)
      val dequeued = sourceQueue.map(q => boreRead(q.io.deq.valid) && boreRead(q.io.deq.ready)).getOrElse(false.B)
      obs.prefetchEnqueueBySource(i)(source) := accumulate(enqueued) + accumulate(fallbackEnq)
      obs.prefetchDedupBySource(i)(source) := accumulate(dequeued && !accepted)
      obs.prefetchEnterBySource(i)(source) := accumulate(accepted) + accumulate(fallbackTake)
      // Read the real FIFO pointers/full flag independently of event totals.
      val sourcePending = sourceQueue.map { q =>
        val head = boreRead(privateField[UInt](q, "head"))
        val tail = boreRead(privateField[UInt](q, "tail"))
        val full = boreRead(privateBool(q, "full"))
        val entries = if (source == L2PfSource.BOP || source == L2PfSource.PBOP)
          module.config.pfBopQueueEntries else module.config.pfRecvQueueEntries
        val distance = Wire(UInt(head.getWidth.W))
        distance := tail - head
        Mux(full, entries.U, distance)
      }.getOrElse(0.U)
      val fallbackPending = fallback.map { q =>
        val head = boreRead(q.head)
        val tail = boreRead(q.tail)
        val full = boreRead(q.full)
        val data = boreRead(q.data)
        PopCount(data.zipWithIndex.map { case (entry, idx) =>
          val occupied = full || Mux(head <= tail, idx.U >= head && idx.U < tail,
            idx.U >= head || idx.U < tail)
          occupied && L2PrefetchTxnID.decode(entry.TxnID) === source.U
        })
      }.getOrElse(0.U)
      obs.prefetchPendingBySource(i)(source) := sourcePending +& fallbackPending
    }
  }
}

class StressTopEvents(cfg: L2Configuration)(implicit p: Parameters) extends StressTopBase(cfg) {
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
    val b = module.bop.get.io.req
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
