// Intent (audit-fix-1): inspect emitted SystemVerilog across independent
// parameter combinations, including fully disabled CDP storage and arbitration.
package oceanus

// Intent (3a): run these real cache/translation/useful checks with OceanusVBOP;
// select badScore=0 to allow the initial reference +2 prediction immediately,
// and explicitly opt into same-page bypass only for the existing fast-path case.

// Intent (phase2): use real demand/refill/Meta paths with a fixed-page TLB model.
// Check addresses, provenance/depth, useful attribution, disabled controls and
// every source queue against a host FIFO; test four-way contention separately
// with controlled producer timing, never force directory metadata.
// Intent: independently count each producer's output before source queue
// admission and each actual slice handshake, including receiver direct traffic.

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

class Phase2QueueProbe(implicit val p: Parameters) extends Bundle {
  val enq = Valid(new FlitREQ)
  val deq = Valid(new FlitREQ)
  val take = Bool()
  val overwrite = Bool()
  val head = UInt(5.W)
  val tail = UInt(5.W)
  val full = Bool()
  val enqPacked = UInt((new FlitREQ).getWidth.W)
  val deqPacked = UInt((new FlitREQ).getWidth.W)
}
class Phase2WriteProbe extends Bundle {
  val valid = Bool()
  val addr = UInt(48.W)
  val source = UInt(4.W)
  val depth = UInt(2.W)
}
class Phase2Observation(val slices: Int = 1, val tshrs: Int = 8, val bopEntries: Int = 16, val recvEntries: Int = 32, val queuesPerBank: Int = 10)(implicit val p: Parameters) extends Bundle {
  // Intent (3a): prove the downgrade counter from an actual ReadUnique event
  // and inspect the resulting CCHI opcode at the real slice admission.
  val needTDowngraded = UInt(64.W)
  val queues = Vec(slices * queuesPerBank, new Phase2QueueProbe)
  val enter = Vec(slices, Valid(new FlitREQ))
  val issue = Vec(slices, Valid(new FlitREQ))
  val write = Vec(slices * tshrs, new Phase2WriteProbe)
  val evictions = UInt(64.W)
  val evictPulses = UInt(log2Ceil(slices * tshrs + 1).W)
  val vbopOverflow = UInt(64.W)
  val cdpOverflow = UInt(64.W)
  val enterPacked = Vec(slices, UInt((new FlitREQ).getWidth.W))
  val issuePacked = Vec(slices, UInt((new FlitREQ).getWidth.W))
  val generated = Vec(10, UInt(log2Ceil(slices + 1).W))
  def capacity(i: Int): Int = if (Set(1, 2)(i % queuesPerBank)) bopEntries else recvEntries
}
object Phase2Observation {
  def attach(top: L2Top)(implicit p: Parameters): Phase2Observation = {
    def read[T <: Data](x: T): T = BoringUtils.bore(x)
    val obs = IO(Output(new Phase2Observation(top.config.sliceNum, top.nMSHR,
      top.config.pfBopQueueEntries, top.config.pfRecvQueueEntries,
      top.pfSourceSlots.size + (if(top.recvFallback.nonEmpty) 1 else 0))))
    val ids = top.getClass.getMethods.find(_.getName == "getIds").get.invoke(top)
      .asInstanceOf[scala.collection.Seq[AnyRef]]
    val allQueues = ids.collect { case q: OverwriteQueue[_] => q.asInstanceOf[OverwriteQueue[FlitREQ]] }
    val sourceCount = top.pfSourceSlots.size
    require(sourceCount == 9 && (top.recvFallback.isEmpty || top.recvFallback.size == top.config.sliceNum))
    require(allQueues.size == top.config.sliceNum * sourceCount + top.recvFallback.size)
    val sourceQueues = allQueues.filterNot(q => top.recvFallback.exists(_ eq q))
    require(sourceQueues.size == top.config.sliceNum * sourceCount)
    // Each bank exposes all source queues followed by its own Recv fallback.
    val queues = (0 until top.config.sliceNum).flatMap(bank =>
      sourceQueues.slice(bank * sourceCount, (bank + 1) * sourceCount) ++ top.recvFallback.lift(bank))
    queues.zip(obs.queues).foreach { case (q, out) =>
      out.enq.valid := read(q.io.enq.valid)
      out.enq.bits := read(q.io.enq.bits)
      out.deq.valid := read(q.io.deq.valid)
      out.deq.bits := read(q.io.deq.bits)
      out.take := read(q.io.deq.ready) && read(q.io.deq.valid)
      out.overwrite := read(q.io.overwrite)
      out.head := read(q.head); out.tail := read(q.tail); out.full := read(q.full)
      out.enqPacked := out.enq.bits.asUInt; out.deqPacked := out.deq.bits.asUInt
    }
    for ((slice, bank) <- top.slices.zipWithIndex) {
      obs.enter(bank).valid := read(slice.io.UpRXREQ.valid) && read(slice.io.UpRXREQ.ready)
      obs.enter(bank).bits := read(slice.io.UpRXREQ.bits)
      obs.issue(bank).valid := read(top.pfQueues(bank).valid) && read(top.pfQueues(bank).ready)
      obs.issue(bank).bits := read(top.pfQueues(bank).bits)
      obs.enterPacked(bank) := obs.enter(bank).bits.asUInt
      obs.issuePacked(bank) := obs.issue(bank).bits.asUInt
      slice.tshrs.zipWithIndex.foreach { case (t, i) =>
        val out = obs.write(bank * top.nMSHR + i)
        val req = t.vPipeREQ
        out.valid := read(req.io.tshr_meta_write_en.pfSource)
        out.addr := read(req.io.tshr_paddr)
        out.source := read(req.io.tshr_meta_write_meta.pfSource)
        out.depth := read(req.io.tshr_meta_write_meta.cdpPfDepth)
      }
    }
    obs.evictions := top.cdpEvents.map(e => read(e.evictions)).getOrElse(0.U)
    obs.evictPulses := PopCount(top.slices.flatMap(_.io.cdpEvict.map(e => read(e.valid))))
    obs.vbopOverflow := top.vbopStub.map(v => read(v.overwritten)).getOrElse(0.U)
    obs.needTDowngraded := top.vbopStub.map(v => read(v.needTDowngraded)).getOrElse(0.U)
    obs.cdpOverflow := top.cdpEvents.map(e => read(e.vpnCollisions) + read(e.filterCollisions)).getOrElse(0.U)
    obs.generated.foreach(_ := 0.U)
    obs.generated(1) := PopCount(top.pfRecvReqs.map(r => read(r.valid)))
    obs.generated(2) := top.vbopStub.map(v => read(v.io.req.valid) && read(v.io.req.ready)).getOrElse(false.B)
    obs.generated(3) := read(top.bop.io.req.valid) && read(top.bopControlEnable)
    obs.generated(9) := top.cdp.map(c => read(c.io.req.valid) && read(c.io.req.ready)).getOrElse(false.B)
    obs
  }
}

class TestTopPhase2(cfg: L2Configuration)(implicit p: Parameters) extends TestTop_L2Top(cfg) {
  val phase2 = Phase2Observation.attach(module)
}

object TestTop_L2PrefetchPhase2 extends App with ChiselSim {
  Constantin.init(false) // reference VBOP constants use their declared defaults
  require(L2TopPhase1aPorts.enabled)
  // Intent: exercise the production default (all predictions translate) as
  // well as the opt-in legacy same-page optimization through the real cache.
  val fastPath = !args.contains("--no-fast-path")
  val cfg = new L2Configuration(8, true, 0 until 1, bopEnable = true,
    pfRecvEnable = true, pfCtrlEnable = true, pfTrainEnable = true, pfTlbEnable = true,
    vbopStubEnable = true, cdpStubEnable = true,
    bopParams = OceanusBOPParameters(badScore = 0, roundMax = 100000),
    vbopParams = OceanusVBOPParameters(badScore = 0, roundMax = 100000, samePagePA = fastPath))
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new TestTopPhase2(cfg)(params)) { dut =>
    val requests = (0 until 200).map(i => UpRequestSpec(MasterId.t1p0, i, 0, 0,
      CCHIOpcode.ReadShared.opcode, BigInt(0), expCompData = true))
    val spec = CaseSpec("phase2", UpConfig(Seq(MasterId.t1p0),
      ExplicitRequestStream(requests, Seq.fill(requests.size)(BigInt(CHIFieldResp.CompData_SC))), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(25), FixedOutstanding(64), ImmediateCreditPolicy()), 30000, (0, 30000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec)
    val dn = new L2TopDownstreamModel(dut, spec)
    val dataQ = mutable.Queue.empty[PendingDnComp]
    val rspQ = mutable.Queue.empty[PendingDnRsp]
    val ackQ = mutable.Queue.empty[PendingUpAck]
    val snpQ = mutable.Queue.empty[PendingSnpRsp]
    val downstream = mutable.ArrayBuffer.empty[BigInt]
    val entered = mutable.ArrayBuffer.empty[(BigInt, Int, Int)]
    val writes = mutable.ArrayBuffer.empty[(BigInt, Int, Int)]
    val enteredByBank = Seq.fill(cfg.sliceNum)(mutable.ArrayBuffer.empty[Int])
    val pointers = mutable.Map.empty[BigInt, BigInt]
    // Literal fixed page table; no PA is derived from a DUT decoder.
    val pages = Seq(0x110 -> 0x51, 0x120 -> 0x52, 0x130 -> 0x53,
      0x210 -> 0x61, 0x220 -> 0x62, 0x230 -> 0x63, 0x310 -> 0x71,
      0x100 -> 0x20, 0x600 -> 0x74, 0x700 -> 0x46, 0x800 -> 0x90)
      .map { case (v, p) => BigInt(v) -> BigInt(p) }.toMap
    val misses = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    val faults = mutable.Set.empty[BigInt]
    val attempts = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    case class Translation(va: BigInt, due: Int, miss: Boolean, fault: Boolean)
    val replies = mutable.Queue.empty[Translation]
    val queueModels = Seq.fill(dut.phase2.queues.size)(mutable.Queue.empty[BigInt])
    val enqueues = Array.fill(dut.phase2.queues.size)(0); val takes = Array.fill(dut.phase2.queues.size)(0); val overwrites = Array.fill(dut.phase2.queues.size)(0)
    val drops = Array.fill(dut.phase2.queues.size)(0); val issued = Array.fill(dut.phase2.queues.size)(0)
    val generated = Array.fill(10)(0); val enteredBySource = Array.fill(10)(0)
    var evictions = BigInt(0)
    var cycle = 0; var txn = 0; var activeAddr = BigInt(0); var beats = 0
    def flag(x: Bool): Boolean = x.peek().litToBoolean
    def value(x: UInt): BigInt = x.peek().litValue
    def source(f: FlitREQ): Int = (value(f.TxnID) >> 4).toInt
    def depth(f: FlitREQ): Int = ((value(f.TxnID) >> 2) & 3).toInt
    def memory(addr: BigInt, beat: Int): BigInt = if (beat == 0) pointers.getOrElse(addr, BigInt(0)) else BigInt(0)
    def useful(src: Int): BigInt = (0 until cfg.sliceNum).map(b => value(dut.observation.bopPerfCountBySliceSource(b)(src)(L2BOPPerf.Useful))).sum
    def controls(v: Boolean = false, c: Boolean = false, b: Boolean = false, master: Boolean = true): Unit = {
      val ctrl = dut.io.pfCtrl.get
      ctrl.l2_pf_master_en.poke(master.B); ctrl.l2_pf_recv_en.poke(true.B)
      ctrl.l2_vbop_en.poke(v.B); ctrl.l2_cdp_en.poke(c.B); ctrl.l2_pbop_en.poke(b.B)
    }
    def tick(): Unit = {
      assert(cycle < 30000, "phase2 watchdog")
      up.clearResponses(); dn.clearResponses()
      ackQ.headOption.foreach { a => up.driveUpCompAck(a.upDBID, a.sliceIdx); ackQ.dequeue() }
      if (!flag(dut.io.t1p.head.UpRSP.valid)) snpQ.headOption.foreach { s => up.driveUpSnpResp(s.snpTxnID, s.tgtId); snpQ.dequeue() }
      dataQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
        dn.driveDnData(p)
        dut.io.chi.rxdat.flit.Data.get.poke(memory(p.addr, p.beat).U)
        dataQ.dequeue(); dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
      }
      rspQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
        dn.driveDnRsp(p); rspQ.dequeue(); dn.noteResponse(p.dnTxnID, true, cycle)
      }
      val tlb = dut.io.pfTlb.get
      tlb.req.ready.poke(true.B); tlb.resp.valid.poke(false.B)
      tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits)); tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      replies.headOption.filter(_.due <= cycle).foreach { r =>
        replies.dequeue(); tlb.resp.valid.poke(true.B)
        tlb.resp.bits.miss.poke(r.miss.B); tlb.resp.bits.excp.head.pf.ld.poke(r.fault.B)
        tlb.resp.bits.paddr.head.poke(((pages(r.va >> 12) << 12) | (r.va & 4095)).U)
      }
      if (flag(tlb.req.valid)) {
        val va = value(tlb.req.bits.vaddr)
        assert(pages.contains(va >> 12), s"unexpected TLB VA=$va cycle=$cycle")
        attempts(va) += 1
        replies.enqueue(Translation(va, cycle + 3, attempts(va) <= misses(va), faults(va)))
      }
      if (flag(dut.io.chi.txreq.flitv)) {
        val f = dut.io.chi.txreq.flit
        val id = DnTxnID(value(f.TxnID.get)); val src = value(f.SrcID.get)
        val op = value(f.Opcode.get); val addr = value(f.Addr.get)
        dn.noteRequest(id)
        if (dn.isEvictReq(op)) rspQ.enqueue(PendingDnRsp(id, src, cycle + 25, dn.downstreamEvictRspOpcode(op)))
        else { downstream += addr
          val resp = if(op == 0x07) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC
          (0 until 2).foreach(b => dataQ.enqueue(PendingDnComp(id, src, cycle + 25 + b, b, BigInt(resp), addr)))
        }
      }
      val port = dut.io.t1p.head
      if (flag(port.DnDAT.valid)) {
        val f = port.DnDAT.bits; val beat = value(f.DataID).toInt
        assert(value(f.Data) == memory(activeAddr, beat)); beats += 1
        if (beat == 1) ackQ.enqueue(PendingUpAck(0, UpDBID(value(f.DBID)), UpTxnID(value(f.TxnID)), cycle + 1, value(f.SrcID).toInt))
      }
      if (flag(port.DnSNP.valid)) snpQ.enqueue(PendingSnpRsp(SnpTxnID(value(port.DnSNP.bits.TxnID)), value(port.DnSNP.bits.SrcID), cycle + 1))
      assert(!flag(port.DnRSP.valid))
      val obs = dut.phase2
      for (bank <- 0 until cfg.sliceNum; (s, local) <- Seq(1 -> 9, 2 -> 1, 3 -> 2, 9 -> 8)) {
        val q = bank * 10 + local
        val o = dut.observation
        assert(value(o.prefetchEnqueueBySource(bank)(s)) == enqueues(q))
        assert(value(o.prefetchEnterBySource(bank)(s)) == issued(q))
        assert(value(o.prefetchQueueOverwriteBySource(bank)(s)) == overwrites(q))
        assert(value(o.prefetchDedupBySource(bank)(s)) == drops(q))
        assert(value(o.prefetchPendingBySource(bank)(s)) == queueModels(q).size)
      }
      obs.enter.zipWithIndex.foreach { case (e, bank) => if (flag(e.valid) && value(e.bits.SrcID) == cfg.pfSrcID) {
        if(source(e.bits) == 2) assert(value(e.bits.Opcode) == CCHIOpcode.StashShared.opcode)
        entered += ((value(e.bits.Addr), source(e.bits), depth(e.bits)))
        enteredBySource(source(e.bits)) += 1
        enteredByBank(bank) += source(e.bits)
      }}
      obs.generated.zipWithIndex.foreach { case (pulse, s) => generated(s) += value(pulse).toInt }
      obs.write.foreach(w => if (flag(w.valid)) writes += ((value(w.addr), value(w.source).toInt, value(w.depth).toInt)))
      assert(value(obs.evictions) == evictions); evictions += value(obs.evictPulses)
      assert(value(obs.vbopOverflow) == 0 && value(obs.cdpOverflow) == 0)
      obs.queues.zipWithIndex.foreach { case (q, i) =>
        val model = queueModels(i); val capacity = obs.capacity(i); val bank = i / 10
        val occupancy = if (flag(q.full)) capacity else (value(q.tail).toInt - value(q.head).toInt + capacity) % capacity
        assert(occupancy == model.size && flag(q.deq.valid) == model.nonEmpty, s"queue occupancy $i")
        if (model.nonEmpty) assert(value(q.deqPacked) == model.head, s"queue payload/order $i")
        val take = flag(q.take); val put = flag(q.enq.valid)
        val overwrite = put && model.size == capacity && !take
        assert(flag(q.overwrite) == overwrite)
        if (take) {
          val bits = model.dequeue(); takes(i) += 1
          val wasIssued = if (i % 10 == 9) flag(obs.enter(bank).valid) && value(obs.enterPacked(bank)) == bits
            else flag(obs.issue(bank).valid) && value(obs.issuePacked(bank)) == bits
          if (wasIssued) issued(i) += 1 else drops(i) += 1
        }
        if (overwrite) { model.dequeue(); overwrites(i) += 1 }
        if (put) { model.enqueue(value(q.enqPacked)); enqueues(i) += 1 }
        assert(enqueues(i) == overwrites(i) + issued(i) + drops(i) + model.size)
      }
      for (bank <- 0 until cfg.sliceNum; metric <- L2BOPPerf.names.indices) {
        val total = value(dut.observation.bopPerfCountBySlice(bank)(metric))
        val sum = (0 to 9).map(s => value(dut.observation.bopPerfCountBySliceSource(bank)(s)(metric))).sum
        assert(total == sum, s"effect accounting metric=$metric total=$total sum=$sum")
      }
      for ((s, q) <- Seq(1 -> 9, 2 -> 1, 3 -> 2, 9 -> 8)) {
        val inventories = (0 until cfg.sliceNum).map(b => b * 10 + q)
        assert(generated(s) == enteredBySource(s) + inventories.map(i => overwrites(i) + drops(i) + queueModels(i).size).sum,
          s"producer/slice conservation source=$s generated=${generated(s)} entered=${enteredBySource(s)}")
      }
      dn.stepWithCredits(cycle); cycle += 1
    }
    def drain(queued: Boolean = true): Unit = {
      var quiet = 0
      while (quiet < 30) {
        tick()
        val idle = dataQ.isEmpty && rspQ.isEmpty && ackQ.isEmpty && snpQ.isEmpty && replies.isEmpty &&
          dut.observation.dirProbeBySlice.flatten.forall(p => !flag(p.valid) && !flag(p.reqActive)) &&
          (!queued || queueModels.forall(_.isEmpty))
        quiet = if (idle) quiet + 1 else 0
      }
    }
    def request(pa: BigInt, va: BigInt, train: Boolean = true, unique: Boolean = false): Unit = {
      activeAddr = pa
      up.driveRead(requests(txn).copy(addr = pa,
        opcode = if(unique) CCHIOpcode.ReadUnique.opcode else CCHIOpcode.ReadShared.opcode))
      dut.io.pfTrain.get.head.vaddrBlock.poke((va >> 6).U)
      dut.io.pfTrain.get.head.trainEnable.poke(train.B)
      while (!flag(dut.io.t1p.head.UpREQ.ready)) tick()
      tick(); up.clearUpReq(); txn += 1
      drain(queued = !flag(dut.holdIssue.get))
    }
    def recv(addr: BigInt): Unit = {
      dut.io.pfRecv.get.addr.poke(addr.U); dut.io.pfRecv.get.addr_valid.poke(true.B)
      tick(); dut.io.pfRecv.get.addr_valid.poke(false.B)
    }
    up.initInputs(); dn.initInputs(); dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
    dut.holdIssue.get.poke(false.B)
    dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get)); dut.io.pfRecv.get.pf_en.poke(true.B)
    dut.io.pfRecv.get.pf_source.poke(10.U) // SMS/source1 in the existing mapping
    dut.io.pfCtrl.get.poke(0.U.asTypeOf(dut.io.pfCtrl.get)); controls()
    dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B); dn.activateRxLink()
    for (w <- -300 until 0) dn.stepWithCredits(w)

    controls(v = true)
    request(0x20040, 0x100040)
    assert((if(fastPath) attempts.isEmpty else attempts(BigInt(0x1000c0)) == 1) && entered.contains((BigInt(0x200c0), 2, 0)) && writes.contains((BigInt(0x200c0), 2, 0)))
    println(s"PHASE2_VBOP_SAME_PAGE pass=true tlb=${if(fastPath) 0 else 1} PA=200c0 source=2 fastPath=$fastPath")
    controls(); val beforeUsefulV = useful(2); val beforeDn = downstream.size
    request(0x200c0, 0x1000c0, train = false)
    assert(useful(2) == beforeUsefulV + 1 && downstream.size == beforeDn)
    println("PHASE2_VBOP_USEFUL pass=true source=2 delta=1")
    controls(v = true)
    request(0x30fc0, 0x10ffc0)
    assert(attempts(BigInt(0x110040)) == 1 && entered.contains((BigInt(0x51040), 2, 0)) && writes.contains((BigInt(0x51040), 2, 0)))
    println("PHASE2_VBOP_CROSS_PAGE pass=true PA=51040 attempts=1")
    misses(BigInt(0x120040)) = 1
    request(0x32fc0, 0x11ffc0)
    assert(attempts(BigInt(0x120040)) == 2 && entered.contains((BigInt(0x52040), 2, 0)))
    println("PHASE2_VBOP_RETRY pass=true attempts=2 PA=52040")
    faults += BigInt(0x130040)
    request(0x34fc0, 0x12ffc0)
    assert(attempts(BigInt(0x130040)) == 1 && !entered.exists(_._1 == BigInt(0x53040)))
    println("PHASE2_VBOP_FAULT pass=true emitted=0")

    controls(c = true)
    // C5: actual demand training reaches the reference's 128-event refresh,
    // establishing a hot 2MiB region before pointer detection. No table force.
    for (_ <- 0 until 130) request(0x3e040, 0x200040)
    pointers(BigInt(0x40040)) = BigInt(0x210080)
    pointers(BigInt(0x61080)) = BigInt(0x2200c0)
    pointers(BigInt(0x620c0)) = BigInt(0x230100)
    val vbopBeforeCDP = entered.count(_._2 == 2)
    request(0x40040, 0x200040)
    assert(entered.count(_._2 == 2) == vbopBeforeCDP)
    for (pa <- Seq(0x61080, 0x610c0, 0x61100))
      assert(entered.contains((BigInt(pa), 9, 3)) && writes.contains((BigInt(pa), 9, 3)))
    assert(attempts(BigInt(0x210080)) == 1 && attempts(BigInt(0x2200c0)) == 0 && attempts(BigInt(0x230100)) == 0)
    println("PHASE2_CDP_CHAIN pass=true PA=61080,610c0,61100 source=9 depth=3 degree=3 refillAtMaxStops=true hotTraining=130")
    controls(); val beforeUsefulC = useful(9); val beforeHitDn = downstream.size
    request(0x61080, 0x210080, train = false); request(0x610c0, 0x2100c0, train = false)
    assert(useful(9) == beforeUsefulC + 2 && downstream.size == beforeHitDn)
    println("PHASE2_CDP_USEFUL pass=true source=9 delta=2")
    val startDisabled = entered.size; val startTlb = attempts.values.sum
    pointers(BigInt(0x42040)) = BigInt(0x310080)
    request(0x42040, 0x300040)
    assert(entered.size == startDisabled && attempts.values.sum == startTlb)
    controls(v = true, c = true, master = false)
    request(0x44040, 0x400040)
    assert(entered.size == startDisabled && attempts.values.sum == startTlb)
    controls(v = true)
    pointers(BigInt(0x46040)) = BigInt(0x310080)
    val cdpBeforeOff = entered.count(_._2 == 9)
    request(0x46040, 0x700040)
    assert(entered.count(_._2 == 9) == cdpBeforeOff && attempts.values.sum == startTlb + (if(fastPath) 0 else 1))
    println("PHASE2_CONTROLS pass=true vbopOff=0 cdpOff=0 masterOff=0")

    // Accumulate each real producer behind the test-only issue pause. PBOP's
    // target is seeded separately to avoid the fixed-offset VBOP duplicate.
    dut.holdIssue.get.poke(true.B)
    controls(b = true); request(0x70040, 0x500040)
    assert((0 until cfg.sliceNum).exists(b => queueModels(b*10+2).nonEmpty))
    controls(v = true, c = true)
    pointers(BigInt(0x74040)) = BigInt(0x310080)
    request(0x74040, 0x600040)
    assert(Seq(1,8).forall(q => (0 until cfg.sliceNum).exists(b => queueModels(b*10+q).nonEmpty)))
    controls(v = true, c = true, b = true)
    // A real demand wins this cycle, forcing the simultaneous receiver into
    // its fallback FIFO. Release all four nonempty producer queues together.
    activeAddr = BigInt(0x7c040)
    up.driveRead(requests(txn).copy(addr = activeAddr))
    dut.io.pfTrain.get.head.trainEnable.poke(false.B)
    while (!flag(dut.io.t1p.head.UpREQ.ready)) tick()
    recv(0x78040); up.clearUpReq(); txn += 1
    drain(queued = false)
    assert((0 until cfg.sliceNum).exists(b => queueModels(b*10+9).nonEmpty))
    val arbStart = entered.size
    val bankStarts = enteredByBank.map(_.size)
    val bankExpected = (0 until cfg.sliceNum).map(b => Seq(1 -> 9, 2 -> 1, 3 -> 2, 9 -> 8).flatMap { case (src, q) => Seq.fill(queueModels(b*10+q).size)(src) })
    dut.holdIssue.get.poke(false.B); drain()
    val order = entered.drop(arbStart).map(_._2).toSeq
    assert(order.sorted == Seq(1, 2, 3, 9, 9, 9).sorted, s"four source completion inventory $order")
    for (bank <- 0 until cfg.sliceNum) assert(enteredByBank(bank).drop(bankStarts(bank)).toSeq == bankExpected(bank), s"four source priority bank=$bank actual=${enteredByBank(bank).drop(bankStarts(bank))} expected=${bankExpected(bank)}")
    println("PHASE2_ARBITRATION pass=true order=Recv,VBOP,PBOP,CDP sources=1,2,3,9,9,9")
    assert(queueModels.forall(_.isEmpty))
    controls(v = true)
    val beforeDowngrade = value(dut.phase2.needTDowngraded)
    request(0x90040, 0x800040, unique = true)
    assert(entered.contains((BigInt(0x900c0), 2, 0)))
    assert(value(dut.phase2.needTDowngraded) == beforeDowngrade + 1)
    println("PHASE3A_NEEDT pass=true training=ReadUnique emitted=StashShared downgraded=1 deviation=D2")
    for (local <- Seq(9, 1, 2, 8)) {
      val indices = (0 until cfg.sliceNum).map(b => b*10+local)
      assert(indices.map(enqueues(_)).sum > 0 && indices.map(issued(_)).sum > 0)
      println(s"PHASE2_SOURCE_ACCOUNT pass=true source=${if(local==9) 1 else local+1} enqueued=${indices.map(enqueues(_)).sum} overwritten=${indices.map(overwrites(_)).sum} issued=${indices.map(issued(_)).sum} dedup=${indices.map(drops(_)).sum} pending=${indices.map(i=>queueModels(i).size).sum}")
    }
    controls()
    val evictionBase = BigInt(0x80000)
    val evictionBank = L2TopBOPExpectations.slice(evictionBase, cfg.sliceNum)
    val evictionAddresses = Iterator.from(0).map(i => evictionBase + BigInt(i) * dut.p(L2ParamsKey).sets * 64).filter(a => L2TopBOPExpectations.slice(a,cfg.sliceNum) == evictionBank).take(dut.p(L2ParamsKey).ways + 6).toSeq
    evictionAddresses.foreach { a => recv(a); drain() }
    assert(value(dut.phase2.evictions) == evictions)
    assert(evictions >= 6)
    println(s"PHASE2_EVICT_COUNT pass=true events=$evictions requests=0")
    assert(generated.sum == enteredBySource.sum && queueModels.forall(_.isEmpty))
    println(s"PHASE2_RESULT pass=true demands=$txn beats=$beats translations=${attempts.values.sum} evictions=$evictions conservation=true")
  }
}

// Intent: elaborate each independent parameter combination in a fresh JVM,
// checking generated modules directly, including stub-only configurations.
object TestTop_L2StubParameters extends App {
  Constantin.init(false)
  val mode = args.head
  val cfg = new L2Configuration(8, true, 0 until 2,
    pfTrainEnable = mode != "off" && mode != "tlb-only",
    pfTlbEnable = mode != "off" && mode != "train-only",
    vbopStubEnable = mode == "vbop" || mode == "both", cdpStubEnable = mode == "cdp" || mode == "both")
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  // SRAMTemplate caches definitions per JVM: elaborate once and inspect
  // the lowered RTL directly, avoiding a second incompatible definition cache.
  // Save inspectable RTL on NVMe and check storage/instances after lowering.
  val sv = _root_.circt.stage.ChiselStage.emitSystemVerilog(new L2Top(cfg)(params),
    firtoolOpts = Array("--disable-all-randomization", "--strip-debug-info"))
  val output = java.nio.file.Paths.get(sys.props.getOrElse("oceanus.parameters.dir", "/data/zhuy/pf-framework/audit-fix-1/rtl"))
  java.nio.file.Files.createDirectories(output)
  java.nio.file.Files.writeString(output.resolve(s"parameters-$mode.sv"), sv)
  assert(sv.contains("module OceanusVBOP") == cfg.vbopStubEnable)
  assert(sv.contains("module OceanusCDP(") == cfg.cdpStubEnable)
  assert(!sv.contains("module L2CDPStub"))
  if (cfg.cdpObserveEnable) {
    assert(sv.contains("L2CDPHalfQueue") && sv.contains("cdpDataArb") && sv.contains("cdpPfDepth"))
    assert(sv.contains("cdpContext_paddr") && sv.contains("cdpBufferValid"))
  } else {
    assert(!sv.contains("L2CDPHalfQueue") && !sv.contains("cdpDataArb"))
    assert(!sv.contains("cdpPfDepth"), "disabled Meta depth storage survives")
    for (name <- Seq("cdpPending", "cdpContext", "cdpBufferValid", "cdpCollisionDrops", "trainDisabled")) {
      val register = ("(?m)^\\s*(?:reg|logic)\\b[^;]*\\b" + name + "(?:_\\w+)?\\b[^;]*;").r
      assert(register.findFirstIn(sv).isEmpty, s"disabled register survives: $name")
    }
  }
  println(s"CDP_RTL_PARAMETERS pass=true mode=$mode observe=${cfg.cdpObserveEnable} tlb=${cfg.pfTlbEnable} train=${cfg.pfTrainEnable} rtl=${output.resolve(s"parameters-$mode.sv")}")
  println(s"PHASE2_PARAMETERS pass=true mode=$mode vbop=${cfg.vbopStubEnable} cdp=${cfg.cdpStubEnable} slices=2")
}
