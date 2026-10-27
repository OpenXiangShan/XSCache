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

object CDPC5Params {
  def apply = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
}

class TestTopCDPC5(cfg: L2Configuration)(implicit p: Parameters) extends TestTop_L2Top(cfg) {
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
    for (i <- 0 until 8) {
      val t = slice.io.cdpTrain(i); val e = slice.io.cdpEvict(i)
      event(s"train$bank.$i", t.valid, "bits" -> t.bits)
      event(s"evict$bank.$i", e.valid, "pa" -> e.bits.paddr, "source" -> e.bits.pfSource, "depth" -> e.bits.pfDepth)
    }
  }
  add("vpnCollisions", module.cdpEvents.get.vpnCollisions)
  add("filterCollisions", module.cdpEvents.get.filterCollisions)
  // H2 lifecycle observations; no production or reference algorithm changes.
  add("h2.index", s.freeEntryIdx)
  add("h2.valid", read(s.valids).asUInt)
  add("h2.pa", VecInit(entries.map(_.paddrValid)).asUInt)
  add("h2.inflight", read(s.reqInflight).asUInt)
  add("h2.eligible", VecInit((0 until 16).map(i => read(s.pftArb.io.in(i).valid))).asUInt)
  event("h2.query", fire(s.io.ftQueryReq), "index" -> s.pftArb.io.chosen,
    "va" -> Cat(entries(read(s.pftArb.io.chosen)).vTag, 0.U(6.W)),
    "pa" -> read(s.pftArb.io.out.bits).paddr)
  event("h2.query2", s.pftS2, "index" -> s.idx2, "canPft" -> s.canPft)
  fields += "h2.queue" -> 0.U(5.W) // Removed queue: keep legacy trace width, without boring a literal.
  add("resident", PopCount(read(s.valids)))
  add("candidatePending", c.c4Input.valid)
  val boundary = IO(Output(UInt(fields.map(_._2.getWidth).sum.W)))
  boundary := Cat(fields.map(_._2).reverse.toSeq)
  val widths = fields.map { case (n, v) => (n, v.getWidth) }.toVector
}

object TestTop_OceanusCDPC5 extends App with ChiselSim {
  Constantin.init(false)
  def opt(n: String, default: Int): Int = args.find(_.startsWith(n + "=")).map(_.split("=", 2)(1).toInt).getOrElse(default)
  val seeds = opt("--seeds", 3); val slices = opt("--slices", 1)
  val overlap = args.contains("--overlap")
  val cfg = new L2Configuration(8, true, 0 until slices, bopEnable = false,
    pfRecvEnable = true, pfCtrlEnable = true, pfTrainEnable = true, pfTlbEnable = true,
    vbopStubEnable = true, cdpStubEnable = true,
    vbopParams = OceanusVBOPParameters(badScore = 0, roundMax = 100000))
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new TestTopCDPC5(cfg)(CDPC5Params.apply)) { dut =>
    val directory = Paths.get(sys.props("oceanus.c5.trace")); Files.createDirectories(directory)
    val tracePath = directory.resolve(s"system-$slices-$seeds.jsonl.gz")
    val trace = new PrintWriter(new GZIPOutputStream(new FileOutputStream(tracePath.toFile)))
    trace.println(ujson.Obj("head" -> sys.props("oceanus.c5.head"), "slices" -> slices,
      "seeds" -> seeds, "widths" -> ujson.Arr.from(dut.widths.map { case (n, w) => ujson.Arr(n, w) })).render())
    val dummy = UpRequestSpec(MasterId.t1p0, 0, 0, 0, CCHIOpcode.ReadShared.opcode, BigInt(0), expCompData = true)
    val spec = CaseSpec("cdp-c5", UpConfig(Seq(MasterId.t1p0), ExplicitRequestStream(Seq(dummy), Seq(BigInt(CHIFieldResp.CompData_SC))), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(80), FixedOutstanding(64), ImmediateCreditPolicy()), 2000000, (0, 2000000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec); val dn = new L2TopDownstreamModel(dut, spec)
    val data = mutable.Queue.empty[PendingDnComp]; val rsp = mutable.Queue.empty[PendingDnRsp]
    val ack = mutable.Queue.empty[PendingUpAck]; val snoop = mutable.Queue.empty[PendingSnpRsp]
    val replies = mutable.Queue.empty[(Int, BigInt, Boolean, Boolean)]
    val attempts = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    val writes = mutable.ArrayBuffer.empty[(BigInt, Int, Int)]
    val emitted = mutable.ArrayBuffer.empty[(BigInt, Int)]
    val admitted = mutable.ArrayBuffer.empty[(BigInt, Int)]
    val useful = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    val seenDownstream = mutable.Set.empty[BigInt]
    var cycle = 0; var txn = 0; var epoch = -1; var beats = 0
    var gate = true; var activePA = BigInt(0); var reset = false
    var allCandidates = 0; var allDrops = 0; var hits = 0; var unused = 0; var contention = 0
    var activeSeed = 0
    def flag(b: Bool) = b.peek().litToBoolean
    def value(b: UInt) = b.peek().litValue
    def pa(va: BigInt): BigInt = if ((va >> 12) >= 0x300 && (va >> 12) < 0x340) va + 0x300000
      else if ((va >> 12) == 0x200) va - 0x1c0000 else va + 0x1000000
    // Byte-exact independent memory image. Every seed has eight pointers;
    // both halves/four lanes participate, [1:0]=0 and bit2=1 is intentional.
    def memory(addr: BigInt, beat: Int): BigInt = {
      val index = ((addr - 0x40000) >> 6).toInt
      if(index >= 0 && index < seeds + 2) {
        val random = new scala.util.Random(0xC50000L + index)
        val page = 0x300 + random.nextInt(32)
        val words = Vector.fill(8)(BigInt(page * 4096 + random.nextInt(60) * 64 + 4))
        words.slice(beat * 4, beat * 4 + 4).zipWithIndex.map { case (v, i) => v << (i * 64) }.sum
      } else BigInt(0)
    }
    def controls(c: Boolean = true, v: Boolean = false): Unit = {
      val ctrl = dut.io.pfCtrl.get
      ctrl.l2_pf_master_en.poke(true); ctrl.l2_pf_recv_en.poke(true)
      ctrl.l2_cdp_en.poke(c); ctrl.l2_vbop_en.poke(v); ctrl.l2_pbop_en.poke(false)
    }
    def tick(): Map[String, BigInt] = {
      up.clearResponses(); dn.clearResponses()
      ack.headOption.foreach { a => up.driveUpCompAck(a.upDBID, a.sliceIdx); ack.dequeue() }
      if(!flag(dut.io.t1p.head.UpRSP.valid)) snoop.headOption.foreach { a => up.driveUpSnpResp(a.snpTxnID, a.tgtId); snoop.dequeue() }
      data.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
        dn.driveDnData(p); dut.io.chi.rxdat.flit.Data.get.poke(memory(p.addr, p.beat)); data.dequeue()
        dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
      }
      rsp.headOption.filter(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
        dn.driveDnRsp(p); rsp.dequeue(); dn.noteResponse(p.dnTxnID, true, cycle)
      }
      val tlb = dut.io.pfTlb.get
      tlb.req.ready.poke(gate); tlb.resp.valid.poke(false)
      tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits)); tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      replies.headOption.filter(_._1 <= cycle).foreach { case (_, va, miss, fault) =>
        replies.dequeue(); tlb.resp.valid.poke(true); tlb.resp.bits.paddr.head.poke(pa(va))
        tlb.resp.bits.miss.poke(miss); tlb.resp.bits.excp.head.pf.ld.poke(fault)
      }
      if(gate && flag(tlb.req.valid)) {
        val va = value(tlb.req.bits.vaddr); attempts(va >> 12) += 1
        val kind = if(epoch >= 0) activeSeed % 7 else 0
        val miss = kind == 1 && attempts(va >> 12) % 2 == 1 || kind == 2
        val fault = kind == 3
        replies.enqueue((cycle + 1 + activeSeed % 7, va, miss, fault))
      }
      if(flag(dut.io.chi.txreq.flitv)) {
        val f = dut.io.chi.txreq.flit
        val id = DnTxnID(value(f.TxnID.get)); val src = value(f.SrcID.get)
        val op = value(f.Opcode.get); val addr = value(f.Addr.get); dn.noteRequest(id)
        if(dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id, src, cycle + 80, dn.downstreamEvictRspOpcode(op)))
        else {
          seenDownstream += addr
          val response = if(op == 0x07) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC
          for(b <- 0 until 2) data.enqueue(PendingDnComp(id, src, cycle + 80 + b, b, BigInt(response), addr))
        }
      }
      val port = dut.io.t1p.head
      if(flag(port.DnDAT.valid)) {
        val f = port.DnDAT.bits; val beat = value(f.DataID).toInt
        assert(value(f.Data) == memory(activePA, beat), s"cache data mismatch PA=$activePA beat=$beat")
        beats += 1
        if(beat == 1) ack.enqueue(PendingUpAck(0, UpDBID(value(f.DBID)), UpTxnID(value(f.TxnID)), cycle + 1, value(f.SrcID).toInt))
      }
      if(flag(port.DnSNP.valid)) snoop.enqueue(PendingSnpRsp(SnpTxnID(value(port.DnSNP.bits.TxnID)), value(port.DnSNP.bits.SrcID), cycle + 1))
      assert(!flag(port.DnRSP.valid))
      val packed = value(dut.boundary)
      var rest = packed
      val row = dut.widths.map { case (n, w) => val v = rest & ((BigInt(1) << w) - 1); rest >>= w; n -> v }.toMap
      assert(rest == 0)
      trace.println(ujson.Arr(cycle, epoch, gate, packed.toString(16)).render())
      if(row("candidate") != 0) allCandidates += 1
      allDrops += row("full").toInt + row("duplicate").toInt + row("filterDrop").toInt + (0 until 16).map(i => row(s"translationDrop$i").toInt).sum
      contention += row("vbopBlockedCDP").toInt
      if(row("out") != 0) {
        assert(row("enable") != 0, "CSR disabled CDP emitted")
        emitted += ((row("out.pa"), row("out.depth").toInt))
      }
      for(bank <- 0 until slices) {
        if(row(s"enter$bank") != 0 && (row(s"enter$bank.txn") >> 4) == 9) {
          val addr = row(s"enter$bank.pa"); val depth = ((row(s"enter$bank.txn") >> 2) & 3).toInt
          assert(emitted.contains((addr, depth)), "source9/depth TxnID without CDP producer")
          admitted += ((addr, depth))
        }
        for(i <- 0 until 8) {
          if(row(s"evict$bank.$i") != 0 && row(s"evict$bank.$i.source") == 9) {
            assert(emitted.exists(_._1 == row(s"evict$bank.$i.pa")), "unused victim was never issued by CDP")
            assert(useful(row(s"evict$bank.$i.pa")) == 0, "used CDP line reported unused")
            unused += 1
          }
        }
      }
      if(row("filter") != 0 && row("filter.used") != 0 && row("filter.source") == 9) {
        useful(row("filter.pa")) += 1; hits += 1
      }
      dn.stepWithCredits(cycle); cycle += 1
      assert(cycle < 2000000, "C5 watchdog")
      row
    }
    def run(n: Int): Unit = for(_ <- 0 until n) tick()
    def drain(): Unit = {
      var quiet = 0
      while(quiet < 35) {
        val row = tick()
        val idle = data.isEmpty && rsp.isEmpty && ack.isEmpty && snoop.isEmpty && replies.isEmpty &&
          row("resident") == 0 && row("candidatePending") == 0 &&
          dut.observation.dirProbeBySlice.flatten.forall(p => !flag(p.valid) && !flag(p.reqActive))
        quiet = if(idle) quiet + 1 else 0
      }
    }
    def request(addr: BigInt, va: BigInt, enable: Boolean = true, opcode: Int = CCHIOpcode.ReadShared.opcode): Unit = {
      activePA = addr
      up.driveRead(dummy.copy(txnId = txn % 256, addr = addr, opcode = opcode))
      dut.io.pfTrain.get.head.vaddrBlock.poke(va >> 6)
      dut.io.pfTrain.get.head.trainEnable.poke(enable)
      while(!flag(dut.io.t1p.head.UpREQ.ready)) tick()
      tick(); up.clearUpReq(); txn += 1
    }
    up.initInputs(); dn.initInputs(); dut.log.dump.poke(false); dut.log.clean.poke(false)
    dut.holdIssue.get.poke(false)
    dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get))
    dut.io.pfCtrl.get.poke(0.U.asTypeOf(dut.io.pfCtrl.get)); controls()
    dut.io.pfTrain.get.foreach(_.poke(0.U.asTypeOf(dut.io.pfTrain.get.head)))
    dut.io.pfTlb.get.req.ready.poke(true); dut.io.pfTlb.get.resp.valid.poke(false)
    dut.io.pfTlb.get.resp.bits.poke(0.U.asTypeOf(dut.io.pfTlb.get.resp.bits))
    dut.io.pfTlb.get.pmp_resp.poke(0.U.asTypeOf(dut.io.pfTlb.get.pmp_resp))
    dut.reset.poke(true); dut.clock.step(8); dut.reset.poke(false); dn.activateRxLink()
    for(w <- -300 until 0) dn.stepWithCredits(w)
    for(_ <- 0 until 130) { request(0x3e040, 0x200040); drain() }
    for(seed <- 0 until seeds) {
      epoch = seed; activeSeed = seed; attempts.clear()
      val before = allCandidates; val outBefore = emitted.size
      gate = false
      request(BigInt(0x40000 + seed * 64), BigInt(0x200000 + seed * 64))
      // Both real refill halves must arrive before releasing translation.
      run(155); gate = true; drain()
      assert(allCandidates > before, s"seed $seed detection was empty")
      if(seed < 2 && emitted.size > outBefore) {
        val (addr, _) = emitted(outBefore)
        assert(admitted.exists(_._1 == addr) && seenDownstream(addr))
        // Actual directory hit/old source -> used feedback, with CSR enabled.
        request(addr, addr - 0x300000); drain()
        assert(useful(addr) == 1, s"missing actual used feedback PA=$addr")
      }
      trace.flush()
      if(seed % 50 == 0) println(s"CDP_C5_PROGRESS slices=$slices seed=$seed cycles=$cycle candidates=$allCandidates outputs=${emitted.size}")
    }
    // Preserve an unused genuine CDP line, then evict it with actual same-set
    // demand replacements. The victim PA must come from the old host Meta.
    epoch = seeds; activeSeed = 0; attempts.clear()
    val unusedTarget = emitted.map(_._1).find(a => useful(a) == 0).get
    def bank(addr: BigInt): Int = if(slices == 1) 0 else if(slices == 2) (addr >> 6).bitCount % 2 else {
      val low = (6 until 48 by 2).count(addr.testBit) % 2
      val high = (7 until 48 by 2).count(addr.testBit) % 2
      low + 2 * high
    }
    val victimsBefore = unused
    val conflicts = (1 to 128).map(i => unusedTarget + i * 8192).filter(a => bank(a) == bank(unusedTarget)).take(10)
    for(addr <- conflicts) { request(addr, 0x200040); drain() }
    assert(unused > victimsBefore, "no unused genuine CDP victim feedback")
    // Occupy all shared translation slots with real VBOP trains. A subsequent
    // pointer line waits alongside VBOP; production assertion checks priority.
    epoch = seeds + 1; gate = false; controls(v = true)
    // Keep VBOP translations outside the pointer-memory image. Otherwise
    // VBOP refills can start new detections while D1 sends are draining, whose
    // different fixed/variable latency changes duplicate residency (not just
    // output order). Pointer detection below still competes for shared slots.
    // The independent overlap entry retains the original legal pointer-bearing
    // VBOP workload; the default case continues to isolate shared TLB contention.
    val contentionVA = if(overlap) 0x200040 else 0x280040
    for(i <- 0 until 12) { request(BigInt(0xb0000 + i * 64), BigInt(contentionVA + i * 4096)); run(120) }
    request(BigInt(0x40000 + seeds * 64), BigInt(0x200000 + seeds * 64)); run(155)
    gate = true; drain(); controls()
    assert(contention > 0, "shared TLB slot contention was empty")
    epoch = seeds + 2
    val disabled = emitted.size; controls(c = false)
    request(0x90000, 0x200040); drain()
    assert(emitted.size == disabled, "CSR off emitted a request")
    assert(emitted.nonEmpty && admitted.nonEmpty && hits > 0)
    assert(beats == txn * 2, "demand data scoreboard did not close")
    trace.close()
    println(s"CDP_C5_SYSTEM_RESULT pass=true seeds=$seeds slices=$slices cycles=$cycle demands=$txn checkedBeats=$beats candidates=$allCandidates discarded=$allDrops outputs=${emitted.size} admitted=${admitted.size} useful=$hits unused=$unused tlbContention=$contention csrOff=0 trace=$tracePath")
  }
}

// Permanent H2 workload: preserve the original overlapping detection/send case.
object TestTop_OceanusCDPC5Overlap {
  def main(args: Array[String]): Unit = TestTop_OceanusCDPC5.main(args :+ "--overlap")
}
