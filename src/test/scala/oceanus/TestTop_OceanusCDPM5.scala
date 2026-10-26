package oceanus

// Audit M5: real cache and CDP generate the depth3 parent. No injected depth,
// directory state, or forced algorithm input. ReferenceCDP.scala stays verbatim.
// The second reference run independently maps the same late-demand transaction
// using MainPipe:592-595/925-937 and MSHR:829-840; it is an event-map oracle,
// not a simulation of the coupledL2 cache pipeline.
import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import oceanus.{cdpreference => ref}
import org.chipsalliance.cde.config.Parameters
import utility._
import scala.collection.mutable
import java.nio.file.{Files, Paths}

class TestTopCDPM5(cfg: L2Configuration)(implicit p: Parameters) extends TestTopCDPC5(cfg) {
  for ((t, i) <- module.slices.head.tshrs.zipWithIndex) {
    val r = t.rbeREQ
    add(s"activePrefetch$i", t.vPipeREQ.io.bopPerf.activePrefetch)
    event(s"rbeIn$i", fire(r.io.in), "pa" -> t.io.fromAlloc.paddr,
      "src" -> r.io.in.bits.req.SrcID, "txn" -> r.io.in.bits.req.TxnID,
      "ownerPA" -> t.tshr_paddr)
    event(s"rbeWait$i", read(r.io.valid) && !read(r.io.out.valid),
      "pa" -> t.tshr_paddr, "src" -> r.io.out.bits.req.SrcID)
    event(s"line$i", t.io.cdpData.valid, "pa" -> t.io.cdpData.bits.paddr,
      "source" -> t.io.cdpData.bits.pfSource, "depth" -> t.io.cdpData.bits.pfDepth,
      "hit" -> t.io.cdpData.bits.isHit, "type" -> t.io.cdpData.bits.eventType, "data" -> t.io.cdpData.bits.data)
    val tr = t.io.cdpTrain
    event(s"rawTrain$i", tr.valid, "pa" -> tr.bits.paddr, "source" -> tr.bits.pfSource,
      "depth" -> tr.bits.pfDepth, "hit" -> tr.bits.hit)
  }
  event("candidateMeta", s.io.in.valid, "source" -> s.io.in.bits.pfSource, "hit" -> s.io.in.bits.isHit)
  val detail = IO(Output(UInt(fields.map(_._2.getWidth).sum.W)))
  detail := Cat(fields.map(_._2).reverse.toSeq)
  val detailWidths = fields.map { case (n, v) => n -> v.getWidth }.toVector
}
class CDPM5Reference(implicit p: Parameters) extends CDPC5ReplayReference {
  val extra = IO(Output(UInt(8.W)))
  extra := Cat(ftTrainPipe.get.io.trainTrigger.fire, vpnTrainReqBuf.io.deq.fire,
    sendUnit.io.in.bits.pfSource, sendUnit.io.in.bits.isHit, sendUnit.io.in.valid)
}

object TestTop_OceanusCDPM5 extends App with ChiselSim {
  Constantin.init(false)
  require(L2TopPhase1aPorts.enabled)
  require(!sys.props.get("oceanus.phase1c2.depth").contains("true"))
  val dest = Paths.get(sys.props("oceanus.m5.trace")); Files.createDirectories(dest)
  val cfg = new L2Configuration(8, true, 0 until 1, bopEnable = false,
    pfRecvEnable = true, pfCtrlEnable = true, pfTrainEnable = true, pfTlbEnable = true,
    cdpStubEnable = true)
  val rootPA = BigInt(0x40000); val targetPA = BigInt(0x600040)
  val parentVA = BigInt(0x300044); val childVA = BigInt(0x310044)
  def pa(va: BigInt): BigInt = va + 0x300000
  def memory(addr: BigInt, beat: Int): BigInt =
    if (beat != 0) BigInt(0) else if (addr == rootPA) parentVA else if (addr == targetPA) childVA else BigInt(0)
  case class Row(cycle: Int, phase: String, r: Map[String, BigInt])
  val rows = mutable.ArrayBuffer.empty[Row]
  case class Candidate(va: BigInt, depth: BigInt, source: BigInt, hit: BigInt)
  def decode(bits: BigInt, widths: Seq[(String, Int)]): Map[String, BigInt] = {
    var v = bits
    widths.map { case (n, w) => val x = v & ((BigInt(1) << w) - 1); v >>= w; n -> x }.toMap
  }
  def candidates(rs: Seq[Row]): Vector[Candidate] = rs.filter(_.r("candidate") != 0).map { row =>
    val r = row.r; Candidate(r("candidate.va"), r("candidate.depth"), r("candidateMeta.source"), r("candidateMeta.hit"))
  }.toVector
  implicit val simulator: HasSimulator = HasSimulator.default
  val mode = args.headOption.getOrElse("cache")
  val arrivalDelay = sys.props.getOrElse("oceanus.m5.delay", "0").toInt
  require(arrivalDelay >= 0)
  lazy val merged: Boolean = rows.exists(row => (0 until 8).exists(i =>
    row.r(s"rbeIn$i") != 0 && row.r(s"rbeIn$i.ownerPA") == targetPA &&
      row.r(s"rbeIn$i.src") == 0 && row.r(s"activePrefetch$i") != 0))
  var lateExecute = -1; var lateArrival = -1; var lateWait = 0; var targetFirstData = -1
  if(mode == "cache") {
  simulate(new TestTopCDPM5(cfg)(CDPC5Params.apply)) { dut =>
    val dummy = UpRequestSpec(MasterId.t1p0, 0, 0, 0, CCHIOpcode.ReadShared.opcode, 0)
    val spec = CaseSpec("cdp-m5-late", UpConfig(Seq(MasterId.t1p0),
      ExplicitRequestStream(Seq(dummy), Seq(BigInt(CHIFieldResp.CompData_SC))), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(80), FixedOutstanding(64), ImmediateCreditPolicy()), 100000, (0,100000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec); val dn = new L2TopDownstreamModel(dut, spec)
    val data = mutable.Queue.empty[PendingDnComp]; val rsp = mutable.Queue.empty[PendingDnRsp]
    val ack = mutable.Queue.empty[PendingUpAck]; val snoop = mutable.Queue.empty[PendingSnpRsp]
    val tlbReplies = mutable.Queue.empty[(Int, BigInt)]
    val downstream = mutable.ArrayBuffer.empty[(Int, BigInt)]
    var cycle = 0; var txn = 0; var phase = "warm"; var activePA = BigInt(0); var beats = 0
    def flag(b: Bool) = b.peek().litToBoolean
    def value(b: UInt) = b.peek().litValue
    def tick(): Map[String, BigInt] = {
      up.clearResponses(); dn.clearResponses()
      ack.headOption.foreach { a => up.driveUpCompAck(a.upDBID, a.sliceIdx); ack.dequeue() }
      if (!flag(dut.io.t1p.head.UpRSP.valid)) snoop.headOption.foreach { s =>
        up.driveUpSnpResp(s.snpTxnID, s.tgtId); snoop.dequeue()
      }
      data.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
        dn.driveDnData(p); dut.io.chi.rxdat.flit.Data.get.poke(memory(p.addr, p.beat))
        if (p.addr == targetPA && targetFirstData < 0) targetFirstData = cycle
        data.dequeue(); dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
      }
      rsp.headOption.filter(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
        dn.driveDnRsp(p); rsp.dequeue(); dn.noteResponse(p.dnTxnID, true, cycle)
      }
      val tlb = dut.io.pfTlb.get
      tlb.req.ready.poke(true); tlb.resp.valid.poke(false)
      tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits)); tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      tlbReplies.headOption.filter(_._1 <= cycle).foreach { case (_, va) =>
        tlbReplies.dequeue(); tlb.resp.valid.poke(true); tlb.resp.bits.paddr.head.poke(pa(va))
      }
      if (flag(tlb.req.valid)) tlbReplies.enqueue((cycle + 1, value(tlb.req.bits.vaddr)))
      if (flag(dut.io.chi.txreq.flitv)) {
        val f = dut.io.chi.txreq.flit; val id = DnTxnID(value(f.TxnID.get)); val src = value(f.SrcID.get)
        val op = value(f.Opcode.get); val addr = value(f.Addr.get); dn.noteRequest(id)
        if (dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id, src, cycle+80, dn.downstreamEvictRspOpcode(op)))
        else {
          downstream += cycle -> addr
          val delay = if (addr == targetPA) 200 else 80
          for (b <- 0 until 2) data.enqueue(PendingDnComp(id, src, cycle+delay+b, b, BigInt(CHIFieldResp.CompData_SC), addr))
        }
      }
      val port = dut.io.t1p.head
      if (flag(port.DnDAT.valid)) {
        val f = port.DnDAT.bits; val b = value(f.DataID).toInt
        assert(value(f.Data) == memory(activePA,b), s"M5 demand data PA=$activePA beat=$b")
        beats += 1
        if (b == 1) ack.enqueue(PendingUpAck(0,UpDBID(value(f.DBID)),UpTxnID(value(f.TxnID)),cycle+1,value(f.SrcID).toInt))
      }
      if (flag(port.DnSNP.valid)) snoop.enqueue(PendingSnpRsp(SnpTxnID(value(port.DnSNP.bits.TxnID)),value(port.DnSNP.bits.SrcID),cycle+1))
      assert(!flag(port.DnRSP.valid))
      val r = decode(value(dut.detail), dut.detailWidths); rows += Row(cycle,phase,r)
      for (i <- 0 until 8) {
        if (r(s"rbeIn$i") != 0 && r(s"rbeIn$i.src") == 0 && phase == "late") lateArrival = cycle
        if (r(s"rbeWait$i") != 0 && r(s"rbeWait$i.pa") == targetPA && r(s"rbeWait$i.src") == 0) lateWait += 1
        if (r(s"rawTrain$i") != 0 && r(s"rawTrain$i.pa") == targetPA) lateExecute = cycle
      }
      dn.stepWithCredits(cycle); cycle += 1; assert(cycle < 100000,"M5 watchdog"); r
    }
    def run(n: Int): Unit = for (_ <- 0 until n) tick()
    def drain(): Unit = {
      var quiet = 0
      while (quiet < 35) {
        val r = tick()
        val idle = data.isEmpty && rsp.isEmpty && ack.isEmpty && snoop.isEmpty && tlbReplies.isEmpty &&
          r("resident") == 0 && r("candidatePending") == 0 &&
          dut.observation.dirProbeBySlice.flatten.forall(p => !flag(p.valid) && !flag(p.reqActive))
        quiet = if (idle) quiet+1 else 0
      }
    }
    def request(addr: BigInt, va: BigInt): Unit = {
      activePA = addr; up.driveRead(dummy.copy(txnId=txn%256,addr=addr))
      dut.io.pfTrain.get.head.vaddrBlock.poke(va >> 6)
      dut.io.pfTrain.get.head.reqSource.poke(0); dut.io.pfTrain.get.head.trainEnable.poke(true)
      while (!flag(dut.io.t1p.head.UpREQ.ready)) tick()
      tick(); up.clearUpReq(); txn += 1
    }
    up.initInputs(); dn.initInputs(); dut.log.dump.poke(false); dut.log.clean.poke(false)
    dut.holdIssue.get.poke(false); dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get))
    dut.io.pfCtrl.get.poke(0.U.asTypeOf(dut.io.pfCtrl.get))
    dut.io.pfCtrl.get.l2_pf_master_en.poke(true); dut.io.pfCtrl.get.l2_cdp_en.poke(true)
    dut.reset.poke(true); dut.clock.step(8); dut.reset.poke(false); dn.activateRxLink()
    for (w <- -300 until 0) dn.stepWithCredits(w)
    for (_ <- 0 until 130) { request(0x3e040,0x200040); drain() }
    phase = "root"; request(rootPA,0x200000)
    while (!downstream.exists(_._2 == targetPA)) tick()
    assert(rows.exists(r => r.r("out") != 0 && r.r("out.pa") == targetPA && r.r("out.depth") == 3),"parent must be issued by real CDP depth3")
    run(arrivalDelay)
    phase = "late"; request(targetPA,parentVA & ~BigInt(63)); drain(); run(20)
    assert(lateArrival >= 0 && lateExecute > targetFirstData && (!merged || lateWait > 0),
      s"late/RBE ordering arrival=$lateArrival data=$targetFirstData execute=$lateExecute wait=$lateWait")
    val trains = rows.flatMap(r => (0 until 8).filter(i => r.r(s"rawTrain$i") != 0 && r.r(s"rawTrain$i.pa") == targetPA).map(i => r.r))
    assert(trains.size == 1 && trains.head("vpn") == 1 && trains.head("filter") == 1)
    val trainedMeta = rows.flatMap(row => (0 until 8).filter(i =>
      row.r(s"rawTrain$i") != 0 && row.r(s"rawTrain$i.pa") == targetPA).map(i =>
      (row.r(s"rawTrain$i.hit"),row.r(s"rawTrain$i.source"),row.r(s"rawTrain$i.depth"))))
    assert(trainedMeta.toVector == Vector((BigInt(1),BigInt(9),BigInt(3))),s"old-meta training=$trainedMeta")
    assert(rows.count(r => r.phase == "late" && r.r("vpnAccepted") != 0) == 1)
    assert(rows.count(r => r.phase == "late" && r.r("filterAccepted") != 0) == 1)
    val lines = rows.flatMap(r => (0 until 8).filter(i => r.r(s"line$i") != 0 && r.r(s"line$i.pa") == targetPA)
      .map(i => (r.cycle,r.r(s"line$i.hit"),r.r(s"line$i.source"),r.r(s"line$i.depth"))))
    val expectedLines = if(merged) Vector((BigInt(0),BigInt(0),BigInt(0))) else
      Vector((BigInt(0),BigInt(9),BigInt(3)),(BigInt(1),BigInt(9),BigInt(3)))
    assert(lines.map(x => (x._2,x._3,x._4)).toVector == expectedLines,s"M5 lines=$lines")
    for(row <- rows; i <- 0 until 8 if row.r(s"line$i") != 0 && row.r(s"line$i.pa") == targetPA)
      assert(row.r(s"line$i.type") == row.r(s"line$i.hit"), "refill/hit event type")
    val child = candidates(rows.toVector).filter(c => (c.va & ~BigInt(63)) >= (childVA & ~BigInt(63)) && (c.va & ~BigInt(63)) < (childVA & ~BigInt(63))+192)
    assert(child.map(_.depth) == Vector.fill(3)(BigInt(if(merged) 3 else 1)),s"M5 child=$child")
    assert(beats == txn*2 && downstream.count(_._2 == targetPA) == 1)
    println(s"CDP_M5_CACHE pass=true head=${sys.props("oceanus.m5.head")} cycles=$cycle demands=$txn beats=$beats parentDepth=3 lateArrival=$lateArrival firstData=$targetFirstData lateExecute=$lateExecute RBEwait=$lateWait trainVPN=1 trainFilter=1 merged=$merged arrivalDelay=$arrivalDelay detectionCount=${lines.size} lines=$lines child=$child")
  }

  val cache = ujson.Obj("head" -> sys.props("oceanus.m5.head"), "lateArrival" -> lateArrival,
    "firstData" -> targetFirstData, "lateExecute" -> lateExecute, "RBEwait" -> lateWait,
    "rows" -> ujson.Arr.from(rows.map(row => ujson.Obj("cycle" -> row.cycle,"phase" -> row.phase,
      "signals" -> ujson.Obj.from(row.r.map { case(k,v) => k -> ujson.Str(v.toString(16)) })))))
  Files.writeString(dest.resolve("m5-cache.json"),cache.render())
  } else {
    val cache = ujson.read(Files.readString(dest.resolve("m5-cache.json")))
    assert(cache("head").str == sys.props("oceanus.m5.head"), "M5 trace HEAD mismatch")
    rows ++= cache("rows").arr.map(r => Row(r("cycle").num.toInt,r("phase").str,
      r("signals").obj.map { case(k,v) => k -> BigInt(v.str,16) }.toMap))
    lateArrival=cache("lateArrival").num.toInt; targetFirstData=cache("firstData").num.toInt
    lateExecute=cache("lateExecute").num.toInt; lateWait=cache("RBEwait").num.toInt
  }

  // Replay the *identical* captured boundary first, checking every candidate.
  // Independently prescribe the late-merge event mapping from MainPipe/MSHR,
  // with identical data, VA, training counts, TLB results/ready. After alignment
  // it matches the DUT; arrivals after prefetch completion retain ordinary hits.
  val rp = CDPC5Params.apply.alterPartial {
    case ref.ReferenceCDPKey => ref.CDPParameters()
    case ref.ReferenceCDPBankBitsKey => 0
  }
  val referenceResults = mutable.Map.empty[String, Vector[Candidate]]
  for (mapping <- Seq(mode).filter(m => m == "identical" || m == "merge")) simulate(new CDPM5Reference()(rp)) { dut =>
    def zero(d: Data): Unit = d.poke(0.U.asTypeOf(d))
    dut.io.enable.poke(false); zero(dut.io.vpnTrain); zero(dut.io.filterTrain)
    zero(dut.io.l2DetectTriggers); zero(dut.io.pfStat); zero(dut.replay)
    dut.io.pftReq.ready.poke(true); dut.io.tlbReq.req.ready.poke(true)
    dut.io.tlbReq.resp.valid.poke(false); zero(dut.io.tlbReq.resp.bits); zero(dut.io.tlbReq.pmp_resp)
    dut.reset.poke(true); dut.clock.step(8); dut.reset.poke(false)
    var pending: Option[BigInt] = None
    val observed = mutable.ArrayBuffer.empty[Candidate]
    val outs = mutable.ArrayBuffer.empty[(BigInt,BigInt)]
    var lateVPN = 0; var lateFilter = 0; var acceptedHalves = 0; var filterDequeued=0; var vpnDequeued=0
    def train(name: String,r: Map[String,BigInt]): Unit = {
      val t = if(name == "vpn") dut.io.vpnTrain else dut.io.filterTrain
      zero(t.bits); t.valid.poke(r(name) != 0)
      t.bits.tag.poke(r(name+".pa") >> 12); t.bits.set.poke((r(name+".pa") >> 6)&63)
      t.bits.evict_tag.get.poke(r(name+".victim") >> 12); t.bits.evict_set.get.poke((r(name+".victim") >> 6)&63)
      t.bits.vaddr.get.poke(r(name+".va")); t.bits.hit.poke(r(name+".hit") != 0)
      t.bits.prefetched.poke(r(name+".prefetched") != 0); t.bits.pfsource.poke(r(name+".source"))
      // MainPipe:773 retains original prefetch reqsource for mergeA.
      t.bits.reqsource.poke(if(mapping == "merge" && merged && r(name+".pa") == targetPA) BigInt(16) else r(name+".reqSource"))
      t.bits.cdp_vpn_train_valid.get.poke(r(name+".vpnValid") != 0)
      t.bits.cdp_filter_train_hit.get.poke(r(name+".used") != 0)
      t.bits.cdp_filter_train_evict.get.poke(r(name+".evict") != 0)
    }
    for (row <- rows) {
      val r = row.r
      dut.io.enable.poke(r("enable") != 0); train("vpn",r); train("filter",r)
      val late = mapping == "merge" && merged && r("half") != 0 && r("halfPA") == targetPA
      // Reference merged meta has default source0/depth0. Its prefetch-only
      // refill is replaced by this single merged refill, with no later hit.
      val suppress = late && row.cycle < lateExecute
      dut.replay.half.poke(r("half") != 0 && !suppress); dut.replay.data.poke(r("half.data"))
      dut.replay.source.poke(if(late) BigInt(0) else r("half.source"))
      dut.replay.depth.poke(if(late) BigInt(0) else r("half.depth"))
      dut.replay.hit.poke(!late && r("half.hit") != 0)
      if(r("half") != 0 && r("halfPA") == targetPA && (!merged && r("half.hit") != 0 || merged && !suppress)) {
        assert(!merged || r("half.hit") == 0 && r("half.source") == 0 && r("half.depth") == 0,
          s"merged detection metadata cycle=${row.cycle}")
        acceptedHalves += 1
      }
      dut.io.tlbReq.resp.valid.poke(pending.nonEmpty); zero(dut.io.tlbReq.resp.bits); zero(dut.io.tlbReq.pmp_resp)
      pending.foreach(va => dut.io.tlbReq.resp.bits.paddr.head.poke(pa(va)))
      pending = if(dut.io.tlbReq.req.valid.peek().litToBoolean) Some(dut.io.tlbReq.req.bits.vaddr.peek().litValue) else None
      val o = decode(dut.observationBits.peek().litValue,dut.widths)
      if(row.phase == "late") { lateVPN += o("vpnAccepted").toInt; if(dut.io.filterTrain.valid.peek().litToBoolean) lateFilter += 1 }
      val meta = dut.extra.peek().litValue
      if(row.phase == "late") { filterDequeued += ((meta >> 7)&1).toInt; vpnDequeued += ((meta >> 6)&1).toInt }
      if(o("candidate") != 0) {
        observed += Candidate(o("candidate.va"),o("candidate.depth"),(meta >> 2)&15,(meta >> 1)&1)
      }
      if(o("out") != 0) outs += o("out.pa") -> o("out.depth")
      if(mapping == "identical") {
        assert(o("candidate") == r("candidate"),s"same-boundary candidate valid cycle=${row.cycle}")
        if(r("candidate") != 0) assert(observed.last == Candidate(r("candidate.va"),r("candidate.depth"),r("candidateMeta.source"),r("candidateMeta.hit")))
      }
      dut.clock.step()
    }
    assert(lateVPN == 1 && lateFilter == 1 && acceptedHalves == 2 && filterDequeued == 1 && vpnDequeued == 1)
    if(mapping == "identical") assert(outs.sorted.toVector == rows.filter(_.r("out") != 0).map(r => r.r("out.pa") -> r.r("out.depth")).sorted.toVector)
    Files.writeString(dest.resolve(s"m5-$mapping.json"),ujson.Obj("head"->sys.props("oceanus.m5.head"),
      "candidates"->ujson.Arr.from(observed.map(c => ujson.Arr(c.va.toString(16),c.depth.toInt,c.source.toInt,c.hit.toInt))),
      "outputs"->ujson.Arr.from(outs.map { case(a,d) => ujson.Arr(a.toString(16),d.toInt) }),
      "VPNaccepted"->lateVPN,"Filteraccepted"->lateFilter,"VPNdequeued"->vpnDequeued,"Filterdequeued"->filterDequeued).render())
    println(s"CDP_M5_REFERENCE pass=true mapping=$mapping trainVPN=$lateVPN trainFilter=$lateFilter targetAcceptedHalves=$acceptedHalves candidates=$observed outputs=$outs")
  }
  if(mode == "verify") {
  def result(mapping: String) = ujson.read(Files.readString(dest.resolve(s"m5-$mapping.json")))
  def readCandidates(mapping: String): Vector[Candidate] = {
    val r=result(mapping); assert(r("head").str == sys.props("oceanus.m5.head"))
    r("candidates").arr.map(c => Candidate(BigInt(c(0).str,16),BigInt(c(1).num.toInt),BigInt(c(2).num.toInt),BigInt(c(3).num.toInt))).toVector
  }
  val exact = readCandidates("identical"); val merged = readCandidates("merge")
  assert(exact.take(3) == merged.take(3) && exact.size == 6 && merged.size == 6)
  assert(exact.drop(3).map(_.va) == merged.drop(3).map(_.va))
  assert(exact == merged)
  val childDepth = if(TestTop_OceanusCDPM5.merged) 3 else 1
  assert(exact.drop(3).map(_.depth) == Vector.fill(3)(BigInt(childDepth)))
  assert(merged.drop(3).map(_.depth) == Vector.fill(3)(BigInt(childDepth)))
  assert(exact == candidates(rows.toVector))
  assert(exact.drop(3).map(c => (c.source,c.hit)) == Vector.fill(3)((BigInt(if(childDepth == 3) 0 else 9),BigInt(if(childDepth == 3) 0 else 1))))
  assert(merged.drop(3).map(c => (c.source,c.hit)) == Vector.fill(3)((BigInt(if(childDepth == 3) 0 else 9),BigInt(if(childDepth == 3) 0 else 1))))
  for(mapping <- Seq("identical","merge")) {
    val outs=result(mapping)("outputs").arr
    assert(outs.size == 6 && outs.drop(3).map(_(1).num.toInt).toVector == Vector.fill(3)(childDepth))
  }
  val json = ujson.Obj("head" -> sys.props("oceanus.m5.head"),"lateArrival" -> lateArrival,"firstData" -> targetFirstData,
    "lateExecute" -> lateExecute,"RBEwait" -> lateWait,"referenceCacheSimulated" -> false,
    "rows" -> ujson.Arr.from(rows.map(row => ujson.Obj("cycle"->row.cycle,"phase"->row.phase,
      "signals"->ujson.Obj.from(row.r.map { case(k,v) => k -> ujson.Str(v.toString(16)) })))),
    "localChildren" -> ujson.Arr.from(exact.drop(3).map(c => ujson.Arr(c.va.toString(16),c.depth.toInt,c.source.toInt,c.hit.toInt))),
    "referenceChildren" -> ujson.Arr.from(merged.drop(3).map(c => ujson.Arr(c.va.toString(16),c.depth.toInt,c.source.toInt,c.hit.toInt))))
  Files.writeString(dest.resolve("m5-result.json"),json.render())
  println(s"CDP_M5_RESULT pass=true aligned=true trainingCountEqual=true detectionCount=${if(childDepth==3) 1 else 2} childDepth=$childDepth degree=3")
  }
}
