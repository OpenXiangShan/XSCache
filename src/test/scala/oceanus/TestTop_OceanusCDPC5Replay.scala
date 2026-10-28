package oceanus

// Offline oracle: untouched ReferenceCDP body, with only its DetectPipeline
// inputs rebound to the recorded post-capture half boundary. Its unused input
// queues stay empty. The original degree queues are monitored: these system
// epochs do not reach their known partial-enqueue dependency defect (C3).
// D1 responses use the recorded page transaction's miss/permission/PA result,
// delivered at the reference's specified next-cycle TLB/next-cycle PMP timing.
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.{cdpreference => ref}
import org.chipsalliance.cde.config._
import utility._
import scala.collection.mutable
import java.nio.file.{Files, Paths}
import java.util.zip.GZIPInputStream
import scala.io.Source

class CDPC5ReplayReference(implicit p: Parameters) extends ref.ReferenceCDP {
  val replay = IO(new Bundle {
    val half = Input(Bool()); val data = Input(UInt(256.W))
    val source = Input(UInt(4.W)); val depth = Input(UInt(2.W)); val hit = Input(Bool())
  })
  val fromCDP = replay.source === ref.PfSource.CDP.id.U
  val hit = replay.hit && fromCDP && (replay.depth === 1.U || replay.depth === 3.U)
  val refill = !replay.hit && replay.depth < 1.U
  for(i <- 0 until 4) {
    val in = detectPipeSeq(i).io.detectReq
    in.valid := replay.half && io.enable && (hit || refill)
    in.bits.data := replay.data(64 * (i + 1) - 1, 64 * i)
    in.bits.pfSource := replay.source; in.bits.pfDepth := replay.depth; in.bits.isHit := replay.hit
  }
  degreeBufSeq.foreach { q =>
    val count = PopCount(q.io.enq.map(_.fire))
    when(!reset.asBool) { assert(count === 0.U || count === 3.U, "C5 raw reference degree queue entered partial-enqueue case") }
  }
  val fields = mutable.ArrayBuffer.empty[(String, UInt)]
  def add(n: String, v: Data): Unit = fields += n -> v.asUInt
  def event(n: String, v: Bool, fields: (String, Data)*): Unit = {
    add(n, v); fields.foreach { case (key, b) => add(n + "." + key, Mux(v, b.asUInt, 0.U(b.getWidth.W))) }
  }
  val s = sendUnit
  def read[T <: Data](v: T): T = BoringUtils.bore(v)
  val entries = read(s.entries)
  event("candidate", s.io.in.valid, "va" -> s.io.in.bits.pfVAddr, "depth" -> s.io.in.bits.pfDepth)
  add("allocated", s.in.valid && !read(s.entryHit) && read(s.hasFreeEntry))
  add("duplicate", s.in.valid && read(s.entryHit))
  add("full", s.in.valid && !read(s.entryHit) && !read(s.hasFreeEntry))
  for(i <- 0 until 16) {
    val va = Cat(entries(i).vTag, 0.U(6.W))
    val secondMiss = read(s.tlb_s2_valid) && s.samePage(va, read(s.tlb_s2_addr)) && read(s.tlb_s2_rsp.valid) && read(s.tlb_s2_rsp.bits.miss) && entries(i).retryEn
    val denied = read(s.tlb_s3_valid) && s.samePage(va, read(s.tlb_s3_addr)) && read(s.tlb_s3_rsp_valid) && !read(s.tlb_s3_rsp_bits).miss && read(s.s3_drop)
    event(s"translationDrop$i", read(s.valids)(i) && (secondMiss || denied), "va" -> va, "depth" -> entries(i).pfDepth)
  }
  event("filterDrop", read(s.pft_s2_valid) && !read(s.canPft), "va" -> Cat(entries(read(s.pft_s2_chosen_idx)).vTag, 0.U(6.W)), "depth" -> read(s.pft_s2_req).cdpPfDepth.get)
  event("out", io.pftReq.fire, "pa" -> io.pftReq.bits.addr, "depth" -> io.pftReq.bits.cdpPfDepth.get)
  // Named H2 observations leave the verbatim ReferenceCDP body untouched.
  add("h2.index", read(s.freeEntryIdx))
  add("h2.valid", read(s.valids).asUInt)
  add("h2.pa", VecInit(entries.map(_.paddrValid)).asUInt)
  add("h2.inflight", read(s.reqInflight).asUInt)
  add("h2.eligible", VecInit((0 until 16).map(i => read(s.pftArb.io.in(i).valid))).asUInt)
  event("h2.query", s.io.ftQueryReq.fire, "index" -> read(s.pftArb.io.chosen),
    "va" -> Cat(entries(read(s.pftArb.io.chosen)).vTag, 0.U(6.W)),
    "pa" -> read(s.pftArb.io.out.bits).addr)
  event("h2.query2", read(s.pft_s2_valid), "index" -> read(s.pft_s2_chosen_idx), "canPft" -> read(s.canPft))
  event("h2.tlb", s.io.tlbReq.req.fire, "va" -> s.io.tlbReq.req.bits.vaddr)
  event("h2.terminal", read(s.tlb_s3_valid) && read(s.tlb_s3_rsp_valid) && !read(s.tlb_s3_rsp_bits).miss,
    "va" -> read(s.tlb_s3_addr))
  add("vpnAccepted", vpnTrainReqBuf.io.enq.fire)
  add("vpnDropped", vpnTrainReqBuf.io.enq.valid && !vpnTrainReqBuf.io.enq.ready)
  val observationBits = IO(Output(UInt(fields.map(_._2.getWidth).sum.W)))
  observationBits := Cat(fields.map(_._2).reverse.toSeq)
  val widths = fields.map { case (n, v) => n -> v.getWidth }.toVector
}

object TestTop_OceanusCDPC5Replay extends App with ChiselSim {
  Constantin.init(false)
  val path = Paths.get(args.head)
  val diagnostic = args.contains("--h2-diagnostic")
  val dispositionReport = args.find(_.startsWith("--disposition-report=")).map(a => Paths.get(a.split("=", 2)(1)))
  require(!diagnostic || dispositionReport.isEmpty, "ordinary-random disposition audit uses the strict C5 replay")
  val differences = mutable.ArrayBuffer.empty[ujson.Value]
  val lifecycle = mutable.ArrayBuffer.empty[ujson.Value]
  val input = Source.fromInputStream(new GZIPInputStream(Files.newInputStream(path)))
  val lines = input.getLines()
  val header = ujson.read(lines.next())
  val widths = header("widths").arr.toVector.map(x => x(0).str -> x(1).num.toInt)
  case class Row(cycle: Int, epoch: Int, gate: Boolean, bits: BigInt) {
    lazy val values: Map[String, BigInt] = {
      var v = bits
      widths.map { case (n, w) => val x = v & ((BigInt(1) << w) - 1); v >>= w; n -> x }.toMap
    }
  }
  val rows = lines.map { line => val a = ujson.read(line); Row(a(0).num.toInt, a(1).num.toInt, a(2).bool, BigInt(a(3).str, 16)) }.toVector
  input.close()
  // Pre-index only external outcomes from the recorded input boundary; no
  // algorithm output is used to choose a translation address or permission.
  case class Page(pa: BigInt, retry: Boolean, miss: Boolean, fault: Boolean, mmio: Boolean, uncache: Boolean)
  val pages = mutable.Map.empty[(Int, BigInt), Page]
  rows.foreach { row =>
    val r = row.values
    if(r("terminal") != 0) pages((row.epoch, r("terminal.va") >> 12)) = Page(r("terminal.pa") & ~BigInt(4095),
      r("terminal.retry") != 0, r("terminal.miss") != 0, r("terminal.fault") != 0, r("terminal.mmio") != 0, r("terminal.uncache") != 0)
  }
  val rp = CDPC5Params.apply.alterPartial {
    case ref.ReferenceCDPKey => ref.CDPParameters()
    case ref.ReferenceCDPBankBitsKey => log2Ceil(header("slices").num.toInt)
  }
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  simulate(new CDPC5ReplayReference()(rp)) { dut =>
    val disposition = dispositionReport.map { _ =>
      Vector("dut", "reference").map(side => new CDPC5Dispositions(side,
        (epoch: Int, va: BigInt) => pages((epoch, va >> 12)).pa | (va & 4095)))
    }
    val expected = mutable.Map.empty[Int, mutable.ArrayBuffer[String]]
    val actual = mutable.Map.empty[Int, mutable.ArrayBuffer[String]]
    val accepted = mutable.Map.empty[Int, mutable.ArrayBuffer[String]]
    val referenceAccepted = mutable.Map.empty[Int, mutable.ArrayBuffer[String]]
    def list(m: mutable.Map[Int, mutable.ArrayBuffer[String]], k: Int) = m.getOrElseUpdate(k, mutable.ArrayBuffer.empty[String])
    def outcomes(r: Map[String, BigInt], buffer: mutable.ArrayBuffer[String]): Unit = {
      def item(kind: String, name: String): Unit = if(r(name) != 0)
        buffer += s"$kind:${r(name + (if(kind == "out") ".pa" else ".va"))}:${r(name + ".depth")}"
      item("out", "out"); item("filter", "filterDrop")
      for(i <- 0 until 16) item("translation", s"translationDrop$i")
      for(kind <- Seq("duplicate", "full")) if(r(kind) != 0)
        buffer += s"$kind:${r("candidate.va") & ~BigInt(63)}:${r("candidate.depth")}"
    }
    def admission(r: Map[String, BigInt], b: mutable.ArrayBuffer[String]): Unit = if(r("allocated") != 0)
      b += s"${r("candidate.va") & ~BigInt(63)}:${r("candidate.depth")}"
    def decode(bits: BigInt): Map[String, BigInt] = {
      var v = bits
      dut.widths.map { case (n, w) => val x = v & ((BigInt(1) << w) - 1); v >>= w; n -> x }.toMap
    }
    def train(name: String, r: Map[String, BigInt]): Unit = {
      val t = if(name == "vpn") dut.io.vpnTrain else dut.io.filterTrain
      t.valid.poke(r(name) != 0 && r(name + ".trainEnable") != 0 && r(name + ".reqSource") != MemReqSource.L1DataPrefetch.id)
      t.bits.poke(0.U.asTypeOf(t.bits))
      t.bits.tag.poke(r(name + ".pa") >> 12); t.bits.set.poke((r(name + ".pa") >> 6) & 63)
      t.bits.evict_tag.get.poke(r(name + ".victim") >> 12); t.bits.evict_set.get.poke((r(name + ".victim") >> 6) & 63)
      t.bits.vaddr.get.poke(r(name + ".va")); t.bits.hit.poke(r(name + ".hit") != 0)
      t.bits.prefetched.poke(r(name + ".prefetched") != 0); t.bits.pfsource.poke(r(name + ".source"))
      t.bits.reqsource.poke(r(name + ".reqSource")); t.bits.cdp_vpn_train_valid.get.poke(r(name + ".vpnValid") != 0)
      t.bits.cdp_filter_train_hit.get.poke(r(name + ".used") != 0); t.bits.cdp_filter_train_evict.get.poke(r(name + ".evict") != 0)
    }
    dut.io.enable.poke(false); dut.io.vpnTrain.valid.poke(false); dut.io.filterTrain.valid.poke(false)
    dut.io.vpnTrain.bits.poke(0.U.asTypeOf(dut.io.vpnTrain.bits)); dut.io.filterTrain.bits.poke(0.U.asTypeOf(dut.io.filterTrain.bits))
    dut.io.l2DetectTriggers.poke(0.U.asTypeOf(dut.io.l2DetectTriggers)); dut.io.pfStat.poke(0.U.asTypeOf(dut.io.pfStat))
    dut.replay.poke(0.U.asTypeOf(dut.replay)); dut.io.pftReq.ready.poke(true)
    dut.io.tlbReq.req.ready.poke(false); dut.io.tlbReq.resp.valid.poke(false)
    dut.io.tlbReq.resp.bits.poke(0.U.asTypeOf(dut.io.tlbReq.resp.bits)); dut.io.tlbReq.pmp_resp.poke(0.U.asTypeOf(dut.io.tlbReq.pmp_resp))
    dut.reset.poke(true); dut.clock.step(8); dut.reset.poke(false)
    var pending: Option[(BigInt, Page, Boolean)] = None
    var pmp: Option[Page] = None
    var previousEpoch = -2
    val attempts = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    var requestCount = 0
    rows.foreach { row =>
      val r = row.values; val e = row.epoch
      if(e != previousEpoch) { attempts.clear(); previousEpoch = e }
      outcomes(r, list(expected, e)); admission(r, list(accepted, e))
      dut.io.enable.poke(r("enable") != 0)
      train("vpn", r); train("filter", r)
      dut.replay.half.poke(r("half") != 0); dut.replay.data.poke(r("half.data"))
      dut.replay.source.poke(r("half.source")); dut.replay.depth.poke(r("half.depth")); dut.replay.hit.poke(r("half.hit") != 0)
      dut.io.pftReq.ready.poke(r("enable") != 0)
      dut.io.tlbReq.req.ready.poke(row.gate && r("enable") != 0)
      dut.io.tlbReq.resp.valid.poke(pending.nonEmpty)
      dut.io.tlbReq.resp.bits.poke(0.U.asTypeOf(dut.io.tlbReq.resp.bits))
      dut.io.tlbReq.pmp_resp.poke(0.U.asTypeOf(dut.io.tlbReq.pmp_resp))
      pmp.foreach { p => dut.io.tlbReq.pmp_resp.mmio.poke(p.mmio); dut.io.tlbReq.pmp_resp.ld.poke(p.fault) }
      pending.foreach { case (va, p, miss) =>
        dut.io.tlbReq.resp.bits.paddr.head.poke(p.pa | (va & 4095))
        dut.io.tlbReq.resp.bits.miss.poke(miss)
        dut.io.tlbReq.resp.bits.excp.head.pf.ld.poke(p.fault)
        dut.io.tlbReq.resp.bits.pbmt.poke(if(p.uncache) 1 else 0)
      }
      pmp = pending.map(_._2)
      pending = None
      if(row.gate && r("enable") != 0 && dut.io.tlbReq.req.valid.peek().litToBoolean) {
        val va = dut.io.tlbReq.req.bits.vaddr.peek().litValue; val page = va >> 12
        val outcome = pages.getOrElse((e, page), throw new AssertionError(s"reference requested an unrecorded input page epoch=$e VA=$va"))
        attempts(page) += 1
        pending = Some((va, outcome, outcome.miss || outcome.retry && attempts(page) == 1))
        requestCount += 1
      }
      val output = decode(dut.observationBits.peek().litValue)
      // The detection/training boundaries retain exact cycle identity. D1 is
      // limited to translated outputs/drops, compared as complete epoch bags.
      for(k <- Seq("candidate", "allocated", "duplicate", "full", "vpnAccepted", "vpnDropped")) {
        if(diagnostic && output(k) != r(k)) differences += ujson.Obj("cycle"->row.cycle,"epoch"->e,
          "field"->k,"dut"->r(k).toInt,"reference"->output(k).toInt)
        else assert(output(k) == r(k), s"cycle=${row.cycle} epoch=$e $k DUT=${r(k)} reference=${output(k)}")
      }
      if(diagnostic && e == header("seeds").num.toInt+1) lifecycle += ujson.Obj("cycle"->row.cycle,
        "dut"->ujson.Obj.from(r.map {case(k,v)=>k->ujson.Str(v.toString(16))}),
        "reference"->ujson.Obj.from(output.map {case(k,v)=>k->ujson.Str(v.toString(16))}))
      if(r("candidate") != 0) for(k <- Seq("candidate.va", "candidate.depth"))
        assert(output(k) == r(k), s"cycle=${row.cycle} epoch=$e $k")
      disposition.foreach { sides =>
        sides(0).sample(row.cycle, e, r)
        sides(1).sample(row.cycle, e, output + ("enable" -> r("enable")))
      }
      outcomes(output, list(actual, e)); admission(output, list(referenceAccepted, e))
      dut.clock.step()
    }
    var orderDifferences = 0
    for(e <- expected.keys.toVector.sorted) {
      val d = list(expected, e).toVector; val r = list(actual, e).toVector
      if(!diagnostic) {
        assert(d.sorted == r.sorted, s"epoch=$e full output/drop multiset mismatch DUT=$d reference=$r")
        assert(list(accepted, e) == list(referenceAccepted, e), s"epoch=$e accepted identities differ")
      }
      if(d != r) orderDifferences += 1
    }
    disposition.foreach { sides =>
      sides.foreach(_.finish())
      val seedCount = header("seeds").num.toInt
      val ordinary = (0 until seedCount).map { epoch =>
        val row = CDPC5Dispositions.compare(sides(0).outputs(epoch), sides(1).outputs(epoch))
        row("epoch") = ujson.Num(epoch)
        row("dutCounters") = sides(0).counters(epoch)
        row("referenceCounters") = sides(1).counters(epoch)
        row
      }
      def total(key: String): Int = ordinary.map(_(key).num.toInt).sum
      val union = total("unionOutputs"); val extra = total("dutExtra"); val missing = total("dutMissing")
      val result = ujson.Obj("status" -> "PASS", "traceHead" -> header("head"),
        "replayHead" -> ujson.Str(sys.props.getOrElse("oceanus.c5.head", "unknown")), "trace" -> path.toString,
        "slices" -> header("slices"), "seeds" -> seedCount, "epochStart" -> 0, "epochEndExclusive" -> seedCount,
        "comparisonKey" -> "candidateCycle:PA:depth; per-epoch multiset then sum",
        "scope" -> "ordinary C5 random epochs only; natural system training; no added Filter writes or output backpressure; warmup and trailing directed epochs excluded",
        "dutOutputs" -> total("dutOutputs"), "referenceOutputs" -> total("referenceOutputs"),
        "unionOutputs" -> union, "dutExtra" -> extra, "dutMissing" -> missing,
        "changedScenarios" -> ordinary.count(r => r("dutExtra").num + r("dutMissing").num > 0),
        "differentFractionOfUnion" -> (if(union == 0) 0.0 else (extra + missing).toDouble / union),
        "random" -> ujson.Arr.from(ordinary))
      val dest = dispositionReport.get
      Option(dest.toAbsolutePath.getParent).foreach(p => Files.createDirectories(p))
      Files.writeString(dest, result.render(2))
      println(s"CDP_C5_NORMAL_DISPOSITIONS seeds=$seedCount slices=${header("slices").num.toInt} dut=${total("dutOutputs")} reference=${total("referenceOutputs")} union=$union extra=$extra missing=$missing fraction=${result("differentFractionOfUnion").num} report=$dest")
    }
    val outputCount = expected.values.flatten.count(_.startsWith("out:"))
    assert(outputCount > 0 && requestCount > 0)
    if(diagnostic) {
      require(differences.nonEmpty,"H2 diagnostic did not preserve the admission counterexample")
      Files.writeString(path.resolveSibling("h2-lifecycle.json"),ujson.Obj("head"->header("head"),
        "diagnosticOnly"->true,"strictComparatorPassed"->false,
        "differences"->ujson.Arr.from(differences),"lifecycle"->ujson.Arr.from(lifecycle),
        "dutOutcomes"->ujson.Obj.from(expected.map {case(e,v)=>e.toString->ujson.Arr.from(v)}),
        "referenceOutcomes"->ujson.Obj.from(actual.map {case(e,v)=>e.toString->ujson.Arr.from(v)})).render())
      println(s"CDP_H2_DIAGNOSTIC recorded=true strictComparatorPassed=false differences=${differences.size} first=${differences.head}")
    } else println(s"CDP_C5_REPLAY_RESULT pass=true seeds=${header("seeds").num.toInt} slices=${header("slices").num.toInt} cycles=${rows.size} outputs=$outputCount outcomes=${expected.values.map(_.size).sum} epochs=${expected.size} d1OrderDifferences=$orderDifferences missing=0 extra=0 referenceBody=unmodified trace=$path")
  }
}

// Separate diagnostic entry; the normal C5 comparator retains all assertions.
object TestTop_OceanusCDPH2Replay {
  def main(args: Array[String]): Unit = TestTop_OceanusCDPC5Replay.main(args :+ "--h2-diagnostic")
}
