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

class CDPRawReplayCore(implicit p: Parameters) extends ref.ReferenceCDP {
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
 }
class CDPRawReplayReference(implicit p:Parameters) extends Module {
  val core=Module(new CDPRawReplayCore)
  val io=IO(core.io.cloneType);io<>core.io
  val replay=IO(core.replay.cloneType);core.replay<>replay
  val sendUnit=core.sendUnit;val vpnTrainReqBuf=core.vpnTrainReqBuf
  val ftTrainPipe=core.ftTrainPipe;val filterTable=core.filterTable
  def read[T <: Data](v:T):T=BoringUtils.bore(v)
  val fields = mutable.ArrayBuffer.empty[(String, UInt)]
  def add(n: String, v: Data): Unit = fields += n -> read(v).asUInt
  def fieldsBuffer(n:String,v:UInt):Unit=fields+=n->v
  def event(n: String, v: Bool, fields: (String, Data)*): Unit = {
    fieldsBuffer(n,read(v).asUInt)
    fields.foreach {case(key,b)=>fieldsBuffer(n+"."+key,Mux(read(v),read(b).asUInt,0.U(b.getWidth.W)))}
  }
  val s = sendUnit
  val entries = read(s.entries)
  event("candidate", s.io.in.valid, "va" -> s.io.in.bits.pfVAddr, "depth" -> s.io.in.bits.pfDepth)
  fieldsBuffer("allocated",(read(s.in.valid) && !read(s.entryHit) && read(s.hasFreeEntry)).asUInt)
  fieldsBuffer("duplicate",(read(s.in.valid) && read(s.entryHit)).asUInt)
  fieldsBuffer("full",(read(s.in.valid) && !read(s.entryHit) && !read(s.hasFreeEntry)).asUInt)
  for(i <- 0 until 16) {
    val va = Cat(entries(i).vTag, 0.U(6.W))
    val secondMiss = read(s.tlb_s2_valid) && s.samePage(va, read(s.tlb_s2_addr)) && read(s.tlb_s2_rsp.valid) && read(s.tlb_s2_rsp.bits.miss) && entries(i).retryEn
    val denied = read(s.tlb_s3_valid) && s.samePage(va, read(s.tlb_s3_addr)) && read(s.tlb_s3_rsp_valid) && !read(s.tlb_s3_rsp_bits).miss && read(s.s3_drop)
    event(s"translationDrop$i", read(s.valids)(i) && (secondMiss || denied), "va" -> va, "depth" -> entries(i).pfDepth)
  }
  event("filterDrop", read(s.pft_s2_valid) && !read(s.canPft), "va" -> Cat(entries(read(s.pft_s2_chosen_idx)).vTag, 0.U(6.W)), "depth" -> read(s.pft_s2_req).cdpPfDepth.get)
  event("out", (read(io.pftReq.valid) && read(io.pftReq.ready)), "pa" -> io.pftReq.bits.addr, "depth" -> io.pftReq.bits.cdpPfDepth.get)
  add("vpnAccepted", (read(vpnTrainReqBuf.io.enq.valid) && read(vpnTrainReqBuf.io.enq.ready)))
  add("vpnDropped", read(vpnTrainReqBuf.io.enq.valid) && !read(vpnTrainReqBuf.io.enq.ready))
  event("candidateMeta",s.io.in.valid,"source"->s.io.in.bits.pfSource,"hit"->s.io.in.bits.isHit)
  val children=core.getClass.getMethods.find(_.getName=="getIds").get.invoke(core)
    .asInstanceOf[scala.collection.Seq[AnyRef]]
  val fq=children.collectFirst { case q:Queue[_] if q.io.enq.bits.isInstanceOf[ref.PrefetchTrain] && (q ne vpnTrainReqBuf)=>q }.get
  add("filterAccepted",(read(fq.io.enq.valid) && read(fq.io.enq.ready)))
  add("filterDropped",read(fq.io.enq.valid) && !read(fq.io.enq.ready))
  add("filterQueueCount",fq.io.count)
  val ft=ftTrainPipe.get;val table=filterTable.get
  add("filterPLRU",read(ft.replacer.asInstanceOf[freechips.rocketchip.util.SetAssocLRU].state_vec))
  event("filterTrainWrite",(read(table.io.trainReq.valid) && read(table.io.trainReq.ready)),"bits"->table.io.trainReq.bits)

  val observationBits = IO(Output(UInt(fields.map(_._2.getWidth).sum.W)))
  observationBits := Cat(fields.map(_._2).reverse.toSeq)
  val widths = fields.map { case (n, v) => n -> v.getWidth }.toVector
}

object TestTop_CDPRawReplay extends App with ChiselSim {
  Constantin.init(false)
  val path = Paths.get(args.head)
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
  simulate(new CDPRawReplayReference()(rp)) { dut =>
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
      for(k <- Seq("candidate", "allocated", "duplicate", "full", "vpnAccepted", "vpnDropped", "filterAccepted", "filterDropped", "filterQueueCount", "filterPLRU", "filterTrainWrite", "filterTrainWrite.bits"))
        assert(output(k) == r(k), s"cycle=${row.cycle} epoch=$e $k DUT=${r(k)} reference=${output(k)}")
      if(r("candidate") != 0) for(k <- Seq("candidate.va", "candidate.depth"))
        assert(output(k) == r(k), s"cycle=${row.cycle} epoch=$e $k")
      if(r("candidate") != 0) {
        assert(output("candidateMeta.source")==r("candidate.source"),s"candidate source cycle=${row.cycle}")
        assert(output("candidateMeta.hit")==r("candidate.hit"),s"candidate isHit cycle=${row.cycle}")
      }
      outcomes(output, list(actual, e)); admission(output, list(referenceAccepted, e))
      dut.clock.step()
    }
    var orderDifferences = 0
    for(e <- expected.keys.toVector.sorted) {
      val d = list(expected, e).toVector; val r = list(actual, e).toVector
      assert(d.sorted == r.sorted, s"epoch=$e full output/drop multiset mismatch DUT=$d reference=$r")
      assert(list(accepted, e) == list(referenceAccepted, e), s"epoch=$e accepted identities differ")
      if(d != r) orderDifferences += 1
    }
    val outputCount = expected.values.flatten.count(_.startsWith("out:"))
    assert(outputCount > 0 && requestCount > 0)
    println(s"CDP_RAW_REPLAY_RESULT pass=true seeds=${header("seeds").num.toInt} slices=${header("slices").num.toInt} cycles=${rows.size} outputs=$outputCount outcomes=${expected.values.map(_.size).sum} epochs=${expected.size} d1OrderDifferences=$orderDifferences missing=0 extra=0 referenceBody=unmodified trace=$path")
  }
}
