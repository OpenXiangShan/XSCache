// C2: compare original FilterTable/FtTrain with production on every clock.
// Test-only probes mask invalid SRAM/stage payloads; valid/fire never masked.
// A host model independently checks SRAM latency/owner, page arithmetic,
// saturation, writes, full-set scans, Queue8 and all PLRU bits every cycle.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.chi._
import oceanus.{cdpreference => ref}
import org.chipsalliance.cde.config._
import freechips.rocketchip.util.SetAssocLRU
import utility._
import java.nio.file.{Files, Paths}
import java.io.PrintWriter
import scala.collection.mutable
import scala.util.Random

class CDPC2Observation extends Bundle {
  val status = UInt(28.W)
  val tasks = Vec(3, UInt(38.W))
  val lru = UInt(192.W)
  val write = UInt(160.W)
  val querySet = UInt(6.W)
  val response = UInt(612.W)
  val sendResponse = UInt(612.W)
}
object CDPC2Mask {
  def apply(v: Vec[Bool], tags: Vec[UInt], sat: Vec[Vec[UInt]]): UInt = {
    val t = VecInit(tags.zip(v).map { case (t, v) => Mux(v, t, 0.U) })
    val s = VecInit(sat.zip(v).map { case (s, v) => Mux(v, s, 0.U.asTypeOf(s)) })
    Cat(v.asUInt, t.asUInt, s.asUInt)
  }
}
class CDPC2Production(implicit p: Parameters) extends OceanusCDPFilterTraining {

  val audit = IO(Output(new CDPC2Observation))
  def read[T <: Data](signal: T): T = chisel3.util.experimental.BoringUtils.bore(signal)
  val s1v = read(pipe.s1_valid); val s2v = read(pipe.s2_valid); val s3v = read(pipe.s3_valid)
  val same = read(pipe.sameVec)
  val flags = Wire(Vec(24, Bool()))
  flags := VecInit(Seq(queue.io.enq.ready, queue.io.enq.fire, queue.io.deq.valid,
    queue.io.deq.fire, Mux(queue.io.deq.valid, queue.io.deq.ready, false.B),
    // RR lastGrant has no reset; idle ready is undefined before first fire.
    // Always compare valid/fire, and compare ready for every valid query.
    pipe.io.queryReq.valid, Mux(pipe.io.queryReq.valid, pipe.io.queryReq.ready, false.B), table.io.queryReq.fire,
    Mux(table.io.queryReq.fire, arb.io.chosen.asBool, false.B), table.io.queryRsp.valid,
    Mux(table.io.queryRsp.valid, chosen.asBool, false.B), table.io.trainReq.valid,
    table.io.trainReq.ready, Mux(io.sendQuery.valid, io.sendQuery.ready, false.B), s1v, s2v, s3v,
    Mux(queue.io.deq.valid, same(0), false.B), Mux(queue.io.deq.valid, same(1), false.B),
    Mux(queue.io.deq.valid, same(2), false.B), read(pipe.s1_ready), queue.io.enq.valid,
    queue.io.enq.valid && !queue.io.enq.ready, table.io.trainReq.ready))
  audit.status := Cat(queue.io.count, flags.asUInt)
  audit.tasks(0) := Mux(s1v, read(pipe.s1_task).asUInt, 0.U)
  audit.tasks(1) := Mux(s2v, read(pipe.s2_task).asUInt, 0.U)
  audit.tasks(2) := Mux(s3v, read(pipe.s3_task).asUInt, 0.U)
  audit.lru := read(pipe.replacer.asInstanceOf[SetAssocLRU].state_vec).asUInt
  audit.write := Mux(table.io.trainReq.valid, table.io.trainReq.bits.asUInt, 0.U)
  audit.querySet := Mux(table.io.queryReq.fire, table.io.queryReq.bits.setIdx, 0.U)
  val rsp = table.io.queryRsp.bits
  audit.response := Mux(table.io.queryRsp.valid,
    CDPC2Mask(rsp.validVec, rsp.tagVec, rsp.satVec), 0.U)
  val sr = io.sendRsp.bits
  audit.sendResponse := Mux(io.sendRsp.valid,
    CDPC2Mask(sr.validVec, sr.tagVec, sr.satVec), 0.U)
}
class CDPC2Reference(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val train = Flipped(Valid(new ref.PrefetchTrain))
    val trainEnable = Input(Bool())
    val sendQuery = Flipped(Decoupled(new ref.FtQueryReq))
    val sendRsp = Valid(new ref.FtQueryRsp)
  })
  val table = Module(new ref.FilterTable)
  val pipe = Module(new ref.FtTrainPipeline)
  val queue = Module(new Queue(new ref.PrefetchTrain, 8))
  queue.io.enq.valid := io.train.valid && io.enable && io.trainEnable &&
    (io.train.bits.cdp_filter_train_hit.get || io.train.bits.cdp_filter_train_evict.get) &&
    io.train.bits.reqsource =/= MemReqSource.L1DataPrefetch.id.U
  queue.io.enq.bits := io.train.bits
  pipe.io.trainTrigger <> queue.io.deq
  val arb = Module(new RRArbiter(new ref.FtQueryReq, 2))
  arb.io.in(0) <> pipe.io.queryReq
  arb.io.in(1) <> io.sendQuery
  table.io.queryReq <> arb.io.out
  table.io.trainReq <> pipe.io.trainReq
  val chosen = RegEnable(arb.io.chosen, arb.io.out.fire)
  pipe.io.queryRsp := Mux(table.io.queryRsp.valid && chosen === 0.U,
    table.io.queryRsp.bits, 0.U.asTypeOf(new ref.FtQueryRsp))
  io.sendRsp.valid := table.io.queryRsp.valid && chosen === 1.U
  io.sendRsp.bits := Mux(io.sendRsp.valid, table.io.queryRsp.bits, 0.U.asTypeOf(new ref.FtQueryRsp))

  val audit = IO(Output(new CDPC2Observation))
  def read[T <: Data](signal: T): T = chisel3.util.experimental.BoringUtils.bore(signal)
  val s1v = read(pipe.s1_valid); val s2v = read(pipe.s2_valid); val s3v = read(pipe.s3_valid)
  val same = read(pipe.sameVec)
  val flags = Wire(Vec(24, Bool()))
  flags := VecInit(Seq(queue.io.enq.ready, queue.io.enq.fire, queue.io.deq.valid,
    queue.io.deq.fire, Mux(queue.io.deq.valid, queue.io.deq.ready, false.B),
    // Same valid-query ready domain as the production test probe.
    pipe.io.queryReq.valid, Mux(pipe.io.queryReq.valid, pipe.io.queryReq.ready, false.B), table.io.queryReq.fire,
    Mux(table.io.queryReq.fire, arb.io.chosen.asBool, false.B), table.io.queryRsp.valid,
    Mux(table.io.queryRsp.valid, chosen.asBool, false.B), table.io.trainReq.valid,
    table.io.trainReq.ready, Mux(io.sendQuery.valid, io.sendQuery.ready, false.B), s1v, s2v, s3v,
    Mux(queue.io.deq.valid, same(0), false.B), Mux(queue.io.deq.valid, same(1), false.B),
    Mux(queue.io.deq.valid, same(2), false.B), read(pipe.s1_ready), queue.io.enq.valid,
    queue.io.enq.valid && !queue.io.enq.ready, table.io.trainReq.ready))
  audit.status := Cat(queue.io.count, flags.asUInt)
  audit.tasks(0) := Mux(s1v, read(pipe.s1_task).asUInt, 0.U)
  audit.tasks(1) := Mux(s2v, read(pipe.s2_task).asUInt, 0.U)
  audit.tasks(2) := Mux(s3v, read(pipe.s3_task).asUInt, 0.U)
  audit.lru := read(pipe.replacer.asInstanceOf[SetAssocLRU].state_vec).asUInt
  audit.write := Mux(table.io.trainReq.valid, table.io.trainReq.bits.asUInt, 0.U)
  audit.querySet := Mux(table.io.queryReq.fire, table.io.queryReq.bits.setIdx, 0.U)
  val rsp = table.io.queryRsp.bits
  audit.response := Mux(table.io.queryRsp.valid,
    CDPC2Mask(rsp.validVec, rsp.tagVec, rsp.satVec), 0.U)
  val sr = io.sendRsp.bits
  audit.sendResponse := Mux(io.sendRsp.valid,
    CDPC2Mask(sr.validVec, sr.tagVec, sr.satVec), 0.U)
}
class CDPC2Harness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val event = Input(UInt(106.W))
    val send = Input(UInt(7.W))
    val audit = Output(new CDPC2Observation)
    val packed = Output(UInt(1724.W))
  })
  val prod = Module(new CDPC2Production)
  val reference = Module(new CDPC2Reference)
  val e = io.event
  prod.io.enable := e(99)
  prod.io.train.valid := e(98)
  prod.io.train.bits := 0.U.asTypeOf(prod.io.train.bits)
  prod.io.train.bits.paddr := e(47,0)
  prod.io.train.bits.evictPaddr := e(95,48)
  prod.io.train.bits.isUsed := e(96)
  prod.io.train.bits.isEvict := e(97)
  prod.io.train.bits.trainEnable := e(100)
  prod.io.train.bits.reqSource := e(105,101)
  // Deliberately no prefetch/source gate on a qualified used event.
  prod.io.train.bits.prefetched := e(0)
  prod.io.train.bits.pfSource := e(4,1)
  reference.io.enable := e(99)
  reference.io.trainEnable := e(100)
  reference.io.train.valid := e(98)
  reference.io.train.bits := 0.U.asTypeOf(reference.io.train.bits)
  reference.io.train.bits.tag := e(47,12)
  reference.io.train.bits.set := e(11,6)
  reference.io.train.bits.evict_tag.get := e(95,60)
  reference.io.train.bits.evict_set.get := e(59,54)
  reference.io.train.bits.cdp_filter_train_hit.get := e(96)
  reference.io.train.bits.cdp_filter_train_evict.get := e(97)
  reference.io.train.bits.reqsource := e(105,101)
  prod.io.sendQuery.valid := io.send(6)
  prod.io.sendQuery.bits.setIdx := io.send(5,0)
  reference.io.sendQuery.valid := io.send(6)
  reference.io.sendQuery.bits.setIdx := io.send(5,0)
  when(!reset.asBool) {
    // Intent (C2 diagnosis): retain both first-failure states without changing
    // any comparison domain or accepting an effective-state discrepancy.
    when(prod.audit.asUInt =/= reference.audit.asUInt) {
      printf(p"C2_MISMATCH event=0x${Hexadecimal(e)} send=0x${Hexadecimal(io.send)} prod=0x${Hexadecimal(prod.audit.asUInt)} reference=0x${Hexadecimal(reference.audit.asUInt)}\n")
    }
    assert(prod.audit.asUInt === reference.audit.asUInt,
      p"C2 Filter/FtTrain valid state/handshake/SRAM/owner/PLRU mismatch prod=0x${Hexadecimal(prod.audit.asUInt)} reference=0x${Hexadecimal(reference.audit.asUInt)}")
    assert(prod.io.sendRsp.valid === reference.io.sendRsp.valid)
  }

  // Verify the actual standalone production top, independently of the sender
  // traffic above. Its C4 sender is inactive; compare to a second reference
  // chain with that same inactive sender. Also elaborate Filter-disabled top.
  val productionTop = Module(new OceanusCDP())
  val disabledTop = Module(new OceanusCDP(OceanusCDPParameters(useFilterTable = false)))
  // Intent (C2 continuation): prove disabled elaboration omits the Filter
  // chain; every tick below also checks the real top and silent outputs.
  require(disabledTop.filterTraining.isEmpty)
  val isolatedReference = Module(new CDPC2Reference)
  for(top <- Seq(productionTop, disabledTop)) {
    top.io.enable := e(99)
    top.io.filterTrain := prod.io.train
    top.io.vpnTrain.valid := false.B
    top.io.vpnTrain.bits := 0.U.asTypeOf(top.io.vpnTrain.bits)
    top.io.l2DetectTriggers := 0.U.asTypeOf(top.io.l2DetectTriggers)
    top.io.pfStat := 0.U.asTypeOf(top.io.pfStat)
    top.io.tlb.req.ready := true.B
    top.io.tlb.resp.valid := false.B
    top.io.tlb.resp.bits := 0.U.asTypeOf(top.io.tlb.resp.bits)
    top.io.req.ready := e(0)
    assert(!top.io.req.valid && !top.io.tlb.req.valid, "C2 top must remain inactive")
  }
  isolatedReference.io.enable := reference.io.enable
  isolatedReference.io.trainEnable := reference.io.trainEnable
  isolatedReference.io.train := reference.io.train
  isolatedReference.io.sendQuery.valid := false.B
  isolatedReference.io.sendQuery.bits := 0.U.asTypeOf(isolatedReference.io.sendQuery.bits)
  val topFilter = productionTop.filterTraining.get
  def read[T <: Data](signal: T): T = chisel3.util.experimental.BoringUtils.bore(signal)
  val topAudit = Wire(new CDPC2Observation)
  val s1v = read(topFilter.pipe.s1_valid); val s2v = read(topFilter.pipe.s2_valid); val s3v = read(topFilter.pipe.s3_valid)
  val same = read(topFilter.pipe.sameVec)
  val flags = Wire(Vec(24, Bool()))
  flags := VecInit(Seq(read(topFilter.queue.io.enq.ready), (read(topFilter.queue.io.enq.valid) && read(topFilter.queue.io.enq.ready)), read(topFilter.queue.io.deq.valid),
    (read(topFilter.queue.io.deq.valid) && read(topFilter.queue.io.deq.ready)), Mux(read(topFilter.queue.io.deq.valid), read(topFilter.queue.io.deq.ready), false.B),
    // The real top has the identical idle-ready comparison contract.
    read(topFilter.pipe.io.queryReq.valid), Mux(read(topFilter.pipe.io.queryReq.valid), read(topFilter.pipe.io.queryReq.ready), false.B), (read(topFilter.table.io.queryReq.valid) && read(topFilter.table.io.queryReq.ready)),
    Mux((read(topFilter.table.io.queryReq.valid) && read(topFilter.table.io.queryReq.ready)), read(topFilter.arb.io.chosen).asBool, false.B), read(topFilter.table.io.queryRsp.valid),
    Mux(read(topFilter.table.io.queryRsp.valid), read(topFilter.chosen).asBool, false.B), read(topFilter.table.io.trainReq.valid),
    read(topFilter.table.io.trainReq.ready), Mux(read(topFilter.io.sendQuery.valid), read(topFilter.io.sendQuery.ready), false.B), s1v, s2v, s3v,
    Mux(read(topFilter.queue.io.deq.valid), same(0), false.B), Mux(read(topFilter.queue.io.deq.valid), same(1), false.B),
    Mux(read(topFilter.queue.io.deq.valid), same(2), false.B), read(topFilter.pipe.s1_ready), read(topFilter.queue.io.enq.valid),
    read(topFilter.queue.io.enq.valid) && !read(topFilter.queue.io.enq.ready), read(topFilter.table.io.trainReq.ready)))
  topAudit.status := Cat(read(topFilter.queue.io.count), flags.asUInt)
  topAudit.tasks(0) := Mux(s1v, read(topFilter.pipe.s1_task).asUInt, 0.U)
  topAudit.tasks(1) := Mux(s2v, read(topFilter.pipe.s2_task).asUInt, 0.U)
  topAudit.tasks(2) := Mux(s3v, read(topFilter.pipe.s3_task).asUInt, 0.U)
  topAudit.lru := read(topFilter.pipe.replacer.asInstanceOf[SetAssocLRU].state_vec).asUInt
  topAudit.write := Mux(read(topFilter.table.io.trainReq.valid), read(topFilter.table.io.trainReq.bits).asUInt, 0.U)
  topAudit.querySet := Mux((read(topFilter.table.io.queryReq.valid) && read(topFilter.table.io.queryReq.ready)), read(topFilter.table.io.queryReq.bits).setIdx, 0.U)
  val rsp = read(topFilter.table.io.queryRsp.bits)
  topAudit.response := Mux(read(topFilter.table.io.queryRsp.valid),
    CDPC2Mask(rsp.validVec, rsp.tagVec, rsp.satVec), 0.U)
  val sr = read(topFilter.io.sendRsp.bits)
  topAudit.sendResponse := Mux(read(topFilter.io.sendRsp.valid),
    CDPC2Mask(sr.validVec, sr.tagVec, sr.satVec), 0.U)
  when(!reset.asBool) {
    assert(topAudit.asUInt === isolatedReference.audit.asUInt,
      "C2 actual production top Filter training mismatch")
  }
  io.audit := prod.audit
  io.packed := prod.audit.asUInt
}

object TestTop_OceanusCDPC2 extends App with ChiselSim {
  Constantin.init(false)
  val params = new Config((_, _, _) => {
    // Intent (C1): the global event inherits HasL2Params/HasCHIParameters;
    // match the existing standalone queue test's PA48/data256 geometry.
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case L2ParamsKey => L2Params()
    case xscache.oceanus.compactchi.CCHIParametersKey => xscache.oceanus.compactchi.CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  def option(name: String, default: Int) = args.find(_.startsWith(name+"=")).map(_.split("=")(1).toInt).getOrElse(default)
  val seeds = option("--seeds", 1000)
  val randomCycles = option("--cycles", 512)
  val traceRoot = Paths.get(sys.props.getOrElse("oceanus.c2.trace", "/data/zhuy/pf-framework/c2/traces"))
  Files.createDirectories(traceRoot)
  val head = sys.props.getOrElse("oceanus.c2.head", sys.error("HEAD property required"))
  val totals = mutable.Map.empty[String, Long].withDefaultValue(0L)
  var cycles = 0L
  case class Entry(tag: Int, sat: Vector[Int])
  case class Task(set: Int, slot: Int, tag: Int, used: Boolean, evict: Boolean) {
    def bits: BigInt = (BigInt(set)<<32) | (BigInt(slot)<<26) | (BigInt(tag)<<2) |
      (if(used) BigInt(2) else BigInt(0)) | (if(evict) BigInt(1) else BigInt(0))
    def same(other: Task): Boolean = set == other.set && tag == other.tag
  }
  def task(e: BigInt): Task = {
    val pa = if(e.testBit(97)) (e >> 48) & ((BigInt(1)<<48)-1) else e & ((BigInt(1)<<48)-1)
    Task(((pa>>18)&63).toInt, ((pa>>12)&63).toInt, (pa>>24).toInt, e.testBit(96), e.testBit(97))
  }
  def event(pa: BigInt, used: Boolean = true, evict: Boolean = false, victim: BigInt = 0,
      enable: Boolean = true, train: Boolean = true, req: Int = MemReqSource.CPULoadData.id,
      valid: Boolean = true): BigInt = {
    var e = pa | (victim << 48) | (BigInt(req)<<101)
    for((v,b) <- Seq(used->96, evict->97, valid->98, enable->99, train->100) if v) e = e.setBit(b)
    e
  }
  def address(tag: Int, set: Int, slot: Int, low: Int = 0): BigInt =
    (BigInt(tag)<<24) | (BigInt(set)<<18) | (BigInt(slot)<<12) | BigInt(low)
  def response(row: Vector[Option[Entry]]): BigInt = {
    var b = BigInt(0)
    for(w <- 0 until 4; e <- row(w)) {
      b = b.setBit(608+w) | (BigInt(e.tag) << (512+w*24))
      for(i <- 0 until 64) b |= BigInt(e.sat(i)) << (w*128+i*2)
    }
    b
  }
  simulate(new CDPC2Harness()(params)) { dut =>
    var table = Array.fill(64)(Vector.fill[Option[Entry]](4)(None))
    var lru = Array.fill(64)(0)
    val queue = mutable.Queue.empty[Task]
    var s1 = Option.empty[Task]
    var s2 = Option.empty[(Task,Vector[Option[Entry]])]
    var s3 = Option.empty[(Task,Vector[Option[Entry]])]
    var pendingRsp = Option.empty[(Int,Vector[Option[Entry]])]
    var initializing = 64
    var localCycle = 0
    var trace: PrintWriter = null
    def take(b: BigInt, start: Int, width: Int): BigInt = (b>>start)&((BigInt(1)<<width)-1)
    def tick(e: BigInt = 0, send: Int = 0, label: String = "idle"): BigInt = {
      dut.io.event.poke(e.U); dut.io.send.poke(send.U)
      val packed = dut.io.packed.peek().litValue
      val sendRsp = take(packed,0,612); val rsp = take(packed,612,612)
      val set = take(packed,1224,6).toInt; val write = take(packed,1230,160)
      val states = take(packed,1390,192); val tasks = take(packed,1582,114)
      val status = take(packed,1696,28)
      def flag(n: Int) = status.testBit(n)
      if(trace != null) trace.println(s"{\"cycle\":$localCycle,\"event\":\"${e.toString(16)}\",\"send\":$send,\"audit\":\"${packed.toString(16)}\",\"label\":\"$label\"}")
      def check(ok: Boolean, what: String): Unit = if(!ok) {
        if(trace != null) trace.flush()
        sys.error(s"C2 model $what cycle=$localCycle label=$label status=${status.toString(16)}")
      }
      check(states == lru.zipWithIndex.foldLeft(BigInt(0)) { case (b,(x,i)) => b | (BigInt(x)<<(i*3)) }, "PLRU state")
      check(flag(14)==s1.isDefined && flag(15)==s2.isDefined && flag(16)==s3.isDefined, "stage valid timing")
      val expectedTasks = Seq(s1, s2.map(_._1), s3.map(_._1)).zipWithIndex.foldLeft(BigInt(0)) {
        case (b,(t,i)) => b | (t.map(_.bits).getOrElse(BigInt(0)) << (38*i))
      }
      check(tasks==expectedTasks,"valid stage payload")
      check(flag(9)==pendingRsp.isDefined,"SRAM one-cycle latency")
      totals("sram-latency-check") += 1
      check(rsp==pendingRsp.map(x=>response(x._2)).getOrElse(BigInt(0)),"SRAM valid/tag/all64sat")
      check(flag(10)==pendingRsp.exists(_._1==1),"response owner")
      totals("query-owner-check") += 1
      check(sendRsp==pendingRsp.filter(_._1==1).map(x=>response(x._2)).getOrElse(BigInt(0)),"sender response isolation")
      pendingRsp.foreach(x => totals(if(x._1==0) "train-response" else "send-response") += 1)
      val warm = initializing > 0
      check(flag(12) == !warm,"SRAM initialization/write-ready")
      if(warm) { totals("reset-block") += 1; check(!flag(7),"reset read blocked") }
      var expectedWrite = BigInt(0)
      var update = Option.empty[(Task,Int,Entry)]
      s3.foreach { case (t,row) =>
        val hit = row.indexWhere(_.exists(_.tag==t.tag))
        val root = (lru(t.set)>>2)&1
        val victim = root*2 + (if(root==1) (lru(t.set)>>1)&1 else lru(t.set)&1)
        val way = if(hit>=0) hit else victim
        if(hit>=0 || t.evict) {
          val sats = if(hit<0) Vector.tabulate(64)(i=>if(i==t.slot) {if(t.used) 1 else 3} else 2)
            else row(way).get.sat.zipWithIndex.map { case (v,i) =>
              if(i!=t.slot) v else if(t.used) math.max(0,v-1) else math.min(3,v+1)
            }
          val satBits = sats.zipWithIndex.foldLeft(BigInt(0)) { case(b,(v,i))=>b|(BigInt(v)<<(i*2)) }
          expectedWrite=(BigInt(t.set)<<154)|(BigInt(way)<<152)|(BigInt(t.tag)<<128)|satBits
          update=Some((t,way,Entry(t.tag,sats)))
          totals(if(hit<0) "evict-allocate" else if(t.used) "used-update" else "unused-update") += 1
          if(hit<0 && table(t.set)(way).isDefined) totals("replacement") += 1
          if(t.used && t.evict) totals(if(hit<0) "both-miss-init1" else "both-hit-decrement") += 1
          if(hit>=0) {
            val old=row(way).get.sat(t.slot)
            // Intent: distinguish a real positive decrement from zero saturation.
            if(t.used && t.evict && old>0) totals("both-hit-positive-decrement") += 1
            if(t.used && old==0) totals("saturate-zero") += 1
            if(!t.used && old==3) totals("saturate-three") += 1
          }
          if(t.slot==31 || t.slot==32) totals("sat-bank-boundary") += 1
        } else totals("used-miss-no-allocation") += 1
      }
      check(flag(11)==update.isDefined,"write valid / ordinary miss no allocation")
      check(write==expectedWrite,"write address/way/tag/saturation")
      if(flag(9) && flag(11)) totals("response-during-write") += 1
      if(flag(11)) {
        check(!flag(7) && !flag(6) && !flag(13),"write priority")
        if((send&64)!=0) totals("write-vs-send") += 1
        if(flag(5)) totals("write-vs-train-read") += 1
      }
      val eligible = Seq(98,99,100).forall(e.testBit) && (e.testBit(96)||e.testBit(97)) &&
        take(e,101,5).toInt!=MemReqSource.L1DataPrefetch.id
      val accepted = eligible && queue.size<8
      check(flag(21)==eligible && flag(0)==(queue.size<8) && flag(1)==accepted,"external qualification/admission")
      check(take(status,24,4).toInt==queue.size && flag(2)==queue.nonEmpty,"Queue8 occupancy")
      check(flag(22)==(eligible && queue.size==8),"Queue8 full drop")
      if(flag(22)) totals("queue-drop") += 1
      if(e.testBit(98) && !eligible) totals("qualification-excluded") += 1
      val inFlight = Seq(s1,s2.map(_._1),s3.map(_._1))
      val hazard = queue.headOption.exists(t=>inFlight.flatten.exists(_.same(t)))
      val ready = s1.isEmpty || (flag(6) && flag(12))
      check(flag(20)==ready,"stage1 backpressure")
      for(i<-0 until 3) check(flag(17+i)==queue.headOption.exists(t=>inFlight(i).exists(_.same(t))),"same-entry stage"+i)
      check(flag(3)==(queue.nonEmpty && ready && !hazard),"dequeue serialization")
      if(queue.nonEmpty) check(flag(4)==(ready && !hazard),"dequeue ready")
      if(hazard) {
        totals("same-entry-stall") += 1
        if(queue.headOption.exists(t=>inFlight.flatten.exists(x=>x.same(t)&&x.slot!=t.slot))) totals("same-entry-different-slot") += 1
      }
      check(flag(5)==s1.isDefined,"training query valid")
      if(flag(7)) {
        val owner=if(flag(8)) 1 else 0
        check(if(owner==0) s1.isDefined && flag(6) && set==s1.get.set
          else (send&64)!=0 && flag(13) && set==(send&63),"accepted query owner/set")
        pendingRsp=Some((owner,table(set)))
        if(s1.isDefined && (send&64)!=0) totals("dual-query-contention") += 1
      } else pendingRsp=None
      val nextS2 = if(flag(5) && flag(6)) {
        check(flag(7) && !flag(8),"pipeline query fire attribution")
        Some((s1.get, table(s1.get.set)))
      } else None
      val nextS1 = if(ready) { if(queue.nonEmpty && !hazard) Some(queue.head) else None } else s1
      s3=s2; s2=nextS2; s1=nextS1
      if(flag(3)) queue.dequeue()
      if(accepted) queue.enqueue(task(e))
      update.foreach { case(t,w,entry) =>
        check(flag(12),"write accepted")
        table(t.set)=table(t.set).updated(w,Some(entry))
        // Binary-tree PLRU model: the root points away from the touched half;
        // only the touched half's leaf changes. No DUT state used as expected.
        val half=w/2; val leaf=1-w%2
        lru(t.set)=(lru(t.set)&3)|((1-half)<<2)
        val pos=if(half==0) 0 else 1
        lru(t.set)=(lru(t.set)& ~(1<<pos))|(leaf<<pos)
      }
      if(warm) initializing-=1
      dut.clock.step(); cycles+=1; localCycle+=1; totals(label)+=1
      totals("actual-top-filter-check") += 1
      totals("filter-disabled-silent-check") += 1
      status
    }
    def idle(n:Int): Unit=(0 until n).foreach(_=>tick())
    def scan(): Unit = {
      idle(80)
      assert(queue.isEmpty && s1.isEmpty && s2.isEmpty && s3.isEmpty)
      for(set<-0 until 64) {
        val st=tick(send=64|set,label="full-set-scan")
        assert(st.testBit(7)&&st.testBit(8))
      }
      idle(2)
    }
    def scenario(name:String, warmEvent: BigInt = 0)(body: => Unit): Unit = {
      dut.io.event.poke(0.U); dut.io.send.poke(0.U)
      dut.reset.poke(true.B); dut.clock.step(6); dut.reset.poke(false.B)
      table=Array.fill(64)(Vector.fill[Option[Entry]](4)(None)); lru=Array.fill(64)(0)
      queue.clear(); s1=None; s2=None; s3=None; pendingRsp=None; initializing=64; localCycle=0
      trace=new PrintWriter(traceRoot.resolve(name+".jsonl").toFile)
      trace.println(s"{\"head\":\"$head\",\"scenario\":\"$name\"}")
      try {
        for(i<-0 until 64) tick(warmEvent,send=64|(i%64),label="initialization")
        tick()
        // Intent: a sole sender query establishes RR history via real fire,
        // independent of arbitrary lastGrant power-on state after each reset.
        val prime = tick(send=64,label="arbiter-prime")
        assert(prime.testBit(7) && prime.testBit(8), "sole sender must establish RR history")
        body; scan()
      } finally { trace.close(); trace=null }
    }
    def separated(e:BigInt,label:String):Unit={tick(e,send=64|5,label=label); (0 until 10).foreach(_=>tick(send=64|5))}
    scenario("initialization-queued-training", event(0,used=false,evict=true,victim=address(7,5,31))) { idle(80) }
    scenario("allocation-saturation-priority") {
      val a=address(7,5,31)
      separated(event(a),"ordinary-cold-hit-event")
      separated(event(a,used=false,evict=true,victim=a),"unused-evict-allocate")
      // Intent: cover identical hit/victim PA at a positive counter (3 -> 2),
      // in addition to the different-PA victim-selection test below.
      separated(event(a,evict=true,victim=a),"both-flags-same-entry-positive-decrement")
      separated(event(address(9,9,9),evict=true,victim=a),"both-flags-positive-decrement")
      for(i<-0 until 6) separated(event(a),"used-to-zero")
      // Both flags choose victim PA and USED wins, regardless of demand PA.
      separated(event(address(9,9,9),evict=true,victim=a),"both-flags-hit")
      for(i<-0 until 6) separated(event(a,used=false,evict=true,victim=a),"unused-to-three")
      separated(event(address(123,33,1),evict=true,victim=address(8,5,32)),"both-flags-miss")
      for(slot<-Seq(0,31,32,63)) {
        separated(event(address(8,5,slot)),"all-slot-banks")
        separated(event(address(8,5,slot,4095)),"same-4KB-page")
      }
    }
    scenario("serialization-replacement-rw-conflicts") {
      for(i<-0 until 200) {
        val pa=address(if(i%4<2) 3 else i%11,5, i%64)
        tick(event(pa,used=i%3==0,evict=i%3!=0,victim=pa),send=64|5,label="queued-conflicts")
      }
      for(tag<-0 until 12) separated(event(0,used=false,evict=true,victim=address(tag,5,0)),"same-set-replacement")
      for(i<-0 until 80) tick(event(address(1,5,i%64)),send=64|5,label="same-entry-slot-serialization")
    }
    scenario("eligibility-disable-drain") {
      for(i<-0 until 32) tick(event(address(1,5,0),used=false,evict=true,victim=address(1,5,0)),label="fill-queue")
      for(i<-0 until 64) tick(event(address(1,5,0),enable=false),label="disabled-drain")
      for(i<-0 until 8) tick(event(0,train=false),label="train-disabled")
      for(i<-0 until 8) tick(event(0,req=MemReqSource.L1DataPrefetch.id),label="l1-prefetch-excluded")
      for(i<-0 until 8) tick(event(0,used=false,evict=false),label="no-filter-flags")
    }
    for(seed<-0 until seeds) {
      val r=new Random(seed.toLong ^ 0xc2f17L)
      scenario(s"seed$seed") {
        for(i<-0 until randomCycles) {
          val pa=if(i%8<6) address(r.nextInt(8),r.nextInt(4),r.nextInt(64),r.nextInt(4096)) else BigInt(48,r)
          val victim=if(i%5==0) BigInt(48,r) else pa
          tick(event(pa,used=r.nextBoolean(),evict=r.nextBoolean(),victim=victim,
            enable=r.nextInt(16)!=0,train=r.nextInt(16)!=0,req=r.nextInt(32),valid=r.nextInt(8)!=0),
            send=if(r.nextInt(4)!=0) 64|r.nextInt(64) else 0,label="random")
        }
      }
      totals("random-seeds")+=1
    }
  }
  for(name<-Seq("reset-block","train-response","send-response","evict-allocate","used-update","unused-update",
      "used-miss-no-allocation","both-hit-decrement","both-miss-init1","saturate-zero","saturate-three",
      "both-hit-positive-decrement","both-flags-same-entry-positive-decrement",
      "same-entry-stall","same-entry-different-slot","queue-drop","qualification-excluded","replacement",
      "sat-bank-boundary","response-during-write","write-vs-send","write-vs-train-read","dual-query-contention","full-set-scan"))
    assert(totals(name)>0,s"missing C2 required coverage $name")
  totals.toSeq.sortBy(_._1).foreach { case(n,c)=>println(s"CDP_C2_CASE name=$n count=$c matched=$c") }
  println(s"CDP_C2_RESULT pass=true cycles=$cycles matched=$cycles seeds=$seeds mismatches=0 host_model=true")
}
