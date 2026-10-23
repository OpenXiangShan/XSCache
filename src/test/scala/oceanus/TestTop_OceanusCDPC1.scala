// Intent (C1, before implementation): compare the unedited ReferenceCDP VPN
// table/pipeline with the production pair on every clock. Test-only subclasses
// expose masked state without adding production debug ports. A host table
// model independently checks recurrence, truncation and refresh precedence.
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

// Intent: invalid metadata/tag registers are deliberately not reset in either
// implementation. Mask them at the observation boundary, never in the DUT.
class CDPC1TableProbe(implicit p: Parameters) extends OceanusCDPVpnTable {
  val snapshot = IO(Output(UInt(1600.W)))
  val tags = VecInit((0 until 4).flatMap(s => (0 until 4).map(w =>
    Mux(validArray(s)(w).asUInt.orR, tagArray(s)(w), 0.U(10.W)))))
  val metas = VecInit((0 until 4).flatMap(s => (0 until 4).flatMap(w => (0 until 4).map(u =>
    Mux(validArray(s)(w)(u), metaArray(s)(w)(u).asUInt, 0.U(21.W))))))
  snapshot := Cat(refreshCnt, validArray.asUInt, tags.asUInt, metas.asUInt)
  val refreshing = IO(Output(Bool()))
  refreshing := isRefresh
}
class CDPC1ReferenceTableProbe(implicit p: Parameters) extends ref.VpnTable {
  val snapshot = IO(Output(UInt(1600.W)))
  val tags = VecInit((0 until 4).flatMap(s => (0 until 4).map(w =>
    Mux(validArray(s)(w).asUInt.orR, tagArray(s)(w), 0.U(10.W)))))
  val metas = VecInit((0 until 4).flatMap(s => (0 until 4).flatMap(w => (0 until 4).map(u =>
    Mux(validArray(s)(w)(u), metaArray(s)(w)(u).asUInt, 0.U(21.W))))))
  snapshot := Cat(refreshCnt, validArray.asUInt, tags.asUInt, metas.asUInt)
  val refreshing = IO(Output(Bool()))
  refreshing := isRefresh
}
class CDPC1PipelineProbe(implicit p: Parameters) extends OceanusCDPVtTrainPipeline {
  val audit = IO(Output(UInt(21.W)))
  audit := Cat(replacer.asInstanceOf[SetAssocLRU].state_vec.asUInt, sameVec.asUInt, stage_valid.asUInt)
  val victims = IO(Output(Vec(4, UInt(2.W))))
  for (s <- 0 until 4) victims(s) := replacer.way(s.U)
  // Intent: compare all valid stage address/weight payloads, not stale bits.
  val stages = IO(Output(Vec(4, UInt(19.W))))
  stages(0) := Mux(stage_valid(1), Cat(s1_main_idx, s1_sub_idx, s1_tag, s1_is_hit_cdp), 0.U)
  stages(1) := Mux(stage_valid(2), Cat(s2_main_idx, s2_sub_idx, s2_tag, s2_is_hit_cdp), 0.U)
  stages(2) := Mux(stage_valid(3), s3_update_info.asUInt, 0.U)
  stages(3) := Mux(stage_valid(4), s4_update_info.asUInt, 0.U)
}
class CDPC1ReferencePipelineProbe(implicit p: Parameters) extends ref.VtTrainPipeline {
  val audit = IO(Output(UInt(21.W)))
  audit := Cat(replacer.asInstanceOf[SetAssocLRU].state_vec.asUInt, sameVec.asUInt, stage_valid.asUInt)
  val victims = IO(Output(Vec(4, UInt(2.W))))
  for (s <- 0 until 4) victims(s) := replacer.way(s.U)
  val stages = IO(Output(Vec(4, UInt(19.W))))
  stages(0) := Mux(stage_valid(1), Cat(s1_main_idx, s1_sub_idx, s1_tag, s1_is_hit_cdp), 0.U)
  stages(1) := Mux(stage_valid(2), Cat(s2_main_idx, s2_sub_idx, s2_tag, s2_is_hit_cdp), 0.U)
  stages(2) := Mux(stage_valid(3), s3_update_info.asUInt, 0.U)
  stages(3) := Mux(stage_valid(4), s4_update_info.asUInt, 0.U)
}

class CDPC1Harness(period: Int, hash: Boolean)(implicit p: Parameters) extends Module {
  val prodParams = p.alterPartial { case OceanusCDPKey => OceanusCDPParameters(vpnResetPeriod = period, useVpnTableHashIndex = hash) }
  val refParams = p.alterPartial { case ref.ReferenceCDPKey => ref.CDPParameters(vpnResetPeriod = period, useVpnTableHashIndex = hash) }
  val io = IO(new Bundle {
    val event = Input(UInt(107.W))
    val queries = Input(UInt(16.W))
    val direct = Input(Bool())
    val directWrite = Input(UInt(20.W))
    val snapshot = Output(UInt(1600.W))
    val activity = Output(UInt(50.W))
  })
  val table = Module(new CDPC1TableProbe()(prodParams))
  val pipe = Module(new CDPC1PipelineProbe()(prodParams))
  val referenceTable = Module(new CDPC1ReferenceTableProbe()(refParams))
  val referencePipe = Module(new CDPC1ReferencePipelineProbe()(refParams))
  val queue = Module(new Queue(new OceanusCDPTrain()(prodParams), 8))
  val referenceQueue = Module(new Queue(new ref.PrefetchTrain, 8))

  // Intent: this mapping supplies the same qualified GLOBAL event to each
  // subsystem. Original CDP :1471-1476 supplies the reference Queue semantics;
  // needHint/reqSource filtering belongs to the explicitly shown outer adapter.
  val e = io.event
  val globalEvent = WireInit(0.U.asTypeOf(new L2CDPTrain))
  globalEvent.vaddr := e(43, 0); globalEvent.hit := e(44)
  globalEvent.pfSource := e(48, 45); globalEvent.reqSource := e(54, 50)
  globalEvent.trainEnable := e(55); globalEvent.paddr := e(106, 59)
  globalEvent.pfDepth := e(1, 0) // explicitly irrelevant to VPN weighting
  val t = WireInit(OceanusCDPTrain.fromVpnEvent(globalEvent)(prodParams))
  // Exercise the local explicit qualification bit and the unused prefetch bit
  // independently of the global adapter's deterministic defaults.
  t.prefetched := e(49); t.vpnValid := e(56)
  queue.io.enq.valid := e(58) && e(57) && t.vpnValid && t.trainEnable &&
    t.reqSource =/= MemReqSource.L1DataPrefetch.id.U && !io.direct
  queue.io.enq.bits := t
  val rt = WireInit(0.U.asTypeOf(new ref.PrefetchTrain))
  rt.vaddr.get := e(43, 0); rt.hit := e(44); rt.pfsource := e(48, 45)
  rt.prefetched := e(49); rt.reqsource := e(54, 50)
  rt.cdp_vpn_train_valid.get := e(56)
  rt.tag := e(106, 71); rt.set := e(70, 65)
  referenceQueue.io.enq.valid := e(58) && e(57) && e(55) &&
    rt.cdp_vpn_train_valid.get && rt.reqsource =/= MemReqSource.L1DataPrefetch.id.U && !io.direct
  referenceQueue.io.enq.bits := rt
  pipe.io.trainTrigger <> queue.io.deq
  referencePipe.io.trainTrigger <> referenceQueue.io.deq
  table.io.queryReq(4) := pipe.io.queryReq
  pipe.io.queryRsp := table.io.queryRsp(4)
  referenceTable.io.queryReq(4) := referencePipe.io.queryReq
  referencePipe.io.queryRsp := referenceTable.io.queryRsp(4)
  table.io.trainReq.valid := Mux(io.direct, io.directWrite(19), pipe.io.trainReq.valid)
  table.io.trainReq.bits := Mux(io.direct, io.directWrite(18, 0).asTypeOf(table.io.trainReq.bits), pipe.io.trainReq.bits)
  referenceTable.io.trainReq.valid := Mux(io.direct, io.directWrite(19), referencePipe.io.trainReq.valid)
  referenceTable.io.trainReq.bits := Mux(io.direct, io.directWrite(18, 0).asTypeOf(referenceTable.io.trainReq.bits), referencePipe.io.trainReq.bits)
  for (i <- 0 until 4) {
    table.io.queryReq(i).valid := e(58 - i)
    table.io.queryReq(i).bits.mainIdx := io.queries(i*4+3, i*4+2)
    table.io.queryReq(i).bits.subIdx := io.queries(i*4+1, i*4)
    referenceTable.io.queryReq(i) := table.io.queryReq(i).asTypeOf(referenceTable.io.queryReq(i))
  }
  // Intent: also drive the actual production OceanusCDP wrapper. Bore only
  // from test code so its real admission/Queue wiring is covered, including
  // enable changes and drain, without production observation ports.
  val productionTop = Module(new OceanusCDP(OceanusCDPParameters(
    // Intent (C2): this VPN-only multi-elaboration test omits Filter SRAM;
    // C2's dedicated harness verifies the real top with Filter on and off.
    vpnResetPeriod = period, useVpnTableHashIndex = hash, useFilterTable = false)))
  productionTop.io.enable := e(57)
  productionTop.io.vpnTrain.valid := e(58) && !io.direct
  productionTop.io.vpnTrain.bits := t
  productionTop.io.filterTrain.valid := false.B
  productionTop.io.filterTrain.bits := 0.U.asTypeOf(productionTop.io.filterTrain.bits)
  productionTop.io.l2DetectTriggers := 0.U.asTypeOf(productionTop.io.l2DetectTriggers)
  productionTop.io.pfStat := 0.U.asTypeOf(productionTop.io.pfStat)
  productionTop.io.tlb.req.ready := true.B
  productionTop.io.tlb.resp.valid := false.B
  productionTop.io.tlb.resp.bits := 0.U.asTypeOf(productionTop.io.tlb.resp.bits)
  productionTop.io.req.ready := true.B
  def read[T <: Data](signal: T): T = chisel3.util.experimental.BoringUtils.bore(signal)
  val topValid = read(productionTop.vpnTable.validArray)
  val topTags = read(productionTop.vpnTable.tagArray)
  val topMeta = read(productionTop.vpnTable.metaArray)
  val topRefresh = read(productionTop.vpnTable.refreshCnt)
  val topMode = read(productionTop.vtTrainPipe.io.addrState)
  val topQueueCount = read(productionTop.vpnTrainReqBuf.io.count)
  val topMaskedTags = VecInit((0 until 4).flatMap(s => (0 until 4).map(w =>
    Mux(topValid(s)(w).asUInt.orR, topTags(s)(w), 0.U(10.W)))))
  val topMaskedMeta = VecInit((0 until 4).flatMap(s => (0 until 4).flatMap(w => (0 until 4).map(u =>
    Mux(topValid(s)(w)(u), topMeta(s)(w)(u).asUInt, 0.U(21.W))))))
  val topSnapshot = Cat(topRefresh, topValid.asUInt, topMaskedTags.asUInt, topMaskedMeta.asUInt)
  val directSeen = RegInit(false.B)
  when(io.direct) { directSeen := true.B }
  val warm = RegInit(0.U(3.W))
  when(warm < 6.U) { warm := warm + 1.U }
  when(warm === 6.U && !reset.asBool) {
    assert(!productionTop.io.req.valid && !productionTop.io.tlb.req.valid, "C1 generated a request")
    when(!io.direct && !directSeen) {
      assert(topSnapshot === table.snapshot, "C1 production wrapper state mismatch")
      assert(topMode.asUInt === pipe.io.addrState.asUInt)
      assert(topQueueCount === queue.io.count)
    }
    assert(table.snapshot === referenceTable.snapshot, "C1 table valid/tag/counters/hot/refresh mismatch")
    assert(table.refreshing === referenceTable.refreshing)
    // Intent (C1): invalid Queue payload may differ between Bundle shapes.
    // Stage validity and PLRU are always defined; sameVec/ready depend on the
    // candidate address and are meaningful only while that candidate is valid.
    assert(pipe.audit(20, 9) === referencePipe.audit(20, 9), "C1 PLRU state mismatch")
    assert(pipe.audit(4, 0) === referencePipe.audit(4, 0), "C1 stage-valid mismatch")
    when(queue.io.deq.valid) {
      assert(pipe.audit(8, 5) === referencePipe.audit(8, 5), "C1 same-main hazard mismatch")
      assert(pipe.io.trainTrigger.ready === referencePipe.io.trainTrigger.ready)
    }
    assert(pipe.stages.asUInt === referencePipe.stages.asUInt, "C1 valid stage payload mismatch")
    assert(pipe.victims.asUInt === referencePipe.victims.asUInt, "C1 replacement candidate mismatch")
    assert(pipe.io.addrState.asUInt === referencePipe.io.addrState.asUInt, "C1 address mode mismatch")
    assert(queue.io.enq.ready === referenceQueue.io.enq.ready && queue.io.count === referenceQueue.io.count)
    assert(queue.io.deq.valid === referenceQueue.io.deq.valid)
    assert(pipe.io.queryReq.valid === referencePipe.io.queryReq.valid)
    when(pipe.io.queryReq.valid) { assert(pipe.io.queryReq.bits.asUInt === referencePipe.io.queryReq.bits.asUInt) }
    assert(pipe.io.trainReq.valid === referencePipe.io.trainReq.valid)
    when(pipe.io.trainReq.valid) { assert(pipe.io.trainReq.bits.asUInt === referencePipe.io.trainReq.bits.asUInt) }
    for (i <- 0 until 5; w <- 0 until 4) {
      val a = table.io.queryRsp(i); val b = referenceTable.io.queryRsp(i)
      // Intent (C1): the four external query addresses are explicitly driven,
      // whereas invalid pipeline query addresses originate in unreset payload.
      when((i < 4).B || pipe.io.queryReq.valid) {
        assert(a.mainValidVec(w) === b.mainValidVec(w) && a.subValidVec(w) === b.subValidVec(w))
        when(a.mainValidVec(w)) { assert(a.tagVec(w) === b.tagVec(w)) }
        when(a.subValidVec(w)) { assert(a.metaVec(w).asUInt === b.metaVec(w).asUInt) }
      }
    }
  }
  io.snapshot := table.snapshot
  io.activity := Cat(pipe.audit(4, 0), queue.io.count, queue.io.enq.valid,
    queue.io.deq.fire, pipe.audit(20, 9), pipe.io.addrState.asUInt,
    queue.io.enq.valid && !queue.io.enq.ready, queue.io.enq.fire,
    queue.io.deq.valid && !queue.io.deq.ready, table.refreshing,
    table.io.trainReq.valid, table.io.trainReq.bits.asUInt)
}

object TestTop_OceanusCDPC1 extends App with ChiselSim {
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
  def option(name: String, default: Int) = args.find(_.startsWith(name + "=")).map(_.split("=")(1).toInt).getOrElse(default)
  val seeds = option("--seeds", 500)
  val randomCycles = option("--cycles", 512)
  val traceRoot = Paths.get(sys.props.getOrElse("oceanus.c1.trace", "/data/zhuy/pf-framework/c1/traces"))
  Files.createDirectories(traceRoot)
  val sourceHead = sys.props.getOrElse("oceanus.c1.head", sys.error("run via scripts/run-pf-c1.py diff STAGE to record HEAD"))
  val totals = mutable.Map.empty[String, Long].withDefaultValue(0L)
  var cycles = 0L
  // Independent host representation: one optional record per sub-entry; use
  // rational EWMA and integer modulo, not DUT helpers or a DUT state readback.
  case class Meta(refs: Int, previous: Int, hot: Boolean)
  class TableModel(val period: Int) {
    val entries = Array.fill[Option[Meta]](64)(None)
    val tags = Array.fill(16)(0)
    var refresh = 0
    def bits: BigInt = {
      var metas = BigInt(0); var valid = BigInt(0); var tagBits = BigInt(0)
      for (i <- entries.indices; m <- entries(i)) {
        valid = valid.setBit(i)
        val word = BigInt(m.refs) | (BigInt(m.previous) << 10) | (if(m.hot) BigInt(1) << 20 else BigInt(0))
        metas |= word << (21*i)
      }
      for (i <- tags.indices if (0 until 4).exists(u => entries(i*4+u).isDefined)) tagBits |= BigInt(tags(i)) << (10*i)
      (BigInt(refresh) << 1568) | (valid << 1504) | (tagBits << 1344) | metas
    }
    def step(write: Int, category: String): Unit = {
      def count(n: String): Unit = totals(n) += 1
      val valid = (write & (1 << 19)) != 0
      if (refresh >= period) {
        count("refresh"); if(valid) count("refresh-overlap")
        refresh = 0
        for(i <- entries.indices; m <- entries(i)) {
          val weighted = (13*m.refs + 3*m.previous) / 16
          count(if(weighted > 2) "hot-true" else "hot-false")
          entries(i) = Some(Meta(0, weighted % 1024, weighted > 2))
        }
      } else if(valid) {
        refresh += 1
        val main = (write >> 13) & 3; val way = (write >> 15) & 3
        val sub = (write >> 11) & 3; val tag = (write >> 1) & 1023
        val allocMain = (write & (1 << 18)) != 0; val allocSub = (write & (1 << 17)) != 0
        val increment = if((write & 1) != 0) 4 else 1
        val idx = (main*4+way)*4+sub
        if(allocMain) {
          count("alloc-main")
          if((0 until 4).exists(u => entries((main*4+way)*4+u).isDefined)) count("replacement")
          tags(main*4+way) = tag
          for(u <- 0 until 4) entries((main*4+way)*4+u) = None
          entries(idx) = Some(Meta(increment, 0, false))
        } else if(allocSub) {
          count("alloc-sub"); entries(idx) = Some(Meta(increment, 0, false))
        } else {
          count(if(increment == 4) "plus4" else "plus1")
          // A stale query may target a sub-entry invalidated by a later
          // in-flight replacement. Such metadata remains unobservable.
          entries(idx).foreach { old =>
            if(old.refs + increment >= 1024) count("wrap")
            entries(idx) = Some(old.copy(refs = (old.refs + increment) % 1024))
          }
        }
      }
    }
  }
  def event(va: BigInt, hit: Boolean = false, source: Int = 0, prefetched: Boolean = false,
            enable: Boolean = true, train: Boolean = true, vpn: Boolean = true,
            req: Int = MemReqSource.CPULoadData.id, valid: Boolean = true, pa: BigInt = 0): BigInt = {
    var e = (va >> 6) | (BigInt(source) << 45) | (BigInt(req) << 50) | (pa << 59)
    for((flag, bit) <- Seq(hit -> 44, prefetched -> 49, train -> 55, vpn -> 56, enable -> 57, valid -> 58) if flag) e = e.setBit(bit)
    e
  }
  def raw(main: Int, sub: Int, way: Int, tag: Int, allocMain: Boolean = false,
          allocSub: Boolean = false, cdp: Boolean = false): Int =
    (1 << 19) | (if(allocMain) 1 << 18 else 0) | (if(allocSub) 1 << 17 else 0) |
      (way << 15) | (main << 13) | (sub << 11) | (tag << 1) | (if(cdp) 1 else 0)
  for((period, hash) <- Seq((128, true), (128, false), (2048, true))) {
    simulate(new CDPC1Harness(period, hash)(params)) { dut =>
      var model = new TableModel(period)
      var trace: PrintWriter = null
      var localCycle = 0
      var modeExpected = 4 // Bundle.asUInt order is isSv39, isSv48, isSv57.
      var lastLabel = ""
      var inFlight = Vector.fill(4)(Option.empty[(Int, Int)])
      def key(blockVA: BigInt): (Int, Int) = {
        val va = blockVA << 6
        val upper = va >> 25
        def fold(width: Int): Int = (0 until upper.bitLength).filter(upper.testBit)
          .foldLeft(0)((v, bit) => v ^ (1 << (bit % width)))
        ((((va >> 23) & 3).toInt ^ (if(hash) fold(2) else 0)), fold(10))
      }
      def reset(): Unit = {
        dut.io.event.poke(0.U); dut.io.queries.poke(0.U)
        dut.io.direct.poke(false.B); dut.io.directWrite.poke(0.U)
        dut.reset.poke(true.B); dut.clock.step(6); dut.reset.poke(false.B)
        model = new TableModel(period); modeExpected = 4; localCycle = 0
        inFlight = Vector.fill(4)(Option.empty[(Int, Int)])
        dut.clock.step(6)
      }
      def tick(e: BigInt = 0, direct: Boolean = false, write: Int = 0, label: String = "idle"): Unit = {
        dut.io.event.poke(e.U); dut.io.direct.poke(direct.B); dut.io.directWrite.poke(write.U)
        dut.io.queries.poke(((localCycle*7919) & 65535).U)
        val activity = dut.io.activity.peek().litValue
        val actual = dut.io.snapshot.peek().litValue
        // Intent: record the offending input/state before any checker can
        // throw, so failure traces retain a replayable complete prefix.
        val effectiveWrite = (activity & ((BigInt(1) << 20)-1)).toInt
        if(trace != null) trace.println(s"{\"cycle\":$localCycle,\"event\":\"${e.toString(16)}\",\"direct\":$direct,\"directWrite\":$write,\"write\":$effectiveWrite,\"activity\":\"${activity.toString(16)}\",\"state\":\"${actual.toString(16)}\",\"category\":\"$label\"}")
        if(actual != model.bits) {
          if(trace != null) trace.flush()
          sys.error(s"C1 host model mismatch period=$period hash=$hash label=$label cycle=$localCycle actual=${actual.toString(16)} expected=${model.bits.toString(16)}")
        }
        assert(((activity >> 24) & 7).toInt == modeExpected, s"C1 mode mismatch $lastLabel $label cycle=$localCycle")
        // On deq.fire the independently queued VA determines the next mode;
        // the queue trace below is maintained only from external admission.
        val accepted = activity.testBit(22); val drop = activity.testBit(23)
        val eligible = !direct && Seq(55, 56, 57, 58).forall(e.testBit) &&
          ((e >> 50) & 31).toInt != MemReqSource.L1DataPrefetch.id
        assert(activity.testBit(40) == eligible, s"admission qualification $label")
        assert(accepted == (eligible && expectedQueue.size < 8), s"Queue8 admission $label")
        assert(drop == (eligible && expectedQueue.size == 8), s"Queue8 drop $label")
        val headKey = expectedQueue.headOption.map(key)
        val stalled = headKey.exists(k => inFlight.contains(Some(k)))
        val deqFire = expectedQueue.nonEmpty && !stalled
        assert(activity.testBit(21) == stalled && activity.testBit(39) == deqFire, s"same-main serialization $label")
        val expectedStages = (if(deqFire) 1 else 0) | inFlight.zipWithIndex.foldLeft(0) {
          case (bits, (entry, index)) => bits | (if(entry.isDefined) 1 << (index+1) else 0)
        }
        assert(((activity >> 45) & 31).toInt == expectedStages, s"stage timing $label")
        inFlight = (if(deqFire) headKey else None) +: inFlight.take(3)
        if(!direct && activity.testBit(20) && activity.testBit(19)) totals("pipeline-refresh-overlap") += 1
        totals("accepted") += (if(accepted) 1 else 0); totals("queue-drop") += (if(drop) 1 else 0)
        totals("same-main-stall") += (if(activity.testBit(21)) 1 else 0)
        model.step(effectiveWrite, label)
        // The small independent FIFO also checks accepted payload ordering and
        // the monotonic address-range classifier, including disable + drain.
        if(activity.testBit(39)) {
          assert(expectedQueue.nonEmpty)
          val va = expectedQueue.dequeue() << 6
          if((va >> 48) != 0) modeExpected = 1
          else if((va >> 39) != 0 && modeExpected == 4) modeExpected = 2
        }
        if(accepted) expectedQueue.enqueue(e & ((BigInt(1) << 44)-1))
        assert(((activity >> 41) & 15).toInt == expectedQueue.size + (if(activity.testBit(39)) 1 else 0) - (if(accepted) 1 else 0))
        dut.clock.step()
        cycles += 1; localCycle += 1; totals(label) += 1; lastLabel = label
      }
      def idle(n: Int): Unit = (0 until n).foreach(_ => tick())
      def separated(e: BigInt, label: String): Unit = { tick(e, label = label); idle(7) }
      // Intent: keep the complete external event trace and matched state for
      // each reset-delimited scenario. Failure leaves its exact prefix on NVMe.
      def scenario(name: String)(body: => Unit): Unit = {
        expectedQueue.clear(); reset()
        trace = new PrintWriter(traceRoot.resolve(s"p${period}-h${hash}-$name.jsonl").toFile)
        trace.println(s"{\"head\":\"$sourceHead\",\"scenario\":\"$name\",\"period\":$period,\"hash\":$hash}")
        try { body; idle(64); assert(expectedQueue.isEmpty) }
        finally { trace.close(); trace = null }
      }
      scenario("ordinary-weight-subentry") {
        separated(event(0x201000), "ordinary")
        separated(event(0x201040), "ordinary")
        separated(event(0x201080, hit = true, source = 9, prefetched = false), "cdp-hit")
        separated(event(0x2010c0, hit = false, source = 9, prefetched = true), "nonhit-source9")
        separated(event(0x201100, hit = true, source = 2, prefetched = true), "noncdp-hit")
        for(u <- 0 until 4) separated(event(BigInt(u) << 21), "alloc-sub")
      }
      if(period == 128) {
        scenario("refresh-hot") {
          for(i <- 0 until 260) separated(event(0x201000, hit = i%3 == 0, source = 9), "default-refresh")
          assert(model.entries.flatten.exists(_.hot))
        }
        scenario("hot-threshold") {
          tick(direct = true, write = raw(0, 0, 0, 7, allocMain = true), label = "hot-threshold")
          for(i <- 0 until 2) tick(direct = true, write = raw(0, 0, 0, 7), label = "hot-threshold")
          tick(direct = true, write = raw(0, 1, 0, 7, allocSub = true, cdp = true), label = "hot-threshold")
          tick(direct = true, write = raw(0, 2, 0, 7, allocSub = true), label = "hot-threshold")
          for(i <- 0 until 123) tick(direct = true, write = raw(0, 2, 0, 7), label = "hot-threshold")
          idle(2)
          assert(model.entries(0).contains(Meta(0, 2, false)))
          assert(model.entries(1).contains(Meta(0, 3, true)))
        }
        scenario("refresh-overlap") {
          for(i <- 0 until 130) tick(direct = true,
            write = raw(0, 0, 0, 7, allocMain = i == 0, cdp = i%2 == 0), label = "refresh-overlap-directed")
        }
      } else {
        scenario("wrap10") {
          // Real pipeline traffic, no force/deposit of counter state.
          for(i <- 0 until 300) separated(event(0x201000, hit = true, source = 9), "wrap10")
          assert(model.entries.flatten.exists(_.refs == 176)) // 300*4 mod 1024
        }
      }
      scenario("conflict-replacement") {
        for(i <- 0 until 160) {
          // Consecutive different tags in one set exercise delayed PLRU and
          // snapshots; same main/tag, different sub tests serialization.
          val tag = i%11
          var va = BigInt(tag) << 25
          if(hash) {
            val folded = (0 until 25).filter(b => ((tag >> b)&1) != 0).foldLeft(0)((v,b) => v ^ (1 << (b%2)))
            va |= BigInt(folded) << 23
          }
          tick(event(va | (BigInt(i%4) << 21), hit = i%2 == 0, source = 9), label = "conflict-replacement")
        }
        for(i <- 0 until 80) tick(event(BigInt(i%4) << 21), label = "same-main-different-sub")
      }
      scenario("address-modes-admission") {
        separated(event((BigInt(1)<<39)-64), "sv39")
        separated(event(BigInt(1)<<39), "sv48")
        separated(event(0), "no-downgrade")
        separated(event(BigInt(1)<<48), "sv57")
        separated(event(0), "no-downgrade")
        for(i <- 0 until 32) tick(event(BigInt(1)<<49, enable = false), label = "disabled")
        for(i <- 0 until 8) tick(event(0x8000000, train = false), label = "train-disabled")
        for(i <- 0 until 8) tick(event(0x8000000, vpn = false), label = "vpn-disabled")
        for(i <- 0 until 8) tick(event(0x8000000, req = MemReqSource.L1DataPrefetch.id), label = "l1-prefetch-excluded")
      }
      if(period == 128 && hash) {
        for(seed <- 0 until seeds) {
          val random = new Random(seed.toLong ^ 0xc1d1ffL)
          scenario(s"seed$seed") {
            for(i <- 0 until randomCycles) {
              val va = if(i%17 < 8) (BigInt(random.nextInt(4)) << 21) | 0x1000
                else BigInt(50, random) & ~BigInt(63)
              tick(event(va, hit = random.nextBoolean(), source = random.nextInt(10),
                prefetched = random.nextBoolean(), enable = random.nextInt(16) != 0,
                train = random.nextInt(16) != 0, vpn = random.nextInt(16) != 0,
                req = random.nextInt(32), valid = random.nextInt(8) != 0, pa = BigInt(48, random)), label = "random")
            }
          }
          totals("random-seeds") += 1
        }
      }
      println(s"CDP_C1_CONFIG pass=true period=$period hash=$hash seeds=${if(period==128 && hash) seeds else 0}")
    }
  }
  for(name <- Seq("plus1", "plus4", "refresh", "refresh-overlap", "wrap", "alloc-main", "alloc-sub", "replacement", "same-main-stall", "queue-drop", "pipeline-refresh-overlap", "hot-true", "hot-false"))
    assert(totals(name) > 0, s"missing required coverage $name")
  assert(totals("random-seeds") >= seeds)
  totals.toSeq.sortBy(_._1).foreach { case (name, count) => println(s"CDP_C1_CASE name=$name count=$count matched=$count") }
  println(s"CDP_C1_RESULT pass=true cycles=$cycles matched=$cycles seeds=$seeds mismatches=0 host_table_model=true")
  // Shared only within the host driver, reset between every scenario/seed.
  // Declaration initialization is above simulate execution (see private def).
  def expectedQueue: mutable.Queue[BigInt] = CDPC1HostQueue.queue
}
private object CDPC1HostQueue { val queue = mutable.Queue.empty[BigInt] }
