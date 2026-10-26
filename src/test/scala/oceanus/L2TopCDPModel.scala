package oceanus

// Intent (1c-2): compare production scans against address-derived memory,
// independently model each lossy FIFO, and check old Meta, forwarding and
// disabled-event totals before any training arbitration can lose an event.

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.ChiselSim
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import scala.collection.mutable

class L2CDPProbe(implicit p: Parameters) extends Bundle {
  val line = Valid(new L2CDPLine)
  val start = Valid(new L2CDPContext)
  val train = Valid(new L2CDPTrain)
  val evict = Valid(new L2CDPEvict)
  val complete = Bool()
  val dataStart = Bool()
  val deallocate = Bool()
  val prefetchActive = Bool()
  val prefetchSource = UInt(4.W)
  val enterREQ = Bool()
  val reuseREQ = Bool()
  val incomingSource = UInt(16.W)
  val prefetchSourceID = UInt(16.W)
  val incomingOpcode = UInt(8.W)
  val incomingData = Bool()
  val ds = Bool()
  val execute = Bool()
  val addr = UInt(48.W)
  val oldSource = UInt(4.W)
  val oldDepth = UInt(2.W)
  val hit = Bool()
  val consume = Bool()
  val writeSource = Bool()
  val newSource = UInt(4.W)
  val newDepth = UInt(2.W)
  val dirRead = Bool()
  val dirSource = UInt(4.W)
  val dirDepth = UInt(2.W)
}

class L2CDPObservation(slices: Int, entries: Int)(implicit p: Parameters) extends Bundle {
  val req = Output(Vec(slices, Vec(entries, new L2CDPProbe)))
  val data = Output(Valid(new L2CDPHalf))
  val chosen = Output(UInt((log2Ceil(slices) max 1).W))
  val drops = Output(Vec(slices, UInt(64.W)))
  val collisions = Output(Vec(slices, UInt(64.W)))
  val disabled = Output(UInt(64.W))
}

object L2TopCDPObservation {
  def attach(top: L2Top, entries: Int)(implicit p: Parameters): L2CDPObservation = {
    def read[T <: Data](signal: T): T = BoringUtils.bore(signal)
    val obs = IO(new L2CDPObservation(top.config.sliceNum, entries))
    obs.data := read(top.cdpData)
    obs.chosen := read(top.cdpDataSlice)
    obs.drops := read(top.cdpHalfDrops)
    obs.collisions := read(top.cdpCollisionDrops)
    obs.disabled := read(top.trainEnableZeroCount)
    top.slices.zipWithIndex.foreach { case (slice, bank) =>
      slice.tshrs.zipWithIndex.foreach { case (tshr, entry) =>
        val probe = obs.req(bank)(entry)
        val req = tshr.vPipeREQ
        probe.line := read(tshr.io.cdpData)
        probe.start := read(req.io.cdpStart)
        probe.train := read(req.io.cdpTrain)
        probe.evict := read(req.io.cdpEvict)
        probe.complete := read(tshr.tshr_buffer_wen_last)
        probe.dataStart := read(tshr.tshr_buffer_wen_RXDAT_0) || read(tshr.tshr_buffer_wen_RXDAT_2)
        probe.deallocate := read(tshr.tshr_dealloc)
        probe.prefetchActive := read(req.io.bopPerf.activePrefetch)
        probe.prefetchSource := read(req.io.bopPerf.activePrefetchSource)
        probe.enterREQ := read(tshr.tshr_enter_REQ)
        probe.reuseREQ := read(tshr.io.fromAlloc.reuse.REQ)
        probe.incomingSource := read(tshr.io.UpRXREQ.SrcID)
        probe.prefetchSourceID := read(tshr.io.consts.pfSrcID)
        probe.incomingOpcode := read(tshr.io.UpRXREQ.Opcode)
        probe.incomingData := read(tshr.io.UpRXREQ.ExpCompData)
        probe.ds := read(tshr.tshr_buffer_wen_DS)
        probe.execute := read(req.io.UpRXREQ.valid)
        probe.addr := read(req.io.tshr_paddr)
        probe.oldSource := read(req.io.tshr_dirResult.pfSource)
        probe.oldDepth := read(req.io.tshr_dirResult.cdpPfDepth)
        probe.hit := read(req.io.tshr_dirResult.hit)
        probe.consume := read(req.prefetched_hit_consume)
        probe.writeSource := read(req.io.tshr_meta_write_en.pfSource)
        probe.newSource := read(req.io.tshr_meta_write_meta.pfSource)
        probe.newDepth := read(req.io.tshr_meta_write_meta.cdpPfDepth)
        probe.dirRead := read(tshr.io.fromDir.DirRdResp) &&
          read(tshr.io.fromDir.TSHRID) === entry.U
        probe.dirSource := read(tshr.io.fromDir.META.pfSource)
        probe.dirDepth := read(tshr.io.fromDir.META.cdpPfDepth)
      }
    }
    obs
  }
}

class L2TopCDPChecker(dut: TestTop_L2Top) extends ChiselSim {
  case class Half(addr: BigInt, source: BigInt, depth: BigInt, hit: Boolean, high: Boolean, data: BigInt)
  val banks = dut.l2cfg.sliceNum
  val queues = Seq.fill(banks)(mutable.Queue.empty[Half])
  val drops = Array.fill[BigInt](banks)(0)
  val collisions = Array.fill[BigInt](banks)(0)
  var disabled = BigInt(0)
  var lines = 0
  var halves = 0
  var forwarded = 0
  var hits = 0
  var simultaneous = 0
  var mergedScans = 0
  var deferredRefills = 0
  var consumes = 0
  val writes = mutable.ArrayBuffer.empty[(BigInt, BigInt, BigInt)]
  val reads = mutable.ArrayBuffer.empty[(BigInt, BigInt, BigInt)]
  val evictions = mutable.ArrayBuffer.empty[(BigInt, BigInt, BigInt)]
  val trains = mutable.ArrayBuffer.empty[(BigInt, BigInt, BigInt, Boolean)]
  private val pending = mutable.Map.empty[(Int, Int), (BigInt, BigInt, BigInt, Boolean)]
  private val dataReady = mutable.Set.empty[(Int, Int)]
  private val waitingMerge = mutable.Set.empty[(Int, Int)]
  private val mergedPending = mutable.Set.empty[(Int, Int)]
  private def flag(signal: Bool): Boolean = signal.peek().litToBoolean
  private def value(signal: UInt): BigInt = signal.peek().litValue
  def sample(): Unit = dut.cdpObservation.foreach { obs =>
    assert(value(obs.disabled) == disabled, s"disabled count got=${value(obs.disabled)} expected=$disabled")
    for (bank <- 0 until banks) {
      assert(value(obs.drops(bank)) == drops(bank))
      assert(value(obs.collisions(bank)) == collisions(bank))
    }
    val nonempty = queues.indices.filter(queues(_).nonEmpty)
    assert(flag(obs.data.valid) == nonempty.nonEmpty)
    if (nonempty.nonEmpty) {
      val bank = value(obs.chosen).toInt
      assert(nonempty.contains(bank))
      val expected = queues(bank).dequeue()
      val actual = obs.data.bits
      assert(Half(value(actual.paddr), value(actual.pfSource), value(actual.pfDepth),
        flag(actual.isHit), flag(actual.high), value(actual.data)) == expected)
      assert(value(actual.eventType) == (if (expected.hit) 1 else 0))
      halves += 1
    }
    var emittingBanks = 0
    obs.req.zipWithIndex.foreach { case (probes, bank) =>
      val emitted = mutable.ArrayBuffer.empty[Seq[Half]]
      probes.zipWithIndex.foreach { case (probe, entry) =>
        val key = (bank, entry)
        // Derive the merge from raw allocator acceptance and the active
        // prefetch identity, never from the RTL's merged flag/output metadata.
        val incomingRead = Set(oceanus.compactchi.CCHIOpcode.ReadShared.opcode,
          oceanus.compactchi.CCHIOpcode.ReadUnique.opcode).contains(value(probe.incomingOpcode).toInt)
        if(flag(probe.enterREQ) && flag(probe.reuseREQ) && flag(probe.prefetchActive) &&
          value(probe.prefetchSource) == 9 && value(probe.incomingSource) != value(probe.prefetchSourceID) &&
          incomingRead && flag(probe.incomingData)) {
          assert(pending.get(key).exists(c => c._2 == 9 && !c._4), "late demand has no pending CDP refill")
          waitingMerge += key
        }
        if (flag(probe.execute)) pending.remove(key)
        if (flag(probe.start.valid)) {
          val start = probe.start.bits
          val old = (value(start.paddr), value(start.pfSource), value(start.pfDepth), flag(start.isHit))
          if(waitingMerge(key) && old._4) {
            assert(old._2 == 9)
            pending(key) = (old._1, BigInt(0), BigInt(0), false)
            mergedPending += key
            waitingMerge -= key
          } else pending(key) = old
          if(!old._4) dataReady -= key
        }
        if(flag(probe.dataStart)) dataReady -= key
        if(flag(probe.complete) || flag(probe.ds)) dataReady += key
        val expectedLine = pending.get(key).exists { context =>
          val cdpRefill = context._2 == 9 && !context._4
          val full = flag(probe.complete) || flag(probe.ds) ||
            (dataReady(key) && (context._4 || cdpRefill || mergedPending(key)))
          full && !waitingMerge(key) && !(cdpRefill && flag(probe.prefetchActive))
        }
        assert(flag(probe.line.valid) == expectedLine,
          s"scan lifecycle mismatch bank=$bank entry=$entry expected=$expectedLine")
        if (flag(probe.line.valid)) {
          val line = probe.line.bits
          assert(pending.remove(key).contains((value(line.paddr), value(line.pfSource),
            value(line.pfDepth), flag(line.isHit))), s"duplicate scan or context misalignment bank=$bank entry=$entry")
          val addr = value(line.paddr)
          val expected = L2TopReferenceMemory.beat(addr, 0) |
            (L2TopReferenceMemory.beat(addr, 1) << 256)
          assert(value(line.data) == expected, s"full-line/forwarding mismatch addr=$addr")
          if(mergedPending.remove(key)) {
            assert(value(line.pfSource) == 0 && value(line.pfDepth) == 0 && !flag(line.isHit))
            mergedScans += 1
          } else if(value(line.pfSource) == 9 && !flag(line.isHit) && !flag(probe.complete)) deferredRefills += 1
          val hit = flag(line.isHit)
          assert(value(line.eventType) == (if (hit) 1 else 0))
          val halves = (0 until 2).map { half => Half(addr, value(line.pfSource),
            value(line.pfDepth), hit, half == 1, L2TopReferenceMemory.beat(addr, half)) }
          emitted += halves
          lines += 1
          if (hit) hits += 1
          if (flag(probe.complete)) forwarded += 1
        }
        if(flag(probe.deallocate)) {
          assert(!pending.contains(key) && !waitingMerge(key) && !mergedPending(key), "deallocated scan context")
          dataReady -= key
        }
        if (flag(probe.train.valid)) {
          val train = probe.train.bits
          assert(value(train.paddr) == value(probe.addr))
          assert(flag(train.hit) == flag(probe.hit))
          assert(value(train.pfSource) == (if (flag(probe.hit)) value(probe.oldSource) else BigInt(0)))
          assert(value(train.pfDepth) == (if (flag(probe.hit)) value(probe.oldDepth) else BigInt(0)))
          trains += ((value(train.paddr), value(train.pfSource), value(train.pfDepth), flag(train.hit)))
          if (!flag(train.trainEnable)) disabled += 1
        }
        if (flag(probe.consume)) {
          assert(flag(probe.writeSource) && value(probe.newDepth) == 0 && value(probe.newSource) == 0)
          consumes += 1
        }
        if (flag(probe.writeSource)) writes += ((value(probe.addr), value(probe.newSource), value(probe.newDepth)))
        if (flag(probe.dirRead)) reads += ((value(probe.addr), value(probe.dirSource), value(probe.dirDepth)))
        if (flag(probe.evict.valid)) {
          val event = probe.evict.bits
          assert(flag(probe.hit))
          assert(value(event.paddr) == value(probe.addr) && value(event.pfSource) == value(probe.oldSource) &&
            value(event.pfDepth) == value(probe.oldDepth))
          evictions += ((value(event.paddr), value(event.pfSource), value(event.pfDepth)))
        }
      }
      if (emitted.nonEmpty) {
        emittingBanks += 1
        collisions(bank) += (emitted.size - 1) * 2
        emitted.head.foreach { half =>
          if (queues(bank).size < 8) queues(bank).enqueue(half) else drops(bank) += 1
        }
      }
    }
    if (emittingBanks > 1) simultaneous += 1
  }
  def report(): Unit = {
    assert(queues.forall(_.isEmpty) && pending.isEmpty && waitingMerge.isEmpty && mergedPending.isEmpty)
    println(s"CDP_MERGE_RESULT pass=true merged=$mergedScans deferredRefills=$deferredRefills")
    println(s"CDP_DATA_RESULT pass=true slices=$banks lines=$lines halves=$halves forwarded=$forwarded hits=$hits simultaneous=$simultaneous consumes=$consumes disabled=$disabled")
  }
}
