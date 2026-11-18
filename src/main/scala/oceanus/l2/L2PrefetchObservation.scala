// Intent (zy-pf-obs): observe existing handshakes using private transaction and
// sampled line records. Never drive a protocol, arbitration, FSM, or SRAM signal.
// All hardware is elaborated under the existing verbose XSPerf print switch;
// records survive perf.clean while accumulated measurements use XSPerf cleanup.
package oceanus.l2

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.tapAndRead
import org.chipsalliance.cde.config.Parameters
import utility._
import scala.collection.mutable
import oceanus.compactchi._

/** Inputs are observations of the enqueue handshake before the existing L1
  * source FIFO. The caller supplies the original request-source sideband. */
case class L2ObservationArrival(fire: Bool, request: FlitREQ, reqSource: UInt,
  upgrade: Option[Bool] = None)

// Physical source buffers can contain several two-entry stages. These are
// Scala observation handles only; no functional Bundle acquires new fields.
case class L2ObservationFifo(enqueue: Bool, address: UInt, dequeue: Bool, source: Int, stages: Int = 1)

object L2PrefetchObservation {
  val latencyBins = Seq(10, 11, 12, 14, 16, 20, 32, 64, 128, 192, 256, 512)
  val entranceBins = Seq(0, 1, 2, 4, 8, 16, 32, 64, 128)
  val ageBins = Seq(1023, 4095, 16383, 65535, 262143)
  val leadBins = Seq(-129, -33, -1, 31, 127, 511, 2047, 8191, 32767)
  val sourceNames = Seq("Recv", "VBOP", "PBOP", "CDP")
  val classNames = Seq("Pending", "DH", "DM", "DU", "LH", "LM", "RR", "RM", "PR",
    "PM_VBOP", "PM_PBOP", "PM_CDP", "Writeback", "Snoop", "Other")
  val stateNames = Seq("S1", "S2_Recv_RBE", "S2_Recv_Entrance", "S2_VBOP_RBE", "S2_VBOP_Entrance",
    "S2_PBOP_RBE", "S2_PBOP_Entrance", "S2_CDP_RBE", "S2_CDP_Entrance", "S3_RBE", "S3_Entrance",
    "S4_RBE", "S4_Entrance", "S5", "NoRecord")

  def enabled(implicit p: Parameters): Boolean = p(PerfCounterOptionsKey).enablePerfPrint &&
    XSPerfLevel.VERBOSE >= p(PerfCounterOptionsKey).perfLevel

  // Intent (zy-pf-obs): GSIM accepts scalar read(probe), but not
  // field selection on read(aggregateProbe). Tap each existing leaf so the
  // emitted FIR contains no read(...).field expression or added data path.
  def read[T <: Data](signal: T): T = {
    val result = Wire(chiselTypeOf(signal))
    def connectLeaves(source: Data, sink: Data): Unit = (source, sink) match {
      case (s: Record, d: Record) => s.elements.foreach { case (name, field) =>
        connectLeaves(field, d.elements(name))
      }
      case (s: Vec[_], d: Vec[_]) => s.zip(d).foreach { case (a, b) => connectLeaves(a, b) }
      case _ => sink := tapAndRead(source)
    }
    connectLeaves(signal, result)
    result
  }
  def group(source: UInt): UInt = MuxLookup(source, 0.U(2.W))(Seq(
    L2PfSource.BOP.U -> 1.U, L2PfSource.PBOP.U -> 2.U, L2PfSource.CDP.U -> 3.U))
  def prefetchClass(c: UInt): Bool = c >= 6.U && c <= 11.U
  def prefetchMiss(c: UInt): Bool = c === 7.U || (c >= 9.U && c <= 11.U)
  def demandMiss(c: UInt): Bool = c === 2.U || c === 5.U
  def delta(end: UInt, begin: UInt): UInt = (end - begin)(31, 0)
  // Intent (zy-pf-obs): share the exact signed modular ordering with the
  // timestamp regression; all observation timestamps use the same clock.
  def beforeOrEqual(a: UInt, b: UInt): Bool = delta(b, a).asSInt >= 0.S

  /** Aggregate same-named events before registering them with XSPerf, so the
    * default output is a sum across slices, including simultaneous events. */
  class Counts(implicit p: Parameters) {
    private val values = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[UInt]]
    def add(name: String, value: UInt): Unit = values.getOrElseUpdate("pfobs_" + name,
      mutable.ArrayBuffer.empty[UInt]) += value
    def event(name: String, enable: Bool): Unit = add(name, enable.asUInt)
    def hist(name: String, value: UInt, enable: Bool, bounds: Seq[Int], signed: Boolean = false): Unit = {
      event(name + "_samples", enable)
      add(name + "_sum", Mux(enable, if (signed) value.asSInt.pad(64).asUInt else value.pad(64), 0.U))
      def le(bound: Int): Bool = if (signed) value.asSInt <= bound.S else value <= bound.U
      for (i <- 0 to bounds.size) {
        val lower = if (i == 0) true.B else !le(bounds(i - 1))
        val upper = if (i == bounds.size) true.B else le(bounds(i))
        event(name + s"_bin$i", enable && lower && upper)
      }
    }
    def emit(): Unit = values.foreach { case (name, xs) => XSPerfAccumulate(name, xs.reduce(_ +& _)) }
  }

  class ArrivalRecord extends Bundle {
    val valid = Bool()
    val t0 = UInt(32.W)
    val t1 = UInt(32.W)
    val tdir = UInt(32.W)
    val lp = Bool()
    val upgrade = Bool()
    val rbe = Bool()
    val classified = Bool()
    val state = UInt(4.W)
    val takeover = Bool()
    val owner = UInt(8.W)
  }
  class SlotRecord extends Bundle {
    val t0 = UInt(32.W)
    val t1 = UInt(32.W)
    val tdir = UInt(32.W)
    val tdn = UInt(32.W)
    val trx = UInt(32.W)
    val tfill = UInt(32.W)
    val t2 = UInt(32.W)
    val dsGrant = UInt(32.W)
    val dsBuffer = UInt(32.W)
    val selected = UInt(32.W)
    val kind = UInt(4.W)
    val source = UInt(4.W)
    val reqSource = UInt(5.W)
    val isREQ = Bool()
    val isRead = Bool()
    val isUpgrade = Bool()
    val lp = Bool()
    val down = Bool()
    val received = Bool()
    val sent = Bool()
    val grantSeen = Bool()
    val bufferSeen = Bool()
    val selectSeen = Bool()
    // Intent (Joseph N2 clarification): retain the actual non-DS supply cause
    // until release, without reclassifying directory-hit ReadUnique requests.
    val downCanceledDS = Bool()
    val snoopData = Bool()
    val existingBuffer = Bool()
    val taken = Bool()
    val filled = Bool()
    // Intent (zy-pf-obs): retain D/LP consumption before directory commit;
    // an RBE follower can execute from local refill metadata first.
    val usedBeforeFill = Bool()
    val lpBeforeFill = Bool()
    val firstUseT0 = UInt(32.W)
  }
  // Intent (Joseph M4/M5 clarification): provenance and usefulness belong to
  // this observation record, independently of the directory pfSource clear.
  // An LP hit only records lpSeen; only the first D hit consumes unused.
  class LineRecord(tagBits: Int) extends Bundle {
    val valid = Bool()
    // Intent (zy-pf-obs): a refill may reuse the way before its victim's CHI
    // eviction completes. Retain only the tag to distinguish those lifetimes.
    val tag = UInt(tagBits.W)
    // Intent (Joseph 2026-11-18): sampled rows use full 32-bit cycle
    // timestamps so long residency does not alias through the former 16-bit
    // coarse clock. Only private observation records change width.
    val tdn = UInt(32.W)
    val tfill = UInt(32.W)
    val source = UInt(4.W) // retained after first use; pfSource in RTL is consumed
    val unused = Bool()
    val lpSeen = Bool()
  }
  class EvictionRecord extends Bundle {
    val valid = Bool()
    val address = UInt(42.W)
    val unused = Bool()
    val lpSeen = Bool()
    val victimSource = UInt(2.W)
    val fillPrefetch = Bool()
    val fillSource = UInt(2.W)
    val time = UInt(32.W)
  }

  def apply(l2: L2Top, arrivals: Seq[L2ObservationArrival],
    fifos: Seq[L2ObservationFifo] = Nil)(implicit p: Parameters): Unit = {
    if (enabled) {
      val c = new Counts
      val now = GTimer()(31, 0)
      val ns = l2.config.sliceNum
      val nt = l2.nMSHR
      val sets = l2.paramL2.sets
      val ways = l2.paramL2.ways
      val sb = log2Ceil(sets)
      val wb = log2Ceil(ways)
      val txnBits = l2.io.t1p.head.UpREQ.bits.TxnID.getWidth
      val upstream = l2.config.upstream.entries
      val nodes = upstream.map(_.nid)
      val nTxn = 1 << txnBits
      val nArrival = nTxn * nodes.size
      require(ns * nt <= 256, "observation owner index must fit")
      require(sets >= 16 && sets % 16 == 0, "sample every sixteenth set")
      def arrivalIndex(src: UInt, txn: UInt): UInt =
        (MuxLookup(src, 0.U)(nodes.zipWithIndex.map { case (n, i) => n.U -> i.U }) << txnBits) | txn
      def knownSource(src: UInt): Bool = nodes.map(src === _.U).reduce(_ || _)
      def dataSource(source: UInt): Bool = source === 2.U || source === 3.U || source === 4.U || source === 6.U
      def readOpcode(op: UInt): Bool = Seq(CCHIOpcode.ReadShared, CCHIOpcode.ReadUnique, CCHIOpcode.ReadOnce).map(op === _.U).reduce(_ || _)
      def sample(address: UInt): Bool = address(9, 6) === 0.U
      def lineIndex(address: UInt, way: UInt): UInt =
        if (sb == 4) way(wb - 1, 0) else Cat(address(6 + sb - 1, 10), way(wb - 1, 0))

      // F1/F2 update during warmup and are deliberately independent of perf.clean.
      val requests = RegInit(VecInit(Seq.fill(nArrival)(0.U.asTypeOf(new ArrivalRecord))))
      val slots = RegInit(VecInit(Seq.fill(ns * nt)(0.U.asTypeOf(new SlotRecord))))
      val lines = Seq.fill(ns)(RegInit(VecInit(Seq.fill(sets / 16 * ways)(0.U.asTypeOf(new LineRecord(42 - sb))))))
      // Intent (zy-pf-obs): EVB snapshots the OLD row before the refiller
      // can reuse its way. This is private per-TSHR observation state.
      val victimSnapshots = RegInit(VecInit(Seq.fill(ns * nt)(0.U.asTypeOf(new LineRecord(42 - sb)))))
      // F4 is a single global direct-mapped table, including the complete line PA.
      // Intent (zy-pf-obs): use an asynchronous private record memory with
      // separately reset valid bits. This keeps the full-address table as an
      // indexed array in generated simulation code, rather than wide muxes.
      val evictions = Mem(2048, new EvictionRecord)
      val evictionValid = RegInit(VecInit(Seq.fill(2048)(false.B)))
      // Intent (Joseph M7 correction): downstream reads are separate lives
      // from their TSHR owner. The REQ pipe serializes reads using the same
      // CHI TxnID, so rearm a private record for EVERY link read and retire it
      // on its first data beat. Never reuse F2's first-owner down/received bits.
      val m7Pending = RegInit(VecInit(Seq.fill(ns * nt)(false.B)))
      val m7Tdn = Reg(Vec(ns * nt, UInt(32.W)))
      val m7N = Reg(Vec(ns * nt, UInt(8.W)))
      val m7Demand = Reg(Vec(ns * nt, Bool()))
      // Reads outside D/LP/prefetch still contribute to global N, but must
      // not enter either class's latency histogram as a default prefetch.
      val m7Eligible = Reg(Vec(ns * nt, Bool()))
      val inflight = PopCount(m7Pending)
      // Tdn/Trx are measured at the actual CHI link. The two-entry TXREQ
      // queue is part of L2 and must not inflate downstream latency.
      val linkTx = read(l2.chiLink.io.out.txreq.flit)
      val linkTxValid = read(l2.chiLink.io.out.txreq.flitv)
      val linkRx = read(l2.chiLink.io.out.rxdat.flit)
      val linkRxValid = read(l2.chiLink.io.out.rxdat.flitv)
      // CHI E ReadShared/ReadClean/ReadOnce/ReadNoSnp/ReadUnique/
      // ReadNotSharedDirty/MakeReadUnique/ReadPreferUnique, excluding upgrades.
      val txReads = Seq(0x01, 0x02, 0x03, 0x04, 0x07, 0x26, 0x41, 0x4c)
      val txIsRead = txReads.map(linkTx.Opcode.get === _.U).reduce(_ || _)
      val linkTxOwner = linkTx.TxnID.get
      val linkRxOwner = linkRx.TxnID.get
      c.hist("M7_inflight", inflight, true.B, Seq(15, 31, 63, 95))

      arrivals.foreach { a =>
        val fire = read(a.fire)
        val src = read(a.request.SrcID)
        val txn = read(a.request.TxnID)
        val opcode = read(a.request.Opcode)
        val source = read(a.reqSource)
        val upgrade = a.upgrade.map(read(_)).getOrElse(false.B) || opcode === CCHIOpcode.MakeUnique.U
        val eligible = fire && knownSource(src) && dataSource(source) &&
          (readOpcode(opcode) || opcode === CCHIOpcode.MakeUnique.U)
        val idx = arrivalIndex(src, txn)
        when(eligible) {
          requests(idx) := 0.U.asTypeOf(new ArrivalRecord)
          requests(idx).valid := true.B
          requests(idx).t0 := now
          requests(idx).lp := source === 6.U
          requests(idx).upgrade := upgrade
        }
      }

      val lineOccupancy = mutable.ArrayBuffer.empty[Seq[UInt]]
      val evictionWrites = mutable.ArrayBuffer.empty[(Bool, UInt, EvictionRecord)]
      l2.slices.zipWithIndex.foreach { case (slice, si) =>
        val sliceSlots = slots.slice(si * nt, (si + 1) * nt)
        val valids = slice.tshrs.map(t => read(t.io.valid))
        val occupied = PopCount(valids)
        // All 33 exact bins plus samples/sum; the sum is the integral oracle.
        c.hist(s"M3_slice${si}_occupancy", occupied, true.B, 0 until 32)
        val sourceTable = lines(si)
        lineOccupancy += sourceNames.indices.map { g =>
          PopCount(sourceTable.map(r => r.valid && r.unused && group(r.source) === g.U)) * 16.U
        }

        slice.tshrs.zipWithIndex.foreach { case (t, ti) =>
          val id = si * nt + ti
          val s = slots(id)
          val alloc = read(t.tshr_alloc)
          val release = read(t.tshr_dealloc) && read(t.io.valid)
          val acceptREQ = read(t.tshr_enter_REQ)
          val reuseREQ = read(t.io.fromAlloc.reuse.REQ)
          val in = read(t.io.UpRXREQ)
          val reqSource = t.io.UpRXREQTrain.map(x => read(x.reqSource)).getOrElse(0.U(5.W))
          val inIndex = arrivalIndex(in.SrcID, in.TxnID)
          val isExternal = knownSource(in.SrcID) && in.SrcID =/= l2.config.pfSrcID.U
          val inRead = isExternal && readOpcode(in.Opcode) && dataSource(reqSource) && !requests(inIndex).upgrade
          val pfSource = L2PrefetchTxnID.decode(in.TxnID)
          val initialOther = Mux(read(t.io.fromAlloc.alloc.SNP), 13.U,
            Mux(read(t.io.fromAlloc.alloc.EVT) || read(t.io.fromAlloc.alloc.EVB), 12.U, 14.U))
          when(alloc) {
            s := 0.U.asTypeOf(new SlotRecord)
            s.t1 := now
            s.kind := Mux(read(t.io.fromAlloc.alloc.REQ), 0.U, initialOther)
            s.isREQ := read(t.io.fromAlloc.alloc.REQ)
          }
          when(acceptREQ) {
            when(alloc) {
              s.t0 := requests(inIndex).t0
              s.source := Mux(isExternal, 0.U, pfSource)
              s.reqSource := reqSource
              s.lp := reqSource === 6.U
              s.isRead := inRead
              s.isUpgrade := isExternal && (in.Opcode === CCHIOpcode.MakeUnique.U || requests(inIndex).upgrade)
            }
            when(inRead && requests(inIndex).valid) {
              requests(inIndex).t1 := now
              requests(inIndex).rbe := reuseREQ
              val taken = reuseREQ && s.source =/= 0.U && !s.filled
              requests(inIndex).takeover := taken
              requests(inIndex).owner := id.U
              when(taken) { s.taken := true.B }
            }
          }
          // Register measurement expressions outside hardware when scopes;
          // Counts emits their accumulated contributions after all slice taps.
          for ((name, lp) <- Seq("D" -> false, "LP" -> true)) {
            c.hist(s"M2_${name}_entrance", delta(now, requests(inIndex).t0),
              acceptREQ && inRead && requests(inIndex).valid && requests(inIndex).lp === lp.B, entranceBins)
          }
          // Tdir for the initial owner is the actual directory response, before
          // the REQ RBE drains. Reused requests see the updated meta on execution.
          val dir = read(t.io.fromDir)
          val dirResponse = dir.DirRdResp && dir.TSHRID === ti.U
          val execution = read(t.vPipeREQ.io.UpRXREQ)
          val exec = execution.valid
          val current = execution.bits
          val exIdx = arrivalIndex(current.SrcID, current.TxnID)
          val address = read(t.tshr_paddr)
          val meta = read(t.dirResult)
          val pending = read(t.rbeREQ.io.out.bits.req)
          val pendingIdx = arrivalIndex(pending.SrcID, pending.TxnID)
          // Intent (zy-pf-obs): a follower queued before the owner's first
          // directory result must be classified on its own later execution.
          val firstDir = dirResponse && read(t.rbeREQ.io.valid) && knownSource(pending.SrcID) && requests(pendingIdx).valid &&
            !requests(pendingIdx).upgrade && !requests(pendingIdx).rbe && !requests(pendingIdx).classified
          val classify = firstDir || (exec && requests(exIdx).valid && !requests(exIdx).upgrade && !requests(exIdx).classified && knownSource(current.SrcID))
          val qidx = Mux(firstDir, pendingIdx, exIdx)
          val hit = Mux(firstDir, dir.META.hit, meta.hit)
          val way = Mux(firstDir, dir.META.way, meta.way)
          val row = sourceTable(lineIndex(address, way))
          val q = requests(qidx)
          val haveRecord = sample(address) && row.valid && row.tag === address(47, 6 + sb)
          val s1 = hit && haveRecord && beforeOrEqual(row.tfill, q.t0)
          val after = !beforeOrEqual(row.tdn, q.t0)
          val recordedState = Mux(!hit, 13.U, Mux(!haveRecord, 14.U, Mux(s1, 0.U,
            Mux(after, Mux(q.rbe, 11.U, 12.U),
              Mux(row.source =/= 0.U, 1.U + (group(row.source) << 1) + !q.rbe,
                Mux(q.rbe, 9.U, 10.U))))))
          // Intent (zy-pf-obs): local refill metadata may supply a follower
          // before F3's directory commit. Use the live owner's full timestamps,
          // as required for requests served directly by the in-flight owner.
          val directFollower = hit && q.rbe && s.down
          val downBeforeArrival = delta(q.t0, s.tdn).asSInt >= 0.S
          val filledBeforeArrival = s.filled && delta(q.t0, s.tfill).asSInt >= 0.S
          val directState = Mux(filledBeforeArrival, 0.U, Mux(!downBeforeArrival, 11.U,
            Mux(s.source =/= 0.U, 1.U + (group(s.source) << 1), 9.U)))
          val state = Mux(directFollower, directState, recordedState)
          when(classify) {
            q.classified := true.B
            q.state := state
            q.tdir := now
          }
          for ((name, lp) <- Seq("D" -> false, "LP" -> true)) {
            val eligible = classify && q.lp === lp.B
            c.event(s"M1_${name}_total", eligible)
            stateNames.zipWithIndex.foreach { case (st, i) => c.event(s"M1_${name}_$st", eligible && state === i.U) }
          }
          when(dirResponse && s.isREQ && s.kind === 0.U) {
            s.tdir := now
            s.kind := Mux(s.isUpgrade, 3.U,
              Mux(s.isRead, Mux(s.lp, Mux(dir.META.hit, 4.U, 5.U), Mux(dir.META.hit, 1.U, 2.U)),
                Mux(s.source === 0.U, 14.U,
                  Mux(group(s.source) === 0.U, Mux(dir.META.hit, 6.U, 7.U),
                    Mux(dir.META.hit, 8.U, 8.U + group(s.source))))))
          }
          val transactionID = (ti << log2Ceil(ns)) | si
          val down = linkTxValid && txIsRead && linkTxOwner === transactionID.U
          val rx = linkRxValid && linkRxOwner === transactionID.U
          val firstDown = down && !s.down
          val firstRx = rx && s.down && !s.received
          when(firstDown) { s.down := true.B; s.tdn := now }
          when(firstRx) { s.received := true.B; s.trx := now }
          // Intent (Joseph M7 correction): capture the EXECUTING request,
          // including an RBE follower, before its downstream request is queued.
          // Keep initial LM in the original demand group as specified; later
          // LP-triggered reads belong to prefetch. D-triggered reads always
          // belong to demand, even for DH under a retained RM/PM owner.
          val m7Executed = RegInit(false.B)
          val m7TriggerDemand = RegInit(false.B)
          val m7TriggerEligible = RegInit(false.B)
          val m7QueuedDemand = RegInit(false.B)
          val m7QueuedEligible = RegInit(false.B)
          val m7ExecSource = t.vPipeREQ.io.UpRXREQTrain.map(x => read(x.reqSource)).getOrElse(0.U(5.W))
          val m7ExecD = Seq(2, 3, 4).map(m7ExecSource === _.U).reduce(_ || _)
          when(alloc) { m7Executed := false.B }
          when(exec) {
            m7Executed := true.B
            m7TriggerEligible := (knownSource(current.SrcID) &&
              (m7ExecD || m7ExecSource === 6.U)) ||
              (current.SrcID === l2.config.pfSrcID.U && L2PrefetchTxnID.decode(current.TxnID) =/= 0.U)
            m7TriggerDemand := knownSource(current.SrcID) &&
              (m7ExecD || (m7ExecSource === 6.U && !m7Executed))
          }
          // The physical TXREQ queue may delay Tdn. Latch its classification
          // on the existing enqueue handshake, independently of later REQs.
          val m7Queue = read(t.io.DnTXREQ)
          val m7QueueRead = txReads.map(m7Queue.bits.Opcode.get === _.U).reduce(_ || _)
          when(m7Queue.valid && m7Queue.ready && m7QueueRead) {
            m7QueuedDemand := m7TriggerDemand
            m7QueuedEligible := m7TriggerEligible
          }
          val m7FirstRx = rx && m7Pending(id)
          when(m7FirstRx) { m7Pending(id) := false.B }
          when(down) {
            m7Pending(id) := true.B
            m7Tdn(id) := now
            m7N(id) := inflight
            m7Demand(id) := m7QueuedDemand
            m7Eligible(id) := m7QueuedEligible
          }
          for ((name, prefetch) <- Seq("Demand" -> false, "Prefetch" -> true)) {
            val eligible = m7FirstRx && m7Eligible(id) && m7Demand(id) === (!prefetch).B
            val elapsed = delta(now, m7Tdn(id))
            c.hist(s"M7_${name}_latency", elapsed, eligible, Seq(64, 96, 128, 160, 192, 256, 384, 512))
            for (b <- 0 until 5) {
              val band = PopCount(Seq(16, 32, 64, 96).map(m7N(id) >= _.U))
              c.event(s"M7_${name}_N${b}_samples", eligible && band === b.U)
              c.add(s"M7_${name}_N${b}_sum", Mux(eligible && band === b.U, elapsed, 0.U))
            }
          }
          val tx = read(t.io.UpTXDAT)
          val txFirst = tx.valid && tx.ready && tx.bits.DataID === 0.U
          when(txFirst && !s.sent) { s.sent := true.B; s.t2 := now }
          val txIndex = arrivalIndex(tx.bits.TgtID, tx.bits.TxnID)
          val returned = requests(txIndex)
          for ((name, lp) <- Seq("D" -> false, "LP" -> true)) {
            val eligible = txFirst && returned.valid && returned.classified && returned.lp === lp.B
            val rs = returned.state
            val cats = Seq("S1" -> (rs === 0.U), "S2" -> (rs >= 1.U && rs <= 8.U),
              "S34" -> (rs >= 9.U && rs <= 12.U), "S5" -> (rs === 13.U))
            cats.foreach { case (st, cond) => c.hist(s"M2_${name}_$st", delta(now, returned.t0), eligible && cond, latencyBins) }
          }
          for ((name, recv) <- Seq("Recv" -> true, "Internal" -> false)) {
            val owner = slots(returned.owner(log2Ceil(ns * nt) - 1, 0))
            c.hist(s"M6_$name", delta(now, owner.trx), txFirst && returned.valid && returned.takeover && prefetchMiss(owner.kind) && owner.received &&
              ((group(owner.source) === 0.U) === recv.B), Seq(2, 4, 8, 12, 16, 24, 32))
          }
          when(txFirst && returned.valid) { returned.valid := false.B }

          // Intent (Joseph N2 clarification): partition ALL DH at release,
          // just like M3. Only the original first response's actual DS grant
          // and buffer write qualify for five segments; later RBE responses
          // must not add samples or supply missing points retroactively.
          val grant = read(t.io.fromDS.DSBufRdArbComp)
          val buffer = read(t.tshr_buffer_wen_DS)
          val selected = read(slice.io.UpTXDAT.valid) && read(slice.io.UpTXDAT.bits.DBID) === ti.U &&
            read(slice.io.UpTXDAT.bits.DataID) === 0.U
          val downstreamCancel = read(t.vPipeREQ.ds_cancel_readunique) &&
            (read(t.vPipeREQ.dn_rxdat_compdata_first) || read(t.vPipeREQ.dn_rxrsp_respsepdata))
          val snoopData = read(t.tshr_buffer_wen_UpRXDAT_0) || read(t.tshr_buffer_wen_UpRXDAT_2)
          val modifiedBuffer = read(t.vPipeREQ.io.tbuf_modified)
          when(!s.sent) {
            when(grant && !s.grantSeen) { s.grantSeen := true.B; s.dsGrant := now }
            when(buffer && !s.bufferSeen) { s.bufferSeen := true.B; s.dsBuffer := now }
            when(selected && !s.selectSeen) { s.selectSeen := true.B; s.selected := now }
            when(downstreamCancel) { s.downCanceledDS := true.B }
            when(snoopData) { s.snoopData := true.B }
            when(exec && modifiedBuffer) { s.existingBuffer := true.B }
          }
          val dhEnd = release && s.kind === 1.U
          val dsDH = dhEnd && s.sent && s.grantSeen && s.bufferSeen
          // Mutually exclusive omissions: downstream cancellation, upstream
          // snoop data, pre-existing modified buffer, no L1 data, missing DS
          // points. The last bin remains visible for unexpected baseline paths.
          val otherReason = Mux(!s.sent, 3.U, Mux(s.downCanceledDS, 0.U,
            Mux(s.snoopData, 1.U, Mux(s.existingBuffer, 2.U, 4.U))))
          c.event("N2_DH_total", dhEnd)
          c.event("N2_DH_DS", dsDH)
          Seq("downstream_cancel_DS", "snoop_data", "existing_buffer", "no_L1_data", "missing_DS_points")
            .zipWithIndex.foreach { case (reason, i) =>
              c.event(s"N2_DH_other_$reason", dhEnd && !dsDH && otherReason === i.U)
            }
          val points = Seq(s.t1, s.tdir, s.dsGrant, s.dsBuffer, s.selected, s.t2)
          for (seg <- 0 until 5) {
            c.event(s"N2_segment${seg}_samples", dsDH)
            c.add(s"N2_segment${seg}_sum", Mux(dsDH, delta(points(seg + 1), points(seg)), 0.U))
          }
          c.hist("N2_slice_wait", delta(s.selected, s.dsBuffer), dsDH, Seq(0, 1, 2, 4))
          c.hist("N2_channel_wait", delta(s.t2, s.selected), dsDH, Seq(0, 1, 2, 4))

          val life = delta(now, s.t1)
          val first = Mux(s.down && s.received, delta(s.trx, s.t1), 0.U)
          val second = Mux(s.sent, delta(s.t2, Mux(s.down, s.trx, s.t1)), 0.U)
          val third = delta(now, Mux(s.sent, s.t2, Mux(s.received, s.trx, s.t1)))
          classNames.zipWithIndex.foreach { case (name, kind) =>
            val end = release && s.kind === kind.U
            c.event(s"M3_${name}_transactions", end)
            c.event(s"M3_${name}_occupancy", valids(ti) && s.kind === kind.U)
            c.add(s"M3_${name}_wait_data", Mux(end, first, 0.U))
            c.add(s"M3_${name}_wait_L1", Mux(end, second, 0.U))
            c.add(s"M3_${name}_wait_ack", Mux(end, third, 0.U))
          }
          for ((name, eligible) <- Seq("DH" -> (s.kind === 1.U), "DM" -> (s.kind === 2.U),
            "PM" -> (s.kind >= 9.U && s.kind <= 11.U))) c.hist(s"M3_${name}_residence", life, release && eligible, latencyBins)
          sourceNames.zipWithIndex.foreach { case (name, g) =>
            c.event(s"M3_${name}_taken_over", release && s.taken && prefetchMiss(s.kind) && group(s.source) === g.U)
          }

          // F3 installation is the directory tag-write commit, not the start
          // of a replacement lookup. Other metadata writes retain history.
          val write = read(t.io.toDir)
          val fill = dir.DirWbArbComp && dir.TSHRID === ti.U && write.TAG_WEN &&
            write.META.state =/= L2Directory.MetaState.I
          val fillIdx = lineIndex(write.PADDR, write.WAY)
          when(fill) { s.filled := true.B; s.tfill := now }
          sourceNames.zipWithIndex.foreach { case (name, g) =>
            c.add(s"M4_${name}_fills_estimated", Mux(fill && sample(write.PADDR) && s.source =/= 0.U &&
              group(s.source) === g.U, 16.U, 0.U))
          }
          when(fill && sample(write.PADDR)) {
            sourceTable(fillIdx).valid := true.B
            sourceTable(fillIdx).tag := write.PADDR(47, 6 + sb)
            sourceTable(fillIdx).tdn := s.tdn
            sourceTable(fillIdx).tfill := now
            sourceTable(fillIdx).source := s.source
            sourceTable(fillIdx).unused := s.source =/= 0.U && !s.usedBeforeFill
            sourceTable(fillIdx).lpSeen := s.lpBeforeFill
          }
          // Snoop invalidations remove residency too, without manufacturing
          // replacement history or changing the directory's update semantics.
          val invalidate = dir.DirWbArbComp && dir.TSHRID === ti.U &&
            write.META_WEN.state && write.META.state === L2Directory.MetaState.I
          when(invalidate && sample(write.PADDR) && sourceTable(fillIdx).tag === write.PADDR(47, 6 + sb)) {
            sourceTable(fillIdx).valid := false.B
          }
          val hitIdx = lineIndex(address, meta.way)
          val hitRow = sourceTable(hitIdx)
          val execSource = t.vPipeREQ.io.UpRXREQTrain.map(x => read(x.reqSource)).getOrElse(0.U(5.W))
          val readOrUpgrade = readOpcode(current.Opcode) || current.Opcode === CCHIOpcode.MakeUnique.U
          val hitExecution = exec && knownSource(current.SrcID) && meta.hit && readOrUpgrade && sample(address)
          val beforeFillHit = hitExecution && s.source =/= 0.U && s.down && !s.filled
          val isD = Seq(2, 3, 4).map(execSource === _.U).reduce(_ || _)
          val pendingD = beforeFillHit && isD && !s.usedBeforeFill
          val pendingLP = beforeFillHit && execSource === 6.U
          val observedHit = hitExecution && !beforeFillHit && hitRow.valid &&
            hitRow.tag === address(47, 6 + sb) && hitRow.source =/= 0.U
          val demandHit = observedHit && isD
          val lpHit = observedHit && execSource === 6.U
          val firstDemand = demandHit && hitRow.unused
          when(lpHit) { hitRow.lpSeen := true.B }
          when(firstDemand) { hitRow.unused := false.B }
          when(pendingLP) { s.lpBeforeFill := true.B }
          when(pendingD) { s.usedBeforeFill := true.B; s.firstUseT0 := requests(exIdx).t0 }
          // Same-edge commit and use are resolved into the installed record.
          when(fill && sample(write.PADDR)) {
            when(pendingD) { sourceTable(fillIdx).unused := false.B }
            when(pendingLP) { sourceTable(fillIdx).lpSeen := true.B }
          }
          // Actual victim invalidation occurs in the nested EVB transaction;
          // cdpEvict is already the physical eviction event, never a retry pick.
          val eviction = Wire(Valid(new L2CDPEvict))
          eviction.valid := read(t.vPipeREQ.meta_wr_state_evictback_I) &&
            read(t.vPipeREQ.io.tshr_meta_write_en.state) &&
            read(t.vPipeREQ.io.tshr_meta_write_meta.state) === L2Directory.MetaState.I &&
            meta.hit && meta.state =/= L2Directory.MetaState.I
          eviction.bits.paddr := address
          eviction.bits.pfSource := meta.pfSource
          eviction.bits.pfDepth := 0.U
          val victim = eviction.bits
          val victimIdx = lineIndex(victim.paddr, meta.way)
          val liveVictimRow = sourceTable(victimIdx)
          val snapshot = victimSnapshots(id)
          val startEviction = read(t.vPipeREQ.rxevb_unsatisfied_evictback)
          when(startEviction && sample(address)) {
            snapshot := liveVictimRow
            snapshot.valid := liveVictimRow.valid && liveVictimRow.tag === address(47, 6 + sb)
          }
          val victimRow = snapshot
          val victimSample = eviction.valid && sample(victim.paddr)
          val unusedVictim = victimSample && victimRow.valid && victimRow.unused
          when(victimSample) {
            snapshot.valid := false.B
            when(liveVictimRow.tag === victim.paddr(47, 6 + sb)) { liveVictimRow.valid := false.B }
          }
          c.event("M4_eviction_without_record", victimSample && !victimRow.valid)
          for (pf <- 1 to 9) c.add(s"M4_pfSource${pf}_unused_evict_estimated",
            Mux(unusedVictim && victimRow.source === pf.U, 16.U, 0.U))
          sourceNames.zipWithIndex.foreach { case (name, g) =>
            c.hist(s"M4_${name}_unused_age", delta(now, victimRow.tfill),
              unusedVictim && group(victimRow.source) === g.U, ageBins)
            val lead = delta(requests(exIdx).t0, hitRow.tfill)
            c.hist(s"M4_${name}_lead", lead, firstDemand && requests(exIdx).valid &&
              group(hitRow.source) === g.U, leadBins, signed = true)
            val pendingLeadT0 = Mux(pendingD, requests(exIdx).t0, s.firstUseT0)
            val pendingLead = delta(pendingLeadT0, now)
            c.hist(s"M4_${name}_lead", pendingLead, fill && sample(write.PADDR) &&
              (s.usedBeforeFill || pendingD) && group(s.source) === g.U, leadBins, signed = true)
            c.add(s"M4_${name}_useful_estimated", Mux((firstDemand && group(hitRow.source) === g.U) ||
              (pendingD && group(s.source) === g.U), 16.U, 0.U))
            c.add(s"M4_${name}_useful_after_LP_estimated", Mux((firstDemand && hitRow.lpSeen &&
              group(hitRow.source) === g.U) || (pendingD && s.lpBeforeFill && group(s.source) === g.U), 16.U, 0.U))
            c.add(s"M4_${name}_unused_evict_after_LP_estimated", Mux(unusedVictim && victimRow.lpSeen &&
              group(victimRow.source) === g.U, 16.U, 0.U))
          }
          // Each global F4 write is one real victim; same-index simultaneous
          // writes have deterministic slice/TSHR priority, as in a direct map.
          val currentEvb = read(t.vPipeREQ.io.UpRXEVB)
          val refillerId = Mux(currentEvb.valid, currentEvb.bits.TshrId, read(t.vPipeREQ.p_rxevb.TshrId))
          val refiller = slots((si.U * nt.U + refillerId)(log2Ceil(ns * nt) - 1, 0))
          // Sampled sets have address[9:6] == 0. Index above those bits so
          // all 2,048 direct-map entries remain usable by sampled victims.
          val evictIndex = victim.paddr(20, 10)
          val evictionWrite = Wire(new EvictionRecord)
          evictionWrite.valid := victimRow.valid
          evictionWrite.address := victim.paddr(47, 6)
          evictionWrite.unused := victimRow.unused
          evictionWrite.lpSeen := victimRow.lpSeen
          evictionWrite.victimSource := group(victimRow.source)
          evictionWrite.fillPrefetch := refiller.source =/= 0.U
          evictionWrite.fillSource := group(refiller.source)
          evictionWrite.time := now
          evictionWrites += ((victimSample, evictIndex, evictionWrite))
          c.event("M5_table_writes", victimSample)
          val lookup = classify && state === 13.U
          val ev = evictions.read(address(20, 10))
          val found = sample(address) && evictionValid(address(20, 10)) && ev.address === address(47, 6)
          val early = lookup && found && ev.unused
          val capacity = lookup && found && !ev.unused && ev.fillPrefetch
          val demandCapacity = lookup && found && !ev.unused && !ev.fillPrefetch
          sourceNames.zipWithIndex.foreach { case (name, g) =>
            c.event(s"M5_early_$name", early && ev.victimSource === g.U)
            c.event(s"M5_early_${name}_LP_seen", early && ev.victimSource === g.U && ev.lpSeen)
            c.event(s"M5_early_${name}_no_LP", early && ev.victimSource === g.U && !ev.lpSeen)
            c.event(s"M5_capacity_$name", capacity && ev.fillSource === g.U)
          }
          c.event("M5_demand_capacity", demandCapacity)
          c.event("M5_not_found", lookup && !found)
          c.hist("M5_early_interval", delta(now, ev.time), early, Seq(1023, 4095, 16383, 65535))
          c.hist("M5_capacity_interval", delta(now, ev.time), capacity, Seq(1023, 4095, 16383, 65535))
          when(lookup && found) { evictionValid(address(20, 10)) := false.B }
        }

        // Only lack of a free shared slot belongs to this statistic. Same-PA
        // busy-RBE and allocation-cluster arbitration are separate causes.
        val input = read(slice.io.UpRXREQ)
        val inRecord = requests(arrivalIndex(input.bits.SrcID, input.bits.TxnID))
        val noSharedFree = valids.take(nt - 3).reduce(_ && _)
        val matching = slice.tshrs.map(t => read(t.io.valid) &&
          (read(t.io.toAlloc.paddr) >> 6) === (input.bits.Addr >> 6)).reduce(_ || _)
        val blocked = input.valid && !input.ready && noSharedFree && !matching && knownSource(input.bits.SrcID)
        for ((name, lp) <- Seq("D" -> false, "LP" -> true)) {
          val enable = blocked && inRecord.valid && (inRecord.lp === lp.B)
          c.event(s"M3_slice${si}_${name}_blocked", enable)
          val buckets = Seq("Hits" -> ((x: UInt) => x === 1.U || x === 4.U),
            "Misses" -> ((x: UInt) => demandMiss(x)), "Prefetch" -> ((x: UInt) => prefetchClass(x)),
            "Other" -> ((x: UInt) => !(x === 1.U || x === 4.U || demandMiss(x) || prefetchClass(x))))
          buckets.foreach { case (kind, predicate) =>
            val count = PopCount(sliceSlots.zip(valids).map { case (slot, valid) => valid && predicate(slot.kind) })
            c.add(s"M3_slice${si}_${name}_blocked_$kind", Mux(enable, count, 0.U))
          }
        }
      }
      sourceNames.zipWithIndex.foreach { case (name, g) =>
        val count = lineOccupancy.map(_(g)).reduce(_ +& _)
        val ownerModule = chisel3.XSCompatibility.currentModule
        // Use the existing XSLog endpoint so start/max reset on exactly the
        // same perf.clean edge as XSPerf, without clearing either record table.
        XSLog.registerCaller { ctrl =>
          val observed = read(count)
          val start = RegInit(0.U(32.W))
          val maximum = RegInit(0.U(32.W))
          when(ctrl.clean) { start := observed; maximum := observed }
            .otherwise { when(observed > maximum) { maximum := observed } }
          XSPerfPrint(ownerModule)(ctrl.dump,
            p"pfobs_M4_${name}_unused_start, $start\npfobs_M4_${name}_unused_end, $observed\npfobs_M4_${name}_unused_max, $maximum\n")
        }
      }
      // M8 producer events precede translation/filtering. PBOP is physical
      // address based and has no translation client, so its translation losses
      // are structurally zero. Existing training/overwrite/effect counters are
      // reused rather than registered a second time under pfobs names.
      l2.vbopStub.foreach { v =>
        c.event("M8_VBOP_generated", read(v.reqFilter.io.in.valid))
      }
      l2.bop.foreach { b =>
        c.event("M8_PBOP_generated", read(b.io.train.valid) && read(b.io.train.ready) &&
          read(b.enable) && read(b.issueEnable))
      }
      l2.cdp.foreach { d =>
        val raw = d.detectFrontend.raw
        c.add("M8_CDP_generated", PopCount(raw.map(x => read(x.valid))) * 3.U)
        // Existing detectFrontend.degree_drop_0..3 cover rejected candidates.
      }
      l2.io.pfRecv.foreach { r => c.event("M8_Recv_generated", read(r.addr_valid)) }
      c.event("M8_PBOP_translation_priority_block", false.B)
      c.event("M8_PBOP_translation_drop", false.B)
      l2.pfTlbAdapter.foreach { adapter =>
        adapter.io.clients.zipWithIndex.foreach { case (client, i) =>
          val name = if (i == 0) "VBOP" else "CDP"
          val offered = read(client.req.valid)
          // Admission priority exists only when a free adapter slot exists;
          // capacity exhaustion and TLB backpressure are not priority losses.
          c.event(s"M8_${name}_translation_priority_block", if (i == 0) false.B else
            offered && adapter.available.map(x => read(x)).reduce(_ || _) && read(adapter.io.clients(0).req.valid))
          c.event(s"M8_${name}_translation_drop", read(client.resp.valid) && read(client.resp.ready) &&
            read(client.resp.bits.miss))
        }
      }
      // Intent (zy-pf-obs): count blocked cycles once per slice/source
      // group, after source enable/history gates. Several Recv subqueues or
      // its direct plus fallback routes must not multiply one blocked cycle.
      for (si <- 0 until ns; (name, g) <- sourceNames.zipWithIndex) {
        val queued = l2.pfObservationQueues.collect { case (`si`, source, q) if
          (if (source == L2PfSource.BOP) 1 else if (source == L2PfSource.PBOP) 2 else if (source == L2PfSource.CDP) 3 else 0) == g =>
          val eligible = l2.pfObservationOffers.find(x => x._1 == si && x._2 == source).get._3
          read(eligible) && !read(q.io.deq.ready)
        }
        val direct = if (g == 0) l2.pfRecvReqs.lift(si).toSeq.map(r => read(r.valid) && !read(r.ready)) else Nil
        c.event(s"M8_${name}_arbitration_wait", (queued ++ direct).reduceOption(_ || _).getOrElse(false.B))
      }

      // Intent (zy-pf-obs): evaluate capacity for the SECOND address. The
      // live slice ready is address-dependent and refers to a different head.
      // Observe the allocator's reservations, REQ-RBE busy bits and earlier
      // EVT allocation, without connecting this result to any functional input.
      def canAccept(slice: L2TSHRCtrl, address: UInt): Bool = {
        val a = slice.tshrAlloc
        require(!a.config.clusterRRArb && a.config.cluster == Seq(Seq(L2TSHRAllocTarget.EVT),
          Seq(L2TSHRAllocTarget.SNP, L2TSHRAllocTarget.EVB, L2TSHRAllocTarget.REQ)))
        val active = a.io.fromTSHR.map(x => read(x.valid))
        val matches = a.io.fromTSHR.zip(active).map { case (x, v) =>
          v && (read(x.bits.paddr) >> 6) === (address >> 6)
        }
        val shared = (0 until nt).filterNot(i => a.config.resv.exists(_._1 == i))
        val reuse = shared.map(i => matches(i) && !read(a.io.fromTSHR(i).bits.busy.REQ)).reduce(_ || _)
        val earlier = (0 until nt).map(i => read(a.alloc_vec(0)(i)))
        val sameEarlier = read(a.postcluster(0).valid) &&
          (read(a.postcluster(0).bits.paddr) >> 6) === (address >> 6) && earlier.reduce(_ || _)
        val earlierLegal = shared.map(earlier(_)).reduce(_ || _)
        val free = shared.map(i => !active(i) && !earlier(i)).reduce(_ || _)
        val noHigher = !read(a.io.fromTSHRCtrl.RXSNP.valid) && !read(a.io.fromTSHRCtrl.RXEVB.valid)
        noHigher && Mux(matches.reduce(_ || _), reuse, Mux(sameEarlier, earlierLegal, free))
      }
      def target(address: UInt): UInt = l2.config.sliceNum match {
        case 1 => l2.map1(address)
        case 2 => l2.map2(address)
        case 3 => l2.map3(address)
        case _ => l2.map4(address)
      }
      fifos.foreach { fifo =>
        require(fifo.stages > 0)
        // QueueBuffer is a chain of Queue(entries=2, pipe=false, flow=false).
        // Shadow each stage separately so I-cache's second stage cannot make
        // an upstream item appear resident in the final FIFO a cycle early.
        val counts = Seq.fill(fifo.stages)(RegInit(0.U(2.W)))
        val heads = Seq.fill(fifo.stages)(RegInit(0.U(1.W)))
        val tails = Seq.fill(fifo.stages)(RegInit(0.U(1.W)))
        val addresses = Seq.fill(fifo.stages)(Reg(Vec(2, UInt(48.W))))
        val moves = (0 until fifo.stages - 1).map(i => counts(i) =/= 0.U && counts(i + 1) =/= 2.U)
        for (stage <- 0 until fifo.stages) {
          val enq = if (stage == 0) read(fifo.enqueue) else moves(stage - 1)
          val deq = if (stage == fifo.stages - 1) read(fifo.dequeue) else moves(stage)
          val address = if (stage == 0) read(fifo.address) else addresses(stage - 1)(heads(stage - 1))
          when(enq) { addresses(stage)(tails(stage)) := address; tails(stage) := tails(stage) + 1.U }
          when(deq) { heads(stage) := heads(stage) + 1.U }
          when(enq =/= deq) { counts(stage) := Mux(enq, counts(stage) + 1.U, counts(stage) - 1.U) }
        }
        val last = fifo.stages - 1
        val firstAddr = addresses(last)(heads(last))
        val secondAddr = addresses(last)(heads(last) ^ 1.U)
        val second = target(secondAddr)
        val accepts = l2.slices.map(slice => canAccept(slice, secondAddr))
        val secondReady = MuxLookup(second, false.B)(l2.config.slices.zip(accepts).map { case (n, r) => n.U -> r })
        c.event(s"M9_source${fifo.source}_collateral", counts(last) === 2.U && !read(fifo.dequeue) &&
          target(firstAddr) =/= second && secondReady)
      }
      // Intent (zy-pf-obs): resolve same-index writes explicitly. Independent
      // indices all update; a later slice/TSHR wins a direct-map collision.
      evictionWrites.zipWithIndex.foreach { case ((valid, index, entry), i) =>
        val overridden = evictionWrites.drop(i + 1).map { case (v, a, _) => v && a === index }
          .reduceOption(_ || _).getOrElse(false.B)
        when(valid && !overridden) {
          evictions.write(index, entry)
          evictionValid(index) := entry.valid
        }
      }
      c.emit()
    }
  }
}
