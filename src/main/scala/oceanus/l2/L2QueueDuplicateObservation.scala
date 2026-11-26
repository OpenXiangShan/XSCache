// Read-only L2Top queue events. No admission, storage or dequeue policy lives here.
package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.compactchi.FlitREQ
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate

object L2QueueDuplicateObservation {
  class Snapshot(entries: Int) extends Bundle {
    val offered = Bool()
    val paHit = Bool()
    val equivalentHit = Bool()
    val inserted = Bool()
    val overwrite = Bool()
    val dequeued = Bool()
    val dedupDrop = Bool()
    val occupancy = UInt(log2Ceil(entries + 1).W)
    val inputSource = UInt(L2PfSource.Width.W)
    val outputSource = UInt(L2PfSource.Width.W)
  }

  // All request attributes are compared conservatively, including SrcID,
  // TgtID, opcode, size, security/order/memory attributes and trace/way hints.
  // Only line offset and the TxnID sequence are excluded. TxnID provenance
  // and CDP depth remain part of the key, also for mixed-source fallback.
  def attributes(req: FlitREQ): UInt = Cat(req.elements.toSeq.collect {
    case (name, field) if name != "Addr" && name != "TxnID" => field.asUInt
  } ++ Seq(L2PrefetchTxnID.decode(req.TxnID), L2PrefetchTxnID.depth(req.TxnID)))

  def snapshot(q: OverwriteQueue[FlitREQ]): Snapshot = {
    import L2PrefetchObservation.read
    val entries = q.data.length
    val head = read(q.head)
    val tail = read(q.tail)
    val full = head === tail && read(q.maybeFull)
    val input = read(q.io.enq.bits)
    val output = read(q.io.deq.bits)
    val result = Wire(new Snapshot(entries))
    val matches = q.data.zipWithIndex.map { case (data, index) =>
      // Physical ring slots valid at cycle START, before any enqueue,
      // dequeue or overwrite. Invalid residual register contents never hit.
      val valid = Mux(head === tail, full,
        Mux(head < tail, index.U >= head && index.U < tail,
          index.U >= head || index.U < tail))
      val pending = read(data)
      val pa = valid && (input.Addr >> 6) === (pending.Addr >> 6)
      (pa, pa && attributes(input) === attributes(pending))
    }
    result.offered := read(q.io.enq.valid) && read(q.io.enq.ready)
    result.paHit := result.offered && matches.map(_._1).reduce(_ || _)
    result.equivalentHit := result.offered && matches.map(_._2).reduce(_ || _)
    // OverwriteQueue is always ready and writes EVERY offer. A full-queue
    // overwrite is an insertion as well; overwrite is a subset of inserted.
    result.inserted := result.offered
    result.overwrite := read(q.io.overwrite)
    result.dequeued := read(q.io.deq.valid) && read(q.io.deq.ready)
    result.dedupDrop := false.B
    result.occupancy := Mux(full, entries.U, (tail - head)(log2Ceil(entries) - 1, 0))
    result.inputSource := L2PrefetchTxnID.decode(input.TxnID)
    result.outputSource := L2PrefetchTxnID.decode(output.TxnID)
    assert(!result.dedupDrop, "queue duplicate observation must never drop a request")
    result
  }

  // Called only under the existing verbose XSPerf switch. XSPerfAccumulate
  // supplies GSIM's usual dump and clean semantics. These are exact L2Top
  // events: no sampling expansion and no multiplication by slice count.
  def count(s: Snapshot, prefix: String, entries: Int, mixedSources: Boolean = false,
    stale: Bool = false.B, sameAsChosen: Bool = false.B, issueOff: Bool = false.B)
    (implicit p: Parameters): Unit = {
    def events(name: String, incoming: Bool, outgoing: Bool): Unit = {
      Seq("offered" -> s.offered, "pa_hit" -> s.paHit,
        "equivalent_hit" -> s.equivalentHit, "inserted" -> s.inserted,
        "overwrite" -> s.overwrite, "dedup_drop" -> s.dedupDrop).foreach {
        case (event, pulse) => XSPerfAccumulate(s"${name}_$event", pulse && incoming)
      }
      XSPerfAccumulate(s"${name}_dequeued", s.dequeued && outgoing)
    }
    events(prefix, true.B, true.B)
    if (mixedSources) for (source <- 0 until (1 << L2PfSource.Width)) {
      // Overwrite is attributed to its incoming request, not its victim.
      // Dequeued is attributed to the actual outgoing head's source.
      events(s"${prefix}_src$source", s.inputSource === source.U, s.outputSource === source.U)
    }
    XSPerfAccumulate(s"${prefix}_occupancy_samples", true.B)
    XSPerfAccumulate(s"${prefix}_occupancy_sum", s.occupancy)
    for (n <- 0 to entries) XSPerfAccumulate(s"${prefix}_occupancy_$n", s.occupancy === n.U)
    // Disjoint reasons for the existing pfQueue removals. Fallback has no
    // history filter; its dequeued event is its actual arbiter handshake.
    XSPerfAccumulate(s"${prefix}_filtered_history", s.dequeued && stale)
    XSPerfAccumulate(s"${prefix}_filtered_same_as_chosen", s.dequeued && !stale && sameAsChosen)
    XSPerfAccumulate(s"${prefix}_drained_issue_off", s.dequeued && !stale && !sameAsChosen && issueOff)
  }
}
