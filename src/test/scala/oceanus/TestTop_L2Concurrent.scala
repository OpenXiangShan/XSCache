package oceanus

import oceanus.compactchi.CCHIOpcode
import oceanus.l2.OceanusBOPParameters

/** Deterministic expectations are specified here before invoking simulation.
  * Random stress is deliberately a separate follow-up. */
object TestTop_L2Concurrent extends App {
  val selected = args.collectFirst { case s if s.startsWith("--case=") => s.stripPrefix("--case=") }.getOrElse("all")
  val queueDepth = args.collectFirst { case s if s.startsWith("--queue-depth=") => s.stripPrefix("--queue-depth=").toInt }.getOrElse(16)
  require(queueDepth > 1 && (queueDepth & (queueDepth - 1)) == 0)
  def read(master: MasterId, addr: BigInt): UpRequestSpec =
    UpRequestSpec(master, 0, 0, 0, CCHIOpcode.ReadShared.opcode, addr)
  // Type-4 ReadShared legality and its CompAck contract are unresolved.
  // Keep the paired case compiled, but exclude it from every default run.
  val defaultNames = Seq("multi-tshr", "prefetch-starvation", "port-readback")
  val names = defaultNames :+ "cross-slice-training"
  require(selected == "all" || names.contains(selected), s"unknown case $selected")
  (if (selected == "all") defaultNames else Seq(selected)).foreach {
    case "port-readback" =>
      // Type-1 only: three distinct-set lines first miss, then hit after
      // fill/commit at cycle 300. Expect six completions/twelve reference
      // beats, and real CompAck on every read. Type-4 coverage is parked
      // pending its opcode whitelist and CompAck contract, not declared PASS.
      val requests = Seq.fill(3)(MasterId.t1p0).zipWithIndex
        .map { case (m, i) => read(m, BigInt("50000", 16) + i * 0x100) }
      println("CONCURRENT_EXPECTED case=port-readback completed=6 checkedBeats=12 pending=0 active=0")
      L2TopConcurrentSimulation.run(L2TopConcurrentPlan("port-readback", 2, Seq(0 -> requests, 300 -> requests))) { t =>
        assert(t.completed == 6 && t.checkedBeats == 12)
        assert(t.trains.size == 3, s"ordinary readbacks must not retrain: ${t.trains}")
        assert(t.accepts.map(_._2.master).toSet == Set(MasterId.t1p0) && t.accepts.size == 6)
      }
    case "multi-tshr" =>
      // D(i)=0x20000+i*0x100: set=4*i, hence no tag alias or replacement.
      // Two-slice parity hash distributes D across both slices. Only t1p0
      // is driven: up to one request handshakes per cycle, with independent
      // live TxnIDs and no wait for earlier completion. Downstream latency=80
      // keeps all eight outstanding; expect maxOutstanding=8 and >=8 TSHRs.
      // Observe raw per-TSHR training collisions here and in starvation for
      // Deviation 4. Record drops/reuse-first overlaps even if zero; this is
      // observation, not a directed guarantee that such overlap occurs.
      // Production BOP remains enabled (default startup badScore disables
      // initial issue). Every demand returns exactly two reference beats.
      val requests = (0 until 8).map(i => read(MasterId.t1p0, BigInt("20000", 16) + i * 0x100))
      println("CONCURRENT_EXPECTED case=multi-tshr completed=8 checkedBeats=16 maxOutstanding=8 pending=0 active=0")
      L2TopConcurrentSimulation.run(L2TopConcurrentPlan("multi-tshr", 2, Seq(0 -> requests))) { t =>
        assert(t.completed == 8 && t.checkedBeats == 16 && t.maxOutstanding == 8 && t.maxTshrs >= 8)
        assert(t.accepts.map(_._1).distinct.size == 8, "t1p0 accepted multiple REQs in one cycle")
        assert(t.accepts.map(_._2.master).toSet == Set(MasterId.t1p0))
        assert(t.accepts.map(_._2.addr).toSet == requests.map(_.addr).toSet)
        assert(t.accepts.map(r => L2TopBOPExpectations.slice(r._2.addr, 2)).toSet == Set(0, 1))
      }
    case "cross-slice-training" =>
      // SUSPENDED: depends on Type-4 ReadShared being legal and on its
      // CompAck rules. With the current base RTL, Type-4 ReadShared waits
      // for an impossible UpRSP CompAck and hangs even after DAT completes.
      // 31609ec is parked on park-type4-compack, pending the port contract,
      // unclassified. Keep the original paired stimulus/derivation below;
      // neither --case=all nor an explicit selection may run it meanwhile.
      println("CONCURRENT_SUSPENDED case=cross-slice-training reason=Type4-ReadShared-opcode-and-CompAck-contract-pending")
      def pendingType4Contract(): Unit = {
        // N=4 independent pairs, issued on consecutive cycles 0..3 while all
        // eight reads remain in flight. t4p0 targets slice0 and
        // t4p1 slice1; cold, distinct sets yield identical directory/RBE timing.
        // Both ports must ACCEPT together and both slices must TRAIN together.
        // FastArbiter starts pendingMask=rrGrantMask=0 -> firstOneOH=slice0.
        // On fire0: pending=10, rrGrant=10 -> next pair selects slice1.
        // On fire1: pending=01, rrGrant=00 -> next pair uses firstOneOH=slice0.
        // Idle cycles do not fire and leave both masks unchanged. Induction:
        // winner sequence 0,1,0,1; receives=4, drops=4, pulses=8.
        // Each losing valid pulse ends that cycle; it is never replayed.
        val n = 4
        val addresses = (0 until 32).map(i => BigInt("30000", 16) + i * 0x100)
        val bySlice = (0 until 2).map(s => addresses.filter(a => L2TopBOPExpectations.slice(a, 2) == s).take(n))
        val pairs = (0 until n).map(i => Seq(read(MasterId.t4p0, bySlice(0)(i)), read(MasterId.t4p1, bySlice(1)(i))))
        val winners = (0 until n).map(i => i % 2)
        val expectedAddresses = (0 until n).map(i => pairs(i)(winners(i)).addr)
        println(s"CONCURRENT_EXPECTED case=cross-slice-training N=$n winners=${winners.mkString(",")} received=$n drops=$n")
        L2TopConcurrentSimulation.run(L2TopConcurrentPlan("cross-slice-training", 2, pairs.zipWithIndex.map { case (p, i) => (i, p) })) { t =>
          assert(t.completed == 2 * n && t.checkedBeats == 4 * n)
          assert(t.accepts.groupBy(_._1).size == n && t.accepts.groupBy(_._1).values.forall(_.size == 2))
          assert(t.accepts.map(_._1).distinct == (0 until n) && t.maxOutstanding == 2 * n)
          assert(t.trains.size == 2 * n && t.trains.groupBy(_._1).size == n &&
            t.trains.groupBy(_._1).values.forall(_.map(_._2).toSet == Set(0, 1)))
          assert(t.received.map(_._2) == expectedAddresses, s"winner sequence differs: ${t.received}")
          assert(t.trains.map(_._1).toSet == t.received.map(_._1).toSet)
          assert(t.trains.size - t.received.size == n && t.priorityDrops == 0)
          println(s"CROSS_SLICE_RESULT winners=${winners.mkString(",")} received=${t.received.size} drops=${t.trains.size - t.received.size}")
        }
      }
    case "prefetch-starvation" =>
      // One slice; t1p0 valid continuously until all 64 requests handshake.
      // D(i)=0x40000+i*0x100, P(i)=D(i)+0x80: disjoint sets (4k vs 4k+2),
      // at most two tags/set. No demand hits, reuse or valid replacements.
      // badScore=0 keeps startup issue enabled; roundMax=100000 prevents any
      // learning phase from completing in 64 trains, preserving offset +2.
      // External REQ is arbiter input0 and queue is input1, so it cannot issue
      // on ANY external-valid cycle, even when external-ready=0.
      // Let M be BOP enqueues before external valid falls. Queue starts empty,
      // depth=Q, no dequeue -> overwrites=max(0,M-Q), retained=last Q hints.
      // Each post-stop enqueue/dequeue updates an independent FIFO ledger;
      // overwritten hints need not issue. Final retained hints must issue,
      // every issued address must match FIFO order, and all TSHRs must drain.
      val requests = (0 until 64).map(i => read(MasterId.t1p0, BigInt("40000", 16) + i * 0x100))
      // The historical oracle is checked with --queue-depth=2. The default
      // production geometry is 16 entries; its retained suffix and overwrite
      // count necessarily change with the queue capacity.
      println(s"CONCURRENT_EXPECTED case=prefetch-starvation requests=64 checkedBeats=128 offset=+2 blockedIssues=0 overwrites=max(0,M-$queueDepth) retained=last$queueDepth")
      L2TopConcurrentSimulation.run(L2TopConcurrentPlan("prefetch-starvation", 1, Seq(0 -> requests),
        bopParams = OceanusBOPParameters(badScore = 0, roundMax = 100000), bopQueueEntries = queueDepth)) { t =>
        assert(t.completed == 64 && t.checkedBeats == 128)
        assert(t.accepts.map(_._2.master).toSet == Set(MasterId.t1p0))
        assert(t.trains.size == 64 && t.received.size == 64 && t.priorityDrops == 0)
        assert(t.enqueues.map(_._2) == t.received.map(_._2 + 0x80))
        assert(t.blockedEnqueues.size > queueDepth && t.releasedAt.nonEmpty)
        assert(t.retained == t.blockedEnqueues.takeRight(queueDepth))
        assert(t.blockedOverwrites == t.blockedEnqueues.size - queueDepth)
        assert(t.issues.forall(_._1 >= t.releasedAt.get))
        assert(t.issues.takeRight(queueDepth).map(_._3) == t.enqueues.takeRight(queueDepth).map(_._2))
        // Capacity 2 -> 16 preserves the demand schedule but retains fourteen
        // additional hints: overwrites 61 -> 47 and slice issues 3 -> 17.
        if (Set(2, 16)(queueDepth)) {
          assert(t.blockedEnqueues.size == 62 && t.releasedAt.contains(1185))
          assert(t.overwrites == 63 - queueDepth && t.issues.size == queueDepth + 1)
          assert(t.retained == (62 - queueDepth until 62).map(i => BigInt("40080", 16) + i * 0x100))
        }
        println(s"STARVATION_RESULT blockedEnqueues=${t.blockedEnqueues.size} expectedBlockedOverwrites=${t.blockedEnqueues.size - queueDepth} totalOverwrites=${t.overwrites} retained=${t.retained.map(_.toString(16))} issues=${t.issues.size} releasedAt=${t.releasedAt.get}")
      }
  }
}
