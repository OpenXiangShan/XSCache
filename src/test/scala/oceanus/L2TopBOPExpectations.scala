package oceanus

import oceanus.l2.OceanusBOPParameters

/** Base 2d expectations computed from stimulus and production parameters before
  * simulation, never from observed prefetch/hit/eviction counts.
  *
  * D(i)=0x8000+64*i, 288 demands = Stage-A 160 + Stage-B 128. The driver
  * starts a request every 33 cycles once the preceding demand completes. Cold
  * base requests use empty ways (no Invalid-victim EvictBack); the downstream
  * delay is 12 cycles and the demand pipeline finishes within that 33-cycle
  * slot. Measure T independently as the mode of adjacent Stage-A train.fire
  * cycle differences, excluding the phase's final prefetched hits. Assert T=33.
  * DelayQueue enqueues at c, expires at c+L, and writes RR at c+L+1. Thus an
  * older line can score only for k*T > L+1: 9*33=297<=301, 10*33=330>301.
  * +10 is the first positive candidate to score, and strict score improvement
  * keeps it on ties with later +12,+15,... candidates. Each gets four probes;
  * scoreMax=31 is unreachable. roundMax=4 ends after 37*4=148 trains. Startup
  * bestScore=0 < badScore=1 disables initial +2, then idle installs +10 with
  * score=4. The next 140 trains cannot finish a second phase, so +10 persists.
  *
  * For i=0..147, P=D(i)+2*64: cross-page i={62,63,126,127}, disabled
  * otherwise (144). For i=148..287, P=D(i)+10*64: cross-page i=182..191
  * and 246..255 (20); 140-20=120 BOP/queue/downstream prefetch issues.
  * Issued target indices are 158..191,202..255,266..297. Timely demand hits
  * are their intersection with 0..287: 158..191,202..255,266..287 (110).
  * All 288 distinct demands train once: 178 misses + 110 prefetched hits.
  * Cross-page target indices: 64..65,128..129,192..201,256..265 (24).
  * Effectiveness: every issued target is cold at its arrival; +10 leads its
  * consumer by 330 cycles versus a 12-cycle downstream latency. Hence useful=110,
  * late=0, timely=110, issued_downstream=120, redundant=after_demand=resident=0.
  * No snoops or valid-victim replacements: useless_evict=useless_snoop=0.
  * Ten untouched targets D288..297 remain prefetched. Independently derive them
  * as issues(288) -- trains(288), and verify the final committed Directory
  * ledger, rather than deriving residence only by subtracting counter values.
  * Accounting: 120 = 110 downstream-prefetch useful + 0 evict + 0 snoop + 10
  * resident. Slice hash partitions every address, so all slice sums are equal
  * to the single-slice totals (per-slice expectations use those address sets).
  *
  * Disabled target indices: 2..63,66..127,130..149 (144).
  *
  * Count EvictBack per first installation, by (slice,set), where set=(addr>>6)
  * &127 and slice is the production mapN hash. Insert each distinct tag only
  * once, because a prefetched demand hit already has its line. The union is
  * D(0..297): sets 0..41 have tags 4,5,6 (three lines); sets 42..127 have
  * tags 4,5 (two lines), since L2 tag=addr>>13. No valid line is replaced. Slicing
  * partitions each set, so 1/2/4 slices all have EvictBack=0/0/0/0. SC fills
  * S(1)/clean; UC fills UU(3)/clean via base StashShared state updates.
  *
  * Collision Stage-A: train=160, issues=12 (targets158..169), hit=2 (158,159),
  * cross-page=4, disabled=144, demandDn=158, EvictBack=0. D160=0xa800 is
  * prefetched, D170=0xaa80 is cold; +10 maps them to D170 and D180=0xad00.
  * C1/C2 hold the D170 prefetch response while demand reuses its TSHR; both
  * demand events train from old bits (hitDelta=2), producing the same two
  * BOP/queue issues and two prefetch downstream RNSDs. C1 is satisfied from
  * S or UU. C2 SC requires ReadUnique 0x07 because S<US and MakeReadUnique
  * is disabled; C2 UC is satisfied from UU and has zero demandDn increment.
  * C3 holds demand D170 first; its later queued Stash hits that fill, so
  * only D180 issues a prefetch downstream read, hitDelta=1, demandDnDelta=1.
  * Effectiveness Stage-A baseline: useful=2, issued_downstream=12, all other
  * metrics=0, ten prefetched lines remain. C1/C2 add useful=2, late=1 and
  * issued_downstream=2; the held internal prefetch is active when the target
  * demand reuses its TSHR. C3 adds useful=1, late=0, issued_downstream=1,
  * redundant_after_demand=1 and redundant_resident=0. That +1 useful is D160
  * only; the queued D170 prefetch contributes zero useful. The held demand is
  * active at the prefetch's allocator reuse; the queued hint later hits it.
  * Each collision ends with ten unused prefetched lines and zero useless loss.
  * All cases: trainDelta=2, completedDelta=3 including the final D170 readback,
  * crossPageDelta=disabledDelta=0. Readback allocates a fresh TSHR (no reuse),
  * reads SRAM prefetched=0, and does not train. D0..170 plus D180 gives at
  * most two tags per set, so every collision has EvictBack=0. Internal Stash
  * hits do not re-mark prefetched; C3 therefore also ends with old bit=0.
  */
object L2TopBOPExpectations {
  val period = 33
  val stageA = 160
  val total = 288
  val params = OceanusBOPParameters(delayQueueLatency = 300, roundMax = 4)
  val phaseTrains: Int = params.offsetList.size * params.roundMax
  val offset: Int = params.offsetList.filter(k => k > 0 && k * period > params.delayQueueLatency + 1).min
  def address(i: Int): BigInt = BigInt(0x8000) + BigInt(i) * 64
  def target(i: Int): BigInt = address(i) + (if (i < phaseTrains) 2 else offset) * 64
  def samePage(i: Int): Boolean = (address(i) >> 12) == (target(i) >> 12)
  def trains(n: Int): Set[BigInt] = (0 until n).map(address).toSet
  def issues(n: Int): Set[BigInt] = (phaseTrains until n).filter(samePage).map(target).toSet
  def crossPage(n: Int): Set[BigInt] = (0 until n).filterNot(samePage).map(target).toSet
  def disabled(n: Int): Set[BigInt] = (0 until n.min(phaseTrains)).filter(samePage).map(target).toSet
  def hits(n: Int): Set[BigInt] = issues(n).intersect(trains(n))
  def slice(addr: BigInt, slices: Int): Int = slices match {
    case 1 => 0
    case 2 => (addr >> 6).bitCount & 1
    case 4 =>
      val even = (0 until 48 by 2).count(addr.testBit) & 1
      val odd = (1 until 48 by 2).count(addr.testBit) & 1
      (odd << 1) | even
  }
  def bySlice(addresses: Set[BigInt], slices: Int): Seq[Int] =
    (0 until slices).map(s => addresses.count(a => slice(a, slices) == s))
  def occupancy(addresses: Set[BigInt], slices: Int): Map[(Int, Int), Int] =
    addresses.groupBy(a => (slice(a, slices), ((a >> 6) & 127).toInt)).view.mapValues(_.size).toMap
  def validReplacements(addresses: Set[BigInt], slices: Int): Int =
    occupancy(addresses, slices).values.map(n => (n - 4).max(0)).sum
}
