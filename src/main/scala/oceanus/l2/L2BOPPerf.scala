package oceanus.l2

import chisel3._

/** Observation only. Indices are shared by slice XSPerf and hierarchical tests. */
object L2BOPPerf {
  val names = Seq("useful", "late", "redundant", "redundant_after_demand",
    "redundant_resident", "issued_downstream", "useless_evict", "useless_snoop")
  val Useful = 0
  val Late = 1
  val Redundant = 2
  val RedundantAfterDemand = 3
  val RedundantResident = 4
  val IssuedDownstream = 5
  val UselessEvict = 6
  val UselessSnoop = 7
}

/** Internal observation wires; never exposed at L2Top. */
class L2BOPReqPerf extends Bundle {
  val activePrefetch = Bool()
  val activeDemand = Bool()
  val useful = Bool()
  val redundant = Bool()
  val issuedDownstream = Bool()
  val uselessEvict = Bool()
  // Provenance of the event. Zero means that the corresponding pulse is
  // inactive; nonzero values use L2PfSource's coupledL2 encoding.
  val source = UInt(L2PfSource.Width.W)
  val activePrefetchSource = UInt(L2PfSource.Width.W)
}
