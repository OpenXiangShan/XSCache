package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.l2._
import oceanus.l2.L2Common._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.SeqBoolBitwiseOps
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.compactchi.CCHIOpcode._


class L2TSHRTarget extends Bundle {
  val EVT = Bool()
  val SNP = Bool()
  val EVB = Bool()
  val REQ = Bool()
}

object L2TSHRTarget {
  def empty = {
    val t = Wire(new L2TSHRTarget)
    t.EVT := false.B
    t.SNP := false.B
    t.EVB := false.B
    t.REQ := false.B
    t
  }
  def asEVT = {
    val t = WireInit(empty)
    t.EVT := true.B
    t
  }
  def asSNP = {
    val t = WireInit(empty)
    t.SNP := true.B
    t
  }
  def asEVB = {
    val t = WireInit(empty)
    t.EVB := true.B
    t
  }
  def asREQ = {
    val t = WireInit(empty)
    t.REQ := true.B
    t
  }
}

// -- Allocation target channel types, with parameterized preferring priority.
//    The greater value, the higher priority.
case class L2TSHRAllocTarget(val name: String, val priority: Int) // Allocation Target

object L2TSHRAllocTarget {
  val EVT = L2TSHRAllocTarget("EVT", 3)
  val SNP = L2TSHRAllocTarget("SNP", 2)
  val EVB = L2TSHRAllocTarget("EVB", 1)
  val REQ = L2TSHRAllocTarget("REQ", 0)
}
// ----------------------------------------------------------------

// --- Reservation target operation types
case class L2TSHRResvTarget(val name: String) // Reservation Target

object L2TSHRResvTarget {
  val L1EVT = L2TSHRResvTarget("L1EVT")
  val L2EVB = L2TSHRResvTarget("L2EVB")
  val L3SNP = L2TSHRResvTarget("L3SNP")
}
// ----------------------------------------------------------------

class L2TSHRAllocConfig(val cluster: Seq[Seq[L2TSHRAllocTarget]], 
                        val resv: Seq[(Int, L2TSHRResvTarget)],
                        val clusterRRArb: Boolean = false)
(implicit val p: Parameters) extends HasL2Params {
  // cluster: allocation clusters, each entry is a sequence of allocation targets that belong to the same cluster
  // resv: reservation configuration, each entry is a tuple of (target index, reservation target)
  // clusterRRArb: whether use the RR Arbiter or Fixed Priority Aribter inner an allocation cluster
  require(cluster.nonEmpty, "Cluster config should not be empty")
  cluster.foreach(c => require(c.nonEmpty, s"Each cluster should have at least one allocation target, current: ${cluster}"))
  require(cluster.flatten.size == cluster.flatten.toSet.size, s"Allocation Targets should be unique, current: ${cluster}")
  require(cluster.flatten.size == 4, s"All Allocation Targets should be covered, current: ${cluster}")
  require(resv.size == resv.map(_._1).toSet.size, s"Reservation Targets should be unique, current: ${resv}")
  resv.map(_._1).foreach(i => require(i < paramL2.mshrSize, s"Reservation target index out of bounds (mshrSize = ${paramL2.mshrSize}), current: ${resv}"))
}

object L2TSHRAlloc {

  class PathFromTSHR(implicit val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W)
    val busy = new L2TSHRTarget
  }

  class PathToTSHR(implicit val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W)
    val alloc = new L2TSHRTarget
    val reuse = new L2TSHRTarget
  }

  class PathFromTSHRCtrl(implicit val p: Parameters) extends Bundle with HasL2Params {
    val RXEVT = Flipped(Decoupled(new FlitEVT))
    val RXSNP = Flipped(Decoupled(new CHIBundleSNP))
    val RXEVB = Flipped(Decoupled(new L2VPipeREQ.FlitEVB))
    val RXREQ = Flipped(Decoupled(new FlitREQ))
  }
}

class L2TSHRAlloc(val config: L2TSHRAllocConfig)(implicit val p: Parameters) extends Module with HasL2Params {

  val clusterCount = config.cluster.size
  val clusterName = config.cluster.zipWithIndex.map { case (cluster, idx) => s"cluster_${idx}_${cluster.map(_.name).mkString("_")}" }

  require(paramL2.prefetchMaxSharedTSHRs.isEmpty || paramL2.pfTshrQuota.isEmpty,
    "Choose either the legacy static quota or pfTshrQuota")
  private val quotaEnabled = paramL2.prefetchMaxSharedTSHRs.nonEmpty || paramL2.pfTshrQuota.nonEmpty
  private val reserveEnabled = paramL2.demandReserve.nonEmpty

  val io = IO(new Bundle {

    val fromTSHR = Flipped(Vec(paramL2.mshrSize, Valid(new L2TSHRAlloc.PathFromTSHR)))
    val toTSHR = Output(Vec(paramL2.mshrSize, new L2TSHRAlloc.PathToTSHR))

    val fromTSHRCtrl = new L2TSHRAlloc.PathFromTSHRCtrl
    val pfSrcID = if (quotaEnabled || reserveEnabled) Some(Input(UInt(p(xscache.oceanus.compactchi.CCHIParametersKey).UpstreamNodeID_Width.W))) else None
  })

  // Keep demand capacity physically separate from deferrable work. In contrast
  // to the PF owner quota, this mask includes external Stash and non-REQ fresh
  // allocations. It does not prevent coherence reuse of an active demand slot:
  // such nesting is required to complete the request and allocates no new slot.
  private val sharedSlots = (0 until nMSHR).filterNot(i => config.resv.exists(_._1 == i))
  val demandReserved = paramL2.demandReserve.map { initialReserve =>
    require(initialReserve >= 0 && initialReserve <= sharedSlots.size,
      "demandReserve must fit shared TSHRs")
    val configured = Constantin.createRecord("demandReserve", BigInt(initialReserve))
    val count = Mux(configured >= sharedSlots.size.U, sharedSlots.size.U, configured)
    VecInit((0 until nMSHR).map { i =>
      val rank = sharedSlots.indexOf(i)
      if (rank < 0) false.B else count > (sharedSlots.size - 1 - rank).U
    })
  }
  val demandRequest = if (reserveEnabled) {
    val req = io.fromTSHRCtrl.RXREQ.bits
    req.SrcID =/= io.pfSrcID.get && req.Opcode =/= CCHIOpcode.StashShared.U &&
      req.Opcode =/= CCHIOpcode.StashUnique.U
  } else true.B

  // -- The unified cluster bundle type to connect different channels into the same target Arbiter
  class ClusterBundle extends Bundle {
    val paddr = UInt((paramL2.physicalAddrWidth).W)
    val opcode = UInt(Seq(io.fromTSHRCtrl.RXEVT.bits.Opcode.getWidth, io.fromTSHRCtrl.RXSNP.bits.Opcode.get.getWidth, io.fromTSHRCtrl.RXREQ.bits.Opcode.getWidth).max.W)
    val target = new L2TSHRTarget
  }
  // ----------------------------------------------------------------

  // -- ClusterBundle wire instances that connect the actual channel input
  val preclusterRXEVT = Wire(Decoupled(new ClusterBundle))
  preclusterRXEVT.valid := io.fromTSHRCtrl.RXEVT.valid
  preclusterRXEVT.bits.paddr := io.fromTSHRCtrl.RXEVT.bits.Addr
  preclusterRXEVT.bits.opcode := io.fromTSHRCtrl.RXEVT.bits.Opcode
  preclusterRXEVT.bits.target := L2TSHRTarget.asEVT
  io.fromTSHRCtrl.RXEVT.ready := preclusterRXEVT.ready

  val preclusterRXSNP = Wire(Decoupled(new ClusterBundle))
  preclusterRXSNP.valid := io.fromTSHRCtrl.RXSNP.valid
  preclusterRXSNP.bits.paddr := io.fromTSHRCtrl.RXSNP.bits.Addr.get << 3
  preclusterRXSNP.bits.opcode := io.fromTSHRCtrl.RXSNP.bits.Opcode.get
  preclusterRXSNP.bits.target := L2TSHRTarget.asSNP
  io.fromTSHRCtrl.RXSNP.ready := preclusterRXSNP.ready

  val preclusterRXEVB = Wire(Decoupled(new ClusterBundle))
  preclusterRXEVB.valid := io.fromTSHRCtrl.RXEVB.valid
  preclusterRXEVB.bits.paddr := io.fromTSHRCtrl.RXEVB.bits.Addr
  preclusterRXEVB.bits.opcode := 0.U
  preclusterRXEVB.bits.target := L2TSHRTarget.asEVB
  io.fromTSHRCtrl.RXEVB.ready := preclusterRXEVB.ready

  // R01: conservative slot ownership. Any accepted PF REQ, including reuse
  // of a demand-origin slot, marks that shared TSHR until the whole slot is
  // deallocated. Demand/SNP/EVT/EVB reuse never clears an outstanding owner.
  // An already-owned same-PA slot costs no additional quota. The check is
  // combinational (one REQ admission per cycle), so there is no overshoot.
  val prefetchQuotaAllows = paramL2.prefetchMaxSharedTSHRs.orElse(paramL2.pfTshrQuota).map { initialLimit =>
    val shared = (0 until nMSHR).filterNot(i => config.resv.exists(_._1 == i))
    require(initialLimit >= 0 && initialLimit <= shared.size, "prefetch quota must fit shared TSHRs")
    val limit = if (paramL2.pfTshrQuota.nonEmpty) {
      val configured = Constantin.createRecord("pfTshrQuota", BigInt(initialLimit))
      Mux(configured >= shared.size.U, shared.size.U, configured)(log2Ceil(shared.size + 1) - 1, 0)
    } else initialLimit.U
    val owners = RegInit(VecInit(Seq.fill(nMSHR)(false.B)))
    val occupied = VecInit((0 until nMSHR).map(i => owners(i) && io.fromTSHR(i).valid))
    val used = PopCount(shared.map(i => occupied(i)))
    val alreadyOwned = shared.map(i => occupied(i) &&
      (io.fromTSHR(i).bits.paddr >> 6) === (io.fromTSHRCtrl.RXREQ.bits.Addr >> 6)).reduceOption(_ || _).getOrElse(false.B)
    val isPrefetch = io.fromTSHRCtrl.RXREQ.bits.SrcID === io.pfSrcID.get
    for(i <- 0 until nMSHR) {
      when(!io.fromTSHR(i).valid || io.toTSHR(i).alloc.asUInt.orR) { owners(i) := false.B }
      when(io.fromTSHRCtrl.RXREQ.fire && isPrefetch &&
        (io.toTSHR(i).alloc.REQ || io.toTSHR(i).reuse.REQ)) { owners(i) := true.B }
    }
    assert(used <= limit, "prefetch shared TSHR quota exceeded")
    if (paramL2.pfTshrQuota.nonEmpty) {
      val blocked = io.fromTSHRCtrl.RXREQ.valid && isPrefetch && !alreadyOwned && used >= limit
      // A denied episode can span many cycles. A changed transaction starts a
      // new episode (overwrite queues may replace their head under backpressure).
      // Intervening demand/arbitration can split one logical PF into episodes;
      // this is intentionally not labelled a globally unique request count.
      val previousBlocked = RegNext(blocked, false.B)
      val previousRequest = RegEnable(io.fromTSHRCtrl.RXREQ.bits.asUInt, blocked)
      val episode = blocked && (!previousBlocked || previousRequest =/= io.fromTSHRCtrl.RXREQ.bits.asUInt)
      XSPerfAccumulate("pfQuota_blocked_cycles", blocked)
      XSPerfAccumulate("pfQuota_denied_episodes", episode)
      XSPerfAccumulate("pfQuota_cycles", true.B)
      XSPerfAccumulate("pfQuota_occupancy_sum", used)
      for (level <- 0 to shared.size) {
        XSPerfAccumulate(s"pfQuota_occupancy_$level", used === level.U)
      }
      val active = PopCount(shared.map(i => io.fromTSHR(i).valid))
      XSPerfAccumulate("pfQuota_shared_full", active === shared.size.U)
      XSPerfAccumulate("pfQuota_req_valid", io.fromTSHRCtrl.RXREQ.valid)
      XSPerfAccumulate("pfQuota_req_stall", io.fromTSHRCtrl.RXREQ.valid && !io.fromTSHRCtrl.RXREQ.ready)
      XSPerfAccumulate("pfQuota_demand_stall", io.fromTSHRCtrl.RXREQ.valid && !isPrefetch && !io.fromTSHRCtrl.RXREQ.ready)
      XSPerfAccumulate("pfQuota_demand_accept", io.fromTSHRCtrl.RXREQ.fire && !isPrefetch)
    }
    dontTouch(used)
    !isPrefetch || alreadyOwned || used < limit
  }.getOrElse(true.B)

  val preclusterRXREQ = Wire(Decoupled(new ClusterBundle))
  preclusterRXREQ.valid := io.fromTSHRCtrl.RXREQ.valid && prefetchQuotaAllows
  preclusterRXREQ.bits.paddr := io.fromTSHRCtrl.RXREQ.bits.Addr
  preclusterRXREQ.bits.opcode := io.fromTSHRCtrl.RXREQ.bits.Opcode
  preclusterRXREQ.bits.target := L2TSHRTarget.asREQ
  io.fromTSHRCtrl.RXREQ.ready := preclusterRXREQ.ready && prefetchQuotaAllows
  // ----------------------------------------------------------------

  val postcluster = Wire(Vec(clusterCount, Decoupled(new ClusterBundle)))

  config.cluster.zipWithIndex.map { case (cluster, idx) => {

    val clustered = cluster.sortWith(_.priority > _.priority).map { _ match {
      case L2TSHRAllocTarget.EVT => preclusterRXEVT
      case L2TSHRAllocTarget.SNP => preclusterRXSNP
      case L2TSHRAllocTarget.EVB => preclusterRXEVB
      case L2TSHRAllocTarget.REQ => preclusterRXREQ
      case _ => { 
        require(false, "Illegal Allocation Target in cluster config")
        preclusterRXEVT // unused
      }
    }}

    if (cluster.size > 1) {
      if (config.clusterRRArb)
        fastArb(clustered, postcluster(idx), Some(s"${clusterName(idx)}"))
      else
        arb(clustered, postcluster(idx), Some(s"${clusterName(idx)}"))
    } else if (cluster.size == 1) {
      postcluster(idx) <> clustered.head
    } else {
      postcluster(idx).valid := false.B
      postcluster(idx).bits := 0.U.asTypeOf(postcluster(idx).bits)
    }
  }}

  val postcluster_isL1EVT = postcluster.map(p => p.bits.target.EVT)
  val postcluster_isL2EVB = postcluster.map(p => p.bits.target.EVB)
  val postcluster_isL3SNP = postcluster.map(p => p.bits.target.SNP)

  val postcluster_paddr_match_mat = Wire(Vec(clusterCount, Vec(clusterCount, Bool())))
  (0 until clusterCount).foreach (c1Idx => 
    (0 until clusterCount).foreach (c2Idx =>
      postcluster_paddr_match_mat(c1Idx)(c2Idx) := {
        if (c1Idx > c2Idx) postcluster(c1Idx).bits.paddr === postcluster(c2Idx).bits.paddr
        else if (c1Idx < c2Idx) postcluster_paddr_match_mat(c2Idx)(c1Idx)
        else true.B
  }))

  val paddr_hit_vec = postcluster.map(p => io.fromTSHR.map(t => t.valid && p.valid && (t.bits.paddr >> 6) === (p.bits.paddr >> 6)))
  val paddr_hit_any = paddr_hit_vec.map(ParallelOR(_))

  paddr_hit_vec.zipWithIndex.foreach { case (hit, idx) =>
    assert(PopCount(hit) <= 1.U, s"Multiple TSHRs hit the same post-cluster PA at ${clusterName(idx)}")
  }

  // -- Allocation and Reuse vector generations
  val alloc_vec = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))
  val reuse_vec = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))

  // Reservation rule: whether TSHR tIdx accepts cluster cIdx's allocation target
  val resv_allow = (0 until clusterCount).map(cIdx => io.fromTSHR.zipWithIndex.map { case (_, tIdx) =>
    val original = config.resv.find(_._1 == tIdx) match {
      case Some((_, L2TSHRResvTarget.L1EVT)) => postcluster_isL1EVT(cIdx)
      case Some((_, L2TSHRResvTarget.L2EVB)) => postcluster_isL2EVB(cIdx)
      case Some((_, L2TSHRResvTarget.L3SNP)) => postcluster_isL3SNP(cIdx)
      case Some(_) => {
        require(false, "Illegal Allocation Reservation in cluster config")
        false.B // unused
      }
      case None => true.B
    }
    demandReserved.map { protectedSlots =>
      original && (!protectedSlots(tIdx) || (postcluster(cIdx).bits.target.REQ && demandRequest))
    }.getOrElse(original)
  })

  // Allocation candidate vector conditions:
  //  - TSHR non-active
  //  - Request type matches on reservation TSHR
  //  - No previous cluster allocating on candidate TSHR with different PA in current cycle (see 'conflict mask')
  val can_alloc_vec = (0 until clusterCount).map(cIdx => io.fromTSHR.zipWithIndex.map { case (t, tIdx) =>
    !t.valid && resv_allow(cIdx)(tIdx) && /* conflict mask from other clusters */ !{
      if (cIdx == 0)
        false.B
      else
        ParallelOR((0 until cIdx).map { c2Idx =>
          alloc_vec(c2Idx)(tIdx) && !postcluster_paddr_match_mat(cIdx)(c2Idx) })
    }
  })

  if (reserveEnabled) {
    val protectedSlots = demandReserved.get
    val protectedActive = PopCount((0 until nMSHR).map(i => protectedSlots(i) && io.fromTSHR(i).valid))
    val deferrable = io.fromTSHRCtrl.RXREQ.valid && !demandRequest
    val hasUnprotectedFree = sharedSlots.map(i => !protectedSlots(i) && !io.fromTSHR(i).valid).reduce(_ || _)
    val protectedFree = sharedSlots.map(i => protectedSlots(i) && !io.fromTSHR(i).valid).reduce(_ || _)
    XSPerfAccumulate("demandReserve_cycles", true.B)
    XSPerfAccumulate("demandReserve_occupancy_sum", protectedActive)
    XSPerfAccumulate("demandReserve_demand_stall", io.fromTSHRCtrl.RXREQ.valid && demandRequest && !io.fromTSHRCtrl.RXREQ.ready)
    XSPerfAccumulate("demandReserve_demand_accept", io.fromTSHRCtrl.RXREQ.fire && demandRequest)
    XSPerfAccumulate("demandReserve_deferrable_stall", deferrable && !io.fromTSHRCtrl.RXREQ.ready)
    XSPerfAccumulate("demandReserve_capacity_blocked", deferrable && !io.fromTSHRCtrl.RXREQ.ready && !hasUnprotectedFree && protectedFree)
    for (i <- 0 until nMSHR) {
      when(protectedSlots(i) && io.toTSHR(i).alloc.asUInt.orR) {
        assert(io.toTSHR(i).alloc.REQ && demandRequest, "non-demand allocated a demand-reserved TSHR")
      }
      when(protectedSlots(i) && io.toTSHR(i).reuse.REQ) {
        assert(demandRequest, "prefetch or Stash reused a demand-reserved TSHR")
      }
    }
  }

  val alloc_vec_mask = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))
  alloc_vec_mask.zipWithIndex.foreach { case (mask, cIdx) =>
    mask.head := false.B
    mask.zipWithIndex.tail.foreach { case (m, tIdx) => m := mask(tIdx - 1) || can_alloc_vec(cIdx)(tIdx - 1) }
  }

  // Reuse candidate vector conditions:
  //  - PA hit on the active TSHR
  //  - For the REQ target only: admitted by the reservation rule on that TSHR, so a
  //    reservation-reserved TSHR is never converted into a long-lived refiller
  //    (SNP/EVT/EVB reuse stays reservation-exempt: snoops must never be blocked,
  //    and their nesting adds only bounded host work)
  //  - Channel receivable (non-busy) by the active TSHR
  val can_reuse_vec = (0 until clusterCount).map(cIdx => io.fromTSHR.zipWithIndex.map { case (t, tIdx) =>
    paddr_hit_vec(cIdx)(tIdx) && (resv_allow(cIdx)(tIdx) || !postcluster(cIdx).bits.target.REQ) && (
      !t.bits.busy.EVT && postcluster(cIdx).bits.target.EVT ||
      !t.bits.busy.SNP && postcluster(cIdx).bits.target.SNP ||
      !t.bits.busy.REQ && postcluster(cIdx).bits.target.REQ ||
      !t.bits.busy.EVB && postcluster(cIdx).bits.target.EVB)
  })

  val can_reuse_any = can_reuse_vec.map(ParallelOR(_))

  val reuse_vec_mask = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))
  reuse_vec_mask.zipWithIndex.foreach { case (mask, cIdx) =>
    mask.head := false.B
    mask.zipWithIndex.tail.foreach { case (m, tIdx) => m := mask(tIdx - 1) || can_reuse_vec(cIdx)(tIdx - 1) }
  }

  // Cross-cluster same-PA merge: when an earlier (higher-priority) cluster allocates a TSHR
  // for the same PA in the same cycle, the later cluster either merges into the SAME TSHR
  // (when the reservation rule admits the later cluster's target on that TSHR) or is blocked
  // for this cycle; a blocked cluster retries and nests into the sibling TSHR through the
  // reuse path. Reuse is reservation-exempt for SNP/EVT/EVB targets; a blocked REQ cluster
  // instead waits for the sibling TSHR to drain, then allocates its own
  // reservation-admitted TSHR.
  val xcluster_merge_valid = Wire(Vec(clusterCount, Bool()))
  val xcluster_merge_oh    = Wire(Vec(clusterCount, Vec(io.fromTSHR.length, Bool())))
  val xcluster_merge_legal = Wire(Vec(clusterCount, Bool()))
  (0 until clusterCount).foreach { cIdx =>
    if (cIdx == 0) {
      xcluster_merge_valid(cIdx) := false.B
      xcluster_merge_oh(cIdx)    := VecInit(Seq.fill(io.fromTSHR.length)(false.B))
      xcluster_merge_legal(cIdx) := false.B
    } else {
      val earlierHits = (0 until cIdx).map(c2Idx =>
        postcluster(c2Idx).valid && postcluster_paddr_match_mat(cIdx)(c2Idx) && alloc_vec(c2Idx).asUInt.orR)
      xcluster_merge_valid(cIdx) := postcluster(cIdx).valid && ParallelOR(earlierHits)
      xcluster_merge_oh(cIdx)    := PriorityMux(earlierHits, (0 until cIdx).map(c2Idx => alloc_vec(c2Idx)))
      xcluster_merge_legal(cIdx) := ParallelOR((0 until io.fromTSHR.length).map(tIdx =>
                                      xcluster_merge_oh(cIdx)(tIdx) && resv_allow(cIdx)(tIdx)))
    }
  }

  // Allocation vector conditions:
  //  - Same-PA earlier cluster allocated: merge into the same TSHR when reservation-legal, else blocked
  //  - Otherwise: the TSHR was the candidate
  //  - Not masked by other better allocation candidate
  //  - No any PA hit on active TSHR (might be the reuse candidate)
  alloc_vec.zipWithIndex.foreach { case (alloc_vec, cIdx) =>
    alloc_vec.zipWithIndex.foreach { case (alloc, tIdx) =>
      alloc := Mux(xcluster_merge_valid(cIdx),
                   xcluster_merge_legal(cIdx) && xcluster_merge_oh(cIdx)(tIdx),
                   can_alloc_vec(cIdx)(tIdx) && !alloc_vec_mask(cIdx)(tIdx) && !paddr_hit_any(cIdx) && postcluster(cIdx).valid) }
    assert(PopCount(alloc_vec) <= 1.U, s"Multiple TSHR allocation on ${clusterName(cIdx)}")
  }

  // Reuse vector conditions:
  //  - The TSHR was the candidate
  //  - Not masked by other better reuse candidate
  reuse_vec.zipWithIndex.foreach { case (reuse_vec, cIdx) =>
    reuse_vec.zipWithIndex.foreach { case (reuse, tIdx) => 
      reuse := can_reuse_vec(cIdx)(tIdx) && !reuse_vec_mask(cIdx)(tIdx) }
    assert(PopCount(reuse_vec) <= 1.U, s"Multiple TSHR reuse on ${clusterName(cIdx)}")
  }

  val alloc_any = alloc_vec.map(_.asUInt.orR)
  val reuse_any = reuse_vec.map(_.asUInt.orR)

  alloc_any.zip(reuse_any).zipWithIndex.foreach { case ((a, r), cIdx) =>
    assert(!a || !r, s"Both allocating and reusing TSHRs on ${clusterName(cIdx)}")
  }
  // ----------------------------------------------------------------

  postcluster.zipWithIndex.foreach { case (p, cIdx) => p.ready := alloc_any(cIdx) || reuse_any(cIdx) }

  config.cluster.zipWithIndex.foreach { case (cluster, cIdx) => {
    cluster.foreach { _ match {
      case L2TSHRAllocTarget.EVT => {
        io.toTSHR.map(_.alloc).zip(alloc_vec(cIdx)).foreach(t => t._1.EVT := t._2 && postcluster(cIdx).bits.target.EVT)
        io.toTSHR.map(_.reuse).zip(reuse_vec(cIdx)).foreach(t => t._1.EVT := t._2 && postcluster(cIdx).bits.target.EVT)
      }
      case L2TSHRAllocTarget.SNP => {
        io.toTSHR.map(_.alloc).zip(alloc_vec(cIdx)).foreach(t => t._1.SNP := t._2 && postcluster(cIdx).bits.target.SNP)
        io.toTSHR.map(_.reuse).zip(reuse_vec(cIdx)).foreach(t => t._1.SNP := t._2 && postcluster(cIdx).bits.target.SNP)
      }
      case L2TSHRAllocTarget.EVB => {
        io.toTSHR.map(_.alloc).zip(alloc_vec(cIdx)).foreach(t => t._1.EVB := t._2 && postcluster(cIdx).bits.target.EVB)
        io.toTSHR.map(_.reuse).zip(reuse_vec(cIdx)).foreach(t => t._1.EVB := t._2 && postcluster(cIdx).bits.target.EVB)
      }
      case L2TSHRAllocTarget.REQ => {
        io.toTSHR.map(_.alloc).zip(alloc_vec(cIdx)).foreach(t => t._1.REQ := t._2 && postcluster(cIdx).bits.target.REQ)
        io.toTSHR.map(_.reuse).zip(reuse_vec(cIdx)).foreach(t => t._1.REQ := t._2 && postcluster(cIdx).bits.target.REQ)
      }
      case _ => {}
  }}}}

  assert((PopCount(io.toTSHR.map(_.alloc).asUInt) + PopCount(io.toTSHR.map(_.reuse).asUInt)) <= clusterCount.U, 
    "Allocated and reused TSHRs more than cluster count")

  assert(PopCount(io.toTSHR.map(_.alloc.EVT).asUInt) <= 1.U, "Multiple TSHR allocated for EVT within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.alloc.SNP).asUInt) <= 1.U, "Multiple TSHR allocated for SNP within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.alloc.EVB).asUInt) <= 1.U, "Multiple TSHR allocated for EVB within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.alloc.REQ).asUInt) <= 1.U, "Multiple TSHR allocated for REQ within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.reuse.EVT).asUInt) <= 1.U, "Multiple TSHR reused for EVT within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.reuse.SNP).asUInt) <= 1.U, "Multiple TSHR reused for SNP within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.reuse.EVB).asUInt) <= 1.U, "Multiple TSHR reused for EVB within 1 cycle")
  assert(PopCount(io.toTSHR.map(_.reuse.REQ).asUInt) <= 1.U, "Multiple TSHR reused for REQ within 1 cycle")

  io.toTSHR.zipWithIndex.foreach { case (t, tIdx) =>
    t.paddr := ParallelMux(alloc_vec.map(_(tIdx)).zip(reuse_vec.map(_(tIdx))).zipWithIndex.map { 
      case ((alloc, reuse), cIdx) => {
        (alloc, postcluster(cIdx).bits.paddr, cIdx) /*'paddr' output valid only on allocation*/ 
    }}.map { case (en, paddr, cIdx) => {
      assert(!en || paddr === t.paddr, s"Disrupted PA output to TSHR ${tIdx} on ${clusterName(cIdx)}")
      (en, paddr)
    }}
  )}

  
  // Experiment-only diagnostics. No signal below feeds the allocator policy.
  // Lifetime type is the first admitted channel, with cluster priority resolving
  // simultaneous same-slot allocations. It never changes on later nesting.
  if (paramL2.pfTshrQuota.nonEmpty || reserveEnabled) {
    val names = Seq("demand_read", "internal_prefetch", "external_stash", "write_release", "snoop", "evictback", "other")
    val kind = RegInit(VecInit(Seq.fill(nMSHR)(6.U(3.W))))
    val req = io.fromTSHRCtrl.RXREQ.bits
    val isInternalPF = req.SrcID === io.pfSrcID.get
    val isStash = req.Opcode === CCHIOpcode.StashShared.U || req.Opcode === CCHIOpcode.StashUnique.U
    val isRead = Seq(CCHIOpcode.ReadShared, CCHIOpcode.ReadUnique, CCHIOpcode.ReadOnce, CCHIOpcode.ReadNoSnp)
      .map(op => req.Opcode === op.U).reduce(_ || _)
    val reqKind = Mux(isInternalPF, 1.U, Mux(isStash, 2.U, Mux(isRead, 0.U, 3.U)))
    for (i <- 0 until nMSHR) {
      val allocating = alloc_vec.map(_(i))
      val selectedKind = PriorityMux(allocating.zipWithIndex.map { case (hit, c) =>
        val t = postcluster(c).bits.target
        hit -> Mux(t.EVT, 3.U, Mux(t.SNP, 4.U, Mux(t.EVB, 5.U, reqKind)))
      })
      when(allocating.reduce(_ || _)) { kind(i) := selectedKind }
    }
    XSPerfAccumulate("quotaDiag_cycles", true.B)
    XSPerfAccumulate("quotaDiag_occupancy_total", PopCount(io.fromTSHR.map(_.valid)))
    for ((name, index) <- names.zipWithIndex) {
      XSPerfAccumulate(s"quotaDiag_occupancy_$name", PopCount((0 until nMSHR).map(i => io.fromTSHR(i).valid && kind(i) === index.U)))
    }
    val offered = io.fromTSHRCtrl.RXREQ.valid
    val blocked = offered && !io.fromTSHRCtrl.RXREQ.ready
    val noSharedFree = sharedSlots.map(i => io.fromTSHR(i).valid).reduce(_ && _)
    // Reasons are exclusive, with precedence quota -> occupied same line ->
    // total shared capacity -> protected capacity -> competing cluster work.
    val quotaReason = !prefetchQuotaAllows
    val sameLineReason = !quotaReason && io.fromTSHR.map(t => t.valid &&
      (t.bits.paddr >> 6) === (req.Addr >> 6)).reduce(_ || _)
    val capacityReason = !quotaReason && !sameLineReason && noSharedFree
    val protectedReason = !quotaReason && !sameLineReason && !capacityReason && !demandRequest &&
      !sharedSlots.map(i => !io.fromTSHR(i).valid && !demandReserved.map(_(i)).getOrElse(false.B)).reduce(_ || _)
    val arbitrationReason = !quotaReason && !sameLineReason && !capacityReason && !protectedReason
    val reasons = Seq("pf_quota" -> quotaReason, "same_line" -> sameLineReason, "shared_full" -> capacityReason,
      "demand_reserved" -> protectedReason, "arbitration" -> arbitrationReason)
    XSPerfAccumulate("quotaDiag_req_valid", offered)
    XSPerfAccumulate("quotaDiag_req_stall", blocked)
    XSPerfAccumulate("quotaDiag_external_read_valid", offered && !isInternalPF && isRead)
    XSPerfAccumulate("quotaDiag_external_read_stall", blocked && !isInternalPF && isRead)
    for ((name, reason) <- reasons) {
      XSPerfAccumulate(s"quotaDiag_stall_$name", blocked && reason)
      XSPerfAccumulate(s"quotaDiag_external_read_stall_$name", blocked && !isInternalPF && isRead && reason)
    }
  }

  // -- Performance counters
  val perf_stallCycleCnt_EVT = RegInit(0.U(32.W))
  val perf_stallCycleCnt_SNP = RegInit(0.U(32.W))
  val perf_stallCycleCnt_EVB = RegInit(0.U(32.W))
  val perf_stallCycleCnt_REQ = RegInit(0.U(32.W))
  when (io.fromTSHRCtrl.RXEVT.fire) {
    perf_stallCycleCnt_EVT := 0.U
  }.elsewhen (io.fromTSHRCtrl.RXEVT.valid && !io.fromTSHRCtrl.RXEVT.ready) {
    perf_stallCycleCnt_EVT := perf_stallCycleCnt_EVT + 1.U
  }
  when (io.fromTSHRCtrl.RXSNP.fire) {
    perf_stallCycleCnt_SNP := 0.U
  }.elsewhen (io.fromTSHRCtrl.RXSNP.valid && !io.fromTSHRCtrl.RXSNP.ready) {
    perf_stallCycleCnt_SNP := perf_stallCycleCnt_SNP + 1.U
  }
  when (io.fromTSHRCtrl.RXEVB.fire) {
    perf_stallCycleCnt_EVB := 0.U
  }.elsewhen (io.fromTSHRCtrl.RXEVB.valid && !io.fromTSHRCtrl.RXEVB.ready) {
    perf_stallCycleCnt_EVB := perf_stallCycleCnt_EVB + 1.U
  }
  when (io.fromTSHRCtrl.RXREQ.fire) {
    perf_stallCycleCnt_REQ := 0.U
  }.elsewhen (io.fromTSHRCtrl.RXREQ.valid && !io.fromTSHRCtrl.RXREQ.ready) {
    perf_stallCycleCnt_REQ := perf_stallCycleCnt_REQ + 1.U
  }

  XSPerfAccumulate("L2TSHRAlloc_alloc_EVT", io.toTSHR.map(_.alloc.EVT).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_alloc_SNP", io.toTSHR.map(_.alloc.SNP).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_alloc_EVB", io.toTSHR.map(_.alloc.EVB).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_alloc_REQ", io.toTSHR.map(_.alloc.REQ).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_reuse_EVT", io.toTSHR.map(_.reuse.EVT).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_reuse_SNP", io.toTSHR.map(_.reuse.SNP).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_reuse_EVB", io.toTSHR.map(_.reuse.EVB).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_reuse_REQ", io.toTSHR.map(_.reuse.REQ).asUInt.orR)
  XSPerfAccumulate("L2TSHRAlloc_stallCycleCnt_EVT_total", io.fromTSHRCtrl.RXEVT.valid && !io.fromTSHRCtrl.RXEVT.ready)
  XSPerfAccumulate("L2TSHRAlloc_stallCycleCnt_SNP_total", io.fromTSHRCtrl.RXSNP.valid && !io.fromTSHRCtrl.RXSNP.ready)
  XSPerfAccumulate("L2TSHRAlloc_stallCycleCnt_EVB_total", io.fromTSHRCtrl.RXEVB.valid && !io.fromTSHRCtrl.RXEVB.ready)
  XSPerfAccumulate("L2TSHRAlloc_stallCycleCnt_REQ_total", io.fromTSHRCtrl.RXREQ.valid && !io.fromTSHRCtrl.RXREQ.ready)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_EVT", perf_stallCycleCnt_EVT, io.fromTSHRCtrl.RXEVT.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_EVT", perf_stallCycleCnt_EVT, io.fromTSHRCtrl.RXEVT.fire, 40, 800, 40, left_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_SNP", perf_stallCycleCnt_SNP, io.fromTSHRCtrl.RXSNP.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_SNP", perf_stallCycleCnt_SNP, io.fromTSHRCtrl.RXSNP.fire, 40, 800, 40, left_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_EVB", perf_stallCycleCnt_EVB, io.fromTSHRCtrl.RXEVB.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_EVB", perf_stallCycleCnt_EVB, io.fromTSHRCtrl.RXEVB.fire, 40, 800, 40, left_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_REQ", perf_stallCycleCnt_REQ, io.fromTSHRCtrl.RXREQ.fire, 0, 40, 2, right_strict = true)
  XSPerfHistogram("L2TSHRAlloc_stallCycleCnt_REQ", perf_stallCycleCnt_REQ, io.fromTSHRCtrl.RXREQ.fire, 40, 800, 40, left_strict = true)

  io.toTSHR.zipWithIndex.foreach { case (t, i) => {
    XSPerfAccumulate(s"L2TSHRAlloc_alloc_${i}_EVT", t.alloc.EVT)
    XSPerfAccumulate(s"L2TSHRAlloc_alloc_${i}_SNP", t.alloc.SNP)
    XSPerfAccumulate(s"L2TSHRAlloc_alloc_${i}_EVB", t.alloc.EVB)
    XSPerfAccumulate(s"L2TSHRAlloc_alloc_${i}_REQ", t.alloc.REQ)
    XSPerfAccumulate(s"L2TSHRAlloc_alloc_${i}", t.alloc.asUInt.orR)
    XSPerfAccumulate(s"L2TSHRAlloc_reuse_${i}_EVT", t.reuse.EVT)
    XSPerfAccumulate(s"L2TSHRAlloc_reuse_${i}_SNP", t.reuse.SNP)
    XSPerfAccumulate(s"L2TSHRAlloc_reuse_${i}_EVB", t.reuse.EVB)
    XSPerfAccumulate(s"L2TSHRAlloc_reuse_${i}_REQ", t.reuse.REQ)
    XSPerfAccumulate(s"L2TSHRAlloc_reuse_${i}", t.reuse.asUInt.orR)
  }}
  // ----------------------------------------------------------------
}
