package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import utility.sram.{SRAMTemplate, SramBroadcastBundle}
import oceanus.l2._
import oceanus.compactchi._
import org.chipsalliance.cde.config.{Field, Parameters}
import xscache.oceanus.compactchi.HasCCHIParameters

case object L2DirectoryBlockRefillFaultModeKey extends Field[Int](0)

object L2DirectoryBlockRefillFaultMode {
  val NeverUnlock = 2
}

object L2Directory {

  // Refill lock table entry: one slot per TSHR (indexed by TSHR ID), holding the
  // victim (set,way) picked by that TSHR's in-flight refill until its fill
  // commit lands. A TSHR holds at most one outstanding lock by construction
  // (repl_reset = rxreq_fire: the replace FSM resets only when a new request
  // enters the TSHR, after the previous refill committed).
  class RefillLockEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
    val valid = Bool()
    val set = UInt(setBits.W)
    val way = UInt(wayBits.W)
  }

  object MetaState {

    def apply() = UInt(2.W)

    def UU: UInt = 3.U(2.W)
    def US: UInt = 2.U(2.W)
    def S: UInt = 1.U(2.W)
    def I: UInt = 0.U(2.W)

    def atLeastS(state: UInt): Bool = state >= MetaState.S
    def atLeastU(state: UInt): Bool = state >= MetaState.US

    def isClientU(state: UInt, client: Bool) = {
      Mux(state === UU, client, false.B)
    }

    def isClientS(state: UInt, client: Bool) = {
      Mux(state === US || state === S, client, false.B)
    }

    def toLocal(state: UInt): L2CacheLocalState = {
      val value = Wire(new L2CacheLocalState)
      value.bits := ParallelLookUp(state, Seq(
        (UU, L2CacheStates.U),
        (US, L2CacheStates.U),
        (S , L2CacheStates.S),
        (I , L2CacheStates.I)
      ))
      value
    }
  }

  // Meta: coherence state (NO tag — tag is internal to Directory)
  class Meta(implicit val p: Parameters) extends Bundle with HasL2Params with HasCCHIParameters {
    val state = MetaState()
    val dirty = Bool()
    val clients = Vec(1, Bool()) // TODO: parameterize with coherent l2 client count
    val alias = UInt(paramCCHI.TagAlias_Width.W)
  }

  class MetaReadResult(implicit override val p: Parameters) extends Meta with HasL2Params {
    val way = UInt(wayBits.W)
    val hit = Bool() // DirRd: tag hit. ReplRd: unused.
  }

  class MetaWriteMask(implicit val p: Parameters) extends Bundle with HasL2Params {
    val state = Bool()
    val dirty = Bool()
    val clients = Vec(1, Bool()) // TODO: parameterize with coherent l2 client count
    val alias = Bool()

    def any = this.asUInt.orR

    def maskAndWrite(dst: Meta, src: Meta): Unit = {
      when (state) { dst.state := src.state }
      when (dirty) { dst.dirty := src.dirty }
      when (alias) { dst.alias := src.alias }
      clients.zip(dst.clients.zip(src.clients)).foreach { case (mask, (dst, src)) => when (mask) { dst := src } }
    }

    def maskAndWrite(dst: Meta, dstFlag: MetaWriteMask, src: Meta): Unit = {
      maskAndWrite(dst, src)
      when (state) { dstFlag.state := true.B }
      when (dirty) { dstFlag.dirty := true.B }
      when (alias) { dstFlag.alias := true.B }
      clients.zip(dstFlag.clients).foreach { case (mask, dst) => when (mask) { dst := true.B } }
    }

    def unmaskAndWrite(dst: Meta, src: Meta): Unit = {
      when (!state) { dst.state := src.state }
      when (!dirty) { dst.dirty := src.dirty }
      when (!alias) { dst.alias := src.alias }
      clients.zip(dst.clients.zip(src.clients)).foreach { case (mask, (dst, src)) => when (!mask) { dst := src } }
    }
  }

  object MetaWriteMask {
    def empty(implicit p: Parameters): MetaWriteMask = 0.U.asTypeOf(new MetaWriteMask)
  }

  class ReplReadResult(implicit override val p: Parameters) extends Bundle with HasL2Params {
    val paddr = UInt(paramL2.physicalAddrWidth.W) // victim PA; valid when META.state =/= I
    val way = UInt(wayBits.W)
  }

  class PathToDirectoryUOPs extends Bundle {
    val DirRd = Bool()
    val DirWb = Bool()
    val ReplRd = Bool()
    val DirRel = Bool() // DRRIP release-type update (commit path)
  }

  class PathToDirectory(implicit val p: Parameters) extends PathToDirectoryUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val PADDR = UInt(paramL2.physicalAddrWidth.W)
    val WAY = UInt(wayBits.W)
    val META = new L2Directory.Meta // only applicable for DirWb
    val META_WEN = new L2Directory.MetaWriteMask // only applicable for DirWb
    val TAG_WEN = Bool()
  }

  class PathFromDirectoryUOPs extends Bundle {
    val DirRdArbComp = Bool()
    val DirRdResp = Bool()
    val DirWbArbComp = Bool()
    val ReplRdArbComp = Bool()
    val ReplRdResp = Bool()
    val ReplRdRetryAck = Bool()
    val DirRelArbComp = Bool()
  }

  class PathFromDirectory(implicit val p: Parameters) extends PathFromDirectoryUOPs with HasL2Params {
    val TSHRID = UInt(mshrIndexWidth.W)
    val META = new L2Directory.MetaReadResult
    val REPL = new L2Directory.ReplReadResult // only applicable for ReplRd
  }

  // Internal SRAM entry: tag + meta together
  class DirEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
    val tag = UInt(tagWidth.W)
    val meta = new L2Directory.Meta
  }
}

// ══════════════════════════════════════════════════════════════════════
// Tree-PLRU, generic power-of-two way count (ways-1 bits/set), selected by
// L2Params.replacement = "plru".
//
// State bits are heap-indexed tree nodes: node 0 is the root, the children
// of node i are 2i+1 (left/lower) and 2i+2 (right/higher). A CLEAR bit
// marks the left (lower) subtree as the older one — the victim walk
// descends left on a clear bit, right on a set bit; a touch sets every
// node on the touched way's root-to-leaf path to point AWAY from it.
// Layout for 8 ways (7 bits):
//                      bit[0]: ways 3-0 older than ways 7-4
//                      /                                  \
//            bit[1]: ways 1+0 > 3+2                bit[2]: ways 5+4 > 7+6
//            /                    \                /                    \
//     bit[3]: way 1>0    bit[4]: way 3>2    bit[5]: way 5>4    bit[6]: way 7>6
// (">" = "older than" when the bit is SET. At 4 ways this degenerates to
// bit[0] root + bit[1]/bit[2] leaves — bit-identical to the old PLRU4.)
// ══════════════════════════════════════════════════════════════════════

object TreePLRU {
  def nBits(ways: Int) = ways - 1

  // Victim: walk the tree from the root, descending into the older subtree
  // (clear bit = left/lower). Way assembled msb-first.
  def getReplaceWay(state: UInt, ways: Int): UInt = {
    def pick(node: Int, sub: Int): UInt =
      if (sub == 2) state(node)
      else Cat(state(node),
               Mux(state(node) === 0.U, pick(2 * node + 1, sub / 2),
                                        pick(2 * node + 2, sub / 2)))
    pick(0, ways)
  }

  // Touch: every node on the touched way's path is set away from the touched
  // direction. Evaluated per node with static indices: node i sits at level
  // l = floor(log2(i+1)) with path prefix (i+1 - 2^l); it is on the touched
  // path iff the touchWay bits above its level equal that prefix.
  def getNextState(state: UInt, touchWay: UInt, ways: Int): UInt = {
    val levels = log2Ceil(ways)
    val next = Wire(Vec(ways - 1, Bool()))
    for (i <- 0 until ways - 1) {
      val l = BigInt(i + 1).bitLength - 1
      val onPath = if (l == 0) true.B
                   else touchWay(levels - 1, levels - l) === (i + 1 - (1 << l)).U
      next(i) := Mux(onPath, !touchWay(levels - 1 - l), state(i))
    }
    next.asUInt
  }
}

// ══════════════════════════════════════════════════════════════════════
// DRRIP: 2-bit RRPV per way, generic way count (selected by
// L2Params.replacement = "drrip").
// ══════════════════════════════════════════════════════════════════════

object DRRIP {
  def nBits(ways: Int) = 2 * ways

  // Victim: way with the maximum RRPV (least re-referenced); ties break to
  // the lowest way index (mirrors coupledL2 get_replace_OH + MaskToOH).
  def getReplaceWay(state: UInt, ways: Int): UInt = {
    val rrpv = VecInit((0 until ways).map(i => state(2 * i + 1, 2 * i)))
    val isMax = VecInit(rrpv.map(e => !VecInit(rrpv.map(o => o > e)).asUInt.orR))
    PriorityEncoder(isMax.asUInt)
  }

  // DirRd hit: promote the hit way to MRU (RRPV 0); other ways unchanged.
  def getNextStateHit(state: UInt, way: UInt, ways: Int): UInt = {
    val next = Wire(Vec(ways, UInt(2.W)))
    next := state.asTypeOf(Vec(ways, UInt(2.W)))
    next(way) := 0.U
    next.asUInt
  }

  // Successful refill: insert the new line at RRPV 0
  def getNextStateRefill(state: UInt, way: UInt, victimValid: Bool, ways: Int): UInt = {
    val cur = VecInit((0 until ways).map(i => state(2 * i + 1, 2 * i)))
    val inc = 3.U(2.W) - cur(way)
    val next = Wire(Vec(ways, UInt(2.W)))
    for (i <- 0 until ways)
      next(i) := Mux(i.U === way, 0.U, Mux(victimValid, cur(i) + inc, cur(i)))
    next.asUInt
  }

  // Release-type update (client release, delivered via the EVT channel — the
  // entRelFire / dirRelFire sites below)
  def getNextStateRelease(state: UInt, way: UInt, reuse: Bool, ways: Int): UInt = {
    val next = Wire(Vec(ways, UInt(2.W)))
    next := state.asTypeOf(Vec(ways, UInt(2.W)))
    next(way) := Mux(reuse, 0.U, 2.U)
    next.asUInt
  }
}

// ══════════════════════════════════════════════════════════════════════
// Directory (with integrated arbiter)
// ══════════════════════════════════════════════════════════════════════
//
// Each TSHR connects one-to-one: io.toDir(i) / io.fromDir(i).
// Internal arbiter selects one request per cycle (ReplRd > DirRd > DirWb,
// with starvation protection on Level-2 and round-robin within a group).
// The winner enters a 3-stage pipeline: S1 decode + SRAM/PLRU read,
// S2 latch, S3 hit/way/result.
//
// ArbComp: DirRd/ReplRd ArbComp asserted combinationally the cycle the
//   request wins arbitration (cycle N). DirWb ArbComp is delayed one
//   cycle (N+1) so the bitmask write has landed and later reads will hit.
// Resp: driven at S3, routed one-to-one to fromDir(s3_tshr).
//
// replStall (internal): any in-flight replacement-state writer blocks new
//   ReplRd grants, closing the replRegs S3-write/S1-read RAW window.

class Directory(implicit val p: Parameters) extends Module with HasL2Params {
  val MAX_AGE = 15.U(4.W)
  val idxW    = log2Ceil(nMSHR)
  // Replacement state width: 2 bits/way RRPV under DRRIP; tree-PLRU uses the
  // low ways-1.
  val replBits = DRRIP.nBits(paramL2.ways) // >= 4 for ways >= 2; covers TreePLRU.nBits
  private val enableDirStateProbe = p(L2SliceDirStateProbeEnableKey)
  private val enableDirPlruProbe = p(L2SliceDirPlruProbeEnableKey)
  private val blockRefillFaultMode = p(L2DirectoryBlockRefillFaultModeKey).U(2.W)

  val io = IO(new Bundle {
    val toDir   = Input(Vec(nMSHR, new L2Directory.PathToDirectory))
    val fromDir = Output(Vec(nMSHR, new L2Directory.PathFromDirectory))
    val tshrIdle = Input(Vec(nMSHR, Bool()))
    // DRRIP release-type update, entrance path: WayValid EVT entrance train,
    // reduced one-hot from the TSHRs (at most one EVT entrance per cycle per
    // slice by allocator construction; see L2TSHRAlloc assertions).
    // Elaborated only when the entrance path is enabled.
    val evtRelTrain = if (paramL2.enableDRRIPEntranceReleaseUpdate) {
      Some(Input(Valid(new Bundle {
        val set = UInt(setBits.W)
        val way = UInt(wayBits.W)
      })))
    } else {
      None
    }
    val debugStateWrite = if (enableDirStateProbe) {
      Some(Output(new Bundle {
        val valid = Bool()
        val set = UInt(setBits.W)
        val way = UInt(wayBits.W)
        val state = L2Directory.MetaState()
        val dirty = Bool()
        val client0 = Bool()
      }))
    } else {
      None
    }
    val debugRrip = if (enableDirPlruProbe) {
      Some(Output(new Bundle {
        val valid = Bool()
        val set = UInt(setBits.W)
        val state = UInt(replBits.W)
        val s3Valid = Bool()
        val s3Tshr = UInt(mshrIndexWidth.W)
        val s3IsDirRd = Bool()
        val s3IsRepl = Bool()
        val wayS3 = UInt(wayBits.W)
        val s3Rrip = UInt(replBits.W)
        val currentSetRrip = UInt(replBits.W)
      }))
    } else {
      None
    }
    val debugRetry = if (enableDirPlruProbe) {
      Some(Output(new Bundle {
        val replStall = Bool()
        val s2Valid = Bool()
        val s2Tshr = UInt(log2Ceil(paramL2.mshrSize).W)
        val s2IsDirRd = Bool()
        val s2IsRepl = Bool()
        val s2Set = UInt(setBits.W)
        val s3Valid = Bool()
        val s3Tshr = UInt(log2Ceil(paramL2.mshrSize).W)
        val s3IsDirRd = Bool()
        val s3IsRepl = Bool()
        val s3Set = UInt(setBits.W)
        val hasInvalid = Bool()
        val invalidWay = UInt(wayBits.W)
        val hitWay = UInt(wayBits.W)
        val replWay = UInt(wayBits.W)
        val chosenWay = UInt(wayBits.W)
        val victimWay = UInt(wayBits.W)
        val blockRefillMaskS2 = UInt(paramL2.ways.W)
        val freeWayMaskS3 = UInt(paramL2.ways.W)
        val dirWbDone = Bool()
        val dirWbDoneSet = UInt(setBits.W)
        val dirWbDoneWay = UInt(wayBits.W)
        val blockRefillLocked = Vec(paramL2.sets, UInt(paramL2.ways.W))
        val ageWb = UInt(4.W)
        val ageRd = UInt(4.W)
        val rrReplRd = UInt(log2Ceil(paramL2.mshrSize).W)
        val rrDirWb = UInt(log2Ceil(paramL2.mshrSize).W)
        val rrDirRd = UInt(log2Ceil(paramL2.mshrSize).W)
        val replRegs = Vec(paramL2.sets, UInt(replBits.W))
      }))
    } else {
      None
    }
  })

  val sets = paramL2.sets
  val ways = paramL2.ways
  val useDRRIP = paramL2.replacement == "drrip"
  require(paramL2.replacement == "plru" || useDRRIP,
    s"Directory: unknown replacement policy '${paramL2.replacement}'")
  require(isPow2(ways) && ways >= 2,
    s"Directory: ways must be a power of two >= 2, got $ways")

  // ── SRAM + replacement state ────────────────────────────────────────
  val dirArray = Module(new SRAMTemplate(
    gen = new L2Directory.DirEntry, set = sets, way = ways,
    singlePort = true, useBitmask = true, hasMbist = paramL2.hasMbist
  ))
  // No top-level broadcast source in a standalone L2; tie off the mbist
  // broadcast IO that SRAMTemplate exposes when hasMbist is set.
  dirArray.io.broadcast.foreach(_ := 0.U.asTypeOf(new SramBroadcastBundle))
  // Replacement state registers (see replBits above).
  val replRegs = RegInit(VecInit(Seq.fill(sets)(0.U(replBits.W))))
  // DRRIP release-train reuse tracking: cleared on refill, set on DirRd hit, 
  // set on a release train. DRRIP-elaboration only.
  val originBits = RegInit(VecInit(Seq.fill(sets)(0.U(ways.W))))
  // Refill lock table: one slot per TSHR, indexed directly by TSHR ID (at most
  // one outstanding refill lock per TSHR by construction). pendClear* is the
  // 1-deep pending-clear register: a lock is released one cycle after the
  // owner's fill-commit DirWb grant, keeping the victim way unpickable through
  // the commit cycle + 1 exactly as the old per-(set,way) design did.
  val lockTable = RegInit(VecInit(Seq.fill(nMSHR)(0.U.asTypeOf(new L2Directory.RefillLockEntry))))
  val pendClearValid = RegInit(false.B)
  val pendClearIdx = RegInit(0.U(idxW.W))
  val initActive = RegInit(true.B)
  val initSet = RegInit(0.U(setBits.W))
  val initMeta = Wire(new L2Directory.Meta)
  initMeta.state := L2Directory.MetaState.I
  initMeta.dirty := false.B
  initMeta.alias := 0.U
  initMeta.clients.foreach(_ := false.B)
  val initEntry = Wire(new L2Directory.DirEntry)
  initEntry.tag := 0.U
  initEntry.meta := initMeta
  val initMask = Wire(new L2Directory.DirEntry)
  initMask.tag := Fill(tagWidth, true.B)
  initMask.meta.state := Fill(2, true.B)
  initMask.meta.dirty := true.B
  initMask.meta.alias := Fill(2, true.B)
  initMask.meta.clients.foreach(_ := true.B)
  val initWayMask = Fill(ways, true.B)
  when(initActive) {
    when(initSet === (sets - 1).U) {
      initActive := false.B
    }.otherwise {
      initSet := initSet + 1.U
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // Internal arbiter
  // ══════════════════════════════════════════════════════════════════
  val replRdVec = VecInit(io.toDir.map(_.ReplRd))
  val dirWbVec  = VecInit(io.toDir.map(_.DirWb))
  val dirRdVec  = VecInit(io.toDir.map(_.DirRd))

  val rawAnyReplRd = replRdVec.asUInt.orR
  val rawAnyDirWb  = dirWbVec.asUInt.orR
  val rawAnyDirRd  = dirRdVec.asUInt.orR
  val anyReplRd = rawAnyReplRd && !initActive
  val anyDirWb  = rawAnyDirWb && !initActive
  val anyDirRd  = rawAnyDirRd && !initActive
  val anyLevel2 = anyDirWb || anyDirRd

  // replStall (declared as wire; driven after S2/S3 regs exist)
  val replStall = Wire(Bool())

  // starvation counters for Level-2 groups
  val ageWb = RegInit(0.U(4.W))
  val ageRd = RegInit(0.U(4.W))
  val wbStarved  = anyDirWb && (ageWb === MAX_AGE)
  val rdStarved  = anyDirRd && (ageRd === MAX_AGE)
  val anyStarved = wbStarved || rdStarved

  val wbWinsAge = anyDirWb && (!anyDirRd || (ageWb >= ageRd))
  val level2WinIsWb = anyLevel2 && Mux(anyStarved,
    Mux(wbStarved && rdStarved, ageWb >= ageRd, wbStarved),
    wbWinsAge)

  // ReplRd preempts Level-2 unless a Level-2 group is starved or replStall
  val replRdWins = anyReplRd && !anyStarved && !replStall

  // round-robin pointers
  val rrReplRd = RegInit(0.U(idxW.W))
  val rrDirWb  = RegInit(0.U(idxW.W))
  val rrDirRd  = RegInit(0.U(idxW.W))

  def roundRobinNext(last: UInt, mask: UInt, n: Int): UInt = {
    val w        = log2Ceil(n)
    val doubled  = Cat(mask, mask)
    val startPos = Mux(last === (n - 1).U, 0.U, last + 1.U)
    val rotated  = (doubled >> startPos)(n - 1, 0)
    val relIdx   = PriorityEncoder(rotated)
    val absRaw   = startPos +& relIdx
    val abs      = Mux(absRaw >= n.U, absRaw - n.U, absRaw)
    abs(w - 1, 0)
  }

  val winReplRd = roundRobinNext(rrReplRd, replRdVec.asUInt, nMSHR)
  val winDirWb  = roundRobinNext(rrDirWb,  dirWbVec.asUInt,  nMSHR)
  val winDirRd  = roundRobinNext(rrDirRd,  dirRdVec.asUInt,  nMSHR)

  val grantReplRd = replRdWins  && anyReplRd
  val grantDirWb  = !replRdWins && anyLevel2 &&  level2WinIsWb && anyDirWb
  val grantDirRd  = !replRdWins && anyLevel2 && !level2WinIsWb && anyDirRd

  val winIdx   = Mux(grantReplRd, winReplRd, Mux(grantDirWb, winDirWb, winDirRd))
  val anyGrant = grantReplRd || grantDirWb || grantDirRd
  val winReq   = io.toDir(winIdx)

  when(grantReplRd) { rrReplRd := winReplRd }
  when(grantDirWb)  { rrDirWb  := winDirWb  }
  when(grantDirRd)  { rrDirRd  := winDirRd  }

  def incrCapped(x: UInt, cond: Bool): UInt = Mux(cond && (x < MAX_AGE), x + 1.U, x)
  ageWb := Mux(grantDirWb, 0.U, incrCapped(ageWb, anyDirWb))
  ageRd := Mux(grantDirRd, 0.U, incrCapped(ageRd, anyDirRd))

  // ══════════════════════════════════════════════════════════════════
  // DirRel arbiter — FULLY SEPARATE from the DirRd/DirWb/ReplRd arbiter
  // ══════════════════════════════════════════════════════════════════
  val dirRelPath = if (paramL2.enableDRRIPReleaseUpdate) {
    val relVec = VecInit(io.toDir.map(_.DirRel))
    val anyRel = relVec.asUInt.orR && !initActive
    val rrRel  = RegInit(0.U(idxW.W))
    val winRel = roundRobinNext(rrRel, relVec.asUInt, nMSHR)
    when(anyRel) { rrRel := winRel }
    Some((anyRel, io.toDir(winRel), winRel))
  } else {
    None
  }
  val dirRelGrant = dirRelPath.map(_._1).getOrElse(false.B)
  val dirRelReq   = dirRelPath.map(_._2).getOrElse(0.U.asTypeOf(new L2Directory.PathToDirectory))
  val dirRelWin   = dirRelPath.map(_._3).getOrElse(0.U(idxW.W))

  // ══════════════════════════════════════════════════════════════════
  // S1: decode the arbitration winner
  // ══════════════════════════════════════════════════════════════════
  val isReplRd = grantReplRd
  val isDirRd  = grantDirRd
  val isDirWb  = grantDirWb
  val isRead   = isReplRd || isDirRd

  val reqSet = winReq.PADDR(offsetBits + setBits - 1, offsetBits)
  val reqTag = winReq.PADDR(paramL2.physicalAddrWidth - 1, offsetBits + setBits)

  val dirReadData_s2 = dirArray.io.r(isRead, reqSet).resp.data
  val replState_s1   = replRegs(reqSet)

  // ── S1 → S2 ───────────────────────────────────────────────────────
  val s2_valid   = RegNext(isRead,        false.B)
  val s2_isRepl  = RegNext(isReplRd,      false.B)
  val s2_isDirRd = RegNext(isDirRd,       false.B)
  val s2_tshr    = RegNext(winReq.TSHRID, 0.U)
  val s2_set     = RegNext(reqSet,        0.U)
  val s2_tag     = RegNext(reqTag,        0.U)
  val s2_repl    = RegNext(replState_s1,  0.U)

  // ── S2 → S3 ───────────────────────────────────────────────────────
  val s3_valid   = RegNext(s2_valid,   false.B)
  val s3_isRepl  = RegNext(s2_isRepl,  false.B)
  val s3_isDirRd = RegNext(s2_isDirRd, false.B)
  val s3_tshr    = RegNext(s2_tshr,    0.U)
  val s3_set     = RegNext(s2_set,     0.U)
  val s3_tag     = RegNext(s2_tag,     0.U)
  val s3_repl    = RegNext(s2_repl,    0.U)

  // ══════════════════════════════════════════════════════════════════
  // DirWb: bitmask partial write at S1
  // ══════════════════════════════════════════════════════════════════
  val wbEntry = Wire(new L2Directory.DirEntry)
  wbEntry.tag  := reqTag
  wbEntry.meta := winReq.META

  val wbMask = Wire(new L2Directory.DirEntry)
  wbMask.tag        := Fill(tagWidth, winReq.TAG_WEN)
  wbMask.meta.state := Fill(2, winReq.META_WEN.state)
  wbMask.meta.dirty := winReq.META_WEN.dirty
  wbMask.meta.alias := Fill(2, winReq.META_WEN.alias)
  wbMask.meta.clients.zip(winReq.META_WEN.clients).foreach { case (m, w) => m := w }

  val dirWriteValid = initActive || isDirWb
  val dirWriteEntry = Mux(initActive, initEntry, wbEntry)
  val dirWriteSet = Mux(initActive, initSet, reqSet)
  val dirWriteWayMask = Mux(initActive, initWayMask, UIntToOH(winReq.WAY(wayBits - 1, 0), ways))
  val dirWriteBitMask = Mux(initActive, initMask.asUInt, wbMask.asUInt)

  dirArray.io.w(
    valid   = dirWriteValid,
    data    = dirWriteEntry,
    setIdx  = dirWriteSet,
    waymask = dirWriteWayMask,
    bitmask = dirWriteBitMask
  )

  if (enableDirStateProbe) {
    io.debugStateWrite.get.valid := isDirWb
    io.debugStateWrite.get.set := reqSet
    io.debugStateWrite.get.way := winReq.WAY(wayBits - 1, 0)
    io.debugStateWrite.get.state := winReq.META.state
    io.debugStateWrite.get.dirty := winReq.META.dirty
    io.debugStateWrite.get.client0 := winReq.META.clients(0)
  }

  // DirWb completion is combinational at the grant cycle: the write lands at the
  // edge ending that cycle and is observed by any access granted from the next
  // cycle on (single grant per cycle, latency-1 synchronous SRAM).

  // ══════════════════════════════════════════════════════════════════
  // S3: hit detect, way select, PLRU update
  // ══════════════════════════════════════════════════════════════════
  val dirData_s3 = RegEnable(dirReadData_s2, 0.U.asTypeOf(dirReadData_s2), s2_valid)

  val tagMatch = VecInit(dirData_s3.map(_.tag === s3_tag))
  val validVec = VecInit(dirData_s3.map(_.meta.state =/= L2Directory.MetaState.I))
  val hitVec   = VecInit(tagMatch.zip(validVec).map { case (t, v) => t && v })
  val hit_s3   = hitVec.asUInt.orR
  val hitWay   = PriorityEncoder(hitVec)
  val blockRefillMask_s2 = Wire(UInt(ways.W))
  blockRefillMask_s2 := VecInit((0 until ways).map { w =>
    VecInit(lockTable.map(e => e.valid && e.set === s2_set && e.way === w.U)).asUInt.orR
  }).asUInt
  val freeWayMask_s2 = Wire(UInt(ways.W))
  freeWayMask_s2 := (~blockRefillMask_s2)(ways - 1, 0)
  val freeWayMask_s3 = RegInit(((BigInt(1) << ways) - 1).U(ways.W))
  when(s2_isRepl) {
    freeWayMask_s3 := freeWayMask_s2
  }

  // replStall: any in-flight replacement-state writer blocks new ReplRd grant.
  replStall :=
    (s2_valid && s2_isRepl) ||
    (s3_valid && s3_isRepl) ||
    (s2_valid && s2_isDirRd) ||
    (s3_valid && s3_isDirRd && hit_s3)

  val invalidVec = VecInit(dirData_s3.map(_.meta.state === L2Directory.MetaState.I))
  val hasInvalid = invalidVec.asUInt.orR
  val invalidWay = PriorityEncoder(invalidVec)
  val replWay    = if (useDRRIP) DRRIP.getReplaceWay(s3_repl, ways)
                   else          TreePLRU.getReplaceWay(s3_repl(TreePLRU.nBits(ways) - 1, 0), ways)
  val chosenWay  = Mux(hasInvalid, invalidWay, replWay)

  val victimMask_s3 = freeWayMask_s3

  val victimWay  = Mux(victimMask_s3(chosenWay), chosenWay, PriorityEncoder(victimMask_s3))
  val replRetry  = s3_valid && s3_isRepl && !victimMask_s3.orR

  val way_s3   = Mux(s3_isRepl, victimWay, Mux(hit_s3, hitWay, 0.U))
  val entry_s3 = dirData_s3(way_s3)
  val metaValid = (s3_isDirRd && hit_s3) || (s3_isRepl && !replRetry)

  // An Invalid entry must never carry client presence: (I, clients!=0) means a live client
  // copy orphaned from the directory (inclusion breach). Readers rely on state==I implying
  // no eviction work -- the REQ vPipe's evict_victim_invalid skip consumes this on ReplRd.
  when (s3_valid && metaValid) {
    assert(!(entry_s3.meta.state === L2Directory.MetaState.I && entry_s3.meta.clients.asUInt.orR),
      "Directory: read response entry is Invalid but carries client presence")
  }

  val replWen = (s3_valid && s3_isDirRd && hit_s3) ||
                (s3_valid && s3_isRepl && !replRetry)
  // Next replacement state, policy-selected at elaboration: DRRIP promotes
  // on hit and inserts-at-0 with aging on valid eviction; PLRU marks the
  // touched way MRU. A DirRd miss and a retried ReplRd never update.
  val replNextState = Wire(UInt(replBits.W))
  if (useDRRIP) {
    // Aging applies only when the actually-selected victim held a valid
    // line (a real eviction); filling an Invalid slot does not age. The
    // actual victim entry is used (not hasInvalid): the lock-mask fallback
    // in victimWay can divert the pick from an Invalid candidate to a
    // valid way.
    val victimHoldsValid = entry_s3.meta.state =/= L2Directory.MetaState.I
    replNextState := Mux(s3_isDirRd,
      DRRIP.getNextStateHit(s3_repl, way_s3, ways),
      DRRIP.getNextStateRefill(s3_repl, way_s3, victimHoldsValid, ways))
  } else {
    replNextState := TreePLRU.getNextState(s3_repl(TreePLRU.nBits(ways) - 1, 0), way_s3, ways).pad(replBits)
  }
  when(replWen) {
    replRegs(s3_set) := replNextState
    if (useDRRIP) {
      // Origin bit (reuse tracking): set on DirRd hit, cleared on refill —
      // mirrors coupledL2's origin := hit_s3 at every replacer update.
      when(s3_isDirRd) {
        originBits(s3_set) := originBits(s3_set) | UIntToOH(way_s3, ways)
      }.otherwise {
        originBits(s3_set) := originBits(s3_set) & ~UIntToOH(way_s3, ways)
      }
    }
  }

  // ── DRRIP release-type updates via the EVT channel ─────────────────────
  // Two fully separate, individually switched delivery paths (L2Params),
  // each elaborated only when its switch is on:
  //   entrance: a WayValid EVT entrance trains immediately (evtRelTrain input,
  //             reduced one-hot from the TSHRs — at most one EVT entrance per
  //             cycle per slice by allocator construction). WayValid-entered
  //             episodes ONLY ever use this path (or none when disabled).
  //   DirRel:   an arbitrated op issued by the proxy's dedicated DirRel FSM
  //             at the EVT vPipe's validated commit, granted by the SEPARATE
  //             DirRel arbiter above and applied at its grant cycle. It never
  //             rides, shares, or disturbs the DirRd/DirWb/ReplRd paths.
  // Both blocks sit after the S3 replWen block so they win same-edge ties
  // (DirRel last of all); an in-flight S2 pipeline writer landing a cycle
  // later can still clobber a train — the same race class this design already
  // tolerates between concurrent DirRd-hit updates (best-effort heuristic
  // state, not architectural), accepted deliberately.
  io.evtRelTrain.foreach { t =>
    val entRelFire = t.valid && useDRRIP.B && !initActive
    when(entRelFire) {
      replRegs(t.bits.set) := DRRIP.getNextStateRelease(replRegs(t.bits.set), t.bits.way, originBits(t.bits.set)(t.bits.way), ways)
      originBits(t.bits.set) := originBits(t.bits.set) | UIntToOH(t.bits.way, ways)
    }
    XSPerfAccumulate("repl_rel_upd_ent", entRelFire)
  }
  val dirRelFire = dirRelGrant && useDRRIP.B
  when(dirRelFire) {
    val relSet = dirRelReq.PADDR(offsetBits + setBits - 1, offsetBits)
    val relWay = dirRelReq.WAY(wayBits - 1, 0)
    replRegs(relSet) := DRRIP.getNextStateRelease(replRegs(relSet), relWay, originBits(relSet)(relWay), ways)
    originBits(relSet) := originBits(relSet) | UIntToOH(relWay, ways)
  }

  // Pending-clear application (one cycle after the release grant): the ONLY
  // per-cycle lock-table maintenance. Listed first so that a same-cycle lock
  // set wins over the clear on the vacated slot, and a same-cycle release
  // grant re-registers pendClearValid below (Chisel last-wins).
  when(pendClearValid) {
    when(blockRefillFaultMode =/= L2DirectoryBlockRefillFaultMode.NeverUnlock.U) {
      lockTable(pendClearIdx).valid := false.B
    }
    pendClearValid := false.B
  }

  when(s3_valid && s3_isRepl && !hit_s3 && !replRetry) {
    // a TSHR may only (re)take its slot once the previous lock is gone; the
    // slot being vacated at this same edge counts as available
    val selfClearing = pendClearValid &&
      blockRefillFaultMode =/= L2DirectoryBlockRefillFaultMode.NeverUnlock.U &&
      pendClearIdx === s3_tshr(idxW - 1, 0)
    assert(!lockTable(s3_tshr).valid || selfClearing,
      "Directory: TSHR took a second refill lock while one is still held")
    assert(!VecInit(lockTable.map(e => e.valid && e.set === s3_set && e.way === way_s3)).asUInt.orR,
      "Directory: double refill lock on (set,way)")
    lockTable(s3_tshr).valid := true.B
    lockTable(s3_tshr).set := s3_set
    lockTable(s3_tshr).way := way_s3
  }

  // Release mark at the owner's fill-commit DirWb grant: the TSHR ID indexes
  // the locker's slot directly, so ownership is structural — a non-owner's
  // DirWb indexes its own slot and is rejected by the valid+set+way gate; an
  // eviction invalidate (meta-only, TAG_WEN=0) never releases. The clear lands
  // one cycle later via pendClear, so the way stays unpickable through the
  // grant cycle + 1.
  val relEntry = lockTable(winReq.TSHRID(idxW - 1, 0))
  when(grantDirWb && winReq.TAG_WEN && relEntry.valid &&
       relEntry.set === reqSet && relEntry.way === winReq.WAY(wayBits - 1, 0)) {
    pendClearValid := true.B
    pendClearIdx := winReq.TSHRID(idxW - 1, 0)
  }

  lockTable.zipWithIndex.foreach { case (e, i) =>
    assert(!(e.valid && io.tshrIdle(i)),
      s"Directory: blockRefill leaked after owner MSHR retired at TSHR $i")
  }

  if (enableDirPlruProbe) {
    io.debugRrip.get.valid := replWen
    io.debugRrip.get.set := s3_set
    io.debugRrip.get.state := Mux(replWen, replNextState, s3_repl)
    io.debugRrip.get.s3Valid := s3_valid
    io.debugRrip.get.s3Tshr := s3_tshr
    io.debugRrip.get.s3IsDirRd := s3_isDirRd
    io.debugRrip.get.s3IsRepl := s3_isRepl
    io.debugRrip.get.wayS3 := way_s3
    io.debugRrip.get.s3Rrip := s3_repl
    io.debugRrip.get.currentSetRrip := replRegs(s3_set)
    io.debugRetry.get.replStall := replStall
    io.debugRetry.get.s2Valid := s2_valid
    io.debugRetry.get.s2Tshr := s2_tshr
    io.debugRetry.get.s2IsDirRd := s2_isDirRd
    io.debugRetry.get.s2IsRepl := s2_isRepl
    io.debugRetry.get.s2Set := s2_set
    io.debugRetry.get.s3Valid := s3_valid
    io.debugRetry.get.s3Tshr := s3_tshr
    io.debugRetry.get.s3IsDirRd := s3_isDirRd
    io.debugRetry.get.s3IsRepl := s3_isRepl
    io.debugRetry.get.s3Set := s3_set
    io.debugRetry.get.hasInvalid := hasInvalid
    io.debugRetry.get.invalidWay := invalidWay
    io.debugRetry.get.hitWay := hitWay
    io.debugRetry.get.replWay := replWay
    io.debugRetry.get.chosenWay := chosenWay
    io.debugRetry.get.victimWay := victimWay
    io.debugRetry.get.blockRefillMaskS2 := blockRefillMask_s2
    io.debugRetry.get.freeWayMaskS3 := freeWayMask_s3
    io.debugRetry.get.dirWbDone := grantDirWb
    io.debugRetry.get.dirWbDoneSet := reqSet
    io.debugRetry.get.dirWbDoneWay := winReq.WAY(wayBits - 1, 0)
    // re-synthesized per-(set,way) view of the CAM lock table (probe-only, default off)
    io.debugRetry.get.blockRefillLocked := VecInit((0 until sets).map { s =>
      VecInit((0 until ways).map { w =>
        VecInit(lockTable.map(e => e.valid && e.set === s.U && e.way === w.U)).asUInt.orR
      }).asUInt
    })
    io.debugRetry.get.ageWb := ageWb
    io.debugRetry.get.ageRd := ageRd
    io.debugRetry.get.rrReplRd := rrReplRd
    io.debugRetry.get.rrDirWb := rrDirWb
    io.debugRetry.get.rrDirRd := rrDirRd
    io.debugRetry.get.replRegs := replRegs
  }


  // ══════════════════════════════════════════════════════════════════
  // Response (one-to-one per TSHR)
  // ══════════════════════════════════════════════════════════════════
  io.fromDir.zipWithIndex.foreach { case (o, i) =>
    o.TSHRID         := i.U
    o.META           := 0.U.asTypeOf(new L2Directory.MetaReadResult)
    o.REPL           := 0.U.asTypeOf(new L2Directory.ReplReadResult)
    o.DirRdArbComp   := false.B
    o.DirRdResp      := false.B
    o.DirWbArbComp   := false.B
    o.ReplRdArbComp  := false.B
    o.ReplRdResp     := false.B
    o.ReplRdRetryAck := false.B
    o.DirRelArbComp  := false.B
  }

  // ArbComp — combinational at grant cycle (N) for all three operations.
  // For DirWb the write lands at the edge ending the grant cycle; any access
  // granted from cycle N+1 on observes it, so no delayed completion is needed.
  when(grantDirRd)  { io.fromDir(winIdx).DirRdArbComp  := true.B }
  when(grantReplRd) { io.fromDir(winIdx).ReplRdArbComp := true.B }
  when(grantDirWb)  { io.fromDir(winIdx).DirWbArbComp  := true.B }
  // DirRelArbComp — from the separate DirRel arbiter (dirRelWin); placed here so
  // the fromDir tie-off defaults above are overridden correctly.
  when(dirRelGrant) { io.fromDir(dirRelWin).DirRelArbComp := true.B }

  // Resp at S3 — routed one-to-one to the originating TSHR
  when(s3_valid) {
    val o = io.fromDir(s3_tshr)
    when(metaValid) {
      o.META.state   := entry_s3.meta.state
      o.META.dirty   := entry_s3.meta.dirty
      o.META.clients := entry_s3.meta.clients
      o.META.alias   := entry_s3.meta.alias
      o.META.way     := way_s3
      o.META.hit     := Mux(s3_isRepl, false.B, hit_s3)
    }
    when(s3_isRepl) {
      o.ReplRdResp := !replRetry
      o.ReplRdRetryAck := replRetry
      when(!replRetry) {
        o.REPL.paddr := Cat(entry_s3.tag, s3_set, 0.U(offsetBits.W))
        o.REPL.way := way_s3
      }
    }.otherwise {
      o.DirRdResp := true.B
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // Assertions
  // ══════════════════════════════════════════════════════════════════
  io.toDir.zipWithIndex.foreach { case (req, i) =>
    assert(PopCount(Seq(req.ReplRd, req.DirRd, req.DirWb, req.DirRel)) <= 1.U,
      s"Directory: TSHR $i asserts multiple op types")
  }
  assert(PopCount(Seq(grantReplRd, grantDirWb, grantDirRd)) <= 1.U,
    "Directory: multiple grants in one cycle")
  assert(!(grantReplRd && replStall),
    "Directory: ReplRd granted while replStall (replacement-state hazard)")
  // Functional requests ARE allowed at the directory ports during the power-on
  // init sweep; they are masked out of arbitration (anyReplRd/anyDirWb/anyDirRd
  // are qualified with !initActive) and simply wait until the sweep completes.
  // The invariant is that no functional request is ever arbitrated during init.
  when(initActive) {
    assert(!anyGrant,
      "Directory: functional traffic must not be arbitrated while directory init is in progress")
  }
  when(s3_valid && s3_isDirRd) {
    assert(PopCount(hitVec) <= 1.U,
      "Directory: multiple ways hit (tag uniqueness)")
  }

  // ══════════════════════════════════════════════════════════════════
  // Perf
  // ══════════════════════════════════════════════════════════════════
  XSPerfAccumulate("grant_dirRd",   grantDirRd)
  XSPerfAccumulate("grant_replRd",  grantReplRd)
  XSPerfAccumulate("grant_dirWb",   grantDirWb)
  XSPerfAccumulate("grant_dirRel",  dirRelGrant)
  XSPerfAccumulate("replStall",     replStall)
  XSPerfAccumulate("replRetry",     replRetry)
  XSPerfAccumulate("blockRefill_locked", PopCount(lockTable.map(_.valid)))
  XSPerfAccumulate("dirRd_hit",     s3_valid && s3_isDirRd && hit_s3)
  XSPerfAccumulate("dirRd_miss",    s3_valid && s3_isDirRd && !hit_s3)
  XSPerfAccumulate("repl_upd",      replWen)
  XSPerfAccumulate("repl_rel_upd",     dirRelFire)
  XSPerfAccumulate("dir_init_active", initActive)
  XSPerfAccumulate("starve_wb",     wbStarved)
  XSPerfAccumulate("starve_rd",     rdStarved)
}
