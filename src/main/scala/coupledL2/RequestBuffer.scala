/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package xscache.coupledL2

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import chisel3._
import chisel3.util._
import xscache.coupledL2._
import xscache.coupledL2.prefetch._
import xscache.coupledL2.utils._
import utility._

class ReqEntry(entries: Int = 4)(implicit p: Parameters) extends L2Bundle() {
  val valid    = Bool()
  val rdy      = Bool()
  val task     = new TaskBundle()
  // Entry epoch distinguishes a held issue from a later reuse of the same
  // physical buffer slot.  It is advanced only when a fresh request is
  // allocated into the slot and is carried by the local issue token.
  val generation = UInt(8.W)
  val timer    = UInt(timestampBits.W) // for prefetch controller analysis

  /* blocked by MainPipe
  * [5] by stage1 at entry allocation
  * [4] by stage2 / a same-set entry just fired
  * [3] by result capture
  * [2] by decision
  * [1] by stage3 apply
  * [0] block release flag
  */
  val waitMP  = UInt(6.W)


  /* Split of waitMS by conflict kind (assigned at alloc alongside waitMS):
  * waitMSReq: conflict via the MSHR's own request line (reqTag). Clears when
  *   the MSHR's deferred refill commit has landed (refillCommitPending=0) --
  *   the Directory/DS state of that line is then final, so a follow-up no
  *   longer needs to wait for the MSHR to fully retire.
  * waitMSVic: conflict via the MSHR's chosen victim (metaTag && needRelease).
  *   Clears only at willFree, as before.
  * */
  val waitMSReq = UInt(mshrsAll.W)
  val waitMSVic = UInt(mshrsAll.W)

  /* buffer_dep_mask[i][j] => entry i should wait entry j
  *   this is used to make sure that same set requests will be sent
  *   to MSHR in order
  */
//  val depMask = Vec(entries, Bool())

}

class ChosenQBundle(idOHWIdth: Int = 2)(implicit p: Parameters) extends L2Bundle {
  val bits = new ReqEntry()
  val idOH = UInt(idOHWIdth.W)
}

class AMergeTask(implicit p: Parameters) extends L2Bundle {
  val idOH = UInt(mshrsAll.W)
  val task = new TaskBundle()
}

class RequestBuffer(flow: Boolean = true, entries: Int = 4)(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val in          = Flipped(DecoupledIO(new TaskBundle))
    val out         = DecoupledIO(new TaskBundle)
    // RequestArb has captured the Directory command and now owns this held
    // source. Eligibility cannot revoke it before the real output handshake.
    val issueCommit = Input(Bool())
    val mshrInfo  = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    // A live snoop owns its address independently of the paired normal MSHR.
    // Keep it visible here so queued A requests do not pass a toN snoop while
    // its Directory update or response is still in flight.
    val snoopInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    val aMergeTask = ValidIO(new AMergeTask)
    val mainPipeBlock = Input(Vec(4, Bool()))
    /* Snoop task from arbiter at stage 2 */
    val taskFromArb_s2 = Flipped(ValidIO(new TaskBundle()))

    /* status of s2 and s3 */
    val pipeStatusVec = Flipped(Vec(3, ValidIO(new PipeStatus)))

    val ATag        = Output(UInt(tagBits.W))
    val ASet        = Output(UInt(setBits.W))

    // when Probe/Release/MSHR enters MainPipe, we need also to block A req
    val s1Entrance = Flipped(ValidIO(new L2Bundle {
      val set = UInt(setBits.W)
    }))

    val pfStatInMSHR = Output(new PfStatInMSHRBundle())
    val hasMergeA = Output(Bool())
  })

  // A real input FIFO promises storage from local registered occupancy.
  // Owner conflict/merge/prefetch searches operate on its retained head and
  // cannot propagate through SinkA.ready to the upstream TileLink channel.
  val ingress = Module(new Queue(new TaskBundle, entries = 2, pipe = false, flow = false))
  ingress.io.enq <> io.in
  val admitted = ingress.io.deq

  /* ======== Data Structure ======== */

  io.ATag := admitted.bits.tag
  io.ASet := admitted.bits.set

  val buffer = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new ReqEntry))))
  val issueArb = Module(new TwoLevelRRArbiter(new ReqEntry, entries))
  ArbPerf(issueArb, "issueArb")
  val chosenQ = Module(new Queue(new ChosenQBundle(entries), entries = 1, pipe = false, flow = false))
  val chosenQValid = chosenQ.io.deq.valid
  // A selected entry is held locally before it is exposed to RequestArb.
  // This cuts the chosenQ/output ready feedback while retaining the complete
  // task and a narrow owner/generation identity.  The buffer entry remains
  // valid until the real output fire, so cancellation can safely return it to
  // issueArb without losing work.
  val issueHold = RegInit(0.U.asTypeOf(new ChosenQBundle(entries)))
  val issueHoldValid = RegInit(false.B)
  val issueHoldCommitted = RegInit(false.B)

  /* ======== Enchantment ======== */
  val NWay = cacheParams.ways
  // count conflict
  def sameAddr(a: TaskBundle, b: TaskBundle):     Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def sameAddr(a: TaskBundle, b: MSHRInfo): Bool = Cat(a.tag, a.set) === Cat(b.reqTag, b.set)
  def sameSet (a: TaskBundle, b: TaskBundle):     Bool = a.set === b.set
  def sameSet (a: TaskBundle, b: MSHRInfo): Bool = a.set === b.set
  def addrConflict(a: TaskBundle, s: MSHRInfo): Bool = {
    a.set === s.set && (a.tag === s.reqTag || a.tag === s.metaTag && s.needRelease)
  }
  // A reqTag conflict against a live normal context blocks a follow-up until
  // that context fully retires.  Early release at refillCommitLanded is NOT
  // safe: the landed-but-not-retired context still owns the address for
  // nested ReleaseData routing (single-owner invariant).  Victim conflicts
  // likewise wait until the registered owner becomes invalid.
  def conflictMaskReq(a: TaskBundle): UInt = VecInit(io.mshrInfo.map(s =>
    // A live context keeps blocking its reqTag until full retirement, even
    // after its refill commit has landed: a follow-up may allocate a second
    // live context on the same address in the landed-but-not-retired window
    // (observed: 11 cycles), and a nested ReleaseData then matches both --
    // violating single-owner routing (tl-test assert "ReleaseData matches
    // multiple normal refill contexts").  SPEC shows the early release is
    // perf-free (<=0.1% of instructions).
    s.valid && sameAddr(a, s.bits))).asUInt
  // A context that has selected this line as its replacement victim keeps
  // holding it for its whole pre-commit window, not just when a lower-level
  // release is owed: Directory suppresses A-channel hits on the victim way
  // from victim selection until the install lands (victimPendingWayMask_s3,
  // `!dirHit && w_replResp && !refillCommitLanded`).  Releasing a follow-up in
  // that window drops it to the miss path (the suppressed hit is invisible),
  // and it then installs a SECOND copy of the same line into another way --
  // leaving the tag in two ways (observed: "Set should not have more than one
  // hit").  Hold it until the context retires instead; `willFree` is reached
  // only after refillCommitLanded, so the whole suppression window is covered.
  def conflictMaskVic(a: TaskBundle): UInt = VecInit(io.mshrInfo.map(s =>
    s.valid && a.set === s.bits.set && a.tag === s.bits.metaTag &&
      // An INVALID replacement way retains an arbitrary old tag, but owns
      // no victim line. CMO misses also set w_replResp without replacing a
      // way. Neither may turn that stale tag into an address dependency.
      (s.bits.needRelease || (!s.bits.isCmo && s.bits.w_replResp &&
        !s.bits.dirHit && s.bits.meta.state =/= MetaData.INVALID)))).asUInt
  def conflictMask(a: TaskBundle): UInt = conflictMaskReq(a) | conflictMaskVic(a)
  def snoopConflict(a: TaskBundle): Bool = VecInit(io.snoopInfo.map(s =>
    s.valid && s.bits.blocksSnoop && sameAddr(a, s.bits)
  )).asUInt.orR
  def conflict(a: TaskBundle): Bool = conflictMask(a).orR || snoopConflict(a)

  def conflictMaskFromA(a: TaskBundle): UInt =
    conflictMask(a) & VecInit(io.mshrInfo.map(_.bits.fromA)).asUInt

  def latePrefetch(a: TaskBundle): (Bool, UInt, UInt) = {
    val matchVec = VecInit(io.mshrInfo.map(s =>
    s.valid && s.bits.isPrefetch && sameAddr(a, s.bits) &&
      a.fromA && (a.opcode === AcquireBlock || a.opcode === AcquirePerm)
    ))
    val matched = matchVec.asUInt.orR
    assert(PopCount(matchVec) <= 1.U, "Multiple late prefetch MSHRs matched")
    val matchSrc = Mux1H(matchVec, io.mshrInfo.map(_.bits.reqSource))
    val matchLatency = Mux1H(matchVec, io.mshrInfo.map(_.bits.reqTimer))
    (matched, matchSrc, matchLatency)
  }

  // count ways
//  def countWaysOH(cond: (MSHRInfo => Bool)): UInt = {
//    VecInit(io.mshrInfo.map(s =>
//      Mux(
//        s.valid && cond(s.bits),
//        UIntToOH(s.bits.way, NWay),
//        0.U(NWay.W)
//      )
//    )).reduceTree(_ | _)
//  }

  // other flags
  val in      = admitted.bits
  val full    = Cat(buffer.map(_.valid)).andR

  // incoming Acquire can be merged with late_pf MSHR block
  val mergeAMask = VecInit(io.mshrInfo.map { case s =>
    val mshrInflight = !(s.bits.w_grantlast && s.bits.w_grant)
    s.valid && s.bits.isPrefetch && sameAddr(in, s.bits) && !s.bits.dirHit && mshrInflight &&
      in.fromA && (in.opcode === AcquireBlock || in.opcode === AcquirePerm) && !s.bits.mergeA && !(in.param === NtoT && s.bits.param === NtoB)
  }).asUInt
  val mergeA = mergeAMask.orR
  assert(!mergeA || PopCount(mergeAMask) <= 1.U, "RequestBuffer: mergeAMask must be one-hot when mergeA is asserted")
  io.aMergeTask.valid := admitted.valid && mergeA
  io.aMergeTask.bits.idOH := mergeAMask
  io.aMergeTask.bits.task := in

  /*
   noFreeWay check: s2 + s3 + mshrs >= ways(L2)
   */
  def noFreeWayForSet(set: UInt): Bool = {
    val task_s2 = io.taskFromArb_s2
    val sameSet_s2 = task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask && task_s2.bits.set === set
    val normalA_capture = RegNext(task_s2.valid && task_s2.bits.fromA && !task_s2.bits.mshrTask, false.B)
    val set_capture = RegEnable(task_s2.bits.set, task_s2.valid)
    val sameSet_s3 = normalA_capture && set_capture === set
    val normalA_decide = RegNext(normalA_capture, false.B)
    val set_decide = RegNext(set_capture)
    val sameSet_decide = normalA_decide && set_decide === set
    val sameSet_apply = RegNext(normalA_decide, false.B) && RegNext(set_decide) === set
    // This interface carries normal contexts only. A normal MSHR with a live
    // ReplaceMSHR child still reserves its own destination way.
    val sameSetCnt = PopCount(VecInit(io.mshrInfo.map(s =>
      s.valid && s.bits.set === set && s.bits.fromA) :+
      sameSet_s2 :+ sameSet_s3 :+ sameSet_decide :+ sameSet_apply).asUInt)
    val noFreeWay = sameSetCnt >= cacheParams.ways.U
    noFreeWay
  }
  def noFreeWay(task: TaskBundle): Bool = noFreeWayForSet(task.set)

  val pfThrottleByMSHR = RegNext(PopCount(io.pipeStatusVec.map(_.valid) ++ io.mshrInfo.map(_.valid)) >= (mshrsAll - 3).U)

  // prefetch valve: once the buffer is mostly occupied, stop admitting prefetch
  // requests so a demand request always finds a free entry; input-side only,
  // entries already inside must stay issuable or a full prefetch buffer deadlocks
  // threshold 3 loses badly on mcf (+10% CPI on mcf_6753): prefetch occupies
  // 3 of 4 entries and starves latency-sensitive demand; 2 is the sweet spot
  val pfOccThreshold = 2
  val pfThrottleByOcc = PopCount(buffer.map(_.valid)) >= pfOccThreshold.U

  def isPrefetchTask(task: TaskBundle): Bool = task.fromA && task.opcode === Hint
  def prefetchBlocked(task: TaskBundle): Bool = isPrefetchTask(task) && (pfThrottleByMSHR || pfThrottleByOcc)
  def prefetchBlockedByMSHR(task: TaskBundle): Bool = isPrefetchTask(task) && pfThrottleByMSHR

  // Every request is owned by a buffer entry before it reaches issueHold.
  // Ingress and chosenQ never bypass the registered output owner.

  //  val depMask    = buffer.map(e => e.valid && sameAddr(admitted.bits, e.task))
  // remove duplicate prefetch if same-addr A req in MSHR or ReqBuf
  val isPrefetch = in.fromA && in.opcode === Hint
  val dupMask    = VecInit(
    io.mshrInfo.map(s =>
      s.valid && s.bits.isAcqOrPrefetch && sameAddr(in, s.bits)) ++
    buffer.map(e =>
      e.valid && sameAddr(in, e.task)
    )
  )
  val dup        = isPrefetch && dupMask.asUInt.orR
  val dupHitSrc = ParallelPriorityMux(dupMask,
    io.mshrInfo.map(_.bits.reqSource) ++ buffer.map(_.task.reqSource)
  )

  // statistics io
  val latePrefetchRes = latePrefetch(in) // demand request hit entry of pf
  io.pfStatInMSHR.hitPf := latePrefetchRes._1 && admitted.valid && !sameAddr(in, RegNext(in))
  io.pfStatInMSHR.hitPfReqSrc := latePrefetchRes._2
  io.pfStatInMSHR.hitPfLatency := latePrefetchRes._3
  io.pfStatInMSHR.pfLate := admitted.valid && dup
  io.pfStatInMSHR.pfLateReqSrc := admitted.bits.reqSource
  io.pfStatInMSHR.pfLateHitReqSrc := dupHitSrc
  io.hasMergeA := mergeA && admitted.valid && !sameAddr(in, RegNext(in))

  //!! TODO: we can also remove those that duplicate with mainPipe

  /* ======== Alloc ======== */
  admitted.ready   := !prefetchBlocked(in) && !full || mergeA || dup

  val insertOH = MaskToOH(buffer.map(!_.valid))
  val alloc = !full && admitted.valid && !prefetchBlocked(in) && !dup && !mergeA
  buffer.zip(insertOH.asBools).foreach { case (entry, sel) =>
    when(alloc && sel){
      val mpBlock = Cat(io.mainPipeBlock).orR
      val pipeBlockOut = io.out.fire && sameSet(in, io.out.bits)
      val probeBlock   = io.s1Entrance.valid && io.s1Entrance.bits.set === in.set // wait for same-addr req to enter MSHR
      val s1Block      = pipeBlockOut || probeBlock

      entry.valid   := true.B
      entry.generation := entry.generation + 1.U
      // when Addr-Conflict / Same-Addr-Dependent / MainPipe-Block / noFreeWay-in-Set, entry not ready
      entry.rdy     := !conflict(in) && !mpBlock && !s1Block && !noFreeWay(in)// && !Cat(depMask).orR
      entry.task    := admitted.bits
      entry.waitMP  := Cat(
        s1Block,
        io.mainPipeBlock(0),
        io.mainPipeBlock(1),
        io.mainPipeBlock(2),
        io.mainPipeBlock(3),
        0.U(1.W))
      entry.timer   := 1.U
      entry.waitMSReq := conflictMaskReq(in)
      entry.waitMSVic := conflictMaskVic(in)
      assert(PopCount(conflictMaskFromA(in)) <= 2.U)
    }
  }

  /* ======== Issue ======== */
  issueArb.io.in.zip(buffer).zipWithIndex.foreach {
    case ((in, e), i) =>
    // A held entry is already reserved for the downstream handshake; do not
    // let the arbiter enqueue a second copy of the same owner into chosenQ.
    in.valid := e.valid && e.rdy &&
      !(issueHoldValid && issueHold.idOH(i))
      in.bits  := e
  }

  /* ======== chosenQ enq ======== */
  // once fired at issueArb, it is ok to enter MainPipe without conflict
  // however, it may be blocked for other reasons such as high-prior reqs or MSHRFull
  // in such case, we need a place to save it
  chosenQ.io.enq.valid := issueArb.io.out.valid
  chosenQ.io.enq.bits.bits := issueArb.io.out.bits
  chosenQ.io.enq.bits.idOH := issueArb.io.chosenOH
  issueArb.io.out.ready := chosenQ.io.enq.ready

  /* ======== Update rdy and masks ======== */
  buffer.zipWithIndex.foreach { case (e, i) =>
    when(e.valid) {
      e.timer := e.timer + 1.U

      val waitMSReqUpdate = WireInit(e.waitMSReq)
      val waitMSVicUpdate = WireInit(e.waitMSVic)
//      val depMaskUpdate = WireInit(e.depMask)

      // Wait for the registered owner-valid to clear before releasing waiters.
      val retiredMask = VecInit(io.mshrInfo.map(s => !s.valid)).asUInt
      // reqTag conflicts release only at full retirement: a commit-landed
      // context still owns its address until then (see conflictMaskReq).
      waitMSReqUpdate := e.waitMSReq & (~retiredMask).asUInt
      waitMSVicUpdate := e.waitMSVic & (~retiredMask).asUInt

      // Recalculate only after the apply edge has made the allocated owner's
      // registered valid visible. Capture and decision are distinct cycles;
      // merging them releases this mask one edge before the new MSHR exists.
      e.waitMP := e.waitMP >> 1
      when(e.waitMP(1) === 0.U && e.waitMP(0) === 1.U) {
        waitMSReqUpdate := conflictMaskReq(e.task)
        waitMSVicUpdate := conflictMaskVic(e.task)
      }

      // when request is sent, clear it in other reqs' depMask
//      when(io.out.fire && !canFlow) {
//        depMaskUpdate(chosenQ.io.deq.bits.id) := false.B
//      }

      // set waitMP if fired-s1-req is the same set
      val s1A_Block = io.out.fire && sameSet(e.task, io.out.bits)
      val s1B_Block = io.s1Entrance.valid && io.s1Entrance.bits.set === e.task.set
      val s1_Block  = s1A_Block || s1B_Block
      when(s1_Block) {
        e.waitMP := (e.waitMP >> 1) | "b010000".U
      }

      // update info
      e.waitMSReq := waitMSReqUpdate
      e.waitMSVic := waitMSVicUpdate
//      e.depMask := depMaskUpdate
      e.rdy     := !waitMSReqUpdate.orR && !waitMSVicUpdate.orR && !snoopConflict(e.task) && !e.waitMP && !s1_Block && !noFreeWay(e.task) &&
        !prefetchBlockedByMSHR(e.task)
    }
  }

  /* ======== Output ======== */
  // when entry.rdy is no longer true,
  // we cancel req in chosenQ, with the entry still held in buffer to issue later
//  val cancel = (canFlow && sameSet(chosenQ.io.deq.bits.bits.task, admitted.bits)) || !buffer(chosenQ.io.deq.bits.id).rdy
  val cancel = !Mux1H(chosenQ.io.deq.bits.idOH, buffer.map(e => e.valid && e.rdy))

  val issueHoldOwnerValid = Mux1H(issueHold.idOH, buffer.map(_.valid))
  val issueHoldGeneration = Mux1H(issueHold.idOH, buffer.map(_.generation))
  val issueHoldSet = Mux1H(issueHold.idOH, buffer.map(_.task.set))
  val issueHoldTag = Mux1H(issueHold.idOH, buffer.map(_.task.tag))
  val issueHoldSource = Mux1H(issueHold.idOH, buffer.map(_.task.sourceId))
  val issueHoldOpcode = Mux1H(issueHold.idOH, buffer.map(_.task.opcode))
  val issueHoldEntryWaitMSReq = Mux1H(issueHold.idOH, buffer.map(_.waitMSReq.orR))
  val issueHoldEntryWaitMSVic = Mux1H(issueHold.idOH, buffer.map(_.waitMSVic.orR))
  val issueHoldEntryWaitMP = Mux1H(issueHold.idOH, buffer.map(_.waitMP.orR))
  val issueHoldEntryReady = Mux1H(issueHold.idOH, buffer.map(_.rdy))
  val issueHoldTask = issueHold.bits.task
  // Eligibility is maintained in the owner's local registers above. Cross-
  // module conflict/way searches only write rdy's D; they never bypass it to
  // revoke a presented task in the same cycle. A canceled hold leaves its
  // buffer owner intact for a later arbitration attempt.
  val issueHoldCurrent = issueHoldValid && issueHoldOwnerValid &&
    issueHoldGeneration === issueHold.bits.generation &&
    issueHoldSet === issueHoldTask.set && issueHoldTag === issueHoldTask.tag &&
    issueHoldSource === issueHoldTask.sourceId &&
    issueHoldOpcode === issueHoldTask.opcode &&
    issueHoldEntryReady &&
    !issueHoldEntryWaitMSReq && !issueHoldEntryWaitMSVic && !issueHoldEntryWaitMP
  // Capture is mandatory, including when RequestArb is already ready. The
  // complete payload and selected owner both come from issueHold's Q.
  val issueHoldCapture = !issueHoldValid && chosenQValid && !cancel
  val issueHoldCancel = issueHoldValid && !issueHoldCommitted && !issueHoldCurrent
  val issueHoldFire = io.out.fire

  chosenQ.io.deq.ready := issueHoldCapture || cancel
  io.out.valid := issueHoldValid && (issueHoldCommitted || issueHoldCurrent)
  io.out.bits := issueHoldTask

  when (issueHoldCapture) {
    issueHold := chosenQ.io.deq.bits
    issueHoldValid := true.B
    issueHoldCommitted := false.B
  }.elsewhen (issueHoldFire || issueHoldCancel) {
    issueHoldValid := false.B
    issueHoldCommitted := false.B
  }.elsewhen(io.issueCommit) {
    issueHoldCommitted := true.B
  }
  when(io.issueCommit) {
    assert(issueHoldCurrent && !issueHoldCommitted && !io.out.fire,
      "RequestArb must commit one eligible retained RequestBuffer owner")
  }

  buffer.zip(issueHold.idOH.asBools).foreach {
    case (e, y) =>
      when (issueHoldFire && y) {
        e.valid := false.B
      }
  }
  when(issueHoldFire) {
    assert(PopCount(issueHold.idOH) === 1.U && issueHoldOwnerValid &&
      (issueHoldCommitted || issueHoldEntryReady),
      "a RequestBuffer issue must consume its registered or committed owner")
  }

  io.pfStatInMSHR.pfReleaseFromReqBuffer := issueHoldFire && isPrefetchTask(issueHoldTask)
  io.pfStatInMSHR.reqBufferPfReqSrc := issueHoldTask.reqSource
  io.pfStatInMSHR.reqBufferHoldLatency := Mux1H(issueHold.idOH, buffer.map(_.timer))

  // for Dir to choose a free way
  io.out.bits.wayMask := Fill(cacheParams.ways, 1.U(1.W))

  // add XSPerf to see how many cycles the req is held in Buffer
  if(cacheParams.enablePerf) {
    XSPerfAccumulate("drop_prefetch", admitted.valid && dup)
    XSPerfAccumulate("req_buffer_alloc", alloc)
    XSPerfAccumulate("req_buffer_full", full)
    // Early release observability: waitMSReq bits cleared while the target
    // context is still live (valid && !willFree) but its commit has landed --
    // i.e. releases that would previously have waited for full retirement.
    val waitMSReqVec = buffer.map(_.waitMSReq).reduce(_ | _)
    val commitLiveMask = VecInit(io.mshrInfo.map(s =>
      s.valid && s.bits.refillCommitLanded)).asUInt
    val commitReleasePrev = RegEnable(commitLiveMask & waitMSReqVec, 0.U(mshrsAll.W), true.B)
    val commitReleaseBits = commitReleasePrev & ~waitMSReqVec
    XSPerfAccumulate("reqbuf_commitlanded_release", PopCount(commitReleaseBits))
    XSPerfAccumulate("recv_prefetch", admitted.fire && isPrefetch)
    XSPerfAccumulate("recv_normal", admitted.fire && !isPrefetch)
    XSPerfAccumulate("chosenQ_cancel", chosenQValid && cancel)
    XSPerfAccumulate("req_buffer_mergeA", io.hasMergeA)
    XSPerfAccumulate("req_buffer_pf_block_occ", admitted.valid && isPrefetchTask(in) && pfThrottleByOcc)
    // TODO: count conflict
    for(i <- 0 until entries){
      val cntEnable = PopCount(buffer.map(_.valid)) === i.U
      XSPerfAccumulate(s"req_buffer_util_$i", cntEnable)
    }
    val bufferTimer = RegInit(VecInit(Seq.fill(entries)(0.U(16.W))))
    buffer zip bufferTimer map {
      case (e, t) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(RegNext(e.valid) && !e.valid)) { t := 0.U }
        assert(t < 20000.U, "ReqBuf Leak")

        val enable = RegNext(e.valid) && !e.valid
        XSPerfHistogram("reqBuf_timer", t, enable, 0, 20, 1, right_strict = true)
        XSPerfHistogram("reqBuf_timer", t, enable, 20, 400, 20, left_strict = true)
        XSPerfMax("max_reqBuf_timer", t, enable)

        // assert !(all entries occupied for 100 cycles)
    }
  }
}
