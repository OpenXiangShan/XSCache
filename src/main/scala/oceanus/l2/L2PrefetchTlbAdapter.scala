package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

// Intent (1c-1): reserve a transaction slot through retry and client retirement;
// capture ownership only on handshakes and pair every response with next-cycle PMP.
// Deviation from reference Prefetcher.scala:250-264 / BestOffsetPrefetch.scala:450-516:
// the reference broadcasts responses and consumes fixed pipeline positions. Here
// clients tolerate arbitrary latency/backpressure and receive only their own result.
// The ID-less external TLB MUST return responses in request order; PMP is exactly
// one cycle after resp.fire. No adapter can recover out-of-order ownership without IDs.
class L2PrefetchTranslationResult(implicit p: Parameters) extends Bundle {
  val vaddr = UInt(50.W)
  val paddr = UInt(p(L2ParamsKey).physicalAddrWidth.W)
  val retried = Bool()
  val miss = Bool() // terminal second miss
  val fault = Bool() // TLB load exception or PMP load denial
  val mmio = Bool()
  val uncache = Bool() // PBMT NC/IO (reserved PBMT is not rejected by reference)
  val dropped = Bool()
}

class L2PrefetchTranslationClientIO(implicit p: Parameters) extends Bundle {
  val req = Flipped(Decoupled(UInt(50.W))) // full byte VA; read/size=3/prefetch
  val resp = Decoupled(new L2PrefetchTranslationResult)
}

class L2PrefetchTlbAdapter(val entries: Int = 8)(implicit val p: Parameters) extends Module {
  require(entries >= 2)
  val io = IO(new Bundle {
    val clients = Vec(2, new L2PrefetchTranslationClientIO) // VBOP > CDP
    val tlb = new L2PrefetchTlbIO
  })

  val indexBits = log2Ceil(entries)
  val free :: pending :: waiting :: retry :: done :: Nil = Enum(5)
  val state = RegInit(VecInit(Seq.fill(entries)(free)))
  val address = Reg(Vec(entries, UInt(50.W)))
  val owner = Reg(Vec(entries, Bool()))
  val retried = Reg(Vec(entries, Bool()))
  val timer = Reg(Vec(entries, UInt(4.W)))
  val results = Reg(Vec(entries, new L2PrefetchTranslationResult))

  val available = VecInit(state.map(_ === free))
  val alloc = PriorityEncoder(available)
  io.clients(0).req.ready := available.asUInt.orR
  io.clients(1).req.ready := available.asUInt.orR && !io.clients(0).req.valid
  for (c <- 0 until 2) {
    when(io.clients(c).req.fire) {
      state(alloc) := pending
      address(alloc) := io.clients(c).req.bits
      owner(alloc) := (c == 1).B
      retried(alloc) := false.B
      timer(alloc) := 0.U
    }
  }
  for (i <- 0 until entries) {
    when(state(i) =/= free && timer(i) =/= 0.U) { timer(i) := timer(i) - 1.U }
  }

  val eligible = VecInit((0 until entries).map(i =>
    state(i) === pending || (state(i) === retry && timer(i) === 0.U)))
  val vbop = VecInit((0 until entries).map(i => eligible(i) && !owner(i)))
  val selected = PriorityEncoder(Mux(vbop.asUInt.orR, vbop.asUInt, eligible.asUInt))
  // A lower-priority request already offered to a stalled TLB must remain stable.
  val locked = RegInit(false.B)
  val lockedIndex = Reg(UInt(indexBits.W))
  val issue = Mux(locked, lockedIndex, selected)
  val inflight = Module(new Queue(UInt(indexBits.W), entries))
  io.tlb.req.valid := (locked || eligible.asUInt.orR) && inflight.io.enq.ready
  io.tlb.req.bits := 0.U.asTypeOf(io.tlb.req.bits)
  io.tlb.req.bits.vaddr := Mux(io.tlb.req.valid, address(issue), 0.U)
  io.tlb.req.bits.isPrefetch := true.B
  io.tlb.req.bits.size := 3.U
  io.tlb.req_kill := false.B
  when(io.tlb.req.valid && !io.tlb.req.ready) { locked := true.B; lockedIndex := issue }
  when(io.tlb.req.fire) { locked := false.B; state(issue) := waiting }
  inflight.io.enq.valid := io.tlb.req.fire
  inflight.io.enq.bits := issue
  io.tlb.resp.ready := true.B
  inflight.io.deq.ready := io.tlb.resp.valid
  when(io.tlb.resp.valid) {
    assert(inflight.io.deq.valid, "TLB response without a reserved request")
    assert(state(inflight.io.deq.bits) === waiting)
  }

  val paired = RegNext(io.tlb.resp.fire, false.B)
  val response = RegEnable(io.tlb.resp.bits, io.tlb.resp.fire)
  val responseIndex = RegEnable(inflight.io.deq.bits, io.tlb.resp.fire)
  // Reference CDP.scala:47,1114-1117,1135,1189-1198: count 10 from TLB miss.
  // Reference BestOffsetPrefetch.scala:48,531-547,563: count 10 from paired PMP.
  when(io.tlb.resp.fire && io.tlb.resp.bits.miss &&
    owner(inflight.io.deq.bits) && !retried(inflight.io.deq.bits)) {
    timer(inflight.io.deq.bits) := 10.U
  }
  val firstMiss = response.miss && !retried(responseIndex)
  val terminal = paired && !firstMiss
  val fault = response.excp.head.pf.ld || response.excp.head.gpf.ld ||
    response.excp.head.af.ld || io.tlb.pmp_resp.ld
  val uncache = response.pbmt === 1.U || response.pbmt === 2.U
  when(paired) {
    when(firstMiss) {
      state(responseIndex) := retry
      retried(responseIndex) := true.B
      when(!owner(responseIndex)) { timer(responseIndex) := 10.U }
    }.otherwise {
      state(responseIndex) := done
      val result = results(responseIndex)
      result.vaddr := address(responseIndex)
      result.paddr := response.paddr.head
      result.retried := retried(responseIndex)
      result.miss := response.miss
      // Like reference, ignore undefined fault/PMP/PBMT on a miss response.
      result.fault := !response.miss && fault
      result.mmio := !response.miss && io.tlb.pmp_resp.mmio
      result.uncache := !response.miss && uncache
      result.dropped := response.miss || fault || io.tlb.pmp_resp.mmio || uncache
    }
  }

  // At most entries transactions exist across ALL stages, including retries
  // and unconsumed results. Each per-client completion FIFO therefore has room
  // even if resp.ready stays low forever; external resp.ready can stay high.
  for (c <- 0 until 2) {
    val completed = Module(new Queue(UInt(indexBits.W), entries))
    completed.io.enq.valid := terminal && owner(responseIndex) === (c == 1).B
    completed.io.enq.bits := responseIndex
    when(completed.io.enq.valid) { assert(completed.io.enq.ready) }
    io.clients(c).resp.valid := completed.io.deq.valid
    io.clients(c).resp.bits := results(completed.io.deq.bits)
    completed.io.deq.ready := io.clients(c).resp.ready
    when(io.clients(c).resp.fire) {
      assert(state(completed.io.deq.bits) === done)
      assert(owner(completed.io.deq.bits) === (c == 1).B)
      state(completed.io.deq.bits) := free
    }
  }
}
