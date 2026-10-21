package oceanus.l2

// Intent (phase2): finite, observable input buffering never stalls cache events.
// VBOP uses a fixed +2-line VA offset and bypasses TLB within a 4 KiB page.
// CDP follows word zero of the low half only, stopping at depth two; eviction
// events only increment a counter. These are path-validation stubs, not algorithms.

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class L2StubRequest(implicit val p: Parameters) extends Bundle {
  val paddr = UInt(p(L2ParamsKey).physicalAddrWidth.W)
  val depth = UInt(2.W)
}

class L2VBOPStub(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val train = Flipped(Valid(new L2PrefetchTrain(true)))
    val tlb = Flipped(new L2PrefetchTranslationClientIO)
    val req = Decoupled(new L2StubRequest)
  })
  val input = Module(new OverwriteQueue(new L2PrefetchTrain(true), 16))
  input.io.enq.valid := io.train.valid && io.train.bits.trainEnable.get && io.enable
  input.io.enq.bits := io.train.bits
  val target = (Cat(input.io.deq.bits.vaddr.get, 0.U(6.W)) + 128.U)(49, 0)
  val samePage = target(49, 12) === input.io.deq.bits.vaddr.get(43, 6)
  val idle :: translating :: Nil = Enum(2)
  val state = RegInit(idle)
  val offer = input.io.deq.valid && state === idle && io.enable
  io.tlb.req.valid := offer && !samePage
  io.tlb.req.bits := target
  io.tlb.resp.ready := !io.enable || io.tlb.resp.bits.dropped || io.req.ready
  io.req.valid := io.enable && Mux(state === idle, offer && samePage,
    io.tlb.resp.valid && !io.tlb.resp.bits.dropped)
  io.req.bits.paddr := Mux(state === idle,
    Cat(input.io.deq.bits.paddr(p(L2ParamsKey).physicalAddrWidth - 1, 12), target(11, 0)),
    io.tlb.resp.bits.paddr)
  io.req.bits.depth := 0.U
  input.io.deq.ready := state === idle && (io.req.fire || io.tlb.req.fire)
  when(io.tlb.req.fire) { state := translating }
  when(io.tlb.resp.fire) { assert(state === translating); state := idle }
  val overwritten = RegInit(0.U(64.W))
  when(input.io.overwrite) { overwritten := overwritten + 1.U }
  dontTouch(overwritten)
}

class L2CDPStub(val evictPorts: Int)(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val data = Flipped(Valid(new L2CDPHalf))
    val evict = Flipped(Vec(evictPorts, Valid(new L2CDPEvict)))
    val tlb = Flipped(new L2PrefetchTranslationClientIO)
    val req = Decoupled(new L2StubRequest)
  })
  val input = Module(new OverwriteQueue(new Bundle {
    val va = UInt(50.W)
    val depth = UInt(2.W)
  }, 16))
  val pointer = io.data.bits.data(63, 0)
  val canonical = pointer(63, 50) === Fill(14, pointer(49))
  input.io.enq.valid := io.enable && io.data.valid && !io.data.bits.high &&
    io.data.bits.pfDepth < 2.U && pointer =/= 0.U && canonical
  input.io.enq.bits.va := Cat(pointer(49, 6), 0.U(6.W))
  input.io.enq.bits.depth := io.data.bits.pfDepth + 1.U
  val waiting = RegInit(false.B)
  val depth = Reg(UInt(2.W))
  io.tlb.req.valid := input.io.deq.valid && !waiting && io.enable
  io.tlb.req.bits := input.io.deq.bits.va
  input.io.deq.ready := io.tlb.req.fire
  when(io.tlb.req.fire) { waiting := true.B; depth := input.io.deq.bits.depth }
  io.req.valid := io.enable && io.tlb.resp.valid && !io.tlb.resp.bits.dropped
  io.req.bits.paddr := Cat(io.tlb.resp.bits.paddr(p(L2ParamsKey).physicalAddrWidth - 1, 6), 0.U(6.W))
  io.req.bits.depth := depth
  io.tlb.resp.ready := !io.enable || io.tlb.resp.bits.dropped || io.req.ready
  when(io.tlb.resp.fire) { assert(waiting); waiting := false.B }
  val evictions = RegInit(0.U(64.W))
  evictions := evictions + PopCount(io.evict.map(_.valid))
  val overwritten = RegInit(0.U(64.W))
  when(input.io.overwrite) { overwritten := overwritten + 1.U }
  Seq(evictions, overwritten).foreach(dontTouch(_))
}
