package oceanus.l2

// Intent (3a): remove L2VBOPStub after OceanusVBOP replaces its integration;
// retain the CDP validation stub until its separate algorithm migration.

// Intent (phase2): finite, observable input buffering never stalls cache events.
// CDP follows word zero of the low half only, stopping at depth two; eviction
// events only increment a counter. This remains a path-validation stub.

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class L2CDPStub(val evictPorts: Int)(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val data = Flipped(Valid(new L2CDPHalf))
    val evict = Flipped(Vec(evictPorts, Valid(new L2CDPEvict)))
    val tlb = Flipped(new L2PrefetchTranslationClientIO)
    val req = Decoupled(new L2PrefetchAlgorithmRequest)
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
