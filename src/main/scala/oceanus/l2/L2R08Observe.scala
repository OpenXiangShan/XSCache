package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility._

// Observation-only lineage survives DirRel's provenance clear. A release is
// not demand use and does not terminate the line's lifetime. Pending refills
// consumed before installation already carry source=0 at the tag commit.
class L2R08Observe(implicit val p: Parameters) extends Module with HasL2Params {
  val io = IO(new Bundle {
    val install = Input(Valid(new Bundle {
      val paddr = UInt(paramL2.physicalAddrWidth.W)
      val way = UInt(wayBits.W)
      val source = UInt(L2PfSource.Width.W)
      val prefetch = Bool()
    }))
    val demand = Input(Vec(nMSHR, Valid(UInt(paramL2.physicalAddrWidth.W))))
    val evict = Input(Vec(nMSHR, Valid(UInt(paramL2.physicalAddrWidth.W))))
  })
  // Keep observer memories as memories through CHIRRTL. Dynamic writes into
  // a Reg Vec from every TSHR expand into replicated mux/decode trees and
  // make full-model generation unnecessarily large.
  val source = Mem(paramL2.sets, Vec(paramL2.ways, UInt(4.W)))
  val tag = Mem(paramL2.sets, Vec(paramL2.ways, UInt(tagWidth.W)))
  val init = RegInit(true.B)
  val initSet = RegInit(0.U(setBits.W))
  when (init) {
    source.write(initSet, VecInit(Seq.fill(paramL2.ways)(0.U(4.W))))
    when (initSet === (paramL2.sets - 1).U) { init := false.B }
      .otherwise { initSet := initSet + 1.U }
  }
  def setOf(pa: UInt): UInt = pa(offsetBits + setBits - 1, offsetBits)
  def tagOf(pa: UInt): UInt = pa(paramL2.physicalAddrWidth - 1, offsetBits + setBits)
  val evictedSources = io.evict.map { e =>
    val s = setOf(e.bits)
    val matches = VecInit((0 until paramL2.ways).map(w => source(s)(w) =/= 0.U && tag(s)(w) === tagOf(e.bits)))
    val usedNow = io.demand.map(d => d.valid && d.bits === e.bits).reduce(_ || _)
    val unused = e.valid && !usedNow && !init
    when(e.valid && !init) { assert(PopCount(matches) <= 1.U, "R08 observation duplicate lineage tag") }
    when(e.valid && !init && matches.asUInt.orR) {
      source.write(s, VecInit(Seq.fill(paramL2.ways)(0.U(4.W))), matches.toSeq)
    }
    Mux(unused, Mux1H(matches, source(s)), 0.U)
  }
  for(d <- io.demand) {
    val s = setOf(d.bits)
    val matches = (0 until paramL2.ways).map(w => tag(s)(w) === tagOf(d.bits))
    when(d.valid && !init && matches.reduce(_ || _)) {
      source.write(s, VecInit(Seq.fill(paramL2.ways)(0.U(4.W))), matches)
    }
  }
  when(io.install.valid && !init) {
    val s = setOf(io.install.bits.paddr)
    val lane = (0 until paramL2.ways).map(w => io.install.bits.way === w.U)
    source.write(s, VecInit(Seq.fill(paramL2.ways)(io.install.bits.source)), lane)
    tag.write(s, VecInit(Seq.fill(paramL2.ways)(tagOf(io.install.bits.paddr))), lane)
  }
  XSPerfAccumulate("r08_insert_prefetch", io.install.valid && io.install.bits.prefetch)
  XSPerfAccumulate("r08_insert_demand", io.install.valid && !io.install.bits.prefetch)
  for(s <- 1 until 16) {
    XSPerfAccumulate(s"r08_unused_evict_source${s}", PopCount(evictedSources.map(_ === s.U)))
  }
}
