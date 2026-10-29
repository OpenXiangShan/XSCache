package oceanus.l2

import chisel3._
import chisel3.util._

// R10 observation only. An event is accepted at the learner admission boundary.
// All other raw one-cycle events (disabled, filtered, arbitration or queue loss)
// are dropped. Rows are BOP, VBOP, CDP VPN and CDP Filter; columns are upstream
// physical SrcIDs followed by an eviction bucket. There is no replay/flow control.
class L2PrefetchTrainCounters(sources: Int, maxEvents: Int) extends Module {
  val io = IO(new Bundle {
    val total = Input(Vec(4, Vec(sources, UInt(log2Ceil(maxEvents+1).W))))
    val accepted = Input(Vec(4, Vec(sources, Bool())))
    val totals = Output(Vec(4, Vec(sources, UInt(64.W))))
    val accepts = Output(Vec(4, Vec(sources, UInt(64.W))))
    val drops = Output(Vec(4, Vec(sources, UInt(64.W))))
  })
  for (sink <- 0 until 4; source <- 0 until sources) {
    val total = RegInit(0.U(64.W))
    val accepted = RegInit(0.U(64.W))
    val dropped = RegInit(0.U(64.W))
    total := total + io.total(sink)(source)
    accepted := accepted + io.accepted(sink)(source)
    dropped := dropped + (io.total(sink)(source) - io.accepted(sink)(source).asUInt)
    assert(io.accepted(sink)(source).asUInt <= io.total(sink)(source), "R10 acceptance without raw event")
    assert(total === (accepted + dropped), "R10 accepted + dropped != total")
    io.totals(sink)(source) := total
    io.accepts(sink)(source) := accepted
    io.drops(sink)(source) := dropped
  }
  dontTouch(io.totals); dontTouch(io.accepts); dontTouch(io.drops)
}
