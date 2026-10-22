// Intent (audit-fix-1): share the production pre-arbitration filtering with a
// directed harness covering simultaneous disabled/enabled TSHRs and slices.
package oceanus.l2

import chisel3._
import chisel3.util._
import utility.FastArbiter
import org.chipsalliance.cde.config.Parameters

object L2PrefetchTrainSelect {
  def enabled(in: ValidIO[L2PrefetchTrain]): Bool =
    in.valid && in.bits.trainEnable.getOrElse(true.B)
  def local(in: Seq[ValidIO[L2PrefetchTrain]]): ValidIO[L2PrefetchTrain] = {
    val out = Wire(Valid(chiselTypeOf(in.head.bits)))
    val candidates = in.map(enabled)
    out.valid := candidates.reduce(_ || _)
    out.bits := PriorityMux(candidates.zip(in.map(_.bits)))
    out
  }
}

class L2PrefetchTrainArb(inputs: Int, sideband: Boolean)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(inputs, Valid(new L2PrefetchTrain(sideband))))
    val out = Decoupled(new L2PrefetchTrain(sideband))
  })
  val arb = Module(new FastArbiter(new L2PrefetchTrain(sideband), inputs))
  arb.io.in.zip(io.in).foreach { case (sink, source) =>
    sink.valid := L2PrefetchTrainSelect.enabled(source)
    sink.bits := source.bits
  }
  io.out <> arb.io.out
}
