// Intent (audit-fix-1): share the production pre-arbitration filtering with a
// directed harness covering simultaneous disabled/enabled TSHRs and slices.
package oceanus.l2

import chisel3._
import chisel3.util._
import utility.{FastArbiter, XSPerfAccumulate, XSPerfReference}
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

class L2PrefetchTrainArb(inputs: Int, sideband: Boolean, buffered: Boolean = false)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(inputs, Valid(new L2PrefetchTrain(sideband))))
    val out = Decoupled(new L2PrefetchTrain(sideband))
  })
  val arb = Module(new FastArbiter(new L2PrefetchTrain(sideband), inputs))
  // One entry per slice before arbitration, matching coupledL2 MainPipe.
  // Non-pipelined queue: a full queue may drop a simultaneous replacement.
  val queues = if (buffered) Seq.fill(inputs)(Module(new Queue(new L2PrefetchTrain(sideband), 1))) else Nil
  arb.io.in.zip(io.in).zipWithIndex.foreach { case ((sink, source), i) =>
    if (buffered) {
      queues(i).io.enq.valid := L2PrefetchTrainSelect.enabled(source)
      queues(i).io.enq.bits := source.bits
      sink <> queues(i).io.deq
      XSPerfAccumulate(s"slice${i}_train_queue_enq", queues(i).io.enq.fire)
      XSPerfAccumulate(s"slice${i}_train_queue_drop", queues(i).io.enq.valid && !queues(i).io.enq.ready)
      XSPerfAccumulate(s"slice${i}_train_arb_unselected", sink.valid && !sink.ready)
      XSPerfReference(s"slice${i}_train_pending", queues(i).io.count)

    } else {
      sink.valid := L2PrefetchTrainSelect.enabled(source)
      sink.bits := source.bits
    }
  }
  io.out <> arb.io.out
  // In buffered mode these are waiting service opportunities, not losses.
  XSPerfAccumulate("slice_train_arb_unselected", PopCount(arb.io.in.map(_.valid)) - arb.io.out.valid.asUInt)
  XSPerfAccumulate("slice_train_arb_fire", arb.io.out.fire)
}
