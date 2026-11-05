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
    val admitted = Output(Vec(inputs, Valid(new L2PrefetchTrain(sideband))))
    val pending = Output(Vec(inputs, Valid(new L2PrefetchTrain(sideband))))
  })
  io.admitted := 0.U.asTypeOf(io.admitted)
  io.pending := 0.U.asTypeOf(io.pending)
  val arb = Module(new FastArbiter(new L2PrefetchTrain(sideband), inputs))
  // One entry per slice before arbitration, matching coupledL2 MainPipe.
  // Pipeline(s.train) in coupledL2 uses pipe=true and flow=false: a
  // serviced entry can be replaced on the same edge without a throughput bubble.
  val queues = if (buffered) Seq.fill(inputs)(Module(new Queue(new L2PrefetchTrain(sideband), 1, pipe=true, flow=false))) else Nil
  arb.io.in.zip(io.in).zipWithIndex.foreach { case ((sink, source), i) =>
    if (buffered) {
      queues(i).io.enq.valid := L2PrefetchTrainSelect.enabled(source)
      queues(i).io.enq.bits := source.bits
      sink <> queues(i).io.deq
      io.admitted(i).valid := queues(i).io.enq.fire
      io.admitted(i).bits := source.bits
      io.pending(i).valid := queues(i).io.deq.valid
      io.pending(i).bits := queues(i).io.deq.bits
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
  if (buffered) {
    val rawConcurrent = PopCount(io.in.map(L2PrefetchTrainSelect.enabled))
    XSPerfAccumulate("slice_train_raw_concurrent_unselected", rawConcurrent - rawConcurrent.orR.asUInt)
    XSPerfAccumulate("slice_train_arb_unselected", PopCount(arb.io.in.map(_.valid)) - arb.io.out.valid.asUInt)
    XSPerfAccumulate("slice_train_arb_fire", arb.io.out.fire)
  }
}
