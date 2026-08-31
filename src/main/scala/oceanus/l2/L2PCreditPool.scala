package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import oceanus.l2.L2Common.fastArb

object L2PCreditPool {

  class Entry extends Bundle {
    val pCrdType = UInt(4.W)
    val srcId = UInt(12.W) // TODO: parameterize with CHI node id width
  }
}

class L2PCreditPool(val sliceNum: Int)(implicit val p: Parameters) extends Module with HasL2Params {

  val entryCount = sliceNum * paramL2.mshrSize

  class EmptyBundle extends Bundle

  val io = IO(new Bundle {
    val pCrdGrant = Flipped(Valid(new L2PCreditPool.Entry))
    val mshrQuery = Input(Vec(sliceNum, Vec(paramL2.mshrSize, Valid(new L2PCreditPool.Entry))))
    val mshrGrant = Output(Vec(sliceNum, Vec(paramL2.mshrSize, Bool())))
  })

  val queue = Module(new Queue(new L2PCreditPool.Entry, entryCount - 2))

  val queueSkid = Module(new Queue(new L2PCreditPool.Entry, 2))

  queue.io.enq.valid := io.pCrdGrant.valid
  queue.io.enq.bits := io.pCrdGrant.bits

  queueSkid.io.enq.bits := queue.io.deq.bits
  queueSkid.io.enq.valid := queue.io.deq.valid
  queue.io.deq.ready := queueSkid.io.enq.ready

  val mshrPCrdHitVec = io.mshrQuery.flatten.map { case m => {
    m.valid && queueSkid.io.deq.valid && m.bits.srcId === queueSkid.io.deq.bits.srcId && m.bits.pCrdType === queueSkid.io.deq.bits.pCrdType 
  }}

  val mshrPCrdArbIn = mshrPCrdHitVec.zip(io.mshrGrant.flatten).map { case (hit, grant) => {
    val p = Wire(Decoupled(new EmptyBundle))
    p.valid := hit
    grant := p.ready
    p
  }}

  val mshrPCrdArbOut = {
    val p = Wire(Decoupled(new EmptyBundle))
    p.ready := true.B
    queueSkid.io.deq.ready := p.valid
    p
  }

  fastArb(mshrPCrdArbIn, mshrPCrdArbOut, Some("PCreditPool"))

  assert(!(queue.io.enq.valid && !queue.io.enq.ready), "P-Credit Pool overflow")
}
