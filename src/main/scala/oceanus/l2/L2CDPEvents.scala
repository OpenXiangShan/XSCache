package oceanus.l2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

// Intent (1c-2): these are cache observations, before CDP's algorithmic
// source/depth filters. Context and data travel together through every queue.
object L2CDPEvent {
  val Refill = 0
  val Hit = 1
}

class L2CDPContext(implicit val p: Parameters) extends Bundle with HasL2Params {
  val paddr = UInt(paramL2.physicalAddrWidth.W)
  val pfSource = UInt(L2PfSource.Width.W)
  val pfDepth = UInt(2.W)
  val isHit = Bool()
  val eventType = UInt(1.W)
}

class L2CDPLine(implicit p: Parameters) extends L2CDPContext {
  val data = UInt(512.W)
}

class L2CDPHalf(implicit p: Parameters) extends L2CDPContext {
  val data = UInt(256.W)
  val high = Bool()
}

class L2CDPEvict(implicit val p: Parameters) extends Bundle with HasL2Params {
  val paddr = UInt(paramL2.physicalAddrWidth.W)
  val pfSource = UInt(L2PfSource.Width.W)
  val pfDepth = UInt(2.W)
}

class L2CDPTrain(implicit p: Parameters) extends L2CDPEvict {
  val vaddr = UInt(44.W)
  val reqSource = UInt(5.W)
  val trainEnable = Bool()
  val hit = Bool()
}

// Intent (1c-2): accept each half independently, including one free slot, and
// count every dropped half. Reclaim a slot on simultaneous dequeue. Advancing
// by the actual accepted count avoids the local utility MIMOQueue tail-fire
// assumption (digest section 6). Never backpressure a cache event.
class L2CDPHalfQueue(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new L2CDPLine))
    val out = Decoupled(new L2CDPHalf)
    val dropped = Output(UInt(64.W))
    val occupancy = Output(UInt(4.W))
  })
  val entries = Reg(Vec(8, new L2CDPHalf))
  val head = RegInit(0.U(3.W))
  val tail = RegInit(0.U(3.W))
  val count = RegInit(0.U(4.W))
  io.out.valid := count =/= 0.U
  io.out.bits := entries(head)
  val space = 8.U(4.W) - count + io.out.fire
  val accepted = Mux(!io.in.valid, 0.U, Mux(space >= 2.U, 2.U, space))
  for (i <- 0 until 2) {
    val half = Wire(new L2CDPHalf)
    half.paddr := io.in.bits.paddr
    half.pfSource := io.in.bits.pfSource
    half.pfDepth := io.in.bits.pfDepth
    half.isHit := io.in.bits.isHit
    half.eventType := io.in.bits.eventType
    half.data := io.in.bits.data(256 * (i + 1) - 1, 256 * i)
    half.high := (i == 1).B
    when(accepted > i.U) { entries((tail + i.U)(2, 0)) := half }
  }
  when(io.out.fire) { head := head + 1.U }
  tail := tail + accepted
  count := count + accepted - io.out.fire
  val dropped = RegInit(0.U(64.W))
  when(io.in.valid) { dropped := dropped + (2.U - accepted) }
  io.dropped := dropped
  io.occupancy := count
  assert(count <= 8.U)
}
