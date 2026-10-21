package xscache.coupledL2

import chisel3._
import chisel3.util._
import chisel3.reflect.DataMirror
import chisel3.ActualDirection
import chisel3.simulator.ChiselSim
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters

// Observations only: allocation, task acceptance, retry and completion all
// drive the real MSHR through its normal ports.
class ObservableGrantAdmissionMSHR(implicit p: Parameters) extends MSHR {
  val admission = IO(Output(new Bundle {
    val accepted = Bool()
    val issued = Bool()
    val writePending = Bool()
  }))
  admission.accepted := grantAccepted
  admission.issued := grantIssued
  admission.writePending := refillWritePending
}

class GrantAdmissionOwnershipHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val allocate = Input(Bool())
    // 0: hit AcquireBlock; 1: miss AcquirePerm; 2: hit Get.
    val mode = Input(UInt(2.W))
    val accept = Input(Bool())
    val grantSent = Input(Bool())
    val replResp = Input(Bool())
    val retry = Input(Bool())
    val writeDone = Input(Bool())
    val snoop = Input(Bool())
    val live = Output(Bool())
    val epoch = Output(Bool())
    val accepted = Output(Bool())
    val issued = Output(Bool())
    val pending = Output(Bool())
    val writePending = Output(Bool())
    val task = Output(Bool())
    val lookup = Output(Bool())
    val commit = Output(Bool())
    val snoopAdmit = Output(Bool())
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val m = Module(new ObservableGrantAdmissionMSHR)
  val rx = Module(new RXSNP)
  zeroInputs(m.io); zeroInputs(rx.io)
  m.io.alloc.valid := io.allocate
  m.io.alloc.bits.state.elements.foreach(_._2 := true.B)
  m.io.alloc.bits.state.s_refill := false.B
  m.io.alloc.bits.state.w_replResp := io.mode =/= 1.U
  m.io.alloc.bits.task.channel := 1.U
  m.io.alloc.bits.task.opcode := Mux(io.mode === 1.U, AcquirePerm,
    Mux(io.mode === 2.U, Get, AcquireBlock))
  m.io.alloc.bits.task.param := NtoB
  m.io.alloc.bits.task.set := 2.U
  m.io.alloc.bits.task.tag := 3.U
  m.io.alloc.bits.dirResult.hit := io.mode =/= 1.U
  m.io.alloc.bits.dirResult.set := 2.U
  m.io.alloc.bits.dirResult.tag := 3.U
  m.io.alloc.bits.dirResult.way := 1.U
  m.io.alloc.bits.dirResult.meta.state := MetaData.TIP
  m.io.tasks.mainpipe.ready := io.accept
  m.io.grantSent := io.grantSent
  m.io.refillWriteDone := io.writeDone
  m.io.replResp.valid := io.replResp
  m.io.replResp.bits.retry := io.retry
  m.io.replResp.bits.hit := io.mode =/= 1.U
  m.io.replResp.bits.set := 2.U
  m.io.replResp.bits.tag := 3.U
  m.io.replResp.bits.way := 1.U
  m.io.replResp.bits.meta.state := Mux(io.mode === 1.U, MetaData.INVALID, MetaData.TIP)
  rx.io.refillInfo(0) := m.io.msInfo
  rx.io.rxsnp.valid := io.snoop
  rx.io.rxsnp.bits.opcode := 7.U
  rx.io.rxsnp.bits.addr := Cat(3.U(m.tagBits.W), 2.U(m.setBits.W), 0.U(m.offsetBits.W)) >> 3
  rx.io.task.ready := true.B
  io.live := m.io.status.valid
  io.epoch := m.io.allocEpoch
  io.accepted := m.admission.accepted
  io.issued := m.admission.issued
  io.pending := m.io.msInfo.bits.grantPending
  io.writePending := m.admission.writePending
  io.task := m.io.tasks.mainpipe.valid
  io.lookup := m.io.tasks.mainpipe.bits.normalRefillRevalidate
  io.commit := m.io.tasks.mainpipe.bits.refillOnly && !m.io.tasks.mainpipe.bits.normalRefillRevalidate
  io.snoopAdmit := rx.io.task.valid
}

object TestGrantAdmissionOwnership extends App with ChiselSim {
  import PairMiterParameters._
  simulate(new GrantAdmissionOwnershipHarness) { d =>
    d.io.allocate.poke(false.B); d.io.mode.poke(0.U); d.io.accept.poke(false.B)
    d.io.grantSent.poke(false.B); d.io.replResp.poke(false.B); d.io.retry.poke(false.B)
    d.io.writeDone.poke(false.B); d.io.snoop.poke(false.B)
    d.clock.step(3)
    def allocate(mode: Int): Unit = {
      d.io.mode.poke(mode.U); d.io.allocate.poke(true.B); d.clock.step()
      d.io.allocate.poke(false.B); d.io.live.expect(true.B)
    }
    def accept(): Unit = {
      d.io.task.expect(true.B); d.io.accept.poke(true.B); d.clock.step(); d.io.accept.poke(false.B)
    }
    def response(retry: Boolean): Unit = {
      d.io.replResp.poke(true.B); d.io.retry.poke(retry.B); d.clock.step()
      d.io.replResp.poke(false.B); d.io.retry.poke(false.B)
    }
    def sent(): Unit = {
      d.io.grantSent.poke(true.B)
      d.io.pending.expect(true.B) // event cannot change the already-reserved window
      d.clock.step(); d.io.grantSent.poke(false.B); d.io.issued.expect(true.B)
    }
    def commitAndRetire(): Unit = {
      d.io.task.expect(true.B); d.io.commit.expect(true.B); accept()
      d.io.writePending.expect(true.B); d.io.pending.expect(true.B)
      for (_ <- 0 until 8) { d.io.live.expect(true.B); d.io.pending.expect(true.B); d.clock.step() }
      d.io.writeDone.poke(true.B); d.clock.step(); d.io.writeDone.poke(false.B)
      d.io.writePending.expect(false.B)
      d.clock.step(8); d.io.pending.expect(false.B); d.io.live.expect(false.B)
    }

    allocate(0)
    d.io.accepted.expect(false.B); d.io.pending.expect(false.B)
    d.clock.step(7); d.io.accepted.expect(false.B)
    accept()
    d.io.accepted.expect(true.B); d.io.pending.expect(true.B); d.io.issued.expect(false.B)
    d.io.snoop.poke(true.B)
    for (_ <- 0 until 11) {
      d.io.pending.expect(true.B); d.io.snoopAdmit.expect(false.B); d.io.issued.expect(false.B)
      d.io.live.expect(true.B); d.clock.step()
    }
    d.io.snoop.poke(false.B)
    sent()
    d.io.lookup.expect(true.B); accept(); response(false)
    commitAndRetire()
    println("PASS: hit Grant admission, SourceD stall, RXSNP hold and commit-to-retire")

    // A folded miss lookup can suppress the accepted Grant at S3. Its
    // reservation must release without reporting a fictitious SourceD fire.
    allocate(1)
    accept(); d.io.pending.expect(true.B); d.io.issued.expect(false.B)
    response(true)
    d.io.accepted.expect(false.B); d.io.pending.expect(false.B); d.io.issued.expect(false.B)
    accept(); d.io.pending.expect(true.B)
    response(false); sent(); commitAndRetire()
    println("PASS: miss folded-lookup retry cancels and replays admission without false Grant completion")

    // A non-client Get does not gain an Acquire snoop reservation.
    allocate(2); accept()
    d.io.accepted.expect(true.B); d.io.pending.expect(false.B); d.io.issued.expect(false.B)
    println("PASS: actual MSHR Grant admission, stalled SourceD, same-line RXSNP block, retry replay, commit hold, retire/reuse and Get qualification")
  }
}
