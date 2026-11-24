package xscache.coupledL2

import chisel3._
import chisel3.util._
import chisel3.reflect.DataMirror
import chisel3.ActualDirection
import chisel3.simulator.ChiselSim
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import org.chipsalliance.cde.config.Parameters

class ObservableTimingOwnershipCtl(implicit p: Parameters) extends MSHRCtl {
  val ownership = IO(Output(new Bundle {
    val normalProbe = UInt(mshrsAll.W)
    val reserve = UInt(mshrsAll.W)
    val wanted = UInt(mshrsAll.W)
    val allocated = Bool()
    val slot = UInt(mshrBits.W)
    val live = UInt(mshrsAll.W)
    val captured = UInt(mshrsAll.W)
  }))
  ownership.normalProbe := VecInit(refillMshrs.map(_.io.tasks.source_b.valid)).asUInt
  ownership.reserve := replaceReserveValid.asUInt
  ownership.wanted := VecInit(refillMshrs.map(_.io.replReadWanted)).asUInt
  ownership.allocated := replaceAllocValid
  ownership.slot := OHToUInt(selectedReplaceSlotOH)
  ownership.live := VecInit(replaceMshrs.map(_.io.info.valid)).asUInt
  ownership.captured := VecInit(replaceMshrs.map(_.io.info.bits.releaseBufReady)).asUInt
}

// Real controller, contexts, SourceB and ReleaseBuf. This boundary test starts
// the lower-response wait bits ready at allocation; it does not model CHI or
// force internal state. The original full-system regression remains required.
class TimingOwnershipCtlHarness(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle {
    val allocate = Input(Bool())
    val cmo = Input(Bool())
    val accept = Input(Bool())
    val response = Input(Bool())
    val orphan = Input(Bool())
    val parent = Input(UInt(mshrBits.W))
    val nested = Input(Bool())
    val nestedTag = Input(UInt(tagBits.W))
    val payload = Input(UInt(blockBits.W))
    val readId = Input(UInt(mshrBits.W))
    val ptr = Output(UInt(mshrBits.W))
    val task = Output(Bool())
    val qos = Output(UInt(3.W))
    val normalProbe = Output(UInt(mshrsAll.W))
    val reserve = Output(UInt(mshrsAll.W))
    val wanted = Output(UInt(mshrsAll.W))
    val allocated = Output(Bool())
    val slot = Output(UInt(mshrBits.W))
    val live = Output(UInt(mshrsAll.W))
    val captured = Output(UInt(mshrsAll.W))
    val write = Output(Bool())
    val writeId = Output(UInt(mshrBits.W))
    val extra = Output(Bool())
    val buffered = Output(UInt(blockBits.W))
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val c = Module(new ObservableTimingOwnershipCtl)
  val b = Module(new MSHRBuffer(2))
  zeroInputs(c.io); zeroInputs(b.io)
  val a = c.io.fromMainPipe.mshr_alloc_s3
  a.valid := io.allocate
  a.bits.state.elements.foreach(_._2 := true.B)
  a.bits.state.s_refill := io.cmo
  a.bits.state.w_replResp := io.cmo
  a.bits.state.s_release := !io.cmo
  a.bits.state.w_releaseack := !io.cmo
  a.bits.state.s_acquire := !io.cmo
  a.bits.state.s_cmoresp := !io.cmo
  a.bits.task.channel := 1.U
  a.bits.task.opcode := Mux(io.cmo, CBOClean, AcquirePerm)
  a.bits.task.param := NtoB
  a.bits.task.cmoTask := io.cmo
  a.bits.task.set := 2.U
  a.bits.task.tag := 3.U
  a.bits.dirResult.hit := io.cmo
  a.bits.dirResult.set := 2.U
  a.bits.dirResult.tag := 3.U
  a.bits.dirResult.way := 1.U
  a.bits.dirResult.meta.state := Mux(io.cmo, MetaData.TIP, MetaData.INVALID)
  a.bits.dirResult.meta.dirty := io.cmo
  a.bits.dirResult.meta.clients := 1.U
  c.io.mshrTask.ready := io.accept
  c.io.fromMainPipe.orphanReleaseData := io.orphan
  c.io.replResp.valid := io.response
  c.io.replResp.bits.mshrId := io.parent
  c.io.replResp.bits.set := 2.U
  c.io.replResp.bits.tag := 9.U
  c.io.replResp.bits.way := 1.U
  c.io.replResp.bits.meta.state := MetaData.TIP
  c.io.replResp.bits.meta.clients := 1.U
  c.io.nestedwb.c_set_dirty := io.nested
  c.io.nestedwb.set := 2.U
  c.io.nestedwb.tag := io.nestedTag
  c.io.nestedwbPayload.data := io.payload
  c.io.nestedReleaseWrites.zip(b.io.w).foreach { case (write, w) =>
    w := write
  }
  b.io.r.valid := true.B
  b.io.r.bits.id := io.readId
  io.buffered := b.io.resp.data.data
  io.ptr := c.io.toMainPipe.mshr_alloc_ptr
  io.task := c.io.mshrTask.valid
  io.qos := c.io.mshrTask.bits.qosClass
  io.normalProbe := c.ownership.normalProbe
  io.reserve := c.ownership.reserve
  io.wanted := c.ownership.wanted
  io.allocated := c.ownership.allocated
  io.slot := c.ownership.slot
  io.live := c.ownership.live
  io.captured := c.ownership.captured
  io.write := c.io.nestedReleaseWrites(0).valid
  io.writeId := c.io.nestedReleaseWrites(0).bits.id
  io.extra := c.io.nestedReleaseWrites(1).valid
}

object TestTimingOwnershipBoundaries extends App with ChiselSim {
  import PairMiterParameters._
  simulate(new SourceB) { dut =>
    dut.io.sourceB.ready.poke(false.B)
    dut.io.task.valid.poke(false.B)
    dut.io.task.bits.off.poke(0.U)
    dut.io.task.bits.opcode.poke(Probe)
    dut.io.task.bits.param.poke(toN)
    dut.io.task.bits.set.poke(0.U)
    dut.io.task.bits.tag.poke(0.U)
    dut.io.task.bits.alias.foreach(_.poke(0.U))
    dut.io.grantStatus.foreach { g =>
      g.valid.poke(false.B); g.set.poke(0.U); g.tag.poke(0.U)
    }
    dut.clock.step(2)
    val setBits = dut.io.task.bits.set.getWidth
    val firstWait = dut.io.grantStatus.size - 1
    val secondWait = dut.io.grantStatus.size - 2
    require(secondWait >= 0)
    val expected = scala.collection.mutable.Map.empty[BigInt, (Int, Int)]
    def address(tag: Int, set: Int): BigInt = ((BigInt(tag) << setBits) | set) << 6
    def push(tag: Int, set: Int, param: Int, alias: Int): Unit = {
      dut.io.task.bits.tag.poke(tag.U); dut.io.task.bits.set.poke(set.U)
      dut.io.task.bits.param.poke(param.U); dut.io.task.bits.alias.foreach(_.poke(alias.U))
      dut.io.task.valid.poke(true.B)
      dut.io.task.ready.expect(true.B)
      expected(address(tag, set)) = (param, if (dut.io.task.bits.alias.isDefined) alias else 0)
      dut.clock.step()
      dut.io.task.valid.poke(false.B)
    }
    push(3, 2, 2, 1)
    // A Grant becomes visible after SourceB captures A but before A's address
    // qualification. The accepted payload must wait for that actual GrantAck.
    dut.io.grantStatus(firstWait).valid.poke(true.B)
    dut.io.grantStatus(firstWait).set.poke(2.U); dut.io.grantStatus(firstWait).tag.poke(3.U)
    dut.io.sourceB.valid.expect(false.B)
    dut.clock.step(3)
    dut.io.sourceB.valid.expect(false.B)
    push(5, 1, 1, 2); push(7, 3, 2, 3); push(11, 4, 1, 0)
    dut.io.task.ready.expect(false.B)
    dut.clock.step(3)
    dut.io.sourceB.ready.poke(true.B)
    val seen = scala.collection.mutable.Set.empty[BigInt]
    def sample(): Unit = {
      if (dut.io.sourceB.valid.peek().litToBoolean) {
        val addr = dut.io.sourceB.bits.address.peek().litValue
        assert(expected.contains(addr) && !seen(addr), s"unexpected/duplicate B address $addr")
        assert(addr != address(3, 2) || !dut.io.grantStatus(firstWait).valid.peek().litToBoolean,
          "conflicting accepted task issued before GrantAck")
        val (param, alias) = expected(addr)
        dut.io.sourceB.bits.opcode.expect(Probe)
        dut.io.sourceB.bits.param.expect(param.U)
        dut.io.sourceB.bits.data.expect((alias << 1).U)
        seen += addr
      }
      dut.clock.step()
    }
    for (_ <- 0 until 24) sample()
    assert(seen.size == 3 && !seen(address(3, 2)))
    dut.io.grantStatus(firstWait).valid.poke(false.B)
    for (_ <- 0 until 12) sample()
    assert(seen.size == 4)
    dut.io.sourceB.valid.expect(false.B)
    dut.io.task.ready.expect(true.B)
    // Grant present at acceptance but gone before checking must not leave
    // a stale waitG dependency in a recycled physical Probe slot.
    dut.io.sourceB.ready.poke(false.B)
    dut.io.grantStatus(secondWait).valid.poke(true.B)
    dut.io.grantStatus(secondWait).set.poke(5.U); dut.io.grantStatus(secondWait).tag.poke(13.U)
    push(13, 5, 2, 2)
    dut.io.grantStatus(secondWait).valid.poke(false.B)
    dut.clock.step(3)
    dut.io.sourceB.ready.poke(true.B)
    for (_ <- 0 until 8) sample()
    assert(seen.size == 5)
    println("PASS: real SourceB capture/Grant insert+invalidate races, four-slot backpressure, full payload and unique B issue")
  }
  for (cmo <- Seq(false, true)) simulate(new TimingOwnershipCtlHarness) { dut =>
    dut.io.allocate.poke(false.B); dut.io.cmo.poke(cmo.B)
    dut.io.accept.poke(false.B); dut.io.response.poke(false.B)
    dut.io.orphan.poke(false.B)
    dut.io.parent.poke(0.U); dut.io.nested.poke(false.B)
    dut.io.nestedTag.poke((if (cmo) 3 else 9).U)
    dut.io.readId.poke(0.U)
    val payload = BigInt("13579bdf02468ace", 16) | (BigInt(1) << 479)
    dut.io.payload.poke(payload.U)
    dut.clock.step(2)
    val parent = dut.io.ptr.peek().litValue
    dut.io.parent.poke(parent.U)
    dut.io.allocate.poke(true.B); dut.clock.step(); dut.io.allocate.poke(false.B)
    val slot = if (cmo) {
      // The child does not exist yet: CMO admission and nested data must name
      // exactly the same new physical owner at this edge.
      dut.io.nested.poke(true.B)
      dut.io.allocated.expect(true.B)
      dut.io.slot.peek().litValue
    } else {
      dut.clock.step(2)
      assert(dut.io.reserve.peek().litValue.testBit(parent.toInt))
      dut.io.task.expect(true.B); dut.io.qos.expect(1.U)
      dut.io.accept.poke(true.B); dut.clock.step(); dut.io.accept.poke(false.B)
      dut.clock.step(2)
      dut.io.response.poke(true.B)
      dut.io.allocated.expect(true.B)
      val selected = dut.io.slot.peek().litValue
      dut.io.normalProbe.expect(0.U)
      dut.clock.step(); dut.io.response.poke(false.B)
      dut.io.nested.poke(true.B)
      selected
    }
    dut.io.normalProbe.expect(0.U)
    dut.io.write.expect(true.B); dut.io.extra.expect(false.B)
    dut.io.writeId.expect(slot.U)
    dut.io.readId.poke(slot.U)
    dut.clock.step(); dut.io.nested.poke(false.B)
    dut.clock.step(2)
    assert(dut.io.live.peek().litValue.testBit(slot.toInt))
    assert(dut.io.captured.peek().litValue.testBit(slot.toInt))
    dut.io.buffered.expect(payload.U)
    dut.io.normalProbe.expect(0.U)
    println(s"PASS: real MSHRCtl/ReleaseBuf ${if(cmo) "first-cycle CMO admission" else "reserved victim + active nested"} target/capture/payload, parent Probe suppression")
  }
  simulate(new TimingOwnershipCtlHarness) { dut =>
    dut.io.allocate.poke(false.B); dut.io.cmo.poke(false.B)
    dut.io.accept.poke(false.B); dut.io.response.poke(false.B)
    dut.io.orphan.poke(false.B); dut.io.parent.poke(0.U)
    dut.io.nested.poke(false.B); dut.io.nestedTag.poke(17.U)
    dut.io.readId.poke(0.U)
    val payload = (BigInt(1) << 511) | BigInt("face12345678", 16)
    dut.io.payload.poke(payload.U)
    dut.clock.step(2)
    dut.io.orphan.poke(true.B); dut.io.nested.poke(true.B)
    dut.io.allocated.expect(true.B); dut.io.write.expect(true.B)
    val slot = dut.io.slot.peek().litValue
    dut.io.writeId.expect(slot.U); dut.io.readId.poke(slot.U)
    dut.clock.step(); dut.io.orphan.poke(false.B); dut.io.nested.poke(false.B)
    dut.clock.step(2)
    assert(dut.io.live.peek().litValue.testBit(slot.toInt))
    assert(dut.io.captured.peek().litValue.testBit(slot.toInt))
    dut.io.buffered.expect(payload.U)
    println("PASS: real MSHRCtl/ReleaseBuf orphan admission owns the same physical payload slot")
  }
}


class ProgressCreditTestCtl(implicit p: Parameters) extends MSHRCtl {
  val creditState = IO(Output(new Bundle {
    val busy = UInt(mshrsAll.W)
    val pending = UInt(mshrsAll.W)
    val active = UInt(mshrsAll.W)
  }))
  creditState.busy := srBusySlotOH
  creditState.pending := srPendingSlotOH
  creditState.active := srActiveSlotOH
}

// Exercise physical reservations through the real controller interfaces. Lower
// completions stay blocked, so ordinary owners cannot make space for B or C.
class ProgressCreditTestHarness(implicit p: Parameters) extends L2Module with xscache.chi.HasCHIOpcodes {
  val io = IO(new Bundle {
    val requestA = Input(Bool())
    val consumeA = Input(Bool())
    val applyA = Input(Bool())
    val set = Input(UInt(setBits.W))
    val tag = Input(UInt(tagBits.W))
    val requestB = Input(Bool())
    val consumeB = Input(Bool())
    val finishB = Input(Bool())
    val requestC = Input(Bool())
    val applyC = Input(Bool())
    val aReady = Output(Bool())
    val aAck = Output(Bool())
    val bReady = Output(Bool())
    val bReserved = Output(Bool())
    val bSlot = Output(UInt(mshrBits.W))
    val cReserved = Output(Bool())
    val cSlot = Output(UInt(mshrBits.W))
    val busy = Output(UInt(mshrsAll.W))
    val pending = Output(UInt(mshrsAll.W))
    val active = Output(UInt(mshrsAll.W))
    val blockC = Output(Bool())
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val c = Module(new ProgressCreditTestCtl)
  zeroInputs(c.io)
  c.io.normalReserveRequest.valid := io.requestA
  c.io.normalReserveRequest.bits.channel := 1.U
  c.io.normalReserveRequest.bits.opcode := CBOClean
  c.io.normalReserveRequest.bits.cmoTask := true.B
  c.io.normalReserveRequest.bits.set := io.set
  c.io.normalReserveRequest.bits.tag := io.tag
  c.io.normalReserveConsume.valid := io.consumeA
  c.io.normalReserveConsume.bits := c.io.normalReserveAck.bits
  val appliedA = RegEnable(c.io.normalReserveAck.bits, io.consumeA)
  val allocation = c.io.fromMainPipe.mshr_alloc_s3
  allocation.valid := io.applyA
  allocation.bits.task := appliedA.task
  allocation.bits.task.mshrId := appliedA.id
  allocation.bits.state.elements.foreach(_._2 := true.B)
  allocation.bits.dirResult.set := appliedA.task.set
  allocation.bits.dirResult.tag := appliedA.task.tag
  c.io.fromMainPipe.normalReservationDone.valid := io.applyA
  c.io.fromMainPipe.normalReservationDone.bits.mshrContext := 2.U
  c.io.fromMainPipe.normalReservationDone.bits.mshrId := appliedA.id
  c.io.fromMainPipe.normalReservationDone.bits.token := appliedA.token

  c.io.snoopAdmission.valid := io.requestB
  c.io.snoopAdmission.bits.channel := 2.U
  c.io.snoopAdmission.bits.set := 30.U
  c.io.snoopAdmission.bits.tag := 50.U
  c.io.snoopAdmission.bits.chiOpcode.get := SnpShared
  val appliedB = RegEnable(c.io.snoopReservationToken, io.consumeB)
  c.io.bServiceConsume.valid := io.consumeB
  c.io.bServiceConsume.bits := c.io.snoopReservationToken
  c.io.fromMainPipe.snoopReservationDone.valid := io.finishB
  c.io.fromMainPipe.snoopReservationDone.bits := appliedB

  c.io.fromMainPipe.orphanReleaseReserveRequest.valid := io.requestC
  c.io.fromMainPipe.orphanReleaseReserveRequest.bits.set := 31.U
  c.io.fromMainPipe.orphanReleaseReserveRequest.bits.tag := 51.U
  c.io.fromMainPipe.orphanReleaseReserveRequest.bits.sequence := 1.U
  c.io.fromMainPipe.orphanReleaseReserveDone.valid := io.applyC
  c.io.fromMainPipe.orphanReleaseReserveDone.bits := c.io.toMainPipe.orphanReleaseReservation
  c.io.fromMainPipe.orphanReleaseData := io.applyC
  c.io.nestedwb.c_set_dirty := io.applyC
  c.io.nestedwb.set := 31.U
  c.io.nestedwb.tag := 51.U
  c.io.nestedwbPayload.data := "h123456789abcdef".U
  c.io.pipeStatusVec(2).valid := io.applyC
  c.io.pipeStatusVec(2).bits.channel := 4.U
  c.io.fromReqArb.status_s1.cReleaseData := true.B

  io.aReady := c.io.normalReserveRequest.ready
  io.aAck := c.io.normalReserveAck.valid
  io.bReady := c.io.snoopAdmission.ready
  io.bReserved := c.io.snoopReservationToken.valid
  io.bSlot := c.io.snoopReservationToken.slot
  io.cReserved := c.io.toMainPipe.orphanReleaseReservation.valid
  io.cSlot := c.io.toMainPipe.orphanReleaseReservation.slot
  io.busy := c.creditState.busy
  io.pending := c.creditState.pending
  io.active := c.creditState.active
  io.blockC := c.io.toReqArb.blockC_s1
}

object TestSnoopAndReleaseProgress extends App with ChiselSim {
  import PairMiterParameters._
  simulate(new ProgressCreditTestHarness) { d =>
    def idle(): Unit = {
      d.io.requestA.poke(false.B); d.io.consumeA.poke(false.B); d.io.applyA.poke(false.B)
      d.io.requestB.poke(false.B); d.io.consumeB.poke(false.B); d.io.finishB.poke(false.B)
      d.io.requestC.poke(false.B); d.io.applyC.poke(false.B)
      d.io.set.poke(0.U); d.io.tag.poke(0.U)
    }
    def waitFor(value: => Boolean): Unit = {
      var cycles = 0
      while (!value && cycles < 12) { d.clock.step(); cycles += 1 }
      assert(value, "real controller failed to provide the reserved progress credit")
    }
    for (bFirst <- Seq(true, false)) {
      idle(); d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B); d.clock.step(2)
      for (i <- 0 until 2) {
        d.io.set.poke((i + 1).U); d.io.tag.poke((i + 1).U)
        d.io.requestA.poke(true.B); waitFor(d.io.aReady.peek().litToBoolean)
        d.clock.step(); d.io.requestA.poke(false.B); d.io.aAck.expect(true.B)
        d.io.consumeA.poke(true.B); d.clock.step(); d.io.consumeA.poke(false.B)
        if (i == 0) { d.io.applyA.poke(true.B); d.clock.step(); d.io.applyA.poke(false.B) }
      }
      // One ordinary owner is active, the other is still an unfilled claim.
      d.io.busy.expect(3.U); d.io.active.expect(1.U); d.io.pending.expect(2.U)
      d.io.set.poke(4.U); d.io.tag.poke(4.U); d.io.requestA.poke(true.B)
      d.io.aReady.expect(false.B); d.clock.step(3); d.io.aReady.expect(false.B)
      d.io.requestA.poke(false.B)
      def reserveB(): Unit = {
        d.io.requestB.poke(true.B); waitFor(d.io.bReady.peek().litToBoolean)
        d.clock.step(); d.io.requestB.poke(false.B)
        d.io.bReserved.expect(true.B); d.io.bSlot.expect(3.U)
      }
      def reserveC(): Unit = {
        d.io.requestC.poke(true.B); waitFor(d.io.cReserved.peek().litToBoolean)
        d.io.requestC.poke(false.B); d.io.cSlot.expect(2.U)
      }
      if (bFirst) { reserveB(); reserveC() } else { reserveC(); reserveB() }
      d.io.busy.expect(15.U); d.io.pending.expect(14.U)
      d.io.bSlot.expect(3.U); d.io.cSlot.expect(2.U); d.io.blockC.expect(true.B)
      d.io.consumeB.poke(true.B); d.clock.step(); d.io.consumeB.poke(false.B)
      d.io.applyA.poke(true.B); d.clock.step(); d.io.applyA.poke(false.B)
      d.io.applyC.poke(true.B); d.clock.step(); d.io.applyC.poke(false.B)
      d.io.busy.expect(15.U); d.io.active.expect(7.U); d.io.pending.expect(8.U)
      d.io.finishB.poke(true.B); d.clock.step(); d.io.finishB.poke(false.B)
      d.io.busy.expect(7.U); d.io.pending.expect(0.U)
      // Only B's progress slot is free: C stays outside the shared pipeline,
      // while another B can reserve and return the same physical slot.
      d.io.blockC.expect(true.B); reserveB()
      d.io.consumeB.poke(true.B); d.clock.step(); d.io.consumeB.poke(false.B)
      d.io.finishB.poke(true.B); d.clock.step(); d.io.finishB.poke(false.B)
      d.io.busy.expect(7.U); d.io.pending.expect(0.U)
      println(s"PASS: real MSHRCtl B/C progress with saturated active+pending ordinary owners, B first=$bFirst")
    }
  }
}


class ObservableReadFenceCtl(implicit p: Parameters) extends MSHRCtl {
  // Test-only bypass of the new interface bit reproduces pre-fix behavior;
  // ownership, reservations, state updates and packet generation stay real.
  val unfenced = IO(Input(Bool()))
  when(unfenced) { refillMshrs.foreach(_.io.readLineBlocked := false.B) }
  val readFence = IO(Output(new Bundle {
    val offered = UInt(mshrsAll.W)
    val blocked = UInt(mshrsAll.W)
    val victims = UInt(mshrsAll.W)
    val acknowledged = UInt(mshrsAll.W)
    val copyback = UInt(mshrsAll.W)
    val victimGenerations = Vec(mshrsAll, UInt(8.W))
  }))
  readFence.offered := VecInit(refillMshrs.map(_.io.tasks.txreq.valid)).asUInt
  readFence.blocked := VecInit(refillMshrs.map(_.io.readLineBlocked)).asUInt
  readFence.victims := VecInit(replaceMshrs.map(_.io.info.valid)).asUInt
  readFence.acknowledged := VecInit(replaceMshrs.map(m =>
    m.io.info.valid && m.io.info.bits.releaseAck)).asUInt
  readFence.copyback := VecInit(replaceMshrs.map(m =>
    m.io.info.valid && m.io.info.bits.copyBackIssued)).asUInt
  readFence.victimGenerations := VecInit(mshrTaskGenerations(2))
}

// All ownership changes use real controller reservations and transport ports.
// MainPipe is represented only by its documented reserve/consume/apply edges.
class DirtyVictimReadFenceHarness(implicit p: Parameters)
  extends L2Module with xscache.chi.HasCHIOpcodes {
  val io = IO(new Bundle {
    val unfenced = Input(Bool())
    val requestA = Input(Bool()); val consumeA = Input(Bool()); val applyA = Input(Bool())
    val aSet = Input(UInt(setBits.W)); val aTag = Input(UInt(tagBits.W))
    val requestC = Input(Bool()); val applyC = Input(Bool())
    val cSet = Input(UInt(setBits.W)); val cTag = Input(UInt(tagBits.W))
    val lowerReady = Input(Bool()); val dataReady = Input(Bool()); val dataDone = Input(Bool())
    val reply = Input(Bool()); val replySlot = Input(UInt(mshrBits.W))
    val replyTxn = Input(UInt(12.W))
    val aReady = Output(Bool()); val aAck = Output(Bool()); val aSlot = Output(UInt(mshrBits.W))
    val cAck = Output(Bool()); val cSlot = Output(UInt(mshrBits.W))
    val offered = Output(UInt(mshrsAll.W)); val blocked = Output(UInt(mshrsAll.W))
    val victims = Output(UInt(mshrsAll.W)); val acknowledged = Output(UInt(mshrsAll.W))
    val copyback = Output(UInt(mshrsAll.W)); val dataValid = Output(Bool())
    val reqValid = Output(Bool()); val reqAddr = Output(UInt(fullAddressBits.W))
    val reqTxn = Output(UInt(12.W)); val wbCount = Output(UInt(8.W))
    val wbTxn = Output(UInt(12.W))
    val victimGenerations = Output(Vec(mshrsAll, UInt(8.W)))
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val c = Module(new ObservableReadFenceCtl)
  zeroInputs(c.io)
  c.unfenced := io.unfenced
  val request = c.io.normalReserveRequest
  request.valid := io.requestA
  request.bits.channel := 1.U
  request.bits.opcode := Get
  request.bits.param := NtoB
  request.bits.set := io.aSet
  request.bits.tag := io.aTag
  request.bits.sourceId := 1.U
  request.bits.size := log2Ceil(blockBytes).U
  c.io.normalReserveConsume.valid := io.consumeA
  c.io.normalReserveConsume.bits := c.io.normalReserveAck.bits
  val acceptedA = RegEnable(c.io.normalReserveAck.bits, io.consumeA)
  val alloc = c.io.fromMainPipe.mshr_alloc_s3
  alloc.valid := io.applyA
  alloc.bits.task := acceptedA.task
  alloc.bits.task.mshrId := acceptedA.id
  alloc.bits.state.elements.foreach(_._2 := true.B)
  alloc.bits.state.s_acquire := false.B
  alloc.bits.state.s_refill := false.B
  alloc.bits.state.w_grant := false.B
  alloc.bits.state.w_grantfirst := false.B
  alloc.bits.state.w_grantlast := false.B
  alloc.bits.dirResult.set := acceptedA.task.set
  alloc.bits.dirResult.tag := acceptedA.task.tag
  alloc.bits.dirResult.meta.state := MetaData.INVALID
  c.io.fromMainPipe.normalReservationDone.valid := io.applyA
  c.io.fromMainPipe.normalReservationDone.bits.mshrId := acceptedA.id
  c.io.fromMainPipe.normalReservationDone.bits.mshrContext := 0.U
  c.io.fromMainPipe.normalReservationDone.bits.token := acceptedA.token

  val orphan = c.io.fromMainPipe.orphanReleaseReserveRequest
  orphan.valid := io.requestC
  orphan.bits.set := io.cSet
  orphan.bits.tag := io.cTag
  orphan.bits.sequence := 1.U
  c.io.fromMainPipe.orphanReleaseReserveDone.valid := io.applyC
  c.io.fromMainPipe.orphanReleaseReserveDone.bits := c.io.toMainPipe.orphanReleaseReservation
  c.io.fromMainPipe.orphanReleaseData := io.applyC
  c.io.nestedwb.c_set_dirty := io.applyC
  c.io.nestedwb.set := io.cSet
  c.io.nestedwb.tag := io.cTag
  c.io.nestedwbPayload.data := (BigInt(0x02ad) << 64).U
  c.io.pipeStatusVec(2).valid := io.applyC
  c.io.pipeStatusVec(2).bits.channel := 4.U
  c.io.fromReqArb.status_s1.cReleaseData := true.B
  c.io.toTXREQ.ready := io.lowerReady
  c.io.txdatDirectSpace := true.B
  c.io.toTXDAT.ready := io.dataReady
  c.io.releaseBufDirectResp.data := (BigInt(0x02ad) << 64).U
  val completedData = RegEnable(c.io.toTXDAT.bits.task, c.io.toTXDAT.fire)
  c.io.txdatDone.valid := io.dataDone
  c.io.txdatDone.bits := completedData
  c.io.resps.rxrsp.valid := io.reply
  c.io.resps.rxrsp.mshrId := io.replySlot + mshrsAll.U
  c.io.resps.rxrsp.txnID := io.replyTxn
  c.io.resps.rxrsp.respInfo.chiOpcode.get := CompDBIDResp
  c.io.resps.rxrsp.respInfo.dbID.get := 17.U
  c.io.resps.rxrsp.respInfo.srcID.get := 1.U
  val wbCount = RegInit(0.U(8.W))
  val wbTxn = RegInit(0.U(12.W))
  when(c.io.toTXREQ.fire && c.io.toTXREQ.bits.opcode === WriteBackFull) {
    wbCount := wbCount + 1.U
    wbTxn := c.io.toTXREQ.bits.txnID
  }
  io.aReady := request.ready
  io.aAck := c.io.normalReserveAck.valid
  io.aSlot := acceptedA.id
  io.cAck := c.io.toMainPipe.orphanReleaseReservation.valid
  io.cSlot := c.io.toMainPipe.orphanReleaseReservation.slot
  io.offered := c.readFence.offered
  io.blocked := c.readFence.blocked
  io.victims := c.readFence.victims
  io.acknowledged := c.readFence.acknowledged
  io.copyback := c.readFence.copyback
  io.dataValid := c.io.toTXDAT.valid
  io.reqValid := c.io.toTXREQ.valid
  io.reqAddr := c.io.toTXREQ.bits.addr
  io.reqTxn := c.io.toTXREQ.bits.txnID
  io.wbCount := wbCount
  io.wbTxn := wbTxn
  io.victimGenerations := c.readFence.victimGenerations
}

object TestDirtyVictimReadFence extends App with ChiselSim {
  import PairMiterParameters._
  simulate(new DirtyVictimReadFenceHarness) { d =>
    def clear(): Unit = {
      d.io.unfenced.poke(false.B)
      d.io.requestA.poke(false.B); d.io.consumeA.poke(false.B); d.io.applyA.poke(false.B)
      d.io.requestC.poke(false.B); d.io.applyC.poke(false.B)
      d.io.aSet.poke(2.U); d.io.aTag.poke(3.U)
      d.io.cSet.poke(2.U); d.io.cTag.poke(3.U)
      d.io.lowerReady.poke(false.B); d.io.dataReady.poke(false.B); d.io.dataDone.poke(false.B)
      d.io.reply.poke(false.B); d.io.replySlot.poke(0.U); d.io.replyTxn.poke(0.U)
      d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B); d.clock.step(2)
    }
    def waitFor(test: => Boolean, what: String, limit: Int = 80): Unit = {
      var left = limit
      while(!test && left > 0) { d.clock.step(); left -= 1 }
      assert(test, s"timeout: $what")
    }
    def reserveA(): BigInt = {
      d.io.requestA.poke(true.B)
      waitFor(d.io.aReady.peek().litToBoolean, "A physical-slot reservation")
      d.clock.step(); d.io.requestA.poke(false.B); d.io.aAck.expect(true.B)
      d.io.consumeA.poke(true.B); d.clock.step(); d.io.consumeA.poke(false.B)
      d.io.aSlot.peek().litValue
    }
    def reserveC(): BigInt = {
      d.io.requestC.poke(true.B)
      waitFor(d.io.cAck.peek().litToBoolean, "dirty-owner address reservation")
      d.io.requestC.poke(false.B)
      d.io.cSlot.peek().litValue
    }
    def noRead(slot: BigInt, cycles: Int): Unit = {
      for (_ <- 0 until cycles) {
        assert(!d.io.offered.peek().litValue.testBit(slot.toInt), "same-line lower read escaped dirty owner")
        d.clock.step()
      }
    }
    // Paired control: the actual controller still owns the dirty victim,
    // yet the unfenced MSHR immediately offers the conflicting lower read.
    clear(); d.io.unfenced.poke(true.B)
    val baselineA = reserveA(); val baselineVictim = reserveC()
    d.io.applyC.poke(true.B); d.clock.step(); d.io.applyC.poke(false.B)
    d.io.applyA.poke(true.B); d.clock.step(); d.io.applyA.poke(false.B)
    assert(d.io.victims.peek().litValue.testBit(baselineVictim.toInt))
    waitFor(d.io.offered.peek().litValue.testBit(baselineA.toInt), "unfenced premature same-line read")
    assert(d.io.victims.peek().litValue.testBit(baselineVictim.toInt))
    println("PASS control: disabling the new fence reproduces a same-line read while the dirty owner is live")

    for (simultaneous <- Seq(false, true)) {
      clear()
      val a = reserveA()
      val victim = reserveC()
      d.io.applyC.poke(true.B)
      d.io.applyA.poke(simultaneous.B)
      d.clock.step(); d.io.applyC.poke(false.B)
      if (!simultaneous) { d.io.applyA.poke(true.B); d.clock.step() }
      d.io.applyA.poke(false.B)
      assert(d.io.victims.peek().litValue.testBit(victim.toInt))
      noRead(a, 4)
      assert(d.io.blocked.peek().litValue.testBit(a.toInt))
      d.io.lowerReady.poke(true.B)
      waitFor(d.io.wbCount.peek().litValue == 1, "dirty WriteBackFull acceptance")
      d.io.replySlot.poke(victim.U); d.io.replyTxn.poke(d.io.wbTxn.peek().litValue.U)
      d.io.reply.poke(true.B); d.clock.step(); d.io.reply.poke(false.B)
      assert(d.io.acknowledged.peek().litValue.testBit(victim.toInt))
      noRead(a, 6)
      waitFor(d.io.dataValid.peek().litToBoolean, "dirty copyback offer")
      d.io.dataReady.poke(true.B); d.clock.step(); d.io.dataReady.poke(false.B)
      assert(d.io.copyback.peek().litValue.testBit(victim.toInt))
      noRead(a, 4)
      d.io.lowerReady.poke(false.B)
      d.io.dataDone.poke(true.B); d.clock.step(); d.io.dataDone.poke(false.B)
      waitFor(!d.io.victims.peek().litValue.testBit(victim.toInt), "actual dirty-owner retirement")
      waitFor(d.io.offered.peek().litValue.testBit(a.toInt), "same-line read after dirty data completes")
      println(s"PASS: committed A claim preserved; same-line read waits for copyback completion, simultaneous=$simultaneous")
    }

    // Exercise legal ownership transitions independently: a live same-line
    // normal receives C data in RefillBuf, so orphan Replace reuse must be
    // allocated before or simultaneously with the next normal context.
    for (allocateOnRetirement <- Seq(true, false)) {
      clear()
      val nextA = reserveA()
      val oldVictim = reserveC()
      d.io.applyC.poke(true.B); d.clock.step(); d.io.applyC.poke(false.B)
      val oldGeneration = d.io.victimGenerations(oldVictim.toInt).peek().litValue
      d.io.lowerReady.poke(true.B)
      waitFor(d.io.wbCount.peek().litValue == 1, "old-generation WriteBackFull")
      d.io.replySlot.poke(oldVictim.U); d.io.replyTxn.poke(d.io.wbTxn.peek().litValue.U)
      d.io.reply.poke(true.B); d.clock.step(); d.io.reply.poke(false.B)
      waitFor(d.io.dataValid.peek().litToBoolean, "old-generation copyback")
      d.io.dataReady.poke(true.B); d.clock.step(); d.io.dataReady.poke(false.B)
      d.io.lowerReady.poke(false.B)
      d.io.dataDone.poke(true.B); d.clock.step(); d.io.dataDone.poke(false.B)
      assert(d.io.victims.peek().litValue.testBit(oldVictim.toInt))
      d.clock.step()
      assert(d.io.victims.peek().litValue.testBit(oldVictim.toInt))
      d.io.applyA.poke(allocateOnRetirement.B)
      d.clock.step(); d.io.applyA.poke(false.B)
      assert(!d.io.victims.peek().litValue.testBit(oldVictim.toInt),
        "the old owner must retire on the tested edge")
      if (allocateOnRetirement) {
        assert(d.io.blocked.peek().litValue.testBit(nextA.toInt))
        noRead(nextA, 1)
        waitFor(d.io.offered.peek().litValue.testBit(nextA.toInt),
          "retirement-edge allocation releases old-generation conflict")
        assert(!d.io.blocked.peek().litValue.testBit(nextA.toInt))
        println("PASS: A allocation on the actual dirty-owner retirement edge blocks initially and then clears")
      } else {
        // The first cycle after retirement can claim the original physical
        // Replace slot. Fill that claim and A on one edge, with no live-normal
        // C routing ambiguity and no cycle in which a read can escape.
        d.io.requestC.poke(true.B)
        noRead(nextA, 1)
        d.io.cAck.expect(true.B)
        d.io.cSlot.expect(oldVictim.U)
        d.io.requestC.poke(false.B)
        d.io.applyC.poke(true.B); d.io.applyA.poke(true.B)
        noRead(nextA, 1)
        d.io.applyC.poke(false.B); d.io.applyA.poke(false.B)
        assert(d.io.victims.peek().litValue.testBit(oldVictim.toInt))
        d.io.victimGenerations(oldVictim.toInt).expect(((oldGeneration + 1) & 255).U)
        assert(d.io.blocked.peek().litValue.testBit(nextA.toInt))
        noRead(nextA, 8)
        d.io.lowerReady.poke(true.B)
        waitFor(d.io.wbCount.peek().litValue == 2, "reused-generation WriteBackFull")
        d.io.replyTxn.poke(d.io.wbTxn.peek().litValue.U)
        d.io.reply.poke(true.B); d.clock.step(); d.io.reply.poke(false.B)
        noRead(nextA, 3)
        waitFor(d.io.dataValid.peek().litToBoolean, "reused-generation copyback")
        d.io.dataReady.poke(true.B); d.clock.step(); d.io.dataReady.poke(false.B)
        noRead(nextA, 3)
        d.io.lowerReady.poke(false.B)
        d.io.dataDone.poke(true.B); d.clock.step(); d.io.dataDone.poke(false.B)
        waitFor(!d.io.victims.peek().litValue.testBit(oldVictim.toInt), "reused-generation retirement")
        waitFor(d.io.offered.peek().litValue.testBit(nextA.toInt), "read after reused-generation data drain")
        assert(!d.io.blocked.peek().litValue.testBit(nextA.toInt))
        println("PASS: earliest same-slot next-generation reuse with simultaneous A allocation has no read-fence gap")
      }
    }

    // Both physical tag and set qualify the fence; same-set unrelated lines
    // and identical tags in another set remain eligible under the dirty owner.
    for ((set, tag) <- Seq((2, 4), (3, 3))) {
      clear(); val a = reserveA()
      d.io.cSet.poke(set.U); d.io.cTag.poke(tag.U)
      val victim = reserveC()
      d.io.applyC.poke(true.B); d.io.applyA.poke(true.B)
      d.clock.step(); d.io.applyC.poke(false.B); d.io.applyA.poke(false.B)
      assert(d.io.victims.peek().litValue.testBit(victim.toInt))
      waitFor(d.io.offered.peek().litValue.testBit(a.toInt), "unrelated physical line remains eligible")
      assert(!d.io.blocked.peek().litValue.testBit(a.toInt))
      println(s"PASS: dirty owner set=$set tag=$tag does not block another physical line")
    }

    // A later reservation cannot retract a request that already reached a
    // Decoupled transport while ready was low; its address and epoch stay fixed.
    clear(); val a = reserveA()
    d.io.applyA.poke(true.B); d.clock.step(); d.io.applyA.poke(false.B)
    waitFor(d.io.reqValid.peek().litToBoolean, "initial read offer")
    val addr = d.io.reqAddr.peek().litValue
    val txn = d.io.reqTxn.peek().litValue
    reserveC(); d.io.applyC.poke(true.B); d.clock.step(); d.io.applyC.poke(false.B)
    for (_ <- 0 until 8) {
      assert(d.io.offered.peek().litValue.testBit(a.toInt))
      d.io.reqValid.expect(true.B); d.io.reqAddr.expect(addr.U); d.io.reqTxn.expect(txn.U)
      d.clock.step()
    }
    println("PASS: an already-offered stalled read keeps valid, address and TxnID stable")
  }
}

// Real SinkC and RefillBuffer: exercise the two producers of its single write
// port without forcing internal state or assuming a controller receipt.
class ObservableSinkCRefillCollision(implicit p: Parameters) extends SinkC {
  val releaseWritePending = IO(Output(Bool()))
  releaseWritePending := releaseDataWritePending
}

class SinkCRefillCollisionHarness(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle {
    val cValid = Input(Bool())
    val cOpcode = Input(UInt(3.W))
    val cParam = Input(UInt(3.W))
    val cAddress = Input(UInt(48.W))
    val cData = Input(UInt((beatBytes * 8).W))
    val taskReady = Input(Bool())
    val routeReady = Input(Bool())
    val probeRefill = Input(Bool())
    val cReady = Output(Bool())
    val taskValid = Output(Bool())
    val probeDone = Output(Bool())
    val releasePending = Output(Bool())
    val writeValid = Output(Bool())
    val writeId = Output(UInt(mshrBits.W))
    val writeData = Output(UInt(blockBits.W))
    val applied = Output(Bool())
    val appliedId = Output(UInt(mshrBits.W))
    val readId = Input(UInt(mshrBits.W))
    val buffered = Output(UInt(blockBits.W))
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val c = Module(new ObservableSinkCRefillCollision)
  val b = Module(new MSHRBuffer(1))
  zeroInputs(c.io); zeroInputs(b.io)
  c.io.c.valid := io.cValid
  c.io.c.bits.opcode := io.cOpcode
  c.io.c.bits.param := io.cParam
  c.io.c.bits.address := io.cAddress
  c.io.c.bits.size := 6.U
  c.io.c.bits.data := io.cData
  c.io.task.ready := io.taskReady
  c.io.probeRouteReady := io.routeReady
  // Like the controller, qualify the write route with the actual last-beat
  // response. This must not feed back into SinkC's write-port ready decision.
  c.io.refillBufWriteId.valid := io.probeRefill && c.io.resp.valid && c.io.resp.respInfo.last
  c.io.refillBufWriteId.bits := 1.U
  c.io.msInfo(6).valid := true.B
  c.io.msInfo(6).bits.set := 7.U
  c.io.msInfo(6).bits.reqTag := 0.U
  c.io.msInfo(6).bits.s_refill := true.B
  c.io.msInfo(6).bits.w_grantlast := true.B
  b.io.w(0) := c.io.refillBufWrite
  b.io.r.valid := true.B
  b.io.r.bits.id := io.readId
  io.cReady := c.io.c.ready
  io.taskValid := c.io.task.valid
  io.probeDone := c.io.probeRouteDone
  io.releasePending := c.releaseWritePending
  io.writeValid := c.io.refillBufWrite.valid
  io.writeId := c.io.refillBufWrite.bits.id
  io.writeData := c.io.refillBufWrite.bits.data.data
  io.applied := RegNext(c.io.refillBufWrite.valid, false.B)
  io.appliedId := RegEnable(c.io.refillBufWrite.bits.id, c.io.refillBufWrite.valid)
  io.buffered := b.io.resp.data.data
}

object TestSinkCRefillWriteCollision extends App with ChiselSim {
  implicit val p: Parameters = PairMiterParameters.p.alterPartial {
    case L2ParamKey => PairMiterParameters.p(L2ParamKey).copy(mshrs = 16)
  }
  simulate(new SinkCRefillCollisionHarness) { d =>
    d.io.cValid.poke(false.B); d.io.cOpcode.poke(ReleaseData)
    d.io.cParam.poke(TtoN); d.io.cAddress.poke(0x1c0.U)
    d.io.cData.poke(0.U); d.io.taskReady.poke(false.B)
    d.io.routeReady.poke(true.B); d.io.probeRefill.poke(false.B)
    d.io.readId.poke(6.U)
    d.reset.poke(true.B); d.clock.step(2); d.reset.poke(false.B); d.clock.step()
    val writes = scala.collection.mutable.ArrayBuffer.empty[(BigInt, BigInt)]
    val applied = scala.collection.mutable.ArrayBuffer.empty[BigInt]
    def step(n: Int = 1): Unit = for (_ <- 0 until n) {
      if (d.io.writeValid.peek().litToBoolean) {
        writes += ((d.io.writeId.peek().litValue, d.io.writeData.peek().litValue))
      }
      if (d.io.applied.peek().litToBoolean) applied += d.io.appliedId.peek().litValue
      d.clock.step()
    }
    def block(low: BigInt, high: BigInt): BigInt = (high << 256) | low
    def receiveRelease(low: BigInt, high: BigInt): Unit = {
      d.io.taskReady.poke(false.B); d.io.probeRefill.poke(false.B)
      d.io.routeReady.poke(true.B)
      d.io.cValid.poke(true.B); d.io.cOpcode.poke(ReleaseData)
      d.io.cParam.poke(TtoN); d.io.cAddress.poke(0x1c0.U)
      d.io.cData.poke(low.U); d.io.cReady.expect(true.B); step()
      d.io.cData.poke(high.U); d.io.cReady.expect(true.B); step()
      d.io.cValid.poke(false.B)
      var waited = 0
      while (!d.io.taskValid.peek().litToBoolean && waited < 12) { step(); waited += 1 }
      assert(d.io.taskValid.peek().litToBoolean, "buffered ReleaseData task did not become ready")
      // A task that has not been accepted cannot create a pending write.
      for (_ <- 0 until 3) {
        d.io.releasePending.expect(false.B); d.io.writeValid.expect(false.B); step()
      }
    }
    def verifyBuffers(release: BigInt, probe: BigInt): Unit = {
      d.io.readId.poke(6.U); step(); d.io.buffered.expect(release.U)
      d.io.readId.poke(1.U); step(); d.io.buffered.expect(probe.U)
    }
    for (extraStall <- Seq(0, 3)) {
      writes.clear(); applied.clear()
      val r0 = BigInt("23456789abcdef01", 16) + extraStall
      val r1 = BigInt("f123456789abcdef", 16) + extraStall
      val p0 = BigInt("a5a55a5a12344321", 16) + extraStall
      val p1 = BigInt("5a5aa5a5fedccdef", 16) + extraStall
      receiveRelease(r0, r1)
      // Accept the ReleaseData task with the first ProbeAckData beat. Its
      // registered write then collides with the offered last probe beat.
      d.io.taskReady.poke(true.B)
      d.io.cValid.poke(true.B); d.io.cOpcode.poke(ProbeAckData)
      d.io.cParam.poke(TtoB); d.io.cAddress.poke(0x380.U)
      d.io.cData.poke(p0.U); d.io.cReady.expect(true.B); step()
      d.io.taskReady.poke(false.B); d.io.cData.poke(p1.U)
      d.io.probeRefill.poke(true.B)
      d.io.releasePending.expect(true.B)
      d.io.cReady.expect(false.B); d.io.probeDone.expect(false.B)
      d.io.writeValid.expect(true.B); d.io.writeId.expect(6.U)
      d.io.writeData.expect(block(r0, r1).U); step()
      // Route backpressure after the reserved write must not duplicate it or
      // accept the held last beat early; retry retains the exact probe data.
      d.io.routeReady.poke(false.B)
      for (_ <- 0 until extraStall) {
        d.io.releasePending.expect(false.B); d.io.cReady.expect(false.B)
        d.io.writeValid.expect(false.B); d.io.probeDone.expect(false.B); step()
      }
      d.io.routeReady.poke(true.B)
      d.io.cReady.expect(true.B); d.io.probeDone.expect(true.B)
      d.io.writeValid.expect(true.B); d.io.writeId.expect(1.U)
      d.io.writeData.expect(block(p0, p1).U); step()
      d.io.cValid.poke(false.B); d.io.probeRefill.poke(false.B); step(4)
      assert(writes.toSeq == Seq((BigInt(6), block(r0, r1)), (BigInt(1), block(p0, p1))),
        s"both write identities/payloads must apply exactly once: $writes")
      assert(applied.toSeq == Seq(BigInt(6), BigInt(1)),
        s"receipts must follow only actual writes, not a blocked probe: $applied")
      verifyBuffers(block(r0, r1), block(p0, p1))
      println(s"PASS: SinkC id6 ReleaseData and id1 ProbeAckData survive collision and $extraStall extra stalls")
    }
    writes.clear(); applied.clear()
    val r0 = BigInt("8899aabbccddeeff", 16)
    val r1 = BigInt("7766554433221100", 16)
    receiveRelease(r0, r1)
    d.io.taskReady.poke(true.B); step(); d.io.taskReady.poke(false.B)
    d.io.cValid.poke(true.B); d.io.cOpcode.poke(ProbeAck)
    d.io.cParam.poke(TtoN); d.io.cAddress.poke(0x380.U)
    d.io.releasePending.expect(true.B); d.io.cReady.expect(true.B)
    d.io.probeDone.expect(true.B); d.io.writeValid.expect(true.B)
    d.io.writeId.expect(6.U); step()
    d.io.cValid.poke(false.B); step(4)
    assert(writes.toSeq == Seq((BigInt(6), block(r0, r1))))
    assert(applied.toSeq == Seq(BigInt(6)))
    println("PASS: a dataless ProbeAck is not blocked by the reserved refill write")
  }
}


// Observation only: queue admission, source freezing and lease returns are DUT logic.
class ObservablePreparedDepthCtl(implicit p: Parameters) extends MSHRCtl {
  val depth = IO(Output(new Bundle {
    val count = UInt(2.W)
    val bPending = UInt(mshrsAll.W)
    val enq = Bool(); val enqSource = UInt(mshrBits.W)
    val enqOperation = UInt(4.W); val enqToken = new MainpipeLeaseToken
    val enqTask = new TaskBundle
    val prepare = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
    val cancel = UInt(mshrsAll.W)
    val cancelToken = Vec(mshrsAll, new MainpipeLeaseToken)
    val release = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
    val released = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
    val held = UInt(mshrsAll.W)
    val replReserved = UInt(mshrsAll.W)
    val replGranted = UInt(mshrsAll.W)
    val blockReplRead = UInt(mshrsAll.W)
    val ownerControlsChanged = UInt(mshrsAll.W)
  }))
  val q = mshrTaskPreparedQueues(0).io
  depth.count := q.count
  depth.bPending := snoopClaimSlotOH
  depth.enq := q.enq.fire
  depth.enqSource := q.enq.bits.source
  depth.enqOperation := q.enq.bits.operation
  depth.enqToken := q.enq.bits.token
  depth.enqTask := q.enq.bits.task
  depth.cancel := VecInit(mshrTaskLeasePorts(0).map(_.cancel.valid)).asUInt
  depth.held := VecInit(mshrTaskLeaseValid(0)).asUInt
  depth.replReserved := replaceReserveValid.asUInt
  depth.replGranted := Mux(replaceReserveGrant, replaceReserveGrantParentOH, 0.U)
  depth.blockReplRead := VecInit(refillMshrs.map(_.io.blockReplRead)).asUInt
  depth.ownerControlsChanged := VecInit(refillMshrs.map { m =>
    val controls = Cat(m.io.replaceActive, m.io.replaceAccepted,
      m.io.replaceDetached, m.io.replaceRefillReady, m.io.deferGrant,
      m.io.postGrantReleaseSnoopActive, m.io.blockReplRead)
    controls =/= RegNext(controls, 0.U(controls.getWidth.W))
  }).asUInt
  mshrTaskLeasePorts(0).zipWithIndex.foreach { case (port, i) =>
    depth.prepare(i) := port.prepare
    depth.cancelToken(i) := port.cancel.bits
    depth.release(i) := port.release
    depth.released(i) := port.released
  }
}

// MainPipe's documented claim/consume/apply/terminal protocol, no forced state.
// Lower-response wait bits start ready at allocation, as a boundary test;
// no CHI/cache-level throughput claim follows from this controlled supply.
class PreparedDepthHarness(implicit p: Parameters) extends L2Module with xscache.chi.HasCHIOpcodes {
  val io = IO(new Bundle {
    val request = Input(Bool()); val consume = Input(Bool()); val apply = Input(Bool())
    val set = Input(UInt(setBits.W)); val tag = Input(UInt(tagBits.W))
    val source = Input(UInt(sourceIdBits.W)); val accept = Input(Bool())
    val reserveB = Input(Bool()); val consumeB = Input(Bool()); val finishB = Input(Bool())
    val bIndex = Input(UInt(2.W)); val bReady = Output(Bool())
    val bReserved = Output(Bool()); val bSlot = Output(UInt(mshrBits.W))
    val bPending = Output(UInt(mshrsAll.W))
    val terminal = Flipped(Valid(new MainpipeLeaseDone))
    val ready = Output(Bool()); val ack = Output(Bool())
    val reservation = Output(new NormalSlotReservation)
    val task = Decoupled(new TaskBundle)
    val token = Output(new MainpipeLeaseToken)
    val taskPacked = Output(UInt((new TaskBundle).getWidth.W))
    val enqPacked = Output(UInt((new TaskBundle).getWidth.W))
    val observed = Output(new Bundle {
      val count = UInt(2.W)
      val enq = Bool(); val enqSource = UInt(mshrBits.W)
      val enqOperation = UInt(4.W); val enqToken = new MainpipeLeaseToken
      val prepare = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
      val cancel = UInt(mshrsAll.W)
      val cancelToken = Vec(mshrsAll, new MainpipeLeaseToken)
      val release = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
      val released = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
      val held = UInt(mshrsAll.W)
      val replReserved = UInt(mshrsAll.W)
      val replGranted = UInt(mshrsAll.W)
      val blockReplRead = UInt(mshrsAll.W)
      val ownerControlsChanged = UInt(mshrsAll.W)
    })
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val c = Module(new ObservablePreparedDepthCtl)
  zeroInputs(c.io)
  // Hold unrelated B Directory decisions through the real physical-slot
  // protocol. Four claims leave two ordinary replacement slots with eight
  // normal sources, so the first five frozen payloads cover both outcomes.
  c.io.snoopAdmission.valid := io.reserveB
  c.io.snoopAdmission.bits.channel := 2.U
  c.io.snoopAdmission.bits.set := 48.U + io.bIndex
  c.io.snoopAdmission.bits.tag := 70.U + io.bIndex
  c.io.snoopAdmission.bits.chiOpcode.get := SnpShared
  val heldB = Reg(Vec(4, new SnoopSlotToken))
  when(io.consumeB) { heldB(io.bIndex) := c.io.snoopReservationToken }
  c.io.bServiceConsume.valid := io.consumeB
  c.io.bServiceConsume.bits := c.io.snoopReservationToken
  // A B hit may complete its Directory decision without allocating a context.
  c.io.fromMainPipe.snoopReservationDone.valid := io.finishB
  c.io.fromMainPipe.snoopReservationDone.bits := heldB(io.bIndex)
  io.bReady := c.io.snoopAdmission.ready
  io.bReserved := c.io.snoopReservationToken.valid
  io.bSlot := c.io.snoopReservationToken.slot
  io.bPending := c.depth.bPending
  val request = c.io.normalReserveRequest
  request.valid := io.request
  request.bits.channel := 1.U
  request.bits.opcode := AcquirePerm
  request.bits.param := NtoB
  request.bits.set := io.set
  request.bits.tag := io.tag
  request.bits.sourceId := io.source
  request.bits.size := log2Ceil(blockBytes).U
  c.io.normalReserveConsume.valid := io.consume
  c.io.normalReserveConsume.bits := c.io.normalReserveAck.bits
  val accepted = RegEnable(c.io.normalReserveAck.bits, io.consume)
  val alloc = c.io.fromMainPipe.mshr_alloc_s3
  alloc.valid := io.apply
  alloc.bits.task := accepted.task
  alloc.bits.task.mshrId := accepted.id
  alloc.bits.state.elements.foreach(_._2 := true.B)
  alloc.bits.state.s_refill := false.B
  alloc.bits.state.w_replResp := false.B
  alloc.bits.dirResult.set := accepted.task.set
  alloc.bits.dirResult.tag := accepted.task.tag
  alloc.bits.dirResult.meta.state := MetaData.INVALID
  c.io.fromMainPipe.normalReservationDone.valid := io.apply
  c.io.fromMainPipe.normalReservationDone.bits.mshrId := accepted.id
  c.io.fromMainPipe.normalReservationDone.bits.mshrContext := 0.U
  c.io.fromMainPipe.normalReservationDone.bits.token := accepted.token
  c.io.fromMainPipe.mainpipeLeaseDone(0) := io.terminal
  c.io.mshrTask.ready := io.accept
  io.task.valid := c.io.mshrTask.valid
  io.task.bits := c.io.mshrTask.bits
  io.token := c.io.mshrTaskLeaseToken
  val normalized = WireDefault(c.io.mshrTask.bits)
  normalized.qosClass := 0.U // Aging may legally update only QoS after preparation.
  io.taskPacked := normalized.asUInt
  val enqNormalized = WireDefault(c.depth.enqTask)
  enqNormalized.qosClass := 0.U
  io.enqPacked := enqNormalized.asUInt
  io.ready := request.ready
  io.ack := c.io.normalReserveAck.valid
  io.reservation := c.io.normalReserveAck.bits
  io.observed.count := c.depth.count
  io.observed.enq := c.depth.enq
  io.observed.enqSource := c.depth.enqSource
  io.observed.enqOperation := c.depth.enqOperation
  io.observed.enqToken := c.depth.enqToken
  io.observed.prepare := c.depth.prepare
  io.observed.cancel := c.depth.cancel
  io.observed.cancelToken := c.depth.cancelToken
  io.observed.release := c.depth.release
  io.observed.released := c.depth.released
  io.observed.held := c.depth.held
  io.observed.replReserved := c.depth.replReserved
  io.observed.replGranted := c.depth.replGranted
  io.observed.blockReplRead := c.depth.blockReplRead
  io.observed.ownerControlsChanged := c.depth.ownerControlsChanged
}

object TestMSHRPreparedDepth extends App with ChiselSim {
  implicit val p: Parameters = PairMiterParameters.p.alterPartial {
    case L2ParamKey => PairMiterParameters.p(L2ParamKey).copy(mshrs = 8)
  }
  simulate(new PreparedDepthHarness) { d =>
    d.io.request.poke(false.B); d.io.consume.poke(false.B); d.io.apply.poke(false.B)
    d.io.accept.poke(false.B); d.io.task.ready.poke(false.B)
    d.io.reserveB.poke(false.B); d.io.consumeB.poke(false.B); d.io.finishB.poke(false.B)
    d.io.bIndex.poke(0.U)
    d.io.set.poke(0.U); d.io.tag.poke(0.U); d.io.source.poke(0.U)
    d.io.terminal.valid.poke(false.B)
    d.io.terminal.bits.mshrContext.poke(0.U); d.io.terminal.bits.mshrId.poke(0.U)
    d.io.terminal.bits.token.generation.poke(0.U); d.io.terminal.bits.token.sequence.poke(0.U)
    d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B)
    case class Header(set: BigInt, tag: BigInt, source: BigInt, generation: BigInt)
    case class Transaction(source: BigInt, generation: BigInt, sequence: BigInt)
    val owners = scala.collection.mutable.Map.empty[BigInt, Header]
    val active = scala.collection.mutable.Map.empty[BigInt, Transaction]
    val prepares = scala.collection.mutable.Set.empty[Transaction]
    val snapshots = scala.collection.mutable.Map.empty[Transaction, BigInt]
    val expectedReplRead = scala.collection.mutable.Map.empty[Transaction, Boolean]
    val issues = scala.collection.mutable.ArrayBuffer.empty[(Int, Transaction)]
    val cancelPrerequisites = scala.collection.mutable.Set.empty[Transaction]
    val cancels = scala.collection.mutable.Set.empty[Transaction]
    val terminals = scala.collection.mutable.Set.empty[Transaction]
    val returns = scala.collection.mutable.Set.empty[Transaction]
    val acks = scala.collection.mutable.Set.empty[Transaction]
    var cycle = 0; var peak = 0
    var stalled = Option.empty[(BigInt, BigInt, BigInt)]
    def transaction(id: BigInt, t: MainpipeLeaseToken): Transaction =
      Transaction(id, t.generation.peek().litValue, t.sequence.peek().litValue)
    def step(n: Int = 1): Unit = for (_ <- 0 until n) {
      peak = peak.max(d.io.observed.count.peek().litValue.toInt)
      val cancelBits = d.io.observed.cancel.peek().litValue
      val reserved = d.io.observed.replReserved.peek().litValue
      val blocked = d.io.observed.blockReplRead.peek().litValue
      val controlsChanged = d.io.observed.ownerControlsChanged.peek().litValue
      val held = d.io.observed.held.peek().litValue
      for (i <- 0 until 8) {
        val id = BigInt(i)
        val prep = d.io.observed.prepare(i)
        if (prep.valid.peek().litToBoolean) {
          val t = transaction(id, prep.bits)
          assert(owners.contains(id) && !active.contains(id) && !prepares(t),
            s"unknown/duplicate prepare $t")
          assert(t.generation == owners(id).generation, s"wrong generation at prepare $t")
          assert(prepares.filter(_.source == id).forall(acks),
            s"source reused before exact return acknowledgment $t")
          prepares += t; active(id) = t
          expectedReplRead(t) = reserved.testBit(i) && !blocked.testBit(i)
          // A ready miss may win its first proposal on the replacement-booking
          // edge. The owner-control change legally cancels this unfrozen lease.
          if (controlsChanged.testBit(i)) {
            assert(reserved.testBit(i) && !blocked.testBit(i),
              s"unexpected owner-control transition at prepare $t")
            cancelPrerequisites += t
          }
        }
        if (cancelBits.testBit(i)) {
          val t = transaction(id, d.io.observed.cancelToken(i))
          assert(active.get(id).contains(t) && cancelPrerequisites(t),
            s"unexpected source/token cancellation $t at cycle $cycle")
          assert(held.testBit(i) && !snapshots.contains(t) && !issues.exists(_._2 == t),
            s"canceled preparation materialized or issued $t")
          if (!cancels(t)) println(s"CANCEL cycle=$cycle source=$id generation=${t.generation} sequence=${t.sequence} after replacement booking")
          cancels += t
        }
        val release = d.io.observed.release(i)
        if (release.valid.peek().litToBoolean) {
          val t = transaction(id, release.bits)
          assert(active.get(id).contains(t) && (cancels(t) || terminals(t)) && !returns(t),
            s"premature/duplicate/wrong-version return $t")
          assert(held.testBit(i), s"return lost held source $t")
          returns += t
        }
        val ack = d.io.observed.released(i)
        if (ack.valid.peek().litToBoolean) {
          val t = transaction(id, ack.bits)
          assert(active.get(id).contains(t) && returns(t) && !acks(t),
            s"unknown/duplicate/wrong-version acknowledgment $t")
          assert(held.testBit(i), s"source released before acknowledgment edge $t")
          acks += t; active.remove(id)
        }
      }
      if (d.io.observed.enq.peek().litToBoolean) {
        val id = d.io.observed.enqSource.peek().litValue
        val t = transaction(id, d.io.observed.enqToken)
        assert(active.get(id).contains(t) && prepares(t) && !snapshots.contains(t) && !cancels(t),
          s"unknown/duplicate/canceled materialization $t")
        d.io.observed.enqOperation.expect(1.U)
        snapshots(t) = d.io.enqPacked.peek().litValue
      }
      if (d.io.task.valid.peek().litToBoolean) {
        val id = d.io.task.bits.mshrId.peek().litValue
        val t = transaction(id, d.io.token)
        val full = (d.io.taskPacked.peek().litValue, t.generation, t.sequence)
        stalled.foreach(previous => assert(previous == full, "stalled payload/token changed"))
        stalled = if (!d.io.accept.peek().litToBoolean) Some(full) else None
        if (d.io.accept.peek().litToBoolean) {
          assert(!issues.exists(_._2.source == id) && active.get(id).contains(t) && snapshots.contains(t),
            s"duplicate/unknown issue $t")
          assert(!cancels(t) && full._1 == snapshots(t), s"issue snapshot changed/canceled $t")
          val h = owners(id)
          d.io.task.bits.mshrContext.expect(0.U); d.io.task.bits.mshrTask.expect(true.B)
          d.io.task.bits.set.expect(h.set.U); d.io.task.bits.tag.expect(h.tag.U)
          d.io.task.bits.sourceId.expect(h.source.U); d.io.task.bits.opcode.expect(Grant)
          // Grant may proceed before replacement capacity is available; only
          // a source with a captured replacement reservation carries the read.
          d.io.task.bits.replTask.expect(expectedReplRead(t).B)
          issues += ((cycle, t))
        }
      } else assert(stalled.isEmpty, "stalled task disappeared")
      d.clock.step(); cycle += 1
    }
    def waitFor(test: => Boolean, label: String): Unit = {
      var left = 80
      while (!test && left > 0) { step(); left -= 1 }
      assert(test, s"timeout $label at cycle $cycle")
    }
    step(2)
    for (i <- 0 until 4) {
      d.io.bIndex.poke(i.U); d.io.reserveB.poke(true.B)
      waitFor(d.io.bReady.peek().litToBoolean, "B physical-slot claim")
      step(); d.io.reserveB.poke(false.B)
      d.io.bReserved.expect(true.B); d.io.bSlot.expect(i.U)
      d.io.consumeB.poke(true.B); step(); d.io.consumeB.poke(false.B)
      d.io.bReserved.expect(false.B)
    }
    d.io.bPending.expect(15.U)
    for (i <- 0 until 8) {
      val set = BigInt(2 + i); val tag = BigInt(17 + 3 * i)
      d.io.set.poke(set.U); d.io.tag.poke(tag.U); d.io.source.poke((i + 1).U)
      d.io.request.poke(true.B)
      waitFor(d.io.ready.peek().litToBoolean, "normal concrete-slot claim")
      step(); d.io.request.poke(false.B); d.io.ack.expect(true.B)
      val id = d.io.reservation.id.peek().litValue
      assert(!owners.contains(id), s"occupied slot reused $id")
      owners(id) = Header(set, tag, i + 1, d.io.reservation.token.generation.peek().litValue)
      d.io.consume.poke(true.B); step(); d.io.consume.poke(false.B)
      d.io.apply.poke(true.B); step(); d.io.apply.poke(false.B)
    }
    waitFor(d.io.observed.count.peek().litValue == 3, "actual prepared FIFO occupancy three")
    step(12)
    assert(peak == 3 && snapshots.size == 5)
    println(s"CAPACITY firstMaterialized=${snapshots.keys.toSeq.sortBy(_.sequence).map(t => s"${t.source}:${expectedReplRead(t)}").mkString(",")} heldB=${d.io.bPending.peek().litValue}")
    assert(snapshots.keys.exists(expectedReplRead) && snapshots.keys.exists(t => !expectedReplRead(t)),
      "exercise both reserved and capacity-blocked replacement reads")
    println(s"FILLED cycle=$cycle prepared=$peak materialized=${snapshots.size} canceled=${cancels.size}")
    assert(cancels.contains(Transaction(0, 1, 0)), "initial booking cancellation was not exercised")
    assert(cancels == cancelPrerequisites, "booking cancellations did not complete")
    assert((prepares -- snapshots.keySet -- cancels).size == 2,
      "two additional leases remain in the full winner FIFO")
    assert(prepares.map(t => (t.generation, t.sequence)).size == prepares.size,
      "prepared transaction tokens must be unique")
    assert(issues.isEmpty && returns == cancels && acks == cancels,
      "only canceled leases may return before issue/terminal")
    d.io.accept.poke(true.B)
    waitFor(issues.size == 8, "eight unique real-controller issues")
    step(5)
    assert(returns == cancels && acks == cancels, "issued lease returned before terminal")
    for ((_, t) <- issues.toSeq) {
      d.io.terminal.bits.mshrId.poke(t.source.U)
      d.io.terminal.bits.token.generation.poke(t.generation.U)
      d.io.terminal.bits.token.sequence.poke(t.sequence.U)
      terminals += t
      d.io.terminal.valid.poke(true.B); step(); d.io.terminal.valid.poke(false.B)
      step()
    }
    waitFor(acks.size == prepares.size, "all exact lease acknowledgments")
    step(12)
    d.io.observed.count.expect(0.U); d.io.observed.held.expect(0.U)
    assert(active.isEmpty && issues.size == 8 && returns == prepares && acks == prepares)
    assert(snapshots.keySet == terminals && cancels.intersect(terminals).isEmpty,
      "every accepted preparation must issue once or cancel once")
    // Return all unrelated pressure claims using their exact captured tokens.
    for (i <- 0 until 4) {
      d.io.bIndex.poke(i.U); d.io.finishB.poke(true.B); step()
      d.io.finishB.poke(false.B)
      d.io.bPending.expect((15 & ~((1 << (i + 1)) - 1)).U)
    }
    step(3); d.io.bPending.expect(0.U); d.io.observed.held.expect(0.U)
    val intervals = issues.map(_._1).sliding(2).map(s => s(1) - s(0)).toSeq
    println(s"PASS actual MSHRCtl: prepared peak=$peak; eight unique generation/sequence/full-payload issues; cancels=${cancels.size}; returns=${returns.size}; acks=${acks.size}")
    println(s"MEASURE controlled eight-owner drain issueCycles=${issues.map(_._1).mkString(",")} intervals=${intervals.mkString(",")}; not sustained/full-system throughput")
  }
}


// Real MSHRCtl -> real RequestArb, with documented normal-slot allocation and
// explicit terminal input. Only MainPipe/Directory are modeled at their ports.
// Pre-ready lower response bits supply independent normal Grant operations;
// this measures the scheduler/ingress service, not full-cache line throughput.
class MSHRStageChainHarness(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle {
    val request = Input(Bool()); val consume = Input(Bool()); val apply = Input(Bool())
    val set = Input(UInt(setBits.W)); val tag = Input(UInt(tagBits.W))
    val source = Input(UInt(sourceIdBits.W))
    val capture = Input(Bool()); val advance = Input(Bool())
    val terminal = Flipped(Valid(new MainpipeLeaseDone))
    val ready = Output(Bool()); val ack = Output(Bool())
    val reservation = Output(new NormalSlotReservation)
    val prepared = Valid(new TaskBundle)
    val preparedToken = Output(new MainpipeLeaseToken)
    val issued = Valid(new TaskBundle)
    val issueToken = Output(new MainpipeLeaseToken)
    val pipe = Valid(new TaskBundle)
    val pipeToken = Output(new MainpipeLeaseToken)
    val grantAdd = Valid(new GrantReservationToken)
    val grantReturn = Valid(new GrantReservationToken)
    val pipeGrant = Valid(new GrantReservationToken)
    val prepare = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
    val cancel = Output(UInt(mshrsAll.W))
    val release = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
    val released = Vec(mshrsAll, Valid(new MainpipeLeaseToken))
    val held = Output(UInt(mshrsAll.W))
    val ingressCount = Output(UInt(2.W))
    val admissionReady = Output(Bool())
    val inputValid = Output(Bool())
  })
  def zeroInputs(data: Data): Unit = data match {
    case r: Record => r.elements.values.foreach(zeroInputs)
    case v: Vec[_] => v.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  val c = Module(new ObservablePreparedDepthCtl)
  val a = Module(new RequestArb)
  zeroInputs(c.io); zeroInputs(a.io)
  c.io.normalReserveRequest.valid := io.request
  val request = c.io.normalReserveRequest.bits
  request.channel := 1.U; request.opcode := AcquirePerm; request.param := NtoB
  request.set := io.set; request.tag := io.tag; request.sourceId := io.source
  request.size := log2Ceil(blockBytes).U
  c.io.normalReserveConsume.valid := io.consume
  c.io.normalReserveConsume.bits := c.io.normalReserveAck.bits
  val accepted = RegEnable(c.io.normalReserveAck.bits, io.consume)
  val alloc = c.io.fromMainPipe.mshr_alloc_s3
  alloc.valid := io.apply
  alloc.bits.task := accepted.task; alloc.bits.task.mshrId := accepted.id
  alloc.bits.state.elements.foreach(_._2 := true.B)
  alloc.bits.state.s_refill := false.B
  alloc.bits.dirResult.hit := true.B
  alloc.bits.dirResult.set := accepted.task.set; alloc.bits.dirResult.tag := accepted.task.tag
  alloc.bits.dirResult.meta.state := MetaData.TIP
  c.io.fromMainPipe.normalReservationDone.valid := io.apply
  c.io.fromMainPipe.normalReservationDone.bits.mshrId := accepted.id
  c.io.fromMainPipe.normalReservationDone.bits.mshrContext := 0.U
  c.io.fromMainPipe.normalReservationDone.bits.token := accepted.token
  c.io.fromMainPipe.mainpipeLeaseDone(0) := io.terminal
  a.io.mshrTask.valid := c.io.mshrTask.valid && io.capture
  a.io.mshrTask.bits := c.io.mshrTask.bits
  a.io.mshrLeaseToken := c.io.mshrTaskLeaseToken
  c.io.mshrTask.ready := a.io.mshrTask.ready && io.capture
  a.io.hint_s1QueueFull := !io.advance
  a.io.dirRead_s1.ready := true.B
  val grantReturn = WireDefault(a.io.taskGrantReservationToken_s2)
  grantReturn.valid := a.io.taskToPipe_s2.valid && a.io.taskGrantReservationToken_s2.valid
  val returned = RegNext(grantReturn, 0.U.asTypeOf(grantReturn))
  a.io.grantReservationReturn(0) := returned
  io.ready := c.io.normalReserveRequest.ready; io.ack := c.io.normalReserveAck.valid
  io.reservation := c.io.normalReserveAck.bits
  io.prepared.valid := c.depth.enq; io.prepared.bits := c.depth.enqTask
  io.preparedToken := c.depth.enqToken
  io.issued.valid := c.io.mshrTask.fire; io.issued.bits := c.io.mshrTask.bits
  io.issueToken := c.io.mshrTaskLeaseToken
  io.pipe := a.io.taskToPipe_s2; io.pipeToken := a.io.taskLeaseToken_s2
  io.grantAdd := a.io.grantReservation; io.grantReturn := returned
  io.pipeGrant := a.io.taskGrantReservationToken_s2
  io.prepare := c.depth.prepare; io.cancel := c.depth.cancel
  io.release := c.depth.release; io.released := c.depth.released; io.held := c.depth.held
  io.ingressCount := PopCount(a.io.setStatus.take(2).map(_.valid))
  io.admissionReady := a.io.mshrTask.ready
  io.inputValid := a.io.mshrTask.valid
}

object TestMSHRStageChain extends App with ChiselSim {
  implicit val p: Parameters = PairMiterParameters.p.alterPartial {
    case L2ParamKey => PairMiterParameters.p(L2ParamKey).copy(mshrs = 16)
  }
  simulate(new MSHRStageChainHarness) { d =>
    d.io.request.poke(false.B); d.io.consume.poke(false.B); d.io.apply.poke(false.B)
    d.io.set.poke(0.U); d.io.tag.poke(0.U); d.io.source.poke(0.U)
    d.io.capture.poke(false.B); d.io.advance.poke(false.B)
    d.io.terminal.valid.poke(false.B)
    d.io.terminal.bits.mshrContext.poke(0.U); d.io.terminal.bits.mshrId.poke(0.U)
    d.io.terminal.bits.token.generation.poke(0.U); d.io.terminal.bits.token.sequence.poke(0.U)
    d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B)
    case class Token(id: BigInt, generation: BigInt, sequence: BigInt)
    def token(id: BigInt, t: MainpipeLeaseToken): Token =
      Token(id, t.generation.peek().litValue, t.sequence.peek().litValue)
    def payload(t: TaskBundle): Map[String, BigInt] = {
      def leaves(data: Data, path: String): Map[String, BigInt] = data match {
        case r: Record => r.elements.toSeq.flatMap { case (k, v) => leaves(v, s"$path.$k") }.toMap
        case v: Vec[_] => v.zipWithIndex.flatMap { case (x, i) => leaves(x, s"$path.$i") }.toMap
        case b: Bool => Map(path -> b.peek().litValue)
        case u: UInt => Map(path -> u.peek().litValue)
        case _ => Map.empty
      }
      leaves(t, "task").removed("task.qosClass")
    }
    val owners = scala.collection.mutable.Map.empty[BigInt, (BigInt, BigInt, BigInt)]
    val active = scala.collection.mutable.Set.empty[Token]
    val snapshots = scala.collection.mutable.Map.empty[Token, Map[String, BigInt]]
    val issuePayloads = scala.collection.mutable.Map.empty[Token, Map[String, BigInt]]
    val issues = scala.collection.mutable.ArrayBuffer.empty[(Int, Token)]
    val pipes = scala.collection.mutable.ArrayBuffer.empty[(Int, Token)]
    val terminals = scala.collection.mutable.Set.empty[Token]
    val returns = scala.collection.mutable.Set.empty[Token]
    val acks = scala.collection.mutable.Set.empty[Token]
    val grantOwners = scala.collection.mutable.Set.empty[(BigInt, BigInt, BigInt)]
    val grantUses = scala.collection.mutable.Set.empty[(BigInt, BigInt, BigInt)]
    var cycle = 0; var full = 0; var maxIngress = 0
    def grant(t: GrantReservationToken): (BigInt, BigInt, BigInt) =
      (t.id.peek().litValue, t.generation.peek().litValue, t.channel.peek().litValue)
    def step(n: Int = 1): Unit = for (_ <- 0 until n) {
      d.io.cancel.expect(0.U)
      val count = d.io.ingressCount.peek().litValue.toInt
      maxIngress = maxIngress.max(count)
      if (count == 2) { full += 1; d.io.admissionReady.expect(false.B) }
      for (i <- 0 until 16) {
        val prep = d.io.prepare(i)
        if (prep.valid.peek().litToBoolean) {
          val t = token(i, prep.bits)
          assert(!active(t)); active += t
        }
        val ret = d.io.release(i)
        if (ret.valid.peek().litToBoolean) {
          val t = token(i, ret.bits)
          assert(active(t) && terminals(t) && !returns(t), s"premature/duplicate return $t")
          returns += t
        }
        val ack = d.io.released(i)
        if (ack.valid.peek().litToBoolean) {
          val t = token(i, ack.bits)
          assert(active(t) && returns(t) && !acks(t)); acks += t; active -= t
        }
      }
      if (d.io.prepared.valid.peek().litToBoolean) {
        val id = d.io.prepared.bits.mshrId.peek().litValue
        val t = token(id, d.io.preparedToken)
        assert(active(t) && !snapshots.contains(t)); snapshots(t) = payload(d.io.prepared.bits)
      }
      if (d.io.issued.valid.peek().litToBoolean) {
        val id = d.io.issued.bits.mshrId.peek().litValue
        val t = token(id, d.io.issueToken)
        val h = owners(id)
        d.io.issued.bits.set.expect(h._1.U); d.io.issued.bits.tag.expect(h._2.U)
        d.io.issued.bits.sourceId.expect(h._3.U)
        val earlier = issues.count(_._2.id == id)
        assert(earlier <= 1, s"unexpected third operation for source $id")
        if (earlier == 0) {
          d.io.issued.bits.opcode.expect(Grant); d.io.issued.bits.refillOnly.expect(false.B)
        } else {
          val previous = issues.find(_._2.id == id).get._2
          assert(acks(previous), s"next operation reused source before exact acknowledgment $id")
          d.io.issued.bits.refillOnly.expect(true.B)
        }
        assert(active(t) && !issuePayloads.contains(t) && snapshots(t) == payload(d.io.issued.bits))
        issuePayloads(t) = payload(d.io.issued.bits); issues += ((cycle, t))
      }
      if (d.io.grantAdd.valid.peek().litToBoolean) {
        val t = grant(d.io.grantAdd.bits)
        assert(!grantOwners(t), s"duplicate Grant reservation $t"); grantOwners += t
      }
      if (d.io.pipe.valid.peek().litToBoolean) {
        val id = d.io.pipe.bits.mshrId.peek().litValue
        val t = token(id, d.io.pipeToken)
        assert(issuePayloads.contains(t) && !pipes.exists(_._2 == t) &&
          issuePayloads(t) == payload(d.io.pipe.bits), s"changed/duplicate pipeline payload $t")
        d.io.pipeGrant.valid.expect(true.B)
        val g = grant(d.io.pipeGrant.bits)
        assert(grantOwners(g) && !grantUses(g)); grantUses += g
        pipes += ((cycle, t))
      }
      if (d.io.grantReturn.valid.peek().litToBoolean) {
        val g = grant(d.io.grantReturn.bits)
        assert(grantOwners(g) && grantUses(g), s"unknown/premature grant return $g")
        grantOwners -= g
      }
      d.clock.step(); cycle += 1
    }
    def waitFor(test: => Boolean, label: String): Unit = {
      var left = 200
      while (!test && left > 0) { step(); left -= 1 }
      assert(test, s"timeout $label at cycle $cycle issues=${issues.size} pipes=${pipes.size}")
    }
    step(3)
    // Alternating DS banks remove the existing MCP2 restriction from this
    // service measurement. Each task keeps a different set and source.
    for (i <- 0 until 16) {
      val set = BigInt(2 + i); val tag = BigInt(0x80 + i)
      d.io.set.poke(set.U); d.io.tag.poke(tag.U); d.io.source.poke(i.U)
      d.io.request.poke(true.B); waitFor(d.io.ready.peek().litToBoolean, "claim")
      step(); d.io.request.poke(false.B); d.io.ack.expect(true.B)
      val id = d.io.reservation.id.peek().litValue
      assert(!owners.contains(id)); owners(id) = (set, tag, BigInt(i))
      d.io.consume.poke(true.B); step(); d.io.consume.poke(false.B)
      d.io.apply.poke(true.B); step(); d.io.apply.poke(false.B)
    }
    step(12)
    // First fill both real ingress slots while MCP2/S1 are explicitly blocked.
    // Opening S1 with full Q must still refuse same-cycle borrowed admission.
    d.io.capture.poke(true.B); d.io.advance.poke(true.B)
    // Gate S1 separately using the existing hint resource once two tasks have
    // progressed; later exact Zhujiang covers real Directory/resource stalls.
    waitFor(pipes.size >= 2, "initial pipeline service")
    d.io.advance.poke(false.B); step(5)
    d.io.advance.poke(true.B)
    waitFor(pipes.size == 16, "sixteen complete descriptor transfers")
    step(4)
    assert(issues.map(_._2) == pipes.map(_._2), "ingress changed descriptor order")
    def longest(xs: Seq[Int]): Int = {
      var best = 0; var run = 0; var last = -2
      xs.foreach { x => run = if (x == last + 1) run + 1 else 1; best = best.max(run); last = x }
      best
    }
    val issueRun = longest(issues.map(_._1).toSeq)
    val pipeRun = longest(pipes.map(_._1).toSeq)
    println(s"SERVICE issues=${issues.map(_._1).mkString(",")} pipes=${pipes.map(_._1).mkString(",")} longestIssue=$issueRun longestPipe=$pipeRun ingressPeak=$maxIngress fullCycles=$full")
    assert(issueRun >= 8 && pipeRun >= 8, "independent-source steady service must sustain eight consecutive cycles")
    assert(returns.isEmpty && acks.isEmpty, "issued source returned before explicit terminal")
    def complete(t: Token): Unit = {
      d.io.terminal.bits.mshrId.poke(t.id.U)
      d.io.terminal.bits.token.generation.poke(t.generation.U)
      d.io.terminal.bits.token.sequence.poke(t.sequence.U)
      assert(!terminals(t)); terminals += t
      d.io.terminal.valid.poke(true.B); step()
    }
    val firstBurst = pipes.map(_._2).toVector
    firstBurst.foreach(complete)
    d.io.terminal.valid.poke(false.B)
    waitFor(firstBurst.forall(acks), "sixteen exact Grant terminal returns")
    // The real normal FSM now supplies a separate refill-commit variant.
    // Its new lease must be checked, drained and returned independently.
    waitFor(pipes.size == 32, "sixteen subsequent refill-commit transfers")
    pipes.map(_._2).filterNot(terminals).toVector.foreach(complete)
    d.io.terminal.valid.poke(false.B)
    waitFor(acks.size == 32, "all Grant and refill-commit terminal returns")
    step(8); d.io.held.expect(0.U); d.io.ingressCount.expect(0.U)
    println(s"FINAL active=$active grantOwners=$grantOwners returns=$returns terminals=$terminals acks=$acks")
    assert(active.isEmpty && grantOwners.isEmpty && returns == terminals && acks == terminals,
      s"final exact ledgers active=${active.size} grant=${grantOwners.size} return=${returns.size} terminal=${terminals.size} ack=${acks.size}")
    println(s"PASS real MSHRCtl -> RequestArb: 16 Grant + 16 refill-commit full-payload/token transfers, longest initial service $issueRun/$pipeRun, exact source and Grant returns")
  }
}

// Real RequestArb at its documented ports. Source/Directory/MainPipe endpoints
// are driven independently; no internal register is forced or replaced.
object TestRequestArbHeadOfLine extends App with ChiselSim {
  implicit val p: Parameters = PairMiterParameters.p.alterPartial {
    case L2ParamKey => PairMiterParameters.p(L2ParamKey).copy(sets = 16, mshrs = 8)
  }
  simulate(new RequestArb) { d =>
    def leaves(data: Data, path: String = "task"): Map[String, BigInt] = data match {
      case r: Record => r.elements.toSeq.flatMap { case (k, v) => leaves(v, s"$path.$k") }.toMap
      case v: Vec[_] => v.zipWithIndex.flatMap { case (x, i) => leaves(x, s"$path.$i") }.toMap
      case b: Bool => Map(path -> b.peek().litValue)
      case u: UInt => Map(path -> u.peek().litValue)
      case _ => Map.empty
    }
    def zero(data: Data, inputsOnly: Boolean = false): Unit = data match {
      case r: Record => r.elements.values.foreach(x => zero(x, inputsOnly))
      case v: Vec[_] => v.foreach(x => zero(x, inputsOnly))
      case x if !inputsOnly || DataMirror.directionOf(x) == ActualDirection.Input => x match {
        case b: Bool => b.poke(false.B)
        case u: UInt => u.poke(0.U)
        case _ =>
      }
      case _ =>
    }
    type GrantOwner = (BigInt, BigInt, BigInt)
    case class Expected(task: Map[String, BigInt], lease: (Int, Int), grant: Boolean,
      snoop: Option[Map[String, BigInt]] = None)
    val expected = scala.collection.mutable.Map.empty[BigInt, Expected]
    val pipes = scala.collection.mutable.ArrayBuffer.empty[BigInt]
    val active = scala.collection.mutable.Set.empty[GrantOwner]
    val used = scala.collection.mutable.Set.empty[GrantOwner]
    var pendingReturn: Option[GrantOwner] = None
    var pendingC: Option[GrantOwner] = None
    var sequence = 0; var cycle = 0
    def grant(t: GrantReservationToken): GrantOwner =
      (t.id.peek().litValue, t.generation.peek().litValue, t.channel.peek().litValue)
    def driveGrant(t: GrantReservationToken, g: GrantOwner): Unit = {
      t.id.poke(g._1.U); t.generation.poke(g._2.U); t.channel.poke(g._3.U)
    }
    def step(n: Int = 1): Unit = for (_ <- 0 until n) {
      d.io.grantReservationReturn(0).valid.poke(pendingReturn.nonEmpty.B)
      pendingReturn.foreach(g => driveGrant(d.io.grantReservationReturn(0).bits, g))
      d.io.cDecisionDone.valid.poke(pendingC.nonEmpty.B)
      pendingC.foreach(g => driveGrant(d.io.cDecisionDone.bits, g))
      val returning = pendingReturn
      pendingReturn = None; pendingC = None
      if (d.io.grantReservation.valid.peek().litToBoolean) {
        val g = grant(d.io.grantReservation.bits)
        assert(!active(g)); active += g
      }
      if (d.io.taskToPipe_s2.valid.peek().litToBoolean) {
        val t = d.io.taskToPipe_s2.bits; val tag = t.tag.peek().litValue
        assert(expected.contains(tag) && !pipes.contains(tag), s"unknown/duplicate task $tag at $cycle")
        val e = expected(tag)
        assert(leaves(t) == e.task, s"changed full descriptor $tag at $cycle")
        d.io.taskLeaseToken_s2.generation.expect(e.lease._1.U)
        d.io.taskLeaseToken_s2.sequence.expect(e.lease._2.U)
        d.io.taskSequence_s2.expect(sequence.U); sequence += 1
        d.io.taskGrantReservationToken_s2.valid.expect(e.grant.B)
        e.snoop match {
          case Some(token) => assert(leaves(d.io.taskSnoopReservationToken_s2) == token)
          case None => d.io.taskSnoopReservationToken_s2.valid.expect(false.B)
        }
        if (e.grant) {
          val g = grant(d.io.taskGrantReservationToken_s2.bits)
          assert(active(g) && !used(g), s"unknown/reused Grant token $g")
          used += g; pendingReturn = Some(g)
          if (g._3 == 4) pendingC = Some(g)
        }
        pipes += tag
      }
      returning.foreach(g => { assert(active(g) && used(g)); active -= g })
      val cFire = d.io.sinkC.valid.peek().litToBoolean && d.io.sinkC.ready.peek().litToBoolean
      val bFire = d.io.sinkB.valid.peek().litToBoolean && d.io.sinkB.ready.peek().litToBoolean
      val aCapture = d.io.normalReserveConsume.valid.peek().litToBoolean
      d.clock.step(); cycle += 1
      if (cFire) d.io.sinkC.valid.poke(false.B)
      if (bFire) d.io.sinkB.valid.poke(false.B)
      if (aCapture) d.io.normalReserveAck.valid.poke(false.B)
    }
    def waitFor(test: => Boolean, label: String): Unit = {
      var left = 80
      while (!test && left > 0) { step(); left -= 1 }
      assert(test, s"timeout $label at cycle $cycle pipes=$pipes")
    }
    def reset(): Unit = {
      zero(d.io, inputsOnly = true)
      expected.clear(); pipes.clear(); active.clear(); used.clear()
      pendingReturn = None; pendingC = None; sequence = 0; cycle = 0
      d.io.dirRead_s1.ready.poke(true.B)
      d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B); step(20)
    }
    def sendMshr(tag: Int, set: Int, commit: Boolean = false, repl: Boolean = false,
      snoop: Boolean = false): Unit = {
      val t = d.io.mshrTask.bits; zero(t)
      t.mshrTask.poke(true.B); t.channel.poke((if (snoop) 2 else 1).U)
      t.opcode.poke(if (snoop) ProbeAck else Grant)
      t.set.poke(set.U); t.tag.poke(tag.U); t.sourceId.poke(3.U); t.mshrId.poke(3.U)
      t.mshrContext.poke((if (snoop) 1 else 0).U)
      t.refillOnly.poke(commit.B); t.readRefillData.poke(commit.B)
      t.replTask.poke(repl.B); t.way.poke(1.U); t.meta.dirty.poke(true.B)
      d.io.mshrLeaseToken.generation.poke(7.U)
      d.io.mshrLeaseToken.sequence.poke((tag & 255).U)
      expected(tag) = Expected(leaves(t), (7, tag & 255), !snoop)
      d.io.mshrTask.valid.poke(true.B)
      waitFor(d.io.mshrTask.ready.peek().litToBoolean, "MSHR admission")
      step(); d.io.mshrTask.valid.poke(false.B)
    }
    def offerA(tag: Int, set: Int, id: Int = 0): Unit = {
      val r = d.io.normalReserveAck.bits; zero(r)
      r.id.poke(id.U); r.token.generation.poke(5.U); r.token.sequence.poke(tag.U)
      val t = r.task
      t.channel.poke(1.U); t.opcode.poke(Get); t.set.poke(set.U); t.tag.poke(tag.U)
      t.sourceId.poke(id.U); t.aRequestIdentity.entry.poke(id.U)
      t.aRequestIdentity.version.poke(3.U); t.aRequestIdentity.sequence.poke(tag.U)
      val claim = d.io.normalClaimOwners(0)(id)
      claim.valid.poke(true.B); claim.bits.inFlight.poke(true.B)
      claim.bits.token.generation.poke(5.U); claim.bits.token.sequence.poke(tag.U)
      claim.bits.requestIdentity.entry.poke(id.U); claim.bits.requestIdentity.version.poke(3.U)
      claim.bits.requestIdentity.sequence.poke(tag.U)
      claim.bits.set.poke(set.U); claim.bits.tag.poke(tag.U); claim.bits.source.poke(id.U)
      expected(tag) = Expected(leaves(t).updated("task.mshrId", BigInt(id)), (5, tag), true)
      d.io.normalReserveAck.valid.poke(true.B)
    }
    def sendA(tag: Int, set: Int, id: Int = 0): Unit = {
      offerA(tag, set, id)
      waitFor(d.io.normalReserveConsume.valid.peek().litToBoolean, "A capture"); step()
    }
    def offerC(tag: Int, set: Int): Unit = {
      val t = d.io.sinkC.bits; zero(t)
      t.channel.poke(4.U); t.opcode.poke(ReleaseData); t.set.poke(set.U); t.tag.poke(tag.U)
      t.sourceId.poke(2.U); t.bufIdx.poke(2.U)
      expected(tag) = Expected(leaves(t), (0, 0), true)
      d.io.sinkC.valid.poke(true.B)
    }
    def finish(tags: Seq[Int]): Unit = {
      waitFor(pipes.size == tags.size, "all pipeline tasks")
      assert(pipes.toSeq == tags.map(BigInt(_)), s"wrong independent-owner service order: $pipes")
      step(4); assert(active.isEmpty && pendingReturn.isEmpty)
      d.io.setStatus.foreach(_.valid.expect(false.B))
    }

    reset()
    d.io.blockRefillOnlyCommit.poke(true.B)
    sendMshr(101, 2, commit = true); sendA(102, 3)
    waitFor(pipes.contains(102), "A bypasses held refill commit")
    assert(!pipes.contains(101))
    sendA(103, 5, id = 1); step(8)
    assert(pipes.toSeq == Seq(BigInt(102)), "a second younger service passed the same held owner")
    d.io.blockRefillOnlyCommit.poke(false.B)
    finish(Seq(102, 101, 103))
    println("PASS held refill commit: independent A service, exact full payload/tokens, one-pass bound")

    reset()
    d.io.dirRead_s1.ready.poke(false.B)
    sendA(111, 2); sendMshr(112, 3)
    waitFor(pipes.contains(112), "no-read MSHR bypasses Directory-held A")
    assert(!pipes.contains(111))
    d.io.dirRead_s1.ready.poke(true.B); finish(Seq(112, 111))
    println("PASS bank/port-held channel: no-read MSHR service without Directory permission")

    reset()
    d.io.fromMainPipe.blockG_s1.poke(true.B)
    sendMshr(121, 2, repl = true); sendA(122, 3)
    waitFor(d.io.dirRead_s1.valid.peek().litToBoolean &&
      d.io.dirRead_s1.bits.tag.peek().litValue == 122, "alternate Directory attempt")
    // Ready was high when the oldest stall was observed. Withdraw it at the
    // actual younger port attempt: the previous observation is no permission.
    d.io.dirRead_s1.ready.poke(false.B); step()
    assert(pipes.isEmpty)
    d.io.dirRead_s1.ready.poke(true.B)
    waitFor(pipes.contains(122), "alternate fresh Directory ready")
    d.io.dirRead_s1.ready.poke(false.B)
    d.io.fromMainPipe.blockG_s1.poke(false.B)
    waitFor(d.io.dirRead_s1.valid.peek().litToBoolean, "retained replacement capture")
    d.io.dirRead_s1.bits.tag.expect(121.U); d.io.dirRead_s1.bits.refill.expect(true.B)
    val heldRead = leaves(d.io.dirRead_s1.bits)
    step(4); assert(pipes.toSeq == Seq(BigInt(122)) && leaves(d.io.dirRead_s1.bits) == heldRead)
    d.io.dirRead_s1.ready.poke(true.B); finish(Seq(122, 121))
    println("PASS replacement: same-set hold, fresh ready withdrawal/retry, retained command and lease")

    reset()
    d.io.blockRefillOnlyCommit.poke(true.B)
    sendMshr(131, 2, commit = true); offerA(132, 2)
    for (_ <- 0 until 8) { d.io.normalReserveConsume.valid.expect(false.B); step() }
    assert(pipes.isEmpty)
    d.io.blockRefillOnlyCommit.poke(false.B)
    waitFor(!d.io.normalReserveAck.valid.peek().litToBoolean, "same-set A after older departure")
    finish(Seq(131, 132))
    println("PASS same-set descriptor cannot be admitted across held predecessor")

    reset()
    d.io.blockRefillOnlyCommit.poke(true.B)
    sendMshr(141, 2, commit = true); offerC(142, 3); step(10)
    assert(pipes.isEmpty, "younger C overtook a held MSHR")
    d.io.sinkC.ready.expect(false.B)
    d.io.blockRefillOnlyCommit.poke(false.B); finish(Seq(141, 142))
    println("PASS younger C stays behind older MSHR and consumes its exact buffered descriptor")

    reset()
    d.io.blockRefillOnlyCommit.poke(true.B)
    sendMshr(151, 2, commit = true)
    val b = d.io.sinkB.bits; zero(b)
    b.channel.poke(2.U); b.chiOpcode.get.poke(7.U)
    b.set.poke(3.U); b.tag.poke(152.U); b.sourceId.poke(2.U)
    b.txnID.get.poke(19.U)
    val bt = d.io.snoopReservationToken; zero(bt)
    bt.valid.poke(true.B); bt.slot.poke(6.U); bt.set.poke(3.U); bt.tag.poke(152.U)
    bt.txnID.poke(19.U); bt.source.poke(2.U)
    bt.generation.poke(9.U); bt.sequence.poke(21.U)
    expected(152) = Expected(leaves(b), (0, 0), false, Some(leaves(bt)))
    d.io.sinkB.valid.poke(true.B)
    waitFor(!d.io.sinkB.valid.peek().litToBoolean, "B entry capture")
    waitFor(d.io.bServiceConsume.valid.peek().litToBoolean, "reserved B bypass service")
    assert(leaves(d.io.bServiceConsume.bits) == leaves(bt))
    step(); waitFor(pipes.contains(152), "B pipeline advance")
    assert(!pipes.contains(151))
    d.io.blockRefillOnlyCommit.poke(false.B); finish(Seq(152, 151))
    println("PASS reserved B forward progress with exact snoop token around held refill commit")
    println("PASS RequestArb bounded cross-lane bypass: six directed ownership/resource scenarios")
  }
}
