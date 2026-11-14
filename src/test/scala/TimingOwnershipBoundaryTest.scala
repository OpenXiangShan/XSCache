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
  Seq(c.io.nestedwbDataId, c.io.nestedwbDataIdExtra).zip(b.io.w).foreach { case (target, w) =>
    w.valid := target.valid
    w.bits.id := target.bits
    w.bits.data.data := io.payload
    w.bits.beatMask := Fill(beatSize, true.B)
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
  io.write := c.io.nestedwbDataId.valid
  io.writeId := c.io.nestedwbDataId.bits
  io.extra := c.io.nestedwbDataIdExtra.valid
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
