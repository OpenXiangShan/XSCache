package xscache.coupledL2

import chisel3._
import chisel3.util._
import chisel3.experimental.UnlocatableSourceInfo
import chisel3.simulator.ChiselSim
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

// Capture the *real inherited monitor predicates*, including their enclosing
// guards, so legal and illegal traffic can share a single simulation. Only the
// test assertion sink changes; production monAssert/assume remain assertions.
class ObservedCmoMonitor(args: TLMonitorArgs) extends TLMonitor(args) {
  private lazy val errorBits = RegInit(VecInit(Seq.fill(7)(false.B)))
  override def legalize(bundle: TLBundle, edge: TLEdge, reset: Reset): Unit = {
    // Allocate the observation registers before inherited when scopes start.
    require(errorBits.size == 7)
    super.legalize(bundle, edge, reset)
  }
  override def monAssert(cond: Bool, message: String): Unit = {
    when (!cond) {
      errorBits(6) := true.B
      Seq("invalid opcode", "re-used a source", "nothing inflight",
        "improper opcode response", "response size", "CBO").zipWithIndex.foreach {
        case (fragment, index) => if (message.contains(fragment)) errorBits(index) := true.B
      }
    }
  }
  override def assume(cond: Bool, message: String): Unit = monAssert(cond, message)
  val failed = IO(Output(UInt(6.W)))
  failed := errorBits.asUInt(5, 0)
  val anyFailed = IO(Output(Bool()))
  anyFailed := errorBits(6)
}

class CmoMonitorHarness extends Module {
  val base = Parameters.empty
  val master = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    name = "cmotest", sourceId = IdRange(0, 3), supportsProbe = TransferSizes(64))))
  val slave = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
    address = Seq(AddressSet(0, 65535)), regionType = RegionType.CACHED,
    supportsAcquireB = TransferSizes(64), supportsAcquireT = TransferSizes(64),
    supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1, 64))),
    beatBytes = 32, endSinkId = 4)
  val edge = new TLEdgeOut(master, slave, base, UnlocatableSourceInfo)
  val io = IO(new Bundle {
    val aValid = Input(Bool()); val aOpcode = Input(UInt(4.W))
    val aSource = Input(UInt(2.W)); val aSize = Input(UInt(3.W))
    val aAddress = Input(UInt(16.W)); val aCorrupt = Input(Bool())
    val dValid = Input(Bool()); val dOpcode = Input(UInt(4.W))
    val dSource = Input(UInt(2.W)); val dSize = Input(UInt(3.W))
    val failed = Output(UInt(6.W)); val anyFailed = Output(Bool())
  })
  val monitor = Module(new ObservedCmoMonitor(TLMonitorArgs(edge)))
  monitor.io.in := 0.U.asTypeOf(monitor.io.in)
  monitor.io.in.a.valid := io.aValid
  monitor.io.in.a.ready := true.B
  monitor.io.in.a.bits.opcode := io.aOpcode
  monitor.io.in.a.bits.source := io.aSource
  monitor.io.in.a.bits.size := io.aSize
  monitor.io.in.a.bits.address := io.aAddress
  monitor.io.in.a.bits.mask := edge.mask(io.aAddress, io.aSize)
  monitor.io.in.a.bits.corrupt := io.aCorrupt
  monitor.io.in.d.valid := io.dValid
  monitor.io.in.d.ready := true.B
  monitor.io.in.d.bits.opcode := io.dOpcode
  monitor.io.in.d.bits.source := io.dSource
  monitor.io.in.d.bits.size := io.dSize
  io.failed := monitor.failed
  io.anyFailed := monitor.anyFailed
}

object TestCmoMonitor extends App with ChiselSim {
  simulate(new CmoMonitorHarness) { dut =>
    def clear(): Unit = {
      dut.io.aValid.poke(false.B); dut.io.dValid.poke(false.B)
      dut.io.aOpcode.poke(12.U); dut.io.aSource.poke(0.U)
      dut.io.aSize.poke(0.U); dut.io.aAddress.poke(0x1000.U)
      dut.io.aCorrupt.poke(false.B)
      dut.io.dOpcode.poke(8.U); dut.io.dSource.poke(0.U); dut.io.dSize.poke(6.U)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      dut.clock.step(); dut.io.anyFailed.expect(false.B)
    }
    def a(opcode: Int, size: Int = 0, source: Int = 0): Unit = {
      dut.io.aOpcode.poke(opcode.U); dut.io.aSize.poke(size.U)
      dut.io.aSource.poke(source.U); dut.io.aValid.poke(true.B)
      dut.clock.step(); dut.io.aValid.poke(false.B); dut.clock.step()
    }
    def d(opcode: Int = 8, size: Int = 6, source: Int = 0): Unit = {
      dut.io.dOpcode.poke(opcode.U); dut.io.dSize.poke(size.U)
      dut.io.dSource.poke(source.U); dut.io.dValid.poke(true.B)
      dut.clock.step(); dut.io.dValid.poke(false.B); dut.clock.step()
    }
    def rejected(bit: Int): Unit = {
      dut.io.anyFailed.expect(true.B)
      assert(dut.io.failed.peek().litValue.testBit(bit), s"expected monitor category $bit")
    }
    for (opcode <- 12 to 14; size <- Seq(0, 6)) {
      clear(); a(opcode, size); d(); dut.io.anyFailed.expect(false.B)
    }
    clear()
    dut.io.aValid.poke(true.B); dut.io.dValid.poke(true.B)
    dut.clock.step(); dut.io.aValid.poke(false.B); dut.io.dValid.poke(false.B)
    dut.clock.step(); dut.io.anyFailed.expect(false.B)
    clear(); a(4); d(1, 0); dut.io.anyFailed.expect(false.B) // ordinary Get
    println("PASS: all three CMO opcodes, scalar/line sizes, same-cycle CBOAck, ordinary Get")

    for (opcode <- Seq(8, 9, 10, 11, 15)) {
      clear(); a(opcode); rejected(0)
    }
    for (opcode <- Seq(3, 7, 9, 10, 11, 12, 13, 14, 15)) {
      clear(); d(opcode); rejected(0)
    }
    clear(); a(12); a(13); rejected(1)
    clear(); d(); rejected(2)
    clear(); a(12); d(0); rejected(3)
    clear(); a(4); d(8); rejected(3)
    clear(); a(12); d(8, 0); rejected(4)
    clear(); a(4); d(1, 6); rejected(4)
    clear(); a(12, source = 3); rejected(5)
    clear(); a(12, size = 1); rejected(5)
    clear(); dut.io.aAddress.poke(0x1001.U); a(12); rejected(5)
    clear(); dut.io.aCorrupt.poke(true.B); a(12); rejected(5)
    println("PASS: reserved A/D opcodes, reused/orphan source, wrong response/size, CMO source/size/alignment/corrupt are rejected")
  }
}
