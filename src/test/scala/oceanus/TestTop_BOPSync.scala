package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.{vbopreference => ref}
import org.chipsalliance.cde.config._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import utility._

class PBOPSyncComparison(useDefaults: Boolean = false)(implicit val p: Parameters) extends Module {
  def read[T <: Data](x: T): T = BoringUtils.bore(x)
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val train = Flipped(Decoupled(UInt(48.W)))
    val ready = Input(Bool())
    val sent = Output(Bool())
    val student = Output(Bool())
  })
  val geometry = if(useDefaults) OceanusBOPParameters() else OceanusBOPParameters(scoreBits=3, roundMax=5, delayQueueLatency=32)
  val d = Module(new OceanusBOP(geometry))
  val rg = ref.BOPParameters(virtualTrain=false, badScore=geometry.badScore,
    scoreBits=geometry.scoreBits, roundMax=geometry.roundMax, dQLatency=geometry.delayQueueLatency,
    crossPage=false, useDelayOut=false, enableStudentCover=true, studentPoolSize=8,
    studentFilterTableBits=2048, studentHashMode="bop_rr", studentCovThreshold=5,
    issueGateEnable=true, offsetList=geometry.offsetList)
  val r = Module(new ref.PBestOffsetPrefetch()(p.alterPartial { case ref.VBOPKey => rg }))
  r.io.enable := io.enable
  d.io.enable := io.enable
  r.io.fdbkDegree := 1.U
  r.io.pfCtrlOfDelayLatency := 0.U
  r.io.train.valid := io.train.valid
  r.io.train.bits := 0.U.asTypeOf(r.io.train.bits)
  r.io.train.bits.paddr := io.train.bits
  r.io.resp.valid := false.B
  r.io.resp.bits := 0.U.asTypeOf(r.io.resp.bits)
  r.io.req.ready := io.ready
  d.io.train.valid := io.train.valid
  d.io.train.bits := io.train.bits
  d.io.delayLatency := 0.U
  d.io.req.ready := io.ready
  io.train.ready := d.io.train.ready
  io.sent := d.io.req.fire
  io.student := read(d.student.get.io.selectedEnable)
  println(s"PBOP_GEOMETRY dut=${d.bopParams} reference=${r.bopParams}")
  val a = r.scoreTable
  val b = d.scoreTable
  val studentEqual = read(r.student.get.pool).asUInt === read(d.student.get.pool).asUInt &&
    read(r.student.get.io.selectedOffset) === read(d.student.get.io.selectedOffset) &&
    read(r.student.get.io.selectedEnable) === read(d.student.get.io.selectedEnable)
  when(!reset.asBool) {
    when(d.io.train.ready =/= r.io.train.ready) {
      printf(p"PBOP_READY dut=${d.io.train.ready} ref=${r.io.train.ready} dst=${read(d.scoreTable.io.req.ready)} rst=${read(r.scoreTable.io.req.ready)} dstudent=${read(d.studentTrainReady)} rstudent=${read(r.studentTrainReady)} dskid=${read(d.skid.io.enq.ready)} rskid=${read(r.reqSkidBuffer.io.enq.ready)} dvalid=${read(d.requestValid)} rvalid=${read(r.s1_req_valid)}\n")
    }
    assert(d.io.train.ready === r.io.train.ready, "PBOP train ready cycle")
    assert(d.io.req.valid === r.io.req.valid, "PBOP output valid cycle")
    when(d.io.req.valid) { assert(d.io.req.bits === r.io.req.bits.addr, "PBOP address/order/skid") }
    assert(read(a.st).asUInt === read(b.st).asUInt, "PBOP teacher scores cycle")
    assert(read(a.ptr) === read(b.ptr) && read(a.round) === read(b.round), "PBOP teacher pointer/round cycle")
    assert(read(a.io.phaseEndPulse) === read(b.io.phaseEndPulse), "PBOP teacher phase cycle")
    assert(read(a.io.prefetchOffset) === read(b.io.prefetchOffset), "PBOP offset cycle")
    assert(studentEqual, "PBOP student pool/selection cycle")
  }
}

class BufferedTrainComparison(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val train = Flipped(Vec(4, Valid(new L2PrefetchTrain(true))))
    val ready = Input(Bool())
    val out = Valid(new L2PrefetchTrain(true))
    val admitted = Output(Vec(4, Bool()))
    val pending = Output(Vec(4, Bool()))
  })
  val arb = Module(new L2PrefetchTrainArb(4, true, buffered=true))
  arb.io.in := io.train
  io.out.valid := arb.io.out.valid
  io.out.bits := arb.io.out.bits
  arb.io.out.ready := io.ready
  io.admitted := VecInit(arb.io.admitted.map(_.valid))
  io.pending := VecInit(arb.io.pending.map(_.valid))
  // Exercise delayed R10 retirement with cycles having raw=0 and accepted=1.
  val counts = Module(new L2PrefetchTrainCounters(1, 4, delayedRows=Set(0)))
  counts.io.total := 0.U.asTypeOf(counts.io.total)
  counts.io.accepted := 0.U.asTypeOf(counts.io.accepted)
  counts.io.total(0)(0) := PopCount(io.train.map(t => L2PrefetchTrainSelect.enabled(t)))
  counts.io.accepted(0)(0) := arb.io.out.fire
}

object TestTop_BOPSync extends App with ChiselSim {
  Constantin.init(false)
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false, enablePerf=false, fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=false, enablePerfDB=false, perfLevel=XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  if(args.contains("--buffer")) {
    simulate(new BufferedTrainComparison()(params)) { d =>
      d.io.ready.poke(false)
      for(t <- d.io.train) { t.valid.poke(false); t.bits.poke(0.U.asTypeOf(t.bits)) }
      d.reset.poke(true); d.clock.step(4); d.reset.poke(false)
      val expected = Array.fill[Option[BigInt]](4)(None)
      val random = new scala.util.Random(300515)
      var admitted, retired, overflow, collisions = 0
      def bit(b: Bool) = b.peek().litToBoolean
      for(cycle <- 0 until 6000) {
        val draining = cycle >= 5800
        val ready = cycle < 64 || draining || random.nextInt(5) != 0
        d.io.ready.poke(ready)
        val offered = Array.tabulate(4)(i => !draining && (if(cycle<64) i==0 else random.nextInt(3)==0))
        val enabled = Array.tabulate(4)(i => cycle < 64 || random.nextInt(7)!=0)
        for(i <- 0 until 4) {
          d.io.train(i).valid.poke(offered(i))
          d.io.train(i).bits.trainEnable.get.poke(enabled(i))
          d.io.train(i).bits.paddr.poke((cycle*256+i*64).U)
        }
        val old = expected.clone()
        val retiring = if(bit(d.io.out.valid) && ready) old.indexWhere(_.contains(d.io.out.bits.paddr.peek().litValue)) else -1
        for(i <- 0 until 4) {
          assert(bit(d.io.pending(i)) == old(i).nonEmpty)
          assert(bit(d.io.admitted(i)) == (offered(i) && enabled(i) && (old(i).isEmpty || retiring==i)))
          if(offered(i) && enabled(i) && old(i).nonEmpty && retiring!=i) overflow += 1
        }
        val live = old.count(_.nonEmpty)
        if(live>1) collisions += live-1
        if(bit(d.io.out.valid) && ready) {
          val address = d.io.out.bits.paddr.peek().litValue
          val owner = old.indexWhere(_.contains(address))
          assert(owner >= 0, s"duplicated or non-pending train $address")
          expected(owner)=None; retired += 1
        }
        for(i <- 0 until 4 if bit(d.io.admitted(i))) {
          expected(i)=Some(BigInt(cycle*256+i*64)); admitted += 1
        }
        d.clock.step()
      }
      assert(expected.forall(_.isEmpty) && admitted==retired)
      assert(overflow>0 && collisions>0)
      println(s"BOP_BUFFER_RESULT pass=true admitted=$admitted retired=$retired overflow=$overflow arb_wait=$collisions cycles=6000")
    }
  } else {
    simulate(new PBOPSyncComparison(args.contains("--defaults"))(params)) { d =>
      d.io.enable.poke(true);d.io.train.valid.poke(false);d.io.train.bits.poke(0);d.io.ready.poke(true)
      d.reset.poke(true);d.clock.step(4);d.reset.poke(false);d.clock.step(300)
      val random=new scala.util.Random(300515)
      var trained, sent, takeovers=0
      var address=BigInt(0x200000)
      for(cycle <- 0 until 30000) {
        val draining=cycle>=29000
        d.io.enable.poke(cycle<9000 || cycle>=9500)
        d.io.ready.poke(draining || random.nextInt(6)!=0)
        d.io.train.valid.poke(!draining && cycle%3!=0)
        d.io.train.bits.poke(address)
        if(d.io.train.ready.peek().litToBoolean && !draining && cycle%3!=0) {
          trained+=1
          if(d.io.student.peek().litToBoolean) takeovers+=1
          address=BigInt(0x200000)+((address-BigInt(0x200000)+128)%4096)
        }
        if(d.io.sent.peek().litToBoolean) sent+=1
        d.clock.step()
      }
      assert(trained>1000 && sent>100 && takeovers>0)
      println(s"PBOP_SYNC_RESULT pass=true cycles=30000 trains=$trained sent=$sent student_trains=$takeovers reference=300515bc")
    }
  }
}
