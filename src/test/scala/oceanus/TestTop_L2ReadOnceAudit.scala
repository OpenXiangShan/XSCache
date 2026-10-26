package oceanus

// Single-request ReadOnce response audit. All probes are passive; the HN uses
// normal credit-controlled CHI and returns both 256-bit beats for every read.
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable

object ReadOnceAuditReflect {
  def field[T](owner: AnyRef, name: String): T = {
    var cls: Class[_] = owner.getClass
    while (cls != null) {
      try { val f = cls.getDeclaredField(name); f.setAccessible(true); return f.get(owner).asInstanceOf[T] }
      catch { case _: NoSuchFieldException => cls = cls.getSuperclass }
    }
    throw new NoSuchFieldException(name)
  }
}

class TestTopL2ReadOnceAudit(cfg: L2Configuration)(implicit p: Parameters) extends TestTop_L2Top(cfg) {
  val fields = mutable.ArrayBuffer.empty[(String, UInt)]
  def add(name: String, x: Data): Unit = fields += name -> BoringUtils.bore(x).asUInt
  for ((slice, bank) <- module.slices.zipWithIndex;
       (t, i) <- ReadOnceAuditReflect.field[Seq[L2TSHR]](slice, "tshrs").zipWithIndex) {
    val v = ReadOnceAuditReflect.field[oceanus.l2.tshr.L2VPipeREQ](t, "vPipeREQ")
    val prefix = s"s$bank.t$i."
    for (n <- Seq("tshr_valid", "tshr_alloc", "tshr_dealloc", "tshr_buffer_wen_RXDAT_0", "tshr_buffer_wen_RXDAT_2"))
      add(prefix + n, ReadOnceAuditReflect.field[Bool](t, n))
    for (n <- Seq("rxreq_fire", "rxreq_satisfied_readonce", "rxreq_unsatisfied_readonce", "expect_dn_rd_comp_and_data",
      "w_rd_dn_data0", "w_rd_dn_data2", "w_rd_dn_comp", "sched_up_rd_compdata", "w_s_rd_up_compdata0",
      "w_s_rd_up_compdata2", "s_rd_up_compdata0", "s_rd_up_compdata2", "w_rd_up_compack", "w_ds_resp",
      "active", "dn_rxdat_compdata")) add(prefix + n, ReadOnceAuditReflect.field[Bool](v, n))
    add(prefix + "p_opcode", ReadOnceAuditReflect.field[UInt](v, "p_rxreq_opcode"))
    add(prefix + "reqOpcode", v.io.UpRXREQ.bits.Opcode)
    add(prefix + "reqTxn", v.io.UpRXREQ.bits.TxnID)
    add(prefix + "dirRead", t.io.fromDir.DirRdResp)
    add(prefix + "dirHit", t.io.fromDir.META.hit)
    add(prefix + "dirState", t.io.fromDir.META.state)
    add(prefix + "rxValid", t.io.DnRXDAT.valid)
    add(prefix + "buf0", v.io.tbuf_data0_valid)
    add(prefix + "buf2", v.io.tbuf_data2_valid)
    add(prefix + "free", v.io.free)
  }
  val boundary = IO(Output(UInt(fields.map(_._2.getWidth).sum.W)))
  boundary := Cat(fields.map(_._2).reverse.toSeq)
  val widths = fields.map { case (n, v) => n -> v.getWidth }.toVector
}

object TestTop_L2ReadOnceAudit extends App with ChiselSim {
  Constantin.init(false)
  val hit = args.contains("--hit")
  val control = args.contains("--control")
  val upstream = new L2UpstreamTable(Seq(L2UpstreamTableEntry(L2UpstreamPortType.Type1, 0)))
  val cfg = new L2Configuration(8, true, 0 until 1, upstream = upstream, bopEnable = false,
    pfTrainEnable = true, pfTlbEnable = true)
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.simulators.verilator(
    verilatorSettings = svsim.verilator.Backend.CompilationSettings.default.withParallelism(
      Some(svsim.verilator.Backend.CompilationSettings.Parallelism.Uniform.default.withNum(2))))
  simulate(new TestTopL2ReadOnceAudit(cfg)(params)) { dut =>
    val address = BigInt(0x80000)
    val original = UpRequestSpec(MasterId.t1p0, 7, 0, 0, CCHIOpcode.ReadOnce.opcode, address, expCompData = true)
    val spec = CaseSpec("readonce-minimal", UpConfig(Seq(MasterId.t1p0),
      ExplicitRequestStream(Seq(original), Seq(BigInt(CHIFieldResp.CompData_SC))), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(80), FixedOutstanding(64), ImmediateCreditPolicy()), 6000, (0, 6000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec)
    val dn = new L2TopDownstreamModel(dut, spec)
    val data = mutable.Queue.empty[PendingDnComp]
    val acknowledgements = mutable.Queue.empty[(UpDBID, Int)]
    val returned = mutable.Map.empty[BigInt, Set[Int]].withDefaultValue(Set.empty)
    var cycle = 0; var requests = 0; var hnBeats = 0; var accepted = 0
    var last = Map.empty[String, BigInt]
    def flag(x: Bool): Boolean = x.peek().litToBoolean
    def value(x: UInt): BigInt = x.peek().litValue
    def row(): Map[String, BigInt] = {
      var bits = value(dut.boundary)
      dut.widths.map { case (n, w) => val v = bits & ((BigInt(1) << w) - 1); bits >>= w; n -> v }.toMap
    }
    def tick(): Unit = {
      up.clearResponses(); dn.clearResponses()
      acknowledgements.headOption.foreach { case (dbid, target) =>
        up.driveUpCompAck(dbid, target)
        if (flag(dut.io.t1p.head.UpRSP.ready)) {
          println(s"READONCE_EVENT cycle=$cycle event=UP_COMPACK dbid=${dbid.v} target=$target")
          acknowledgements.dequeue()
        }
      }
      data.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
        dn.driveDnData(p); data.dequeue(); dn.noteResponse(p.dnTxnID, p.beat == 1, cycle); hnBeats += 1
        println(s"READONCE_EVENT cycle=$cycle event=HN_COMPDATA txn=${p.dnTxnID.v} dataID=${p.beat * 2} target=${p.srcId} addr=0x${p.addr.toString(16)} credit=${dn.canDriveRxDat}")
      }
      val port = dut.io.t1p.head
      if (flag(port.UpREQ.valid) && flag(port.UpREQ.ready)) {
        accepted += 1
        println(s"READONCE_EVENT cycle=$cycle event=UP_ACCEPT txn=${value(port.UpREQ.bits.TxnID)} opcode=${value(port.UpREQ.bits.Opcode)} addr=0x${value(port.UpREQ.bits.Addr).toString(16)}")
      }
      if (flag(dut.io.chi.txreq.flitv)) {
        val f = dut.io.chi.txreq.flit
        val id = DnTxnID(value(f.TxnID.get)); val src = value(f.SrcID.get); val addr = value(f.Addr.get)
        assert(!dn.isEvictReq(value(f.Opcode.get)), "unexpected eviction in single-line test")
        requests += 1; dn.noteRequest(id)
        println(s"READONCE_EVENT cycle=$cycle event=L2_DN_READ txn=${id.v} opcode=${value(f.Opcode.get)} src=$src addr=0x${addr.toString(16)}")
        for (b <- 0 until 2) data.enqueue(PendingDnComp(id, src, cycle + 80 + b, b, BigInt(CHIFieldResp.CompData_SC), addr))
      }
      if (flag(port.DnDAT.valid) && flag(port.DnDAT.ready)) {
        val f = port.DnDAT.bits; val txn = value(f.TxnID); val beat = value(f.DataID).toInt
        assert(value(f.Data) == dn.referenceData(address, beat), "upstream data mismatch")
        assert(!returned(txn)(beat), "duplicate upstream beat")
        returned(txn) += beat
        println(s"READONCE_EVENT cycle=$cycle event=L2_UP_COMPDATA txn=$txn dataID=$beat dbid=${value(f.DBID)}")
        if (returned(txn).size == 2) acknowledgements.enqueue(UpDBID(value(f.DBID)) -> value(f.SrcID).toInt)
      }
      if (flag(port.DnRSP.valid)) println(s"READONCE_EVENT cycle=$cycle event=L2_UP_RSP opcode=${value(port.DnRSP.bits.Opcode)} txn=${value(port.DnRSP.bits.TxnID)}")
      if (flag(dut.io.chi.txrsp.flitv)) println(s"READONCE_EVENT cycle=$cycle event=L2_DN_RSP opcode=${value(dut.io.chi.txrsp.flit.Opcode.get)} txn=${value(dut.io.chi.txrsp.flit.TxnID.get)}")
      val now = row()
      val changes = now.toSeq.sortBy(_._1).filter { case (n, v) => last.get(n).forall(_ != v) }
      if (changes.nonEmpty) println(s"READONCE_STATE cycle=$cycle " + changes.map { case (n, v) => s"$n=$v" }.mkString(" "))
      last = now
      try dn.stepWithCredits(cycle)
      catch { case t: Throwable =>
        println(s"READONCE_ABORT cycle=$cycle accepted=$accepted dnRequests=$requests hnBeats=$hnBeats targetBeats=${returned(BigInt(7)).size} hnPending=${data.size} state=${last.toSeq.sortBy(_._1).mkString(",")}")
        throw t
      }
      cycle += 1
    }
    def run(n: Int): Unit = for (_ <- 0 until n) tick()
    def request(req: UpRequestSpec): Unit = {
      up.driveRead(req)
      var waited = 0
      while (!flag(dut.io.t1p.head.UpREQ.ready)) { tick(); waited += 1; assert(waited < 500) }
      tick(); up.clearUpReq()
    }
    up.initInputs(); dn.initInputs()
    dut.log.dump.poke(false); dut.log.clean.poke(false)
    dut.reset.poke(true); dut.clock.step(8); dut.reset.poke(false)
    dn.activateRxLink()
    for (w <- -300 until 0) dn.stepWithCredits(w)
    println(s"READONCE_BEGIN head=${sys.props.getOrElse("oceanus.c5.head", "unknown")} hit=$hit control=$control address=0x${address.toString(16)} timeout=4000")
    if (hit) {
      request(original.copy(txnId = 6, opcode = CCHIOpcode.ReadShared.opcode))
      run(200)
      assert(returned(BigInt(6)).size == 2 && acknowledgements.isEmpty && data.isEmpty, "ReadShared setup failed")
    }
    request(if (control) original.copy(opcode = CCHIOpcode.ReadShared.opcode) else original)
    run(4000)
    println(s"READONCE_RESULT head=${sys.props.getOrElse("oceanus.c5.head", "unknown")} hit=$hit control=$control cycles=$cycle accepted=$accepted dnRequests=$requests hnBeats=$hnBeats targetBeats=${returned(BigInt(7)).size} hnPending=${data.size} ackPending=${acknowledgements.size} state=${last.toSeq.sortBy(_._1).mkString(",")}")
    assert(returned(BigInt(7)).size == 2, "L2 did not return both ReadOnce data beats within 4000 cycles")
  }
}
