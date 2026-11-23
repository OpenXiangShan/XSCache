// Intent (zy-pf-obs): drive real Recv fills, LP/D hits, replacement and refault.
// Keep directory semantics intact and check the independent lifecycle through
// the normal XSPerf clean/dump endpoint, including a warmup-spanning LP hit.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable

class PrefetchLifecycleTop(cfg: L2Configuration)(implicit p: Parameters) extends TestTop_L2Top(cfg) {
  override protected def attachExtraObservations(): Unit = {
    // Intent (zy-pf-obs): F1 is a PRE-FIFO timestamp. Model the real two-entry
    // source queue and carry the existing train context in this test harness.
    val queues = io.t1p.zipWithIndex.map { case (port, i) =>
      val q = Module(new Queue(new Bundle {
        val request = new oceanus.compactchi.FlitREQ
        val context = new L2PrefetchTrainSideband
      }, 2))
      q.io.enq.valid := port.UpREQ.valid
      q.io.enq.bits.request := port.UpREQ.bits
      q.io.enq.bits.context := io.pfTrain.get(i)
      port.UpREQ.ready := q.io.enq.ready
      module.io.t1p(i).UpREQ.valid := q.io.deq.valid
      module.io.t1p(i).UpREQ.bits := q.io.deq.bits.request
      module.io.pfTrain.get(i) := q.io.deq.bits.context
      q.io.deq.ready := module.io.t1p(i).UpREQ.ready
      q
    }
    L2PrefetchObservation(module, io.t1p.zipWithIndex.map { case (port, i) =>
      L2ObservationArrival(port.UpREQ.fire, port.UpREQ.bits, io.pfTrain.get(i).reqSource)
    }, io.t1p.zipWithIndex.map { case (port, i) =>
      L2ObservationFifo(port.UpREQ.fire, port.UpREQ.bits.Addr, queues(i).io.deq.fire, cfg.upstream.type1(i).nid,
        port.UpREQ.valid && !port.UpREQ.ready)
    })
  }
}

object TestTop_L2PrefetchObservationLifecycle extends App with ChiselSim {
  Constantin.init(false)
  val cfg = new L2Configuration(8, true, 0 until 1, pfSrcID = 3, bopEnable = false,
    pfRecvEnable = true, pfCtrlEnable = true, pfTlbEnable = true, pfTrainEnable = true)
  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 4, sets = 16, ways = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11,
      reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters(WayIndex_Width = 3)
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  simulate(new PrefetchLifecycleTop(cfg)(config)) { dut =>
    val spec = CaseSpec("pfobs-lifecycle", UpConfig(Seq(MasterId.t1p0),
      ExplicitRequestStream(Seq.empty, Seq.empty), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(12), FixedOutstanding(64), ImmediateCreditPolicy()), 20000, (0,20000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec)
    val dn = new L2TopDownstreamModel(dut, spec)
    val data = mutable.Queue.empty[PendingDnComp]
    val rsp = mutable.Queue.empty[PendingDnRsp]
    val ack = mutable.Queue.empty[PendingUpAck]
    val snp = mutable.Queue.empty[PendingSnpRsp]
    val evicted = mutable.Set.empty[BigInt]
    val readsSent = mutable.Set.empty[BigInt]
    val dirReads = mutable.ArrayBuffer.empty[(BigInt, Boolean, BigInt)]
    // Intent (zy-pf-obs): opt-in host-only RBE unique regression. The
    // elaborated L2/observer hardware is identical to the existing lifecycle.
    val rbeUnique = sys.props.get("oceanus.pfobs.rbeUnique").contains("true")
    var cycle = 0
    var txn = 0
    var returned = 0
    var downstream = 0
    def flag(x: Bool) = x.peek().litToBoolean
    def value(x: UInt) = x.peek().litValue
    up.initInputs(); dn.initInputs()
    dut.log.clean.poke(false.B); dut.log.dump.poke(false.B)
    dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get))
    dut.io.pfRecv.get.pf_en.poke(true.B)
    dut.io.pfCtrl.get.poke(0.U.asTypeOf(dut.io.pfCtrl.get))
    dut.io.pfCtrl.get.l2_pf_master_en.poke(true.B)
    dut.io.pfCtrl.get.l2_pf_recv_en.poke(true.B)
    dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B)
    dn.activateRxLink()
    for (i <- -300 until 0) dn.stepWithCredits(i)
    def tick(): Unit = {
      assert(cycle < 20000, "lifecycle watchdog")
      up.clearResponses(); dn.clearResponses()
      ack.headOption.filter(_.due <= cycle).foreach { a => up.driveUpCompAck(a.upDBID, a.sliceIdx); ack.dequeue() }
      if (!flag(dut.io.t1p(0).UpRSP.valid)) snp.headOption.foreach { s =>
        up.driveUpSnpResp(s.snpTxnID, s.tgtId); snp.dequeue()
      }
      data.headOption.filter(d => d.due <= cycle && dn.canDriveRxDat).foreach { d =>
        dn.driveDnData(d); data.dequeue(); dn.noteResponse(d.dnTxnID, d.beat == 1, cycle)
      }
      rsp.headOption.filter(r => r.due <= cycle && dn.canDriveRxRsp).foreach { r =>
        dn.driveDnRsp(r); rsp.dequeue(); dn.noteResponse(r.dnTxnID, true, cycle)
      }
      dut.observation.dirProbeBySlice.flatten.foreach { d =>
        if (flag(d.dirRdResp)) dirReads += ((value(d.tshrPaddr), flag(d.fromDirMetaHit), value(d.fromDirMetaPfSource)))
      }
      if (flag(dut.io.chi.txreq.flitv)) {
        val f = dut.io.chi.txreq.flit
        val id = DnTxnID(value(f.TxnID.get)); val src = value(f.SrcID.get); val op = value(f.Opcode.get)
        val pa = value(f.Addr.get)
        dn.noteRequest(id)
        if (dn.isEvictReq(op)) {
          evicted += pa
          rsp.enqueue(PendingDnRsp(id, src, cycle + 12, dn.downstreamEvictRspOpcode(op)))
        } else {
          readsSent += pa
          downstream += 1
          if (rbeUnique) println(s"PFOBS_DN_READ cycle=$cycle txn=${id.v} address=$pa opcode=$op")
          // Intent (zy-pf-obs): model the real downstream unique response for
          // the DH hit-S path which cancels its pending DS read.
          // CHI ReadUnique is 0x07; compact-CHI ReadUnique is 0x10.
          val resp = if (op == 0x07) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC
          for (b <- 0 until 2) data.enqueue(PendingDnComp(id, src, cycle + 12 + b, b, resp, pa))
        }
      }
      val p = dut.io.t1p(0)
      if (flag(p.DnDAT.valid) && flag(p.DnDAT.ready) && value(p.DnDAT.bits.DataID) == 1) {
        returned += 1
        ack.enqueue(PendingUpAck(0, UpDBID(value(p.DnDAT.bits.DBID)), UpTxnID(value(p.DnDAT.bits.TxnID)),
          cycle + 1, value(p.DnDAT.bits.SrcID).toInt))
      }
      if (flag(p.DnSNP.valid) && flag(p.DnSNP.ready)) snp.enqueue(PendingSnpRsp(
        SnpTxnID(value(p.DnSNP.bits.TxnID)), value(p.DnSNP.bits.SrcID), cycle + 1))
      dn.stepWithCredits(cycle); cycle += 1
    }
    def settle(): Unit = for (_ <- 0 until 100) tick()
    def recv(pa: BigInt): Unit = {
      dut.io.pfRecv.get.addr.poke(pa.U); dut.io.pfRecv.get.pf_source.poke(10.U)
      dut.io.pfRecv.get.addr_valid.poke(true.B); tick(); dut.io.pfRecv.get.addr_valid.poke(false.B); settle()
    }
    def request(pa: BigInt, lp: Boolean, unique: Boolean = false): Unit = {
      val before = returned
      up.driveRead(UpRequestSpec(MasterId.t1p0, txn, 0, 0, (if (unique) CCHIOpcode.ReadUnique else CCHIOpcode.ReadShared).opcode, pa, expCompData = true))
      dut.io.pfTrain.get(0).reqSource.poke((if (lp) 6 else 2).U)
      dut.io.pfTrain.get(0).trainEnable.poke(false.B)
      while (!flag(dut.io.t1p(0).UpREQ.ready)) tick()
      tick(); up.clearUpReq(); txn += 1; settle()
      assert(returned == before + 1, s"request completion pa=$pa lp=$lp")
    }
    def dump(name: String): Unit = {
      println(s"PFOBS_WINDOW $name cycle=$cycle")
      dut.log.dump.poke(true.B); tick(); dut.log.dump.poke(false.B)
    }
    def clean(): Unit = { dut.log.clean.poke(true.B); tick(); dut.log.clean.poke(false.B) }
    val a = BigInt(0x20000)
    recv(a); request(a, true)
    dump("LP_warmup")
    clean()
    dirReads.clear()
    request(a, false); request(a, false)
    assert(dirReads.toSeq == Seq((a, true, BigInt(0)), (a, true, BigInt(0))),
      s"LP must clear directory pfSource while observation retains its history: $dirReads")
    dump("D_after_LP")
    val b = BigInt(0x20400)
    recv(b); request(b, true)
    val before = downstream
    var i = 2
    while (!evicted(b) && i < 50) { recv(a + i * 0x400); i += 1 }
    assert(evicted(b), "LP-only line must be selected as an actual replacement victim")
    val refaultDown = downstream
    request(b, false)
    assert(downstream == refaultDown + 1, "evicted LP-only line must refault")
    dump("LP_eviction_refault")
    // Intent (zy-pf-obs): a demand joins the same-PA RBE while the Recv
    // fill is still in flight, exercising the direct-owner classification.
    val c = BigInt(0x50000)
    dut.io.pfRecv.get.addr.poke(c.U); dut.io.pfRecv.get.pf_source.poke(10.U)
    dut.io.pfRecv.get.addr_valid.poke(true.B); tick(); dut.io.pfRecv.get.addr_valid.poke(false.B)
    while (!readsSent(c)) tick()
    request(c, false, unique = rbeUnique)
    dump("RBE_follower")
    // Intent (Joseph N2 clarification): ReadUnique on the resident shared
    // line remains DH, gets downstream data, and has no DS segment samples.
    if (!rbeUnique) {
    clean()
    dirReads.clear()
    val uniqueDown = downstream
    request(c, false, unique = true)
    assert(dirReads.exists(x => x._1 == c && x._2), "ReadUnique must hit the resident shared line")
    assert(downstream == uniqueDown + 1, "ReadUnique hit-S must obtain unique data downstream")
    dump("DH_ReadUnique_hit_S")
    }
    println(s"PFOBS_LIFECYCLE_RESULT pass=true requests=$txn returned=$returned replacementFills=${downstream-before} cycles=$cycle")
  }
}
