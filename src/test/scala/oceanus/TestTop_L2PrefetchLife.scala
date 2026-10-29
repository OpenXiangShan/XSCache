package oceanus

// L1 directed stimuli use real producers. Coverage is host-only observation;
// metadata is never forced, and every hit follows a checked protocol outcome.
import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import chisel3.util.experimental.BoringUtils
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable

class L1LifeTop(cfg:L2Configuration)(implicit p:Parameters) extends TestTopPhase2(cfg) {
  val freshDepth=IO(Output(Vec(cfg.sliceNum,Vec(nMSHR,UInt(2.W)))))
  module.slices.zipWithIndex.foreach { case(slice,bank)=> slice.tshrs.zip(freshDepth(bank)).foreach { case(t,out)=>
    out:=BoringUtils.bore(t.io.fromDir.META.cdpPfDepth)
  }}
}
object TestTop_L2PrefetchLife extends App with ChiselSim {
  Constantin.init(false)
  val producer = args.find(_.startsWith("--source=")).map(_.drop(9)).getOrElse("recv")
  val source = Map("recv" -> 1, "vbop" -> 2, "pbop" -> 3, "cdp" -> 9)(producer)
  val variants = Seq("consume", "resident", "after-demand", "late", "snoop", "evict",
    "shared-hit", "unique-hit", "unique-dirty", "stash-miss", "queue", "lineclass", "snoop-preserve", "recv-labels", "cdp-collision")
  val requestedSlices = args.find(_.startsWith("--slices=")).map(_.drop(9).toInt).getOrElse(1)
  require(Set(1,2,4)(requestedSlices))
  val selected = args.find(_.startsWith("--case=")).map(_.drop(7))
  require(requestedSlices==1 || selected.forall(variants.take(10).contains))
  val cfg = new L2Configuration(8, true, 0 until requestedSlices, bopEnable = true,
    pfRecvEnable = true, pfCtrlEnable = true, pfTrainEnable = true, pfTlbEnable = true,
    vbopStubEnable = true, cdpStubEnable = true,
    bopParams = OceanusBOPParameters(badScore = 0, roundMax = 100000),
    vbopParams = OceanusVBOPParameters(badScore = 0, roundMax = 100000))
  val params = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(replacement = "plru", mshrSize = 8, ways = 4, sets = 128)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = PFSimulator.bounded
  simulate(new L1LifeTop(cfg)(params)) { dut =>
    val slices = cfg.sliceNum
    for (variant <- variants if selected.fold(variants.take(10).contains(variant))(name =>
        if(name=="stash-all")Set("shared-hit","unique-hit","unique-dirty","stash-miss","snoop-preserve")(variant) else name==variant)) {
      val cov = new PFCoverage(s"life-$producer-$variant")
      val queueCoverage = new PFQueueCoverage(dut.phase2, s"life-queues-$producer-$variant")
      val target = Iterator.from(0).map(i => BigInt(0x20080) + i*64).find(a => L2TopBOPExpectations.slice(a,slices)==0).get
      val root = if (producer == "cdp") BigInt(0x50440) else target - 128
      val pointers = mutable.Map.empty[BigInt, BigInt]
      if (producer == "cdp") pointers(root) = target ^ BigInt(0x1000000)
      def memory(addr: BigInt, beat: Int): BigInt =
        if (beat == 0) pointers.getOrElse(addr, BigInt(0)) else ((addr << 64) | 0x12345678)
      val spec = CaseSpec("life", UpConfig(Seq(MasterId.t1p0), ExplicitRequestStream(Seq.empty, Seq.empty), SnoopPolicy.SnpRespI),
        DnConfig(FixedLatency(25), FixedOutstanding(64), ImmediateCreditPolicy()), 20000, (0, 20000), Seq.empty)
      val up = new L2TopUpstreamModel(dut, spec)
      val dn = new L2TopDownstreamModel(dut, spec)
      val data = mutable.Queue.empty[PendingDnComp]
      val rsp = mutable.Queue.empty[PendingDnRsp]
      val ack = mutable.Queue.empty[PendingUpAck]
      val snps = mutable.Queue.empty[PendingSnpRsp]
      val active = mutable.Map.empty[BigInt, BigInt]
      val down = mutable.ArrayBuffer.empty[BigInt]
      val entries = mutable.ArrayBuffer.empty[(BigInt, Int)]
      val reads = mutable.ArrayBuffer.empty[(BigInt, Boolean, BigInt)]
      val depthReads=mutable.ArrayBuffer.empty[(BigInt,BigInt)]
      val writes = mutable.ArrayBuffer.empty[(BigInt, Int, Int)]
      val translations = mutable.Queue.empty[(BigInt, Int)]
      val scanStarts = mutable.ArrayBuffer.empty[(BigInt,Boolean)]
      val scanLines = mutable.ArrayBuffer.empty[BigInt]
      var snoopOpcode = 7
      var releaseTxn: Option[BigInt] = None
      var collisionHalves=BigInt(0)
      val selectedHalves=mutable.Queue.empty[(BigInt,Boolean,BigInt)]
      var cycle = 0; var txn = 0; var checkedBeats = 0
      var snoopSent = 0; var snoopDone = 0
      var slowTarget = false
      var sendSnoop = false
      var dirtyUpgrade = false
      def flag(x: Bool): Boolean = x.peek().litToBoolean
      def value(x: UInt): BigInt = x.peek().litValue
      def controls(enabled: Boolean = false): Unit = {
        val c = dut.io.pfCtrl.get
        c.l2_pf_master_en.poke(true.B); c.l2_pf_recv_en.poke(true.B)
        c.l2_pbop_en.poke((enabled && producer == "pbop").B)
        c.l2_vbop_en.poke((enabled && producer == "vbop").B)
        c.l2_cdp_en.poke((enabled && producer == "cdp").B)
      }
      def perf(metric: Int): BigInt = value(dut.observation.bopPerfCountBySliceSource(0)(source)(metric))
      def tick(): Unit = {
        assert(cycle < 20000, s"life watchdog $producer/$variant down=$down active=$active")
        up.clearResponses(); dn.clearResponses()
        ack.headOption.foreach { a => up.driveUpCompAck(a.upDBID, a.sliceIdx); ack.dequeue() }
        if (!flag(dut.io.t1p.head.UpRSP.valid)) snps.headOption.foreach { s =>
          up.driveUpSnpResp(s.snpTxnID, s.tgtId); snps.dequeue()
        }
        if (sendSnoop && dn.canDriveRxSnp) {
          val f = dut.io.chi.rxsnp.flit
          dut.io.chi.rxsnp.flitv.poke(true.B); dut.io.chi.rxsnp.flitpend.poke(true.B)
          f.poke(0.U.asTypeOf(f)); f.Opcode.get.poke(snoopOpcode.U)
          f.DoNotGoToSD_DoNotDataPull.get.poke(3.U); f.Addr.get.poke((target >> 3).U)
          f.TxnID.get.poke((snoopSent + 1).U); f.SrcID.get.poke(0.U)
          snoopSent += 1; sendSnoop = false
        }
        if (flag(dut.io.chi.txrsp.flitv) && value(dut.io.chi.txrsp.flit.Opcode.get) == 1) snoopDone += 1
        data.dequeueFirst(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
          dn.driveDnData(p); dut.io.chi.rxdat.flit.Data.get.poke(memory(p.addr, p.beat).U)
          dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
        }
        rsp.dequeueFirst(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
          dn.driveDnRsp(p); dn.noteResponse(p.dnTxnID, true, cycle)
        }
        // Ordered three-cycle external translation, with a nonidentity mapping.
        val tlb = dut.io.pfTlb.get
        tlb.req.ready.poke(true.B); tlb.resp.valid.poke(false.B)
        tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits)); tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
        translations.headOption.filter(_._2 <= cycle).foreach { case (va, _) =>
          translations.dequeue(); tlb.resp.valid.poke(true.B)
          tlb.resp.bits.paddr.head.poke((va ^ BigInt(0x1000000)).U)
        }
        if (flag(tlb.req.valid)) translations.enqueue((value(tlb.req.bits.vaddr), cycle + 3))
        dut.phase2.enter.foreach { e => if (flag(e.valid) && value(e.bits.SrcID) == cfg.pfSrcID)
          entries += ((value(e.bits.Addr), (value(e.bits.TxnID) >> 4).toInt)) }
        dut.phase2.write.foreach(w => if (flag(w.valid)) writes += ((value(w.addr), value(w.source).toInt, value(w.depth).toInt)))
        dut.observation.dirProbeBySlice.zipWithIndex.foreach { case(bankProbes,bank) => bankProbes.zipWithIndex.foreach { case(p,i) =>
          if (flag(p.dirRdResp)) {
            reads += ((value(p.tshrPaddr), flag(p.fromDirMetaHit), value(p.fromDirMetaPfSource)))
            depthReads += ((value(p.tshrPaddr),value(dut.freshDepth(bank)(i))))
          }
        }}
        if (flag(dut.io.chi.txreq.flitv)) {
          val f = dut.io.chi.txreq.flit
          val id = DnTxnID(value(f.TxnID.get)); val src = value(f.SrcID.get)
          val op = value(f.Opcode.get); val addr = value(f.Addr.get)
          dn.noteRequest(id)
          if (dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id, src, cycle + 25, dn.downstreamEvictRspOpcode(op)))
          else if (op == 0x0c) {
            down += addr
            rsp.enqueue(PendingDnRsp(id, src, cycle+25, BigInt(L2TopDownstreamOpcodes.Comp)))
          } else {
            down += addr
            val latency = if (slowTarget && addr == target) 300 else 25
            val resp = if (dirtyUpgrade && addr == target && op == 7) CHIFieldResp.CompData_UD_PD
              else if (op == 7) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC
            for (b <- 0 until 2) data.enqueue(PendingDnComp(id, src, cycle + latency + b, b, resp, addr))
          }
        }
        val port = dut.io.t1p.head
        if (flag(port.DnDAT.valid)) {
          val f = port.DnDAT.bits; val id = value(f.TxnID); val addr = active(id)
          val beat = value(f.DataID).toInt
          assert(value(f.Data) == memory(addr, beat)); checkedBeats += 1
          if (beat == 1) {
            active.remove(id)
            ack.enqueue(PendingUpAck(0, UpDBID(value(f.DBID)), UpTxnID(id), cycle + 1, value(f.SrcID).toInt))
          }
        }
        if (flag(port.DnSNP.valid)) snps.enqueue(PendingSnpRsp(SnpTxnID(value(port.DnSNP.bits.TxnID)), value(port.DnSNP.bits.SrcID), cycle + 1))
        if (flag(port.DnRSP.valid)) {
          assert((variant == "lineclass" || variant == "snoop-preserve") && value(port.DnRSP.bits.Opcode) == CCHIOpcode.Comp.opcode)
          val id=value(port.DnRSP.bits.TxnID)
          if (releaseTxn.contains(id)) releaseTxn=None
          else {
            assert(active.remove(id).nonEmpty,s"unexpected Comp id=$id active=$active release=$releaseTxn")
            ack.enqueue(PendingUpAck(0,UpDBID(value(port.DnRSP.bits.DBID)),UpTxnID(id),cycle+1,value(port.DnRSP.bits.SrcID).toInt))
          }
        }
        if (variant == "lineclass" || variant == "cdp-collision") {
          val obs=dut.cdpObservation.get
          if(variant=="cdp-collision") {
            assert(value(obs.collisions(0))==collisionHalves && value(obs.drops(0))==0)
            assert(flag(obs.data.valid)==selectedHalves.nonEmpty)
            if(selectedHalves.nonEmpty){
              val (a,h,d)=selectedHalves.dequeue()
              assert(value(obs.data.bits.paddr)==a && flag(obs.data.bits.high)==h && value(obs.data.bits.data)==d)
            }
          }
          val emitted=mutable.ArrayBuffer.empty[BigInt]
          obs.req.flatten.foreach { p =>
            if (flag(p.start.valid)) scanStarts += ((value(p.start.bits.paddr),flag(p.start.bits.isHit)))
            if (flag(p.line.valid)) {
              val a=value(p.line.bits.paddr)
              assert(value(p.line.bits.data) == (memory(a,0) | (memory(a,1)<<256)))
              scanLines += a;emitted+=a
            }
          }
          if(variant=="cdp-collision" && emitted.nonEmpty){
            collisionHalves+=(emitted.size-1)*2
            for(h<-0 until 2)selectedHalves.enqueue((emitted.head,h==1,memory(emitted.head,h)))
          }
        }
        for (bank <- 0 until slices; m <- L2BOPPerf.names.indices)
          assert(value(dut.observation.bopPerfCountBySlice(bank)(m)) == (0 to 9).map(s => value(dut.observation.bopPerfCountBySliceSource(bank)(s)(m))).sum)
        queueCoverage.sample()
        dn.stepWithCredits(cycle); cycle += 1
      }
      def drain(ignoreQueues: Boolean = false): Unit = {
        var quiet = 0
        while (quiet < 35) {
          tick()
          val idle = releaseTxn.isEmpty && active.isEmpty && data.isEmpty && rsp.isEmpty && ack.isEmpty && snps.isEmpty && translations.isEmpty &&
            (ignoreQueues || dut.observation.prefetchPendingBySource.flatten.forall(x => value(x) == 0)) &&
            dut.observation.dirProbeBySlice.flatten.forall(p => !flag(p.valid) && !flag(p.reqActive))
          quiet = if (idle) quiet + 1 else 0
        }
      }
      def release(addr: BigInt): Unit = {
        val evt=dut.io.t1p.head.UpEVT
        releaseTxn=Some(BigInt(txn));txn+=1
        evt.bits.poke(0.U.asTypeOf(evt.bits));evt.bits.Opcode.poke(CCHIOpcode.Evict.U)
        evt.bits.TxnID.poke(releaseTxn.get.U);evt.bits.Addr.poke(addr.U);evt.valid.poke(true.B)
        while(!flag(evt.ready))tick()
        tick();evt.valid.poke(false.B);drain()
        assert(releaseTxn.isEmpty)
      }
      def request(addr: BigInt, op: BigInt = CCHIOpcode.ReadShared.opcode, wait: Boolean = true): Unit = {
        val stash = op == CCHIOpcode.StashShared.opcode || op == CCHIOpcode.StashUnique.opcode
        val id = txn; txn += 1
        up.driveRead(UpRequestSpec(MasterId.t1p0, id, 0, 0, op, addr, expCompData = !stash && op != CCHIOpcode.MakeUnique.opcode))
        dut.io.pfTrain.get.head.vaddrBlock.poke(((addr ^ BigInt(0x1000000)) >> 6).U)
        dut.io.pfTrain.get.head.trainEnable.poke(true.B)
        while (!flag(dut.io.t1p.head.UpREQ.ready)) tick()
        if (!stash) active(BigInt(id)) = addr
        tick(); up.clearUpReq()
        if (wait) drain()
      }
      def readback(addr: BigInt, expected: Int): Unit = {
        val start = reads.size;val depthStart=depthReads.size
        request(addr, CCHIOpcode.StashShared.opcode)
        assert(reads.drop(start).toSeq == Seq((addr, true, BigInt(expected))), s"fresh SRAM $producer/$variant ${reads.drop(start)} expected=$expected")
        val expectedDepth=if(expected==9 && producer=="cdp") 3 else 0
        assert(depthReads.drop(depthStart).toSeq==Seq((addr,BigInt(expectedDepth))),s"fresh SRAM depth $producer/$variant ${depthReads.drop(depthStart)} expected=$expectedDepth")
      }
      def generate(wait: Boolean = true): Unit = {
        val start = entries.size
        controls(true)
        if (producer == "recv") {
          val r = dut.io.pfRecv.get
          r.addr.poke(target.U); r.addr_valid.poke(true.B); tick(); r.addr_valid.poke(false.B)
        } else request(root, wait = false)
        while (!entries.drop(start).contains((target, source)) || (producer == "cdp" && entries.drop(start).count(_._2 == 9) < 3)) tick()
        controls()
        if (wait) drain()
      }
      up.initInputs(); dn.initInputs(); dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
      dut.holdIssue.get.poke(false.B)
      dut.io.pfRecv.get.poke(0.U.asTypeOf(dut.io.pfRecv.get))
      dut.io.pfRecv.get.pf_en.poke(true.B); dut.io.pfRecv.get.pf_source.poke(10.U)
      dut.io.pfCtrl.get.poke(0.U.asTypeOf(dut.io.pfCtrl.get)); controls()
      dut.reset.poke(true.B); dut.clock.step(8); dut.reset.poke(false.B); dn.activateRxLink()
      for (c <- -300 until 0) dn.stepWithCredits(c)
      // C5 adaptation: establish a genuine hot VPN via demand training before
      // using the former stub lifecycle stimuli. Default refill depth is 3.
      if (producer == "cdp") {
        controls(true)
        for (_ <- 0 until 130) request(0x90440)
        controls()
      }
      if (variant == "cdp-collision") {
        require(producer=="pbop" && dut.cdpObservation.nonEmpty && slices==1)
        for(offset<-0 until 40 if collisionHalves==0){
          val a=BigInt(0xc00000+offset*0x100);val b=a+0x80
          val r=dut.io.pfRecv.get
          r.addr.poke(a.U);r.pf_source.poke(9.U);r.addr_valid.poke(true.B)
          tick();r.addr_valid.poke(false.B);drain();readback(a,3)
          request(b,wait=false)
          for(_<-0 until offset)tick()
          request(a,wait=false);drain()
        }
        assert(collisionHalves>0 && selectedHalves.isEmpty,s"no same-slice scan collision in timing sweep; count=$collisionHalves")
        cov.hit("D.collision")
        println(s"L1_CDP_COLLISION pass=true droppedHalves=$collisionHalves winnerPayload=true noCacheBackpressure=true")
      } else if (variant == "recv-labels") {
        require(producer == "pbop")
        for (((raw,mapped),i) <- Seq(8->2,9->3,16->9).zipWithIndex) {
          dut.holdIssue.get.poke(true.B)
          val a=BigInt(0x600000+i*0x1000);val expected=a+128
          val enq=value(dut.observation.prefetchEnqueueBySource(0)(3))
          controls(true);request(a,wait=false)
          while(value(dut.observation.prefetchEnqueueBySource(0)(3))==enq)tick()
          controls();drain(ignoreQueues=true)
          val recvAddr=BigInt(0x800000+i*0x1000)
          dut.io.pfRecv.get.pf_source.poke(raw.U)
          dut.io.pfRecv.get.addr.poke(recvAddr.U);dut.io.pfRecv.get.addr_valid.poke(true.B)
          request(BigInt(0xa00000+i*0x1000),CCHIOpcode.StashShared.opcode,wait=false)
          dut.io.pfRecv.get.addr_valid.poke(false.B);drain(ignoreQueues=true)
          val n=entries.size
          controls(true);dut.holdIssue.get.poke(false.B);drain();controls()
          assert(entries.drop(n).toSeq==Seq((recvAddr,mapped),(expected,3)),s"Recv label=$mapped changed producer priority: ${entries.drop(n)}")
        }
        cov.hit("D.recv_labels")
      } else if (variant == "lineclass") {
        require(producer == "pbop" && dut.cdpObservation.nonEmpty)
        val ops=Seq(CCHIOpcode.ReadShared.opcode,CCHIOpcode.ReadUnique.opcode,
          CCHIOpcode.StashShared.opcode,CCHIOpcode.StashUnique.opcode)
        for ((op,i) <- ops.zipWithIndex) {
          val a=BigInt(0x900000+i*0x100); val n=scanStarts.size;val l=scanLines.size
          request(a,op)
          assert(scanStarts.drop(n).toSeq == Seq((a,false)) && scanLines.drop(l).toSeq == Seq(a))
        }
        val n=scanStarts.size;val l=scanLines.size
        request(BigInt(0x910040),CCHIOpcode.MakeUnique.opcode)
        assert(scanStarts.size==n && scanLines.size==l,"MakeUnique must not establish a data scan")
        generate()
        val h=scanStarts.size;val k=scanLines.size
        request(target)
        assert(scanStarts.drop(h).toSeq==Seq((target,true)) && scanLines.drop(k).toSeq==Seq(target))
        val r=scanStarts.size;val q=scanLines.size
        release(BigInt(0x900000))
        assert(scanStarts.size==r && scanLines.size==q,"independent release must not scan")
        cov.hit("H.lineclass")
      } else if (variant == "snoop-preserve") {
        generate();readback(target,source)
        snoopOpcode=1;sendSnoop=true;val n=snoopDone
        while(sendSnoop || snoopDone==n)tick()
        drain();readback(target,source)
        assert(perf(L2BOPPerf.Useful)==0 && perf(L2BOPPerf.UselessSnoop)==0)
        // A clean external Evict is an idempotent client clear even when the
        // prefetch has no tracked L1 client. It performs a real masked Meta
        // write, and must preserve the independent source/depth fields.
        release(target);readback(target,source)
        assert(perf(L2BOPPerf.Useful)==0 && perf(L2BOPPerf.UselessSnoop)==0 && perf(L2BOPPerf.UselessEvict)==0)
        val beforeDown=down.size
        request(target);readback(target,0)
        assert(down.size==beforeDown && perf(L2BOPPerf.Useful)==1)
        cov.hit(s"F.$producer.snoop_preserve")
        println(s"L1_SNOOP_PRESERVE pass=true producer=$producer source=$source release=true idempotentClientClear=true")
      } else if (variant == "queue") {
        val queueInputs = Iterator.from(0).map(i => BigInt(0x100000) + i*0x100).filter(a => L2TopBOPExpectations.slice(a+128,slices)==0).take(40).toVector
        dut.holdIssue.get.poke(true.B)
        val expectedEnqueues = Array.fill(slices)(0)
        for (i <- 0 until 40) {
          val input = queueInputs(i)
          val predicted = input + 128
          controls(true)
          if (producer == "recv") {
            dut.io.pfRecv.get.addr.poke(predicted.U)
            dut.io.pfRecv.get.addr_valid.poke(true.B)
            request(input, wait=false)
            dut.io.pfRecv.get.addr_valid.poke(false.B)
          } else {
            if (producer == "cdp") pointers(input) = predicted ^ BigInt(0x1000000)
            request(input, wait=false)
          }
          val expectedAddresses = if (producer=="cdp") Seq(predicted,predicted+64,predicted+128) else Seq(predicted)
          expectedAddresses.foreach(a => expectedEnqueues(L2TopBOPExpectations.slice(a,slices)) += 1)
          // Wait for every expected producer output in its routed source queue.
          while ((0 until slices).exists(b => value(dut.observation.prefetchEnqueueBySource(b)(source)) < expectedEnqueues(b))) tick()
          controls(); drain(ignoreQueues=true)
        }
        val capacity = if (producer == "vbop" || producer == "pbop") 16 else 32
        for (bank <- 0 until slices) {
          assert(value(dut.observation.prefetchEnqueueBySource(bank)(source)) == expectedEnqueues(bank))
          assert(value(dut.observation.prefetchQueueOverwriteBySource(bank)(source)) == (expectedEnqueues(bank)-capacity).max(0))
          assert(value(dut.observation.prefetchPendingBySource(bank)(source)) == expectedEnqueues(bank).min(capacity))
        }
        assert(expectedEnqueues.exists(_ > capacity), "queue overflow stimulus vacuous")
        controls(true); dut.holdIssue.get.poke(false.B); drain(); controls()
        cov.hit("B.hold_resume")
      } else if (variant == "resident") {
        request(target, CCHIOpcode.StashShared.opcode)
        val before = down.count(_ == target)
        generate(); readback(target, 0)
        assert(down.count(_ == target) == before && perf(L2BOPPerf.RedundantResident) == 1)
        cov.hit(s"A.$producer.s$slices.redundant_resident")
      } else if (variant == "after-demand") {
        slowTarget = true
        request(target, wait = false)
        while (!down.contains(target)) tick()
        generate(); readback(target, 0)
        assert(perf(L2BOPPerf.RedundantAfterDemand) == 1)
        cov.hit(s"A.$producer.s$slices.redundant_after_demand")
      } else if (variant == "stash-miss") {
        request(target, CCHIOpcode.StashShared.opcode); readback(target, 0)
        assert(perf(L2BOPPerf.Useful) == 0 && perf(L2BOPPerf.IssuedDownstream) == 0)
        val before=down.size;request(target);readback(target,0)
        assert(down.size==before && perf(L2BOPPerf.Useful)==0)
        cov.hit(s"F.$producer.miss")
      } else {
        slowTarget = variant == "late"
        generate(wait = variant != "late")
        if (variant == "late") {
          while (!down.contains(target)) tick()
          request(target); readback(target, 0)
          assert(perf(L2BOPPerf.Late) == 1 && perf(L2BOPPerf.Useful) == 1)
          cov.hit(s"A.$producer.s$slices.late")
        } else {
          val depth = if (producer == "cdp") 3 else 0
          assert(writes.contains((target, source, depth)))
          readback(target, source)
          if(producer=="recv")assert(value(dut.observation.bopTrainFireCount)==0,"Recv must not self-train")
          Seq("train", "issue", "refill", "meta").foreach(e => cov.hit(s"A.$producer.s$slices.$e"))
          if (variant == "consume") {
            val before = down.size
            request(target); readback(target, 0); request(target)
            assert(perf(L2BOPPerf.Useful) == 1 && down.size == before)
            cov.hit(s"A.$producer.s$slices.useful"); cov.hit(s"A.$producer.s$slices.clear")
          } else if (variant == "snoop") {
            for (_ <- 0 until 2) {
              sendSnoop = true; val before = snoopDone
              while (sendSnoop || snoopDone == before) tick()
              drain()
              assert(perf(L2BOPPerf.UselessSnoop) == 1)
            }
            cov.hit(s"A.$producer.s$slices.useless_snoop")
          } else if (variant == "evict") {
            for (a <- Iterator.from(1).map(i => target + BigInt(i)*dut.p(L2ParamsKey).sets*64).filter(a => L2TopBOPExpectations.slice(a,slices)==0).take(dut.p(L2ParamsKey).ways*2).toSeq) request(a,CCHIOpcode.StashShared.opcode)
            assert(perf(L2BOPPerf.UselessEvict) == 1 && perf(L2BOPPerf.UselessSnoop) == 0)
            cov.hit(s"A.$producer.s$slices.useless_evict")
          } else {
            dirtyUpgrade = variant == "unique-dirty"
            val opcode = if (variant == "shared-hit") CCHIOpcode.StashShared.opcode else CCHIOpcode.StashUnique.opcode
            val before = perf(L2BOPPerf.Useful)
            request(target, opcode); readback(target, source)
            assert(perf(L2BOPPerf.Useful) == before)
            val beforeDown=down.size
            request(target);readback(target,0)
            assert(down.size==beforeDown && perf(L2BOPPerf.Useful)==before+1)
            val suffix = Map("shared-hit" -> "shared_hit", "unique-hit" -> "unique_hit", "unique-dirty" -> "unique_dirty")(variant)
            cov.hit(s"F.$producer.$suffix")
          }
        }
      }
      drain(); assert(active.isEmpty)
      queueCoverage.finish()
      println("L1_LIFE_META_ORACLE depthSram=true recvSelfTrainChecked=true")
      if(Set("shared-hit","unique-hit","unique-dirty","stash-miss","snoop-preserve")(variant))
        println("L1_STASH_DATA_ORACLE demandReadback=true")
      cov.finish()
      println(s"L1_LIFE_RESULT pass=true producer=$producer source=$source slices=$slices variant=$variant cycles=$cycle beats=$checkedBeats")
    }
  }
}
