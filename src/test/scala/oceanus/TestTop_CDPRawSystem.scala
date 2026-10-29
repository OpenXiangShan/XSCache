package oceanus
import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import scala.collection.mutable
import java.nio.file.{Files,Paths}
import java.io.{PrintWriter,FileOutputStream}
import java.util.zip.GZIPOutputStream
import utility._
object TestTop_CDPRawAudit extends App with ChiselSim {
  Constantin.init(false)
  def opt(n: String, default: Int): Int = args.find(_.startsWith(n + "=")).map(_.split("=", 2)(1).toInt).getOrElse(default)
  val seeds = opt("--seeds", 3); val slices = opt("--slices", 1)
  val conflictMode=args.contains("--conflicts")
  val csrMode=args.contains("--csr")
  val off = sys.props.get("oceanus.validation.prefetchOff").contains("true")
  val traceCHI = sys.props.get("oceanus.m3.traceCHI").contains("true")
  val upstream=if(conflictMode) new L2UpstreamTable(Seq(0,2,6,7).map(n=>L2UpstreamTableEntry(L2UpstreamPortType.Type1,n)) ++
    Seq(4,5).map(n=>L2UpstreamTableEntry(L2UpstreamPortType.Type4,n))) else L2UpstreamTable.defaults
  val cfg = new L2Configuration(8, true, 0 until slices, upstream=upstream,bopEnable = false, prefetchIssueEnable = !off,
    pfRecvEnable = !off, pfCtrlEnable = !off, pfTrainEnable = !off, pfTlbEnable = !off,
    vbopStubEnable = !off, cdpStubEnable = !off,
    vbopParams = OceanusVBOPParameters(badScore = 0, roundMax = 100000))
  implicit val simulator: HasSimulator = CDPAuditSimulator.bounded
  simulate(new TestTopCDPRawAudit(cfg)(CDPC5Params.apply)) { dut =>
    val directory = Paths.get(sys.props.getOrElse("oceanus.c5.trace","/data/zhuy/pf-audit-tests/traces")); Files.createDirectories(directory)
    val tracePath = directory.resolve(s"system-$slices-$seeds.jsonl.gz")
    val trace = new PrintWriter(new GZIPOutputStream(new FileOutputStream(tracePath.toFile), 8192, traceCHI))
    trace.println(ujson.Obj("head" -> sys.props.getOrElse("oceanus.c5.head","402504254fe4fffbd4e6b80785e52b973c6d4c9e"), "slices" -> slices,
      "seeds" -> seeds, "structuralPrefetchOff" -> off, "widths" -> ujson.Arr.from(dut.widths.map { case (n, w) => ujson.Arr(n, w) })).render())
    val dummy = UpRequestSpec(MasterId.t1p0, 0, 0, 0, CCHIOpcode.ReadShared.opcode, BigInt(0), expCompData = true)
    val spec = CaseSpec("cdp-c5", UpConfig(Seq(MasterId.t1p0), ExplicitRequestStream(Seq(dummy), Seq(BigInt(CHIFieldResp.CompData_SC))), SnoopPolicy.SnpRespI),
      DnConfig(FixedLatency(80), FixedOutstanding(64), ImmediateCreditPolicy()), 2000000, (0, 2000000), Seq.empty)
    val up = new L2TopUpstreamModel(dut, spec); val dn = new L2TopDownstreamModel(dut, spec)
    val data = mutable.Queue.empty[PendingDnComp]; val rsp = mutable.Queue.empty[PendingDnRsp]
    val ack = mutable.Queue.empty[PendingUpAck]; val snoop = mutable.Queue.empty[PendingSnpRsp]
    val replies = mutable.Queue.empty[(Int, BigInt, Boolean, Boolean)]
    val attempts = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    val writes = mutable.ArrayBuffer.empty[(BigInt, Int, Int)]
    val emitted = mutable.ArrayBuffer.empty[(BigInt, Int)]
    val admitted = mutable.ArrayBuffer.empty[(BigInt, Int)]
    val useful = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    val seenDownstream = mutable.Set.empty[BigInt]
    var cycle = 0; var txn = 0; var epoch = -1; var beats = 0; var checkedDemandRequests = 0
    var gate = true; var activePA = BigInt(0); var reset = false
    var allCandidates = 0; var allDrops = 0; var hits = 0; var unused = 0; var contention = 0
    var activeSeed = 0
    var responseDelay=0
    var holdExternalTLB=false
    var residualRetryAfterReopen=0L
    var holdAcks=false
    var holdEvictComp=false
    val evictTransactions=mutable.Set.empty[(Int,BigInt)]
    var evictCompletions=0L
    var competingDemand:Option[BigInt]=None
    var evictionRaceIssued=0L
    var disabledHalves=0L
    val filterImage=mutable.Map.empty[Int,BigInt]
    var filterQuery:Option[Int]=None
    var filterMemoryWrites=0L
    val acceptedPorts=mutable.Set.empty[Int]
    val demandAddresses=mutable.Map.empty[(Int,BigInt),BigInt]
    // ReadOnce returns data without an upstream CompAck (including Type-1 audit traffic).
    val demandOpcodes=mutable.Map.empty[(Int,BigInt),BigInt]
    val demandBeats=mutable.Map.empty[(Int,BigInt),Set[Int]].withDefaultValue(Set.empty)
    val portAcks=Seq.fill(cfg.upstream.type1.size)(mutable.Queue.empty[(BigInt,BigInt)])
    def flag(b: Bool) = b.peek().litToBoolean
    def value(b: UInt) = b.peek().litValue
    def rowSourcePending:BigInt=(0 until slices).map(b=>value(dut.observation.prefetchPendingBySource(b)(9))).sum
    def pa(va: BigInt): BigInt = if ((va >> 12) >= 0x300 && (va >> 12) < 0x340) va + 0x300000
      else if ((va >> 12) == 0x200) va - 0x1c0000 else va + 0x1000000
    // Byte-exact independent memory image. Every seed has eight pointers;
    // both halves/four lanes participate, [1:0]=0 and bit2=1 is intentional.
    def memory(addr: BigInt, beat: Int): BigInt = {
      if(addr>=0x48000 && addr<0x49000) {
        val index=((addr-0x48000)>>6).toInt
        return (0 until 4).map { i=>
          val page=0x300+((index*8+beat*4+i)%64)
          BigInt(page*4096+(((index+beat*4+i)*7)%60)*64+4)<<(i*64)
        }.sum
      }
      val index = ((addr - 0x40000) >> 6).toInt
      if(index >= 0 && index < seeds + 2) {
        val random = new scala.util.Random(0xC50000L + index)
        val page = 0x300 + random.nextInt(32)
        val words = Vector.fill(8)(BigInt(page * 4096 + random.nextInt(60) * 64 + 4))
        words.slice(beat * 4, beat * 4 + 4).zipWithIndex.map { case (v, i) => v << (i * 64) }.sum
      } else BigInt(0)
    }
    val reference=new CDPRawEventReference(slices,memory _,dut.nMSHR,dut.p(L2ParamsKey).sets)
    val identities=new CDPCandidateLedger(allowRetainedDisabledOutput=csrMode)
    val tlbOwnership=new CDPTlbOwnershipLedger
    val sourceOwnership=Seq.fill(slices)(mutable.Queue.empty[(BigInt,Int,Long)])
    var currentGeneration=0L
    var residualSourceRetired=0L
    var newOwnershipDuringResidual=0L
    def controls(c: Boolean = true, v: Boolean = false): Unit = {
      dut.io.pfCtrl.foreach { ctrl =>
      ctrl.l2_pf_master_en.poke(true); ctrl.l2_pf_recv_en.poke(true)
      ctrl.l2_cdp_en.poke(c); ctrl.l2_vbop_en.poke(v); ctrl.l2_pbop_en.poke(false)
      }
    }
    def observe():Map[String,BigInt]={
      var bits=value(dut.boundary)
      dut.widths.map {case(n,w)=>val item=bits&((BigInt(1)<<w)-1);bits>>=w;n->item}.toMap
    }
    def tick(): Map[String, BigInt] = {
      acceptedPorts.clear()
      up.clearResponses(); dn.clearResponses()
      dut.io.t1p.foreach(_.UpRSP.valid.poke(false))
      dut.io.t1p.foreach(_.DnRSP.ready.poke(!holdEvictComp))
      for((queue,index)<-portAcks.zipWithIndex) queue.headOption.filter(_=> !holdAcks).foreach {case(dbid,target)=>
        if (traceCHI && cycle >= 23180) println(s"M3_UPACK cycle=$cycle portType=Type1 port=$index SrcID=${cfg.upstream.type1(index).nid} TxnID=$dbid TgtID=$target")
        val p=dut.io.t1p(index).UpRSP
        p.valid.poke(true);p.bits.poke(0.U.asTypeOf(p.bits));p.bits.TxnID.poke(dbid);p.bits.SrcID.poke(cfg.upstream.type1(index).nid)
        p.bits.TgtID.poke(target);p.bits.Opcode.poke(CCHIOpcode.CompAck.opcode)
      }
      ack.headOption.foreach { a => up.driveUpCompAck(a.upDBID, a.sliceIdx); ack.dequeue() }
      if(!flag(dut.io.t1p.head.UpRSP.valid)) snoop.headOption.foreach { a => up.driveUpSnpResp(a.snpTxnID, a.tgtId); snoop.dequeue() }
      data.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
        dn.driveDnData(p); dut.io.chi.rxdat.flit.Data.get.poke(memory(p.addr, p.beat)); data.dequeue()
        dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
      }
      rsp.headOption.filter(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
        dn.driveDnRsp(p); rsp.dequeue(); dn.noteResponse(p.dnTxnID, true, cycle)
      }
      // Sample acknowledgement fires only after all port valids are driven;
      // an earlier per-port ready peek can change when another port joins
      // the same slice arbiter, losing an acknowledgement in the host model.
      for((queue,index)<-portAcks.zipWithIndex) {
        val p=dut.io.t1p(index).UpRSP
        if(queue.nonEmpty && !holdAcks && flag(p.valid) && flag(p.ready))queue.dequeue()
      }
      // RXRSP completion and a resident hit have independent physical inputs.
      // Launch one cycle before the eviction write from its real pending response.
      competingDemand.filter(_ => rsp.headOption.exists(p=>p.due<=cycle+4)).foreach { addr =>
        val index=1;val q=dut.io.t1p(index).UpREQ
        if(!flag(q.valid)) {
          q.valid.poke(true);q.bits.poke(0.U.asTypeOf(q.bits));q.bits.SrcID.poke(cfg.upstream.type1(index).nid)
          q.bits.TxnID.poke(txn%256);q.bits.Opcode.poke(CCHIOpcode.ReadShared.opcode);q.bits.Size.poke(6)
          q.bits.MemAttr.poke(5);q.bits.ExpCompData.poke(true);q.bits.Addr.poke(addr)
          dut.io.pfTrain.foreach { t => t(index).trainEnable.poke(true);t(index).reqSource.poke(0)
            t(index).vaddrBlock.poke(BigInt(0x200040)>>6) }
          txn+=1;competingDemand=None;evictionRaceIssued+=1
        }
      }
      dut.io.pfTlb.foreach { tlb =>
      tlb.req.ready.poke(gate); tlb.resp.valid.poke(false)
      tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits)); tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      replies.headOption.filter(_ => !holdExternalTLB).filter(_._1 <= cycle).foreach { case (_, va, miss, fault) =>
        replies.dequeue(); tlb.resp.valid.poke(true); tlb.resp.bits.paddr.head.poke(pa(va))
        tlb.resp.bits.miss.poke(miss); tlb.resp.bits.excp.head.pf.ld.poke(fault)
      }
      if(gate && flag(tlb.req.valid)) {
        val va = value(tlb.req.bits.vaddr); attempts(va >> 12) += 1
        val kind = if(epoch >= 0) activeSeed % 7 else 0
        val miss = kind == 1 && attempts(va >> 12) % 2 == 1 || kind == 2
        val fault = kind == 3
        replies.enqueue((cycle + 1 + activeSeed % 7 + responseDelay, va, miss, fault))
      }
      }
      if(flag(dut.io.chi.txreq.flitv)) {
        val f = dut.io.chi.txreq.flit
        val id = DnTxnID(value(f.TxnID.get)); val src = value(f.SrcID.get)
        val op = value(f.Opcode.get); val addr = value(f.Addr.get); dn.noteRequest(id)
        if(dn.isEvictReq(op)) rsp.enqueue(PendingDnRsp(id, src, cycle + 80, dn.downstreamEvictRspOpcode(op)))
        else {
          seenDownstream += addr
          val response = if(op == 0x07) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC
          for(b <- 0 until 2) data.enqueue(PendingDnComp(id, src, cycle + 80 + b, b, BigInt(response), addr))
        }
      }
      for((port,index)<-dut.io.t1p.zipWithIndex) {
        if(flag(port.DnDAT.valid) && flag(port.DnDAT.ready)) {
          val f=port.DnDAT.bits;val beat=value(f.DataID).toInt;val key=index->value(f.TxnID)
          if (traceCHI && cycle >= 23180) println(s"M3_UPDATA cycle=$cycle portType=Type1 port=$index TxnID=${value(f.TxnID)} DBID=${value(f.DBID)} SrcID=${value(f.SrcID)} DataID=$beat Resp=${value(f.Resp)}")
          val addr=demandAddresses.getOrElse(key,throw new AssertionError(s"unknown demand response $key"))
          assert(value(f.Data)==memory(addr,beat),s"cache data mismatch key=$key PA=$addr beat=$beat")
          assert(!demandBeats(key)(beat));demandBeats(key)+=beat;beats+=1
          if(demandBeats(key).size==2) {
            val opcode = demandOpcodes.remove(key).getOrElse(throw new AssertionError(s"missing request opcode $key"))
            if (opcode != CCHIOpcode.ReadOnce.opcode) portAcks(index).enqueue(value(f.DBID)->value(f.SrcID))
            demandBeats.remove(key);demandAddresses.remove(key)
          }
        }
        if(flag(port.DnSNP.valid)) snoop.enqueue(PendingSnpRsp(SnpTxnID(value(port.DnSNP.bits.TxnID)),value(port.DnSNP.bits.SrcID),cycle+1))
        if(flag(port.DnRSP.valid) && flag(port.DnRSP.ready)) {
          val key=index->value(port.DnRSP.bits.TxnID)
          assert(evictTransactions.remove(key) && value(port.DnRSP.bits.Opcode)==CCHIOpcode.Comp.opcode,s"unexpected nondata response port=$index key=$key")
          evictCompletions+=1
        }
        if(flag(port.UpREQ.valid) && flag(port.UpREQ.ready)) {
          acceptedPorts+=index;checkedDemandRequests+=1
          val p=port.UpREQ;val ctx=dut.io.pfTrain.map(_(index));val key=index->value(p.bits.TxnID)
          if (traceCHI && cycle >= 23180) println(s"M3_UPREQ cycle=$cycle portType=Type1 port=$index Opcode=${value(p.bits.Opcode)} SrcID=${value(p.bits.SrcID)} TxnID=${value(p.bits.TxnID)} Addr=0x${value(p.bits.Addr).toString(16)} ExpCompData=${flag(p.bits.ExpCompData)} ExpCompAck=absent-in-CCHI")
          assert(!demandAddresses.contains(key));demandAddresses(key)=value(p.bits.Addr)
          assert(!demandOpcodes.contains(key));demandOpcodes(key)=value(p.bits.Opcode)
          reference.upstream(value(p.bits.SrcID),value(p.bits.TxnID),value(p.bits.Addr),
            ctx.map(c => (value(c.vaddrBlock)>>6<<12)|(value(p.bits.Addr)&0xfc0)).getOrElse(value(p.bits.Addr)&0xfc0),
            ctx.map(c => value(c.reqSource).toInt).getOrElse(0),ctx.exists(c => flag(c.trainEnable)))
        }
      }
      for(port<-dut.io.t4p.map(_.UpREQ)) if(flag(port.valid) && flag(port.ready))
        reference.upstream(value(port.bits.SrcID),value(port.bits.TxnID),value(port.bits.Addr),value(port.bits.Addr)&0xfc0,0,false)
      val packed = value(dut.boundary)
      var rest = packed
      val row = dut.widths.map { case (n, w) => val v = rest & ((BigInt(1) << w) - 1); rest >>= w; n -> v }.toMap
      assert(rest == 0)
      if(row.getOrElse("filterResponse",BigInt(0))!=0) filterQuery.foreach(i=>filterImage(i)=row("filterResponse.masked"))
      filterQuery=if(row.getOrElse("filterQuery",BigInt(0))!=0) Some(row("filterQuery.set").toInt) else None
      filterMemoryWrites+=row.getOrElse("filterMemoryWrites",BigInt(0)).toLong
      if(row("enable")==0) {
        assert(row("tlbRequest")==0,"M4 disabled new CDP client admission")
        if(row("half")!=0) {assert(row("halfAccepted")==0 && row("halfRejected")>0,"M4 captured half not rejected");disabledHalves+=1}
      }
      reference.sample(dut,row,cycle)
      if (!off) tlbOwnership.sample(row,cycle,currentGeneration)
      val outputGeneration=if(row("out")!=0)Some(identities.retirementGeneration(value(dut.audit.outputIndex).toInt)) else None
      identities.sample(dut.audit,cycle)
      trace.println(ujson.Arr(cycle, epoch, gate, packed.toString(16)).render())
      if (traceCHI && cycle >= 23180) trace.flush()
      if(row("candidate") != 0) allCandidates += 1
      allDrops += row("full").toInt + row("duplicate").toInt + row("filterDrop").toInt + (0 until 16).map(i => row(s"translationDrop$i").toInt).sum
      contention += row("vbopBlockedCDP").toInt
      if(row("out") != 0) {
        if(!csrMode)assert(row("enable") != 0, "CSR disabled CDP emitted")
        emitted += ((row("out.pa"), row("out.depth").toInt))

      }
      if(csrMode && !off) for(b<-0 until slices) {
        val q=sourceOwnership(b)
        assert(q.size==value(dut.observation.prefetchPendingBySource(b)(9)),s"M4 source9 occupancy bank=$b")
        if(row(s"source9deq$b")!=0 || row(s"source9overwrite$b")!=0) {
          val item=q.dequeue()
          if(row("enable")==0)assert(item._3<=currentGeneration,"M4 disabled source9 belongs to future generation")
          if(row(s"source9deq$b")!=0) {
            assert(item._1==row(s"source9deq$b.pa") && item._2==((row(s"source9deq$b.txn")>>2)&3).toInt,"M4 source9 FIFO identity changed")
            if(row(s"enter$b")!=0 && (row(s"enter$b.txn")>>4)==9) {
              assert(row(s"enter$b.pa")==item._1,"M4 source9 issue changed residual owner")
              if(item._3<currentGeneration)residualSourceRetired+=1
            }
          }
        }
        if(row(s"source9enq$b")!=0) {
          assert(outputGeneration.nonEmpty && row("out.pa")==row(s"source9enq$b.pa"),"M4 source9 enqueue without owned candidate output")
          q.enqueue((row(s"source9enq$b.pa"),((row(s"source9enq$b.txn")>>2)&3).toInt,outputGeneration.get))
        }
      }
      for(bank <- 0 until slices) {
        if (off) assert(row(s"enter$bank")==0,s"structural-off PF REQ entered slice $bank")
        if(row(s"enter$bank") != 0 && (row(s"enter$bank.txn") >> 4) == 9) {
          val addr = row(s"enter$bank.pa"); val depth = ((row(s"enter$bank.txn") >> 2) & 3).toInt
          assert(emitted.contains((addr, depth)), "source9/depth TxnID without CDP producer")
          admitted += ((addr, depth))

        }
        for(i <- 0 until dut.nMSHR) {
          if(row(s"evict$bank.$i") != 0 && row(s"evict$bank.$i.source") == 9) {
            assert(emitted.exists(_._1 == row(s"evict$bank.$i.pa")), "unused victim was never issued by CDP")
            assert(useful(row(s"evict$bank.$i.pa")) == 0, "used CDP line reported unused")
            unused += 1
          }
        }
      }
      if(row("filter") != 0 && row("filter.used") != 0 && row("filter.source") == 9) {
        useful(row("filter.pa")) += 1; hits += 1
      }
      dn.stepWithCredits(cycle); cycle += 1
      if(evictionRaceIssued>0 && acceptedPorts(1)) dut.io.t1p(1).UpREQ.valid.poke(false)
      if(cycle%2048==0 && (conflictMode || csrMode)) println(s"CDP_AUDIT_PROGRESS cycle=$cycle demands=$txn beats=$beats addresses=$demandAddresses beatMasks=$demandBeats portAcks=${portAcks.map(_.size)} data=${data.size} rsp=${rsp.size} snoop=${snoop.size} replies=${replies.size} rawCoverage=${reference.coverage}")
      assert(cycle < 60000, "audit watchdog")
      row
    }
    def run(n: Int): Unit = for(_ <- 0 until n) tick()
    def drain(): Unit = {
      var quiet = 0
      val began=cycle
      while(quiet < 35) {
        assert(cycle-began<4000,s"audit drain watchdog demand=$demandAddresses masks=$demandBeats ack=${portAcks.map(_.size)} data=$data rsp=$rsp snoop=$snoop replies=$replies")
        val row = tick()
        val idle = data.isEmpty && rsp.isEmpty && ack.isEmpty && portAcks.forall(_.isEmpty) && demandAddresses.isEmpty && snoop.isEmpty && replies.isEmpty &&
          row("resident") == 0 && row("candidatePending") == 0 &&
          dut.observation.dirProbeBySlice.flatten.forall(p => !flag(p.valid) && !flag(p.reqActive))
        quiet = if(idle) quiet + 1 else 0
      }
    }
    def request(addr: BigInt, va: BigInt, enable: Boolean = true, opcode: Int = CCHIOpcode.ReadShared.opcode): Unit = {
      activePA = addr
      up.driveRead(dummy.copy(txnId = txn % 256, addr = addr, opcode = opcode,memAttr=5))
      dut.io.pfTrain.foreach { t => t.head.vaddrBlock.poke(va >> 6);t.head.trainEnable.poke(enable) }
      while(!flag(dut.io.t1p.head.UpREQ.ready)) tick()
      tick(); up.clearUpReq(); txn += 1
    }
    up.initInputs(); dn.initInputs(); dut.log.dump.poke(false); dut.log.clean.poke(false)
    for(port<-dut.io.t1p.drop(1)) {
      port.UpREQ.valid.poke(false);port.UpREQ.bits.poke(0.U.asTypeOf(port.UpREQ.bits))
      port.UpEVT.valid.poke(false);port.UpEVT.bits.poke(0.U.asTypeOf(port.UpEVT.bits))
      port.UpDAT.valid.poke(false);port.UpDAT.bits.poke(0.U.asTypeOf(port.UpDAT.bits))
      port.UpRSP.valid.poke(false);port.UpRSP.bits.poke(0.U.asTypeOf(port.UpRSP.bits))
      port.DnDAT.ready.poke(true);port.DnRSP.ready.poke(true);port.DnSNP.ready.poke(true)
    }
    dut.holdIssue.foreach(_.poke(false))
    dut.io.pfRecv.foreach(x => x.poke(0.U.asTypeOf(x)))
    dut.io.pfCtrl.foreach(x => x.poke(0.U.asTypeOf(x))); controls()
    dut.io.pfTrain.foreach(_.foreach(x => x.poke(0.U.asTypeOf(x))))
    dut.io.pfTlb.foreach { tlb =>
      tlb.req.ready.poke(true);tlb.resp.valid.poke(false)
      tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits));tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
    }
    dut.reset.poke(true); dut.clock.step(8); dut.reset.poke(false); dn.activateRxLink()
    for(w <- -300 until 0) dn.stepWithCredits(w)
    for(_ <- 0 until 130) { request(0x3e040, 0x200040); drain() }
    for(seed <- 0 until seeds) {
      epoch = seed; activeSeed = seed; attempts.clear()
      val before = allCandidates; val outBefore = emitted.size
      gate = false
      request(BigInt(0x40000 + seed * 64), BigInt(0x200000 + seed * 64))
      // Both real refill halves must arrive before releasing translation.
      run(155); gate = true; drain()
      if (!off) assert(allCandidates > before, s"seed $seed detection was empty")
      if(seed < 2 && emitted.size > outBefore) {
        val (addr, _) = emitted(outBefore)
        assert(admitted.exists(_._1 == addr) && seenDownstream(addr))
        // Actual directory hit/old source -> used feedback, with CSR enabled.
        request(addr, addr - 0x300000); drain()
        assert(useful(addr) == 1, s"missing actual used feedback PA=$addr")
      }
      trace.flush()
      if(seed % 50 == 0) println(s"CDP_C5_PROGRESS slices=$slices seed=$seed cycles=$cycle candidates=$allCandidates outputs=${emitted.size}")
    }
    // Preserve an unused genuine CDP line, then evict it with actual same-set
    // demand replacements. The victim PA must come from the old host Meta.
    epoch = seeds; activeSeed = 0; attempts.clear()
    val unusedTarget = if (off) BigInt(0x40000) else emitted.map(_._1).find(a => useful(a) == 0).get
    def bank(addr: BigInt): Int = if(slices == 1) 0 else if(slices == 2) (addr >> 6).bitCount % 2 else {
      val low = (6 until 48 by 2).count(addr.testBit) % 2
      val high = (7 until 48 by 2).count(addr.testBit) % 2
      low + 2 * high
    }
    val victimsBefore = unused
    val conflicts = (1 to 128).map(i => unusedTarget + i * dut.p(L2ParamsKey).sets * 64).filter(a => bank(a) == bank(unusedTarget)).take(10)
    for(addr <- conflicts) { request(addr, 0x200040); drain() }
    if (!off) assert(unused > victimsBefore, "no unused genuine CDP victim feedback")
    // Occupy all shared translation slots with real VBOP trains. A subsequent
    // pointer line waits alongside VBOP; production assertion checks priority.
    epoch = seeds + 1; gate = false; controls(v = true)
    // Keep VBOP translations outside the pointer-memory image. Otherwise
    // VBOP refills can start new detections while D1 sends are draining, whose
    // different fixed/variable latency changes duplicate residency (not just
    // output order). Pointer detection below still competes for shared slots.
    for(i <- 0 until 12) { request(BigInt(0xb0000 + i * 64), BigInt(0x280040 + i * 4096)); run(120) }
    request(BigInt(0x40000 + seeds * 64), BigInt(0x200000 + seeds * 64)); run(155)
    gate = true; drain(); controls()
    if (!off) assert(contention > 0, "shared TLB slot contention was empty")
    epoch = seeds + 2
    val disabled = emitted.size; controls(c = false)
    request(0x90000, 0x200040); drain()
    assert(emitted.size == disabled, "CSR off emitted a request")
    if (!off) assert(emitted.nonEmpty && admitted.nonEmpty && hits > 0)
    assert(beats == txn * 2, "demand data scoreboard did not close")
    if(csrMode && !off) {
      controls();drain()
      def transient(row:Map[String,BigInt]):BigInt=row("resident")+row("candidatePending")+row("tlbSlots")+row("outputPending")+
        row("filterQueueCount")+row("vpnQueueCount")+(0 until slices).map(b=>row(s"halfCount$b")).sum+rowSourcePending
      val cases=Seq("retry","slots8","waitingResult","outputBackpressure","halfQueue","source9Queue")
      for((name,index)<-cases.zipWithIndex) {
        epoch=1000+index;activeSeed=if(name=="retry")1 else 0;attempts.clear();controls()
        responseDelay=if(name=="waitingResult")250 else 0
        gate=name!="slots8"
        dut.holdIssue.get.poke(name=="source9Queue")
        val rejectedBefore=disabledHalves
        request(BigInt(0x48000+index*64),0x200040)
        var row=observe();var waited=0
        def triggered(r:Map[String,BigInt]):Boolean=name match {
          case "retry"=>r("tlbRetry")>0
          case "slots8"=>r("tlbSlots")==8
          case "waitingResult"=>r("tlbWaiting")>0
          case "outputBackpressure"=>r("outputValid")>0
          case "halfQueue"=>(0 until slices).exists(b=>r(s"halfCount$b")>0)
          case "source9Queue"=>rowSourcePending>0
        }
        while(!triggered(row) && waited<1200) {tick();row=observe();waited+=1}
        assert(triggered(row),s"M4 trigger absent state=$name row=$row")
        val retainedIDs=identities.closeGeneration()
        tlbOwnership.closeGeneration(currentGeneration)
        controls(c=false);row=tick()
        if(name=="outputBackpressure")assert(row("outputValid")>0 && row("outputReady")==0,"M4 no actual output backpressure")
        val atClose=transient(row);val imageBefore=filterImage.toMap;val vpnBefore=row("vpnValids")
        val candidatesBefore=identities.counters;val physicalWritesBefore=filterMemoryWrites
        val vpnSnapshotBefore=row("vpnSnapshot")
        def sourceLedger:Vector[BigInt]=Seq(dut.observation.prefetchEnqueueBySource,dut.observation.prefetchEnterBySource,
          dut.observation.prefetchDedupBySource,dut.observation.prefetchQueueOverwriteBySource,dut.observation.prefetchPendingBySource)
          .map(x=>(0 until slices).map(b=>value(x(b)(9))).sum).toVector
        val sourceBefore=sourceLedger
        gate=true;responseDelay=0
        // Retained page retries/results may complete; no new admission is permitted.
        if(Set("retry","slots8","waitingResult")(name)) {
          gate=false;holdExternalTLB=true
          run(3)
        } else run(350)
        row=tick()
        val retained=transient(row);val vpnAfter=row("vpnValids")
        val candidatesAfter=identities.counters;val sourceAfter=sourceLedger
        assert(sourceAfter(0)-sourceBefore(0)==sourceAfter(1)-sourceBefore(1)+sourceAfter(2)-sourceBefore(2)+sourceAfter(3)-sourceBefore(3)+sourceAfter(4)-sourceBefore(4),"M4 source9 conservation")
        assert(candidatesAfter("allocated")-candidatesBefore("allocated")==
          candidatesAfter("output")-candidatesBefore("output")+candidatesAfter("filterDrop")-candidatesBefore("filterDrop")+
          candidatesAfter("translationDrop")-candidatesBefore("translationDrop")+candidatesAfter("pending")-candidatesBefore("pending"),"M4 retirement conservation")
        val transientFields=Seq("resident","candidatePending","tlbSlots","tlbRetry","tlbWaiting","tlbDone","outputPending","filterQueueCount","vpnQueueCount")
        println(s"CDP_CSR_ACCOUNT state=$name candidatesBefore=$candidatesBefore candidatesAfter=$candidatesAfter sourceBefore=${sourceBefore.mkString(",")} sourceAfter=${sourceAfter.mkString(",")} transient=${transientFields.map(k=>s"$k:${row(k)}").mkString(",")} halfRejected=${disabledHalves-rejectedBefore} vpnSnapshotUnchanged=${vpnSnapshotBefore==row("vpnSnapshot")} filterPhysicalWrites=${filterMemoryWrites-physicalWritesBefore}")
        val filterAfter=filterImage.toMap
        val tableMode=if(imageBefore==filterAfter) "retained-observed-sets" else "changed-observed-sets"
        println(s"CDP_CSR_CASE state=$name trigger=1 atClose=$atClose beforeReopen=$retained disabledHalfRejected=${disabledHalves-rejectedBefore} vpnBefore=$vpnBefore vpnAfter=$vpnAfter filter=$tableMode filterSets=${imageBefore.size}")
        // Residuals are allowed by the revised contract. Keep their old IDs
        // alive while admitting fresh pointer lines after re-enable.
        currentGeneration+=1;identities.reopenGeneration()
        val beforeFresh=identities.ownershipCounters("freshAllocated")
        val oldTlbBefore=tlbOwnership.counters("oldRetired")
        val retryBefore=tlbOwnership.counters("retries")
        controls();dut.holdIssue.get.poke(false);gate=true;holdExternalTLB=false
        request(BigInt(0x48000+(index+8)*64),0x200040)
        run(130);drain()
        val fresh=identities.ownershipCounters("freshAllocated")-beforeFresh
        if(name=="retry") {
          assert(tlbOwnership.counters("oldRetired")>oldTlbBefore,"M4 no pre-disable TLB identity completed after reopening")
          assert(tlbOwnership.counters("retries")>retryBefore,"M4 no retained retry after reopening")
          residualRetryAfterReopen+=1
        }
        assert(fresh>0,s"M4 no new candidate generation state=$name")
        if(retained>0)newOwnershipDuringResidual+=1
        println(s"CDP_CSR_OWNERSHIP state=$name oldGeneration=${currentGeneration-1} newGeneration=$currentGeneration retainedIDs=${retainedIDs.size} residualBeforeReopen=$retained freshAllocated=$fresh candidate=${identities.ownershipCounters} tlb=${tlbOwnership.counters} residualSourceRetired=$residualSourceRetired")
        row=tick();assert(transient(row)==0,s"M4 failed to drain after reopening state=$name")
        identities.checkConservation()
      }
      assert(disabledHalves>0,"M4 no captured-half rejection observed")
      println(s"CDP_CSR_RESULT exercised=${cases.size} disabledHalfRejected=$disabledHalves residualAllowed=true freshWithResidual=$newOwnershipDuringResidual candidateOwnership=${identities.ownershipCounters} tlbOwnership=${tlbOwnership.counters} residualSourceRetired=$residualSourceRetired accounting=true")
    }
    if (csrMode && off) {
      // No CSR/TLB/candidate storage exists. Keep each original pointer-demand
      // address and disabled/reopen timing, with protocol/data/drain checks.
      for ((name,index) <- Seq("retry","slots8","waitingResult","outputBackpressure","halfQueue","source9Queue").zipWithIndex) {
        epoch=1000+index;activeSeed=if(name=="retry")1 else 0;attempts.clear()
        request(BigInt(0x48000+index*64),0x200040);run(1200)
        controls(c=false);run(if(Set("retry","slots8","waitingResult")(name))3 else 350)
        controls();request(BigInt(0x48000+(index+8)*64),0x200040);run(130);drain()
        println(s"CDP_CSR_OFF_CASE state=$name retainedOwnership=inapplicable demandScoreboard=true drain=true")
      }
    }
    if(conflictMode) {
      // Actual upstream ports create independent buffered request lifecycles.
      // Concurrent repeated hits build capture pressure without forcing any DUT event.
      def burst(round: Int,negative: Int = -1,base: BigInt = BigInt(0x60000)): Unit = {
        val pending=mutable.Set.from(dut.io.t1p.indices)
        for(i<-dut.io.t1p.indices) {
          val port=dut.io.t1p(i).UpREQ;val addr=base+((round*4+i)%64)*64
          port.valid.poke(true);port.bits.poke(0.U.asTypeOf(port.bits))
          port.bits.SrcID.poke(cfg.upstream.type1(i).nid);port.bits.TxnID.poke(txn%256)
          port.bits.Opcode.poke(if(negative==0) CCHIOpcode.ReadOnce.opcode else CCHIOpcode.ReadShared.opcode)
          port.bits.Size.poke(6);port.bits.MemAttr.poke(5);port.bits.ExpCompData.poke(true);port.bits.Addr.poke(addr)
          dut.io.pfTrain.foreach { t => val ctx=t(i);ctx.vaddrBlock.poke((BigInt(0x200000)|(addr&0xfc0))>>6)
            ctx.reqSource.poke(if(negative==1) MemReqSource.L1DataPrefetch.id else 0);ctx.trainEnable.poke(negative!=2) }
          txn+=1
        }
        while(pending.nonEmpty) {
          tick();acceptedPorts.toVector.foreach {i=>dut.io.t1p(i).UpREQ.valid.poke(false);pending-=i}
        }
      }
      controls()
      def portRequest(index:Int,addr:BigInt):Unit={
        val q=dut.io.t1p(index).UpREQ
        q.valid.poke(true);q.bits.poke(0.U.asTypeOf(q.bits));q.bits.SrcID.poke(cfg.upstream.type1(index).nid)
        q.bits.TxnID.poke(txn%256);q.bits.Opcode.poke(CCHIOpcode.ReadShared.opcode);q.bits.Size.poke(6)
        q.bits.MemAttr.poke(5);q.bits.ExpCompData.poke(true);q.bits.Addr.poke(addr)
        dut.io.pfTrain.foreach { t => t(index).trainEnable.poke(true);t(index).reqSource.poke(0)
          t(index).vaddrBlock.poke(BigInt(0x200040)>>6) };txn+=1
        do tick() while(!acceptedPorts(index))
        q.valid.poke(false)
      }
      def evict(index:Int,addr:BigInt):Unit={
        val e=dut.io.t1p(index).UpEVT;val id=BigInt(txn%256);txn+=1
        evictTransactions+=index->id
        e.valid.poke(true);e.bits.poke(0.U.asTypeOf(e.bits));e.bits.SrcID.poke(cfg.upstream.type1(index).nid)
        e.bits.TxnID.poke(id);e.bits.Addr.poke(addr);e.bits.Opcode.poke(CCHIOpcode.Evict.opcode)
        while(!flag(e.ready))tick();tick();e.valid.poke(false)
      }
      // Two independent release channels: old REQ waits for UpRSP CompAck,
      // old EVT waits for DnRSP Comp acceptance. Both queued REQs already have
      // directory responses; release both blockers at the same edge.
      val first=BigInt(0xa0000)
      val peer=(1 until 128).map(i=>first+i*64).find(a=>bank(a)==bank(first)).get
      request(first,0x200040);drain();portRequest(1,peer);drain()
      holdAcks=true;request(first,0x200040);run(30)
      assert(portAcks(0).nonEmpty,"M3 CompAck blocker absent")
      holdEvictComp=true;evict(1,peer);run(20)
      request(first,0x200040);portRequest(1,peer);run(12)
      holdAcks=false;holdEvictComp=false;drain()
      println("CDP_M3_DUAL_CHANNEL_RELEASE "+reference.coverage.toSeq.sorted.mkString(" "))
      // Align actual victim invalidation (RXSNP-independent RXRSP completion)
      // with an unrelated used-hit event. The reference still predicts both.
      for(i<-0 until 12) {
        val target=emitted.map(_._1).find(a=>useful(a)==0 && reference.directory.values.exists(x=>x._1==a && x._2.source==9))
        target.foreach { victim =>
          val old=reference.coverage("evictions")
          val candidates=(1 to 256).map(j=>victim+j*dut.p(L2ParamsKey).sets*64).filter(a=>bank(a)==bank(victim)).take(12)
          competingDemand=Some(first)
          for(a<-candidates if reference.coverage("evictions")==old) {request(a,0x200040);drain()}
          competingDemand=None
        }
      }
      // Hold real completion acknowledgements, enqueue another request to
      // each same-PA RBE, then release independent upstream CompAck ports
      // together. Multiple old TSHRs can execute their waiting demands at
      // one edge even though the slice accepts only one new request per edge.
      for(r<-0 until 12) {
        holdAcks=true;burst(r);run(180)
        assert(portAcks.count(_.nonEmpty)>=2,"M3 acknowledgement barrier empty")
        burst(r);run(8);holdAcks=false;drain()
      }
      for(r<-12 until 96) {burst(r);run(if(r<16) 120 else r%3)}
      drain()
      // Build genuine prefetched resident lines first. Fresh first-use hits
      // carry capture provenance; repeatedly hitting ordinary source0 rows
      // cannot exercise the half queue because first use clears the mark.
      dut.io.pfRecv.foreach { recv =>
      recv.pf_en.poke(true);recv.pf_source.poke(MemReqSource.Prefetch2L2SMS.id)
      for(i<-0 until 64){recv.addr.poke(BigInt(0x70000+i*64));recv.addr_valid.poke(true);tick()}
      recv.addr_valid.poke(false) };drain()
      for(r<-0 until 16)burst(r,base=BigInt(0x70000))
      drain()
      println("CDP_M3_FIRST_USE_PRESSURE "+reference.coverage.toSeq.sortBy(_._1).map{case(k,v)=>s"$k=$v"}.mkString(" "))
      for(negative<-Seq(1,2)) {burst(0,negative);drain()}
      // All Type1 NIDs alias one client; drop that client's SC copy using
      // real EVT before RO. Cold RO is BASE_KNOWN_LIMITATION, tested separately.
      val roPA=BigInt(0xc0000)
      request(roPA,0x200000);drain();evict(0,roPA);drain()
      val roBefore=reference.coverage("ReadOnce");val roBeats=beats
      request(roPA,0x200000,opcode=CCHIOpcode.ReadOnce.opcode);run(200)
      assert(reference.coverage("ReadOnce")-roBefore==1,"M3 legal ReadOnce execution absent")
      val legalHit=reference.directory.values.exists(x=>x._1==roPA && x._2.state!=0)
      assert(legalHit,"M3 ReadOnce stimulus must be an L2 hit")
      assert(beats-roBeats==2,s"M3 ReadOnce must return both data beats, got ${beats-roBeats}")
      assert(demandAddresses.isEmpty && demandOpcodes.isEmpty,"M3 ReadOnce left an outstanding demand")
      assert(portAcks.forall(_.isEmpty),"M3 ReadOnce must not enqueue an upstream CompAck")
      println(s"CDP_READONCE_RESULT legalClientI=true L2Hit=true eligibilityChecked=1 returnedBeats=${beats-roBeats} outstanding=${demandAddresses.size} upstreamCompAck=false hitResponse=PASS countedFailure=true")
      trace.close()
      println("CDP_M3_COVERAGE "+reference.coverage.toSeq.sortBy(_._1).map {case(k,v)=>s"$k=$v"}.mkString(" "))
      if (!off) reference.requireCoverage(Seq("crossSliceTrain","vpnFilterDifferent","captureSimultaneous","ReadOnce","L1DataPrefetch","trainEnable0") ++
        (if(slices>1) Seq("captureOneSlot","captureFull") else Seq.empty))
      // Keep all requested witnesses explicit: zero is a failing coverage gap.
      if (!off) reference.requireCoverage(Seq("crossTSHRTrain","trainEvictSameCycle","usedEvictConflict"))
      assert(reference.coverage("multiEvict")==0,"structural assumption changed: inspect eviction implementation")
      println("CDP_M3_STRUCTURAL multiEvict=unreachable configWriteEvictOrEvict=true singletonExternalRXRSP=true evictValidRequiresCompDBID=true")
    }
    if (off) {
      assert(allCandidates==0 && allDrops==0 && emitted.isEmpty && admitted.isEmpty && replies.isEmpty)
      assert(beats==checkedDemandRequests*2 && demandAddresses.isEmpty && demandOpcodes.isEmpty && evictTransactions.isEmpty)
      println(s"CDP_STRUCTURAL_OFF_RESULT pass=true rawDemandExecutions=${reference.coverage("executed")} checkedBeats=$beats pfGenerated=0 pfIssued=0 learnerCoverage=inapplicable retainedOwnership=inapplicable")
    }
    reference.finish();identities.finish(s"audit=raw slices=$slices")
    trace.close()
    println(s"CDP_RAW_SYSTEM_RESULT pass=true seeds=$seeds slices=$slices cycles=$cycle demands=$txn checkedBeats=$beats candidates=$allCandidates discarded=$allDrops outputs=${emitted.size} admitted=${admitted.size} useful=$hits unused=$unused tlbContention=$contention csrOff=0 trace=$tracePath")
    // CSR reopening keeps old ownership; all generations must eventually close.
    if(csrMode && !off) {
      assert(newOwnershipDuringResidual>0,"M4 no residual/new-generation overlap")
      assert(identities.ownershipCounters("retainedRetired")>0,"M4 no old candidate retirement")
      assert(residualSourceRetired>0,"M4 no old source9 identity completed after reopen")
      assert(residualRetryAfterReopen>0 && tlbOwnership.counters("oldRetired")>0,"M4 no retained TLB ownership witness")
      tlbOwnership.finish()
    }
  }
}
