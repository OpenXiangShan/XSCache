package oceanus

import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.compactchi.CCHIOpcode
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.l2._
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}
import scala.collection.mutable

/** Directed production-path checks. No forced metadata or synthetic prefetch bit. */
object TestTop_L2Prefetched extends App with ChiselSim {
  val selectedName = args.collectFirst { case s if s.startsWith("--case=") => s.stripPrefix("--case=") }.get
  require(args.sliding(2).exists(a => a.sameElements(Array("--slices", "1"))), "2c directed cases use one slice")
  val caseNames = if (selectedName == "bop-2c-all") {
    for (fill <- Seq("sc", "uc"); variant <- Seq("readshared", "readunique", "makeunique", "makeunique-miss", "external-stash", "replacement"))
      yield s"bop-2c-$variant-$fill"
  } else if (selectedName == "bop-useless-all") {
    Seq("useless-evict", "useless-snoop", "redundant-resident").map(v => s"bop-2c-$v-sc")
  } else if (selectedName == "bop-stash-all") {
    Seq("stashunique-hit", "stashshared-hit", "stashunique-dirty").map(v => s"bop-2c-$v-sc")
  } else Seq(selectedName)
  // These directed cases require internal fills; opt in despite base's off default.
  val l2cfg = new L2Configuration(8, true, Seq(0), bopEnable = true, pfSrcID = 3, bopParams = OceanusBOPParameters(badScore = 0))
  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params(mshrSize = 8)
    case CHIParametersKey => CHIParameters(issue = EnumCHIIssue.E, nodeIdWidth = 11, reqAddrWidth = 48, dataWidth = 256)
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = true, fpgaPlatform = false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = true, enablePerfDB = false,
      perfLevel = XSPerfLevel.VERBOSE, 0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new TestTop_L2Top(l2cfg)(config)) { dut =>
    // One hardware configuration; each variant gets a complete reset, SRAM/BOP
    // initialization, and fresh host queues/counters before independent checks.
    caseNames.foreach { name =>
      println(s"BOP_2C_RESET case=$name")
      val fillUC = name.endsWith("-uc")
      val variant = name.stripPrefix("bop-2c-").stripSuffix(if (fillUC) "-uc" else "-sc")
      require(Set("readshared", "readunique", "makeunique", "makeunique-miss", "external-stash", "replacement", "stashunique-hit", "stashshared-hit", "stashunique-dirty", "useless-evict", "useless-snoop", "redundant-resident").contains(variant))
      val stashPreserve = variant.startsWith("stash")
      val stashUpgrade = stashPreserve && variant != "stashshared-hit"
      val dirtyUpgrade = variant == "stashunique-dirty"
      val demand = BigInt("10000", 16)
      val target = demand + 0x80
      val replacement = BigInt("18080", 16)
      val hitOpcode = variant match {
        case "readunique" => CCHIOpcode.ReadUnique.opcode
        case "makeunique" | "makeunique-miss" => CCHIOpcode.MakeUnique.opcode
        case _ => CCHIOpcode.ReadShared.opcode
      }
      def req(addr: BigInt, opcode: BigInt) = UpRequestSpec(MasterId.t1p0, 0, 0, 0, opcode, addr)
      val rawRequests = variant match {
        // Cold MU trains once, produces P via +2, then an ordinary second
        // MU hit does not train. Readback Stash verifies the demand bit=0.
        // Downstream MU is dataless Comp; SC/UC affects the internal P fill
        // only. Expected total train=1, internal fills={P}, no upstream data.
        case "makeunique-miss" => Seq(req(demand, hitOpcode), req(demand, hitOpcode),
          req(demand, CCHIOpcode.StashShared.opcode))
        case "redundant-resident" => Seq(req(target, CCHIOpcode.StashShared.opcode),
          req(demand, CCHIOpcode.ReadShared.opcode), req(target, CCHIOpcode.StashShared.opcode))
        case "useless-evict" =>
          Seq("10000", "12000", "14000", "16000", "18080", "1a080").map(a => req(BigInt(a, 16), CCHIOpcode.ReadShared.opcode))
        case "useless-snoop" => Seq(req(demand, CCHIOpcode.ReadShared.opcode))
        case "stashunique-hit" | "stashshared-hit" | "stashunique-dirty" =>
          Seq(req(demand, CCHIOpcode.ReadShared.opcode),
            req(target, if (stashUpgrade) CCHIOpcode.StashUnique.opcode else CCHIOpcode.StashShared.opcode),
            req(target, CCHIOpcode.StashShared.opcode),
            req(target, CCHIOpcode.ReadShared.opcode),
            req(target, CCHIOpcode.StashShared.opcode))
        case "external-stash" => Seq.fill(2)(req(target, CCHIOpcode.StashShared.opcode))
        case "replacement" => Seq(req(demand, CCHIOpcode.ReadShared.opcode)) ++
          Seq("12080", "14080", "16080").map(a => req(BigInt(a, 16), CCHIOpcode.StashShared.opcode)) ++
          Seq(req(replacement, CCHIOpcode.ReadShared.opcode), req(replacement, CCHIOpcode.StashShared.opcode))
        case _ => Seq(req(demand, CCHIOpcode.ReadShared.opcode), req(target, hitOpcode),
          req(target, hitOpcode), req(target, CCHIOpcode.StashShared.opcode))
      }
      val requests = rawRequests.zipWithIndex.map { case (r, i) =>
        r.copy(txnId = i, expCompData = r.opcode != CCHIOpcode.StashShared.opcode && r.opcode != CCHIOpcode.StashUnique.opcode)
      }
      val fillResp = BigInt(if (fillUC) CHIFieldResp.CompData_UC else CHIFieldResp.CompData_SC)
      val spec = CaseSpec(name, UpConfig(Seq(MasterId.t1p0), ExplicitRequestStream(requests,
        Seq.fill(requests.size)(BigInt(CHIFieldResp.CompData_UC))), SnoopPolicy.SnpRespI),
        DnConfig(FixedLatency(12), FixedOutstanding(64), ImmediateCreditPolicy(), readNotSharedDirtyResp = fillResp),
        2500, (0, 2500), Seq.empty)

      // Expectations established before simulation:
      // D=0x10000 miss trains once and production BOP(badScore=0, offset=2)
      // fills P=D+2*64=0x10080 with prefetched=1, state SC->S / UC->UU, clean.
      // RS/RU/MU first P hit reads SRAM bit=1, trains once using OLD meta, clears
      // bit, and creates one additional prefetch P+0x80 in set 4. Second P hit
      // reads SRAM bit=0 and never trains. Final external StashShared hit reads
      // bit=0 and neither trains nor clears/sets. Total training=2 (D + first P),
      // internal prefetch fills=2; RS/RU return 2 beats per hit, MU one Comp.
      // AcquirePerm is included in coupledL2 MainPipe:173,759-766 -> MU trains.
      // Use nondefault pfSrcID=3 to verify the entire consts path (smoke uses 1).
      // External StashShared: SrcID=0 != pfSrcID=3; first fills P with bit=0,
      // second SRAM hit reads 0; train=0, internal fills=0, no upstream response.
      // Replacement: P(set 2) is inserted in way 0. External stash 0x12080,
      // 0x14080,0x16080 fill ways 1,2,3 without training or client presence.
      // PLRU touches 0,1,2,3 leave victim way 0. Demand 0x18080(set 2) must pick
      // P with SRAM victim bit=1 and install a NEW tag with bit=0 AND mask=1.
      // Final external StashShared hit on 0x18080 must read bit=0 from SRAM.
      // Total train=2 (D + replacement miss); two BOP prefetch fills use sets 2,4.
      // Stash independence expectations, written before running:
      // Internal P fills S/clean/prefetched=1. External SrcID=0 StashUnique
      // hits S and issues ReadUnique (0x07); its UC response upgrades to UU
      // while the prefetched write mask stays CLOSED. External StashShared
      // hits silently and keeps the bit. A fresh SRAM readback StashShared
      // still sees 1, no BOP train/useful increment. Following demand RS sees
      // old bit=1, trains exactly once (useful +1), clears, and creates P+0x80.
      // Final Stash reads 0. Total train=2, useful=1, internal fills=2.
      // stashunique-dirty returns UD_PD on the upgrade ONLY: expected resulting
      // combination prefetched=1, dirty=1, state=UU. This is a known combination
      // for later silent-evict work; external Stash must preserve the BOP mark.
      // Resident redundancy: external Stash first installs P with bit=0 and
      // frees its TSHR. Demand D subsequently generates internal +2 hint P;
      // it hits the resident line: redundant_resident=1, after_demand=0,
      // issued_downstream=0, useful=0. Its silent hit must not re-mark P.
      // Useless eviction: four cold demands D=0x10000/12000/14000/16000
      // produce internal P=D+0x80 in the SAME set 2, prefetched=1, S, no clients.
      // Four empty ways fill, PLRU order 0,1,2,3 => victim 0. Demand misses
      // 0x18080 and 0x1a080 then replace prefetched ways 0 and 2 respectively.
      // Each replacement counts once at the victim host's actual valid->I
      // REQ meta write: useless_evict=2, not at ReplRd or EVB acceptance.
      // Six trains produce six downstream internal prefetches; none is useful.
      // Four prefetched lines remain: 6 = 0 useful + 2 evict + 0 snoop + 4 resident.
      // Useless snoop: D produces P=S/prefetched=1; downstream SnpUnique P
      // (RetToSrc=0) invalidates it: useless_snoop=1, useless_evict=0, useful=0.
      // Repeat SnpUnique on now-invalid P: no further valid->I, still count=1.
      val up = new L2TopUpstreamModel(dut, spec)
      val dn = new L2TopDownstreamModel(dut, spec)
      val dataQ = mutable.Queue.empty[PendingDnComp]
      val rspQ = mutable.Queue.empty[PendingDnRsp]
      val ackQ = mutable.Queue.empty[PendingUpAck]
      val snpQ = mutable.Queue.empty[PendingSnpRsp]
      val landed = mutable.Map.empty[BigInt, (BigInt, Boolean)]
      val installed = mutable.ArrayBuffer.empty[(BigInt, Boolean, Boolean)]
      val internalFills = mutable.Set.empty[BigInt]
      var cycle = 0
      var current = -1
      var accepted = false
      var trainCount = 0
      var usefulCount = 0
      var downstreamCount = 0
      var dirtyPreserveObserved = false
      var externalPrefetchMaskWrites = 0
      var injectSnoop = false
      var snoopSent = false
      var snoopResponses = 0
      var nextSnoopTxn = 1
      val resident = mutable.Map.empty[BigInt, Boolean]
      def perf: Seq[BigInt] = dut.observation.bopPerfCountBySlice(0).map(_.peek().litValue).toSeq
      var dataCount = 0
      var compCount = 0
      val reads = mutable.ArrayBuffer.empty[(Boolean, Boolean)]
      val victims = mutable.ArrayBuffer.empty[(BigInt, Boolean)]
      def flag(b: Bool): Boolean = b.peek().litToBoolean
      def value(u: UInt): BigInt = u.peek().litValue
      def quiet: Boolean = dataQ.isEmpty && rspQ.isEmpty && ackQ.isEmpty && snpQ.isEmpty &&
        !flag(dut.observation.bopReqValid) &&
        dut.observation.dirProbeBySlice(0).forall(d => !flag(d.valid) && !flag(d.reqActive))
      def tick(): Unit = {
        up.clearResponses()
        dn.clearResponses()
        if (injectSnoop && dn.canDriveRxSnp) {
          dut.io.chi.rxsnp.flitv.poke(true.B)
          dut.io.chi.rxsnp.flitpend.poke(true.B)
          dut.io.chi.rxsnp.flit.poke(0.U.asTypeOf(dut.io.chi.rxsnp.flit))
          dut.io.chi.rxsnp.flit.Opcode.get.poke(0x07.U) // CHI SnpUnique
          // SnpUnique requires DoNotGoToSD=1 in this base protocol path.
          // Poke the packed storage field; the accessor methods elaborate a
          // slice and cannot be called from the simulator thread.
          dut.io.chi.rxsnp.flit.DoNotGoToSD_DoNotDataPull.get.poke(3.U)
          dut.io.chi.rxsnp.flit.Addr.get.poke((target >> 3).U)
          dut.io.chi.rxsnp.flit.TxnID.get.poke(nextSnoopTxn.U)
          dut.io.chi.rxsnp.flit.SrcID.get.poke(0.U)
          injectSnoop = false; snoopSent = true
        }
        if (flag(dut.io.chi.txrsp.flitv) && value(dut.io.chi.txrsp.flit.Opcode.get) == 0x01)
          snoopResponses += 1 // SnpResp, one per non-forward SnpUnique
        var upRspDriven = false
        ackQ.headOption.filter(_.due <= cycle).foreach { a =>
          up.driveUpCompAck(a.upDBID, 0); ackQ.dequeue(); upRspDriven = true
        }
        if (!upRspDriven) snpQ.headOption.filter(_.due <= cycle).foreach { s =>
          up.driveUpSnpResp(s.snpTxnID, s.tgtId); snpQ.dequeue()
        }
        dataQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxDat).foreach { p =>
          dn.driveDnData(p); dataQ.dequeue(); dn.noteResponse(p.dnTxnID, p.beat == 1, cycle)
        }
        rspQ.headOption.filter(p => p.due <= cycle && dn.canDriveRxRsp).foreach { p =>
          dn.driveDnRsp(p)
          if (p.opcode == L2TopDownstreamOpcodes.Comp) dut.io.chi.rxrsp.flit.Resp.get.poke(CHIFieldResp.Comp_UC.U)
          rspQ.dequeue(); dn.noteResponse(p.dnTxnID, finalBeat = true, cycle)
        }
        if (current >= 0 && flag(dut.io.t1p(0).UpREQ.valid) && flag(dut.io.t1p(0).UpREQ.ready)) accepted = true
        if (flag(dut.observation.bopTrainFire)) {
          assert(current >= 0 && value(dut.observation.bopTrainAddr) == requests(current).addr,
            "training must identify the current demand, never its Stash prefetch")
          trainCount += 1
          println(s"BOP_2C_TRAIN cycle=$cycle addr=0x${value(dut.observation.bopTrainAddr).toString(16)}")
        }
        usefulCount += value(dut.observation.prefetchedHitTrainCount).toInt
        dut.observation.dirProbeBySlice(0).foreach { d =>
          val addr = value(d.tshrPaddr)
          // Eviction invalidation may cancel Directory writeback because the
          // refiller overwrites that physical way. Remove the old tag on the
          // actual local invalidating write as well as tracking committed rows.
          if (flag(d.metaWrReqState) && value(d.metaWrReqMetaState) == 0) resident(addr) = false
          if (flag(d.dirWbLand)) resident(addr) = value(d.dirResultState) != 0 && flag(d.dirResultPrefetched)
          if (flag(d.dirWbLand)) landed(addr) = (value(d.dirResultState), flag(d.dirResultPrefetched))
          if (stashPreserve && current == 1 && addr == target) {
            if (flag(d.metaWrReqPrefetched)) externalPrefetchMaskWrites += 1
            if (flag(d.dirWbLand) && flag(d.dirResultPrefetched) && flag(d.dirResultDirty))
              dirtyPreserveObserved = true
          }
          if (flag(d.tagWrite)) {
            installed += ((addr, flag(d.metaWrReqPrefetched), flag(d.metaWrReqMetaPrefetched)))
            assert(flag(d.metaWrReqPrefetched), "every new tag must overwrite prefetched, even for data=0")
            if (flag(d.pRxReqIsPrefetch)) internalFills += addr
          }
          if (current >= 0 && addr == requests(current).addr) {
            if (flag(d.dirRdResp)) {
              reads += ((flag(d.fromDirMetaHit), flag(d.fromDirMetaPrefetched)))
              println(s"BOP_2C_SRAM index=$current hit=${flag(d.fromDirMetaHit)} prefetched=${flag(d.fromDirMetaPrefetched)}")
            }
            if (flag(d.replRdResp)) victims += ((value(d.fromDirReplPaddr), flag(d.fromDirMetaPrefetched)))
          }
        }
        if (flag(dut.io.chi.txreq.flitv)) {
          val txn = DnTxnID(value(dut.io.chi.txreq.flit.TxnID.get))
          val src = value(dut.io.chi.txreq.flit.SrcID.get)
          val op = value(dut.io.chi.txreq.flit.Opcode.get)
          downstreamCount += 1
          dn.noteRequest(txn)
          if (dn.isEvictReq(op) || op == 0x0c) {
            val response = if (op == 0x0c) BigInt(L2TopDownstreamOpcodes.Comp) else dn.downstreamEvictRspOpcode(op)
            rspQ.enqueue(PendingDnRsp(txn, src, cycle + 12, response))
          } else {
            val resp = if (op == L2TopDownstreamOpcodes.ReadNotSharedDirty) fillResp else BigInt(if (dirtyUpgrade && current == 1) CHIFieldResp.CompData_UD_PD else CHIFieldResp.CompData_UC)
            dataQ.enqueue(PendingDnComp(txn, src, cycle + 12, 0, resp), PendingDnComp(txn, src, cycle + 13, 1, resp))
          }
        }
        if (flag(dut.io.t1p(0).DnDAT.valid) && flag(dut.io.t1p(0).DnDAT.ready)) {
          assert(current >= 0 && value(dut.io.t1p(0).DnDAT.bits.TxnID) == requests(current).txnId)
          dataCount += 1
          if (value(dut.io.t1p(0).DnDAT.bits.DataID) == 1) ackQ.enqueue(PendingUpAck(0,
            UpDBID(value(dut.io.t1p(0).DnDAT.bits.DBID)), UpTxnID(requests(current).txnId), cycle + 1, 0))
        }
        if (flag(dut.io.t1p(0).DnRSP.valid) && flag(dut.io.t1p(0).DnRSP.ready)) {
          assert(current >= 0 && requests(current).opcode == CCHIOpcode.MakeUnique.opcode)
          assert(value(dut.io.t1p(0).DnRSP.bits.Opcode) == CCHIOpcode.Comp.opcode)
          compCount += 1
          ackQ.enqueue(PendingUpAck(0, UpDBID(value(dut.io.t1p(0).DnRSP.bits.DBID)),
            UpTxnID(requests(current).txnId), cycle + 1, 0))
        }
        if (flag(dut.io.t1p(0).DnSNP.valid) && flag(dut.io.t1p(0).DnSNP.ready))
          snpQ.enqueue(PendingSnpRsp(SnpTxnID(value(dut.io.t1p(0).DnSNP.bits.TxnID)),
            value(dut.io.t1p(0).DnSNP.bits.SrcID), cycle + 1))
        dn.stepWithCredits(cycle)
        cycle += 1
        assert(cycle < 2500, s"2c watchdog index=$current pending=${dataQ.size}/${rspQ.size}/${ackQ.size}/${snpQ.size}")
      }
      up.initInputs(); dn.initInputs()
      dut.log.dump.poke(false.B); dut.log.clean.poke(false.B)
      dut.reset.poke(true.B)
      // Reset hardware before starting fresh host credit trackers for each case.
      dut.clock.step(8)
      dut.reset.poke(false.B); dn.activateRxLink()
      for (i <- 0 until 300) dn.stepWithCredits(i - 300)
      requests.indices.foreach { index =>
        current = index; accepted = false; reads.clear(); victims.clear()
        val beforeTrain = trainCount
        val beforeData = dataCount
        val beforeComp = compCount
        val beforeUseful = usefulCount
        val beforeDownstream = downstreamCount
        up.driveRead(index)
        while (!accepted) tick()
        up.clearUpReq()
        var drained = 0
        while (drained < 16) { tick(); drained = if (quiet) drained + 1 else 0 }
        val r = requests(index)
        val stash = r.opcode == CCHIOpcode.StashShared.opcode || r.opcode == CCHIOpcode.StashUnique.opcode
        assert(dataCount - beforeData == (if (stash || r.opcode == CCHIOpcode.MakeUnique.opcode) 0 else 2))
        assert(compCount - beforeComp == (if (r.opcode == CCHIOpcode.MakeUnique.opcode) 1 else 0))
        if (variant == "external-stash") {
          assert(trainCount == 0 && landed(target)._2 == false)
          assert(reads.toSeq == Seq((index != 0, false)), s"external Stash SRAM reads=$reads")
        } else if (variant == "redundant-resident") {
          assert(trainCount == (if (index == 0) 0 else 1) && usefulCount == 0)
          if (index != 1) assert(reads.toSeq == Seq((index == 2, false)))
          assert(!landed(target)._2)
        } else if (variant == "useless-evict") {
          assert(trainCount - beforeTrain == 1 && usefulCount == 0)
          if (index < 4) {
            assert(perf(L2BOPPerf.UselessEvict) == 0)
            assert(resident.count(_._2) == index + 1)
          } else {
            val expectedVictim = if (index == 4) target else BigInt("14080", 16)
            assert(victims.toSeq == Seq((expectedVictim, true)), s"prefetched victim=$victims expected=$expectedVictim")
            assert(perf(L2BOPPerf.UselessEvict) == index - 3)
            assert(resident.count(_._2) == 4)
          }
          assert(perf(L2BOPPerf.UselessSnoop) == 0)
        } else if (variant == "useless-snoop") {
          assert(index == 0 && landed(target) == ((BigInt(1), true)))
          assert(usefulCount == 0)
        } else if (stashPreserve) {
          val consumes = index == 3
          assert(trainCount - beforeTrain == (if (index == 0 || consumes) 1 else 0))
          assert(usefulCount - beforeUseful == (if (consumes) 1 else 0))
          assert(externalPrefetchMaskWrites == 0, "external Stash hit must never write prefetched")
          if (index == 0) {
            assert(landed(target) == ((BigInt(1), true)))
          } else {
            assert(reads.toSeq == Seq((true, index <= 3)), s"Stash preservation SRAM reads=$reads")
            // Silent Stash has no DirWb; inspect its independent SRAM read.
            if (index == 1 && stashUpgrade) {
              assert(downstreamCount - beforeDownstream == 1)
              assert(landed(target) == ((BigInt(3), true)))
              if (dirtyUpgrade) assert(dirtyPreserveObserved)
            } else if (stash) assert(downstreamCount == beforeDownstream)
            if (index >= 3) assert(!landed(target)._2)
          }
        } else if (variant == "makeunique-miss") {
          assert(reads.toSeq == Seq((index != 0, false)))
          assert(trainCount - beforeTrain == (if (index == 0) 1 else 0))
          assert(landed(demand)._2 == false)
          assert(landed(target) == ((if (fillUC) BigInt(3) else BigInt(1)), true))
          assert(internalFills.toSet == Set(target))
        } else if (index == 0) {
          assert(trainCount - beforeTrain == 1 && landed(target) == ((if (fillUC) BigInt(3) else BigInt(1)), true))
          assert(internalFills.toSet == Set(target))
        } else if (variant == "replacement") {
          if (index == 4) {
            assert(reads.toSeq == Seq((false, false)))
            assert(victims.toSeq == Seq((target, true)), s"replacement must evict prefetched P: $victims")
            assert(installed.exists(e => e == ((replacement, true, false))))
            assert(landed(replacement)._2 == false && trainCount - beforeTrain == 1)
          } else {
            assert(trainCount == beforeTrain)
            assert(reads.toSeq == Seq((index == 5, false)))
            assert(landed(r.addr)._2 == false)
          }
        } else {
          assert(reads.toSeq == Seq((true, index == 1)), s"first/second hit SRAM reads=$reads")
          assert(trainCount - beforeTrain == (if (index == 1) 1 else 0))
          assert(landed(target)._2 == false)
        }
        println(s"BOP_2C_PHASE pass=true case=$name index=$index trainDelta=${trainCount - beforeTrain} dataDelta=${dataCount - beforeData} compDelta=${compCount - beforeComp}")
      }
      assert(quiet)
      if (variant == "useless-snoop") {
        for (_ <- 0 until 2) {
          injectSnoop = true; snoopSent = false
          val previousResponses = snoopResponses
          while (!snoopSent || snoopResponses == previousResponses) tick()
          var drained = 0
          while (drained < 16) { tick(); drained = if (quiet) drained + 1 else 0 }
          assert(perf(L2BOPPerf.UselessSnoop) == 1)
          nextSnoopTxn += 1
        }
        assert(landed(target) == ((BigInt(0), true)), "SNP invalidation preserves stale bit but state=I")
      }
      val expectedTrains = if (variant == "useless-evict") 6 else if (variant == "useless-snoop" || variant == "redundant-resident") 1 else if (variant == "external-stash") 0 else if (variant == "makeunique-miss") 1 else 2
      assert(trainCount == expectedTrains)
      assert(internalFills.size == (if (variant == "redundant-resident") 0 else expectedTrains))
      if (!variant.startsWith("useless") && variant != "redundant-resident" && variant != "external-stash" && variant != "makeunique-miss") assert(internalFills.toSet == Set(target,
        (if (variant == "replacement") replacement else target) + 0x80))
      if (variant == "redundant-resident") {
        assert(perf == Seq(0,0,1,0,1,0,0,0).map(BigInt(_)))
        println("BOP_RESIDENT_RESULT pass=true redundant=1 redundant_after_demand=0 redundant_resident=1 issued_downstream=0 useful=0")
      }
      if (variant.startsWith("useless")) {
        val expected = if (variant == "useless-evict") Seq(0,0,0,0,0,6,2,0) else Seq(0,0,0,0,0,1,0,1)
        assert(perf == expected.map(BigInt(_)), s"useless metrics=$perf expected=$expected")
        assert(resident.count(_._2) == (if (variant == "useless-evict") 4 else 0))
        if (variant == "useless-evict") {
          assert(resident.collect { case (a, true) => a }.toSet ==
            Seq("12080", "16080", "18100", "1a100").map(BigInt(_, 16)).toSet)
        }
        println(s"BOP_USELESS_RESULT pass=true case=$name " + L2BOPPerf.names.zip(perf).map { case (n,v) => s"$n=$v" }.mkString(" ") +
          s" resident=${resident.count(_._2)} snoopResponses=$snoopResponses")
      }
      assert(perf(L2BOPPerf.Useful) == usefulCount)
      if (stashPreserve) {
        assert(usefulCount == 1)
        println(s"BOP_STASH_RESULT pass=true case=$name useful=$usefulCount externalMaskWrites=$externalPrefetchMaskWrites " +
          s"upgrade=$stashUpgrade dirtyPreserve=$dirtyPreserveObserved")
      }
      println(s"BOP_2C_RESULT pass=true case=$name train=$trainCount internalFills=${internalFills.size} pending=0 active=0 SRAM_READBACK=true")
    }
  }
}
