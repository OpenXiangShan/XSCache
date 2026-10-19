package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import org.chipsalliance.cde.config._
import utility._
import oceanus.l2._
import oceanus.l2.tshr._
import oceanus.chi.bundle.CHIBundleDAT
import oceanus.compactchi._

class L2TopObservationBuckets extends Bundle {
  val cycles = UInt(64.W)
  val grant = UInt(64.W)
  val blocked = UInt(64.W)
  val starved = UInt(64.W)
  val busy = UInt(64.W)
}

class L2TopGrantCounts extends Bundle {
  val dirRd = UInt(64.W)
  val dirWb = UInt(64.W)
  val dirReplRd = UInt(64.W)
  val dsWb = UInt(64.W)
  val dsRd = UInt(64.W)
  val dsAheadRd = UInt(64.W)
}

class L2TopTshr0Probe extends Bundle {
  val wUnlockDs = Bool()
  val wUnlockDir = Bool()
  val lockDs = Bool()
  val expectReplace = Bool()
  val selfUnlockDs = Bool()
  val unlockSelfEvictback = Bool()
  val wSRepl = Bool()
  val triggerReplace = Bool()
  val toDirReplRd = Bool()
  val sEvict = Bool()
  val stateDirReadPreArb = Bool()
  val stateDirReadReplPreArb = Bool()
  val stateDirReadPostArb = Bool()
  val dirResultState = UInt(2.W)
  val dirResultDirty = Bool()
  val dirResultHit = Bool()
  val dirResultWay = UInt(4.W)
  val dirRdResp = Bool()
  val dirRdRespState = UInt(2.W)
  val dirRdRespDirty = Bool()
  val dirRdRespHit = Bool()
  val dirRdRespWay = UInt(4.W)
  val replRdResp = Bool()
  val replRdRespState = UInt(2.W)
  val replRdRespDirty = Bool()
  val replRdRespWay = UInt(4.W)
  val replWay = UInt(4.W)
  val replPaddr = UInt(48.W)
  val fromDirMetaState = UInt(2.W)
  val fromDirMetaDirty = Bool()
  val fromDirMetaHit = Bool()
  val fromDirMetaWay = UInt(4.W)
  val fromDirReplWay = UInt(4.W)
  val fromDirReplPaddr = UInt(48.W)
}

class L2TopTshr0Counts extends Bundle {
  val wUnlockDs = UInt(64.W)
  val wUnlockDir = UInt(64.W)
  val lockDs = UInt(64.W)
  val expectReplace = UInt(64.W)
  val selfUnlockDs = UInt(64.W)
  val unlockSelfEvictback = UInt(64.W)
  val wSRepl = UInt(64.W)
  val triggerReplace = UInt(64.W)
  val toDirReplRd = UInt(64.W)
  val sEvict = UInt(64.W)
}

class L2TopRawReqCounts extends Bundle {
  val replRdVec = UInt(64.W)
  val dirRdVec = UInt(64.W)
  val dirWbVec = UInt(64.W)
  val wbVec = UInt(64.W)
  val rdVec = UInt(64.W)
}

class L2TopAllocatorProbe(entries: Int) extends Bundle {
  val reqValid = Bool()
  val reqReady = Bool()
  val reqPaddr = UInt(48.W)
  val postclusterValid = Bool()
  val postclusterReady = Bool()
  val paddrHitVec = UInt(entries.W)
  val canReuseVec = UInt(entries.W)
  val allocVec = UInt(entries.W)
  val reuseVec = UInt(entries.W)
  val allocAny = Bool()
  val reuseAny = Bool()
}

class L2TopDSWbProbeEntry(implicit val p: Parameters) extends Bundle with HasL2Params {
  val valid = Bool()
  val set = UInt(32.W)
  val way = UInt(32.W)
  val data = UInt(512.W)
}

class L2TopDebugProbeEntry extends Bundle {
  val tbufWenLast = Bool()
  val dsWbCancel = Bool()
  val dsWritePreArb = Bool()
  val dsWritePostArb = Bool()
  val dsWritePreArbPostArb = Bool()
  val dsWriteDone = Bool()
  val reqWData0 = Bool()
  val reqWData2 = Bool()
  val reqWComp = Bool()
  val reqWDsResp = Bool()
  val reqWSUpData0 = Bool()
  val reqWSUpData2 = Bool()
  val reqSUpData0 = Bool()
  val reqSUpData2 = Bool()
  val reqDnCompData = Bool()
  val reqDnCompDataFirst = Bool()
}

class L2TopBEStageProbe extends Bundle {
  val valid = Bool()
  val ready = Bool()
  val opcode = UInt(8.W)
  val dataId = UInt(2.W)
  val be = UInt(32.W)
  val data = UInt(256.W)
}

class L2TopBEProbe(entries: Int = 8) extends Bundle {
  val vpipeReq = new L2TopBEStageProbe
  val tshr = new L2TopBEStageProbe
  val vpipeReqAll = Vec(entries, new L2TopBEStageProbe)
  val tshrAll = Vec(entries, new L2TopBEStageProbe)
  val slice = new L2TopBEStageProbe
  val chiLink = new L2TopBEStageProbe
  val top = new L2TopBEStageProbe
  val reqDirState = UInt(2.W)
  val reqDirDirty = Bool()
  val reqDirHit = Bool()
  val reqDirStateAll = Vec(entries, UInt(2.W))
  val reqDirDirtyAll = Vec(entries, Bool())
  val reqDirHitAll = Vec(entries, Bool())
  val reqStateIsIAll = Vec(entries, Bool())
  val reqStateNotIAll = Vec(entries, Bool())
  val reqBEExprAll = Vec(entries, UInt(32.W))
  val sEvictCbWrData0All = Vec(entries, Bool())
  val sEvictCbWrData2All = Vec(entries, Bool())
  val reqStateIsI = Bool()
  val reqStateNotI = Bool()
  val reqBEEnable = Bool()
  val reqBEExpr = UInt(32.W)
  val sEvictCbWrData0 = Bool()
  val sEvictCbWrData2 = Bool()
}

class L2TopDirTimingProbeEntry extends Bundle {
  val valid = Bool()
  val alloc = Bool()
  val reuse = Bool()
  val dealloc = Bool()
  val enterREQ = Bool()
  val enterEVT = Bool()
  val enterSNP = Bool()
  val tshrPaddr = UInt(48.W)
  val rxReqFire = Bool()
  val reqAlloc = Bool()
  val reqReuse = Bool()
  val reqAllocPaddr = UInt(48.W)
  val pRxReqIsPrefetch = Bool()
  val pRxReqSrcID = UInt(8.W)
  val pRxReqExpCompData = Bool()
  val pRxReqSize = UInt(3.W)
  val pRxReqOpcode = UInt(8.W)
  val pRxReqEvictback = Bool()
  val reqActive = Bool()
  val reqStateMask = UInt(64.W)
  val upTxRspFire = Bool()
  val upTxDatFire = Bool()
  val bopTrainValid = Bool()
  val upTxRspOpcode = UInt(8.W)
  val dnTxRspFire = Bool()
  val dnTxRspOpcode = UInt(8.W)
  val dirRdReq = Bool()
  val dirRdArbComp = Bool()
  val dirRdResp = Bool()
  val replRdReq = Bool()
  val replRdArbComp = Bool()
  val dirWbReq = Bool()
  val dirWbLand = Bool()
  val paddr = UInt(48.W)
  val fromDirMetaState = UInt(2.W)
  val fromDirMetaDirty = Bool()
  val fromDirMetaHit = Bool()
  val fromDirMetaWay = UInt(4.W)
  val dirResultState = UInt(2.W)
  val dirResultDirty = Bool()
  val dirResultHit = Bool()
  // Preserve the Boolean baseline probes and expose raw SRAM/write payloads for encoding checks.
  val dirResultPfSource = UInt(4.W)
  val fromDirMetaPfSource = UInt(4.W)
  val metaWrReqMetaPfSource = UInt(4.W)
  val dirResultPrefetched = Bool()
  val fromDirMetaPrefetched = Bool()
  val fromDirReplPaddr = UInt(48.W)
  val tagWrite = Bool()
  val metaWrReqPrefetched = Bool()
  val metaWrReqMetaPrefetched = Bool()
  val dirResultWay = UInt(4.W)
  val rdDone = Bool()
  val replRdResp = Bool()
  val metaWrStateEvictbackI = Bool()
  val metaWrStateEvictbackIEvict = Bool()
  val metaWrStateEvictbackIWrite = Bool()
  val fireTxreqEvict = Bool()
  val fireTxreqWriteBackFull = Bool()
  val fireTxreqWriteEvictFull = Bool()
  val fireTxreqWriteEvictOrEvict = Bool()
  val expectDnEvictCompdbid = Bool()
  val wEvictDnCompdbid = Bool()
  val dnRxrspComp = Bool()
  val dnRxrspCompdbidresp = Bool()
  val pRxreqEvictback = Bool()
  val dsRdEvictback = Bool()
  val dsRdEn = Bool()
  val dsRdDone = Bool()
  val dsReqRd = Bool()
  val dsRdArbComp = Bool()
  val dsRdResp = Bool()
  val dsReqAheadRd = Bool()
  val dsAheadRdArbComp = Bool()
  val dsAheadRdResp = Bool()
  val dsReqWb = Bool()
  val dsWbArbComp = Bool()
  val dsWbComp = Bool()
  val dsRdData = UInt(512.W)
  val metaWrDirtySASet = Bool()
  val metaWrReqState = Bool()
  val metaWrReqDirty = Bool()
  val metaWrReqMetaState = UInt(2.W)
  val metaWrReqMetaDirty = Bool()
  val metaWrEvtState = Bool()
  val metaWrEvtDirty = Bool()
  val metaWrSnpState = Bool()
  val metaWrSnpDirty = Bool()
  val metaDrop = Bool()
}

class L2TopSliceTshrProbe extends Bundle {
  val valid = Bool()
  val alloc = Bool()
  val reuse = Bool()
  val dealloc = Bool()
  val tshrPaddr = UInt(48.W)
  val pRxReqIsPrefetch = Bool()
  val pRxReqOpcode = UInt(8.W)
  val pRxReqEvictback = Bool()
  val reqStateMask = UInt(64.W)
  val sEvict = Bool()
  val dirRdResp = Bool()
  val dirRdRespState = UInt(2.W)
  val dirRdRespDirty = Bool()
  val dirRdRespHit = Bool()
  val dirRdRespWay = UInt(4.W)
  val replRdResp = Bool()
  val replRdRespState = UInt(2.W)
  val replRdRespDirty = Bool()
  val replRdRespWay = UInt(4.W)
  val replPaddr = UInt(48.W)
  val fromDirMetaState = UInt(2.W)
  val fromDirMetaDirty = Bool()
  val fromDirMetaHit = Bool()
  val fromDirMetaWay = UInt(4.W)
  val fromDirReplWay = UInt(4.W)
  val fromDirReplPaddr = UInt(48.W)
}


class L2TopObservationIO(entries: Int, sliceCount: Int)(implicit val p: Parameters) extends Bundle {
  val sliceNum = Output(UInt(8.W))
  val bankBits = Output(UInt(8.W))
  val offsetBits = Output(UInt(8.W))
  val directory = Output(new L2TopObservationBuckets)
  val dataStorage = Output(new L2TopObservationBuckets)
  val upRXREQ = Output(new L2TopObservationBuckets)
  val upTXDAT = Output(new L2TopObservationBuckets)
  val grants = Output(new L2TopGrantCounts)
  val tshr0 = Output(new L2TopTshr0Probe)
  val tshr0Counts = Output(new L2TopTshr0Counts)
  val rawReqCounts = Output(new L2TopRawReqCounts)
  val allocator = Output(new L2TopAllocatorProbe(entries))
  val bopTrainFire = Output(Bool())
  val bopTrainFireCount = Output(UInt(64.W))
  val sliceTrainPulseCount = Output(Vec(sliceCount, UInt(64.W)))
  val sliceTrainValid = Output(Vec(sliceCount, Bool()))
  val sliceTrainAddr = Output(Vec(sliceCount, UInt(48.W)))
  val prefetchQueueValid = Output(Vec(sliceCount, Bool()))
  val prefetchQueueReady = Output(Vec(sliceCount, Bool()))
  val prefetchQueueOverwrite = Output(Vec(sliceCount, Bool()))
  val bopTrainReceiveCount = Output(UInt(64.W))
  val bopTrainAddr = Output(UInt(48.W))
  val bopReqAddr = Output(UInt(48.W))
  val bopDropAddr = Output(UInt(48.W))
  val bopReqValid = Output(Bool())
  val bopReqReady = Output(Bool())
  val bopReqEnqReady = Output(Bool())
  val bopReqFire = Output(Bool())
  val bopTrainValid = Output(Bool())
  val bopTrainReady = Output(Bool())
  val bopPerfCountBySlice = Output(Vec(sliceCount, Vec(L2BOPPerf.names.size, UInt(64.W))))
  val prefetchedHitTrainCount = Output(UInt(8.W))
  val prefetchedHitTrainAddr = Output(UInt(48.W))
  val prefetchedHitTrainValidBySlice = Output(Vec(sliceCount, Bool()))
  val prefetchedHitTrainAddrBySlice = Output(Vec(sliceCount, UInt(48.W)))
  val bopQueueOverwrite = Output(Bool())
  val bopPrefetchIssue = Output(Bool())
  val bopPrefetchIssueAddr = Output(UInt(48.W))
  val bopPrefetchIssueBySlice = Output(Vec(sliceCount, Bool()))
  val bopPrefetchIssueAddrBySlice = Output(Vec(sliceCount, UInt(48.W)))
  val bopCrossPageDrop = Output(Bool())
  val bopDisabledDrop = Output(Bool())
  val bopDelayQueueFullDrop = Output(Bool())
  val bopTrainStall = Output(Bool())
  val dsWb = Output(Vec(entries, new L2TopDSWbProbeEntry))
  val debug = Output(Vec(entries, new L2TopDebugProbeEntry))
  val beProbe = Output(new L2TopBEProbe(entries))
  val dirProbe = Output(Vec(entries, new L2TopDirTimingProbeEntry))
  val dirProbeBySlice = Output(Vec(sliceCount, Vec(entries, new L2TopDirTimingProbeEntry)))
  val sliceTshrProbe = Output(Vec(sliceCount, Vec(entries, new L2TopSliceTshrProbe)))
  val sliceSourceFireVec = Output(Vec(sliceCount, UInt(entries.W)))
  val sliceEvictBackFire = Output(Vec(sliceCount, Bool()))
  val sliceEvictBackFireCount = Output(Vec(sliceCount, UInt(64.W)))
  val sliceAllocVec = Output(Vec(sliceCount, UInt(entries.W)))
  val sliceReuseVec = Output(Vec(sliceCount, UInt(entries.W)))
}

object L2TopObservation {
  def attach(module: L2Top, entries: Int)(implicit p: Parameters): L2TopObservationIO = {
    val io = IO(new L2TopObservationIO(entries, module.config.sliceNum))
    new Connector(module, io)
    io
  }

  private class Connector(module: L2Top, io: L2TopObservationIO)(implicit val p: Parameters) extends HasL2Params {
  private def privateField[T](owner: AnyRef, name: String): T = {
    val field = owner.getClass.getDeclaredField(name)
    field.setAccessible(true)
    field.get(owner).asInstanceOf[T]
  }
  // A missing field is a stale probe, not an absent signal: the freeze-tag
  // harness used to swallow NoSuchFieldException as false.B, which made
  // p_rxreq_isPrefetch read as constantly false on base and turned every
  // prefetch-exclusion check into a no-op. Fail elaboration instead. A probe
  // that is genuinely absent must be annotated optional with the reason.
  private def privateBool(owner: AnyRef, name: String, optional: Option[String] = None): Bool = {
    try privateField[Bool](owner, name) catch {
      case _: NoSuchFieldException =>
        require(optional.nonEmpty,
          s"probe ${owner.getClass.getName}.$name does not exist; annotate optional with the reason it may be absent")
        false.B
    }
  }

  private val slice0 = privateField[Seq[L2TSHRCtrl]](module, "slices").head
  private val bop = privateField[OceanusBOP](module, "bop")
  private val slices = privateField[Seq[L2TSHRCtrl]](module, "slices")
  private val pfQueues = privateField[Seq[DecoupledIO[FlitREQ]]](module, "pfQueues")
  // The production queue instances are local to the enabled construction
  // block. Inspect the elaborated direct-child IDs, then bore the actual
  // overwrite pulse; do not infer the observed counter from a shadow FIFO.
  private val moduleIds = module.getClass.getMethods.find(_.getName == "getIds").get
    .invoke(module).asInstanceOf[scala.collection.Seq[AnyRef]]
  private val productionPfQueues = moduleIds.collect { case q: OverwriteQueue[_] => q }
  require(productionPfQueues.size == pfQueues.size, "prefetch overwrite probe must cover every queue")
  require(pfQueues.size == (if (module.config.bopEnable) module.config.sliceNum else 0),
    "prefetch observation must cover every production queue")
  private val tshrs = privateField[Seq[L2TSHR]](slice0, "tshrs")
  private val sliceTshrs = slices.map(slice => privateField[Seq[L2TSHR]](slice, "tshrs"))
  private val tshrAllocator = slice0.tshrAlloc
  private val reqClusterIndex = tshrAllocator.config.cluster.indexWhere(_.contains(L2TSHRAllocTarget.REQ))
  private val allocatorPostcluster = privateField[Vec[DecoupledIO[Data]]](tshrAllocator, "postcluster")
  private val allocatorPaddrHitVec = privateField[Seq[Seq[Bool]]](tshrAllocator, "paddr_hit_vec")
  private val allocatorCanReuseVec = privateField[Seq[Seq[Bool]]](tshrAllocator, "can_reuse_vec")
  private val allocatorAllocAny = privateField[Seq[Bool]](tshrAllocator, "alloc_any")
  private val allocatorReuseAny = privateField[Seq[Bool]](tshrAllocator, "reuse_any")
  private val directory = slice0.directory
  private val dataStorage = slice0.dataStorage
  private val chiLink = privateField[L2DownstreamCHI](module, "chiLink")
  private def boreRead[T <: Data](data: T): T = {
    try BoringUtils.bore(data) catch { case _: NoSuchElementException => data }
  }


  // Existing hierarchical observation: mirror the exact numeric XSPerf input,
  // including multiple TSHRs in one cycle, without adding production top IO.
  slices.zipWithIndex.foreach { case (slice, s) =>
    val pulses = boreRead(slice.bopPerf)
    L2BOPPerf.names.indices.foreach { i =>
      val count = RegInit(0.U(64.W))
      count := count + pulses(i)
      io.bopPerfCountBySlice(s)(i) := count
    }
  }

  io.sliceNum := module.config.sliceNum.U
  io.bankBits := log2Ceil(module.config.sliceNum).U
  io.offsetBits := offsetBits.U

  private def observe(name: String, grant: Bool, blocked: Bool, starved: Bool, busy: Bool): L2TopObservationBuckets = {
    val cycles = RegInit(0.U(64.W))
    val grantCount = RegInit(0.U(64.W))
    val blockedCount = RegInit(0.U(64.W))
    val starvedCount = RegInit(0.U(64.W))
    val busyCount = RegInit(0.U(64.W))

    cycles := cycles + 1.U
    when(grant) { grantCount := grantCount + 1.U }
    when(blocked) { blockedCount := blockedCount + 1.U }
    when(starved) { starvedCount := starvedCount + 1.U }
    when(busy) { busyCount := busyCount + 1.U }

    assert(grantCount + blockedCount + starvedCount + busyCount === cycles,
      s"L2TopObservation $name bucket counts do not match elapsed cycles")

    val out = Wire(new L2TopObservationBuckets)
    out.cycles := cycles
    out.grant := grantCount
    out.blocked := blockedCount
    out.starved := starvedCount
    out.busy := busyCount
    out
  }

  val dirReplRdVec = boreRead(directory.replRdVec)
  val dirRdVec = boreRead(directory.dirRdVec)
  val dirWbVec = boreRead(directory.dirWbVec)
  val dirReplRdWins = boreRead(directory.replRdWins)
  val dirGrantDirRd = boreRead(directory.grantDirRd)
  val dirGrantDirWb = boreRead(directory.grantDirWb)
  val dsWbVec = boreRead(dataStorage.wbVec)
  val dsRdVec = boreRead(dataStorage.rdVec)
  val dsAheadRdVec = boreRead(dataStorage.aheadRdVec)
  val dsAnyGrant = boreRead(dataStorage.anyGrant)
  val dsGrantWb = boreRead(dataStorage.grantWb)
  val dsGrantRd = boreRead(dataStorage.grantRd)
  val dsGrantAheadRd = boreRead(dataStorage.grantAheadRd)
  val dsDatStop = boreRead(dataStorage.datStop_r)

  val allDirProbes = Wire(Vec(module.config.sliceNum, Vec(io.dirProbe.length, new L2TopDirTimingProbeEntry)))

  private val tshr0 = tshrs.head
  private val vPipeREQs = tshrs.map(privateField[L2VPipeREQ](_, "vPipeREQ"))
  private val vPipeREQ0 = vPipeREQs.head
  private val proxyDirs = tshrs.map(privateField[L2TSHRDirectoryProxy](_, "proxyDir"))
  private val proxyDir0 = proxyDirs.head
  private val stateDirRead0 = privateField[L2TSHRDirectoryProxy#DirReadFSM](proxyDir0, "state_dirRead")
  private val dirResult0 = privateField[L2Directory.MetaReadResult](tshr0, "dirResult")
  private val replResult0 = privateField[L2Directory.ReplReadResult](tshr0, "replResult")

  val wUnlockDs0 = boreRead(privateBool(vPipeREQ0, "w_unlock_ds"))
  val wUnlockDir0 = boreRead(privateBool(vPipeREQ0, "w_unlock_dir"))
  val lockDs0 = boreRead(privateBool(vPipeREQ0, "lock_ds"))
  val expectReplace0 = boreRead(privateBool(vPipeREQ0, "expect_replace"))
  val selfUnlockDs0 = boreRead(vPipeREQ0.io.self_unlock_ds)
  val unlockSelfEvictback0 = boreRead(privateBool(vPipeREQ0, "unlock_self_evictback"))
  val wSRepl0 = boreRead(privateBool(vPipeREQ0, "w_s_repl"))
  // base folded the freeze-tag trigger_replace (L2VPipeREQ.scala:1300 there)
  // into allow_replace (L2VPipeREQ.scala:1575 here), adding the directory
  // write-back and data-beat gating. No signal keeps the old name, so the
  // tshr0 probe stays 0 and is diagnostic only.
  val triggerReplace0 = boreRead(privateBool(vPipeREQ0, "trigger_replace",
    optional = Some("renamed into allow_replace on base; diagnostic only")))
  val toDirReplRd0 = boreRead(proxyDir0.io.toDir.ReplRd)
  val sEvict0 = boreRead(privateBool(vPipeREQ0, "s_evict_up_evict"))
  val stateDirReadPreArb0 = boreRead(stateDirRead0.PreArb)
  val stateDirReadReplPreArb0 = boreRead(stateDirRead0.ReplPreArb)
  val stateDirReadPostArb0 = boreRead(stateDirRead0.PostArb)
  val dirResultState0 = boreRead(dirResult0.state)
  val dirResultDirty0 = boreRead(dirResult0.dirty)
  val dirResultHit0 = boreRead(dirResult0.hit)
  val dirResultWay0 = boreRead(dirResult0.way)
  private val fromDirDirRdResp0 = privateBool(proxyDir0, "fromDir_DirRdResp")
  private val fromDirReplRdResp0 = privateBool(proxyDir0, "fromDir_ReplRdResp")
  val dirRdResp0 = boreRead(fromDirDirRdResp0)
  val dirRdRespState0 = boreRead(dirResult0.state)
  val dirRdRespDirty0 = boreRead(dirResult0.dirty)
  val dirRdRespHit0 = boreRead(dirResult0.hit)
  val dirRdRespWay0 = boreRead(dirResult0.way)
  val replRdResp0 = boreRead(fromDirReplRdResp0)
  val replRdRespState0 = boreRead(dirResult0.state)
  val replRdRespDirty0 = boreRead(dirResult0.dirty)
  val replRdRespWay0 = boreRead(replResult0.way)
  val replWay0 = boreRead(replResult0.way)
  val replPaddr0 = boreRead(replResult0.paddr)
  val fromDirMetaState0 = boreRead(proxyDir0.io.fromDir.META.state)
  val fromDirMetaDirty0 = boreRead(proxyDir0.io.fromDir.META.dirty)
  val fromDirMetaHit0 = boreRead(proxyDir0.io.fromDir.META.hit)
  val fromDirMetaWay0 = boreRead(proxyDir0.io.fromDir.META.way)
  val fromDirReplWay0 = boreRead(proxyDir0.io.fromDir.REPL.way)
  val fromDirReplPaddr0 = boreRead(proxyDir0.io.fromDir.REPL.paddr)
  val reqDirStateProbe0 = boreRead(vPipeREQ0.io.tshr_dirResult.state)
  val reqDirDirtyProbe0 = boreRead(vPipeREQ0.io.tshr_dirResult.dirty)
  val reqDirHitProbe0 = boreRead(vPipeREQ0.io.tshr_dirResult.hit)
  val reqStateIsI0 = reqDirStateProbe0 === L2Directory.MetaState.I
  val reqBEEnable0 = reqDirStateProbe0 =/= L2Directory.MetaState.I
  val reqBEExpr0 = Fill(paramCHI.datBEWidth, reqBEEnable0)
  val sEvictCbWrData0 = boreRead(privateBool(vPipeREQ0, "s_evict_dn_cbwrdata0"))
  val sEvictCbWrData2 = boreRead(privateBool(vPipeREQ0, "s_evict_dn_cbwrdata2"))

  private def connectBEStage(stage: L2TopBEStageProbe, valid: Bool, ready: Bool, bits: CHIBundleDAT): Unit = {
    val validProbe = boreRead(valid)
    stage.valid := validProbe
    stage.ready := boreRead(ready)
    stage.opcode := Mux(validProbe, boreRead(bits.Opcode.get), 0.U)
    stage.dataId := Mux(validProbe, boreRead(bits.DataID.get), 0.U)
    stage.be := Mux(validProbe, boreRead(bits.BE.get), 0.U)
    stage.data := Mux(validProbe, boreRead(bits.Data.get), 0.U)
  }

  connectBEStage(io.beProbe.vpipeReq, vPipeREQ0.io.DnTXDAT.valid, vPipeREQ0.io.DnTXDAT.ready, vPipeREQ0.io.DnTXDAT.bits)
  connectBEStage(io.beProbe.tshr, tshr0.io.DnTXDAT.valid, tshr0.io.DnTXDAT.ready, tshr0.io.DnTXDAT.bits)
  vPipeREQs.zip(tshrs).zipWithIndex.foreach { case ((vPipeREQ, tshr), i) =>
    connectBEStage(io.beProbe.vpipeReqAll(i), vPipeREQ.io.DnTXDAT.valid, vPipeREQ.io.DnTXDAT.ready, vPipeREQ.io.DnTXDAT.bits)
    connectBEStage(io.beProbe.tshrAll(i), tshr.io.DnTXDAT.valid, tshr.io.DnTXDAT.ready, tshr.io.DnTXDAT.bits)
    val reqDirState = boreRead(vPipeREQ.io.tshr_dirResult.state)
    val reqDirDirty = boreRead(vPipeREQ.io.tshr_dirResult.dirty)
    val reqDirHit = boreRead(vPipeREQ.io.tshr_dirResult.hit)
    val reqStateIsI = reqDirState === L2Directory.MetaState.I
    val reqStateNotI = reqDirState =/= L2Directory.MetaState.I
    io.beProbe.reqDirStateAll(i) := reqDirState
    io.beProbe.reqDirDirtyAll(i) := reqDirDirty
    io.beProbe.reqDirHitAll(i) := reqDirHit
    io.beProbe.reqStateIsIAll(i) := reqStateIsI
    io.beProbe.reqStateNotIAll(i) := reqStateNotI
    io.beProbe.reqBEExprAll(i) := Fill(paramCHI.datBEWidth, reqStateNotI)
    io.beProbe.sEvictCbWrData0All(i) := boreRead(privateBool(vPipeREQ, "s_evict_dn_cbwrdata0"))
    io.beProbe.sEvictCbWrData2All(i) := boreRead(privateBool(vPipeREQ, "s_evict_dn_cbwrdata2"))
  }
  connectBEStage(io.beProbe.slice, slice0.io.DnTXDAT.valid, slice0.io.DnTXDAT.ready, slice0.io.DnTXDAT.bits)
  connectBEStage(io.beProbe.chiLink, chiLink.io.txdat.valid, chiLink.io.txdat.ready, chiLink.io.txdat.bits)
  io.beProbe.top.valid := boreRead(module.io.chi.txdat.flitv)
  io.beProbe.top.ready := boreRead(module.io.chi.txdat.lcrdv)
  io.beProbe.top.opcode := Mux(io.beProbe.top.valid, boreRead(module.io.chi.txdat.flit.Opcode.get), 0.U)
  io.beProbe.top.dataId := Mux(io.beProbe.top.valid, boreRead(module.io.chi.txdat.flit.DataID.get), 0.U)
  io.beProbe.top.be := Mux(io.beProbe.top.valid, boreRead(module.io.chi.txdat.flit.BE.get), 0.U)
  io.beProbe.top.data := Mux(io.beProbe.top.valid, boreRead(module.io.chi.txdat.flit.Data.get), 0.U)
  io.beProbe.reqDirState := reqDirStateProbe0
  io.beProbe.reqDirDirty := reqDirDirtyProbe0
  io.beProbe.reqDirHit := reqDirHitProbe0
  io.beProbe.reqStateIsI := boreRead(reqStateIsI0)
  io.beProbe.reqStateNotI := boreRead(reqBEEnable0)
  io.beProbe.reqBEEnable := boreRead(reqBEEnable0)
  io.beProbe.reqBEExpr := boreRead(reqBEExpr0)
  io.beProbe.sEvictCbWrData0 := sEvictCbWrData0
  io.beProbe.sEvictCbWrData2 := sEvictCbWrData2

  slices.zipWithIndex.foreach { case (slice, sliceIdx) =>
    io.sliceTrainValid(sliceIdx) := boreRead(slice.io.bopTrain.valid)
    io.sliceTrainAddr(sliceIdx) := boreRead(slice.io.bopTrain.bits)
    val sliceTshrSet = sliceTshrs(sliceIdx)
    val sliceProxyDirs = sliceTshrSet.map(privateField[L2TSHRDirectoryProxy](_, "proxyDir"))
    sliceProxyDirs.zip(sliceTshrSet).zip(allDirProbes(sliceIdx)).foreach { case ((proxyDir, tshr), probe) =>
    val vPipeREQ = privateField[L2VPipeREQ](tshr, "vPipeREQ")
    val proxyDS = privateField[L2TSHRDataStorageProxy](tshr, "proxyDS")
    val dirResult = privateField[L2Directory.MetaReadResult](tshr, "dirResult")
    val tshrAlloc = privateBool(tshr, "tshr_alloc")
    val tshrReuse = privateBool(tshr, "tshr_reuse")
    val tshrDealloc = privateBool(tshr, "tshr_dealloc")
    val tshrEnterREQ = privateBool(tshr, "tshr_enter_REQ")
    val tshrEnterEVT = privateBool(tshr, "tshr_enter_EVT")
    val tshrEnterSNP = privateBool(tshr, "tshr_enter_SNP")
    val tshrPaddr = privateField[UInt](tshr, "tshr_paddr")
    val pRxReq = privateField[FlitREQStripped](vPipeREQ, "p_rxreq")
    val reqActive = privateBool(vPipeREQ, "active")
    val rxReqFire = privateBool(vPipeREQ, "rxreq_fire")
    val metaWriteREQMask = privateField[L2Directory.MetaWriteMask](tshr, "meta_write_REQ_mask")
    val metaWriteREQMeta = privateField[L2Directory.Meta](tshr, "meta_write_REQ_meta")
    val metaWriteEVTMask = privateField[L2Directory.MetaWriteMask](tshr, "meta_write_EVT_mask")
    val metaWriteSNPMask = privateField[L2Directory.MetaWriteMask](tshr, "meta_write_SNP_mask")
    val metaDrop = privateBool(tshr, "meta_drop")
    val metaWrStateEvictbackI = privateBool(vPipeREQ, "meta_wr_state_evictback_I")
    val metaWrStateEvictbackIEvict = privateBool(vPipeREQ, "meta_wr_state_evictback_I_evict")
    val metaWrStateEvictbackIWrite = privateBool(vPipeREQ, "meta_wr_state_evictback_I_write")
    val fireTxreqEvict = privateBool(vPipeREQ, "fire_txreq_evict")
    val fireTxreqWriteBackFull = privateBool(vPipeREQ, "fire_txreq_writebackfull")
    val fireTxreqWriteEvictFull = privateBool(vPipeREQ, "fire_txreq_writeevictfull")
    val fireTxreqWriteEvictOrEvict = privateBool(vPipeREQ, "fire_txreq_writeevictorevict")
    val expectDnEvictCompdbid = privateBool(vPipeREQ, "expect_dn_evict_compdbid")
    val wEvictDnCompdbid = privateBool(vPipeREQ, "w_evict_dn_compdbid")
    val dnRxrspComp = privateBool(vPipeREQ, "dn_rxrsp_comp")
    val dnRxrspCompdbidresp = privateBool(vPipeREQ, "dn_rxrsp_compdbidresp")
    val pRxreqEvictback = privateBool(vPipeREQ, "p_rxreq_evictback")
    val wSnpResp0 = privateBool(vPipeREQ, "w_snpresp0")
    val wSnpResp2 = privateBool(vPipeREQ, "w_snpresp2")
    val sSnpCompAck = privateBool(vPipeREQ, "s_snpcompack")
    val sDnTxReq = privateBool(vPipeREQ, "s_dn_txreq")
    val wDnPCrdGrant = privateBool(vPipeREQ, "w_dn_pcrdgrant")
    val wDsResp = privateBool(vPipeREQ, "w_ds_resp")
    val wSRdDnCompAck = privateBool(vPipeREQ, "w_s_rd_dn_compack")
    val sRdDnCompAck = privateBool(vPipeREQ, "s_rd_dn_compack")
    val wRdDnData0 = privateBool(vPipeREQ, "w_rd_dn_data0")
    val wRdDnData2 = privateBool(vPipeREQ, "w_rd_dn_data2")
    val wRdDnComp = privateBool(vPipeREQ, "w_rd_dn_comp")
    val wRdUpCompAck = privateBool(vPipeREQ, "w_rd_up_compack")
    val wSRdUpCompData0 = privateBool(vPipeREQ, "w_s_rd_up_compdata0")
    val sRdUpCompData0 = privateBool(vPipeREQ, "s_rd_up_compdata0")
    val wSRdUpCompData2 = privateBool(vPipeREQ, "w_s_rd_up_compdata2")
    val sRdUpCompData2 = privateBool(vPipeREQ, "s_rd_up_compdata2")
    val wSRdUpComp = privateBool(vPipeREQ, "w_s_rd_up_comp")
    val sRdUpComp = privateBool(vPipeREQ, "s_rd_up_comp")
    val wUnlockDir = privateBool(vPipeREQ, "w_unlock_dir")
    val wUnlockDs = privateBool(vPipeREQ, "w_unlock_ds")
    val wSRepl = privateBool(vPipeREQ, "w_s_repl")
    val sRepl = privateBool(vPipeREQ, "s_repl")
    val wSEvictUpEvict = privateBool(vPipeREQ, "w_s_evict_up_evict")
    val sEvictUpEvict = privateBool(vPipeREQ, "s_evict_up_evict")
    val wEvictSDnTxReq = privateBool(vPipeREQ, "w_evict_s_dn_txreq")
    val wEvictDnComp = privateBool(vPipeREQ, "w_evict_dn_comp")
    val wSEvictDnCbWrData0 = privateBool(vPipeREQ, "w_s_evict_dn_cbwrdata0")
    val wSEvictDnCbWrData2 = privateBool(vPipeREQ, "w_s_evict_dn_cbwrdata2")
    val sEvictDnCbWrData0State = privateBool(vPipeREQ, "s_evict_dn_cbwrdata0")
    val sEvictDnCbWrData2State = privateBool(vPipeREQ, "s_evict_dn_cbwrdata2")
    val sEvictDnCompAck = privateBool(vPipeREQ, "s_evict_dn_compack")
    // base renamed w_evict_peer_unlock_ds to w_s_evict_peer_unlock_ds
    // (L2VPipeREQ.scala:386). It is only bit 32 of the diagnostic
    // reqStateMask, which no assertion reads, so the bit stays 0.
    val wEvictPeerUnlockDs = privateBool(vPipeREQ, "w_evict_peer_unlock_ds",
      optional = Some("renamed to w_s_evict_peer_unlock_ds on base; diagnostic bit only"))
    val dsRdEvictback = privateBool(vPipeREQ, "ds_rd_evictback")
    val dsRd = privateBool(vPipeREQ, "ds_rd")
    val fromDSDSBufAheadRdArbComp = privateBool(proxyDS, "fromDS_DSBufAheadRdArbComp")
    val fromDSDSBufAheadRdResp = privateBool(proxyDS, "fromDS_DSBufAheadRdResp")
    val fromDSDSBufRdArbComp = privateBool(proxyDS, "fromDS_DSBufRdArbComp")
    val fromDSDSBufRdResp = privateBool(proxyDS, "fromDS_DSBufRdResp")
    val fromDSDSBufWbArbComp = privateBool(proxyDS, "fromDS_DSBufWbArbComp")
    val fromDSDSBufWbComp = privateBool(proxyDS, "fromDS_DSBufWbComp")
    val fromDirDirRdArbComp = privateBool(proxyDir, "fromDir_DirRdArbComp")
    val fromDirDirRdResp = privateBool(proxyDir, "fromDir_DirRdResp")
    val fromDirDirWbArbComp = privateBool(proxyDir, "fromDir_DirWbArbComp")
    val fromDirReplRdArbComp = privateBool(proxyDir, "fromDir_ReplRdArbComp")
    val fromDirReplRdResp = privateBool(proxyDir, "fromDir_ReplRdResp")
    probe.valid := boreRead(tshr.io.valid)
    probe.alloc := boreRead(tshrAlloc)
    probe.reuse := boreRead(tshrReuse)
    probe.dealloc := boreRead(tshrDealloc)
    probe.enterREQ := boreRead(tshrEnterREQ)
    probe.enterEVT := boreRead(tshrEnterEVT)
    probe.enterSNP := boreRead(tshrEnterSNP)
    probe.tshrPaddr := boreRead(tshrPaddr)
    probe.rxReqFire := boreRead(rxReqFire)
    // Allocation's PA is valid before tshr_paddr is registered. Keep REQ
    // allocation distinct from the existing EVB allocation probes.
    probe.reqAlloc := boreRead(tshr.io.fromAlloc.alloc.REQ)
    probe.reqReuse := boreRead(tshr.io.fromAlloc.reuse.REQ)
    probe.reqAllocPaddr := boreRead(tshr.io.fromAlloc.paddr)
    // base dropped p_rxreq_isPrefetch (freeze L2VPipeREQ.scala:226 decoded it
    // from TxnID(7)). Derive it after boreRead: the comparison has to live in
    // this module, since the fields are not visible across the hierarchy.
    // An injected prefetch is the only StashShared carrying pfSrcID.
    val pRxReqSrcID = boreRead(pRxReq.SrcID)
    val pRxReqOpcode = boreRead(pRxReq.Opcode)
    probe.pRxReqIsPrefetch := pRxReqSrcID === module.config.pfSrcID.U &&
      pRxReqOpcode === CCHIOpcode.StashShared.U
    probe.pRxReqOpcode := pRxReqOpcode
    probe.pRxReqSrcID := pRxReqSrcID
    probe.pRxReqExpCompData := boreRead(pRxReq.ExpCompData)
    probe.pRxReqSize := boreRead(pRxReq.Size)
    probe.pRxReqEvictback := boreRead(pRxreqEvictback)
    probe.reqActive := boreRead(reqActive)
    val upTxRspValid = boreRead(vPipeREQ.io.UpTXRSP.valid)
    val upTxRspReady = boreRead(vPipeREQ.io.UpTXRSP.ready)
    val dnTxRspValid = boreRead(vPipeREQ.io.DnTXRSP.valid)
    val dnTxRspReady = boreRead(vPipeREQ.io.DnTXRSP.ready)
    probe.upTxRspFire := upTxRspValid && upTxRspReady
    probe.upTxDatFire := boreRead(vPipeREQ.io.UpTXDAT.valid) && boreRead(vPipeREQ.io.UpTXDAT.ready)
    probe.bopTrainValid := boreRead(vPipeREQ.io.bopTrain.valid)
    probe.upTxRspOpcode := Mux(upTxRspValid, boreRead(vPipeREQ.io.UpTXRSP.bits.Opcode), 0.U)
    probe.dnTxRspFire := dnTxRspValid && dnTxRspReady
    probe.dnTxRspOpcode := Mux(dnTxRspValid, boreRead(vPipeREQ.io.DnTXRSP.bits.Opcode.get), 0.U)
    probe.reqStateMask := VecInit(Seq(
      wSnpResp0, wSnpResp2, sSnpCompAck, sDnTxReq, wDnPCrdGrant, wDsResp,
      wSRdDnCompAck, sRdDnCompAck, wRdDnData0, wRdDnData2, wRdDnComp, wRdUpCompAck,
      wSRdUpCompData0, sRdUpCompData0, wSRdUpCompData2, sRdUpCompData2, wSRdUpComp, sRdUpComp,
      wUnlockDir, wUnlockDs, wSRepl, sRepl, wSEvictUpEvict, sEvictUpEvict, wEvictSDnTxReq, wEvictDnComp,
      wEvictDnCompdbid, wSEvictDnCbWrData0, wSEvictDnCbWrData2, sEvictDnCbWrData0State,
      sEvictDnCbWrData2State, sEvictDnCompAck, wEvictPeerUnlockDs
    ).map(boreRead(_))).asUInt
    probe.dirRdReq := boreRead(proxyDir.io.toDir.DirRd)
    probe.dirRdArbComp := boreRead(fromDirDirRdArbComp)
    probe.dirRdResp := boreRead(fromDirDirRdResp)
    probe.replRdReq := boreRead(proxyDir.io.toDir.ReplRd)
    probe.replRdArbComp := boreRead(fromDirReplRdArbComp)
    probe.dirWbReq := boreRead(proxyDir.io.toDir.DirWb)
    probe.dirWbLand := boreRead(fromDirDirWbArbComp)
    probe.paddr := boreRead(proxyDir.io.toDir.PADDR)
    probe.fromDirMetaState := boreRead(proxyDir.io.fromDir.META.state)
    probe.fromDirMetaDirty := boreRead(proxyDir.io.fromDir.META.dirty)
    probe.fromDirMetaHit := boreRead(proxyDir.io.fromDir.META.hit)
    probe.fromDirMetaWay := boreRead(proxyDir.io.fromDir.META.way)
    probe.dirResultState := boreRead(dirResult.state)
    probe.dirResultDirty := boreRead(dirResult.dirty)
    probe.dirResultHit := boreRead(dirResult.hit)
    probe.dirResultPfSource := boreRead(dirResult.pfSource)
    probe.fromDirMetaPfSource := boreRead(proxyDir.io.fromDir.META.pfSource)
    probe.dirResultPrefetched := probe.dirResultPfSource =/= 0.U
    probe.fromDirMetaPrefetched := probe.fromDirMetaPfSource =/= 0.U
    probe.fromDirReplPaddr := boreRead(proxyDir.io.fromDir.REPL.paddr)
    probe.tagWrite := boreRead(vPipeREQ.io.tshr_tag_write_en)
    probe.metaWrReqPrefetched := boreRead(metaWriteREQMask.pfSource)
    probe.metaWrReqMetaPfSource := boreRead(metaWriteREQMeta.pfSource)
    probe.metaWrReqMetaPrefetched := probe.metaWrReqMetaPfSource =/= 0.U
    probe.dirResultWay := boreRead(dirResult.way)
    probe.rdDone := boreRead(proxyDir.io.rd_done)
    probe.replRdResp := boreRead(fromDirReplRdResp)
    probe.metaWrStateEvictbackI := boreRead(metaWrStateEvictbackI)
    probe.metaWrStateEvictbackIEvict := boreRead(metaWrStateEvictbackIEvict)
    probe.metaWrStateEvictbackIWrite := boreRead(metaWrStateEvictbackIWrite)
    probe.fireTxreqEvict := boreRead(fireTxreqEvict)
    probe.fireTxreqWriteBackFull := boreRead(fireTxreqWriteBackFull)
    probe.fireTxreqWriteEvictFull := boreRead(fireTxreqWriteEvictFull)
    probe.fireTxreqWriteEvictOrEvict := boreRead(fireTxreqWriteEvictOrEvict)
    probe.expectDnEvictCompdbid := boreRead(expectDnEvictCompdbid)
    probe.wEvictDnCompdbid := boreRead(wEvictDnCompdbid)
    probe.dnRxrspComp := boreRead(dnRxrspComp)
    probe.dnRxrspCompdbidresp := boreRead(dnRxrspCompdbidresp)
    probe.pRxreqEvictback := boreRead(pRxreqEvictback)
    probe.dsRdEvictback := boreRead(dsRdEvictback)
    probe.dsRdEn := boreRead(dsRd)
    probe.dsRdDone := boreRead(vPipeREQ.io.ds_rd_done)
    probe.dsReqRd := boreRead(proxyDS.io.toDS.DSBufRd)
    probe.dsRdArbComp := boreRead(fromDSDSBufRdArbComp)
    probe.dsRdResp := boreRead(fromDSDSBufRdResp)
    probe.dsReqAheadRd := boreRead(proxyDS.io.toDS.DSBufAheadRd)
    probe.dsAheadRdArbComp := boreRead(fromDSDSBufAheadRdArbComp)
    probe.dsAheadRdResp := boreRead(fromDSDSBufAheadRdResp)
    probe.dsReqWb := boreRead(proxyDS.io.toDS.DSBufWb)
    probe.dsWbArbComp := boreRead(fromDSDSBufWbArbComp)
    probe.dsWbComp := boreRead(fromDSDSBufWbComp)
    probe.dsRdData := boreRead(proxyDS.io.fromDS.DATA)
    probe.metaWrDirtySASet := boreRead(privateBool(vPipeREQ, "meta_wr_dirty_sa_set"))
    probe.metaWrReqState := boreRead(metaWriteREQMask.state)
    probe.metaWrReqDirty := boreRead(metaWriteREQMask.dirty)
    probe.metaWrReqMetaState := boreRead(metaWriteREQMeta.state)
    probe.metaWrReqMetaDirty := boreRead(metaWriteREQMeta.dirty)
    probe.metaWrEvtState := boreRead(metaWriteEVTMask.state)
    probe.metaWrEvtDirty := boreRead(metaWriteEVTMask.dirty)
    probe.metaWrSnpState := boreRead(metaWriteSNPMask.state)
    probe.metaWrSnpDirty := boreRead(metaWriteSNPMask.dirty)
    probe.metaDrop := boreRead(metaDrop)
    }
  }

  io.dirProbe := allDirProbes(0)
  io.dirProbeBySlice := allDirProbes

  sliceTshrs.zipWithIndex.foreach { case (sliceTshrSet, sliceIdx) =>
    val allocator = slices(sliceIdx).tshrAlloc
    // EvictBack allocates through alloc.EVB/reuse.EVB (L2TSHRAlloc.scala:342-344),
    // never the REQ channel, so the target mask must come from the EVB bits.
    io.sliceAllocVec(sliceIdx) := VecInit(allocator.io.toTSHR.map(t => boreRead(t.alloc.EVB))).asUInt
    io.sliceReuseVec(sliceIdx) := VecInit(allocator.io.toTSHR.map(t => boreRead(t.reuse.EVB))).asUInt
    // Base sends EvictBack on UpTXEVB (L2VPipeREQ.scala:1693, L2TSHR.scala:606),
    // not UpTXRSP. fastArb wires each TSHR's UpTXEVB straight through
    // (L2Common.scala:28), so a source fire is that port's own fire; the slice
    // fire is the arbiter output EVB that feeds the allocator
    // (L2TSHRCtrl.scala:127,143).
    io.sliceEvictBackFire(sliceIdx) := {
      val evb = privateField[DecoupledIO[L2VPipeREQ.FlitEVB]](slices(sliceIdx), "EVB")
      boreRead(evb.valid) && boreRead(evb.ready)
    }
    io.sliceSourceFireVec(sliceIdx) := VecInit(sliceTshrSet.map { tshr =>
      boreRead(tshr.io.UpTXEVB.valid) && boreRead(tshr.io.UpTXEVB.ready)
    }).asUInt
    sliceTshrSet.zipWithIndex.foreach { case (tshr, tshrIdx) =>
      val probe = io.sliceTshrProbe(sliceIdx)(tshrIdx)
      val vPipeREQ = privateField[L2VPipeREQ](tshr, "vPipeREQ")
      val proxyDir = privateField[L2TSHRDirectoryProxy](tshr, "proxyDir")
      val dirResult = privateField[L2Directory.MetaReadResult](tshr, "dirResult")
      val replResult = privateField[L2Directory.ReplReadResult](tshr, "replResult")
      val stateDirRead = privateField[L2TSHRDirectoryProxy#DirReadFSM](proxyDir, "state_dirRead")
      val pRxReq = privateField[FlitREQStripped](vPipeREQ, "p_rxreq")
      val pRxReqEvictback = privateBool(vPipeREQ, "p_rxreq_evictback")
      val tshrPaddr = privateField[UInt](tshr, "tshr_paddr")
      probe.valid := boreRead(tshr.io.valid)
      probe.alloc := boreRead(privateBool(tshr, "tshr_alloc"))
      probe.reuse := boreRead(privateBool(tshr, "tshr_reuse"))
      probe.dealloc := boreRead(privateBool(tshr, "tshr_dealloc"))
      probe.tshrPaddr := boreRead(tshrPaddr)
      val pRxReqSrcID = boreRead(pRxReq.SrcID)
      val pRxReqOpcode = boreRead(pRxReq.Opcode)
      probe.pRxReqIsPrefetch := pRxReqSrcID === module.config.pfSrcID.U &&
        pRxReqOpcode === CCHIOpcode.StashShared.U
      probe.pRxReqOpcode := pRxReqOpcode
      probe.pRxReqEvictback := boreRead(pRxReqEvictback)
      probe.reqStateMask := Cat(0.U(40.W), probe.sEvict, 0.U(23.W))
      probe.sEvict := boreRead(privateBool(vPipeREQ, "s_evict_up_evict"))
      probe.dirRdResp := boreRead(privateBool(proxyDir, "fromDir_DirRdResp"))
      probe.dirRdRespState := boreRead(dirResult.state)
      probe.dirRdRespDirty := boreRead(dirResult.dirty)
      probe.dirRdRespHit := boreRead(dirResult.hit)
      probe.dirRdRespWay := boreRead(dirResult.way)
      probe.replRdResp := boreRead(privateBool(proxyDir, "fromDir_ReplRdResp"))
      probe.replRdRespState := boreRead(dirResult.state)
      probe.replRdRespDirty := boreRead(dirResult.dirty)
      probe.replRdRespWay := boreRead(replResult.way)
      probe.replPaddr := boreRead(replResult.paddr)
      probe.fromDirMetaState := boreRead(proxyDir.io.fromDir.META.state)
      probe.fromDirMetaDirty := boreRead(proxyDir.io.fromDir.META.dirty)
      probe.fromDirMetaHit := boreRead(proxyDir.io.fromDir.META.hit)
      probe.fromDirMetaWay := boreRead(proxyDir.io.fromDir.META.way)
      probe.fromDirReplWay := boreRead(proxyDir.io.fromDir.REPL.way)
      probe.fromDirReplPaddr := boreRead(proxyDir.io.fromDir.REPL.paddr)
    }
  }

  val dirHasWork = dirReplRdVec.asUInt.orR || dirRdVec.asUInt.orR || dirWbVec.asUInt.orR
  val dirGrant = dirReplRdWins || dirGrantDirRd || dirGrantDirWb
  val dirBlocked = dirHasWork && !dirGrant
  val dirStarved = !dirHasWork

  val dsHasWork = dsWbVec.asUInt.orR || dsRdVec.asUInt.orR || dsAheadRdVec.asUInt.orR
  val dsGrant = dsAnyGrant
  val dsBusy = dsDatStop
  val dsBlocked = dsHasWork && !dsGrant && !dsBusy
  val dsStarved = !dsHasWork && !dsBusy

  val upRXREQGrant = module.io.t1p(0).UpREQ.fire
  val upRXREQBlocked = module.io.t1p(0).UpREQ.valid && !module.io.t1p(0).UpREQ.ready
  val upRXREQStarved = !module.io.t1p(0).UpREQ.valid

  val upTXDATGrant = module.io.t1p(0).DnDAT.fire
  val upTXDATBlocked = module.io.t1p(0).DnDAT.valid && !module.io.t1p(0).DnDAT.ready
  val upTXDATStarved = !module.io.t1p(0).DnDAT.valid

  io.allocator.reqValid := boreRead(tshrAllocator.io.fromTSHRCtrl.RXREQ.valid)
  io.allocator.reqReady := boreRead(tshrAllocator.io.fromTSHRCtrl.RXREQ.ready)
  io.allocator.reqPaddr := boreRead(tshrAllocator.io.fromTSHRCtrl.RXREQ.bits.Addr)
  io.allocator.postclusterValid := boreRead(allocatorPostcluster(reqClusterIndex).valid)
  io.allocator.postclusterReady := boreRead(allocatorPostcluster(reqClusterIndex).ready)
  io.allocator.paddrHitVec := VecInit(allocatorPaddrHitVec(reqClusterIndex).map(boreRead)).asUInt
  io.allocator.canReuseVec := VecInit(allocatorCanReuseVec(reqClusterIndex).map(boreRead)).asUInt
  io.allocator.allocVec := VecInit(tshrAllocator.io.toTSHR.map(t => boreRead(t.alloc.REQ))).asUInt
  io.allocator.reuseVec := VecInit(tshrAllocator.io.toTSHR.map(t => boreRead(t.reuse.REQ))).asUInt
  io.allocator.allocAny := boreRead(allocatorAllocAny(reqClusterIndex))
  io.allocator.reuseAny := boreRead(allocatorReuseAny(reqClusterIndex))

  io.directory := observe("directory", dirGrant, dirBlocked, dirStarved, false.B)
  io.dataStorage := observe("dataStorage", dsGrant, dsBlocked, dsStarved, dsBusy)
  io.upRXREQ := observe("upRXREQ", upRXREQGrant, upRXREQBlocked, upRXREQStarved, false.B)
  io.upTXDAT := observe("upTXDAT", upTXDATGrant, upTXDATBlocked, upTXDATStarved, false.B)
  io.tshr0.wUnlockDs := wUnlockDs0
  io.tshr0.wUnlockDir := wUnlockDir0
  io.tshr0.lockDs := lockDs0
  io.tshr0.expectReplace := expectReplace0
  io.tshr0.selfUnlockDs := selfUnlockDs0
  io.tshr0.unlockSelfEvictback := unlockSelfEvictback0
  io.tshr0.wSRepl := wSRepl0
  io.tshr0.triggerReplace := triggerReplace0
  io.tshr0.toDirReplRd := toDirReplRd0
  io.tshr0.sEvict := sEvict0
  io.tshr0.stateDirReadPreArb := stateDirReadPreArb0
  io.tshr0.stateDirReadReplPreArb := stateDirReadReplPreArb0
  io.tshr0.stateDirReadPostArb := stateDirReadPostArb0
  io.tshr0.dirResultState := dirResultState0
  io.tshr0.dirResultDirty := dirResultDirty0
  io.tshr0.dirResultHit := dirResultHit0
  io.tshr0.dirResultWay := dirResultWay0
  io.tshr0.dirRdResp := dirRdResp0
  io.tshr0.dirRdRespState := dirRdRespState0
  io.tshr0.dirRdRespDirty := dirRdRespDirty0
  io.tshr0.dirRdRespHit := dirRdRespHit0
  io.tshr0.dirRdRespWay := dirRdRespWay0
  io.tshr0.replRdResp := replRdResp0
  io.tshr0.replRdRespState := replRdRespState0
  io.tshr0.replRdRespDirty := replRdRespDirty0
  io.tshr0.replRdRespWay := replRdRespWay0
  io.tshr0.replWay := replWay0
  io.tshr0.replPaddr := replPaddr0
  io.tshr0.fromDirMetaState := fromDirMetaState0
  io.tshr0.fromDirMetaDirty := fromDirMetaDirty0
  io.tshr0.fromDirMetaHit := fromDirMetaHit0
  io.tshr0.fromDirMetaWay := fromDirMetaWay0
  io.tshr0.fromDirReplWay := fromDirReplWay0
  io.tshr0.fromDirReplPaddr := fromDirReplPaddr0

  private def accumulate(cond: Bool): UInt = {
    val count = RegInit(0.U(64.W))
    when(cond) { count := count + 1.U }
    count
  }

  io.bopTrainFire := boreRead(bop.io.train.valid) && boreRead(bop.io.train.ready)
  io.bopTrainFireCount := accumulate(io.bopTrainFire)
  io.bopTrainReceiveCount := io.bopTrainFireCount
  slices.zipWithIndex.foreach { case (slice, sliceIdx) =>
    io.sliceTrainPulseCount(sliceIdx) := accumulate(boreRead(slice.io.bopTrain.valid))
    io.sliceEvictBackFireCount(sliceIdx) := accumulate(io.sliceEvictBackFire(sliceIdx))
  }
  io.bopTrainAddr := boreRead(bop.io.train.bits)
  io.bopReqAddr := boreRead(bop.io.req.bits)
  // Drop pulses qualify the current training attempt before the edge. req.bits
  // still contains the preceding attempt until that edge updates the register.
  io.bopDropAddr := boreRead(privateField[UInt](bop, "newAddress"))
  val bopReqValid = boreRead(bop.io.req.valid)
  val bopReqReady = boreRead(bop.io.req.ready)
  val bopTrainValid = boreRead(bop.io.train.valid)
  val bopTrainReady = boreRead(bop.io.train.ready)
  io.bopReqValid := bopReqValid
  io.bopReqReady := bopReqReady
  io.bopReqEnqReady := bopReqReady
  io.bopReqFire := bopReqValid && bopReqReady
  io.bopTrainValid := bopTrainValid
  io.bopTrainReady := bopTrainReady
  // Test-only bores of the REQ-local event; production statistics stay inside
  // REQ as XSPerfAccumulate and acquire no cross-module observation IO.
  val hitTrains = sliceTshrs.map(_.map { tshr =>
    val req = privateField[L2VPipeREQ](tshr, "vPipeREQ")
    (boreRead(privateBool(req, "prefetched_hit_train")), boreRead(req.io.bopTrain.bits))
  })
  io.prefetchedHitTrainCount := PopCount(VecInit(hitTrains.flatten.map(_._1)))
  io.prefetchedHitTrainAddr := Mux1H(hitTrains.flatten)
  hitTrains.zipWithIndex.foreach { case (events, sliceIdx) =>
    io.prefetchedHitTrainValidBySlice(sliceIdx) := VecInit(events.map(_._1)).asUInt.orR
    io.prefetchedHitTrainAddrBySlice(sliceIdx) := Mux1H(events)
  }
  io.bopQueueOverwrite := false.B
  // Observe the production queue's issued flit, after the issue-enable gate.
  // Every per-slice event remains visible when multiple slices issue together.
  slices.indices.foreach { i =>
    io.prefetchQueueOverwrite(i) := productionPfQueues.lift(i)
      .map(q => boreRead(privateBool(q, "overwrite"))).getOrElse(false.B)
    io.prefetchQueueValid(i) := pfQueues.lift(i).map(q => boreRead(q.valid)).getOrElse(false.B)
    io.prefetchQueueReady(i) := pfQueues.lift(i).map(q => boreRead(q.ready)).getOrElse(false.B)
    io.bopPrefetchIssueBySlice(i) := pfQueues.lift(i).map(q => boreRead(q.valid) && boreRead(q.ready)).getOrElse(false.B)
    io.bopPrefetchIssueAddrBySlice(i) := pfQueues.lift(i).map(q => boreRead(q.bits.Addr)).getOrElse(0.U)
  }
  io.bopPrefetchIssue := io.bopPrefetchIssueBySlice.asUInt.orR
  io.bopPrefetchIssueAddr := Mux1H(io.bopPrefetchIssueBySlice.zip(io.bopPrefetchIssueAddrBySlice))
  io.bopCrossPageDrop := boreRead(bop.io.crossPageDrop)
  io.bopDisabledDrop := boreRead(bop.io.disabledDrop)
  io.bopDelayQueueFullDrop := boreRead(bop.io.delayQueueFullDrop)
  io.bopTrainStall := bopTrainValid && !bopTrainReady

  io.tshr0Counts.wUnlockDs := accumulate(wUnlockDs0)
  io.tshr0Counts.wUnlockDir := accumulate(wUnlockDir0)
  io.tshr0Counts.lockDs := accumulate(lockDs0)
  io.tshr0Counts.expectReplace := accumulate(expectReplace0)
  io.tshr0Counts.selfUnlockDs := accumulate(selfUnlockDs0)
  io.tshr0Counts.unlockSelfEvictback := accumulate(unlockSelfEvictback0)
  io.tshr0Counts.wSRepl := accumulate(wSRepl0)
  io.tshr0Counts.triggerReplace := accumulate(triggerReplace0)
  io.tshr0Counts.toDirReplRd := accumulate(toDirReplRd0)
  io.tshr0Counts.sEvict := accumulate(sEvict0)

  io.grants.dirRd := accumulate(dirGrantDirRd)
  io.grants.dirWb := accumulate(dirGrantDirWb)
  io.grants.dirReplRd := accumulate(dirReplRdWins)
  io.grants.dsWb := accumulate(dsGrantWb)
  io.grants.dsRd := accumulate(dsGrantRd)
  io.grants.dsAheadRd := accumulate(dsGrantAheadRd)
  io.rawReqCounts.replRdVec := accumulate(dirReplRdVec.asUInt.orR)
  io.rawReqCounts.dirRdVec := accumulate(dirRdVec.asUInt.orR)
  io.rawReqCounts.dirWbVec := accumulate(dirWbVec.asUInt.orR)
  io.rawReqCounts.wbVec := accumulate(dsWbVec.asUInt.orR)
  io.rawReqCounts.rdVec := accumulate(dsRdVec.asUInt.orR)
  dataStorage.io.fromTSHR.zip(io.dsWb).foreach { case (fromTSHR, probe) =>
    probe.valid := boreRead(fromTSHR.DSBufWb)
    probe.set := boreRead(fromTSHR.SET)
    probe.way := boreRead(fromTSHR.WAY)
    probe.data := boreRead(fromTSHR.DATA)
  }
  tshrs.zip(io.debug).foreach { case (tshr, probe) =>
    val proxyDS = privateField[L2TSHRDataStorageProxy](tshr, "proxyDS")
    val vPipeREQ = privateField[L2VPipeREQ](tshr, "vPipeREQ")
    val dsWrite = privateField[L2TSHRDataStorageProxy#DSWriteFSM](proxyDS, "state_dsWrite")
    probe.tbufWenLast := boreRead(privateBool(tshr, "tshr_buffer_wen_last"))
    probe.dsWbCancel := boreRead(proxyDS.io.wb_cancel)
    probe.dsWritePreArb := boreRead(dsWrite.PreArb)
    probe.dsWritePostArb := boreRead(dsWrite.PostArb)
    probe.dsWritePreArbPostArb := boreRead(dsWrite.PreArb_PostArb)
    probe.dsWriteDone := boreRead(dsWrite.Done)
    probe.reqWData0 := boreRead(privateBool(vPipeREQ, "w_rd_dn_data0"))
    probe.reqWData2 := boreRead(privateBool(vPipeREQ, "w_rd_dn_data2"))
    probe.reqWComp := boreRead(privateBool(vPipeREQ, "w_rd_dn_comp"))
    probe.reqWDsResp := boreRead(privateBool(vPipeREQ, "w_ds_resp"))
    probe.reqWSUpData0 := boreRead(privateBool(vPipeREQ, "w_s_rd_up_compdata0"))
    probe.reqWSUpData2 := boreRead(privateBool(vPipeREQ, "w_s_rd_up_compdata2"))
    probe.reqSUpData0 := boreRead(privateBool(vPipeREQ, "s_rd_up_compdata0"))
    probe.reqSUpData2 := boreRead(privateBool(vPipeREQ, "s_rd_up_compdata2"))
    probe.reqDnCompData := boreRead(privateBool(vPipeREQ, "dn_rxdat_compdata"))
    probe.reqDnCompDataFirst := boreRead(privateBool(vPipeREQ, "dn_rxdat_compdata_first"))
  }

}
}
