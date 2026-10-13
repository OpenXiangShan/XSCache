package oceanus

import chisel3._
import _root_.circt.stage.ChiselStage
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.simulator.{ChiselSim, HasSimulator}
import utility._
import oceanus.l2._
import oceanus.chi._
import oceanus.chi.field.CHIFieldResp
import oceanus.compactchi.{CCHIOpcode, CCHIResp}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import scala.util.Try
import java.io.File
import svsim.verilator.Backend.CompilationSettings.TraceKind
import svsim.verilator.Backend.CompilationSettings.TraceStyle

sealed trait MasterId { def name: String }
object MasterId {
  case object T1P0 extends MasterId { val name = "t1p0" }
  case object T4P0 extends MasterId { val name = "t4p0" }
  case object T4P1 extends MasterId { val name = "t4p1" }
  val t1p0: MasterId = T1P0
  val t4p0: MasterId = T4P0
  val t4p1: MasterId = T4P1
}

case class UpRequestSpec(
  master: MasterId,
  txnId: BigInt,
  srcId: BigInt,
  tgtId: BigInt,
  opcode: BigInt,
  addr: BigInt,
  size: BigInt = 6,
  ns: Boolean = false,
  order: BigInt = 0,
  memAttr: BigInt = 0xf,
  excl: Boolean = false,
  expCompData: Boolean = true
)

sealed trait StreamSpec {
  def requestCount: Int
  def request(index: Int): UpRequestSpec
  def downstreamDirtyResp(index: Int): Boolean
  def downstreamDataResp(index: Int, opcode: BigInt): BigInt =
    BigInt(if (downstreamDirtyResp(index)) CHIFieldResp.CompData_UD_PD else CHIFieldResp.CompData_SC)
}

case class ExplicitRequestStream(
  requests: Seq[UpRequestSpec],
  dataRespByIndex: Seq[BigInt]
) extends StreamSpec {
  def requestCount: Int = requests.size
  def request(index: Int): UpRequestSpec = requests(index)
  def downstreamDirtyResp(index: Int): Boolean = downstreamDataResp(index, request(index).opcode) == CHIFieldResp.CompData_UD_PD
  override def downstreamDataResp(index: Int, opcode: BigInt): BigInt = dataRespByIndex.lift(index).getOrElse(BigInt(CHIFieldResp.CompData_SC))
}

case class FixedReadStream(
  master: MasterId,
  baseAddr: BigInt,
  strideBits: Int,
  requestCount: Int,
  opcode: BigInt,
  dirtyDataResp: Boolean
) extends StreamSpec {
  def request(index: Int): UpRequestSpec =
    UpRequestSpec(master, BigInt(index & 0x7f), 0, 0, opcode, baseAddr + (BigInt(index) << strideBits))

  def downstreamDirtyResp(index: Int): Boolean = dirtyDataResp
}

sealed trait SnoopPolicy { def name: String }
object SnoopPolicy {
  case object SnpRespI extends SnoopPolicy { val name = "snpresp-i" }
  case object SnpRespDataI_PD extends SnoopPolicy { val name = "snprespdata-i-pd" }
}

case class UpConfig(
  masters: Seq[MasterId],
  stream: StreamSpec,
  snoopPolicy: SnoopPolicy
)

sealed trait L2TopLatencyOp
object L2TopLatencyOp {
  case object CompData extends L2TopLatencyOp
  case object CompDBIDResp extends L2TopLatencyOp
}

sealed trait LatencySpec {
  def delay(cycle: Int, op: L2TopLatencyOp, beat: Int): Int
}

case class FixedLatency(cycles: Int) extends LatencySpec {
  def delay(cycle: Int, op: L2TopLatencyOp, beat: Int): Int = cycles
}

sealed trait OutstandingPolicy { def limit(cycle: Int): Int }
case class FixedOutstanding(value: Int) extends OutstandingPolicy {
  def limit(cycle: Int): Int = value
}

sealed trait CreditPolicy {
  def maxTxLCredits: Int
  def maxRxRspLCredits: Int
  def maxRxDatLCredits: Int
  def maxRxSnpLCredits: Int
  def txCreditReturnDelay: Int
}

case class ImmediateCreditPolicy(
  maxTxLCredits: Int = 15,
  maxRxRspLCredits: Int = 15,
  maxRxDatLCredits: Int = 15,
  maxRxSnpLCredits: Int = 4,
  txCreditReturnDelay: Int = 0
) extends CreditPolicy {
  require(txCreditReturnDelay >= 0)
}

case class DnConfig(
  latency: LatencySpec,
  maxOutstanding: OutstandingPolicy,
  creditPolicy: CreditPolicy,
  holdTxReqCreditUntilResponse: Boolean = false,
  // Resp returned for a downstream ReadNotSharedDirty. ReadShared and
  // StashShared both issue it (L2VPipeREQ.scala:612,616), and the fill state
  // follows this resp, so the smoke test has to be able to choose it.
  // Defaults to SC, which fills the line as S.
  readNotSharedDirtyResp: BigInt = BigInt(CHIFieldResp.CompData_SC)
)

sealed trait Check { def name: String }
object Check {
  case object LoadMissGrantAndOccupancy extends Check { val name = "load-miss-grant-occupancy" }
  case object DirtyVictimKnownWatchdog extends Check { val name = "dirty-victim-watchdog" }
}

case class CaseSpec(
  name: String,
  up: UpConfig,
  dn: DnConfig,
  cycles: Int,
  window: (Int, Int),
  checks: Seq[Check]
)

trait L2TopCaseProvider {
  def cases: Seq[CaseSpec]
}

object L2TopBuiltInCaseProvider extends L2TopCaseProvider {
  private val commonDn = DnConfig(
    latency = FixedLatency(12),
    maxOutstanding = FixedOutstanding(64),
    creditPolicy = ImmediateCreditPolicy()
  )

  val loadMiss: CaseSpec = CaseSpec(
    name = "load-miss",
    up = UpConfig(
      masters = Seq(MasterId.t1p0),
      stream = FixedReadStream(MasterId.t1p0, BigInt("8000", 16), 13, 1, CCHIOpcode.ReadShared.opcode, dirtyDataResp = false),
      snoopPolicy = SnoopPolicy.SnpRespI
    ),
    dn = commonDn,
    cycles = 1200,
    window = (300, 1200),
    checks = Seq(Check.LoadMissGrantAndOccupancy)
  )

  val dirtyVictim: CaseSpec = CaseSpec(
    name = "dirty-victim",
    up = UpConfig(
      masters = Seq(MasterId.t1p0),
      stream = FixedReadStream(MasterId.t1p0, BigInt("8000", 16), 13, 5, CCHIOpcode.ReadUnique.opcode, dirtyDataResp = true),
      snoopPolicy = SnoopPolicy.SnpRespI
    ),
    dn = commonDn,
    cycles = 1200,
    window = (300, 1200),
    checks = Seq(Check.DirtyVictimKnownWatchdog)
  )

  val snoopDataPassDirty: CaseSpec = CaseSpec(
    name = "snoop-data-passdirty",
    up = UpConfig(
      masters = Seq(MasterId.t1p0),
      stream = ExplicitRequestStream(Seq(
        UpRequestSpec(MasterId.t1p0, 0, 0, 0, CCHIOpcode.ReadShared.opcode, BigInt("8000", 16)),
        UpRequestSpec(MasterId.t1p0, 1, 0, 0, CCHIOpcode.ReadUnique.opcode, BigInt("8000", 16))
      ), dataRespByIndex = Seq(BigInt(CHIFieldResp.CompData_SC), BigInt(CHIFieldResp.CompData_UC))),
      snoopPolicy = SnoopPolicy.SnpRespDataI_PD
    ),
    dn = commonDn,
    cycles = 1200,
    window = (300, 1200),
    checks = Seq.empty
  )

  val prefetchedClear: CaseSpec = CaseSpec(
    name = "prefetched-clear",
    up = UpConfig(
      masters = Seq(MasterId.t1p0),
      stream = ExplicitRequestStream(Seq(
        UpRequestSpec(MasterId.t1p0, 0, 0, 0, CCHIOpcode.ReadShared.opcode, BigInt("8000", 16)),
        UpRequestSpec(MasterId.t1p0, 1, 0, 0, CCHIOpcode.ReadShared.opcode, BigInt("8000", 16))
      ), dataRespByIndex = Seq.fill(2)(BigInt(CHIFieldResp.CompData_SC))),
      snoopPolicy = SnoopPolicy.SnpRespI
    ),
    dn = commonDn,
    cycles = 1200,
    window = (300, 1200),
    checks = Seq.empty
  )

  val prefetchedClearReadUnique: CaseSpec = CaseSpec(
    name = "prefetched-clear-readunique",
    up = UpConfig(
      masters = Seq(MasterId.t1p0),
      stream = ExplicitRequestStream(Seq(
        UpRequestSpec(MasterId.t1p0, 0, 0, 0, CCHIOpcode.ReadUnique.opcode, BigInt("8000", 16)),
        UpRequestSpec(MasterId.t1p0, 1, 0, 0, CCHIOpcode.ReadUnique.opcode, BigInt("8000", 16))
      ), dataRespByIndex = Seq.fill(2)(BigInt(CHIFieldResp.CompData_UC))),
      snoopPolicy = SnoopPolicy.SnpRespI
    ),
    dn = commonDn,
    cycles = 1200,
    window = (300, 1200),
    checks = Seq.empty
  )

  // Base 2d counts, address sets, timing and valid-victim replacements are
  // derived independently in L2TopBOPExpectations before simulation. Keep
  // frozen stimulus/parameters: Stage-A 160 + Stage-B 128 = 288 demands.
  val bopClosedLoop: CaseSpec = CaseSpec(
    name = "bop-closed-loop",
    up = UpConfig(
      masters = Seq(MasterId.t1p0),
      stream = FixedReadStream(MasterId.t1p0, BigInt("8000", 16), 6, 288, CCHIOpcode.ReadShared.opcode, dirtyDataResp = false),
      snoopPolicy = SnoopPolicy.SnpRespI
    ),
    dn = commonDn,
    cycles = 11000,
    window = (300, 11000),
    checks = Seq.empty
  )

  val bopClosedLoopUC: CaseSpec = bopClosedLoop.copy(
    name = "bop-closed-loop-uc",
    dn = bopClosedLoop.dn.copy(readNotSharedDirtyResp = BigInt(CHIFieldResp.CompData_UC))
  )

  val bopOverwriteQueue: CaseSpec = bopClosedLoop.copy(
    name = "bop-overwrite-queue",
    cycles = 6000
  )

  // Step 11 expectations, derived before simulation. Eight cold ReadShared
  // demands at D(k)=0x10000+k*0x100, k=0..7, complete one at a time.
  // Smoke-only badScore=0: startup idle evaluates 0<0=false, so disable stays
  // false; the production default badScore=1 instead disables at startup.
  // Eight trains cannot finish even one 37-offset round: the initial offset
  // stays +2 and (with this explicit test parameter) disable stays false.
  // All original eight-request assertions are retained. Each train produces
  // P(k)=D(k)+2*64=0x10080+k*0x100. All addresses share page 0x10, and
  // P never equals a demand. Sets are 4*k for D and 4*k+2 for P, so every
  // access uses an empty set: no replacement, EvictBack=0/0/0/0.
  // Each P must issue once, allocate one REQ TSHR (no reuse), register
  // SrcID=pfSrcID / Opcode=StashShared / ExpCompData=0 / Size=B64, and issue
  // one downstream RNSD (0x26). SC fills S(1), dirty=0; UC fills UU(3),
  // dirty=0 (L2VPipeREQ meta_wr_state_stashshared_*). No prefetch may
  // produce upstream RSP/DAT or train BOP. Thus demandDn=8, prefetchDn=8,
  // upTxDat=16, completed=8, TRAIN_FIRE=8, crossPageDrops=0; pending=active=0.
  // Route expectations use the production mapN formulas, computed per byte
  // address before running (test slice index equals NID):
  // k                 0 1 2 3 4 5 6 7
  // map2(D):          1 0 0 1 0 1 1 0  (parity of D>>6)
  // map2(P):          0 1 1 0 1 0 0 1
  // map4(D):          1 0 3 2 0 1 2 3  (odd-bit parity:even-bit parity)
  // map4(P):          3 2 1 0 2 3 0 1
  // Adding 0x80 flips bit 7 only, so P changes map2 by xor 1, map4 by xor 2.
  // map1(D)=map1(P)=0. The harness waits for quiescence before the next D.
  val bopPrefetchSmoke: CaseSpec = bopClosedLoop.copy(
    name = "bop-prefetch-smoke",
    up = bopClosedLoop.up.copy(stream = FixedReadStream(MasterId.t1p0, BigInt("10000", 16), 8, 8, CCHIOpcode.ReadShared.opcode, dirtyDataResp = false)),
    cycles = 2000
  )

  val bopPrefetchSmokeUC: CaseSpec = bopPrefetchSmoke.copy(
    name = "bop-prefetch-smoke-uc",
    dn = bopPrefetchSmoke.dn.copy(readNotSharedDirtyResp = BigInt(CHIFieldResp.CompData_UC))
  )

  val bopCollisionCases: Seq[CaseSpec] = Seq("c1", "c2", "c3").map { variant =>
    // A future multi-slice collision case must drive two independent upstream
    // ports and complete both responses. For N consecutive cycles with both
    // slice inputs valid, FastArbiter.scala:35-51 predicts winners
    // slice0,slice1,slice0,slice1,...: cycle 0 selects firstOneOH=slice0;
    // each fire records the other valid input in pendingMask, and the next
    // rrGrantMask starts after the previous winner. With BOP train ready, the
    // expected global BOP trainEvents is N, while per-slice train pulses are
    // N and N and received events are N. This response model currently drives
    // only t1p0 and requires single-slice collision cases, so it cannot yet
    // instantiate that two-port proof without adding t4 completion handling.
    val base = bopClosedLoop.up.stream.asInstanceOf[FixedReadStream]
    val stageA = (0 until 160).map(base.request)
    val trigger = base.request(160)
    val collision = base.request(170)
    val requests = if (variant == "c3") Seq(collision, trigger) else Seq(
      trigger, collision.copy(opcode = if (variant == "c2") CCHIOpcode.ReadUnique.opcode else CCHIOpcode.ReadShared.opcode)
    )
    val readback = base.request(171).copy(addr = collision.addr)
    bopClosedLoop.copy(
      name = s"bop-collision-$variant",
      up = bopClosedLoop.up.copy(stream = ExplicitRequestStream(stageA ++ requests :+ readback,
        Seq.fill(stageA.size + requests.size + 1)(BigInt(CHIFieldResp.CompData_SC)))),
      cycles = 8500
    )
  }

  // C2 SC requires a ReadUnique downstream upgrade from S. UC fills UU,
  // satisfying the same RU demand without an upgrade; all other C2 checks
  // stay identical. See the independent collision derivation.
  val bopCollisionC2UC: CaseSpec = bopCollisionCases(1).copy(
    name = "bop-collision-c2-uc",
    dn = bopCollisionCases(1).dn.copy(readNotSharedDirtyResp = BigInt(CHIFieldResp.CompData_UC))
  )

  def cases: Seq[CaseSpec] = Seq(bopClosedLoopUC, bopCollisionC2UC, loadMiss, dirtyVictim, snoopDataPassDirty, prefetchedClear, prefetchedClearReadUnique, bopClosedLoop, bopOverwriteQueue, bopPrefetchSmoke, bopPrefetchSmokeUC) ++ bopCollisionCases
}

trait L2TopResponseModel {
  def run(args: Array[String]): Unit
}

object L2TopCases {
  private var responseModel: Option[L2TopResponseModel] = None

  def registerResponseModel(model: L2TopResponseModel): Unit = {
    responseModel = Some(model)
  }

  def dispatch(args: Array[String]): Unit = {
    val managedArgs =
      args.contains("--help") ||
      args.contains("--simulate") ||
      args.contains("--vcd") ||
      args.exists(_.startsWith("--case=")) ||
      args.exists(_.startsWith("--mshr-size=")) ||
      args.contains("--policy-probe") ||
      args.exists(_.startsWith("--dn-latency=")) ||
      args.exists(_.startsWith("--dn-max-outstanding=")) ||
      args.exists(_.startsWith("--dn-credit-return-delay=")) ||
      args.exists(_.startsWith("--probe-requests=")) ||
      args.exists(_.startsWith("--probe-cycles="))
    if (managedArgs) {
      responseModel.getOrElse(L2TopBuiltInResponseModel).run(args)
      System.exit(0)
    }
  }

  private def discoveredProviders: Seq[L2TopCaseProvider] = {
    val loader = Thread.currentThread.getContextClassLoader
    val providerNames = sys.props.get("java.class.path").toSeq.flatMap(_.split(File.pathSeparator).toSeq).flatMap { entry =>
      val dir = new File(entry, "oceanus")
      if (dir.isDirectory) {
        Option(dir.listFiles()).toSeq.flatten.collect {
          case file if file.getName.endsWith("CaseProvider$.class") =>
            "oceanus." + file.getName.stripSuffix(".class")
        }
      } else {
        Seq.empty[String]
      }
    }.distinct

    providerNames.flatMap { name =>
      Try(Class.forName(name, true, loader).getField("MODULE$").get(null).asInstanceOf[L2TopCaseProvider]).toOption
    }.filterNot(_ == L2TopBuiltInCaseProvider)
  }

  private lazy val registry: Map[String, CaseSpec] =
    (L2TopBuiltInCaseProvider +: discoveredProviders).flatMap(_.cases).map(spec => spec.name -> spec).toMap

  def names: Seq[String] = registry.keys.toSeq.sorted

  def apply(name: String): CaseSpec = registry(name)
}
