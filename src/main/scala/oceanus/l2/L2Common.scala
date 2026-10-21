package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.compactchi.CCHIComponent
import org.chipsalliance.cde.config.Parameters
import oceanus.compactchi.CCHIOpcode
import oceanus.compactchi.CCHIChannelType
import utility.ParallelOR
import utility.ParallelMux
import oceanus.chi.EnumCHIChannel.SNP
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.FastArbiter

object L2Common {

  def arb[T <: Bundle](in: Seq[DecoupledIO[T]], out: DecoupledIO[T], name: Option[String] = None): Unit = {
    val arb = Module(new Arbiter[T](chiselTypeOf(out.bits), in.size))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) { a <> req }
    out <> arb.io.out
  }

  def fastArb[T <: Bundle](in: Seq[DecoupledIO[T]], out: DecoupledIO[T], name: Option[String] = None, rotateOnBlock: Boolean = false): Unit = {
    val arb = Module(new FastArbiter[T](chiselTypeOf(out.bits), in.size, rotateOnBlock))
    if (name.nonEmpty) { arb.suggestName(s"${name.get}_arb") }
    for ((a, req) <- arb.io.in.zip(in)) { a <> req }
    out <> arb.io.out
  }
}

object FSMPerfHistogram {
  def apply(name: String, prev: Bool, next: Bool)(implicit p: Parameters): Unit = {
    val counter = RegInit(0.U(32.W))
    when (prev) {
      when (!next) {
        counter := 0.U
      }.otherwise {
        counter := counter + 1.U
      }
    }
    XSPerfAccumulate(name + "_total", prev)
    XSPerfHistogram(name, counter, prev && !next, 0, 40, 2, right_strict = true)
    XSPerfHistogram(name, counter, prev && !next, 40, 800, 40, left_strict = true)
  }
}

object FSMTransPerfHistogram {
  def apply(name: String, prev: Bool, next: Bool)(implicit p: Parameters): Unit = {
    val counter = RegInit(0.U(32.W))
    when (prev) {
      when (next) {
        counter := 0.U
      }.otherwise {
        counter := counter + 1.U
      }
    }
    XSPerfAccumulate(name + "_cnt", prev && next)
    XSPerfHistogram(name, counter, prev && next, 0, 40, 2, right_strict = true)
    XSPerfHistogram(name, counter, prev && next, 40, 800, 40, left_strict = true)
  }
}
// Phase 1b: keep only VPN while a request waits; the page offset comes from
// its PA at execution. This payload belongs to the REQ RBE entry, not p_rxreq.
class L2PrefetchTrainContext extends Bundle {
  val vpn = UInt(38.W)
  val reqSource = UInt(5.W)
  val trainEnable = Bool()
}

// One Queue entry owns both fields, including allocation/reuse and replay.
// Disabling the interface removes the context from the generated storage.
class L2BufferedREQ(val trainEnabled: Boolean)(implicit p: Parameters) extends Bundle {
  val req = new oceanus.compactchi.FlitREQStripped
  val train = if (trainEnabled) Some(new L2PrefetchTrainContext) else None
}

// coupledL2 PrefetchTrain.vaddr is block VA (VA[49:6]); reqSource is the
// MemReqSource enum, separate from directory pfSource. trainEnable carries
// PrefetchKey/needHint for future consumers and never gates the existing BOP.
class L2PrefetchTrain(val trainEnabled: Boolean)(implicit p: Parameters) extends Bundle {
  // Intent (3a): preserve opcode-derived permission and CCHI request identity
  // through the global event for reference VBOP's permission/source dedup key.
  private val cchi = p(xscache.oceanus.compactchi.CCHIParametersKey)
  val needT = if (trainEnabled) Some(Bool()) else None
  val sourceId = if (trainEnabled) Some(UInt((cchi.TxnID_Width + cchi.UpstreamNodeID_Width).W)) else None
  val paddr = UInt(p(L2ParamsKey).physicalAddrWidth.W)
  val vaddr = if (trainEnabled) Some(UInt(44.W)) else None
  val reqSource = if (trainEnabled) Some(UInt(5.W)) else None
  val trainEnable = if (trainEnabled) Some(Bool()) else None
}

// Intent (3a): VBOP and the remaining CDP stub share only a transport payload,
// independent of either algorithm implementation.
class L2PrefetchAlgorithmRequest(implicit val p: Parameters) extends Bundle {
  val paddr = UInt(p(L2ParamsKey).physicalAddrWidth.W)
  val depth = UInt(2.W)
}
