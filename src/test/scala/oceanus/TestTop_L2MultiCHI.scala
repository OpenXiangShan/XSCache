package oceanus

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config._
import scala.collection.mutable.ArrayBuffer
import utility._
import oceanus.l2.{L2Configuration, L2Params, L2ParamsKey, L2Top}
import oceanus.chi.{CHIParameters, CHIParametersKey, EnumCHIIssue}
import oceanus.chi.bundle.{CHIBundleDAT, CHIBundleREQ, CHIBundleRSP, CHIBundleSNP}
import oceanus.chi.channel.AbstractCHIChannel
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}

/*  Oceanus L2 x N, CCHI upstream exported (cohestra_v3 contract) and the
 *  downstream CHI RN-F link of every L2 exported DIRECTLY as top-level pins
 *  for connection to other SoCs / NoCs (no OpenLLC/OpenNCB in the path):
 *
 *       CCHI upstream ports (one Type-1 + two Type-4 per L2)
 *                     |
 *                L2Top x numL2
 *                     |  CHIRNFInterface (oceanus.chi bundles)
 *                     v
 *              exported CHI RN-F link pins, per L2 i
 *
 *  Per-instance identities:
 *    --chi-nids       CHI downstream node ID (SrcID on the RN-F link) per L2
 *    --cchi-t1-nids   CCHI Type-1 port NID per L2 (upstream coherent client;
 *                     also the snoop target and client-table NID)
 *    --cchi-t4p0-nids / --cchi-t4p1-nids
 *                     CCHI Type-4 (read-only non-coherent) port NIDs per L2
 *
 *  CHI pin styles (functional ports): exactly ONE style is exported per build,
 *  selected by --chi-style (default packed):
 *    packed:   {F}{i}_{chan}_bits            one wide packed flit pin
 *    separate: {F}{i}_{chan}_bits_{Field}    one pin per flit field,
 *              canonical CHI field names (alias-split, see below)
 *  Both styles also carry {F}{i}_{chan}_{flitpend,flitv,lcrdv} and the
 *  link pins {F}{i}_{tx,rx}linkactive{req,ack}, {F}{i}_{t,r}xsactive,
 *  {F}{i}_sysco{req,ack}.
 *  DUT TX channels (txreq/txrsp/txdat): flitpend/flitv/flit-pins are outputs,
 *  lcrdv is an input. DUT RX channels (rxsnp/rxrsp/rxdat): reversed.
 *  In separate style, every canonical alias gets its own pin in BOTH
 *  directions (the cohestra_v3 traits reference every alias name
 *  unconditionally): TX pins are outputs (aliases share physical bits —
 *  read-only, no conflict); RX pins are inputs where only the PRIMARY alias
 *  of each physical union field (ReturnNID, StashNIDValid, ReturnTxnID, LPID,
 *  Excl, SnpAttr, FwdState, DBID, FwdTxnID, DoNotGoToSD) is consumed by the
 *  DUT — a driver writes all aliases with union-consistent slice values, so
 *  the redundant alias pins are intentionally left unread.
 *
 *  CHI monitor ports (--chi-mon off|separate|packed|both, default both):
 *  passive output-only taps of every L2's typed CHI channels (identical in
 *  both functional styles; they mirror exactly what the DUT drives/receives):
 *    {M}{i}_{chan}_{flitpend,flitv,lcrdv}      handshake taps
 *    {M}{i}_{chan}_flit                        packed tap
 *    {M}{i}_{chan}_flit_{Field}                field taps (all aliases)
 *
 *  Packed flit bit order is spec LSB-first (QoS at [3:0] of the REQ flit...),
 *  the same layout the raw-channel/CLog path is validated against in CHIron.
 *
 *  Pin prefixes {F}/{M} follow the cohestra_v3 contract
 *  (cchi/cohestra/cohestra_v3/interface_chi.hpp) and are selected by
 *  --chi-names (default cohestra):
 *    cohestra: chi_rn{i}_* / mon_chi_rn{i}_* — the exact auto-detection
 *              names; the harness binds its CHI RN / monitor-RN interfaces
 *              with no remapping.
 *    compat:   l2chi_rn{i}_* / l2chi_mon_rn{i}_* — collision-free, for the
 *              CURRENT unmodified cohestra_v3 harness (use for smokes).
 *  The current /mnt/c CHIron cohestra_v3 CHI pin path has three independent
 *  bugs that prevent it from compiling against ANY Verilator DUT (its
 *  top_dummy.sv has no CHI pins, so the path was never compiled):
 *    1. Macro bug: V3_CHI_{REQ,RSP,DAT,SNP}_FIELD_PTR(name) reference PORT,
 *       which is only a parameter of the outer V3_CHI_DEFINE_*_PIN_TRAITS
 *       macro — the shared field-pointer macros never receive it, so the
 *       traits reference phantom `chi_rnPORT_*` names (literal "PORT").
 *       Fix: thread PORT through as an argument of the field macros.
 *    2. Pointer-to-member: the traits take &VT::pin, but Verilator 5.050
 *       emits all top-level ports as reference members. The same pins exist
 *       as VALUE members on the model's ___024root class — binding the
 *       traits against *top.rootp (or switching to accessor functions) is
 *       the minimal fix. (Verified by compile probes in
 *       tmp/smoke_multichi_20260920/.)
 *    3. Packed-flit decode additionally needs the harness's CHI flit types
 *       built as CHI::Eb::FlitConfiguration<11,48,4,4,256,true,true,true>
 *       to match this DUT's pinned CHIParameters.
 *
 *  CCHI upstream pin contract (unchanged, cohestra_v3-compatible):
 *    cchi_t1p{i}_{rxevt,rxreq,txsnp,txrsp,rxrsp,txdat,rxdat}_{valid,ready,bits_*}
 *    cchi_t4p{2i}/cchi_t4p{2i+1}_{rxreq,txdat}_*
 *
 *  Known boundaries:
 *    - DnTXREQ.TgtID (home node ID) stays hardwired to 0 in L2VPipeREQ
 *      ("E-SAM only currently"); only the L2's own node ID is configurable.
 *    - Slice NIDs stay 0 until numSlices (internal to each L2).
 *    - CCHI NIDs must fit CCHIParameters.UpstreamNodeID_Width (default 4).
 */

sealed trait CHIFlitPinStyle
object CHIFlitPinStyle {
  case object Packed extends CHIFlitPinStyle
  case object Separate extends CHIFlitPinStyle
}

sealed trait CHIMonitorMode
object CHIMonitorMode {
  case object Off extends CHIMonitorMode
  case object Separate extends CHIMonitorMode
  case object Packed extends CHIMonitorMode
  case object Both extends CHIMonitorMode

  def separateOn(mode: CHIMonitorMode): Boolean = mode == Separate || mode == Both
  def packedOn(mode: CHIMonitorMode): Boolean = mode == Packed || mode == Both
}

/* Pin naming policy for the exported CHI ports:
 *   Cohestra — exact cohestra_v3 contract prefixes (chi_rn / mon_chi_rn);
 *              directly attachable once the harness binds by name.
 *   Compat   — collision-free prefixes (l2chi_rn / l2chi_mon_rn) for use with
 *              the CURRENT unmodified cohestra_v3 harness, whose CHI traits
 *              take pointer-to-member of every pin and therefore fail to
 *              compile against Verilator 5.050 models (all top-level ports
 *              are reference members there; binding works against the
 *              model's rootp class, which holds the same pins as values). */
sealed trait CHIPinNaming
object CHIPinNaming {
  case object Cohestra extends CHIPinNaming
  case object Compat extends CHIPinNaming

  def functional(naming: CHIPinNaming): String = naming match {
    case Cohestra => "chi_rn"
    case Compat   => "l2chi_rn"
  }
  def monitor(naming: CHIPinNaming): String = naming match {
    case Cohestra => "mon_chi_rn"
    case Compat   => "l2chi_mon_rn"
  }
}

object TestTop_L2MultiCHI_Fields {

  /* Canonical CHI field views of a flit, in cohestra_v3's V3_CHI_*_FIELDS
   * naming. Accessors return Option[UInt] bit-select views (no hardware is
   * created); None = field absent under this issue/config. */
  def req(f: CHIBundleREQ): Seq[(String, Option[UInt])] = Seq(
    "QoS" -> f.QoS, "TgtID" -> f.TgtID, "SrcID" -> f.SrcID, "TxnID" -> f.TxnID,
    "ReturnNID" -> f.ReturnNID, "StashNID" -> f.StashNID, "SLCRepHint" -> f.SLCRepHint,
    "StashNIDValid" -> f.StashNIDValid, "Endian" -> f.Endian, "Deep" -> f.Deep,
    "ReturnTxnID" -> f.ReturnTxnID, "StashLPIDValid" -> f.StashLPIDValid, "StashLPID" -> f.StashLPID,
    "Opcode" -> f.Opcode, "Size" -> f.Size, "Addr" -> f.Addr, "NS" -> f.NS,
    "LikelyShared" -> f.LikelyShared, "AllowRetry" -> f.AllowRetry, "Order" -> f.Order,
    "PCrdType" -> f.PCrdType, "MemAttr" -> f.MemAttr, "SnpAttr" -> f.SnpAttr, "DoDWT" -> f.DoDWT,
    "LPID" -> f.LPID, "PGroupID" -> f.PGroupID, "StashGroupID" -> f.StashGroupID,
    "TagGroupID" -> f.TagGroupID, "Excl" -> f.Excl, "SnoopMe" -> f.SnoopMe,
    "ExpCompAck" -> f.ExpCompAck, "TraceTag" -> f.TraceTag, "TagOp" -> f.TagOp,
    "MPAM" -> f.MPAM, "RSVDC" -> f.RSVDC
  )

  def rsp(f: CHIBundleRSP): Seq[(String, Option[UInt])] = Seq(
    "QoS" -> f.QoS, "TgtID" -> f.TgtID, "SrcID" -> f.SrcID, "TxnID" -> f.TxnID,
    "Opcode" -> f.Opcode, "RespErr" -> f.RespErr, "Resp" -> f.Resp,
    "FwdState" -> f.FwdState, "DataPull" -> f.DataPull, "CBusy" -> f.CBusy,
    "DBID" -> f.DBID, "PGroupID" -> f.PGroupID, "StashGroupID" -> f.StashGroupID,
    "TagGroupID" -> f.TagGroupID, "PCrdType" -> f.PCrdType, "TagOp" -> f.TagOp,
    "TraceTag" -> f.TraceTag
  )

  def dat(f: CHIBundleDAT): Seq[(String, Option[UInt])] = Seq(
    "QoS" -> f.QoS, "TgtID" -> f.TgtID, "SrcID" -> f.SrcID, "TxnID" -> f.TxnID,
    "HomeNID" -> f.HomeNID, "Opcode" -> f.Opcode, "RespErr" -> f.RespErr, "Resp" -> f.Resp,
    "FwdState" -> f.FwdState, "DataPull" -> f.DataPull, "DataSource" -> f.DataSource,
    "CBusy" -> f.CBusy, "DBID" -> f.DBID, "CCID" -> f.CCID, "DataID" -> f.DataID,
    "TagOp" -> f.TagOp, "Tag" -> f.Tag, "TU" -> f.TU, "TraceTag" -> f.TraceTag,
    "RSVDC" -> f.RSVDC, "BE" -> f.BE, "Data" -> f.Data, "DataCheck" -> f.DataCheck,
    "Poison" -> f.Poison
  )

  def snp(f: CHIBundleSNP): Seq[(String, Option[UInt])] = Seq(
    "QoS" -> f.QoS, "SrcID" -> f.SrcID, "TxnID" -> f.TxnID, "FwdNID" -> f.FwdNID,
    "FwdTxnID" -> f.FwdTxnID, "StashLPIDValid" -> f.StashLPIDValid, "StashLPID" -> f.StashLPID,
    "VMIDExt" -> f.VMIDExt, "Opcode" -> f.Opcode, "Addr" -> f.Addr, "NS" -> f.NS,
    "DoNotGoToSD" -> f.DoNotGoToSD, "DoNotDataPull" -> f.DoNotDataPull,
    "RetToSrc" -> f.RetToSrc, "TraceTag" -> f.TraceTag, "MPAM" -> f.MPAM
  )

  /* Physical (union-carrier) field name -> primary canonical alias, for
   * separate-style RX input pins (one pin per physical field). */
  private val primaryAlias = Map(
    "ReturnNID_StashNID_SLCRepHint"               -> "ReturnNID",
    "StashNIDValid_Endian_Deep"                   -> "StashNIDValid",
    "ReturnTxnID_StashLPIDValid_StashLPID"        -> "ReturnTxnID",
    "LPID_PGroupID_StashGroupID_TagGroupID"       -> "LPID",
    "Excl_SnoopMe"                                -> "Excl",
    "SnpAttr_DoDWT"                               -> "SnpAttr",
    "FwdState_DataPull"                           -> "FwdState",
    "FwdState_DataPull_DataSource"                -> "FwdState",
    "DBID_PGroupID_StashGroupID_TagGroupID"       -> "DBID",
    "FwdTxnID_StashLPIDValid_StashLPID_VMIDExt"   -> "FwdTxnID",
    "DoNotGoToSD_DoNotDataPull"                   -> "DoNotGoToSD"
  )

  def primaryOf(physicalName: String): String = primaryAlias.getOrElse(physicalName, physicalName)
}

class TestTop_L2MultiCHI(
  val numL2: Int,
  val numSlices: Int,
  val chiNIDs: Seq[Int],
  val t1NIDs: Seq[Int],
  val t4p0NIDs: Seq[Int],
  val t4p1NIDs: Seq[Int],
  val chiStyle: CHIFlitPinStyle,
  val chiMon: CHIMonitorMode,
  val chiNames: CHIPinNaming
)(implicit p: Parameters) extends Module {

  import TestTop_L2MultiCHI_Fields._

  val chiPrefix = CHIPinNaming.functional(chiNames)
  val chiMonPrefix = CHIPinNaming.monitor(chiNames)

  val l2s = Seq.tabulate(numL2)(i => Module(new L2Top(new L2Configuration(
    nodeId = chiNIDs(i),
    eSAM = true,
    slices = 0 until numSlices,
    t1p0NID = t1NIDs(i),
    t4p0NID = t4p0NIDs(i),
    t4p1NID = t4p1NIDs(i)))))

  // -- Cohestra V3 pin-level export ------------------------------------------
  // Every external pin is an individual IO named via suggestName, so emitted
  // port names follow the contract (see header) regardless of how the
  // internal bundles/channels are named.

  /* Export a Decoupled channel as {prefix}_valid / {prefix}_ready /
   * {prefix}_bits_<field> pins. dutDrives = true for channels the DUT
   * sources (valid/bits are outputs, ready is an input). */
  def exportChannel[T <: Bundle](prefix: String, chan: ReadyValidIO[T], dutDrives: Boolean): Unit = {
    val vld = IO(if (dutDrives) Output(Bool()) else Input(Bool())).suggestName(s"${prefix}_valid")
    val rdy = IO(if (dutDrives) Input(Bool()) else Output(Bool())).suggestName(s"${prefix}_ready")
    if (dutDrives) { vld := chan.valid; chan.ready := rdy }
    else           { chan.valid := vld; rdy := chan.ready }
    chan.bits.elements.foreach { case (name, field) =>
      val pin = IO(if (dutDrives) Output(chiselTypeOf(field)) else Input(chiselTypeOf(field)))
        .suggestName(s"${prefix}_bits_${name}")
      if (dutDrives) pin := field else field := pin
    }
  }

  /* Packed flit in spec LSB-first order (same layout as the raw-channel /
   * CLog path: first-declared field at LSB). */
  def packFlit(flit: Bundle): UInt = Cat(flit.getElements.map(_.asUInt))

  /* CHI channel handshake pins ({prefix}_{flitpend,flitv,lcrdv}); direction
   * follows dutDrives (true = DUT sources the channel). */
  def exportCHIHandshake[T <: Bundle](prefix: String, chan: AbstractCHIChannel[T], dutDrives: Boolean): Unit = {
    val pend = IO(if (dutDrives) Output(Bool()) else Input(Bool())).suggestName(s"${prefix}_flitpend")
    val vld  = IO(if (dutDrives) Output(Bool()) else Input(Bool())).suggestName(s"${prefix}_flitv")
    val crd  = IO(if (dutDrives) Input(Bool()) else Output(Bool())).suggestName(s"${prefix}_lcrdv")
    if (dutDrives) { pend := chan.flitpend; vld := chan.flitv; chan.lcrdv := crd }
    else           { chan.flitpend := pend; chan.flitv := vld; crd := chan.lcrdv }
  }

  /* Functional RN-F CHI channel export: handshake pins plus exactly ONE flit
   * style copy selected by chiStyle.
   *   packed:   {prefix}_bits wide pin (RX: drives the DUT)
   *   separate: {prefix}_bits_{Field} — TX: every canonical alias (outputs);
   *             RX: one input per physical field under its primary alias. */
  def exportCHIFunctional[T <: Bundle](prefix: String, chan: AbstractCHIChannel[T],
                                       dutDrives: Boolean,
                                       aliases: Seq[(String, Option[UInt])]): Unit = {
    exportCHIHandshake(prefix, chan, dutDrives)
    if (chiStyle == CHIFlitPinStyle.Packed) {
      if (dutDrives) {
        val bits = IO(Output(UInt(chan.flit.getWidth.W))).suggestName(s"${prefix}_bits")
        bits := packFlit(chan.flit)
      } else {
        val bits = IO(Input(UInt(chan.flit.getWidth.W))).suggestName(s"${prefix}_bits")
        var lsb = 0
        chan.flit.getElements.reverse.foreach { element =>
          val width = element.getWidth
          if (width > 0) {
            element := bits(lsb + width - 1, lsb)
            lsb += width
          }
        }
        require(lsb == chan.flit.getWidth,
          s"packed RX flit width mismatch on $prefix: consumed $lsb of ${chan.flit.getWidth}")
      }
    } else {
      if (dutDrives) {
        aliases.foreach { case (name, field) => field.foreach { view =>
          val pin = IO(Output(chiselTypeOf(view))).suggestName(s"${prefix}_bits_${name}")
          pin := view
        }}
      } else {
        // RX separate: one input pin per canonical alias (the cohestra_v3
        // traits reference every alias name unconditionally). Only the
        // primary alias of each physical union field is consumed by the DUT;
        // a driver writes all aliases with union-consistent slice values, so
        // the redundant alias pins are intentionally left unread.
        val pins = aliases.flatMap { case (name, field) => field.map { view =>
          name -> IO(Input(chiselTypeOf(view))).suggestName(s"${prefix}_bits_${name}")
        }}.toMap
        chan.flit.elements.foreach { case (physicalName, field) =>
          pins.get(primaryOf(physicalName)).foreach(pin => field := pin)
        }
      }
    }
  }

  /* Monitor channel export: passive output-only taps of the DUT's typed
   * channel, in the style(s) enabled by chiMon. */
  def monitorCHIChannel[T <: Bundle](prefix: String, chan: AbstractCHIChannel[T],
                                     aliases: Seq[(String, Option[UInt])]): Unit = {
    if (chiMon == CHIMonitorMode.Off) return
    Seq("flitpend" -> chan.flitpend, "flitv" -> chan.flitv, "lcrdv" -> chan.lcrdv).foreach {
      case (name, signal) =>
        val pin = IO(Output(Bool())).suggestName(s"${prefix}_${name}")
        pin := signal
    }
    if (CHIMonitorMode.separateOn(chiMon)) {
      aliases.foreach { case (name, field) => field.foreach { view =>
        val pin = IO(Output(chiselTypeOf(view))).suggestName(s"${prefix}_flit_${name}")
        pin := view
      }}
    }
    if (CHIMonitorMode.packedOn(chiMon)) {
      val pin = IO(Output(UInt(chan.flit.getWidth.W))).suggestName(s"${prefix}_flit")
      pin := packFlit(chan.flit)
    }
  }

  l2s.zipWithIndex.foreach { case (l2, i) =>
    // -- Exported upstream CCHI ports, one Type-1 + two Type-4 per L2
    exportChannel(s"cchi_t1p${i}_rxevt", l2.io.t1p0.UpEVT, dutDrives = false)
    exportChannel(s"cchi_t1p${i}_rxreq", l2.io.t1p0.UpREQ, dutDrives = false)
    exportChannel(s"cchi_t1p${i}_txsnp", l2.io.t1p0.DnSNP, dutDrives = true)
    exportChannel(s"cchi_t1p${i}_txrsp", l2.io.t1p0.DnRSP, dutDrives = true)
    exportChannel(s"cchi_t1p${i}_rxrsp", l2.io.t1p0.UpRSP, dutDrives = false)
    exportChannel(s"cchi_t1p${i}_txdat", l2.io.t1p0.DnDAT, dutDrives = true)
    exportChannel(s"cchi_t1p${i}_rxdat", l2.io.t1p0.UpDAT, dutDrives = false)

    exportChannel(s"cchi_t4p${2*i}_rxreq",   l2.io.t4p0.UpREQ, dutDrives = false)
    exportChannel(s"cchi_t4p${2*i}_txdat",   l2.io.t4p0.DnDAT, dutDrives = true)
    exportChannel(s"cchi_t4p${2*i+1}_rxreq", l2.io.t4p1.UpREQ, dutDrives = false)
    exportChannel(s"cchi_t4p${2*i+1}_txdat", l2.io.t4p1.DnDAT, dutDrives = true)

    // -- Exported downstream CHI RN-F link (functional, one style copy)
    val aliasesTxReq = req(l2.io.chi.txreq.flit)
    val aliasesTxRsp = rsp(l2.io.chi.txrsp.flit)
    val aliasesTxDat = dat(l2.io.chi.txdat.flit)
    val aliasesRxSnp = snp(l2.io.chi.rxsnp.flit)
    val aliasesRxRsp = rsp(l2.io.chi.rxrsp.flit)
    val aliasesRxDat = dat(l2.io.chi.rxdat.flit)

    exportCHIFunctional(s"${chiPrefix}${i}_txreq", l2.io.chi.txreq, dutDrives = true, aliasesTxReq)
    exportCHIFunctional(s"${chiPrefix}${i}_txrsp", l2.io.chi.txrsp, dutDrives = true, aliasesTxRsp)
    exportCHIFunctional(s"${chiPrefix}${i}_txdat", l2.io.chi.txdat, dutDrives = true, aliasesTxDat)
    exportCHIFunctional(s"${chiPrefix}${i}_rxsnp", l2.io.chi.rxsnp, dutDrives = false, aliasesRxSnp)
    exportCHIFunctional(s"${chiPrefix}${i}_rxrsp", l2.io.chi.rxrsp, dutDrives = false, aliasesRxRsp)
    exportCHIFunctional(s"${chiPrefix}${i}_rxdat", l2.io.chi.rxdat, dutDrives = false, aliasesRxDat)

    // -- CHI monitor taps
    monitorCHIChannel(s"${chiMonPrefix}${i}_txreq", l2.io.chi.txreq, aliasesTxReq)
    monitorCHIChannel(s"${chiMonPrefix}${i}_txrsp", l2.io.chi.txrsp, aliasesTxRsp)
    monitorCHIChannel(s"${chiMonPrefix}${i}_txdat", l2.io.chi.txdat, aliasesTxDat)
    monitorCHIChannel(s"${chiMonPrefix}${i}_rxsnp", l2.io.chi.rxsnp, aliasesRxSnp)
    monitorCHIChannel(s"${chiMonPrefix}${i}_rxrsp", l2.io.chi.rxrsp, aliasesRxRsp)
    monitorCHIChannel(s"${chiMonPrefix}${i}_rxdat", l2.io.chi.rxdat, aliasesRxDat)

    // -- CHI link-active / sactive / sysco
    val txlinkactivereq = IO(Output(Bool())).suggestName(s"${chiPrefix}${i}_txlinkactivereq")
    txlinkactivereq := l2.io.chi.txlinkactivereq
    val txlinkactiveack = IO(Input(Bool())).suggestName(s"${chiPrefix}${i}_txlinkactiveack")
    l2.io.chi.txlinkactiveack := txlinkactiveack
    val rxlinkactivereq = IO(Input(Bool())).suggestName(s"${chiPrefix}${i}_rxlinkactivereq")
    l2.io.chi.rxlinkactivereq := rxlinkactivereq
    val rxlinkactiveack = IO(Output(Bool())).suggestName(s"${chiPrefix}${i}_rxlinkactiveack")
    rxlinkactiveack := l2.io.chi.rxlinkactiveack

    val txsactive = IO(Output(Bool())).suggestName(s"${chiPrefix}${i}_txsactive")
    txsactive := l2.io.chi.txsactive
    val rxsactive = IO(Input(Bool())).suggestName(s"${chiPrefix}${i}_rxsactive")
    l2.io.chi.rxsactive := rxsactive

    val syscoreq = IO(Output(Bool())).suggestName(s"${chiPrefix}${i}_syscoreq")
    syscoreq := l2.io.chi.syscoreq
    val syscoack = IO(Input(Bool())).suggestName(s"${chiPrefix}${i}_syscoack")
    l2.io.chi.syscoack := syscoack
  }

  // -- Logging
  val log = IO(new Bundle {
    val dump = Input(Bool())
    val clean = Input(Bool())
  })

  val cycle = RegInit(0.U(64.W))
  cycle := cycle + 1.U

  val timer = WireDefault(0.U(64.W))
  val logEnable = WireDefault(false.B)
  val clean = WireDefault(false.B)
  val dump = WireDefault(false.B)

  timer := cycle
  logEnable := true.B
  clean := log.clean
  dump := log.dump

  dontTouch(timer)
  dontTouch(logEnable)
  dontTouch(clean)
  dontTouch(dump)

  XSLog.collect(timer, logEnable, clean, dump)
}

object TestTop_L2MultiCHI extends App {

  val usage = """
Usage: TestTop_L2MultiCHI [<--option> <values>]

      --l2 <l2_num>             specify the number of Oceanus L2 instances, 1 by default
      --slices <slice_num>      specify the number of slices per L2, 2 by default;
                                external SAM supports 1 to 4 slices
      --noperf                  disable all DUT performance counters (drops the
                                LogPerfEndpoint bulk; much faster firtool/verilation)
      --chi-nids <n,n,...>      CHI downstream node ID (RN-F SrcID) of each L2;
                                one value per L2, defaults to 0,1,2,...
      --cchi-t1-nids <n,n,...>  CCHI Type-1 port NID of each L2 (upstream coherent
                                client NID), one value per L2, defaults to all 0
      --cchi-t4p0-nids <n,n,...>
                                CCHI Type-4 port 0 NID of each L2, defaults to all 4
      --cchi-t4p1-nids <n,n,...>
                                CCHI Type-4 port 1 NID of each L2, defaults to all 5
      --chi-style <packed|separate>
                                flit pin style of the functional RN-F CHI ports;
                                exactly one style copy is exported, packed by default
      --chi-mon <off|separate|packed|both>
                                CHI monitor port taps, both styles by default
      --chi-names <cohestra|compat>
                                CHI pin prefixes: 'cohestra' (default) emits the exact
                                cohestra_v3 contract names (chi_rn / mon_chi_rn) for
                                direct attachment; 'compat' emits collision-free
                                prefixes (l2chi_rn / l2chi_mon_rn) for use with the
                                current unmodified cohestra_v3 harness build
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  var numL2 = 1
  var numSlices = 2
  var noPerf = false
  var chiNIDsArg: Option[Seq[Int]] = None
  var t1NIDsArg: Option[Seq[Int]] = None
  var t4p0NIDsArg: Option[Seq[Int]] = None
  var t4p1NIDsArg: Option[Seq[Int]] = None
  var chiStyleArg: Option[String] = None
  var chiMonArg: Option[String] = None
  var chiNamesArg: Option[String] = None

  def parseNIDList(value: String): Seq[Int] = value.split(",").map(_.trim.toInt).toIndexedSeq

  val varArgs = ArrayBuffer[String]()
  var i = 0
  while (i < args.length) {
    args(i) match {
      case "--l2"             => numL2 = args(i + 1).toInt; i += 2
      case "--slices"         => numSlices = args(i + 1).toInt; i += 2
      case "--noperf"         => noPerf = true; i += 1
      case "--chi-nids"       => chiNIDsArg = Some(parseNIDList(args(i + 1))); i += 2
      case "--cchi-t1-nids"   => t1NIDsArg = Some(parseNIDList(args(i + 1))); i += 2
      case "--cchi-t4p0-nids" => t4p0NIDsArg = Some(parseNIDList(args(i + 1))); i += 2
      case "--cchi-t4p1-nids" => t4p1NIDsArg = Some(parseNIDList(args(i + 1))); i += 2
      case "--chi-style"      => chiStyleArg = Some(args(i + 1)); i += 2
      case "--chi-mon"        => chiMonArg = Some(args(i + 1)); i += 2
      case "--chi-names"      => chiNamesArg = Some(args(i + 1)); i += 2
      case other              => varArgs += other; i += 1
    }
  }
  varArgs.trimToSize()

  require(numL2 >= 1, s"Unsupported L2 count $numL2")
  require(numSlices >= 1 && numSlices <= 4, s"Unsupported slice count $numSlices under eSAM")

  val chiStyle = chiStyleArg match {
    case None | Some("packed")   => CHIFlitPinStyle.Packed
    case Some("separate")        => CHIFlitPinStyle.Separate
    case Some(other)             =>
      throw new IllegalArgumentException(s"Unknown --chi-style '$other' (expected packed|separate)")
  }

  val chiMon = chiMonArg match {
    case None | Some("both")     => CHIMonitorMode.Both
    case Some("off")             => CHIMonitorMode.Off
    case Some("separate")        => CHIMonitorMode.Separate
    case Some("packed")          => CHIMonitorMode.Packed
    case Some(other)             =>
      throw new IllegalArgumentException(s"Unknown --chi-mon '$other' (expected off|separate|packed|both)")
  }

  val chiNames = chiNamesArg match {
    case None | Some("cohestra") => CHIPinNaming.Cohestra
    case Some("compat")          => CHIPinNaming.Compat
    case Some(other)             =>
      throw new IllegalArgumentException(s"Unknown --chi-names '$other' (expected cohestra|compat)")
  }

  val chiNIDs = chiNIDsArg.getOrElse(0 until numL2)
  val t1NIDs = t1NIDsArg.getOrElse(Seq.fill(numL2)(0))
  val t4p0NIDs = t4p0NIDsArg.getOrElse(Seq.fill(numL2)(4))
  val t4p1NIDs = t4p1NIDsArg.getOrElse(Seq.fill(numL2)(5))

  Seq(("chi-nids", chiNIDs), ("cchi-t1-nids", t1NIDs),
      ("cchi-t4p0-nids", t4p0NIDs), ("cchi-t4p1-nids", t4p1NIDs)).foreach { case (name, list) =>
    require(list.length == numL2, s"--$name lists ${list.length} NIDs but --l2 is $numL2")
  }

  // Keep in sync with the CHIParametersKey / CCHIParametersKey configs below.
  val chiNodeIdWidth = 11
  require(chiNIDs.forall(n => n >= 0 && n < (1 << chiNodeIdWidth)),
    s"CHI node IDs must fit nodeIdWidth=$chiNodeIdWidth: $chiNIDs")

  val cchiUpstreamNIDWidth = CCHIParameters().UpstreamNodeID_Width
  require((t1NIDs ++ t4p0NIDs ++ t4p1NIDs).forall(n => n >= 0 && n < (1 << cchiUpstreamNIDWidth)),
    s"CCHI upstream NIDs must fit UpstreamNodeID_Width=$cchiUpstreamNIDWidth: " +
    s"$t1NIDs / $t4p0NIDs / $t4p1NIDs")

  val config = new Config((_, _, _) => {
    case L2ParamsKey => L2Params (
      physicalAddrWidth = 48,
      mshrSize = 16,
      ways = 4,
      sets = 32
    )
    case CHIParametersKey => CHIParameters (
      issue = EnumCHIIssue.E,
      nodeIdWidth = chiNodeIdWidth,
      reqAddrWidth = 48,
      reqRsvdcWidth = 4,
      datRsvdcWidth = 4,
      dataWidth = 256,
      dataCheckPresent = true,
      poisonPresent = true,
      mpamPresent = true
    )
    case CCHIParametersKey => CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = !noPerf,
      fpgaPlatform = false
    )
    case PerfCounterOptionsKey => PerfCounterOptions (
      enablePerfPrint = !noPerf,
      enablePerfDB = false,
      perfLevel = if (noPerf) XSPerfLevel.NORMAL else XSPerfLevel.VERBOSE,
      0
    )
  })

  (new ChiselStage).execute(varArgs.toArray,
    ChiselGeneratorAnnotation(() =>
      new TestTop_L2MultiCHI(numL2, numSlices, chiNIDs, t1NIDs, t4p0NIDs, t4p1NIDs,
        chiStyle, chiMon, chiNames)(config))
      +: TestTopFirtoolOptions())
}
