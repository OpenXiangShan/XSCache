package oceanus

import chisel3._
import chisel3.util._
import oceanus.chi.bundle.{CHIBundleDAT, CHIBundleREQ, CHIBundleRSP, CHIBundleSNP}
import oceanus.chi.channel.AbstractCHIChannel

/* Shared pin-level CHI/CCHI export utilities for the Oceanus test tops
 * (TestTop_L2MultiCHI, TestTop_L2Avocado). Every external pin is an
 * individual IO named via suggestName, so emitted port names follow the
 * cohestra_v3 contract regardless of how the internal bundles/channels are
 * named.
 *
 *  Pin styles (functional ports):
 *    packed:   {prefix}_{packedPin}            one wide packed flit pin
 *              (packedPin = "bits" for the cohestra_v3 harness contract,
 *               packedPin = "flit" for the avocado DUT contract)
 *    separate: {prefix}_bits_{Field}           one pin per flit field,
 *              canonical CHI field names (alias-split)
 *  Both styles also carry {prefix}_{flitpend,flitv,lcrdv}.
 *  DUT TX channels (txreq/txrsp/txdat): flitpend/flitv/flit-pins are outputs,
 *  lcrdv is an input. DUT RX channels (rxsnp/rxrsp/rxdat): reversed.
 *
 *  Packed flit bit order is spec LSB-first (QoS at [3:0] of the REQ flit...),
 *  the same layout the raw-channel/CLog path is validated against in CHIron.
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

object TestTop_CHIExport {

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
   * style copy selected by style.
   *   packed:   {prefix}_{packedPin} wide pin (RX: drives the DUT); packedPin
   *             is "bits" for the cohestra_v3 contract and "flit" for the
   *             avocado DUT contract
   *   separate: {prefix}_bits_{Field} — TX: every canonical alias (outputs);
   *             RX: one input per physical field under its primary alias. */
  def exportCHIFunctional[T <: Bundle](prefix: String, chan: AbstractCHIChannel[T],
                                       dutDrives: Boolean,
                                       aliases: Seq[(String, Option[UInt])],
                                       style: CHIFlitPinStyle,
                                       packedPin: String = "bits"): Unit = {
    exportCHIHandshake(prefix, chan, dutDrives)
    if (style == CHIFlitPinStyle.Packed) {
      if (dutDrives) {
        val bits = IO(Output(UInt(chan.flit.getWidth.W))).suggestName(s"${prefix}_${packedPin}")
        bits := packFlit(chan.flit)
      } else {
        val bits = IO(Input(UInt(chan.flit.getWidth.W))).suggestName(s"${prefix}_${packedPin}")
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
   * channel, in the style(s) enabled by mode. */
  def monitorCHIChannel[T <: Bundle](prefix: String, chan: AbstractCHIChannel[T],
                                     aliases: Seq[(String, Option[UInt])],
                                     mode: CHIMonitorMode): Unit = {
    if (mode == CHIMonitorMode.Off) return
    Seq("flitpend" -> chan.flitpend, "flitv" -> chan.flitv, "lcrdv" -> chan.lcrdv).foreach {
      case (name, signal) =>
        val pin = IO(Output(Bool())).suggestName(s"${prefix}_${name}")
        pin := signal
    }
    if (CHIMonitorMode.separateOn(mode)) {
      aliases.foreach { case (name, field) => field.foreach { view =>
        val pin = IO(Output(chiselTypeOf(view))).suggestName(s"${prefix}_flit_${name}")
        pin := view
      }}
    }
    if (CHIMonitorMode.packedOn(mode)) {
      val pin = IO(Output(UInt(chan.flit.getWidth.W))).suggestName(s"${prefix}_flit")
      pin := packFlit(chan.flit)
    }
  }
}
