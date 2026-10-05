package oceanus

import chisel3._
import circt.stage.ChiselStage
import chisel3.util._
import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config._
import scala.collection.mutable.ArrayBuffer
import utility._
import oceanus.l2.{L2Configuration, L2Params, L2ParamsKey, L2Top, L2UpstreamPortType, L2UpstreamTable, L2UpstreamTableEntry}
import oceanus.chi.{CHIParameters, CHIParametersKey, EnumCHIIssue}
import xscache.oceanus.compactchi.{CCHIParameters, CCHIParametersKey}

/*  Oceanus L2 x 4 as the DUT of the CHIron Avocado integration package
 *  (cchi/cohestra/cohestra_packages/avocado): every L2 exports one CCHI
 *  Type-1 upstream port to the cohestra_v3 harness (Taurus upstream nodes)
 *  and one packed CHI RN-F downstream port that avocado_top mates 1:1 with
 *  the RNFEESAM ports of the external Avocado 3x2 mesh render:
 *
 *       cchi_t1p{0..3}_*  (harness Taurus upstream nodes, CCHI NIDs 0..3)
 *                     |
 *                L2Top x 4
 *                     |  CHIRNFInterface, packed
 *                     v
 *       chi_rn{0..3}_*  -->  mesh RNF{32,64,12,44} (LDID order)
 *
 *  The avocado DUT contract (integration/avocado_dut_if.svh) this top emits:
 *    - module clock/reset (high-active reset), no other control pins;
 *    - per port P in 0..3: cchi_t1p{P}_{rxevt,rxreq,txsnp,txrsp,rxrsp,txdat,
 *      rxdat}_{valid,ready,bits_<Field>} — separate-field, identical to the
 *      cohestra_v3 CCHI Type-1 contract. NOTE: this boundary is the L2's own
 *      CCHI contract, kept independent of the AMBA CHI configuration; the
 *      hand-written adapter scripts/avocado/avocado_dut.sv (the actual
 *      `AVOCADO_DUT_MODULE`) bridges the shape differences against the
 *      package's wire declarations (unpacked-array Data, and the narrower
 *      TxnID/DBID/DataID fields of the contract's Type-1 view);
 *    - per port P: chi_rn{P}_{txreq,txrsp,txdat,rxrsp,rxdat,rxsnp}_
 *      {flitpend,flitv,flit,lcrdv} — packed flit pin named ..._flit
 *      (REQ 162b / RSP 73b / DAT 422b / SNP 115b under the pinned
 *      CHIParameters below — field-for-field identical to the mesh render's
 *      documented layout in avocado/rtl/avocado.sv);
 *    - per port P: mon_chi_rn{P}_{txreq,txrsp,txdat,rxsnp,rxrsp,rxdat}_
 *      {flitpend,flitv,lcrdv[,flit][,flit_<Field>]} — passive output-only
 *      monitor taps of the downstream link, styles selected by --chi-mon
 *      (packed by default);
 *    - NO link-active / SACTIVE / SYSCO / EVENT pins: avocado_top manages
 *      them for the mesh, and this TestTop applies the same policy to the
 *      L2s internally (link always up, always active, coherency always
 *      granted), so the L2 link pins never reach the boundary.
 *
 *  Per-instance identities (defaults match the mesh render):
 *    --chi-nids   CHI downstream node ID (RN-F SrcID) per L2, default
 *                 32,64,12,44 — the mesh's RNFEESAM LDID order;
 *    --cchi-nids  upstream CCHI Type-1 node ID per L2, default 0,1,2,3 —
 *                 one coherent client per L2 (harness upstream.type1.nodeid).
 *
 *  Known boundaries (same as TestTop_L2MultiCHI):
 *    - DnTXREQ.TgtID (home node ID) stays hardwired to 0 in L2VPipeREQ
 *      ("E-SAM only currently"); safe here because the mesh RN-F ports are
 *      RNFEESAM — the XP-hosted external RN-SAM overrides TgtID by address
 *      for every opcode except PCrdReturn.
 *    - CCHI NIDs must fit CCHIParameters.UpstreamNodeID_Width (default 4).
 */

class TestTop_L2Avocado(
  val numSlices: Int,
  val chiNIDs: Seq[Int],
  val cchiNIDs: Seq[Int],
  val chiMon: CHIMonitorMode = CHIMonitorMode.Packed
)(implicit p: Parameters) extends Module {

  import TestTop_CHIExport._

  // The avocado contract fixes exactly 4 CCHI + 4 CHI RN port groups.
  val numL2 = 4

  val l2s = Seq.tabulate(numL2)(i => Module(new L2Top(new L2Configuration(
    nodeId = chiNIDs(i),
    eSAM = true,
    slices = 0 until numSlices,
    upstream = new L2UpstreamTable(Seq(
      L2UpstreamTableEntry(L2UpstreamPortType.Type1, cchiNIDs(i))))))))

  l2s.zipWithIndex.foreach { case (l2, i) =>
    // -- Exported upstream CCHI Type-1 port (avocado contract: cchi_t1p{P}_*)
    l2.io.t1p.foreach { port =>
      exportChannel(s"cchi_t1p${i}_rxevt", port.UpEVT, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_rxreq", port.UpREQ, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_txsnp", port.DnSNP, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_txrsp", port.DnRSP, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_rxrsp", port.UpRSP, dutDrives = false)
      exportChannel(s"cchi_t1p${i}_txdat", port.DnDAT, dutDrives = true)
      exportChannel(s"cchi_t1p${i}_rxdat", port.UpDAT, dutDrives = false)
    }

    // -- Exported downstream CHI RN-F link: packed, monitor-style ..._flit pin
    exportCHIFunctional(s"chi_rn${i}_txreq", l2.io.chi.txreq, dutDrives = true,
      req(l2.io.chi.txreq.flit), CHIFlitPinStyle.Packed, packedPin = "flit")
    exportCHIFunctional(s"chi_rn${i}_txrsp", l2.io.chi.txrsp, dutDrives = true,
      rsp(l2.io.chi.txrsp.flit), CHIFlitPinStyle.Packed, packedPin = "flit")
    exportCHIFunctional(s"chi_rn${i}_txdat", l2.io.chi.txdat, dutDrives = true,
      dat(l2.io.chi.txdat.flit), CHIFlitPinStyle.Packed, packedPin = "flit")
    exportCHIFunctional(s"chi_rn${i}_rxsnp", l2.io.chi.rxsnp, dutDrives = false,
      snp(l2.io.chi.rxsnp.flit), CHIFlitPinStyle.Packed, packedPin = "flit")
    exportCHIFunctional(s"chi_rn${i}_rxrsp", l2.io.chi.rxrsp, dutDrives = false,
      rsp(l2.io.chi.rxrsp.flit), CHIFlitPinStyle.Packed, packedPin = "flit")
    exportCHIFunctional(s"chi_rn${i}_rxdat", l2.io.chi.rxdat, dutDrives = false,
      dat(l2.io.chi.rxdat.flit), CHIFlitPinStyle.Packed, packedPin = "flit")

    // -- CHI monitor taps (passive output-only; mon_chi_rn{P}_*)
    monitorCHIChannel(s"mon_chi_rn${i}_txreq", l2.io.chi.txreq, req(l2.io.chi.txreq.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_txrsp", l2.io.chi.txrsp, rsp(l2.io.chi.txrsp.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_txdat", l2.io.chi.txdat, dat(l2.io.chi.txdat.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_rxsnp", l2.io.chi.rxsnp, snp(l2.io.chi.rxsnp.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_rxrsp", l2.io.chi.rxrsp, rsp(l2.io.chi.rxrsp.flit), chiMon)
    monitorCHIChannel(s"mon_chi_rn${i}_rxdat", l2.io.chi.rxdat, dat(l2.io.chi.rxdat.flit), chiMon)

    // -- Link-active / SACTIVE / SYSCO: the avocado contract keeps these off
    // the DUT boundary (avocado_top manages them for the mesh). Apply the
    // same policy to the L2s internally: link always up (peer always requests
    // and instantly acks), always active, coherency always granted. L2 output
    // pins (rxlinkactiveack, txsactive, syscoreq) are intentionally left open.
    l2.io.chi.rxlinkactivereq := true.B
    l2.io.chi.txlinkactiveack := l2.io.chi.txlinkactivereq
    l2.io.chi.rxsactive := true.B
    l2.io.chi.syscoack := true.B
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

object TestTop_L2Avocado extends App {

  val usage = """
Usage: TestTop_L2Avocado [<--option> <values>]

      --slices <slice_num>      specify the number of slices per L2, 2 by default;
                                external SAM supports 1 to 4 slices
      --noperf                  disable all DUT performance counters (drops the
                                LogPerfEndpoint bulk; much faster firtool/verilation)
      --chi-nids <n,n,n,n>      CHI downstream node ID (RN-F SrcID) of each L2;
                                defaults to 32,64,12,44 — the Avocado mesh render's
                                RNFEESAM LDID order (chi_rn0->RNF32, chi_rn1->RNF64,
                                chi_rn2->RNF12, chi_rn3->RNF44)
      --cchi-nids <n,n,n,n>     upstream CCHI Type-1 node ID of each L2 (one
                                coherent client per L2); defaults to 0,1,2,3,
                                matching the harness's upstream.type1.nodeid
      --chi-mon <off|separate|packed|both>
                                CHI monitor port taps (mon_chi_rn{P}_*), packed
                                by default
  """

  if (args.contains("--help"))
  {
    println(usage)
    System.exit(0)
  }

  var numSlices = 2
  var noPerf = false
  var chiNIDsArg: Option[Seq[Int]] = None
  var cchiNIDsArg: Option[Seq[Int]] = None
  var chiMonArg: Option[String] = None

  def parseNIDList(value: String): Seq[Int] = value.split(",").map(_.trim.toInt).toIndexedSeq

  val varArgs = ArrayBuffer[String]()
  var i = 0
  while (i < args.length) {
    args(i) match {
      case "--slices"    => numSlices = args(i + 1).toInt; i += 2
      case "--noperf"    => noPerf = true; i += 1
      case "--chi-nids"  => chiNIDsArg = Some(parseNIDList(args(i + 1))); i += 2
      case "--cchi-nids" => cchiNIDsArg = Some(parseNIDList(args(i + 1))); i += 2
      case "--chi-mon"   => chiMonArg = Some(args(i + 1)); i += 2
      case other         => varArgs += other; i += 1
    }
  }
  varArgs.trimToSize()

  val numL2 = 4 // fixed by the avocado DUT contract (see header)
  require(numSlices >= 1 && numSlices <= 4, s"Unsupported slice count $numSlices under eSAM")

  val chiMon = chiMonArg match {
    case None | Some("packed")   => CHIMonitorMode.Packed
    case Some("off")             => CHIMonitorMode.Off
    case Some("separate")        => CHIMonitorMode.Separate
    case Some("both")            => CHIMonitorMode.Both
    case Some(other)             =>
      throw new IllegalArgumentException(s"Unknown --chi-mon '$other' (expected off|separate|packed|both)")
  }

  val chiNIDs = chiNIDsArg.getOrElse(Seq(32, 64, 12, 44))
  val cchiNIDs = cchiNIDsArg.getOrElse(Seq(0, 1, 2, 3))
  require(chiNIDs.length == numL2, s"--chi-nids lists ${chiNIDs.length} NIDs but the contract fixes $numL2 L2s")
  require(cchiNIDs.length == numL2, s"--cchi-nids lists ${cchiNIDs.length} NIDs but the contract fixes $numL2 L2s")

  // Keep in sync with the CHIParametersKey / CCHIParametersKey configs below.
  val chiNodeIdWidth = 11
  require(chiNIDs.forall(n => n >= 0 && n < (1 << chiNodeIdWidth)),
    s"CHI node IDs must fit nodeIdWidth=$chiNodeIdWidth: $chiNIDs")

  val cchiUpstreamNIDWidth = CCHIParameters().UpstreamNodeID_Width
  require(cchiNIDs.forall(n => n >= 0 && n < (1 << cchiUpstreamNIDWidth)),
    s"CCHI upstream NIDs must fit UpstreamNodeID_Width=$cchiUpstreamNIDWidth: $cchiNIDs")

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
    // CCHI parameters are the L2's own upstream contract and stay independent
    // of the AMBA CHI side; the avocado integration layer
    // (scripts/avocado/avocado_dut.sv) adapts any width differences at the
    // boundary instead of bending this configuration.
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
      new TestTop_L2Avocado(numSlices, chiNIDs, cchiNIDs, chiMon)(config))
      +: TestTopFirtoolOptions())
}
