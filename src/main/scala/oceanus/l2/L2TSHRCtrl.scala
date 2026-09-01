package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.chi.bundle._
import oceanus.compactchi._
import oceanus.l2._
import oceanus.l2.L2Common._
import oceanus.l2.L2Directory._
import oceanus.l2.L2DataStorage._
import oceanus.l2.tshr._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.SeqBoolBitwiseOps
import freechips.rocketchip.util.SeqToAugmentedSeq
import oceanus.compactchi.CCHIOpcode._


trait L2SliceLocatable {

  val sliceNum: Int
  val sliceIdx: Int
  val sliceNID: Int

  def getTSHRIdFromDnTxnID(txnId: UInt) = txnId >> log2Ceil(sliceNum)

  def getTSHRIdFromUpTxnID(txnId: UInt) = txnId

  def getSliceIdxFromTxnID(txnId: UInt) = if (sliceNum > 1) txnId(log2Ceil(sliceNum) - 1, 0) else 0.U

  def routeSliceFromTxnID(txnId: UInt) = getSliceIdxFromTxnID(txnId) === sliceIdx.U

  def getTxnIDFromTSHRId(tshrId: Int) = (tshrId.U << log2Ceil(sliceNum)) | sliceIdx.U(log2Ceil(sliceNum).W)
}

class L2TSHRCtrl(val sliceNum: Int, val sliceIdx: Int, val sliceNID: Int)(implicit val p: Parameters) extends Module 
    with HasL2Params 
    with L2SliceLocatable {

  val io = IO(new Bundle {
    val UpRXEVT = Flipped(Decoupled(new FlitEVT))
    val DnRXSNP = Flipped(Decoupled(new CHIBundleSNP))
    val UpRXREQ = Flipped(Decoupled(new FlitREQ))

    val UpTXREQ = Decoupled(new FlitREQ)

    val UpTXSNP = Decoupled(new FlitSNP)

    val DnTXREQ = Decoupled(new CHIBundleREQ)

    val UpRXRSP = Flipped(Valid(new FlitUpRSP))
    val UpRXDAT = Flipped(Valid(new FlitUpDAT))
    val UpTXRSP = Decoupled(new FlitDnRSP)
    val UpTXDAT = Decoupled(new FlitDnDAT)

    val DnRXRSP = Flipped(Valid(new CHIBundleRSP))
    val DnRXDAT = Flipped(Valid(new CHIBundleDAT))
    val DnTXRSP = Decoupled(new CHIBundleRSP)
    val DnTXDAT = Decoupled(new CHIBundleDAT)

    val toPCreditPool = Vec(paramL2.mshrSize, Valid(new L2PCreditPool.Entry))
    val fromPCreditPool = Input(Vec(paramL2.mshrSize, Bool()))

    val toClientTableREQ = Output(Vec(paramL2.mshrSize, UInt(8.W))) // TODO: configurable with upstream nodeId width
    val fromClientTableREQ = Input(Vec(paramL2.mshrSize, Vec(1, Bool()))) // TODO: parameterize with coherent l2 client count

    val toClientTableEVT = Output(Vec(paramL2.mshrSize, UInt(8.W))) // TODO: configurable with upstream nodeId width
    val fromClientTableEVT = Input(Vec(paramL2.mshrSize, Vec(1, Bool()))) // TODO: parameterize with coherent l2 client count
  })

  // -- RX channel connections
  val tshrs = Seq.tabulate(paramL2.mshrSize)(i => Module(new L2TSHR(sliceNum, sliceIdx, sliceNID, i)))

  tshrs.foreach { case t => 
    t.io.UpRXEVT := io.UpRXEVT.bits
    t.io.DnRXSNP := io.DnRXSNP.bits
    t.io.UpRXREQ := io.UpRXREQ.bits

    t.io.UpRXRSP.valid := io.UpRXRSP.valid && getTSHRIdFromUpTxnID(io.UpRXRSP.bits.TxnID) === t.tshrId.U 
    t.io.UpRXRSP.bits := io.UpRXRSP.bits

    t.io.UpRXDAT.valid := io.UpRXDAT.valid && getTSHRIdFromUpTxnID(io.UpRXDAT.bits.TxnID) === t.tshrId.U
    t.io.UpRXDAT.bits := io.UpRXDAT.bits

    t.io.DnRXRSP.valid := io.DnRXRSP.valid && getTSHRIdFromDnTxnID(io.DnRXRSP.bits.TxnID.get) === t.tshrId.U
    t.io.DnRXRSP.bits := io.DnRXRSP.bits

    t.io.DnRXDAT.valid := io.DnRXDAT.valid && getTSHRIdFromDnTxnID(io.DnRXDAT.bits.TxnID.get) === t.tshrId.U
    t.io.DnRXDAT.bits := io.DnRXDAT.bits
  }

  assert(!io.UpRXRSP.valid || (PopCount(tshrs.map(_.io.UpRXRSP.valid)) =/= 0.U), "UpRXRSP valid but no TSHR selected, flit lost")
  assert(!io.UpRXDAT.valid || (PopCount(tshrs.map(_.io.UpRXDAT.valid)) =/= 0.U), "UpRXDAT valid but no TSHR selected, flit lost")
  assert(!io.DnRXRSP.valid || (PopCount(tshrs.map(_.io.DnRXRSP.valid)) =/= 0.U), "DnRXRSP valid but no TSHR selected, flit lost")
  assert(!io.DnRXDAT.valid || (PopCount(tshrs.map(_.io.DnRXDAT.valid)) =/= 0.U), "DnRXDAT valid but no TSHR selected, flit lost")
  // ----------------------------------------------------------------

  // -- TSHR Allocation connections
  val tshrAlloc = Module(new L2TSHRAlloc(new L2TSHRAllocConfig(
    cluster = Seq(Seq(L2TSHRAllocTarget.EVT), Seq(L2TSHRAllocTarget.SNP, L2TSHRAllocTarget.REQ)), // TODO: parameterize clustering
    resv = Seq(
      (paramL2.mshrSize - 1, L2TSHRResvTarget.L2EVT),
      (paramL2.mshrSize - 2, L2TSHRResvTarget.L1EVT),
      (paramL2.mshrSize - 3, L2TSHRResvTarget.L3SNP)) // TODO: parameterize reservation
  )))

  tshrAlloc.io.fromTSHRCtrl.RXEVT <> io.UpRXEVT
  tshrAlloc.io.fromTSHRCtrl.RXSNP <> io.DnRXSNP
  tshrAlloc.io.fromTSHRCtrl.RXREQ <> io.UpRXREQ

  tshrs.foreach { case t => 
    tshrAlloc.io.fromTSHR(t.tshrId).bits := t.io.toAlloc
    tshrAlloc.io.fromTSHR(t.tshrId).valid := t.io.valid
    t.io.fromAlloc := tshrAlloc.io.toTSHR(t.tshrId)
  }
  // ----------------------------------------------------------------

  // -- TX channel connections
  fastArb(tshrs.map(_.io.UpTXREQ), io.UpTXREQ, Some("TSHRsToUpTXREQ"))

  fastArb(tshrs.map(_.io.UpTXSNP), io.UpTXSNP, Some("TSHRsToUpTXSNP"))

  fastArb(tshrs.map(_.io.UpTXRSP), io.UpTXRSP, Some("TSHRsToUpTXRSP"))
  fastArb(tshrs.map(_.io.UpTXDAT), io.UpTXDAT, Some("TSHRsToUpTXDAT"))

  fastArb(tshrs.map(_.io.DnTXREQ), io.DnTXREQ, Some("TSHRsToDnTXREQ"))

  fastArb(tshrs.map(_.io.DnTXRSP), io.DnTXRSP, Some("TSHRsToDnTXRSP"))
  fastArb(tshrs.map(_.io.DnTXDAT), io.DnTXDAT, Some("TSHRsToDnTXDAT"))
  // ----------------------------------------------------------------

  // -- Directory connections
  val directory = Module(new Directory)

  directory.io.toDir.zip(tshrs.map(_.io.toDir)).foreach { case (sink, source) => { 
    sink := source 
  }}

  directory.io.fromDir.zip(tshrs.map(_.io.fromDir)).foreach { case (source, sink) => {
    sink := source
  }}

  directory.io.tshrIdle := tshrs.map(!_.io.valid)
  // ----------------------------------------------------------------

  // -- Data Storage connections
  val dataStorage = Module(new DataStorage)

  dataStorage.io.fromTSHR.zip(tshrs.map(_.io.toDS)).foreach { case (sink, source) => {
    sink := source
  }}

  dataStorage.io.toTSHR.zip(tshrs.map(_.io.fromDS)).foreach { case (source, sink) => {
    sink := source
  }}
  // ----------------------------------------------------------------

  // -- TSHR Inter-Unlocking connections
  tshrs.map(_.io.self_unlock_dir).zipWithIndex.foreach { case (sink, i) => {
    sink := tshrs.map(_.io.peer_unlock_dir(i)).orR
  }}

  tshrs.map(_.io.self_unlock_ds).zipWithIndex.foreach { case (sink, i) => {
    sink := tshrs.map(_.io.peer_unlock_ds(i)).orR
  }}
  // ----------------------------------------------------------------

  // -- P-Credit Pool connections
  io.toPCreditPool.zip(tshrs.map(_.io.toPCreditPool)).foreach { case (sink, source) => {
    sink := source
  }}

  io.fromPCreditPool.zip(tshrs.map(_.io.fromPCreditPool)).foreach { case (source, sink) => {
    sink := source
  }}
  // ----------------------------------------------------------------

  // -- Client Table connections
  io.toClientTableREQ.zip(tshrs.map(_.io.toClientTableREQ)).foreach { case (sink, source) => {
    sink := source
  }}

  io.fromClientTableREQ.zip(tshrs.map(_.io.fromClientTableREQ)).foreach { case (source, sink) => {
    sink := source
  }}

  io.toClientTableEVT.zip(tshrs.map(_.io.toClientTableEVT)).foreach { case (sink, source) => {
    sink := source
  }}

  io.fromClientTableEVT.zip(tshrs.map(_.io.fromClientTableEVT)).foreach { case (source, sink) => {
    sink := source
  }}
  // ----------------------------------------------------------------
}
