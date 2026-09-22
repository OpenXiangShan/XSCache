package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import xscache.oceanus.compactchi.HasCCHIParameters


class L2ClientTable(val sliceNum: Int, val clientNIDs: Seq[Int] = Seq(0))(implicit val p: Parameters) 
    extends Module 
    with HasL2Params
    with HasCCHIParameters {

  // Single coherent upstream client for now (TODO: parameterize with coherent
  // l2 client count). Every Type-1 upstream port NID of the L2UpstreamTable is
  // translated to the same client bit: multiple Type-1 ports are different
  // upstream sources of the same client and share its Directory client bit.
  val theOnlyDCacheNIDs = clientNIDs

  val io = IO(new Bundle {
    val queryREQ = Input(Vec(sliceNum, Vec(paramL2.mshrSize, UInt(paramCCHI.UpstreamNodeID_Width.W))))
    val clientsREQ = Output(Vec(sliceNum, Vec(paramL2.mshrSize, Vec(1, Bool())))) // TODO: parameterize with coherent l2 client count

    val queryEVT = Input(Vec(sliceNum, Vec(paramL2.mshrSize, UInt(paramCCHI.UpstreamNodeID_Width.W))))
    val clientsEVT = Output(Vec(sliceNum, Vec(paramL2.mshrSize, Vec(1, Bool())))) // TODO: parameterize with coherent l2 client count
  })

  io.queryREQ.zip(io.clientsREQ).foreach { case (query, clients) => {
    query.zip(clients).foreach { case (query, clients) =>
      clients.head := theOnlyDCacheNIDs.map(nid => query === nid.U).reduce(_ || _)
  }}}

  io.queryEVT.zip(io.clientsEVT).foreach { case (query, clients) => {
    query.zip(clients).foreach { case (query, clients) =>
      clients.head := theOnlyDCacheNIDs.map(nid => query === nid.U).reduce(_ || _)
  }}}
}
