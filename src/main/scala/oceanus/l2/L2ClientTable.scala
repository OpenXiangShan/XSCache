package oceanus.l2

import chisel3._
import chisel3.util._
import utility._
import oceanus.l2._
import org.chipsalliance.cde.config.Parameters
import xscache.oceanus.compactchi.HasCCHIParameters


class L2ClientTable(val sliceNum: Int)(implicit val p: Parameters) 
    extends Module 
    with HasL2Params
    with HasCCHIParameters {

  val theOnlyDCacheNID = 0

  val io = IO(new Bundle {
    val queryREQ = Input(Vec(sliceNum, Vec(paramL2.mshrSize, UInt(paramCCHI.UpstreamNodeID_Width.W))))
    val clientsREQ = Output(Vec(sliceNum, Vec(paramL2.mshrSize, Vec(1, Bool())))) // TODO: parameterize with coherent l2 client count

    val queryEVT = Input(Vec(sliceNum, Vec(paramL2.mshrSize, UInt(paramCCHI.UpstreamNodeID_Width.W))))
    val clientsEVT = Output(Vec(sliceNum, Vec(paramL2.mshrSize, Vec(1, Bool())))) // TODO: parameterize with coherent l2 client count
  })

  io.queryREQ.zip(io.clientsREQ).foreach { case (query, clients) => {
    query.zip(clients).foreach { case (query, clients) =>
      clients.head := query === theOnlyDCacheNID.U
  }}}

  io.queryEVT.zip(io.clientsEVT).foreach { case (query, clients) => {
    query.zip(clients).foreach { case (query, clients) =>
      clients.head := query === theOnlyDCacheNID.U
  }}}
}
