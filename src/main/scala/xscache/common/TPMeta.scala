package xscache.common

import chisel3._

class TPmetaReq(hartIdLen: Int, fullAddressBits: Int, offsetBits: Int) extends Bundle {
  val hartid = UInt(hartIdLen.W)
  val set = UInt(10.W)
  val way = UInt(4.W)
  val wmode = Bool()
  val rawData = Vec(512 / (fullAddressBits - offsetBits), UInt((fullAddressBits - offsetBits).W))
}

class TPmetaResp(hartIdLen: Int, fullAddressBits: Int, offsetBits: Int) extends Bundle {
  val hartid = UInt(hartIdLen.W)
  val rawData = Vec(512 / (fullAddressBits - offsetBits), UInt((fullAddressBits - offsetBits).W))
}
