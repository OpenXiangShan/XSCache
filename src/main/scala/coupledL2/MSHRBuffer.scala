/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package xscache.coupledL2

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xscache.coupledL2.utils._

class MSHRBufRead(implicit p: Parameters) extends L2Bundle {
  val id = Output(UInt(mshrBits.W))
}

class MSHRBufResp(implicit p: Parameters) extends L2Bundle {
  val id = Output(UInt(mshrBits.W))
  val data = Output(new DSBlock)
}

class MSHRBufWrite(implicit p: Parameters) extends L2Bundle {
  val id = Output(UInt(mshrBits.W))
  val data = Output(new DSBlock)
  val beatMask = Output(UInt(beatSize.W))
}

// MSHR Buffer is indexed by physical MSHR ID. MSHRCtl grants a per-entry lease
// before either context can issue a Probe that may return ProbeAckData.
class MSHRBuffer(wPorts: Int = 1)(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle() {
    val r = Flipped(ValidIO(new MSHRBufRead))
    val resp = new MSHRBufResp
    // Second read port for the direct ReplaceMSHR->TXDAT copyback path
    // (flop-based array, so this is just another read mux).
    val r2 = Flipped(ValidIO(new MSHRBufRead))
    val resp2 = new MSHRBufResp
    val w = Vec(wPorts, Flipped(ValidIO(new MSHRBufWrite)))
  })

  val buffer = Reg(Vec(mshrsAll, Vec(beatSize, UInt((beatBytes * 8).W))))

  buffer.zipWithIndex.foreach {
    case (block, i) =>
      val wens = VecInit(io.w.map(w => w.valid && w.bits.id === i.U)).asUInt
      assert(PopCount(wens) <= 2.U, "triple write to the same MSHR buffer entry")

      val w_data = PriorityMux(wens, io.w.map(_.bits.data))
      val w_beatSel = PriorityMux(wens, io.w.map(_.bits.beatMask))
      when(wens.orR) {
        // block := w_data
        block.zip(w_beatSel.asBools).zipWithIndex.foreach { case ((beat, sel), i) =>
          when (sel) { beat := w_data.data((i+1) * beatBytes * 8 - 1, i * beatBytes * 8) }
        }
      }
  }

  val rdata = buffer(io.r.bits.id).asUInt
  val rid = RegEnable(io.r.bits.id, 0.U, io.r.valid)
  io.resp.id := rid
  io.resp.data.data := RegEnable(rdata, 0.U.asTypeOf(rdata), io.r.valid)

  val rdata2 = buffer(io.r2.bits.id).asUInt
  val rid2 = RegEnable(io.r2.bits.id, 0.U, io.r2.valid)
  io.resp2.id := rid2
  io.resp2.data.data := RegEnable(rdata2, 0.U.asTypeOf(rdata2), io.r2.valid)
}

// may consider just choose an empty entry to insert
// instead of using MSHRId to index
// by this, we can reduce the size of MSHRBuffer
// yet another problem, MSHR may be unable to insert because of FULL
