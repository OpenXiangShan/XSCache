/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package xscache.openLLC

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{FastArbiter}
import xscache.chi._
import xscache.chi.CHICohStates._

class RefillBufRead(implicit p: Parameters) extends LLCBundle {
  val id = Output(UInt(log2Ceil(mshrs.refill).W))
}

class RefillState(implicit p: Parameters) extends LLCBundle {
  val s_refill = Bool()
  val w_datRsp = Bool()
  val w_snpRsp = Bool()
}

class RefillRequest(implicit p: Parameters) extends LLCBundle {
  val state = new RefillState()
  val task = new Task()
  val dirResult = new DirResult()
  val isWrite = Bool()
}

class RefillEntry(implicit p: Parameters) extends TaskEntry {
  val state = new RefillState()
  val data = new DSBlock()
  val beatValids = Vec(beatSize, Bool())
  val dirResult = new DirResult()
  val isWrite = Bool()
}

class RefillUnit(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* receive refill requests from mainpipe */
    val alloc = Flipped(ValidIO(new RefillRequest()))
  
    /* send refill task to request arbiter */
    val task = DecoupledIO(new Task())

    /* response from upstream RXDAT/RXRSP channel */ 
    val respData = Flipped(ValidIO(new RespWithData()))
    val resp = Flipped(ValidIO(new Resp()))

    /* response from downstream RXDAT channel */
    val snRespData = Flipped(ValidIO(new RespWithData()))
    val bypassData = Flipped(Vec(beatSize, ValidIO(new RespWithData())))

    /* refill data read */
    val read = Flipped(ValidIO(new RefillBufRead()))
    val data = Output(new DSBlock())

    /* refill buffers info */
    val refillInfo = Vec(mshrs.refill, ValidIO(new BlockInfo()))
  })

  val rnRespData = io.respData
  val snRespData = io.snRespData
  val bypassRespData = io.bypassData
  val rsp = io.resp

  /* Data Structure */
  val buffer   = RegInit(VecInit(Seq.fill(mshrs.refill)(0.U.asTypeOf(new RefillEntry()))))
  val issueArb = Module(new FastArbiter(new Task(), mshrs.refill))

  val full = Cat(buffer.map(_.valid)).andR

  /* Alloc */
  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val canAlloc = !full && io.alloc.valid
  when(canAlloc) {
    val entry = buffer(insertIdx)
    entry.valid := true.B
    entry.state := io.alloc.bits.state
    entry.task := io.alloc.bits.task
    entry.task.bufID := insertIdx
    entry.dirResult := io.alloc.bits.dirResult
    entry.beatValids := VecInit(Seq.fill(beatSize)(false.B))
    entry.isWrite := io.alloc.bits.isWrite
  }
  assert(!full || !io.alloc.valid, "RefillBuf overflow")

  /* Update state */
  def updateRespDataEntry(entry: RefillEntry, responseData: RespWithData): Unit = {
    val isWrite = entry.isWrite
    val inv_CBWrData = responseData.resp === I
    val cancel = isWrite && inv_CBWrData
    val clients_hit = entry.dirResult.clients.hit
    val clients_meta = entry.dirResult.clients.meta

    assert(
      !isWrite || inv_CBWrData || clients_hit && clients_meta(responseData.srcID).valid,
      "Non-exist block release?(addr: 0x%x)",
      Cat(entry.task.tag, entry.task.set, entry.task.bank, entry.task.off)
    )

    val beatId = responseData.dataID >> log2Ceil(beatBytes / 16)
    val newBeatValids = entry.beatValids.asUInt | UIntToOH(beatId)
    entry.valid := !cancel
    entry.beatValids := VecInit(newBeatValids.asBools)
    entry.state.w_datRsp := newBeatValids.andR
    entry.data.data(beatId) := responseData.data
    entry.task.resp := responseData.resp
    when(responseData.opcode === SnpRespData) {
      val src_idOH  = UIntToOH(responseData.srcID)(numRNs - 1, 0)
      val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
      entry.task.snpVec := newSnpVec
      entry.state.w_snpRsp := !Cat(newSnpVec).orR
    }
  }

  def updateBypassRespDataEntry(entry: RefillEntry): Unit = {
    val bypassBeatValids = VecInit(bypassRespData.map(_.valid))
    val newBeatValids = entry.beatValids.asUInt | bypassBeatValids.asUInt
    entry.beatValids := VecInit(newBeatValids.asBools)
    entry.state.w_datRsp := newBeatValids.andR
    entry.task.resp := PriorityMux(bypassRespData.map(d => d.valid -> d.bits.resp))

    bypassRespData.zipWithIndex.foreach { case (data, i) =>
      val beatId = data.bits.dataID >> log2Ceil(beatBytes / 16)
      assert(!data.valid || beatId === i.U, "Refill bypass data beat mismatch")
      when(data.valid) {
        entry.data.data(i) := data.bits.data
      }
    }
  }

  val rnRespDataUpdateVec = VecInit(buffer.map(e => e.task.reqID === rnRespData.bits.txnID && e.valid))
  val snRespDataUpdateVec = VecInit(buffer.map(e =>
    e.task.reqID === snRespData.bits.txnID && e.valid && e.task.chiOpcode === StashOnceShared
  ))
  val bypassRespDataValid = bypassRespData.map(_.valid).reduce(_ || _)
  val bypassRespDataTxnID = PriorityMux(bypassRespData.map(d => d.valid -> d.bits.txnID))
  val bypassRespDataUpdateVec = VecInit(buffer.map(e =>
    e.task.reqID === bypassRespDataTxnID && e.valid && e.task.chiOpcode === StashOnceShared
  ))
  assert(!rnRespData.valid || PopCount(rnRespDataUpdateVec) < 2.U, "Refill task repeated")
  assert(!snRespData.valid || PopCount(snRespDataUpdateVec) < 2.U, "Refill task repeated")
  assert(!bypassRespDataValid || PopCount(bypassRespDataUpdateVec) < 2.U, "Refill task repeated")
  bypassRespData.foreach { data =>
    assert(!bypassRespDataValid || !data.valid || data.bits.txnID === bypassRespDataTxnID,
      "Refill bypass data has multiple TxnIDs"
    )
  }
  buffer.zipWithIndex.foreach { case (entry, i) =>
    val rnRespDataHit = rnRespData.valid && rnRespDataUpdateVec(i)
    val snRespDataHit = snRespData.valid && snRespDataUpdateVec(i)
    val bypassRespDataHit = bypassRespDataValid && bypassRespDataUpdateVec(i)
    assert(
      !(rnRespDataHit && snRespDataHit) &&
        !(rnRespDataHit && bypassRespDataHit) &&
        !(snRespDataHit && bypassRespDataHit),
      "Refill task receives data from multiple sources"
    )
    when(rnRespDataHit) {
      updateRespDataEntry(entry, rnRespData.bits)
    }.elsewhen(snRespDataHit) {
      updateRespDataEntry(entry, snRespData.bits)
    }.elsewhen(bypassRespDataHit) {
      updateBypassRespDataEntry(entry)
    }
  }

  when(rsp.valid) {
    val update_vec = buffer.map(e =>
      e.task.reqID === rsp.bits.txnID && e.valid && !e.state.w_snpRsp && rsp.bits.opcode === SnpResp
    )
    assert(PopCount(update_vec) < 2.U, "Refill task repeated")
    val canUpdate = Cat(update_vec).orR
    val update_id = PriorityEncoder(update_vec)
    when(canUpdate) {
      val entry = buffer(update_id)
      val src_idOH = UIntToOH(rsp.bits.srcID)(numRNs - 1, 0)
      val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH).asBools)
      entry.task.snpVec := newSnpVec
      entry.state.w_snpRsp := !Cat(newSnpVec).orR
    }
  }

  when(rnRespData.valid && rsp.valid) {
    when(rnRespData.bits.opcode === SnpRespData && rsp.bits.opcode === SnpResp) {
      when(rnRespData.bits.txnID === rsp.bits.txnID) {
        val update_vec = buffer.map(e => e.task.reqID === rsp.bits.txnID && e.valid && !e.state.w_snpRsp)
        assert(PopCount(update_vec) < 2.U, "Refill task repeated")
        val update_id = PriorityEncoder(update_vec)
        val entry = buffer(update_id)
        val canUpdate = Cat(update_vec).orR
        when(canUpdate) {
          val src_idOH_dat = UIntToOH(rnRespData.bits.srcID)(numRNs - 1, 0)
          val src_idOH_rsp = UIntToOH(rsp.bits.srcID)(numRNs - 1, 0)
          val newSnpVec = VecInit((entry.task.snpVec.asUInt & ~src_idOH_dat & ~src_idOH_rsp).asBools)
          entry.task.snpVec := newSnpVec
          entry.state.w_snpRsp := !Cat(newSnpVec).orR
        }
      }
    }
  }

  /* Issue */
  issueArb.io.in.zip(buffer).foreach { case (in, e) =>
    val waitSnpRsp = !e.task.replSnp || e.task.replSnp && e.state.w_snpRsp
    in.valid := e.valid && e.state.w_datRsp && !e.state.s_refill && waitSnpRsp
    in.bits := e.task
  }
  issueArb.io.out.ready := true.B
  when(io.task.fire) {
    val entry = buffer(issueArb.io.chosen)
    entry.state.s_refill := true.B
  }

  io.task.valid := issueArb.io.out.valid
  io.task.bits := issueArb.io.out.bits

  /* Data read */
  val ridReg = RegEnable(io.read.bits.id, 0.U(log2Ceil(mshrs.refill).W), io.read.valid)
  io.data := buffer(ridReg).data

  /* Dealloc */
  buffer.foreach {e =>
    val isStashRefill = e.task.chiOpcode === StashOnceShared
    val cancel = e.valid && !isStashRefill && !e.state.w_datRsp && !e.isWrite &&
      e.state.w_snpRsp && !e.beatValids.asUInt.orR
    when(cancel) {
      e.valid := false.B
    }
  }

  when(io.read.valid) {
    val entry = buffer(io.read.bits.id)
    entry.valid := false.B
    entry.state.w_datRsp := false.B
  }

  /* block info */
  io.refillInfo.zipWithIndex.foreach { case (m, i) =>
    m.valid := buffer(i).valid
    m.bits.tag := buffer(i).task.tag
    m.bits.set := buffer(i).task.set
    m.bits.opcode := buffer(i).task.chiOpcode
    m.bits.reqID := buffer(i).task.reqID
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs.refill)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "RefillBuf Leak(id: %d)", i.U)
    }
  }

}
