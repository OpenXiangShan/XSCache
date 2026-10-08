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
import xscache.chi.HasCHIOpcodes

class SnoopEntry(implicit p: Parameters) extends TaskEntry {
  val ready  = Bool()
  // TXSNP may finish before the target RN returns SnpResp/SnpRespData.  Keep
  // this context visible to RequestArb throughout that response window.
  val issued = Bool()
  val waitID = UInt(TXNID_WIDTH.W) // Indicates which CompAck the task needs to wait for to wake itself up
}

class SnoopUnit(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    /* receive snoop requests from mainpipe */
    val in = Flipped(ValidIO(new Task()))
  
    /* send snoop task to upstream TXSNP channel */
    val out = DecoupledIO(new Task())

    /* block info from ResponseUnit */
    val respInfo = Flipped(Vec(mshrs.response, ValidIO(new ResponseInfo())))

    /* CompAck from upstream RXRSP channel */
    val ack = Flipped(ValidIO(new Resp()))

    /* Snoop completion from upstream RXRSP/RXDAT channels */
    val snpRsp = Flipped(ValidIO(new Resp()))
    val snpData = Flipped(ValidIO(new RespWithData()))

    /* snoop buffers info */
    val snpInfo = Vec(mshrs.snoop, ValidIO(new BlockInfo()))
  })

  val in  = io.in
  val out = io.out
  val ack = io.ack

  val snpRsp = io.snpRsp
  val snpData = io.snpData

  /* Data Structure */
  val buffer   = RegInit(VecInit(Seq.fill(mshrs.snoop)(0.U.asTypeOf(new SnoopEntry()))))
  val issueArb = Module(new FastArbiter(new Task(), mshrs.snoop))

  val full = Cat(buffer.map(_.valid)).andR
  val arbValid = issueArb.io.out.valid

  /* Enchantment */
  def sameAddr(a: Task, b: ResponseInfo): Bool = Cat(a.tag, a.set) === Cat(b.tag, b.set)
  def snpConflictMask(a: Task): UInt = VecInit(io.respInfo.map(s =>
    s.valid && a.replSnp && sameAddr(a, s.bits) && !s.bits.w_compack &&
    (s.bits.opcode === ReadNotSharedDirty || s.bits.opcode === ReadUnique || s.bits.opcode === MakeUnique)
  )).asUInt
  def snpConflict(a: Task): Bool = snpConflictMask(a).orR

  /* Alloc */
  /**
    * A snoop caused by a replacement may be blocked if it is preceded by a
    * snoop with the same target address triggered by a Read/Dataless request
    */
  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  // Do not bypass the buffer: a direct TXSNP flow would have no entry to
  // reserve the address while its SnpResp is still in flight.
  val alloc = !full && in.valid
  when(alloc) {
    val entry = buffer(insertIdx)
    val conflictIdx = PriorityEncoder(snpConflictMask(in.bits))
    entry.valid := true.B
    entry.ready := !snpConflict(in.bits)
    entry.issued := false.B
    entry.task := in.bits
    entry.waitID := io.respInfo(conflictIdx).bits.reqID
  }
  assert(!full || !in.valid, "SnoopBuf overflow")

  /* Update ready */
  when(ack.valid) {
    val update_vec = buffer.map(e =>
      e.valid && !e.issued && !e.ready && ack.bits.opcode === CompAck && ack.bits.txnID === e.waitID
    )
    assert(PopCount(update_vec) < 2.U, "Snoop task repeated")
    val canUpdate = Cat(update_vec).orR
    val update_id = PriorityEncoder(update_vec)
    when(canUpdate) {
      val entry = buffer(update_id)
      entry.ready := true.B
    }
  }

  /* Issue */
  // Issued entries stay allocated until every selected RN has returned its
  // snoop response; only unissued entries are candidates for TXSNP.
  issueArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready && !e.issued
    in.bits := e.task
  }
  issueArb.io.out.ready := out.ready
  out.valid := arbValid
  out.bits := issueArb.io.out.bits

  /* Mark issued, then retain the address until SnpResp/SnpRespData. */
  when(out.fire && arbValid) {
    val entry = buffer(issueArb.io.chosen)
    entry.ready := false.B
    entry.issued := true.B
    when(!entry.task.snpVec.asUInt.orR) {
      entry.valid := false.B
      entry.issued := false.B
    }
  }

  val lastSnpDataID = (beatBytes * (beatSize - 1) * 8).U(
    log2Ceil(blockBytes * 8) - 1,
    log2Ceil(blockBytes * 8) - 2
  )
  buffer.foreach { entry =>
    val rspMatch = snpRsp.valid && snpRsp.bits.opcode === SnpResp &&
      entry.valid && entry.issued && entry.task.txnID === snpRsp.bits.txnID
    val dataMatch = snpData.valid && isSnpRespDataX(snpData.bits.opcode) &&
      snpData.bits.dataID === lastSnpDataID && entry.valid && entry.issued &&
        entry.task.txnID === snpData.bits.txnID
    val rspOH = UIntToOH(snpRsp.bits.srcID)(numRNs - 1, 0)
    val dataOH = UIntToOH(snpData.bits.srcID)(numRNs - 1, 0)
    val completedOH = Mux(rspMatch, rspOH, 0.U(numRNs.W)) |
      Mux(dataMatch, dataOH, 0.U(numRNs.W))
    val remainingOH = entry.task.snpVec.asUInt & ~completedOH
    when(completedOH.orR) {
      entry.task.snpVec := VecInit(remainingOH.asBools)
      when(!remainingOH.orR) {
        entry.valid := false.B
        entry.ready := false.B
        entry.issued := false.B
      }
    }
  }

  /* block info */
  io.snpInfo.zipWithIndex.foreach { case (m, i) =>
    m.valid := buffer(i).valid
    m.bits.tag := buffer(i).task.tag
    m.bits.set := buffer(i).task.set
    m.bits.opcode := buffer(i).task.chiOpcode
    m.bits.reqID := buffer(i).task.txnID
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(mshrs.snoop)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "SnoopBuf Leak(id: %d)", i.U)
    }
  }

}
