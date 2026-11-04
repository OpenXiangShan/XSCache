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

    /* block info from MemUnit: WriteNoSnpFull entries still waiting for their write data */
    val memInfo = Flipped(Vec(mshrs.memory, ValidIO(new MemInfo())))

    /* CompAck from upstream RXRSP channel */
    val ack = Flipped(ValidIO(new Resp()))

    /* snoop buffers info */
    val snpInfo = Vec(mshrs.snoop, ValidIO(new BlockInfo()))
  })

  val in  = io.in
  val out = io.out
  val ack = io.ack

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

  /**
    * CHI E.b 2.8.3 / 4.11.2: after sending the completion of a CopyBack (CompDBIDResp to WriteCleanFull),
    * the HN must not snoop the same line until it has received the WriteData. A WriteCleanFull leaves the
    * requester in the snoop filter, so a later snoop-filter replacement can pick that line; hold such a
    * replacement snoop while a MemUnit WriteNoSnpFull for the line still waits for its data.
    * Request snoops are serialised at the RequestArb entrance (blockByMem). Only replacement snoops are held
    * here: a CleanShared/CleanInvalid WriteNoSnpFull waits for the data of its own request snoop.
    * The hold ends when all beats have arrived (w_datRsp), or when MemUnit drops the write on a beat without
    * PassDirty (no dirty data to protect then).
    */
  def cbPending(a: Task): Bool = a.replSnp && VecInit(io.memInfo.map(m =>
    m.valid && m.bits.opcode === WriteNoSnpFull && !m.bits.w_datRsp && Cat(m.bits.tag, m.bits.set) === Cat(a.tag, a.set)
  )).asUInt.orR

  // flow not allowed when arbiter output is valid, or entries might starve
  val canFlow = !snpConflict(in.bits) && !cbPending(in.bits) && !arbValid
  val doFlow = canFlow && out.ready

  /* Alloc */
  /**
    * A snoop caused by a replacement may be blocked if it is preceded by a
    * snoop with the same target address triggered by a Read/Dataless request
    */
  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val alloc = !full && in.valid && !doFlow
  when(alloc) {
    val entry = buffer(insertIdx)
    val conflictIdx = PriorityEncoder(snpConflictMask(in.bits))
    entry.valid := true.B
    entry.ready := !snpConflict(in.bits)
    entry.task := in.bits
    entry.waitID := io.respInfo(conflictIdx).bits.reqID
  }
  assert(!full || !in.valid || doFlow, "SnoopBuf overflow")

  /* Update ready */
  when(ack.valid) {
    val update_vec = buffer.map(e =>
      e.valid && !e.ready && ack.bits.opcode === CompAck && ack.bits.txnID === e.waitID
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
  // once fired at issueArb, it is ok to enter TXSNP without conflict
  issueArb.io.in.zip(buffer).foreach { case (in, e) =>
    in.valid := e.valid && e.ready && !cbPending(e.task)
    in.bits := e.task
  }
  issueArb.io.out.ready := out.ready
  out.valid := in.valid && canFlow || arbValid
  out.bits := Mux(canFlow, in.bits, issueArb.io.out.bits)

  /* Dealloc */
  when(out.fire && arbValid) {
    val entry = buffer(issueArb.io.chosen)
    entry.valid := false.B
    entry.ready := false.B
  }

  /* block info */
  io.snpInfo.zipWithIndex.foreach { case (m, i) =>
    m.valid := buffer(i).valid
    m.bits.tag := buffer(i).task.tag
    m.bits.set := buffer(i).task.set
    m.bits.opcode := buffer(i).task.chiOpcode
    m.bits.reqID := buffer(i).task.reqID
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
