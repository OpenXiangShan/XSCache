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
import xscache.chi.HasCHIOpcodes

class RequestBuffer(entries: Int = 8)(implicit p: Parameters) extends LLCModule with HasCHIOpcodes {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new Task()))
    val out = DecoupledIO(new Task())
    val candidates = Output(Vec(entries, Valid(new Task())))
    val eligible = Input(Vec(entries, Bool()))
  })

  /* Data Structure */
  val buffer = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(Valid(new Task())))))
  io.candidates := buffer

  val in   = io.in
  val out  = io.out
  val full = Cat(buffer.map(_.valid)).andR

  /* Alloc */
  in.ready := !full

  val insertIdx = PriorityEncoder(buffer.map(!_.valid))
  val alloc = in.fire
  when(alloc) {
    val entry = buffer(insertIdx)
    entry.valid := true.B
    entry.bits := in.bits
  }

  /* Age-based issue arbitration */
  // For each pair i < j, ageOlder(i)(j) means entry i was enqueued first.
  // Allocating a new request only changes the relations in its matrix row/column.
  val agePairCount = entries * (entries - 1) / 2
  val ageOlderUpper = RegInit(VecInit(Seq.fill(agePairCount)(false.B)))
  def agePairIndex(i: Int, j: Int): Int = {
    require(i < j)
    i * (2 * entries - i - 1) / 2 + (j - i - 1)
  }
  def isOlder(i: Int, j: Int): Bool = {
    if (i == j) true.B
    else if (i < j) ageOlderUpper(agePairIndex(i, j))
    else !ageOlderUpper(agePairIndex(j, i))
  }

  for (i <- 0 until entries; j <- i + 1 until entries) {
    val pairIndex = agePairIndex(i, j)
    when(alloc && insertIdx === i.U && buffer(j).valid) {
      ageOlderUpper(pairIndex) := false.B
    }.elsewhen(alloc && insertIdx === j.U && buffer(i).valid) {
      ageOlderUpper(pairIndex) := true.B
    }
  }

  val issueValid = VecInit(buffer.zip(io.eligible).map { case (entry, eligible) =>
    entry.valid && eligible
  })
  val oldestEligible = VecInit((0 until entries).map { i =>
    issueValid(i) && (0 until entries).filter(_ != i).map { j =>
      !issueValid(j) || isOlder(i, j)
    }.reduce(_ && _)
  })
  val anyEligible = issueValid.asUInt.orR
  assert(!anyEligible || PopCount(oldestEligible) === 1.U,
    "RequestBuffer age matrix must select exactly one oldest eligible entry")

  out.valid := anyEligible
  out.bits := Mux1H(oldestEligible, buffer.map(_.bits))

  /* Dealloc */
  when(out.fire) {
    val entry = buffer(OHToUInt(oldestEligible))
    entry.valid := false.B
  }

  /* Performance Counter */
  if(cacheParams.enablePerf) {
    val bufferTimer = RegInit(VecInit(Seq.fill(entries)(0.U(16.W))))
    buffer.zip(bufferTimer).zipWithIndex.map { case ((e, t), i) =>
        when(e.valid) { t := t + 1.U }
        when(RegNext(e.valid, false.B) && !e.valid) { t := 0.U }
        assert(t < timeoutThreshold.U, "ReqBuf Leak(id: %d)", i.U)
    }
  }

}
