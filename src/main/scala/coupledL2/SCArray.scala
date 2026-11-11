// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package xscache.coupledL2

import chisel3._
import chisel3.util._

class SCArrayIO(params: SDBPParams) extends Bundle {
  val query = Flipped(Valid(Vec(3, UInt(log2Ceil(params.predictorEntries).W))))
  val response = Valid(Vec(3, UInt(2.W)))
  val training = Flipped(Valid(new SDBPTraining(params.predictorEntries)))
}

/** Register predictor. An s1 query snapshots counters before that cycle's s3 training. */
class SCArray(params: SDBPParams) extends Module {
  val io = IO(new SCArrayIO(params))
  val counters = Seq.fill(3)(RegInit(VecInit(Seq.fill(params.predictorEntries)(0.U(2.W)))))
  io.response.valid := RegNext(io.query.valid, false.B)
  for (table <- 0 until 3) {
    io.response.bits(table) := RegEnable(counters(table)(io.query.bits(table)), io.query.valid)
    when(io.training.valid) {
      val index = io.training.bits.indices(table)
      // Training reads the current counter for the old signature, independently
      // of the current request's prediction snapshot.
      counters(table)(index) := SDBPHash.train(counters(table)(index), io.training.bits.dead, table)
    }
  }
}
