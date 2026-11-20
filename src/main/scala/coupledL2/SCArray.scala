// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package xscache.coupledL2

import chisel3._
import chisel3.util._

class SCArrayIO(params: SDBPParams) extends Bundle {
  val query = Flipped(Valid(new Bundle {
    val indices = Vec(3, UInt(log2Ceil(params.predictorEntries).W))
    val source = SDBPSource()
  }))
  val response = Valid(Vec(3, UInt(2.W)))
  val training = Flipped(Valid(new SDBPTraining(params.predictorEntries)))
}

/** Register predictor. An s1 query snapshots counters before that cycle's s3 training. */
class SCArray(params: SDBPParams) extends Module {
  val io = IO(new SCArrayIO(params))
  val sources = SDBPSource.predictorSources(params.splitPrefetchSources)
  val counters = sources.map { _ =>
    Seq.fill(3)(RegInit(VecInit(Seq.fill(params.predictorEntries)(0.U(2.W)))))
  }
  io.response.valid := RegNext(io.query.valid, false.B)
  for (table <- 0 until 3) {
    val snapshot = Mux1H(sources.zip(counters).map { case (source, bank) =>
      (io.query.bits.source === source) -> bank(table)(io.query.bits.indices(table))
    })
    io.response.bits(table) := RegEnable(snapshot, io.query.valid)
    for ((source, bank) <- sources.zip(counters)) {
      when(io.training.valid && io.training.bits.source === source) {
        val index = io.training.bits.indices(table)
        // Read the old signature's current counter independently of the prediction snapshot.
        bank(table)(index) := SDBPHash.train(bank(table)(index), io.training.bits.dead, table)
      }
    }
  }
  assert(!io.query.valid || sources.map(_ === io.query.bits.source).reduce(_ || _))
  assert(!io.training.valid || sources.map(_ === io.training.bits.source).reduce(_ || _))
}
