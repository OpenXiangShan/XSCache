// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package xscache.coupledL2

import chisel3._
import chisel3.util._

case class L1ICopyParams(sets: Int, ways: Int, blockBytes: Int = 64) {
  require(isPow2(sets) && sets >= 2 && ways > 0 && isPow2(blockBytes))
  val setBits = log2Ceil(sets)
  val blockBits = 64 - log2Ceil(blockBytes)
  val physicalIndexBits = math.min(setBits, 12 - log2Ceil(blockBytes))
  require(physicalIndexBits > 0)
}

/** Lossless, unbuffered events from the actual ICache tag/valid write ports. */
class L1ICopyUpdate(geometry: L1ICopyParams) extends Bundle {
  val write = Valid(new Bundle {
    val set = UInt(geometry.setBits.W)
    val wayMask = UInt(geometry.ways.W)
    val block = UInt(geometry.blockBits.W)
  })
  val invalidate = Vec(2, Valid(new Bundle {
    val set = UInt(geometry.setBits.W)
    val wayMask = UInt(geometry.ways.W)
  }))
  val flushAll = Bool()
}

class L1ICopyTracker(geometry: L1ICopyParams, queryPorts: Int) extends Module {
  val io = IO(new Bundle {
    val update = Input(new L1ICopyUpdate(geometry))
    val query = Input(Vec(queryPorts, UInt(geometry.blockBits.W)))
    val present = Output(Vec(queryPorts, Bool()))
  })
  val valid = RegInit(VecInit(Seq.fill(geometry.sets)(0.U(geometry.ways.W))))
  val blocks = Reg(Vec(geometry.sets, Vec(geometry.ways, UInt(geometry.blockBits.W))))
  val nextValid = WireInit(valid)
  when(io.update.write.valid) {
    nextValid(io.update.write.bits.set) := valid(io.update.write.bits.set) | io.update.write.bits.wayMask
    for (way <- 0 until geometry.ways) {
      when(io.update.write.bits.wayMask(way)) {
        blocks(io.update.write.bits.set)(way) := io.update.write.bits.block
      }
    }
  }
  // Match ICacheMetaInterleavedBank priority exactly, including a flush and
  // installation in the same virtual set: the flush overrides the whole row.
  for (invalidation <- io.update.invalidate) {
    when(invalidation.valid) {
      nextValid(invalidation.bits.set) := valid(invalidation.bits.set) & ~invalidation.bits.wayMask
    }
  }
  when(io.update.flushAll) { nextValid := 0.U.asTypeOf(nextValid) }
  valid := nextValid
  io.present := VecInit(io.query.map { block =>
    // Physical tags include the virtual alias bits above the page offset.
    // Search all aliased virtual sets, rather than indexing by physical set.
    val candidates = (0 until (1 << (geometry.setBits - geometry.physicalIndexBits))).flatMap { alias =>
      val low = block(geometry.physicalIndexBits - 1, 0)
      val set = if (geometry.setBits == geometry.physicalIndexBits) low
        else Cat(alias.U((geometry.setBits - geometry.physicalIndexBits).W), low)
      (0 until geometry.ways).map { way =>
        val writing = io.update.write.valid && io.update.write.bits.set === set && io.update.write.bits.wayMask(way)
        val tag = Mux(writing, io.update.write.bits.block, blocks(set)(way))
        // Forward same-edge residency changes; there is no pending event queue.
        nextValid(set)(way) && tag === block
      }
    }
    candidates.reduce(_ || _)
  })
}
