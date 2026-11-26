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
import xscache.coupledL2.utils.TrueLRU

private[coupledL2] object DrripSampleSets {
  def matches(set: UInt, setBits: Int): (Bool, Bool) = {
    val half = setBits / 2
    val upper = set(setBits - 1, half)
    val lower = set(setBits - half - 1, 0)
    (upper === lower, upper === ~lower)
  }
}

class FdpPollutionSampler(setBits: Int, tagBits: Int) extends Module {
  private val bufferWays = 6
  private val sampleSets = 1 << (setBits / 2 + 1)
  private val lru = new TrueLRU(bufferWays)

  class Block extends Bundle {
    val set = UInt(setBits.W)
    val tag = UInt(tagBits.W)
  }

  val io = IO(new Bundle {
    val demandMiss = Flipped(Valid(new Block))
    val fill = Flipped(Valid(new Block))
    val evictedDemand = Flipped(Valid(new Block))
    val sampledDemandMiss = Output(Bool())
    val pollutionMiss = Output(Bool())
  })

  def sampleSet(req: Valid[Block]): (Bool, UInt) = {
    if (setBits <= 1) {
      (req.valid, req.bits.set.pad(1))
    } else {
      val (same, complement) = DrripSampleSets.matches(req.bits.set, setBits)
      // For an odd width, the overlapping bit is already determined by the match and low bits.
      val index = Cat(complement, req.bits.set(setBits / 2 - 1, 0))
      (req.valid && (same || complement), index)
    }
  }

  val tags = Reg(Vec(sampleSets, Vec(bufferWays, UInt(tagBits.W))))
  val valid = RegInit(VecInit(Seq.fill(sampleSets)(0.U(bufferWays.W))))
  val recency = RegInit(VecInit(Seq.fill(sampleSets)(0.U(lru.nBits.W))))

  def matchingWays(index: UInt, tag: UInt): UInt =
    VecInit(tags(index).map(_ === tag)).asUInt & valid(index)

  val (sampledMiss, missIndex) = sampleSet(io.demandMiss)
  val (sampledFill, fillIndex) = sampleSet(io.fill)
  val (sampledVictim, victimIndex) = sampleSet(io.evictedDemand)
  val consumed = Mux(sampledMiss, matchingWays(missIndex, io.demandMiss.bits.tag), 0.U)
  val filled = Mux(sampledFill, matchingWays(fillIndex, io.fill.bits.tag), 0.U)

  io.sampledDemandMiss := sampledMiss
  io.pollutionMiss := consumed.orR

  val freedWays = Mux(missIndex === victimIndex, consumed, 0.U) |
    Mux(fillIndex === victimIndex, filled, 0.U)
  val freeWays = ~valid(victimIndex) | freedWays
  val existing = matchingWays(victimIndex, io.evictedDemand.bits.tag)
  val victimRecency = recency(victimIndex)
  val insertWay = Mux(existing.orR, OHToUInt(existing),
    Mux(freeWays.orR, PriorityEncoder(freeWays), lru.get_replace_way(victimRecency)))
  val insertOH = UIntToOH(insertWay, bufferWays)

  when(sampledVictim) {
    tags(victimIndex)(insertWay) := io.evictedDemand.bits.tag
    recency(victimIndex) := lru.get_next_state(victimRecency, insertWay)
  }
  for (index <- 0 until sampleSets) {
    val cleared = Mux(missIndex === index.U, consumed, 0.U) | Mux(fillIndex === index.U, filled, 0.U)
    val inserted = Mux(sampledVictim && victimIndex === index.U, insertOH, 0.U)
    valid(index) := (valid(index) & ~cleared) | inserted
  }
}
