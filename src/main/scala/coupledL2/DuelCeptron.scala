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

class DuelCeptronIndices extends Bundle {
  val pc = UInt(8.W)
  val page = UInt(8.W)
  val pcOffset = UInt(9.W)
}

class DuelCeptronContext extends Bundle {
  val predictBrrip = Bool()
  val indices = new DuelCeptronIndices
}

private[coupledL2] class DuelCeptron(pcBits: Int, pageBits: Int, offsetBits: Int) extends Module {
  val io = IO(new Bundle {
    val demand = Input(Bool())
    val acquire = Input(Bool())
    val pc = Input(UInt(pcBits.W))
    val page = Input(UInt(pageBits.W))
    val loadOffset = Input(UInt(offsetBits.W))
    val prediction = Output(Valid(new DuelCeptronContext))
    val train = Input(Valid(new DuelCeptronIndices))
    val trainBrripLeader = Input(Bool())
  })

  private val pcWeights = RegInit(VecInit(Seq.fill(256)(0.S(9.W))))
  private val pageWeights = RegInit(VecInit(Seq.fill(256)(0.S(7.W))))
  private val pcOffsetWeights = RegInit(VecInit(Seq.fill(512)(0.S(7.W))))
  private val lastPage = RegInit(0.U(pageBits.W))
  private val prevPage = RegInit(0.U(pageBits.W))

  private def foldIndex(value: UInt, indexBits: Int): UInt = {
    val width = value.getWidth
    val padded = value.pad((width + indexBits - 1) / indexBits * indexBits)
    (0 until width by indexBits).map(bit => padded(bit + indexBits - 1, bit)).reduce(_ ^ _)
  }

  when (io.demand) {
    prevPage := lastPage
    lastPage := io.page
  }

  private val indices = Wire(new DuelCeptronIndices)
  indices.pc := foldIndex(io.pc >> 12, indices.pc.getWidth)
  indices.page := foldIndex(prevPage ^ lastPage ^ io.page, indices.page.getWidth)
  indices.pcOffset := foldIndex(io.pc ^ io.loadOffset, indices.pcOffset.getWidth)

  private val predict_s1 = io.demand && io.acquire
  private val indices_s2 = RegEnable(indices, predict_s1)
  private val pcWeight_s2 = RegEnable(pcWeights(indices.pc), predict_s1)
  private val pageWeight_s2 = RegEnable(pageWeights(indices.page), predict_s1)
  private val pcOffsetWeight_s2 = RegEnable(pcOffsetWeights(indices.pcOffset), predict_s1)
  private val predict_s2 = RegNext(predict_s1, false.B)
  io.prediction.valid := RegNext(predict_s2, false.B)
  io.prediction.bits.indices := RegEnable(indices_s2, predict_s2)
  private val score_s2 = (pcWeight_s2 +& pageWeight_s2) +& pcOffsetWeight_s2
  io.prediction.bits.predictBrrip := RegEnable(score_s2 <= 0.S, predict_s2)

  private def train(weights: Vec[SInt], index: UInt): Unit = {
    val weight = weights(index)
    val width = weight.getWidth
    val max = ((1 << (width - 1)) - 1).S(width.W)
    val min = (-(1 << (width - 1))).S(width.W)
    when (io.trainBrripLeader && weight =/= max) {
      weight := weight + 1.S
    }.elsewhen (!io.trainBrripLeader && weight =/= min) {
      weight := weight - 1.S
    }
  }

  when (io.train.valid) {
    train(pcWeights, io.train.bits.pc)
    train(pageWeights, io.train.bits.page)
    train(pcOffsetWeights, io.train.bits.pcOffset)
  }
}
