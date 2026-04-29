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

package coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.utils._

class HintQueueEntry(implicit p: Parameters) extends L2Bundle {
  val source = UInt(sourceIdBits.W)
  val opcode = UInt(4.W)
  val isKeyword = Bool()
}

class CustomL1HintIOBundle(implicit p: Parameters) extends L2Bundle {
  // input information
  val mshrHintQInfo = Flipped(ValidIO(new TaskBundle()))
  val sinkCHintQInfo = Flipped(ValidIO(new TaskBundle()))
  val retry_s2 = Input(Bool())

  val s3 = new L2Bundle {
      val task      = Flipped(ValidIO(new TaskBundle()))
      val need_mshr = Input(Bool())
  }

  // output shadow D token stream
  val l1Hint = DecoupledIO(new L2HintShadowToken())
}

// Build a per-slice shadow D stream from early pipeline predictions.
class CustomL1Hint(implicit p: Parameters) extends L2Module {
  val io = IO(new CustomL1HintIOBundle)

  val mshr_s1 = io.mshrHintQInfo.bits
  val sinkC_s1 = io.sinkCHintQInfo.bits
  val task_s3 = io.s3.task
  val mshrReq_s3 = task_s3.bits.mshrTask
  val need_mshr_s3 = io.s3.need_mshr

  def isRelease(t: TaskBundle): Bool = t.fromC && (t.opcode === Release || t.opcode === ReleaseData)
  def effOpcode(t: TaskBundle): UInt = Mux(t.mergeA, t.aMergeTask.opcode, t.opcode)
  def effSource(t: TaskBundle): UInt = Mux(t.mergeA, t.aMergeTask.sourceId, t.sourceId)
  def effIsKeyword(t: TaskBundle): Bool = Mux(
    t.mergeA,
    t.aMergeTask.isKeyword.getOrElse(false.B),
    t.isKeyword.getOrElse(false.B)
  )
  def usesSourceD(t: TaskBundle): Bool = t.fromA && (t.opcode =/= AccessAck) && (t.opcode =/= HintAck || t.mergeA)
  def toShadowToken(entry: HintQueueEntry, firstBeat: Bool, beats1: UInt): L2HintShadowToken = {
    val token = Wire(new L2HintShadowToken)
    token.sourceId := entry.source
    token.opcode := entry.opcode
    token.isKeyword := entry.isKeyword
    token.firstBeat := firstBeat
    token.beats1 := beats1
    token
  }

  // ==================== Shadow Event Generation ====================
  // Keep the current branch timing/retry behavior: s1 predictions come from
  // dedicated MSHR / SinkC sideband signals, while s3 contributes channel hits.
  val mshr_dResp_s1 = io.mshrHintQInfo.valid && usesSourceD(mshr_s1)
  val chn_Release_s1 = io.sinkCHintQInfo.valid
  assert(Mux(chn_Release_s1, sinkC_s1.fromC, true.B))
  assert(Mux(chn_Release_s1, sinkC_s1.opcode === Release || sinkC_s1.opcode === ReleaseData, true.B))

  val enqValid_s1 = mshr_dResp_s1 || chn_Release_s1
  val enqBits_s1 = Wire(new HintQueueEntry)
  enqBits_s1.source := Mux(chn_Release_s1, sinkC_s1.sourceId, effSource(mshr_s1))
  enqBits_s1.opcode := Mux(chn_Release_s1, ReleaseAck, effOpcode(mshr_s1))
  enqBits_s1.isKeyword := Mux(chn_Release_s1, sinkC_s1.isKeyword.getOrElse(false.B), effIsKeyword(mshr_s1))

  val chn_dResp_s3 = task_s3.valid && !mshrReq_s3 && !need_mshr_s3 && usesSourceD(task_s3.bits)
  val enqValid_s3 = chn_dResp_s3
  val enqBits_s3 = Wire(new HintQueueEntry)
  enqBits_s3.source := effSource(task_s3.bits)
  enqBits_s3.opcode := effOpcode(task_s3.bits)
  enqBits_s3.isKeyword := effIsKeyword(task_s3.bits)

  // ==================== Shadow Event Queue ====================
  val hintEntries = mshrsAll * 2
  val hintQueue = Module(new Queue(new HintQueueEntry, hintEntries))
  val canFlow_s1 = !hintQueue.io.deq.valid || hintQueue.io.count === 1.U && hintQueue.io.deq.fire
  val flow_s1, drop_s1, enq_s3 = Wire(Decoupled(new HintQueueEntry))

  // noSpaceForSinkReq in GrantBuffer may ensure that these queues will not overflow
  assert(enq_s3.ready || !enq_s3.valid)

  val hint_s1Queue = Module(new Pipeline(new HintQueueEntry))
  hint_s1Queue.io.in.valid := enqValid_s1 && (!canFlow_s1 || !flow_s1.ready)
  hint_s1Queue.io.in.bits := enqBits_s1
  assert(!enqValid_s1 || hint_s1Queue.io.in.ready || flow_s1.ready)

  drop_s1.valid := hint_s1Queue.io.out.valid && !io.retry_s2
  drop_s1.bits := hint_s1Queue.io.out.bits
  hint_s1Queue.io.out.ready := drop_s1.ready || io.retry_s2

  flow_s1.valid := enqValid_s1 && canFlow_s1
  flow_s1.bits := enqBits_s1

  enq_s3.valid := enqValid_s3
  enq_s3.bits := enqBits_s3
  arb(Seq(enq_s3, drop_s1, flow_s1), hintQueue.io.enq, Some("Hint"))

  val shadowTokenBufValid = RegInit(false.B)
  val shadowTokenBuf = RegInit(0.U.asTypeOf(new L2HintShadowToken))
  val blockDeqForRetry = io.retry_s2 && !hint_s1Queue.io.out.valid
  val firstToken = toShadowToken(
    hintQueue.io.deq.bits,
    firstBeat = true.B,
    beats1 = Mux(
      hintQueue.io.deq.bits.opcode(0),
      (beatSize - 1).U(scala.math.max(1, log2Ceil(beatSize)).W),
      0.U
    )
  )
  val secondToken = toShadowToken(
    hintQueue.io.deq.bits,
    firstBeat = false.B,
    beats1 = 0.U
  )

  hintQueue.io.deq.ready := io.l1Hint.ready && !shadowTokenBufValid && !blockDeqForRetry

  when (hintQueue.io.deq.valid && io.l1Hint.ready && !shadowTokenBufValid && !blockDeqForRetry && hintQueue.io.deq.bits.opcode(0)) {
    shadowTokenBufValid := true.B
    shadowTokenBuf := secondToken
  }
  when (shadowTokenBufValid && io.l1Hint.ready) {
    shadowTokenBufValid := false.B
  }

  io.l1Hint.valid := shadowTokenBufValid || (hintQueue.io.deq.valid && !blockDeqForRetry)
  io.l1Hint.bits := Mux(shadowTokenBufValid, shadowTokenBuf, firstToken)
}
