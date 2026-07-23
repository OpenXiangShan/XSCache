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

package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{MemReqSource, XSPerfAccumulate}
import xscache.chi.{CHIREQ, CHIRSP, HasCHIOpcodes, MPAM, MemAttr, OrderEncodings, SAM}
import xscache.coupledL2.PrefetchRecv
import xscache.coupledL2.utils.OverwriteQueue

class StashPrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  val addr = UInt(fullAddressBits.W)
  val pfSource = UInt(MemReqSource.reqSourceBits.W)
}

class StashPrefetchEntry(implicit p: Parameters) extends PrefetchModule with HasCHIOpcodes {
  val io = IO(new Bundle {
    val alloc = Flipped(DecoupledIO(new StashPrefetchReq))
    val txreq = DecoupledIO(new CHIREQ)
    val rxrsp = Flipped(ValidIO(new CHIRSP))
    val id = Input(UInt(TXNID_WIDTH.W))
    val waitResp = Output(Bool())
  })

  val s_invalid :: s_sendReq :: s_waitResp :: Nil = Enum(3)
  val state = RegInit(s_invalid)
  val req = Reg(new StashPrefetchReq)

  io.alloc.ready := state === s_invalid
  when (io.alloc.fire) {
    req := io.alloc.bits
    state := s_sendReq
  }.elsewhen (io.txreq.fire) {
    state := s_waitResp
  }.elsewhen (io.rxrsp.valid) {
    state := s_invalid
  }

  assert(!io.rxrsp.valid || io.rxrsp.bits.opcode =/= RetryAck, "Stash prefetch does not support CHI retry")

  if(cacheParams.enablePerf) {
    val timer = RegInit(0.U(16.W))
    when(state === s_invalid) {
      timer := 0.U
    }.otherwise {
      timer := timer + 1.U
    }
    assert(state === s_invalid || timer < 20000.U, "StashPrefetchEntry Leak(id: %d)", io.id)
  }

  io.waitResp := state === s_waitResp
  io.txreq.valid := state === s_sendReq
  io.txreq.bits := 0.U.asTypeOf(new CHIREQ)
  io.txreq.bits.qos := 0.U(QOS_WIDTH.W)
  io.txreq.bits.tgtID := SAM(sam).lookup(req.addr)
  io.txreq.bits.srcID := 0.U
  io.txreq.bits.txnID := io.id
  io.txreq.bits.opcode := StashOnceShared
  io.txreq.bits.size := log2Ceil(blockBytes).U(SIZE_WIDTH.W)
  io.txreq.bits.addr := req.addr
  io.txreq.bits.ns := enableNS.B
  io.txreq.bits.allowRetry := false.B
  io.txreq.bits.pCrdType := 0.U
  io.txreq.bits.expCompAck := false.B
  io.txreq.bits.likelyshared := false.B
  io.txreq.bits.snpAttr := true.B
  io.txreq.bits.order := OrderEncodings.None
  io.txreq.bits.memAttr := MemAttr(
    cacheable = true.B,
    allocate = true.B,
    device = false.B,
    ewa = true.B
  )
  io.txreq.bits.stashNIDValid := false.B
  io.txreq.bits.stashNID := 0.U
  io.txreq.bits.returnTxnID := 0.U
  io.txreq.bits.snoopMe := false.B
  io.txreq.bits.mpam.foreach(_ := MPAM(io.txreq.bits.ns))
}

class StashPrefetcher(implicit p: Parameters) extends PrefetchModule with HasCHIOpcodes {
  private val stashPrefetchEntryCount = 16
  private val stashPrefetchIdBits = TXNID_WIDTH - bankBits - 2

  require(stashPrefetchEntryCount > 0)
  require(stashPrefetchIdBits > 0)
  require(stashPrefetchEntryCount <= (1 << stashPrefetchIdBits))

  val io = IO(new Bundle {
    val recv = Input(new PrefetchRecv)
    val txreq = DecoupledIO(new CHIREQ)
    val rxrsp = Flipped(DecoupledIO(new CHIRSP))
  })

  val l3PftQueue = Module(new OverwriteQueue(
    gen = new StashPrefetchReq,
    entries = stashPrefetchEntryCount,
    hasFlow = true
  ))
  l3PftQueue.io.enq.valid := io.recv.addr_valid && io.recv.pf_en
  l3PftQueue.io.enq.bits.addr := io.recv.addr(fullAddressBits - 1, 0)
  l3PftQueue.io.enq.bits.pfSource := io.recv.pf_source

  val stashPrefetchEntries = Seq.tabulate(stashPrefetchEntryCount) { i =>
    val entry = Module(new StashPrefetchEntry)
    entry.io.id := i.U
    entry
  }

  val allocReadys = VecInit(stashPrefetchEntries.map(_.io.alloc.ready))
  val allocOH = PriorityEncoderOH(allocReadys)
  l3PftQueue.io.deq.ready := allocReadys.asUInt.orR
  stashPrefetchEntries.zipWithIndex.foreach { case (entry, i) =>
    entry.io.alloc.valid := l3PftQueue.io.deq.valid && allocOH(i)
    entry.io.alloc.bits := l3PftQueue.io.deq.bits
    entry.io.rxrsp.valid := io.rxrsp.valid && entry.io.waitResp && io.rxrsp.bits.txnID === i.U
    entry.io.rxrsp.bits := io.rxrsp.bits
  }
  val rxrspHit = VecInit(stashPrefetchEntries.zipWithIndex.map {
    case (entry, i) => entry.io.waitResp && io.rxrsp.bits.txnID === i.U
  }).asUInt.orR
  io.rxrsp.ready := rxrspHit
  assert(!io.rxrsp.valid || rxrspHit, "Stash prefetch received a response with no matching entry")

  fastArb(stashPrefetchEntries.map(_.io.txreq), io.txreq, Some("stash_prefetch_txreq"))

  XSPerfAccumulate("l3_prefetch_recv", io.recv.addr_valid && io.recv.pf_en)
  XSPerfAccumulate("l3_prefetch_queue_fire", l3PftQueue.io.deq.fire)
  XSPerfAccumulate("l3_prefetch_txreq_valid", io.txreq.valid)
  XSPerfAccumulate("l3_prefetch_txreq_blocked", io.txreq.valid && !io.txreq.ready)
  XSPerfAccumulate("l3_prefetch_txreq_fire", io.txreq.fire)
  XSPerfAccumulate("l3_prefetch_rxrsp_fire", io.rxrsp.fire)
}
