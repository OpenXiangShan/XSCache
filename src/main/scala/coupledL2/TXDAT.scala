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

package xscache.coupledL2

import chisel3._
import chisel3.util._
import utility._
import org.chipsalliance.cde.config.Parameters
import xscache.coupledL2.{TaskWithData, TaskBundle, DSBlock, DSBeat}
import xscache.chi.{CHIDAT, HasCHIOpcodes, RespErrEncodings}
import xscache.chi.CHICohStates._

class TXDATBlockBundle(implicit p: Parameters) extends TXBlockBundle {
  val blockSinkBReqEntrance = Bool()

  override def apply() = 0.U.asTypeOf(this)
}

class TXDAT(implicit p: Parameters) extends CoupledL2Module with HasCHIOpcodes {
  val io = IO(new Bundle() {
    val in = Flipped(DecoupledIO(new TaskWithData()))
    // Direct ReplaceMSHR CopyBackWrData path (victim writeback data).  The
    // mainpipe input keeps strict priority; the direct input is gated by
    // queue pressure with margin for its in-flight read.
    val mshrIn = Flipped(DecoupledIO(new TaskWithData()))
    val directSpace = Output(Bool())
    val out = DecoupledIO(new CHIDAT())

    val pipeStatusVec = Flipped(Vec(5, ValidIO(new PipeStatusWithCHI)))
    val toReqArb = Output(new TXDATBlockBundle)
    // Pulse only after the final data beat leaves the TXDAT channel.
    val done = ValidIO(new TaskBundle)
  })

  assert(!io.in.valid || io.in.bits.task.toTXDAT, "txChannel is wrong for TXDAT")
  assert(!io.in.valid || io.in.ready, "TXDAT should never be full")
  assert(!io.mshrIn.valid || io.mshrIn.bits.task.toTXDAT, "txChannel is wrong for direct TXDAT")
  require(chiOpt.isDefined)
  require(beatBytes * 8 == DATA_WIDTH)

  // Worst-case data pressure in the multi-context MSHR design:
  //   normal cbwrdata (Get-on-TRUNK/CMO paths) <= mshrsAll
  //   ReplaceMSHR CopyBackWrData + SnoopMSHR SnpRespData <= mshrsAll
  //     (S/R are mutually exclusive per slot)
  // The pre-split bound of mshrsAll no longer holds; see TXREQ for details.
  // Use customized SRAM: dual_port, max 256bits:
  val queue = Module(new Queue(new TaskBundle(), entries = 2 * mshrsAll, flow = true))
  val queueData0 = Module(new Queue(new DSBeat(), entries = 2 * mshrsAll, flow = true))
  val queueData1 = Module(new Queue(new DSBeat(), entries = 2 * mshrsAll, flow = true))
  val enqData = Mux(io.in.valid, io.in.bits.data, io.mshrIn.bits.data).asTypeOf(Vec(beatSize, new DSBeat))

  // Back pressure logic from TXDAT
  val queueCnt = queue.io.count
  // TODO: this may be imprecise, review this later
  val pipeStatus_s1_s5 = io.pipeStatusVec
  val pipeStatus_s1_s3 = pipeStatus_s1_s5.take(3)
  val pipeStatus_s2_s3 = pipeStatus_s1_s3.tail
  val pipeStatus_s4_s5 = pipeStatus_s1_s5.drop(3)
  // inflightCnt equals the number of reqs on s2~s5 that may flow into TXDAT soon, plus queueCnt.
  // The calculation of inflightCnt might be imprecise and leads to false positive back pressue.
  val inflightCnt = PopCount(Cat(pipeStatus_s4_s5.map(s => s.valid && s.bits.toTXDAT && (s.bits.fromB || s.bits.mshrTask)))) +
    PopCount(Cat(pipeStatus_s2_s3.map(s => s.valid && Mux(s.bits.mshrTask, s.bits.toTXDAT, s.bits.fromB)))) +
    queueCnt

  assert(inflightCnt <= (2 * mshrsAll + 4).U, "in-flight overflow at TXDAT")

  val noSpaceForSinkBReq = inflightCnt >= (2 * mshrsAll).U
  val noSpaceForMSHRReq = inflightCnt >= (2 * mshrsAll - 2).U

  io.toReqArb.blockSinkBReqEntrance := noSpaceForSinkBReq
  io.toReqArb.blockMSHRReqEntrance := noSpaceForMSHRReq

  // The mainpipe input keeps strict priority and its "never full" invariant;
  // the direct copyback input is admitted only with margin (the direct path
  // holds at most one request between ReleaseBuf read and enqueue, which the
  // 4-entry margin covers; see MSHRCtl).
  val noSpaceForDirect = inflightCnt >= (2 * mshrsAll - 4).U
  io.directSpace := !noSpaceForDirect
  queue.io.enq.valid := io.in.valid || io.mshrIn.valid && !noSpaceForDirect
  queue.io.enq.bits := Mux(io.in.valid, io.in.bits.task, io.mshrIn.bits.task)
  io.in.ready := queue.io.enq.ready
  io.mshrIn.ready := !io.in.valid && !noSpaceForDirect && queue.io.enq.ready
  queueData0.io.enq.valid := queue.io.enq.valid
  queueData0.io.enq.bits := enqData(0)
  queueData1.io.enq.valid := queue.io.enq.valid
  queueData1.io.enq.bits := enqData(1)

  val beatValids = RegInit(VecInit(Seq.fill(beatSize)(false.B)))
  val taskValid = beatValids.asUInt.orR
  val taskR = RegInit(0.U.asTypeOf(new TaskWithData))

  val dequeueReady = !taskValid // TODO: this may introduce bubble?
  queue.io.deq.ready := dequeueReady
  queueData0.io.deq.ready := dequeueReady
  queueData1.io.deq.ready := dequeueReady
  when (queue.io.deq.fire) {
    beatValids.foreach(_ := true.B)
    taskR.task := queue.io.deq.bits
    taskR.data := Cat(queueData1.io.deq.bits.data, queueData0.io.deq.bits.data).asTypeOf(new DSBlock)
  }

  val data = taskR.data.data
  val beatsOH = beatValids.asUInt
  val (beat, next_beatsOH) = getBeat(data, beatsOH)

  io.out.valid := taskValid
  io.out.bits := toCHIDATBundle(taskR.task, beat, beatsOH)
  // for TXDAT, WriteBack & SnpX will not allow CompData with NDERR
  io.out.bits.respErr := Mux(taskR.task.corrupt, RespErrEncodings.DERR, RespErrEncodings.OK)

  when (io.out.fire) {
    beatValids := VecInit(next_beatsOH.asBools)
  }
  io.done.valid := io.out.fire && !next_beatsOH.orR
  io.done.bits := taskR.task

  def getBeat(data: UInt, beatsOH: UInt): (UInt, UInt) = {
    // get one beat from data according to beatsOH
    require(data.getWidth == (blockBytes * 8))
    require(beatsOH.getWidth == beatSize)
    // next beat
    val next_beat = ParallelPriorityMux(beatsOH, data.asTypeOf(Vec(beatSize, UInt((beatBytes * 8).W))))
    val selOH = PriorityEncoderOH(beatsOH)
    // remaining beats that haven't been sent out
    val next_beatsOH = beatsOH & ~selOH
    (next_beat, next_beatsOH)
  }

  def deassertData(data: UInt, mask: UInt): UInt = {
    require(data.getWidth == mask.getWidth * 8)
    FillInterleaved(8, mask) & data
  }

  def toCHIDATBundle(task: TaskBundle, beat: UInt, beatsOH: UInt): CHIDAT = {
    val dat = WireInit(0.U.asTypeOf(new CHIDAT()))

    // width parameters and width check
    require(beat.getWidth == dat.data.getWidth)
    val beatOffsetWidth = log2Up(beatBytes)

    val deassertBE = task.chiOpcode.get === CopyBackWrData && task.resp.get === I ||
      task.chiOpcode.get === WriteDataCancel
    val be = Fill(BE_WIDTH, !deassertBE)

    val data = deassertData(beat, be)
    val dataCheck = if (enableDataCheck) {
      dataCheckMethod match {
        case 1 => VecInit((0 until DATACHECK_WIDTH).map(i => data(8 * (i + 1) - 1, 8 * i).xorR ^ true.B)).asUInt
        case 2 =>
          val code = new SECDEDCode
          VecInit((0 until DATACHECK_WIDTH).map(i => code.encode(data(8 * (i + 1) - 1, 8 * i)))).asUInt
        case _ => 0.U(DATACHECK_WIDTH.W)
      }
    } else {
      DontCare
    }

    dat.tgtID := task.tgtID.get
    dat.srcID := task.srcID.get
    dat.txnID := task.txnID.get
    dat.homeNID := task.homeNID.get
    dat.dbID := task.dbID.get
    dat.opcode := task.chiOpcode.get
    // The CCID field must match the value of Addr[5:4] of the original request.
    dat.ccID := task.off >> ChunkOffsetWidth
    // The DataID field value must be set to Addr[5:4] because the DataID field represents Addr[5:4] of the lowest
    // addressed byte within the packet.
    // dat.dataID := ParallelPriorityMux(beatsOH.asBools.zipWithIndex.map(x => (x._1, (x._2 << beatOffsetWidth).U(5, 4))))
    dat.dataID := ParallelPriorityMux(
      beatsOH,
      List.tabulate(beatSize)(i => (i << (beatOffsetWidth - ChunkOffsetWidth)).U)
    )
    dat.be := be
    dat.data := data
    dat.resp := task.resp.get
    // dat.fwdState := task.fwdState.get
    dat.setFwdState(task.fwdState.get)
    dat.traceTag := task.traceTag.get
    dat.dataCheck match {
      case Some(x) =>
        x := dataCheck
      case None =>
    }
    dat.poison match {
      case Some(x) =>
        x := Fill(POISON_WIDTH, task.corrupt)
      case None =>
    }

    dat
  }

}
