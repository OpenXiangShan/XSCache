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
import utility.mbist.MbistPipeline
import org.chipsalliance.cde.config.Parameters
import xscache.coupledL2._
import xscache.coupledL2.prefetch.{PrefetchIO, CDPDetectTask, CDPParameters, PfSource}
import utility.MemReqSource
import xscache.chi.{DecoupledPortIO, HasCHIMsgParameters}

class OuterBundle(implicit p: Parameters) extends DecoupledPortIO with BaseOuterBundle

class Slice()(implicit p: Parameters) extends BaseSlice[OuterBundle]
  with HasCoupledL2Parameters
  with HasCHIMsgParameters {

  val io = IO(new BaseSliceIO[OuterBundle] {
    override val out: OuterBundle = new OuterBundle
  })
  val io_pCrd = IO(Vec(mshrsAll, new PCrdQueryBundle))

  val io_cdp_triggers = Option.when(hasCDP) (IO(ValidIO(new CDPDetectTask(dataBits=blockBits))))

  /* Upwards TileLink-related modules */
  val sinkA = Module(new SinkA)
  val sinkC = Module(new SinkC)
  val grantBuf = Module(new GrantBuffer)

  /* Downwards CHI-related modules */
  val txreq = Module(new TXREQ())
  val txdat = Module(new TXDAT())
  val txrsp = Module(new TXRSP())
  val rxsnp = Module(new RXSNP())
  val rxdat = Module(new RXDAT())
  val rxrsp = Module(new RXRSP())

  /* Data path and control path */
  val directory = Module(new Directory())
  val dataStorage = Module(new DataStorage())
  val refillBuf = Module(new MSHRBuffer(wPorts = 4))
  val releaseBuf = Module(new MSHRBuffer(wPorts = 5))

  val reqArb = Module(new RequestArb())
  val mainPipe = Module(new MainPipe())
  val reqBuf = Module(new RequestBuffer())
  val mshrCtl = Module(new MSHRCtl())
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(2, "L2Slice", p(L2ParamKey).hasMbist)
  sinkC.io.msInfo := mshrCtl.io.refillInfo
  sinkC.io.refillBufWriteId := mshrCtl.io.refillBufWriteId
  sinkC.io.snoopRefillBufWriteId := mshrCtl.io.snoopRefillBufWriteId

  grantBuf.io.d_task <> mainPipe.io.toSourceD
  grantBuf.io.fromReqArb.status_s1 := reqArb.io.status_s1
  grantBuf.io.pipeStatusVec := reqArb.io.status_vec ++ mainPipe.io.status_vec_toD

  val status_vec_toTX = reqArb.io.status_vec_toTX ++ mainPipe.io.status_vec_toTX
  txreq.io.pipeReq <> mainPipe.io.toTXREQ
  txreq.io.mshrReq <> mshrCtl.io.toTXREQ
  txreq.io.replacePendingCnt := mshrCtl.io.replaceTxreqPendingCnt
  txreq.io.pipeStatusVec := status_vec_toTX
  txreq.io.sliceId := io.sliceId

  txdat.io.in <> mainPipe.io.toTXDAT
  txdat.io.mshrIn <> mshrCtl.io.toTXDAT
  mshrCtl.io.txdatDirectSpace := txdat.io.directSpace
  txdat.io.pipeStatusVec := status_vec_toTX

  txrsp.io.pipeRsp <> mainPipe.io.toTXRSP
  txrsp.io.mshrRsp <> mshrCtl.io.toTXRSP
  txrsp.io.pipeStatusVec := status_vec_toTX

  rxsnp.io.refillInfo := mshrCtl.io.refillInfo
  rxsnp.io.snoopInfo := mshrCtl.io.snoopInfo
  rxsnp.io.replaceInfo := mshrCtl.io.replaceInfoForSnoop
  rxsnp.io.refillVictimDelegated := mshrCtl.io.refillVictimDelegated
  rxsnp.io.replaceAlloc := mshrCtl.io.replaceAllocForSnoop

  directory.io.read <> reqArb.io.dirRead_s1
  directory.io.metaWReq := mainPipe.io.metaWReq
  directory.io.tagWReq := mainPipe.io.tagWReq
  directory.io.refillInfo := mshrCtl.io.refillInfo
  directory.io.snoopInfo := mshrCtl.io.snoopInfo
  directory.io.replaceInfo := mshrCtl.io.replaceInfo

  dataStorage.io.en := mainPipe.io.toDS.en_s3
  dataStorage.io.req := mainPipe.io.toDS.req_s3
  dataStorage.io.wdata := mainPipe.io.toDS.wdata_s3

  reqArb.io.ATag := reqBuf.io.ATag
  reqArb.io.ASet := reqBuf.io.ASet
  reqArb.io.sinkA <> reqBuf.io.out
  reqArb.io.sinkB <> rxsnp.io.task
  reqArb.io.sinkC <> sinkC.io.task
  reqArb.io.mshrTask <> mshrCtl.io.mshrTask
  reqArb.io.mshrLeaseToken := mshrCtl.io.mshrTaskLeaseToken
  mshrCtl.io.bServiceStart := reqArb.io.bServiceStart
  mshrCtl.io.sidecarReservationHold := reqArb.io.sidecarReservationHold
  mshrCtl.io.contextReservation := reqArb.io.contextReservation
  txdat.io.reservedB := reqArb.io.reservedB
  txrsp.io.reservedB := reqArb.io.reservedB
  reqArb.io.fromMSHRCtl := mshrCtl.io.toReqArb
  reqArb.io.fromMainPipe := mainPipe.io.toReqArb
  reqArb.io.blockRefillOnlyCommit := mainPipe.io.blockRefillOnlyCommit
  reqArb.io.fromGrantBuffer := grantBuf.io.toReqArb
  reqArb.io.fromTXDAT := txdat.io.toReqArb
  reqArb.io.fromTXRSP := txrsp.io.toReqArb
  reqArb.io.fromTXREQ := txreq.io.toReqArb
  reqArb.io.msInfo := mshrCtl.io.refillInfo

  reqBuf.io.in <> sinkA.io.task
  reqBuf.io.mshrInfo := mshrCtl.io.refillInfo
  reqBuf.io.snoopInfo := mshrCtl.io.snoopInfo
  reqBuf.io.mainPipeBlock := mainPipe.io.toReqBuf
  reqBuf.io.pipeStatusVec(0) := reqArb.io.status_vec(1) // s2 status
  reqBuf.io.pipeStatusVec(1) := mainPipe.io.status_vec_toD(3) // result capture
  reqBuf.io.pipeStatusVec(2) := mainPipe.io.status_vec_toD(0) // S3 apply
  reqBuf.io.s1Entrance := reqArb.io.s1Entrance
  reqBuf.io.taskFromArb_s2 := reqArb.io.taskToPipe_s2

  mainPipe.io.taskFromArb_s2 := reqArb.io.taskToPipe_s2
  mainPipe.io.taskLeaseToken_s2 := reqArb.io.taskLeaseToken_s2
  mainPipe.io.mshrHintQInfo := reqArb.io.mshrHintQInfo
  mainPipe.io.sinkCHintQInfo := reqArb.io.sinkCHintQInfo
  reqArb.io.hint_s1QueueFull := mainPipe.io.hint_s1QueueFull
  mainPipe.io.fromReqArb.status_s1 := reqArb.io.status_s1
  // Complete S3 event: Directory, task (in MainPipe), C payload and both
  // MSHR-buffer returns advance together. No individual hit/retry bypass
  // reaches the owner/allocation logic ahead of the registered transaction.
  val directoryEvent = Wire(new Bundle {
    val result = Valid(new DirResult)
    val replacement = Valid(new ReplacerResult)
    val metaOnHit = new MetaEntry
    val errOnSnp = Bool()
    val wayOH = UInt(cacheParams.ways.W)
    val replWayOH = UInt(cacheParams.ways.W)
    val cmoHitInvalid = Bool()
  })
  directoryEvent.result := directory.io.resp
  directoryEvent.replacement := directory.io.replResp
  directoryEvent.metaOnHit := directory.io.metaOnHit
  directoryEvent.errOnSnp := directory.io.errOnSnp
  directoryEvent.wayOH := directory.io.wayOH
  directoryEvent.replWayOH := directory.io.replWayOH
  directoryEvent.cmoHitInvalid := directory.io.cmoHitInvalid
  val directoryEventReg = RegNext(directoryEvent, 0.U.asTypeOf(directoryEvent))
  mainPipe.io.bufResp := RegNext(sinkC.io.bufResp)
  mainPipe.io.dirResp_s3 := directoryEventReg.result.bits
  mainPipe.io.replResp := directoryEventReg.replacement
  mainPipe.io.metaOnHit_s3 := directoryEventReg.metaOnHit
  mainPipe.io.errOnSnp_s3 := directoryEventReg.errOnSnp
  mainPipe.io.dirWayOH_s3 := directoryEventReg.wayOH
  mainPipe.io.dirReplWayOH_s3 := directoryEventReg.replWayOH
  mainPipe.io.cmoHitInvalid := directoryEventReg.cmoHitInvalid
  mainPipe.io.retryFastFwd_s2 := directory.io.retryFastFwd
  mainPipe.io.fromMSHRCtl <> mshrCtl.io.toMainPipe
  mainPipe.io.refillBufResp_s3.valid := RegNext(RegNext(refillBuf.io.r.valid, false.B), false.B)
  mainPipe.io.refillBufResp_s3.bits.id := RegNext(refillBuf.io.resp.id)
  mainPipe.io.refillBufResp_s3.bits.data := RegNext(refillBuf.io.resp.data)
  mainPipe.io.releaseBufResp_s3.valid := RegNext(RegNext(releaseBuf.io.r.valid, false.B), false.B)
  mainPipe.io.releaseBufResp_s3.bits := RegNext(releaseBuf.io.resp.data)
  mainPipe.io.toDS.rdata_s5 := dataStorage.io.rdata
  mainPipe.io.toDS.error_s5 := dataStorage.io.error
  // mainPipe.io.grantBufferHint := grantBuf.io.l1Hint
  // mainPipe.io.globalCounter := grantBuf.io.globalCounter

  mshrCtl.io.fromReqArb.status_s1 := reqArb.io.status_s1
  mshrCtl.io.fromMainPipe <> mainPipe.io.toMSHRCtl
  mshrCtl.io.fromMainPipe.mshr_alloc_s3 := mainPipe.io.toMSHRCtl.mshr_alloc_s3
  mshrCtl.io.txdatDone := txdat.io.done
  mshrCtl.io.txrspDone := txrsp.io.done
  mshrCtl.io.grantStatus := grantBuf.io.grantStatus
  mshrCtl.io.resps.sinkC := sinkC.io.resp
  mshrCtl.io.sinkCReleaseData := sinkC.io.releaseData
  mshrCtl.io.resps.rxrsp := rxrsp.io.in
  mshrCtl.io.resps.rxdat := rxdat.io.in
  mshrCtl.io.nestedwb := mainPipe.io.nestedwb
  mshrCtl.io.replResp := directoryEventReg.replacement
  mshrCtl.io.aMergeTask := reqBuf.io.aMergeTask
  // TODO: This is ugly
  mshrCtl.io.pipeStatusVec(0) := (reqArb.io.status_vec)(1) // s2 status
  mshrCtl.io.pipeStatusVec(1) := mainPipe.io.status_vec_toD(3) // result capture
  mshrCtl.io.pipeStatusVec(2) := mainPipe.io.status_vec_toD(0) // S3 apply

  /* Read and write release buffer */
  releaseBuf.io.r := reqArb.io.releaseBufRead_s2
  // Second read port: direct ReplaceMSHR->TXDAT copyback path.
  releaseBuf.io.r2 := mshrCtl.io.releaseBufDirectRd
  mshrCtl.io.releaseBufDirectResp := releaseBuf.io.resp2
  val nestedWriteReleaseBuf,
    nestedWriteReleaseBufExtra,
    sinkCWriteReleaseBuf,
    sinkCWriteReleaseBufExtra,
    mpWriteReleaseBuf = Wire(Valid(new MSHRBufWrite()))
  mshrCtl.io.nestedwbPayload := mainPipe.io.nestedwbData
  nestedWriteReleaseBuf := mshrCtl.io.nestedReleaseWrites(0)
  nestedWriteReleaseBufExtra := mshrCtl.io.nestedReleaseWrites(1)
  def releaseSnapshotIsOlder(id: UInt): Bool =
    (UIntToOH(id, mshrsAll) & mshrCtl.io.nestedReleaseAuthority).orR
  sinkCWriteReleaseBuf match { case x =>
    x := sinkC.io.releaseBufWrite
    x.bits.id := mshrCtl.io.releaseBufWriteId
    x.valid := sinkC.io.releaseBufWrite.valid && !releaseSnapshotIsOlder(x.bits.id)
  }
  sinkCWriteReleaseBufExtra match { case x =>
    x := sinkC.io.releaseBufWrite
    x.valid := sinkC.io.releaseBufWrite.valid && mshrCtl.io.releaseBufWriteIdExtra.valid &&
      !releaseSnapshotIsOlder(mshrCtl.io.releaseBufWriteIdExtra.bits)
    x.bits.id := mshrCtl.io.releaseBufWriteIdExtra.bits
  }
  mpWriteReleaseBuf := mainPipe.io.releaseBufWrite
  mpWriteReleaseBuf.valid := mainPipe.io.releaseBufWrite.valid &&
    !releaseSnapshotIsOlder(mainPipe.io.releaseBufWrite.bits.id)
  releaseBuf.io.w <> VecInit(Seq(
    nestedWriteReleaseBuf,
    nestedWriteReleaseBufExtra,
    sinkCWriteReleaseBuf,
    sinkCWriteReleaseBufExtra,
    mpWriteReleaseBuf
  ))

  /* Read and write refill buffer */
  refillBuf.io.r := reqArb.io.refillBufRead_s2
  // RefillBuf has no direct-path consumer; tie off the second read port.
  refillBuf.io.r2.valid := false.B
  refillBuf.io.r2.bits := 0.U.asTypeOf(new MSHRBufRead)
  val nestedWriteRefillBuf = Wire(Valid(new MSHRBufWrite()))
  nestedWriteRefillBuf.valid := mshrCtl.io.nestedwbRefillDataId.valid
  nestedWriteRefillBuf.bits.data := mainPipe.io.nestedwbData
  nestedWriteRefillBuf.bits.id := mshrCtl.io.nestedwbRefillDataId.bits
  nestedWriteRefillBuf.bits.beatMask := Fill(beatSize, true.B)
  val rxdatWriteRefillBuf = WireInit(rxdat.io.refillBufWrite)
  val rxdatTargetsReleaseOwnedRefill =
    (UIntToOH(rxdat.io.refillBufWrite.bits.id, mshrsAll) & mshrCtl.io.refillBufReleaseOwner).orR
  // A CompData whose DBID belongs to the previous tenant of this TxnID must
  // not overwrite the new tenant's RefillBuf (see MSHRCtl.rxdatStaleSlots).
  val rxdatTargetsStaleSlot =
    (UIntToOH(rxdat.io.refillBufWrite.bits.id, mshrsAll) & mshrCtl.io.rxdatStaleSlots).orR
  rxdatWriteRefillBuf.valid := rxdat.io.refillBufWrite.valid &&
    !rxdatTargetsReleaseOwnedRefill && !rxdatTargetsStaleSlot
  // A Get-on-TRUNK DS snapshot is only a fallback.  Once matching
  // ReleaseData owns RefillBuf, a later S5 snapshot must not overwrite the
  // authoritative dirty payload.
  val mainPipeWriteRefillBuf = WireInit(mainPipe.io.refillBufWrite)
  val mainPipeTargetsReleaseOwnedRefill =
    (UIntToOH(mainPipe.io.refillBufWrite.bits.id, mshrsAll) & mshrCtl.io.refillBufReleaseOwner).orR
  mainPipeWriteRefillBuf.valid := mainPipe.io.refillBufWrite.valid && !mainPipeTargetsReleaseOwnedRefill
  refillBuf.io.w <> VecInit(Seq(
    rxdatWriteRefillBuf,
    sinkC.io.refillBufWrite,
    mainPipeWriteRefillBuf,
    nestedWriteRefillBuf
  ))

  io.prefetch.foreach { p =>
    p.train <> mainPipe.io.prefetchTrain.get
    sinkA.io.prefetchReq.get <> p.req
    p.resp <> grantBuf.io.prefetchResp.get
    p.tlb_req.req.ready := true.B
    p.tlb_req.resp.valid := false.B
    p.tlb_req.resp.bits := DontCare
    p.tlb_req.pmp_resp := DontCare
    p.recv_addr := 0.U.asTypeOf(p.recv_addr)
  }

  /* IO Connection */
  io.l1Hint <> mainPipe.io.l1Hint
  io.pfMonitorStat.foreach { stat =>
    val pfSourceCount = PfSource.PfSourceCount.id
    val zeroVec = VecInit(Seq.fill(pfSourceCount)(0.U(2.W)))

    val dirResult = directory.io.resp.bits
    val dirPfHit = directory.io.resp.valid &&
      !directory.io.replResp.valid &&
      dirResult.replacerInfo.channel === 1.U &&
      MemReqSource.isCPUReq(dirResult.replacerInfo.reqSource) &&
      dirResult.hit &&
      dirResult.meta.prefetch.getOrElse(false.B)
    val dirPfSrc = dirResult.meta.prefetchSrc.getOrElse(PfSource.NoWhere.id.U)
    val dirHitInc = VecInit((0 until pfSourceCount).map { i =>
      Mux(dirPfHit && dirPfSrc === i.U, 1.U(2.W), 0.U(2.W))
    })

    val mshrPfHit = reqBuf.io.pfStatInMSHR.hitPf
    val mshrPfSrc = PfSource.fromMemReqSource(reqBuf.io.pfStatInMSHR.hitPfReqSrc)
    val mshrHitInc = VecInit((0 until pfSourceCount).map { i =>
      Mux(mshrPfHit && mshrPfSrc === i.U, 1.U(2.W), 0.U(2.W))
    })

    val sentInc = io.prefetch.map { p =>
      val sentPfSrc = PfSource.fromMemReqSource(p.req.bits.pfSource)
      VecInit((0 until pfSourceCount).map { i =>
        Mux(p.req.fire && sentPfSrc === i.U, 1.U(2.W), 0.U(2.W))
      })
    }.getOrElse(zeroVec)

    stat.pfSentVec := sentInc
    stat.pfHitVec := VecInit((0 until pfSourceCount).map(i => dirHitInc(i) +& mshrHitInc(i)))
  }
  prefetchOpt.foreach {
    _ =>
      io.replaceRecord.get := mainPipe.io.replaceRecord.get
      io.dataRefill.get := mshrCtl.io.dataRefill.get
      io.busContention.get := mshrCtl.io.busContention.get
  }
  prefetchOrTopDownOpt.foreach {
    _ =>
      io.dirResult.get.valid := directory.io.resp.valid && !directory.io.replResp.valid // exclude MSHR-Grant read-dir
      io.dirResult.get.bits := directory.io.resp.bits
      io.pfStatInMSHR.get := reqBuf.io.pfStatInMSHR
      io.pfSent.get.valid := io.prefetch.fold(false.B)(_.req.fire)
      io.pfSent.get.bits := io.prefetch.fold(MemReqSource.NoWhere.id.U)(p => p.req.bits.pfSource)
  }
  topDownOpt.foreach (
    _ => {
      io.msStatus.get := mshrCtl.io.msStatus.get
      io.msAlloc.get := mshrCtl.io.msAlloc.get
    }
  )
  io.l2Miss := mshrCtl.io.l2Miss

  /* Connect upwards channels */
  val inBuf = cacheParams.innerBuf
  sinkA.io.a <> inBuf.a(io.in.a)
  io.in.b <> inBuf.b(mshrCtl.io.toSourceB)
  sinkC.io.c <> inBuf.c(io.in.c)
  io.in.d <> inBuf.d(grantBuf.io.d)
  grantBuf.io.e <> inBuf.e(io.in.e)
  io.error.valid := RegNext(mainPipe.io.error.valid, false.B)
  io.error.bits := RegNext(mainPipe.io.error.bits)

  /* Connect downwards channels */
  io.out.tx.req <> txreq.io.out
  io.out.tx.dat <> txdat.io.out
  io.out.tx.rsp <> txrsp.io.out
  rxsnp.io.rxsnp <> io.out.rx.snp
  rxdat.io.out <> io.out.rx.dat
  rxrsp.io.out <> io.out.rx.rsp

  io_pCrd <> mshrCtl.io.pCrd

  /* Connect l2 flush All channel */ 
  sinkA.io.cmoAll.foreach {cmoAll => cmoAll.cmoLineDone := mainPipe.io.cmoLineDone.getOrElse(false.B)}
  sinkA.io.cmoAll.foreach { cmoAll =>
    cmoAll.mshrValid := VecInit(
      mshrCtl.io.refillInfo.map(_.valid) ++
      mshrCtl.io.snoopInfo.map(_.valid) ++
      mshrCtl.io.replaceInfo.map(_.valid)
    ).reduce(_|_)
  }
  sinkA.io.cmoAll.foreach {cmoAll => cmoAll.l2Flush := io.l2Flush.getOrElse(false.B)}
  mainPipe.io.cmoAllBlock.foreach {_ := sinkA.io.cmoAll.map(_.cmoAllBlock).getOrElse(false.B)}

  io.l2FlushDone.foreach {_ := RegNext(sinkA.io.cmoAll.map(_.l2FlushDone).getOrElse(false.B))}

  /* CDP Prefetcher Detect Trigger */
  if (hasCDP) {
    io_cdp_triggers.get <> mainPipe.io.cdp_trigger.get
  }

  /* ===== Hardware Performance Monitor ===== */
  val perfEvents = Seq(mshrCtl, mainPipe).flatMap(_.getPerfEvents)
  generatePerfEvent()
}
