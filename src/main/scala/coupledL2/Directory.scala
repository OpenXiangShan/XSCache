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
import xscache.coupledL2.utils._
import utility.{ChiselDB, Code, MemReqSource, ParallelPriorityMux, RegNextN, XSPerfAccumulate, MaskToOH}
import utility.sram.SRAMTemplate
import org.chipsalliance.cde.config.Parameters
import xscache.coupledL2.prefetch.PfSource
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.util.SeqToAugmentedSeq

class MetaEntry(implicit p: Parameters) extends L2Bundle {
  val dirty = Bool()
  val state = UInt(stateBits.W)
  val clients = UInt(clientBits.W)  // valid-bit of clients
  // TODO: record specific state of clients instead of just 1-bit
  val alias = aliasBitsOpt.map(width => UInt(width.W)) // alias bits of client
  val prefetch = if (hasPrefetchBit) Some(Bool()) else None // whether block is prefetched
  val prefetchSrc = if (hasPrefetchSrc) Some(UInt(PfSource.pfSourceBits.W)) else None // prefetch source
  val accessed = Bool()
  val tagErr = Bool() // ECC error from L1/L3; DataCheck for CHI
  val dataErr = Bool()

  // for CDP
  val cdpPfDepth = if (hasCDP) Some(UInt(cdpPfDepthBits.get.W)) else None

  def =/=(entry: MetaEntry): Bool = {
    this.asUInt =/= entry.asUInt
  }
}

object MetaEntry {
  def apply()(implicit p: Parameters) = {
    val init = WireInit(0.U.asTypeOf(new MetaEntry))
    init
  }
  def apply(dirty: Bool, state: UInt, clients: UInt, alias: Option[UInt], prefetch: Bool = false.B,
            pfsrc: UInt = PfSource.NoWhere.id.U, accessed: Bool = false.B,
            tagErr: Bool = false.B, dataErr: Bool = false.B, cdpPfDepth: UInt = 0.U
  )(implicit p: Parameters) = {
    val entry = Wire(new MetaEntry)
    entry.dirty := dirty
    entry.state := state
    entry.clients := clients
    entry.alias.foreach(_ := alias.getOrElse(0.U))
    entry.prefetch.foreach(_ := prefetch)
    entry.prefetchSrc.foreach(_ := pfsrc)
    entry.accessed := accessed
    entry.tagErr := tagErr
    entry.dataErr := dataErr
    entry.cdpPfDepth.foreach(_ := cdpPfDepth)
    entry
  }
}

class DirRead(implicit p: Parameters) extends L2Bundle {
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  // dirResult.way must only be in the wayMask
  val wayMask = UInt(cacheParams.ways.W)
  val replacerInfo = new ReplacerInfo()
  // dirRead when refill
  val refill = Bool()
  // RefillBuf commit for a request that originally hit. Return the current
  // hit way when it still exists; otherwise return a replacement candidate.
  val normalRefillRevalidate = Bool()
  val mshrId = UInt(mshrBits.W)
  // when flush l2
  val cmoAll = Bool()
  val cmoWay = UInt(wayBits.W)
}

class DirResult(implicit p: Parameters) extends L2Bundle {
  val hit = Bool()
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)  // hit way or victim way
  val meta = new MetaEntry()
  val error = Bool()
  val replacerInfo = new ReplacerInfo() // for TopDown usage
}

class ReplacerResult(implicit p: Parameters) extends L2Bundle {
  // Valid only for normalRefillRevalidate. It distinguishes a current hit
  // from a replacement candidate without overloading the victim metadata.
  val hit = Bool()
  val tag = UInt(tagBits.W)
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
  val meta = new MetaEntry()
  val mshrId = UInt(mshrBits.W)
  val retry = Bool()
  val validHold = Bool()
}

class MetaWrite(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val wayOH = UInt(cacheParams.ways.W)
  val wmeta = new MetaEntry
}

class TagWrite(implicit p: Parameters) extends L2Bundle {
  val set = UInt(setBits.W)
  val wayOH = UInt(cacheParams.ways.W)
  val wtag = UInt(tagBits.W)
}

// DB entry for prefetch lifecycle tracking (arrival, access, eviction)
class PrefetchDbEntry(implicit p: Parameters) extends L2Bundle {
  val setIdx = UInt(setBits.W)
  val isPrefetch = Bool()
  val isHit = Bool()
  val tag = UInt(tagBits.W)
  val way = UInt(wayBits.W)
  val metaSource = UInt(PfSource.pfSourceBits.W)
  val reqSource = UInt(MemReqSource.reqSourceBits.W)
}


class Directory(implicit p: Parameters) extends L2Module {

  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new DirRead))
    val resp = ValidIO(new DirResult)
    val metaWReq = Flipped(ValidIO(new MetaWrite))
    val tagWReq = Flipped(ValidIO(new TagWrite))
    val replResp = ValidIO(new ReplacerResult)
    // Independent normal, snoop, and replacement ownership views are needed
    // to reserve all live ways without hiding a normal request behind a
    // replacement victim.
    val refillInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    // Snoop contexts are not normal MSHRs, but a data-carrying snoop can
    // retain a DS fallback dependency on its hit way.
    val snoopInfo = Vec(mshrsAll, Flipped(ValidIO(new MSHRInfo)))
    val replaceInfo = Vec(mshrsAll, Flipped(ValidIO(new ReplaceMSHRInfo)))
    val metaOnHit = new MetaEntry()
    val errOnSnp = Bool()
    val wayOH = Output(UInt(cacheParams.ways.W))
    val replWayOH = Output(UInt(cacheParams.ways.W))
    val cmoHitInvalid = Output(Bool())
    val retryFastFwd = Output(Bool())
  })

  def invalid_way_sel(metaVec: Seq[MetaEntry]) = {
    val invalid_vec = metaVec.map(_.state === MetaData.INVALID)
    val has_invalid_way = Cat(invalid_vec).orR
    val invalid_oh = MaskToOH(invalid_vec.asUInt)
    val invalid_way = ParallelPriorityMux(invalid_vec.zipWithIndex.map(x => x._1 -> x._2.U(wayBits.W)))
    (has_invalid_way, invalid_way, invalid_oh) // one-hot of invalid ways
  }

  val sets = cacheParams.sets
  val ways = cacheParams.ways

  val tagWen  = io.tagWReq.valid
  val metaWen = io.metaWReq.valid
  val replacerWen = WireInit(false.B)

  // val tagArray  = Module(new SRAMTemplate(UInt(tagBits.W), sets, ways, singlePort = true))
  private val mbist = p(L2ParamKey).hasMbist
  private val hasSramCtl = p(L2ParamKey).hasSramCtl
  val tagArray = if (enableTagECC) {
    Module(new SplittedSRAM(
      gen = UInt((tagBankSplit * encTagBankBits).W),
      set = sets,
      way = ways,
      waySplit = 2,
      dataSplit = if (enableTagSRAMSplit) {
        tagSRAMSplit
      } else {
        1
      },
      singlePort = true,
      readMCP2 = false,
      hasMbist = mbist,
      hasSramCtl = hasSramCtl
    ))
  } else {
    Module(new SplittedSRAM(
      gen = UInt(tagBits.W),
      set = sets,
      way = ways,
      waySplit = 2,
      singlePort = true,
      readMCP2 = false,
      hasMbist = mbist,
      hasSramCtl = hasSramCtl
    ))
  }

  val metaArray = Module(new SRAMTemplate(new MetaEntry, sets, ways, singlePort = true, hasMbist = mbist, hasSramCtl = hasSramCtl))

  val metaRead = Wire(Vec(ways, new MetaEntry()))

  // Replacer
  val repl = ReplacementPolicy.fromString(cacheParams.replacement, ways)
  val random_repl = cacheParams.replacement == "random"
  val replacer_sram_opt = if(random_repl) None else
    Some(Module(new SRAMTemplate(UInt(repl.nBits.W), sets, 1, singlePort = true, shouldReset = true, hasMbist = mbist, hasSramCtl = hasSramCtl)))

  /* ====== Generate response signals ====== */
  // hit/way calculation in stage 3, Cuz SRAM latency is high under high frequency
  /* stage 1: io.read.fire, access Tag/Meta
     stage 2: get Tag/Meta, latch
     stage 3: calculate hit/way and chosen meta/tag by way
  */
  val reqValid_s2 = RegNext(io.read.fire, false.B)
  val reqValid_s3 = RegNext(reqValid_s2, false.B)
  val req_s1 = io.read.bits
  val req_s2 = RegEnable(req_s1, io.read.fire)
  val req_s3 = RegEnable(req_s2, 0.U.asTypeOf(req_s2), reqValid_s2)
  val cmoWayOH_s3 = RegEnable(UIntToOH(req_s2.cmoWay, ways), reqValid_s2)

  val refillReqValid_s2 = RegNext(io.read.fire && io.read.bits.refill, false.B)
  val refillReqValid_s3 = RegNext(refillReqValid_s2, false.B)
  val refillReqValid_hold_s3 = RegEnable(refillReqValid_s2, false.B, !RegNext(refillReqValid_s2))

  // Tag(ECC) R/W
  val tagWrite = if (enableTagECC) {
    Cat(VecInit(Seq.tabulate(tagBankSplit)(i =>
      io.tagWReq.bits.wtag(tagBankBits * (i + 1) - 1, tagBankBits * i))).map(tag => cacheParams.dataCode.encode(tag)))
  } else {
    io.tagWReq.bits.wtag
  }
  val tagRead = tagArray.io.r(io.read.fire, io.read.bits.set).resp.data
  when (io.tagWReq.valid) {
    assert(PopCount(io.tagWReq.bits.wayOH) === 1.U, "Tag write should be one-hot")
  }
  tagArray.io.w(
    tagWen,
    tagWrite,
    io.tagWReq.bits.set,
    io.tagWReq.bits.wayOH
  )

  // Meta R/W
  metaRead := metaArray.io.r(io.read.fire, io.read.bits.set).resp.data
  metaArray.io.w(
    metaWen,
    io.metaWReq.bits.wmeta,
    io.metaWReq.bits.set,
    io.metaWReq.bits.wayOH
  )

  val metaAll_s3 = RegEnable(metaRead, 0.U.asTypeOf(metaRead), reqValid_s2)
  val tagRead_s3 = RegEnable(tagRead, reqValid_s2)
  val tagAll_s3 = if (enableTagECC) {
    tagRead_s3.map(x =>
      Cat(VecInit(Seq.tabulate(tagBankSplit)(i => x(encTagBankBits * (i + 1) - 1, encTagBankBits * i)(tagBankBits - 1, 0))))
    )
  } else {
    tagRead_s3
  }
  val errorAll_s3 = if (enableTagECC) {
    tagRead_s3.map(x =>
      VecInit(Seq.tabulate(tagBankSplit)(i => x(encTagBankBits * (i + 1) - 1, encTagBankBits * i))).
        map(tag => cacheParams.dataCode.decode(tag).error).reduce(_ | _)
    )
  } else {
    VecInit(Seq.fill(ways)(false.B))
  }

  val tagMatchVec = tagAll_s3.map(_ (tagBits - 1, 0) === req_s3.tag)
  val metaValidVec = metaAll_s3.map(_.state =/= MetaData.INVALID)
  val hitVecRaw = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  // Eviction-window guard (perf report 补充二十五): a channel-A lookup that
  // hits a line just selected as a refill victim must not complete as a hit.
  // In the deferred design the parent's commit overwrites the victim way much
  // later than the victim selection; a hit in between would grant/promote an
  // L1 copy that the sidecar never probes (its probe decision latched the
  // lookup-time meta), orphaning the copy when the commit lands.  Suppressing
  // the hit drops the request to the miss path, whose MSHR/CHI flow is
  // serialized behind the eviction writeback at openLLC (and the MSHR-alloc
  // conflict check on the victim metaTag holds it in the RequestBuffer until
  // the eviction retires).  Snoop (B) and release (C) lookups keep seeing the
  // entry: they are ordered through the replaceInfo/nestedwb machinery.
  // The mask is evaluated at s3 from the *live* refillInfo so a lookup whose
  // SRAM read raced the victim selection is still caught.
  val victimPendingWayMask_s3 = VecInit(io.refillInfo.map(s =>
    Mux(
      s.valid && (s.bits.set === req_s3.set) && !s.bits.dirHit &&
        s.bits.w_replResp && !s.bits.refillCommitLanded,
      UIntToOH(s.bits.way, ways),
      0.U(ways.W)
    )
  )).reduceTree(_ | _)
  val suppressAHitDying_s3 = req_s3.replacerInfo.channel(0) && !req_s3.refill &&
    !req_s3.normalRefillRevalidate && !req_s3.cmoAll
  val hitVec = hitVecRaw.zipWithIndex.map { case (h, i) =>
    h && !(suppressAHitDying_s3 && victimPendingWayMask_s3(i))
  }
  XSPerfAccumulate("dir_dying_hit_suppress_cnt",
    reqValid_s3 && suppressAHitDying_s3 && (victimPendingWayMask_s3 & hitVecRaw.asUInt).orR)

  /* ====== refill retry ====== */
  // when refill, ways that have not finished writing its refillData back to DS (in MSHR Release),
  // or using by Alias-Acquire (hit), can not be used for replace.
  // choose free way to refill, if all ways are occupied, we cancel the Grant and LET IT RETRY
  // compare is done at Stage2 for better timing
  // The requesting MSHR's own tentative way must not block its own lookup:
  // its dirResult.way lock only protects that way from OTHER in-flight
  // lookups.  Otherwise a fill whose replacer state has not changed since
  // allocation re-picks its own tentative way and is spuriously redirected
  // to MaskToOH (measured: 99.8% of all fallbacks are such self-collisions).
  val normalOccWayMask_s1 = VecInit(io.refillInfo.zipWithIndex.map { case (s, i) =>
    Mux(
      s.valid && (s.bits.set === req_s1.set) && (s.bits.blockRefill || s.bits.dirHit) &&
        req_s1.mshrId =/= i.U,
      UIntToOH(s.bits.way, ways),
      0.U(ways.W)
    )
  }).reduceTree(_ | _)
  val snoopWayHoldMask_s1 = VecInit(io.snoopInfo.map(s =>
    Mux(
      s.valid && (s.bits.set === req_s1.set) && s.bits.blockRefill,
      UIntToOH(s.bits.way, ways),
      0.U(ways.W)
    )
  )).reduceTree(_ | _)
  // ReplaceMSHR owns the victim ReleaseBuf independently of its normal parent.
  // The Directory way only needs protection until the parent's deferred refill
  // commit is visible: parentAttached drops at the parent's retire edge, which
  // is gated on commitIssued + refillWriteDone + the way-commit margin, so the
  // new tag/meta at that way are already installed before the hold releases.
  // Holding the way for the sidecar's whole writeback lifetime shrinks
  // freeWayMask for dozens of extra cycles and degrades victim selection.
  val replaceWayHoldMask_s1 = VecInit(io.replaceInfo.map(s =>
    Mux(
      s.valid && s.bits.parentAttached && (s.bits.set === req_s1.set),
      UIntToOH(s.bits.way, ways),
      0.U(ways.W)
    )
  )).reduceTree(_ | _)
  val occWayMask_s1 = normalOccWayMask_s1 | snoopWayHoldMask_s1 | replaceWayHoldMask_s1 |
    Mux(refillReqValid_s3 || reqValid_s3 && io.resp.bits.hit, UIntToOH(io.resp.bits.way, ways), 0.U(ways.W))

  val occWayMask_s2 = RegEnable(occWayMask_s1, io.read.fire && io.read.bits.refill)
  val snoopWayHoldMask_s2 = RegEnable(snoopWayHoldMask_s1, io.read.fire && io.read.bits.refill)
  val replaceWayHoldMask_s2 = RegEnable(replaceWayHoldMask_s1, io.read.fire && io.read.bits.refill)
  // Debug-only copies for fallback-cause decomposition (same sampling window).
  val normalOccWayMask_s2 = RegEnable(normalOccWayMask_s1, io.read.fire && io.read.bits.refill)
  val selfOccWayMask_s1 = VecInit(io.refillInfo.zipWithIndex.map { case (s, i) =>
    Mux(
      s.valid && (s.bits.set === req_s1.set) && (s.bits.blockRefill || s.bits.dirHit) &&
        io.read.bits.mshrId === i.U,
      UIntToOH(s.bits.way, ways),
      0.U(ways.W)
    )
  }).reduceTree(_ | _)
  val selfOccWayMask_s2 = RegEnable(selfOccWayMask_s1, io.read.fire && io.read.bits.refill)

  io.retryFastFwd := occWayMask_s2.andR && refillReqValid_s2
  val freeWayMask_s3 = RegEnable(~occWayMask_s2, refillReqValid_s2)
  val refillRetry = RegEnable(occWayMask_s2.andR, refillReqValid_s2)

  // val hitWay = OHToUInt(hitVec)
  val hitOH = hitVec.asUInt
  assert(PopCount(hitVec) <= 1.U, "Set should not have more than one hit")
  val replaceWay = WireInit(UInt(wayBits.W), 0.U)
  val replaceOH = WireInit(UInt(ways.W), 0.U)
  val (inv, invalidWay, invOH) = invalid_way_sel(metaAll_s3)
  val chosenOH = Mux(inv, invOH, replaceOH)
  // for retry bug fixing: if the chosenway not in freewaymask, choose another way
  val finalReplOH = Mux(
    Mux1H(chosenOH, freeWayMask_s3),
    chosenOH,
    MaskToOH(freeWayMask_s3)
  )
  val hit_s3 = Cat(hitVec).orR || req_s3.cmoAll
  val wayOH_s3 = Mux(req_s3.cmoAll, cmoWayOH_s3, Mux(hit_s3, hitOH, finalReplOH))
  when (reqValid_s3 && req_s3.refill && !hit_s3) {
    assert(!(wayOH_s3 & snoopWayHoldMask_s2).orR,
      "a refill replacement must not select a DS way held by SnoopMSHR data")
    assert(!(wayOH_s3 & replaceWayHoldMask_s2).orR,
      "a refill replacement must not select a DS way held by ReplaceMSHR")
  }
  val way_s3 = OHToUInt(wayOH_s3)
  val meta_s3 = Mux1H(wayOH_s3, metaAll_s3)
  val metaOnHit_s3 = Mux1H(hitOH, metaAll_s3) // only valid when hit
  val tag_s3 = Mux1H(wayOH_s3, tagAll_s3)
  val set_s3 = req_s3.set
  val replacerInfo_s3 = req_s3.replacerInfo
  // Debug counters for the victim-selection-degradation investigation: how
  // often the free-way mask forces the MaskToOH fallback instead of the
  // replacer's pick, how often a refill read is retried, and the clients
  // composition of selected victims.
  val replLookup_s3 = reqValid_s3 && req_s3.refill && !hit_s3
  val chosenBlocked_s3 = replLookup_s3 && !Mux1H(chosenOH, freeWayMask_s3)
  XSPerfAccumulate("repl_fallback_cnt", chosenBlocked_s3)
  // Decompose what covers the chosen way when the fallback fires: the
  // requesting MSHR's own tentative way (spurious self-collision), another
  // normal context, a ReplaceMSHR hold, or a SnoopMSHR hold.
  XSPerfAccumulate("repl_fallback_selfWay_cnt",
    chosenBlocked_s3 && (chosenOH & selfOccWayMask_s2).orR)
  XSPerfAccumulate("repl_fallback_normalOther_cnt",
    chosenBlocked_s3 && (chosenOH & normalOccWayMask_s2 & ~selfOccWayMask_s2).orR)
  XSPerfAccumulate("repl_fallback_replaceHold_cnt",
    chosenBlocked_s3 && (chosenOH & replaceWayHoldMask_s2).orR)
  XSPerfAccumulate("repl_fallback_snoopHold_cnt",
    chosenBlocked_s3 && (chosenOH & snoopWayHoldMask_s2).orR)
  // Occupancy level of each mask at lookup time (sum over lookups).
  XSPerfAccumulate("lookup_occ_normal_sum",
    Mux(replLookup_s3, PopCount(normalOccWayMask_s2), 0.U))
  XSPerfAccumulate("lookup_occ_replace_sum",
    Mux(replLookup_s3, PopCount(replaceWayHoldMask_s2), 0.U))
  XSPerfAccumulate("lookup_occ_snoop_sum",
    Mux(replLookup_s3, PopCount(snoopWayHoldMask_s2), 0.U))
  XSPerfAccumulate("repl_lookup_total", io.replResp.valid)
  XSPerfAccumulate("repl_retry_cnt", io.replResp.valid && io.replResp.bits.retry)
  XSPerfAccumulate("repl_victim_valid_total",
    io.replResp.valid && !io.replResp.bits.retry && !io.replResp.bits.hit &&
      io.replResp.bits.meta.state =/= MetaData.INVALID)
  XSPerfAccumulate("repl_victim_clients1",
    io.replResp.valid && !io.replResp.bits.retry && !io.replResp.bits.hit &&
      io.replResp.bits.meta.state =/= MetaData.INVALID && io.replResp.bits.meta.clients.orR)
  XSPerfAccumulate("repl_victim_dirty1",
    io.replResp.valid && !io.replResp.bits.retry && !io.replResp.bits.hit &&
      io.replResp.bits.meta.state =/= MetaData.INVALID && io.replResp.bits.meta.dirty)
  // Write-port busy cycles regardless of whether a read is presented.
  // Channel lookups never assert io.read.valid while the port is busy
  // (RequestArb gates chnl_task_s1.valid with dirRead_s1.ready), so the
  // dir_read_stall_* counters above only see MSHR replRead stalls.  These
  // busy-cycle counters give the denominator for by_dir attribution:
  // dir_busy_repl_only is exactly the share of stalls that S2 (channel
  // lookups bypassing replacerWen) could remove.

  XSPerfAccumulate("dir_busy_tag_only_cnt",
    io.tagWReq.valid && !io.metaWReq.valid && !replacerWen)

  XSPerfAccumulate("dir_busy_meta_only_cnt",
    !io.tagWReq.valid && io.metaWReq.valid && !replacerWen)

  XSPerfAccumulate("dir_busy_repl_only_cnt",
    !io.tagWReq.valid && !io.metaWReq.valid && replacerWen)

  XSPerfAccumulate("dir_busy_mix_cnt",
    (PopCount(Seq(io.tagWReq.valid, io.metaWReq.valid, replacerWen)) >= 2.U))
  val errorOnSNP_s3 = if (enableTagECC) {
    Mux1H(hitOH, errorAll_s3)
  } else {
    false.B
  }

  val error_s3 = if (enableTagECC) {
    Mux1H(wayOH_s3, errorAll_s3) && reqValid_s3 && !req_s3.cmoAll && meta_s3.state =/= MetaData.INVALID
  } else {
    false.B
  }

  io.resp.valid      := reqValid_s3
  io.resp.bits.hit   := hit_s3
  io.resp.bits.way   := way_s3
  io.resp.bits.meta  := meta_s3
  io.metaOnHit := metaOnHit_s3
  io.resp.bits.tag   := tag_s3
  io.resp.bits.set   := set_s3
  io.resp.bits.error := error_s3  // depends on ECC
  io.errOnSnp := errorOnSNP_s3
  io.resp.bits.replacerInfo := replacerInfo_s3
  io.wayOH := wayOH_s3

  dontTouch(io)
  dontTouch(metaArray.io)
  dontTouch(tagArray.io)

  // S2 (channel lookups bypassing replacerWen) is reverted pending a
  // dedicated hardening pass: a bypassed miss latches garbage replacer-derived
  // victim info into the allocated MSHR, and F1+S2 still fails tl-test
  // (see perf report 补充二十五).  Keep plain write-priority behaviour.
  val dirReadReady = !io.metaWReq.valid && !io.tagWReq.valid && !replacerWen
  io.read.ready := dirReadReady
  // Directory write-port breakdown for by_dir attribution: read-side stalls
  // decomposed by which write type is blocking -- a read request held off
  // only by tagWReq / only by metaWReq / only by replacerWen. The last one
  // is what port-splitting (S2) would remove.
  XSPerfAccumulate("dir_read_stall_tag_cnt",
    io.read.valid && !dirReadReady && io.tagWReq.valid)
  XSPerfAccumulate("dir_read_stall_meta_cnt",
    io.read.valid && !dirReadReady && !io.tagWReq.valid && io.metaWReq.valid)
  XSPerfAccumulate("dir_read_stall_replacer_cnt",
    io.read.valid && !dirReadReady && !io.tagWReq.valid && !io.metaWReq.valid && replacerWen)

  /* ======!! Replacement logic !!====== */
  /* ====== Read, choose replaceWay ====== */
  val repl_state_s3 = if(random_repl) {
    when(io.tagWReq.fire){
      repl.miss
    }
    0.U
  } else {
    val repl_sram_r = replacer_sram_opt.get.io.r(io.read.fire, io.read.bits.set).resp.data(0)
    val repl_state = RegEnable(repl_sram_r, 0.U(repl.nBits.W), reqValid_s2)
    repl_state
  }

  if (cacheParams.replacement == "random") {
    replaceWay := repl.get_replace_way(repl_state_s3)
    replaceOH := UIntToOH(replaceWay, ways)
  } else {
    replaceOH := repl.get_replace_OH(repl_state_s3)
    assert(PopCount(replaceOH) === 1.U, "Replacement way should be one-hot")
    replaceWay := OHToUInt(replaceOH)
  }

  val normalRefillRevalidateHit = req_s3.normalRefillRevalidate && hit_s3
  val replRespOH = Mux(normalRefillRevalidateHit, hitOH, finalReplOH)
  io.replResp.valid := refillReqValid_s3
  io.replResp.bits.hit := normalRefillRevalidateHit
  io.replResp.bits.tag := Mux1H(replRespOH, tagAll_s3)
  io.replResp.bits.set := req_s3.set
  io.replResp.bits.way := OHToUInt(replRespOH)
  io.replResp.bits.meta := Mux1H(replRespOH, metaAll_s3)
  io.replResp.bits.mshrId := req_s3.mshrId
  // Stale-victim-meta guard: a meta/tag write landing in the cycle right
  // after this lookup's SRAM read is not reflected in replResp.meta (the
  // single-port read port is write-priority, so only a write exactly one
  // cycle after the read can race).  If that write targeted the way we just
  // picked -- e.g. a concurrent A-hit promotion granting a new L1 copy, or
  // another parent's commit overwriting it -- the victim's clients/dirty
  // bits may have changed; force a retry so the selection re-reads fresh
  // state instead of evicting from stale meta.
  val dirWrValid_d1 = RegNext(io.metaWReq.valid || io.tagWReq.valid, false.B)
  val dirWrSet_d1 = RegEnable(
    Mux(io.tagWReq.valid, io.tagWReq.bits.set, io.metaWReq.bits.set), 0.U(setBits.W),
    io.metaWReq.valid || io.tagWReq.valid)
  val dirWrWayOH_d1 = RegEnable(
    Mux(io.tagWReq.valid, io.tagWReq.bits.wayOH, io.metaWReq.bits.wayOH), 0.U(ways.W),
    io.metaWReq.valid || io.tagWReq.valid)
  val victimMetaJustWritten = dirWrValid_d1 && dirWrSet_d1 === req_s3.set &&
    (dirWrWayOH_d1 & replRespOH).orR
  XSPerfAccumulate("dir_victim_race_retry_cnt",
    io.replResp.valid && !normalRefillRevalidateHit && !refillRetry && victimMetaJustWritten)
  io.replResp.bits.retry := !normalRefillRevalidateHit && (refillRetry || victimMetaJustWritten)
  io.replResp.bits.validHold := refillReqValid_hold_s3
  io.replWayOH := replRespOH

  /* ====== Update ====== */
  // PLRU: update replacer only when A hit or refill, at stage 3
  // RRIP: update replacer when A/C hit or refill
  val updateHit = if(cacheParams.replacement == "drrip" || cacheParams.replacement == "srrip"){
    reqValid_s3 && hit_s3 &&
    ((req_s3.replacerInfo.channel(0) && (req_s3.replacerInfo.opcode === AcquirePerm || req_s3.replacerInfo.opcode === AcquireBlock || req_s3.replacerInfo.opcode === Hint)) ||
     (req_s3.replacerInfo.channel(2) && (req_s3.replacerInfo.opcode === Release || req_s3.replacerInfo.opcode === ReleaseData)))
  } else {
    reqValid_s3 && hit_s3 && req_s3.replacerInfo.channel(0) &&
    (req_s3.replacerInfo.opcode === AcquirePerm || req_s3.replacerInfo.opcode === AcquireBlock)
  }
  val updateRefill = refillReqValid_s3 && !refillRetry
  // update replacer when A/C hit or refill
  replacerWen := updateHit || updateRefill

  // Directory write-port breakdown for by_dir attribution: replacer state
  // updates split by cause (hit promotion vs refill insertion).
  XSPerfAccumulate("dir_wen_replacer_updateHit_cnt", updateHit)
  XSPerfAccumulate("dir_wen_replacer_updateRefill_cnt", updateRefill)

  // hit-Promotion, miss-Insertion for RRIP
  // origin-bit marks whether the data_block is reused
  val origin_bit_opt = if(random_repl) None else
    Some(Module(new SRAMTemplate(Bool(), sets, ways, singlePort = true, shouldReset = true, hasMbist = mbist, hasSramCtl = hasSramCtl)))
  val origin_bits_r = origin_bit_opt.get.io.r(io.read.fire, io.read.bits.set).resp.data
  val origin_bits_hold = Wire(Vec(ways, Bool()))
  origin_bits_hold := RegEnable(origin_bits_r, reqValid_s2)
  origin_bit_opt.get.io.w(replacerWen, hit_s3, set_s3, wayOH_s3)
  val rrip_req_type = WireInit(0.U(4.W))
  // [3]: 0-firstuse, 1-reuse;
  // [2]: 0-acquire, 1-release;
  // [1]: 0-non-prefetch, 1-prefetch;
  // [0]: 0-not-refill, 1-refill
  rrip_req_type := Cat(Mux1H(hitOH, origin_bits_hold),
    req_s3.replacerInfo.channel(2),
    (!refillReqValid_s3 && req_s3.replacerInfo.channel(0) && req_s3.replacerInfo.opcode === Hint) ||
      (req_s3.replacerInfo.channel(2) && Mux1H(wayOH_s3, metaAll_s3).prefetch.getOrElse(false.B)) ||
      (refillReqValid_s3 && req_s3.replacerInfo.refill_prefetch),
    req_s3.refill
  )
  private val mbistPl = MbistPipeline.PlaceMbistPipeline(1, "L2Directory", mbist)
  if(cacheParams.replacement == "srrip"){
    val next_state_s3 = repl.get_next_state(repl_state_s3, wayOH_s3, hit_s3, inv, rrip_req_type)
    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(replacerWen, next_state_s3, set_s3, 1.U)

  } else if(cacheParams.replacement == "drrip"){
    // Set Dueling
    val PSEL = RegInit(512.U(10.W)) //32-monitor sets, 10-bits psel
    // track monitor sets' hit rate for each policy
    // basic SDMs complement-selection policy: srrip--set_idx[group-:]==set_idx[group_offset-:]; brrip--set_idx[group-:]==!set_idx[group_offset-:]
    val setBits = log2Ceil(sets)
    val half_setBits = setBits >> 1
    val match_a = set_s3(setBits-1,half_setBits)===set_s3(setBits-half_setBits-1,0)
    val match_b = set_s3(setBits-1,half_setBits)===(~set_s3(setBits-half_setBits-1,0))
    when(refillReqValid_s3 && match_a && !hit_s3 && (PSEL=/=1023.U)){  //SDMs_srrip miss
      PSEL := PSEL + 1.U
    } .elsewhen(refillReqValid_s3 && match_b && !hit_s3 && (PSEL=/=0.U)){ //SDMs_brrip miss
      PSEL := PSEL - 1.U
    }
    // decide use which policy by policy selection counter, for insertion
    /* if set -> SDMs: use fix policy
       else if PSEL(MSB)==0: use srrip
       else if PSEL(MSB)==1: use brrip */
    val repl_type = WireInit(false.B)
    repl_type := Mux(match_a, false.B,
                    Mux(match_b, true.B,
                      Mux(PSEL(9)===0.U, false.B, true.B)))    // false.B - srrip, true.B - brrip

    val next_state_s3 = repl.get_next_state(repl_state_s3, wayOH_s3, hit_s3, inv, repl_type, rrip_req_type)

    val repl_init = Wire(Vec(ways, UInt(2.W)))
    repl_init.foreach(_ := 2.U(2.W))
    replacer_sram_opt.get.io.w(replacerWen, next_state_s3, set_s3, 1.U)
  } else {
    val next_state_s3 = repl.get_next_state(repl_state_s3, way_s3)
    replacer_sram_opt.get.io.w(replacerWen, next_state_s3, set_s3, 1.U)
  }

  io.cmoHitInvalid := Mux1H(cmoWayOH_s3, metaAll_s3).state === MetaData.INVALID

  /* ====== Reset ====== */

  XSPerfAccumulate("dirRead_cnt", io.read.fire)
  XSPerfAccumulate("choose_busy_way", reqValid_s3 && !Mux1H(chosenOH, req_s3.wayMask))

  /* ====== ChiselDB logging for  prefetcher lifecycle ====== */
  if (cacheParams.enableMonitor && !cacheParams.FPGAPlatform) {
    val defaultPfSrc = PfSource.NoWhere.id.U
    val hartId = cacheParams.hartId
    val pfReqWriteTable = ChiselDB.createTable(s"L2_Slice${p(SliceIdKey)}_Write_Prefetch_hart$hartId", new PrefetchDbEntry, basicDB = false)
    val pfReqReadTable = ChiselDB.createTable(s"L2_Slice${p(SliceIdKey)}_Read_Prefetch_hart$hartId", new PrefetchDbEntry, basicDB = false)
    val pfReqEvictTable = ChiselDB.createTable(s"L2_Slice${p(SliceIdKey)}_Evict_Prefetch_hart$hartId", new PrefetchDbEntry, basicDB = false)
    
    // Write: meta write that marks a block as prefetched
    val wmeta = io.metaWReq.bits.wmeta
    val pfReqWriteEn = io.metaWReq.valid && wmeta.prefetch.getOrElse(false.B) 
    val pfReqWrite = Wire(new PrefetchDbEntry)
    val writeHasTag = io.tagWReq.valid && (io.tagWReq.bits.set === io.metaWReq.bits.set) &&
      (io.metaWReq.bits.wayOH === io.tagWReq.bits.wayOH) // try to attach tag when tagWReq coincides with metaWReq

    pfReqWrite.isHit := false.B //useless for write req, just set it to false.B
    pfReqWrite.setIdx := io.metaWReq.bits.set // when meta write, the set idx to be written
    pfReqWrite.way := OHToUInt(io.metaWReq.bits.wayOH) // when meta write, way to be written
    pfReqWrite.tag := Mux(writeHasTag, io.tagWReq.bits.wtag, 0.U)
    pfReqWrite.isPrefetch := wmeta.prefetch.getOrElse(false.B) // write data is for a prefetched block or not
    pfReqWrite.metaSource := wmeta.prefetchSrc.getOrElse(defaultPfSrc) // source of the block that writed
    pfReqWrite.reqSource := wmeta.prefetchSrc.getOrElse(defaultPfSrc) // The source of the request that causes this meta write
    pfReqWriteTable.log(pfReqWrite, pfReqWriteEn, s"L2${hartId}_${p(SliceIdKey)}", clock, reset)

    // Read: when a read hits a prefetched block
    val pfReqReadEn = io.resp.valid && io.resp.bits.hit && io.resp.bits.meta.prefetch.getOrElse(false.B) 
    val pfReqRead = Wire(new PrefetchDbEntry)
    pfReqRead.isHit := io.resp.bits.hit // read req hit or not
    pfReqRead.setIdx := io.resp.bits.set
    pfReqRead.tag := io.resp.bits.tag // read req accsess a prefetched block with which tag
    pfReqRead.isPrefetch := io.resp.bits.meta.prefetch.getOrElse(false.B) // read req accsess a prefetched block
    pfReqRead.way := io.resp.bits.way // read req accsess a prefetched block in which way
    pfReqRead.metaSource := io.resp.bits.meta.prefetchSrc.getOrElse(defaultPfSrc) // source of the block that read req accessed
    pfReqRead.reqSource := io.resp.bits.replacerInfo.reqSource // The source of the read request (eg:bop,tp,prefetcher, etc.)
    pfReqReadTable.log(pfReqRead, pfReqReadEn, s"L2${hartId}_${p(SliceIdKey)}", clock, reset)

    // Eviction: when Directory issues a replacement for a prefetched block
    val evictBlockEn = io.replResp.valid && !io.replResp.bits.retry
    val evictBlockMeta = Mux1H(finalReplOH, metaAll_s3) // meta of the block to be evicted 
    val pfReqEvictEn = evictBlockEn && evictBlockMeta.prefetch.getOrElse(false.B)
    val pfReqEvict = Wire(new PrefetchDbEntry)

    // read request info that causes eviction
    pfReqEvict.isHit := io.resp.bits.hit // read request :read hit 
    pfReqEvict.reqSource := req_s3.replacerInfo.reqSource // The source of the read request that caused this Eviction
    
    // evict block info
    pfReqEvict.setIdx := io.replResp.bits.set //set idx of evict block 
    pfReqEvict.tag := io.replResp.bits.tag //tag of evict block 
    pfReqEvict.isPrefetch := io.replResp.bits.meta.prefetch.getOrElse(false.B)
    pfReqEvict.way := io.replResp.bits.way // way of evict block
    pfReqEvict.metaSource := io.replResp.bits.meta.prefetchSrc.getOrElse(defaultPfSrc) // source of the block that evicted
    pfReqEvictTable.log(pfReqEvict, pfReqEvictEn, s"L2${hartId}_${p(SliceIdKey)}", clock, reset)
  }
}
