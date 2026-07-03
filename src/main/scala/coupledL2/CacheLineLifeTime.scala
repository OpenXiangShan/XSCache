/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of Mulan PSL v2.
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
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate

class CacheLineLifeTimeEvent(implicit p: Parameters) extends L2Bundle {
  val valid = Bool()
  val set = UInt(setBits.W)
  val way = UInt(wayBits.W)
}

class CacheLineLifeTimeFillEvent(implicit p: Parameters) extends CacheLineLifeTimeEvent {
  val isPrefetch = Bool()
  val isMergeA = Bool()
}

class CacheLineLifeTime(implicit p: Parameters) extends L2Module {
  val io = IO(new Bundle {
    val fill = Input(new CacheLineLifeTimeFillEvent)
    val access = Input(new CacheLineLifeTimeEvent)
    val snoopInvalid = Input(new CacheLineLifeTimeEvent)
  })

  val sInvalid :: sPrefetchWait :: sAcquireWait :: sAccessed :: Nil = Enum(4)

  val states = RegInit(VecInit(Seq.fill(cacheParams.sets)(VecInit(Seq.fill(cacheParams.ways)(sInvalid)))))
  val timestamps = RegInit(VecInit(Seq.fill(cacheParams.sets)(VecInit(Seq.fill(cacheParams.ways)(0.U(64.W))))))
  val globalTimer = RegInit(0.U(64.W))
  globalTimer := globalTimer + 1.U

  def wayIndex(way: UInt): UInt = if (cacheParams.ways == 1) 0.U(1.W) else way

  val fillWay = wayIndex(io.fill.way)
  val accessWay = wayIndex(io.access.way)
  val snoopInvalidWay = wayIndex(io.snoopInvalid.way)
  val fillState = states(io.fill.set)(fillWay)
  val accessState = states(io.access.set)(accessWay)
  val fillInterval = globalTimer - timestamps(io.fill.set)(fillWay)
  val accessInterval = globalTimer - timestamps(io.access.set)(accessWay)

  val prefetchPrepareTime = WireInit(0.U(64.W))
  val acquirePrepareTime = WireInit(0.U(64.W))
  val activeTime = WireInit(0.U(64.W))
  val deadTime = WireInit(0.U(64.W))
  val snpInvTime = WireInit(0.U(64.W))
  val stateSeq = Seq.tabulate(cacheParams.sets, cacheParams.ways) { case (set, way) => states(set)(way) }.flatten
  val invalidTime = PopCount(stateSeq.map(_ === sInvalid)).asUInt.pad(64)
  val unknownPrefetchWaitTimeCorrection = WireInit(0.U(64.W))
  val unknownAcquireWaitTimeCorrection = WireInit(0.U(64.W))
  val unknownAccessedTimeCorrection = WireInit(0.U(64.W))
  val unknownPrefetchWaitTime = PopCount(stateSeq.map(_ === sPrefetchWait)).asUInt.pad(64) + unknownPrefetchWaitTimeCorrection
  val unknownAcquireWaitTime = PopCount(stateSeq.map(_ === sAcquireWait)).asUInt.pad(64) + unknownAcquireWaitTimeCorrection
  val unknownAccessedTime = PopCount(stateSeq.map(_ === sAccessed)).asUInt.pad(64) + unknownAccessedTimeCorrection
  val unknownTime = unknownPrefetchWaitTime + unknownAcquireWaitTime + unknownAccessedTime

  def minus(value: UInt): UInt = (-(value.asSInt)).asUInt

  when (io.fill.valid) {
    when (fillState =/= sInvalid) {
      deadTime := fillInterval
      when (fillState === sPrefetchWait) {
        unknownPrefetchWaitTimeCorrection := minus(fillInterval)
      }.elsewhen (fillState === sAcquireWait) {
        unknownAcquireWaitTimeCorrection := minus(fillInterval)
      }.elsewhen (fillState === sAccessed) {
        unknownAccessedTimeCorrection := minus(fillInterval)
      }
    }
  }

  when (io.access.valid && !io.fill.valid && !io.snoopInvalid.valid) {
    when (accessState === sPrefetchWait) {
      prefetchPrepareTime := accessInterval
      unknownPrefetchWaitTimeCorrection := minus(accessInterval)
    }.elsewhen (accessState === sAcquireWait) {
      acquirePrepareTime := accessInterval
      unknownAcquireWaitTimeCorrection := minus(accessInterval)
    }.elsewhen (accessState === sAccessed) {
      activeTime := accessInterval
      unknownAccessedTimeCorrection := minus(accessInterval)
    }
   }

  when (io.snoopInvalid.valid) {
    val snoopInvalidState = states(io.snoopInvalid.set)(snoopInvalidWay)
    val snoopInvalidInterval = globalTimer - timestamps(io.snoopInvalid.set)(snoopInvalidWay)
    when (snoopInvalidState === sPrefetchWait) {
      snpInvTime := snoopInvalidInterval
      unknownPrefetchWaitTimeCorrection := minus(snoopInvalidInterval)
    }.elsewhen (snoopInvalidState === sAcquireWait) {
      snpInvTime := snoopInvalidInterval
      unknownAcquireWaitTimeCorrection := minus(snoopInvalidInterval)
    }.elsewhen (snoopInvalidState === sAccessed) {
      snpInvTime := snoopInvalidInterval
      unknownAccessedTimeCorrection := minus(snoopInvalidInterval)
    }
  }

  when (io.fill.valid) {
    // mergeA turns a prefetch refill into an accessed demand block.
    states(io.fill.set)(fillWay) := Mux(io.fill.isMergeA, sAccessed, Mux(io.fill.isPrefetch, sPrefetchWait, sAcquireWait))
    timestamps(io.fill.set)(fillWay) := globalTimer
  }.elsewhen (io.snoopInvalid.valid) {
    // Snoop invalid closes the line without charging the interval to dead time.
    states(io.snoopInvalid.set)(snoopInvalidWay) := sInvalid
    timestamps(io.snoopInvalid.set)(snoopInvalidWay) := globalTimer
  }.elsewhen (io.access.valid) {
    when (accessState === sPrefetchWait || accessState === sAcquireWait || accessState === sAccessed) {
      states(io.access.set)(accessWay) := sAccessed
      timestamps(io.access.set)(accessWay) := globalTimer
    }
  }

  XSPerfAccumulate("cacheline_prefetch_prepare_time", prefetchPrepareTime)
  XSPerfAccumulate("cacheline_acquire_prepare_time", acquirePrepareTime)
  XSPerfAccumulate("cacheline_active_time", activeTime)
  XSPerfAccumulate("cacheline_dead_time", deadTime)
  XSPerfAccumulate("cacheline_snp_inv_time", snpInvTime)
  XSPerfAccumulate("cacheline_invalid_time", invalidTime)
  XSPerfAccumulate("cacheline_unknown_prefetch_wait_time", unknownPrefetchWaitTime)
  XSPerfAccumulate("cacheline_unknown_acquire_wait_time", unknownAcquireWaitTime)
  XSPerfAccumulate("cacheline_unknown_accessed_time", unknownAccessedTime)
  XSPerfAccumulate("cacheline_unknown_time", unknownTime)
  XSPerfAccumulate("total_time", (cacheParams.sets * cacheParams.ways).U)
  XSPerfAccumulate("cacheline_accounted_time", prefetchPrepareTime + acquirePrepareTime + activeTime + deadTime + snpInvTime + invalidTime + unknownTime)
}
