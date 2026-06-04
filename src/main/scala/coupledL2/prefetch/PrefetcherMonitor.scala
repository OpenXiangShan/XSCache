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
import utility._

class PrefetcherMonitor(implicit p: Parameters) extends PrefetchModule {
  val banks = 1 << bankBits

  val io = IO(new Bundle {
    val sliceStat = Input(Vec(banks, new PrefetchStatDelta))
    val stat = Output(new PrefetchStat)
  })

  io.stat := 0.U.asTypeOf(new PrefetchStat)

  for (pfSrcId <- 0 until PfSource.PfSourceCount.id) {
    val sentAcc = RegInit(0.U(64.W))
    val hitAcc  = RegInit(0.U(64.W))

    val sentThisCycle = io.sliceStat.map(_.pfSentVec(pfSrcId)).reduce(_ +& _)
    val hitThisCycle  = io.sliceStat.map(_.pfHitVec(pfSrcId)).reduce(_ +& _)

    sentAcc := sentAcc + sentThisCycle
    hitAcc  := hitAcc + hitThisCycle

    io.stat.pfSentVec(pfSrcId) := sentAcc
    io.stat.pfHitVec(pfSrcId) := hitAcc
  }
}
