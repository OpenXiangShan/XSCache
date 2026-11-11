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
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, LogPerfIO, MemReqSource}

class MshrLifecycleAllocation(implicit p: Parameters) extends L2Bundle {
  val entry = UInt(mshrBits.W)
  val line = UInt((tagBits + setBits).W)
  val source = UInt(MemReqSource.reqSourceBits.W)
  val channel = UInt(3.W)
  val prefetch = Bool()
  val miss = Bool()
}

class MshrLifecycleMerge(implicit p: Parameters) extends L2Bundle {
  val entry = UInt(mshrBits.W)
  val line = UInt((tagBits + setBits).W)
  val source = UInt(MemReqSource.reqSourceBits.W)
}

class MshrLifecycleEvents(implicit p: Parameters) extends L2Bundle {
  val bank = UInt(bankBits.W)
  val allocated = ValidIO(new MshrLifecycleAllocation)
  val merged = ValidIO(new MshrLifecycleMerge)
  val freed = Vec(mshrsAll, ValidIO(UInt(64.W)))
}

class MshrLifecycleRow(implicit p: Parameters) extends L2Bundle {
  val events = new MshrLifecycleEvents
  val nativeCycle = UInt(64.W)
  val clean = Bool()
  val nativeDump = Bool()
  val logEnable = Bool()
}

class MshrLifecycleLogger(implicit p: Parameters) extends CoupledL2Module {
  val io = IO(new Bundle {
    val events = Input(new MshrLifecycleEvents)
    val control = Input(new LogPerfIO)
  })
  val row = Wire(new MshrLifecycleRow)
  row.events := io.events
  row.nativeCycle := io.control.timer
  row.clean := io.control.clean
  row.nativeDump := io.control.dump
  row.logEnable := io.control.logEnable
  val changed = io.events.allocated.valid || io.events.merged.valid ||
    io.events.freed.map(_.valid).reduce(_ || _) || row.clean || row.nativeDump
  val table = ChiselDB.createTable(s"${cacheParams.name}MshrLifecycle", new MshrLifecycleRow, basicDB = true)
  table.log(row, changed, "MSHRCtl", clock, reset)
}
