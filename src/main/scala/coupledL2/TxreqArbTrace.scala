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
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, LogPerfIO}

class TxreqArbEvents(ports: Int) extends Bundle {
  val validMask = UInt(ports.W)
  val readyMask = UInt(ports.W)
  val outReady = Bool()
}

class TxreqArbRow(ports: Int) extends Bundle {
  val events = new TxreqArbEvents(ports)
  val nativeCycle = UInt(64.W)
  val clean = Bool()
  val nativeDump = Bool()
  val logEnable = Bool()
}

class TxreqArbLogger(ports: Int)(implicit p: Parameters) extends CoupledL2Module {
  val io = IO(new Bundle {
    val events = Input(new TxreqArbEvents(ports))
    val control = Input(new LogPerfIO)
  })
  val row = Wire(new TxreqArbRow(ports))
  row.events := io.events
  row.nativeCycle := io.control.timer
  row.clean := io.control.clean
  row.nativeDump := io.control.dump
  row.logEnable := io.control.logEnable
  val table = ChiselDB.createTable(s"${cacheParams.name}TxreqArbTrace", new TxreqArbRow(ports), basicDB = true)
  table.log(row, row.events.validMask.orR || row.clean || row.nativeDump, "CoupledL2", clock, reset)
}
