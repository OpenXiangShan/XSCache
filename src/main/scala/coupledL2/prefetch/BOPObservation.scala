/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND.
 * *************************************************************************************
 */

package xscache.coupledL2.prefetch

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.ChiselDB
import utility.XSPerfAccumulate

object BOPChoice {
  val offsetBits = 16
}

// Read-only observation; issueEnable excludes the external emission enable, so a disabled
// engine can still report its learned choice. Offsets are signed cache-line distances.
class BOPChoice extends Bundle {
  val teacherOffset      = SInt(BOPChoice.offsetBits.W)
  val teacherEnable      = Bool()
  val teacherPhaseOffset = SInt(BOPChoice.offsetBits.W)
  val studentOffset      = SInt(BOPChoice.offsetBits.W)
  val studentValid       = Bool()
  val studentEnable      = Bool()
  val issueOffset        = SInt(BOPChoice.offsetBits.W)
  val issueEnable        = Bool()
  val phaseEnd           = Bool()
  val trainFire          = Bool()
  val trainAddr          = UInt(64.W)
  val sentFire           = Bool()
  val sentOffset         = SInt(BOPChoice.offsetBits.W)
}

// ChiselDB writers accept raw UInt fields; cast signed values in the parent module.
class BOPChoiceEntry extends Bundle {
  val teacherOffset      = UInt(BOPChoice.offsetBits.W)
  val teacherEnable      = Bool()
  val teacherPhaseOffset = UInt(BOPChoice.offsetBits.W)
  val studentOffset      = UInt(BOPChoice.offsetBits.W)
  val studentValid       = Bool()
  val studentEnable      = Bool()
  val issueOffset        = UInt(BOPChoice.offsetBits.W)
  val issueEnable        = Bool()
  val phaseEnd           = Bool()
  val trainFire          = Bool()
  val trainAddr          = UInt(64.W)
  val sentFire           = Bool()
  val sentOffset         = UInt(BOPChoice.offsetBits.W)

  def capture(choice: BOPChoice): Unit = {
    teacherOffset      := choice.teacherOffset.asUInt
    teacherEnable      := choice.teacherEnable
    teacherPhaseOffset := choice.teacherPhaseOffset.asUInt
    studentOffset      := choice.studentOffset.asUInt
    studentValid       := choice.studentValid
    studentEnable      := choice.studentEnable
    issueOffset        := choice.issueOffset.asUInt
    issueEnable        := choice.issueEnable
    phaseEnd           := choice.phaseEnd
    trainFire          := choice.trainFire
    trainAddr          := choice.trainAddr
    sentFire           := choice.sentFire
    sentOffset         := choice.sentOffset.asUInt
  }
}

class BOPComparisonEntry extends Bundle {
  val pbop = new BOPChoiceEntry
  val vbop = new BOPChoiceEntry
  val ubop = new BOPChoiceEntry
}

class BOPComparisonMonitor(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val pbop = Input(new BOPChoice)
    val vbop = Input(new BOPChoice)
    val ubop = Input(new BOPChoice)
  })

  private def compare(prefix: String, event: Bool, available: Bool, actual: SInt, reference: SInt): Unit = {
    XSPerfAccumulate(prefix + "_comparable", event && available)
    XSPerfAccumulate(prefix + "_same", event && available && actual === reference)
    XSPerfAccumulate(prefix + "_different", event && available && actual =/= reference)
    XSPerfAccumulate(prefix + "_unavailable", event && !available)
  }

  // Compare the choice used at UBOP's training handshake with the live shadow choices.
  // Shadow engines may accept different inputs; equality here is numerical, not address equality.
  val trainChoice = io.ubop.trainFire && io.ubop.issueEnable
  for ((name, shadow) <- Seq("pbop" -> io.pbop, "vbop" -> io.vbop)) {
    compare(
      "ubop_train_vs_" + name + "_student",
      trainChoice,
      shadow.studentEnable,
      io.ubop.issueOffset,
      shadow.studentOffset
    )
    compare(
      "ubop_req_vs_" + name + "_student",
      io.ubop.sentFire,
      shadow.studentEnable,
      io.ubop.sentOffset,
      shadow.studentOffset
    )
    compare(
      "ubop_req_vs_" + name + "_teacher",
      io.ubop.sentFire,
      shadow.teacherEnable,
      io.ubop.sentOffset,
      shadow.teacherOffset
    )
    compare(
      "ubop_req_vs_" + name + "_choice",
      io.ubop.sentFire,
      shadow.issueEnable,
      io.ubop.sentOffset,
      shadow.issueOffset
    )
  }
  val bothStudents = io.ubop.sentFire && io.pbop.studentEnable && io.vbop.studentEnable
  val sameP        = io.ubop.sentOffset === io.pbop.studentOffset
  val sameV        = io.ubop.sentOffset === io.vbop.studentOffset
  XSPerfAccumulate("ubop_req_both_students_comparable", bothStudents)
  XSPerfAccumulate("ubop_req_same_both_students", bothStudents && sameP && sameV)
  XSPerfAccumulate("ubop_req_same_only_pbop_student", bothStudents && sameP && !sameV)
  XSPerfAccumulate("ubop_req_same_only_vbop_student", bothStudents && !sameP && sameV)
  XSPerfAccumulate("ubop_req_same_neither_student", bothStudents && !sameP && !sameV)

  // Enabled only in ChiselDB builds/runs; snapshots share a clock/STAMP and include
  // accepted training addresses, phase boundaries, fallback state and actual UBOP issue offset.
  val table = ChiselDB.createTable("BOPComparisonTable", new BOPComparisonEntry, basicDB = true)
  val entry = Wire(new BOPComparisonEntry)
  entry.pbop.capture(io.pbop)
  entry.vbop.capture(io.vbop)
  entry.ubop.capture(io.ubop)
  val event = io.pbop.phaseEnd || io.vbop.phaseEnd || io.ubop.phaseEnd ||
    io.pbop.trainFire || io.vbop.trainFire || io.ubop.trainFire || io.ubop.sentFire
  table.log(entry, event, site = "BOPComparison", clock, reset)
}
