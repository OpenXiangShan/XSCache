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

package xscache.common

import chisel3._
import freechips.rocketchip.util.{BundleField, ControlKey}

case object PrefetchKey extends ControlKey[Bool](name = "needHint")

case class PrefetchField() extends BundleField[Bool](PrefetchKey, Output(Bool()), _ := false.B)

// L1 DCache writes this TileLink A user field for an MDP-hinted demand load;
// SinkA consumes it and stores the marker in the L2 task.
case object MdpHintKey extends ControlKey[Bool](name = "mdpHint")

case class MdpHintField() extends BundleField[Bool](MdpHintKey, Output(Bool()), _ := false.B)

// The remaining fields describe the exact hinted load. L1 writes them on the
// TileLink A request and L2 keeps them until refill data is sent to L2 MDP.
case object MdpImmKey extends ControlKey[UInt](name = "mdpImm")
case class MdpImmField(width: Int) extends BundleField[UInt](MdpImmKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object MdpChainImmKey extends ControlKey[UInt](name = "mdpChainImm")
case class MdpChainImmField(width: Int)
  extends BundleField[UInt](MdpChainImmKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object MdpChainValidKey extends ControlKey[Bool](name = "mdpChainValid")
case class MdpChainValidField()
  extends BundleField[Bool](MdpChainValidKey, Output(Bool()), _ := false.B)

case object MdpChainLoadSizeKey extends ControlKey[UInt](name = "mdpChainLoadSize")
case class MdpChainLoadSizeField()
  extends BundleField[UInt](MdpChainLoadSizeKey, Output(UInt(2.W)), _ := 0.U(2.W))

case object MdpChainLoadUnsignedKey extends ControlKey[Bool](name = "mdpChainLoadUnsigned")
case class MdpChainLoadUnsignedField()
  extends BundleField[Bool](MdpChainLoadUnsignedKey, Output(Bool()), _ := false.B)

case object MdpOriginKey extends ControlKey[UInt](name = "mdpOrigin")
case class MdpOriginField(width: Int)
  extends BundleField[UInt](MdpOriginKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object MdpVaddrKey extends ControlKey[UInt](name = "mdpVaddr")
case class MdpVaddrField(width: Int) extends BundleField[UInt](MdpVaddrKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object MdpPCKey extends ControlKey[UInt](name = "mdpPC")
case class MdpPCField(width: Int) extends BundleField[UInt](MdpPCKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object MdpLoadSizeKey extends ControlKey[UInt](name = "mdpLoadSize")
case class MdpLoadSizeField() extends BundleField[UInt](MdpLoadSizeKey, Output(UInt(2.W)), _ := 0.U(2.W))

case object MdpLoadUnsignedKey extends ControlKey[Bool](name = "mdpLoadUnsigned")
case class MdpLoadUnsignedField() extends BundleField[Bool](MdpLoadUnsignedKey, Output(Bool()), _ := false.B)

case object DirtyKey extends ControlKey[Bool](name = "dirty")

case class DirtyField() extends BundleField[Bool](DirtyKey, Output(Bool()), _ := false.B)

case object AliasKey extends ControlKey[UInt]("alias")

case class AliasField(width: Int) extends BundleField[UInt](AliasKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object IsHitKey extends ControlKey[Bool](name = "isHitInL3")

case class IsHitField() extends BundleField[Bool](IsHitKey, Output(Bool()), _ := true.B)
