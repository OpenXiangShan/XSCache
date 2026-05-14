package xscache.common

import chisel3._
import freechips.rocketchip.util.{BundleField, ControlKey}

case object PrefetchKey extends ControlKey[Bool](name = "needHint")

case class PrefetchField() extends BundleField[Bool](PrefetchKey, Output(Bool()), _ := false.B)

case object AliasKey extends ControlKey[UInt]("alias")

case class AliasField(width: Int) extends BundleField[UInt](AliasKey, Output(UInt(width.W)), _ := 0.U(width.W))

case object IsHitKey extends ControlKey[Bool](name = "isHitInL3")

case class IsHitField() extends BundleField[Bool](IsHitKey, Output(Bool()), _ := true.B)
