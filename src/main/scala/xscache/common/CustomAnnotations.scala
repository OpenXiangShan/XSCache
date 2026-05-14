package xscache.common

import chisel3.Module

case class SRAMClkDivBy2Annotation(mod: firrtl.annotations.ModuleName)
  extends firrtl.annotations.SingleTargetAnnotation[firrtl.annotations.ModuleName] {
  override val target: firrtl.annotations.ModuleName = mod

  override def duplicate(n: firrtl.annotations.ModuleName): firrtl.annotations.Annotation = this.copy(n)
}

case class SRAMSpecialDepthAnnotation(mod: firrtl.annotations.ModuleName)
  extends firrtl.annotations.SingleTargetAnnotation[firrtl.annotations.ModuleName] {
  override val target: firrtl.annotations.ModuleName = mod

  override def duplicate(n: firrtl.annotations.ModuleName): firrtl.annotations.Annotation = this.copy(n)
}

object CustomAnnotations {
  def annotateClkDivBy2(mod: Module): Unit = {
    chisel3.experimental.annotate(mod)(Seq(SRAMClkDivBy2Annotation(mod.toNamed)))
  }

  def annotateSpecialDepth(mod: Module): Unit = {
    chisel3.experimental.annotate(mod)(Seq(SRAMSpecialDepthAnnotation(mod.toNamed)))
  }
}
