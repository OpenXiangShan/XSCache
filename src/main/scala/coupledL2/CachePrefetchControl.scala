// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package xscache.coupledL2

import chisel3._
import chisel3.util._
import utility.Constantin

/** One startup setting per hart for cache prefetch other than ICache FDIP. */
object CachePrefetchControl {
  def name(hartId: Int): String = s"cache_prefetch_enable$hartId"

  // With Constantin disabled this is the elaboration-time default. With it
  // enabled, every consumer reads the same startup value from the cst file.
  def enabled(hartId: Int, defaultEnable: Boolean): Bool =
    Constantin.createRecord(name(hartId), defaultEnable)

  /** Consume disabled prefetches without blocking their producer. */
  def gate[T <: Data](source: DecoupledIO[T], enable: Bool): DecoupledIO[T] = {
    val request = Wire(Decoupled(chiselTypeOf(source.bits)))
    request.bits := source.bits
    request.valid := source.valid && enable
    source.ready := !enable || request.ready
    request
  }
}
