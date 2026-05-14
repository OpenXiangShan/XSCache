package xscache.common

import chisel3.util.log2Ceil
import org.chipsalliance.cde.config.Field

case class CacheParameters(
  name: String,
  sets: Int,
  ways: Int,
  blockGranularity: Int,
  blockBytes: Int = 64,
  aliasBitsOpt: Option[Int] = None,
  inner: Seq[CacheParameters] = Nil
) {
  val capacity = sets * ways * blockBytes
  val setBits = log2Ceil(sets)
  val offsetBits = log2Ceil(blockBytes)
  val needResolveAlias = aliasBitsOpt.nonEmpty
}

case object BankBitsKey extends Field[Int]
