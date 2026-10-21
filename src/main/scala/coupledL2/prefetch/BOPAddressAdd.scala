package xscache.coupledL2.prefetch

import chisel3._
import chisel3.util._

/** Byte address plus a signed cache-line offset, without a full-address ripple carry. */
object BOPAddressAdd {
  def apply(addr: UInt, offset: SInt, offsetBits: Int): UInt = {
    require(offsetBits > 0)
    val width = addr.getWidth - offsetBits
    require(width > 0 && offset.getWidth <= width)
    val lhs = addr(addr.getWidth - 1, offsetBits)
    val rhs = offset.pad(width).asUInt
    val chunkBits = 8
    val chunks = (0 until width by chunkBits).map { lo =>
      val hi = math.min(lo + chunkBits, width) - 1
      val a = lhs(hi, lo)
      val b = rhs(hi, lo)
      val sum0 = a +& b
      val sum1 = sum0 + 1.U
      (sum0, sum1, (a ^ b).andR, hi - lo + 1)
    }

    // Compute every group carry in parallel. Only the small local additions
    // ripple; the prefix depth grows logarithmically with the address width.
    var generate = chunks.map { case (sum0, _, _, bits) => sum0(bits) }
    var propagate = chunks.map(_._3)
    var distance = 1
    while (distance < chunks.size) {
      val nextGenerate = generate.indices.map { i =>
        if (i < distance) generate(i)
        else generate(i) || (propagate(i) && generate(i - distance))
      }
      val nextPropagate = propagate.indices.map { i =>
        if (i < distance) propagate(i)
        else propagate(i) && propagate(i - distance)
      }
      generate = nextGenerate
      propagate = nextPropagate
      distance *= 2
    }

    val sums = chunks.zipWithIndex.map { case ((sum0, sum1, _, bits), i) =>
      if (i == 0) sum0(bits - 1, 0)
      else Mux(generate(i - 1), sum1(bits - 1, 0), sum0(bits - 1, 0))
    }
    Cat(Cat(sums.reverse), addr(offsetBits - 1, 0))
  }
}
