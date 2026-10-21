package oceanus.l2

// Intent (1c-2): centralize source4/depth2/sequence packing; legacy producers
// explicitly default to depth zero while CDP can carry recursion depth.

import chisel3._
import chisel3.util.Cat

// Internal CCHI requests carry [pfSource(4) | depth(2) | sequence]. This is
// provenance, not a downstream transaction ID (8 bits gives 2 sequence bits).
// Keep packing and extraction together so additional producers share the layout.
object L2PrefetchTxnID {
  def sequenceWidth(txnWidth: Int): Int = {
    require(txnWidth >= L2PfSource.Width + 2,
      s"Internal prefetch TxnID needs at least ${L2PfSource.Width + 2} bits, got $txnWidth")
    txnWidth - L2PfSource.Width - 2
  }

  def encode(source: UInt, sequence: UInt, txnWidth: Int): UInt = {
    encode(source, 0.U(2.W), sequence, txnWidth)
  }

  def encode(source: UInt, depth: UInt, sequence: UInt, txnWidth: Int): UInt = {
    val lowWidth = sequenceWidth(txnWidth)
    require(source.getWidth == L2PfSource.Width)
    require(depth.getWidth == 2)
    if (lowWidth == 0) Cat(source, depth) else Cat(source, depth, sequence.pad(lowWidth)(lowWidth - 1, 0))
  }

  def decode(txnID: UInt): UInt = {
    val lowWidth = sequenceWidth(txnID.getWidth)
    txnID(txnID.getWidth - 1, lowWidth + 2)
  }

  def depth(txnID: UInt): UInt = {
    val lowWidth = sequenceWidth(txnID.getWidth)
    txnID(lowWidth + 1, lowWidth)
  }
}
