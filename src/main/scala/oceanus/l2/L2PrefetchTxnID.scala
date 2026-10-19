package oceanus.l2

import chisel3._
import chisel3.util.Cat

// Internal CCHI requests carry [pfSource(4) | sequence]. This is provenance,
// not a downstream transaction ID; a four-bit TxnID has no sequence field.
// Keep packing and extraction together so additional producers share the layout.
object L2PrefetchTxnID {
  def sequenceWidth(txnWidth: Int): Int = {
    require(txnWidth >= L2PfSource.Width,
      s"Internal prefetch TxnID needs at least ${L2PfSource.Width} bits, got $txnWidth")
    txnWidth - L2PfSource.Width
  }

  def encode(source: UInt, sequence: UInt, txnWidth: Int): UInt = {
    val lowWidth = sequenceWidth(txnWidth)
    require(source.getWidth == L2PfSource.Width)
    if (lowWidth == 0) source else Cat(source, sequence.pad(lowWidth)(lowWidth - 1, 0))
  }

  def decode(txnID: UInt): UInt = {
    val lowWidth = sequenceWidth(txnID.getWidth)
    txnID(txnID.getWidth - 1, lowWidth)
  }
}
