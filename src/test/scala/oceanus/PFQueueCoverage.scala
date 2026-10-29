package oceanus

import chisel3._
import scala.collection.mutable

/** Passive host FIFO oracle on production queues; no stimulus or stepping. */
final class PFQueueCoverage(obs: Phase2Observation, name: String, legacy: Boolean = false) extends chisel3.simulator.ChiselSim {
  private val cov = new PFCoverage(name)
  private val models = Vector.fill(obs.queues.size)(mutable.Queue.empty[(BigInt, BigInt)])
  private val oldTail = Array.fill(obs.queues.size)(0)
  private val enqueued = Array.fill(obs.queues.size)(0L)
  private val entered = Array.fill(obs.queues.size)(0L)
  private val lastIssued = Array.fill[Option[BigInt]](obs.slices)(None)
  private val producers = Map(1 -> "vbop", 2 -> "pbop", 8 -> "cdp", (if(legacy) 0 else 9) -> "recv")
  private def flag(x: Bool) = x.peek().litToBoolean
  private def value(x: UInt) = x.peek().litValue
  def sample(): Unit = {
    for ((q, i) <- obs.queues.zipWithIndex) {
      val capacity = obs.capacity(i); val bank = i / obs.queuesPerBank; val local = i % obs.queuesPerBank
      val model = models(i)
      val tail = value(q.tail).toInt
      val occupancy = if (flag(q.full)) capacity else (tail - value(q.head).toInt + capacity) % capacity
      assert(occupancy == model.size && flag(q.deq.valid) == model.nonEmpty)
      if (model.nonEmpty) assert(value(q.deqPacked) == model.head._1)
      val take = flag(q.take); val put = flag(q.enq.valid)
      val overwrite = put && model.size == capacity && !take
      assert(flag(q.overwrite) == overwrite)
      producers.get(local).foreach { producer =>
        if (tail < oldTail(i) && enqueued(i) >= 2 * capacity) cov.hit(s"D.$producer.wrap")
        if (put && take) cov.hit(s"D.$producer.simultaneous")
        if (overwrite && (!legacy || producer == "recv")) cov.hit(s"D.$producer.overflow")
      }
      oldTail(i) = tail
      if (take) {
        val (packed, addr) = model.dequeue()
        val issue = if (local == 9) flag(obs.enter(bank).valid) && value(obs.enterPacked(bank)) == packed
          else flag(obs.issue(bank).valid) && value(obs.issuePacked(bank)) == packed
        if (issue) entered(i) += 1
        else producers.get(local).foreach { producer =>
          if (flag(obs.issue(bank).valid) && value(obs.issue(bank).bits.Addr) == addr)
            cov.hit(s"D.$producer.chosen")
          else if (lastIssued(bank).contains(addr)) cov.hit(s"D.$producer.last")
          else assert(false, s"queue $i removed an item without issue/dedup")
        }
      }
      if (overwrite) model.dequeue()
      if (put) {
        enqueued(i) += 1
        model.enqueue((value(q.enqPacked), value(q.enq.bits.Addr)))
      }
    }
    for (bank <- 0 until obs.slices) if (flag(obs.issue(bank).valid)) lastIssued(bank) = Some(value(obs.issue(bank).bits.Addr))
  }
  def finish(): Unit = {
    assert(models.forall(_.isEmpty))
    producers.foreach { case (i, producer) => if ((0 until obs.slices).exists(b => entered(b*obs.queuesPerBank+i) > 0)) cov.hit(s"D.$producer.drain") }
    cov.finish()
  }
}
