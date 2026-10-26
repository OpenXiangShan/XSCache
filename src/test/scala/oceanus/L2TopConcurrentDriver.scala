package oceanus

import chisel3._
import chisel3.simulator.ChiselSim
import oceanus.compactchi.CCHIOpcode
import scala.collection.mutable

/** Read-only backing memory: all 32 bytes depend on address and byte position.
  * Both demand and internal-prefetch refills use it, including cache readbacks.
  * No expected value is learned from DUT responses or downstream TxnIDs. */
object L2TopReferenceMemory {
  def beat(addr: BigInt, beat: Int): BigInt = (0 until 32).foldLeft(BigInt(0)) { (data, byte) =>
    val line = addr >> 6
    val value = ((line * 131 + (line >> 8) * 17 + beat * 71 + byte * 29) & 255).toInt
    data | (BigInt(value) << (byte * 8))
  }
}

/** Independent per-port TxnID allocation, response matching, and two-beat
  * scoreboarding. Requests remain stable until handshake; completion on one
  * port never gates request issue on another port. Type-1 completes on the
  * accepted CompAck; Type-4 has no RSP channel and completes on its last DAT.
  */
class L2TopConcurrentDriver(dut: TestTop_L2Top, upstream: L2TopUpstreamModel, type1Ports: Int = 1) extends ChiselSim {
  require(type1Ports == 1 || type1Ports == 2)
  val masters = Seq(MasterId.t1p0, MasterId.t4p0, MasterId.t4p1) ++
    (if (type1Ports == 2) Seq(MasterId.t1p1) else Seq.empty)
  private def type1Index(master: MasterId): Int = if (master == MasterId.T1P1) 1 else 0
  private def isType1(master: MasterId): Boolean = master == MasterId.T1P0 || master == MasterId.T1P1
  private def reqPort(master: MasterId) = master match {
    case MasterId.T1P0 => dut.io.t1p(0).UpREQ
    case MasterId.T1P1 => dut.io.t1p(1).UpREQ
    case MasterId.T4P0 => dut.io.t4p(0).UpREQ
    case MasterId.T4P1 => dut.io.t4p(1).UpREQ
  }
  private def dataPort(master: MasterId) = master match {
    case MasterId.T1P0 => dut.io.t1p(0).DnDAT
    case MasterId.T1P1 => dut.io.t1p(1).DnDAT
    case MasterId.T4P0 => dut.io.t4p(0).DnDAT
    case MasterId.T4P1 => dut.io.t4p(1).DnDAT
  }
  private val queues = masters.map(_ -> mutable.Queue.empty[UpRequestSpec]).toMap
  private val nextID = mutable.Map(masters.map(_ -> 0): _*)
  private case class Read(req: UpRequestSpec, beats: mutable.Set[Int] = mutable.Set.empty)
  private val active = mutable.Map.empty[(MasterId, BigInt), Read]
  private val held = mutable.Map.empty[MasterId, UpRequestSpec]
  private case class Ack(key: (MasterId, BigInt), dbid: BigInt, slice: BigInt)
  private val acks = mutable.Queue.empty[Ack]
  val accepted = mutable.ArrayBuffer.empty[(Int, UpRequestSpec)]
  var completed = 0
  var checkedBeats = 0
  var maxOutstanding = 0
  def outstanding: Int = active.size
  def pending: Int = active.size + held.size + queues.values.map(_.size).sum + acks.size
  def queued: Int = held.size + queues.values.map(_.size).sum
  def enqueue(requests: Seq[UpRequestSpec]): Unit = {
    // Type-4 whitelist: CHIron cchi_protocol_encoding.hpp:175-187.
    // Use the upstream drive boundary check; validate the whole
    // batch first so an illegal request cannot partially enqueue a batch.
    requests.foreach(L2TopUpstreamLegality.check)
    requests.foreach(r => queues(r.master).enqueue(r))
  }
  def drive(cycle: Int): Unit = {
    upstream.clearUpReq()
    masters.foreach { master =>
      if (!held.contains(master) && queues(master).nonEmpty) {
        val count = 1 << reqPort(master).bits.TxnID.getWidth
        (0 until count).map(i => (nextID(master) + i) % count)
          .find(id => !active.contains((master, BigInt(id)))).foreach { id =>
            val src = master match {
              case MasterId.T1P0 => 0
              case MasterId.T1P1 => 2
              case MasterId.T4P0 => 4
              case MasterId.T4P1 => 5
            }
            held(master) = queues(master).dequeue().copy(txnId = id, srcId = src,
              expCompData = isType1(master))
          }
      }
      held.get(master).foreach { req =>
        L2TopUpstreamLegality.check(req)
        upstream.driveRead(req)
      }
    }
    // Existing acks only: a data response observed below cannot be acked at
    // the same edge, and DBID is always qualified by its source slice.
    acks.groupBy(a => type1Index(a.key._1)).values.foreach(_.headOption.foreach(a =>
      upstream.driveUpCompAck(UpDBID(a.dbid), a.slice.toInt, type1Index(a.key._1))))
  }
  def sample(cycle: Int): Unit = {
    (0 until type1Ports).foreach { idx =>
      val port = dut.io.t1p(idx).UpRSP
      if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
        acks.dequeueFirst(a => type1Index(a.key._1) == idx).foreach { ack =>
          assert(active.remove(ack.key).nonEmpty)
          completed += 1
        }
      }
    }
    masters.foreach { master =>
      val port = reqPort(master)
      if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
        val request = held.remove(master).get
        L2TopUpstreamLegality.check(request)
        val key = (master, request.txnId)
        assert(!active.contains(key), s"TxnID reused while active: $key")
        active(key) = Read(request)
        nextID(master) = (request.txnId.toInt + 1) % (1 << port.bits.TxnID.getWidth)
        accepted += ((cycle, request))
        maxOutstanding = maxOutstanding.max(active.size)
        println(s"CONCURRENT_ACCEPT cycle=$cycle master=${master.name} txn=${request.txnId} addr=0x${request.addr.toString(16)}")
      }
      val data = dataPort(master)
      if (data.valid.peek().litToBoolean && data.ready.peek().litToBoolean) {
        val id = data.bits.TxnID.peek().litValue
        val key = (master, id)
        val read = active.getOrElse(key, throw new AssertionError(s"unmatched upstream response: $key"))
        assert(data.bits.Opcode.peek().litValue == CCHIOpcode.CompData.opcode)
        val beat = data.bits.DataID.peek().litValue.toInt
        assert(Set(0, 1).contains(beat) && !read.beats(beat), s"duplicate/invalid beat: $key beat=$beat")
        val actual = data.bits.Data.peek().litValue
        val expected = L2TopReferenceMemory.beat(read.req.addr, beat)
        assert(actual == expected, s"data mismatch $key addr=0x${read.req.addr.toString(16)} beat=$beat actual=0x${actual.toString(16)} expected=0x${expected.toString(16)}")
        read.beats += beat
        checkedBeats += 1
        if (read.beats.size == 2) {
          if (isType1(master)) {
            acks.enqueue(Ack(key, data.bits.DBID.peek().litValue, data.bits.SrcID.peek().litValue))
          } else {
            active.remove(key)
            completed += 1
          }
        }
      }
    }
  }
}
