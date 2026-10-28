package oceanus

import scala.collection.mutable

// Host-only identity scoreboard for the recorded C5 system and reference replay.
// Allocation-cycle identity distinguishes repeated requests to the same PA/depth.
// Count only ordinary random epochs; still track every epoch to check ownership.
class CDPC5Dispositions(side: String, physical: (Int, BigInt) => BigInt) {
  case class Item(cycle: Int, epoch: Int, va: BigInt, depth: BigInt)
  private val live = mutable.Map.empty[Int, Item]
  private val issued = mutable.Map.empty[Int, mutable.ArrayBuffer[String]]
  private val counts = mutable.Map.empty[Int, mutable.Map[String, Long]]
  private def count(epoch: Int, key: String, n: Long = 1): Unit = {
    val c = counts.getOrElseUpdate(epoch, mutable.Map.empty[String, Long].withDefaultValue(0L))
    c(key) += n
  }
  def sample(cycle: Int, epoch: Int, r: Map[String, BigInt]): Unit = {
    def valid(n: String): Boolean = r(n) != 0
    def check(ok: Boolean, message: String): Unit =
      assert(ok, s"C5 disposition side=$side cycle=$cycle epoch=$epoch $message")
    check(r("h2.valid") == live.keys.map(i => BigInt(1) << i).foldLeft(BigInt(0))(_ | _), "live slot ownership")
    count(epoch, "cycles")
    if(!valid("enable")) count(epoch, "disabledCycles")
    if(r.contains("filterAccepted")) count(epoch, "naturalFilterTrainAccepted", r("filterAccepted").toLong)
    if(valid("candidate")) count(epoch, "candidates")
    for(k <- Seq("allocated", "duplicate", "full")) if(valid(k)) count(epoch, k)
    for(i <- 0 until 16 if valid(s"translationDrop$i")) {
      val it = live.remove(i).getOrElse(throw new AssertionError(s"$side translation retired absent slot $i at $cycle"))
      check(it.epoch == epoch && it.va == r(s"translationDrop$i.va") && it.depth == r(s"translationDrop$i.depth"), "translation identity")
      count(epoch, "translationDrops")
    }
    if(valid("filterDrop") || valid("out")) {
      check(valid("h2.query2"), "retirement without two-cycle Filter completion")
      val index = r("h2.query2.index").toInt
      val it = live.remove(index).getOrElse(throw new AssertionError(s"$side completion retired absent slot $index at $cycle"))
      check(it.epoch == epoch, "candidate crossed a drained epoch")
      check(valid("filterDrop") != valid("out"), "two dispositions for one completion")
      if(valid("filterDrop")) {
        check(!valid("h2.query2.canPft") && it.va == r("filterDrop.va") && it.depth == r("filterDrop.depth"), "Filter identity")
        count(epoch, "filterDrops")
      } else {
        check(valid("h2.query2.canPft") && valid("enable"), "unqualified output")
        check(it.depth == r("out.depth") && physical(it.epoch, it.va) == r("out.pa"), "output PA/depth")
        issued.getOrElseUpdate(epoch, mutable.ArrayBuffer.empty[String]) += s"${it.cycle}:${r("out.pa")}:${it.depth}"
        count(epoch, "outputs")
      }
    }
    if(valid("allocated")) {
      val index = r("h2.index").toInt
      check(!live.contains(index), "allocation reused live slot")
      live(index) = Item(cycle, epoch, r("candidate.va") & ~BigInt(63), r("candidate.depth"))
    }
  }
  def finish(): Unit = assert(live.isEmpty, s"C5 disposition unfinished $side: $live")
  def outputs(epoch: Int): Vector[String] = issued.get(epoch).map(_.toVector).getOrElse(Vector.empty)
  def counters(epoch: Int): ujson.Obj = ujson.Obj.from(counts.get(epoch).toSeq.flatMap(_.toSeq).map {
    case (k, v) => k -> ujson.Num(v.toDouble)
  })
}

object CDPC5Dispositions {
  // A bag rather than a PA set: duplicate addresses and candidate identity matter.
  def compare(dut: Seq[String], reference: Seq[String]): ujson.Obj = {
    def bag(xs: Seq[String]): Map[String, Int] = xs.groupMapReduce(identity)(_ => 1)(_ + _)
    val d = bag(dut); val r = bag(reference)
    def excess(a: Map[String, Int], b: Map[String, Int]): Vector[String] =
      a.toVector.sortBy(_._1).flatMap { case (k, n) => Vector.fill((n - b.getOrElse(k, 0)).max(0))(k) }
    val extra = excess(d, r); val missing = excess(r, d)
    val union = (d.keySet ++ r.keySet).toVector.map(k => d.getOrElse(k, 0).max(r.getOrElse(k, 0))).sum
    ujson.Obj("dutOutputs" -> dut.size, "referenceOutputs" -> reference.size,
      "unionOutputs" -> union, "dutExtra" -> extra.size, "dutMissing" -> missing.size,
      "differentFractionOfUnion" -> (if(union == 0) 0.0 else (extra.size + missing.size).toDouble / union),
      "extraIdentities" -> ujson.Arr.from(extra), "missingIdentities" -> ujson.Arr.from(missing))
  }
}
