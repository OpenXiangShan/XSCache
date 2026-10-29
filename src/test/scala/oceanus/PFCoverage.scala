package oceanus

import scala.collection.mutable

/** Host-only semantic coverage. No Chisel signals, pokes, steps, or feedback.
  * Call hit only after the owning independent assertion has succeeded. A
  * failed invocation is excluded by the runner, even if it printed earlier.
  */
final class PFCoverage(val name: String) {
  private val counts = mutable.Map.empty[String, Long].withDefaultValue(0L)
  def hit(id: String, count: Long = 1L): Unit = {
    require(PFCoverage.ids.contains(id), s"unknown coverage ID: $id")
    require(count >= 0)
    counts(id) += count
  }
  def finish(): Unit = {
    println(s"PF_COVER_BEGIN case=$name points=${PFCoverage.ids.size}")
    PFCoverage.points.foreach { case (id, priority, category) =>
      println(s"PF_COVER case=$name id=$id priority=$priority category=$category count=${counts(id)}")
    }
    println(s"PF_COVER_END case=$name")
  }
}

object PFCoverage {
  lazy val points: Vector[(String, String, String)] = {
    val path = sys.props.getOrElse("oceanus.coverage.manifest", "scripts/pf-v1-coverage.tsv")
    val input = scala.io.Source.fromFile(path)
    try input.getLines().drop(1).filter(_.nonEmpty).map { line =>
      val cells = line.split("\t", -1)
      (cells(0), cells(1), cells(2))
    }.toVector finally input.close()
  }
  lazy val ids: Set[String] = points.map(_._1).toSet
}

/** Limit C++ build workers independently of simulation stimulus timing. */
object PFSimulator {
  def bounded: chisel3.simulator.HasSimulator = chisel3.simulator.HasSimulator.simulators.verilator(
    verilatorSettings = svsim.verilator.Backend.CompilationSettings.default
      .withParallelism(Some(svsim.verilator.Backend.CompilationSettings.Parallelism.Uniform.default.withNum(2))))
}
