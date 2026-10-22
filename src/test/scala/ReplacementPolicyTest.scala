package cache

import xscache.coupledL2.utils._
import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.simulator.ChiselSim
import chisel3.simulator.HasSimulator
import svsim.verilator.Backend.CompilationSettings.TraceStyle
import svsim.verilator.Backend.CompilationSettings.TraceKind

import xscache.coupledL2.utils.PseudoLRU
import freechips.rocketchip.util.UIntToAugmentedUInt
import oceanus.l2.TreePLRU

class PLRU(way: Int) extends Module {
  val repl = new PseudoLRU(way)
  val io = IO(new Bundle {
    val s = Input(UInt(repl.nBits.W))
  })


  /** @param state state_reg bits for this sub-tree
    * @param tree_nways number of ways in this sub-tree
    */
  def get_replace_way(state: UInt, tree_nways: Int): UInt = {
    require(state.getWidth == (tree_nways-1), s"wrong state bits width ${state.getWidth} for $tree_nways ways")

    // this algorithm recursively descends the binary tree, filling in the way-to-replace encoded value from msb to lsb
    if (tree_nways > 2) {
      // we are at a branching node in the tree, so recurse
      val right_nways: Int = 1 << (log2Ceil(tree_nways) - 1)  // number of ways in the right sub-tree
      val left_nways:  Int = tree_nways - right_nways         // number of ways in the left sub-tree
      val left_subtree_older  = state(tree_nways-2)
      val left_subtree_state  = state.extract(tree_nways-3, right_nways-1)
      val right_subtree_state = state(right_nways-2, 0)

      if (left_nways > 1) {
        // we are at a branching node in the tree with both left and right sub-trees, so recurse both sub-trees
        Cat(left_subtree_older,      // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(left_subtree_older,  // if left sub-tree is older, recurse left, else recurse right
            get_replace_way(left_subtree_state,  left_nways),    // recurse left
            get_replace_way(right_subtree_state, right_nways)))  // recurse right
      } else {
        // we are at a branching node in the tree with only a right sub-tree, so recurse only right sub-tree
        Cat(left_subtree_older,      // return the top state bit (current tree node) as msb of the way-to-replace encoded value
          Mux(left_subtree_older,  // if left sub-tree is older, return and do not recurse right
            0.U(1.W),
            get_replace_way(right_subtree_state, right_nways)))  // recurse right
      }
    } else if (tree_nways == 2) {
      // we are at a leaf node at the end of the tree, so just return the single state bit as lsb of the way-to-replace encoded value
      state(0)
    } else {  // tree_nways <= 1
      // we are at an empty node in an unbalanced tree for non-power-of-2 ways, so return single zero bit as lsb of the way-to-replace encoded value
      0.U(1.W)
    }
  }

  def get_replace_way(state: UInt): UInt = get_replace_way(state, way)

  assert(OHToUInt(repl.get_replace_OH(io.s)) === get_replace_way(io.s))
}

object TestPLRU extends App with ChiselSim {
  val dumpVcd = false
  implicit val sim: chisel3.simulator.HasSimulator = if (dumpVcd)
    HasSimulator.simulators.verilator(
      verilatorSettings = svsim.verilator.Backend.CompilationSettings(
        traceStyle = Some(TraceStyle(TraceKind.Vcd)),
        outputSplit = Some(30000),
        outputSplitCFuncs = Some(30000),
        disabledWarnings = Seq("STMTDLY", "WIDTH")
      )
    )
  else
    HasSimulator.default

  simulate(
    new PLRU(way = 11))
  { s =>
    for (i <- 0 until math.pow(2, 10).toInt) {
      println(s"Testing $i")
      s.io.s.poke(i.U)
      s.clock.step(1)
    }
  }
}

// ---------------------------------------------------------------------------
// TreePLRU (oceanus.l2) checks:
//  1. Exhaustive 4-way equivalence against the pre-TreePLRU PLRU4 formulas
//     (all 8 states x 4 touch ways, combinational asserts in hardware).
//  2. 8-way cross-check against an independent node-walking software model
//     (all 128 states x 8 touch ways, bench-compared via IO).
// ---------------------------------------------------------------------------

class TreePLRU4Equiv extends Module {
  val io = IO(new Bundle {
    val s = Input(UInt(3.W))
    val w = Input(UInt(2.W))
  })

  // Reference: the pre-TreePLRU PLRU4 formulas from L2Directory.
  def plru4ReplaceWay(state: UInt): UInt = {
    val way = WireDefault(0.U(2.W))
    when(state(0) === 0.U) { way := Mux(state(1) === 0.U, 0.U, 1.U) }
      .otherwise           { way := Mux(state(2) === 0.U, 2.U, 3.U) }
    way
  }
  def plru4NextState(state: UInt, touchWay: UInt): UInt = {
    val next = Wire(Vec(3, Bool()))
    next := state.asTypeOf(Vec(3, Bool()))
    val inLeft = touchWay(1) === 0.U
    val first  = touchWay(0) === 0.U
    next(0) := Mux(inLeft, true.B, false.B)
    when(inLeft) { next(1) := Mux(first, true.B, false.B) }
      .otherwise { next(2) := Mux(first, true.B, false.B) }
    next.asUInt
  }

  assert(TreePLRU.getReplaceWay(io.s, 4) === plru4ReplaceWay(io.s),
    "TreePLRU 4-way victim mismatch")
  assert(TreePLRU.getNextState(io.s, io.w, 4) === plru4NextState(io.s, io.w),
    "TreePLRU 4-way next-state mismatch")
}

class TreePLRUPort(val ways: Int) extends Module {
  val io = IO(new Bundle {
    val s = Input(UInt((ways - 1).W))
    val w = Input(UInt(log2Ceil(ways).W))
    val victim = Output(UInt(log2Ceil(ways).W))
    val next = Output(UInt((ways - 1).W))
  })
  io.victim := TreePLRU.getReplaceWay(io.s, ways)
  io.next   := TreePLRU.getNextState(io.s, io.w, ways)
}

object TestTreePLRU extends App with ChiselSim {
  // Independent software model: walks the heap-indexed tree node-by-node
  // (no shared closed form with the hardware implementation).
  def swVictim(state: Int, ways: Int): Int = {
    var node = 0
    var sub = ways
    var way = 0
    while (sub > 1) {
      val bit = (state >> node) & 1
      way = (way << 1) | bit
      node = if (bit == 0) 2 * node + 1 else 2 * node + 2
      sub /= 2
    }
    way
  }
  def swNext(state: Int, touch: Int, ways: Int): Int = {
    var st = state
    var node = 0
    var sub = ways
    while (sub > 1) {
      val lvl = Integer.numberOfTrailingZeros(sub) // log2(sub)
      val dir = (touch >> (lvl - 1)) & 1           // 0 = left/lower
      val bit = dir ^ 1                            // set away from the touch
      st = if (bit == 1) st | (1 << node) else st & ~(1 << node)
      node = if (dir == 0) 2 * node + 1 else 2 * node + 2
      sub /= 2
    }
    st
  }

  implicit val sim: chisel3.simulator.HasSimulator = HasSimulator.default

  // 1. 4-way hardware-formula equivalence (asserts inside the module).
  simulate(new TreePLRU4Equiv) { s =>
    for (st <- 0 until 8; w <- 0 until 4) {
      s.io.s.poke(st.U)
      s.io.w.poke(w.U)
      s.clock.step(1)
    }
  }
  println("TreePLRU 4-way vs PLRU4 formulas: all 32 state/touch combinations match")

  // 2. 8-way vs the independent software model.
  simulate(new TreePLRUPort(8)) { s =>
    for (st <- 0 until 128; w <- 0 until 8) {
      s.io.s.poke(st.U)
      s.io.w.poke(w.U)
      s.clock.step(1)
      val v = s.io.victim.peek().litValue.toInt
      val n = s.io.next.peek().litValue.toInt
      require(v == swVictim(st, 8),
        f"victim mismatch: state=$st%02x touch=$w hw=$v sw=${swVictim(st, 8)}")
      require(n == swNext(st, w, 8),
        f"next-state mismatch: state=$st%02x touch=$w hw=$n%02x sw=${swNext(st, w, 8)}%02x")
    }
  }
  println("TreePLRU 8-way vs software model: all 1024 state/touch combinations match")
}
