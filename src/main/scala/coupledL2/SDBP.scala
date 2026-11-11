// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE for details.
package xscache.coupledL2

import chisel3._
import chisel3.util._
import xscache.coupledL2.utils.TrueLRU

case class SDBPParams(
  samplerSets: Int = 128,
  samplerWays: Option[Int] = None,
  predictorEntries: Int = 4096,
  deadThreshold: Int = 8
) {
  require(isPow2(samplerSets) && samplerSets >= 2)
  require(samplerWays.forall(_ > 0))
  require(isPow2(predictorEntries) && predictorEntries >= 2)
  require(deadThreshold >= 0 && deadThreshold <= 9)
  def ways(l2Ways: Int): Int = samplerWays.getOrElse(math.max(1, l2Ways * 3 / 4))
  def checkSets(localSets: Int): Unit = {
    require(isPow2(localSets) && localSets >= 4)
    require(samplerSets <= localSets)
  }
}

/** Bit-for-bit implementation of SDBP-ROOT's xor_fold and mixed table hashes. */
object SDBPHash {
  val signatureBits = 15
  private val wordMask = (BigInt(1) << 64) - 1
  def signature(pc: UInt): UInt = {
    // Client PCs contain the implemented virtual-address bits. Reconstruct
    // RV64 canonical PCs before ROOT's unsigned shift/fold.
    val shifted = pc.asSInt.pad(64).asUInt >> 1
    val chunks = (0 until math.max(1, shifted.getWidth) by signatureBits).map { low =>
      shifted(math.min(low + signatureBits, shifted.getWidth) - 1, low).pad(signatureBits)
    }
    chunks.reduce(_ ^ _)
  }
  def mixed(value: UInt): UInt = {
    val x = value.pad(64)(63, 0)
    val a = ((x ^ (x >> 30)) * "hbf58476d1ce4e5b9".U(64.W))(63, 0)
    val b = ((a ^ (a >> 27)) * "h94d049bb133111eb".U(64.W))(63, 0)
    b ^ (b >> 31)
  }
  def indices(signature: UInt, entries: Int): Vec[UInt] = VecInit((0 until 3).map { table =>
    val seed = (BigInt("9e3779b97f4a7c15", 16) * (table + 1)) & wordMask
    mixed(signature.pad(64) ^ seed.U(64.W))(log2Ceil(entries) - 1, 0)
  })
  def sampled(set: UInt, localSets: Int, sampledSets: Int): Bool = {
    val lowBits = log2Ceil(sampledSets)
    val compareBits = log2Ceil(localSets) - lowBits
    if (compareBits == 0) true.B else set(log2Ceil(localSets) - 1, lowBits) === set(compareBits - 1, 0)
  }
  def train(counter: UInt, dead: Bool, table: Int): UInt = {
    val live = if ((table & 1) != 0) counter >> 1 else Mux(counter === 0.U, 0.U, counter - 1.U)
    Mux(dead, Mux(counter === 3.U, 3.U, counter + 1.U), live)
  }
}

class SDBPTraining(entries: Int) extends Bundle {
  val indices = Vec(3, UInt(log2Ceil(entries).W))
  val dead = Bool()
}

class SDBPSamplerEntry extends Bundle {
  val valid = Bool()
  val tag = UInt(15.W)
  val signature = UInt(15.W)
  val signatureValid = Bool()
  val dead = Bool()
}

class SDBPSamplerCommit extends Bundle {
  val signature = UInt(15.W)
  val signatureValid = Bool()
  val dead = Bool()
  val prefetch = Bool()
}

/** Register sampler: s1 snapshot, s2 lookup, s3 one-way commit. No event queue. */
class SDBPSampler(localSets: Int, l2Ways: Int, params: SDBPParams) extends Module {
  params.checkSets(localSets)
  private val sets = params.samplerSets
  private val ways = params.ways(l2Ways)
  private val wayBits = math.max(1, log2Ceil(ways))
  private val setBits = log2Ceil(sets)
  val io = IO(new Bundle {
    val read = Flipped(Valid(new Bundle {
      val set = UInt(log2Ceil(localSets).W)
      val tag = UInt(15.W)
    }))
    val commit = Flipped(Valid(new SDBPSamplerCommit))
    val training = Valid(new SDBPTraining(params.predictorEntries))
    val writeEnable = Output(Bool())
    val hit = Output(Bool())
    val selectedWay = Output(UInt(wayBits.W))
    val previousDead = Output(Bool())
  })
  val entries = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(0.U.asTypeOf(new SDBPSamplerEntry))))))
  val readSampled = io.read.valid && SDBPHash.sampled(io.read.bits.set, localSets, sets)
  val valid_s2 = RegNext(readSampled, false.B)
  val valid_s3 = RegNext(valid_s2, false.B)
  val index_s2 = RegEnable(io.read.bits.set(setBits - 1, 0), readSampled)
  val tag_s2 = RegEnable(io.read.bits.tag, readSampled)
  val entries_s2 = RegEnable(entries(io.read.bits.set(setBits - 1, 0)), readSampled)
  val hits = VecInit(entries_s2.map(e => e.valid && e.tag === tag_s2))
  val invalid = VecInit(entries_s2.map(e => !e.valid)).asUInt
  val dead = VecInit(entries_s2.map(e => e.valid && e.dead)).asUInt
  val lru = if (ways > 1) Some(new TrueLRU(ways)) else None
  val lruState = if (ways > 1) Some(RegInit(VecInit(Seq.fill(sets)(0.U(lru.get.nBits.W))))) else None
  val state_s2 = lruState.map(state => RegEnable(state(io.read.bits.set(setBits - 1, 0)), readSampled))
  val fallback = lru.map(_.get_replace_way(state_s2.get)).getOrElse(0.U(wayBits.W))
  val selected_s2 = Mux(hits.asUInt.orR, PriorityEncoder(hits.asUInt),
    Mux(invalid.orR, PriorityEncoder(invalid), Mux(dead.orR, PriorityEncoder(dead), fallback)))
  val selected_s3 = RegEnable(selected_s2.pad(wayBits), valid_s2)
  val index_s3 = RegEnable(index_s2, valid_s2)
  val tag_s3 = RegEnable(tag_s2, valid_s2)
  val old_s3 = RegEnable(if (ways == 1) entries_s2(0) else entries_s2(selected_s2), valid_s2)
  val hit_s3 = RegEnable(hits.asUInt.orR, valid_s2)
  val state_s3 = state_s2.map(state => RegEnable(state, valid_s2))
  val update = io.commit.valid && valid_s3 && !(io.commit.bits.prefetch && hit_s3)
  io.writeEnable := update
  io.hit := valid_s3 && hit_s3
  io.selectedWay := selected_s3
  io.previousDead := old_s3.dead
  io.training.valid := update && old_s3.valid && old_s3.signatureValid
  io.training.bits.indices := RegEnable(
    SDBPHash.indices((if (ways == 1) entries_s2(0) else entries_s2(selected_s2)).signature,
      params.predictorEntries), valid_s2)
  io.training.bits.dead := !hit_s3
  when(update) {
    val entry = if (ways == 1) entries(index_s3)(0) else entries(index_s3)(selected_s3)
    entry.valid := true.B
    entry.tag := tag_s3
    entry.signature := io.commit.bits.signature
    entry.signatureValid := io.commit.bits.signatureValid && !io.commit.bits.prefetch
    entry.dead := io.commit.bits.dead && io.commit.bits.signatureValid && !io.commit.bits.prefetch
    lruState.foreach(state => state(index_s3) := lru.get.get_next_state(state_s3.get, selected_s3))
    assert(selected_s3 < ways.U)
  }
  when(valid_s2) { assert(PopCount(hits) <= 1.U) }
  assert(!(readSampled && update), "Sampler writes must share Directory write exclusion")
}

class SDBPRead(localSets: Int, params: SDBPParams) extends Bundle {
  val set = UInt(log2Ceil(localSets).W)
  val tag = UInt(15.W)
  val candidate = Bool()
  val prefetch = Bool()
  val signatureValid = Bool()
  val signature = UInt(15.W)
  val indices = Vec(3, UInt(log2Ceil(params.predictorEntries).W))
}

class SDBPCommit(localSets: Int, l2Ways: Int) extends Bundle {
  val set = UInt(log2Ceil(localSets).W)
  val way = UInt(log2Ceil(l2Ways).W)
  val refill = Bool()
}

/** s3 commits are produced by MainPipe only for real tag hits/installations. */
class SDBP(localSets: Int, l2Ways: Int, params: SDBPParams) extends Module {
  val io = IO(new Bundle {
    val read = Flipped(Valid(new SDBPRead(localSets, params)))
    val commit = Flipped(Valid(new SDBPCommit(localSets, l2Ways)))
    val deadWrite = Valid(new Bundle {
      val set = UInt(log2Ceil(localSets).W)
      val way = UInt(log2Ceil(l2Ways).W)
      val dead = Bool()
    })
    val samplerWrite = Output(Bool())
    val liveTraining = Output(Bool())
    val deadTraining = Output(Bool())
  })
  val sampler = Module(new SDBPSampler(localSets, l2Ways, params))
  val predictor = Module(new SCArray(params))
  val candidate = io.read.bits.candidate
  val needsPrediction = candidate && io.read.bits.signatureValid && !io.read.bits.prefetch
  val read = io.read.valid && candidate
  sampler.io.read.valid := read
  sampler.io.read.bits.set := io.read.bits.set
  sampler.io.read.bits.tag := io.read.bits.tag
  predictor.io.query.valid := io.read.valid && needsPrediction
  predictor.io.query.bits := io.read.bits.indices
  val read_s2 = RegNext(read, false.B)
  val read_s3 = RegNext(read_s2, false.B)
  val metadata_s2 = RegEnable(io.read.bits, read)
  val metadata_s3 = RegEnable(metadata_s2, read_s2)
  val predictedDead_s3 = RegEnable(predictor.io.response.bits.reduce(_ +& _) >= params.deadThreshold.U,
    false.B, predictor.io.response.valid)
  val predictionValid_s3 = RegNext(predictor.io.response.valid, false.B)
  val dead = predictionValid_s3 && predictedDead_s3 && metadata_s3.signatureValid && !metadata_s3.prefetch
  sampler.io.commit.valid := io.commit.valid
  sampler.io.commit.bits.signature := metadata_s3.signature
  sampler.io.commit.bits.signatureValid := metadata_s3.signatureValid
  sampler.io.commit.bits.dead := dead
  sampler.io.commit.bits.prefetch := metadata_s3.prefetch
  predictor.io.training := sampler.io.training
  io.deadWrite.valid := io.commit.valid && (io.commit.bits.refill || !metadata_s3.prefetch)
  io.deadWrite.bits.set := io.commit.bits.set
  io.deadWrite.bits.way := io.commit.bits.way
  io.deadWrite.bits.dead := dead
  io.samplerWrite := sampler.io.writeEnable
  io.liveTraining := sampler.io.training.valid && !sampler.io.training.bits.dead
  io.deadTraining := sampler.io.training.valid && sampler.io.training.bits.dead
  assert(!io.commit.valid || read_s3, "SDBP commit without matching s1 lookup")
  when(io.commit.valid) { assert(io.commit.bits.set === metadata_s3.set) }
}
