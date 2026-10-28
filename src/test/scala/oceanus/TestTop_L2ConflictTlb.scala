// Intent (pf-v2): independent 8-slot simultaneous-owner/full-capacity conflict coverage.
package oceanus

import chisel3._
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import org.chipsalliance.cde.config._
import scala.collection.mutable

// Intent (1c-1): drive only public Decoupled ports. An ordered TLB model has
// configurable latency/miss/fault/PBMT and independently delayed PMP attributes;
// a host scoreboard checks ownership, attempts, exact retry timers and stalls.
// Hold client responses to exhaust ALL reserved slots, then drain and reuse.
object TestTop_L2ConflictTlb extends App with ChiselSim {
  val params = new Config((_, _, _) => { case L2ParamsKey => L2Params() })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new L2PrefetchTlbAdapter(entries = 8)(params)) { dut =>
    case class Spec(client: Int, va: BigInt, delay: Int = 1, misses: Int = 0,
      fault: Int = 0, pmpFault: Boolean = false, mmio: Boolean = false, pbmt: Int = 0,
      exactRetry: Boolean = true)
    case class Reply(spec: Spec, attempt: Int, due: Int)
    val specs = mutable.Map.empty[BigInt, Spec]
    val held = mutable.Map.empty[Int, Spec]
    val accepted = mutable.Set.empty[BigInt]
    val attempts = mutable.Map.empty[BigInt, Int].withDefaultValue(0)
    val replies = mutable.Queue.empty[Reply]
    val finished = mutable.Set.empty[BigInt]
    val sends = mutable.ArrayBuffer.empty[(Int, BigInt)]
    val missAt = mutable.Map.empty[BigInt, Int]
    val blockedResult = mutable.Map.empty[Int, BigInt]
    var oldPmp: Option[Reply] = None
    var reqStall: Option[BigInt] = None
    var dual=0; var full=0; var retiredAccepted=0
    var cycle = 0
    var tlbReady = true
    val clientReady = Array(true, true)
    def flag(x: Bool): Boolean = x.peek().litToBoolean
    def value(x: UInt): BigInt = x.peek().litValue
    def pa(va: BigInt): BigInt = (va ^ BigInt("123456000", 16)) & ((BigInt(1) << 48) - 1)
    def add(s: Spec): Unit = {
      assert(!held.contains(s.client) && !specs.contains(s.va))
      specs(s.va) = s; held(s.client) = s
    }
    def tick(): Unit = {
      assert(cycle < 10000, "watchdog")
      dut.io.clients.zipWithIndex.foreach { case (c, i) =>
        c.req.valid.poke(held.contains(i).B)
        c.req.bits.poke(held.get(i).map(_.va).getOrElse(BigInt(0)).U)
        c.resp.ready.poke(clientReady(i).B)
      }
      val tlb = dut.io.tlb
      tlb.req.ready.poke(tlbReady.B)
      tlb.resp.valid.poke(false.B)
      tlb.resp.bits.poke(0.U.asTypeOf(tlb.resp.bits))
      tlb.pmp_resp.poke(0.U.asTypeOf(tlb.pmp_resp))
      oldPmp.foreach { r =>
        tlb.pmp_resp.ld.poke(r.spec.pmpFault.B)
        tlb.pmp_resp.mmio.poke(r.spec.mmio.B)
      }
      val response = replies.headOption.filter(_.due <= cycle)
      response.foreach { r =>
        replies.dequeue()
        val s = r.spec
        tlb.resp.valid.poke(true.B)
        tlb.resp.bits.paddr.head.poke(pa(s.va).U)
        tlb.resp.bits.miss.poke((r.attempt <= s.misses).B)
        tlb.resp.bits.pbmt.poke(s.pbmt.U)
        tlb.resp.bits.excp.head.pf.ld.poke((s.fault == 1).B)
        tlb.resp.bits.excp.head.gpf.ld.poke((s.fault == 2).B)
        tlb.resp.bits.excp.head.af.ld.poke((s.fault == 3).B)
        if (r.attempt == 1 && s.misses > 0) missAt(s.va) = cycle
      }
      if(dut.io.clients.forall(c=>flag(c.req.valid))) dual+=1
      if(dut.io.clients.forall(c=> !flag(c.req.ready)) && held.nonEmpty) full+=1
      if(dut.io.clients.exists(c=>flag(c.resp.valid)&&flag(c.resp.ready)) && dut.io.clients.exists(c=>flag(c.req.valid)&&flag(c.req.ready))) retiredAccepted+=1
      assert(flag(tlb.resp.ready) && !flag(tlb.req_kill))
      dut.io.clients.zipWithIndex.foreach { case (c, i) =>
        if (flag(c.req.valid) && flag(c.req.ready)) {
          val s = held.remove(i).get
          assert(accepted.add(s.va))
        }
        if (flag(c.resp.valid)) {
          val bits = c.resp.bits
          val va = value(bits.vaddr)
          val s = specs(va)
          assert(s.client == i && accepted(va) && !finished(va), s"misrouted/duplicate va=$va client=$i")
          val secondMiss = s.misses >= 2
          val fault = !secondMiss && (s.fault != 0 || s.pmpFault)
          val mmio = !secondMiss && s.mmio
          val uncache = !secondMiss && Set(1, 2)(s.pbmt)
          assert(flag(bits.retried) == (s.misses > 0))
          assert(flag(bits.miss) == secondMiss && flag(bits.fault) == fault)
          assert(flag(bits.mmio) == mmio && flag(bits.uncache) == uncache)
          assert(flag(bits.dropped) == (secondMiss || fault || mmio || uncache))
          assert(value(bits.paddr) == pa(va))
          assert(attempts(va) == (if (s.misses > 0) 2 else 1))
          blockedResult.get(i).foreach(v => assert(bits.peek().litValue == v, "response changed while stalled"))
          if (clientReady(i)) { assert(finished.add(va)); blockedResult.remove(i) }
          else blockedResult(i) = bits.peek().litValue
        } else assert(!blockedResult.contains(i), "response valid dropped while stalled")
      }
      reqStall.foreach(v => assert(flag(tlb.req.valid) && value(tlb.req.bits.vaddr) == v))
      reqStall = if (flag(tlb.req.valid) && !tlbReady) Some(value(tlb.req.bits.vaddr)) else None
      if (flag(tlb.req.valid) && tlbReady) {
        val va = value(tlb.req.bits.vaddr); val s = specs(va)
        assert(accepted(va) && !finished(va))
        assert(value(tlb.req.bits.cmd) == 0 && value(tlb.req.bits.size) == 3)
        assert(flag(tlb.req.bits.isPrefetch) && !flag(tlb.req.bits.kill) && !flag(tlb.req.bits.no_translate))
        attempts(va) += 1
        assert(attempts(va) <= 2)
        if (attempts(va) == 2) {
          // No competing requests in retry cases: CDP timer starts at response,
          // VBOP one cycle later at PMP; zero is eligible on the following cycle.
          val minimum = if (s.client == 0) 12 else 11
          assert(if (s.exactRetry) cycle - missAt(va) == minimum else cycle - missAt(va) >= minimum,
            s"retry timing client=${s.client} resp=${missAt(va)} request=$cycle")
        }
        sends += ((cycle, va))
        replies.enqueue(Reply(s, attempts(va), cycle + s.delay))
      }
      dut.clock.step()
      cycle += 1; oldPmp = response
    }
    dut.io.clients.foreach { c => c.req.valid.poke(false.B); c.req.bits.poke(0.U); c.resp.ready.poke(false.B) }
    dut.io.tlb.req.ready.poke(false.B)
    dut.io.tlb.resp.valid.poke(false.B)
    dut.io.tlb.resp.bits.poke(0.U.asTypeOf(dut.io.tlb.resp.bits))
    dut.io.tlb.pmp_resp.poke(0.U.asTypeOf(dut.io.tlb.pmp_resp))
    dut.reset.poke(true.B); dut.clock.step(5); dut.reset.poke(false.B)
    def drain(): Unit = {
      while (finished.size != specs.size || held.nonEmpty || replies.nonEmpty || oldPmp.nonEmpty) tick()
      tick(); tick()
    }
    def single(label: String, s: Spec): Unit = {
      val start = cycle; add(s); drain()
      assert(finished(s.va))
      println(s"TLB_CASE $label pass=true cycles=${cycle-start} attempts=${attempts(s.va)} client=${s.client}")
    }
    single("hit-latency1", Spec(0, 0x1000))
    single("hit-latency7", Spec(1, 0x2000, delay = 7))
    single("vbop-retry-success", Spec(0, 0x3000, misses = 1))
    single("cdp-retry-success", Spec(1, 0x4000, delay = 5, misses = 1))
    single("second-miss-drop", Spec(0, 0x5000, misses = 2, fault = 1, mmio = true))
    single("cdp-second-miss-drop", Spec(1, 0x6000, misses = 2))
    for (f <- 1 to 3) single(s"fault-$f-drop", Spec(f % 2, 0x7000 + f * 0x100, fault = f))
    single("pmp-deny-drop", Spec(1, 0x8000, pmpFault = true))
    single("mmio-drop", Spec(0, 0x9000, mmio = true))
    for (b <- 1 to 3) single(s"pbmt-$b", Spec(b % 2, 0xa000 + b * 0x100, pbmt = b))

    val start = sends.size
    add(Spec(0, 0xb000, delay = 7, mmio = true))
    add(Spec(1, 0xc000, delay = 2, pmpFault = true))
    tick(); assert(!held.contains(0) && held.contains(1), "VBOP must win simultaneous client requests")
    drain()
    assert(sends.drop(start).map(_._2).toSeq == Seq(BigInt(0xb000), BigInt(0xc000)))
    // Both replies return consecutively after the older delayed reply; their
    // different PMP reasons detect a one-cycle pairing/ownership mixup.
    println("TLB_CASE simultaneous-priority-owner-pmp pass=true")

    // CDP is offered while external ready is low. Later VBOP arrival must not
    // change that already offered payload; once it fires, VBOP gets priority.
    tlbReady = false
    add(Spec(1, 0xd000)); tick(); tick()
    add(Spec(0, 0xe000)); tick(); tick(); tick()
    tlbReady = true; drain()
    println("TLB_CASE tlb-request-stability pass=true")

    clientReady(0) = false; clientReady(1) = false
    val previous = finished.size
    for (i <- 0 until 8) {
      add(Spec(i % 2, 0x10000 + i * 0x1000, delay = if (i == 0) 12 else 1,
        mmio = i == 1, pmpFault = i == 2))
      while (held.nonEmpty) tick()
    }
    add(Spec(0, 0x19000))
    for (_ <- 0 until 25) {
      tick()
      assert(!flag(dut.io.clients(0).req.ready) && !flag(dut.io.clients(1).req.ready))
      assert(held.contains(0) && finished.size == previous)
    }
    // Releasing only CDP must retire its results even while VBOP stays blocked.
    clientReady(1) = true
    for (_ <- 0 until 12) tick()
    assert(finished.size == previous + 4 && !held.contains(0))
    clientReady(0) = true; drain()
    println("TLB_CASE full-capacity-backpressure-recovery pass=true entries=8")
    // Intent (1c-1): fill every slot with a first miss, hold all client results,
    // and require every retry/drop to fit without external response backpressure.
    clientReady(0) = false; clientReady(1) = false
    val retryStart = finished.size
    for (i <- 0 until 8) {
      add(Spec(i % 2, 0x20000 + i * 0x1000, delay = 3,
        misses = if (i == 3) 2 else 1, exactRetry = false))
      while (held.nonEmpty) tick()
    }
    add(Spec(1, 0x29000))
    for (_ <- 0 until 45) {
      tick()
      assert(held.contains(1) && finished.size == retryStart)
      assert(!flag(dut.io.clients(0).req.ready) && !flag(dut.io.clients(1).req.ready))
    }
    for (i <- 0 until 8) assert(attempts(BigInt(0x20000 + i * 0x1000)) == 2)
    clientReady(0) = true; clientReady(1) = true; drain()
    println("TLB_CASE full-capacity-retries pass=true entries=8 retries=8")
    assert(dual>0 && full>0)
    println(s"CONFLICT_TLB pass=true entries=8 dualValid=$dual fullBlocked=$full simultaneousRetireAccept=$retiredAccepted pending=0")
    println(s"TLB_RESULT pass=true transactions=${finished.size} externalRequests=${sends.size} cycles=$cycle")
  }
}
