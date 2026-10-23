// C4 standalone integration: genuine VPN training -> four-lane detection ->
// degree -> SentUnit -> shared adapter/PMP -> queued source9/depth TxnID.
// L2Top's CDP stub is intentionally not changed until C5.
package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim, HasSimulator}
import oceanus.l2._
import oceanus.chi._
import org.chipsalliance.cde.config._
import scala.collection.mutable
import utility._

class CDPC4TopHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val train = Input(Bool()); val va = Input(UInt(50.W))
    val trigger = Input(UInt(520.W)); val bank = Input(UInt(2.W))
    val ready = Input(Bool()); val filterTrain = Input(Bool()); val filterPA = Input(UInt(48.W))
    val packed = Output(UInt(185.W))
  })
  val prod = Module(new OceanusCDP(OceanusCDPParameters(), 4, withSend = true))
  val adapter = Module(new L2PrefetchTlbAdapter)
  prod.io.enable := true.B
  prod.io.vpnTrain.valid := io.train; prod.io.vpnTrain.bits := 0.U.asTypeOf(prod.io.vpnTrain.bits)
  prod.io.vpnTrain.bits.vaddr := io.va(49,6)
  prod.io.vpnTrain.bits.vpnValid := true.B; prod.io.vpnTrain.bits.trainEnable := true.B
  prod.io.filterTrain.valid := io.filterTrain; prod.io.filterTrain.bits := 0.U.asTypeOf(prod.io.filterTrain.bits)
  prod.io.filterTrain.bits.trainEnable := true.B; prod.io.filterTrain.bits.isEvict := true.B
  prod.io.filterTrain.bits.evictPaddr := io.filterPA
  prod.io.pfStat := 0.U.asTypeOf(prod.io.pfStat)
  for(i <- 0 until 4) {
    val t = prod.io.l2DetectTriggers(i)
    t.valid := io.trigger(519) && io.bank === i.U
    t.bits.data := io.trigger(511,0); t.bits.pfDepth := io.trigger(513,512)
    t.bits.pfSource := io.trigger(517,514); t.bits.isHit := io.trigger(518)
  }
  adapter.io.clients(1) <> prod.io.tlb
  adapter.io.clients(0).req.valid := false.B; adapter.io.clients(0).req.bits := 0.U
  adapter.io.clients(0).resp.ready := true.B
  adapter.io.tlb.req.ready := true.B
  adapter.io.tlb.resp.valid := RegNext(adapter.io.tlb.req.fire, false.B)
  adapter.io.tlb.resp.bits := 0.U.asTypeOf(adapter.io.tlb.resp.bits)
  val reqVA = RegEnable(adapter.io.tlb.req.bits.vaddr, adapter.io.tlb.req.fire)
  adapter.io.tlb.resp.bits.paddr.head := Cat(reqVA(47,12) ^ "h123456".U(36.W), reqVA(11,0))
  adapter.io.tlb.pmp_resp := 0.U.asTypeOf(adapter.io.tlb.pmp_resp)
  class Queued extends Bundle { val paddr = UInt(48.W); val txn = UInt(8.W) }
  val q = Module(new Queue(new Queued, 8))
  val sequence = RegInit(0.U(2.W))
  q.io.enq.valid := prod.io.req.valid
  q.io.enq.bits.paddr := prod.io.req.bits.paddr
  q.io.enq.bits.txn := L2PrefetchTxnID.encode(prod.io.req.bits.pfSource, prod.io.req.bits.pfDepth, sequence, 8)
  prod.io.req.ready := q.io.enq.ready
  when(q.io.enq.fire) { sequence := sequence + 1.U }
  q.io.deq.ready := io.ready
  when(q.io.deq.valid) { assert(L2PrefetchTxnID.decode(q.io.deq.bits.txn) === 9.U) }
  val dv = BoringUtils.bore(prod.c4Input.valid); val db = BoringUtils.bore(prod.c4Input.bits)
  val raw = BoringUtils.bore(prod.detectFrontend.raw)
  val fields = Seq(dv, db.pfVAddr, db.pfDepth, PopCount(raw.map(_.valid)),
    prod.io.req.fire, prod.io.req.bits.paddr, prod.io.req.bits.pfDepth,
    q.io.deq.fire, q.io.deq.bits.paddr, q.io.deq.bits.txn,
    adapter.io.clients(1).req.fire)
  require(fields.map(_.getWidth).sum == 165)
  io.packed := Cat(fields.reverse).pad(185)
}

object TestTop_OceanusCDPC4Top extends App with ChiselSim {
  Constantin.init(false)
  val params = new Config((_,_,_) => {
    case L2ParamsKey => L2Params()
    case CHIParametersKey => CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case xscache.oceanus.compactchi.CCHIParametersKey => xscache.oceanus.compactchi.CCHIParameters()
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator: HasSimulator = HasSimulator.default
  simulate(new CDPC4TopHarness()(params)) { dut =>
    val pending = mutable.Map.empty[BigInt,Int]
    val queued = mutable.Queue.empty[(BigInt,Int)]
    var inputs=0;var raw=0;var sent=0;var retired=0;var duplicate=0;var full=0;var translations=0
    var dropFilterPage: Option[BigInt]=None
    def pa(va: BigInt): BigInt = ((va>>12)^BigInt("123456",16))<<12|(va&4032)
    def tick(trigger: BigInt=0, bank: Int=0, train: Boolean=false, ready: Boolean=true,
      filterTrain: Boolean=false): Unit = {
      dut.io.train.poke(train);dut.io.va.poke(0x12000);dut.io.trigger.poke(trigger);dut.io.bank.poke(bank)
      dut.io.ready.poke(ready);dut.io.filterTrain.poke(filterTrain);dut.io.filterPA.poke(pa(0x12000))
      var v=dut.io.packed.peek().litValue
      def get(n: Int): BigInt={val x=v&((BigInt(1)<<n)-1);v>>=n;x}
      val dv=get(1)!=0;val va=get(50)& ~BigInt(63);val depth=get(2).toInt;raw+=get(3).toInt
      val enq=get(1)!=0;val addr=get(48);val d=get(2).toInt
      val out=get(1)!=0;val outpa=get(48);val txn=get(8).toInt
      if(get(1)!=0)translations+=1
      assert(v==0)
      // Admission uses pre-edge occupancy; output retirement cannot lend space.
      val add = dv && !pending.contains(pa(va)) && pending.size<16
      if(dv) {
        inputs+=1
        if(pending.contains(pa(va)))duplicate+=1 else if(pending.size==16)full+=1
      }
      if(enq) {
        assert(pending.remove(addr).contains(d),s"unexpected top output PA=$addr depth=$d")
        queued.enqueue((addr,d));sent+=1
      }
      if(out) {
        assert(queued.dequeue()==((outpa,(txn>>2)&3)))
        assert((txn>>4)==9 && (txn&3)==(retired&3));retired+=1
      }
      if(add && !dropFilterPage.contains(pa(va)>>12))pending(pa(va))=depth
      dut.clock.step()
    }
    dut.io.train.poke(false);dut.io.va.poke(0);dut.io.trigger.poke(0);dut.io.bank.poke(0)
    dut.io.ready.poke(false);dut.io.filterTrain.poke(false);dut.io.filterPA.poke(0)
    dut.reset.poke(true);dut.clock.step(3);dut.reset.poke(false)
    for(_<-0 until 130){tick(train=true);for(_<-0 until 6)tick()}
    for(_<-0 until 32)tick()
    val data=(0 until 8).map(i=>BigInt(0x12fc4+i*0x1000)<<(64*i)).sum
    val atLimit = data | (BigInt(3)<<512) | (BigInt(9)<<514) | (BigInt(1)<<519)
    tick(atLimit); for(_<-0 until 64)tick()
    assert(inputs==0 && raw==0,"refill at depth max must not recurse")
    for(bank<-0 until 4;hit<-Seq(false,true)) {
      val e=data|(if(hit)(BigInt(3)<<512)|(BigInt(9)<<514)|(BigInt(1)<<518)else BigInt(0))|(BigInt(1)<<519)
      tick(e,bank,ready=false)
      for(i<-0 until 96)tick(ready=i>=35 && i%4!=0)
      assert(pending.isEmpty && queued.isEmpty)
    }
    assert(raw==64 && inputs==192 && sent>0 && sent==retired && translations>0)
    val before=sent
    // Real Filter training saturates physical page slot, then actual detection
    // in that page must be suppressed. No synthetic table response is used.
    tick(filterTrain=true);for(_<-0 until 32)tick()
    dropFilterPage=Some(pa(0x12000)>>12)
    val filteredData=(0 until 8).map(i=>BigInt(0x12004+i*64)<<(64*i)).sum
    val e=filteredData|(BigInt(3)<<512)|(BigInt(9)<<514)|(BigInt(1)<<518)|(BigInt(1)<<519)
    tick(e);for(_<-0 until 160)tick()
    assert(sent==before && pending.isEmpty && queued.isEmpty)
    println(s"CDP_C4_TOP_RESULT pass=true raw=$raw c4_inputs=$inputs sent=$sent retired=$retired duplicate=$duplicate full=$full page_translations=$translations real_filter_suppressed=24 training=130 banks=4 max_depth_refill_rejected=true")
  }
}
