package oceanus

// Diagnostic counterexample: translation residency can change full/admission.
// No production or reference algorithm is changed. Never a differential PASS.
import chisel3._
import chisel3.util._
import chisel3.simulator.{ChiselSim, HasSimulator}
import org.chipsalliance.cde.config._
import oceanus.l2._
import utility._
import scala.collection.mutable
import java.nio.file.{Files, Paths}

class CDPM4AdmissionWindowHarness(implicit p: Parameters) extends CDPC4Harness {
  val debugFields=mutable.ArrayBuffer.empty[(String,UInt)]
  def add(n:String,d:Data):Unit=debugFields+=n->read(d).asUInt
  add("d.valids",read(prod.valids).asUInt);add("r.valids",read(reference.valids).asUInt)
  add("d.allocate",prod.allocate);add("r.allocate",reference.in.valid && !read(reference.entryHit) && read(reference.hasFreeEntry))
  add("d.duplicate",prod.duplicate);add("r.duplicate",reference.in.valid && read(reference.entryHit))
  add("d.full",prod.fullDrop);add("r.full",reference.in.valid && !read(reference.entryHit) && !read(reference.hasFreeEntry))
  add("d.free",prod.freeEntryIdx);add("r.free",reference.freeEntryIdx)
  add("d.terminal",prod.io.tlb.resp.fire);add("d.terminalVA",prod.io.tlb.resp.bits.vaddr)
  add("r.terminal",read(reference.tlb_s3_valid) && read(reference.tlb_s3_rsp_valid) && !read(reference.tlb_s3_rsp_bits).miss)
  add("r.terminalVA",reference.tlb_s3_addr)
  add("d.out",prod.io.out.fire);add("r.out",reference.io.out.fire)
  add("d.drop",prod.filterDrop);add("r.drop",read(reference.pft_s2_valid) && !read(reference.canPft))
  add("d.outIndex",prod.idx2);add("r.outIndex",reference.pft_s2_chosen_idx)
  add("d.outPA",prod.io.out.bits.paddr);add("r.outPA",reference.io.out.bits.addr)
  add("d.outDepth",prod.io.out.bits.pfDepth);add("r.outDepth",reference.io.out.bits.cdpPfDepth.get)
  val debug=IO(Output(UInt(debugFields.map(_._2.getWidth).sum.W)))
  debug:=Cat(debugFields.map(_._2).reverse.toSeq)
  val widths=debugFields.map {case(n,d)=>n->d.getWidth}.toVector
}
object TestTop_CDPM4AdmissionWindow extends App with ChiselSim {
  Constantin.init(false)
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=CDPAuditSimulator.bounded
  val dest=Paths.get(sys.props("oceanus.c5.trace"));Files.createDirectories(dest)
  simulate(new CDPM4AdmissionWindowHarness()(params)){dut=>
    def scene(keep:Set[Int],fixed:Boolean=false,save:Boolean=false):Boolean={
      dut.io.fixed.poke(fixed);dut.io.delay.poke(24);dut.io.candidate.poke(0)
      dut.io.gate.poke(true);dut.io.ready.poke(true);dut.io.queryReady.poke(true)
      dut.io.filter.poke(false);dut.io.vbop.poke(false);dut.io.vbopVA.poke(0)
      dut.reset.poke(true);dut.clock.step(4);dut.reset.poke(false)
      var mismatch=false;val rows=mutable.ArrayBuffer.empty[ujson.Value]
      val allocated=Array.fill(2)(0);val retired=Array.fill(2)(0);val full=Array.fill(2)(0)
      for(c<-0 until 160){
        val va=BigInt(0x300000)+(c%4)*0x10000+(c/4)*64
        val bits=if(keep(c))(BigInt(1)<<57)|(va<<7)|(BigInt(3)<<5)|(BigInt(9)<<1) else BigInt(0)
        dut.io.candidate.poke(bits);dut.io.gate.poke(!(c>=7 && c<12))
        var bitsOut=dut.debug.peek().litValue
        val row=dut.widths.map{case(n,w)=>val v=bitsOut&((BigInt(1)<<w)-1);bitsOut>>=w;n->v}.toMap
        if(keep(c) && row("d.full")==1 && row("r.allocate")==1)mismatch=true
        for((side,prefix)<-Seq(0->"d",1->"r")){
          allocated(side)+=row(prefix+".allocate").toInt
          retired(side)+=(row(prefix+".out")+row(prefix+".drop")).toInt
          full(side)+=row(prefix+".full").toInt
        }
        if(save)rows+=ujson.Obj("cycle"->c,"valid"->keep(c),"va"->ujson.Str(va.toString(16)),
          "signals"->ujson.Obj.from(row.map{case(n,v)=>n->ujson.Str(v.toString(16))}))
        if(c==159)assert(row("d.valids")==0 && row("r.valids")==0 && allocated.sameElements(retired),"nonconserving minimum")
        dut.clock.step()
      }
      if(save)Files.writeString(dest.resolve("m4-minimum.json"),ujson.Obj("head"->sys.props("oceanus.c5.head"),
        "kept"->ujson.Arr.from(keep.toVector.sorted),"mismatchPreserved"->mismatch,"allocated"->ujson.Arr.from(allocated),
        "full"->ujson.Arr.from(full),"rows"->ujson.Arr.from(rows)).render())
      mismatch
    }
    val keep=(0 until 17).toSet
    assert(scene(keep,save=true),"full-versus-allocate minimum absent")
    assert(!scene(keep,fixed=true),"fixed translation retained full mismatch")
    for(i<-keep)assert(!scene(keep-i),s"not one-minimal remove=$i")
    println("CDP_M4_ADMISSION_MINIMUM reproduced=true differentialPass=false candidates=17 oneMinimal=true fixedTranslationAligned=true scope=full-versus-allocate")
  }
}
