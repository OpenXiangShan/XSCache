package oceanus

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.{ChiselSim,HasSimulator}
import oceanus.chi._
import oceanus.l2._
import oceanus.l2.tshr.L2VPipeREQ
import org.chipsalliance.cde.config._
import utility._
import xscache.oceanus.compactchi.{CCHIParameters,CCHIParametersKey}

// A request-vPipe boundary reproduction of CopyBack cancellation after RetryAck.
// The directory snapshot changes only after an independently completed SNP;
// the full-cache random trace is the evidence for reaching this boundary.
class HNRetryCancelMinimal(implicit p:Parameters) extends L2VPipeREQ(Seq.empty,1) {
  val observed=IO(Output(UInt(6.W)))
  observed:=Cat(BoringUtils.bore(w_evict_dn_compdbid),BoringUtils.bore(w_unlock_ds),
    BoringUtils.bore(w_unlock_dir),BoringUtils.bore(w_dn_pcrdgrant),
    BoringUtils.bore(w_s_evict_peer_unlock_dir),BoringUtils.bore(w_s_evict_peer_unlock_ds))
}
object TestHNRetryCancelMinimal extends App with ChiselSim {
  Constantin.init(false)
  val cancel= !args.contains("--control")
  val expectBaseIssue=args.contains("--expect-base-issue")
  require(!expectBaseIssue||cancel,"known-base mode requires the cancel case")
  val params=new Config((_,_,_)=>{
    case L2ParamsKey=>L2Params(mshrSize=8,ways=4,sets=128)
    case CHIParametersKey=>CHIParameters(issue=EnumCHIIssue.E,nodeIdWidth=11,reqAddrWidth=48,dataWidth=256)
    case CCHIParametersKey=>CCHIParameters()
    case LogUtilsOptionsKey=>LogUtilsOptions(enableDebug=false,enablePerf=false,fpgaPlatform=false)
    case PerfCounterOptionsKey=>PerfCounterOptions(enablePerfPrint=false,enablePerfDB=false,perfLevel=XSPerfLevel.VERBOSE,0)
  })
  implicit val simulator:HasSimulator=HasSimulator.default
  simulate(new HNRetryCancelMinimal()(params)){d=>
    val inputs=Seq[Data](d.io.consts,d.io.UpRXREQ,d.io.UpRXEVB,d.io.DnRXRSP,d.io.DnRXDAT,
      d.io.UpRXRSP,d.io.UpRXDAT,d.io.tshr_paddr,d.io.tshr_dirResult,d.io.tshr_dealloc,
      d.io.tshr_meta_modified,d.io.tshr_tag_modified,d.io.tbuf_modified,d.io.tbuf_data0_valid,
      d.io.tbuf_data2_valid,d.io.fromSA,d.io.ds_rd_done,d.io.repl_retry,d.io.repl_done,
      d.io.repl_resp,d.io.repl_meta,d.io.dir_wb_accept,d.io.dir_wb_done,d.io.fromPCreditPool,
      d.io.fromClientTable,d.io.self_unlock_dir,d.io.self_unlock_dir_tshrId,d.io.self_unlock_ds,
      d.io.self_unlock_dir_ack,d.io.L1EVT_active)
    inputs.foreach(x=>x.poke(0.U.asTypeOf(x)))
    Seq(d.io.DnTXREQ.ready,d.io.DnTXRSP.ready,d.io.DnTXDAT.ready,d.io.UpTXRSP.ready,
      d.io.UpTXDAT.ready,d.io.UpTXEVB.ready).foreach(_.poke(true.B))
    d.io.consts.nodeId.poke(8.U);d.io.consts.tshrId.poke((if(args.contains("--txn-zero"))0 else 7).U)
    d.io.tshr_paddr.poke(0x210280.U)
    d.reset.poke(true.B);d.clock.step(8);d.reset.poke(false.B)
    var cycle=0
    def step():Unit={
      println(s"RETRY_CANCEL_STEP cycle=$cycle cancel=$cancel free=${d.io.free.peek().litToBoolean} locks=${d.observed.peek().litValue} txvalid=${d.io.DnTXREQ.valid.peek().litToBoolean} opcode=${d.io.DnTXREQ.bits.Opcode.get.peek().litValue}")
      d.clock.step();cycle+=1
    }
    d.io.tshr_dirResult.hit.poke(true.B);d.io.tshr_dirResult.state.poke(1.U);d.io.tshr_dirResult.way.poke(1.U)
    d.io.UpRXEVB.valid.poke(true.B);d.io.UpRXEVB.bits.TshrId.poke(4.U)
    d.io.UpRXEVB.bits.Addr.poke(0x210280.U);d.io.UpRXEVB.bits.Way.poke(1.U)
    d.io.fromSA.SnpResp.poke(true.B);step()
    d.io.UpRXEVB.valid.poke(false.B);d.io.fromSA.SnpResp.poke(false.B)
    assert(d.io.DnTXREQ.valid.peek().litToBoolean)
    assert(d.io.DnTXREQ.bits.Opcode.get.peek().litValue==0x42)
    step()
    d.io.ds_rd_done.poke(true.B);step();d.io.ds_rd_done.poke(false.B)
    d.io.tbuf_data0_valid.poke(true.B);d.io.tbuf_data2_valid.poke(true.B)
    d.io.DnRXRSP.valid.poke(true.B);d.io.DnRXRSP.bits.Opcode.get.poke(3.U)
    d.io.DnRXRSP.bits.SrcID.get.poke(0.U);d.io.DnRXRSP.bits.PCrdType.get.poke(1.U);step()
    d.io.DnRXRSP.valid.poke(false.B)
    // Completed SnpUnique invalidates the victim while its CopyBack is retried.
    // Control retains the valid victim and completes the reissued CopyBack.
    if(cancel)d.io.tshr_dirResult.state.poke(0.U)
    d.io.self_unlock_dir_ack.poke(true.B);step();d.io.self_unlock_dir_ack.poke(false.B)
    d.io.fromPCreditPool.poke(true.B);step();d.io.fromPCreditPool.poke(false.B)
    val op=d.io.DnTXREQ.bits.Opcode.get.peek().litValue
    assert(d.io.DnTXREQ.valid.peek().litToBoolean&&op==(if(cancel)5 else 0x42))
    println(s"RETRY_CANCEL_EVENT cycle=$cycle event=${if(cancel)"PCrdReturn" else "reissue"} opcode=$op TxnID=${d.io.DnTXREQ.bits.TxnID.get.peek().litValue}")
    step()
    if(!cancel){
      d.io.ds_rd_done.poke(true.B)
      d.io.DnRXRSP.valid.poke(true.B);d.io.DnRXRSP.bits.Opcode.get.poke(5.U);step()
      d.io.ds_rd_done.poke(false.B)
      d.io.DnRXRSP.valid.poke(false.B)
    }
    for(i<-0 until 16)step()
    val free=d.io.free.peek().litToBoolean
    println(s"RETRY_CANCEL_RESULT cancel=$cancel free=$free locks=${d.observed.peek().litValue} observed=${if(free)"COMPLETED" else "STUCK"}")
    if(expectBaseIssue) {
      assert(!free&&d.observed.peek().litValue==24,"known-base boundary lock signature changed")
      println("RETRY_BASE_KNOWN status=XFAIL issues=locks base 已知问题(见 retry-base-note.md)")
    } else assert(free,s"CopyBack cancellation retains request-vPipe locks after PCrdReturn: ${d.observed.peek().litValue}")
  }
}
