package oceanus

import chisel3._
import chisel3.simulator.ChiselSim

// Thin pin adapter; v3 can reuse the host model with its own memory function
// and callbacks. Model checks use actual link flits, independently of drive.
class StressHNExtensionPins(dut:StressTopBase,dn:StressDownstreamModel,
  val model:L2HNExtensionModel, val requester:BigInt, val dataPeriod:Int=1,
  val onRequest:(HNExtReq,Int)=>Unit=(_:HNExtReq,_:Int)=>(),
  val onData:(HNExtDat,Int)=>Unit=(_:HNExtDat,_:Int)=>(),
  val onSnoopDone:(BigInt,Int)=>Unit=(_:BigInt,_:Int)=>()) extends ChiselSim {
  private def v(x:UInt)=x.peek().litValue
  private def f(x:Bool)=x.peek().litToBoolean
  private var driven=HNExtDrive(None,None,None,Seq.empty)
  private def req:HNExtReq=HNExtReq(dut.io.chi.txreq.flit.elements.toSeq.collect {
    case(name,u:UInt)=>name->v(u)
  }.toMap)
  def offer(pa:BigInt,op:Option[Int]=None):Unit=model.offerSnoop(requester,pa,op)
  def drive(c:Int,paused:BigInt=>Boolean):Unit={
    driven=model.drive(c,dn.canDriveRxRsp,dn.canDriveRxDat&&c%dataPeriod==0,dn.canDriveRxSnp,paused)
    driven.rsp.foreach{r=>
      dn.driveDnRsp(PendingDnRsp(DnTxnID(r.txn),r.rn,c,r.op))
      val p=dut.io.chi.rxrsp.flit
      p.SrcID.get.poke(r.src.U);p.DBID_PGroupID_StashGroupID_TagGroupID.get.poke(r.dbid.U)
      p.Resp.get.poke(r.resp.U);p.PCrdType.get.poke(r.credit.U)
    }
    driven.dat.foreach{d=>
      dn.driveDnData(PendingDnComp(DnTxnID(d.txn),d.rn,c,d.beat/2,d.resp,d.addr))
      val p=dut.io.chi.rxdat.flit
      p.Opcode.get.poke(d.op.U);p.SrcID.get.poke(d.src.U);p.HomeNID.get.poke(d.home.U)
      p.DBID.get.poke(d.dbid.U);p.Data.get.poke(model.memory(d.addr,d.beat/2).U)
    }
    driven.snp.foreach{s=>
      val p=dut.io.chi.rxsnp
      p.flitv.poke(true.B);p.flitpend.poke(true.B);p.flit.poke(0.U.asTypeOf(p.flit))
      p.flit.Opcode.get.poke(s.op.U);p.flit.SrcID.get.poke(s.hn.U);p.flit.TxnID.get.poke(s.txn.U)
      p.flit.Addr.get.poke((s.addr>>3).U);p.flit.RetToSrc.get.poke(s.ret.U)
      p.flit.DoNotGoToSD_DoNotDataPull.get.poke(s.dnSD.U)
      p.flit.FwdNID.get.poke(s.fwd.U);p.flit.FwdTxnID_StashLPIDValid_StashLPID_VMIDExt.get.poke(s.fwdTxn.U)
    }
  }
  def sample(c:Int):Unit={
    for((name,valid,flit)<-Seq(
      ("TXREQ",dut.io.chi.txreq.flitv,dut.io.chi.txreq.flit),
      ("TXRSP",dut.io.chi.txrsp.flitv,dut.io.chi.txrsp.flit),
      ("TXDAT",dut.io.chi.txdat.flitv,dut.io.chi.txdat.flit),
      ("RXRSP",dut.io.chi.rxrsp.flitv,dut.io.chi.rxrsp.flit),
      ("RXDAT",dut.io.chi.rxdat.flitv,dut.io.chi.rxdat.flit),
      ("RXSNP",dut.io.chi.rxsnp.flitv,dut.io.chi.rxsnp.flit))) {
      if(f(valid))model.trace(s"HN_EXT_FLIT cycle=$c channel=$name fields="+
        flit.elements.toSeq.collect{case(k,u:UInt)=>s"$k=${v(u)}"}.mkString(","))
    }
    // Outbound acknowledgments cannot be justified by an incoming response
    // on this same edge. Inbound completions likewise cannot bypass old SNP.
    if(f(dut.io.chi.txrsp.flitv)) {
      val p=dut.io.chi.txrsp.flit;val op=v(p.Opcode.get).toInt
      if(op==2)model.compAck(v(p.SrcID.get),v(p.TgtID.get),v(p.TxnID.get),c)
      else if(Set(1,9)(op)) {
        val before=model.pendingSnoops
        model.snoopResponse(v(p.SrcID.get),v(p.TgtID.get),v(p.TxnID.get),op,v(p.Resp.get).toInt,v(p.FwdState_DataPull.get).toInt,None,None,c)
        if(model.pendingSnoops<before)onSnoopDone(v(p.TxnID.get),c)
      } else model.check(false,s"unexpected TXRSP opcode=$op cycle=$c")
    }
    if(f(dut.io.chi.txdat.flitv)) {
      val p=dut.io.chi.txdat.flit;val op=v(p.Opcode.get).toInt
      if(op==4)model.forwardedData(v(p.SrcID.get),v(p.TgtID.get),v(p.TxnID.get),v(p.HomeNID.get),v(p.DBID.get),v(p.Resp.get).toInt,v(p.DataID.get).toInt,v(p.Data.get),c)
      else if(op==2)model.copyBackData(v(p.SrcID.get),v(p.TgtID.get),v(p.TxnID.get),v(p.Resp.get).toInt,v(p.DataID.get).toInt,v(p.Data.get),c)
      else if(Set(1,6)(op))model.snoopResponse(v(p.SrcID.get),v(p.TgtID.get),v(p.TxnID.get),op,v(p.Resp.get).toInt,(v(p.FwdState_DataPull_DataSource.get)&7).toInt,Some(v(p.DataID.get).toInt),Some(v(p.Data.get)),c)
      else model.check(false,s"unexpected TXDAT opcode=$op cycle=$c")
    }
    if(f(dut.io.chi.rxrsp.flitv)) {
      val p=dut.io.chi.rxrsp.flit
      val r=HNExtRsp(v(p.TgtID.get),v(p.TxnID.get),v(p.Opcode.get).toInt,v(p.SrcID.get),v(p.DBID_PGroupID_StashGroupID_TagGroupID.get),v(p.Resp.get).toInt,v(p.PCrdType.get).toInt)
      model.check(driven.rsp.contains(r),"RXRSP differs from chosen message");model.rxRsp(r,c)
    }
    if(f(dut.io.chi.rxdat.flitv)) {
      val p=dut.io.chi.rxdat.flit;val chosen=driven.dat.get
      val d=HNExtDat(v(p.TgtID.get),v(p.TxnID.get),v(p.Opcode.get).toInt,v(p.SrcID.get),v(p.HomeNID.get),v(p.DBID.get),v(p.Resp.get).toInt,v(p.DataID.get).toInt,chosen.addr)
      model.check(chosen==d&&v(p.Data.get)==model.memory(d.addr,d.beat/2),"RXDAT differs from chosen message")
      model.rxDat(d,c);onData(d,c)
    }
    if(f(dut.io.chi.rxsnp.flitv)) {
      val p=dut.io.chi.rxsnp.flit
      val s=HNExtSnp(requester,v(p.SrcID.get),v(p.TxnID.get),v(p.Addr.get)<<3,v(p.Opcode.get).toInt,v(p.RetToSrc.get).toInt,v(p.DoNotGoToSD_DoNotDataPull.get).toInt,v(p.FwdNID.get),v(p.FwdTxnID_StashLPIDValid_StashLPID_VMIDExt.get))
      model.check(driven.snp.contains(s),"RXSNP differs from chosen message");model.rxSnp(s,c)
    }
    if(f(dut.io.chi.txreq.flitv)){val q=req;model.request(q,c);onRequest(q,c)}
    // v3 records HN/CDP windows in StressHNCDPOverlap; shared protocol checks above stay active.
  }
}
