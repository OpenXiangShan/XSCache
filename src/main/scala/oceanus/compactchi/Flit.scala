package oceanus.compactchi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xscache.oceanus.compactchi.HasCCHIParameters

class FlitEVTStripped(implicit val p: Parameters) extends Bundle with HasCCHIParameters {
  val TxnID = UInt(paramCCHI.TxnID_Width.W)
  val SrcID = UInt(paramCCHI.UpstreamNodeID_Width.W)
  val TgtID = UInt(paramCCHI.DownstreamNodeID_Width.W)
  val Opcode = UInt(1.W)
  val NS = Bool()
  val MemAttr = Bool() // allocate hint per CCHI spec; currently not consumed by L2
  val WayValid = Bool()
  val Way = UInt(paramCCHI.WayIndex_Width.W)
  val TraceTag = UInt(1.W)
}

class FlitEVT(implicit p: Parameters) extends FlitEVTStripped {
  val Addr = UInt(48.W) // TODO: configured by Addr_Width
}

class FlitREQStripped(implicit val p: Parameters) extends Bundle with HasCCHIParameters {
  val TxnID = UInt(paramCCHI.TxnID_Width.W)
  val SrcID = UInt(paramCCHI.UpstreamNodeID_Width.W)
  val TgtID = UInt(paramCCHI.DownstreamNodeID_Width.W)
  val Opcode = UInt(6.W) // TODO: variable width between different types of components
  val TagAlias = UInt(paramCCHI.TagAlias_Width.W)
  val Size = UInt(3.W)
  val NS = Bool()
  val Order = UInt(2.W)
  val MemAttr = UInt(4.W)
  val Excl = Bool()
  val ExpCompData = Bool()
  def ExpCompStash = ExpCompData
  val WayValid = Bool()
  val Way = UInt(paramCCHI.WayIndex_Width.W)
  val TraceTag = UInt(1.W)
}

class FlitREQ(implicit p: Parameters) extends FlitREQStripped {
  val Addr = UInt(48.W) // TODO: configured by Addr_Width
}

class FlitSNPStripped(implicit val p: Parameters) extends Bundle with HasCCHIParameters {
  val TxnID = UInt(paramCCHI.DBID_Width.W) 
  val SrcID = UInt(paramCCHI.DownstreamNodeID_Width.W)
  val TgtID = UInt(paramCCHI.UpstreamNodeID_Width.W)
  val Opcode = UInt(2.W) // TODO: variable width between different types of components
  val NS = Bool()
  val TraceTag = UInt(1.W)
}

class FlitSNP(implicit p: Parameters) extends FlitSNPStripped {
  val Addr = UInt((48 - 3).W) // TODO: configured by Addr_Width
  val alias = UInt(2.W) // L2->L1 snoop locator; pprobe uses localMeta.alias, rprobe uses req.alias
}

class FlitDnRSP(implicit val p: Parameters) extends Bundle with HasCCHIParameters {
  val TxnID = UInt(paramCCHI.TxnID_Width.W)
  val SrcID = UInt(paramCCHI.DownstreamNodeID_Width.W)
  val TgtID = UInt(paramCCHI.UpstreamNodeID_Width.W)
  val DBID = UInt(paramCCHI.DBID_Width.W)
  val Opcode = UInt(3.W) // TODO: variable width between different types of components
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val CBusy = UInt(3.W)
  val WayValid = Bool()
  val Way = UInt(paramCCHI.WayIndex_Width.W)
  val TraceTag = UInt(1.W)
}

class FlitUpRSP(implicit val p: Parameters) extends Bundle with HasCCHIParameters {
  val TxnID = UInt(paramCCHI.DBID_Width.W)
  val SrcID = UInt(paramCCHI.UpstreamNodeID_Width.W)
  val TgtID = UInt(paramCCHI.DownstreamNodeID_Width.W)
  val Opcode = UInt(1.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val TraceTag = UInt(1.W)
}

class FlitDnDATWithoutData(implicit val p: Parameters) extends Bundle with HasCCHIParameters {
  val TxnID = UInt(paramCCHI.TxnID_Width.W)
  val SrcID = UInt(paramCCHI.DownstreamNodeID_Width.W)
  val TgtID = UInt(paramCCHI.UpstreamNodeID_Width.W)
  val DBID = UInt(paramCCHI.DBID_Width.W)
  val Opcode = UInt(1.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val DataID = UInt(2.W)
  val DataSource = UInt(5.W)
  val CBusy = UInt(3.W)
  val WayValid = Bool()
  val Way = UInt(paramCCHI.WayIndex_Width.W)
  val TraceTag = UInt(1.W)
}

class FlitDnDAT(implicit p: Parameters) extends FlitDnDATWithoutData {
  val Data = UInt(paramCCHI.Data_Width.W)
}

class FlitUpDATWithoutData(implicit val p: Parameters) extends Bundle with HasCCHIParameters {
  val TxnID = UInt(paramCCHI.DBID_Width.W)
  val SrcID = UInt(paramCCHI.UpstreamNodeID_Width.W)
  val TgtID = UInt(paramCCHI.DownstreamNodeID_Width.W)
  val Opcode = UInt(2.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val DataID = UInt(2.W)
  val TraceTag = UInt(1.W)
}

class FlitUpDAT(implicit p: Parameters) extends FlitUpDATWithoutData {
  val Data = UInt(paramCCHI.Data_Width.W)
  val BE = UInt((paramCCHI.Data_Width / 8).W)
}
