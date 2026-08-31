package oceanus.compactchi

import chisel3._
import chisel3.util._

class CCHIInterfaceType1 extends Bundle {
  // TODO: also applicable for Link Credit, but sanity check required here in future
  val UpEVT = Flipped(Decoupled(new FlitEVT))
  val UpREQ = Flipped(Decoupled(new FlitREQ))
  val DnSNP = Decoupled(new FlitSNP)
  val UpRSP = Flipped(Decoupled(new FlitUpRSP))
  val UpDAT = Flipped(Decoupled(new FlitUpDAT))
  val DnRSP = Decoupled(new FlitDnRSP)
  val DnDAT = Decoupled(new FlitDnDAT)

  def <>(other: CCHIInterfaceType1) = {
    other.UpEVT <> UpEVT
    other.UpREQ <> UpREQ
    other.DnSNP <> DnSNP
    other.UpRSP <> UpRSP
    other.UpDAT <> UpDAT
    other.DnRSP <> DnRSP
    other.DnDAT <> DnDAT
  }
}

class CCHIInterfaceType4 extends Bundle {
  // TODO: also applicable for Link Credit, but sanity check required here in future
  val UpREQ = Flipped(Decoupled(new FlitREQ))
  val DnDAT = Decoupled(new FlitDnDAT)

  def <>(other: CCHIInterfaceType1) = {
    other.UpREQ <> UpREQ
    other.DnDAT <> DnDAT
  }
}
