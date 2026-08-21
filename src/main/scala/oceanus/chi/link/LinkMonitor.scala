package oceanus.chi.link

import chisel3._
import chisel3.util._
import oceanus.chi.EnumCHIChannel.{DAT, REQ, RSP, SNP}
import oceanus.chi.bundle._
import oceanus.chi.intf.CHIRNFInterface
import org.chipsalliance.cde.config.Parameters
import xscache.chi.{
  ChannelIO,
  Decoupled2LCredit,
  LCredit2Decoupled,
  LinkState,
  LinkStates
}

case class RNFLinkParams(
  rxsnpLCredits: Int = 4,
  rxrspLCredits: Int = 15,
  rxdatLCredits: Int = 15
)

class LinkMonitor(linkParams: RNFLinkParams = RNFLinkParams())(implicit val p: Parameters)
    extends Module {
  val io = IO(new Bundle {
    val txreq = Flipped(Decoupled(new CHIBundleREQ))
    val txrsp = Flipped(Decoupled(new CHIBundleRSP))
    val txdat = Flipped(Decoupled(new CHIBundleDAT))

    val rxsnp = Decoupled(new CHIBundleSNP)
    val rxrsp = Valid(new CHIBundleRSP)
    val rxdat = Valid(new CHIBundleDAT)

    val out = new CHIRNFInterface
    val linkEnable = Input(Bool())
  })

  val txState = RegInit(LinkStates.STOP)
  val rxState = RegInit(LinkStates.STOP)

  txState := nextLinkState(io.out.txlinkactivereq, io.out.txlinkactiveack)
  rxState := nextLinkState(io.out.rxlinkactivereq, io.out.rxlinkactiveack)

  val txreqChannel = Wire(new ChannelIO(new CHIBundleREQ))
  val txrspChannel = Wire(new ChannelIO(new CHIBundleRSP))
  val txdatChannel = Wire(new ChannelIO(new CHIBundleDAT))

  Decoupled2LCredit(io.txreq, txreqChannel, LinkState(txState), Some("oceanus_txreq"))
  Decoupled2LCredit(io.txrsp, txrspChannel, LinkState(txState), Some("oceanus_txrsp"))
  Decoupled2LCredit(io.txdat, txdatChannel, LinkState(txState), Some("oceanus_txdat"))

  OceanusChannelAdapter.connectTX(txreqChannel, io.out.txreq, REQ)
  OceanusChannelAdapter.connectTX(txrspChannel, io.out.txrsp, RSP)
  OceanusChannelAdapter.connectTX(txdatChannel, io.out.txdat, DAT)

  val rxsnpChannel = Wire(new ChannelIO(new CHIBundleSNP))
  val rxrspChannel = Wire(new ChannelIO(new CHIBundleRSP))
  val rxdatChannel = Wire(new ChannelIO(new CHIBundleDAT))

  OceanusChannelAdapter.connectRX(io.out.rxsnp, rxsnpChannel, SNP)
  OceanusChannelAdapter.connectRX(io.out.rxrsp, rxrspChannel, RSP)
  OceanusChannelAdapter.connectRX(io.out.rxdat, rxdatChannel, DAT)

  val rxsnpReclaimed, rxrspReclaimed, rxdatReclaimed = Wire(Bool())
  LCredit2Decoupled(
    rxsnpChannel,
    io.rxsnp,
    LinkState(rxState),
    rxsnpReclaimed,
    Some("oceanus_rxsnp"),
    linkParams.rxsnpLCredits,
    blocking = true
  )

  val rxrsp = Wire(Decoupled(new CHIBundleRSP))
  val rxdat = Wire(Decoupled(new CHIBundleDAT))
  rxrsp.ready := true.B
  rxdat.ready := true.B

  LCredit2Decoupled(
    rxrspChannel,
    rxrsp,
    LinkState(rxState),
    rxrspReclaimed,
    Some("oceanus_rxrsp"),
    linkParams.rxrspLCredits,
    blocking = false
  )
  LCredit2Decoupled(
    rxdatChannel,
    rxdat,
    LinkState(rxState),
    rxdatReclaimed,
    Some("oceanus_rxdat"),
    linkParams.rxdatLCredits,
    blocking = false
  )

  io.rxrsp.valid := rxrsp.valid
  io.rxrsp.bits := rxrsp.bits
  io.rxdat.valid := rxdat.valid
  io.rxdat.bits := rxdat.bits

  val rxCreditsReclaimed = rxsnpReclaimed && rxrspReclaimed && rxdatReclaimed
  io.out.txlinkactivereq := RegNext(io.linkEnable, init = false.B)
  io.out.rxlinkactiveack := RegNext(
    RegNext(io.out.rxlinkactivereq, init = false.B) || !rxCreditsReclaimed,
    init = false.B
  )
  io.out.txsactive := RegNext(io.linkEnable, init = false.B)

  private def nextLinkState(req: Bool, ack: Bool): UInt =
    MuxLookup(Cat(req, ack), LinkStates.STOP)(Seq(
      Cat(true.B, false.B) -> LinkStates.ACTIVATE,
      Cat(true.B, true.B) -> LinkStates.RUN,
      Cat(false.B, true.B) -> LinkStates.DEACTIVATE,
      Cat(false.B, false.B) -> LinkStates.STOP
    ))
}
