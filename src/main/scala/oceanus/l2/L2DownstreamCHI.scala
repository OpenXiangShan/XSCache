package oceanus.l2

import chisel3._
import chisel3.util._
import oceanus.chi.EnumCHIChannel.{DAT, REQ, RSP, SNP}
import oceanus.chi.bundle._
import oceanus.chi.intf.CHIRNFInterface
import oceanus.chi.link.OceanusChannelAdapter
import oceanus.chi.opcode.CHIRNFOpcodesRSP
import org.chipsalliance.cde.config.Parameters
import xscache.chi.{ChannelIO, Decoupled2LCredit, LCredit2Decoupled, LinkState, LinkStates}

case class RNFLinkParams(
  rxsnpLCredits: Int = 4,
  rxrspLCredits: Int = 15,
  rxdatLCredits: Int = 15
)

class L2DownstreamCHI(
  linkParams: RNFLinkParams = RNFLinkParams()
)(implicit val p: Parameters)
    extends Module
    with HasL2Params
    with CHIRNFOpcodesRSP {
  val io = IO(new Bundle {
    val txreq = Flipped(Decoupled(new CHIBundleREQ))
    val txrsp = Flipped(Decoupled(new CHIBundleRSP))
    val txdat = Flipped(Decoupled(new CHIBundleDAT))
    val rxsnp = Decoupled(new CHIBundleSNP)
    val rxrsp = Valid(new CHIBundleRSP)
    val rxdat = Valid(new CHIBundleDAT)
    val pCrdGrant = Valid(new L2PCreditPool.Entry)

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
  val rxsnpChannel = Wire(new ChannelIO(new CHIBundleSNP))
  val rxrspChannel = Wire(new ChannelIO(new CHIBundleRSP))
  val rxdatChannel = Wire(new ChannelIO(new CHIBundleDAT))

  val txreqQueue = Module(new Queue(new CHIBundleREQ, entries = 2, pipe = false, flow = false))
  val txrspQueue = Module(new Queue(new CHIBundleRSP, entries = 2, pipe = false, flow = false))
  val txdatQueue = Module(new Queue(new CHIBundleDAT, entries = 2, pipe = false, flow = false))
  val rxsnpQueue = Module(new Queue(new CHIBundleSNP, entries = 2, pipe = false, flow = false))

  txreqQueue.io.enq <> io.txreq
  txrspQueue.io.enq <> io.txrsp
  txdatQueue.io.enq <> io.txdat

  Decoupled2LCredit(txreqQueue.io.deq, txreqChannel, LinkState(txState), Some("oceanus_txreq"))
  Decoupled2LCredit(txrspQueue.io.deq, txrspChannel, LinkState(txState), Some("oceanus_txrsp"))
  Decoupled2LCredit(txdatQueue.io.deq, txdatChannel, LinkState(txState), Some("oceanus_txdat"))

  OceanusChannelAdapter.connectTX(txreqChannel, io.out.txreq, REQ)
  OceanusChannelAdapter.connectTX(txrspChannel, io.out.txrsp, RSP)
  OceanusChannelAdapter.connectTX(txdatChannel, io.out.txdat, DAT)
  OceanusChannelAdapter.connectRX(io.out.rxsnp, rxsnpChannel, SNP)
  OceanusChannelAdapter.connectRX(io.out.rxrsp, rxrspChannel, RSP)
  OceanusChannelAdapter.connectRX(io.out.rxdat, rxdatChannel, DAT)

  val rxsnpLink = Wire(Decoupled(new CHIBundleSNP))
  val rxrspLink = Wire(Decoupled(new CHIBundleRSP))
  val rxdatLink = Wire(Decoupled(new CHIBundleDAT))
  val rxsnpDeact, rxrspDeact, rxdatDeact = Wire(Bool())

  LCredit2Decoupled(
    rxsnpChannel,
    rxsnpLink,
    LinkState(rxState),
    rxsnpDeact,
    Some("oceanus_rxsnp"),
    linkParams.rxsnpLCredits,
    blocking = true
  )
  LCredit2Decoupled(
    rxrspChannel,
    rxrspLink,
    LinkState(rxState),
    rxrspDeact,
    Some("oceanus_rxrsp"),
    linkParams.rxrspLCredits,
    blocking = false
  )
  LCredit2Decoupled(
    rxdatChannel,
    rxdatLink,
    LinkState(rxState),
    rxdatDeact,
    Some("oceanus_rxdat"),
    linkParams.rxdatLCredits,
    blocking = false
  )

  rxsnpQueue.io.enq <> rxsnpLink
  io.rxsnp <> rxsnpQueue.io.deq

  rxrspLink.ready := true.B
  rxdatLink.ready := true.B

  val rxrspIsPCrdGrant = rxrspLink.bits.Opcode.get === CHI_PCrdGrant.asUIntForRSP
  io.rxrsp.valid := rxrspLink.valid && !rxrspIsPCrdGrant
  io.rxrsp.bits := rxrspLink.bits
  io.rxdat.valid := rxdatLink.valid
  io.rxdat.bits := rxdatLink.bits

  io.pCrdGrant.valid := rxrspLink.valid && rxrspIsPCrdGrant
  io.pCrdGrant.bits.srcId := rxrspLink.bits.SrcID.get
  io.pCrdGrant.bits.pCrdType := rxrspLink.bits.PCrdType.get

  val rxDeact = rxsnpDeact && rxrspDeact && rxdatDeact

  io.out.txlinkactivereq := RegNext(io.linkEnable, init = false.B)
  io.out.rxlinkactiveack := RegNext(
    RegNext(io.out.rxlinkactivereq, init = false.B) || !rxDeact,
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
