package oceanus.chi.link

import chisel3._
import chisel3.util.Cat
import oceanus.chi.EnumCHIChannel
import oceanus.chi.bundle.AbstractCHIBundle
import oceanus.chi.channel.AbstractCHIChannel
import xscache.chi.ChannelIO

object OceanusChannelAdapter {

  def connectTX[T <: AbstractCHIBundle](
    source: ChannelIO[T],
    sink: AbstractCHIChannel[T],
    expectedChannel: EnumCHIChannel
  ): Unit = {
    require(sink.channelType == expectedChannel)
    require(source.flit.getWidth == sink.flit.getWidth)

    sink.flitpend := source.flitpend
    sink.flitv := source.flitv
    source.lcrdv := sink.lcrdv

    var lsb = 0
    sink.flit.getElements.reverse.foreach { element =>
      val width = element.getWidth
      if (width > 0) {
        element := source.flit(lsb + width - 1, lsb).asTypeOf(element)
        lsb += width
      }
    }
    require(lsb == source.flit.getWidth)
  }

  def connectRX[T <: AbstractCHIBundle](
    source: AbstractCHIChannel[T],
    sink: ChannelIO[T],
    expectedChannel: EnumCHIChannel
  ): Unit = {
    require(source.channelType == expectedChannel)
    require(source.flit.getWidth == sink.flit.getWidth)

    sink.flitpend := source.flitpend
    sink.flitv := source.flitv
    sink.flit := Cat(source.flit.getElements.map(_.asUInt))
    source.lcrdv := sink.lcrdv
  }
}
