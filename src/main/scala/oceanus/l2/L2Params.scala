package oceanus.l2

import org.chipsalliance.cde.config.Field
import org.chipsalliance.cde.config.Parameters
import oceanus.chi.HasCHIParameters
import chisel3.util.{log2Ceil, log2Up}
import utility.Code

/**
  * Cache geometry is per Directory/slice: `sets` x `ways` lines of `blockBytes`,
  * with `mshrSize` TSHR slots and a `physicalAddrWidth`-bit physical address.
  * `dataSRAMSplit` sets the data banking factor. `enableDataECC` enables the
  * codec selected by `dataECC`; `hasMbist` and `hasSramCtl` control DFT support.
  * `replacement` selects plru, drrip, srrip, or brrip.
  *
  * RRIP replacement storage has three configurations:
  *   - drripUseSRAM=false: RRPV and origin use FF rows; originUseSRAM is ignored.
  *   - drripUseSRAM=true, originUseSRAM=false: RRPV uses SRAM plus a CAM, while
  *     origin uses FF rows; CAM entries contain RRPV but no origin payload.
  *   - drripUseSRAM=true, originUseSRAM=true: both use SRAM plus a CAM whose
  *     entries contain complete RRPV and origin rows.
  * PLRU keeps its state in the FF replacement rows and does not use the RRIP CAM
  * or origin updates. The drripUseSRAM name is shared by all RRIP policies.
  *
  * enableDRRIPReleaseUpdate enables the DirRel path only for DRRIP/SRRIP;
  * BRRIP and PLRU omit the EVT/proxy release interface even when this is true.
  * dirRelArbitration selects strict DirRel priority (dirrel-high),
  * class-level round-robin (rr), or normal-request priority (dirrel-low).
  * dirRelSingleCycle=false uses the registered two-cycle DirRel sequence. The
  * single-cycle option is restricted to the RRIP-SRAM/origin-FF configuration.
  * enableDRRIPEntranceReleaseUpdate is a deprecated no-op retained only for
  * source compatibility; entrance release training has been removed.
  *
  * These are case-class defaults, not the release-top overrides. Top_L2Top uses
  * 8 ways, 1024 sets, 32 TSHRs by default, ECC enabled, and MBIST enabled.
  */
case class L2Params(
    physicalAddrWidth: Int = 48,
    mshrSize: Int = 64,
    ways: Int = 4,
    sets: Int = 128,
    blockBytes: Int = 64,
    dataSRAMSplit: Int = 4,
    enableDataECC: Boolean = false,
    hasMbist: Boolean = false,
    hasSramCtl: Boolean = false,
    dataECC: Option[String] = Some("secded"),
    replacement: String = "drrip",
    drripUseSRAM: Boolean = true,
    originUseSRAM: Boolean = true,
    // DRRIP release-type updates use the arbitrated DirRel operation issued
    // when the EVT vPipe accepts an Evict/WriteBackFull.
    enableDRRIPReleaseUpdate: Boolean = true,
    dirRelArbitration: String = "rr",
    dirRelSingleCycle: Boolean = false,
    enableDRRIPEntranceReleaseUpdate: Boolean = true,
) {
    require(Seq("dirrel-high", "rr", "dirrel-low").contains(dirRelArbitration),
      s"DirRel arbitration must be dirrel-high, rr, or dirrel-low: $dirRelArbitration")
    require(!dirRelSingleCycle || (drripUseSRAM && !originUseSRAM),
      "Single-cycle DirRel requires the RRIP-SRAM/origin-FF backend")
}

case object L2ParamsKey extends Field[L2Params](L2Params())
case object L2SliceDirStateProbeEnableKey extends Field[Boolean](false)
case object L2SliceDirPlruProbeEnableKey extends Field[Boolean](false)
case object L2SliceDisableReplRetryReplayKey extends Field[Boolean](false)

trait HasL2Params extends HasCHIParameters {

    implicit val p: Parameters

    val paramL2 = p(L2ParamsKey)

    def enableRRIPReleaseUpdate: Boolean = paramL2.enableDRRIPReleaseUpdate &&
      Seq("drrip", "srrip").contains(paramL2.replacement)
    def mshrIndexWidth = log2Up(paramL2.mshrSize) + 1
    def nMSHR = paramL2.mshrSize
    def offsetBits = log2Ceil(paramL2.blockBytes)
    def setBits = log2Ceil(paramL2.sets)
    def wayBits = log2Ceil(paramL2.ways)
    def tagWidth = paramL2.physicalAddrWidth - offsetBits - setBits
    def blocks = paramL2.sets * paramL2.ways
    def blockBits = paramCHI.dataWidth * 2
    def beatBytes = paramCHI.dataWidth / 8
    def dataBankSplit = paramL2.dataSRAMSplit
    def dataBankBits = blockBits / dataBankSplit
    def encBankBits = cacheParams.dataCode.width(dataBankBits)
    def encDataPadBits = 0
    def cacheParams = paramL2
    def enableDataECC = paramL2.enableDataECC

    require(paramCHI.dataWidth == 256, "L2 requires CHI data width to be 256 bits")
}

object L2Params {
    implicit final class L2ParamsOps(private val p: L2Params) extends AnyVal {
        def dataCode: Code = Code.fromString(p.dataECC)
    }
}
