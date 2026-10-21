//*****************************************************************************
// biscotto_dut.sv — Biscotto DUT shell binding XSCache TestTop_L2Biscotto
// (3x Oceanus L2) to the CHIron Biscotto integration contract.
//
// TestTop_L2Biscotto already emits the contract pin NAMES (cchi_t1p0..2_*
// separate-field, chi_rn0..2_* packed with ..._flit). This shell adapts the
// remaining shape differences between the DUT's own CCHI contract (which is
// kept independent of the AMBA CHI configuration) and the package's wire
// declarations in biscotto_dut_if.svh (its Type-1 view: 7-bit TxnID, 7-bit
// DBID, 5-bit SrcID/TgtID, 4-bit Way, 8-bit TagAlias, 1-bit DataID,
// unpacked-array Data). All adapted values provably fit the narrower DUT
// side (the harness drives NIDs 0..2 and the L2 allocates DBIDs/Ways/aliases
// from its small internal pools):
//
//   - Data:   contract pins are unpacked arrays of 8x 32-bit words; the DUT
//             emits flat [255:0] pins. Word w maps to bits [w*32 +: 32],
//             matching the harness's little-endian word (VlWide) order.
//   - TxnID:  DUT 8 bits, contract 7 (7 channels).
//   - DBID:   DUT 8 bits, contract 7 (txrsp/txdat).
//   - SrcID/TgtID: DUT 4 bits, contract 5 (all 7 channels).
//   - Way:    DUT 2 bits, contract 4 (rxevt/rxreq/txrsp/txdat).
//   - TagAlias: DUT 2 bits, contract 8 (rxreq).
//   - DataID: DUT 2 bits, contract 1 (rxdat/txdat; with 256-bit data a 64B
//             line is exactly 2 beats, so only DataID[0] is ever used).
//
// RX (harness->DUT) adapters truncate; TX (DUT->harness) adapters zero-extend.
// VCS rejects wildcard `.*` connections with width mismatches, so every
// adapted pin is connected explicitly and `.*` covers only the rest.
//
// The DUT additionally exports passive CHI monitor taps mon_chi_rn0..2_*
// (per channel: flitpend/flitv/lcrdv + the packed flit at the pin width).
// They are plain packed vectors needing no adaptation, so they pass through
// 1:1 by name via `.*` like any other unadapted pin.
//
// The module keeps the default contract name biscotto_dut, so no
// +define+BISCOTTO_DUT_MODULE override is needed.
//*****************************************************************************

`include "biscotto_dut_if.svh"

// ---------------------------------------------------------------------------
// Passive CHI-E RN monitor taps of port P (pure DUT outputs): per channel
// (txreq/txrsp/txdat/rxsnp/rxrsp/rxdat) the 1-bit flitpend/flitv/lcrdv taps
// plus the packed flit at the pin width (REQ 162b, RSP 73b, SNP 115b, DAT
// 422b). Names/widths match TestTop_L2Biscotto's mon_chi_rn<P>_* pins exactly,
// so the `.*` on the DUT instantiation connects them — no adaptation.
// ---------------------------------------------------------------------------
`define BISCOTTO_DUT_CHI_MON_PORT_DECL(P) \
    output logic         mon_chi_rn``P``_txreq_flitpend, \
    output logic         mon_chi_rn``P``_txreq_flitv, \
    output logic         mon_chi_rn``P``_txreq_lcrdv, \
    output logic [161:0] mon_chi_rn``P``_txreq_flit, \
    output logic         mon_chi_rn``P``_txrsp_flitpend, \
    output logic         mon_chi_rn``P``_txrsp_flitv, \
    output logic         mon_chi_rn``P``_txrsp_lcrdv, \
    output logic [72:0]  mon_chi_rn``P``_txrsp_flit, \
    output logic         mon_chi_rn``P``_txdat_flitpend, \
    output logic         mon_chi_rn``P``_txdat_flitv, \
    output logic         mon_chi_rn``P``_txdat_lcrdv, \
    output logic [421:0] mon_chi_rn``P``_txdat_flit, \
    output logic         mon_chi_rn``P``_rxsnp_flitpend, \
    output logic         mon_chi_rn``P``_rxsnp_flitv, \
    output logic         mon_chi_rn``P``_rxsnp_lcrdv, \
    output logic [114:0] mon_chi_rn``P``_rxsnp_flit, \
    output logic         mon_chi_rn``P``_rxrsp_flitpend, \
    output logic         mon_chi_rn``P``_rxrsp_flitv, \
    output logic         mon_chi_rn``P``_rxrsp_lcrdv, \
    output logic [72:0]  mon_chi_rn``P``_rxrsp_flit, \
    output logic         mon_chi_rn``P``_rxdat_flitpend, \
    output logic         mon_chi_rn``P``_rxdat_flitv, \
    output logic         mon_chi_rn``P``_rxdat_lcrdv, \
    output logic [421:0] mon_chi_rn``P``_rxdat_flit

module biscotto_dut (
    input  logic        clock,
    input  logic        reset,

    `BISCOTTO_CCHI_T1_PORT_DECL(0),
    `BISCOTTO_CCHI_T1_PORT_DECL(1),
    `BISCOTTO_CCHI_T1_PORT_DECL(2),

    `BISCOTTO_CHI_RN_PORT_DECL(0),
    `BISCOTTO_CHI_RN_PORT_DECL(1),
    `BISCOTTO_CHI_RN_PORT_DECL(2),

    `BISCOTTO_DUT_CHI_MON_PORT_DECL(0),
    `BISCOTTO_DUT_CHI_MON_PORT_DECL(1),
    `BISCOTTO_DUT_CHI_MON_PORT_DECL(2)
);

    // ------------------------------------------------------------------
    // Shape adapters per port (see header). Wire suffix _q4/_q2/_q1 carry
    // the DUT-side (narrower) values; _flat the flat 256-bit data.
    // ------------------------------------------------------------------
`define BISCOTTO_DUT_ADAPT(P) \
    logic [255:0] txdat_data_flat_``P; \
    logic [255:0] rxdat_data_flat_``P; \
    logic [1:0]   txdat_dataid_``P; \
    logic [1:0]   rxdat_dataid_``P; \
    logic [7:0]   rxevt_txnid_``P, rxreq_txnid_``P, rxrsp_txnid_``P, rxdat_txnid_``P; \
    logic [7:0]   txsnp_txnid_``P, txrsp_txnid_``P, txdat_txnid_``P; \
    logic [7:0]   txrsp_dbid_``P, txdat_dbid_``P; \
    logic [3:0]   rxevt_srcid_``P, rxevt_tgtid_``P, rxreq_srcid_``P, rxreq_tgtid_``P; \
    logic [3:0]   rxrsp_srcid_``P, rxrsp_tgtid_``P, rxdat_srcid_``P, rxdat_tgtid_``P; \
    logic [3:0]   txsnp_srcid_``P, txsnp_tgtid_``P, txrsp_srcid_``P, txrsp_tgtid_``P; \
    logic [3:0]   txdat_srcid_``P, txdat_tgtid_``P; \
    logic [1:0]   rxevt_way_``P, rxreq_way_``P, txrsp_way_``P, txdat_way_``P; \
    logic [1:0]   rxreq_tagalias_``P; \
    for (genvar w = 0; w < 8; w++) begin : g_data_adapt_``P \
        assign cchi_t1p``P``_txdat_bits_Data[w] = txdat_data_flat_``P``[w*32 +: 32]; \
        assign rxdat_data_flat_``P``[w*32 +: 32] = cchi_t1p``P``_rxdat_bits_Data[w]; \
    end \
    /* TX (DUT->contract): zero-extend */ \
    assign cchi_t1p``P``_txsnp_bits_TxnID  = txsnp_txnid_``P``[6:0]; \
    assign cchi_t1p``P``_txrsp_bits_TxnID  = txrsp_txnid_``P``[6:0]; \
    assign cchi_t1p``P``_txdat_bits_TxnID  = txdat_txnid_``P``[6:0]; \
    assign cchi_t1p``P``_txrsp_bits_DBID   = txrsp_dbid_``P``[6:0]; \
    assign cchi_t1p``P``_txdat_bits_DBID   = txdat_dbid_``P``[6:0]; \
    assign cchi_t1p``P``_txsnp_bits_SrcID  = {1'b0, txsnp_srcid_``P}; \
    assign cchi_t1p``P``_txsnp_bits_TgtID  = {1'b0, txsnp_tgtid_``P}; \
    assign cchi_t1p``P``_txrsp_bits_SrcID  = {1'b0, txrsp_srcid_``P}; \
    assign cchi_t1p``P``_txrsp_bits_TgtID  = {1'b0, txrsp_tgtid_``P}; \
    assign cchi_t1p``P``_txdat_bits_SrcID  = {1'b0, txdat_srcid_``P}; \
    assign cchi_t1p``P``_txdat_bits_TgtID  = {1'b0, txdat_tgtid_``P}; \
    assign cchi_t1p``P``_txrsp_bits_Way    = {2'b00, txrsp_way_``P}; \
    assign cchi_t1p``P``_txdat_bits_Way    = {2'b00, txdat_way_``P}; \
    assign cchi_t1p``P``_txdat_bits_DataID = txdat_dataid_``P``[0]; \
    /* RX (contract->DUT): truncate (values provably fit) */ \
    assign rxevt_txnid_``P  = cchi_t1p``P``_rxevt_bits_TxnID; \
    assign rxreq_txnid_``P  = cchi_t1p``P``_rxreq_bits_TxnID; \
    assign rxrsp_txnid_``P  = cchi_t1p``P``_rxrsp_bits_TxnID; \
    assign rxdat_txnid_``P  = cchi_t1p``P``_rxdat_bits_TxnID; \
    assign rxevt_srcid_``P  = cchi_t1p``P``_rxevt_bits_SrcID[3:0]; \
    assign rxevt_tgtid_``P  = cchi_t1p``P``_rxevt_bits_TgtID[3:0]; \
    assign rxreq_srcid_``P  = cchi_t1p``P``_rxreq_bits_SrcID[3:0]; \
    assign rxreq_tgtid_``P  = cchi_t1p``P``_rxreq_bits_TgtID[3:0]; \
    assign rxrsp_srcid_``P  = cchi_t1p``P``_rxrsp_bits_SrcID[3:0]; \
    assign rxrsp_tgtid_``P  = cchi_t1p``P``_rxrsp_bits_TgtID[3:0]; \
    assign rxdat_srcid_``P  = cchi_t1p``P``_rxdat_bits_SrcID[3:0]; \
    assign rxdat_tgtid_``P  = cchi_t1p``P``_rxdat_bits_TgtID[3:0]; \
    assign rxevt_way_``P    = cchi_t1p``P``_rxevt_bits_Way[1:0]; \
    assign rxreq_way_``P    = cchi_t1p``P``_rxreq_bits_Way[1:0]; \
    assign rxreq_tagalias_``P = cchi_t1p``P``_rxreq_bits_TagAlias[1:0]; \
    assign rxdat_dataid_``P = {1'b0, cchi_t1p``P``_rxdat_bits_DataID};

    `BISCOTTO_DUT_ADAPT(0)
    `BISCOTTO_DUT_ADAPT(1)
    `BISCOTTO_DUT_ADAPT(2)

`undef BISCOTTO_DUT_ADAPT

    // ------------------------------------------------------------------
    // DUT: 3x Oceanus L2 (XSCache TestTop_L2Biscotto). All unadapted pins
    // connect 1:1 by name via .* — including the passive mon_chi_rn0..2_*
    // monitor taps; the logging knobs are tied off, and the DUT's snoop
    // TagAlias outputs (outside the biscotto contract) are left open.
    // ------------------------------------------------------------------
`define BISCOTTO_DUT_PORT_ADAPT(P) \
    .cchi_t1p``P``_txdat_bits_Data   (txdat_data_flat_``P), \
    .cchi_t1p``P``_rxdat_bits_Data   (rxdat_data_flat_``P), \
    .cchi_t1p``P``_rxevt_bits_TxnID  (rxevt_txnid_``P), \
    .cchi_t1p``P``_rxreq_bits_TxnID  (rxreq_txnid_``P), \
    .cchi_t1p``P``_rxrsp_bits_TxnID  (rxrsp_txnid_``P), \
    .cchi_t1p``P``_rxdat_bits_TxnID  (rxdat_txnid_``P), \
    .cchi_t1p``P``_txsnp_bits_TxnID  (txsnp_txnid_``P), \
    .cchi_t1p``P``_txrsp_bits_TxnID  (txrsp_txnid_``P), \
    .cchi_t1p``P``_txdat_bits_TxnID  (txdat_txnid_``P), \
    .cchi_t1p``P``_txrsp_bits_DBID   (txrsp_dbid_``P), \
    .cchi_t1p``P``_txdat_bits_DBID   (txdat_dbid_``P), \
    .cchi_t1p``P``_rxevt_bits_SrcID  (rxevt_srcid_``P), \
    .cchi_t1p``P``_rxevt_bits_TgtID  (rxevt_tgtid_``P), \
    .cchi_t1p``P``_rxreq_bits_SrcID  (rxreq_srcid_``P), \
    .cchi_t1p``P``_rxreq_bits_TgtID  (rxreq_tgtid_``P), \
    .cchi_t1p``P``_rxrsp_bits_SrcID  (rxrsp_srcid_``P), \
    .cchi_t1p``P``_rxrsp_bits_TgtID  (rxrsp_tgtid_``P), \
    .cchi_t1p``P``_rxdat_bits_SrcID  (rxdat_srcid_``P), \
    .cchi_t1p``P``_rxdat_bits_TgtID  (rxdat_tgtid_``P), \
    .cchi_t1p``P``_txsnp_bits_SrcID  (txsnp_srcid_``P), \
    .cchi_t1p``P``_txsnp_bits_TgtID  (txsnp_tgtid_``P), \
    .cchi_t1p``P``_txrsp_bits_SrcID  (txrsp_srcid_``P), \
    .cchi_t1p``P``_txrsp_bits_TgtID  (txrsp_tgtid_``P), \
    .cchi_t1p``P``_txdat_bits_SrcID  (txdat_srcid_``P), \
    .cchi_t1p``P``_txdat_bits_TgtID  (txdat_tgtid_``P), \
    .cchi_t1p``P``_rxevt_bits_Way    (rxevt_way_``P), \
    .cchi_t1p``P``_rxreq_bits_Way    (rxreq_way_``P), \
    .cchi_t1p``P``_txrsp_bits_Way    (txrsp_way_``P), \
    .cchi_t1p``P``_txdat_bits_Way    (txdat_way_``P), \
    .cchi_t1p``P``_rxreq_bits_TagAlias (rxreq_tagalias_``P), \
    .cchi_t1p``P``_txdat_bits_DataID (txdat_dataid_``P), \
    .cchi_t1p``P``_rxdat_bits_DataID (rxdat_dataid_``P),

    TestTop_L2Biscotto u_dut (
        `BISCOTTO_DUT_PORT_ADAPT(0)
        `BISCOTTO_DUT_PORT_ADAPT(1)
        `BISCOTTO_DUT_PORT_ADAPT(2)
        .cchi_t1p0_txsnp_bits_alias (),
        .cchi_t1p1_txsnp_bits_alias (),
        .cchi_t1p2_txsnp_bits_alias (),
        .log_dump                  (1'b0),
        .log_clean                 (1'b0),
        .*
    );

`undef BISCOTTO_DUT_PORT_ADAPT
`undef BISCOTTO_DUT_CHI_MON_PORT_DECL

endmodule
