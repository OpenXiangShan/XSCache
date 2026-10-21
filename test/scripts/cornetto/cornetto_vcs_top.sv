//*****************************************************************************
// cornetto_vcs_top.sv — Cornetto DUT top adapter for the cohestra_vcs (VCS)
// harness.
//
// The cohestra_vcs bridge (cohestra_vcs_bridge.sv) instantiates the DUT top
// with the full Avocado-style boundary contract: 4x Type-1 CCHI ports, 2x
// AXI4 memory ports, 1x AXI4 config port (plus optional CHI monitor taps,
// compiled out for non-avocado tops). cornetto_top itself exports only 2x
// CCHI (t1p0..1) + 1x AXI memory (axi_m0) + 1x AXI config (axi_s0).
//
// This adapter presents the full contract to the bridge and instantiates
// cornetto_top underneath:
//   - t1p0/t1p1, axi_m0, axi_s0 pass through 1:1 (names match; `.*`)
//   - t1p2/t1p3 are parked: rx* inputs ignored, tx* outputs and rx* readies
//     tied inert ('0) — the Cornetto config binds no harness nodes there
//   - axi_m1 is parked the same way (Cornetto has a single memory port)
//   - mon_chi_rn0..1 are declared at the boundary so `.*` can connect them;
//     the bridge leaves them open (COHESTRA_VCS_CHI_MON unset)
//
// Macros come from cornetto_dut_if.svh (resolved through the filelist's
// +incdir of the package integration/ directory); the monitor-tap port
// declaration is a private copy below (cornetto_top.sv `undef`s its own).
// This file must follow cornetto_top.sv on the VCS command line
// (TOP_SOURCES) so the DUT module is visible.
//*****************************************************************************

`default_nettype none
`include "cornetto_dut_if.svh"

// Private copy of the CHI monitor tap port declaration (cornetto_top.sv
// `undef`s CORNETTO_TOP_CHI_MON_PORT_DECL after use, so it is not visible
// here). Pin names/widths must match cornetto_top's exactly for `.*`.
`define CORNETTO_VCS_MON_PORT_DECL(P) \
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

// Park one unused Type-1 CCHI port: all DUT-facing outputs tied inert.
`define CORNETTO_VCS_PARK_T1(P) \
    assign cchi_t1p``P``_rxevt_ready            = '0; \
    assign cchi_t1p``P``_rxreq_ready            = '0; \
    assign cchi_t1p``P``_rxrsp_ready            = '0; \
    assign cchi_t1p``P``_rxdat_ready            = '0; \
    assign cchi_t1p``P``_txsnp_valid            = '0; \
    assign cchi_t1p``P``_txsnp_bits_TxnID       = '0; \
    assign cchi_t1p``P``_txsnp_bits_SrcID       = '0; \
    assign cchi_t1p``P``_txsnp_bits_TgtID       = '0; \
    assign cchi_t1p``P``_txsnp_bits_Opcode      = '0; \
    assign cchi_t1p``P``_txsnp_bits_Addr        = '0; \
    assign cchi_t1p``P``_txsnp_bits_NS          = '0; \
    assign cchi_t1p``P``_txsnp_bits_TraceTag    = '0; \
    assign cchi_t1p``P``_txrsp_valid            = '0; \
    assign cchi_t1p``P``_txrsp_bits_TxnID       = '0; \
    assign cchi_t1p``P``_txrsp_bits_SrcID       = '0; \
    assign cchi_t1p``P``_txrsp_bits_TgtID       = '0; \
    assign cchi_t1p``P``_txrsp_bits_DBID        = '0; \
    assign cchi_t1p``P``_txrsp_bits_Opcode      = '0; \
    assign cchi_t1p``P``_txrsp_bits_RespErr     = '0; \
    assign cchi_t1p``P``_txrsp_bits_Resp        = '0; \
    assign cchi_t1p``P``_txrsp_bits_CBusy       = '0; \
    assign cchi_t1p``P``_txrsp_bits_WayValid    = '0; \
    assign cchi_t1p``P``_txrsp_bits_Way         = '0; \
    assign cchi_t1p``P``_txrsp_bits_TraceTag    = '0; \
    assign cchi_t1p``P``_txdat_valid            = '0; \
    assign cchi_t1p``P``_txdat_bits_TxnID       = '0; \
    assign cchi_t1p``P``_txdat_bits_SrcID       = '0; \
    assign cchi_t1p``P``_txdat_bits_TgtID       = '0; \
    assign cchi_t1p``P``_txdat_bits_DBID        = '0; \
    assign cchi_t1p``P``_txdat_bits_Opcode      = '0; \
    assign cchi_t1p``P``_txdat_bits_RespErr     = '0; \
    assign cchi_t1p``P``_txdat_bits_Resp        = '0; \
    assign cchi_t1p``P``_txdat_bits_DataSource  = '0; \
    assign cchi_t1p``P``_txdat_bits_CBusy       = '0; \
    assign cchi_t1p``P``_txdat_bits_DataID      = '0; \
    for (genvar i = 0; i < 8; i++) begin : park_t1p``P``_txdat_data \
      assign cchi_t1p``P``_txdat_bits_Data[i] = '0; \
    end \
    assign cchi_t1p``P``_txdat_bits_WayValid    = '0; \
    assign cchi_t1p``P``_txdat_bits_Way         = '0; \
    assign cchi_t1p``P``_txdat_bits_TraceTag    = '0;

module cornetto_vcs_top (
    input  logic        clock,
    input  logic        reset,

    // 4x Type-1 CCHI ports (bridge contract; only 0..1 reach the DUT)
    `CORNETTO_CCHI_T1_PORT_DECL(0),
    `CORNETTO_CCHI_T1_PORT_DECL(1),
    `CORNETTO_CCHI_T1_PORT_DECL(2),
    `CORNETTO_CCHI_T1_PORT_DECL(3),

    // 2x AXI4 memory ports (bridge contract; only 0 reaches the DUT)
    `CORNETTO_AXI_M_PORT_DECL(0),
    `CORNETTO_AXI_M_PORT_DECL(1),

    // 1x AXI4 config port (bridge contract; reaches the DUT)
    `CORNETTO_AXI_S_PORT_DECL(0),

    // 2x passive CHI-E RN monitor taps (declared for `.*`; bridge leaves open)
    `CORNETTO_VCS_MON_PORT_DECL(0),
    `CORNETTO_VCS_MON_PORT_DECL(1)
);

    cornetto_top u_dut (.*);

    `CORNETTO_VCS_PARK_T1(2)
    `CORNETTO_VCS_PARK_T1(3)

    // Park the second AXI4 memory port (Cornetto has a single memory port).
    assign axi_m1_awvalid = '0;
    assign axi_m1_awid    = '0;
    assign axi_m1_awaddr  = '0;
    assign axi_m1_awlen   = '0;
    assign axi_m1_awsize  = '0;
    assign axi_m1_awburst = '0;
    assign axi_m1_wvalid  = '0;
    assign axi_m1_wdata   = '0;
    assign axi_m1_wstrb   = '0;
    assign axi_m1_wlast   = '0;
    assign axi_m1_bready  = '0;
    assign axi_m1_arvalid = '0;
    assign axi_m1_arid    = '0;
    assign axi_m1_araddr  = '0;
    assign axi_m1_arlen   = '0;
    assign axi_m1_arsize  = '0;
    assign axi_m1_arburst = '0;
    assign axi_m1_rready  = '0;

endmodule

`default_nettype wire
