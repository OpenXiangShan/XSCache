module por_ck_gate (
  se,
  clk,
  clk_enable,
  gated_clk
);
  input         se;
  input         clk;
  input         clk_enable;
  output        gated_clk;

  assign gated_clk = clk;

  // inputs intentionally unused in the forced-open override
  wire unused_ok = &{1'b0, se, clk_enable};

endmodule
