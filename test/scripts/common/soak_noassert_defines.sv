// soak_noassert_defines.sv — soak/stress-build defines for the firtool-emitted
// assertion guards. Verilator preprocesses the source list in order with a
// global macro table, so these defines must come FIRST in
// COHESTRA_V3_RTL_SOURCES (test/boot.sh prepends this file when
// SOAK_NOASSERT=1).
//
// Every firtool-emitted assertion body is gated like:
//   `ifndef STOP_COND_ / `ifdef STOP_COND / `define STOP_COND_ (`STOP_COND) ...
// Pre-seeding STOP_COND=0 makes STOP_COND_ expand to (0), skipping $fatal;
// ASSERT_VERBOSE_COND=0 likewise skips the $error print (which in Verilator
// also aborts the run). Assertions become complete no-ops so a soak run keeps
// stressing past assertion failures; functional mismatches are still caught by
// the cache-line data monitor and stalls by the watchdog.

`ifndef SOAK_NOASSERT_DEFINES_SV
`define SOAK_NOASSERT_DEFINES_SV

`define STOP_COND 0
`define ASSERT_VERBOSE_COND 0

`endif // SOAK_NOASSERT_DEFINES_SV
