#!/usr/bin/env bash
#*****************************************************************************
# gen_expected_pins.sh — regenerate test/scripts/zhujiang/expected_pins.txt, the
# cohestra_v3 contract pin list for TestTop_L2ZhuJiang:
#   - clock/reset/log_* + cchi_t1p{0..3}_* + mon_chi_rn{0..3}_* (packed)
#    taken from the proven build/l2avocado/TestTop_L2Avocado.sv pin set;
#   - axi_m0_* expanded from AVOCADO_AXI_M_PORT_DECL(0) of CHIron's
#     cohestra_packages/avocado/integration/avocado_dut_if.svh.
#
# Re-run only when the contract itself changes (not part of the normal flow).
#*****************************************************************************
set -euo pipefail

XSCACHE_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)
CHIRON=${CHIRON:-$XSCACHE_ROOT/test/CHIron}
AVO_SV="$XSCACHE_ROOT/build/l2avocado/TestTop_L2Avocado.sv"
DUT_IF="$CHIRON/cchi/cohestra/cohestra_packages/avocado/integration/avocado_dut_if.svh"
OUT="$(dirname "${BASH_SOURCE[0]}")/expected_pins.txt"

[ -f "$AVO_SV" ]  || { echo "gen_expected_pins: $AVO_SV not found (build the avocado top first)"; exit 1; }
[ -f "$DUT_IF" ]  || { echo "gen_expected_pins: $DUT_IF not found"; exit 1; }

extract() {
    # normalize firtool SV port lines: "<name> <width>" (width = bit count).
    # firtool prints the direction keyword only when it changes, so scan the
    # whole module header region and take every name before a comma.
    awk '
        /^module[[:space:]]/ { inh = 1; next }
        inh && /^\);/        { inh = 0 }
        inh {
            line = $0
            sub(/\/\/.*$/, "", line)
            if (line !~ /[A-Za-z0-9_][[:space:]]*,?[[:space:]]*$/) next
            w = 1
            if (match(line, /\[[0-9]+:[0-9]+\]/)) {
                s = substr(line, RSTART + 1, RLENGTH - 2)
                split(s, m, ":")
                w = m[1] - m[2] + 1
            }
            match(line, /[A-Za-z_][A-Za-z0-9_]*[[:space:]]*,?[[:space:]]*$/)
            name = substr(line, RSTART, RLENGTH)
            gsub(/[[:space:],]/, "", name)
            print name, w
        }' "$1"
}

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# clock/reset/log_* + cchi_t1p{0..3}_* + mon_chi_rn{0..3}_* from the avocado top
extract "$AVO_SV" \
  | grep -E '^(clock|reset|log_|cchi_t1p[0-3]_|mon_chi_rn[0-3]_)' > "$work/base.txt"

# axi_m0_* from AVOCADO_AXI_M_PORT_DECL: each line is "dir logic [w] axi_m``P``_x"
awk '/`define AVOCADO_AXI_M_PORT_DECL/{on=1} on{print} on && /axi_m``P``_rready/{exit}' "$DUT_IF" \
  | grep -oE '(input|output)[[:space:]]+logic[[:space:]]*(\[[0-9]+:[0-9]+\][[:space:]]*)?axi_m``P``_[a-z]+' \
  | sed 's/``P``/0/g' \
  | awk '{
      name = $NF; w = 1;
      if ($0 ~ /\[[0-9]+:[0-9]+\]/) {
        match($0, /\[([0-9]+):([0-9]+)\]/, m);
        w = m[1] - m[2] + 1;
      }
      print name, w;
    }' > "$work/axi.txt"

[ -s "$work/base.txt" ] || { echo "gen_expected_pins: no base pins extracted"; exit 1; }
[ -s "$work/axi.txt" ]  || { echo "gen_expected_pins: no AXI pins extracted"; exit 1; }

sort -u "$work/base.txt" "$work/axi.txt" > "$OUT"
echo "gen_expected_pins: wrote $(wc -l < "$OUT") pins to $OUT"
