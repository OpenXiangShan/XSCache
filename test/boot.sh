#!/usr/bin/env bash
#*****************************************************************************
# boot.sh — all-in-one caller for every XSCache Cohestra DUT flow. Thin
# dispatcher: it validates the DUT name and forwards the stage list (and every
# environment knob, by inheritance) to the per-DUT driver in test/scripts/.
# See test/README.md for the full documentation.
#
# Usage:
#   test/boot.sh [<dut>] [stages...] [--skip-rtl] [-h|--help]
#
#   dut:      openllc (default; alias l2openllc) | zhujiang |
#             avocado | biscotto | cornetto |
#             multichi (legacy alias -> the openllc driver with DUT=l2multichi)
#   stages:   driver-specific; default: all. Multiple stages run in order.
#             Common vocabulary (per-DUT details in test/README.md):
#               rtl [lint] [filelist] build run [fuzz]            (Verilator)
#               vcs-build vcs-run vcs-fuzz [vcs-smoke]            (VCS)
#               all (= Verilator chain)   vcs-all (= vcs-build+vcs-run)
#   --skip-rtl: drop the rtl stage (reuse the existing DUT Verilog)
#
# Every environment knob (CYCLES, NUM_L2, NUM_SLICE, CONFIG, VCS_CONFIG,
# WAVEFORM, CLOGB, CLOGB_FILE, SOAK_NOASSERT, NO_PERF, THREADS/VL_THREADS,
# BUILD_DIR, VCS_BUILD_DIR, *_MESH_ROOT, CHIRON/CHIRON_OVERRIDE, EXTRA_*,
# VCS_HOME, VERDI_HOME, ...) passes through unchanged by environment
# inheritance — set it before this call exactly as documented for the
# per-DUT driver.
#
# Examples:
#   test/boot.sh                              # legacy default: openllc all
#   test/boot.sh zhujiang build run
#   test/boot.sh zhujiang vcs-build vcs-run
#   NUM_L2=4 test/boot.sh openllc vcs-build vcs-run
#   AVOCADO_MESH_ROOT=/path/to/Mesh32_Avocado test/boot.sh avocado all
#*****************************************************************************
set -euo pipefail

XSCACHE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPTS="$XSCACHE_ROOT/test/scripts"

usage() {
    sed -n '2,34p' "${BASH_SOURCE[0]}"
}

dut_arg=""
skip_rtl=0
stages=()
for arg in "$@"; do
    case "$arg" in
        -h|--help) usage; exit 0 ;;
        --skip-rtl) skip_rtl=1 ;;
        -*) echo "boot.sh: unknown flag: $arg" >&2; usage >&2; exit 2 ;;
        *) if [ -z "$dut_arg" ]; then dut_arg="$arg"; else stages+=("$arg"); fi ;;
    esac
done

# DUT resolution: positional arg, else the legacy DUT env, else the default.
dut_arg="${dut_arg:-${DUT:-openllc}}"
case "$dut_arg" in
    openllc|l2openllc)   dut=openllc ;;
    multichi|l2multichi) dut=openllc; export DUT=l2multichi ;;
    zhujiang|avocado|biscotto|cornetto) dut="$dut_arg" ;;
    *) echo "boot.sh: unknown DUT: $dut_arg (expected: openllc|zhujiang|avocado|biscotto|cornetto|multichi)" >&2; usage >&2; exit 2 ;;
esac

driver="$SCRIPTS/$dut/boot_$dut.sh"
[ -f "$driver" ] || { echo "boot.sh: driver not found: $driver" >&2; exit 1; }

if [ ${#stages[@]} -eq 0 ]; then
    if [ "$skip_rtl" = 1 ]; then
        case "$dut" in
            openllc) stages=(stage build run) ;;
            *)       stages=(lint filelist build run) ;;
        esac
    else
        stages=(all)
    fi
elif [ "$skip_rtl" = 1 ]; then
    filtered=()
    for st in "${stages[@]}"; do
        [ "$st" = rtl ] || filtered+=("$st")
    done
    stages=("${filtered[@]}")
fi

exec bash "$driver" "${stages[@]}"
