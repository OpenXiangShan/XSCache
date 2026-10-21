#!/usr/bin/env bash
#*****************************************************************************
# boot_zhujiang.sh — build & smoke driver for the XSCache TestTop_L2ZhuJiang
# DUT (NUM_L2 x Oceanus L2 -> ZhuJiang CHI interconnect -> SN AXI4) under the
# CHIron Cohestra harness, covering BOTH simulators:
#
#   Verilator flow (cohestra_v3, CUSTOM DUT preset; contract pins, no
#   CHIron-side wrapper — the harness binds TestTop_L2ZhuJiang directly).
#
#   VCS flow (cohestra_vcs, VCS + DPI) via the unified wrapper top
#   test/scripts/common/xscache_vcs_top.sv (+define+-parameterized per DUT
#   family and L2 count; no CHIron-side wrapper for zhujiang). The harness is
#   fully generic: the wrapper alone absorbs every DUT/contract pin delta, so
#   cohestra_vcs_bridge.sv / vcs_top.hpp / executable.cpp are used unchanged.
#
# Stages (multiple may be given, run in order):
#   rtl       regenerate the DUT Verilog (make test-top-zhujiang)
#   lint      contract pin check (test/scripts/zhujiang/check_pins.sh)
#   filelist  regenerate build/zhujiang/zhujiang.f (DUT sources, absolute paths)
#   build     configure & build the cohestra_v3 executable (Verilator; long)
#   run       idle smoke (test/configs/config.xscache_zhujiang.ini)
#   fuzz      traffic smoke (test/configs/config.xscache_zhujiang_fuzz.ini)
#   all       rtl + lint + filelist + build + run (default)
#   vcs-filelist  generate $VCS_BUILD_DIR/zhujiang_vcs.f (+incdir for
#                avocado_dut_if.svh + the wrapper + build/zhujiang/*.sv)
#   vcs-build  vcs-filelist + cohestra_vcs/build.sh all (harness archive +
#                simv; long). NUM_L2 selects the wrapper's
#                +define+XSCACHE_VCS_TOP_L2_P* set and MUST match the RTL in
#                build/zhujiang
#   vcs-run    idle smoke under VCS: zero-traffic variant of $VCS_CONFIG
#                (stimulator.0 disabled), short run — bring-up gate
#   vcs-fuzz   traffic smoke under VCS ($VCS_CONFIG, FuzzARI)
#   vcs-all    vcs-build + vcs-run
#
# Env knobs:
#   CHIRON / CHIRON_OVERRIDE (default test/CHIron, the in-repo checkout)
#   BUILD_DIR  Verilator harness build dir (default build/cohestra_v3_zhujiang)
#   VCS_BUILD_DIR  VCS harness build dir (default build/cohestra_vcs_zhujiang)
#   NUM_L2     L2 count of the DUT (1..4, default 4) — selects the wrapper's
#              +define+XSCACHE_VCS_TOP_L2_P* presence set (see
#              test/scripts/common/xscache_vcs_top_defines.sh); must match the
#              RTL in build/zhujiang
#   NUM_SLICE  (default 2)
#   CCHI_NIDS  CCHI node-ID list for the rtl stage (default: RTL generator's)
#   CHI_MON    CHI monitor taps knob for the rtl stage (default: generator's)
#   VL_THREADS Verilator --threads for the build (default 0)
#   BUILD_JOBS cmake --build parallelism (default nproc)
#   NO_PERF    1 (default) = strip LogPerfEndpoint from the RTL; 0 keeps it
#   CYCLES     run budget: run (default 200000), fuzz (default 300000),
#              vcs-run (default 20000), vcs-fuzz (default 200000); harness time
#              runs 2 units per clock on the VCS side
#   VCS_CONFIG VCS config (default test/configs/config.xscache_vcs_zhujiang.ini)
#   SOAK_NOASSERT=1|0  define STOP_COND=0 + ASSERT_VERBOSE_COND=0 on the
#              Verilator command line, making firtool assertion bodies no-ops
#              (default 0 — assertions stay ENABLED; escape hatch for
#              soak-style runs only)
#   CLOGB=1|0  append [clogb]/[clogb.topo] sections to a derived copy of the
#              run config, exporting CLog.B of the per-L2 RN-F CHI links via
#              the monitor taps (default 0). Node IDs come from
#              build/zhujiang/zj_topology.txt (written by the RTL generator).
#   CLOGB_FILE=<path>  CLog.B output path for CLOGB=1
#              (default zhujiang.clogb in the sim's cwd)
#   VCS_HOME / VERDI_HOME (vcs-* stages only — honored when their bin/ exists,
#              else auto-detected from PATH; never blindly inherited)
#
# Run (after vcs-build):
#   (cd "$VCS_BUILD_DIR" && ./simv \
#       +cohestra_config=$XSCACHE_ROOT/test/configs/config.xscache_vcs_zhujiang.ini \
#       +cohestra_cycles=200000)
#   # plus optionally: +cohestra_fsdb=dump.fsdb / +cohestra_vcd=dump.vcd
#*****************************************************************************
set -euo pipefail

DUT_SCRIPT="$(basename "${BASH_SOURCE[0]}")"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../common/common.sh"
install_on_err
setup_java

RTL_DIR="$XSCACHE_ROOT/build/zhujiang"
BUILD_DIR=${BUILD_DIR:-$XSCACHE_ROOT/build/cohestra_v3_zhujiang}
VCS_BUILD_DIR=${VCS_BUILD_DIR:-$XSCACHE_ROOT/build/cohestra_vcs_zhujiang}
VL_THREADS=${VL_THREADS:-0}
BUILD_JOBS=${BUILD_JOBS:-$(nproc)}
NO_PERF=${NO_PERF:-1}
NUM_L2=${NUM_L2:-4}
NUM_SLICE=${NUM_SLICE:-2}
CCHI_NIDS=${CCHI_NIDS:-}
CHI_MON=${CHI_MON:-}
SOAK_NOASSERT=${SOAK_NOASSERT:-0}
CLOGB=${CLOGB:-0}
CLOGB_FILE=${CLOGB_FILE:-zhujiang.clogb}

TOP_SV="$RTL_DIR/TestTop_L2ZhuJiang.sv"
FILELIST="$RTL_DIR/zhujiang.f"

WRAPPER="$XSCACHE_ROOT/test/scripts/common/xscache_vcs_top.sv"
VCS_TOP_MODULE="xscache_vcs_top"
VCS_FILELIST="$VCS_BUILD_DIR/zhujiang_vcs.f"
VCS_CONFIG="${VCS_CONFIG:-$XSCACHE_ROOT/test/configs/config.xscache_vcs_zhujiang.ini}"

# clogb hook: node IDs are read from zj_topology.txt — rnf.nodeid.<idx> follow
# the CC (L2) order; DMT CompData can carry SN SrcIDs, so the topo map covers
# the whole ring.
clogb_sections() {
    local topo="$RTL_DIR/zj_topology.txt"
    [ -f "$topo" ] || { echo "clogb_sections: $topo missing (run the rtl stage first)" >&2; exit 1; }
    local idx=0 id
    echo "[clogb]"
    echo "enable       = 1"
    echo "path         = $CLOGB_FILE"
    for id in $(sed -n 's/^cc //p' "$topo");  do echo "rnf.nodeid.$idx = $id"; idx=$((idx+1)); done
    echo
    echo "[clogb.topo]"
    for id in $(sed -n 's/^cc //p' "$topo");  do echo "$id = rnf"; done
    for id in $(sed -n 's/^hnf //p' "$topo"); do echo "$id = hnf"; done
    for id in $(sed -n 's/^snf //p' "$topo"); do echo "$id = snf"; done
    for id in $(sed -n 's/^hni //p' "$topo"); do echo "$id = hni"; done
    for id in $(sed -n 's/^mn //p' "$topo");  do echo "$id = mn"; done
}

# ------------------------------------------------------------ Verilator flow
stage_rtl() {
    setup_java
    make -C "$XSCACHE_ROOT" test-top-zhujiang NO_PERF=$NO_PERF NUM_L2=$NUM_L2 \
        NUM_SLICE=$NUM_SLICE CCHI_NIDS="$CCHI_NIDS" CHI_MON="$CHI_MON"
}

stage_lint() {
    "$XSCACHE_ROOT/test/scripts/zhujiang/check_pins.sh" "$TOP_SV"
}

stage_filelist() {
    [ -f "$RTL_DIR/filelist.f" ] || { echo "filelist.f missing in $RTL_DIR (run the rtl stage first)"; exit 1; }
    {
        # everything except the top module file — that one is passed as
        # COHESTRA_V3_RTL_SOURCES (same split as the avocado package flow)
        sed "s|^|$RTL_DIR/|" "$RTL_DIR/filelist.f" | grep -v "/TestTop_L2ZhuJiang.sv\$"
        ls "$RTL_DIR"/*.v 2>/dev/null || true
    } > "$FILELIST"
    echo "filelist: wrote $(wc -l < "$FILELIST") entries to $FILELIST"
}

stage_build() {
    # -Wno-UNOPTTHREADS: on the small n=1 design Verilator's scheduler cannot
    # partition 7/8 threads and raises UNOPTTHREADS, which is fatal here. The
    # partial parallelism is exactly what the perf matrix wants to measure.
    local vargs="-f;$FILELIST;$XSCACHE_ROOT/test/scripts/zhujiang/zhujiang_waivers.vlt;--output-split;20000;-Wno-UNOPTTHREADS"
    if [ "$SOAK_NOASSERT" = 1 ]; then
        # the +define+s no-op the firtool assertion bodies. Escape hatch only;
        # the smoke flow always runs with assertions ENABLED.
        vargs="$vargs;+define+STOP_COND=0;+define+ASSERT_VERBOSE_COND=0"
    fi
    cmake -S "$CHIRON/cchi/cohestra/cohestra_v3" -B "$BUILD_DIR" \
        -DCOHESTRA_V3_RTL=CUSTOM \
        -DCOHESTRA_V3_TOP_MODULE=TestTop_L2ZhuJiang \
        -DCOHESTRA_V3_CHI_NODEID_WIDTH=11 \
        -DCOHESTRA_V3_CHI_DATACHECK_PRESENT=1 \
        -DCOHESTRA_V3_CHI_POISON_PRESENT=1 \
        -DCOHESTRA_V3_CHI_MPAM_PRESENT=1 \
        -DCOHESTRA_V3_RTL_SOURCES="$TOP_SV" \
        -DCOHESTRA_V3_THREADS="$VL_THREADS" \
        -DCOHESTRA_V3_VERILATOR_ARGS="$vargs"
    cmake --build "$BUILD_DIR" -j"$BUILD_JOBS"
}

stage_run() {
    run_cohestra_v3 "$BUILD_DIR" \
        "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_zhujiang.ini" "$BUILD_DIR")" \
        "${CYCLES:-200000}"
}

stage_fuzz() {
    run_cohestra_v3 "$BUILD_DIR" \
        "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_zhujiang_fuzz.ini" "$BUILD_DIR")" \
        "${CYCLES:-300000}"
}

stage_all() { stage_rtl; stage_lint; stage_filelist; stage_build; stage_run; }

# ----------------------------------------------------------------- VCS flow
vcs_need_dut() {
    case "$NUM_L2" in 1|2|3|4) ;; *) echo "NUM_L2 must be 1..4 (got $NUM_L2)" >&2; exit 2 ;; esac
}

stage_vcs-rtl() {
    # No-op by design: the DUT RTL is regenerated by the rtl stage (or an
    # external make). Only verify the file set exists and is settled.
    wait_rtl_settled 'zhujiang' "build/zhujiang"
    [ -f "$TOP_SV" ] \
        || { echo "error: $TOP_SV missing — run the rtl stage first" >&2; exit 1; }
    echo "$DUT_SCRIPT: DUT RTL present in $RTL_DIR ($(ls "$RTL_DIR"/*.sv | wc -l) .sv files)"
}

stage_vcs-filelist() {
    vcs_need_dut
    mkdir -p "$VCS_BUILD_DIR"
    # +incdir must precede the wrapper so its `include "avocado_dut_if.svh"
    # resolves regardless of how positionally VCS applies incdirs; TOP_SOURCES
    # (see vcs-build) carries the same +incdir for the pre-filelist position.
    #
    # Source list comes from the rtl step's authoritative filelist.f — NOT a
    # glob of $RTL_DIR: bare `make test-top-zhujiang` runs do not clean the dir,
    # so stale .sv from older generations linger and mix.
    {
        echo "+incdir+$AVOCADO_INTEGRATION"
        echo "$WRAPPER"
        sed "s|^|$RTL_DIR/|" "$RTL_DIR/filelist.f"
        ls "$RTL_DIR"/*.v 2>/dev/null || true
    } > "$VCS_FILELIST"
    echo "$DUT_SCRIPT: filelist written: $VCS_FILELIST ($(wc -l < "$VCS_FILELIST") entries)"
}

stage_vcs-build() {
    vcs_need_dut
    resolve_vcs_tools
    stage_vcs-rtl
    # always regenerate: a cached .f silently pins stale RTL paths — regen is
    # trivial
    stage_vcs-filelist
    CHIRON="$CHIRON" \
        BUILD_DIR="$VCS_BUILD_DIR" \
        COHESTRA_VCS_TOP_MODULE="$VCS_TOP_MODULE" \
        COHESTRA_VCS_CHI_MON=1 COHESTRA_VCS_CHI_MON_PORTS="$NUM_L2" \
        EXTRA_VCS_FLAGS="$("$XSCACHE_ROOT/test/scripts/common/xscache_vcs_top_defines.sh" zhujiang "$NUM_L2")" \
        TOP_SOURCES="+incdir+$AVOCADO_INTEGRATION" \
        FILELIST="$VCS_FILELIST" \
        bash "$VCS_DIR/build.sh" all
}

stage_vcs-run() {
    vcs_need_dut
    # Idle bring-up: the smoke config with the FuzzARI stimulator disabled
    # (generated artifact in the build dir, not a repo file). The transaction
    # watchdog stays quiet with zero outstanding transactions, so no other
    # key needs changing. Reset still runs (preamble 0 + 2x8192 cycles).
    local idle_ini="$VCS_BUILD_DIR/config.xscache_vcs_zhujiang_idle.ini"
    derive_idle_config "$VCS_CONFIG" "$idle_ini"
    run_simv "$VCS_BUILD_DIR" "$idle_ini" "${CYCLES:-20000}"
}

stage_vcs-fuzz() {
    vcs_need_dut
    run_simv "$VCS_BUILD_DIR" "$VCS_CONFIG" "${CYCLES:-200000}"
}

stage_vcs-all() { stage_vcs-build; stage_vcs-run; }

# ------------------------------------------------------------- stage driver
[ -d "$CHIRON" ] || { echo "CHIron checkout not found: $CHIRON" >&2; exit 1; }
[ -f "$WRAPPER" ] || { echo "wrapper not found: $WRAPPER" >&2; exit 1; }

usage() {
    echo "usage: $0 [stages...] (default: all)"
    echo "  stages: rtl lint filelist build run fuzz all | vcs-filelist vcs-build vcs-run vcs-fuzz vcs-all"
}

st_to_fn() {
    case "$1" in
        all)     echo stage_all ;;
        vcs-all) echo stage_vcs-all ;;
        *)       echo "stage_$1" ;;
    esac
}

stages=("$@")
[ $# -eq 0 ] && stages=(all)
for st in "${stages[@]}"; do
    fn="$(st_to_fn "$st")"
    declare -F "$fn" >/dev/null 2>&1 || { echo "unknown stage: $st" >&2; usage >&2; exit 2; }
done
for st in "${stages[@]}"; do
    "$(st_to_fn "$st")"
done
