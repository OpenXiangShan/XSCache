#!/usr/bin/env bash
#*****************************************************************************
# boot_biscotto.sh — build & smoke driver for the XSCache Biscotto DUT
# (TestTop_L2Biscotto: 3x Oceanus L2 inside the CHIron Biscotto integration
# package, backed by the external 2x2 mesh package).
#
# Stages:
#   rtl       regenerate the DUT Verilog (make test-top-l2biscotto) and
#             re-stage it (rsync --checksum, see below)
#   lint      contract-lint the DUT against biscotto_dut_if.svh (check_dut.sh)
#   filelist  regenerate this flow's biscotto_top_l2biscotto.f (mesh + DUT
#             sources; a name of its own so a concurrent template-DUT flow
#             sharing the CHIron package cannot clobber it — the package
#             default biscotto_top.f belongs to the template-DUT flow)
#   build     configure & build the cohestra_v3 executable (Verilator; long)
#   run       boot-only smoke (test/configs/config.xscache_biscotto.ini)
#   fuzz      traffic smoke   (test/configs/config.xscache_biscotto_fuzz.ini)
#   all       rtl + lint + filelist + build + run (default)
#
# VCS stages (delegate to CHIron's cohestra_vcs harness — DPI/VCS build of the
# same biscotto_top + DUT; requires a VCS installation, see smoke).
# *** UNTESTED on this box (no VCS license here) — carried over from the
# *** avocado/cornetto drivers for parity; the avocado build.sh they delegate
# *** to is itself avocado-flavoured (see cohestra_vcs/build.sh env docs):
#   vcs-smoke  go/no-go preflight for this host (license/compile/run marker)
#   vcs-build  preflight + build harness archive + simv
#   vcs-run    boot-only smoke under VCS (config.xscache_biscotto.ini)
#   vcs-fuzz   traffic smoke under VCS (config.xscache_biscotto_fuzz.ini)
#
# RTL staging (rsync --checksum --no-times, same trick as the openllc flow):
# firtool rewrites every .sv on each run (new inode/ctime, identical content)
# and Verilator's --skip-identical keys off inode/ctime for files too large
# to hash, so feeding it build/l2biscotto directly would re-verilate the
# world after every rtl stage. The rtl stage therefore syncs the firtool
# output into the content-stable build/l2biscotto_staged/ (unchanged files
# left completely untouched — no data write, no mtime/ctime bump), and every
# downstream consumer (lint/filelist/build) reads the STAGED tree.
#
# Env knobs:
#   CHIRON             CHIron checkout
#                      (default test/CHIron, the in-repo checkout)
#   BISCOTTO_MESH_ROOT mesh package with the Verilator patch applied
#                      (no default; required by the filelist/build/vcs-build stages)
#   BUILD_DIR          harness build dir (default build/cohestra_v3_l2biscotto)
#   VL_THREADS=<N>     Verilator worker threads: -DCOHESTRA_V3_THREADS=N
#                      (0 = legacy single-threaded model, default 0)
#   BUILD_JOBS=<N>     parallel compile jobs for cmake --build (default nproc)
#   CYCLES             sim time budget for run (default 200000) / fuzz
#                      (default 300000); harness time runs 2 units per clock
#   NO_PERF=1|0        strip DUT perf counters in the rtl stage (default 1)
#   NUM_SLICE=<1-4>    L2 slices per instance in the rtl stage (default 2)
#   SOAK_NOASSERT=1|0  define STOP_COND=0 + ASSERT_VERBOSE_COND=0 on the
#                      Verilator command line, making firtool assertion bodies
#                      no-ops (default 0 —
#                      assertions stay ENABLED; the smoke configs instead run
#                      reset_preamble=0 + reset_repeat=2 so the DUT is reset
#                      from simulation start — keeping RegInit-dependent
#                      assertions such as the L-credit conservation check in
#                      xscache/chi/LinkLayer.scala valid — while the second
#                      pulse still gives the mesh PUB fabric its required
#                      reset-assertion edge after unreset runtime. Keep this
#                      knob as an escape hatch for soak-style runs; it is also
#                      the documented workaround if the fuzz smoke trips a DUT
#                      assertion on the known Verilator-2-state mesh
#                      mis-settle — see config.xscache_biscotto_fuzz.ini)
#   CG_OPEN=1|0        prepend test/scripts/biscotto/verilator_override to the
#                      Verilator -y search path, shadowing por_ck_gate with a
#                      forced-open ICG (clock-gating is power-only; carried
#                      over from the avocado flow as a diagnostic for the
#                      mesh-internal duplicated response flits seen in the
#                      fuzz smoke there — hypothesis C1). Default 0 until
#                      proven.
#   CLOGB=1|0          append a [clogb]/[clogb.topo] section to a derived copy
#                      of the run config, exporting CLog.B of the 3 RN-F CHI
#                      links via the CHI monitor taps (default 0)
#   CLOGB_FILE=<path>  CLog.B output path for CLOGB=1
#                      (default biscotto.clogb in the sim's cwd)
#
# cohestra_v3 exit-code legend (printed after every run/fuzz):
#   0 = FINISHED (pass), 1 = usage/config error,
#   2 = verification failure (instance FAILED or cache-line data mismatch)
#*****************************************************************************
set -euo pipefail

DUT_SCRIPT="$(basename "${BASH_SOURCE[0]}")"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../common/common.sh"
install_on_err
BISCOTTO_MESH_ROOT=${BISCOTTO_MESH_ROOT:-}

# Mesh-package gate: required (and validated) only by the stages that consume
# it (filelist / build / vcs-build — see the require_mesh_root calls there).
require_mesh_root() {
    if [ -z "$BISCOTTO_MESH_ROOT" ]; then
        echo "boot_biscotto: BISCOTTO_MESH_ROOT is not set — point it at the mesh package (Verilator patch applied)" >&2
        exit 2
    fi
    [ -d "$BISCOTTO_MESH_ROOT" ] || { echo "boot_biscotto: mesh package not found: $BISCOTTO_MESH_ROOT" >&2; exit 1; }
    [ -f "$BISCOTTO_MESH_ROOT/verilator/verilator_waivers.vlt" ] || { echo "boot_biscotto: $BISCOTTO_MESH_ROOT is missing verilator/verilator_waivers.vlt (not a patched mesh package?)" >&2; exit 1; }
}
PKG="$CHIRON/cchi/cohestra/cohestra_packages/biscotto"
# Flow-specific filelist (see the filelist stage doc above); overridable for
# exotic setups but must NOT collide with the package-default biscotto_top.f.
TOP_F=${TOP_F:-$PKG/build/biscotto_top_l2biscotto.f}
RTL_DIR="$XSCACHE_ROOT/build/l2biscotto"
RTL_STAGED="$XSCACHE_ROOT/build/l2biscotto_staged"
BUILD_DIR=${BUILD_DIR:-$XSCACHE_ROOT/build/cohestra_v3_l2biscotto}
VL_THREADS=${VL_THREADS:-0}
BUILD_JOBS=${BUILD_JOBS:-$(nproc)}
NO_PERF=${NO_PERF:-1}
NUM_SLICE=${NUM_SLICE:-2}
SOAK_NOASSERT=${SOAK_NOASSERT:-0}
CG_OPEN=${CG_OPEN:-0}
CLOGB=${CLOGB:-0}
CLOGB_FILE=${CLOGB_FILE:-biscotto.clogb}

# clogb hook (see clogb_config in common.sh): the [clogb]/[clogb.topo] body
# for this DUT's mesh. Time unit in the log = harness ticks.
clogb_sections() {
    cat <<EOF

[clogb]
enable       = 1
path         = $CLOGB_FILE
rnf.nodeid.0 = 4
rnf.nodeid.1 = 12
rnf.nodeid.2 = 44

[clogb.topo]
0  = hni
4  = rnf
8  = hnf
12 = rnf
32 = rni
36 = snf
40 = hnf
44 = rnf
EOF
}

# Content-based sync of the generated RTL into the stable staging directory
# (see the header). Unchanged files stay completely untouched, so Verilator's
# --skip-identical actually engages across rtl re-runs.
sync_staged_rtl() {
    [ -d "$RTL_DIR" ] || { echo "DUT RTL not found: $RTL_DIR (run the rtl stage first)" >&2; exit 1; }
    rsync -r --checksum --delete --no-times "$RTL_DIR/" "$RTL_STAGED/"
}

dut_sources() {
    [ -f "$RTL_STAGED/filelist.f" ] || sync_staged_rtl
    echo "$XSCACHE_ROOT/test/scripts/biscotto/biscotto_dut.sv"
    sed "s|^|$RTL_STAGED/|" "$RTL_STAGED/filelist.f"
    ls "$RTL_STAGED"/*.v 2>/dev/null || true
}

stage_rtl() {
    # firtool does not clean its output directory: files from a previous
    # configuration (e.g. LogPerfEndpoint.sv left over when toggling NO_PERF)
    # would linger and be picked up into the verilation sources.
    rm -rf "$RTL_DIR"
    make -C "$XSCACHE_ROOT" test-top-l2biscotto NO_PERF=$NO_PERF NUM_SLICE=$NUM_SLICE
    sync_staged_rtl
}

stage_lint() {
    (cd "$PKG/scripts" && bash ./check_dut.sh $(dut_sources))
}

stage_filelist() {
    require_mesh_root
    (cd "$PKG/scripts" && BISCOTTO_MESH_ROOT="$BISCOTTO_MESH_ROOT" \
        BISCOTTO_TOP_F="$TOP_F" \
        bash ./gen_filelist.sh $(dut_sources))
}

stage_build() {
    require_mesh_root
    local sources="$PKG/biscotto_top.sv"
    # -Wno-BADVLTPRAGMA: the mesh package's own verilator_waivers.vlt opens
    # with a `// verilator_waivers.vlt` comment that Verilator 5.050 rejects
    # (extra underscore in meta-comment); waiving it here keeps the external
    # mesh package untouched.
    local vargs="-f;$TOP_F;$BISCOTTO_MESH_ROOT/verilator/verilator_waivers.vlt;--output-split;20000;+define+BISCOTTO_DUT_MODULE=biscotto_dut;-Wno-BADVLTPRAGMA"
    if [ "$SOAK_NOASSERT" = 1 ]; then
        # the +define+s no-op the firtool assertion bodies. Do NOT also pass
        # test/scripts/common/soak_noassert_defines.sv as a source: Verilator
        # processes the -f filelist first, so the file lands too late to help
        # the DUT sources, and its redefinition of these command-line defines
        # trips a fatal DEFOVERRIDE warning.
        vargs="$vargs;+define+STOP_COND=0;+define+ASSERT_VERBOSE_COND=0"
    fi
    if [ "$CG_OPEN" = 1 ]; then
        # forced-open ICG override shadows the render's por_ck_gate.v via -y
        # first-match ordering (the filelist resolves modules lazily)
        vargs="-y;$XSCACHE_ROOT/test/scripts/biscotto/verilator_override;+libext+.v;$vargs"
    fi
    cmake -S "$CHIRON/cchi/cohestra/cohestra_v3" -B "$BUILD_DIR" \
        -DCOHESTRA_V3_RTL=CUSTOM \
        -DCOHESTRA_V3_TOP_MODULE=biscotto_top \
        -DCOHESTRA_V3_CHI_NODEID_WIDTH=11 \
        -DCOHESTRA_V3_CHI_REQ_ADDR_WIDTH=48 \
        -DCOHESTRA_V3_CHI_REQ_RSVDC_WIDTH=4 \
        -DCOHESTRA_V3_CHI_DAT_RSVDC_WIDTH=4 \
        -DCOHESTRA_V3_CHI_DATA_WIDTH=256 \
        -DCOHESTRA_V3_CHI_DATACHECK_PRESENT=1 \
        -DCOHESTRA_V3_CHI_POISON_PRESENT=1 \
        -DCOHESTRA_V3_CHI_MPAM_PRESENT=1 \
        -DCOHESTRA_V3_RTL_SOURCES="$sources" \
        -DCOHESTRA_V3_THREADS="$VL_THREADS" \
        -DCOHESTRA_V3_VERILATOR_ARGS="$vargs"
    cmake --build "$BUILD_DIR" -j"$BUILD_JOBS"
}

stage_run() {
    # budget: 2x(8192+8192) reset + 25000 warm-up + ~60-entry script, x2 time
    run_cohestra_v3 "$BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_biscotto.ini" "$BUILD_DIR")" "${CYCLES:-200000}"
}

stage_fuzz() {
    run_cohestra_v3 "$BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_biscotto_fuzz.ini" "$BUILD_DIR")" "${CYCLES:-300000}"
}

# -- VCS stages (cohestra_vcs harness in CHIron) — UNTESTED, no VCS license ---
VCS_DIR="$CHIRON/cchi/cohestra/cohestra_vcs"
VCS_BUILD_DIR=${VCS_BUILD_DIR:-$XSCACHE_ROOT/build/cohestra_vcs_l2biscotto}

stage_vcs-smoke() {
    bash "$VCS_DIR/smoke/run_vcs_smoke.sh"
}

stage_vcs-build() {
    require_mesh_root
    stage_vcs-smoke   # go/no-go preflight
    [ -f "$TOP_F" ] || stage_filelist
    # The cohestra_vcs bridge expects the full Avocado-style boundary (4x CCHI
    # T1 + 2x AXI-M); biscotto_vcs_top.sv adapts biscotto_top (3x T1 + 1x
    # AXI-M) to it, parking the unused ports. It must compile after
    # biscotto_top.sv (uses BISCOTTO_TOP_CHI_MON_PORT_DECL from there).
    CHIRON="$CHIRON" BISCOTTO_MESH_ROOT="$BISCOTTO_MESH_ROOT" \
        BUILD_DIR="$VCS_BUILD_DIR" \
        COHESTRA_VCS_TOP_MODULE=biscotto_vcs_top \
        COHESTRA_VCS_CHI_MON=1 COHESTRA_VCS_CHI_MON_PORTS=3 \
        FILELIST="$TOP_F" \
        TOP_SOURCES="$PKG/biscotto_top.sv $XSCACHE_ROOT/test/scripts/biscotto/biscotto_vcs_top.sv" \
        bash "$VCS_DIR/build.sh" all
}

stage_vcs-run() {
    run_simv "$VCS_BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_biscotto.ini" "$VCS_BUILD_DIR")" "${CYCLES:-200000}"
}

stage_vcs-fuzz() {
    run_simv "$VCS_BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_biscotto_fuzz.ini" "$VCS_BUILD_DIR")" "${CYCLES:-300000}"
}

[ -d "$CHIRON" ] || { echo "CHIron checkout not found: $CHIRON"; exit 1; }

stage_all() { stage_rtl; stage_lint; stage_filelist; stage_build; stage_run; }
stage_vcs-all() { stage_vcs-build; stage_vcs-run; }

usage() {
    echo "usage: $0 [stages...] (default: all)"
    echo "  stages: rtl lint filelist build run fuzz all | vcs-smoke vcs-build vcs-run vcs-fuzz vcs-all"
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
