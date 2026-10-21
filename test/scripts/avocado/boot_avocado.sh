#!/usr/bin/env bash
#*****************************************************************************
# boot_avocado.sh — build & smoke driver for the XSCache Avocado DUT
# (TestTop_L2Avocado: 4x Oceanus L2 inside the CHIron Avocado integration
# package, backed by the external 3x2 mesh package).
#
# Stages:
#   rtl       regenerate the DUT Verilog (make test-top-l2avocado)
#   lint      contract-lint the DUT against avocado_dut_if.svh (check_dut.sh)
#   filelist  regenerate the package's avocado_top.f (mesh + DUT sources)
#   build     configure & build the cohestra_v3 executable (Verilator; long)
#   run       boot-only smoke (test/configs/config.xscache_avocado.ini)
#   fuzz      traffic smoke   (test/configs/config.xscache_avocado_fuzz.ini)
#   all       rtl + lint + filelist + build + run (default)
#
# VCS stages (delegate to CHIron's cohestra_vcs harness — DPI/VCS build of the
# same avocado_top + DUT; requires a VCS installation, see smoke):
#   vcs-smoke  go/no-go preflight for this host (license/compile/run marker)
#   vcs-build  preflight + build harness archive + simv
#   vcs-run    boot-only smoke under VCS (config.xscache_avocado.ini)
#   vcs-fuzz   traffic smoke under VCS (config.xscache_avocado_fuzz.ini)
#
# Env knobs:
#   CHIRON             CHIron checkout
#                      (default test/CHIron, the in-repo checkout)
#   AVOCADO_MESH_ROOT  mesh package with the Verilator patch applied
#                      (no default; required by the filelist/build/vcs-build stages)
#   BUILD_DIR          harness build dir (default build/cohestra_v3_l2avocado)
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
#                      knob as an escape hatch for soak-style runs)
#   CG_OPEN=1|0        prepend test/scripts/avocado/verilator_override to the
#                      Verilator -y search path, shadowing por_ck_gate with a
#                      forced-open ICG (clock-gating is power-only; diagnostic
#                      for the mesh-internal duplicated response flits seen in
#                      the fuzz smoke — hypothesis C1). Default 0 until proven.
#   CLOGB=1|0          append a [clogb]/[clogb.topo] section to a derived copy
#                      of the run config, exporting CLog.B of the 4 RN-F CHI
#                      links via the CHI monitor taps (default 0)
#   CLOGB_FILE=<path>  CLog.B output path for CLOGB=1
#                      (default avocado.clogb in the sim's cwd)
#*****************************************************************************
set -euo pipefail

DUT_SCRIPT="$(basename "${BASH_SOURCE[0]}")"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../common/common.sh"
install_on_err
AVOCADO_MESH_ROOT=${AVOCADO_MESH_ROOT:-}

# Mesh-package gate: required (and validated) only by the stages that consume
# it (filelist / build / vcs-build — see the require_mesh_root calls there).
require_mesh_root() {
    if [ -z "$AVOCADO_MESH_ROOT" ]; then
        echo "boot_avocado: AVOCADO_MESH_ROOT is not set — point it at the mesh package (Verilator patch applied)" >&2
        exit 2
    fi
    [ -d "$AVOCADO_MESH_ROOT" ] || { echo "boot_avocado: mesh package not found: $AVOCADO_MESH_ROOT" >&2; exit 1; }
    [ -f "$AVOCADO_MESH_ROOT/verilator/verilator_waivers.vlt" ] || { echo "boot_avocado: $AVOCADO_MESH_ROOT is missing verilator/verilator_waivers.vlt (not a patched mesh package?)" >&2; exit 1; }
}
PKG="$CHIRON/cchi/cohestra/cohestra_packages/avocado"
RTL_DIR="$XSCACHE_ROOT/build/l2avocado"
BUILD_DIR=${BUILD_DIR:-$XSCACHE_ROOT/build/cohestra_v3_l2avocado}
VL_THREADS=${VL_THREADS:-0}
BUILD_JOBS=${BUILD_JOBS:-$(nproc)}
NO_PERF=${NO_PERF:-1}
NUM_SLICE=${NUM_SLICE:-2}
SOAK_NOASSERT=${SOAK_NOASSERT:-0}
CG_OPEN=${CG_OPEN:-0}
CLOGB=${CLOGB:-0}
CLOGB_FILE=${CLOGB_FILE:-avocado.clogb}

# clogb hook (see clogb_config in common.sh): the [clogb]/[clogb.topo] body
# for this DUT's mesh. Time unit in the log = harness ticks.
clogb_sections() {
    cat <<EOF

[clogb]
enable       = 1
path         = $CLOGB_FILE
rnf.nodeid.0 = 32
rnf.nodeid.1 = 64
rnf.nodeid.2 = 12
rnf.nodeid.3 = 44

[clogb.topo]
0  = hni
4  = snf
8  = hnf
12 = rnf
32 = rnf
36 = hnf
40 = hnf
44 = rnf
64 = rnf
68 = hnf
72 = snf
76 = rni
EOF
}

dut_sources() {
    echo "$XSCACHE_ROOT/test/scripts/avocado/avocado_dut.sv"
    sed "s|^|$RTL_DIR/|" "$RTL_DIR/filelist.f"
    ls "$RTL_DIR"/*.v 2>/dev/null || true
}

stage_rtl() {
    make -C "$XSCACHE_ROOT" test-top-l2avocado NO_PERF=$NO_PERF NUM_SLICE=$NUM_SLICE
}

stage_lint() {
    (cd "$PKG/scripts" && bash ./check_dut.sh $(dut_sources))
}

stage_filelist() {
    require_mesh_root
    (cd "$PKG/scripts" && AVOCADO_MESH_ROOT="$AVOCADO_MESH_ROOT" \
        bash ./gen_filelist.sh $(dut_sources))
}

stage_build() {
    require_mesh_root
    local sources="$PKG/avocado_top.sv"
    # -Wno-BADVLTPRAGMA: the mesh package's own verilator_waivers.vlt opens
    # with a `// verilator_waivers.vlt` comment that Verilator 5.050 rejects
    # (extra underscore in meta-comment); waiving it here keeps the external
    # mesh package untouched.
    local vargs="-f;$PKG/build/avocado_top.f;$AVOCADO_MESH_ROOT/verilator/verilator_waivers.vlt;--output-split;20000;-Wno-BADVLTPRAGMA"
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
        vargs="-y;$XSCACHE_ROOT/test/scripts/avocado/verilator_override;+libext+.v;$vargs"
    fi
    cmake -S "$CHIRON/cchi/cohestra/cohestra_v3" -B "$BUILD_DIR" \
        -DCOHESTRA_V3_RTL=CUSTOM \
        -DCOHESTRA_V3_TOP_MODULE=avocado_top \
        -DCOHESTRA_V3_CHI_NODEID_WIDTH=11 \
        -DCOHESTRA_V3_CHI_DATACHECK_PRESENT=1 \
        -DCOHESTRA_V3_CHI_POISON_PRESENT=1 \
        -DCOHESTRA_V3_CHI_MPAM_PRESENT=1 \
        -DCOHESTRA_V3_RTL_SOURCES="$sources" \
        -DCOHESTRA_V3_THREADS="$VL_THREADS" \
        -DCOHESTRA_V3_VERILATOR_ARGS="$vargs"
    cmake --build "$BUILD_DIR" -j"$BUILD_JOBS"
}

stage_run() {
    # budget: 2x(8192+8192) reset + 25000 warm-up + 96-entry script, x2 time
    run_cohestra_v3 "$BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_avocado.ini" "$BUILD_DIR")" "${CYCLES:-200000}"
}

stage_fuzz() {
    run_cohestra_v3 "$BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_avocado_fuzz.ini" "$BUILD_DIR")" "${CYCLES:-300000}"
}

# -- VCS stages (cohestra_vcs harness in CHIron) -------------------------------
VCS_DIR="$CHIRON/cchi/cohestra/cohestra_vcs"
VCS_BUILD_DIR=${VCS_BUILD_DIR:-$XSCACHE_ROOT/build/cohestra_vcs_l2avocado}

stage_vcs-smoke() {
    bash "$VCS_DIR/smoke/run_vcs_smoke.sh"
}

stage_vcs-build() {
    require_mesh_root
    stage_vcs-smoke   # go/no-go preflight
    [ -f "$PKG/build/avocado_top.f" ] || stage_filelist
    CHIRON="$CHIRON" AVOCADO_MESH_ROOT="$AVOCADO_MESH_ROOT" \
        BUILD_DIR="$VCS_BUILD_DIR" \
        FILELIST="$PKG/build/avocado_top.f" TOP_SOURCES="$PKG/avocado_top.sv" \
        bash "$VCS_DIR/build.sh" all
}

stage_vcs-run() {
    run_simv "$VCS_BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_avocado.ini" "$VCS_BUILD_DIR")" "${CYCLES:-200000}"
}

stage_vcs-fuzz() {
    run_simv "$VCS_BUILD_DIR" "$(clogb_config "$XSCACHE_ROOT/test/configs/config.xscache_avocado_fuzz.ini" "$VCS_BUILD_DIR")" "${CYCLES:-300000}"
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
