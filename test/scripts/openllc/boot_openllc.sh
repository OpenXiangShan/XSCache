#!/usr/bin/env bash
#*****************************************************************************
# boot_openllc.sh — build & smoke driver for the XSCache TestTop_L2OpenLLC DUT
# (NUM_L2 x Oceanus L2 -> OpenLLC -> OpenNCB -> AXI4) under the CHIron Cohestra
# harness, covering BOTH simulators:
#
#   Verilator flow (cohestra_v3, CUSTOM DUT preset) — the flow that used to be
#   test/boot.sh. No CHIron files are modified: the harness is built
#   out-of-source into build/cohestra_v3*/.
#
#   VCS flow (cohestra_vcs, VCS + DPI) via the unified wrapper top
#   test/scripts/common/xscache_vcs_top.sv (+define+-parameterized per L2
#   count; the harness bridge/vcs_top.hpp are used unchanged).
#
# Stages (multiple may be given, run in order):
#   rtl          regenerate the DUT Verilog (make test-top-$DUT; rm -rf first —
#                firtool leaves stale files across configurations)
#   stage        rsync --checksum --delete --no-times build/$DUT/ ->
#                build/${DUT}_staged/ (content-stable handoff so Verilator's
#                --skip-identical engages; firtool rewrites every .sv each run)
#   build        configure & build the cohestra_v3 executable (Verilator; long)
#   run          boot cohestra_v3 with $CONFIG (default the 1-L2 FuzzARI smoke)
#   all          rtl + stage + build + run (default)
#   vcs-filelist generate $VCS_BUILD_DIR/l2openllc_vcs.f (+incdir for
#                avocado_dut_if.svh + the wrapper + build/l2openllc_staged/*)
#   vcs-build    vcs-filelist + cohestra_vcs/build.sh all (harness archive +
#                simv; long). NUM_L2 selects the wrapper's
#                +define+XSCACHE_VCS_TOP_L2_P* set and MUST match the staged RTL
#   vcs-run      idle smoke under VCS: zero-traffic variant of $VCS_CONFIG
#                (stimulator.0 disabled), short run — bring-up gate
#   vcs-fuzz     traffic smoke under VCS ($VCS_CONFIG, FuzzARI)
#   vcs-all      vcs-build + vcs-run
#
# Env knobs:
#   DUT          l2openllc (default; TestTop_L2OpenLLC) | l2multichi
#                (TestTop_L2MultiCHI, Verilator flow only — there is no
#                multichi VCS wrapper; harness build dir build/cohestra_v3_l2multichi)
#   CHIRON / CHIRON_OVERRIDE (default test/CHIron, the in-repo checkout; used
#                strictly read-only)
#   NUM_L2       (default 1) L2 count for rtl/build; the VCS wrapper's define
#                set comes from the same knob and must match the staged RTL
#                (e.g. NUM_L2=4 for the 4-L2 VCS smoke)
#   NUM_SLICE    (default 2)
#   CONFIG       Verilator run config (default
#                test/configs/config.xscache.ini; absolutized — the sim runs
#                from the build dir)
#   VCS_CONFIG   VCS config (default test/configs/config.xscache_vcs_openllc.ini)
#   CYCLES       run budget (default 200000; vcs-run default 20000 harness-time
#                units — harness time runs 2 units per clock)
#   WAVEFORM     FST (default) | VCD — compiled-in trace format; the config's
#                [verilated] waveform.type must match
#   SOAK_NOASSERT (default 0; 1 = prepend soak_noassert_defines.sv to the RTL
#                sources so firtool assertion bodies become no-ops — escape
#                hatch for soak-style runs only; smokes run with assertions ON)
#   NO_PERF      (default 1 — DUT performance counters DISABLED; ~66% faster
#                end-to-end build at 16 TSHRs/slice. NO_PERF=0 re-enables)
#   THREADS      (default 0 = legacy single-threaded verilated model; N >= 1
#                builds the mtask-partitioned model with N worker threads)
#   SKIP_RUN     (default 0; 1 = stop after the Verilator harness build)
#   EXTRA_MAKE_ARGS   extra args appended to the RTL make line
#   EXTRA_CMAKE_ARGS  extra args appended to the harness cmake configure line
#   VERILATOR_BIN (default: auto-detected real verilator binary from PATH)
#   VCS_HOME / VERDI_HOME (vcs-* stages only — honored when their bin/ exists,
#                else auto-detected from PATH; never blindly inherited)
#   JAVA_HOME    (probed when PATH java is too old for mill; then standard
#                system locations; hard error if no JDK >= 11 is found)
#*****************************************************************************
set -euo pipefail

DUT_SCRIPT="$(basename "${BASH_SOURCE[0]}")"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../common/common.sh"
install_on_err
setup_java

# DUT selection: make target + Verilator top module + build directories.
DUT="${DUT:-l2openllc}"
case "$DUT" in
  l2openllc)  TOP_MODULE="TestTop_L2OpenLLC" ;;
  l2multichi) TOP_MODULE="TestTop_L2MultiCHI" ;;
  *) echo "unknown DUT: $DUT (supported: l2openllc, l2multichi)" >&2; exit 1 ;;
esac
RTL_DIR="$XSCACHE_ROOT/build/$DUT"
RTL_STAGED="$XSCACHE_ROOT/build/${DUT}_staged"
if [[ "$DUT" == "l2openllc" ]]; then
  BUILD_DIR="$XSCACHE_ROOT/build/cohestra_v3"
else
  BUILD_DIR="$XSCACHE_ROOT/build/cohestra_v3_$DUT"
fi
VCS_BUILD_DIR=${VCS_BUILD_DIR:-$XSCACHE_ROOT/build/cohestra_vcs_l2openllc}

EXTRA_MAKE_ARGS="${EXTRA_MAKE_ARGS:-}"
EXTRA_CMAKE_ARGS="${EXTRA_CMAKE_ARGS:-}"
CONFIG="${CONFIG:-$XSCACHE_ROOT/test/configs/config.xscache.ini}"
# absolutize: the sim runs from $BUILD_DIR, so a relative CONFIG would break
CONFIG="$(realpath -m "$CONFIG")"
VCS_CONFIG="${VCS_CONFIG:-$XSCACHE_ROOT/test/configs/config.xscache_vcs_openllc.ini}"
CYCLES="${CYCLES:-200000}"
NUM_L2="${NUM_L2:-1}"
NUM_SLICE="${NUM_SLICE:-2}"
WAVEFORM="${WAVEFORM:-FST}"
WAVEFORM="${WAVEFORM^^}" # CMake expects FST or VCD, uppercase
SOAK_NOASSERT="${SOAK_NOASSERT:-0}"
NO_PERF="${NO_PERF:-1}"
THREADS="${THREADS:-0}"
SKIP_RUN="${SKIP_RUN:-0}"

WRAPPER="$XSCACHE_ROOT/test/scripts/common/xscache_vcs_top.sv"
VCS_TOP_MODULE="xscache_vcs_top"
VCS_FILELIST="$VCS_BUILD_DIR/l2openllc_vcs.f"

COHESTRA_V3_SRC="$CHIRON/cchi/cohestra/cohestra_v3"

# In-repo bootstrap (default CHIRON source = the test/CHIron submodule): make
# sure the submodule and its nested spdlog/simpleini submodules are
# initialized. No-op once everything is in place. When CHIRON/CHIRON_OVERRIDE
# points elsewhere, the override tree is used strictly read-only — no git
# actions on it.
if [[ "${CHIRON}" == "$XSCACHE_ROOT/test/CHIron" ]]; then
  if [[ ! -f "$CHIRON/common/eventbus.hpp" ]]; then
    git -C "$XSCACHE_ROOT" submodule update --init --recursive test/CHIron
  elif [[ ! -f "$CHIRON/cchi/cohestra/spdlog/CMakeLists.txt" \
       || ! -f "$CHIRON/cchi/cohestra/simpleini/SimpleIni.h" ]]; then
    git -C "$CHIRON" submodule update --init --recursive
  fi
fi

# ------------------------------------------------------------ Verilator flow
stage_rtl() {
  STAGE="rtl (make test-top-$DUT)"
  echo "== generating $TOP_MODULE RTL (NUM_L2=$NUM_L2 NUM_SLICE=$NUM_SLICE no_perf=$NO_PERF)"
  # firtool does not clean its output directory: files from a previous
  # configuration (e.g. LogPerfEndpoint.sv left over when toggling NO_PERF)
  # would linger and be picked up into the verilation sources.
  rm -rf "$RTL_DIR"
  make -C "$XSCACHE_ROOT" "test-top-$DUT" NUM_L2="$NUM_L2" NUM_SLICE="$NUM_SLICE" NO_PERF="$NO_PERF" $EXTRA_MAKE_ARGS
  [[ -f "$RTL_DIR/$TOP_MODULE.sv" ]] || { echo "missing $RTL_DIR/$TOP_MODULE.sv" >&2; exit 1; }
}

stage_stage() {
  # Content-based sync of the generated RTL into a stable staging directory.
  # firtool rewrites every .sv on each run (new inode/ctime, identical content);
  # Verilator's --skip-identical records each source's inode/ctime and cannot
  # fall back to hashing for large files, so without a content-stable handoff
  # EVERY build triggers a full re-verilation and a rewrite of all generated
  # C++ (i.e. incremental builds never happen). rsync --checksum --no-times
  # leaves unchanged files completely untouched (no data write, no mtime
  # tweak — even an mtime sync bumps ctime and breaks skip-identical for
  # the large files Verilator cannot hash), so skip-identical actually engages.
  STAGE="stage (rsync --checksum)"
  [ -d "$RTL_DIR" ] || { echo "DUT RTL not found: $RTL_DIR (run the rtl stage first)" >&2; exit 1; }
  rsync -r --checksum --delete --no-times "$RTL_DIR/" "$RTL_STAGED/"
}

configure_harness() {
  # Oceanus CCHI carries a 2-bit TagAlias (Parameters.scala TagAlias_Width=2);
  # without this the harness ties the DUT's TagAlias pins to 0.
  cmake -S "$COHESTRA_V3_SRC" -B "$BUILD_DIR" \
    -DCOHESTRA_V3_RTL=CUSTOM \
    -DCOHESTRA_V3_TOP_MODULE="$TOP_MODULE" \
    -DCOHESTRA_V3_WAVEFORM_FORMAT="$WAVEFORM" \
    -DCOHESTRA_V3_THREADS="$THREADS" \
    -DCOHESTRA_V3_TAGALIAS_WIDTH=2 \
    -DCOHESTRA_V3_RTL_SOURCES="$SOURCES" \
    $EXTRA_CMAKE_ARGS \
    "${VERILATOR_BIN_ARG[@]}"
}

stage_build() {
  STAGE="build (cohestra_v3 harness: $TOP_MODULE, waveform=$WAVEFORM, soak_noassert=$SOAK_NOASSERT, threads=$THREADS)"
  echo "== $STAGE"
  SOURCES="$(find "$RTL_STAGED" -maxdepth 1 \( -name '*.sv' -o -name '*.v' \) | sort | tr '\n' ';')$XSCACHE_ROOT/test/scripts/common/xscache_waivers.vlt"
  if [[ "$SOAK_NOASSERT" -eq 1 ]]; then
    # must be first: Verilator preprocessor defines are global and order-dependent
    SOURCES="$XSCACHE_ROOT/test/scripts/common/soak_noassert_defines.sv;$SOURCES"
  fi

  # Work around a Verilator packaging pitfall that breaks incremental builds:
  # CMake's find_program() picks $VERILATOR_ROOT/bin/verilator_bin, which on
  # this install is a perl redirect that execs the real binary with a bare
  # argv[0]="verilator_bin". Verilator records argv[0] as a source dependency
  # in <Mdir>/<prefix>__verFiles.dat, but that bare name can never be stat'ed
  # again, so --skip-identical never engages: EVERY build re-verilates from
  # scratch and rewrites all generated C++, forcing a full recompile+relink on
  # every boot. Pre-seeding the real binary's absolute path into the CMake
  # cache fixes the recorded dependency and restores skip-identical.
  VERILATOR_BIN_ARG=()
  if [[ -n "${VERILATOR_BIN:-}" ]]; then
    VERILATOR_BIN_ARG=(-DVERILATOR_BIN="$VERILATOR_BIN")
  else
    _vb="$(command -v verilator_bin || true)"
    # must be the real ELF binary, not the perl redirect script
    if [[ -n "$_vb" ]] && [[ "$(head -c 4 "$_vb" 2>/dev/null)" == $'\x7fELF' ]]; then
      VERILATOR_BIN_ARG=(-DVERILATOR_BIN="$_vb")
    fi
  fi

  # A stale CMake cache records the previous -S source dir; configuring against
  # a different cohestra_v3 tree (e.g. after switching CHIRON) fails hard with
  # "source does not match the source used to generate cache". Detect the
  # mismatch and start the harness build dir fresh.
  if [[ -f "$BUILD_DIR/CMakeCache.txt" ]]; then
    _cached_src="$(sed -n 's/^CMAKE_HOME_DIRECTORY:INTERNAL=//p' "$BUILD_DIR/CMakeCache.txt")"
    if [[ -n "$_cached_src" && "$_cached_src" != "$COHESTRA_V3_SRC" ]]; then
      echo "$DUT_SCRIPT: CMake cache was generated from $_cached_src — wiping $BUILD_DIR"
      rm -rf "$BUILD_DIR"
    fi
  fi
  configure_harness

  if ! cmake --build "$BUILD_DIR" -j "$(nproc)"; then
    # When RTL changes alter the verilated C++ file SET (e.g. a different
    # number of Verilator split files), the build starts from the stale
    # <prefix>.cmake file list of the previous configuration and fails at
    # link time with undefined references. Re-running cmake alone does NOT
    # help: the verilate() module only rebuilds that list when its recorded
    # command line changes (and it rewrites verilator_args.txt on every
    # configure). Deleting verilator_args.txt forces a configure-time
    # re-verilation, which rebuilds the file list; one rebuild then converges.
    echo "$DUT_SCRIPT: build failed — forcing re-verilation and retrying once" >&2
    rm -f "$BUILD_DIR/CMakeFiles/cohestra_v3.dir/V${TOP_MODULE}.dir/verilator_args.txt"
    configure_harness
    cmake --build "$BUILD_DIR" -j "$(nproc)"
  fi
}

stage_run() {
  if [[ "$SKIP_RUN" -eq 1 ]]; then
    echo "== SKIP_RUN=1 — harness build complete, skipping boot"
    return 0
  fi
  STAGE="run (cohestra_v3, config=$(basename "$CONFIG"), cycles=$CYCLES)"
  echo "== $STAGE"
  local rc=0
  (cd "$BUILD_DIR" && ./cohestra_v3 -c "$CONFIG" -n "$CYCLES" 2>&1 | tee smoke.log) || rc=$?
  print_exit_legend "$rc"
  grep -E "mismatch|watchdog|denial|denied|FAILED|FINISHED" "$BUILD_DIR/smoke.log" | tail -25 || true
  return "$rc"
}

stage_all() { stage_rtl; stage_stage; stage_build; stage_run; }

# ----------------------------------------------------------------- VCS flow
vcs_need_dut() {
  [[ "$DUT" == "l2openllc" ]] || { echo "$DUT_SCRIPT: the VCS flow is l2openllc-only (no $DUT wrapper)" >&2; exit 2; }
  case "$NUM_L2" in 1|2|3|4) ;; *) echo "NUM_L2 must be 1..4 (got $NUM_L2)" >&2; exit 2 ;; esac
}

stage_vcs-rtl() {
  # No-op by design: the DUT RTL is regenerated by the rtl stage (or an
  # external make). Only verify the staged file set exists and is settled.
  wait_rtl_settled 'l2openllc' "build/l2openllc"
  [ -f "$RTL_STAGED/TestTop_L2OpenLLC.sv" ] \
      || { echo "error: $RTL_STAGED/TestTop_L2OpenLLC.sv missing — run the rtl+stage stages first" >&2; exit 1; }
  echo "$DUT_SCRIPT: DUT RTL present in $RTL_STAGED ($(ls "$RTL_STAGED"/*.sv | wc -l) .sv files)"
}

stage_vcs-filelist() {
  vcs_need_dut
  mkdir -p "$VCS_BUILD_DIR"
  # +incdir must precede the wrapper so its `include "avocado_dut_if.svh"
  # resolves regardless of how positionally VCS applies incdirs; TOP_SOURCES
  # (see vcs-build) carries the same +incdir for the pre-filelist position.
  {
      echo "+incdir+$AVOCADO_INTEGRATION"
      echo "$WRAPPER"
      # authoritative source list from the rtl step — NOT a glob: raw/staged
      # dirs accumulate stale .sv across bare make runs
      sed "s|^|$RTL_STAGED/|" "$RTL_STAGED/filelist.f"
      ls "$RTL_STAGED"/*.v 2>/dev/null || true
  } > "$VCS_FILELIST"
  echo "$DUT_SCRIPT: filelist written: $VCS_FILELIST ($(wc -l < "$VCS_FILELIST") entries)"
}

stage_vcs-build() {
  vcs_need_dut
  resolve_vcs_tools
  stage_vcs-rtl
  # always regenerate: a cached .f silently pins stale RTL paths after
  # RTL_STAGED changes or regenerations — regen is trivial
  stage_vcs-filelist
  CHIRON="$CHIRON" \
      BUILD_DIR="$VCS_BUILD_DIR" \
      COHESTRA_VCS_TOP_MODULE="$VCS_TOP_MODULE" \
      EXTRA_VCS_FLAGS="$("$XSCACHE_ROOT/test/scripts/common/xscache_vcs_top_defines.sh" openllc "$NUM_L2")" \
      TOP_SOURCES="+incdir+$AVOCADO_INTEGRATION" \
      FILELIST="$VCS_FILELIST" \
      bash "$VCS_DIR/build.sh" all
}

stage_vcs-run() {
  vcs_need_dut
  # Idle bring-up: the smoke config with the FuzzARI stimulator disabled
  # (generated artifact in the build dir, not a repo file). The transaction
  # watchdog stays quiet with zero outstanding transactions, so no other
  # key needs changing. Reset still runs (preamble 0 + 2x(100+100) cycles).
  local idle_ini="$VCS_BUILD_DIR/config.xscache_vcs_openllc_idle.ini"
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

usage() {
    echo "usage: $0 [stages...] (default: all)"
    echo "  stages: rtl stage build run all | vcs-filelist vcs-build vcs-run vcs-fuzz vcs-all"
}

st_to_fn() {
    case "$1" in
        all)     echo stage_all ;;
        vcs-all) echo stage_vcs-all ;;
        *)       echo "stage_$1" ;;
    esac
}

stages=("${@:-all}")
[ $# -eq 0 ] && stages=(all)
for st in "${stages[@]}"; do
    fn="$(st_to_fn "$st")"
    declare -F "$fn" >/dev/null 2>&1 || { echo "unknown stage: $st" >&2; usage >&2; exit 2; }
done
for st in "${stages[@]}"; do
    "$(st_to_fn "$st")"
done
