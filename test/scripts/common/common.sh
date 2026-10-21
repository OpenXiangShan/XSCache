#*****************************************************************************
# common.sh — shared machinery for the test/scripts/<dut>/boot_<dut>.sh drivers.
# Sourced (never executed): each driver sets DUT_SCRIPT (for messages) and then
# sources this file via
#   DUT_SCRIPT="$(basename "${BASH_SOURCE[0]}")"
#   source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../common/common.sh"
#
# Provides (all functions; only XSCACHE_ROOT/CHIRON/VCS_DIR/AVOCADO_INTEGRATION
# are set at source time):
#   install_on_err        stage-aware ERR trap (set STAGE=<name> per stage)
#   setup_java            mill needs JDK >= 11 (PATH java may be too old)
#   resolve_vcs_tools     validated VCS_HOME/VERDI_HOME detection + export
#   rtl_regen_active <pat> / wait_rtl_settled <pat> <dir>
#   clogb_config <base.ini> <out-dir>   (caller defines clogb_sections)
#   derive_idle_config <base.ini> <out.ini>
#   print_exit_legend <rc>
#   run_cohestra_v3 <build_dir> <config> <cycles>
#   run_simv <build_dir> <config> <cycles>
#*****************************************************************************

COMMON_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
XSCACHE_ROOT="$(cd "$COMMON_DIR/../../.." && pwd)"
# CHIRON source tree: the in-repo submodule by default, or an external checkout
# via CHIRON (or the legacy CHIRON_OVERRIDE alias) — used strictly read-only.
CHIRON="${CHIRON:-${CHIRON_OVERRIDE:-$XSCACHE_ROOT/test/CHIron}}"
VCS_DIR="$CHIRON/cchi/cohestra/cohestra_vcs"
AVOCADO_INTEGRATION="$CHIRON/cchi/cohestra/cohestra_packages/avocado/integration"

DUT_SCRIPT="${DUT_SCRIPT:-$(basename "$0")}"

# ------------------------------------------------------------------ errors
# Every stage is covered: any failing command aborts the script immediately
# and reports which stage failed, the exit code and the script line.
STAGE="startup"
on_err() {
    rc=$?
    echo "" >&2
    echo "$DUT_SCRIPT: ERROR in stage [$STAGE] — exit code $rc (script line ${BASH_LINENO[0]:-$LINENO})" >&2
    exit "$rc"
}
install_on_err() { trap on_err ERR; }

# ------------------------------------------------------------------ java
# mill needs JDK >= 11; the default PATH java may be a bundled Java 8.
# Probes $JAVA_HOME, then standard system locations; hard error if none works.
setup_java() {
    if ! java -version 2>&1 | grep -qE '"(1[1-9]|[2-9][0-9])\.'; then
        local jh
        for jh in "${JAVA_HOME:-}" /usr/lib/jvm/default-java /usr/lib/jvm/java-17-openjdk-amd64; do
            if [ -n "$jh" ] && [ -x "$jh/bin/java" ]; then
                export JAVA_HOME="$jh"
                export PATH="$JAVA_HOME/bin:$PATH"
                break
            fi
        done
    fi
    java -version 2>&1 | grep -qE '"(1[1-9]|[2-9][0-9])\.' \
        || { echo "$DUT_SCRIPT: ERROR: JDK >= 11 not found; set JAVA_HOME" >&2; exit 2; }
}

# ------------------------------------------------------- VCS/Verdi tools
# Validated — a requirement, not mere defaults: the ambient shell may carry a
# stale VCS_HOME (W-2024.09-SP1 — not actually installed here; its bin/vcs does
# not exist), so a pre-set VCS_HOME/VERDI_HOME is honored only when its binary
# exists; otherwise the tool is auto-detected from PATH. Called only by the
# vcs-* stages, so Verilator-only runs never require a VCS installation.
resolve_vcs_tools() {
    local b
    if [ ! -x "${VCS_HOME:-}/bin/vcs" ]; then
        b="$(command -v vcs || true)"
        [ -n "$b" ] && VCS_HOME="$(dirname "$(dirname "$(readlink -f "$b")")")"
    fi
    [ -x "${VCS_HOME:-}/bin/vcs" ] \
        || { echo "$DUT_SCRIPT: ERROR: VCS not found; set VCS_HOME or put vcs on PATH" >&2; exit 2; }
    export VCS_HOME
    export PATH="$VCS_HOME/bin:$PATH"
    if [ ! -x "${VERDI_HOME:-}/bin/verdi" ]; then
        b="$(command -v verdi || true)"
        [ -n "$b" ] && VERDI_HOME="$(dirname "$(dirname "$(readlink -f "$b")")")"
    fi
    [ -x "${VERDI_HOME:-}/bin/verdi" ] \
        || { echo "$DUT_SCRIPT: ERROR: Verdi not found; set VERDI_HOME or put verdi on PATH" >&2; exit 2; }
    export VERDI_HOME
}

# ------------------------------------------------------------ RTL settle
# True while a make/firtool whose args mention <pat> is alive (the wrapper
# shell of an external regen job may linger into its follow-up sim stages;
# only the build tools themselves rewrite the RTL dir).
rtl_regen_active() {
    ps -eo comm,args | grep -E '^(make|firtool)' | grep -q "$1"
}
wait_rtl_settled() {
    while rtl_regen_active "$1"; do
        echo "$DUT_SCRIPT: $2 regen in progress; waiting 30 s ..."
        sleep 30
    done
}

# ------------------------------------------------------------------ clogb
# clogb_config <base.ini> <out-dir> — echo the config path to use: with
# CLOGB=1, a derived copy in <out-dir> with any [clogb]/[clogb.topo] sections
# stripped and fresh ones appended (deterministic regardless of the INI
# parser's duplicate-key semantics). The sourcing driver defines
# clogb_sections() to print the [clogb]/[clogb.topo] body (it may use
# $CLOGB_FILE and its own DUT variables). Time unit in the log = harness ticks.
clogb_config() {
    local base="$1" dir="$2"
    [ "${CLOGB:-0}" = 1 ] || { echo "$base"; return; }
    local out="$dir/$(basename "$base" .ini).clogb.ini"
    mkdir -p "$dir"
    awk '/^\[(clogb|clogb\.topo)\]/{skip=1; next} /^\[/{skip=0} !skip' "$base" > "$out"
    { echo; clogb_sections; } >> "$out"
    echo "$out"
}

# -------------------------------------------------------- idle configs
# derive_idle_config <base.ini> <out.ini> — zero-traffic variant of a smoke
# config (stimulator.0 disabled), generated artifact, verified. The transaction
# watchdog stays quiet with zero outstanding transactions, so no other key
# needs changing. Reset still runs per the base config.
derive_idle_config() {
    local base="$1" out="$2"
    sed 's/^stimulator\.0\.enable             = 1/stimulator.0.enable             = 0/' \
        "$base" > "$out"
    grep -q '^stimulator\.0\.enable             = 0' "$out" \
        || { echo "$DUT_SCRIPT: error: failed to derive idle config from $base" >&2; exit 1; }
}

# ------------------------------------------------------------- exit codes
# cohestra exit-code legend (printed after every run): 0 = FINISHED (pass),
# 1 = usage/config error, 2 = verification failure (instance FAILED or
# cache-line data mismatch).
print_exit_legend() {
    local rc=$1
    echo "================================================================"
    echo "cohestra exit code: $rc (0=FINISHED, 1=usage/config error, 2=verification failure)"
    return "$rc"
}

# ------------------------------------------------------------ run wrappers
# run_cohestra_v3 <build_dir> <config> <cycles> — run the Verilator harness;
# the sim's own exit code propagates (never killed by set -e).
run_cohestra_v3() {
    local dir="$1" config="$2" cycles="$3" rc=0
    (cd "$dir" && ./cohestra_v3 -c "$config" -n "$cycles") || rc=$?
    print_exit_legend "$rc"
    return "$rc"
}

# run_simv <build_dir> <config> <cycles> — same for the VCS harness. VCS
# $finish exits 0 even when the harness latched a failure, so the process
# exit code is NOT authoritative: the bridge's COHESTRA_VCS_EXIT_STATUS
# marker (teed to simv_last_run.log) is — =1 or a missing marker flips the
# result to verification failure (2).
run_simv() {
    local dir="$1" config="$2" cycles="$3" rc=0
    (cd "$dir" && ./simv +cohestra_config="$config" +cohestra_cycles="$cycles" 2>&1 | tee simv_last_run.log) || rc=$?
    if [ "$rc" -eq 0 ]; then
        if grep -q '^COHESTRA_VCS_EXIT_STATUS=1' "$dir/simv_last_run.log" 2>/dev/null; then
            echo "run_simv: COHESTRA_VCS_EXIT_STATUS=1 — harness reported FAILED" >&2
            rc=2
        elif ! grep -q '^COHESTRA_VCS_EXIT_STATUS=' "$dir/simv_last_run.log" 2>/dev/null; then
            echo "run_simv: no COHESTRA_VCS_EXIT_STATUS marker — run ended abnormally" >&2
            rc=2
        fi
    fi
    print_exit_legend "$rc"
    return "$rc"
}
