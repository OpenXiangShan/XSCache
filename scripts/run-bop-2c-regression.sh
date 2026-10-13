#!/usr/bin/env bash
set -euo pipefail
# Run from the source worktree; build/out must retain their NVMe symlinks.
stage=${1:?usage: bash scripts/run-bop-2c-regression.sh A|B|smoke|new}
test -f build.sc
if test -d /data/zhuy; then
  test "$(readlink build)" = /data/zhuy/bop-on-base/build
  test "$(readlink out)" = /data/zhuy/bop-on-base/out
  regression_dir=${BOP_REGRESSION_DIR:-/data/zhuy/bop-on-base/regression-20261012-2c}
else
  echo '本机无 /data/zhuy，产物仍写在 NFS'
  regression_dir=${BOP_REGRESSION_DIR:-regression-20261012-2c}
fi
mkdir -p "$regression_dir/$stage"
fingerprint() {
  git rev-parse HEAD
  git status --short
  git diff HEAD -- src/main src/test | sha256sum
  if test -f src/test/scala/oceanus/L2TopPrefetchedModel.scala; then
    sha256sum src/test/scala/oceanus/L2TopPrefetchedModel.scala
  fi
  if test -f src/test/scala/oceanus/L2TopBOPExpectations.scala; then
    sha256sum src/test/scala/oceanus/L2TopBOPExpectations.scala
  fi
}
compile_log="$regression_dir/$stage/compile.log"
if test -f "$compile_log"; then mv "$compile_log" "$compile_log.previous-$(date +%H%M%S)"; fi
fingerprint > "$compile_log"
CCACHE_DISABLE=1 mill --no-server XSCache.testtop.oceanus.compile >> "$compile_log" 2>&1
echo "COMPILE_PASS stage=$stage log=$compile_log"
regression_cp=$(jq -r '.value | map(sub("^[^:]*:[^:]*:[^:]*:"; "")) | join(":")' out/XSCache/testtop/oceanus/runClasspath.json)
run_case() {
  local case_name=$1 slices=$2
  shift 2
  local case_log="$regression_dir/$stage/$case_name-${slices}slice.log"
  if test -f "$case_log"; then mv "$case_log" "$case_log.previous-$(date +%H%M%S)"; fi
  local before after new_count
  before=$(ls -1 build/chiselsim)
  fingerprint > "$case_log"
  printf 'CHISELSIM_BEFORE\n%s\n' "$before" >> "$case_log"
  local sim_exit=0
  local entry=oceanus.TestTop_L2Top
  if [[ "$case_name" == bop-2c-* ]]; then entry=oceanus.TestTop_L2Prefetched; fi
  CCACHE_DISABLE=1 java -cp "$regression_cp" "$entry" --simulate --case="$case_name" --slices "$slices" "$@" >> "$case_log" 2>&1 || sim_exit=$?
  after=$(ls -1 build/chiselsim)
  printf 'CHISELSIM_AFTER\n%s\nEXIT=%s\n' "$after" "$sim_exit" >> "$case_log"
  new_count=$(comm -13 <(printf '%s\n' "$before") <(printf '%s\n' "$after") | wc -l)
  test "$new_count" -eq 1
  if test "$sim_exit" -ne 0; then
    echo "SIM_FAIL stage=$stage case=$case_name slices=$slices exit=$sim_exit log=$case_log"
    tail -n 30 "$case_log"
    return "$sim_exit"
  fi
  echo "SIM_PASS stage=$stage case=$case_name slices=$slices log=$case_log"
  rg 'BOP_ISSUE_OFF_BASELINE|BOP_PREFETCH_SMOKE_RESULT|BOP_2C_RESULT|COUNTS total|TRAIN_FIRE count|CASE .*actual|SLICE_EVICTBACK_FIRE' "$case_log" || true
}
case "$stage" in
  A|B)
    for slices in 1 2 4; do
      run_case load-miss "$slices" --no-bop
      run_case dirty-victim "$slices" --no-bop
    done
    ;;
  smoke)
    run_case bop-prefetch-smoke 1
    run_case bop-prefetch-smoke-uc 1
    run_case bop-prefetch-smoke 2
    run_case bop-prefetch-smoke 4
    ;;
  new)
    # All variants share this configuration. Reset SRAM/BOP and all host
    # queues/counters between independent cases in the same generated model.
    run_case bop-2c-all 1
    ;;
  *) echo "unknown stage: $stage" >&2; exit 2 ;;
esac
