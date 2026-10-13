#!/usr/bin/env bash
set -euo pipefail
# Run only from the source worktree. Never replace the build/out symlinks.
stage=${1:?usage: bash scripts/run-bop-2d-regression.sh single|collision|multi}
test -f build.sc
if test -d /data/zhuy; then
  test "$(readlink build)" = /data/zhuy/bop-on-base/build
  test "$(readlink out)" = /data/zhuy/bop-on-base/out
  regression_dir=${BOP_REGRESSION_DIR:-/data/zhuy/bop-on-base/regression-20261013-2d}
else
  echo '本机无 /data/zhuy，产物仍写在 NFS'
  regression_dir=${BOP_REGRESSION_DIR:-regression-20261013-2d}
fi
mkdir -p "$regression_dir/$stage"
# The allowed compile target is run separately. Use its compiled classpath.
regression_cp=$(jq -r '.value | map(sub("^[^:]*:[^:]*:[^:]*:"; "")) | join(":")' out/XSCache/testtop/oceanus/runClasspath.json)
run_case() {
  local case_name=$1 slices=$2
  local case_log="$regression_dir/$stage/$case_name-${slices}slice.log"
  if test -f "$case_log"; then mv "$case_log" "$case_log.previous-$(date +%H%M%S)"; fi
  local before after new_count sim_exit=0
  before=$(ls -1 build/chiselsim)
  {
    git rev-parse HEAD
    git status --short
    git diff HEAD -- src/main src/test | sha256sum
    sha256sum src/test/scala/oceanus/L2TopBOPExpectations.scala
    printf 'CHISELSIM_BEFORE\n%s\n' "$before"
  } > "$case_log"
  CCACHE_DISABLE=1 java -cp "$regression_cp" oceanus.TestTop_L2Top --simulate --case="$case_name" --slices "$slices" >> "$case_log" 2>&1 || sim_exit=$?
  after=$(ls -1 build/chiselsim)
  printf 'CHISELSIM_AFTER\n%s\nEXIT=%s\n' "$after" "$sim_exit" >> "$case_log"
  new_count=$(comm -13 <(printf '%s\n' "$before") <(printf '%s\n' "$after") | wc -l)
  test "$new_count" -eq 1
  if test "$sim_exit" -ne 0; then
    echo "SIM_FAIL stage=$stage case=$case_name slices=$slices exit=$sim_exit log=$case_log"
    tail -n 35 "$case_log"
    return "$sim_exit"
  fi
  # Count every EVB fire, including internal prefetch sources that the demand
  # handle ledger excludes. Independent set occupancy predicts zero for all.
  rg -q '^SLICE_EVICTBACK_FIRE counts=0(,0)*$' "$case_log"
  echo "SIM_PASS stage=$stage case=$case_name slices=$slices log=$case_log"
  rg 'BOP_CLOSED_LOOP_BASE_EXPECTED|BOP_CLOSED_LOOP_RESULT|BOP_COLLISION_BASE|BOP_COLLISION_EXPECTED|BOP_COLLISION_RESULT|COUNTS total|TRAIN_FIRE count|SLICE_EVICTBACK_FIRE' "$case_log"
}
case "$stage" in
  single)
    run_case bop-closed-loop 1
    run_case bop-closed-loop-uc 1
    ;;
  collision)
    run_case bop-collision-c1 1
    run_case bop-collision-c2 1
    run_case bop-collision-c2-uc 1
    run_case bop-collision-c3 1
    ;;
  multi)
    for slices in 2 4; do
      run_case bop-closed-loop "$slices"
      run_case bop-closed-loop-uc "$slices"
    done
    ;;
  *) echo "unknown stage: $stage" >&2; exit 2 ;;
esac
