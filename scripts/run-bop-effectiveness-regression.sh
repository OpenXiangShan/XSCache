#!/usr/bin/env bash
set -euo pipefail
# Run from the worktree root. Compile only an approved Mill target, then use
# the resulting Java classes; never invoke Mill in /data or alter its symlinks.
stage=${1:-all}
case "$stage" in baseline|issue-off|smoke|single|collision|directed|multi|all) ;; *) exit 2 ;; esac
test -f build.sc
repo_name=${PWD##*/}
if test -d /data/zhuy; then
  test "$(readlink build)" = "/data/zhuy/$repo_name/build"
  test "$(readlink out)" = "/data/zhuy/$repo_name/out"
  log_root=${BOP_REGRESSION_DIR:-/data/zhuy/$repo_name/bop-effectiveness-$(date +%Y%m%d-%H%M%S)}
else
  echo '本机无 /data/zhuy，产物仍写在 NFS'
  log_root=${BOP_REGRESSION_DIR:-bop-effectiveness-$(date +%Y%m%d-%H%M%S)}
fi
mkdir -p "$log_root/$stage" build/chiselsim
fingerprint() {
  git rev-parse HEAD
  git status --short
  git diff HEAD -- src/main src/test scripts | sha256sum
  git ls-files --others --exclude-standard -z -- src/main src/test scripts | xargs -0 -r sha256sum
}
compile_log="$log_root/$stage/compile.log"
test ! -e "$compile_log"
fingerprint > "$compile_log"
printf 'CHISELSIM_BEFORE\n' >> "$compile_log"
ls -1 build/chiselsim >> "$compile_log"
CCACHE_DISABLE=1 mill --no-server XSCache.testtop.oceanus.compile >> "$compile_log" 2>&1
printf 'EXIT=0\n' >> "$compile_log"
# compile does not populate runClasspath.json. Include the compiled dependency
# classes and resource roots from compileClasspath, plus the test entry classes.
regression_cp=$(python3 - <<'PY'
import json,pathlib
p=pathlib.Path('out/XSCache/testtop/oceanus')
items=[x.split(':',3)[3] for x in json.loads((p/'compileClasspath.json').read_text())['value']]
paths=[]
for item in items:
    paths.append(item)
    if item.endswith('/compile-resources'):
        base=pathlib.Path(item).parent
        paths.extend(str(base/d) for d in ('src/main/resources','resources'))
paths.append(json.loads((p/'compile.json').read_text())['value']['classes'].split(':',3)[3])
print(':'.join(dict.fromkeys(paths)))
PY
)
run_case() {
  local case_name=$1 slices=$2 entry=oceanus.TestTop_L2Top
  shift 2
  if [[ "$case_name" == bop-2c-* || "$case_name" == bop-stash-all || "$case_name" == bop-useless-all ]]; then
    entry=oceanus.TestTop_L2Prefetched
  fi
  local case_log="$log_root/$stage/$case_name-${slices}slice.log" before after code=0
  test ! -e "$case_log"
  before=$(ls -1 build/chiselsim)
  fingerprint > "$case_log"
  printf 'CHISELSIM_BEFORE\n%s\n' "$before" >> "$case_log"
  CCACHE_DISABLE=1 java -cp "$regression_cp" "$entry" --simulate --case="$case_name" --slices "$slices" "$@" >> "$case_log" 2>&1 || code=$?
  after=$(ls -1 build/chiselsim)
  printf 'CHISELSIM_AFTER\n%s\nEXIT=%s\n' "$after" "$code" >> "$case_log"
  echo "SIM case=$case_name slices=$slices exit=$code log=$case_log"
  test "$code" -eq 0
  rg 'BOP_PERF_RESULT|BOP_PERF_ACCOUNTING|BOP_STASH_RESULT|BOP_USELESS_RESULT|BOP_RESIDENT_RESULT|BOP_2C_RESULT|BOP_ISSUE_OFF_BASELINE' "$case_log"
}
if [[ "$stage" == baseline || "$stage" == all ]]; then
  for slices in 1 2 4; do
    run_case load-miss "$slices" --no-bop
    run_case dirty-victim "$slices" --no-bop
  done
fi
# Fresh current-HEAD A/B supplement: baseline is queue-free (--no-bop),
# issue-off retains BOP training/queues while disabling prefetch injection.
# Before simulation, expect the six demand/count summaries to match each
# other and the frozen historical A/B controls; no prefetch may enter L2.
if [[ "$stage" == issue-off || "$stage" == all ]]; then
  for slices in 1 2 4; do
    run_case load-miss "$slices" --no-prefetch-issue
    run_case dirty-victim "$slices" --no-prefetch-issue
  done
fi
if [[ "$stage" == smoke || "$stage" == all ]]; then
  run_case bop-prefetch-smoke 1
  run_case bop-prefetch-smoke-uc 1
  run_case bop-prefetch-smoke 2
  run_case bop-prefetch-smoke 4
fi
if [[ "$stage" == single || "$stage" == all ]]; then
  run_case bop-closed-loop 1
  run_case bop-closed-loop-uc 1
fi
if [[ "$stage" == collision || "$stage" == all ]]; then
  for suffix in c1 c2 c2-uc c3; do run_case "bop-collision-$suffix" 1; done
fi
if [[ "$stage" == directed || "$stage" == all ]]; then
  run_case bop-2c-all 1
  run_case bop-stash-all 1
  run_case bop-useless-all 1
fi
if [[ "$stage" == multi || "$stage" == all ]]; then
  for slices in 2 4; do
    run_case bop-closed-loop "$slices"
    run_case bop-closed-loop-uc "$slices"
  done
fi
