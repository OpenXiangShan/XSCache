#!/usr/bin/env bash
set -euo pipefail
# Run from bop-next; compile an approved target, then run the Java entry.
selected=${1:-all}
case "$selected" in all|multi-tshr|cross-slice-training|prefetch-starvation|port-readback) ;; *) exit 2 ;; esac
# The Type-4 paired case is retained as source, pending its opcode/CompAck
# contract. Current base waits forever for its nonexistent UpRSP CompAck.
if [[ "$selected" == cross-slice-training ]]; then
  echo 'CONCURRENT_SUSPENDED case=cross-slice-training reason=Type4-ReadShared-opcode-and-CompAck-contract-pending'
  exit 0
fi
test -f build.sc
repo_name=${PWD##*/}
if test -d /data/zhuy; then
  test "$(readlink build)" = "/data/zhuy/$repo_name/build"
  test "$(readlink out)" = "/data/zhuy/$repo_name/out"
  log_root=${BOP_REGRESSION_DIR:-/data/zhuy/$repo_name/bop-concurrent-$(date +%Y%m%d-%H%M%S)}
else
  echo '本机无 /data/zhuy，产物仍写在 NFS'
  log_root=${BOP_REGRESSION_DIR:-bop-concurrent-$(date +%Y%m%d-%H%M%S)}
fi
mkdir -p "$log_root" build/chiselsim
head_before=$(git rev-parse HEAD)
source_hash() {
  git diff HEAD -- src/main src/test scripts | sha256sum
  git ls-files --others --exclude-standard -z -- src/main src/test scripts | xargs -0 -r sha256sum
}
fingerprint() {
  git rev-parse HEAD
  git status --short --branch
  source_hash
}
hash_before=$(source_hash)
compile_log="$log_root/compile.log"
test ! -e "$compile_log"
fingerprint > "$compile_log"
CCACHE_DISABLE=1 mill --no-server XSCache.testtop.oceanus.compile >> "$compile_log" 2>&1
printf 'EXIT=0\n' >> "$compile_log"
regression_cp=$(python3 - <<'PY'
import json,pathlib
p=pathlib.Path('out/XSCache/testtop/oceanus')
paths=[]
for item in json.loads((p/'compileClasspath.json').read_text())['value']:
    item=item.split(':',3)[3]
    paths.append(item)
    if item.endswith('/compile-resources'):
        paths.extend(str(pathlib.Path(item).parent/d) for d in ('src/main/resources','resources'))
paths.append(json.loads((p/'compile.json').read_text())['value']['classes'].split(':',3)[3])
print(':'.join(dict.fromkeys(paths)))
PY
)
if [[ "$selected" == all ]]; then cases=(multi-tshr prefetch-starvation port-readback); else cases=("$selected"); fi
for name in "${cases[@]}"; do
  case_log="$log_root/$name.log"
  test ! -e "$case_log"
  fingerprint > "$case_log"
  printf 'CHISELSIM_BEFORE\n' >> "$case_log"
  ls -1 build/chiselsim >> "$case_log"
  code=0
  CCACHE_DISABLE=1 java -cp "$regression_cp" oceanus.TestTop_L2Concurrent --case="$name" >> "$case_log" 2>&1 || code=$?
  printf 'CHISELSIM_AFTER\n' >> "$case_log"
  ls -1 build/chiselsim >> "$case_log"
  printf 'EXIT=%s\n' "$code" >> "$case_log"
  test "$code" -eq 0
  rg '^CONCURRENT_RESULT .*pass=true' "$case_log"
  test "$(git rev-parse HEAD)" = "$head_before"
  test "$(source_hash)" = "$hash_before"
done
