#!/usr/bin/env bash
set -euo pipefail
# First run the unchanged HEAD with run-bop-2c-regression.sh A in this directory.
# Then change only the production default and explicit test opt-ins, and run
# this driver. Expected: all 38 scenarios and six A/B groups match the 2d logs;
# closed-loop T is measured, +10 is predicted by k*T > L+1 then measured.
# Do not edit any source or running script until the full driver has exited.
test -f build.sc
if test -d /data/zhuy; then
  export BOP_REGRESSION_DIR=${BOP_REGRESSION_DIR:-/data/zhuy/bop-on-base/regression-20261013-default-off}
  previous_regression=/data/zhuy/bop-on-base/regression-20261013-2d
else
  echo '本机无 /data/zhuy，产物仍写在 NFS'
  export BOP_REGRESSION_DIR=${BOP_REGRESSION_DIR:-regression-20261013-default-off}
  previous_regression=regression-20261013-2d
fi
bash scripts/run-bop-2c-regression.sh B
bash scripts/run-bop-2c-regression.sh new
bash scripts/run-bop-2c-regression.sh smoke
for stage in single collision multi; do
  bash scripts/run-bop-2d-regression.sh "$stage"
done
# Read only the explicit 27 model logs; never traverse the large artifact tree.
python3 - "$BOP_REGRESSION_DIR" "$previous_regression" <<'PY' | tee "$BOP_REGRESSION_DIR/verification.log"
import hashlib
import pathlib
import re
import subprocess
import sys

current, previous = map(pathlib.Path, sys.argv[1:])
baseline = [f'{case}-{slices}slice.log' for slices in (1, 2, 4)
            for case in ('load-miss', 'dirty-victim')]
logs = [(side, name) for side in ('A', 'B') for name in baseline]
logs += [('new', 'bop-2c-all-1slice.log')]
logs += [('smoke', f'{case}-{slices}slice.log') for case, slices in
         [('bop-prefetch-smoke', 1), ('bop-prefetch-smoke-uc', 1),
          ('bop-prefetch-smoke', 2), ('bop-prefetch-smoke', 4)]]
logs += [('single', f'{case}-1slice.log') for case in ('bop-closed-loop', 'bop-closed-loop-uc')]
logs += [('collision', f'bop-collision-{case}-1slice.log') for case in ('c1', 'c2', 'c2-uc', 'c3')]
logs += [('multi', f'{case}-{slices}slice.log') for slices in (2, 4)
         for case in ('bop-closed-loop', 'bop-closed-loop-uc')]
summary = re.compile(r'^(CASE .*actual|COUNTS total|TRAIN_FIRE count|EVICTBACK_COUNTS|'
                     r'SLICE_EVICTBACK_FIRE|SEVICT_COUNTS|BOP_ISSUE_OFF_BASELINE|'
                     r'BOP_2C_PHASE|BOP_2C_RESULT|BOP_PREFETCH_SMOKE_RESULT|'
                     r'BOP_CLOSED_LOOP_BASE_EXPECTED|BOP_CLOSED_LOOP_RESULT|'
                     r'BOP_COLLISION_EXPECTED|BOP_COLLISION_RESULT).*$', re.M)
head = subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip()
source_diff = subprocess.check_output(['git', 'diff', 'HEAD', '--', 'src/main', 'src/test'])
source_sha = hashlib.sha256(source_diff).hexdigest()
empty_sha = hashlib.sha256(b'').hexdigest()
timestamps = set()
results = {}
for stage, name in logs:
    path = current / stage / name
    data = path.read_text()
    assert data.splitlines()[0] == head, f'HEAD mismatch: {path}'
    expected_sha = empty_sha if stage == 'A' else source_sha
    assert re.search(rf'^{expected_sha}  -$', data, re.M), f'source fingerprint mismatch: {path}'
    assert data.endswith('EXIT=0\n'), f'incomplete/failed: {path}'
    before = set(data.split('CHISELSIM_BEFORE\n', 1)[1].split('CHISELSIM_AFTER\n', 1)[0].splitlines())
    after = set(data.split('CHISELSIM_AFTER\n', 1)[1].split('EXIT=', 1)[0].splitlines())
    generated = after - before
    assert len(generated) == 1, f'not one new model: {path}'
    timestamp = generated.pop()
    assert timestamp not in timestamps, f'reused model: {path}'
    timestamps.add(timestamp)
    result = [m.group(0) for m in summary.finditer(data)]
    old = [m.group(0) for m in summary.finditer((previous / stage / name).read_text())]
    assert result and result == old, f'result differs from original 2d: {path}'
    results[(stage, name)] = result
    print(f'VERIFIED {stage}/{name} timestamp={timestamp} EXIT=0 original=identical')
for name in baseline:
    assert results[('A', name)] == results[('B', name)], f'A/B mismatch: {name}'
matrix = (current / 'new/bop-2c-all-1slice.log').read_text()
assert len(re.findall(r'^BOP_2C_RESULT pass=true ', matrix, re.M)) == 12
assert len(re.findall(r'^BOP_2C_PHASE pass=true ', matrix, re.M)) == 46
print(f'HEAD={head} B_SOURCE_DIFF_SHA256={source_sha}')
print('AB_ALL_COUNTS_IDENTICAL groups=6 models=27 scenarios=38 phase_assertions=46 ORIGINAL_RESULTS_IDENTICAL')
PY
