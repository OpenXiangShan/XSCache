#!/usr/bin/env python3
"""Intent (C0): seal 8a99c46 bytecode before adding CDP definitions, then
compare every existing regression result in two modes and every directed test.
All scripts/logs/temp files and generated RTL stay on the local NVMe.
"""
from pathlib import Path
import concurrent.futures
import importlib.util
import json
import os
import re
import subprocess
import sys

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
BASE = '8a99c4684ccab046573667fca6d0fd3252591ac4'
LOCAL = Path('/data/zhuy/pf-framework/c0')
action, stage = sys.argv[1:3]
DEST = LOCAL / stage
DEST.mkdir(parents=True, exist_ok=True)
spec = importlib.util.spec_from_file_location('helper', ROOT / 'scripts/run-pf-1c2.py')
h = importlib.util.module_from_spec(spec)
spec.loader.exec_module(h)
h.DEST = DEST
tmp = LOCAL / 'tmp'
tmp.mkdir(exist_ok=True)
h.ENV.update(CCACHE_DISABLE='1', TMPDIR=str(tmp), JAVA_TOOL_OPTIONS='-Djava.io.tmpdir=' + str(tmp))

def bytecode():
    base = ROOT / 'out/XSCache'
    return {str(p.relative_to(base)): h.sha(p.read_bytes())
            for d in ('compile.dest/classes', 'test/compile.dest/classes', 'testtop/oceanus/compile.dest/classes')
            for p in (base / d).rglob('*.class')}

def java(entry, args=(), props=()):
    return ['java', '-Xmx6g', *props, '-cp', h.classpath(), entry, *args]

def plan():
    # Keep the accepted 37-case plan; reproduce all later directed coverage too.
    suite = json.loads((LOCAL / 'baseline/suite-plan.json').read_text())
    jobs = []
    for mode in ('off', 'on'):
        for name, entry, args in suite:
            jobs.append((java(entry, args, ['-Doceanus.phase1a.ports=' + str(mode == 'on').lower()]), mode + '/' + name))
    jobs += [(java('oceanus.TestTop_L2PrefetchTlb'), 'tlb.log'),
             (java('oceanus.TestTop_L2CDPQueue'), 'cdp-queue.log'),
             (java('oceanus.TestTop_L2PrefetchFairness'), 'fairness.log'),
             (java('oceanus.TestTop_L2TrainArbitration'), 'train-arbitration.log')]
    for n in (2, 4):
        props = ['-Doceanus.phase1a.ports=true', '-Doceanus.phase1b.drive=true', '-Doceanus.phase1c2.check=true']
        jobs += [(java('oceanus.TestTop_L2Train', ['--slices=' + str(n)], props), f'train-{n}slice.log'),
                 (java('oceanus.TestTop_L2Train', ['--slices=' + str(n), '--mixed-enable'], props), f'mixed-train-{n}.log')]
    jobs.append((java('oceanus.TestTop_L2CDPDepth', props=['-Doceanus.phase1a.ports=true', '-Doceanus.phase1c2.check=true', '-Doceanus.phase1c2.depth=true']), 'cdp-depth.log'))
    # The legacy parameter main writes RTL to a fixed path. Run these in order
    # in each suite and preserve a stage-specific copy immediately afterwards.
    for mode in ('off', 'train-only', 'tlb-only', 'none', 'vbop', 'cdp', 'both'):
        jobs.append((java('oceanus.TestTop_L2StubParameters', [mode]), 'parameters-' + mode + '.log'))
    for fast in (False, True):
        jobs.append((java('oceanus.TestTop_L2PrefetchPhase2', [] if fast else ['--no-fast-path'], ['-Doceanus.phase1a.ports=true', '-Doceanus.phase2=true']), f'phase2-{fast}.log'))
    for defaults in (False, True):
        jobs.append((java('oceanus.TestTop_OceanusVBOP', ['--samples=10', *(['--defaults'] if defaults else [])]), f'vbop-{defaults}.log'))
    for seed in (1, 6, 11, 16):
        jobs.append((java('oceanus.TestTop_L2PrefetchDrain', ['--slices=1', '--seed=' + str(seed), '--cycles=5000'], ['-Doceanus.phase1a.ports=true']), f'seed{seed}.log'))
    assert len(jobs) == 98
    return jobs

def run_job(cmd, name):
    h.run(cmd, name)
    if name.startswith('parameters-'):
        source = Path('/data/zhuy/pf-framework/audit-fix-1/rtl') / name.replace('.log', '.sv')
        target = DEST / 'rtl' / source.name
        target.parent.mkdir(exist_ok=True)
        target.write_bytes(source.read_bytes())

def verify_logs(folder):
    result = {}
    fp = json.loads((folder / 'compiled-sources.json').read_text())
    for path in folder.rglob('*.log'):
        body = h.checked(path)
        source = json.loads(Path(str(path) + '.sources.json').read_text())
        # Intent: baseline JVMs run sealed old bytecode while sources may be
        # edited. The per-invocation fingerprint is truthful; compiled-sources
        # and the invariant bytecode manifest identify the executed design.
        if folder.name != 'baseline':
            assert source['sources'] == fp['sources'], path
        assert body.startswith('HEAD=' + source['head'] + '\n'), path
        result[str(path.relative_to(folder))] = dict(bytes=path.stat().st_size, sha256=h.sha(path.read_bytes()))
    return result

if action == 'compile':
    if stage == 'baseline':
        assert h.git('rev-parse', 'HEAD') == BASE
        for name in h.git('ls-files', 'src/main', 'src/test').splitlines():
            assert (ROOT / name).read_bytes() == subprocess.check_output(['git', 'show', BASE + ':' + name], cwd=ROOT), name
        h.dump('suite-plan.json', json.loads(Path('/data/zhuy/pf-framework/audit-fix-1/final/suite-plan.json').read_text()))
    h.dump('reference-before.json', h.reference())
    for target in h.TARGETS:
        h.run(['mill', '--no-server', target], target + '.log')
    h.dump('compiled-sources.json', h.fingerprint())
    h.dump('compiled-bytecode.json', bytecode())
elif action == 'suite':
    with concurrent.futures.ThreadPoolExecutor(max_workers=int(os.environ.get('PF_REGRESSION_JOBS', '12'))) as pool:
        futures = [pool.submit(run_job, *job) for job in plan()]
        for future in futures:
            future.result()
    assert bytecode() == json.loads((DEST / 'compiled-bytecode.json').read_text())
    h.dump('log-manifest.json', verify_logs(DEST))
    h.dump('sealed.json', dict(HEAD=BASE, logs=101, bytecode_unchanged=True))
elif action == 'helpers':
    run_job(java('oceanus.TestTop_OceanusCDP'), 'cdp-c0.log')
elif action == 'seal-baseline':
    # Intent: seal completed baseline runs without rerunning them; the initial
    # verifier incorrectly equated launch-time edited source with old bytecode.
    assert stage == 'baseline'
    assert bytecode() == json.loads((DEST / 'compiled-bytecode.json').read_text())
    for _, name in plan():
        h.checked(DEST / name)
    h.dump('log-manifest.json', verify_logs(DEST))
    h.dump('sealed.json', dict(HEAD=BASE, logs=101, bytecode_unchanged=True))
elif action == 'verify':
    assert h.fingerprint()['sources'] == json.loads((DEST / 'compiled-sources.json').read_text())['sources']
    assert bytecode() == json.loads((DEST / 'compiled-bytecode.json').read_text())
    assert h.reference() == json.loads((LOCAL / 'baseline/reference-before.json').read_text())
    assert not h.git('diff', BASE, '--', 'src/main/scala/coupledL2')
    text = (ROOT / 'src/test/scala/oceanus/ReferenceCDP.scala').read_text()
    copied = text.split('// BEGIN VERBATIM CDP.scala:35-1580\n', 1)[1].split('// END VERBATIM CDP.scala:35-1580', 1)[0]
    original = ''.join(Path('/data/zhuy/coupledL2/prefetch/CDP.scala').read_text().splitlines(keepends=True)[34:1580])
    assert copied == original + '\n', 'ReferenceCDP algorithm text changed'
    h.dump('reference-audit.json', dict(range='CDP.scala:35-1580', sha256=h.sha(original.encode()), verbatim=True))
    comparisons = {}
    directed = re.compile(r'^(?:TLB_[A-Z_]+|CDP_[A-Z_]+|VA_[A-Z_]+|TRAIN_[A-Z_]+|PHASE[23][A-Z0-9_]*|VBOP_[A-Z_]+|DRAIN_[A-Z_]+).*$', re.M)
    for _, name in plan():
        now, old = h.checked(DEST / name), h.checked(LOCAL / 'baseline' / name)
        rows = h.results(DEST / name) if name.startswith(('off/', 'on/')) else directed.findall(now)
        previous = h.results(LOCAL / 'baseline' / name) if name.startswith(('off/', 'on/')) else directed.findall(old)
        # Legacy RTL diagnostics contain their output path (identical here).
        assert rows and rows == previous, name
        comparisons[name] = dict(identical=True, current=rows, baseline=previous)
    h.dump('comparison.json', comparisons)
    body = h.checked(DEST / 'cdp-c0.log')
    assert 'CDP_C0_RESULT pass=true' in body
    h.dump('log-manifest.json', verify_logs(DEST))
    h.dump('accepted-sources.json', h.fingerprint())
    print('C0_ACCEPTED compile=3 off=37/37 on=37/37 directed=24/24 helper=true reference_unchanged=true fork_diff=0')
else:
    raise ValueError(action)
