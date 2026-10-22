#!/usr/bin/env python3
"""Intent (C1): seal 320b5a4 bytecode before implementing VPN training, then
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
BASE = '320b5a4a6c550a4ecb346a0e750dbe45107464eb'
LOCAL = Path('/data/zhuy/pf-framework/c1')
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
    suite = json.loads(Path('/data/zhuy/pf-framework/c0/baseline/suite-plan.json').read_text())
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
    # Each legacy parameter mode writes its own fixed filename. Preserve a
    # stage-specific copy immediately after that mode finishes.
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
        # Each stage freezes its sources until all JVMs and checks finish.
        assert source['sources'] == fp['sources'], path
        assert body.startswith('HEAD=' + source['head'] + '\n'), path
        result[str(path.relative_to(folder))] = dict(bytes=path.stat().st_size, sha256=h.sha(path.read_bytes()))
    return result

def audit_migration():
    original = Path('/data/zhuy/coupledL2/prefetch/CDP.scala').read_text().splitlines(keepends=True)
    expected = ''.join(original[289:448]) + '\n' + ''.join(original[694:867])
    for old, new in [('VpnTableMetaInfo', 'OceanusCDPVpnMeta'), ('VtTrainPipeline', 'OceanusCDPVtTrainPipeline'),
                     ('VpnTable', 'OceanusCDPVpnTable'), ('VtQueryReq', 'OceanusCDPVtQueryReq'),
                     ('VtQueryRsp', 'OceanusCDPVtQueryRsp'), ('VtTrainReq', 'OceanusCDPVtTrainReq'),
                     ('PrefetchTrain', 'OceanusCDPTrain'), ('CDPModule', 'OceanusCDPModule'), ('CDPBundle', 'OceanusCDPBundle')]:
        expected = re.sub(r'\b' + re.escape(old) + r'\b', new, expected)
    expected = expected.replace('trainTrigger.bits.vaddr.getOrElse(0.U)', 'trainTrigger.bits.vaddr')
    expected = expected.replace('trainTrigger.bits.pfsource === PfSource.CDP.id.U', 'trainTrigger.bits.pfSource === L2PfSource.CDP.U')
    a = expected.index('  val maxAddrWidth = if (trainTrigger.bits.vaddr.isDefined)')
    b = expected.index('\n\n  val nextState', a)
    expected = expected[:a] + '''  // Intent (C1): the local event always carries block VA[49:6]; unlike
  // the reference optional field there is no absent-VA elaboration branch.
  val maxAddrWidth = trainTrigger.bits.vaddr.getWidth + log2Ceil(blockBytes)''' + expected[b:]
    expected = expected.replace('ChiselDB.createTable("cdpTrain"', 'ChiselDB.createTable("oceanusCDPTrain"')
    production = (ROOT / 'src/main/scala/oceanus/l2/OceanusCDP.scala').read_text()
    copied = 'class OceanusCDPVpnTable' + production.split('class OceanusCDPVpnTable', 1)[1].split('\nobject OceanusCDP {', 1)[0]
    expected = '\n'.join(line.rstrip() for line in expected.split('\n'))
    assert copied.strip() == expected.strip(), 'C1 migration has an unregistered source edit'
    h.dump('migration-audit.json', dict(ranges=['CDP.scala:290-448', 'CDP.scala:695-867'],
        original_sha256=h.sha((''.join(original[289:448])+''.join(original[694:867])).encode()),
        adapted_sha256=h.sha(copied.strip().encode()), expression_equivalent=True,
        adaptations=['local type prefixes', 'mandatory block VA instead of optional field',
                     'pfSource local spelling and source9 constant', 'debug table name oceanusCDPTrain', 'trailing whitespace removed']))

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
    with concurrent.futures.ThreadPoolExecutor(max_workers=min(int(os.environ.get('PF_REGRESSION_JOBS', '8')), max(1, len(os.sched_getaffinity(0)) // 24))) as pool:
        futures = [pool.submit(run_job, *job) for job in plan()]
        for future in futures:
            future.result()
    assert bytecode() == json.loads((DEST / 'compiled-bytecode.json').read_text())
    h.dump('log-manifest.json', verify_logs(DEST))
    h.dump('sealed.json', dict(HEAD=h.git('rev-parse', 'HEAD'), logs=len(list(DEST.rglob('*.log'))), bytecode_unchanged=True))
elif action == 'diff':
    run_job(java('oceanus.TestTop_OceanusCDPC1', sys.argv[3:], ['-Doceanus.c1.trace=' + str(DEST / 'traces'), '-Doceanus.c1.head=' + h.git('rev-parse', 'HEAD')]), 'cdp-c1.log')
elif action == 'helpers':
    run_job(java('oceanus.TestTop_OceanusCDP'), 'cdp-c0.log')
elif action == 'verify':
    audit_migration()
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
    diff = h.checked(DEST / 'cdp-c1.log')
    result = re.search(r'CDP_C1_RESULT pass=true cycles=(\d+) matched=(\d+) seeds=(\d+) mismatches=0 host_table_model=true', diff)
    assert result and result[1] == result[2] and int(result[3]) >= 500
    cases = {name: dict(count=int(count), matched=int(matched)) for name, count, matched in
             re.findall(r'CDP_C1_CASE name=(\S+) count=(\d+) matched=(\d+)', diff)}
    for name in ('plus1', 'plus4', 'refresh', 'refresh-overlap', 'wrap', 'alloc-main', 'alloc-sub', 'replacement', 'same-main-stall', 'queue-drop', 'pipeline-refresh-overlap', 'hot-true', 'hot-false'):
        assert cases[name]['count'] > 0 and cases[name]['count'] == cases[name]['matched'], name
    assert cases['random-seeds']['count'] >= 500
    h.dump('differential-results.json', dict(cycles=int(result[1]), matched=int(result[2]), seeds=int(result[3]), cases=cases))
    trace_manifest = {}
    trace_cycles = 0
    for path in sorted((DEST / 'traces').glob('*.jsonl')):
        content = path.read_bytes()
        lines = content.splitlines()
        metadata = json.loads(lines[0])
        assert metadata['head'] == json.loads((DEST / 'compiled-sources.json').read_text())['head']
        trace_cycles += len(lines)-1
        trace_manifest[path.name] = dict(bytes=len(content), sha256=h.sha(content), cycles=len(lines)-1, metadata=metadata)
    assert trace_cycles == int(result[1])
    assert sum('-seed' in name for name in trace_manifest) >= 500
    h.dump('trace-manifest.json', trace_manifest)
    # Independently identify baseline execution as the requested commit.
    baseline = json.loads((LOCAL / 'baseline/compiled-sources.json').read_text())
    for name in h.git('ls-files', 'src/main', 'src/test').splitlines():
        if name in baseline['sources']:
            assert h.sha(subprocess.check_output(['git', 'show', BASE + ':' + name], cwd=ROOT)) == baseline['sources'][name], name
    h.dump('baseline-identity.json', dict(commit=BASE, compiled_sources_match=True))
    h.dump('accepted-sources.json', h.fingerprint())
    print('C1_ACCEPTED compile=3 off=37/37 on=37/37 directed=24/24 helper=true differential=true reference_unchanged=true fork_diff=0')
else:
    raise ValueError(action)
