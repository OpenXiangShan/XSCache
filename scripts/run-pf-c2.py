#!/usr/bin/env python3
"""Intent (C2): seal the Filter implementation, compare against the read-only
reference and the fca38f5 74-log baseline, and retain reproducible local evidence.
Usage: run-pf-c2.py compile|diff|suite|inherited|verify STAGE [diff arguments]
verify STAGE [REGRESSION_STAGE] can reuse sealed legacy regression evidence
when only CDP test harnesses changed and all other bytecode is identical.
Only the three approved Mill compile targets are used. No /tmp output.
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
BASE = 'fca38f57e1e2b269cb02d55c8c18a16d0947648b'
LOCAL = Path('/data/zhuy/pf-framework/c2')
BASELINE = Path('/data/zhuy/pf-framework/overnight-20261022/task1')
action, stage = sys.argv[1:3]
DEST = LOCAL / stage
REGRESSION = LOCAL / sys.argv[3] if action == 'verify' and len(sys.argv) > 3 else DEST
DEST.mkdir(parents=True, exist_ok=True)
spec = importlib.util.spec_from_file_location('helper', ROOT / 'scripts/run-pf-1c2.py')
h = importlib.util.module_from_spec(spec)
spec.loader.exec_module(h)
h.DEST = DEST
scratch = LOCAL / 'scratch'
scratch.mkdir(exist_ok=True)
h.ENV.update(CCACHE_DISABLE='1', TMPDIR=str(scratch),
             JAVA_TOOL_OPTIONS='-Djava.io.tmpdir=' + str(scratch))


def bytecode():
    base = ROOT / 'out/XSCache'
    return {str(p.relative_to(base)): h.sha(p.read_bytes())
            for d in ('compile.dest/classes', 'test/compile.dest/classes', 'testtop/oceanus/compile.dest/classes')
            for p in (base / d).rglob('*.class')}


def java(entry, args=(), props=()):
    return ['java', '-Xmx6g', *props, '-cp', h.classpath(), entry, *args]


def plan():
    suite = json.loads(Path('/data/zhuy/pf-framework/c0/baseline/suite-plan.json').read_text())
    assert len(suite) == 37
    return [(java(entry, args, ['-Doceanus.phase1a.ports=' + str(mode == 'on').lower()]), mode + '/' + name)
            for mode in ('off', 'on') for name, entry, args in suite]


def reference_audit():
    original = Path('/data/zhuy/coupledL2/prefetch/CDP.scala').read_text().splitlines(keepends=True)
    text = (ROOT / 'src/test/scala/oceanus/ReferenceCDP.scala').read_text()
    copied = text.split('// BEGIN VERBATIM CDP.scala:35-1580\n', 1)[1].split('// END VERBATIM CDP.scala:35-1580', 1)[0]
    assert copied == ''.join(original[34:1580]) + '\n'
    assert h.reference() == json.loads((BASELINE / 'reference-before.json').read_text())
    assert not h.git('diff', BASE, '--', 'src/main/scala/coupledL2')
    h.dump('reference-audit.json', dict(verbatim=True, range='CDP.scala:35-1580',
        sha256=h.sha(copied.encode()), reference_unchanged=True, fork_diff=0))
    return original


def migration_audit():
    original = reference_audit()
    expected = ''.join(original[466:551]) + '\n' + ''.join(original[565:693])
    for name in ('FilterTable', 'FtTrainPipeline', 'FtQueryReq', 'FtQueryRsp', 'FtTrainReq'):
        expected = re.sub(r'\b' + name + r'\b', 'OceanusCDP' + name, expected)
    for old, new in [('CDPModule', 'OceanusCDPModule'), ('PrefetchTrain', 'OceanusCDPTrain')]:
        expected = re.sub(r'\b' + old + r'\b', new, expected)
    for old, new in [('cacheParams.hasMbist', 'false'), ('cacheParams.hasSramCtl', 'false'),
                     ('trainTrigger.bits.cdp_filter_train_evict.get', 'trainTrigger.bits.isEvict'),
                     ('trainTrigger.bits.cdp_filter_train_hit.get', 'trainTrigger.bits.isUsed'),
                     ('trainTrigger.bits.evict_addr', 'trainTrigger.bits.evictPaddr'),
                     ('trainTrigger.bits.addr', 'trainTrigger.bits.paddr')]:
        expected = expected.replace(old, new)
    production = (ROOT / 'src/main/scala/oceanus/l2/OceanusCDP.scala').read_text()
    copied = 'class OceanusCDPFilterTable' + production.split('class OceanusCDPFilterTable', 1)[1].split('// C2: reference :1478', 1)[0]
    clean = lambda s: '\n'.join(line.rstrip() for line in s.splitlines()).strip()
    assert clean(copied) == clean(expected), 'unregistered Filter/FtTrain source edit'
    # The completed C1 source is frozen by the preceding accepted commit.
    before = subprocess.check_output(['git', 'show', BASE + ':src/main/scala/oceanus/l2/OceanusCDP.scala'], cwd=ROOT, text=True)
    c1 = lambda s: s.split('class OceanusCDPVpnTable', 1)[1].split('// C2: expression-equivalent', 1)[0].split('\nobject OceanusCDP {', 1)[0].strip()
    assert c1(production) == c1(before), 'C1 algorithm changed'
    h.dump('migration-audit.json', dict(expression_equivalent=True, c1_unchanged=True,
        ranges=['CDP.scala:467-551', 'CDP.scala:566-693'], adapted_sha256=h.sha(clean(copied).encode()),
        adaptations=['local type names', 'full PA and mandatory used/evict flags',
                     'standalone MBIST/SRAM control false', 'trailing whitespace']))


def baseline_audit():
    baseline = json.loads((BASELINE / 'sources.json').read_text())
    assert baseline['head'] == BASE
    for name, digest in baseline['sources'].items():
        assert h.sha(subprocess.check_output(['git', 'show', BASE + ':' + name], cwd=ROOT)) == digest, name
    assert json.loads((BASELINE / 'baseline-identity.json').read_text())['worktree_clean']
    h.dump('baseline-identity.json', dict(commit=BASE, source_files=len(baseline['sources']),
        exact_committed_sources=True, baseline=str(BASELINE)))


def sealed():
    assert h.fingerprint()['sources'] == json.loads((DEST / 'compiled-sources.json').read_text())['sources']
    assert bytecode() == json.loads((DEST / 'compiled-bytecode.json').read_text())


def class_references(data):
    """Read class constants, including descriptors, without loading classes."""
    assert data[:4] == bytes.fromhex('cafebabe')
    size, offset, index = int.from_bytes(data[8:10], 'big'), 10, 1
    strings = []
    fixed = {3: 4, 4: 4, 5: 8, 6: 8, 7: 2, 8: 2, 9: 4, 10: 4,
             11: 4, 12: 4, 15: 3, 16: 2, 17: 4, 18: 4, 19: 2, 20: 2}
    while index < size:
        tag = data[offset]
        offset += 1
        if tag == 1:
            length = int.from_bytes(data[offset:offset+2], 'big')
            offset += 2
            strings.append(data[offset:offset+length].decode('utf-8', errors='replace'))
            offset += length
        else:
            offset += fixed[tag]
            if tag in (5, 6):
                index += 1
        index += 1
    # Exact class names are UTF8 constants; L...; descriptors cover arrays and
    # type-only references. Extra string matches conservatively add edges.
    return set(strings) | {n for s in strings for n in re.findall(r'L([^;<>]+);', s)}


def regression_identity():
    """Keep both original execution snapshots; never relabel an old log."""
    if REGRESSION == DEST:
        return
    old = json.loads((REGRESSION / 'compiled-sources.json').read_text())['sources']
    new = h.fingerprint()['sources']
    changed_sources = {n for n in old.keys() | new.keys() if old.get(n) != new.get(n)}
    assert changed_sources == {
        'src/test/scala/oceanus/TestTop_OceanusCDP.scala',
        'src/test/scala/oceanus/TestTop_OceanusCDPC1.scala',
        'src/test/scala/oceanus/TestTop_OceanusCDPC2.scala',
        'scripts/run-pf-c2.py'}, changed_sources
    old_classes = json.loads((REGRESSION / 'compiled-bytecode.json').read_text())
    new_classes = bytecode()
    changed_classes = sorted(n for n in old_classes.keys() | new_classes.keys()
                             if old_classes.get(n) != new_classes.get(n))
    assert changed_classes
    allowed = re.compile(r'^testtop/oceanus/compile.dest/classes/oceanus/(?:TestTop_OceanusCDP(?:\$.*)?|CDPC1Harness(?:\$.*)?|TestTop_OceanusCDPC1(?:\$.*)?|CDPC1HostQueue(?:\$.*)?|TestTop_OceanusCDPC2(?:\$.*)?)\.class$')
    assert all(allowed.fullmatch(n) for n in changed_classes), changed_classes
    changed_names = {n.split('/classes/', 1)[1][:-6] for n in changed_classes}
    paths = {n.split('/classes/', 1)[1][:-6]: ROOT / 'out/XSCache' / n for n in new_classes}
    suite = json.loads(Path('/data/zhuy/pf-framework/c0/baseline/suite-plan.json').read_text())
    pending = [entry.replace('.', '/') for _, entry, _ in suite]
    visited = set()
    while pending:
        name = pending.pop()
        if name in visited or name not in paths:
            continue
        assert name not in changed_names, 'legacy entry depends on changed test class: ' + name
        visited.add(name)
        pending.extend(class_references(paths[name].read_bytes()) - visited)
    h.dump('regression-identity.json', dict(original_execution=str(REGRESSION),
        changed_sources=sorted(changed_sources), changed_classes=changed_classes,
        all_production_and_reference_sources_identical=True,
        legacy_test_sources_identical=True,
        legacy_dependency_classes_checked=len(visited),
        legacy_dependencies_unchanged=True,
        all_other_bytecode_identical=True))


if action == 'compile':
    migration_audit()
    baseline_audit()
    h.dump('reference-before.json', h.reference())
    for target in h.TARGETS:
        h.run(['mill', '--no-server', target], target + '.log')
    h.dump('compiled-sources.json', h.fingerprint())
    h.dump('compiled-bytecode.json', bytecode())
elif action == 'diff':
    sealed()
    h.run(java('oceanus.TestTop_OceanusCDPC2', sys.argv[3:],
        ['-Doceanus.c2.trace=' + str(DEST / 'traces'), '-Doceanus.c2.head=' + h.git('rev-parse', 'HEAD')]), 'cdp-c2.log')
    sealed()
elif action == 'suite':
    sealed()
    # Leave room for the verification agents and one simultaneous C2 diff.
    workers = min(8, max(1, os.cpu_count() // 6))
    h.dump('suite-plan.json', dict(workers=workers, jobs=[name for _, name in plan()]))
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        futures = [pool.submit(h.run, *job) for job in plan()]
        for future in futures:
            future.result()
    sealed()
elif action == 'inherited':
    sealed()
    h.run(java('oceanus.TestTop_OceanusCDP'), 'cdp-c0.log')
    h.run(java('oceanus.TestTop_OceanusCDPC1', ['--seeds=500'],
        ['-Doceanus.c1.trace=' + str(DEST / 'c1-traces'), '-Doceanus.c1.head=' + h.git('rev-parse', 'HEAD')]), 'cdp-c1.log')
    sealed()
elif action == 'verify':
    sealed()
    regression_identity()
    migration_audit()
    baseline_audit()
    for target in h.TARGETS:
        h.checked(DEST / (target + '.log'))
    comparisons = {}
    for _, name in plan():
        current, previous = h.results(REGRESSION / name), h.results(BASELINE / name)
        assert current == previous, name
        comparisons[name] = dict(identical=True, current=current, baseline=previous)
    h.dump('comparison.json', comparisons)
    body = h.checked(DEST / 'cdp-c2.log')
    result = re.search(r'CDP_C2_RESULT pass=true cycles=(\d+) matched=(\d+) seeds=(\d+) mismatches=0 host_model=true', body)
    assert result and result[1] == result[2] and int(result[3]) >= 1000
    cases = {n: dict(count=int(c), matched=int(m)) for n, c, m in
             re.findall(r'CDP_C2_CASE name=(\S+) count=(\d+) matched=(\d+)', body)}
    for name in ('reset-block', 'evict-allocate', 'used-miss-no-allocation', 'both-hit-decrement',
                 'both-miss-init1', 'both-hit-positive-decrement', 'both-flags-same-entry-positive-decrement',
                 'saturate-zero', 'saturate-three', 'same-entry-different-slot',
                 'replacement', 'write-vs-send', 'write-vs-train-read', 'response-during-write',
                 'dual-query-contention', 'train-response', 'send-response', 'queue-drop',
                 'actual-top-filter-check', 'filter-disabled-silent-check', 'sram-latency-check',
                 'query-owner-check', 'sat-bank-boundary', 'same-4KB-page', 'full-set-scan'):
        assert cases[name]['count'] > 0 and cases[name]['count'] == cases[name]['matched'], name
    assert cases['random-seeds']['count'] == int(result[3])
    h.dump('differential-results.json', dict(cycles=int(result[1]), matched=int(result[2]), seeds=int(result[3]), cases=cases))
    traces = {}
    for path in sorted((DEST / 'traces').glob('*.jsonl')):
        content = path.read_bytes()
        lines = content.splitlines()
        metadata = json.loads(lines[0])
        assert metadata['head'] == json.loads((DEST / 'compiled-sources.json').read_text())['head']
        traces[path.name] = dict(bytes=len(content), sha256=h.sha(content), cycles=len(lines)-1, metadata=metadata)
    assert sum(t['cycles'] for t in traces.values()) == int(result[1])
    assert sum(n.startswith('seed') for n in traces) == int(result[3])
    h.dump('trace-manifest.json', traces)
    assert 'CDP_C0_RESULT pass=true' in h.checked(DEST / 'cdp-c0.log')
    assert 'seeds=500 mismatches=0 host_table_model=true' in h.checked(DEST / 'cdp-c1.log')
    manifest = {}
    selected = [(DEST / (t + '.log'), DEST) for t in h.TARGETS]
    selected += [(DEST / n, DEST) for n in ('cdp-c0.log', 'cdp-c1.log', 'cdp-c2.log')]
    selected += [(REGRESSION / n, REGRESSION) for _, n in plan()]
    for path, seal in selected:
        fp = json.loads((seal / 'compiled-sources.json').read_text())
        body = h.checked(path)
        source = json.loads(Path(str(path) + '.sources.json').read_text())
        assert source['sources'] == fp['sources'] and source['head'] == fp['head'], path
        assert body.startswith('HEAD=' + source['head'] + '\n'), path
        manifest[str(path.relative_to(LOCAL))] = dict(bytes=path.stat().st_size, sha256=h.sha(path.read_bytes()))
    h.dump('log-manifest.json', manifest)
    h.dump('accepted-sources.json', h.fingerprint())
    print('C2_ACCEPTED compile=3 off=37/37 on=37/37 differential_seeds=' + result[3] +
          ' c0=true c1=true reference_unchanged=true fork_diff=0', flush=True)
else:
    raise ValueError(action)
