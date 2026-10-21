#!/usr/bin/env python3
"""Intent (3a, before RTL edits): seal a freshly compiled 962c07b baseline;
compare all 37 regressions in each parameter group and the five old directed
tests. Keep the reference snapshot immutable, fingerprint every invocation,
and use only the three authorized Mill compile targets. All output is local.
"""
from pathlib import Path
import concurrent.futures, importlib.util, json, os, re, sys
sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('phase2', ROOT / 'scripts/run-pf-phase2.py')
p2 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(p2)
p = p2.previous
DEST = Path(sys.argv[2] if len(sys.argv) > 2 else '/data/zhuy/pf-framework/pf-3a-20261021/final')
BASE = Path('/data/zhuy/pf-framework/pf-3a-20261021/baseline')
HIST = Path('/data/zhuy/pf-framework/pf-phase2-20261021/final')
HEAD = '962c07beb6c400ec458eb735a06ff7ea7bf2baa0'
p.DEST = DEST

def parallel(jobs):
    with concurrent.futures.ThreadPoolExecutor(max_workers=int(os.environ.get('PF_REGRESSION_JOBS', '10'))) as pool:
        futures = [pool.submit(p.run, *job) for job in jobs]
        for f in futures: f.result()

if __name__ == '__main__':
    DEST.mkdir(parents=True, exist_ok=True)
    action = sys.argv[1]
    if action == 'init':
        p.dump('suite-plan.json', json.loads((HIST / 'suite-plan.json').read_text()))
        p.dump('reference-before.json', p.reference())
        p.dump('initial-sources.json', p.fingerprint())
    elif action == 'compile':
        for target in p.TARGETS: p.run(['mill', '--no-server', target], target + '.log')
        p.dump('compiled-bytecode.json', p2.bytecode())
        p.dump('compiled-sources.json', p.fingerprint())
    elif action == 'suite':
        cp = p.classpath()
        jobs = []
        for mode in ('off', 'on'):
            for name, entry, args in json.loads((DEST / 'suite-plan.json').read_text()):
                jobs.append((['java', '-Doceanus.phase1a.ports=' + str(mode == 'on').lower(),
                              '-cp', cp, entry, *args], mode + '/' + name))
        jobs.extend(p2.old_directed())
        parallel(jobs)
    elif action == 'seal-baseline':
        assert DEST == BASE and p.git('rev-parse', 'HEAD') == HEAD
        assert p2.bytecode() == json.loads((DEST / 'compiled-bytecode.json').read_text())
        for mode in ('off', 'on'):
            for name, _, _ in json.loads((DEST / 'suite-plan.json').read_text()):
                assert p.results(DEST / mode / name) == p.results(HIST / mode / name), (mode, name)
        p.dump('baseline-sealed.json', dict(head=HEAD, off=37, on=37, bytecode_unchanged=True))
        print('BASELINE_SEALED head=' + HEAD + ' off=37/37 on=37/37')
    elif action in ('directed', 'differential'):
        cp = p.classpath()
        jobs = [(['java', '-Doceanus.phase1a.ports=true', '-Doceanus.phase2=true', '-cp', cp,
                  'oceanus.TestTop_L2PrefetchPhase2'], 'phase2-real-vbop.log'),
                (['java', '-cp', cp, 'oceanus.TestTop_OceanusVBOP'], 'vbop-differential.log')]
        jobs.append((['java', '-cp', cp, 'oceanus.TestTop_OceanusVBOP', '--defaults'], 'vbop-defaults.log'))
        jobs.append((['java', '-Doceanus.phase1a.ports=true', '-Doceanus.phase2=true', '-cp', cp,
                      'oceanus.TestTop_L2PrefetchPhase2', '--no-fast-path'], 'phase2-default-vbop.log'))
        for mode in ('none', 'vbop', 'cdp', 'both'):
            jobs.append((['java', '-cp', cp, 'oceanus.TestTop_L2StubParameters', mode], 'parameters-' + mode + '.log'))
        if action == 'differential':
            jobs = [job for job in jobs if job[1] in ('vbop-differential.log', 'vbop-defaults.log')]
        parallel(jobs)
    elif action == 'verify':
        assert json.loads((BASE / 'baseline-sealed.json').read_text())['bytecode_unchanged']
        assert p.reference() == json.loads((BASE / 'reference-before.json').read_text())
        assert not p.git('diff', HEAD, '--', 'src/main/scala/coupledL2')
        # Test oracle audit: the complete algorithm body is verbatim reference,
        # after the standalone-only imports/configuration/bundle adapters.
        original = Path('/data/zhuy/coupledL2/prefetch/BestOffsetPrefetch.scala').read_text().splitlines(True)
        assert (ROOT / 'src/test/scala/oceanus/ReferenceVBOP.scala').read_text().endswith(''.join(original[121:794]))
        fp = p.fingerprint()
        assert fp['sources'] == json.loads((DEST / 'compiled-sources.json').read_text())['sources']
        comparisons = {}
        for mode in ('off', 'on'):
            comparisons[mode] = []
            for name, _, _ in json.loads((DEST / 'suite-plan.json').read_text()):
                a, b = p.results(DEST / mode / name), p.results(BASE / mode / name)
                assert a == b, (mode, name, a, b)
                comparisons[mode].append(dict(log=name, current=a, baseline=b))
        extra = re.compile(r'^(?:TLB_CASE|TLB_RESULT|CDP_[A-Z_]+|VA_[A-Z_]+|TRAIN_GATE_RESULT).*$', re.M)
        for _, name in p2.old_directed():
            a, b = (extra.findall(p.checked(d / name)) for d in (DEST, BASE))
            assert a and a == b, (name, a, b)
        for name, marker in [('phase2-real-vbop.log', 'PHASE2_RESULT pass=true'),
                             ('phase2-default-vbop.log', 'PHASE2_RESULT pass=true'),
                             ('vbop-differential.log', 'VBOP_DIFF_RESULT pass=true'),
                             ('vbop-defaults.log', 'VBOP_DIFF_RESULT pass=true')]:
            assert marker in p.checked(DEST / name)
        for mode in ('none', 'vbop', 'cdp', 'both'):
            assert 'PHASE2_PARAMETERS pass=true' in p.checked(DEST / ('parameters-' + mode + '.log'))
        differential = {}
        for name, sequences in [('vbop-differential.log', 15), ('vbop-defaults.log', 5)]:
            body = p.checked(DEST / name)
            rows = re.findall(r'^VBOP_DIFF_SEQUENCE .*$', body, re.M)
            needt = re.findall(r'^VBOP_DIFF_NEEDT .*$', body, re.M)
            assert len(rows) == len(needt) == sequences
            assert f'VBOP_DIFF_RESULT pass=true sequences={sequences} identical={sequences}' in body
            for row in needt:
                fields = dict(re.findall(r'(\w+)=(\S+)', row))
                assert fields['deviation'] == 'D2'
                assert int(fields['original_equal']) == int(fields['actual_equal']) + int(fields['downgraded'])
            differential[name] = dict(sequences=rows, needT=needt)
        assert 'VBOP_HANDSHAKE pass=true latency=7 requests=64 identical=64' in p.checked(DEST / 'vbop-differential.log')
        for name in ('phase2-real-vbop.log', 'phase2-default-vbop.log'):
            assert 'PHASE3A_NEEDT pass=true training=ReadUnique emitted=StashShared downgraded=1 deviation=D2' in p.checked(DEST / name)
        logs = {}
        for path in DEST.rglob('*.log'):
            body = p.checked(path)
            assert body.startswith('HEAD=' + fp['head'] + '\n'), path
            assert json.loads(Path(str(path) + '.sources.json').read_text())['sources'] == fp['sources'], path
            logs[str(path.relative_to(DEST))] = dict(bytes=path.stat().st_size, sha256=p.sha(path.read_bytes()))
        p.dump('comparison.json', comparisons)
        p.dump('differential-summary.json', differential)
        p.dump('log-manifest.json', logs)
        p.dump('accepted-sources.json', fp)
        p.dump('acceptance.json', dict(accepted=True, compile=3, off=37, on=37, existing_directed=5, logs=len(logs)))
        print('ACCEPTED compile=3 off=37/37 on=37/37 existing_directed=5 reference_zero_diff=true')
    else:
        raise ValueError(action)
