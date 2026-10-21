#!/usr/bin/env python3
"""Intent (phase2): fingerprint all logs, rerun the baseline at 5365e3f before
editing RTL, and compare both parameter groups plus every existing directed test.
Keep all generated data and logs on the local NVMe; use only approved targets.
"""
from pathlib import Path
import concurrent.futures, importlib.util, json, os, re, sys
sys.dont_write_bytecode = True

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('previous', ROOT / 'scripts/run-pf-1c2.py')
previous = importlib.util.module_from_spec(spec)
spec.loader.exec_module(previous)
DEST = Path(sys.argv[2] if len(sys.argv) > 2 else '/data/zhuy/pf-framework/pf-phase2-recheck')
previous.DEST = DEST
BASE = Path(os.environ.get('PF_PHASE2_BASELINE', '/data/zhuy/pf-framework/pf-phase2-20261021/baseline'))
HIST = Path('/data/zhuy/pf-framework/pf-1c2-20261021/final')
HEAD = '5365e3f1aac41e2949e1a3e35d44542420efa1ec'

def old_directed():
    cp = previous.classpath()
    jobs = [(['java', '-cp', cp, 'oceanus.TestTop_L2PrefetchTlb'], 'tlb.log'),
            (['java', '-cp', cp, 'oceanus.TestTop_L2CDPQueue'], 'cdp-queue.log')]
    for n in (2, 4):
        jobs.append((['java', '-Doceanus.phase1a.ports=true', '-Doceanus.phase1b.drive=true',
                      '-Doceanus.phase1c2.check=true', '-cp', cp, 'oceanus.TestTop_L2Train',
                      '--slices=' + str(n)], 'train-' + str(n) + 'slice.log'))
    jobs.append((['java', '-Doceanus.phase1a.ports=true', '-Doceanus.phase1c2.check=true',
                  '-Doceanus.phase1c2.depth=true', '-cp', cp, 'oceanus.TestTop_L2CDPDepth'], 'cdp-depth.log'))
    return jobs

def parallel(jobs):
    with concurrent.futures.ThreadPoolExecutor(max_workers=int(os.environ.get('PF_REGRESSION_JOBS', '8'))) as pool:
        futures = [pool.submit(previous.run, *job) for job in jobs]
        for f in futures: f.result()

def bytecode():
    result = {}
    for target in ('compile', 'test/compile', 'testtop/oceanus/compile'):
        folder = ROOT / 'out/XSCache' / (target + '.dest/classes')
        result[target] = {str(p.relative_to(folder)): previous.sha(p.read_bytes()) for p in folder.rglob('*.class')}
    return result

if __name__ == '__main__':
    DEST.mkdir(parents=True, exist_ok=True)
    action = sys.argv[1]
    if action == 'init':
        accepted = json.loads((HIST / 'accepted-sources.json').read_text())
        for name, digest in accepted['sources'].items():
            assert previous.sha(previous.subprocess.check_output(['git', 'show', HEAD + ':' + name], cwd=ROOT)) == digest, name
        previous.dump('baseline-validation.json', dict(head=HEAD, sources=accepted['sources']))
        previous.dump('suite-plan.json', json.loads((HIST / 'suite-plan.json').read_text()))
        previous.dump('reference-before.json', previous.reference())
    elif action == 'compile':
        for target in previous.TARGETS: previous.run(['mill', '--no-server', target], target + '.log')
    elif action == 'suite':
        previous.dump('accepted-sources.json', previous.fingerprint())
        cp = previous.classpath()
        jobs = []
        for mode in ('off', 'on'):
            for name, entry, args in json.loads((DEST / 'suite-plan.json').read_text()):
                jobs.append((['java', '-Doceanus.phase1a.ports=' + str(mode == 'on').lower(),
                              '-cp', cp, entry, *args], mode + '/' + name))
        jobs.extend(old_directed())
        parallel(jobs)
    elif action == 'seal-baseline':
        assert DEST == BASE
        assert bytecode() == json.loads((DEST / 'baseline-bytecode.json').read_text())
        for mode in ('off', 'on'):
            for name, _, _ in json.loads((DEST / 'suite-plan.json').read_text()):
                assert previous.results(DEST / mode / name) == previous.results(HIST / mode / name)
        for _, name in old_directed(): previous.checked(DEST / name)
        # Source edits during this baseline run do not change its classpath.
        # Seal the unchanged compiled code separately from invocation-time files.
        previous.dump('runtime-baseline-sources.json', json.loads((DEST / 'XSCache.testtop.oceanus.compile.log.sources.json').read_text()))
        previous.dump('baseline-sealed.json', dict(head=HEAD, bytecode_unchanged=True, off=37, on=37, existing_directed=5))
        print('BASELINE_SEALED off=37/37 on=37/37 bytecode_unchanged=true')
    elif action == 'directed':
        cp = previous.classpath()
        jobs = [(['java', '-Doceanus.phase1a.ports=true', '-Doceanus.phase2=true',
                  '-cp', cp, 'oceanus.TestTop_L2PrefetchPhase2'], 'phase2.log')]
        for mode in ('none', 'vbop', 'cdp', 'both'):
            jobs.append((['java', '-cp', cp, 'oceanus.TestTop_L2StubParameters', mode], 'parameters-' + mode + '.log'))
        parallel(jobs)
    elif action == 'verify':
        assert json.loads((BASE / 'baseline-sealed.json').read_text())['bytecode_unchanged']
        assert previous.reference() == json.loads((DEST / 'reference-before.json').read_text())
        assert previous.reference() == json.loads((BASE / 'reference-before.json').read_text())
        assert not previous.git('diff', HEAD, '--', 'src/main/scala/coupledL2')
        comparisons = {}
        for mode in ('off', 'on'):
            rows = []
            for name, _, _ in json.loads((DEST / 'suite-plan.json').read_text()):
                new = previous.results(DEST / mode / name)
                old = previous.results(BASE / mode / name)
                assert new == old, (mode, name, new, old)
                assert old == previous.results(HIST / mode / name), (mode, name, 'historical mismatch')
                rows.append(dict(log=name, current=new, baseline=old))
            comparisons[mode] = rows
        extra = re.compile(r'^(?:TLB_CASE|TLB_RESULT|CDP_[A-Z_]+|VA_[A-Z_]+|TRAIN_GATE_RESULT).*$', re.M)
        for _, name in old_directed():
            a, b = (extra.findall(previous.checked(d / name)) for d in (DEST, BASE))
            assert a and a == b, (name, a, b)
        previous.dump('comparison.json', comparisons)
        current = previous.fingerprint()
        accepted = json.loads((DEST / 'accepted-sources.json').read_text())
        assert current['sources'] == accepted['sources']
        logs = {}
        for path in DEST.rglob('*.log'):
            body = previous.checked(path)
            assert body.startswith('HEAD=' + accepted['head'] + '\n')
            assert json.loads(Path(str(path) + '.sources.json').read_text())['sources'] == current['sources'], path
            logs[str(path.relative_to(DEST))] = dict(bytes=path.stat().st_size, sha256=previous.sha(path.read_bytes()))
        body = previous.checked(DEST / 'phase2.log')
        rows = re.findall(r'^PHASE2_.*$', body, re.M)
        assert rows and 'PHASE2_RESULT pass=true' in body
        for mode in ('none', 'vbop', 'cdp', 'both'):
            body = previous.checked(DEST / ('parameters-' + mode + '.log'))
            assert 'PHASE2_PARAMETERS pass=true mode=' + mode in body
        previous.dump('directed-results.json', rows)
        previous.dump('log-manifest.json', logs)
        previous.dump('acceptance.json', dict(accepted=True, compile=3, off=37, on=37, existing_directed=5, new=rows))
        print('ACCEPTED compile=3 off=37/37 on=37/37 existing_directed=5 reference_zero_diff=true')
    else: raise ValueError(action)
