#!/usr/bin/env python3
"""Intent (1c-2): reproducible local-disk acceptance, with no /tmp dependencies.

Usage: run-pf-1c2.py init|compile|suite|directed|verify [output-directory] [directed-log-name]
Only the three approved Mill compile targets are used; simulation runs Java.
"""
from pathlib import Path
import concurrent.futures, hashlib, json, os, re, subprocess, sys, time

ROOT = Path(__file__).resolve().parents[1]
DEST = Path(sys.argv[2] if len(sys.argv) > 2 else '/data/zhuy/pf-framework/pf-1c2-20261021')
OLD = Path('/data/zhuy/pf-framework/pf-1c1-20261021/final')
ENV = {**os.environ, 'CCACHE_DISABLE': '1'}
TARGETS = ('XSCache.compile', 'XSCache.test.compile', 'XSCache.testtop.oceanus.compile')
SUMMARY = re.compile(r'^(?:BOP_[A-Z0-9_]+|PF_SOURCE_PHASE|CONCURRENT_[A-Z0-9_]+|STARVATION_RESULT|PORT_[A-Z0-9_]+|MULTI_[A-Z0-9_]+|CASE .*actual|COUNTS total|TRAIN_FIRE count|EVICTBACK_COUNTS|SLICE_EVICTBACK_FIRE|SEVICT_COUNTS|RECV_[A-Z0-9_]+).*$', re.M)

def sha(b): return hashlib.sha256(b).hexdigest()
def git(*args): return subprocess.check_output(['git', *args], cwd=ROOT, text=True).strip()
def dump(name, value):
    p = DEST / name
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(json.dumps(value, indent=2) + '\n')
def fingerprint():
    files = set(git('ls-files', 'src/main', 'src/test', 'scripts', 'build.sc', 'common.sc').splitlines())
    files.update(git('ls-files', '--others', '--exclude-standard', 'src/main', 'src/test', 'scripts').splitlines())
    return dict(head=git('rev-parse', 'HEAD'), branch=git('branch', '--show-current'),
                sources={n: sha((ROOT / n).read_bytes()) for n in sorted(files)})
def reference():
    root = Path('/data/zhuy/coupledL2')
    return {str(p.relative_to(root)): sha(p.read_bytes()) for p in root.rglob('*') if p.is_file()}
def classpath():
    d = ROOT / 'out/XSCache/testtop/oceanus'
    paths = []
    for item in json.loads((d / 'compileClasspath.json').read_text())['value']:
        item = item.split(':', 3)[3]
        paths.append(item)
        if item.endswith('/compile-resources'):
            paths.extend(str(Path(item).parent / n) for n in ('src/main/resources', 'resources'))
    paths.append(json.loads((d / 'compile.json').read_text())['value']['classes'].split(':', 3)[3])
    return ':'.join(dict.fromkeys(paths))
def run(cmd, name):
    path = DEST / name
    path.parent.mkdir(parents=True, exist_ok=True)
    assert not path.exists(), path
    fp = fingerprint()
    dump(name + '.sources.json', fp)
    with path.open('w') as f:
        f.write('HEAD=' + fp['head'] + '\nBRANCH=' + fp['branch'] + '\nSOURCES_SHA256=' + sha(json.dumps(fp['sources'], sort_keys=True).encode()) + '\nCOMMAND=' + repr(cmd) + '\n')
        f.flush()
        start = time.monotonic()
        code = subprocess.run(cmd, cwd=ROOT, env=ENV, stdout=f, stderr=subprocess.STDOUT).returncode
        f.write('\nELAPSED_SECONDS=%.3f\nEXIT=%d\n' % (time.monotonic() - start, code))
    print(name, 'EXIT=' + str(code), flush=True)
    assert code == 0, path
def checked(path):
    text = path.read_text()
    assert path.stat().st_size > 0 and 'EXIT=0' in text, path
    return text
def results(path):
    rows = SUMMARY.findall(checked(path))
    assert rows, path
    return rows

if __name__ == '__main__':
    DEST.mkdir(parents=True, exist_ok=True)
    action = sys.argv[1]
    if action == 'init':
        accepted = json.loads((OLD / 'accepted-sources.json').read_text())
        base = '6882fec1384e4c8c000fef1ff12c7f90fd3397ca'
        for n, h in accepted['sources'].items():
            assert sha(subprocess.check_output(['git', 'show', base + ':' + n], cwd=ROOT)) == h, n
        plan = json.loads((OLD / 'suite-plan.json').read_text())
        assert len(plan) == 37
        for mode in ('off', 'on'):
            for name, _, _ in plan: results(OLD / mode / name)
        dump('baseline-validation.json', dict(head=base, identical_sources=accepted['sources'], historical_head=accepted['head'], logs=74))
        dump('reference-before.json', reference())
        dump('suite-plan.json', plan)
    elif action == 'compile':
        for t in TARGETS: run(['mill', '--no-server', t], t + '.log')
    elif action == 'suite':
        dump('accepted-sources.json', fingerprint())
        cp = classpath()
        jobs = []
        for mode in ('off', 'on'):
            for name, entry, args in json.loads((DEST / 'suite-plan.json').read_text()):
                jobs.append((['java', '-Doceanus.phase1a.ports=' + str(mode == 'on').lower(), '-cp', cp, entry, *args], mode + '/' + name))
        # Intent (1c-2): bound independent simulation processes on the local
        # NVMe host; every process retains its own ChiselSim output directory.
        with concurrent.futures.ThreadPoolExecutor(max_workers=int(os.environ.get('PF_REGRESSION_JOBS', '12'))) as pool:
            futures = [pool.submit(run, *job) for job in jobs]
            for f in futures: f.result()
    elif action == 'directed':
        cp = classpath()
        jobs = [(['java', '-cp', cp, 'oceanus.TestTop_L2CDPQueue'], 'cdp-queue.log')]
        for n in (2, 4):
            jobs.append((['java', '-Doceanus.phase1a.ports=true', '-Doceanus.phase1b.drive=true', '-Doceanus.phase1c2.check=true', '-cp', cp, 'oceanus.TestTop_L2Train', '--slices=' + str(n)], 'train-' + str(n) + 'slice.log'))
        jobs.append((['java', '-Doceanus.phase1a.ports=true', '-Doceanus.phase1c2.check=true', '-Doceanus.phase1c2.depth=true', '-cp', cp, 'oceanus.TestTop_L2CDPDepth'], 'cdp-depth.log'))
        for job in jobs:
            if len(sys.argv) < 4 or job[1] == sys.argv[3]: run(*job)
    elif action == 'verify':
        for t in TARGETS: checked(DEST / (t + '.log'))
        assert reference() == json.loads((DEST / 'reference-before.json').read_text())
        assert not git('diff', '6882fec', '--', 'src/main/scala/coupledL2')
        current = fingerprint()
        assert current['sources'] == json.loads((DEST / 'accepted-sources.json').read_text())['sources']
        comparisons = {}
        for mode in ('off', 'on'):
            rows = []
            for name, _, _ in json.loads((DEST / 'suite-plan.json').read_text()):
                new, old = results(DEST / mode / name), results(OLD / mode / name)
                assert new == old, (mode, name, new, old)
                rows.append(dict(log=name, results=new, baseline=old))
            comparisons[mode] = rows
        dump('comparison.json', comparisons)
        directed = {}
        for name in ('cdp-queue.log', 'train-2slice.log', 'train-4slice.log', 'cdp-depth.log'):
            body = checked(DEST / name)
            assert 'pass=false' not in body
            lines = re.findall(r'^(?:CDP_[A-Z_]+|TRAIN_GATE_RESULT|VA_DIRECTED_RESULT).*$', body, re.M)
            assert lines and any(line.startswith('CDP_') and 'pass=true' in line for line in lines), name
            directed[name] = lines
        dump('directed-results.json', directed)
        for path in DEST.rglob('*.log'):
            body = checked(path)
            assert 'HEAD=6882fec1384e4c8c000fef1ff12c7f90fd3397ca' in body, path
            assert json.loads(Path(str(path) + '.sources.json').read_text())['sources'] == current['sources'], path
        logs = {str(p.relative_to(DEST)): dict(bytes=p.stat().st_size, sha256=sha(p.read_bytes())) for p in DEST.rglob('*.log')}
        dump('log-manifest.json', logs)
        print('ACCEPTED compile=3 off=37/37 on=37/37 reference_zero_diff=true')
    else: raise ValueError(action)
