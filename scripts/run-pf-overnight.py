#!/usr/bin/env python3
"""Local-NVMe overnight tests. No RTL changes, staging, commits or pushes.
Compile only the three authorized targets; simulate the compiled entry points
with Java. Each log records HEAD, command and all test/RTL source hashes.
"""
from pathlib import Path
import concurrent.futures, hashlib, importlib.util, json, os, re, shutil, subprocess, sys, time
sys.dont_write_bytecode=True
ROOT=Path(__file__).resolve().parents[1]
DEST=Path('/data/zhuy/pf-framework/overnight-20261021')
HEAD='4c3b80755c251cf2e0615f4b7513a492b34adee8'
spec=importlib.util.spec_from_file_location('helpers', ROOT/'scripts/run-pf-1c2.py')
p=importlib.util.module_from_spec(spec); spec.loader.exec_module(p)
p.DEST=DEST
ENV={**os.environ,'CCACHE_DISABLE':'1','TMPDIR':'/data/zhuy/pf-framework/overnight-20261021/tmp'}
Path(ENV['TMPDIR']).mkdir(parents=True,exist_ok=True)
def run(cmd,name,timeout=14400):
    path=DEST/name; path.parent.mkdir(parents=True,exist_ok=True)
    if path.exists(): raise ValueError('Refuse overwrite: '+str(path))
    fp=p.fingerprint(); assert fp['head']==HEAD
    Path(str(path)+'.sources.json').write_text(json.dumps(fp,indent=2)+'\n')
    with path.open('w') as f:
        f.write('HEAD='+fp['head']+'\nCOMMAND='+repr(cmd)+'\nSOURCES_SHA256='+p.sha(json.dumps(fp['sources'],sort_keys=True).encode())+'\n'); f.flush()
        start=time.monotonic()
        try: code=subprocess.run(cmd,cwd=ROOT,env=ENV,stdout=f,stderr=subprocess.STDOUT,timeout=timeout).returncode
        except subprocess.TimeoutExpired: code=124; f.write('\nWALL_WATCHDOG=true\n')
        f.write(f'\nELAPSED_SECONDS={time.monotonic()-start:.3f}\nEXIT={code}\n')
    print(name,'EXIT='+str(code),flush=True)
    return dict(log=name,exit=code,bytes=path.stat().st_size,sha256=p.sha(path.read_bytes()))
def java(entry,args=()):
    frozen=DEST/'sealed-classpath.json'
    cp=json.loads(frozen.read_text())['classpath'] if frozen.exists() else p.classpath()
    return ['java','-Xmx8g','-Djava.io.tmpdir='+ENV['TMPDIR'],'-Doceanus.phase1a.ports=true','-cp',cp,entry,*args]
def parallel(jobs,workers):
    results=[]
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        pending={pool.submit(run,*j):j for j in jobs}
        while pending:
            done,_=concurrent.futures.wait(pending,return_when=concurrent.futures.FIRST_COMPLETED)
            for future in done:
                job=pending.pop(future)
                try:
                    result=future.result(); results.append(result)
                    # A failed seed aborts its JVM. Still attempt every later
                    # seed in the batch in a fresh process, preserving evidence.
                    if result['exit'] and 'oceanus.TestTop_L2PrefetchOvernight' in job[0]:
                        cmd=list(job[0]); body=(DEST/job[1]).read_text()
                        begun=[int(x) for x in re.findall(r'^OVERNIGHT_BEGIN seed=(\d+)',body,re.M)]
                        first=next(int(x.split('=')[1]) for x in cmd if x.startswith('--seed='))
                        last=next((int(x.split('=')[1]) for x in cmd if x.startswith('--last-seed=')),first)
                        failed=max(begun) if begun else first
                        if failed<last:
                            cmd=[f'--seed={failed+1}' if x.startswith('--seed=') else x for x in cmd]
                            name=job[1].removesuffix('.log')+f'-continue{failed+1}.log'
                            tail=(cmd,name)
                            pending[pool.submit(run,*tail)]=tail
                except Exception as e: results.append(dict(error=repr(e),job=job[1]))
    return results
def write(name,value): (DEST/name).write_text(json.dumps(value,indent=2)+'\n')
def diffjobs(first,last,kind,latency,label='diff'):
    return (java('oceanus.TestTop_OceanusVBOPOvernight',[f'--kind={kind}',f'--latency={latency}',f'--first={first}',f'--last={last}']),f'{label}/{kind}-latency{latency}-{first}-{last}.log')
if __name__=='__main__':
    DEST.mkdir(parents=True,exist_ok=True)
    action=sys.argv[1]
    if action=='init':
        write('reference-before.json',p.reference()); write('initial-sources.json',p.fingerprint())
    elif action=='compile':
        write('compile-results.json',[run(['mill','--no-server',t],t+'.log') for t in p.TARGETS])
        write('compiled-sources.json',p.fingerprint())
    elif action=='seal':
        cp=[]; hashes={}
        stamp=time.strftime('%Y%m%d-%H%M%S')
        seal=DEST/'bytecode'/stamp; seal.mkdir(parents=True)
        for i,item in enumerate(p.classpath().split(':')):
            src=Path(item)
            if src.is_dir() and item.endswith('/classes'):
                dst=seal/str(i); shutil.copytree(src,dst); cp.append(str(dst))
                for f in dst.rglob('*.class'): hashes[str(f.relative_to(seal))]=p.sha(f.read_bytes())
            else: cp.append(item)
        write('sealed-classpath.json',dict(classpath=':'.join(cp),stamp=stamp,classes=hashes,sources=p.fingerprint()))
    elif action=='smoke-diff':
        write('smoke-diff-results.json',parallel([diffjobs(1,1,'random',l,'smoke-diff') for l in (1,3,7)],3))
    elif action=='diff':
        jobs=[]
        for kind,n in [('random',500),('in-page-stride',50),('cross-page-stride',50),('alternating-offsets',50),('noise',50)]:
            for latency in (1,3,7):
                for first in range(1,n+1,25): jobs.append(diffjobs(first,min(first+24,n),kind,latency))
        write('diff-results.json',parallel(jobs,int(os.environ.get('PF_OVERNIGHT_JOBS','12'))))
    elif action in ('soak','soak-final','smoke-soak','soak-long','soak-harsh','soak-fast'):
        jobs=[]
        number=100 if action=='soak-long' else (1 if action=='smoke-soak' else 20)
        batch=10 if action=='soak-long' else (1 if action=='smoke-soak' else 5)
        cycles=5000 if action in ('soak','soak-final','smoke-soak') else 50000
        flags=['--harsh-tlb'] if action=='soak-harsh' else (['--fast'] if action=='soak-fast' else [])
        for slices in (1,2,4):
            for seed in range(1,number+1,batch):
                last=min(seed+batch-1,number)
                jobs.append((java('oceanus.TestTop_L2PrefetchOvernight',[f'--slices={slices}',f'--seed={seed}',f'--last-seed={last}',f'--cycles={cycles}',*flags]),f'{action}/slices{slices}-seeds{seed}-{last}.log'))
        write(action+'-results.json',parallel(jobs,int(os.environ.get('PF_OVERNIGHT_JOBS','6'))))
    elif action=='audit':
        assert p.reference()==json.loads((DEST/'reference-before.json').read_text())
        assert not p.git('diff',HEAD,'--','src/main')
        write('final-sources.json',p.fingerprint())
        write('log-manifest.json',{str(x.relative_to(DEST)):dict(bytes=x.stat().st_size,sha256=p.sha(x.read_bytes())) for x in DEST.rglob('*.log') if x.stat().st_size})
    else: raise ValueError(action)
