#!/usr/bin/env python3
"""Intent, before fixes: fingerprint baseline/final compile and simulation logs,
compare the complete existing two-mode suite, and keep all artifacts on NVMe.
Only the three authorized compile targets are used; Java runs simulations.
"""
from pathlib import Path
import concurrent.futures, importlib.util, json, os, sys
sys.dont_write_bytecode=True
ROOT=Path(__file__).resolve().parents[1]
spec=importlib.util.spec_from_file_location('helper',ROOT/'scripts/run-pf-1c2.py')
h=importlib.util.module_from_spec(spec);spec.loader.exec_module(h)
BASE_ROOT=Path('/data/zhuy/pf-framework/audit-fix-1')
DEST=Path('/data/zhuy/pf-framework/audit-fix-1')/sys.argv[2]
DEST.mkdir(parents=True,exist_ok=True)
h.DEST=DEST
TMP=Path('/data/zhuy/pf-framework/audit-fix-1/tmp')
h.ENV.update(CCACHE_DISABLE='1',TMPDIR=str(TMP),JAVA_TOOL_OPTIONS='-Djava.io.tmpdir='+str(TMP))
def parallel(jobs):
 with concurrent.futures.ThreadPoolExecutor(max_workers=int(os.environ.get('PF_REGRESSION_JOBS','8'))) as pool:
  fs=[pool.submit(h.run,*j) for j in jobs]
  for f in fs: f.result()
def java(entry,args=(),props=()):
 return ['java','-Xmx6g',*props,'-cp',h.classpath(),entry,*args]
def suite_plan():
 jobs=[]
 def model(folder,case,slices,extra=(),entry='oceanus.TestTop_L2Top'):
  jobs.append((f'simulation/{folder}/{folder}/{case}-{slices}slice.log',entry,
               ['--simulate','--case='+case,'--slices',str(slices),*extra]))
 for case in ('dirty-victim','load-miss'):
  for n in (1,2,4):model('baseline',case,n,['--no-bop'])
 for case in ('c1','c2','c2-uc','c3'):model('collision','bop-collision-'+case,1)
 for case in ('multi-tshr','port-readback','prefetch-starvation'):
  jobs.append(('simulation/concurrent/'+case+'.log','oceanus.TestTop_L2Concurrent',['--case='+case]))
 for case in ('bop-2c-all','bop-stash-all','bop-useless-all'):
  model('directed',case,1,entry='oceanus.TestTop_L2Prefetched')
 for case in ('dirty-victim','load-miss'):
  for n in (1,2,4):model('issue-off',case,n,['--no-prefetch-issue'])
 for case in ('bop-closed-loop','bop-closed-loop-uc'):
  for n in (2,4):model('multi',case,n)
 for case in ('bop-closed-loop','bop-closed-loop-uc'):model('single',case,1)
 for n in (1,2,4):model('smoke','bop-prefetch-smoke',n)
 model('smoke','bop-prefetch-smoke-uc',1)
 jobs.append(('starvation-depth2.log','oceanus.TestTop_L2Concurrent',['--case=prefetch-starvation','--queue-depth=2']))
 for n in (1,2,4):jobs.append((f'recv-{n}slice.log','oceanus.TestTop_L2Recv',['--slices='+str(n)]))
 jobs.append(('sustained-N257.log','oceanus.TestTop_L2Recv',['--case=sustained','--recv-cycles=257']))
 assert len(jobs)==37
 return jobs
def bytecode():
 base=ROOT/'out/XSCache'
 return {str(p.relative_to(base)):h.sha(p.read_bytes())
  for d in ('compile.dest/classes','test/compile.dest/classes','testtop/oceanus/compile.dest/classes')
  for p in (base/d).rglob('*.class')}
action=sys.argv[1]
if action=='compile':
 h.dump('sources-before.json',h.fingerprint())
 h.dump('reference.json',h.reference())
 for t in h.TARGETS:h.run(['mill','--no-server',t],t+'.log')
 h.dump('compiled-sources.json',h.fingerprint())
 h.dump('compiled-bytecode.json',bytecode())
elif action=='suite':
 plan=suite_plan()
 h.dump('suite-plan.json',plan)
 jobs=[]
 for mode in ('off','on'):
  for name,entry,args in plan:jobs.append((java(entry,args,['-Doceanus.phase1a.ports='+str(mode=='on').lower()]),mode+'/'+name))
 parallel(jobs)
elif action=='drain':
 parallel([(java('oceanus.TestTop_L2PrefetchDrain',['--slices=1','--seed='+str(n),'--cycles=5000'],['-Doceanus.phase1a.ports=true']),f'seed{n}.log') for n in (1,6,11,16)])
elif action=='old-directed':
 jobs=[(java('oceanus.TestTop_L2PrefetchTlb'),'tlb.log'),(java('oceanus.TestTop_L2CDPQueue'),'cdp-queue.log')]
 for n in (2,4):jobs.append((java('oceanus.TestTop_L2Train',['--slices='+str(n)],['-Doceanus.phase1a.ports=true','-Doceanus.phase1b.drive=true','-Doceanus.phase1c2.check=true']),f'train-{n}slice.log'))
 jobs.append((java('oceanus.TestTop_L2CDPDepth',props=['-Doceanus.phase1a.ports=true','-Doceanus.phase1c2.check=true','-Doceanus.phase1c2.depth=true']),'cdp-depth.log'))
 parallel(jobs)
elif action=='new-directed':
 jobs=[(java('oceanus.TestTop_L2PrefetchFairness'),'fairness.log'),(java('oceanus.TestTop_L2TrainArbitration'),'train-arbitration.log')]
 for mode in ('off','train-only','tlb-only','none','vbop','cdp','both'):
  jobs.append((java('oceanus.TestTop_L2StubParameters',[mode]),'parameters-'+mode+'.log'))
 for n in (2,4):
  jobs.append((java('oceanus.TestTop_L2Train',['--slices='+str(n),'--mixed-enable'],['-Doceanus.phase1a.ports=true','-Doceanus.phase1b.drive=true','-Doceanus.phase1c2.check=true']),'mixed-train-'+str(n)+'.log'))
 for fast in (True,False):
  jobs.append((java('oceanus.TestTop_L2PrefetchPhase2',[] if fast else ['--no-fast-path'],['-Doceanus.phase1a.ports=true','-Doceanus.phase2=true']),'phase2-'+str(fast)+'.log'))
 parallel(jobs)
elif action=='phase2-baseline':
 parallel([(java('oceanus.TestTop_L2PrefetchPhase2',[] if fast else ['--no-fast-path'],['-Doceanus.phase1a.ports=true','-Doceanus.phase2=true']),'phase2-'+str(fast)+'.log') for fast in (True,False)])
elif action=='differential':
 parallel([(java('oceanus.TestTop_OceanusVBOP',['--samples=10',*(['--defaults'] if defaults else [])]),'vbop-'+str(defaults)+'.log') for defaults in (False,True)])
elif action=='verify':
 baseline=BASE_ROOT/'baseline'
 compiled=json.loads((DEST/'compiled-sources.json').read_text())
 assert h.fingerprint()['sources']==compiled['sources'], 'source changed after compilation'
 assert bytecode()==json.loads((DEST/'compiled-bytecode.json').read_text()), 'compiled bytecode changed'
 assert h.reference()==json.loads((baseline/'reference.json').read_text())
 assert not h.git('diff','be2112e','--','src/main/scala/coupledL2')
 plan=json.loads((DEST/'suite-plan.json').read_text())
 comparison={}
 for mode in ('off','on'):
  comparison[mode]=[]
  for name,_,_ in plan:
   current,old=h.results(DEST/mode/name),h.results(baseline/mode/name)
   comparison[mode].append(dict(log=name,identical=current==old,current=current,baseline=old))
 h.dump('comparison.json',comparison)
 differences=[(mode,row['log']) for mode,rows in comparison.items() for row in rows if not row['identical']]
 assert not differences, differences
 directed={}
 import re
 for name in ('tlb.log','cdp-queue.log','train-2slice.log','train-4slice.log','cdp-depth.log','phase2-True.log','phase2-False.log'):
  pattern=r'^(?:TLB_CASE|TLB_RESULT|CDP_[A-Z_]+|VA_[A-Z_]+|TRAIN_GATE_RESULT|PHASE[23][A-Z0-9_]*).*'
  current=re.findall(pattern,h.checked(DEST/name),re.M)
  old=re.findall(pattern,h.checked(baseline/name),re.M)
  assert current and old
  directed[name]=dict(identical=current==old,current=current,baseline=old)
 h.dump('directed-comparison.json',directed)
 fair=h.checked(DEST/'fairness.log')
 assert len(re.findall(r'^TLB_FAIR_CASE pass=true',fair,re.M))==4
 arb=h.checked(DEST/'train-arbitration.log')
 assert 'patterns=256 payload=true pulses=true' in arb
 for n in (2,4):assert 'TRAIN_MIXED_INTEGRATION pass=true' in h.checked(DEST/f'mixed-train-{n}.log')
 for mode in ('off','train-only','tlb-only','none','vbop','cdp','both'):
  assert 'CDP_RTL_PARAMETERS pass=true mode='+mode in h.checked(DEST/('parameters-'+mode+'.log'))
 for defaults in (False,True):
  body=h.checked(DEST/f'vbop-{defaults}.log')
  assert 'VBOP_DIFF_RESULT pass=true sequences=50 identical=50' in body
  assert len(re.findall(r'^VBOP_DIFF_SEQUENCE pass=true',body,re.M))==50
 for seed in (1,6,11,16):
  body=h.checked(DEST/f'seed{seed}.log')
  assert 'itemClosure=true' in body and len(re.findall(r'^DRAIN_DISPOSITION ',body,re.M))==2
 logs={}
 for path in DEST.rglob('*.log'):
  body=h.checked(path)
  fp=json.loads(Path(str(path)+'.sources.json').read_text())
  assert fp['sources']==compiled['sources'], path
  assert body.startswith('HEAD='+fp['head']+'\n'),path
  logs[str(path.relative_to(DEST))]=dict(bytes=path.stat().st_size,sha256=h.sha(path.read_bytes()))
 h.dump('log-manifest.json',logs)
 h.dump('accepted-sources.json',h.fingerprint())
 print('SUITE_ACCEPTED off=37/37 on=37/37 reference_unchanged=true coupledL2_zero_diff=true')
else:raise ValueError(action)
