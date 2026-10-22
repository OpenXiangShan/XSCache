#!/usr/bin/env python3
"""Verify nonempty HEAD-tagged evidence and append one completed soak item.
Usage: report-pf-overnight.py soak|soak-long|soak-harsh|soak-fast
"""
from pathlib import Path
import collections, hashlib, json, re, sys
ROOT=Path(__file__).resolve().parents[1]
DEST=Path('/data/zhuy/pf-framework/overnight-20261021')
HEAD='4c3b80755c251cf2e0615f4b7513a492b34adee8'
PHASES={'soak-final':(2,20,5000,'四源系统浸泡'), 'soak':(2,20,5000,'四源系统浸泡'), 'soak-long':(4,100,50000,'50000 拍加量浸泡'),
        'soak-harsh':(5,20,50000,'恶劣 TLB 浸泡'), 'soak-fast':(6,20,50000,'D3 快路系统浸泡')}
def fields(line): return dict(re.findall(r'(\w+)=(\S+)',line))
def summarize(phase):
    item,seeds,cycles,label=PHASES[phase]
    outcomes={}; sources={}; tlbs={}; attempted=set(); failures=[]; manifests={}
    for path in sorted((DEST/phase).glob('*.log')):
        text=path.read_text()
        assert path.stat().st_size>0 and text.startswith('HEAD='+HEAD+'\n'),path
        if not re.search(r'^EXIT=\d+$',text,re.M): continue
        manifests[str(path.relative_to(DEST))]={'bytes':path.stat().st_size,'sha256':hashlib.sha256(path.read_bytes()).hexdigest()}
        begins=[fields(x) for x in re.findall(r'^OVERNIGHT_BEGIN .*$',text,re.M)]
        attempted.update((int(x['slices']),int(x['seed'])) for x in begins)
        for row in re.findall(r'^OVERNIGHT_(?:SOAK|SOURCE|TLB) .*$',text,re.M):
            d=fields(row); key=(int(d['slices']),int(d['seed']))
            if row.startswith('OVERNIGHT_SOAK '):
                assert int(d['activeCycles'])>=cycles and d['pass']=='true'
                assert int(d['completed'])==int(d['demands']) and int(d['checkedBeats'])==int(d['demands'])*2
                assert all(d[x]=='true' for x in ('scoreboard','sourceClosure','totalConservation','recvConservation')) and d['watchdog']=='false'
                assert key not in outcomes,('duplicate accepted seed',key)
                outcomes[key]=d
            elif row.startswith('OVERNIGHT_TLB '):
                assert d['modelRetries']==d['actualRetries'] and d['modelDrops']==d['actualDrops']
                tlbs[key]=d
            else: sources[(key,int(d['source']))]=d
        if '\nEXIT=0\n' not in text:
            messages=re.findall(r'^.*(?:Exception|AssertionError|assertion failed|watchdog).*$',text,re.M)
            failures.append(dict(log=str(path.relative_to(DEST)),messages=messages[-4:]))
    expected={(s,seed) for s in (1,2,4) for seed in range(1,seeds+1)}
    missing=sorted(expected-attempted); unpassed=sorted(expected-set(outcomes))
    accepted=not unpassed and not failures
    # Do not silently mark a group complete when later seeds were never run.
    if missing:
        print(json.dumps({'phase':phase,'incomplete':True,'missing':missing,'passed':len(outcomes),'failures':failures},indent=2)); return 2
    for key in outcomes:
        assert key in tlbs
        for src in (1,2,3,9):
            x=sources[(key,src)]
            assert int(x['entered'])>0 and int(x['pending'])==0
            assert int(x['enqueued'])==sum(int(x[k]) for k in ('overwritten','queueEntered','dedup','pending'))
            assert int(x['produced'])==sum(int(x[k]) for k in ('overwritten','entered','dedup','pending'))
    summary=dict(phase=phase,accepted=accepted,attempted=sorted(attempted),passed=len(outcomes),unpassed=unpassed,
        failures=failures,results=list(outcomes.values()),sourceResults=list(sources.values()),tlbResults=list(tlbs.values()),logs=manifests)
    (DEST/(phase+'-summary.json')).write_text(json.dumps(summary,indent=2)+'\n')
    report=ROOT/'report-pf-overnight.md'; text=report.read_text()
    title=f'{item}. {label}：'
    conclusion=('通过' if accepted else '有问题')+f'，{len(outcomes)}/{len(expected)} 种子通过（每种配置 {seeds} 种子、至少 {cycles} 拍）。'
    text=re.sub(r'^'+re.escape(title)+r'.*$',title+conclusion,text,flags=re.M)
    text+=f'\n## 第 {item} 项完成：{label}\n\n{conclusion}\n'
    text+='\n|slice|尝试种子|通过|完成demand|核对数据beat|总周期|\n|---:|---:|---:|---:|---:|---:|\n'
    for s in (1,2,4):
        rows=[v for k,v in outcomes.items() if k[0]==s]
        text+=f'|{s}|{sum(k[0]==s for k in attempted)}|{len(rows)}|{sum(int(v["completed"]) for v in rows)}|{sum(int(v["checkedBeats"]) for v in rows)}|{sum(int(v["cycles"]) for v in rows)}|\n'
    text+='\n每来源账目（仅通过种子汇总；逐seed完整数值在 JSON 和日志中）：\n\n|slice|来源|生成|入队|覆盖|实际进入|队列进入|dedup|pending|useful|redundant|\n|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n'
    for s in (1,2,4):
        for src in (1,2,3,9):
            rows=[v for (key,source),v in sources.items() if key[0]==s and source==src and key in outcomes]
            nums=[sum(int(v[k]) for v in rows) for k in ('produced','enqueued','overwritten','entered','queueEntered','dedup','pending','useful','redundant')]
            text+='|'+ '|'.join(map(str,[s,src,*nums]))+'|\n'
    text+='\nRecv实际进入包括直接路径，故可大于其入队数；队列守恒用 queueEntered，端到端守恒用 entered。每64拍及排空结束独立检查全部10种来源/每slice，8种效果指标来源和等于总计数。\n'
    if phase in ('soak-harsh','soak-fast'):
        text+='\n|slice|TLB请求|注入miss|注入fault|模型/实际重试|模型/实际丢弃|fast分配|\n|---:|---:|---:|---:|---:|---:|---:|\n'
        for s in (1,2,4):
            rows=[v for k,v in tlbs.items() if k[0]==s and k in outcomes]
            vals=[sum(int(v[k]) for v in rows) for k in ('requests','injectedMisses','injectedFaults','modelRetries','actualRetries','modelDrops','actualDrops','fastAllocations')]
            text+=f'|{s}|{vals[0]}|{vals[1]}|{vals[2]}|{vals[3]}/{vals[4]}|{vals[5]}/{vals[6]}|{vals[7]}|\n'
    if failures:
        text+='\n失败命令已记录各日志 COMMAND，以下日志已核验非空并带正确 HEAD：\n'
        for f in failures: text+='\n- '+str(DEST/f['log'])+'：'+'; '.join(f['messages'])+'\n'
    text+=f'\n复现：`python3 scripts/run-pf-overnight.py {phase}`（已有目录不可覆盖；精确Java命令见每份日志）。汇总与全部日志SHA-256：`{DEST/(phase+"-summary.json")}`。\n'
    report.write_text(text)
    print(f'{phase} accepted={accepted} passed={len(outcomes)}/{len(expected)}')
    return 0 if accepted else 1
if __name__=='__main__': sys.exit(summarize(sys.argv[1]))
