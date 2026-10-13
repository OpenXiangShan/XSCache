# BOP Step 10 issue-off regression (2026-10-10)

All six cases match the recorded pre-injection baseline. Expectations and their derivation were added to L2TopResponseModel.scala before compiling and running; they are enforced by BOP_ISSUE_OFF_BASELINE assertions.

Source HEAD: `5f04336d3991405335d004507190fa8996e22527`. Each log records the full HEAD, git status (only L2TopResponseModel.scala modified), chiselsim directory before/after, and exit code 0. Logs: `/data/zhuy/bop-on-base/regression-20261010-step10/`.

Build ran from the bop-on-base worktree using `CCACHE_DISABLE=1 mill --no-server XSCache.testtop.oceanus.compile`. Simulation ran from the same worktree using `CCACHE_DISABLE=1 java -cp <decoded out/XSCache/testtop/oceanus/runClasspath.json> oceanus.TestTop_L2Top --simulate --case=<case> --slices <n> --no-prefetch-issue`. The classpath entries are Mill PathRefs: retain the path after the third colon. build/out symlinks remain on local NVMe; no waveform was requested.

| Case | Slices | EvictBack created/completed/pending/active | dnTxReq/upTxDat/dnTxDat | TRAIN_FIRE | Per-slice EvictBack fire | Log | Chiselsim timestamp |
|---|---|---|---|---|---|---|---|
| load-miss | 1 | 0/0/0/0 | 1/2/0 | 1 | 0 | `load-miss-1slice.log` | `2026-10-10T17-31-53.297903357` |
| dirty-victim | 1 | 1/1/0/0 | 2/2/2 | 5 | 1 | `dirty-victim-1slice.log` | `2026-10-10T17-33-24.402381293` |
| load-miss | 2 | 0/0/0/0 | 1/2/0 | 1 | 0,0 | `load-miss-2slice.log` | `2026-10-10T17-34-55.736848774` |
| dirty-victim | 2 | 1/1/0/0 | 2/2/2 | 5 | 0,1 | `dirty-victim-2slice.log` | `2026-10-10T17-37-41.958405949` |
| load-miss | 4 | 0/0/0/0 | 1/2/0 | 1 | 0,0,0,0 | `load-miss-4slice.log` | `2026-10-10T17-40-48.220477207` |
| dirty-victim | 4 | 1/1/0/0 | 2/2/2 | 5 | 0,0,1,0 | `dirty-victim-4slice.log` | `2026-10-10T17-47-13.892841500` |

load-miss completed=1; dirty-victim completed=5. dirty-victim's 2/2/2 is the fifth-request replacement phase, with completedDelta=1; its whole-run totals include four preloads. All dirty-victim variants issue WriteBackFull 0x1B and return two victim data beats matching the preload, each with BE=0xffffffff.

Read-only queue-depth check: freeze tag prefetch-bop-freeze-20260929 (e2dc7b6), src/main/scala/oceanus/l2/L2Top.scala:205, uses OverwriteQueue(entries=2). bop-on-base L2Top.scala:181 also uses entries=2. The depth-16 text at freeze line 200 describes coupledL2, not this production pfQueues.

The reference worktree is clean on bop-explore at e2dc7b6; the freeze tag was not moved. Its four pre-existing dirty files were saved on bop-explore-dev4-fourq-obs at 3d01091, with the abandoned C3 expectation explicitly described in the commit. The rescue patch remains at /data/zhuy/bop-on-base/rescue/prefetch-bop-dirty-20261010.patch.
