# test — CHIron Cohestra harness for XSCache

This directory wires the **Cohestra** co-simulation/verification framework into
XSCache: per-DUT boot/smoke drivers for Verilator (cohestra_v3) and VCS
(cohestra_vcs), the run configs, and `boot.sh`, the all-in-one caller.

## Layout

```
test/
├── boot.sh              # all-in-one caller — dispatches to the per-DUT drivers
├── configs/             # cohestra run configs (config.xscache*.ini)
├── scripts/
│   ├── common/          # shared machinery
│   │   ├── common.sh               # sourced shell library (tools, clogb, run wrappers)
│   │   ├── xscache_vcs_top.sv      # unified VCS wrapper top (openllc + zhujiang)
│   │   ├── xscache_vcs_top_defines.sh  # emits its +define+ set (<family> <num_l2>)
│   │   ├── xscache_waivers.vlt     # Verilator waivers (openllc flow)
│   │   └── soak_noassert_defines.sv    # SOAK_NOASSERT=1 assertion no-op defines
│   ├── openllc/boot_openllc.sh    # TestTop_L2OpenLLC (also l2multichi, Verilator only)
│   ├── zhujiang/boot_zhujiang.sh  # TestTop_L2ZhuJiang
│   ├── zhujiang/{check_pins.sh, gen_expected_pins.sh, expected_pins.txt,
│   │             zhujiang_waivers.vlt}
│   ├── avocado/{boot_avocado.sh, avocado_dut.sv, verilator_override/}
│   ├── cornetto/{boot_cornetto.sh, cornetto_dut.sv, cornetto_vcs_top.sv,
│   │             verilator_override/}
│   └── biscotto/{boot_biscotto.sh, biscotto_dut.sv, biscotto_vcs_top.sv,
│                 verilator_override/}
└── CHIron/              # git submodule → RISMicroDevices/CHIron
```

## Bootstrap (fresh clone)

```sh
git submodule update --init --recursive test/CHIron
```

`--recursive` pulls the nested `cchi/cohestra/spdlog` and
`cchi/cohestra/simpleini` submodules. The openllc driver runs this
automatically when the submodule is not initialized.

Requires: CMake ≥ 3.21, C++20 compiler, Verilator (`find_package(verilator)`),
JDK ≥ 11 (mill). VCS stages additionally require Synopsys VCS + Verdi
(`VCS_HOME`/`VERDI_HOME`, honored only when their `bin/` exists — otherwise
auto-detected from PATH).

## boot.sh — the all-in-one caller

```
test/boot.sh [<dut>] [stages...] [--skip-rtl] [-h|--help]
```

- `<dut>`: `openllc` (default; alias `l2openllc`), `zhujiang`, `avocado`,
  `cornetto`, `biscotto`, `multichi` (legacy alias → the openllc driver with
  `DUT=l2multichi`). May also be given via the legacy `DUT` env var.
- `[stages...]`: one or more stage names, run in order (default: `all`).
  Unknown DUT/stage names abort with a usage error before anything runs.
- `--skip-rtl`: drop the `rtl` stage (reuse the existing DUT Verilog).
- **Every environment knob passes through by inheritance** — set it before the
  call exactly as documented for the per-DUT driver below.

The caller itself lives in `test/boot.sh`; all logic lives in
`test/scripts/<dut>/boot_<dut>.sh` (which you can also invoke directly with
the same stage arguments).

### Stage vocabulary

All drivers accept multiple stages and the pseudo-stages `all` and `vcs-all`:

| stage | flow | meaning |
|---|---|---|
| `rtl` | Verilator | regenerate the DUT Verilog (`make test-top-*`) |
| `lint` | Verilator | contract/pin lint of the generated top (not openllc) |
| `filelist` | Verilator | regenerate the simulator filelist |
| `stage` | Verilator | openllc only: rsync `build/$DUT/` → `build/${DUT}_staged/` |
| `build` | Verilator | configure & build the `cohestra_v3` executable (long) |
| `run` | Verilator | smoke run (openllc: FuzzARI traffic via `$CONFIG`) |
| `fuzz` | Verilator | traffic fuzz run (not openllc — its `run` already is) |
| `all` | — | the whole Verilator chain (default; never includes VCS) |
| `vcs-filelist` | VCS | generate the VCS filelist (openllc/zhujiang) |
| `vcs-build` | VCS | build harness archive + `simv` (long; openllc/zhujiang include `vcs-filelist`) |
| `vcs-run` | VCS | smoke run (openllc/zhujiang: idle derived config; mesh: boot script) |
| `vcs-fuzz` | VCS | traffic fuzz run |
| `vcs-smoke` | VCS | mesh DUTs only: VCS go/no-go preflight |
| `vcs-all` | — | `vcs-build` + `vcs-run` |

### Common environment knobs

| knob | default | meaning |
|---|---|---|
| `CHIRON` / `CHIRON_OVERRIDE` | `test/CHIron` | CHIron source tree (see below) |
| `CYCLES` | per stage | run budget in harness ticks (VCS: 2 ticks/clock) |
| `NUM_L2` | openllc 1, zhujiang 4 | L2 count; for VCS it must match the built RTL |
| `NUM_SLICE` | 2 | slices per L2 for the RTL generator |
| `NO_PERF` | 1 | 1 = strip DUT perf counters (faster builds) |
| `SOAK_NOASSERT` | 0 | 1 = no-op firtool assertions (soak escape hatch only) |
| `CLOGB` / `CLOGB_FILE` | 0 / `<dut>.clogb` | CLog.B export via derived config (mesh + zhujiang) |
| `VL_THREADS` / `THREADS` | 0 | Verilator `--threads` (mesh/zhujiang / openllc) |
| `BUILD_JOBS` | `nproc` | cmake build parallelism |
| `BUILD_DIR` / `VCS_BUILD_DIR` | per DUT | harness build directories |
| `EXTRA_MAKE_ARGS` / `EXTRA_CMAKE_ARGS` | empty | openllc: appended to the RTL make / harness cmake lines |
| `VCS_HOME` / `VERDI_HOME` | auto | VCS/Verdi roots (validated; vcs-* stages only) |

## Per-DUT drivers

### openllc — `test/scripts/openllc/boot_openllc.sh`

DUT: `TestTop_L2OpenLLC` (NUM_L2× Oceanus L2 → OpenLLC → OpenNCB → AXI4).
`DUT=l2multichi` selects `TestTop_L2MultiCHI` (Verilator only). Build dirs:
`build/cohestra_v3[_l2multichi]` (Verilator), `build/cohestra_vcs_l2openllc`
(VCS). Stages: `rtl stage build run all | vcs-filelist vcs-build vcs-run
vcs-fuzz vcs-all`. Knobs: `CONFIG` (Verilator config, default
`configs/config.xscache.ini`), `VCS_CONFIG` (default
`configs/config.xscache_vcs_openllc.ini`), `WAVEFORM=FST|VCD`, `SKIP_RUN=1`
(stop after the harness build), `THREADS`, `VERILATOR_BIN`. The VCS wrapper
define set comes from `NUM_L2` (1..4) and **must match the staged RTL**.

```sh
test/boot.sh                                  # = openllc all (legacy default)
test/boot.sh openllc --skip-rtl               # reuse RTL: stage+build+run
test/boot.sh openllc rtl stage build          # rebuild only, NO_PERF=0:
NO_PERF=0 test/boot.sh openllc rtl stage build
NUM_L2=4 test/boot.sh openllc vcs-build vcs-run        # VCS idle smoke (4 L2)
NUM_L2=4 test/boot.sh openllc vcs-fuzz                 # VCS FuzzARI traffic
DUT=l2multichi test/boot.sh                   # legacy multichi Verilator flow
```

### zhujiang — `test/scripts/zhujiang/boot_zhujiang.sh`

DUT: `TestTop_L2ZhuJiang` (NUM_L2× Oceanus L2 → ZhuJiang CHI interconnect →
SN AXI4). Build dirs: `build/cohestra_v3_zhujiang`, `build/cohestra_vcs_zhujiang`.
Stages: `rtl lint filelist build run fuzz all | vcs-filelist vcs-build vcs-run
vcs-fuzz vcs-all`. Knobs: `NUM_L2=1..4` (default 4 — wrapper defines AND the
`lint` contract expect the matching RTL; keep 4 for the stock flow),
`CCHI_NIDS`, `CHI_MON`, `VCS_CONFIG` (default
`configs/config.xscache_vcs_zhujiang.ini`). `run`/`fuzz` support `CLOGB=1`
(node IDs from `build/zhujiang/zj_topology.txt`).

```sh
test/boot.sh zhujiang all                     # regen + lint + build + idle run
test/boot.sh zhujiang build run               # reuse RTL
test/boot.sh zhujiang fuzz                    # ARI traffic smoke
test/boot.sh zhujiang vcs-build vcs-run       # VCS build + idle smoke
test/boot.sh zhujiang vcs-fuzz                # VCS FuzzARI traffic
CLOGB=1 test/boot.sh zhujiang vcs-fuzz        #   with CLog.B export
NUM_L2=2 test/boot.sh zhujiang rtl lint       # regenerate a 2-L2 DUT
```

### avocado — `test/scripts/avocado/boot_avocado.sh`

DUT: `TestTop_L2Avocado` (4× Oceanus L2 inside the CHIron Avocado package,
backed by the external 3x2 CMN-700 mesh). Requires
`AVOCADO_MESH_ROOT=<mesh package with the Verilator patch applied>` for
`filelist`/`build`/`vcs-build` (e.g. `/path/to/Mesh32_Avocado`). Build dirs:
`build/cohestra_v3_l2avocado`, `build/cohestra_vcs_l2avocado`. Stages:
`rtl lint filelist build run fuzz all | vcs-smoke vcs-build vcs-run vcs-fuzz
vcs-all`. Extra knob: `CG_OPEN=1` (forced-open ICG override diagnostic).

```sh
AVOCADO_MESH_ROOT=/path/to/Mesh32_Avocado test/boot.sh avocado all
AVOCADO_MESH_ROOT=/path/to/Mesh32_Avocado test/boot.sh avocado build run
AVOCADO_MESH_ROOT=/path/to/Mesh32_Avocado test/boot.sh avocado fuzz
AVOCADO_MESH_ROOT=/path/to/Mesh32_Avocado test/boot.sh avocado vcs-build vcs-run
AVOCADO_MESH_ROOT=/path/to/Mesh32_Avocado CLOGB=1 test/boot.sh avocado vcs-fuzz
```

### cornetto — `test/scripts/cornetto/boot_cornetto.sh`

DUT: `TestTop_L2Cornetto` (2× Oceanus L2, CHIron Cornetto package, external
1x3 mesh). Same stage set and conventions as avocado, with
`CORNETTO_MESH_ROOT` (e.g. `/path/to/Mesh13_Cornetto`). Build dirs:
`build/cohestra_v3_l2cornetto`, `build/cohestra_vcs_l2cornetto`. The VCS flow
uses the `cornetto_vcs_top.sv` adapter (2×T1 + 1×AXI-M onto the Avocado-style
bridge, unused ports parked; `COHESTRA_VCS_CHI_MON_PORTS=2`).

```sh
CORNETTO_MESH_ROOT=/path/to/Mesh13_Cornetto test/boot.sh cornetto all
CORNETTO_MESH_ROOT=/path/to/Mesh13_Cornetto test/boot.sh cornetto fuzz
CORNETTO_MESH_ROOT=/path/to/Mesh13_Cornetto test/boot.sh cornetto vcs-build vcs-run
```

### biscotto — `test/scripts/biscotto/boot_biscotto.sh`

DUT: `TestTop_L2Biscotto` (3× Oceanus L2, CHIron Biscotto package, external
2x2 mesh). Same stage set as avocado/cornetto, with `BISCOTTO_MESH_ROOT`
(e.g. `/path/to/Mesh22_Biscotto`). Adds content-stable RTL staging
(`build/l2biscotto_staged`) and a flow-private filelist
(`biscotto_top_l2biscotto.f`, overridable via `TOP_F`). Its VCS stages are
carried over for parity and were historically untested — treat red results
there as pre-existing unless the Verilator flow is also red.

```sh
BISCOTTO_MESH_ROOT=/path/to/Mesh22_Biscotto test/boot.sh biscotto all
BISCOTTO_MESH_ROOT=/path/to/Mesh22_Biscotto test/boot.sh biscotto fuzz
BISCOTTO_MESH_ROOT=/path/to/Mesh22_Biscotto test/boot.sh biscotto vcs-build vcs-run
```

## configs/

`config.xscache*.ini` — the cohestra run configs consumed by the stages above
(harness-agnostic INI; `[verilated] waveform.type` must match the Verilator
build's `WAVEFORM`). Notable: `config.xscache.ini` (default openllc 1-L2
FuzzARI smoke), `config.xscache_2l2.ini`, `config.xscache_multichi*.ini`,
`config.xscache_{avocado,cornetto,biscotto,zhujiang}{,_fuzz}.ini`,
`config.xscache_vcs_{openllc,zhujiang}.ini` (VCS; `waveform.type=fsdb`).
`CLOGB=1` runs derive `*.clogb.ini` copies into the build dir; idle VCS runs
derive `*_idle.ini` (stimulator.0 disabled) — generated artifacts, not repo
files.

## CHIRON / CHIRON_OVERRIDE — use another CHIron source tree

By default everything builds against the in-repo submodule `test/CHIron`
(`CHIRON` wins over the legacy `CHIRON_OVERRIDE` alias when both are set):

```sh
CHIRON=/path/to/CHIron test/boot.sh zhujiang build run
```

The override tree is used strictly **read-only**: no submodule init, fetch, or
any other git action is taken on it. (Git bootstrap only ever runs against the
default in-repo submodule.)

### Updating CHIron

```sh
git -C test/CHIron fetch origin main
git -C test/CHIron merge --ff-only origin/main
git -C test/CHIron submodule update --init --recursive
git add test/CHIron   # stage the gitlink
```

## Exit codes

`0` = FINISHED (pass), `1` = usage/config error, `2` = verification failure
(instance FAILED or cache-line data mismatch). Every run prints the legend.

## Legacy invocation migration

The pre-consolidation env-style calls keep working: bare `test/boot.sh`
(= `openllc all`), `test/boot.sh --skip-rtl` (= `openllc stage build run`),
`DUT=l2multichi test/boot.sh`, and every env knob (`CONFIG`, `CYCLES`,
`NUM_L2`, `CHIRON_OVERRIDE`, …) passes through to the drivers unchanged.
`test/config/` is now `test/configs/`; the old per-simulator zhujiang/openllc
VCS scripts are merged into the single per-DUT drivers under `test/scripts/`.
