# Stress HN extension

`TestTop_L2PrefetchStress --hn-extension=true` uses the shared HN model imported from hn-ext-wip 8451de3d, with the stress memory pattern and real CHI link credit trackers. The default legacy scheduler remains available with `--hn-extension=false`.

Retry is fixed off in this suite. Reads select split response/data at 50%; nonsplit reads select peer data at 50%. Forwarding snoops also exercise the L2 as a data source to an independent virtual RN. HN closure waits for the Home response and target CompAck. Invalid-line forwarding fallbacks are counted separately from actual two-beat DCT. `--hn-dct=false`, `--hn-split=false` and `--cdp-enable=false` support separate ablations.

Stress profile memory delay bounds, minimum data beat gap, data-period, credits and TLB settings remain in effect. Split response/data adds its own 0..512-cycle gap and randomized beats. The HN's RNG and mixed SNP opcodes change the transaction schedule, so equal seeds are paired stress conditions rather than fixed transaction replay.

`STRESS_HN_EXT` reports actual interface counts and closure. `STRESS_HN_OVERLAP` reports same-64B-line inclusive 512-cycle event windows. CDP-emitted is the algorithm handshake; CDP-issued is a real downstream request attributed from the issuing TSHR's saved SrcID and decoded prefetch source. Actual forward DCT completion, requester DCT, split response, ordinary SNP and Stash SNP are separate events. Every window count and line-cycle sum is checked against an independent interval scan. Event proximity does not imply simultaneous outstanding transactions or prefetch usefulness.

Only successful full-drain cases establish closure. Failed cases retain their actual watchdog/assertion status and prefix counters. The bounded final 512 HN trace records assist diagnosis. `TestHNExtensionModel` covers shared host protocol negative checks; `TestStressHNIntegration` checks long-run SNP identity reuse, real beat spacing, and window boundaries. These host tests are not RTL functional coverage.
