#!/usr/bin/env bash
# xscache_vcs_top_defines.sh <family> <num_l2> — echo the +define+ tokens that
# configure test/scripts/common/xscache_vcs_top.sv for a build: the family
# selector (zhujiang => +define+XSCACHE_VCS_TOP_ZHUJIANG; openllc => none)
# plus the
# per-L2-index presence defines +define+XSCACHE_VCS_TOP_L2_P1..P(n-1) (index 0
# is unconditional). Consumed via EXTRA_VCS_FLAGS by boot_openllc.sh /
# boot_zhujiang.sh (VCS applies command-line +define+ compilation-wide
# regardless of its position on the vcs command line).
set -euo pipefail

fam="${1:?usage: $0 <openllc|zhujiang> <num_l2 1..4>}"
n="${2:?usage: $0 <openllc|zhujiang> <num_l2 1..4>}"

case "$fam" in
    openllc)  out="" ;;
    zhujiang) out="+define+XSCACHE_VCS_TOP_ZHUJIANG" ;;
    *) echo "xscache_vcs_top_defines: unknown family: $fam" >&2; exit 2 ;;
esac
case "$n" in
    1|2|3|4) ;;
    *) echo "xscache_vcs_top_defines: NUM_L2 must be 1..4 (got $n)" >&2; exit 2 ;;
esac
for ((i=1; i<n; i++)); do
    out="$out +define+XSCACHE_VCS_TOP_L2_P$i"
done
echo "${out# }"
