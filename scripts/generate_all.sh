#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/.." && pwd)
parser="$repo_root/scripts/parser.py"
output_dir="$repo_root/gen"
work_dir=""

usage() {
  cat <<EOF
Usage: ${0##*/} (--coupledl2 | --oceanus) [--tshr N]... [Oceanus options]

Generate and package one CoupledL2 or Oceanus L2 release RTL configuration.

  --coupledl2
            Generate CoupledL2 only.
  --oceanus Generate Oceanus L2 only.
  --tshr N  Generate Oceanus L2 with N TSHRs per slice. Repeat for multiple
            configurations. Defaults to 32; N must be at least 4. Only valid
            with --oceanus.
  --l2-drrip-use-sram <0|1>
            Use the RRIP SRAM+CAM backend (1, default) or FF backend (0).
  --l2-origin-use-sram <0|1>
            Use origin SRAM (1, default) or origin FF (0).
  --l2-drrip-release-update <0|1>
            Enable DRRIP release updates through DirRel (1, default).
  --l2-dirrel-arbitration <mode>
            Select dirrel-high, rr (default), or dirrel-low arbitration.
  --l2-dirrel-single-cycle <0|1>
            Enable single-cycle DirRel (0, default); requires RRIP SRAM and
            origin FF. All --l2-* options are only valid with --oceanus.
  -h, --help
            Show this help.

Examples:
  ${0##*/} --oceanus
  ${0##*/} --coupledl2
  ${0##*/} --oceanus --tshr 48 --tshr 64
  ${0##*/} --oceanus --l2-origin-use-sram 0 --l2-dirrel-single-cycle 1

Releases and tar.gz archives are written to $output_dir.
EOF
}

check_destination() {
  local name=$1
  if [[ -e "$output_dir/$name" || -e "$output_dir/$name.tar.gz" ]]; then
    printf 'Refusing to overwrite existing release: %s\n' "$output_dir/$name" >&2
    exit 1
  fi
}

package_release() {
  local top=$1
  local config=$2
  local rtl_dir=$3
  local log=$4
  local stage="$work_dir/package/$top"
  local release name destination archive temporary_archive
  local releases=()

  if [[ ! -f "$rtl_dir/$top.sv" ]]; then
    printf 'Expected generated top is missing: %s\n' "$rtl_dir/$top.sv" >&2
    return 1
  fi

  mkdir -p "$stage/home/build" "$stage/output"
  printf 'Packaging %s (%s)\n' "$top" "$config"
  (
    cd "$stage/output"
    python3 "$parser" "$top" \
      --config "$config" \
      --prefix bosc_ \
      --sram-replace \
      --xs-home "$stage/home" \
      --include "$rtl_dir"
  ) >>"$log" 2>&1

  releases=("$stage/output/bosc_${top}-Release-${config}-"*)
  if (( ${#releases[@]} != 1 )) || [[ ! -d "${releases[0]}" ]]; then
    printf 'Could not identify packaged release in %s\n' "$stage/output" >&2
    return 1
  fi

  release=${releases[0]}
  name=${release##*/}
  destination="$output_dir/$name"
  archive="$destination.tar.gz"
  temporary_archive="$output_dir/.$name.tar.gz.tmp.$$"
  check_destination "$name"

  mv -- "$log" "$release/generate_$top.log"
  mv -- "$release" "$destination"
  if ! tar -czf "$temporary_archive" -C "$output_dir" "$name"; then
    rm -f -- "$temporary_archive"
    return 1
  fi
  if [[ -e "$archive" ]]; then
    rm -f -- "$temporary_archive"
    printf 'Refusing to overwrite existing archive: %s\n' "$archive" >&2
    return 1
  fi
  mv -- "$temporary_archive" "$archive"
  printf 'Created %s\nCreated %s\n' "$destination" "$archive"
}

cleanup() {
  local status=$?
  if [[ -n "$work_dir" && -d "$work_dir" ]]; then
    if (( status == 0 )) && [[ "$work_dir" == "$repo_root"/build/generate_all.* ]]; then
      rm -rf -- "$work_dir"
    else
      printf 'Generation workspace retained for diagnosis: %s\n' "$work_dir" >&2
    fi
  fi
}
trap cleanup EXIT

target=""
tshrs=()
oceanus_options_seen=false
l2_drrip_use_sram=1
l2_origin_use_sram=1
l2_drrip_release_update=1
l2_dirrel_arbitration=rr
l2_dirrel_single_cycle=0
while (( $# > 0 )); do
  case "$1" in
    --coupledl2|--oceanus)
      if [[ -n "$target" ]]; then
        printf 'Only one cache selector may be specified\n' >&2
        usage >&2
        exit 2
      fi
      target=${1#--}
      shift
      ;;
    --tshr)
      if (( $# < 2 )); then
        printf 'Missing value for --tshr\n' >&2
        usage >&2
        exit 2
      fi
      tshrs+=("$2")
      shift 2
      ;;
    --l2-drrip-use-sram|--l2-origin-use-sram|--l2-drrip-release-update|--l2-dirrel-single-cycle)
      if (( $# < 2 )); then
        printf 'Missing value for %s\n' "$1" >&2
        usage >&2
        exit 2
      fi
      if [[ "$2" != 0 && "$2" != 1 ]]; then
        printf 'Invalid value for %s (expected 0 or 1): %s\n' "$1" "$2" >&2
        exit 2
      fi
      oceanus_options_seen=true
      case "$1" in
        --l2-drrip-use-sram) l2_drrip_use_sram=$2 ;;
        --l2-origin-use-sram) l2_origin_use_sram=$2 ;;
        --l2-drrip-release-update) l2_drrip_release_update=$2 ;;
        --l2-dirrel-single-cycle) l2_dirrel_single_cycle=$2 ;;
      esac
      shift 2
      ;;
    --l2-dirrel-arbitration)
      if (( $# < 2 )); then
        printf 'Missing value for --l2-dirrel-arbitration\n' >&2
        usage >&2
        exit 2
      fi
      case "$2" in
        dirrel-high|rr|dirrel-low) l2_dirrel_arbitration=$2 ;;
        *)
          printf 'Invalid DirRel arbitration mode: %s\n' "$2" >&2
          exit 2
          ;;
      esac
      oceanus_options_seen=true
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown argument: %s\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -z "$target" ]]; then
  printf 'Exactly one of --coupledl2 or --oceanus must be specified\n' >&2
  usage >&2
  exit 2
fi
if [[ "$target" == coupledl2 ]] && (( ${#tshrs[@]} > 0 )); then
  printf '%s\n' '--tshr is only valid with --oceanus' >&2
  usage >&2
  exit 2
fi
if [[ "$target" == coupledl2 && "$oceanus_options_seen" == true ]]; then
  printf '%s\n' 'Oceanus --l2-* options are only valid with --oceanus' >&2
  usage >&2
  exit 2
fi
if [[ "$target" == oceanus ]] && (( ${#tshrs[@]} == 0 )); then
  tshrs=(32)
fi
if (( l2_dirrel_single_cycle == 1 )) &&
    (( l2_drrip_use_sram == 0 || l2_origin_use_sram == 1 )); then
  printf '%s\n' 'Single-cycle DirRel requires --l2-drrip-use-sram 1 and --l2-origin-use-sram 0' >&2
  exit 2
fi

normalized_tshrs=()
declare -A selected_tshrs=()
for tshr in "${tshrs[@]}"; do
  if [[ ! "$tshr" =~ ^[0-9]+$ ]]; then
    printf 'Invalid TSHR count (expected a nonnegative integer): %s\n' "$tshr" >&2
    exit 2
  fi
  tshr=$((10#$tshr))
  if (( tshr < 4 )); then
    printf 'Invalid TSHR count (must be at least 4): %s\n' "$tshr" >&2
    exit 2
  fi
  if [[ -n "${selected_tshrs[$tshr]+selected}" ]]; then
    printf 'Duplicate TSHR count: %s\n' "$tshr" >&2
    exit 2
  fi
  selected_tshrs[$tshr]=1
  normalized_tshrs+=("$tshr")
done
tshrs=("${normalized_tshrs[@]}")

oceanus_variant=()
if (( l2_drrip_use_sram == 0 )); then
  oceanus_variant+=(ff)
elif (( l2_origin_use_sram == 0 )); then
  oceanus_variant+=(orgff)
fi
if (( l2_dirrel_single_cycle == 1 )); then
  oceanus_variant+=(sc)
fi
if (( l2_drrip_release_update == 0 )); then
  oceanus_variant+=(norel)
fi
if [[ "$l2_dirrel_arbitration" != rr ]]; then
  oceanus_variant+=("$l2_dirrel_arbitration")
fi
oceanus_variant_suffix=""
for descriptor in "${oceanus_variant[@]}"; do
  oceanus_variant_suffix+="-$descriptor"
done

for command in make mill python3 tar mktemp date; do
  if ! command -v "$command" >/dev/null 2>&1; then
    printf 'Required command not found: %s\n' "$command" >&2
    exit 1
  fi
done
if ! python3 -c 'import xlsxwriter' >/dev/null 2>&1; then
  printf 'Required Python module not found: xlsxwriter\n' >&2
  exit 1
fi
if [[ ! -f "$parser" ]]; then
  printf 'Parser not found: %s\n' "$parser" >&2
  exit 1
fi

release_date=$(date +%b-%d-%Y)
if [[ "$target" == coupledl2 ]]; then
  check_destination "bosc_TopCoupledL2-Release-DefaultConfig_sram-$release_date"
else
  for tshr in "${tshrs[@]}"; do
    config="DefaultConfig-T${tshr}${oceanus_variant_suffix}_sram"
    check_destination "bosc_Top_L2Top-Release-${config}-$release_date"
  done
fi

mkdir -p "$repo_root/build" "$output_dir"
work_dir=$(mktemp -d "$repo_root/build/generate_all.XXXXXX")
printf 'Generation workspace: %s\n' "$work_dir"

if [[ "$target" == coupledl2 ]]; then
  rtl_dir="$work_dir/rtl/coupledl2"
  log="$work_dir/generate_TopCoupledL2.log"
  printf 'Generating CoupledL2 release RTL\n'
  make -C "$repo_root" gen-top-coupledl2 BUILD_DIR_COUPLEDL2_REL="$rtl_dir" 2>&1 | tee "$log"
  package_release TopCoupledL2 DefaultConfig_sram "$rtl_dir" "$log"
else
  for tshr in "${tshrs[@]}"; do
    config="DefaultConfig-T${tshr}${oceanus_variant_suffix}_sram"
    rtl_dir="$work_dir/rtl/l2top_t${tshr}${oceanus_variant_suffix}"
    log="$work_dir/generate_Top_L2Top-T${tshr}${oceanus_variant_suffix}.log"
    printf 'Generating Oceanus L2 release RTL with %s TSHRs per slice (%s)\n' "$tshr" "$config"
    make -C "$repo_root" gen-top-l2top \
      TSHR="$tshr" L2_SLICES=4 TOP_L2TOP_PERF=0 \
      L2_DRRIP_USE_SRAM="$l2_drrip_use_sram" \
      L2_ORIGIN_USE_SRAM="$l2_origin_use_sram" \
      L2_DRRIP_RELEASE_UPDATE="$l2_drrip_release_update" \
      L2_DIRREL_ARBITRATION="$l2_dirrel_arbitration" \
      L2_DIRREL_SINGLE_CYCLE="$l2_dirrel_single_cycle" \
      BUILD_DIR_TOP_L2TOP="$rtl_dir" 2>&1 | tee "$log"
    package_release Top_L2Top "$config" "$rtl_dir" "$log"
  done
fi
