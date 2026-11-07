#!/usr/bin/env bash
# Instance x algorithm table of the best gap (%) over the runs, the figure
# of the thesis' final table.
#   scripts/best_gaps.sh RESULTS.csv [INSTANCE_PREFIX] [ALGORITHM...]
set -euo pipefail
results=$1
prefix=${2:-}
shift $(( $# >= 2 ? 2 : 1 ))
columns=${*:-ts aco gvns-first gvns-best rots has}

awk -F, -v prefix="$prefix" -v columns="$columns" '
  # with BKS = 0 (esc16f) the relative gap is undefined: show the cost
  function cell(key) {
    if (!(key in best)) return "-"
    if (gap[key] == "") return "z=" best[key]
    return sprintf("%.2f", gap[key])
  }
  NR == 1 || index($1, prefix) != 1 { next }
  {
    key = $1 SUBSEP $3
    if (!(key in best) || $6 < best[key]) { best[key] = $6; gap[key] = $8 }
    if (!($1 in seen)) { seen[$1] = 1; order[++instances] = $1 }
  }
  END {
    count = split(columns, algorithm, " ")
    printf "%-10s", "instance"
    for (c = 1; c <= count; c++) printf " %10s", algorithm[c]
    printf "\n"
    for (i = 1; i <= instances; i++) {
      printf "%-10s", order[i]
      for (c = 1; c <= count; c++) {
        key = order[i] SUBSEP algorithm[c]
        printf " %10s", cell(key)
      }
      printf "\n"
    }
  }' "$results"
