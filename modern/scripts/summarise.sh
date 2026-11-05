#!/usr/bin/env bash
# Summarises a benchmark CSV: per algorithm, on how many instances the best
# of the runs reached the BKS, the mean and worst gap of the runs, and the
# mean time to the best solution. Instances with BKS 0 are left out of the
# gaps (a relative gap is undefined) but counted when they are solved.
#   scripts/summarise.sh RESULTS.csv [INSTANCE_PREFIX]
set -euo pipefail
results=$1
prefix=${2:-}

awk -F, -v prefix="$prefix" '
  NR == 1 || index($1, prefix) != 1 { next }
  {
    key = $3 SUBSEP $1
    if (!(key in best) || $6 < best[key]) best[key] = $6
    bks[$1] = $7
    algorithms[$3] = 1
    runs[$3]++
    time_sum[$3] += $9
    if ($8 != "") {
      gap_sum[$3] += $8; gap_runs[$3]++
      if ($8 > worst[$3]) worst[$3] = $8
    }
  }
  END {
    printf "%-11s %10s %9s %9s %12s\n", "algorithm", "solved", "mean gap", "worst gap", "time to best"
    for (a in algorithms) {
      solved = 0; total = 0
      for (key in best) {
        split(key, part, SUBSEP)
        if (part[1] != a) continue
        total++
        if (best[key] == bks[part[2]]) solved++
      }
      printf "%-11s %4d / %-3d %8.3f%% %8.3f%% %11.3fs\n", a, solved, total,
             (gap_runs[a] ? gap_sum[a] / gap_runs[a] : 0), worst[a], time_sum[a] / runs[a]
    }
  }' "$results" | { read -r header; echo "$header"; sort; }
