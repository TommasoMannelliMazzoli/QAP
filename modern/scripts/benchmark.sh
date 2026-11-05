#!/usr/bin/env bash
# Runs algorithms on instances; one CSV row per run.
#   [BKS_FILE=...] [INSTANCES=...] scripts/benchmark.sh SECONDS RUNS JOBS OUTPUT.csv [ALGORITHM...]
# The instances are those of BKS_FILE (default data/bks_repository.txt) that
# have a file NAME.dat in the directory INSTANCES (default ../Instancies).
# Run k uses random stream k. JOBS runs execute at the same time: keep it
# at most the number of physical performance cores, the budget is wall-clock.
# An interrupted benchmark resumes: the runs already in OUTPUT are kept and
# not repeated.
set -euo pipefail
cd "$(dirname "$0")/.."

if [ $# -lt 4 ]; then
  echo "usage: $0 SECONDS RUNS JOBS OUTPUT.csv [ALGORITHM...]" >&2
  exit 2
fi
seconds=$1 runs=$2 jobs=$3 output=$4
shift 4
algorithms=${*:-ts aco gvns-first gvns-best rots has}
solver=build/gnu-release/qap_solve
bks_file=${BKS_FILE:-data/bks_repository.txt}
instances=${INSTANCES:-../Instancies}

# "name algorithm stream" for every run of the benchmark
all_runs() {
  local name algorithm run
  for name in $(cut -d' ' -f1 "$bks_file"); do
    [ -f "$instances/$name.dat" ] || continue
    for algorithm in $algorithms; do
      for run in $(seq 1 "$runs"); do
        echo "$name $algorithm $run"
      done
    done
  done
}

# the runs whose instance, algorithm and stream are not yet in the output
pending() {
  awk -F, 'NR == FNR { if (FNR > 1) done[$1 " " $3 " " $5] = 1; next } !($0 in done)' "$output" -
}

[ -s "$output" ] ||
  echo 'instance,n,algorithm,run,stream,cost,bks,gap_percent,time_to_best,elapsed,iterations' > "$output"
all_runs | pending | xargs -r -P "$jobs" -L 1 sh -c \
  '"$0" "$3/$4.dat" --algorithm "$5" --seed "$6" --time "$1" --bks-file "$2" 2>/dev/null' \
  "$solver" "$seconds" "$bks_file" "$instances" >> "$output"
echo "$(($(wc -l < "$output") - 1)) runs in $output"
