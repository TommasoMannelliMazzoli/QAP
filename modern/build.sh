#!/usr/bin/env bash
# Builds qap_solve, qap_evaluate and test_qap into build/<compiler>-<mode>/.
#   ./build.sh [release|debug|sanitize]          with gfortran
#   FC=flang-new-19 ./build.sh [release|debug]   with LLVM flang
#   OPENMP=no ./build.sh ...                     sequential runs, no OpenMP runtime needed
# release:  -O3 -march=native
# debug:    bounds checking and floating-point traps (gfortran)
# sanitize: AddressSanitizer + UndefinedBehaviorSanitizer (gfortran, Linux)
set -euo pipefail
cd "$(dirname "$0")"

FC=${FC:-gfortran}
MODE=${1:-release}
OPENMP_FLAG=$([ "${OPENMP:-yes}" = no ] || echo -fopenmp)
MODULES=(qap_rng qap_instance qap_solution qap_moves qap_run qap_params qap_greedy
         qap_local_search qap_vns qap_tabu qap_colony qap_annealing qap_algorithms qap_cli)

compiler_family() {
  case "$("$FC" --version 2>&1 | head -1)" in
    *flang*)        echo flang ;;
    *GNU\ Fortran*) echo gnu ;;
    *)              echo "unsupported compiler: $FC" >&2; exit 2 ;;
  esac
}

gnu_flags() {
  local common="-std=f2018 -fimplicit-none -Wall -Wextra -Wno-unused-dummy-argument $OPENMP_FLAG"
  case "$(uname -s)" in
    MINGW*|MSYS*|CYGWIN*) common="$common -static" ;;   # no runtime DLLs needed on Windows
  esac
  case "$MODE" in
    release)  echo "$common -O3 -march=native -flto=auto" ;;
    debug)    echo "$common -O0 -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow" ;;
    sanitize) echo "$common -O1 -g -fcheck=all -fsanitize=address,undefined -fno-omit-frame-pointer" ;;
    *)        usage ;;
  esac
}

flang_flags() {
  local common="-std=f2018 -fimplicit-none $OPENMP_FLAG"
  case "$MODE" in
    release) echo "$common -O3 -march=native" ;;
    debug)   echo "$common -O0 -g" ;;
    *)       usage ;;
  esac
}

usage() {
  echo "usage: [FC=compiler] $0 [release|debug|sanitize]" >&2
  exit 2
}

FAMILY=$(compiler_family)
FLAGS=$("${FAMILY}_flags")
OUT="build/$FAMILY-$MODE"
mkdir -p "$OUT"

objects=()
for module in "${MODULES[@]}"; do
  $FC $FLAGS -J "$OUT" -c "src/$module.f90" -o "$OUT/$module.o"
  objects+=("$OUT/$module.o")
done
for program in app/qap_solve app/qap_evaluate test/test_qap; do
  $FC $FLAGS -J "$OUT" "$program.f90" "${objects[@]}" -o "$OUT/$(basename "$program")"
done
echo "built qap_solve, qap_evaluate and test_qap in $OUT"
