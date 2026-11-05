# QAP heuristics in modern Fortran

A reimplementation of the algorithms of the master's thesis (see the
repository root) in standard Fortran 2018, with the defects of the original
code fixed, plus two reference metaheuristics from the literature.

## Build and test

Requirements: gfortran >= 14 or LLVM flang >= 19. No external libraries;
OpenMP (optional) runs independent runs in parallel.

```bash
./build.sh                       # gfortran, optimised  -> build/gnu-release/
./build.sh debug                 # bounds checking, floating-point traps
./build.sh sanitize              # AddressSanitizer + UBSan (Linux)
FC=flang-new-19 OPENMP=no ./build.sh
build/gnu-release/test_qap ../Instancies
```

The tests check, among other things, that every move delta equals the cost
difference, that the O(n^2) delta-table update stays exact, that every
algorithm reports the true cost of the permutation it returns, and that the
metaheuristics find the optimum of small instances (checked by enumeration).

## Run

```bash
build/gnu-release/qap_solve ../Instancies/bur26a.dat --algorithm rots --time 60 \
    --runs 5 --threads 5 --bks-file data/bks_repository.txt --header
```

One CSV row per run:
`instance,n,algorithm,run,stream,cost,bks,gap_percent,time_to_best,elapsed,iterations`.
Run k uses random stream `seed + k - 1` (MRG32k3a, exact integer arithmetic),
so a run is reproducible on any compiler and machine. The time limit is
wall-clock: use at most one run per physical performance core.

Parameters (defaults in `src/qap_params.f90`) can be overridden with
`--params FILE`, a namelist such as

```
&qap_params
  params%aco%ants = 5
  params%rots%aspiration_factor = 2.0
/
```

`scripts/benchmark.sh` runs several algorithms on the instances of the BKS
file found in a directory; an interrupted benchmark resumes where it stopped.
`scripts/summarise.sh` summarises its CSV and `scripts/best_gaps.sh` prints
the instance x algorithm table of the best gaps, as in the thesis.

The protocol of the thesis' final table (41 QAPLIB instances, best of 5 runs
of 60 s):

```bash
scripts/fetch_qaplib.sh data/qaplib $(cat data/thesis_instances.txt)
for name in $(cat data/thesis_instances.txt); do
  build/gnu-release/qap_evaluate data/qaplib/$name.dat data/qaplib/$name.sln
done
INSTANCES=data/qaplib scripts/benchmark.sh 60 5 8 results.csv
scripts/best_gaps.sh results.csv
```

`qap_evaluate` checks that the QAPLIB solution of an instance has the stated
cost, which validates both the instance reader and the BKS. The results of
this protocol, and the machine they were obtained on, are in `results/`.

## Algorithms

| name | algorithm | start |
|---|---|---|
| `greedy1`, `greedy2`, `greedy3` | constructive heuristics of the thesis, every starting pair | - |
| `2opt-first`, `2opt-best`, `3opt-first`, `3opt-best` | one descent | random |
| `gvns-first`, `gvns-best` | GVNS of the thesis (VND on swaps and 3-cycles) | Greedy3 |
| `ts` | tabu search of the thesis (Skorin-Kapov style, long-term memory) | Greedy3 |
| `aco` | ant colony of the thesis (variant of HAS-QAP) | random |
| `rots` | robust tabu search, Taillard (1991) - reference | random |
| `has` | HAS-QAP, Gambardella, Taillard, Dorigo (1999) - reference | random |
| `sa` | simulated annealing (replaces the unused TEMPLE) | random |

## Differences from the original code

Defects fixed (the algorithms otherwise follow the thesis' description):

* **Tabu search**: the current cost was updated with the previous best delta,
  so the reported values were not the costs of the solutions found; the tabu
  list stored locations but was checked against positions; the long-term
  memory was computed and never used, so every restart produced the same
  permutation. The delta was truncated to an integer and the cost kept in
  single precision.
* **Greedy2**: the thesis takes the argmin, the code took the argmax.
* **ACO**: the ants were regenerated at random at every iteration; of the R
  pheromone-guided swaps only the last one survived; the stagnation counter
  was never reset, so after 5n iterations the trail was re-initialised at
  every iteration; the logical flag of the intensification was uninitialised.
  The thesis' diversification "T <- 0" would leave nothing to sample from:
  the trail is re-initialised instead.
* **Random indices**: `NINT((n-1)*u+1)` gave indices 1 and n half the
  probability of the others.
* **Random numbers**: NAG G05EHF is replaced by an internal generator with
  explicit streams.
* **Portability**: the original did not compile with standard compilers
  (18 errors with gfortran), e.g. `. AND.` with a blank, a function declared
  REAL and defined DOUBLE PRECISION, an assignment inside an IF.

Additions: Taillard's O(n^2) delta table for 2-opt best improvement and the
tabu searches; a perturbation of the thesis' tabu-search restart when the
memory does not change it (otherwise the same descent would repeat until the
time limit). 2-opt/3-opt first improvement keep scanning after an improving
move instead of restarting from the first pair.

## Instances

Both the QAPLIB layout (`n A B`) and the original one (`sim n A B`) are read;
symmetry is computed from the data. `esc32f.dat` declares n = 132: the size
implied by the data (32) is used, with a warning. `nug.dat` is not a valid
instance.

`data/bks_repository.txt` lists the best known value of the 135 QAPLIB
instances and whether it is proven optimal; `data/BKS_SOURCES.md` gives the
sources and the changes since the official table (2012).
`data/thesis_instances.txt` lists the 41 instances of the thesis' final table:
the QAPLIB solution file of each has the value of `bks_repository.txt`,
checked with `qap_evaluate` (`tho30.sln` and `tho150.sln` list the inverse
permutation).
