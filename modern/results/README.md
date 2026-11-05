# Results

`thesis-60s.csv` is the protocol of the thesis' final table: the 41 instances
of `data/thesis_instances.txt`, the algorithms `ts aco gvns-first gvns-best
rots has`, 5 runs of 60 s each (run k uses random stream k), BKS from
`data/bks_repository.txt`. Rows are sorted by instance, algorithm and stream.

It was produced on 2026-11-03 with

```bash
./build.sh
INSTANCES=data/qaplib scripts/benchmark.sh 60 5 8 build/thesis-60s.csv
```

on an Intel Core i9-14900K (8 performance cores, 8 runs at a time), Debian 13
under WSL2, gfortran 14.2.0 (`-O3 -march=native -flto=auto -fopenmp`). The PC
went to sleep after 590 runs; the benchmark was resumed with the same binary.

The GVNS stop after the 10000 loops of the thesis, often before 60 s (column
`elapsed`). The thesis tabu search can exceed the limit by up to about 1 s on
the largest instances (a Greedy3 restart is not interrupted); no run found its
best solution after 60 s.

`scripts/summarise.sh thesis-60s.csv`:

| algorithm | solved | mean gap | worst gap | time to best |
|---|---|---|---|---|
| rots | 30 / 41 | 0.011% | 0.112% | 6.3 s |
| aco | 30 / 41 | 0.027% | 0.223% | 8.9 s |
| has | 30 / 41 | 0.051% | 0.473% | 3.5 s |
| gvns-first | 26 / 41 | 0.259% | 1.571% | 13.6 s |
| ts | 23 / 41 | 0.389% | 1.661% | 14.4 s |
| gvns-best | 16 / 41 | 0.735% | 2.479% | 10.5 s |

`scripts/best_gaps.sh thesis-60s.csv` prints the best gap of each instance
and algorithm, as in the thesis' table.
