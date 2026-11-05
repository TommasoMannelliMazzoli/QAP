# QAPLIB best known solutions — snapshot of 2026-11-03

`bks_repository.txt` lists one instance per line as `name value status`:
`OPT` means the value is proven optimal, and `BKS` means it is the best known
upper bound. `qap_solve --bks-file` reads only the first two fields. The file
must not contain comment lines.

There are 135 instances: the 134 in the official QAPLIB table, plus `esc32f`.
107 are proven optimal and 28 are still open.

## Sources

- The official QAPLIB table at Lehigh:
  https://coral.ise.lehigh.edu/data-sets/qaplib/qaplib-problem-instances-and-solutions/.
  It was last updated in April 2012, so it does not include the changes below.
- H. Mittelmann, *Improved QAPLIB lower bounds*, page dated 2026-08-26:
  https://plato.asu.edu/ftp/qaplib_bounds.html. Use its lower bounds only. Its
  upper-bound column is outdated for tai100a and has a typo for tai60a (see below).
- Taillard's instance files, the Edinburgh DataShare QAPLIB archive (2022), and
  the GitHub mirrors mhahsler/qap and jeffrey-chr/ISA-QAP-Instances.
- A. Misevičius et al., *Mathematics* 12(23):3726 (2024), Table 2 (BKVs for 140
  instances). Its tai100a value is not the latest.

## Changes since the official table

| Instance | Official table | Now | Source |
|---|---|---|---|
| tai35b | 283315445 BKS | 283315445 **OPT** | Gonçalves, Pessoa, Bentes, Farias, Drummond, *INFORMS J. Computing* 29(4):676–687 (2017), doi:10.1287/ijoc.2017.0754 |
| tai40b | 637250948 BKS | 637250948 **OPT** | same paper |
| tai30a | 1818146 BKS | 1818146 **OPT** | Fujii, Ito, Kim, Kojima, Shinano, Toh, *Solving challenging large scale QAPs*, arXiv:2101.09629 (2021) |
| sko42 | 15812 BKS | 15812 **OPT** | same paper |
| tai100a | 21052466 BKS | **21040996** BKS | Sergienko, Shylo, Roshchyn, Shylo, Boyarchuk, Moroz, *Cybernetics and Computer Technologies* 2024(3):25–33, doi:10.34229/2707-451X.24.3.3 |
| esc32f | not listed | 2 OPT | Brüngger, Clausen, Marzetta, Perregaard (1996), QAPLIB news. Later dropped from the table. |

Other improvements to tai100a, in order:

| Value | Source |
|---|---|
| 21052466 | Misevičius, ITS (2005) |
| 21044752 | Shylo (2017) |
| 21043560 | Misevičius, *Memetic Computing* 11:331–332 (2019) |
| 21040996 | Sergienko et al. (2024) |

The 2019 and 2024 permutations were recomputed here on the official `tai100a.dat`.
Their costs match the published values exactly.

A 2026 paper by the same Kyiv group, Sergienko et al., *Cybernetics and Systems
Analysis* 62(4):578–587, doi:10.1007/s10559-026-00893-0, says it improved the
2024 record again. The new value is behind a paywall and could not be checked.

## Open instances

The lower bounds (LB) are Mittelmann's (2026-08-26). Gap = (BKS − LB) / BKS.
For sko100a, sko100c, sko100d and sko100e (marked †), the bound comes from his
NewtBracket log for that instance (https://plato.asu.edu/ftp/qaplib_bounds/).
The log gives a valid fractional bound, for example 147971.38 for sko100a.
The data are integers, so the optimum is an integer and the bound can be
rounded up; the log's own summary line does this. His table shows the bound
rounded down. sko100b, sko100f and tho150 are not rounded up: their
fractional part is 0.002 or less, which is within numerical tolerance.

| Instance | BKS | LB | Gap % |
|---|---:|---:|---:|
| tai35a | 2422002 | 2292261 | 5.36 |
| tai40a | 3139370 | 2923073 | 6.89 |
| tai50a | 4938796 | 4470692 | 9.48 |
| tai60a | 7205962 | 6371803 | 11.58 |
| tai80a | 13499184 | 11730513 | 13.10 |
| tai100a | 21040996 | 17853840 | 15.15 |
| tai50b | 458821517 | 448410946 | 2.27 |
| tai60b | 608215054 | 595752237 | 2.05 |
| tai80b | 818415043 | 790015925 | 3.47 |
| tai100b | 1185996137 | 1160455924 | 2.15 |
| tai150b | 498896643 | 473337386 | 5.12 |
| tai256c | 44759294 | 44445735 | 0.70 |
| sko49 | 23386 | 22941 | 1.90 |
| sko56 | 34458 | 33449 | 2.93 |
| sko64 | 48498 | 47078 | 2.93 |
| sko72 | 66256 | 64643 | 2.43 |
| sko81 | 90998 | 88592 | 2.64 |
| sko90 | 115534 | 112423 | 2.69 |
| sko100a | 152002 | 147972 † | 2.65 |
| sko100b | 153890 | 149668 | 2.74 |
| sko100c | 147862 | 144169 † | 2.50 |
| sko100d | 149576 | 145352 † | 2.82 |
| sko100e | 149150 | 145250 † | 2.61 |
| sko100f | 149036 | 144755 | 2.87 |
| tho40 | 240516 | 228079 | 5.17 |
| tho150 | 8133398 | 7854894 | 3.42 |
| wil50 | 48816 | 48397 | 0.86 |
| wil100 | 273038 | 269313 | 1.36 |

The tai256c lower bound was computed with QUADI (T. Baylis, quadi.dev) and is
listed only on Mittelmann's page: no paper or quadi.dev page publishes it. The
best bound in a peer-reviewed paper is 44200000: Fujii, Kim, Kojima,
Mittelmann, Shinano, *Optimization Letters* 19(6):1075–1097 (2025),
doi:10.1007/s11590-024-02157-2.

Other problems on Mittelmann's page: the tai50a gap should be 9.48 %, not
10.3 %. The tai100a bound 17853840 is the log's 7-digit printout 1.785384e+07,
so the exact bound may be a few units lower. No log is posted for the wil100
bound 269313.

## Known errors in the sources

- `kra32.sln` (official site and mirrors): the header says 88900, but its
  permutation evaluates to the true optimum, 88700. Don't read BKS values from
  `.sln` headers.
- The official `.sln` files for tai60a, tai80a, tho150, kra30a, kra30b, ste36c,
  tho30 and esc128 store the inverse permutation. `tai40a.sln` is numbered from 0.
- Mittelmann's page gives 7505962 as the tai60a upper bound. That is a typo:
  the verified BKS is 7205962, and the page's 15.1 % gap is computed from the typo.
- Taillard's instance files still have older, worse values in their headers:
  tai50a 4941410, tai80a 13557864, tai150b 499348972, tai256c 44951708.

## Benchmarks outside QAPLIB

`other_benchmarks_bks.tsv` collects best known values for 293 instances in these
families:

| Family | Instances | Proven optimal |
|---|---:|---:|
| Drezner dre15–dre132 | 12 | 12, optimal by construction |
| Drezner–Hahn–Taillard tai\*e (27e to 729e) | 130 | 22 |
| Palubeckis Inst20–Inst200 | 10 | 10, optimal by construction |
| de Carvalho–Rahmann microarray bl and ci | 14 | 0 |
| Taillard grey16_16_m | 127 | not marked |

This table has been checked less thoroughly than the QAPLIB file:

- Taillard's site was unreachable on 2026-11-03, so its values come from Wayback
  copies. Its summary file is from 2024 and has not changed since 2021.
- The tai\*e values for n ≥ 125 may be out of date. Silva, Coelho and Darvish,
  EJOR (2021), reportedly improved most of them, but their per-instance tables
  could not be accessed.
- Some values are not peer-reviewed: tai175e01 = 59286 comes from a 2026 master's
  thesis.
- The tai343e and tai729e values listed by Misevičius et al. (2024) cite no source.
