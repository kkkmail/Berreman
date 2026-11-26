# Step 0458 — state of the world

## Where we are

Spec 0050, section S13.2 (PTB OCDB, the Henke compound calculator and glass catalogues). Earlier steps built the
pieces this step tests:

- step 444: `pullRecord`, `refreshPulls`, `updateSourceSnapshot`;
- step 448: the SQLite pull class;
- step 453: σ on the tabulated table;
- steps 454–455: the PTB reader and adapter;
- steps 456–457: the Henke compound calculator and adapter.

This step proves the PTB and Henke pulls end to end over their real adapters and the real library database in SQLite
`:memory:`. It changes tests only; no product file.

## What's working

- Added three end-to-end facts to `MaterialSourcePullSqliteTests` over SQLite `:memory:` and fixture snapshots; nothing
  reaches the network.
- A pulled PTB OCDB Co record is a user material at v1 that keeps its one-sigma σn/σk in the library database and
  credits PTB under CC-BY-4.0 with its citation.
- An update with changed PTB bounds mints v2 (first σn = (0.96810 − 0.96772) / 6) and keeps v1 with its own σ.
- A pulled Henke `SiO2@2.2` is a user material at v1 under CXRO's provenance, its table `indexTableWith`'s, its
  attribution naming `LicenseRef-CXRO` alone.
- Shared the class's fixture composition (`fixtureWith`, `pulledIn`, hit-taking `resultAt`, `tabulatedTableOf`)
  across all three adapters; the existing four facts keep every assertion.

## Tests

These are development checks; the gate engine runs the roster after this session and is the authority. Every figure
is from the final source.

| Check | Result | Time / deadline |
|---|---|---|
| build | exit 0, 0 errors; no FS or MSB warning (12 × exempt NU1902; 4 × SYSLIB0051 from vendored MathNet, pre-existing) | 85 s / 600 s |
| `MaterialSourcePullSqliteTests`, filtered | `Passed!`, 7 of 7; `--list-tests` shows the three census identities | 7 s |
| constructor-unit-tests, whole suite as the gate selects it (`all_discovered`) | `Passed!`, 1823 of 1823 | 106 s / 180 s |
| elevation, all 13 projects (standalone) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 241 s / 600 s |
| the 21 s44 invariants | each decoded `git grep` finds nothing, as written and with `--untracked` | — |
| LF | numstat equals the CR-ignoring numstat | — |

- **constructor-unit-tests:** 1823 is step 0457's 1820 plus this step's three facts, the "count rising by 3" the
  acceptance asks for.
- **Deadlines:** no development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1823
```

## Architecture

- No architectural change. Step 444's pull and update serve the PTB and Henke adapters unchanged, which these facts
  confirm through SQLite.
- The test class composes every adapter through one `fixtureWith database adapter`. A new source's SQLite fact needs
  only its own adapter builder.

## Deferred

Nothing. All three facts the step names landed, with every assertion the slice lists.

## Gotchas

- **σ equality is exact.** The test computes `(HB − LB) / 6.0` with the same doubles the reader parses, and the
  database payload round-trips doubles. A tolerance would hide a lost or rescaled σ.
- **Snapshot folders are named in the tests.** `ptb-ocdb` and `cxro-henke` are private to the adapters' layout, so
  the tests name them, as `MaterialSourceStoreTests` does.
- **`refreshedWith`'s three files hold for both archives:** refractiveindex.info's, and PTB's (`COPYING`, `Co.txt`,
  `Ru_2019.txt`).
- **SYSLIB0051 is pre-existing.** It comes from `MathNetNumerics/Numerics/Exceptions.cs`, untouched since 2022, and
  appears whenever the vendored MathNet recompiles. It is not this step's warning.

## Changelog

- 2026-11-17 — step 0458: added the PTB OCDB pull / update and Henke pull end-to-end facts over SQLite `:memory:`
  (constructor-unit-tests 1820 → 1823); generalized the class's fixture helpers; tests only.
