# Step 0400 — state of the world

## Where we are

Spec 0050 S11.4 (fit results) continues. Step 399 gave the nonlinear engine its `EngineProblems`. This step gives the
linear engine's object stage its own:

- each group's sixteen-element solve as a `FittedProblem`;
- each fitted run's own rows' residual at its group's solve;
- each series' product check against the cascade of its objects' own solves.

To get there, `reconstructLinear`'s projection and grouping moved into `objectSystemsWith`, which both members now
read. Step 401 gives the calibration stage its problems, and step 402 turns the engines' problems into each entry's σ.

## What's working

- Moved the object stage's projection and grouping into `StagedStudy.objectSystemsWith`. Each group is an
  `ObjectGroupSystem` of per-run `ObjectRunSystem`s. `reconstructLinear` now reads its groups and answers exactly what
  it did.
- Added `StagedStudy.objectProblemsWith`, which reads one `FittedProblem` per object solve.
  - Its sixteen coordinates are in vec order: QZ's own entries, or a series' matrix elements.
  - It also answers each run's own residual with its RMSE.
- Each series whose objects are solved alone gets a product check, stored through `ProductCheckData.storedFor`.
- Added `SeriesProductCheck.rmseRatio`.
- Added three `StudyEngineTests` facts over step 390's fixtures. Constructor tests rose from 1710 to 1713.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt `NU1902` advisory | 32 s / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13) | 218 s / 600 s |
| constructor-unit-tests | 1713/1713 passed, whole suite (1710 + 3) | 128 s / 180 s |
| unit-tests (selected `BerremanTests.MuellerReconstructionTests`) | 25/25 passed | 3 s / 7200 s |
| invariant-s39-* (20) | each ran its own command: `INVARIANT HOLDS ... found nothing` | < 1 s each / 300 s |

The three new facts all passed:

- `an object solve's problem is its design over the matrix's elements, each coordinate an entry of the object`
- `each run of an object stage has its own rows' residual at its object's solve`
- `a series is checked against the cascade of its objects' own solves`

Step 390's three facts over `reconstructLinear` pass unchanged. A mutation check confirmed that each new fact fails on
a defect in its own part of `objectProblemsWith` (see the impl-log).

```yaml
gates:
  constructor_unit_tests: 1713
  berreman_unit_tests: 25
```

`berreman_unit_tests` is the selected class's own count. Step 398 also selected `MuellerReconstructionTests`, at 25.

## Architecture

- **One system source for the object stage.** `objectSystemsWith` is the only place a run's capture rows are projected
  and grouped. `reconstructLinear` solves its groups, and `objectProblemsWith` reads them. A group's `system` is its
  runs' rows stacked in run order, row for row the system the solve is over.
- **An object problem is linear in its own coordinates.** The residual is s ↦ A · s − b over the group's system, with
  centre zero and unit one, so a σ is in the element's own units.
  - A lone object's coordinates are its own `m<r><c>` entries, within their bounds.
  - A series' coordinates are its combined matrix's elements, unbounded.
- **The product check is stored, not recomputed.** It reuses S10.3's `productCheckOf`, `cascadeProduct` (generalised
  to k objects by folding each next one on) and `ProductRmseRatio.between`. `storedFor` is the one conversion to the
  fit run's `SeriesProductCheck`.
- `objectSystemsWith` and its two record types sit directly before `reconstructLinear`, not after
  `nonlinearProblemsWith`, because F# needs a member declared before its caller (a recorded deviation).

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `objectProblemsWith` re-solves every group over all sixteen elements, as the slice prescribes. A partly known object
  that `reconstructLinear` solves only on its free columns can be rank-deficient over sixteen, and then answers
  `ObjectSolveFitErr`.
- A lone object's coefficient with no item-owned layout entry (known, calibrated, a link follower, or a non-item
  scope) keeps its one-entry `EntryCoordinate` and is unbounded. Step 402's `sigmasFor` reads only layout entries, so
  such a coordinate gives no entry a σ.
- The exact-equality assertions rely on the MathNet SVD answering bitwise the same for row-for-row equal systems:
  QZ's start against `reconstructLinear`'s entries, and the series' `fitRmse` against its fit's RMSE.
- At step 390's exact data every residual is about 1e-13. Facts that need residual-sized numbers jitter every row's
  `avgTotal` by 0.01 · (−1)ⁱ (`jittered`).

## Changelog

- 2026-11-14 — Step 0400: moved the object stage's projection and grouping into `objectSystemsWith`. Added
  `ObjectRunSystem`, `ObjectGroupSystem`, `ProductCheckData.storedFor`, `SeriesProductCheck.rmseRatio` and
  `StagedStudy.objectProblemsWith` in `StudyEngine.fs`, and three `StudyEngineTests` facts over step 390's fixtures.
