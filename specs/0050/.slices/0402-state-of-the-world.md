# Step 0402 — state of the world

## Where we are

Spec 0050 S11.4 (fit results) continues. Steps 399–401 gave each engine its `EngineProblems`: the nonlinear fit's
study residual, the object stage's solves, and the calibration stage's three AIR fringes.

This step turns those problems into the fit's results:

- one call reads any solution's problems;
- each layout entry gets its σ;
- the suspect entries are named;
- every black-box object the fitted runs use is checked for realizability at its fitted values.

## What's working

- Added `StagedStudy.engineProblemsWith`, which reads a solution's problems from the engine `runFit` took.
- Added `EngineProblems.sigmasFor`: each layout entry, in order, gets the σ of the first problem holding it, else `None`.
- Added `SuspectCoordinate` and `EngineProblems.suspectsWith`, which name an entry suspect when its σ is undetermined,
  wide against its bounds, or correlated past the threshold.
- Added `StagedStudy.realizabilityWith`: each black box's Cloude eigenvalues at its first fitted run's values, at its
  reference orientation and that run's operating wavelength.
- Added four `StudyEngineTests` facts. Constructor tests rose from 1716 to 1720.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt `NU1902` advisory | 47 s (full), 10 s (rebuild) / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13) | 219 s / 600 s |
| constructor-unit-tests | 1720/1720 passed, whole suite (1716 + 4) | 137 s / 180 s |
| unit-tests (selected `MuellerReconstructionTests`, `MuellerInverseTests`) | 45/45 passed (25 + 20) | 673 s / 7200 s |
| invariant-s39-* (20) | each ran its own command: `INVARIANT HOLDS ... found nothing` | < 1 s each / 300 s |

The four new facts all passed:

- `an entry is suspect when its sigma is undetermined, wide against its bounds, or correlated past the threshold`
- `every entry takes the sigma of the coordinate that holds it`
- `every black-box object of a fit is checked for realizability at its fitted values`
- `each engine's results are read off the solves it made`

A mutation check planted one defect in each new member. Each defect turned exactly its own fact red (see the
impl-log). Every earlier `StudyEngineTests` fact passes unchanged.

```yaml
gates:
  constructor_unit_tests: 1720
  berreman_unit_tests: 45
```

`berreman_unit_tests` is the two selected classes' own count: 25 `MuellerReconstructionTests` and 20
`MuellerInverseTests`.

## Architecture

- **One dispatch.** `engineProblemsWith` matches `(solution.engine, this.stage)` with `runFit`'s three arms, so a fit
  run's results are always read from the solves its own engine made.
- **An entry's σ is its coordinate's.** `sigmasFor` matches each layout entry by parameter and owner against each
  problem's `entrySigmas` (step 397), the first problem winning. The layout's order is kept: an object solve's
  vec-ordered coordinate 4c + r gives the row-major entry (r, c) its σ.
- **Suspects are read per coordinate.** For each problem, `quality` (for the correlation matrix) is read once and
  `sigmaAt k` once per entry coordinate.
  - The reasons come in the slice's order: undetermined, wide (σ over the entry's own layout bounds' span), then one
    correlated reason per other entry coordinate in coordinate order, named by its first entry.
  - An entry with no layout entry has no bounds and so is never wide.
  - Visibility and element coordinates are neither suspects nor partners.
- **Realizability reuses the forward.** The black boxes are `Experiment.itemsUsedBy` the fitted runs, in its order,
  each read at the first fitted run whose own `itemsUsedBy` holds it.
  - Its values come from `layout.assignmentFor` and `valuesOf`.
  - Its wavelength is `boundWith`'s `operatingWaveLength` (step 367's rule, called rather than re-composed).
  - Its matrix is `BoundOpticalForm (form, values)` at `ElementOrientation.transmittingAt Angle.zero`, and its
    eigenvalues are `MuellerInverse.cloudeEigenvalues`.
  - The realizable-or-not verdict stays `ObjectRealizability.realizability` (step 396).
- The four additions sit after `calibrationProblemsWith` and before `StudyFitRequest`, as the slice places them.
  `SuspectCoordinate` is `[<ReferenceEquality>]` because it holds a `FittedProblem`, a record holding a function.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- Step 397's two least-squares problems are private to `FitResultsTests`, which this step may not edit. They are
  rebuilt test-locally in `StudyEngineTests` over their own entries (x0, x1 full rank; y0, y1 rank-deficient), so each
  suspect's problem is unambiguous.
- `sessionFixture` now takes QZ's truth (`sessionFixture qzTruth` at its four earlier call sites), so the realizability
  fact can generate QZ's rows from diag(1, 1.5, 0, 0). No existing assertion changed.
- QZ's fitted retarder has eigenvalues (1, 0, 0, 0), the same as its identity start. Only the diag(1, 1.5, 0, 0) case
  tells fitted values from starts, which the `start` mutant confirmed.
- The sigma fact asserts that m10's and m01's σ differ (vec coordinates 1 and 4). Without that, a row-major-versus-vec
  mix-up could pass unseen.
- `SuspectCoordinate`'s labels `entry` and `reasons` are also `FitEntrySigma`'s and `ParameterProfile`'s. Annotate any
  record construction or lambda over them.
- StudyEngineTests is the constructor suite's critical path (134 s of the 137 s wall clock, against 180 s). A further
  nonlinear fit there costs about 6 s.

## Changelog

- 2026-11-14 — Step 0402: added `StagedStudy.engineProblemsWith`, `EngineProblems.sigmasFor`, `SuspectCoordinate` with
  `EngineProblems.suspectsWith`, and `StagedStudy.realizabilityWith` to `StudyEngine.fs`. Added four `StudyEngineTests`
  facts, with `sessionFixture` parametrised by QZ's truth.
