# Step 0399 — state of the world

## Where we are

Spec 0050 S11.4 (fit results) continues. Steps 396 and 397 built the result shapes and the `FittedProblem` that a
solve's quality, σ and profiles are read from. Step 398 exposed the stage-one residuals. This step gives the nonlinear
engine its results source: `EngineProblems`, plus `StagedStudy.nonlinearProblemsWith`, which reads them off a
`fitNonlinear` solution. Steps 400 and 401 give the object and calibration stages their own `EngineProblems`. Step 402
turns the engines' problems into each entry's σ and the stored results.

## What's working

- Added `EngineProblems` to `StudyEngine.fs`: a fit's solves, each fitted run's residual and each series' product check.
- Added `StagedStudy.nonlinearProblemsWith`. It answers the study residual and its priors as one `StudyEntriesProblem`
  over the fit's scaled coordinates at the fitted vector.
- Each fitted run's study residual over that run alone is answered as a `RunResidual` with its RMSE. The nonlinear
  engine solves no series, so it answers no product check.
- Added two `StudyEngineTests` facts over step 387's polarimeter study. Constructor tests rose from 1708 to 1710.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt NuGet advisories and vendored MathNet `SYSLIB0051` | 28 s final (78 s first) / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13) | 222 s / 600 s |
| constructor-unit-tests | 1710/1710 passed, whole suite (1708 + 2) | 129 s / 180 s |
| unit-tests (selected `BerremanTests.MuellerInverseTests`) | 20/20 passed (the class is untouched by this step) | 644 s / 7200 s |
| invariant-s39-* (20) | each decoded `git grep` found nothing | < 1 s each / 300 s |

The two new facts both passed:

- `the nonlinear fit's problem is the study residual over its scaled coordinates at the fitted vector`
- `each run of a nonlinear fit has the study residual over that run alone`

A mutation check confirmed that each fact fails on a defect in its own half of the member (see the impl-log).

```yaml
gates:
  constructor_unit_tests: 1710
  berreman_unit_tests: 20
```

`berreman_unit_tests` is the selected class's own count. Step 397 ran it with `MuellerReconstructionTests` for 45; step 398 ran only that class, at 25.

## Architecture

- **One results source per engine.** `EngineProblems` is the shape every engine answers its results from. The
  nonlinear engine fills it from the problem it minimised; steps 400 and 401 add the object and calibration stages'
  members. It is `[<ReferenceEquality>]` because `FittedProblem` carries its residual function.
- **The nonlinear problem is re-derived, not stored.** `nonlinearProblemsWith` calls `scaledProblemWith` again, with
  the same context and data as `fitNonlinear`. It then places the search's start at the fitted vector's scaled
  coordinates, (fittedₖ − centreₖ) / unitₖ, so it reads the same residual, frame and box the fit searched. A
  `StudyFitSolution` therefore needs no residual closure.
- **Per-run residuals carry no prior rows.** Each one is `Study.residualWith` over a `StudyData` holding that run
  alone, at `solution.fitted`, in the staged context.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- Step 387's fit is exact, so at the fitted vector every residual entry is about 1e-13 and the RMSE is 2.97e-13. An
  absolute 1e-12 tolerance there only catches gross errors. The RMSE assertion is therefore relative, and fact 1 also
  compares the residual at the parameters' starts.
- Do not compare the problem's χ² with the member's relatively. Re-deriving the scaled start moves it by about 1e-13,
  which at this residual size can shift χ² by a large fraction of itself.
- `StudyEngineTests` now opens `OpticalConstructor.Optimization.FittedProblems`, which brings its record fields into
  scope, including `ProfileRun.points` / `iterations` and `FittedProblem`'s fields. The build showed no inference
  change.

## Changelog

- 2026-11-14 — Step 0399: added `EngineProblems` and `StagedStudy.nonlinearProblemsWith` in `StudyEngine.fs`, and two
  `StudyEngineTests` facts over step 387's polarimeter study.
