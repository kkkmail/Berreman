# Step 0397 — state of the world

## Where we are

Spec 0050, section S11.4: fit results (uncertainty, correlation, realizability, product check, profile likelihood).

- Step 396 declared the result shapes.
- This step adds `FittedProblem`, the one record every engine's results are read from: its Jacobian, σ, quality and
  χ² profiles.
- Steps 399–401 build `FittedProblem`s for the nonlinear fit, the object solves and the calibration fits. Steps 402–403
  pick and profile the suspects, and step 404 stores the results.

## What's working

- Added `FittedProblems.fs` with `FittedProblem`: one least-squares solve, its coordinates and its scaled frame.
  - It reads its Jacobian and `FitQuality` report about a frame offset by one hundred.
  - It reads its rank, condition, column norms and correlation off `ResidualJacobian`.
  - It gives σ in physical units and the per-entry σ.
- Added `FittedProblem.profileAt`: a coordinate's χ² profile by held re-fits through `ScaledSearch.solveWith`.
  - The points are x̂ ± jσ within the bounds, or seven values across finite bounds when σ is undetermined.
  - The re-fits run under the fit's watch, and a cancellation answers `ProfileCancelledErr`.
- Added `IterationWatch.after`, moved out of `fitNonlinear`, which now counts its members' iterations through it.
  Every count it reports is unchanged.
- Added `FitCoordinateIndex`, `FitProfileError` and `ProfileRun`.
- Added five `FitResultsTests` facts over test-local least-squares systems.

## Tests

Run locally on 2026-11-14 as development runs; the supervisor's gate engine is authoritative. Every run finished within
its effective deadline.

- `build`: exit 0, 0 errors. The first build took 82 s, and the final incremental one 9 s (deadline 600 s).
  - The warnings are only the exempt NU1902 advisories and the vendored MathNet `SYSLIB0051` pair.
- `constructor-unit-tests`, whole suite: 1705/1705, 121 s on both runs (deadline 180 s). Elevation or the
  BerremanTests run was going at the same time.
  - The count is 1700 → 1705, +5, as the acceptance requires.
  - The TRX shows all five new IDs `Passed`.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 226 s, then 224 s on the final tree (deadline
  600 s).
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests` and `BerremanTests.MuellerInverseTests`: 45/45,
  11 m 32 s, then 11 m 13 s on the final binaries (deadline 7200 s).
- The twenty `invariant-s39-*` greps all find nothing, untracked files included.
- Mutation checks, each run over a test-project-only build. The file was restored byte for byte before the final build
  and runs.
  - Dropping `earlier` from `IterationWatch.after` fails two facts:
    - the cancel/watch fact;
    - `StudyEngineTests`' `the first member starts at the parameters' starts …`, its 1, 2, … count, which shows that
      `fitNonlinear` counts through `.after`.
  - Setting the frame offset to zero fails three facts: the Jacobian, the rank-deficiency and the profile facts.

```yaml
gates:
  constructor_unit_tests: 1705
  berreman_unit_tests: 45
```

## Architecture

- **One record per solve.** `FittedProblem` holds the subject, the coordinates, the residual over the scaled vector,
  the search (start = the solution, bounds = the box), centre and unit.
  - Every engine maps onto it: the nonlinear fit with its own scaling, the linear and calibration solves with a zero
    centre and a unit scale.
  - Nothing is cached: each member recomputes from the residual.
- **The offset-by-100 frame.** `FitQuality`'s relative step √ε · max(|x|, 1e-12) becomes 100 · 2⁻²⁶ ≈ 1.5e-6. That
  matches ALGLIB's 1e-6 order, perturbs a zero coordinate, and is exact on 100's ulp grid.
- **Iteration counts are the counts told to the watch.**
  - `fitNonlinear` and `profileAt` each keep the last count the watch was told, and solve under `.after` of it. The
    solver's own `iterations` can be smaller than the reports, and a failed solve carries none.
  - The keeping is shared through two `internal` `IterationWatch` members: `cancellation` and `tellingAlso`, both
    moved out of `fitNonlinear`.
  - An unwatched profile counts zero.
- **A held re-fit solves the reduced problem.** It drops the coordinate from the start and the bounds, and inserts the
  held value into the residual. With no other coordinate, the point is the residual at the held vector.

## Deferred

Nothing. Every requirement of the step landed this round. Building each engine's problems (399–401), suspect
selection (402), the profile pass (403) and storage (404) are separate steps of the spec.

## Gotchas

- **Shared record labels.** These labels also name fields of other records:
  - `subject` / `coordinates` (`FitProblemQuality`);
  - `residual` / `search` / `centre` / `unit` (`ScaledStudyProblem`);
  - `points` (`ParameterProfile`);
  - `iterations` (`NonlinearSolution`, `OptimizationResult`).

  Annotate the record expressions that build the new shapes.
- **`sigmaAt` and `entrySigmas` each take a Jacobian and a report.** That is 2 · (n + 1) residual evaluations per call.
  Read them once per solve.
- **`ProfileRun.iterations` is zero unwatched.** It counts only what the re-fits told the watch.
- **STL namesakes.** STL's VNC viewer has `member this.after` on its own receivers. ELEV019 compares per receiver, so
  they do not collide.

## Changelog

- 2026-11-14 — Step 0397: `FittedProblems.fs` (`IterationWatch.after`, `FitCoordinateIndex`, `FitProfileError`,
  `ProfileRun`, `FittedProblem`).
  - `fitNonlinear` now counts its members through `.after`.
  - Added five `FitResultsTests` facts.
