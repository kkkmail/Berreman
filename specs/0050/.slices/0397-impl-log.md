# Step 0397 — impl log

## Progress

- [x] Read the inputs:
  - the protocol (`arc-runner.system-md`, `implement_worker.system-md`);
  - the project prompt and the slice;
  - step 0396's SoW;
  - the code the step touches: `FitQuality.fs`, `MuellerInverse.fs:ResidualJacobian`, `MuellerFitting.fs:ScaledSearch`,
    `StudyEngine.fs:fitNonlinear` and `FitRuns.fs`' S11.4 shapes.
- [x] Wrote the impl-plan.
- [x] Added `FittedProblems.fs` and its compile line (after `StagedMuellerPath.fs`, before `StudyEngine.fs`).
- [x] `fitNonlinear` now counts its members' iterations through `IterationWatch.after`.
- [x] Added five `FitResultsTests` facts with the exact authorized IDs.
- [x] Ran the build, the tests, elevation and the invariants locally (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`: one
  `<Compile Include="FittedProblems.fs" />` line, no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/FittedProblems.fs`: new.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`:
  - opens `FittedProblems`;
  - `fitNonlinear` drops its local `watchAfter` and its local cancellation match;
  - each member solves under `telling.after told.Value`;
  - the now-unused `open System.Threading` is dropped.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitResultsTests.fs`: test-local systems and helpers, and the five
  new facts.

## Decisions

- **The count is kept where the solver tells it, in both callers.** The solver's `iterations` is ALGLIB's
  `iterationscount`, but the watch is told once per report call, which can be more
  (`OptimizationInterfaceTests` asserts reports ≥ iterations).
  - Chaining `earlier` from `solution.iterations` would re-tell counts already told. It would also lose a failed solve's
    count, since `NonlinearSolverError` carries none.
  - So `fitNonlinear` and `profileAt` both keep the last told count and solve under `.after` of it, and every count
    `fitNonlinear` reports is unchanged.
- **Two `internal` members on `IterationWatch` hold the shared logic.**
  - They are `cancellation` (the watch's token) and `tellingAlso` (a tee handing each count to a callback).
  - Both are moved out of `fitNonlinear` (its local `cancellation` match and the recording half of `watchAfter`), so
    `profileAt` reuses them instead of copying them (AGENTS.md "Reuse, never copy").
  - They are `internal`, not public, so no contract beyond the slice's list is added.
- **An unwatched profile counts no iterations.** `ProfileRun.iterations` is the counts its re-fits told the watch.
  Nothing tells an unwatched watch, so it stays zero. This matches `fitNonlinear`, whose unwatched count never moved
  either. Step 403 chains `earlier` only to count the pass on through the fit's watch.
- **A held re-fit solves the reduced problem.** It removes the coordinate from the start and bounds, and inserts the
  held value back into the residual. It does not pin bounds, because `levenbergMarquardtSolver` refuses an empty start
  and the brief says a one-coordinate solve has nothing to re-fit. That case returns the residual at the held vector.
- **Every held point reads cancellation before and after its re-fit.** This includes a one-coordinate solve's point,
  for uniformity. The middle point is the solve's own and is never a re-fit.
- **Out-of-range indices.** An index outside the solve gives no profile points and `sigmaAt` `None`, never a throw.
- **Offset frame.** The frame computes `start + (y − 100)` with the difference first, so the base point gives exactly
  `residual start`.
  - √ε is 2⁻²⁶ and 100 · 2⁻²⁶ is exact on 100's ulp grid, so the Jacobian of a linear residual is exact to round-off.
  - The 1e-9 relative profile tolerances therefore hold.
- **Even spacing without σ.** The sigma-less values are `lower + i / 6 · (upper − lower)`, so i = 6 lands exactly on
  `upper`.
- **Test extensions within the five authorized facts.** These add assertions only; there is no new identity:
  - the first fact also checks `entrySigmas`, column norms, subject and coordinates;
  - the last also checks a watched profile after five iterations: reports 6, 7, … contiguous, and `run.iterations`
    equal to their count.

## Testing state

These are development runs; the supervisor's gate engine is authoritative. The SoW's Tests section has the
durations, and every run finished within its deadline.

- build: clean. The only warnings are the exempt NU1902 advisories and the vendored `SYSLIB0051` pair.
- `FitResultsTests`: 8/8, the 3 existing facts and the 5 new ones.
- constructor-unit-tests: 1705/1705 (+5) on both runs.
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` on both runs.
- unit-tests, the selected `MuellerReconstructionTests` and `MuellerInverseTests`: 45/45 on both runs.
- The twenty s39 invariants find nothing.
- Mutation checks were caught by the facts, and the file was restored byte for byte:
  - without `earlier` in `after`, the cancel/watch fact and `StudyEngineTests`' member-count fact fail;
  - with a zero offset, the Jacobian, rank and profile facts fail.

`commit_ready: true`.

## Artifacts

- `specs/0050/.artifacts/0397-build-dev-01.log`, `0397-build-dev-02.log`
- `specs/0050/.artifacts/0397-constructor-unit-tests-dev-01.log`, `-dev-02.log`
- `specs/0050/.artifacts/0397-elevation-dev-01.log`, `-dev-02.log`
- `specs/0050/.artifacts/0397-unit-tests-dev-01.log`, `-dev-02.log`

## Gotchas

- **Record labels.** `FittedProblem` shares `subject` and `coordinates` with `FitProblemQuality`, and `residual`,
  `search`, `centre` and `unit` with `ScaledStudyProblem`. `ProfileRun` shares `points` with `ParameterProfile` and
  `iterations` with `NonlinearSolution`. Annotate the record expressions that build them (`quality` and
  `ProfileRun` are annotated).
- **`after` namesakes in STL.** STL's VNC viewer declares `member this.after` on other receivers. ELEV019 compares
  extension members per receiver only, so they do not collide.
- **Name check.** The name check over `C:\GitHub\Softellect` is slow because it walks `bin`/`obj`; restrict it to
  `--include=*.fs`.
