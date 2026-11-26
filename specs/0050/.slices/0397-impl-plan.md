# Step 0397 — impl plan

## Approach

New `OpticalConstructor.Optimization/FittedProblems.fs` (`module FittedProblems`), compiled after
`StagedMuellerPath.fs` and before `StudyEngine.fs`, in the order the brief gives:

1. `type IterationWatch with member this.after (earlier : IterationCount) : IterationWatch`: the counting-on rule
   moved out of `fitNonlinear`'s `watchAfter`.
2. `FitCoordinateIndex of int` with `.value`.
3. `FitProfileError = ProfileCancelledErr of reason : string` with `.describe`.
4. `ProfileRun = { points : ProfilePoint list; iterations : IterationCount }`.
5. `[<ReferenceEquality>] FittedProblem` with `jacobian`, `report`, `quality`, `sigmaAt`, `entrySigmas` and `profileAt`.
   - `jacobian` and `report` work in a frame offset by one hundred: y ↦ residual (start + (y − 100)) at y = 100.
   - `quality` reads rank, condition, correlation and column norms off `ResidualJacobian`. The condition and the
     correlation are `Some` only at full rank with a finite condition.
   - `sigmaAt` is unitₖ · the report's standard error k, at full rank only.
   - `profileAt` holds the coordinate at seven values and re-fits the rest through `ScaledSearch.solveWith`, over a
     reduced problem, under `watch.after`.
     - With σ, the values are x̂ ± jσ, dropping any outside the bounds; the middle point is not re-fitted.
     - Without σ, they are seven values across finite bounds.
     - Cancellation is read before and after each re-fit.

`fitNonlinear` (`StudyEngine.fs`) drops its local `watchAfter`. Each member now runs under `.after` of the earlier
members' told count, so every count it reports stays the same.

The told count is still needed: the solver's `iterations` is ALGLIB's `iterationscount`, but the watch is told one
count per report call, which can be more. Two callers need that count kept: `fitNonlinear` and `profileAt`. So two
`internal` members on `IterationWatch` are shared between them:

- `cancellation`: the watch's token;
- `tellingAlso`: a tee that also hands each count to a callback.

These are moves of `fitNonlinear`'s own logic, not copies.

## Files

- `OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`: one `<Compile Include="FittedProblems.fs" />` line.
- `OpticalConstructor.Optimization/FittedProblems.fs`: new.
- `OpticalConstructor.Optimization/StudyEngine.fs`: `fitNonlinear` through `watch.after`.
- `OpticalConstructor.Tests/FitResultsTests.fs`: five new facts.

## Risks

- Record-label shadowing. Opening `FittedProblems` in `StudyEngine.fs` brings the labels `subject`, `coordinates`,
  `residual`, `search`, `centre` and `unit` into scope. `ScaledStudyProblem` is declared later, so it still wins, but
  the build has to confirm it.
- ALGLIB has to accept infinite bounds in the test problems. It does: `minlmsetbc` takes ±INF.
- The 1e-9 relative χ² tolerance depends on LM converging on the reduced linear problems. With epsX at 1e-12, it does.
