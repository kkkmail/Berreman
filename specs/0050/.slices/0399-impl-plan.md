# Step 0399 — impl plan

## Approach

The nonlinear fit's results (S11.4) are read off the problem it minimised. Two declarations land in
`StudyEngine.fs`, between `reconstructLinear` (step 390) and `StudyFitRequest` (step 391):

1. `[<ReferenceEquality>] EngineProblems = { problems : FittedProblem list; runResiduals : RunResidual list;
   productChecks : SeriesProductCheck list }` — the shape each engine answers its results from (steps 396, 397).
2. `type StagedStudy with member this.nonlinearProblemsWith (context) (data) (solution) : Result<EngineProblems,
   StudyFitError>`:
   - `this.scaledProblemWith context data` (step 387; its errors pass through unchanged);
   - one `FittedProblem`: `StudyEntriesProblem`, one `EntryCoordinate [ { parameter; owner } ]` per layout entry in
     order, the problem's `residual`, its `search` with `start` = (fitted − centre) / unit entry by entry, and its
     `centre` and `unit`;
   - `runResiduals`: each run of `data.runs` that `this.fittedRuns` names, in order, `this.study.residualWith staged
     problem.layout { data with runs = [ run ] } solution.fitted` over the context with `kit.items = this.items`, with
     the entries' root mean square (zero for none); an error maps to `StudyForwardFitErr`;
   - `productChecks = []`.

No other member is added (the slice says so), so the RMSE formula is written inline.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` — the record and the member.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` — the two facts, after step 387's
  nonlinear-engine facts, over its polarimeter study (`plateStudyWith None`, `fitNonlinear` unwatched).

## Risks

- The quality's rank 2 and the σ equality depend on `FittedProblem`'s Jacobian at a near-zero residual; the 1e-12
  χ² tolerance relies on (fitted − centre) / unit reproducing ALGLIB's scaled solution to ~1e-13.
- ELEV: the new record's fields are all our types; the member's parameters are our types. `float` locals only.
- `open FittedProblems` is already in `StudyEngine.fs`; the test file has to open it for `FitCoordinateIndex`.
