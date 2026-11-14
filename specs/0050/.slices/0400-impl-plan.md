# Step 0400 — impl plan

## Approach

The object stage's results are read off the linear solves `reconstructLinear` (step 390) makes. The projection and
grouping half of `reconstructLinear` moves into a new `StagedStudy.objectSystemsWith`, which answers one
`ObjectGroupSystem` per group (its items, and per run an `ObjectRunSystem` of that run's own projected rows).
`reconstructLinear` then calls it and keeps everything it did with the groups: the coefficient maps, the lone systems,
the series solves and the solution. Each group's `system` is `LeastSquaresSystem.concat` of its runs' systems in run
order, which is row for row the system `reconstructLinear` solved before, so every number it answers is unchanged.

`StagedStudy.objectProblemsWith` re-solves each group with `reconstruct solver group.system` and answers the
`EngineProblems` (step 399):

- one `FittedProblem` per group: `ObjectSolveProblem group.items`, sixteen coordinates in vec order (k = 4c + r);
  - for one item, `EntryCoordinate` of the item's `m<r><c>` with its `ItemOwner`, within the entry's unknown bounds;
  - for a series, `ElementCoordinate (r, c)`, unbounded;
  - the residual s ↦ `group.system.residualsAt s`, starting at the solved vec, centre zero and unit one.
- each run's own rows' residual at its group's solve, in study order, with its RMSE;
- one product check per series whose every item has its own group: `productCheckOf later first series group.system`,
  stored through the new `ProductCheckData.storedFor`.

`SeriesProductCheck.rmseRatio` is `ProductRmseRatio.between` of the stored RMSEs.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
  - `ObjectRunSystem`, `ObjectGroupSystem` (+ `system`), `objectSystemsWith`: directly before `reconstructLinear`,
    because F# needs the callee first.
  - `reconstructLinear`, rewritten over `objectSystemsWith`.
  - `ProductCheckData.storedFor`, `SeriesProductCheck.rmseRatio` and `objectProblemsWith`, after
    `nonlinearProblemsWith`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`: the three authorized facts, over step
  390's session and series fixtures.

## Risks

- Declaration order: the slice places (1) and (2) after `nonlinearProblemsWith`, but `reconstructLinear` (earlier in
  the file) must call `objectSystemsWith`. They go directly before `reconstructLinear` instead, and the deviation is
  recorded.
- `evaluationErr` is needed by both halves. A private module function over `ItemEvaluationError` alone would be
  `ELEV013P`, and the slice forbids a new member. So the shared mapping is a private generic function over a `Result`
  (an FSharp.Core type, which ELEV013 does not read).
- Record-label inference: the new records add the `run`, `system`, `items` and `runs` labels. Every new construction
  is annotated, and the build shows any inference change elsewhere.
- At step 390's exact data every residual is about 1e-13. The σ and run-residual facts therefore move every row's
  `avgTotal` by 0.01 · (−1)ⁱ, so their comparisons can tell defects apart.
