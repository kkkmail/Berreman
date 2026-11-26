# Step 0400 — impl log

## Progress

- [x] `ObjectRunSystem`, `ObjectGroupSystem` (+ `system`) and `StagedStudy.objectSystemsWith`: the projection and
  grouping moved out of `reconstructLinear`.
- [x] `reconstructLinear` rewritten over `objectSystemsWith`. Everything it did with the groups is kept.
- [x] `ProductCheckData.storedFor`, `SeriesProductCheck.rmseRatio` and `StagedStudy.objectProblemsWith`, after
  `nonlinearProblemsWith`.
- [x] The three authorized facts in `StudyEngineTests`. With step 390's three facts, 6/6 passed.
- [x] Mutation check: three mutations, each failing exactly its own fact. The production file was then restored.
- [x] Final build, constructor-unit-tests (whole), elevation, the twenty s39 invariants, unit-tests (selected
  `BerremanTests.MuellerReconstructionTests`).
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (+213 / −36)
  - `let private evaluatedForFit`: the item-evaluation error mapping that both halves of the object stage now share.
  - `type ObjectRunSystem = { run : ExperimentName; system : LeastSquaresSystem }`.
  - `type ObjectGroupSystem = { items : ItemId list; runs : ObjectRunSystem list }`, with
    `member this.system : LeastSquaresSystem` (`LeastSquaresSystem.concat` of its runs' systems, in order).
  - `StagedStudy.objectSystemsWith context prefits data : Result<ObjectGroupSystem list, StudyFitError>`. It holds
    `reconstructLinear`'s former projection code unchanged (`sessionOf`, `darkOf`, `projectedOf`, `anchorOf`, `ruleOf`,
    `fullSystemOf`) and its grouping by the segment's items, in first-run order.
  - `reconstructLinear`: same signature and doc.
    - It is `objectSystemsWith` followed by its coefficient maps, lone systems and series solves.
    - `solveSeries` takes the group and solves `group.system`.
  - `ProductCheckData.storedFor (items) : SeriesProductCheck`: the cascade's `rowMajorValues`, the difference and the
    two RMSEs.
  - `SeriesProductCheck.rmseRatio : ProductRmseRatio`: `ProductRmseRatio.between this.productRmse this.fitRmse`.
  - `StagedStudy.objectProblemsWith context prefits data solver : Result<EngineProblems, StudyFitError>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (+196 / −6)
  - `open Berreman.MathNetNumericsMath`, for the test's own `RealMatrix` / `RealVector` normal equations.
  - `seriesTruth` and `seriesFixture ()`. Step 390's series fact was moved onto them; its assertions are unchanged.
  - A new S11.4 section after step 390's facts:
    - the private helpers `jittered`, `qzProblemsOver`, `vecPlaces` and `vecValuesOf`;
    - the three authorized facts.

## Decisions

- **Placement deviation (F# declaration order).** The slice puts (1) and (2) after `nonlinearProblemsWith`. But
  `reconstructLinear`, which comes earlier in the file, must call `objectSystemsWith`, and a type extension only sees
  members declared before it. So `ObjectRunSystem`, `ObjectGroupSystem` and `objectSystemsWith` sit directly before
  `reconstructLinear`. The moved projection code then stays nearly in place in the diff. `storedFor`, `rmseRatio` and
  `objectProblemsWith` follow `nonlinearProblemsWith`, as the slice says.
- **The shared error mapping is a private generic function over `Result`.** Both halves map an
  `ItemEvaluationError`, so keeping one local closure in each would duplicate a body. The alternatives were worse:
  - A private module function of one `ItemEvaluationError` parameter is `ELEV013P`: the analyzer enforces private
    signatures by default, and the type is not FSharp.Core's.
  - A member on the error type is excluded by "No other member is added".
  - `evaluatedForFit : Result<'Value, ItemEvaluationError> -> Result<'Value, StudyFitError>` takes an FSharp.Core
    type, which ELEV013 does not read. It is not a member.
- **`reconstructLinear` answers what it answered before.** Each group's `system` concatenates its runs'
  `fullSystemOf` systems in run order. Those runs are the same, in the same order, as the `(projected, system)` pairs it
  grouped before. So `rowsOf lone` and the series' rows are row for row the systems it solved. Step 390's three facts
  pass unchanged.
- **Coordinates of a group of one item.** Coordinate k = 4c + r is
  `EntryCoordinate [ { parameter = ItemParameterRef (item, m<r><c>); owner = ItemOwner item } ]`. Its bounds are those
  of the layout entry holding that parameter with that owner (matched as `engineWith`'s `itemOwned` does). A
  coefficient with no such entry keeps the same one-entry coordinate (the `EntryCoordinate` doc: "one in the nonlinear
  and object solves") and is unbounded. That covers a known, calibrated or linked coefficient, or a non-item scope. For
  QZ all free, as in every fact, the ref equals the layout entry's own ref, and the bounds are QZ's [−2, 2].
- **Each problem re-solves its group with `reconstruct solver group.system`, as the slice says.** For an object whose
  sixteen-column design is rank-deficient (a partly known black box observed by too few families), this answers
  `ObjectSolveFitErr` as step 390's `reconstruct` does. The member takes no solution argument.
- **`runResiduals` follow `this.fittedRuns`**, which is study order (the same order `objectSystemsWith` walks the
  prefits in). Each one's RMSE is `LeastSquaresSystem.residualRmseAt`, the solver's and product check's one
  definition, with a 0 for a run of no rows instead of 0/0.
- **The product check's later cascade** is `rest |> List.fold (fun soFar next -> cascadeProduct next soFar) second`,
  so `productCheckOf later first` forms Mₖ ⋯ M₂ · M₁. For [QZ; LR] that is exactly M_LR · M_QZ.
- **Test facts read off the solve itself, with jitter where the data is exact.**
  - The σ fact and the run-residual fact move every capture row's `avgTotal` by 0.01 · (−1)ⁱ, i the row's place in
    its run. At step 390's exact data every residual is about 1e-13, so an absolute tolerance could not tell defects
    apart (step 399's gotcha).
  - The expected σ of m00 is computed in the test from the system handed to the solver: Aᵀ built by hand, (AᵀA)⁻¹
    through `RealMatrix.inverse`, x̂ from the normal equations and χ² = ‖A x̂ − b‖².
- **Assertions added inside the authorized facts, all within their stated substance.** No test identity was added or
  renamed.
  - Fact 1: the problem's design is the run law's (`assertSystemOver`), and its residual at the identity's vec is
    A · s − b. It also checks centre, unit and bounds.
  - Fact 3: the check's `rmseRatio` is `productRmse / fitRmse`; the series' problem starts at the series fit's
    elements, unbounded; the group order of the three problems.
- **Step 390's series fact moved onto `seriesFixture ()`** (reuse, never copy). The new series fact builds the same
  fixture. Only the fixture lines and the `combined` name changed (now `seriesTruth`); every assertion is as it was.

## Testing state

Development runs on 2026-11-14. The supervisor's gate engine is the authority; these are not gate results.

- `build`: `dotnet build Berreman.slnx -c Release -nologo -v:m`, exit 0, 0 errors, 32 s (deadline 600 s).
  - The only warnings are the exempt `NU1902` advisory (12 occurrences).
  - Both test bins hold the restored `Softellect.Berreman.OpticalConstructor.Optimization.dll` (checked with `cmp`
    against the project's own output).
- The three new facts and step 390's three: 6/6 passed, 5 s.
- `constructor-unit-tests`, the gate's own command, whole suite: **1713/1713 passed in 128 s** (deadline 180 s). That is
  1710 + 3, the count rising by 3 as the acceptance requires.
- `elevation`: `Run-ElevationGate.ps1 -ExcludeRules ELEV004` gave `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`.
  - It took 218 s (deadline 600 s), with no comparison base, so all 13 projects were analyzed.
  - Optimization's suppressed count went from 93 to 94. ELEV004 is the only excluded rule and the known list is empty,
    so the extra one is the new `ParameterValue.value` read where an entry's bounds enter the problem's `ScaledVector`.
    `scaledProblemWith` reads its bounds the same way.
- The twenty `invariant-s39-*` gates: each ran its own command, `INVARIANT HOLDS ... found nothing`
  (`0400-invariants-final.log`).
- `unit-tests` (selected `BerremanTests.MuellerReconstructionTests`, the gate's own command): **25/25 passed in 3 s**
  (deadline 7200 s).
- Mutation check: one build with three mutations at once, each reaching one fact; all three facts failed at their own
  defect.
  - `productCheckOf later first` → `productCheckOf first later` failed the series fact at the cascade comparison
    (expected 0.1750, actual 0.3388).
  - Squaring the run RMSE failed the run-residual fact (expected 9.80e-6, actual 9.60e-11).
  - Row-major coefficient order for the coordinates failed the problem fact at the coordinate list.
  - The production file was restored from a backup under `.artifacts/`, verified identical with `cmp`, and the backup
    deleted. The final build compiled from it.
- Line endings: LF throughout. `git diff --numstat` equals `--ignore-cr-at-eol`, and no touched file holds a CR.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0400-mutation-build.log`, `0400-mutation-tests.log` (all three new facts red under the mutations)
- `0400-build-final.log`
- `0400-constructor-unit-tests-final.log`
- `0400-elevation-final.log`
- `0400-invariants-final.log`
- `0400-unit-tests-final.log`

## Gotchas

- **The slice's "after `nonlinearProblemsWith`" cannot hold for `objectSystemsWith`.** `reconstructLinear` calls it and
  is earlier in the file. See Decisions.
- **The problem fact's exact start relies on the SVD being deterministic.** QZ's start must equal
  `reconstructLinear`'s fitted entries bitwise, and the series' `fitRmse` its fit's RMSE. Both come from two
  `createMathNetSvd ()` solves of row-for-row equal systems. They matched in every run here.
- **`objectProblemsWith` re-solves all sixteen elements of every group.** A study whose object stage
  `reconstructLinear` solves only on its free columns may still be rank-deficient over sixteen. Such a study answers
  `ObjectSolveFitErr` here, as the slice prescribes. Step 402's `engineProblemsWith` inherits this.
- `StudyEngineTests` now opens `Berreman.MathNetNumericsMath`. The build showed no inference change.
