# Step 0399 — impl log

## Progress

- [x] `EngineProblems` record in `StudyEngine.fs`, between `reconstructLinear` (step 390) and `StudyFitRequest`
  (step 391).
- [x] `StagedStudy.nonlinearProblemsWith`, in its own `type StagedStudy with` block right after the record.
- [x] Two facts in `StudyEngineTests`, in a new S11.4 section after step 387's nonlinear-engine facts.
- [x] First build, then the two facts alone: green.
- [x] Mutation check (two mutations). Strengthened the tests, then restored the production file.
- [x] Final build, constructor-unit-tests (whole), elevation, the twenty s39 invariants.
- [x] unit-tests (selected `BerremanTests.MuellerInverseTests`): see Testing state.
- [x] Wrote the state-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (+64, additions only)
  - `[<ReferenceEquality>] type EngineProblems = { problems : FittedProblem list; runResiduals : RunResidual list;
    productChecks : SeriesProductCheck list }`.
  - `type StagedStudy with member this.nonlinearProblemsWith (context) (data) (solution) : Result<EngineProblems,
    StudyFitError>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (+97, additions only)
  - `open OpticalConstructor.Optimization.FittedProblems`, for `FitCoordinateIndex`.
  - A private `engineProblemsOf` result unwrapper.
  - The two authorized facts.

## Decisions

- **`scaledProblemWith` gets the caller's `context`; `residualWith` gets the staged one.** `scaledProblemWith` swaps
  `kit.items` for `this.items` itself, which is how `fitNonlinear` calls it. Passing `context` gives the same problem the
  fit minimised. `residualWith` takes the context as given, so the member builds
  `{ context with kit = { context.kit with items = this.items } }` for it, as the slice says.
- **The fitted problem's `search` is the scaled problem's own, with only `start` replaced** (copy-and-update), so its
  bounds are the box the fit searched.
- **`runResiduals` filter `data.runs` by `this.fittedRuns`, in `data.runs` order**, which is study order
  (`Study.dataWith`). An error from any run stops the list (`ItemEvaluation.sequenced`) and answers `StudyForwardFitErr`.
- **The RMSE is written inline:** `if count = 0 then ResidualRmse 0.0 else ResidualRmse (sqrt (Σ / count))`.
  `FittedProblem.quality` (step 397) uses the same one-line definition over its residual. The slice forbids any other
  member ("No other member is added"), so there is no shared helper on `ResidualVector`.
- **Record expressions are annotated** (`: FitEntryRef`, `: RunResidual`, `: EngineProblems`). `FitRunEntry` also has
  `parameter`/`owner` fields, and `FitRunResults` also has `problems`/`runResiduals`/`productChecks`, so the annotation
  keeps inference from depending on declaration order.
- **Two assertions strengthened, inside the two authorized facts (additions only).** No test identity was added or
  renamed.
  - Fact 1 also compares the problem's residual with the scaled problem's at the parameters' starts (the scaled
    origin). At the converged fit every entry is about 1e-13, so the 1e-12 comparison at the start alone barely tests
    the function.
  - Fact 2 compares the RMSE relatively (1e-12 × the RMSE) and asserts it is positive. The measured RMSE is
    2.97e-13, so an absolute 1e-12 could not tell an RMSE from its square (see the mutation check).

## Testing state

Development runs on 2026-11-14. The supervisor's gate engine is the authority; these are not gate results.

- `build`: `dotnet build Berreman.slnx -c Release -nologo -v:m`, exit 0, 0 errors.
  - First build 78 s; final build 28 s.
  - The only warnings are the exempt NuGet advisories and the vendored MathNet `SYSLIB0051` pair.
  - `Softellect.Berreman.OpticalConstructor.Optimization.dll` (05:30:12) and `OpticalConstructor.Tests.dll` (05:30:33)
    are newer than the restored `StudyEngine.fs` (05:29:55).
- The two new facts alone: 2/2 passed, 18 s.
- `constructor-unit-tests`, the gate's own command, whole suite: **1710/1710 passed in 129 s** (deadline 180 s). That is
  1708 + 2, the count rising by 2 as the acceptance requires.
- `elevation`: `Run-ElevationGate.ps1 -ExcludeRules ELEV004` gave `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`.
  - It took 222 s (deadline 600 s).
  - There was no comparison base, so all 13 projects were analyzed.
  - It ran concurrently with the unit-tests run.
- The twenty `invariant-s39-*` gates: each decoded `git grep` found nothing (`0399-invariants-final.log`).
- `unit-tests` (selected `BerremanTests.MuellerInverseTests`): **20/20 passed in 644 s** (MTP duration 10 m 43 s; deadline 7200 s), the gate's own command over `--filter-method "BerremanTests.MuellerInverseTests.*" --filter-method "BerremanTests.MuellerInverseTests"`. Its first 222 s overlapped the elevation run. The class is untouched by this step. Its 20 matches step 397's selection: that step ran this class with `MuellerReconstructionTests` (25) for 45. Step 398 selected only the latter (25), and `.checkpoints-json` records no `berreman_unit_tests` floor.
- Mutation check: one test-project build with two mutations at once, each mutation reaching only one fact.
  - `search = { problem.search with start = atFitted }` → `search = problem.search` failed fact 1 at the start
    comparison (expected −0.3088, actual 0).
  - Dropping the `sqrt` from the RMSE initially **passed** fact 2: the RMSE is 2.97e-13 and its square 8.8e-26, both
    under an absolute 1e-12. After the relative comparison it fails fact 2 (expected 2.97e-13, actual 8.8e-26).
  - The production file was restored from a backup, verified identical with `cmp`, and the backup deleted. The final
    build recompiled from it.
- Line endings: LF throughout. `git diff --numstat` equals `--ignore-cr-at-eol`, and no file holds a CR.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0399-build-dev.log`, `0399-build-final.log`
- `0399-mutation-build.log`, `0399-mutation-tests.log` (the second mutation run, both facts red)
- `0399-constructor-unit-tests-final.log`
- `0399-unit-tests-final.log`
- `0399-elevation-final.log`
- `0399-invariants-final.log`

## Gotchas

- **Step 387's fit is exact, so every residual at the fitted vector is about 1e-13.** An absolute 1e-12 tolerance on
  anything read there (entries, χ², RMSE) only catches gross errors, such as a wrong vector or a wrong start. A later
  fact that needs to tell finer differences apart must compare relatively, or away from the solution.
- **A relative χ² comparison against the member's is NOT safe.** Re-deriving the scaled start as (fitted − centre) /
  unit moves it by about 1e-13 scaled units. At 1e-13 residual entries, that can shift χ² by a large fraction of itself.
  The slice's absolute 1e-12 on χ² is kept as written.
