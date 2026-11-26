# Step 0388 — impl log

## Progress

- [x] Read protocol, project prompt, AGENTS.md, slice (no operator ruling or note for this step).
- [x] MuellerReconstruction.fs: `Family.text`, `MatrixKind.text`, `SolverError.describe`, `AnalyzerModel.linearAt` /
  `circularAt`, `LeastSquaresSystem.concat` / `ofProjections`; `buildDesign` calls `ofProjections`.
- [x] StagedMuellerPath.fs: `StageOneAirRows`, `calibrateFromAirRows`, `airAnchorGainOf`, `projectCaptureRow`,
  `StagedMuellerError.describe`; the reference path calls them and keeps no private copy.
- [x] StagedMuellerPathTests.fs: five facts, under the five exact ids of the effects.
- [x] Dev build green; class run 10/10; mutation check of fact 1 (see Testing state).
- [x] Full rebuild, constructor-unit-tests whole, elevation, `MuellerReconstructionTests`, s38 invariants, LF check.
- [x] Bit-identity of the reference path measured against a build of HEAD.
- [x] State-of-the-world written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StagedMuellerPathTests.fs`

## Decisions

- `LeastSquaresSystem.concat` is an intrinsic member of the record: it reads only the record. `ofProjections` is an
  intrinsic type extension placed right after `designRow`, because it calls `designRow`, which is declared after the
  record (AGENTS.md: towards a later declaration, an extension declared after it). It sits in the same module and
  file, so it compiles as a real static member of `LeastSquaresSystem`.
- `ofProjections`' body is the record `buildDesign` built (same lambdas, same `List.toArray`). `concat`'s body is the
  `Array.concat` pair `systemOf` built. Both are moved, not copied.
- `airAnchorGainOf` takes the run's rows already selected. The emptiness check moved into it, with the
  `EmptyCaptureSetErr` text unchanged, so the existing fact naming `LP-(AIR#0)-LP-2R` still holds. The private
  `AirBlockRows` record is deleted: it existed only for non-emptiness and `meanTime`, and both moved. The private
  `airBlock` and `fitAirGain` are gone. `airAnchor` keeps the run selection (the `toExperimentName = Ok run` filter) and
  passes the selected rows to `airAnchorGainOf`.
- The mean-time arithmetic is unchanged: `t0` is the first row's ticks, then the mean of the deltas as `float`, then
  `Math.Round`. The list is the one `firstRow :: laterRows` rebuilt, so the gains and times are bit-identical.
- `prepareRow` is deleted. `reconstructStaged` keeps a local `projectionOf` closure, the five families' shared adapter
  (reading → φ, session rule → gain), which only calls `projectCaptureRow`. It is not a copy of the projection.
- `reduceAirTraces` loses the `(AIR#0)` filter; `calibrateStageOne` applies the same predicate to each family file
  before calling `calibrateFromAirRows`. `List.filter` preserves order, so the same rows reach the same `groupBy`.
- `StageOneCalibrationData.lpAnalyzer` / `cplAnalyzer` are one-liners over `linearAt` / `circularAt`. The
  `- retardance.degrees * 1.0<deg>` expression moved verbatim, so the double is the same.
- The test file reaches `ItemModels.DialSign`, `Library.PolarizerKind`, `Experiments.ExperimentName` and
  `Studies.MissingAnchorGainErr` qualified rather than opening those modules. Studies and ItemModels declare records
  with the labels `experiment`, `capturedAt`, `description` and `kind`, and the file's existing `airRows` literal leans
  on label resolution. This is a scope choice, not a collision fix.

## Testing state

Run locally on 2026-11-13 on the final code. Every run finished within its effective deadline.

- Build: exit 0, 0 errors, 43 s (deadline 600 s). The only warnings are the exempt NU1902 advisories, 12 lines. The
  earlier dev build also printed the vendored SYSLIB0051 lines; see Gotchas.
- `constructor-unit-tests`, whole suite: 1682/1682, 104 s (deadline 180 s). The count is 1677 → 1682, +5. The TRX
  shows each of the five new ids `Passed`.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests`: 25/25, 3 s (deadline 7200 s).
- Elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 222s` (deadline 600 s). This was a standalone run with
  no `-Base`, so every project was analyzed. Domain and Optimization have 0 findings.
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all exit 1
  with no hit.
- Bit-identity. HEAD's Domain and Optimization (with Core, Analytics, OpticalProperties and MathNet) were extracted by
  `git archive` into a scratch folder under the artifacts and built there. The seven lab files' 862 rows were parsed
  once by the current Storage parser and frozen as IEEE bits. `reconstructStaged` was then replayed over them on both
  builds. The 85 output lines (every double as its bits) are byte-identical (`cmp`). The 70 MB scratch tree was then
  deleted.
- LF: 0 CR bytes in every file written; `git diff --numstat` equals `--ignore-cr-at-eol`.

No requirement of the slice is left open.

## Artifacts

- `specs/0050/.artifacts/0388-dev-build.log`: first dev build.
- `specs/0050/.artifacts/0388-staged-class.log`: `StagedMuellerPathTests` class run, 10/10.
- `specs/0050/.artifacts/0388-mutation-build.log`, `0388-mutation-run.log`: the mutation check. With the `(AIR#0)`
  filter removed from `calibrateStageOne`, the first new fact fails and the other nine pass. The source was then
  restored byte for byte (`cmp`).
- `specs/0050/.artifacts/0388-build-local.log`: final full build.
- `specs/0050/.artifacts/0388-constructor-unit-tests-local.log`: whole constructor suite, 1682/1682.
- `specs/0050/.artifacts/0388-unit-tests-local.log`: `MuellerReconstructionTests`, 25/25.
- `specs/0050/.artifacts/0388-elevation-local.log`: elevation, OK.
- `specs/0050/.artifacts/0388-bitcheck/`: the bit-identity check.
  - `freeze.fsx` parses the lab files and freezes their rows.
  - `replay.template.fsx` replays the staged path, with `__BIN__` standing for the build folder.
  - `head-numbers.txt` and `new-numbers.txt` are the two outputs, identical.
  - `head-build.log` is the HEAD copy's build.

## Gotchas

- The dev build printed 4 `SYSLIB0051` warnings from the vendored `MathNetNumerics/Numerics` C#, which this step does
  not touch. They show whenever that project recompiles rather than being skipped as up to date (the step 0001–0006
  build logs carry them too). The build gate's `error` regex is case-sensitive and the text holds no `error`.
