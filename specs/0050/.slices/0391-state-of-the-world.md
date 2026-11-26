# Step 0391 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs).

- Steps 386–390 built the engine choice and the three engines: nonlinear, the linear stage 1, and the linear stage 2
  or joint.
- This step joins them into one fit: `StudyFitParam.runFit` takes a `StudyFitRequest` (a study, a stage and a
  calibration use) and answers one `FitRun` stamped by the injected clock. It writes nothing.
- Step 392 drives the staged hand-off through it. Step 393 hosts it on `JobRunner` and writes the run on completion.

## What's working

- Added `StudyFitRequest`, `StudyFitParam` and `StudyFitParam.runFit`. `runFit` stages the study, checks and reads its
  data, dispatches to the engine `engineWith` chooses, and answers a clock-stamped `FitRun`, writing nothing.
- The fit run holds the runs, the data hashes read, every item and material parameter with its state, and the
  entries' starts and fitted values.
- Added `UnknownFitStudyErr`, `the kit has no such study: <reason>`.
- Two new `StudyEngineTests` facts and the widened describe fact. Step 387's polarimeter study and every step 390 linear
  fixture now run through automatic dispatch under stage 2 and joint.

## Tests

Run locally on 2026-11-13/14 on the final code. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors. Timings: 45 s cold, 24–27 s incremental (deadline 600 s). The only warnings are the exempt
  NU1902 advisories (12 lines), and the log holds no `error`.
- `constructor-unit-tests`, whole suite: 1689/1689, 114 s (deadline 180 s).
  - The count is 1687 → 1689, +2, as the acceptance requires.
  - The TRX shows each of these `Passed` under its exact name:
    - `a fit run records its name, stamp, stage, engine, fitted runs, the data it read and the vector it used and
      produced` (12.3 s);
    - `a fit naming no study of the kit, or refused by its stage, records nothing`;
    - the widened `every study-fit error renders its condition and its payload`.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests` + `BerremanTests.MuellerInverseTests`: 45/45, 710 s
  (deadline 7200 s).
- `elevation`, standalone over all 13 projects: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 226s` (deadline
  600 s).
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all twenty
  find nothing.
- Mutation checks, each restored byte for byte afterwards. Every one fails the new fact:
  - forced nonlinear dispatch;
  - the attachment's own hash for a changed file;
  - no material components;
  - entry starts from the fitted values.
- Every file this step wrote is LF (0 CR bytes); `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1689
```

## Architecture

- **One pipeline, in the brief's order.**
  - The study is found by name, else `UnknownFitStudyErr`.
  - It is staged by `stageWith` (`FitStageErr`).
  - Over the staged context: `prefitWith` with what the study's latest fit run (by stamp) read, then `dataWith`
    (`StudyForwardFitErr`).
  - The engine is `engineWith`'s choice: `calibrateLinear` for a linear `CalibrationStage`, `reconstructLinear` for
    any other linear stage, else `fitNonlinear` under the watch.
  - Then the clock, read once, and the record.
- **The record's kit-derived parts are settled before any engine runs.** `dataRead` and `parameters` are pure reads, so
  a refusal never costs a fit.
- **`dataRead`.** Each prefit with data, in order, at the hash the fit read:
  - `current` for changed since attach or since the last fit run;
  - the attachment's own hash when unchanged;
  - a missing file is refused as `PrefitFailedErr (_, MissingDataErr _)`, never recorded.
- **`parameters`.** `Experiment.itemsUsedBy` the fitted runs over the staged items, each parameter with its state, then
  every opened material's components.
- **`entries`.** The solution's layout, each entry with its start-vector and fitted values.
- `StudyFitParam` is `[<ReferenceEquality>]`, the `*Param` convention, built once by the caller: step 393's
  `StudyFitEnvironment`.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The series fixture is not driven through `runFit`.**
  - Step 386 makes a study nonlinear when a fitted path holds two unknown matrices. Step 390's series fact reaches
    stage 2 only by calling `reconstructLinear` directly.
  - Measured: `runFit` dispatches it to `fitNonlinear`, and the watch is told iterations.
  - Every case the brief and acceptance name is driven under both stages:
    - the partial variants with 15, 14 and 1 free;
    - the ideal CP of either handedness (rank 1, one entry);
    - the constant and compound optics;
    - the fixed surrounding objects;
    - the equality links (28 entries, rank 28, links exact);
    - the rank-deficient case (`ObjectSolveFitErr` after one real solve, the clock never read).
- **Shared labels.** `StudyFitRequest`'s labels `study` / `stage` / `calibrationUse` are shared with `StagedStudy` and
  `FitRun`. Annotate a `FitRun` or `StagedStudy` record expression wherever `StudyEngine` is open.
- **Step 385's study is rebuilt locally in `StudyEngineTests`.** Its fixture is private to `FitRunTests`, outside this
  step's edits.
- **A `ChangedSinceFitFlag` leaves no trace in the run.** It records the same hash as unchanged, so only the
  changed-since-attach hash is observable, and that is the one checked.
- **Time.** The fit-run fact runs the polarimeter's nonlinear fit twice, as the brief compares with `fitNonlinear`
  (12 s). The suite is at 114 s of its 180 s.

## Changelog

- 2026-11-13/14 — Step 0391: one fit, one fit run.
  - `StudyFitRequest`, `StudyFitParam` and `StudyFitParam.runFit` stage, check, read, dispatch and stamp a `FitRun`
    holding the runs, the data hashes, every parameter read and the vector used and produced, writing nothing.
  - New `UnknownFitStudyErr`.
  - Two new `StudyEngineTests` facts and the widened describe fact.
