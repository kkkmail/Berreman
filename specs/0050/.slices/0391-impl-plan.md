# Step 0391 — impl plan

## Approach

One fit: a study, a stage and a calibration use in; one `FitRun` out, stamped by the clock, nothing written (spec 0050
S11.3; 001 §P `RunFit`, §E).

`StudyEngine.fs`, appended last:

1. `StudyFitRequest = { study : StudyName; stage : FitStage; calibrationUse : CalibrationUse }`.
2. `[<ReferenceEquality>] StudyFitParam = { forward; clock; linearSolver; deviates; fitRuns }`.
3. `type StudyFitParam with member this.runFit (request) (watch) : Result<FitRun, StudyFitError>`:
   - the study of `forward.kit.studies` by name, else the new `UnknownFitStudyErr`
     (`the kit has no such study: <reason>`);
   - `stageWith`, an error `FitStageErr`;
   - over the staged context, `prefitWith` of the latest (by stamp) of the study's `fitRuns`' `dataRead`
     (empty before its first), then `dataWith`, an error `StudyForwardFitErr`;
   - the record's data-derived parts before any engine runs, so a refusal never wastes a fit: `dataRead` (each prefit
     with data, in order, at the hash the fit read) and `parameters` (`Experiment.itemsUsedBy` the fitted runs over the
     staged items, each parameter with its state, then every opened material component);
   - `engineWith` → `calibrateLinear` (linear `CalibrationStage`), `reconstructLinear` (other linear stage) or
     `fitNonlinear` with the watch;
   - `clock.now ()` once, then the `FitRun`: schema current, `fitRunNameAt`, stamp, the study's mode, the request's
     stage and use, the solution's engine, `runs` the fitted runs, entries from the layout with start-vector and fitted
     values, members, object matrices, `FitRunNotes.none`.

`StudyEngineTests.fs`:

- New fact `a fit run records its name, stamp, stage, engine, fitted runs, the data it read and the vector it used and
  produced`: the polarimeter study (step 387) through a fixed clock (2026-11-04 09:30:00 UTC), compared with a direct
  `fitNonlinear`. Then every successful step 390 fixture through `runFit` under `ObjectStage` and `JointStage`:
  - the sessions fixture: all free, m33 known (15), m23 known with m32 calibrated, m00 alone;
  - the ideal CP of either handedness (m00 alone, rank 1);
  - the constant-Mueller preparation;
  - QZ between fixed objects;
  - the equality-linked pair;
  - the series;
  - the rank-deficient design answering the solve's own `ObjectSolveFitErr`.

  All of these go through a recording SVD that delegates to the real one. A watch records every iteration report and a
  deviate provider fails on any draw, so a nonlinear solve fails the fact.
- New fact `a fit naming no study of the kit, or refused by its stage, records nothing`: `LR` → `UnknownFitStudyErr`.
  Step 385's study rebuilt here with CPL1's retardance unknown → `FitStageErr (InstrumentNotCalibratedErr _)`. In
  both, the clock is never read and nothing is solved.
- The describe fact gains the new case.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`

## Risks

- The new record's labels `study` / `stage` / `calibrationUse` shadow `StagedStudy`'s and `FitRun`'s for later
  unannotated record expressions. Only the test file opens `StudyEngine`, and every construction there is annotated.
- `MissingDataFileFlag` is not named by the brief. `dataWith` already refuses a failed check, so it is reached only if
  the status and the read disagree. It is refused as `PrefitFailedErr (_, MissingDataErr _)`, so a run never records a
  hash nobody read.
- ELEV: no new module-level function; everything is a member or a closure.
