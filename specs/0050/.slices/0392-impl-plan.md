# Step 0392 — impl plan

## Approach

Tests only (no product code), in `OpticalConstructor.Tests/StudyEngineTests.fs`, the one authorized edit.

- **Fixture** (`handOffFixture`): one study over steps 389 and 390's runs:
  - step 389's AIR Calibration runs S1–S3 and the Dark run K1;
  - QZ's sixteen free-session Science runs, using step 390's `freeFamilyRuns "Q" [ qzAllFree ] qzTruth`;
  - a kit holding the five instrument parameters unknown (`realLp2Unknown`, `cpl2Unknown`, `cpl1Unknown`), the plate
    and QZ all free.
- **QZ's Science rows** are observed by the run law (`observedOver`) at the values the reference stage one reads from
  the AIR rows (`calibrateFromAirRows`, the S10.3 stage one). Stage 2 can then recover M_QZ within 1e-9 only if it
  uses exactly the values stage 1 published.
  - Step 390's own rows were made at the stage-1 truth, with CPL1's offset at +1°. Stage one reads −1° through LP2's
    negative dial (step 389's Gotcha), so those rows cannot meet 1e-9 after a faithful hand-off.
- **Shared helpers, so nothing is copied:**
  - `revisedWith`, factored out of `publishing`: `publishRevision` or fail.
  - `stageOneAirRows` and `stageOneValuesOf`, factored out of step 389's fact, which then uses them with unchanged
    assertions.
  - `prefitStaged` gains a `CalibrationUse` argument; existing callers pass `CalibrationHeldFixed`.
  - `stageOneRunOf`: stage 1 through `runFit`.
  - `publishedBy`: each item's s1 entries published as `CalibratedParameter { value; sigma 1e-6; fitRun = s1.name }`,
    recorded into the context's items.
- **Fact 1**, `publishing stage one fixes the instruments' values for stage two`:
  - `ObjectStage` is refused with `FitStageErr (InstrumentNotCalibratedErr _)`.
  - s1 is linear, and its five entries are exactly the reference's values.
  - After the publish, s2 (with `fitRuns = [ s1 ]`) is linear. Its parameters hold each calibration exactly. Its
    entries are QZ's sixteen alone, checked through `assertLoneFitRun`, within 1e-9 of M_QZ.
- **Fact 2**, `as priors, stage two frees the published calibration with its priors for the nonlinear engine`:
  - The published kit is staged under `ObjectStage` and `CalibrationAsPrior`, and every pre-fit is ready.
  - `engineWith` chooses `NonlinearStudyEngine`.
  - The layout holds 21 entries: QZ's sixteen without a prior, and each of the five calibrations as an entry starting
    at its s1 value, bounded at ± 5.0e-5, with the prior (value, 1e-6).

## Risks

- `publishRevision` might refuse CPL2's dial zero at 178° (3.11 rad), which is outside the unknown's start box. The
  rules check only the state, so this is expected to pass; it will be verified by running the facts.
- Suite time: the suite sits at ~114 s of its 180 s deadline, and the new facts are expected to add only a few seconds.
