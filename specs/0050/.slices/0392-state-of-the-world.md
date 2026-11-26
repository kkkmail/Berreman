# Step 0392 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs).

- Steps 386–390 built the engine choice and the three engines.
- Step 391 joined them into one fit, `StudyFitParam.runFit`.
- This step drives the staged hand-off end to end over the engines (001 §L). Stage 1 calibrates the instruments on
  the AIR runs. Its values are published as item revisions (`Item.publishRevision`, step 235). Stage 2 then fits QZ
  with those values held fixed, or frees them as priors with their σ.
- These are tests only; no product code changed. S11.5's `PublishCalibration` will perform the same write with S11.4's
  sigmas; these facts use a test-local σ of 1e-6.
- Step 393 hosts `runFit` on `JobRunner` and writes the run on completion.

## What's working

- Added `publishing stage one fixes the instruments' values for stage two`. Stage 2 is refused until stage 1 publishes;
  stage 1's five values are published from its run; stage 2 then reads them exactly and recovers M_QZ within 1e-9.
- Added `as priors, stage two frees the published calibration with its priors for the nonlinear engine`. Under
  `CalibrationAsPrior` the engine is nonlinear, and the layout frees each calibration at its value within ± 5e-5 with
  its prior (value, 1e-6).
- Built one study over steps 389 and 390's runs. QZ's rows are observed by the run law at the values the reference stage
  one reads.
- Factored the shared helpers instead of copying them: `revisedWith`, `stageOneAirRows`, `stageOneValuesOf`, and
  `prefitStaged`'s calibration use.

## Tests

Run locally on 2026-11-14 on the final code. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors, 40 s (80 s for the first build, which recompiled MathNet; deadline 600 s). The log holds
  no `error`. Warnings:
  - the exempt NU1902 advisories;
  - the vendored MathNet `SYSLIB0051` pair, already named as existing in step 0185's operator note. It shows only when
    `MathNetNumerics/Numerics` recompiles.
- `constructor-unit-tests`, whole suite: 1691/1691, 116 s (deadline 180 s).
  - The count is 1689 → 1691, +2, as the acceptance requires.
  - The TRX shows each of these `Passed` under its exact id:
    - `OpticalConstructor.Tests.StudyEngineTests.publishing stage one fixes the instruments' values for stage two`;
    - `OpticalConstructor.Tests.StudyEngineTests.as priors, stage two frees the published calibration with its priors
      for the nonlinear engine`.
  - Step 389's refactored `the calibration stage recovers …` also passes, with its assertions unchanged.
- `elevation`, standalone with no `-Base`, so all 13 projects: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s),
  217s` (deadline 600 s). The test project is outside ELEV's scope; no product file changed.
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all twenty
  find nothing.
- Mutation checks. Each fails exactly its target fact, and each source was restored byte for byte, checked with `cmp`:
  - M1: rows made at step 390's truth instead. The hand-off fact misses M_QZ by 5.7e-4.
  - M2: `ParameterState.currentValue` reads a calibration 1e-4 off. The hand-off fact misses M_QZ by 6.5e-5.
  - M3: `CalibrationAsPrior` frees nothing. The priors fact gets `LinearStudyEngine`.
  - M4a: a box of 40 σ. The priors fact fails on the bounds.
  - M4b: the prior's σ doubled. The priors fact fails on the prior.
- The file this step wrote is LF (0 CR bytes); `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1691
```

## Architecture

- **The fixture's truth is what the reference stage one reads.**
  - `handOffFixture` reads step 389's AIR rows with `calibrateFromAirRows`, the S10.3 reference.
  - It sets the three instruments *known* at those constants and observes QZ's sixteen free-session Science runs (step
    390's `freeFamilyRuns "Q"`) through the run law.
  - So M_QZ comes back within 1e-9 only if stage 2 uses exactly the published values.
  - The rows are observed with known states, so they never share the calibrated-value read that stage 2 is checked on
    (M2).
- **One study, one kit.**
  - Study order: S1–S3, K1, then Q1–Q16.
  - The kit holds the five parameters unknown (`stageOneInstruments`), plus the plate and QZ all free.
  - Slots reference items by id, so step 390's family benches serve the unknown items unchanged.
- **The hand-off** is `publishedBy`: every kit item owning s1 entries goes to its next revision through
  `Item.publishRevision`. The entries are published as `CalibratedParameter { value = fitted; sigma = 1e-6; fitRun =
  s1.name }` into the forward context's items. s2's `StudyFitParam` holds `fitRuns = [ s1 ]`, as the kit holds it once
  step 393 writes it.
- **The priors fact stages through `stageWith`** (step 385, by way of the widened `prefitStaged`). It reads
  `engineWith` (step 386) and `parameterLayoutWith` (step 372), and runs no nonlinear fit: the brief asks for the
  choice and the layout.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Step 390's Science rows cannot meet a faithful hand-off.**
  - They were made at the stage-1 truth, CPL1's retarder offset +1°. Stage one reads −1° through LP2's negative dial
    (step 389).
  - Measured: stage 2 at the published values then misses M_QZ by 5.7e-4 (M1).
  - Any later fact that combines stage one's reading with rows made at the truth meets the same gap.
- **CPL2's published dial zero is 178° (3.11 rad), outside its unknown's start box [−0.5, 0.5].**
  - `publishRevision` accepts it, as only the state is checked.
  - Under `CalibrationAsPrior` its box is 178° ± 5e-5, so the box follows the calibration, not the old unknown.
- **Shell:** a long Bash heredoc holding apostrophes fails in this harness. The section was written with the Write tool
  under `.artifacts/0392-scripts/` and appended with Python.
- **Time:** the suite is at 116 s of its 180 s. The two new facts take about 1.4 s together.

## Changelog

- 2026-11-14 — Step 0392: the staged hand-off.
  - Two new `StudyEngineTests` facts drive stage 1 → publish → stage 2 through `runFit`: values held fixed, then freed
    as priors for the nonlinear engine.
  - Shared fixture helpers factored, with no product change.
