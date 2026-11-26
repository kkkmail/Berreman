# Step 0385 — state of the world

## Where we are

This step is in spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs). Step 383 gave the domain
its `FitRun` record and the `FitStage` / `CalibrationUse` tags, and step 384 stored fit runs in the kit. This step
decides what one fit fits: the runs and the unknowns of its stage. It works through the study's own levers, so
S11.2's layout, prefit, data and residual (steps 372–379) serve every stage unchanged. The S11.3 steps that follow run
the engines over a `StagedStudy` and scale a freed calibration by its sigma (step 387).

## What's working

- Added `FitRunError` (`InstrumentNotCalibratedErr`, `NoStageRunErr`, `NoStageUnknownErr`), each case rendered with
  its reason.
- Added `StagedStudy`: the study as a stage reads it, the kit's items as the stage holds them, and the runs it fits.
- Added `Study.stageWith`. Stage 1 fits the Calibration runs with only the instruments' unknowns free. Stage 2 fits the
  Science and Drift-check runs and refuses an instrument's uncalibrated item-scoped unknown. Joint fits every residual
  run. Runs outside the stage are disabled; AIR anchors of fitted sessions and Dark runs stay enabled.
- `CalibrationAsPrior` frees each calibration of an item the fitted runs use: its value is the start and the prior
  centre, its sigma is the prior's, and its box is ±50σ.
- Added five `FitRunTests` facts over test-local LP1, CPL1, QZ and Phone items, setups A and Q, and runs A1, A2, E1 and
  K1.

## Tests

Run locally on 2026-11-13. Every run finished within its effective deadline:

- `build`: pass. 0 errors; the only warnings are the exempt NU1902 advisories. Took 47 s (deadline 600 s).
- `constructor-unit-tests`: pass, 1667/1667. Took 74 s (deadline 180 s). The count is 1662 → 1667, so +5, matching
  the acceptance. All five new identities pass under their exact slice names (checked in the TRX).
- `elevation`: pass, `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`. Took 211 s (deadline 600 s). This was a
  standalone run with no `-Base`, so all projects were analyzed.
- The twenty `invariant-s38-*` must-find-nothing greps: all OK. They were also re-run with `--untracked`.

```yaml
gates:
  constructor_unit_tests: 1667
```

## Architecture

- `stageWith` is pure and writes nothing. A staged study is never stored; the fit run records its stage (step 383).
- Every lever is an existing one:
  - residual runs: `fittedRunsIn` (step 372);
  - used items: `StudyContext.itemsUsedBy` (step 362) over the study narrowed to the runs in question;
  - disabling: `withExperimentEnablement` (step 365);
  - holding material components: `withMaterialParameterState` (step 361);
  - scopes: `scopeOf` (step 362);
  - sessions and AIR runs: `sessionsIn` and `gainModelOf` (step 358);
  - the nothing-to-fit check: `unknownsWith` (step 362).
- Refusal order: no run fitted, then (stage 2) an instrument's item-scoped unknown, read on the items as given, then
  nothing to fit.
- Item states are mapped in place. Each state produced keeps `Parameter.validate`'s rules, and none needs an
  `ItemError` the stage error cannot carry.

## Deferred

Nothing.

## Gotchas

- The four-line `SessionGainModel` → AIR-run match mirrors `Study.withSessionGainModel`'s. Sharing it needs a new
  member on `Studies.fs`, which is outside this step's edits and forbidden by the slice.
- The folds over `withExperimentEnablement` and `withMaterialParameterState` use `Result.defaultValue`. Each call is
  given a run or component the study itself holds, so the refusal branch cannot occur.
- Building the Domain project alone recompiles the vendored MathNet and prints its `SYSLIB0051` warnings. The solution
  build does not.

## Changelog

- 2026-11-13 — Step 0385: `Study.stageWith` stages a study for a joint, stage-1 or stage-2 fit, fixed or as priors,
  with `FitRunError`, `StagedStudy` and five new facts.
