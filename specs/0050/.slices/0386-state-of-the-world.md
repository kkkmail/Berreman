# Step 0386 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs). Step 383 gave the fit run its
`StudyEngineKind` tag and step 385 staged a study for one fit (`StagedStudy`). This step opens
`OpticalConstructor.Optimization/StudyEngine.fs` and makes the engine choice automatic: a pure structural rule over the
staged study decides between the linear path (S10.3's staged Mueller path) and the nonlinear one. Later S11.3 steps
build on it in the same file:

- 387 adds the nonlinear engine;
- 389 and 390 add the linear calibration and object reconstruction, each taken only where `engineWith` found its shape;
- 391 dispatches through `runFit`.

## What's working

- Added `StudyFitError` (`StudyForwardFitErr`, `FitStageErr`), each case rendered with its payload's `describe`.
- Added `CalibrationPolarizer` (linear or circular) with `familyWith`, giving LP-LP, LP-CPL, CPL-LP or CPL-CPL, and
  `ItemModelChoice.calibrationPolarizerOpt`:
  - the ideal-LP preset and the real linear polarizer are linear;
  - the built-in LP-then-retarder-at-45° compound is circular;
  - everything else is none.
- Added `StagedStudy.engineWith`, which answers `LinearStudyEngine` in two cases and `NonlinearStudyEngine` otherwise:
  - under stage 1, for the reference AIR calibration over capture rows;
  - under stage 2 or joint, for any non-empty subset of free sixteen-element black-box coefficients, linked or not,
    each fitted path bearing at most one unknown matrix behind fixed optics.
- Counted unknown-bearing matrices per slot, so a double pass (one free black box held in two slots of a path) is
  nonlinear.
- Added five `StudyEngineTests` facts over test-local items, setups, capture-row prefits and synthetic truths. The
  black-box fact also covers the double pass.

## Tests

Run locally on 2026-11-13 on the final code of attempt 2. Every run finished within its effective deadline:

- `build`: pass, 0 errors, 43 s (deadline 600 s). The only warnings are the exempt NU1902 advisories.
- `constructor-unit-tests`: pass, 1672/1672, 74 s (deadline 180 s). The count is 1667 → 1672, so +5, matching the
  acceptance.
  - All five new identities pass under their exact slice names (checked in the TRX).
  - The double-pass case is an extra assertion inside the existing black-box fact. It adds no identity.
  - Against the old code it was red: `Expected: NonlinearStudyEngine`, `Actual: LinearStudyEngine` at the new
    assertion.
- `elevation`: pass, `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 214s` (deadline 600 s). This was a
  standalone run with no `-Base`, so every project was analyzed.
- `unit-tests`: pass, 45/45 for `BerremanTests.MuellerReconstructionTests` and `BerremanTests.MuellerInverseTests`,
  686 s (deadline 7200 s).
- The twenty `invariant-s38-*` must-find-nothing greps: all OK, also run with `--untracked`.

```yaml
gates:
  constructor_unit_tests: 1672
```

## Architecture

- `engineWith` is pure and structural. It reads no rank, score or threshold and adds no option, so rank deficiency
  reaches the SVD solver (step 390) and does not change the classification. It reads:
  - the staged items, placed into a record copy of the context's kit;
  - the staged study's layout (step 372);
  - each fitted run's slots in light order with its items' `choiceOf`;
  - the study's links (step 364);
  - the prefits (step 377).
- A link follower whose leader is a layout entry takes a fitted value, so it counts as fitted. An item is
  *unknown-bearing* when it holds a fitted parameter.
- Black-box shape:
  - The brief's affine-path condition is checked as "at most one unknown-bearing item per fitted run". This keeps any
    fixed optics around the unknown matrix linear: ideal CP of either hand, constant-Mueller presets, compounds, fixed
    objects.
  - It turns two free matrices on one path nonlinear, whether they are in series or coupled by a link.
  - The count is per slot, not per distinct item. One free item held in two slots (a double pass, M·F·M) bears two
    unknown matrices, and its residual is quadratic in the item's coefficients. Neither `Setup.create` nor
    `SetupSlot.fittingItems` keeps an item out of a second slot, so this configuration can occur.
  - `calibrationPolarizerOpt` is consulted only by the calibration shape.
- The stage-1 constants per family live on a private `CalibrationRunFamily` record as `stageOneConstants`:
  - LP-LP: the analyzer's `dialZero`;
  - LP-CPL: the analyzer's `dialZero` and `c2.retardance`;
  - CPL-LP: the prep's `c2.fastAxisOffset` and `c2.retardance`.
- The sixteen coefficient names are the `SixteenElementMueller` form's own axes, not re-derived.

## Deferred

Nothing.

## Gotchas

- **At most one, not exactly one, unknown-bearing slot per fitted run, counted per slot.**
  - Never deduplicate the held items before counting: a double pass would read as one matrix and be classified
    linear (the attempt-1 defect).
  - A fitted run with no unknown is a constant row. A joint fit also fits calibration (AIR) runs, and step 391 expects
    step 390's AIR-anchored fixture to stay linear under `JointStage`.
- **The calibration shape also refuses a link following a fitted parameter.**
  - Stage 1 of the reference fits each constant independently, so a follower would take its leader's constant rather
    than one fitted under the link.
  - Such a study goes to the nonlinear engine instead. This is my one addition to brief (a).
- **The shipped circular polarizer's compound offsets are matched exactly** against `Angle.zero` and
  `Angle.degree 45.0<deg>`. `BuiltInCatalogueIntegrityTests` already pins the shipped form to those exact doubles.
- **Test fixtures.**
  - An AIR setup needs an (empty) object slot between the prep and the analyzer. Without it the analyzer is not behind
    an object and takes the `PrepSlotRole`.
  - The source slot must hold a source item, or the prefit refuses the unspecified slot.
- **Counting CRs in this shell.** `grep -c $'\r'` counts lines containing an `r`; use `tr -cd '\r' | wc -c`. Every file
  of this step is LF.

## Changelog

- 2026-11-13 — Step 0386: added the study engine's automatic choice (`StagedStudy.engineWith`), `StudyFitError`,
  `CalibrationPolarizer` with `familyWith` and `ItemModelChoice.calibrationPolarizerOpt`, and five `StudyEngineTests`
  facts.
- 2026-11-13 — Step 0386 attempt 2: counted unknown-bearing matrices per slot, so a free black box held in two slots
  (a double pass) is `NonlinearStudyEngine`; the black-box fact asserts it under both object stages.
