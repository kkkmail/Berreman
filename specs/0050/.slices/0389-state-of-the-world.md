# Step 0389 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs).

- Step 386 chose the engine by the study's shape.
- Step 387 added the nonlinear engine.
- Step 388 made the pieces of the staged Mueller path callable.
- This step adds the linear engine's stage 1, the reference AIR calibration over a study's own runs (001 §L; the S11.3
  brief's "Eigenvalue Calibration Method path over the AIR runs"). `StagedStudy.calibrateLinear` hands S10.3's stage one
  the study's Dark runs and its LP-LP, LP-CPL and CPL-LP Calibration runs. Stage one's constants become the
  instruments' parameter values.
- Step 390 builds the object reconstruction on the same path.

## What's working

- Added `StagedStudy.calibrateLinear`. It hands every Dark run's capture rows and each family's Calibration rows to
  `calibrateFromAirRows`, and gives each instrument entry its stage-1 constant.
- Added `Angle.parameterValue` and `Retardance.parameterValue` (radians), and the error case
  `StudyFitError.StagedPathFitErr`, which renders through `StagedMuellerError.describe`.
- Moved the family resolution (`heldItemsIn`, `fittedHeldIn`, `calibrationRunOf`) out of `engineWith` so both callers
  share it, and wrote the parameter-to-constant table once (`StageOneConstant`).
- Added two `StudyEngineTests` facts over a synthetic stage-1 truth, and widened the describe fact to the new case.

## Tests

Run locally on 2026-11-13 on the final code. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors, 31 s (deadline 600 s). The only warnings are the exempt NU1902 advisories (12 lines).
- `constructor-unit-tests`, whole suite: 1684/1684, 103 s (deadline 180 s).
  - The count is 1682 → 1684, +2, as the acceptance requires.
  - The TRX shows these three ids `Passed` under their exact names:
    - `the calibration stage recovers the instruments' zeros, retardances and offset through the reference stage one`;
    - `a calibration stage without a dark run is refused with the staged path's error`;
    - the widened `every study-fit error renders its condition and its payload`.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests`: 25/25, 3 s (deadline 7200 s). No BerremanTests file
  is edited.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 224s` (deadline 600 s). This was a standalone run with
  no `-Base`, so all 13 projects were analyzed. Optimization and Domain have 0 findings. No source changed after it.
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all twenty
  find nothing.
- Mutation check of the main new fact. With LP-CPL's dial zero pointed at `lpZero`, that fact fails and the other 13 of
  the class pass. The source was then restored byte for byte.
- Every file this step wrote is LF (0 CR bytes); `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1684
```

## Architecture

- **One resolution, two callers.** `engineWith`'s private closures `heldIn`, `fitted` and `calibrationRunOf` moved
  verbatim into private module functions. `engineWith` and `calibrateLinear` both call them, so a run's family is
  decided in one place.
- **The stage-1 table is data.** `CalibrationRunFamily.stageOneConstants` answers `StageOneConstant` records, each
  pairing a parameter with its value read from a `StageOneCalibrationData`.
  - `engineWith` reads the parameters, which is the same list as before.
  - `calibrateLinear` reads the values.
  - CPL1's fast-axis offset is `thetaRel − 45°`, through the module's own `retarderComponentOffset`.
- **The two `parameterValue` extensions sit ahead of `engineWith`, not after `fitNonlinear`.** The constant table
  inside `CalibrationRunFamily` reads them, and F# compiles in declaration order. Their module, and so their contract
  symbols, are unchanged; a one-line comment there says why. `calibrateLinear` is after `fitNonlinear`, as asked.
- **Rows come from the pre-fit as it is.** The pre-fit check has already kept the points by `keptBy` (step 314,
  through `loadAndValidate`), so the rows are not re-kept.
- **Unusable runs are refused, not dropped.** A run stage one reads whose check failed, or whose points are not capture
  rows, refuses with an existing `StudyForwardFitErr` case (`PrefitFailedErr` / `ReadingMismatchErr`), as `dataWith`
  does.
- **The answer** is one member from the layout's starts to the constants, with no χ², and no object matrices.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Stage one reports an axis zero within [0°, 180°)** (`angleMod180` in `freeCosineFit`).
  - The truth's CPL2 zero −2° comes back as 178°, which is the same axis.
  - The fitted value must equal stage one's constant exactly, so the fact checks CPL2's zero as `cplZero + Angle.pi`
    within 1e-9. LP2's 3° is checked directly.
  - `angleMod180` is private to Domain, so the test states the half-turn itself.
- **Behind a negative-dial LP2, the reference's stage one reads the CPL source's θ as its mirror.** The sine coefficient
  of the LP-frame fit flips sign, so 46° comes back as 44°, and CPL1's offset is −1°. The retardance is unaffected.
  - The fact checks the offset against stage one's own constant only, and the two zeros and both retardances against
    the truth.
  - For step 390: rows built like this fixture's would not be reproduced by stage two, because the mirrored source flips
    S₂ while the dial flips sin 2β. A synthetic stage-two fixture has to be built in the reference's frame.
- **Test-file scope.** `SweptValue` and `CurvePointCount` live in `Domain.Propagation`, which the test file does not
  open, so they are qualified. `StagedMuellerPath` is now opened in both `StudyEngine.fs` and the test file, and no
  record label collided.
- **Shell escaping.** A long Python heredoc with backticks and apostrophes failed in the Bash tool. Scripted edits run
  from a file under `.artifacts/0389-scripts/`.

## Changelog

- 2026-11-13 — Step 0389: added the linear engine's stage 1.
  - `StagedStudy.calibrateLinear` runs S10.3's stage one (`calibrateFromAirRows`) over the study's Dark and LP-LP /
    LP-CPL / CPL-LP Calibration runs.
  - The new `Angle.parameterValue` and `Retardance.parameterValue` turn the constants into the instruments' values.
  - New `StagedPathFitErr`.
  - The run-family resolution and the stage-1 table are shared with `engineWith`.
  - Two new `StudyEngineTests` facts, and the widened describe fact.
