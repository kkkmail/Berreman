# Step 0389 — impl log

## Progress

- [x] Read protocol, project prompt, AGENTS.md and the slice. There is no operator ruling or operator note for this step.
- [x] StudyEngine.fs:
  - `Angle.parameterValue` and `Retardance.parameterValue`;
  - the `StagedPathFitErr` case and its render;
  - the shared family resolution, moved out of `engineWith`;
  - the stage-1 constant table;
  - `StagedStudy.calibrateLinear`.
- [x] StudyEngineTests.fs: the two new facts and the widened describe fact. The class passes 14/14.
- [x] Mutation check of the main new fact.
- [x] Full build, whole constructor suite, `MuellerReconstructionTests`, elevation, the twenty s38 invariants, LF check.
- [x] State-of-the-world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`

## Decisions

- **The family resolution is moved, not copied.** `engineWith`'s local `heldIn`, its `fitted` list and its
  `calibrationRunOf` are now private module functions:
  - `heldItemsIn` and `fittedHeldIn`, the moved `heldIn` and `fitted`;
  - `calibrationRunOf`, its body unchanged.

  `engineWith` and `calibrateLinear` both call them. `heldIn` read `kit.setups` and `this.items`, and the moved functions
  read `context.kit.setups` (the same setups, since only `items` was replaced) and the staged study's `items`. The
  behaviour is identical, and the existing engine facts all pass.
- **The parameter-to-constant table is written once.** `CalibrationRunFamily.stageOneConstants` now answers private
  `StageOneConstant` records, each pairing a parameter with its value read from a `StageOneCalibrationData`.
  - `engineWith` maps them to their parameters, which is exactly the list it read before.
  - `calibrateLinear` evaluates them on stage one's answer.
  - A second, parallel per-family list of values would have been a re-transcribed table.
- **Placement of the two `parameterValue` extensions (a deviation from the slice's "after `fitNonlinear`").** They are
  declared just before `StageOneConstant`, ahead of `engineWith`, because the constant table inside
  `CalibrationRunFamily` reads them and F# compiles in declaration order. They are in the same module, so the contract
  symbols `StudyEngine.Angle.parameterValue` and `StudyEngine.Retardance.parameterValue` are unchanged. A one-line
  comment there says why. `calibrateLinear` itself sits after `fitNonlinear`, as the slice asks.
- **The `parameterValue` bodies are `ParameterValue this.value`.** ELEV001 sanctions `Ctor this.value` inside a member
  of the source type, and ELEV006 only pairs `to<Pascal>` names. With `ParameterValue.angle` the pair is therefore not a
  reported bijection, which matches the precedent `ParameterValue.thickness` / `Thickness.parameterValueOpt`.
- **CPL1's fast-axis offset** is `(thetaRel - retarderComponentOffset).parameterValue`. It reuses the module's own 45°
  (`retarderComponentOffset`, the built-in circular polarizer's retarder at 45° within it) and `Angle`'s `(-)`.
- **The pre-fit rows are taken as the check answers them.** `prefitCheckWith` → `ExperimentDataLoad.loadAndValidate`
  already applies `points.keptBy data.exclusions` (step 314). Applying `keptBy` a second time would renumber the kept
  points and exclude the wrong ones. "Exclusions applied by `keptBy`" is therefore satisfied by the prefit itself, and
  the code comment says so.
- **A run stage one reads is refused, never dropped silently.**
  - If its check failed, the refusal is `StudyForwardFitErr (PrefitFailedErr (name, error))`, as `Study.dataWith`
    refuses a failed check.
  - If its points are not capture rows, the refusal is `StudyForwardFitErr (ReadingMismatchErr …)`.
  - Both are existing cases, so `StudyFitError` gains only `StagedPathFitErr`.
  - Dropping such a run would silently move the dark mean or a family's trace.
- **Selection order.** The rows are taken by filtering `prefits`, which are in study order (`enabledExperiments`):
  - every `DarkRun` first;
  - then the fitted runs of each family, `LpLp`, `LpCpl` and `CplLp`.

  A `CplCpl` AIR run (allowed by the calibration shape, with no constants) hands no rows, as the slice lists only the
  three families.
- **An entry no family names keeps its start.** Under the calibration shape every entry is a constant (`engineWith`
  checks exactly this), so the fallback only keeps `calibrateLinear` total. It adds no mapping. When two families name
  one parameter, the first in study order wins; the reference's instruments never share a parameter across families.
- **The member's start** is the layout entries' starts, i.e. the values `startVector` is built from.
- **Tests.**
  - LP1, CPL1 (`cpl1Unknown`), CPL2 (`cpl2Unknown`) and the Phone (`phone`, gain known 1000) are the file's existing
    items, which match the slice's description.
  - LP2 is new: `realLp2Draft` (draft 17: the real linear polarizer, `DialMounted DialNegative`) and `realLp2Unknown`.
    It is added to `drafts` so `choiceOf` resolves it.
  - The benches LP1→LP2 and CPL1→LP2 are new (`lp1AirRealLp2`, `cpl1AirRealLp2`), and LP1→CPL2 is the existing
    `lpCplAir`. Both new benches are in `benches`.
  - The runs scan `VaryDialReading` from 0° to 160° in 9 points (20° steps). Their rows come from the new
    `phoneRowsOf` (1000 · prediction + 5), which `capturesOf` now also calls with its old prediction expression, so its
    numbers are unchanged.
  - `engineOf`'s staging and pre-fit moved into `prefitStaged`, which the new facts reuse.

## Testing state

Run locally on 2026-11-13 on the final code. Every run finished within its effective deadline.

- `build`: exit 0, 31 s (deadline 600 s). The only warnings are the exempt NU1902 advisories, 12 lines.
  - The incremental dev builds of the Optimization project also printed the vendored MathNet `SYSLIB0051` lines,
    as step 388 recorded.
- `constructor-unit-tests`, whole suite: 1684/1684, 103 s (deadline 180 s).
  - The count is 1682 → 1684, +2, as the acceptance requires.
  - The TRX shows `Passed` for each of the two new ids and the widened describe fact, under their exact names.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests`: 25/25, 3 s (deadline 7200 s).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 224s` (deadline 600 s; ELEV004 excluded; known
  findings: 0). This was a standalone run with no `-Base`, so all 13 projects were analyzed. Optimization and Domain
  have 0 findings. No source changed after it.
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all twenty
  find nothing.
- Mutation check. With LP-CPL's dial zero pointed at `lpZero` instead of `cplZero`, the main new fact fails
  (expected 3.1067, actual 0.0524) and the other 13 facts of the class pass. The source was then restored byte for
  byte (`cmp`).
- LF: 0 CR bytes in every file written; `git diff --numstat` equals `--ignore-cr-at-eol`.

No requirement of the slice is left open.

## Artifacts

- `specs/0050/.artifacts/0389-dev-build-optimization.log`, `0389-dev-build-tests.log`: the dev builds.
- `specs/0050/.artifacts/0389-study-engine-class.log`: the `StudyEngineTests` class, 14/14.
- `specs/0050/.artifacts/0389-mutation-build.log`, `0389-mutation-run.log`: the mutation check.
- `specs/0050/.artifacts/0389-build-local.log`: the final full build.
- `specs/0050/.artifacts/0389-constructor-unit-tests-local.log`: the whole constructor suite, 1684/1684.
- `specs/0050/.artifacts/0389-unit-tests-local.log`: `MuellerReconstructionTests`, 25/25.
- `specs/0050/.artifacts/0389-elevation-local.log`: elevation.
- `specs/0050/.artifacts/0389-invariants-local.log`: the twenty invariant greps.
- `specs/0050/.artifacts/0389-scripts/`:
  - `edit_tests.py`, the scripted test-file edit;
  - `run_invariants.py`, which decodes and runs the snapshot's invariant greps.

## Gotchas

- **Stage one reports an axis within [0°, 180°).** `freeCosineFit` wraps every zero by `angleMod180`, so the truth's
  CPL2 zero −2° comes back as 178°. A circular analyzer turned half a turn is the same instrument, so this is the same
  zero, and the fitted value has to equal stage one's constant exactly. The fact therefore checks CPL2's zero as
  `(cplZero + Angle.pi)` within 1e-9, and LP2's 3° directly. The slice's "−2° within 1e-9" is read as the same axis.
  `angleMod180` is private to Domain, which this step does not touch, so the test states the half-turn itself.
- **Through a negative-dial LP2, the reference reads the CPL source's θ mirrored.** `lpFrameSourceModel` fits
  cos/sin 2(α − z) on the raw dial. Under `DialNegative` the sine coefficient flips sign, so
  2θ = atan2(1 − q, u) → 180° − 2θ. Stage one answers θ = 44° for the truth's 46°, so CPL1's offset is −1°, not +1°.
  - The retardance is unaffected, because sin² 2θ is unchanged.
  - The slice asks only that each value equal stage one's own constant, which holds exactly. The fact's truth checks
    therefore cover the two zeros and the two retardances, and the offset is checked against stage one alone. A comment
    in the fact says why.
  - This is the reference's stage-one convention (the port is bit-identical to it, step 388), and this step takes its
    constants as they are.
  - Note for step 390. With θ' = 90° − θ the source's S₂ flips sign, and the negative dial flips sin 2β. Stage two's
    prediction for rows built like this fixture's (from `CplSource (46°, 91°)` behind a negative-dial LP) would
    therefore not reproduce them. A synthetic fixture for stage two has to be built in the reference's own frame.
- **Escaping on this shell.** A long Python heredoc with backticks and apostrophes failed in the Bash tool, so the test
  edit runs from a script file (`0389-scripts/edit_tests.py`).
- `SweptValue` and `CurvePointCount` live in `OpticalConstructor.Domain.Propagation`, which the test file does not open.
  They are qualified rather than opening one more module into the file's label scope.
