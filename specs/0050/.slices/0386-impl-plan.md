# Step 0386 — impl plan

## Approach

Add `OpticalConstructor.Optimization/StudyEngine.fs` (`module StudyEngine`), compiled directly after
`StagedMuellerPath.fs`, holding in order:

1. `StudyFitError` (`StudyForwardFitErr of error : StudyForwardError`, `FitStageErr of error : FitRunError`) with
   `describe`.
2. `CalibrationPolarizer` (`LinearCalibrationPolarizer` | `CircularCalibrationPolarizer`, derived, no tag) with
   `familyWith`, and the extension `ItemModelChoice.calibrationPolarizerOpt` (ideal-LP preset and real linear polarizer
   → linear; the built-in LP-then-retarder-at-45° compound → circular; else `None`).
3. `StagedStudy.engineWith (context) (prefits) : StudyEngineKind`, a pure structural rule over the staged items, the
   staged study's layout (`parameterLayoutWith` over the context with `kit.items = this.items`), the fitted runs' slots in
   light order with their items' `choiceOf`, the study's links and the prefits:
   - `CalibrationStage`: every fitted run is an AIR run of one prep and one analyzer reference polarizer, empty objects,
     an intensity detector and capture rows; LP-LP, LP-CPL and CPL-LP each present; every entry is an item-owned stage-1
     constant of its family (LP-LP analyzer `dialZero`; LP-CPL analyzer `dialZero`, `c2.retardance`; CPL-LP prep
     `c2.fastAxisOffset`, `c2.retardance`); no link follows an entry.
   - `ObjectStage` / `JointStage`: every entry is an item-owned `m<r><c>` of a sixteen-element black box; every link
     follower of an entry is too; every fitted run resolves, is read by an intensity detector over capture rows, and holds
     at most one unknown-bearing item (an item with an entry or a follower of one), so fixed optics of any kind surround
     it; `calibrationPolarizerOpt` is not consulted.
   - otherwise `NonlinearStudyEngine`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj` (one Compile line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (five new facts, test-local fixtures)

## Risks

- ELEV rules on new names (DU cases, extension members) — grep for collisions before building.
- Prefit validity in fixtures: source slot must hold a source item, data must be `CapturePoints` read through a stub
  `ExperimentDataProxy`, descriptions inside the scan range.
- Synthetic truth for the black-box fixed coefficients: observations generated with the reference glue
  (`designRow` · `vecColumnMajor`) from the same `retarderMueller` truth whose elements the fixed coefficients hold.

## Attempt 2 (code-judge route-back)

- In `blackBoxShape`, count unknown-bearing occurrences per slot: drop the `List.distinct` in `oneUnknownMatrix`. With
  that change, one free black box held in two slots (a double pass) is `NonlinearStudyEngine`.
- Prove it red first, inside the existing black-box fact. Add a bench with the free QZ in two object slots behind the
  published instruments, with capture rows from `qzTruth * qzTruth`, and assert `NonlinearStudyEngine` under both
  object stages through `engineOf`. No new test identity, and every existing assertion kept.
