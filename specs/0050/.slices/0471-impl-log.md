# Step 0471 — impl-log

## Progress

- Read the task, the system prompts, the project prompt, operator rulings R0001–R0005 and the slice. None of the
  rulings touches this step's files; each says to preserve earlier behavior, and this step does not touch it.
- Read the readers this step reuses: `Studies.fs` (`OpenedMaterial`, `currentPhysics`, `crystalOpt`, `unknownsWith`,
  `StudyContext.itemsUsedBy`), `ElementId.fs` (`Sample`, `SampleLayer`), `PlannerSymmetry.fs`
  (`CrystalOrientation.normalAxisOpt`) and `ItemModels.fs` (`Thickness.parameterValueOpt`).

- `MeasurementPlanner.fs`:
  - `PlannerError` gained `UnopenedPlannerMaterialErr` and `NonCrystalPlannerMaterialErr`, with their `describe` arms.
  - Added `PlannerPlate`, `PlannerPrior`, `PlannerInputs` and `Study.plannerInputsWith`, after the lab profile.
  - Added opens for `Berreman.Media`, `Placement`, `MaterialLibrary` and `Studies`.
  - Step 470's comment "read in step 471" now names `PlannerPlate`.
- `PlannerInputsTests.fs`:
  - New fixtures: the crystal entry and version, the QZ-Z / QZ-X / film / infinite samples and their stack-sample
    drafts, the study and the context.
  - Three new facts. The `unresolvedReasonOf` match now covers the new error cases.
- `PlannerReportTests.fs`: its exhaustive `PlannerError` match gained the two new cases (FS0025 is an error under
  `--warnaserror+:25`).
- Build: green in 84 s.
- Filtered run of `PlannerInputsTests` and `PlannerReportTests`: 18/18.

- Whole constructor suite: 1861/1861.
- Elevation: OK.
- The 21 invariants: nothing found.
- Files are LF-only (CR byte count 0); `--ignore-cr-at-eol` numstat is identical.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasurementPlanner.fs` (+113 / −2)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerInputsTests.fs` (+266 / −4)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlannerReportTests.fs` (+2 / −1). This is the forced
  exhaustive-match arm, outside the listed effects; see Gotchas.

## Testing state

Development runs only; the arc-runner's gate engine is the authority.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | succeeded, 0 errors; warnings only `NU1902` (exempt) and vendored-MathNet `SYSLIB0051` (pre-existing) | 84 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` | 231 s | 600 s |
| constructor-unit-tests | 1861/1861 passed (checkpoint 1858, +3) | 108 s | 180 s |
| 21 × invariant-s45-* | each decoded `git grep` finds nothing | < 5 s | 300 s |

No run exceeded its deadline.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0471-build.log`
- `0471-constructor-unit-tests.log`
- `0471-elevation.log`
- `0471-invariant-commands.txt`
- `0471-invariants.log`

## Gotchas

- **PlannerReportTests.fs.** Adding the two mandated `PlannerError` cases turned step 469's exhaustive match into
  FS0025, which is an error under `--warnaserror+:25`. One arm became an or-pattern; no assertion or identity changed.
  This is the same precedent as step 470.
- **Tabulated fixture.** The tabulated material reuses the public `TabulatedStudyFixtures` (step 460) rather than
  copying it. The crystal entry is new and test-local, because `StudyMaterialTests.crystalEntry` is private.
- **Brief vs code.** The brief writes `StackSampleChoice (version, sample)`. The actual case carries the model too, so
  the match is `StackSampleChoice (_, version, sample)`.
- **Rulings.** R0001–R0005 bind earlier steps (0427, 0428, 0436). This step touches none of their files or behavior.
- **Invariant runner.** The first attempt ran the greps through Python `subprocess(['bash', ...])`, which reached the
  WSL stub, and every line read FOUND. Rerunning them in Git Bash found nothing.
