# Step 0383 — impl plan

## Approach

A fit run is the append-only record of one study fit, `<study>@<stamp>.fitrun-json` (spec 0050 S11.3; 001 §A, §E,
§I, §L). This step adds only its domain shape and its name; the file is step 0384's and the stage step 0385's.

1. **Domain (`OpticalConstructor.Domain/FitRuns.fs`, new).** `module FitRuns` in `namespace OpticalConstructor.Domain`,
   compiled directly after `StudyForward.fs`, the last Domain file. In the how_to's order:
   - `FitStage` (`JointStage` | `CalibrationStage` | `ObjectStage`), `CalibrationUse` (`CalibrationHeldFixed` |
     `CalibrationAsPrior`) and `StudyEngineKind` (`LinearStudyEngine` | `NonlinearStudyEngine`), each with an int
     `value` (1-based), `tryOfWire`, `text` and `all`, as `FitMode` has;
   - `FitRunNotes` (`.value`, `none`), `FitChiSquared` and `FitMemberFailure` (`.value`);
   - `FitMemberOutcome` (`MemberConverged` | `MemberFailed`, no int tag) and `FitRunMember`;
   - `FitRunEntry` and `FitRunParameter`;
   - `ObjectMatrixFit` (the readouts of `MuellerReconstruction.ReconstructedMatrix`, the items in light order);
   - `FitRun` with `currentSchemaVersion` = 1;
   - `StudyName.fitRunNameAt`, `FitRunName.fileStem` and `NonlinearSolverError.failureText` as type extensions.
2. **Project file.** One `<Compile Include="FitRuns.fs" />` after `StudyForward.fs`, no comment.
3. **Tests (`OpticalConstructor.Tests/FitRunTests.fs`, new).** `module FitRunTests`, appended last in the test
   project's compile list, with the three facts the step names.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/FitRuns.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitRunTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **Unique names (ELEV014/015/016/019).** Every new type, case and extension member was grepped in the tree and is
  free.
- **The stamp's UTC.** `ClockTime` is UTC by its clock's contract (`DateTime.UtcNow`); the name formats the instant
  after `ToUniversalTime`, which leaves a UTC value unchanged, so the `Z` it writes is true.
- **Record-label inference.** `FitRunEntry` and `FitRunParameter` share `parameter`; `FitRunMember` and
  `FitRunEntry` share `start`. Nothing consumes the records yet; a later record expression that is ambiguous takes a
  type annotation.
