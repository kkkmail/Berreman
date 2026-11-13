# Step 0383 — impl log

## Progress

- [x] Domain: `FitRuns.fs` and its compile line after `StudyForward.fs` (2026-11-13)
- [x] Tests: `FitRunTests.fs` and its compile line, last in the test project (2026-11-13)
- [x] Development runs: build, constructor-unit-tests, elevation, the twenty s38 invariants (2026-11-13)

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/FitRuns.fs` (new). `module FitRuns` in
  `namespace OpticalConstructor.Domain`; its doc is "The record of one study fit and the stage it fits (spec 0050
  S11.3)". In the how_to's order:
  1. `FitStage` = `JointStage` | `CalibrationStage` | `ObjectStage`, each case with its one-line doc. `value` 1–3,
     `tryOfWire`, `text` (`Joint`, `Stage 1 (calibration)`, `Stage 2 (objects)`) and `all`, as `FitMode` has them.
  2. `CalibrationUse` = `CalibrationHeldFixed` | `CalibrationAsPrior`: `value` 1–2, `tryOfWire`, `text` (`fixed`,
     `as priors`) and `all`.
  3. `StudyEngineKind` = `LinearStudyEngine` | `NonlinearStudyEngine`: `value` 1–2, `tryOfWire`, `text` (`linear`,
     `nonlinear`) and `all`.
  4. `FitRunNotes of string` (`.value`, `none`), `FitChiSquared of float` (`.value`) and `FitMemberFailure of string`
     (`.value`).
  5. `FitMemberOutcome` = `MemberConverged of fitted : ParameterValue list * chiSquaredOpt : FitChiSquared option` |
     `MemberFailed of failure : FitMemberFailure`, with no int tag. `FitRunMember = { start; outcome }`.
  6. `FitRunEntry = { parameter : StudyParameterRef; owner : UnknownOwner; start; fitted }` and
     `FitRunParameter = { parameter; state : ParameterState }`.
  7. `ObjectMatrixFit = { items : ItemId list; elements : ParameterValue list; rank : MatrixRank; rmse : ResidualRmse }`.
  8. `FitRun`, with the fifteen fields in the how_to's order and `static member currentSchemaVersion` =
     `FileSchemaVersion 1`.
  9. `type StudyName with member this.fitRunNameAt (at : ClockTime) : FitRunName` and
     `type FitRunName with member this.fileStem : FileStem` (`FileStem.ofText` of its text).
  10. `type NonlinearSolverError with member this.failureText : FitMemberFailure`. It renders
      `the solver did not converge: <reason>` and `the solve request is not valid: <reason>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one line,
  `<Compile Include="FitRuns.fs" />`, after `StudyForward.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitRunTests.fs` (new). `module FitRunTests` in
  `namespace OpticalConstructor.Tests`, with the three facts the step names:
  - `a fit run is named by its study's file stem and the clock's instant`;
  - `a fit stage, a calibration use and an engine round-trip their wire tags and render their text`;
  - `a solver failure renders the reason a multi-start member failed`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one line,
  `<Compile Include="FitRunTests.fs" />`, last in the compile list, with no comment.

## Decisions

- **The stamp is formatted after `ToUniversalTime()`.** how_to (9) asks for the instant "invariant culture, UTC".
  `ClockTime` is UTC by its clock's contract (`KitOpening.fs` supplies `DateTime.UtcNow`). A `Utc`-kind value is
  unchanged by the call, and a `Local`-kind one is converted, so the `Z` written is always true. The format is
  `yyyyMMdd'T'HHmmss'Z'` with `CultureInfo.InvariantCulture`.
- **`FitStage`'s cases carry one-line docs; the other two unions do not.** how_to (1) gives each stage case a meaning
  ("every unknown at once", "stage 1: …", "stage 2: …"), which the docs record. `CalibrationUse` and
  `StudyEngineKind` are explained by their type docs, as `FitMode` is.
- **The solver-failure fact checks equality and the reason's presence.** how_to: "render their two texts, each holding
  its reason". The fact asserts the whole `FitMemberFailure` and that its text ends with the reason.
- **No catch-all case.** `tryOfWire` answers `None` for any other tag, which is the caller's typed error, as `FitMode`
  and `ParameterScope` do (ELEV017).

## Testing state

Development runs on 2026-11-13. These are the worker's checks; the arc-runner's gate engine is the authority.

| Run | Outcome | Time (deadline) |
|---|---|---|
| build (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`) | exit 0, 0 errors | 79 s (600 s) |
| constructor-unit-tests (the gate's own command, whole suite) | `Passed!`, succeeded 1656, failed 0 | 78 s (180 s) |
| elevation (`ASG_ARC_BASE_COMMIT` = `.arc-base-json` origin, `-ExcludeRules ELEV004`) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0 | 217 s (600 s) |
| twenty s38 invariants (each decoded `git grep`) | all twenty find nothing | < 5 s (300 s each) |

- **constructor-unit-tests count.** 1653 (step 0382's checkpoint) → 1656: the three new `FitRunTests` facts, +3 as
  the acceptance requires. All three are `Passed` in the TRX.
- **Elevation's suppressed count.** It is 1540, against 1539 at step 0382; only the Domain project moved (435 → 436).
  The SARIF report shows the one new finding is ELEV004, `FitRuns.fs:188`: the `at.value` that formats the stamp. That
  rule is excluded in the gate command and measured only (AGENTS.md, spec 0048 `.manual/002` R7).
  `KitLockHolder.describe` formats `acquiredAt.value` the same way. A `ClockTime` stamp member would be a member the
  step does not add ("No other type or member is added").
- **The elevation run analyzes all 13 projects.** `Berreman.slnx` changed since the arc origin, so the gate widens to
  every project.
- No run exceeded its deadline.
- **Build warnings.**
  - NU1902 (exempt).
  - SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`, as in steps 0360–0382. It is not from this
    step's code.

## Artifacts

- `specs/0050/.artifacts/0383-dev-build.log`
- `specs/0050/.artifacts/0383-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0383-dev-elevation.log`
- `specs/0050/.artifacts/0383-dev-invariants.log`

## Gotchas

- **`UnknownOwner`'s doc still says "Derived, never stored, so it carries no tag".** A fit run's entries now hold an
  owner, which step 0384's file stores in the serializer's case form (how_to (6)). The "no tag" half stays true.
  `Studies.fs` is outside this step's authorized edits, so the doc is left as it is.
- **Record labels are shared.** `FitRunEntry` and `FitRunParameter` both have `parameter`; `FitRunMember` and
  `FitRunEntry` both have `start`. `ObjectMatrixFit` shares `rank` and `rmse` with `ReconstructedMatrix` and
  `LeastSquaresSolution`. F# infers a record expression from its labels, so a later expression that opens
  `FitRuns` and is ambiguous needs a type annotation.
