# Step 0358 — impl log

## Progress

- [x] Read the task, the system prompts (`arc-runner.system-md`, `implement_worker.system-md`), the project prompt,
  AGENTS.md and the slice. The operator note is empty and the task carries no operator rulings for this step. Read
  `Studies.fs` and `StudySessionTests.fs` (steps 356–357), `Experiments.fs` (`ExperimentRole`, `SessionName`,
  `ExperimentName`), `Kit.fs` (`ItemId`, `ItemLabel`, `Parameter`, `KitFileKind`, `Item.relabelledAmong`),
  `LibraryEntryFiles.fs` (`FileStem`), `MaterialLibrary.fs` (`EntryName`, `MaterialVersionId`), `ItemModels.fs`
  (`ParameterName`) and `CrystalPhysics.fs` (`CrystalParameters`).
- [x] `Studies.fs`: the study shape (1)–(13), `Study.create` / `sessionsIn` / `gainModelOf` (14)–(16), and the four
  new `StudyError` cases with their `describe` texts.
- [x] `StudyTests.fs` (five facts), appended last in `OpticalConstructor.Tests.fsproj`; `StudySessionTests`' error
  render fact widened to the four new cases.
- [x] Development runs of build, constructor-unit-tests, elevation and the twenty s36 invariants (results under
  Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs` (+256):
  - opens `System`, `System.Globalization`, `CrystalPhysics`, `MaterialLibrary`, `ItemModels`, `LibraryEntryFiles`
    and `Kit` (compile order);
  - `StudyError` gains `InvalidStudyNameErr`, `DuplicateStudyNameErr`, `UnknownStudyExperimentErr` and
    `InvalidResidualWeightErr`, rendered with the brief's texts;
  - after `StudySession`, in the brief's order: `StudyName`, `StudyNotes`, `ParameterScope`, `StudyParameterRef`,
    `ScopedParameter`, `ParameterLink`, `SessionGainSetting`, `FitMode`, `ResidualWeight`,
    `ExperimentWeightSetting`, `DetectorWeightSetting`, `StudyFitSettings`, `OpenedMaterial`, `Study` (with
    `create`, `sessionsIn`, `gainModelOf`).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs` (new, `module StudyTests` in
  `namespace OpticalConstructor.Tests`): the five authorized facts over test-local experiments built as
  `StudySessionTests`' are.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (+1): `StudyTests.fs` last
  in the compile list, no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudySessionTests.fs` (+13 / −1): `every study error renders
  its condition and its payload` renders all seven cases and checks the four new texts.

After the elevation run, the sample `DuplicateStudyNameErr` reason in the widened fact was corrected to the real
message shape (`'QZ-LR' beside 'qz-lr': both are stored as qz-lr.study-json, ignoring case`). Elevation does not
analyse the test projects, so build and constructor-unit-tests were re-run over the final tree.

No other file is touched. `specs/0050/.manifest.state.json` was already modified by the supervisor when the round
started.

## Decisions

- **Duplicate name rule** follows `Item.relabelledAmong` (step 216): the stems compared with
  `StringComparison.OrdinalIgnoreCase`. The reason names both studies and the shared file
  (`StudyKitFile.fileNameOf`, the existing `.study-json` kind): `'QZ-LR' beside 'qz-lr': both are stored as
  qz-lr.study-json, ignoring case`. There is no shared stem-equality helper on `FileStem`, and `LibraryEntryFiles.fs`
  is outside this step's edits, so the comparison is inline, as in `Kit.fs` and `DataSplits.fs`.
- **Refusal order in `create`:** the held name first, then the first unknown experiment (the brief's order). Both are
  found up front and matched as a pair, so neither check needs a nested `match`.
- **Each once, in order:** `List.distinct` keeps first occurrences in order (`[E2; E1; E2]` → `[E2; E1]`).
- **`sessionsIn`** picks each study experiment from `kitExperiments` in study order (`List.choose` over the study's
  names), then calls `StudySession.splitAll`. A name the kit no longer holds is skipped; `create` already refuses
  one when the study is made.
- **`StudyParameterRef.describe`** renders the `Guid` in its default `D` form, and the version number with the
  invariant culture: `item <GUID> <name>`, `material <GUID> v<n> <name>`.
- **`ResidualWeight.tryCreate`** keeps a value only when `Double.IsFinite` and `> 0.0`. The reason renders the value
  with `"R"` and the invariant culture.
- **Session case name:** `SessionBlockScope`, as the brief says, because STL's `TrustFactors.fs` declares
  `SessionScope` (confirmed by grep). None of the step's other new type or case names occur in Berreman or STL.
- **Tests:** the sessions fact's kit holds E1, E2, E3 and then D1, a different order from the study's
  `[E3; D1; E1]`. A split in kit order would give `Session 1` [E1], so the fact shows the split follows study
  order. It also sets one session's gain to show `gainModelOf` returns the set model and leaves the other free. The
  residual-weight fact also checks `ResidualWeight.unit` is 1.0. These are extra assertions inside the authorized
  facts, not new identities.

## Testing state

These are development runs of each gate's own command from `0358.gates`, over the final tree:

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors, no `error` text in stdout. Warnings are only NU1902 (exempt) and the vendored MathNet C# `SYSLIB0051` (when that project recompiles, as in step 0357) | 76 s first, 23 s after the last test edit | 600 s |
| constructor-unit-tests | `Test run summary: Passed!`, 1579 of 1579, 0 skipped, run again over the final tree. That is 1574 at step 0357 plus 5. The five new facts and the widened `every study error renders its condition and its payload` passed in the TRX under exactly their authorized ids | 70 s | 180 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1513 (all ELEV004, excluded by the gate command; 1510 at step 0357) | 228 s | 600 s |
| invariant-s36-\* (20) | each snapshot command reports `INVARIANT HOLDS`. The untracked `StudyTests.fs`, which `git grep` cannot see, was grepped for every pattern too: nothing found | under 1 s each | 300 s |

No run went past its effective deadline. The elevation run had no `ASG_ARC_BASE_COMMIT` in this shell, so it
analysed all 13 projects (`no comparison base (all)`). That is wider than the gate's arc-origin scope.

## Artifacts

- `specs/0050/.artifacts/0358-dev-build.log`
- `specs/0050/.artifacts/0358-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0358-dev-elevation.log`
- `specs/0050/.artifacts/0358-dev-invariants.log`
- `specs/0050/.artifacts/0358-invariant-commands.tsv` (the twenty invariant commands, extracted from the snapshot)

## Gotchas

- `Study` reuses the labels `name` / `experiments` that `StudySession` has, and `OpenedMaterial` reuses `version`
  from `MaterialVersionId`. Code below them that builds a `StudySession` record must annotate it
  (`({ name = …; experiments = … } : StudySession)`), as `StudySessionTests.sessionOf` and the new sessions fact do.
  `StagedMuellerPath.fs` (Optimization, which opens `Studies`) still compiles unchanged.
- The shell heredoc could not append the F# code: `\"R\"` inside an interpolated hole broke bash quoting. F# also
  rejects a quoted literal inside a `$"…"` hole, so the rendered weight is bound with `let` first.
