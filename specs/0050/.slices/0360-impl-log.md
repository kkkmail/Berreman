# Step 0360 — impl log

## Progress

- [x] Domain: `Study.withExperiments`, with the shared unknown-experiment check
- [x] Tests: StudyTests (+2)
- [x] Development runs: build, constructor-unit-tests, elevation, invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`:
  - **`Study.withExperiments`** (new), AddExperimentsToStudy's pure half. The first name the kit lacks is refused as
    `UnknownStudyExperimentErr`, naming it, and nothing changes. Otherwise it answers the study's experiments in place,
    then each given name it does not hold yet, in the given order and each once, with every other field kept.
  - **`Study.unknownExperimentAmong`** (new, `static member private`): the first of a name list the kit lacks, as the
    `UnknownStudyExperimentErr` a study answers.
  - **`Study.create`** now calls the shared check instead of its own copy. Its local `inKit` is deleted, and its refusal
    text and order (duplicate name first) are unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyTests.fs`:
  - two new facts, placed after the CreateStudy fact;
  - the module doc names AddExperimentsToStudy's pure half beside CreateStudy's.

## Decisions

- **One check, not two.** `create` already held the "first experiment the kit lacks" check and its message. Writing a
  second copy for `withExperiments` would be the duplicated body AGENTS.md calls a defect. The check moved into one
  private static member that both call. It stays on `Study`, beside its only two callers, rather than becoming a
  module-level function.
- **"In place" taken literally.** `withExperiments` keeps the study's own list exactly as it is and appends
  `names |> List.filter (not held) |> List.distinct`. It does not run `List.distinct` over the concatenation, which would
  also reorder or drop anything already in the study's list.
- **The append fact reuses the QZ-LR fixture.** It uses `StudyFileFixtures.qzLrStudyOver studyProbeMaterial` (in
  `KitLibraryTests.fs`, which compiles before `StudyTests.fs`), with its experiments set to [E2].
  - QZ-LR sets every other field to a non-initial value: materials, scopes, links, three gain settings, both weights,
    a staged fit with a disabled run, and notes.
  - So "its other fields equal" catches a member that rebuilt the study from `create`'s defaults.
  - No new fixture was written.
- **The refusal fact** also asserts the reason does not name E1, the name the kit holds, so it names exactly the first
  missing one.
- **No removal member was added.** Disabling an experiment is step 365's.

## Testing state

These are development runs of each gate's own command from `0360.gates`, over the final tree.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`, from `Berreman/`):
  - exit 0, `0 Error(s)`, no lowercase `error` in stdout;
  - 76 s against a 600 s deadline.
  - The warnings are NU1902 (exempt) and SYSLIB0051 from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. The
    SYSLIB0051 warnings are pre-existing (see Gotchas).
- **constructor-unit-tests** (the gate command, whole suite, empty selector):
  - `Test run summary: Passed!`, 1590 of 1590, 0 skipped. That is 1588 + 2, the count rising by 2 as the acceptance
    requires.
  - The TRX shows both authorized ids `Passed` under exactly their ids:
    - `OpticalConstructor.Tests.StudyTests.adding experiments appends the new ones in order and keeps those the study holds`
    - `OpticalConstructor.Tests.StudyTests.adding an experiment the kit lacks changes nothing`
  - 70 s against a 180 s deadline.
- **elevation** (`Elevation/Run-ElevationGate.ps1 -ExcludeRules ELEV004`, from the repo root):
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, suppressed 1517 (ELEV004 only, which is
    excluded; the same total as step 0359);
  - with no `ASG_ARC_BASE_COMMIT` in this shell it analysed all 13 projects;
  - 208 s against a 600 s deadline.
- **invariant-s36-\* (20):** every snapshot command answers `INVARIANT HOLDS`, each in 0.2 s against 300 s. No new
  untracked source file exists, so `git grep` sees the whole change.
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol` for both files.

## Artifacts

- `specs/0050/.artifacts/0360-build-dev-run.log`
- `specs/0050/.artifacts/0360-constructor-unit-tests-dev-run.log`
- `specs/0050/.artifacts/0360-invariants-dev-run.log`
- `specs/0050/.artifacts/0360-elevation-dev-run.log`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/TestResults/constructor-unit-tests.trx`: written by the gate
  command.

## Gotchas

- **SYSLIB0051 in the build output.** The vendored C# `MathNetNumerics/Numerics/Exceptions.cs` (lines 24 and 77) raises
  SYSLIB0051 whenever that project recompiles rather than builds incrementally.
  - It is pre-existing: the same warning appears in this arc's build logs from step 0001 on.
  - It is outside this step's `touches` and does not trip the gate's `error` match.
  - This step was not authorised to edit the vendored MathNet, so it was not touched.
