# Step 0384 — impl log

## Progress

- Read the task, both system prompts, the project prompt, AGENTS.md and the slice.
- Surveyed the precedents: step 359 (the study file: schema, validator, converters, `KitContentFiles`
  members, `KitStudyFileTests`) and step 383 (`FitRuns.fs`).
- Wrote the production code (Kit.fs, the schema, the fsproj, SchemaValidation.fs, CanonicalJson.fs,
  KitContentFiles.fs). The Storage project built clean on its own before any test was written.
- Wrote the tests: a fixture and three facts in KitContentFileJsonTests, the new KitFitRunFileTests, and
  the widened CanonicalJsonTests and KitTests.
- The first full build failed with FS0039: `ParameterValue` needed `open OpticalConstructor.Domain.ItemModels`
  in KitFitRunFileTests. The open was added and the rebuild is green.
- Ran the constructor suite (1662/1662), the ELEV gate (OK) and the twenty s38 invariant greps (all OK).
- Extended `a kit's fit runs round-trip…` with the read-refusal path. These are new assertions only, with no
  new identity. Rebuilt and re-ran: 1662/1662.
- Checked that every edited and new file is LF (no CR bytes) and that `git diff --numstat` matches
  `--ignore-cr-at-eol`.

## Files modified

- `OpticalConstructor.Domain/Kit.fs`: added `KitError.FitRunExistsErr of reason : string`, rendered
  `the kit already holds that fit run: <reason>`. Only `describe` matches `KitError` exhaustively.
- `OpticalConstructor.Storage/schema/fitrun.schema.json` (new): draft 2020-12 with `$id`
  `https://berreman/schema/fitrun.json`. The version is checked unconditionally; the fifteen root fields are
  checked under `if` v1.
- `OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: the schema added as a `PreserveNewest`
  `Content` item beside `study.schema.json`, with no comment.
- `OpticalConstructor.Storage/SchemaValidation.fs`: added `validateFitRunFile` over `validateWith`. Its
  schema sits behind a `lazy` that goes through `tryLoadLibrarySchema`, which loads the common schema first.
- `OpticalConstructor.Storage/CanonicalJson.fs`: registered `ClosedSetTagConverter`s for `FitStage`,
  `CalibrationUse` and `StudyEngineKind` beside `ParameterScope` / `FitMode`, and updated the doc comment.
- `OpticalConstructor.Storage/KitContentFiles.fs`: added `FitRun.fileText`, `KitFileText.parseFitRun`,
  `KitProxy.writeFitRun` and `KitProxy.readFitRuns`.
- `OpticalConstructor.Tests/KitContentFileJsonTests.fs`: added the `FitRunFileFixtures` module before the
  test module, plus the three facts `a fit run is stored under its name`, `a fit run file round-trips through
  its canonical text`, and `a fit run file of another schema version, a schema-invalid one or a malformed one
  is refused`.
- `OpticalConstructor.Tests/KitFitRunFileTests.fs` (new; the declared `ADD_TEST
  OpticalConstructor.Tests.KitFitRunFileTests`, declared as `module KitFitRunFileTests` in `namespace
  OpticalConstructor.Tests`): three facts.
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `KitFitRunFileTests.fs` appended last in the
  compile list, with no comment.
- `OpticalConstructor.Tests/CanonicalJsonTests.fs`: `every closed set with an int tag…` now also writes and
  reads back each `FitStage`, `CalibrationUse` and `StudyEngineKind`. `an unknown disk tag is refused` now
  also refuses stage tag 4, use tag 3 and engine tag 3, both directly and as `MalformedJsonErr` through
  `parseVersionedText`.
- `OpticalConstructor.Tests/KitTests.fs`: `every kit error renders…` covers `FitRunExistsErr`; the distinct
  count went from 10 to 11.

## Decisions

- `parseFitRun` passes `this.toContentJsonText` (the private `JsonText` view the four sibling parsers use)
  instead of the slice's shorthand `this.value`. `parseVersionedText` takes a `JsonText`, and this way the
  existing seam is reused.
- `FitRunExistsErr` names the file already held, `FitRunKitFile.fileNameOf held`. When the names differ in
  case alone, the reason names the file actually on disk.
- `readFitRuns` sorts with `compare first.stamp second.stamp`, a structural compare on the `ClockTime` DU, so
  the order needs no `.value`. Ties fall back to `String.CompareOrdinal` on the name.
- Test kits are created with step 359's `createContentTestKit (FolderName "KitFitRunFileTests")`. This is
  exactly the slice's folder (`AppContext.BaseDirectory/KitFitRunFileTests/<guid>`) with `KitProxy.create`
  and `initialiseKit` of a test-local kit file, so it is reused rather than re-typed.
- Test stamps are DateTime literals (elevated at origin), not `ClockTime (x.value.AddHours …)`.
- Within the authorized facts I added assertions only: the case-differing name in `…never rewritten…`, the
  read-refusal path in `…round-trip…`, and the seam check in `an unknown disk tag is refused`. No test
  identity was added or renamed.

## Testing state

- `build`: green, 0 errors. The only warnings are the exempt NU1902 advisories. It took 45 s, then 24 s
  incrementally (deadline 600 s).
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 224 s (deadline 600 s). This was a
  standalone run with all projects analyzed.
- `constructor-unit-tests`: `Test run summary: Passed!`, 1662/1662 in 73 s (deadline 180 s). The count rose
  from 1656 (checkpoint key 382 = step 383) to 1662, which is +6 as the acceptance requires.
- `invariant-s38-*` (20): every grep found nothing, both tracked-only and with `--untracked`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0384-build-01.log`: the FS0039 failure.
- `0384-build-02.log`, `0384-build-03.log`: the green builds.
- `0384-constructor-unit-tests-01.log`, `0384-constructor-unit-tests-02.log`: the suite runs.
- `0384-elevation-01.log`: the ELEV gate.
- `0384-invariants.log`: the s38 greps.

## Gotchas

- `KitFitRunFileTests` carries a private `folderBytes`, like `KitStudyFileTests` (step 359's precedent),
  `KitItemFileTests`, `KitOpeningTests` and `KitWindowModelTests`. They can't be merged into one helper here
  because those files are outside this step's authorized edits.
- A fsproj path written in Python as `"schema\fitrun…"` turns `\f` into a form feed. I caught it in the diff
  and fixed it before any build.
- From Python on this host, `subprocess.run(['bash', …])` resolves to WSL's `bash` (no distribution
  installed). Run the decoded invariant greps in Git Bash instead.
