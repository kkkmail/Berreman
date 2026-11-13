# Step 0359 — impl log

## Progress

- [x] Domain: `StudyFile`, `Study.studyFile`, `Study.libraryReferences`
- [x] Storage: `study.schema.json`, fsproj content item, `validateStudyFile`
- [x] Storage: `ParameterScope` / `FitMode` converters
- [x] Storage: `StudyFile.fileText`, `parseStudyFile`, `writeStudy`, `readStudies`, `recordStudy`
- [x] Tests: KitContentFileJsonTests (+4), CanonicalJsonTests (2 widened), KitStudyFileTests (+3), KitLibraryTests (+2)
- [x] Development runs: build, constructor-unit-tests, elevation, invariants

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Studies.fs`:
  - `Study.libraryReferences`;
  - `StudyFile` with `currentSchemaVersion` = 1;
  - `type Study with member studyFile`, one direction only (ELEV006).
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/study.schema.json` (new):
  - draft 2020-12, `$id` `https://berreman/schema/study.json`;
  - the version is checked unconditionally, and only version 1's body is validated, under `if`/`then`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: the schema as a `Content` item
  copied `PreserveNewest`, after `item.schema.json`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`: `validateStudyFile` over `validateWith`. Its
  schema sits behind a private `lazy` that loads the common schema first (`tryLoadLibrarySchema`).
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`:
  - `ClosedSetTagConverter<ParameterScope>` and `<FitMode>`, registered right after `ExperimentRole`'s;
  - the `options` doc lists them;
  - `open OpticalConstructor.Domain.Studies`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`:
  - `StudyFile.fileText`;
  - `KitFileText.parseStudyFile`;
  - `KitProxy.writeStudy` / `readStudies`;
  - `KitLibraryContext.recordStudy`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`:
  - a new `StudyFileFixtures` module above `KitLibraryTests`;
  - two new facts;
  - `open OpticalConstructor.Storage.MaterialDatabaseStore`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitContentFileJsonTests.fs`:
  - four new facts;
  - one `editedRendering` helper, which the experiment, setup and item edit helpers now call instead of each holding a
    copy of the same four lines.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`: two facts widened.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitStudyFileTests.fs` (new): `module KitStudyFileTests`, three facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `KitStudyFileTests.fs` appended
  last in the compile list, with no comment.

## Decisions

- **The shared fixture lives in `KitLibraryTests.fs`.** The QZ-LR study is shared, not copied. `StudyFileFixtures` exports
  `openedCrystalMaterial` (the `OpenedMaterial` record literal with `n3` unknown under a prior), `studyProbeMaterial`
  (the test-local probe version) and `qzLrStudyOver`. It sits in `KitLibraryTests.fs`, the first of the three consuming
  files in compile order. That is the precedent of `CrystalMaterialFixtures` (in `MaterialProxyTests.fs`) and
  `KitContentRoundTripFixture` (in `SetupRoundTripTests.fs`). The crystal is `CrystalMaterialFixtures.absorbingTriclinicParameters`,
  reused rather than re-transcribed.
- **QZ-LR's contents:**
  - E1 and E2;
  - `n3` scoped `ExperimentScope`;
  - CPL2's `c2.retardance` linked to CPL1's;
  - one setting each of `FreeSessionGain`, `AnchoredSessionGain [E1]` and `InterpolatedSessionGain {E1 → E2}`;
  - an experiment weight (E1, 2.0) and a detector weight (0.5);
  - `StagedFit` with E2 disabled;
  - two lines of notes.

  `StagedFit` and `ExperimentScope` make the tag check non-trivial, since tags 2 and 4 are not the first case.
- **`KitStudyFileTests` reuses `KitContentRoundTripFixture.createContentTestKit`.** It is step 218's precedent: a folder
  under `AppContext.BaseDirectory/KitStudyFileTests/<GUID>`, `KitProxy.create`, and `initialiseKit` of a test-local kit
  file.
- **The `recordStudy` fact uses the library database, not the in-memory store set.** In
  `CatalogueImportProxy.createInMemory`, `materials.saveMaterial` writes only the material store. The import proxy's
  `tryHeldEnvelope` reads its own entry map, so a material saved there is unheld and `snapshotReferences` answers
  `UnheldEntryErr`. `BuiltinCatalogue.freshLibraryDatabase` is "its built-in library". Its `MaterialProxy.createInDatabase`
  and `CatalogueImportProxy.createInDatabase` share one database, so the saved material resolves. The recordSetup fact
  uses this precedent.
- **Contents of the `recordStudy` fact.** It reuses `observeContentWrites`, which proves the snapshots and pins come
  before the content write. The kit's snapshots are the material v1 and its category (glass). The pins are exactly
  those two, in visit order.
- **The unknown-tag refusals.** `an unknown disk tag is refused` refuses scope tag 5 and mode tag 3, both through
  `Throws<JsonException>` and through `parseVersionedText` with no validator, as `MalformedJsonErr`. This mirrors the role
  check: the study schema would refuse mode 3 first, as `SchemaValidationErr`.
- **An extra malformed case.** The schema-invalid study fact also feeds scope tag 5 inside the file. It shows that the
  inner shapes are bound by the serializer (`MalformedJsonErr`).

## Testing state

These are development runs of each gate's own command from `0359.gates`, over the final tree:

- **build:**
  - `dotnet build Berreman.slnx -c Release -nologo -v:m`: exit 0, 0 errors.
  - The only warnings are NU1902, which is exempt.
  - 22 to 44 s against a 600 s deadline.
- **constructor-unit-tests:**
  - `Test run summary: Passed!`, 1588 of 1588, 0 skipped. That is 1579 + 9, the count rising by 9.
  - The TRX holds all eleven authorized ids, each `Passed`: the nine new facts and the two widened ones.
  - 72 s against a 180 s deadline.
- **elevation:**
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, suppressed 1517 (ELEV004 only, which is
    excluded).
  - With no `ASG_ARC_BASE_COMMIT` in this shell it analysed all 13 projects.
  - 218 s against a 600 s deadline.
- **invariant-s36-\* (20):** every snapshot command answers `INVARIANT HOLDS`. The two new untracked files were grepped
  for every pattern as well, with nothing found.
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol`, and the new files hold no CR.

## Artifacts

- `specs/0050/.artifacts/0359-elevation-dev-run.log`: the development elevation run's full output.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/TestResults/constructor-unit-tests.trx`: the development test run's
  TRX, written by the gate command.

## Gotchas

- The Bash tool unescapes `\\n` / `\r` inside heredoc'd Python strings, which wrote a real CR and newlines into an F#
  string literal. The literals were repaired byte-exactly, and the file holds no CR. Use the Edit tool for F# string
  literals with escapes.
- F# indexer chains in argument position (`node["a"]["b"] <- …` inside a list of lambdas) raise FS3369. Use
  `node.["a"].["b"]`.
