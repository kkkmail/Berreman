# Step 0291 — impl log

## Progress

- [x] Domain: `ImportProfileName`, `DataRowKey`, `DataRowSelection`, `MappedAttachment`
- [x] Domain: `Experiment.dataOpt`, `attachDataFile`
- [x] Domain: `DetectorKind.builtInProfile`, `loadAndValidate` by profile
- [x] Storage: schema, `copyExperimentsFrom`
- [x] Ui: attach arm, validation, `dataFileRow`, handoff
- [x] Fixtures and tests
- [x] Development runs (final tree)

## Files modified

### Product

- `OpticalConstructor.Domain/Kit.fs`, after `DataAttachment` and its `KitDataFile.attachment` extension:
  - `ImportProfileName` has `.value`, `tryCreate` (`None` for an empty or white-space text) and the built-in
    `intensitySeries` (`Intensity series`) and `ellipsometricSeries` (`Ellipsometric series`).
  - `DataRowKey` has `.value`.
  - `DataRowSelection = AllDataRows | KeyedDataRows of key : DataRowKey`, with no int tag.
  - `MappedAttachment = { attachment; profile; rows }`.
- `OpticalConstructor.Domain/Experiments.fs`:
  - `Experiment.dataFileOpt` becomes `dataOpt : MappedAttachment option`.
  - `attachDataFile` takes `(data : MappedAttachment)`.
  - `commit` gives an appended experiment `dataOpt = None` and keeps an edited one's, by its existing `e with` update.
- `OpticalConstructor.Domain/ExperimentDataLoad.fs`:
  - `type DetectorKind with member this.builtInProfile` answers `intensitySeries` for `Intensity` and
    `ellipsometricSeries` for `Ellipsometer`.
  - `loadAndValidate` takes `(data : MappedAttachment)`. The local `readByProfile` picks the reader by profile, then
    checks the rows:
    - `intensitySeries` reads with `tryLoadIntensity`, and `ellipsometricSeries` with `tryLoadEllipsometric`;
    - any other profile is `MalformedDataFileErr "the import profile '<name>' is not available"`;
    - `KeyedDataRows key` is `MalformedDataFileErr "<path>: an X,Y data file has no key column to select '<key>' by"`.
  - Both refusals read nothing.
  - The series is then validated against `detectorKindOf library slots`, unchanged. The detector no longer picks the
    reader.
  - Module and function docs now cite S9.1.
- `OpticalConstructor.Storage/schema/experiment.schema.json`:
  - The required key `dataFileOpt` becomes `dataOpt`: `null`, or an object holding exactly `attachment`, `profile` and
    `rows`.
  - `attachment` is step 146's `path` + `contentHash` object, and `profile` is a string matching `\S`.
  - `rows` takes the serializer's case form, a string or an object; see Gotchas.
- `OpticalConstructor.Storage/KitContentFiles.fs` (`copyExperimentsFrom`):
  - `dataPaths` reads `dataOpt`'s attachment paths. `readLabFiles` already makes them distinct.
  - `writeExperiments` gives each copy `{ data with attachment = <the moved attachment of its path> }`, so the profile
    and rows are kept.
- `OpticalConstructor.Ui/TableAndElementRotationView.fs`:
  - New `open OpticalConstructor.Domain.ExperimentDataLoad`, for the `builtInProfile` extension.
  - `validateExperimentAttachment` takes `(data : MappedAttachment)` and passes it to `loadAndValidate`.
  - `withKitContent` checks `data.attachment`'s status and validates `data`.
  - The `AttachDataFileTo` arm builds `{ attachment; profile; rows = AllDataRows }`:
    - `profile` is `(detectorKindOf model.library <slots>).builtInProfile`;
    - the slots are the held experiment's `slotsIn model.kitSetups`, defaulting to `[]`, as `SolverHandoffView.buildRows`
      does;
    - the arm then attaches, records and validates, as before.
  - `dataFileRow` shows `file: <path> (<profile>)`.
- `OpticalConstructor.Ui/SolverHandoffView.fs`:
  - `handoffFileCaption` is `<path> (<profile>)`.
  - `validateExperiment` reads `dataOpt` and passes the mapped attachment.

### Tests

- **New facts (3):**
  - `KitTests`: `an import profile name refuses a blank text, a row selection names every row or one key, and the two built-in names are the X,Y parsers'`.
  - `ExperimentDataProxyTests`: `loading data reads it through its import profile and refuses a profile or a row selection the X,Y files cannot serve`.
    - It runs step 288's one-axis experiment over the seeded mock.
    - Mapped by `intensitySeries`, it answers the canned series.
    - Mapped by `ellipsometricSeries`, the canned ellipsometric file answers `DataUnitsMismatchErr`.
    - Mapped by `Phone ROI CSV`, and keyed `CPL-(QZ#45)-CPL`, it answers the exact `MalformedDataFileErr` texts, over a
      mock whose loaders `failwith`.
  - `ExperimentCopyTests`: `a copy keeps each copied experiment's import profile and row selection`.
    - E1 is mapped by `Phone ROI CSV` to the rows keyed `K1`.
    - The target holds a file at its path, so the copy has a new attachment (`run 1 (2).csv`) with the same profile
      and rows.
- **Changed expectations:**
  - `KitContentFileJsonTests`:
    - The round-trip fact also round-trips `e1KeyedRows` (`Phone ROI CSV`, keyed `CPL-(QZ#45)-CPL`).
    - The schema fact's key list names `dataOpt`, and the five `dataFileOpt` edits move under `dataOpt.attachment`.
    - It adds `dataOpt.profile ""` and a `dataOpt` without `rows`.
  - `ExperimentProxyTests.attachDataFile binds one data attachment per experiment` binds a mapped attachment, with its
    assertions kept.
  - `InverseConstructorTests.attaching a valid intensity file …` also finds the profile `Intensity series` and
    `AllDataRows`.
- **Fixtures** (rule 6): each attachment is mapped by `intensitySeries` over `AllDataRows`, every assertion kept, in:
  - `ExperimentCollectionRoundTripTests` (e1, e3), `ExperimentProxyTests` (`held`), `KitContentFileJsonTests` (e1),
    `ExperimentCopyTests` (`darkLine`) and `KitWindowModelTests` (`darkLine`);
  - `ExperimentDataProxyTests` (lifted `intensityData`) and `SolverHandoffWindowTests` (`withFile`).
  - SolverHandoffWindowTests' `ellipso.csv` uses the new `withMappedFile ellipsometricSeries`.
  - `ExperimentCopyTests` reads its assertions through `attachmentOf` and re-points with `reattached`.
  - ExperimentDataProxyTests' step-288 fixtures (slots, attachment, `sweepOver` and `oneAxis`) are lifted to private
    module values, so the new fact shares them. The step-288 fact's assertions are unchanged.
  - `ScrollExtentTests` builds no attachment and is unchanged.
- **Renamed `dataFileOpt = None` to `dataOpt = None`:** the listed `KitWindowTests`, `KitWindowModelTests`,
  `ExperimentProxyTests`, `ExperimentCollectionRoundTripTests`, `KitContentFileJsonTests` and
  `SolverHandoffWindowTests`, and the unlisted files in Gotchas.

## Decisions

- **The profile is checked before the row selection.** An unavailable profile cannot say whether it has a key column.
  The "no key column" refusal is a fact about the two X,Y profiles.
- **`builtInProfile` lives in `ExperimentDataLoad`**, as the slice places it. Callers open
  `OpticalConstructor.Domain.ExperimentDataLoad`: the Ui file and KitTests.
- **The attach arm keeps the shape it had.** The profile is computed from the held experiment's slots, and an id the
  collection lacks still leaves the model unchanged.
- **Test fixtures write the mapped record inline.** No shared test helper file is in the step's edits.

## Testing state

These are development runs on the final tree, with the snapshot's commands and selectors, run one after another. The
gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | ~21 s (incremental) | 600 s |
| constructor-unit-tests | Passed!, 1490 of 1490 | 69 s | 180 s |
| ui-tests (8 selected classes, the gate's exact command) | Passed!, 118 of 118 | 130 s | 360 s |
| ui-smoke | Passed!, 249 of 249 | 208 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 201 s | 600 s |

- **constructor-unit-tests:** 1490 is step 0290's 1487 plus the three new facts. The TRX shows `Passed` for the new
  facts, the three changed facts, and the restructured step-288 fact.
- **ui-smoke:** 249, unchanged.
- **Elevation:** the standalone driver had no comparison base, so it analyzed all 12 product projects. That is a
  superset of the gate's arc-origin scope.
- No run exceeded its effective deadline.
- **ui-tests:** 118 over this step's selection. Per class:
  - DataFilePickerFolder 7, ExperimentControls 36, FsxAcceptance 14 and InverseConstructor 14;
  - KitWindow 16, ScrollExtent 14, SolverHandoffWindow 9 and WorkbenchInfoAndCapture 8.
  - Every class step 0290 also ran has the same count, so no UI fact was added or removed.
  - The TRX shows `attaching a valid intensity file records a typed Loaded status and the attachment` `Passed`.
- **invariant-s29-\* (16):** every decoded `git grep`, run in Git Bash, finds nothing (`0291-invariants.log`).
- **Mutation (red):**
  - Two edits were made: the copy rebuilt the mapped attachment as intensity over every row, and `loadAndValidate`
    read a key selection.
  - After a rebuild, exactly the two new facts failed: 2 of 24 in ExperimentCopyTests + ExperimentDataProxyTests.
  - Both files were restored and checked by SHA-256, rebuilt with 0 warnings, and re-ran 24 of 24.
- **Line endings:** `git diff --numstat` equals the `--ignore-cr-at-eol` diff (444 / 142), and no changed file holds a
  CR.

## Artifacts

- `0291-probe/probe.fsx`: shows that the serializer writes a fieldless case as its name and a keyed case as
  `{Case, Fields}`.
- Builds:
  - `0291-build-dev-1.log`: two FS3369 warnings, from a triple index in a test, fixed.
  - `0291-build-dev-2.log`: green, 0 warnings.
- `0291-constructor-unit-tests-dev-1.log`: 1490 / 1490.
- `0291-ui-tests-selector.txt` and `0291-ui-tests-dev-1.log`: 118 / 118.
- `0291-invariant-commands.txt` and `0291-invariants.log`.
- Mutation evidence:
  - `0291-mutation-hashes.txt`
  - `0291-mutation-build.log` and `0291-mutation-run.log`
  - `0291-build-restored.log` and `0291-restored-run.log`
- `0291-elevation-dev-1.log` and `0291-ui-smoke-dev-1.log`.

## Gotchas

- **`rows` in the schema is "the serializer's case form", not "an object".**
  - The slice describes `rows` as "an object", but also says its cases are "written in the serializer's case form, as
    `SlotTurn`'s are".
  - Storage's `JsonFSharpConverter` runs `WithUnionUnwrapFieldlessTags`, so `AllDataRows` is written as the JSON string
    `"AllDataRows"`. Only `KeyedDataRows` is an object (`{"Case": "KeyedDataRows", "Fields": ["<key>"]}`). The probe is
    `0291-probe/probe.fsx`.
  - An object-only `rows` would refuse every attachment an attach records. The schema therefore takes
    `"type": ["string", "object"]` and leaves the shape to the serializer, as `setup.schema.json` does for a slot.
  - Both acceptance refusals (`profile ""` and a missing `rows`) hold, and an unknown case name fails binding as
    `MalformedJsonErr`.
- **Unlisted files edited for the build:** each holds an `Experiment` literal with `dataFileOpt = None`, now
  `dataOpt = None`. Steps 0288 and 0290 re-pointed the same files under their own fixture rules.
  - KitLibraryTests, KitOpeningTests, KitStartTests, OutOfBandDiagnosticTests and SetupTests;
  - KitStartWindowTests and LauncherTests.
  - SetupModelTests' one assertion `Assert.Equal<DataAttachment option>(None, experiment.dataFileOpt)` reads
    `Assert.Equal<MappedAttachment option>(None, experiment.dataOpt)`.
- **SolverHandoffWindowTests' `the detector kind selects the ellipsometric load path`:**
  - The profile, not the detector, now picks the reader, so its fixture must be mapped by `ellipsometricSeries`, as
    rule (6) says for an ellipsometric fixture.
  - InverseConstructorTests' fact of the same name needs no change: its attach records the ellipsometer's built-in
    profile.
- **The ellipsometric case of the new ExperimentDataProxyTests fact maps `data/run-ellipsometric.csv`.**
  - That is the seeded mock's ellipsometric file. Read by `ellipsometricSeries`, the intensity file has no canned
    ellipsometric series and would answer `MalformedDataFileErr`, not the `DataUnitsMismatchErr` the slice asks for.
- **Python heredocs on this machine collapse backslashes**, as step 0290 found: `"\\S"` and a `\n` in an F# string
  literal. Both were fixed by hand.
