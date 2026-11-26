# Step 0291 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.
- Steps 288 to 290 gave experiments their scan, the dial-reading variable and their names.

This step makes an experiment's data a mapped attachment (001 §K):
- a kit file (path and hash);
- the named import profile it is read through;
- the selection of its rows that are the experiment's.

The two built-in profiles are the existing X,Y parsers. S9.2 makes them real profiles stored in `.kit-json`, and lets a
key column split one file into many experiments.

## What's working

- `Experiment.dataOpt` holds a `MappedAttachment` (attachment, `ImportProfileName`, `DataRowSelection`).
  `ImportProfileName.tryCreate` refuses a blank name.
- `loadAndValidate` reads the file through its profile:
  - `Intensity series` and `Ellipsometric series` read through the X,Y parsers;
  - an unavailable profile or a key selection is refused as a typed `MalformedDataFileErr`, and reads nothing;
  - the series is then validated against the detector, as before.
- An attach records the detector's built-in profile (`DetectorKind.builtInProfile`) over every row.
- The data-file row and the solver hand-off caption show the profile.
- The experiment file's schema carries `dataOpt` (`attachment`, `profile`, `rows`), and the file round-trips it. A copy
  re-points the attachment and keeps the profile and the rows.
- Tests:
  - three new facts, in KitTests, ExperimentDataProxyTests and ExperimentCopyTests;
  - four changed facts, in KitContentFileJsonTests ×2, ExperimentProxyTests and InverseConstructorTests;
  - every fixture attachment is mapped.

## Tests

These are development runs on the final tree, with the snapshot's commands and selectors, run one after another. The
arc-runner's gate engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | ~21 s (incremental) | 600 s |
| constructor-unit-tests | Passed!, 1490 of 1490, 0 failed, 0 skipped | 69 s | 180 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 208 s | 600 s |
| ui-tests (8 selected classes, the gate's exact command) | Passed!, 118 of 118, 0 failed, 0 skipped | 130 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 201 s | 600 s |

- **constructor-unit-tests:** 1490 is step 0290's 1487 plus the three new facts, as the acceptance requires.
  - The TRX shows `Passed` for the three new facts.
  - It also shows `Passed` for the changed `an experiment file round-trips through its canonical text`,
    `a schema-invalid or malformed experiment file is refused before it binds` and
    `attachDataFile binds one data attachment per experiment`.
- **ui-tests:** 118 over this step's eight classes. Per class:
  - DataFilePickerFolder 7, ExperimentControls 36, FsxAcceptance 14 and InverseConstructor 14;
  - KitWindow 16, ScrollExtent 14, SolverHandoffWindow 9 and WorkbenchInfoAndCapture 8.
  - Each class step 0290 also ran keeps its count, so no UI fact was added or removed.
  - The changed `attaching a valid intensity file records a typed Loaded status and the attachment` passes.
- **ui-smoke:** 249, unchanged.
- **invariant-s29-\*** (16): every decoded `git grep` finds nothing.
- **Mutation check (red):**
  - Two edits: the copy rebuilt the mapping as intensity over every row, and the loader read a key selection.
  - Exactly the two new facts failed (2 of 24). The files were restored byte for byte (SHA-256 checked), rebuilt, and
    re-ran 24 of 24.

```yaml
gates:
  constructor_unit_tests: 1490
  ui_smoke_tests: 249
  ui_tests: 118
```

## Architecture

- **The profile, not the detector, picks the reader.**
  - `loadAndValidate` maps the profile name to a reader. The two built-in names map to the X,Y parsers, and any other
    name is refused.
  - The detector still decides the validation. A profile that does not fit the detector is
    `validateAgainstExperiment`'s `DataUnitsMismatchErr`.
  - The detector chooses only the profile an attach records, through `DetectorKind.builtInProfile`.
- **The refusals come before any read, in a fixed order.** First an unavailable profile, then a key selection, because
  the X,Y files have no key column. Then the scan check, unchanged.
- **The mapped attachment wraps the existing `DataAttachment`.**
  - The status check (`statusAgainst` / `attachmentStatus`) still works on the attachment.
  - The data proxy's loaders and the copy's file moves also use the attachment.
  - The profile and the rows ride beside it.
- **The serialized shape:**
  - `dataOpt` is `{ attachment: { path, contentHash }, profile: "<name>", rows: "AllDataRows" | { Case, Fields } }`.
  - `DataRowSelection` has no int tag. Its cases are written in the serializer's case form, as `SlotTurn`'s are.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **`rows` is a string or an object in the schema, not only an object.**
  - Storage's `JsonFSharpConverter` unwraps fieldless tags, so `AllDataRows` is written as the string `"AllDataRows"`.
    Only `KeyedDataRows` is an object. The probe is `.artifacts/0291-probe/probe.fsx`.
  - An object-only `rows`, as the slice's parenthesis reads, would refuse every attachment. The schema takes
    `["string", "object"]` and leaves the shape to the serializer, as `setup.schema.json` does for slots.
  - The acceptance refusals (`profile ""`, a missing `rows`) hold.
- **Files edited outside the step's list, because the build required them:**
  - KitLibraryTests, KitOpeningTests, KitStartTests, OutOfBandDiagnosticTests and SetupTests;
  - KitStartWindowTests and LauncherTests.
  - Each held `dataFileOpt = None`, now `dataOpt = None`.
  - SetupModelTests' one `dataFileOpt` assertion now reads `dataOpt` (`MappedAttachment option`), its expectation
    unchanged.
- **An ellipsometric fixture must be mapped by `ellipsometricSeries`:**
  - The fixture is SolverHandoffWindowTests' `the detector kind selects the ellipsometric load path`.
  - Mapped by the intensity profile, it would read the intensity loader.
  - The new helper `withMappedFile` does this, and `withFile` is its intensity form.
- **`builtInProfile` is an extension in `ExperimentDataLoad`.** A caller must
  `open OpticalConstructor.Domain.ExperimentDataLoad`; the Ui view and KitTests now do.
- **Python heredocs on this machine collapse backslashes.** Write JSON patterns and F# `\n` literals with the editor.

## Changelog

- 2026-11-11 — Step 0291: an experiment's data is a mapped attachment.
  - Domain: `ImportProfileName`, `DataRowKey`, `DataRowSelection`, `MappedAttachment`, `Experiment.dataOpt`,
    `DetectorKind.builtInProfile`, and `loadAndValidate` by profile.
  - Storage: the schema's `dataOpt`, and a copy that keeps the profile and rows.
  - Ui: an attach records the detector's built-in profile over every row, and both captions name the profile.
  - Counts: constructor tests 1490 (+3), smoke 249, selected ui-tests 118 (the per-class counts unchanged).
