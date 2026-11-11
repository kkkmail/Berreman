# Step 0305 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, AGENTS.md, the touched files, their callers and the ELEV013
  analyzer's private-signature rule.
- [x] Tests first (red). The test project failed to compile. Every error was a missing production symbol (`readMapped`,
  `readPickedFile`, `selectedBy`) or its inference knock-on (`.artifacts/0305-red-build.log`).
- [x] Production code: `ImportProfiles.fs`, `ExperimentDataProxy.fs`, `ExperimentDataLoad.fs`,
  `ExperimentDataStore.fs` and the Storage fsproj move.
- [x] UI test mocks moved to the new fields. The stale Tests-fsproj comments were deleted.
- [x] First green: solution build, 0 warnings; constructor-unit-tests 1518 (1514 + 4), every named identity `Passed`.
- [x] Mutation check: four defects in one build, each caught by its own fact. The files were restored and their
  SHA-256 verified.
- [x] Final rebuild, then development runs of every gate in `0305.gates` and the acceptance grep. All passed (see
  Testing state).

## Files modified

- **`OpticalConstructor.Domain/ImportProfiles.fs`.** `MappedPoints.selectedBy (rows : DataRowSelection)` sits in the
  same extension block, after `validatedFor`.
  - `AllDataRows` is `Ok this`.
  - `KeyedDataRows key` keeps:
    - the capture rows whose `experiment` label's text is `key`'s;
    - the observations whose `key` is it;
    - the mapped rows whose `keyOpt` is `Some key`.
  - Refusals:
    - a `SeriesPoints` is step 291's text, moved here without its path prefix;
    - `ColumnPoints` none of whose rows holds a key is `the import profile maps no key column to select '<key>' by`;
    - an empty selection is `EmptyDataFileErr` `no row of the data file holds the key '<key>'`.
  - The module doc names it.
- **`OpticalConstructor.Domain/ExperimentDataProxy.fs`.** The record is
  `{ readPickedFile; attachDataFile; attachmentStatus; readMapped }`, still `[<ReferenceEquality>]`. The two loads
  are deleted. It opens `ImportProfiles`, and the module doc and the new fields' docs follow the slice.
- **`OpticalConstructor.Domain/ExperimentDataLoad.fs`.** `loadAndValidate` is `dataProxy.readMapped data` bound to
  `validatedFor (detectorKindOf library slots) experiment.scan`. Step 291's reader choice and both of its refusals
  are deleted. The doc and the `MeasuredData` open comment follow.
- **`OpticalConstructor.Storage/ExperimentDataStore.fs`.**
  - The picked-file read is one private extension member, `FileName.pickedLabFile`. It answers
    `{ file; content }` or `KitIoErr`. `readPickedFile` is that member, and `attachDataFile` binds it into
    `kit.attachLabFile KitDataFolder.root`.
  - `readMapped`, in order:
    1. reads `kit.readKitFile ()`;
    2. resolves `ChosenImportProfile.tryOfName kitFile.importProfiles data.profile`; `None` is
       `the import profile '<name>' is not available`;
    3. reads `kit.readDataFile data.attachment.path`;
    4. maps the file's `text` by `ChosenImportProfile.mapText`;
    5. answers `selectedBy data.rows`.
  - A `KitError` of either read is `MalformedDataFileErr`
    `could not read the data file '<path>' for the import profile '<name>': <describe>`.
  - `DataFileKind` and `loadThrough` are deleted. It opens `ImportProfiles` and `LabFileImport`, and the module doc
    names the kit and the profile.
- **`OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`.** The `ExperimentDataStore.fs` `<Compile>` line
  moves to directly after `LabFileImport.fs`, with no comment; its old step-036 comment is deleted with it.
  `MuellerDataStore.fs` and `LabFileImport.fs`, which now compile before it, do not name it.
- **`OpticalConstructor.Tests/ImportProfileTests.fs`.** New fact `a row selection keeps every row or the rows of one
  key`, at the end of the module, over the existing `captureAt`, `observationOf`, `scanCellOf` and
  `assertValidation`. The module doc names the selection.
- **`OpticalConstructor.Tests/ExperimentDataProxyTests.fs`.**
  - The mock answers `readMapped` by the attachment's path with canned `SeriesPoints`, and `readPickedFile` by the
    picked file's name with canned bytes (a byte-order mark and `X,Y\n`). An unknown key is a typed error.
  - The `intensityAttachment` / `intensityData` fixtures move above the facts, and `ellipsometricData` joins them,
    because the first two facts now read them.
  - Three facts are renamed, as the census says:
    - `a mock ExperimentDataProxy reads canned mapped points and picked files through its exact signature`: all four
      fields pinned by typed locals; attach and status as before; the picked file's name and bytes; both canned
      series.
    - `an unknown path yields a typed ExperimentDataError, never a throw`: `readMapped` under both X,Y profiles, and
      `readPickedFile` of an unknown file is `KitIoErr`.
    - `loading data validates the points the attachment's profile maps`: the canned intensity points are validated,
      and the ellipsometric attachment is `DataUnitsMismatchErr`. The `readsNothing` part is deleted with the
      refusals it covered, which now live in the store and in `selectedBy`.
- **`OpticalConstructor.Tests/ExperimentDataStoreTests.fs`.**
  - `newKit ()` initialises each kit folder through `initialiseKit` of a test-local kit file holding a test-local
    copy of step 302's `Lab scan`. `newFolder ()` makes the outside folders, which stay plain.
  - The three X,Y facts read through `readMapped` of `mappedBy <profile>` over `AllDataRows`, keeping their names and
    assertions. The missing-file fact is renamed and asserts the new message through both X,Y profiles.
  - Three new facts:
    - the `Lab scan` K1 rows, plus the whole file's 3 points;
    - `Nope` and an X,Y key;
    - a picked file's exact bytes, with a BOM and a non-UTF-8 byte, the kit's paths unchanged, plus a missing pick's
      `KitIoErr`.
  - The two attach facts are unchanged except that they open initialised kits.
- **`OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`** (outside the effects; see Decisions). The stale
  step-035 and step-036 comment blocks above `ExperimentDataProxyTests.fs` and `ExperimentDataStoreTests.fs` are
  deleted.
- **`OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`.**
  - `mockIntensity` and `mockEllipsometric` answer `readMapped` with `SeriesPoints` of their series (helper
    `intensityPoints`), and `readPickedFile` with `KitIoErr`.
  - The three record copies that replaced loads now replace `readMapped`. The open-kit fact records
    `data.attachment`, so its `[ attachment ]` assertion holds as before.
  - One comment no longer names the deleted field.
- **`OpticalConstructor.Ui.Tests/SolverHandoffWindowTests.fs`.** `mockIntensityByPath` answers `readMapped` keyed by
  `data.attachment.path`, and `readPickedFile` with `KitIoErr`. `mockIntensity`, `mockEllipsometric` and
  `recordingIntensity` replace `readMapped`.
- **`OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`.** `noData` answers both new fields with an error.
- **Unchanged:** `AppContext.fs`, `SolverHandoffView.fs`, `SolverHandoffWindow.fs` and
  `TableAndElementRotationView.fs`. They hold the proxy, build none and call `loadAndValidate` and `createFileBacked`
  as before, as the slice says. They compile unchanged.

## Decisions

- **Deviation: `OpticalConstructor.Tests.fsproj` is edited although the effects do not list it.**
  - Its comment above `ExperimentDataProxyTests.fs` names `tryLoadIntensity / tryLoadEllipsometric`. The acceptance
    `git grep -n -E "tryLoadIntensity|tryLoadEllipsometric" -- Berreman/` MUST find nothing, which cannot hold while
    that comment stands.
  - The project (`OpticalConstructor.Tests`) is in `touches`.
  - AGENTS.md says "delete, do not shorten" and allows no new `.fsproj` comments, so the block was deleted, not
    reworded.
  - The step-036 block above `ExperimentDataStoreTests.fs` was deleted with it. It describes the deleted seam ("a
    missing path yields a typed ExperimentDataError from both fields"), and the fact it describes is renamed in this
    step.
  - Nothing else in the file changed.
- **The shared picked-file read is a private extension member (`FileName.pickedLabFile`), not a private free
  function.**
  - The analyzers enforce private signatures by default (`enforcePrivateSignatures`; this repo's
    `.elevation-analyzers.json` does not override it).
  - ELEV013 fires on a function whose one parameter is one of our unions, and `FileName` is a Softellect union. A
    `let private readPicked (file : FileName)` would therefore be ELEV013P.
  - The member keeps the slice's substance: one private read, shared by `readPickedFile` and `attachDataFile`. Its
    name is new among `FileName`'s extensions (ELEV019).
- **The single-series UI mocks answer `readMapped` for any attachment.** The slice says the mocks are "keyed by the
  attachment's path".
  - `SolverHandoffWindowTests.mockIntensityByPath`, the only mock that ever keyed by path, still does.
  - `mockIntensity` / `mockEllipsometric`, in both classes, answered one series for any path before and still do.
  - The old mocks also split by profile (an intensity mock refused an ellipsometric load). One `readMapped` cannot,
    and need not: I checked every fact of the three classes, and none attaches under a profile its mock does not
    answer for. All 37 pass with their assertions unchanged.
- **`selectedBy` compares a capture's label by its text** (`capture.experiment.value = key.value`), as the slice says
  ("whose `experiment` label's text"). That is ELEV004, which is measured and not gated (spec 0048 `.manual/002` R7).
  No re-wrap and no new cross-type member is introduced.
- **The new `ExperimentDataStoreTests` copy of `Lab scan` is test-local.** `KitTests.labScanProfile` compiles after
  `ExperimentDataStoreTests.fs`, so the class cannot reach it. The copy has the same name, delimiter, header, BOM
  choice and roles.
- **Extra assertions beyond the slice's minimum, in the new store facts only:** the `Lab scan` file read over every
  row has 3 points, and a missing pick is `KitIoErr` naming the file. They exercise the same fields; no test identity
  is added for them.

## Testing state

These were development runs of each gate's own command over the final build. The gate engine runs the gates itself.
No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 57 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known 0; ELEV004 excluded (suppressed 1413) | 218 s | 600 s |
| constructor-unit-tests | Passed!, 1518 of 1518, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 224 s | 600 s |
| ui-tests (3 selected classes) | Passed!, 37 of 37, 0 failed, 0 skipped | 30 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **constructor-unit-tests:** 1518, the 1514 baseline plus the 4 new facts. Every identity the census names is
  `Passed` in the TRX under its exact name. None of the four old names is present.
- **ui-tests:** the selection is InverseConstructorTests 14, ScrollExtentTests 14 and SolverHandoffWindowTests 9.
  - Inverse + SolverHandoff = 23, the previous step's count for those two classes.
  - ScrollExtentTests' facts are untouched; only its `noData` record changed.
  - No fact was added or removed in any of the three classes.
- **Elevation** ran standalone, with no `ASG_ARC_BASE_COMMIT`, so it covered all 12 projects. That is a superset of
  the gate's arc-origin scope. The ELEV004 suppressed count went from 1412 (step 304) to 1413; ELEV004 is measured,
  not gated.
- **Acceptance grep:** `git grep -n -E "tryLoadIntensity|tryLoadEllipsometric" -- Berreman/` exits 1 and finds
  nothing.
- **Line endings:** every touched file is `i/lf w/lf`. `git diff --numstat` and `--ignore-cr-at-eol` agree.
- **Red first:** the test project failed to compile on the missing symbols only (`0305-red-build.log`).
- **Mutation check:** four defects went into one build, and each failed its own fact
  (`0305-mutation-tests.log`):
  1. the keyed captures not filtered: `a row selection keeps every row or the rows of one key`;
  2. the store dropping `selectedBy`: the two store facts on keys;
  3. `readPickedFile` dropping the bytes: `createFileBacked reads a picked file whole without attaching it`;
  4. `loadAndValidate` skipping `validatedFor`: both `loading data …` facts.

  The three files were restored from copies and their SHA-256 verified
  (`0305-mutation-backup/sha256.txt`). The solution was then rebuilt, and every gate above ran on that build.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0305-red-build.log`
- `0305-build.log`
- `0305-constructor-unit-tests.log`
- `0305-ui-tests.log`
- `0305-ui-smoke.log`
- `0305-elevation.log`
- `0305-mutation-build.log`
- `0305-mutation-tests.log`
- `0305-mutation-backup/sha256.txt`

## Gotchas

- **ELEV013 enforces private signatures here.** `enforcePrivateSignatures` defaults on and the repo does not override
  it. A private module function of one `FileName`, or any other Softellect or Berreman union, is ELEV013P. Make it a
  (private) extension member instead.
- **`readMapped` needs an initialised kit**, because it reads the kit file first. A test that writes data into a bare
  folder and reads it must `initialiseKit` the folder first. Attaching works either way.
- **`KitTests.labScanProfile` is not reachable from `ExperimentDataStoreTests`**: it compiles later in the Tests
  project. Hence the test-local copy.
- **The TRX writes apostrophes literally in `testName`.** Grep with `.` in place of `'`, not `&apos;`.
- **No `python3` in this Git Bash.** Use the Edit / Write tools for multi-line rewrites.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
