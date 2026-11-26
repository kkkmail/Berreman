# Step 0303 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, AGENTS.md, and steps 301 and 302 (code and SoW). Also read
  `MeasuredData.fs`, `MuellerDataStore.parseMuellerCsv`, `MuellerDataError`, and the `MeasuredDataTests` /
  `MuellerReconstructionTests` fixtures.
- [x] Tests first. The three facts were appended to `LabFileImportTests`. Red: the test project failed to compile with
  exactly one `FS0039`, "The type 'ChosenImportProfile' does not define … 'mapText'", and nothing else
  (`.artifacts/0303-red-build.log`).
- [x] Production code: `ChosenImportProfile.mapText` in `LabFileImport.fs`, and the `MeasuredData.fs` module doc.
- [x] Green: `LabFileImportTests`, 9 of 9; constructor-unit-tests 1512.
- [x] Mutation check, run twice: once on the first test draft, and again after the facts were reworked to report
  their reads in full. Three defects went into one build, and each new fact failed on its own defect. The file was
  restored and its SHA-256 verified.
- [x] Final solution build, then development runs of every gate in `0303.gates` (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs`
  - Adds `open OpticalConstructor.Storage.MuellerDataStore`, for the `DataFileText.parseMuellerCsv` extension.
  - The module doc now also names the one read through any profile.
  - Adds `type ChosenImportProfile with member this.mapText (text : KitFileText) : Result<MappedPoints,
    ExperimentDataError>`, one match on the choice:
    - `IntensitySeriesProfile`: `text.dataFileText.parseIntensitySeries`, as `SeriesPoints (IntensityData _)`.
    - `EllipsometricSeriesProfile`: `text.dataFileText.parseEllipsometricSeries`, as
      `SeriesPoints (EllipsometricData _)`.
    - `PhoneRoiCsvProfile`: `text.dataFileText.parseMuellerCsv`, as `CapturePoints`, its `MuellerDataError` mapped
      onto `ExperimentDataError`.
    - `MuellerObservationProfile`: `text.parseMuellerObservations` (step 301), as `ObservationPoints`.
    - `KitProfileChoice profile`: `profile.mapText text` (step 302), as `ColumnPoints`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasuredData.fs`: documentation only. The nine-line
  paragraph on the two STRICT schema-v1 shapes, with its "Deliberately NOT in scope" sentence, becomes two lines: the
  two parsers are the bodies of the `Intensity series` and `Ellipsometric series` import profiles (spec 0050 S9.2).
  No code changed.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LabFileImportTests.fs`
  - Adds `open OpticalConstructor.Storage.MuellerDataStore` and widens the module doc.
  - Lifts step 302's lab-scan text and its two expected rows out of `a column mapping reads each row's scan values,
    signal, σ and key` into the private `labScanText` and `labScanRows`. That fact still asserts exactly the same
    equality, and the new kit fact reads the same two bindings, so step 302's fixture is reused, not copied.
  - Adds `phoneRoiHeader` and `phoneRoiText`, `muellerCsvFixture`'s byte-order mark, ten-column header and two rows,
    written test-locally. It also adds `assertReads`, which compares one profile's read with the expected one and
    prints both in full on a mismatch.
  - Adds three facts:
    - `each built-in profile reads its fixture exactly as its parser did`
    - `a kit profile reads through its column mapping`
    - `the Phone ROI profile maps the parser's errors onto the data channel`

## Decisions

- **The Phone ROI error mapping follows the slice literally.**
  - `MalformedRowErr r` becomes `MalformedDataFileErr r`.
  - `EmptyFileErr r` becomes `EmptyDataFileErr r`.
  - `ArchiveUnreadableErr (_, r)` becomes `MalformedDataFileErr r`.
  - `DataSetMissingErr (_, archive)` becomes `MalformedDataFileErr archive.value`, the archive's path.
  - A text parse never answers the two archive cases. They are mapped only so the match is total; one comment line
    says so.
- **The mapping is a lambda inside the member, not a named function.** A free function of one `MuellerDataError`
  would be an ELEV013 breach. A member on `MuellerDataError` would add a public surface that the slice's contract list
  does not authorize.
- **Each built-in's oracle is its parser's own answer for the same text.** The fact first checks that each parser
  answers its fixture's shape:
  - three intensity points;
  - one ellipsometric point;
  - the malformed X,Y file's `MalformedDataFileErr`;
  - two Phone ROI rows;
  - two observations.

  So no equality can hold over two matching refusals by accident. It then compares `mapText` with the wrapped parser
  answer through structural equality on `Result<MappedPoints, ExperimentDataError>`. `MuellerMatrix` wraps MathNet's
  `Matrix<double>`, whose `Equals` compares the elements, and both sides parse the same text.
- **The observation profile reads step 301's `writtenText ()`**, the module's own observation fixture, as the slice
  says ("over step 301's text").
- **The Phone ROI error fact builds its two texts from `phoneRoiHeader`.**
  - Removing `,avg_total` from the header and the last cell from the row gives "missing required column(s):
    avg_total", so the answer is `MalformedDataFileErr` naming `avg_total`.
  - The header alone (with its byte-order mark) gives FSharp.Data's empty row list, `parseMuellerCsv`'s `EmptyFileErr`
    and so `EmptyDataFileErr`.
  - The step 302 probe (`.artifacts/0302-csv-probe/`) shows that a header-only text parses to zero rows and does not
    throw.
- **`MeasuredDataTests` and `MuellerReconstructionTests` are not edited**, as the slice requires.

## Testing state

Development runs of each gate's own command over the final build. No run exceeded its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 62 s after the edits, 9 s final incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded; exit 0 | 227 s | 600 s |
| constructor-unit-tests | Passed!, 1512 of 1512, 0 failed, 0 skipped | 76 s | 180 s |
| ui-tests (FsxAcceptanceTests) | Passed!, 14 of 14, 0 failed, 0 skipped | 96 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS`, exit 0 | < 1 s each | 300 s |

- constructor-unit-tests rose from 1509 to 1512, by the three new facts. All three are `Passed` in the TRX under the
  slice's exact names.
- ui-tests kept 14 tests, the same count as step 302. `FsxAcceptanceTests` was not edited.
- The suppressed-finding total went from 1407 (step 302) to 1408. The one addition is ELEV004 on `archive.value`
  (Storage SARIF, `LabFileImport.fs:364`). The gate excludes ELEV004: it is measured but not enforced (spec 0048
  `.manual/002` R7).
- The final build was incremental after the mutation restore. The Storage DLL (13:24:21) is newer than the restored
  source (13:24:08), and every test project's copy of it carries the same timestamp.

## Artifacts

- `specs/0050/.artifacts/0303-red-build.log` — the red build (the single `FS0039`).
- `specs/0050/.artifacts/0303-mutation/` — `mutate.py`, the mutated run's log, and the SHA-256 before and after the
  restore.
- `specs/0050/.artifacts/0303-build.log`, `0303-constructor-unit-tests.log`, `0303-ui-tests.log`,
  `0303-elevation.log`, `0303-invariants.log` — the development gate runs.

## Gotchas

- **A cold build of the test project prints two `SYSLIB0051` warnings** in the vendored, untouched
  `MathNetNumerics/Numerics/Exceptions.cs` (seen in the red build). The final solution build reports 0 warnings, as
  step 302 recorded.
- **`Assert.Equal` over an F# `Result` prints only the type names on a mismatch.** The facts compare through
  `assertReads`, which shows both reads with `%A`.
- **Backslash escapes do not survive a bash heredoc in this harness.** A Python edit script fed through one wrote
  real newlines into the new fixtures' `\n` escapes. This was caught on review and repaired before any test run of
  that draft. The final file has no raw newline inside a string literal.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
