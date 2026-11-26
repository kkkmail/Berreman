# Step 0302 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, AGENTS.md, steps 300 and 301 (code and SoW), and
  `parseMuellerCsv` / `SpectrumSidecarText.parseSpectrum`.
- [x] Probed FSharp.Data 8.2.0's `CsvFile.Parse` (`.artifacts/0302-csv-probe/`).
- [x] Tests first. The three facts were appended to `LabFileImportTests`. Red: the test project failed to compile with
  exactly two `FS0039` errors, "`ImportProfile` does not define … `mapText`", and nothing else.
- [x] Production code: the private `labFileRows` and `ImportProfile.mapText` in `LabFileImport.fs`.
- [x] Green: `LabFileImportTests`, 6 of 6.
- [x] Mutation check: three defects in one build, and each new fact failed on its own defect. The file was restored
  and its SHA-256 verified.
- [x] Development runs of every gate in `0302.gates` (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs`
  - `open FSharp.Data` and `open OpticalConstructor.Domain.MuellerReconstruction` (for `CaptureIndex`); the module doc
    now covers the column mapping as well.
  - `let private labFileRows (delimiter) (header) (text) : Result<DataFileCell list option * DataFileCell list list,
    ExperimentDataError>`, the one CSV read by a layout (step 307's preview will call it).
  - `type ImportProfile with member this.mapText (text : KitFileText) : Result<MappedRow list, ExperimentDataError>`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LabFileImportTests.fs`
  - Adds `open System.Globalization` and `open OpticalConstructor.Domain` (for `Units.UnitMagnitude`), and widens the
    module doc.
  - Adds the test-local profiles `labScanProfile` and `muellerProfile`, their scan columns, and `mappedRows`.
  - Adds three facts:
    - `a column mapping reads each row's scan values, signal, σ and key`
    - `a semicolon file without a header maps its Mueller elements and throughput`
    - `a cell the mapping cannot read names its row and column`

## Decisions

- **"The first cell" is the leftmost cell in the row, whichever role reads it.**
  - Every role is read, and each refusal carries its `DataColumnNumber`.
  - A local `both` combinator keeps the smaller column when two reads fail. `all` folds a list through it.
  - Rows are refused in row order: the first refused data row is reported.
  - So a bad key in column 1 is reported before a bad signal in column 2, as a lab reads the row.
- **Data rows are numbered from 1 among the rows the read answers**, after the header and after blank rows are
  skipped. "Data row n" is the n-th data row, as the slice's test reads it ("data row 2").
- **A blank text is `EmptyDataFileErr "the lab file is empty"`**, answered by `labFileRows` before FSharp.Data runs.
  - Without the check, FSharp.Data would say "header row not found" or "no data rows found", which become
    `MalformedDataFileErr`.
  - A blank file has no data row, so it is `EmptyDataFileErr`, as `parseMuellerCsv` answers `EmptyFileErr` for empty
    text.
  - Step 307's "an empty text is `EmptyDataFileErr`, and the read's error passes through" is then met by the read
    itself.
- **The rows are enumerated inside the `try`.** FSharp.Data raises its row-width error ("Couldn't parse row 2
  according to schema: Expected 4 columns, got 3") lazily, during enumeration, as the probe showed. The error is
  `MalformedDataFileErr $"the lab file's rows could not be read by {delimiter.text}: {message}"`.
- **The throughput scales all sixteen elements.** This follows the slice's wording: element (i, j) is column 4i + j
  "times the throughput when a throughput column is given". With a normalized file (m00 = 1) it equals step 301's
  m00 = throughput, and the fact checks (0, 0) = 0.5.
- **A Mueller mapping whose element list is not `MuellerElementIndex.all`'s length (16) is refused up front.**
  - The error is `MalformedDataFileErr`, naming the profile and its count.
  - `ColumnRoles.tryOfChoices` never builds one. A record-built profile can, and the check keeps
    `List.chunkBySize 4 |> RealMatrix4x4.create` total.
  - `ExperimentDataError` is the member's error type, so the profile defect is reported through its malformed case.
- **Values are elevated at their parse.** The scan, signal and σ readers are `numberOf >> Option.map UnitMagnitude`,
  `… MeasuredIntensity` and `… MeasuredSigma`; the timestamp and capture index wrap inside their `TryParse` match.
  No bare local is wrapped later.
- **The observable is read through `MuellerInverse.Observable.tryOfCell`**, qualified because `Observable` is also an
  FSharp.Core module (step 301's gotcha). It gets the untrimmed `DataFileCell`, because `tryOfCell` trims for itself.
- **Every helper is local to the member or private.** `labFileRows`'s signature carries only elevated types
  (`DataDelimiter`, `HeaderRowChoice`, `KitFileText`, `DataFileCell`, `ExperimentDataError`), so ELEV003P and
  ELEV010P have nothing to report.
- **The Mueller fact reuses `absoluteMatrix`**, the test file's existing helper, instead of a new fraction table:
  - `absoluteMatrix 1.0` gives the sixteen cells: 1 and fifteen fractions, written in `R` text;
  - `absoluteMatrix 0.5` is the expected matrix, compared exactly, since scaling by 0.5 is exact.
- **The facts build their texts as `KitFileText` literals** (elevated at origin). `mappedRows` takes a
  `KitFileText`, not a string.

## Testing state

All runs were development runs over the final code, with the gates' own commands.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 44 s | 600 s |
| constructor-unit-tests | Passed!, 1509 of 1509 (1506 + 3) | 73 s | 180 s |
| ui-tests (FsxAcceptanceTests) | Passed!, 14 of 14 | 77 s | 360 s |
| invariant-s30-\* (16) | every one `INVARIANT HOLDS`, exit 0 | < 1 s each | 300 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded; exit 0 | 217 s | 600 s |

- The three new identities are `Passed` in `TestResults/constructor-unit-tests.trx`, named exactly as the slice names
  them.
- **Mutation check** (`.artifacts/0302-mutation-check.txt`). Three defects went into one build:
  - σ read from the signal column: `a column mapping …` failed, the rows differ;
  - throughput ignored: `a semicolon file …` failed, expected 0.5, actual 1;
  - data rows numbered from 0: `a cell the mapping cannot read …` failed, sub-string not found.

  The file was restored byte for byte (SHA-256 checked), and the solution was rebuilt clean before the runs above.

## Artifacts

- `specs/0050/.artifacts/0302-csv-probe/probe.fsx` and `probe-output.txt`: how `CsvFile.Parse` treats quotes, short
  rows, blank lines, an empty text and a header-only text.
- `specs/0050/.artifacts/0302-build.log`, `0302-constructor-unit-tests.log`, `0302-ui-tests.log`,
  `0302-elevation.log`, `0302-invariants.log`, `0302-mutation-check.txt`.

## Gotchas

- **`ColumnRoles` and `MappedRow` have the same six field names.** Every record expression of either needs an
  annotation (`let row : MappedRow = …`, `let roles : ColumnRoles = …`).
- **FSharp.Data already skips blank lines and all-blank rows (`,`)**, as the probe showed. The explicit filter the
  slice asks for is kept, as `parseMuellerCsv` keeps it.
- **A Python here-doc rewrite turned the tests' `\n` escapes into real newlines.** It was caught and the literals were
  restored with the Edit tool, before any build. The final file is LF only, with no CR.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
