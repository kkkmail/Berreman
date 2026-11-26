# Step 0302 — state of the world

## Where we are

Spec 0050 section S9.2 covers import profiles and the column-mapping wizard (001 §K).

- Steps 298 to 300 added the closed sets, the kit profiles, the built-in profiles and `MappedRow`.
- Step 301 added the Mueller-matrix observation file in `OpticalConstructor.Storage/LabFileImport.fs`.
- This step adds the custom mapping. A kit profile reads a lab file of any delimited shape through its delimiter,
  header choice and column roles, and answers `MappedRow`s.
- Nothing calls `mapText` yet. Step 307's preview will reuse the CSV read this step adds, and later steps will attach
  mapped rows to experiments.

## What's working

- Add `ImportProfile.mapText` (S9.2): reads a lab file through a kit profile's delimiter, header and column roles into
  `MappedRow`s.
- Read scan values, signal and σ, or m00…m33 times an optional throughput, plus the timestamp, capture index,
  observable and key.
- Refuse a row at its leftmost cell that is missing, blank or unreadable, naming the data row, the column and the cell.
  A header-only or blank file is `EmptyDataFileErr`.
- Add a private CSV read over FSharp.Data's `CsvFile`, with the delimiter and the header choice, that skips blank rows
  and maps every reader exception to `MalformedDataFileErr`. Step 307's preview will reuse it.
- Add three facts to `LabFileImportTests`; constructor-unit-tests rises by 3, to 1509.

## Tests

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0; `0 Warning(s)`, `0 Error(s)`; no lowercase `error` in stdout | 44 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded; exit 0 | 217 s | 600 s |
| constructor-unit-tests | Passed!, 1509 of 1509, 0 failed, 0 skipped | 73 s | 180 s |
| ui-tests (FsxAcceptanceTests) | Passed!, 14 of 14, 0 failed, 0 skipped | 77 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **constructor-unit-tests: 1509**, the 1506 baseline plus the 3 new facts. Each new identity is `Passed` in the TRX,
  named exactly as the slice names it:
  - `a column mapping reads each row's scan values, signal, σ and key`
  - `a semicolon file without a header maps its Mueller elements and throughput`
  - `a cell the mapping cannot read names its row and column`
- **ui-tests** ran over `OpticalConstructor.Ui.Tests.FsxAcceptanceTests` only, as the step selects. All 14 passed, the
  same count as step 301, and that class was not edited.
- **Red first.** Before the production code, the test project failed to compile with exactly the two `FS0039` errors
  for the missing `ImportProfile.mapText`.
- **Mutation check.** Three defects in one build each failed their own fact:
  - σ read from the signal column;
  - the throughput ignored;
  - data rows numbered from 0.

  The file was restored byte for byte (SHA-256 checked) and the solution rebuilt before the runs above.

```yaml
gates:
  constructor_unit_tests: 1509
  ui_tests: 14
```

## Architecture

- **One CSV read per layout.** The private `labFileRows` in `LabFileImport.fs` answers the header's cells (when the
  first row is a header) and the data rows' cells, as `DataFileCell` (step 300).
  - It calls `CsvFile.Parse` with the delimiter's character as `separators` and `hasHeaders` from the header choice,
    as `parseMuellerCsv` and `parseSpectrum` parse.
  - It enumerates the rows inside its `try`, because FSharp.Data raises its row-width error lazily.
  - It skips all-blank rows and answers a blank text as `EmptyDataFileErr`.
  - Its signature carries only elevated types, so it is ELEV003P-clean. Step 307's `previewWith` will call it.
- **`ImportProfile.mapText` is an extension member in Storage**, beside step 301's observation-file members. Every
  helper is local to the member.
- **The leftmost refusal wins.**
  - Each role read answers `Result<_, DataColumnNumber * ExperimentDataError>`.
  - A local `both` combinator keeps the smaller column when two reads fail, and `all` folds a list through it.
  - So "the first cell" is the row's left-to-right order, whichever role reads the cell.
  - The first refused data row is the file's error, collected with `Softellect.Sys.Rop.unzip` as step 301 collects.
- **Values are elevated at their parse.** The readers are `numberOf >> Option.map UnitMagnitude`, `MeasuredIntensity`
  and `MeasuredSigma`; `RowTimestamp`, `CaptureIndex` and `DataRowKey` are built inside their `TryParse` match. The
  observable goes through step 300's `Observable.tryOfCell`.
- **The Mueller matrix is the values in rows of four.** Element (i, j) is the column at index 4i + j of the profile's
  element list, and `List.chunkBySize 4 |> RealMatrix4x4.create` builds it. With a throughput column, every element is
  multiplied by it.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **`ColumnRoles` (Kit.fs) and `MappedRow` (ImportProfiles.fs) share all six field names.** Annotate every record
  expression of either, or F# picks the later type.
- **`Observable` is also an FSharp.Core module.** Storage calls `MuellerInverse.Observable.tryOfCell`.
- **A data row's number counts only the rows the read answers**: after the header, and after any skipped blank row.
- **The throughput multiplies m00 too.** For a normalized file (m00 = 1) that is step 301's "m00 is the throughput".
  An un-normalized file with a throughput column is scaled as read; the slice adds no check for it.
- **A Mueller mapping with other than sixteen element columns is refused up front, as `MalformedDataFileErr`.**
  `ColumnRoles.tryOfChoices` never builds one, but a record-built profile can. `ExperimentDataError` has no profile
  case.
- **FSharp.Data facts, from the probe in `.artifacts/0302-csv-probe/`:**
  - quotes are removed;
  - blank lines and all-blank rows are skipped already;
  - a row of another width than the header throws;
  - an empty text throws "header row not found" or "no data rows found".
- **A cold full build reports two `SYSLIB0051` warnings** in the vendored, untouched
  `MathNetNumerics/Numerics/Exceptions.cs`. The solution build after this step's change reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0302: a kit profile's column mapping (S9.2).
  - `LabFileImport.fs` gains the private `labFileRows` CSV read and `ImportProfile.mapText`.
  - `LabFileImportTests` gains three facts; constructor-unit-tests rises by three, to 1509.
