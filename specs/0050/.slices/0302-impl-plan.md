# Step 0302 — impl plan

## Goal

A kit profile reads a lab file of any delimited shape through its column roles (spec 0050 S9.2; 001 §K).
`ImportProfile.mapText` turns a `KitFileText` into `MappedRow`s (step 300's record) through the profile's delimiter,
header choice and `ColumnRoles`, refusing the first unreadable cell with its data row, column and text.

## Approach

All production code goes in `OpticalConstructor.Storage/LabFileImport.fs` (step 301), the only Storage file the step
may edit.

1. **A private CSV read**, `labFileRows : DataDelimiter -> HeaderRowChoice -> KitFileText -> Result<DataFileCell list
   option * DataFileCell list list, ExperimentDataError>`.
   - It calls FSharp.Data's `CsvFile.Parse`, as `parseMuellerCsv` and `SpectrumSidecarText.parseSpectrum` do, with
     `separators` set to the delimiter's character and `hasHeaders` set by the header choice.
   - The rows are enumerated inside the `try`, because FSharp.Data's row-shape error is raised lazily. Every exception
     becomes `MalformedDataFileErr` with its message.
   - Rows whose cells are all blank are skipped.
   - It answers the header's cells (when there is a header) and the data rows' cells, both as `DataFileCell`s (step
     300), so no primitive appears in a private signature (ELEV003P).
   - A blank text is `EmptyDataFileErr`, so an empty file is not reported as FSharp.Data's "header row not found".
   - Step 307's preview will call the same function.
2. **`type ImportProfile with member this.mapText`**.
   - It reads the rows. No data row is `EmptyDataFileErr`.
   - Each row becomes one `MappedRow`, each mapped cell taken by `DataColumnNumber.index` and trimmed:
     - scan columns are `ScanCell`s of `UnitMagnitude`;
     - the reading is `SignalReading` (`MeasuredIntensity`, `MeasuredSigma`) or `MuellerReading`, whose element
       (i, j) is column 4i + j, times the throughput when there is one;
     - the timestamp uses `DateTimeOffset.TryParse` (invariant, `AssumeUniversal`);
     - the capture index uses `Int32.TryParse` into `CaptureIndex`;
     - the observable uses `Observable.tryOfCell`;
     - the key is `DataRowKey`.
   - A refused cell keeps its column, and the leftmost refusal in the row wins. So "the first cell" is the row's own
     left-to-right order, whichever role reads it, and the first refused row is reported.
   - A profile whose Mueller mapping does not hold exactly `MuellerElementIndex.all`'s sixteen columns is refused up
     front, so the matrix build never indexes out of range.
3. **Tests** go in `LabFileImportTests`, over test-local profile records: the three facts the slice names. The Mueller
   fact reuses the test file's existing `absoluteMatrix` helper for its fifteen fractions and its expected matrix.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs` (edit)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LabFileImportTests.fs` (edit)

## Risks

- **`ColumnRoles` and `MappedRow` share every field name**, so record expressions need type annotations.
- **`Observable` is also an FSharp.Core module.** Use `MuellerInverse.Observable.tryOfCell`, as step 301 does.
- **FSharp.Data throws on a row shorter than the header.** This was probed in
  `.artifacts/0302-csv-probe/probe-output.txt`. The exception is caught and answered as `MalformedDataFileErr`.
- **ELEV rules.** Keep every module-level signature elevated. The analyzers report the call-site rewrap only for
  `T x.value`, but values are still elevated at their parse (`numberOf >> Option.map MeasuredIntensity`), never
  wrapped later.
