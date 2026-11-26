# Step 0303 — impl plan

## Approach

One read of a lab file through any import profile (spec 0050 S9.2; 001 §K "Built-in profiles REUSE the existing
parsers"; 001 §R).

1. **`OpticalConstructor.Storage/LabFileImport.fs`**: add
   `type ChosenImportProfile with member this.mapText (text : KitFileText) : Result<MappedPoints, ExperimentDataError>`
   after step 302's `ImportProfile.mapText`. It is one match on the choice:
   - `IntensitySeriesProfile`: `text.dataFileText.parseIntensitySeries`, as `SeriesPoints (IntensityData _)`.
   - `EllipsometricSeriesProfile`: `text.dataFileText.parseEllipsometricSeries`, as `SeriesPoints (EllipsometricData _)`.
   - `PhoneRoiCsvProfile`: `text.dataFileText.parseMuellerCsv` (`MuellerDataStore`), as `CapturePoints`. Its
     `MuellerDataError` is mapped onto the data channel: `MalformedRowErr r` becomes `MalformedDataFileErr r`,
     `EmptyFileErr r` becomes `EmptyDataFileErr r`, `ArchiveUnreadableErr (_, r)` becomes `MalformedDataFileErr r`, and
     `DataSetMissingErr (_, archive)` becomes `MalformedDataFileErr` of the archive's path.
   - `MuellerObservationProfile`: `text.parseMuellerObservations` (step 301), as `ObservationPoints`.
   - `KitProfileChoice profile`: `profile.mapText text` (step 302), as `ColumnPoints`.

   Each parser is called, never copied. `MuellerDataStore` is opened for its `DataFileText.parseMuellerCsv` extension.
2. **`OpticalConstructor.Domain/MeasuredData.fs`**: replace the module doc's paragraph on the two STRICT schema-v1
   shapes, and its "Deliberately NOT in scope" sentence, with one to three lines. They say the two parsers are the
   bodies of the `Intensity series` and `Ellipsometric series` import profiles (spec 0050 S9.2). No code changes.
3. **Tests first**, in `LabFileImportTests`, three facts named exactly as the slice names them:
   - `each built-in profile reads its fixture exactly as its parser did`. The intensity, 4-column ellipsometric and
     malformed X,Y texts and the BOM-prefixed Phone ROI text are written test-locally. Each `mapText` answer equals
     the wrapped parser answer, and each parser answer is checked for its shape first, so the equality is never
     vacuous. The observation profile is compared over step 301's written text.
   - `a kit profile reads through its column mapping`: `KitProfileChoice labScanProfile` over step 302's text answers
     `ColumnPoints` of its two rows.
   - `the Phone ROI profile maps the parser's errors onto the data channel`: no `avg_total` column gives
     `MalformedDataFileErr` naming `avg_total`; the full ten-column header alone gives `EmptyDataFileErr`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LabFileImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MeasuredData.fs` (doc comment only)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LabFileImportTests.fs`

## Risks

- `Observable` and other names collide with FSharp.Core modules; the member needs no such name.
- Structural equality over `MappedPoints`: `MuellerMatrix` wraps MathNet's `Matrix<double>`, whose `Equals` compares
  values, and both sides parse the same text into identical doubles.
- The step 302 fixture `labScanProfile` and its text are reused, not copied: the kit fact uses the private profile
  already in the module, and the step 302 text is lifted into one private binding that both facts read.
- ELEV: the error mapping is a lambda inside the member, not a free function (ELEV013). `.value` appears only in
  the archive path's diagnostic (ELEV004 is excluded).
