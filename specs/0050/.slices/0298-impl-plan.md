# Step 0298 — impl plan

## Approach

S9.2's first step: the closed sets an import profile is made of, as domain types with int disk tags, and their
converters in the shared JSON options. Nothing reads them yet (step 299 puts profiles in the kit file).

1. **`OpticalConstructor.Domain/Kit.fs`**
   - Move `ImportProfileName` (members unchanged) from after `DataAttachmentStatus` / `DataAttachment` /
     `KitDataFile.attachment` to directly before `NamingTemplate`. `DataRowKey`, `DataRowSelection` and
     `MappedAttachment` stay where step 291 put them.
   - Add `ImportProfileName.phoneRoiCsv` (`Phone ROI CSV`), `.muellerObservations` (`Mueller-matrix observations`) and
     `.builtIns` (the four, in the slice's order).
   - Directly after it, five fieldless unions, each with `value : int` (1..n), `tryOfWire`, `text` and `all`:
     `DataDelimiter` (+ `character`), `HeaderRowChoice`, `ByteOrderMark`, `ScanColumnUnit` (+ `spectralUnitOpt` over
     `Units.UnitOfMeasure`) and `ScanReading`.
   - Then `DataColumnNumber of int` with `.value`, `tryCreate` (None below 1) and `index` (value - 1).
2. **`OpticalConstructor.Storage/CanonicalJson.fs`**: five `ClosedSetTagConverter` registrations (step 166's `KitStar`
   form), before the `JsonFSharpConverter`; the options' doc lists them.
3. **Tests**
   - `KitTests`: the new fact `the closed sets of an import profile round-trip their disk tags and render their texts`.
   - `CanonicalJsonTests`: the two facts gain the slice's assertions (all five through `assertWrittenAsTag`, plus the
     named literal tags; the refused tags `DataDelimiter 4` and `ScanColumnUnit 7`).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`

## Risks

- ELEV016 (unique DU case names): every new case name was grepped across Berreman and STL — none exists.
- `Units.UnitOfMeasure` cases are reached qualified (`Units.Nanometer`), since `Kit.fs` does not open `Units`.
- `CanonicalJson.fs` does not open `OpticalConstructor.Domain.Kit`; the converters name the types qualified, as the
  boolean converters do, to avoid widening the module's opens.
