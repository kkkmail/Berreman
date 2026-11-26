# Step 0317 — impl plan

## Approach

S9.3's "one file → many experiments: split by a key column" (001 §K, §P `SplitByColumn`), as pure Domain. The slice
asks for one reading of a point's key, shared by the row selection and the split. This step adds no UI.

1. **`ImportProfiles.fs`**, before `selectedBy`:
   - `type MuellerExperimentLabel with member this.rowKey : DataRowKey`. A Phone ROI row's label is the key it is
     selected by. `MuellerReconstruction.fs` is not edited.
   - `MappedPoints.pointKeys : DataRowKey option list`, each point's key in point order:
     - `None` for each point of a series;
     - a capture's `experiment.rowKey`;
     - an observation's `key`;
     - a mapped row's `keyOpt`.
   - `selectedBy` keeps its contract and its texts. It keeps the points whose `pointKeys` entry is `Some key`, zipped
     in point order. The "maps no key column" refusal of `ColumnPoints` now reads `pointKeys` too.
2. **New `DataSplits.fs`** (`module DataSplits`), compiled directly after `ExperimentDataLoad.fs`. It contains, in this
   order:
   - `SplitKeyColumn` with `.value`;
   - `ChosenImportProfile.splitKeyOpt`: `experiment`, `sample`, `column <n>` or `None`;
   - `KeyedGroup = { key; points }`;
   - `MappedPoints.splitGroups`. It takes the distinct `Some` keys of `pointKeys` in first-appearance order
     (`List.distinct`, whose order is documented), each counted. A series, or `ColumnPoints` without a key, is refused
     with `MalformedDataFileErr`.
3. **Tests:**
   - `ImportProfileTests` gains the pointKeys/selection fact.
   - New `DataSplitTests` holds two facts, appended last in the test project.
   - Fixture builders are reused, not copied. `observationOf` and `twoPointIntensity` become public. The selection
     fact's local `labScanRow` moves to module level, public, with its body unchanged. A new public `capturesLabelled`
     builds Phone ROI rows from labels over `captureAt`. `DataSplitTests` calls these, as `ImportProfileTests` already
     calls `KitTests.labScanProfile`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ImportProfiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DataSplits.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ImportProfileTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataSplitTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **Record-label inference.** `KeyedGroup`'s `key` shares its label with `KeyedObservation.key`. Every construction
  is annotated.
- **Step 305's selection fact must pass as written.** Moving its local `labScanRow` out leaves every assertion
  unchanged. The refusal texts are kept byte for byte.
- **Elevation.**
  - `DataRowKey this.value` inside a member of `MuellerExperimentLabel` is the sanctioned conversion-body shape for
    ELEV001 and ELEV005.
  - `rowKey`, `pointKeys`, `splitKeyOpt`, `splitGroups`, `SplitKeyColumn`, `KeyedGroup` and `DataSplits` collide with
    no name in Berreman or Softellect.Sys (checked by grep).
