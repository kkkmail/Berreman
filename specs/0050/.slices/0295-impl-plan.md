# Step 0295 — impl plan

**Goal.** Each experiment's data status becomes one pure Domain badge (spec 0050 S9.1; 001 §K status badges, 001 §E a
changed or missing file is flagged). The window's `ExperimentFileStatus` moves into Domain as `ExperimentDataBadge` and is
deleted, so one type remains. An attach the kit refuses is told in the kit status.

## Approach

1. **Domain (`ExperimentDataLoad.fs`, after `loadAndValidate`).** Add `ExperimentDataBadge`:
   - cases `NoDataBadge`, `MappedDataBadge of problemOpt`, `ValidatedDataBadge of points`,
     `DataChangedBadge of attachment * currentOpt` and `OutOfRangeBadge of problem`;
   - `member this.text` (`no data`, `mapped`, `validated (<n> points)`, `data changed`, `out of range`);
   - `static member ofCheck dataOpt statusOpt outcomeOpt`, the one rule, in the slice's order: no data, then a changed or
     missing file, then the outcome.
2. **View (`TableAndElementRotationView.fs`).**
   - Delete `ExperimentFileStatus` and `ExperimentDataError.fileStatus`.
   - `experimentDataStatus` holds an `ExperimentDataBadge` per attached experiment.
   - The one private validation function becomes `experimentDataBadge model experiment`. It reads the attachment's status
     (a `KitError` becomes `MalformedDataFileErr e.describe`, as `ExperimentDataStore` maps one), validates through
     `loadAndValidate` only while the file is unchanged, and answers `ofCheck`. `withKitContent` and the attach arm both
     call it.
   - The attach arm's `Error` keeps the experiment's data and badge and sets `kitStatus` to `Not attached — <describe>`.
   - Add `model.dataBadgeOf experiment`: the map's badge, else `ofCheck experiment.dataOpt None None`.
   - `dataFileRow` shows `(model.dataBadgeOf experiment).caption`.
3. **Captions (`ConstructorUiIdOfDomain.fs`).** Add `ExperimentDataBadge.caption`. It keeps every caption the window
   showed and adds `No data` and `Mapped — not validated`.
4. **Tests.**
   - `ExperimentDataProxyTests` gains the badge fact.
   - In `InverseConstructorTests`, the four changed facts assert badges, with their captions unchanged.
   - The attach-failed fact is renamed and asserts `dataOpt = None`, `NoDataBadge` and the kit status.
   - Three other facts that name the retired type switch to the badge type mechanically, keeping their assertions.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentDataLoad.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentDataProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/InverseConstructorTests.fs`

## Risks

- **A setup revision the kit lacks** used to give `Validation error — <describe>`. The badge needs an
  `ExperimentDataError`, so it becomes `DataUnitsMismatchErr` of the `SetupError`'s `describe`. That is an
  `OutOfRangeBadge`, so the caption is unchanged. The detector that fixes the expected units is unresolved.
- **The attach arm now also reads the attachment status.** A freshly attached file is unchanged, so every mock and the
  real store answer `AttachedDataUnchanged`.
- **ELEV names.** All new names are unique among ours, checked by grep over Berreman and STL.
