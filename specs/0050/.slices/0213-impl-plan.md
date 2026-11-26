# Step 0213 — impl plan (IMPLEMENT: an item's identity and the model it instantiates)

## Approach

`OpticalConstructor.Domain/Kit.fs` gains three opens (`ItemModels`, `Library`, `Placement`) and, appended at the end
of the `Kit` module in the brief's order, the item identity contracts (spec 0050 S7.1; 001 §I):

1. `ItemError`: seven `of reason : string` cases with a `describe` member in step 39's form —
   `InvalidItemLabelErr`, `DuplicateItemLabelErr`, `DuplicateItemIdErr`, `UnknownItemParameterErr`,
   `InvalidParameterStateErr`, `InvalidItemModelBindingErr`, `UnknownKitItemErr`.
2. `ItemId of Guid` with `.value`.
3. `ItemLabel of string` with `.value`, `tryCreate` (refuses blank / white space with `InvalidItemLabelErr`) and
   `fileStem` = `FileStem.ofText this.value`.
4. `ItemNotes of string` with `.value`.
5. `ItemRevision of int` with `.value`, `first` (1) and `next`.
6. `ItemModelInstance` = `PresetInstance of model : ItemModelVersionId * preset : ElementValueId` |
   `SampleInstance of model : ItemModelVersionId * sample : SampleVersionId` |
   `ItemModelVersionInstance of ItemModelVersionId`, with
   - `entryReferenceOpt : EntryReference option` — always `Some` of the model version's `entryReference`
     (step 210's `ItemModelVersionId.entryReference`), for every case, a non-GUID preset included;
   - `bindingReferenceOpt : EntryReference option` — step 137's rules: a preset is
     `tryToLibraryEntryId |> Option.map EntryLatestReference`, a sample version is the `EntryVersionReference` of
     its id and version, a standalone model is `None`.

Tests: new `OpticalConstructor.Tests/KitItemTests.fs` (`module KitItemTests`, namespace `OpticalConstructor.Tests`),
appended at the end of the test project's compile list, with the four declared facts.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- Opening `Library` / `Placement` / `ItemModels` in `Kit.fs` could shadow a name `Kit.fs` already uses; the build
  shows it. Names were checked unique across the repo and STL (ELEV014 / ELEV016).
- The sample binding's `EntryVersionReference` is built in `Kit.fs` because `VersionRef.entryReference`
  (`Lifecycle.fs`) compiles after `Kit.fs` and lies outside this step's authorized edits.
