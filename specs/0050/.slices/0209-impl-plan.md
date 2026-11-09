# Step 0209 — impl plan (ADD_CONTRACT STORE_BigBoy_0006 ItemModelProxy)

## Approach

Declare the item-model store seam at the end of `OpticalConstructor.Domain/ItemModels.fs`, after step 208's forms,
and pin it with a new contract suite, `OpticalConstructor.Tests/ItemModelProxyTests.fs`. Tests first (red build),
then the domain types.

1. Types, in this order, each with a one-to-three-line doc citing spec 0050 S7.1:
   - `ItemModelId of Guid` with `.value`;
   - `ItemModelVersionId = { itemModelId : ItemModelId; version : VersionNumber }`;
   - `ItemModelEntry = { id; name : EntryName; description : EntryDescription; model : ItemModelForm;
     protection : EntryProtection }`, shaped like `PolarizerPreset`;
   - `ItemModelVersion = { version : VersionNumber; entry : ItemModelEntry }` with `member this.versionId`;
   - `module ItemModelIds`, the eleven frozen GUIDs (`Guid.Parse "…" |> ItemModelId`), shaped like
     `MaterialLibrary.MaterialIds`;
   - `ItemModelError` = `DuplicateItemModelErr | InvalidItemModelErr | ItemModelStoreUnavailableErr`, each
     `of reason : string`, with `describe` in step 39's `<condition>: <reason>` form;
   - `[<ReferenceEquality>] ItemModelProxy = { listItemModels; resolveItemModelVersion; saveItemModel }` and
     `static member createInMemory () : ItemModelProxy`, a closure-local `Map<ItemModelId, ItemModelVersion>`
     mutated only inside the closure, as `CatalogueProxy.createInMemory` keeps its store.
2. The in-memory list sorts by `(entry.name, entry.id)`, the order `SourceProxy.createInDatabase` uses, so the
   database store of step 211 can run the same contract.
3. Tests: `[<AbstractClass>] ItemModelProxyContract` with `abstract freshItemModels : unit -> ItemModelProxy` and the
   three contract facts; `ItemModelProxyTests` inherits it over `createInMemory` and adds the error-render fact.
   Test-local `UserManaged` entries `Lab retarder` (`RetarderPlateModel ConstantRetardance`) and `Lab polarizer`
   (`RealPolarizerModel DiattenuationLeakage`) under test-local GUIDs. The file is appended last in the test
   project's compile list, with no comment.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ItemModels.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemModelProxyTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- ELEV014/015/016: every new type, module-level and case name was checked against Berreman and STL — none exists.
- The brief says "`module ItemModelProxyTests` in `namespace OpticalConstructor.Tests`" and also
  "`type ItemModelProxyTests () = inherit ItemModelProxyContract ()`". One name cannot be both a module and the
  class that the census names (`OpticalConstructor.Tests.ItemModelProxyTests`), so the file is
  `namespace OpticalConstructor.Tests` holding the contract and the class, as every other contract suite here is
  (`SourceLibraryTests.fs`, `LibraryBackupProxyTests.fs`). The test data goes in a private module.
- `.contracts-json` records `STORE_BigBoy_0006` as `declared` with `declaringStep: 216`. It is not in this step's
  authorized edits, and prior ADD_CONTRACT steps left the registry to the arc-runner, so it is not edited.
