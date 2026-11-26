# Step 0220 — impl plan (WIRE: `AppContext.itemModels`)

## Approach

The step composes the existing item-model store into the app scope. It adds no behaviour.

- **`OpticalConstructor.Ui/AppContext.fs`:**
  - The `AppContext` record gains `itemModels : ItemModelProxy`, with a one-line doc: the item-model store over the
    app's library database (spec 0050 S7.1).
  - `compose` sets it to `ItemModelProxy.createInDatabase database` (step 211), beside the other database stores.
  - `create` and `recreateLibrary` both go through `compose`, so both carry the store.
  - The file opens `OpticalConstructor.Domain.ItemModels` (the type) and
    `OpticalConstructor.Storage.ItemModelDatabaseStore` (the extension).
  - No other field changes, and no window reads the field.
- **`OpticalConstructor.Ui.Tests/AppContextTests.fs`:** adds the declared fact
  `the app scope serves the Built-in item models from its library database`, in the pure-contract (`ui-tests`) block,
  with no `ui-smoke` trait.
  - `TestAppScope.freshAppContext owner` (owner = `FolderName "AppContextTests"`) composes the scope and settles the
    start-up import.
  - `itemModels.listItemModels ()` must equal `Ok (BuiltinCatalogue.itemModels () |> List.sortBy name)`.
  - That ordering is what `ItemModelProxySqliteTests` already uses for the same store.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`

Both are the slice's authorized edits.

## Risks

- **Every `AppContext` record construction must set the new field.** A grep for `experimentExport =` and
  `kitHistory =` record literals outside `compose` will find any. A `{ context with … }` copy needs no change.
- **List ordering.** The database store sorts by `(name, id)`. The brief says "ordered by name". The Built-in names
  are unique (the store test proves `sortBy name` equals the store's list), so `List.sortBy (fun m -> m.entry.name)`
  matches.
- **Elevation.**
  - The new field's type is an elevated proxy record, not a primitive.
  - The extension member is already boundary-marked.
  - Gate engine verdict (Invariant 6). A development run from the arc origin checks it.
