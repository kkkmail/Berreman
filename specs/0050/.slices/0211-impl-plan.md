# Step 0211 — impl plan (IMPLEMENT_CONTRACT STORE_BigBoy_0006 ItemModelProxy)

**Approach.** Add the SQLite item-model store, `ItemModelProxy.createInDatabase`, in a new
`OpticalConstructor.Storage/ItemModelDatabaseStore.fs`. It follows the shape of the other library stores (steps 76 and 79:
`SampleDatabaseStore`, `SourceDatabaseStore`, `CategoryDatabaseStore`, `PresetDatabaseStore`).

- Each field is one `LibraryDatabase.run`, and each write has one `SaveChanges ()`.
- Every `StorageError` and every decoder `LibraryEntryError` becomes `ItemModelStoreUnavailableErr` carrying its
  `describe`.
- `listItemModels`: `latestStoredOfKind ItemModelEntryKind`, whatever the lifecycle, decoded with `toItemModel` into
  `{ version; entry }`, sorted by name and then by id.
- `resolveItemModelVersion`: `tryStoredVersion` on the item model's entry id and version, keeping only a row of kind
  `ItemModelEntryKind`, then decoded.
- `saveItemModel`, in this order:
  1. The latest row is an item model: `DuplicateItemModelErr`, naming the id and its latest version.
  2. The latest row is another kind: `InvalidItemModelErr`, naming the id and that kind's extension.
  3. `ofItemModel v.version UserProvenance v.entry` refuses: `InvalidItemModelErr` with the builder's `describe`.
  4. Otherwise: `stageNewEntry`, then `SaveChanges`.

**Files.**

- New: `Storage/ItemModelDatabaseStore.fs`.
- Edited: `Storage.fsproj` (one `<Compile>` line after `PresetDatabaseStore.fs`) and `Tests/ItemModelProxyTests.fs`.
- `.contracts-json`: `STORE_BigBoy_0006` set to `implemented` at step 211.

**Tests.** Add `ItemModelProxySqliteTests`, which inherits `ItemModelProxyContract` and implements `IDisposable`.

- The three contract facts each run over an empty `:memory:` database per store. The class records every database it
  opens and disposes them all.
- Its own three facts each run over `BuiltinCatalogue.freshLibraryDatabase ()`, which the fact disposes:
  - it lists and resolves the Built-in item models;
  - it holds user item models only and refuses ids it cannot hold;
  - a disposed database gives `ItemModelStoreUnavailableErr`.
- The expected count is +6 (1281 → 1287).

**Risks.**

- ELEV cleanliness: the store follows the boundary-marked pattern exactly.
- An unintended FS0025 or other warning.
- The order of the save checks: the duplicate check comes before the builder, so a `ProtectedBuiltIn` entry under a
  held built-in id is `DuplicateItemModelErr`.
