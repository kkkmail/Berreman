# Step 0210 — impl plan (IMPLEMENT: the item model becomes a library kind)

## Approach

The item model (step 209's `ItemModelEntry`) becomes the seventh library-entry kind, end to end:

1. **Domain, `LibraryEntryFiles.fs`.**
   - `LibraryEntryKind.ItemModelEntryKind`: tag 7, `.item-model-json`, schema version 1, last in `all`.
   - `ItemModelId.toLibraryEntryId` and `ItemModelVersionId.entryReference`.
   - `ItemModelPayload of ItemModelForm`, with `ofItemModel` and `toItemModel` (the second marked `StorageBoundary`).
   - `references` answers `[]` for an item model.
2. **Every exhaustive match** on the kind or the payload gains the case, with no wildcard:
   - `CatalogueImport` (apply order; the in-memory push through `ItemModelProxy.saveItemModel`; pinning);
   - `SchemaValidation` (schema file and lazy);
   - `LibraryDatabaseStore` (`facetRows`, `sampleMaterialRows`);
   - `ConstructorUiIdOfDomain` (caption `Item model`);
   - the test helpers.
   The compiler (FS0025 is an error) lists the rest.
3. **`InMemoryCatalogueStores.itemModels`**, built by `CatalogueImportProxy.createInMemory` with
   `ItemModelProxy.createInMemory ()`. A first put of an item model decodes it and pushes `{ version; entry }`.
4. **`schema/library-item-model.schema.json`**, written like the per-kind schemas: `kind` const 7, the v1 body
   closed, `payload` `{ Case: "ItemModelPayload", Fields: [ <form> ] }`. It is a `Content` item copied
   `PreserveNewest`.
5. **The `ParameterKind` wire tag, in `EntryFileJson.fs`.** The entry file stores each detector noise axis's kind
   as its int `value`, and reads it back through `ParameterKind.tryOfWire`. An unknown tag is `MalformedJsonErr`
   at the read seam.
   - It is a node adapter over the canonical stack (`nodeOf`, `CanonicalJson.options`, `contentHashOf`): the
     content's file node is the canonical node with those tags as ints.
   - The hash covers exactly that node, so the file stays "content + the hash over exactly it".
   - Every other kind's node is unchanged, so its hash and its bytes do not change.
6. **`Catalogue/export-item-models.fsx`** (throw-away).
   - It reads `Builtin/`, seals the eleven built-in item models, rebuilds the catalogue and writes it back through
     the product proxy.
   - The detector noise defaults come from `BerremanTests.NoiseParam.decentLab`, read once.
   - Run it once. The eleven new files and the manifest change; every other file is rewritten byte for byte.
7. **Tests.**
   - `BuiltinCatalogue.itemModels` / `itemModel`.
   - Integrity: count 11 item models, plus a new fact for the eleven by id, name, form, version, protection and
     provenance.
   - `LibraryEntryFilesTests`: seven tags, `None` for 0 and 8, and a new builder/decoder fact.
   - `CatalogueImportProxyTests`: the tape ends with the item models.

## Files

The authorized effects, plus every file holding a kind match the compiler reports incomplete (the slice's
requirement (2), "every match … gains the case"): `KitLibrary.fs`, `PresetDatabaseStore.fs` and
`CatalogueImportInMemoryTests.fs`. Each one is recorded in the impl log.

## Risks

- **Adding 11 rows to the shipped catalogue** reaches every suite that reads `Builtin/`: database staging, start-up
  import, kit overlays. The whole constructor suite and the selected UI classes are run to find any count or
  shape that assumed six kinds.
- **The script runs under `dotnet fsi`, where `AppContext.BaseDirectory` is the SDK's folder.** The schema loader
  resolves `schema/` there, so the script points `APP_CONTEXT_BASE_DIRECTORY` at the Tests output, where the
  build copied the schemas.
- **The hash of an item-model entry is over its file node** (int tags), not over `contentHashOf content`. The
  integrity fact therefore checks the hash through `sealed`.
