# Step 0222 — impl-plan (IMPLEMENT, S72-ITEM-MODEL-OVERLAY)

## Approach

Add one type extension to `OpticalConstructor.Domain/KitLibrary.fs`, after step 142's store extensions (at the end of
the module): `type ItemModelProxy with member this.overlaidWith (overlay : KitLibraryOverlay) : ItemModelProxy`.
It reuses step 142's private helpers, never a copy of them:

- `resolveItemModelVersion v` → `resolveOverlayReference overlay v.entryReference (store read) decode`. That helper
  already answers a re-bound entry's held envelope, then the kit's snapshot of exactly that version, then the store.
  `ItemModelVersionId.entryReference` (step 213) is the exact-version reference an item pins.
- `listItemModels ()` → the store's listing, then `overlayListingSnapshots overlay [ ItemModelEntryKind ]` (one per
  id at its highest version, re-bound ids left out) minus the store-held ids, decoded and sorted by name, then id.
- `saveItemModel` passes through (`{ this with … }`).
- `decode` goes only through `LibraryEntryContent.toItemModel` (step 210). A refusal is
  `ItemModelStoreUnavailableErr $"snapshot {content.describe}: {error.describe}"`, as the sibling stores do.

`KitLibraryStores` is unchanged. Add `open OpticalConstructor.Domain.ItemModels` to the file.

## Tests (`KitLibraryTests.fs`)

- `a kit's item models resolve from its snapshots against an empty library`: snapshot the circular-polarizer and
  retarder-plate models at v1 from the Built-in library, then reopen read-only over the same folder against
  `CatalogueImportProxy.createInMemory ()`. Resolve both through `(ItemModelProxy.createInMemory ()).overlaidWith`,
  and check that `listItemModels` answers the two by name.
- `the overlaid item-model store answers the kit's divergent version and passes a save through`: over `freshStores`,
  a kit whose circular-polarizer v1 snapshot has another description.
  - `resolveItemModelVersion` of that version answers the kit's copy.
  - `listItemModels` equals the store's listing, with one circular polarizer.
  - A test-local user model saved through the overlay is held by the underlying store.
  - It also covers the spec's re-binding and refusal rules.
- Extend the step-219 helper `assertItemsReopenAgainstEmptyLibrary`, so the reopened kit resolves each item's model
  through the overlaid item-model store (the section's "reopening … MUST resolve the model from its
  .item-model-json snapshot"). This only adds assertions.

## Risks

- The spec says "the store's eight item models". The Built-in catalogue has shipped eleven since the operator's
  0050-210 manual commit added the three binding models. The fact asserts the store's own listing (eleven); see the
  impl-log's Gotchas.
- Opening `ItemModels` may shadow names in `KitLibrary.fs`. The build will tell.
