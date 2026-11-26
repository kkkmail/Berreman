# Step 0212 — impl plan (IMPLEMENT: item-model entry file and import facts; the exporter goes)

## Approach

The step is test-side plus one deletion. No product source changes (the brief says so).

0. **Delete the exporter.** `Berreman/OpticalConstructor/Catalogue/export-item-models.fsx` goes. After that,
   `Catalogue/` holds only `Builtin/`. Nothing references the script: a grep outside `specs/` finds only the
   script itself.
1. **`EntryFileJsonTests`** gains `an item-model entry file round-trips every model form`.
   - `detectorNoiseAxes` is read from the shipped Mueller polarimeter's form
     (`BuiltinCatalogue.itemModel ItemModelIds.muellerPolarimeterModelId`), never transcribed.
     `zeroNoiseAxes` is the same descriptors with `defaultValue = ParameterValue 0.0`.
   - Test-local entries cover every form:
     - the real LP under both leakages;
     - the retarder under `ConstantRetardance` and under `InverseWaveLengthRetardance` of 632.8 nm;
     - the black box under both normalisations;
     - a compound whose second component is itself a compound;
     - `EmptySlotModel`;
     - the three detectors over each noise list;
     - the three binding forms, so that "every model form" holds literally.
   - Each entry is sealed under `BuiltInProvenance` and under `UserProvenance`, with the protection that
     provenance requires. Its file text parses back as `ItemModelEntryKind` to an equal envelope, and
     `toItemModel` answers the entry.
   - For each detector entry, the fact checks that:
     - the file's noise axes carry int `kind` tags equal to `ParameterKind.value`;
     - the three defaults survive;
     - tags of 0, of 8 (unrecognised) and of the case-name string are each `MalformedJsonErr`.
   - The real LP's file parsed as `PolarizerEntryKind` is `SchemaValidationErr`.
2. **`CatalogueImportInMemoryTests.fs`**: `ImportStoreSet` gains `itemModels : ItemModelProxy`.
   - The in-memory fixture fills it from `stores.itemModels`, and the SQLite fixture from
     `ItemModelProxy.createInDatabase database`.
   - `importFrom fills empty stores…` adds: `listItemModels` answers one version per item-model row (the
     count, and the exact set of `versionId`s).
   - `every imported entry resolves…` adds, per item-model row: `resolveItemModelVersion` of the decoded
     version's `versionId` answers exactly that version. The existing `tryHeldEnvelope` assertion is kept.

## Files

- delete `Berreman/OpticalConstructor/Catalogue/export-item-models.fsx`
- edit `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`
- edit `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs`

## Risks

- **The SQLite import must store item models where `ItemModelProxy.createInDatabase` reads them.** Step 211's
  Built-in-database fact suggests it does, because the import stores every kind's envelope.
- **The kind-tag edit must reach `readParameterKindTags` before any schema rejection.** The item-model schema
  does not constrain the payload's form, so it does.
- **Count:** +1 constructor fact (1287 → 1288). The two changed facts run in both classes, so their count does
  not change.
