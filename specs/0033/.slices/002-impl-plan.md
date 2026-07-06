# Slice 002 — impl plan

## Goal

Elevate material and sample identity: `MaterialId` and `SampleId` as Guid-backed
single-case DUs in `OpticalConstructor.Domain` (`.value` accessor, `static member
create : unit -> _` minting `Guid.NewGuid()`); no raw string material/sample id
remains in any domain record. No legacy string-id path, no fallback, no migration.

## Approach

1. **Domain/MaterialLibrary.fs** — add `MaterialId` (plus `tryCreate : string ->
   MaterialId option` for the two genuine IO boundaries: the library-JSON import
   and the drag payload) and a `MaterialIds` module of FIXED literal Guids
   (`Guid.Parse` of literals) for the 12 built-ins, so seeds/trees/tests stay
   deterministic. Re-type `MaterialEntry.id : MaterialId`; `resolveMaterial` /
   `resolveMaterialWithDisp` become `MaterialId` lookups; relabel
   `MaterialError.UnknownMaterialId` to carry `reason : string` (the diagnostic
   text includes the Guid string form).
2. **Domain/ElementId.fs** — add `SampleId`; re-type `Sample.id : SampleId`,
   `SampleLayer.materialId : MaterialId`, `SampleStructure.lower : MaterialId
   option`. Restructure the 11 seeded samples into a named `SeedSamples` module
   (fixed literal Guids), `seedEntries` built from `SeedSamples.all`, and
   `seedTrees` leaves referencing the seeded samples programmatically (a
   `sampleLeaf` helper over `(SampleItem s).entryId`) instead of repeating
   literals. `LibraryEntry.entryId` returns the Guid STRING form for samples so
   the Selector `valueId` binding seam stays a string.
3. **Domain/Propagation.fs** — types flow through `resolveSampleMaterials`
   unchanged (compile check only).
4. **Storage/MaterialImport.fs** — imported entries MINT `MaterialId.create ()`
   (drop the string id parameter of `entryFromTabulated`; the formula-1 branch
   mints inline).
5. **Storage/Report.fs** (`Storage.MaterialLibrary`) — the DTO keeps `id :
   string` (the persisted Guid string form); `dtoToEntry` parses through
   `MaterialId.tryCreate`, a non-Guid id is a typed `JsonParseError` (no legacy
   path); `exportMaterials` writes `string e.id.value`.
6. **Ui/StackEditor.fs** — `mediumFromMaterial` / `layerMaterialDrop` take
   `MaterialId`.
7. **Ui/MaterialsView.fs** — `Filter.selected : MaterialId option`,
   `SelectMaterial of MaterialId`; the drag payload carries the Guid string form
   and the drop handler parses it back via `tryCreate` (unparsable ⇒ no-op).
8. **Ui/Schematic.fs** — re-key the curated colour map by the built-ins' Guid
   string forms (from `MaterialIds`); `colorForMaterial` keeps its string
   "identity key" signature because `Schematic.layout` callers supply synthetic
   per-layer keys (`layer-%d`), not library ids.
9. **TestWindows/TableAndElementRotationView.fs** — `bandColorHex` becomes a
   `Map<MaterialId, string>` over `MaterialIds` (the host id literals go away);
   `materialDisplayName` / `sampleBandSpecs` take `MaterialId`;
   `materialErrorText` prints the error's `reason`.
10. **Tests** — update all construction/lookup sites (DispersionModels,
    LibraryProxy, Propagation, StackEdit, SchematicGeometry) to the named
    `MaterialIds` / `SeedSamples`; add the acceptance round-trip tests
    (create-store-lookup by minted `MaterialId`; seeded + minted `SampleId`
    round-trips through `entryId`/`tryGetEntry`).
11. **Ui.Tests** — MaterialsPanel/LibraryControls/ExperimentControls/LayerBands
    tests reference seeded ids programmatically (outside the declared `touches`,
    required to keep the `ui-smoke`/`ui-tests` gates green — recorded in
    Gotchas).

## Risks

- Wide compile surface (5 projects + 2 test projects); mitigated by frequent
  `dotnet build`.
- `OpticalPropertiesWithDisp` holds closures — round-trip assertions compare by
  reference, never structural equality.
- Ui.Tests are outside `touches` but bind seeded sample ids by string literal;
  they MUST be updated or the `ui-tests`/`ui-smoke` gates regress.
- Line endings: keep LF (verify with `git diff --numstat` vs `--ignore-cr-at-eol`).
