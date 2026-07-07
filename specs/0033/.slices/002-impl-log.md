# Slice 002 — impl log

## Progress

- [x] Recon: read slice, MaterialLibrary.fs, ElementId.fs, Propagation.fs,
      MaterialImport.fs, Report.fs, StackEditor.fs, MaterialsView.fs,
      Schematic.fs, TableAndElementRotationView.fs, all affected test files,
      gates snapshot, slice-001 impl-log/SoW (precedent).
- [x] Domain/MaterialLibrary.fs: `MaterialId` Guid-backed single-case DU
      (`.value`, `create : unit -> MaterialId` minting `Guid.NewGuid`, and
      `tryCreate : string -> MaterialId option` for the two genuine IO
      boundaries); `MaterialIds` module of 12 FIXED literal Guids for the
      built-ins; `MaterialEntry.id : MaterialId`;
      `resolveMaterial`/`resolveMaterialWithDisp` are `MaterialId` lookups;
      `MaterialError.UnknownMaterialId` relabelled to `reason : string`
      (diagnostic text carries the Guid string form). Module-level
      `tryMaterialId`/`newMaterialId` helpers added because the qualified
      `MaterialLibrary.MaterialId.tryCreate` resolves to the union CASE, not
      the type (the same collision `Library.elementId` documents).
- [x] Domain/ElementId.fs: `SampleId` Guid-backed single-case DU (+
      `newSampleId` helper); `Sample.id : SampleId`,
      `SampleLayer.materialId : MaterialId`,
      `SampleStructure.lower : MaterialId option`; the 11 seeded samples
      restructured into a named `SeedSamples` module (FIXED literal Guids,
      `SeedSamples.all` in display order); `seedEntries` built from
      `SeedSamples.all`; `seedTrees` sample leaves reference the seeded sample
      VALUES via a `sampleLeaf` helper over `(SampleItem s).entryId` — no id
      literal repeated; `LibraryEntry.entryId` returns the Guid STRING form for
      samples so the Selector `valueId` binding seam is unchanged.
- [x] Domain/Propagation.fs: unchanged — `resolveSampleMaterials` /
      `sampleToSystem` flow the elevated types through (compile-verified).
- [x] Storage/MaterialImport.fs: imported entries MINT `MaterialId.create ()`
      (the string id parameter of `entryFromTabulated` removed; the formula-1
      branch mints inline).
- [x] Storage/Report.fs (Storage.MaterialLibrary): DTO keeps `id : string`
      (persisted Guid string form); `dtoToEntry` returns
      `Result<MaterialEntry, StorageError>` parsing through
      `MaterialId.tryCreate` — a non-Guid persisted id is a typed
      `JsonParseError`, no legacy path; `importMaterials` folds the DTO list
      short-circuiting on the first bad id; `exportMaterials` writes
      `string e.id.value`.
- [x] Ui/StackEditor.fs: `mediumFromMaterial` / `layerMaterialDrop` take
      `MaterialId`.
- [x] Ui/MaterialsView.fs: `Filter.selected : MaterialId option`;
      `SelectMaterial of MaterialId`; the drag payload carries the Guid string
      form and `onDrop` parses it back via `MaterialLibrary.tryMaterialId`
      (unparsable ⇒ no-op); `materialDrop` takes `MaterialId`.
- [x] Ui/Schematic.fs: curated colour map re-keyed by the built-ins' Guid
      string forms (from `MaterialIds`); `colorForMaterial` keeps its string
      "identity key" signature because `Schematic.layout` callers supply
      synthetic per-layer keys (`layer-%d`), not library ids.
- [x] TestWindows/TableAndElementRotationView.fs: the host id literals removed —
      `bandColorHex` is now a `Map<MaterialId, string>` over `MaterialIds`;
      `materialDisplayName` / `sampleBandSpecs` take `MaterialId`;
      `materialErrorText` prints the error's `reason`.
- [x] Tests updated: DispersionModelsTests (silicon by `MaterialIds`, unknown
      id minted, reason asserted to carry the Guid string), StackEditTests
      (same), LibraryProxyTests (seed/expandedFilms sites re-typed),
      PropagationTests (glassSample minted id; seeded-sample tests keyed by
      `SeedSamples.*.id` if/elif instead of string matches; unknown-material
      tests mint ids and assert the reason), SchematicGeometryTests (curated
      keys are the Guid string forms).
- [x] New acceptance tests (create-store-lookup round-trips by id, spec
      acceptance): DispersionModelsTests ``a minted MaterialId round-trips
      create-store-lookup through the library`` (+ `tryCreate` string-form
      round-trip and non-Guid rejection); LibraryProxyTests ``every seeded
      sample round-trips store-lookup by its Guid entry id``, ``a MINTED
      SampleId round-trips create-store-lookup through a proxy of the same
      shape``, ``the seeded sample ids are distinct, non-empty Guids``.
- [x] Ui.Tests updated (outside declared `touches` — see Gotchas):
      MaterialsPanelTests (MaterialId selection/drop; unknown drop minted),
      LibraryControlsTests / ExperimentControlsTests / LayerBandsControlsTests
      (seeded-sample entry ids referenced programmatically via
      `(Library.SampleItem Library.SeedSamples.*).entryId` helpers).
- [x] Local verification (advisory — the arc-runner gate engine re-runs the
      authoritative gates after exit): build 0 errors
      (`--warnaserror+:25` clean); BerremanTests 84 passed / 5 skipped;
      OpticalConstructor.Tests 310 passed (+4, none removed); ui-smoke 54
      passed; ui-tests 249 passed. Capture:
      `specs/0033/.artifacts/002-local-verify.log`.
- [x] LF policy: StackEditTests.fs's working copy was fully CRLF (pre-existing,
      like slice 001's schema JSON); normalized to LF. `git diff --numstat`
      identical with and without `--ignore-cr-at-eol`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ElementId.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/Report.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/StackEditor.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/Schematic.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PropagationTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StackEditTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SchematicGeometryTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsPanelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryControlsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ExperimentControlsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LayerBandsControlsTests.fs`

## Testing state

All five suites pass locally (advisory run): build clean (0 errors,
`--warnaserror+:25` respected), unit-tests 84 passed / 5 skipped
(pre-existing skips), constructor-unit-tests 310 passed (+4 added, none
removed), ui-smoke 54 passed, ui-tests 249 passed. Every `count_at_least`
baseline holds or grows. No blockers; every requirement of the slice landed
this round.

## Artifacts

- `specs/0033/.artifacts/002-local-verify.log` — captured build/test outcomes
  for all five gates (advisory).

## Gotchas

- **Ui.Tests touched outside the slice's `touches` list.** Four Ui.Tests files
  bound seeded samples by their old string-id literals
  (`"sample-glass-1mm"` …); with `entryId` now the Guid string form those
  binds would miss and the `ui-smoke`/`ui-tests` gates would regress. Updated
  them to reference the seeded samples programmatically — the same
  "no repeated literals" rule the slice mandates for `seedTrees`.
- **Case/type name collision on qualified access.** `MaterialLibrary.MaterialId.tryCreate`
  resolves to the union CASE constructor (which has no members), not the type —
  exactly the collision `Library.elementId` already documents for `ElementId`.
  Added module-level `tryMaterialId` / `newMaterialId` / `newSampleId` helpers
  for qualified callers; unqualified `MaterialId.tryCreate` (with the module
  opened) resolves fine and is used inside Storage.
- **`Schematic.colorForMaterial` keeps a `string` key.** Its only production
  caller (`Schematic.layout` via `ResultsView.materialKey`) supplies SYNTHETIC
  per-layer keys ("layer-0" …), not library ids — an engine `Layer` carries no
  material id. The curated map is re-keyed by the built-ins' Guid STRING forms
  so no legacy id string survives; forcing `MaterialId` there would have made
  ResultsView mint fake Guids per layer index.
- **Source/detector/polarizer preset ids stay strings.** The slice elevates
  material and sample ids only; `entryId` still returns those presets' string
  ids unchanged. The acceptance ("no domain record carries a raw string
  material/sample id") holds.
- **Old persisted library JSON with non-Guid ids now fails import** with a
  typed `JsonParseError` naming the bad id — intentional per "no legacy
  string-id path, no fallback, no migration". The `materialEntry` schema is
  untouched (its `id` is already `type: string`; the Guid string form
  satisfies it — the AC-I10 export-validates test still passes).
- **StackEditTests.fs's worktree copy was CRLF before this round** (index LF —
  same pre-existing situation slice 001 hit with the schema JSON); normalized
  to LF. No CRLF churn: `git diff --numstat` identical with/without
  `--ignore-cr-at-eol`. (`specs/0033/.manifest.state.json` also warns CRLF but
  is the arc-runner's own file — left alone.)
- Operator note: the project prompt's "Operator note" section is present but
  empty — no operator constraints in flight this attempt.
