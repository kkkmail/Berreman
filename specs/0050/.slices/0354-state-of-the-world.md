# Step 0354 — state of the world

## Where we are

This is the second step of spec 0050 S11.1, material physics forms.
- Step 0353 moved the three crystal-parameter records ahead of `MaterialLibrary.fs`.
- This step widens a material's physics from "its complexity" (.spec-md §3.2) to a union,
  `MaterialPhysics = ComplexityPhysics of complexity : MaterialComplexity`. It has one case for now.
- `MaterialEntry` holds `physics : MaterialPhysics`, and `properties` is the physics' `toProperties`.
- Every reader now goes through the physics, so the next step can add the crystal case beside `ComplexityPhysics`
  without touching every reader again.
- Behaviour, stored bytes and hashes are unchanged.

## What's working

- Added `MaterialPhysics` with the single `ComplexityPhysics` case. `MaterialEntry.complexity` became
  `physics : MaterialPhysics`, with `properties = physics.toProperties` (spec 0050 S11.1).
- Added the shared physics members, one arm each: `anisotropy` (LibraryFacets), `editableComplexityOpt`
  (MaterialComplexityEditor), and `gyrationComponents` / `muKind` (NkDispersionChart).
- Moved the readers to the physics: the versioning payload, the facets, the out-of-band diagnostic, the
  entry-file codec (the payload stays the complexity, so the bytes are kept), the importers, and the Material
  editor and Materials window.
- Migrated every test fixture without changing an assertion. Added the two declared facts, and the view-only
  editor fact now reads the shown physics.

## Tests

These are development runs of each gate's own command from `0354.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; warnings only NU1902 (exempt) and, in the product-only build, the vendored MathNet SYSLIB0051 | 28 s (incremental) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1509 (ELEV004 excluded, the same as step 0353) | 225 s | 600 s |
| constructor-unit-tests | Passed!, 1560 of 1560; checkpoint 352 recorded 1559, so +1 (the new `MaterialComplexityTests` fact, Passed in the TRX) | 72 s | 180 s |
| ui-smoke | Passed!, 258 of 258 (checkpoint 346 recorded 258) | 233 s | 600 s |
| ui-tests (six selected classes) | Passed!, 107 of 107: FsxAcceptance 14, MaterialEditorWindow 29, MaterialsControls 3, MaterialsWindow 45, NkDispersionChart 14, OutOfBandBadge 2. +1 is the new NkDispersionChart fact; the changed `init seeds …` fact passed | 72 s | 360 s |
| invariant-s36-\* (20) | each one, run through ASG's `section_invariants --find-nothing` with the snapshot's own argument, found nothing | under 1 s each | 300 s |

- No run went past its effective deadline.
- The elevation run had no comparison base, so it analysed all 13 projects. That is wider than the gate's arc-origin
  scope.
- Entry files keep their bytes and hashes. `BuiltInCatalogueIntegrityTests`, `EntryFileJsonTests`,
  `LibraryEntryFilesTests` and the catalogue import tests pass unchanged within the 1560.
- An extra development run of the whole non-smoke UI suite is recorded in the impl-log's Testing state.

```yaml
gates:
  constructor_unit_tests: 1560
  ui_smoke_tests: 258
  ui_tests: 107
```

## Architecture

- **`MaterialPhysics` lives in `MaterialLibrary.fs`, directly after `MaterialComplexity`.** Its one member there is
  `toProperties`.
- **Readers that sit in later modules get their own extension members:**
  - `MaterialPhysics.anisotropy` in `LibraryFacets`;
  - `.editableComplexityOpt` in `MaterialComplexityEditor`;
  - `.gyrationComponents` and `.muKind` in `Ui/NkDispersionChart`.

  Each has one arm today. The crystal case adds one arm to each, and to the private facet readers and
  `ReachableMaterial.ofEntry`.
- **`editableComplexityOpt` is an option, not a flag (ELEV012).** It carries the complexity the editor seeds from.
  `initMaterialEditor`, `editableSelection` and the view panel all gate on it.
- **`EditorMode.ViewOnlyMaterial` carries `shown : MaterialPhysics`.** The view-only previews read the physics'
  members.
- **The versioning payload is `VersionPayload<MaterialPhysics, MaterialMetadata>`.** The shared
  `decideVersioning` rule compares the physics structurally, as it compared the complexity before.
- **The serialized contract is unchanged.** `LibraryEntryContent.ofMaterialEntry` files `MaterialPayload` of the
  physics' complexity, and `toMaterialEntry` reads it back as `ComplexityPhysics`.
- **Contract changes:**
  - Created: `MaterialLibrary.MaterialPhysics`, `MaterialEntry.physics`, `MaterialPhysics.anisotropy`,
    `.editableComplexityOpt`, `.gyrationComponents` and `.muKind`.
  - Retired: `MaterialEntry.complexity`.
  - Changed shape: `MaterialEditorView.EditorMode`.

## Deferred

- Nothing. Every part of the brief, (1) to (5), is in place.

## Gotchas

- **The effects list missed three test files the build needs:** `OpticalConstructor.Tests/CanonicalJsonTests.fs`,
  `CatalogueProxyTests.fs` and `KitLibraryTests.fs`.
  - Each builds or reads `MaterialEntry.complexity`.
  - They got the same mechanical fixture repair under rule (4) of the brief, as steps 0259, 0291, 0313 and 0353 did
    for their unlisted fixtures.
  - No assertion or test identity changed.
- **Entry-to-entry comparisons and copies read `.physics`** (`Assert.Equal(a.physics, b.physics)`,
  `{ x with physics = y.physics }`). Reads of the complexity itself go through a `ComplexityPhysics` match. With one
  case the two are the same assertion; when the crystal case lands, only the matches need a second arm.
- **`MaterialPhysics.gyrationComponents` / `.muKind` are extensions in `OpticalConstructor.Ui.NkDispersionChart`.** A
  file must open that module to use them. `MaterialEditorView` and `NkDispersionChartTests` already do.
- **`MaterialComplexityTests` now opens `OpticalConstructor.Domain.LibraryFacets`** for `MaterialPhysics.anisotropy`.
- **Invariant `s36-complexity-mandatory`** forbids `complexity = None` and `complexity = Some`. The new code writes
  `Some complexity` and `physics = ComplexityPhysics …`, which it allows.

## Changelog

- 2026-11-13 — Step 0354 (spec 0050 S11.1): `MaterialEntry.complexity` became `physics : MaterialPhysics`, a one-case
  union (`ComplexityPhysics`) with `toProperties`, `anisotropy`, `editableComplexityOpt`, `gyrationComponents` and
  `muKind`. Every reader and fixture moved to it, and entry-file bytes are unchanged. Two facts were added
  (constructor 1560, selected UI 107), and the view-only init fact reads the shown physics. Development runs: build,
  elevation, constructor-unit-tests, ui-smoke, ui-tests (six selected classes) and the 20 s36 invariants.
