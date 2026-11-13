# Step 0354 — impl plan

## Approach

Widen a material's physics from "its complexity" to a one-case union, `MaterialPhysics = ComplexityPhysics of
complexity : MaterialComplexity` (spec 0050 S11.1). The next step adds the crystal case beside it. Behaviour does not
change. The record field's type change ripples into every reader at once, so this is one step.

1. **`MaterialLibrary.fs`.** Declare `MaterialPhysics` directly after `MaterialComplexity`, with `toProperties`. Then
   `MaterialEntry.complexity` becomes `physics : MaterialPhysics`, and `properties` becomes
   `this.physics.toProperties`.
2. **Shared members, one arm each:**
   - `MaterialPhysics.anisotropy` goes in `LibraryFacets.fs`;
   - `MaterialPhysics.editableComplexityOpt` goes in `MaterialComplexityEditor.fs`;
   - `MaterialPhysics.gyrationComponents` and `.muKind` go in `NkDispersionChart.fs`.
3. **Readers:**
   - `MaterialStore.versionPayload`, typed over `MaterialPhysics`;
   - in `LibraryFacets`, the four private readers, the anisotropy facet and the transparency facet;
   - `OutOfBandDiagnostic.ofEntry`;
   - `LibraryEntryFiles.ofMaterialEntry` and `toMaterialEntry`; the payload stays `MaterialPayload complexity`, so
     bytes and hashes do not move;
   - `MaterialImport`'s two builders and `Report.dtoToEntry`;
   - `MaterialEditorView`: `EditorMode.ViewOnlyMaterial` carries `shown : MaterialPhysics`, the three previews read
     the physics' members, `initMaterialEditor` seeds through `editableComplexityOpt`, and `entryUnder` builds
     `ComplexityPhysics`;
   - `MaterialsWindowView`: editability reads `editableComplexityOpt`, and the chart reads `physics.anisotropy`;
   - `MaterialEditorWindow`: the module doc names the physics.
4. **Fixtures.** Every test that builds a `MaterialEntry` writes `physics = ComplexityPhysics c`. Every read of the
   complexity goes through a match on the physics. Where a test only copies or compares two entries' physics, it
   copies or compares `.physics`, which is the same value. No assertion changes.
5. **Tests.** Add two facts: `MaterialComplexityTests` `a complexity physics composes, classifies and offers its
   complexity as the complexity does`, and `NkDispersionChartTests` `a complexity physics charts the gyration
   components and the mu kind of its complexity`. `MaterialEditorWindowTests`' view-only init fact reads the shown
   physics.

## Files

The effects list, plus three test files the compiler needs that the list misses:
`OpticalConstructor.Tests/CanonicalJsonTests.fs`, `CatalogueProxyTests.fs` and `KitLibraryTests.fs`. Each reads or
builds `MaterialEntry.complexity`, and each gets the same mechanical fixture repair under rule (4) of the brief.
Steps 0259, 0291, 0313 and 0353 set the precedent.

## Risks

- **Invariant `s36-complexity-mandatory`** (`complexity = (None|Some)` must find nothing). No line I write may read
  `complexity = Some` or `complexity = None`.
- **Bytes and hashes of entry files.** The payload stays `MaterialPayload` of the complexity, so the
  `BuiltInCatalogueIntegrityTests` hashes do not change.
- **Extension visibility.** `MaterialPhysics.gyrationComponents` and `.muKind` live in the `NkDispersionChart` module.
  `MaterialEditorView` and the UI tests must open it, and `MaterialEditorView` already does.
- **ELEV.** New names must be unique: `MaterialPhysics` (ELEV014), `ComplexityPhysics` (ELEV016), and the extension
  members on `MaterialPhysics` (ELEV019).
