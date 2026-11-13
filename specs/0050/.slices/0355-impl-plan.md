# Step 0355 — impl plan

## Approach

Add the crystal-parameter case to a material's physics (spec 0050 S11.1, .spec-md §3.16), then give every shared
member and every complete payload match its crystal arm.

1. **`CrystalPhysics.fs`**: after the three records, `CrystalParameters = UniaxialCrystal | TriclinicCrystal |
   AbsorbingTriclinicCrystal`, with `opticalProperties` (each case's record builder) and `toProperties`
   (`this.opticalProperties.dispersive`, so the same tensors at every wavelength). It needs `open Berreman.Dispersion`.
2. **`MaterialLibrary.fs`**: `MaterialPhysics` gains `CrystalParameterPhysics of crystal : CrystalParameters`, and
   `toProperties` answers `crystal.toProperties`. It needs `open OpticalConstructor.Domain.CrystalPhysics`.
3. **Shared members**:
   - `LibraryFacets`: `CrystalParameters.anisotropy` / `.transparency`, `MaterialPhysics.anisotropy` crystal arm;
     the private value-tree readers answer `None`; the transparency facet extracts `crystal.transparency`.
   - `MaterialComplexityEditor`: `editableComplexityOpt` answers `None`.
   - `NkDispersionChart`: `gyrationComponents` answers `allGyrationComponents`, `muKind` `ScalarMuKind`.
   - `OutOfBandDiagnostic`: `ReachableMaterial.ofEntry` answers no defined segment.
4. **Entry files**: `LibraryEntryPayload.CrystalMaterialPayload of CrystalParameters` (kind `MaterialEntryKind`);
   `ofMaterialEntry` writes it for a crystal; `toMaterialEntry` reads it; `references` treats it like
   `MaterialPayload`; `toItemModel`'s complete match gains it. The schema's payload becomes a `oneOf` of the two
   tags, the existing `MaterialPayload` branch unchanged, so every existing file keeps its bytes.
5. **Tests**: one shared set of three crystal literals, then the eight constructor facts and one UI fact.

## Files

Domain: `CrystalPhysics.fs`, `MaterialLibrary.fs`, `LibraryFacets.fs`, `MaterialComplexityEditor.fs`,
`OutOfBandDiagnostic.fs`, `LibraryEntryFiles.fs`. Storage: `schema/library-material.schema.json`. Ui:
`NkDispersionChart.fs`. Tests: `MaterialComplexityTests.fs`, `LibraryFacetsTests.fs`, `OutOfBandDiagnosticTests.fs`,
`LibraryEntryFilesTests.fs`, `EntryFileJsonTests.fs`, `MaterialProxyTests.fs`, `MaterialEditorWindowTests.fs`.

## Risks

- **Shared test literals.** Six constructor test files need the crystals. To avoid re-transcribing the literals,
  one module of fixtures is declared in `MaterialProxyTests.fs`, the first of those files in compile order. The
  UI test project cannot see it, so it writes its one uniaxial literal itself.
- **`previewProperties` is `internal`** to `OpticalConstructor.Ui`, and the brief forbids view changes. The UI
  fact therefore reads the shown physics' `toProperties`, which is exactly what `previewProperties` answers in
  view-only mode (`MaterialEditorView.fs:253`).
- **ELEV**:
  - `CrystalParameters` is a new type name, and its three case names are new and two-word.
  - The extension members are on our types.
  - Nothing returns a bool, and no `.value` is added.
- **The serializer**: `JsonFSharpConverter` writes the new union in adjacent-tag form. The SQLite store validates
  against the schema, so the schema must accept the crystal tag.
