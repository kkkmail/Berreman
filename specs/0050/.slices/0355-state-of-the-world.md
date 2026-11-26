# Step 0355 — state of the world

## Where we are

This is the third step of spec 0050 S11.1, material physics forms.
- Step 0353 moved the three crystal-parameter records ahead of the material library.
- Step 0354 made a material's physics a one-case union.
- This step adds the second case, `CrystalParameterPhysics of crystal : CrystalParameters`, over the new
  `CrystalParameters` union of the three records (.spec-md §3.16).
- A crystal material composes its record's builder at every wavelength, classifies by its crystal, never flags out
  of band, opens the editor view-only, and is filed as `CrystalMaterialPayload`. Every existing material keeps
  `MaterialPayload` and its bytes.
- Promote to library (S11.5) and the import/drop paths (S3.1–S3.2) reach the library through this entry file. No
  crystal-parameter editor is added.

## What's working

- Added `CrystalParameters` (`UniaxialCrystal` / `TriclinicCrystal` / `AbsorbingTriclinicCrystal`), with
  `opticalProperties` and `toProperties` (the same tensors at every wavelength), and the
  `MaterialPhysics.CrystalParameterPhysics` case (spec 0050 S11.1).
- A crystal classifies by its own anisotropy and transparency, the ladder's value-tree facets do not apply to it, and
  it never flags out of band.
- A crystal entry opens the Material editor view-only, previewing every gyration component and a scalar μ.
- Crystal materials are filed as `CrystalMaterialPayload`, which the version-1 material schema accepts beside the
  unchanged `MaterialPayload`, so existing files keep their bytes.
- Added eight constructor facts and one UI fact over one shared set of crystal literals.

## Tests

These are development runs of each gate's own command from `0355.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; warnings only NU1902 (exempt) | 23 s (incremental) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1509 (ELEV004 excluded), the same as step 0354 | 220 s | 600 s |
| constructor-unit-tests | Passed!, 1568 of 1568; checkpoint 353 recorded 1560, so +8 (all eight new facts Passed in the TRX, both stores included) | 71 s | 180 s |
| ui-smoke | Passed!, 258 of 258 (checkpoint 353 recorded 258) | 232 s | 600 s |
| ui-tests (three selected classes) | Passed!, 89 of 89: MaterialEditorWindow 30, MaterialsWindow 45, NkDispersionChart 14. That is +1 over the three classes' 88, and the new fact Passed | 9 s | 360 s |
| invariant-s36-\* (20) | each one, run with the snapshot's own argument (also with `--untracked`), found nothing | under 1 s each | 300 s |

- No run went past its effective deadline.
- The elevation run had no comparison base, so it analysed all 13 projects. That is wider than the gate's arc-origin
  scope.
- Entry files keep their bytes and hashes. `BuiltInCatalogueIntegrityTests`, `EntryFileJsonTests`,
  `LibraryEntryFilesTests` and the catalogue import tests pass unchanged within the 1568.

```yaml
gates:
  constructor_unit_tests: 1568
  ui_smoke_tests: 258
  ui_tests: 89
```

## Architecture

- **`CrystalParameters` lives in `CrystalPhysics.fs`, after the three records.**
  - `opticalProperties` delegates to each record's builder.
  - `toProperties` is `opticalProperties.dispersive`, which gives the engine's non-dispersive `…WithoutDisp`
    cases, so `MaterialEntry.dispersion` is `ConstantMaterial` with no new rule.
- **`MaterialPhysics` now has two cases.** Every shared member has a crystal arm:
  - `toProperties` (MaterialLibrary);
  - `anisotropy` (LibraryFacets);
  - `editableComplexityOpt` (MaterialComplexityEditor), which answers `None`;
  - `gyrationComponents` and `muKind` (Ui/NkDispersionChart);
  - `ReachableMaterial.ofEntry` (OutOfBandDiagnostic);
  - the four private facet readers, which answer `None`.
- **`CrystalParameters.anisotropy` / `.transparency` are extensions in `LibraryFacets`**, beside
  `MaterialComplexity.anisotropy`, because `Anisotropy` and `Transparency` are the editor's DUs, compiled after
  `CrystalPhysics`.
- **The entry file.**
  - `LibraryEntryPayload.CrystalMaterialPayload of CrystalParameters` is a material-kind payload, with the same
    category reference as `MaterialPayload`.
  - The schema's payload is a `oneOf` of the two tags. The crystal record is bound by the serializer, so a malformed
    one is `MalformedJsonErr`.
  - The schema version stays 1, and no stored file changes.
  - The database stores the canonical payload, so no other Storage code changed.
- **Contract changes:**
  - Created: `CrystalPhysics.CrystalParameters`, `LibraryFacets.CrystalParameters.anisotropy` / `.transparency`
    and `LibraryEntryFiles.LibraryEntryPayload.CrystalMaterialPayload`.
  - Changed shape: `MaterialLibrary.MaterialPhysics`, which gains `CrystalParameterPhysics`.

## Deferred

- Nothing. Every part of the brief, (1) to (6), is in place.

## Gotchas

- **Three test files outside the effects list were edited:** `OpticalConstructor.Tests/CanonicalJsonTests.fs`,
  `ExportImportTests.fs` and `MaterialImportTests.fs`.
  - A second physics case makes their single-arm `match … with ComplexityPhysics` reads incomplete. That is FS0025,
    an error in this build.
  - Each gained a `CrystalParameterPhysics` arm that fails the test with "expected a complexity physics". No
    assertion or test identity changed.
  - The same repair was needed at four sites in the listed `MaterialEditorWindowTests.fs`, one in
    `MaterialComplexityTests.fs` and one in `LibraryFacetsTests.fs`. The `anisotropyOf` helper there answers
    `crystal.anisotropy`.
  - Precedent: steps 0259, 0291, 0313, 0353 and 0354.
- **The crystal literals for `OpticalConstructor.Tests` live once, in `CrystalMaterialFixtures` at the top of
  `MaterialProxyTests.fs`.** That file is the first of the six consumers in compile order. Ui.Tests does not compile
  it, so the UI fact writes its own uniaxial literal.
- **`MaterialEditorModel.previewProperties` is `internal`.**
  - The UI fact reads the shown physics' `toProperties` instead, which is exactly what `previewProperties` answers in
    view-only mode (`MaterialEditorView.fs:253`).
  - Making it public would have changed a view the brief keeps unchanged.
- **The step-0353 fact `each crystal record's builder composes its permittivity from its own fields` now binds the
  shared records.** Its own uniaxial literal differed only in g11/g33, which it never reads. All its assertions are
  unchanged.
- **The ui-tests selection has three classes this step**, where step 0354 selected six. Its 89 is not comparable to
  step 0354's 107. The three classes held 88 before this step.
- **Opening `CrystalPhysics` brings `g11` / `g33` record labels into scope.** Every record literal written here
  carries a type annotation, or is bound through an expected type, so it does not resolve to a crystal record.

## Changelog

- 2026-11-13 — Step 0355 (spec 0050 S11.1): material physics gains `CrystalParameterPhysics` over the new
  `CrystalParameters` (uniaxial / triclinic / absorbing triclinic). The physics' shared members, the facets, the
  out-of-band diagnostic, the editor's view-only path and the entry files (`CrystalMaterialPayload`, version-1 schema
  `oneOf`) follow. Eight constructor facts were added (1568) and one UI fact (selected 89). Development runs: build,
  elevation, constructor-unit-tests, ui-smoke, ui-tests and the 20 s36 invariants.
