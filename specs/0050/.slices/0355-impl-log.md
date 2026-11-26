# Step 0355 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, `0355.gates`, and the 0353/0354 records.
- [x] Domain: `CrystalParameters`, `CrystalParameterPhysics`, and the shared members.
- [x] Entry files and schema.
- [x] Ui: `NkDispersionChart`.
- [x] Tests: eight constructor facts and one UI fact, plus the crystal arm in every test match over the physics.
- [x] Development runs (see Testing state).

## Files modified

### Product

- **`OpticalConstructor.Domain/CrystalPhysics.fs`**
  - Added `CrystalParameters = UniaxialCrystal of UniaxialParameters | TriclinicCrystal of TriclinicParameters |
    AbsorbingTriclinicCrystal of AbsorbingTriclinicParameters`, declared after the three records.
  - `member this.opticalProperties : OpticalProperties` answers each case's record builder.
  - `member this.toProperties : OpticalPropertiesWithDisp` is `this.opticalProperties.dispersive`, so it gives the
    same tensors at every wavelength.
  - Opens `Berreman.Dispersion` for `OpticalProperties.dispersive`.
  - No int tag: the serializer stores the case form, as for `SlotTurn`.
- **`OpticalConstructor.Domain/MaterialLibrary.fs`**: `MaterialPhysics` gains `CrystalParameterPhysics of crystal :
  CrystalParameters`, and `toProperties` answers `crystal.toProperties`. Opens `CrystalPhysics`.
- **`OpticalConstructor.Domain/LibraryFacets.fs`**
  - Added `type CrystalParameters with member this.anisotropy` (`Uniaxial` for the uniaxial case, `Biaxial` for
    both triclinic cases) and `member this.transparency` (`Absorbing` only for `AbsorbingTriclinicCrystal`).
  - `MaterialPhysics.anisotropy` answers the crystal's anisotropy.
  - The private `constantEpsOpt`, `dispersiveEpsOpt`, `activeOpt` and `magneticOpt` answer `None` for a crystal.
    As a result, the dispersion-model, gyration-class, handedness and magnetic facets do not apply to it.
  - The transparency facet extracts `crystal.transparency`.
  - The dispersion facet is unchanged: it reads `MaterialEntry.dispersion`, which is `ConstantMaterial` for a
    crystal.
- **`OpticalConstructor.Domain/MaterialComplexityEditor.fs`**: `editableComplexityOpt` answers `None` for a crystal.
- **`OpticalConstructor.Domain/OutOfBandDiagnostic.fs`**: `ReachableMaterial.ofEntry` answers no defined segment for
  a crystal.
- **`OpticalConstructor.Domain/LibraryEntryFiles.fs`**
  - `LibraryEntryPayload` gains `CrystalMaterialPayload of CrystalParameters`, and its `kind` is
    `MaterialEntryKind`.
  - `references` treats it as `MaterialPayload`: the category at its latest.
  - `ofMaterialEntry` writes it for a crystal.
  - `toMaterialEntry` reads either material payload through one `physicsOpt` match, then builds the entry once.
    That avoids a second copy of the record construction.
  - `toItemModel`'s complete match gains the case. The other decoders already answer `EntryKindMismatchErr`
    through their wildcard.
- **`OpticalConstructor.Storage/schema/library-material.schema.json`**
  - Under version 1, `payload` is a `oneOf` of two branches:
    - the existing `MaterialPayload` branch, unchanged;
    - a `CrystalMaterialPayload` branch whose single field is `"type": "object"`, so the serializer binds the
      record and a malformed record is `MalformedJsonErr`.
  - The schema version stays 1. The header description names both forms.
- **`OpticalConstructor.Ui/NkDispersionChart.fs`**: `MaterialPhysics.gyrationComponents` answers
  `allGyrationComponents` and `muKind` answers `ScalarMuKind` for a crystal.
- **No change to `MaterialEditorView.fs` or `MaterialsWindowView.fs`.**
  - A crystal entry opens `ViewOnlyMaterial` with step 0354's caption, through `editableComplexityOpt = None`.
  - Its preview charts `shown.toProperties`.
  - The Materials window offers no Edit for it, through the same option.

### Tests

- **`MaterialProxyTests.fs`**
  - New public module `CrystalMaterialFixtures`, declared at the top. It holds `uniaxialParameters` (n_o 1.544,
    n_e 1.553, g11 3.0e-5, g33 −1.0e-5), `triclinicParameters`, `absorbingTriclinicParameters` (fifteen distinct
    values) and `crystals` (the three cases in order).
  - New `MaterialProxyContract` fact `a crystal-parameter material is stored and resolved at its version by its
    physics`, so it runs under both `MaterialProxyTests` and `MaterialProxySqliteTests`. A minted absorbing crystal
    is saved, resolves at v1 to an equal entry, and is listed by `listMaterials ActiveOnly`.
- **`MaterialComplexityTests.fs`**: two new facts.
  - `a crystal-parameter material composes its record's engine properties at every wavelength`: for each crystal,
    at 400, 600 and 800 nm, the result equals its record's `opticalProperties` within `propertiesClose 1e-15`, and
    `dispersion = ConstantMaterial`.
  - `a crystal-parameter physics offers no editable complexity, so its entry opens view-only`.
  - The step-0353 fact `each crystal record's builder composes its permittivity from its own fields` now binds
    the shared records instead of its own literals. See Decisions.
- **`LibraryFacetsTests.fs`**
  - `materialEntryWith` now delegates to a new `physicsEntryWith`.
  - New fact `a crystal-parameter material classifies its anisotropy and transparency from its crystal, and the
    ladder's facets do not apply`. It checks:
    - the member values;
    - the facet keys: anisotropy, transparency, and the `Constant` dispersion;
    - that the four ladder facets are inapplicable and extract nothing.
- **`OutOfBandDiagnosticTests.fs`**
  - `entryOf` now delegates to a new `physicsEntryOf`.
  - New fact `a crystal-parameter material never flags out of band`: no defined segment, and a 300–900 nm sweep is
    `InBand`.
- **`LibraryEntryFilesTests.fs`**: new fact `a crystal-parameter material's builder and decoder are inverse`. It
  checks the schema, `CrystalMaterialPayload crystal`, the category reference, `toMaterialEntry = Ok entry`, and
  `toSample` = `EntryKindMismatchErr`.
- **`EntryFileJsonTests.fs`**
  - `materialWith` now delegates to a new `physicsMaterialWith`.
  - New fact `a crystal-parameter material entry file round-trips each crystal record`. It runs `sealed`,
    `fileText` (the text names `CrystalMaterialPayload`) and `parseEntry MaterialEntryKind`, giving an equal
    envelope whose decoder answers the entry.
  - The same fact also checks that a crystal record with an unknown case is `MalformedJsonErr`, which proves the
    schema leaves the record to the serializer.
- **`MaterialEditorWindowTests.fs`** (Ui.Tests): new fact `a crystal-parameter entry opens the editor view-only and
  previews its crystal's properties`.
  - It uses a uniaxial literal and asserts `ViewOnlyMaterial` with step 0354's caption, the shown physics equal to
    the entry's, and the previewed properties at 600 nm equal to the crystal's `opticalProperties` (the file's
    `close`).
  - It also asserts `previewGyrationComponents = allGyrationComponents` and `previewMuKind = ScalarMuKind`.
- **The crystal arm in existing test matches.** With a second physics case, every single-arm `match … with
  ComplexityPhysics c` in the tests became incomplete, which is FS0025, an error. Each one gained a
  `CrystalParameterPhysics` arm:
  - Most arms fail with "expected a complexity physics", in `CanonicalJsonTests.fs`, `ExportImportTests.fs`,
    `MaterialComplexityTests.fs`, two sites in `MaterialImportTests.fs` and four sites in
    `MaterialEditorWindowTests.fs`.
  - The `anisotropyOf` helper in `LibraryFacetsTests.fs` answers `crystal.anisotropy` instead.

  No assertion was removed or changed.

## Decisions

- **One shared set of crystal literals, in `MaterialProxyTests.fs`.**
  - Six constructor test files need the crystals. Writing the literals in each file would re-transcribe the same
    constants six times (AGENTS.md "Reuse, never copy").
  - `MaterialProxyTests.fs` is the earliest of those files in `OpticalConstructor.Tests.fsproj`'s compile order, and
    it is in the effects list. A new fixture file would need an unlisted `.fsproj` edit.
  - The records are still test-local literals, as the brief asks.
  - The UI test project does not compile `MaterialProxyTests.fs`, so its fact writes the one uniaxial literal itself.
- **The step-0353 fact now reads the shared records.** Its triclinic and absorbing literals were exactly the values
  the shared fixtures now hold. Its uniaxial literal differed only in g11/g33, which that fact never reads: it
  asserts ε from the indices, and those are identical (1.544 / 1.553). Every assertion is unchanged.
- **The UI fact reads the preview through the shown physics, not through `previewProperties`.**
  - `MaterialEditorModel.previewProperties` is `member internal` in `OpticalConstructor.Ui`, and Ui.Tests has no
    `InternalsVisibleTo`. The brief also says the views do not change, and `MaterialEditorView.fs` is not in the
    effects list.
  - In view-only mode `previewProperties` is defined as `Some shown.toProperties` (`MaterialEditorView.fs:253`).
    The fact matches `ViewOnlyMaterial (reason, shown)` and asserts `shown.toProperties.getProperties 600 nm`
    against the crystal's `opticalProperties`. That is the same value, read through the public mode.
- **`toMaterialEntry` uses one `physicsOpt` match**, with every non-material payload named, rather than a second
  copy of the entry record for the crystal arm.
- **The schema uses `oneOf`.**
  - The `MaterialPayload` branch is byte-for-byte the old payload schema, so every existing file validates as
    before.
  - The crystal branch constrains only the adjacent-tag shape, so binding errors stay `MalformedJsonErr`, as the
    brief asks. A schema `enum` on the crystal's case would have made them `SchemaValidationErr` instead.

## Testing state

These are development runs of each gate's own command from `0355.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, `0 Error(s)`, 23 s (incremental).
  - The only warnings are NU1902, which is exempt: 12 lines, 6 distinct.
  - No FS or MSB warning comes from our code. The FS3369 warnings from the first draft of the malformed-record edit
    were fixed by using `.[ ]` indexing.
  - No CRLF churn: `git diff --numstat` equals `--ignore-cr-at-eol`.
- **constructor-unit-tests** (whole suite): `Test run summary: Passed!`, 1568 / 1568, 0 failed, 0 skipped, 71 s.
  - Checkpoint 353 recorded 1560, so the count rose by 8, as the acceptance requires.
  - The TRX shows all eight new facts Passed, both `MaterialProxyTests` and `MaterialProxySqliteTests` included.
- **ui-tests** (the three selected classes, the snapshot's selector): Passed!, 89 / 89, 9 s.
  - By class: MaterialEditorWindow 30, MaterialsWindow 45, NkDispersionChart 14.
  - Before this step the three classes held 88 (29 + 45 + 14), so the count rose by 1.
  - The new fact Passed in the TRX.
- **ui-smoke** (`--filter-trait "Category=ui-smoke"`): Passed!, 258 / 258, 232 s. Checkpoint 353 recorded 258;
  this step adds no smoke test.
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, standalone, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 220s`. Known findings 0; suppressed 1509, the same as step
  0354, so no new `.value` read was added either.
- No run went past its effective deadline: build 600 s, elevation 600 s, constructor-unit-tests 180 s, ui-smoke
  600 s, ui-tests 360 s.
- **invariant-s36-\* (20)**: each snapshot command, decoded and run both as written and with `--untracked`, found
  nothing.

## Artifacts

- `specs/0050/.artifacts/0355-build-dev.log`
- `specs/0050/.artifacts/0355-constructor-unit-tests-dev.log`
- `specs/0050/.artifacts/0355-ui-tests-dev.log`
- `specs/0050/.artifacts/0355-ui-smoke-dev.log`
- `specs/0050/.artifacts/0355-elevation-dev.log`
