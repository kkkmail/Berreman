# Step 0354 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice, `0354.gates`, the bound rulings (manual 005, 012 and 013) and
      every file that reads `MaterialEntry.complexity`.
- [x] Domain: `MaterialPhysics`, `MaterialEntry.physics`, and the shared members and readers.
- [x] Storage: `MaterialImport` and `Report`.
- [x] Ui: `MaterialEditorView`, `MaterialsWindowView`, `NkDispersionChart` and `MaterialEditorWindow`.
- [x] Test fixtures and the two new facts.
- [x] Development runs (see Testing state).

## Files modified

### Product

- **`OpticalConstructor.Domain/MaterialLibrary.fs`**
  - `MaterialPhysics = ComplexityPhysics of complexity : MaterialComplexity`, declared directly after
    `MaterialComplexity`.
  - `member this.toProperties` on `MaterialPhysics` answers the complexity's `toProperties`.
  - `MaterialEntry.complexity : MaterialComplexity` is replaced by `physics : MaterialPhysics`, and
    `properties = this.physics.toProperties`.
  - The docs say the physics is the entry's data and `properties` is its engine composition (spec 0050 S11.1).
- **`OpticalConstructor.Domain/LibraryFacets.fs`**
  - `type MaterialPhysics with member this.anisotropy`, directly after `MaterialComplexity.anisotropy`.
  - The private `constantEpsOpt`, `dispersiveEpsOpt`, `activeOpt` and `magneticOpt` match `entry.physics`
    (`ComplexityPhysics complexity` → the old body).
  - The anisotropy facet extracts `entry.physics.anisotropy`, and its doc names the physics.
  - The transparency facet wraps the old rule in `ComplexityPhysics _`.
- **`OpticalConstructor.Domain/MaterialComplexityEditor.fs`**: `type MaterialPhysics with member
  this.editableComplexityOpt : MaterialComplexity option`, directly after `MaterialComplexity.isEditable`. It returns
  `Some complexity` when `isEditable` holds and `None` otherwise.
- **`OpticalConstructor.Domain/MaterialStore.fs`**: `versionPayload : VersionPayload<MaterialPhysics,
  MaterialMetadata>` with `physics = this.physics`.
- **`OpticalConstructor.Domain/OutOfBandDiagnostic.fs`**: `ReachableMaterial.ofEntry` matches the physics, with
  `ComplexityPhysics complexity` → `definedSegmentsOf complexity`.
- **`OpticalConstructor.Domain/LibraryEntryFiles.fs`**
  - `ofMaterialEntry` files `MaterialPayload` of the physics' complexity, with a one-line doc saying why: the bytes
    are kept.
  - `toMaterialEntry` reads the payload back as `ComplexityPhysics complexity`.
- **`OpticalConstructor.Storage/MaterialImport.fs`** (`entryFromTabulated`, `entryOfAxis`) and **`Report.fs`**
  (`dtoToEntry`) now build `physics = ComplexityPhysics …`.
- **`OpticalConstructor.Ui/MaterialEditorView.fs`**
  - `EditorMode.ViewOnlyMaterial of reason : ControlCaption * shown : MaterialPhysics`.
  - The previews read `shown.toProperties`, `shown.gyrationComponents` and `shown.muKind`.
  - `initMaterialEditor` seeds through `entry.physics.editableComplexityOpt`:
    - `Some complexity` → `ofComplexity`, and its `Error e` gives `ViewOnlyMaterial (e.caption, entry.physics)`;
    - `None` → the old view-only caption with `entry.physics`.
  - `entryUnder` builds `physics = ComplexityPhysics complexity`.
  - The module and type docs name the physics.
- **`OpticalConstructor.Ui/MaterialsWindowView.fs`**
  - `editableSelection` and the view panel's editability match `entry.physics.editableComplexityOpt`
    (`Some _` is editable).
  - The chart reads `shownEntry.physics.anisotropy`. That one `let` feeds both chart call sites: the chart and its
    style.
- **`OpticalConstructor.Ui/NkDispersionChart.fs`**: `type MaterialPhysics with member this.gyrationComponents`
  (`gyrationComponentsOfComplexity` of the complexity) and `member this.muKind` (the complexity's `muKind`). They
  sit directly after `MaterialComplexity.muKind`, which they read.
- **`OpticalConstructor.Ui/MaterialEditorWindow.fs`**: the type doc names the physics.

### Tests (no assertion changes, no test identity changes, except the two declared new facts)

- **Fixture construction** writes `physics = ComplexityPhysics c` in:
  - `CatalogueProxyTests.fs`, `DispersionModelsTests.fs`, `EntryFileJsonTests.fs`, `LibraryFacetsTests.fs` and
    `OutOfBandDiagnosticTests.fs`;
  - `FsxAcceptanceTests.fs`, `OutOfBandBadgeTests.fs` and `MaterialEditorWindowTests.fs` (the published-gyration
    fixture).
- **Copying or comparing two entries' physics** uses `.physics`: `{ x with physics = y.physics }` and
  `Assert.Equal(a.physics, b.physics)`. This applies in `MaterialProxyTests.fs`, `SampleProxyTests.fs`,
  `CatalogueImportInMemoryTests.fs`, `KitLibraryTests.fs`, `MaterialsWindowTests.fs` and two places in
  `MaterialEditorWindowTests.fs` (see Decisions).
- **Reading the complexity itself** goes through `match … with ComplexityPhysics complexity` in:
  - `CanonicalJsonTests.fs`, `ExportImportTests.fs`, `LibraryFacetsTests.fs` (a local `anisotropyOf` inside the
    fact) and `MaterialComplexityTests.fs`;
  - four sites in `MaterialImportTests.fs`, where record patterns become `ComplexityPhysics { eps = … }`;
  - four sites in `MaterialEditorWindowTests.fs`.
- **`MaterialEditorWindowTests`' `init seeds a new material, an existing editable entry, and a view-only entry`**:
  the view-only case now asserts `Assert.Equal<MaterialPhysics>(siliconEntry.physics, shown)` (the declared
  `change_expectation`). Its other assertions are unchanged.
- **New fact `MaterialComplexityTests.a complexity physics composes, classifies and offers its complexity as the
  complexity does`**. It covers a constant isotropic complexity and a uniaxial dispersive one, built through the
  editor ladder (`ChooseAnisotropy Uniaxial; SetDispersion DispersiveSegments`). For each, `ComplexityPhysics`:
  - composes `toProperties.getProperties` at 600 nm equal to the complexity's (the file's own `propertiesClose` at
    tolerance 0);
  - answers `anisotropy` equal to the complexity's (Isotropic and Uniaxial respectively);
  - answers `editableComplexityOpt = Some` of it.

  A complexity with `PublishedLawRho LangasiteGyrationLaw` answers `None`. The file opens
  `OpticalConstructor.Domain.LibraryFacets` for `MaterialPhysics.anisotropy`.
- **New fact `NkDispersionChartTests.a complexity physics charts the gyration components and the mu kind of its
  complexity`**. A uniaxial-active complexity, made magnetic so its μ kind differs from the no-magnetic fallback,
  answers:
  - `gyrationComponents = gyrationComponentsOfComplexity` of it (`[G11; G33]`);
  - `muKind` = its `muKind` (`ScalarMuKind`).
- **Comments** that named the entry's `complexity` field now name its physics: `LibraryFacetsTests.fs`,
  `MaterialImportTests.fs`, the `MaterialComplexityTests.fs` module doc, the `MaterialEditorWindowTests.fs` module
  doc and two comments in it, and two in `MaterialsWindowTests.fs`. Test names are unchanged.

## Decisions

- **Three test files outside the effects list are edited:** `OpticalConstructor.Tests/CanonicalJsonTests.fs`,
  `CatalogueProxyTests.fs` and `KitLibraryTests.fs`.
  - Each one builds or reads `MaterialEntry.complexity`, so the build fails without the change.
  - The brief's rule (4), "Every test fixture that builds a `MaterialEntry` … every test read of `.complexity`",
    covers them; its file enumeration missed them.
  - The same mechanical fixture repair is the whole change. No assertion or test identity changes.
  - Precedent: steps 0259, 0291, 0313 and 0353.
- **Entry-to-entry comparisons and copies use `.physics` rather than a match.** Examples:
  `Assert.Equal(glass.physics, e.physics)` and `{ glass with physics = other.physics }`.
  - With one case, `physics` equality is exactly `complexity` equality, so each assertion means what it meant.
  - A match there would only unwrap two values to compare them again.
  - Reads that need the complexity itself (its `eps`, `toProperties`, `anisotropy`, or `ofComplexity`) go through a
    `ComplexityPhysics` match, as the brief asks.
- **`editableSelection` uses a nested match** rather than `Option.isSome` or `Option.filter`, under the
  pattern-match rule.
- **The `MaterialPhysics` extensions in `NkDispersionChart.fs` sit after `MaterialComplexity.muKind`, not between
  it and `gyrationComponentsOfComplexity`.** F# needs `MaterialComplexity.muKind` declared before
  `MaterialPhysics.muKind` reads it, so one extension block after both is the closest "beside" both.
- **Entry files keep their bytes and hashes.** `ofMaterialEntry` still writes `MaterialPayload complexity` and
  `toMaterialEntry` reads the same payload. Nothing in the serialized contract changes.
- **Manual 005 (step 0354 clause):** the retired parity class is not recreated, and the byte/hash requirement is
  held by `BuiltInCatalogueIntegrityTests`, unchanged.
- **Manual 012 (step 0354 clause):** `Report.fs` keeps the structural-schema / caller-membership split. The only
  edit is `physics = ComplexityPhysics vacuumPlaceholderComplexity` in `dtoToEntry`. `ExportImportTests`'
  assertion is the same comparison, read through the match.

## Testing state

These are development runs of each gate's own command from `0354.gates`, over the final tree, with logs under
`.artifacts/`. They are evidence only; the arc-runner's gate engine runs the gates.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, `0 Error(s)`, 28 s (incremental).
  - Warnings are only NU1902 (exempt).
  - The earlier product-only build of `OpticalConstructor.Ui.fsproj` also showed the two known SYSLIB0051 warnings
    in vendored MathNet `Exceptions.cs`.
  - No FS or MSB warning comes from our code, and there is no CRLF churn
    (`git diff --numstat` equals `--ignore-cr-at-eol`).
- **elevation** (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, standalone, no base, so all 13 projects):
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 225s`. Known findings 0; suppressed 1509, the same as step 0353.
- **constructor-unit-tests**: `Test run summary: Passed!`, 1560 total / 1560 succeeded / 0 failed / 0 skipped, 72 s.
  - Checkpoint 352 recorded 1559, so the count rose by exactly 1.
  - The TRX shows `MaterialComplexityTests.a complexity physics composes, classifies and offers its complexity as the
    complexity does` Passed.
- **ui-smoke**: Passed!, 258 / 258, 233 s. Checkpoint 346 recorded 258; this step adds no smoke test.
- **ui-tests** (six selected classes, the snapshot's selector): Passed!, 107 / 107, 72 s.
  - By class: FsxAcceptance 14, MaterialEditorWindow 29, MaterialsControls 3, MaterialsWindow 45,
    NkDispersionChart 14, OutOfBandBadge 2.
  - `NkDispersionChartTests.a complexity physics charts the gyration components and the mu kind of its complexity`
    and the changed `MaterialEditorWindowTests.init seeds a new material, an existing editable entry, and a view-only
    entry` both Passed in the TRX.
  - The count rises by 1, the one new identity; none was removed.
- **invariant-s36-\* (20)**: each was run through `python -m ai_strategy_generator.step_compiler.section_invariants
  --find-nothing <snapshot argument>` and found nothing.
- **Extra check, not a gate:** the whole non-smoke UI suite (`--filter-not-trait` ui-smoke and kit-acceptance, no
  selector) passed 724 / 724 in 236 s. Its TRX was written under a separate name and then deleted, so the gate's
  `ui-tests.trx` is untouched.
- **Deadlines:** no development run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0354-dev-build-ui.log` — the UI project build (product compile check).
- `specs/0050/.artifacts/0354-dev-build.log` — the solution build.
- `specs/0050/.artifacts/0354-dev-constructor-unit-tests.log` — the constructor suite.
- `specs/0050/.artifacts/0354-dev-elevation.log` — the elevation run.
- `specs/0050/.artifacts/0354-dev-ui-smoke.log`, `0354-dev-ui-tests.log` — the UI gates.
- `specs/0050/.artifacts/0354-dev-ui-tests-whole.log` — the extra whole non-smoke UI run.
