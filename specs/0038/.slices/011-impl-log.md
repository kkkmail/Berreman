# Step 011 — impl log (IMPLEMENT, attempt 1)

## Progress

- [x] Read task file, worker system prompt (`implement_worker.system-md`),
  project prompt, slice spec, steps 009/010 SoW, and all referenced sources
  (Facets.fs, FacetBuckets.fs, MaterialLibrary.fs, DispersionModels.fs,
  MaterialComplexityEditor.fs, ElementId.fs, engine Dispersion.fs,
  WorkbenchSettings.fs, Units.fs, spec-md Part D, .manual 002/004/006).
- [x] Impl-plan written.
- [x] MaterialLibrary.fs: the private `hasDispersion : MaterialEntry -> bool`
  elevated to the public two-case DU `MaterialDispersion = ConstantMaterial |
  DispersiveMaterial` + `materialDispersion : MaterialEntry ->
  MaterialDispersion`; `byQuery`'s `DispersionFilter` arms rerouted through
  it. No behaviour change (all 559 constructor facts incl. the pre-existing
  MaterialProxy/dispersion-filter suite pass).
- [x] Domain/LibraryFacets.fs: the two concrete facet catalogues as data —
  `materialFacets : AttributeDef<MaterialEntry> list` (8 defs) and
  `libraryFacets : MaterialEntry list -> AttributeDef<LibraryEntry> list`
  (kind + 8 lifted material defs + 4 sample-structural defs), plus the
  public key constants, vocabulary key helpers (`anisotropyKey`,
  `dispersionKey`, `transparencyKey`, `dispersionModelKey`,
  `modelDispersionKey`, `gyrationClassKey`, `handednessKey`, `muKindKey`,
  `entryKindKey`), the value-tree classifiers (`anisotropyOf`,
  `transparencyOf`) and `offeredGyrationClassKeys` (derived from
  `availableGyrationClasses`, never re-derived).
- [x] Domain fsproj: `LibraryFacets.fs` compiled after `FacetBuckets.fs`.
- [x] Tests/LibraryFacetsTests.fs: 32 pure facts over the seeded corpora
  plus targeted synthetic entries; Tests fsproj entry added after
  `FacetBucketsTests.fs`.
- [x] Diagnostic build + all four suites run, logs in `.artifacts/`.
- [x] State-of-the-world written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
  — `hasDispersion` → public `MaterialDispersion` DU + `materialDispersion`;
  `byQuery` arms updated.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
  — NEW: the two facet catalogues (spec 0038 step 011).
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
  — compile entry.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs`
  — NEW: 32 facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
  — compile entry.

## Testing state

Diagnostic verification only — the arc-runner's gate engine is the sole gate
authority (IMPLEMENT Invariant 6); results below are advisory:

- `dotnet build Berreman.slnx -c Release` — 0 errors, **no MSB3277**; the
  only warnings are the step-001-catalogued pre-existing set in untouched
  files (FS1125 SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow,
  SYSLIB0051 vendored MathNet ×2, NU1701 Wolfram.NETLink ×2). Zero warnings
  from the touched projects. Log: `.artifacts/011-diag-build.log`.
- OpticalConstructor.Tests: **559/559** (checkpoint 527, +32 — the whole
  LibraryFacetsTests suite). Log: `.artifacts/011-diag-constructor-unit-tests.log`.
- BerremanTests: **119 passed / 5 pre-existing skips** (== checkpoint).
  Log: `.artifacts/011-diag-unit-tests.log`.
- ui-smoke: **124/124** (== checkpoint). Log: `.artifacts/011-diag-ui-smoke.log`.
- ui-tests: **361/361** (== checkpoint). Log: `.artifacts/011-diag-ui-tests.log`.
- Line endings verified LF (git diff `--numstat` == `--ignore-cr-at-eol`;
  new files carry zero CR bytes).

Nothing deferred; every slice requirement landed this round.

## Artifacts

- `specs/0038/.artifacts/011-diag-build.log`
- `specs/0038/.artifacts/011-diag-constructor-unit-tests.log`
- `specs/0038/.artifacts/011-diag-unit-tests.log`
- `specs/0038/.artifacts/011-diag-ui-smoke.log`
- `specs/0038/.artifacts/011-diag-ui-tests.log`

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT
  worker prompt lives at
  `src/ai_strategy_generator/multistep/implement_worker.system-md` in the
  tool repo (the step-007..010 gotcha recurred); located and read in full.
- **The dispersion-model facet cannot recover the ten `DispersionModel`
  kinds from a stored entry** — the editor lowers every model through
  `toEpsAxis` at save (spec 0033), so segments store `EpsAxisDispersion`
  term data (Sellmeier becomes complex-ε terms, the transcendental four an
  opaque closure) and shape-recognizing them back would be the re-derived
  physics §D.0 forbids. RECORDED INTERPRETATION: the facet classifies the
  three storable shapes ("Real n/k terms" / "Complex ε terms" / "Evaluated
  (transcendental)") per axis/segment, and `modelDispersionKey :
  DispersionModel -> DiscreteKey` pins where each of the ten cases lands
  once lowered — multi-valuedness (the acceptance) is proven with a
  uniaxial two-segment entry extracting all three keys.
- **Coded presets (complexity = None) offer NO value-tree facet** —
  anisotropy, transparency, dispersion model, activity, magnetic all
  vanish for silicon/langasite/vacuum; notably langasite (physically
  active) and vacuum (constant) do not offer activity/transparency because
  there is no data to classify. Category and constant-vs-dispersive read
  `category`/`properties` and apply to every entry. Pinned in tests.
- **The seeded "Uniaxial crystal" classifies Biaxial** — its complexity is
  encoded as per-axis `BiaxialTransparent` values (the MaterialLibrary seed
  comment explains the (o, e, e) diagonal has no engine uniaxial shape), and
  the facet classifies the ENCODING. Pinned.
- **Sample constituents = `SampleStructure.referencedMaterials`** (films +
  substrate plate + LOWER half-space): the slice's "substrate plus film
  layers" is read through the existing constituents seam, so
  "Langasite film on silicon" matches Semiconductor through its lower
  half-space — excluding `lower` would be user-hostile for exactly that
  seed. Pinned ("lifted over every constituent including the lower
  half-space").
- **`Transparency` appliesTo is a conjunction**: the entry classifies
  `ConstantMaterial` (per `materialDispersion`, so a constant-eps entry with
  a dispersive μ/ρ stays Dispersive and never offers it) AND carries a
  `ConstantEpsValue` tree.
- **Unresolvable constituent ids contribute nothing** — a material id the
  supplied corpus does not hold cannot be classified and is dropped
  (`List.choose`), never a throw and never a Guid-string branch.
- **Thickness magnitudes round-trip through meters** — `Thickness` stores
  `double<meter>`, so a nominal 1000 nm layer can come back a few ulp off
  1000.0 and 1-2-5 rung-boundary assertions on such values would be
  fragile; the bucket-flow test uses off-rung magnitudes (12/30/700 nm) and
  the seed-value pins use tolerance equality. The per-layer nm read goes
  through the sole `Units` seam (`fromMeters Nanometer`), pattern-matching
  `Thickness` directly (measure-preserving; `Propagation.thicknessMeters`
  strips the measure for Ui consumption) with `Infinity` yielding no
  bucketable magnitude.
- **`WorkbenchSettings.ThicknessBucketCap.defaultValue` does not resolve
  from outside** — the case name shadows the type under module-qualified
  access (the `MaterialId`/`elementId` collision class); tests open
  `WorkbenchSettings` and use `ThicknessBucketCap.defaultValue` (the
  FacetBucketsTests precedent).
- Step 002–010 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the window registry is app-global
  test state in Ui.Tests; the appsettings.json write-back into test output
  copies is expected).
