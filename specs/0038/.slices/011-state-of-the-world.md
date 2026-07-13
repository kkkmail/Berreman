# Step 011 — state of the world

## Where we are

Step 011 completes the pure-Domain half of spec 0038 Part D (the faceted
engine): on top of step 009's generic engine (`Domain/Facets.fs`) and step
010's numeric buckets (`Domain/FacetBuckets.fs`), the two CONCRETE facet
catalogues now exist as data in `Domain/LibraryFacets.fs` — the MATERIAL
facets over `MaterialEntry` (category, anisotropy, constant-vs-dispersive,
transparency for constant materials only, the multi-valued per-axis/segment
dispersion-model facet, optical activity → symmetry class + handedness, and
magnetic → scalar/gyromagnetic) and the LIBRARY-ENTRY facets over
`LibraryEntry` (the kind facet always; every material facet lifted to samples
as the distinct union over constituent materials; substrate material for
Plate/Wedge only; 'has thin films'; film materials; per-film thickness bucketed
by step 010). Part E (`FacetedTreeControls` + the Materials window) renders
these catalogues; Part F instantiates the Library window over the entry corpus.

## What's working

- Add Domain/LibraryFacets.fs: the material facet catalogue (8 defs) — category
  via the seeded catalogue names, anisotropy/transparency from the eps value
  trees (editor DUs reused), constant-vs-dispersive from properties for every
  entry incl. coded presets, the multi-valued per-axis/segment dispersion-model
  facet, activity class + handedness keyed by the editor's class labels with
  the vocabulary from availableGyrationClasses, magnetic scalar/gyromagnetic.
- Add the library-entry catalogue: the kind facet, every material facet lifted
  to samples as the distinct union over constituents (films + substrate +
  lower half-space — any constituent matches), substrate material (Plate/Wedge
  only), has-thin-films (independent of SubstrateKind), name-keyed film
  materials, and per-film thickness in nm feeding the step-010 buckets.
- Elevate MaterialLibrary's private hasDispersion bool to the public
  MaterialDispersion DU (ConstantMaterial | DispersiveMaterial) reused by
  byQuery and the facet extractors — no behaviour change.
- 32 new pure facts in LibraryFacetsTests over the seeded corpora
  (builtInEntries, SeedSamples) plus targeted synthetic entries, proving every
  acceptance observation.
- Suites 559 (+32) / 119 / 124 / 361; build clean, no MSB3277, zero warnings
  from touched projects.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (IMPLEMENT Invariant 6 — the worker acts, it runs no checks).
  The roster for this step is `build`, `unit-tests`, `constructor-unit-tests`,
  `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release` succeeded with 0 errors, **no MSB3277**, and zero warnings from the
  touched projects (Domain / Tests) — the only warnings are the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051
  vendored MathNet ×2, NU1701 Wolfram.NETLink ×2). Suites:
  OpticalConstructor.Tests **559/559** (checkpoint 527, +32: the whole
  LibraryFacetsTests suite — every extractor and appliesTo rule over the
  seeded corpora, dependent facets vanishing entirely when inapplicable, the
  multi-valued dispersion-model facet with branch counts exceeding the total,
  any-constituent sample matching incl. the lower half-space, name-sorted film
  materials, and per-film thicknesses flowing into the step-010 buckets with a
  bucket chip reproducing its count), BerremanTests **119 passed / 5
  pre-existing skips** (== checkpoint), ui-smoke **124/124** (== checkpoint),
  ui-tests **361/361** (== checkpoint). Logs in
  `specs/0038/.artifacts/011-diag-*.log`.
- Nothing deferred.

## Architecture

- **Catalogues are data over the two engines, not new machinery**:
  `materialFacets` is a plain `AttributeDef<MaterialEntry> list`;
  `libraryFacets` is parameterized by the material corpus (the only input
  needed to resolve constituent ids to entries and display names) and returns
  a plain `AttributeDef<LibraryEntry> list`. `Facets.fs` and `FacetBuckets.fs`
  are untouched.
- **The lift is one combinator**: `liftToSamples` turns any material def into
  a sample def — applicable when ANY constituent has the facet applicable,
  extraction the distinct union over applicable constituents — so every
  material facet (present and future) lifts uniformly; non-sample entries
  never offer material facets. Constituents come from the existing
  `SampleStructure.referencedMaterials` seam (films + substrate + lower).
- **Value-tree facets vanish for coded presets by the applicability gate
  alone**: the physics facets classify the serializable value trees
  (`MaterialComplexity`), so `complexity = None` means
  `InapplicableAttribute` — the "dependent facets vanish" rule needs no
  special-casing anywhere.
- **The dispersion-model facet classifies the three storable shapes**
  (`EpsAxisDispersion`: real n/k terms / complex ε terms / evaluated), one
  classification per axis per segment — the ten `DispersionModel` kinds are
  not recoverable from lowered term data, and `modelDispersionKey` pins where
  each of the ten lands (recorded interpretation, see Gotchas).
- **Vocabularies reuse the editor's seams**: `Anisotropy`, `Transparency`,
  `MuKind`, `gyrationClassLabel`, and `availableGyrationClasses` (the offered
  class-key vocabulary) come from `MaterialComplexityEditor`; category names
  resolve through `standardCategories` via `categoryName`; the elevated
  `MaterialDispersion` DU replaces the private bool in `MaterialLibrary`.
- **Per-film thickness is an ordinary `NumericAttribute`**: per-layer nm
  magnitudes (a `Repeated` cell once, `Infinity` yields nothing) through the
  sole `Units` seam; step 010's `bucketsFor`/`constraintFor` bucket and chip
  them with zero new code.

## Deferred

- The domain-free `FacetedTreeControls` rendering control, Show/Search gating
  above `TreeAutoBuildThreshold`, the representation picker, and the Materials
  window instantiation — Part E (step 012+).
- The Library window over the whole entry corpus, `EntryProtection`, and the
  polarizer-category facet for non-sample entries — Part F.
- Per-item-per-bucket dedup for multi-valued numeric facets (an item whose two
  DIFFERENT film thicknesses land in the same bucket counts twice in that
  bucket's magnitude count — the step-010 note; the per-film facet counts
  layer values, not items, by design here).
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–010).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool
  repo; located and read in full (the step-007..010 gotcha recurred).
- **The ten `DispersionModel` kinds are NOT recoverable from a stored
  entry** — the editor lowers models via `toEpsAxis` at save (spec 0033), so
  dispersive segments carry `EpsAxisDispersion` term data and shape-recognizing
  it back would re-derive physics (§D.0). The dispersion-model facet therefore
  classifies the three storable shapes per axis/segment, and
  `modelDispersionKey : DispersionModel -> DiscreteKey` pins the ten-case
  mapping (ConstantNK/Cauchy/raw-real → "Real n/k terms";
  Sellmeier/Lorentz/Drude → "Complex ε terms"; the transcendental four →
  "Evaluated (transcendental)"). Recorded interpretation, pinned in tests.
- **Coded presets offer no value-tree facet**: langasite (physically active)
  and vacuum (constant) do not offer activity/transparency — there is no data
  to classify; only category and constant-vs-dispersive apply to them. Pinned.
- **The seeded "Uniaxial crystal" classifies Biaxial** — its complexity is
  encoded as per-axis `BiaxialTransparent` values (seed comment: the (o, e, e)
  diagonal has no engine uniaxial shape); the facet classifies the ENCODING.
  Pinned.
- **Sample constituents include the LOWER half-space**
  (`referencedMaterials`): "Langasite film on silicon" matches Semiconductor
  through `lower` — the slice's "substrate plus film layers" is read through
  the existing constituents seam. Pinned.
- **Transparency's appliesTo is a conjunction**: classifies `ConstantMaterial`
  AND carries a `ConstantEpsValue` tree — a constant-eps entry with a
  dispersive μ/ρ stays Dispersive and never offers it.
- **Thickness magnitudes round-trip through meters** — a nominal 1000 nm layer
  can come back a few ulp off 1000.0, so never pin a bucket assertion on a
  value sitting exactly on a 1-2-5 rung (the bucket-flow test uses 12/30/700
  nm; seed-value pins use tolerance equality).
- **`WorkbenchSettings.ThicknessBucketCap.defaultValue` does not resolve
  module-qualified from outside** (case name shadows the type — the
  `MaterialId` collision class); open the module and use
  `ThicknessBucketCap.defaultValue`.
- Step 002–010 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the window registry is app-global
  test state in Ui.Tests; the appsettings.json write-back into test output
  copies is expected).

## Changelog

- 2026-07-11 — Step 011 (IMPLEMENT, attempt 1): added the two concrete facet
  catalogues `Domain/LibraryFacets.fs` (material facets: category, anisotropy,
  constant-vs-dispersive, constant-only transparency, multi-valued per-axis/
  segment dispersion model, activity class + handedness via
  availableGyrationClasses, magnetic scalar/gyromagnetic; library facets:
  kind, material facets lifted over any constituent, Plate/Wedge substrate
  material, has-thin-films, film materials, per-film thickness into the
  step-010 buckets), elevated `MaterialLibrary.hasDispersion` to the public
  `MaterialDispersion` DU, and 32 pure facts in LibraryFacetsTests over the
  seeded corpora. Build clean (no MSB3277); suites 559 / 119 / 124 / 361.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 559
  ui_smoke_tests: 124
  ui_tests: 361
```
