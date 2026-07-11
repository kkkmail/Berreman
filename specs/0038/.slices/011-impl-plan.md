# Step 011 — impl plan (IMPLEMENT, attempt 1)

## Goal

Add the two concrete facet catalogues as DATA on top of step 009's generic
engine (`Domain/Facets.fs`) and step 010's numeric buckets
(`Domain/FacetBuckets.fs`): the MATERIAL facets over `MaterialEntry` and the
LIBRARY-ENTRY facets over `LibraryEntry`, in a new `Domain/LibraryFacets.fs`,
with unit tests pinning every extractor and every `appliesTo` rule over the
seeded corpora (`builtInEntries`, `SeedSamples`).

## Approach

1. **`MaterialLibrary.fs` (small seam change).** The constant-vs-dispersive
   classification already exists as `private hasDispersion : MaterialEntry ->
   bool`. The facet catalogue needs it, but a public bool-returning function
   violates the elevate-every-primitive rule — so replace it with a public
   two-case DU `MaterialDispersion = ConstantMaterial | DispersiveMaterial`
   plus `materialDispersion : MaterialEntry -> MaterialDispersion`, and route
   `byQuery`'s `DispersionFilter` arms through it. No behaviour change.

2. **New `Domain/LibraryFacets.fs`** (compiles after `FacetBuckets.fs`):
   - MATERIAL facets (`AttributeDef<MaterialEntry> list`): category (names
     resolved through the seeded catalogue via `categoryName`); anisotropy
     (the `ConstantEpsValue`/`EpsDispersiveValue` shape of the complexity's
     eps value tree, reusing the editor's `Anisotropy` DU); constant-vs-
     dispersive (via `materialDispersion`, applies to ALL entries including
     coded presets); transparent-vs-absorbing (CONSTANT materials only, from
     the `ConstantEpsValue` case, reusing the editor's `Transparency` DU);
     dispersion model (MULTI-VALUED per axis/segment — see the interpretation
     note below); optical activity → symmetry class + handedness (only when
     active; class keys via `gyrationClassLabel`, the offered vocabulary via
     `availableGyrationClasses`, never re-derived); magnetic → scalar/
     gyromagnetic (only when magnetic, reusing the editor's `MuKind`).
   - LIBRARY-ENTRY facets (`libraryFacets : MaterialEntry list ->
     AttributeDef<LibraryEntry> list` — parameterized by the material corpus
     that resolves constituent ids): the kind facet always; every material
     facet lifted to samples as the DISTINCT UNION over constituent materials
     (`SampleStructure.referencedMaterials` — films + substrate plate + lower
     half-space; any constituent matches); substrate material (Plate/Wedge
     only, from `structure.substrate`); 'has thin films' from
     `structure.films` non-empty, independent of `SubstrateKind`; film
     material(s) (corpus-derived names, multiselect = the engine's discrete
     key-set OR); per-film thickness as a `NumericAttribute` in nm (per-layer,
     a `Repeated` cell's layers once), bucketed by step 010's `bucketsFor`.

3. **New `Tests/LibraryFacetsTests.fs`**: ~30 pure facts pinning each
   extractor and each appliesTo rule over `builtInEntries`/`SeedSamples`,
   plus synthetic entries for what the seeds cannot reach (a data-dispersive
   uniaxial two-segment entry for the multi-valued dispersion-model proof,
   magnetic entries, a wedge sample, a mixed transparent+absorbing film
   sample, and a small film corpus for the bucket-flow acceptance).

## The dispersion-model interpretation (the one genuine ambiguity)

The slice says the facet is "per axis/segment over the DispersionModel cases
(DispersionModels.fs:168-178)". A stored `MaterialEntry` does NOT carry
`DispersionModel` values: the editor lowers every model through `toEpsAxis`
at save (spec 0033), so the complexity's dispersive segments carry
`EpsAxisDispersion` term data (`RealNK` / `ComplexEps` / `EpsAxisEvaluated`)
and the ten analytic kinds are not recoverable (Sellmeier lowers to
`ComplexEps` terms; the transcendental four to an opaque closure).
Recognizing lowered term shapes back into the ten kinds would be exactly the
"re-derived physics" the spec forbids and is unsound anyway. So the facet
classifies each axis/segment by its `EpsAxisDispersion` case — three keys:
"Real n/k terms" / "Complex ε terms" / "Evaluated (transcendental)" — and a
public `modelDispersionKey : DispersionModel -> DiscreteKey` (via `toEpsAxis`)
pins how each of the ten `DispersionModel` cases maps onto the vocabulary.
This keeps the facet honest, multi-valued per axis/segment (the acceptance),
and connected to the ten-case DU. Recorded in the impl-log Gotchas.

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialLibrary.fs`
  (elevate `hasDispersion` → `MaterialDispersion` + `materialDispersion`)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- Thickness values round-trip through meters (`Thickness` stores
  `double<meter>`), so a nominal 1000 nm film may come back as
  999.9999999999999 — never pin a bucket assertion on a value that sits
  exactly on a 1-2-5 rung; the bucket-flow test uses off-rung magnitudes
  (12/30/700 nm) and the seed-value pins use precision-tolerant equality.
- Facet vocabulary keys double as branch labels (step 009), so key strings
  are user-visible — chosen to match the editor's picker labels
  (`gyrationClassLabel`) where an existing label seam exists.
- Coded presets (complexity = None: silicon, langasite, vacuum) cannot offer
  the value-tree-derived facets (anisotropy, transparency, model, activity,
  magnetic) — they are `InapplicableAttribute` there, which is exactly the
  "dependent facets vanish" rule; pinned in tests and recorded.
