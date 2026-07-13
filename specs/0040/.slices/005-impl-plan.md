# Step 005 — impl-plan

## Goal

Make the **Anisotropy** and **Transparency** material facets TOTAL (spec 0040
Part B.2, operator 010/Q2): every `MaterialEntry` classifies under exactly one
branch of each, so — over the seed corpus — the Category, Anisotropy, Dispersion
and Transparency branch counts each sum to the material count. Every dispersive
material must land under **Absorbing**.

## Approach

Two production edits in
`OpticalConstructor.Domain/LibraryFacets.fs`, plus test updates in
`OpticalConstructor.Tests/LibraryFacetsTests.fs`.

1. **`materialAnisotropyDef`** — drop the `appliesTo` gate to
   `fun _ -> ApplicableAttribute`. Its `extract` already reads
   `anisotropyOf complexity` for a `Some` value tree and yields `[]` for the
   defensive `None`; after step 004 every built-in carries `complexity = Some`,
   so over the corpus the anisotropy applicability is unchanged by this edit
   (it already applied to all built-ins) — the edit closes the totality
   contract for any hypothetical `complexity = None` entry.

2. **`materialTransparencyDef`** — drop the `appliesTo` gate to
   `fun _ -> ApplicableAttribute`, and rewrite `extract` to classify via
   `materialDispersion` (the primary discriminator the spec names):
   - `ConstantMaterial` with a constant eps value tree → `transparencyOf constant`.
   - Every `DispersiveMaterial` (and the defensive constant-without-value-tree
     case) → `DiscreteValue (transparencyKey Absorbing)`.
   This is the only behavioural change over the real corpus: dispersive
   built-ins (silicon / langasite / the EUV metals if dispersive) now classify
   Absorbing where they previously did not offer the facet at all.

3. Leave the multi-valued **Dispersion-model**, **Film-material** and
   **Film-thickness** facets untouched (their counts need not sum to the total).

4. Update the module header bullet and the two def doc-comments so the
   applicability model reads accurately (the four single-valued classifiers are
   total; the dispersion-model / activity / magnetic facets remain dependent).

## Tests (`LibraryFacetsTests`)

- **New acceptance test**: over `builtInEntries`, the Category, Anisotropy,
  Dispersion and Transparency single-facet trees each have branch counts that
  sum to `List.length builtInEntries`.
- **New acceptance test**: every dispersive built-in extracts `["Absorbing"]`
  from the transparency facet, and the Absorbing branch of the transparency
  tree counts at least the dispersive-material count.
- **Repoint** three now-stale tests to the new totality:
  - `the transparency facet is offered for constant data-carrying materials only`
    → transparency applies to every entry; dispersive materials classify Absorbing.
  - `dependent facets vanish entirely over a population they never apply to`
    (silicon+langasite) → only the magnetic facet vanishes now; transparency joins.
  - `a lifted facet vanishes when no constituent offers it` (langasite-on-silicon)
    → use the MAGNETIC facet as the vanishing one; anisotropy AND transparency now
    apply.

## Risks

- **Net test count must not regress** (`count_at_least`): I modify three tests in
  place and ADD two, so the count rises (+2). No test is removed.
- UI tests reference the transparency/anisotropy keys only as representation
  orderings and a "transparency stays offered over constant materials" / "anisotropy
  leads by-physics" assertion — both hold under totality (applicability is only
  ADDED, never removed over the real corpus). No UI edit expected.
- The `materialDispersion`-based transparency extract intentionally overrides a
  constant eps classification when the material is dispersive by μ/ρ (spec: classify
  via `materialDispersion`), so every dispersive material lands under Absorbing.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs` (edit)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs` (edit)
