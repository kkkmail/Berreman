# Step 005 — impl-log

## Progress

- [x] Read protocol, project prompt, slice spec, step-004 SoW.
- [x] `materialAnisotropyDef` — drop `appliesTo` gate (total).
- [x] `materialTransparencyDef` — drop `appliesTo` gate, classify via `materialDispersion`.
- [x] Doc-comment updates (module header + two defs).
- [x] Repoint three now-stale tests to the totality reality.
- [x] Add the two acceptance tests (branch-count sums; every dispersive under Absorbing).
- [x] Diagnostic verification (NOT gate authority).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryFacets.fs`
  - `materialAnisotropyDef.appliesTo` → `fun _ -> ApplicableAttribute` (extract unchanged; already
    reads `anisotropyOf complexity` with a defensive `None -> []`).
  - `materialTransparencyDef.appliesTo` → `fun _ -> ApplicableAttribute`; `extract` rewritten to
    classify via `materialDispersion`: `ConstantMaterial` + constant eps → `transparencyOf constant`;
    every `DispersiveMaterial` (and the defensive constant-without-value-tree case) → Absorbing.
  - Module header bullet + the two def doc-comments updated to the totality model.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryFacetsTests.fs`
  - Repointed: `the transparency facet is offered for constant data-carrying materials only`
    → `the transparency facet is total and lands dispersive materials under Absorbing`.
  - Repointed: `dependent facets vanish entirely over a population they never apply to`
    (only magnetic vanishes now; transparency joins the applicable set).
  - Repointed: `a lifted facet vanishes when no constituent offers it`
    (uses the MAGNETIC facet as the vanishing one; anisotropy + transparency now apply).
  - Added: `category, anisotropy, dispersion and transparency branch counts each sum to the material count`.
  - Added: `every dispersive material appears under the Transparency Absorbing branch`.

## Decisions

- **Transparency discriminator is `materialDispersion`, not `constantEpsOf`.** The slice names
  `materialDispersion` as the classifier, so a material that is dispersive by μ/ρ but carries a
  constant eps value tree still lands under Absorbing (every dispersive material → Absorbing). Over
  the seed corpus this and the `constantEpsOf`-first reading coincide, but the spec's wording is
  honoured for the general case.
- **Anisotropy `extract` left as-is.** It already reads `anisotropyOf complexity` for `Some` and
  yields `[]` for the defensive `None`; only the `appliesTo` gate needed dropping.

## Testing state

Gate execution belongs to the arc-runner's deterministic gate engine after this worker exits
(IMPLEMENT Invariant 6 — the worker acts, it runs no checks). Roster: `build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`.

**Diagnostic verification (NOT gate authority)** — following the step-004 in-arc precedent, a
local build + the affected test projects were run only to de-risk the round:

- `dotnet build Berreman.slnx -c Release` — **0 errors**; 4 warnings, all exempt (2× NU1701
  Wolfram.NETLink fallback-framework, 2× SYSLIB0051 in the vendored MathNet.Numerics C# source).
  Zero warnings from our F# code.
- `BerremanTests` (`unit-tests`) — **119 passed / 5 skipped** (unchanged from step 004; the solver
  is untouched).
- `OpticalConstructor.Tests` (`constructor-unit-tests`) — **679 passed** (was 677 at step 004; **+2
  net**: the two new acceptance tests. Three tests repointed in place, none removed).
- `OpticalConstructor.Ui.Tests` (`ui-smoke` + `ui-tests`) — **656 passed** (unchanged; the totality
  change only ADDS applicability over the real corpus, and the UI references the two facet keys only
  as representation orderings / an "offered over constants" and "anisotropy leads by-physics"
  assertion — both still hold).

No `count_at_least` gate can regress: every suite is at or above its prior count.

## Artifacts

Under `C:\GitHub\Berreman\specs\0040\.artifacts`:

- `005-diagnostic-build.log`
- `005-diagnostic-berreman-tests.log`
- `005-diagnostic-constructor-tests.log`
- `005-diagnostic-ui-tests.log`
