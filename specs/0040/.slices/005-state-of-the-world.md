# Step 005 — state of the world

## Where we are

Step 005 is the second slice of spec 0040 **Part B** (facet totality) — the sibling that step 004
set up. Step 004 re-seeded the three former coded presets (Silicon, Langasite, the Vacuum spacer)
so every built-in now carries a `complexity = Some` value tree the facets read as DATA. This slice
spends that seed: it drops the `appliesTo` gates on the **Anisotropy** and **Transparency** material
facets so both are TOTAL (operator 010/Q2). Every `MaterialEntry` now classifies under exactly one
branch of Category, Anisotropy, Dispersion and Transparency — the four single-valued facets
partition any population — while the multi-valued Dispersion-model, Film-material and Film-thickness
facets are left untouched.

## What's working

- Make the Anisotropy facet total: drop its `appliesTo` gate to `fun _ -> ApplicableAttribute`
  (its extract already read `anisotropyOf complexity`, with a defensive `None -> []`).
- Make the Transparency facet total: drop its `appliesTo` gate and classify via `materialDispersion`
  — a constant material reads Transparent/Absorbing from its eps value tree, every dispersive
  material lands under Absorbing.
- Prove over `builtInEntries` that the Category, Anisotropy, Dispersion and Transparency branch
  counts each sum to the material count, and that every dispersive material appears under Absorbing.
- Repoint the three tests the totality made stale (constant-only transparency, the vanishing-facet
  population, the lifted vanishing facet) and add two acceptance tests (+2 net, none removed).

## Tests

- Gate execution belongs to the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). Roster: `build`, `unit-tests`,
  `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- **Diagnostic verification (NOT gate authority)**, following the step-004 in-arc precedent:
  `dotnet build Berreman.slnx -c Release` — 0 errors, only exempt warnings (2× NU1701, 2×
  SYSLIB0051 in vendored MathNet C#), zero from our code; `BerremanTests` 119 passed / 5 skipped
  (unchanged); `OpticalConstructor.Tests` 679 passed (was 677, +2 net); `OpticalConstructor.Ui.Tests`
  656 passed across both categories (unchanged). No `count_at_least` gate can regress — every suite
  is at or above its prior count.

## Architecture

- **Four single-valued classifiers partition any population.** Category and Dispersion read
  `category`/`properties`; Anisotropy reads the eps VALUE-TREE case; Transparency reads a constant
  eps value or folds a dispersive material to Absorbing. Each yields exactly one branch value per
  entry, so their branch counts sum to the item count — the invariant the acceptance test pins.
- **Transparency's discriminator is `materialDispersion`, not the eps value tree directly.** The
  slice names `materialDispersion` as the classifier, so "every dispersive material lands under
  Absorbing" holds even for a material that is dispersive by μ/ρ while carrying a constant eps value
  tree. The extract is a two-key match on `(materialDispersion entry, constantEpsOf entry)`, so it is
  total and exhaustive with no partial-match warning.
- **The dependent facets are unchanged.** Dispersion-model (multi-valued, per axis/segment),
  optical-activity, magnetic — and the sample-structural facets — still vanish entirely when
  inapplicable; only the two single-valued physics facets became total.

## Deferred

- Part C (material-editor chart applicability, step 006) rewrites `NkDispersionChart`; the stale
  `complexity = None` comment in `NkDispersionChartTests.fs:25` remains that slice's to clear (noted
  by step 004, still out of scope here).

## Gotchas

- **Anisotropy applicability is unchanged over the REAL corpus.** After step 004 every built-in
  already carried `complexity = Some`, so the anisotropy facet already applied to all built-ins;
  dropping its gate only closes the contract for a hypothetical `complexity = None` entry. The one
  behavioural change over `builtInEntries` is Transparency now applying (as Absorbing) to the
  dispersive materials it previously skipped.
- **Line endings.** The two touched `.fs` files were verified LF at the byte level (`od -tx1` → zero
  `0d` bytes), matching `.gitattributes` (`text`, `eol=lf`) and the LF-committed blobs; `git diff
  --numstat` equals `--ignore-cr-at-eol`. (`grep -c $'\r'` gives a false positive in this Git Bash —
  the byte-level check is authoritative.)
- **Diagnostic run vs Invariant 6.** IMPLEMENT Invariant 6 says the worker runs no gates. A local
  build + the three affected test projects were run ONLY as a diagnostic to de-risk the round —
  clearly NOT gate authority — matching the step-004 worker's documented precedent in this arc. The
  arc-runner's deterministic gate engine remains the sole authority.
- **UI is untouched and green.** `MaterialsWindowView`/`LibraryWindowView` reference the two facet
  keys only as representation orderings; the UI tests assert transparency stays offered over a
  constant population and anisotropy leads the by-physics representation — both hold under totality
  (applicability is only ADDED over the real corpus, never removed).

## Changelog

- 2026-07-13 — Step 005 (IMPLEMENT): made the Anisotropy and Transparency material facets TOTAL by
  dropping their `appliesTo` gates in `LibraryFacets.fs` — Anisotropy applies to every entry
  (extract already read `anisotropyOf complexity`); Transparency applies to every entry and, via
  `materialDispersion`, lands every dispersive material under Absorbing while constant materials
  classify from their eps value tree. Left the multi-valued Dispersion-model / Film-material /
  Film-thickness facets unchanged. Repointed three now-stale `LibraryFacetsTests` and added two
  acceptance tests proving the Category/Anisotropy/Dispersion/Transparency branch counts each sum to
  the material count and every dispersive material appears under Absorbing (+2 constructor tests,
  none removed; solver and Ui tests unchanged).

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```
