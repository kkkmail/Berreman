# Step 004 — state of the world

## Where we are

Step 004 is the first slice of spec 0040 **Part B** (facet totality). It re-seeds the three
former coded-preset materials — Silicon, Langasite and the Vacuum spacer — so their
single-valued class is DATA the facets read, rather than an unclassifiable engine closure.
Each now carries a `complexity = Some` value tree with `properties = complexity.toProperties`,
exactly the seed convention the other nine built-ins already use, so `LibraryFacets.anisotropyOf`
reads the class from the value-tree CASE (Silicon Isotropic, Langasite Uniaxial, Vacuum
Isotropic) with no engine-tensor evaluation. Step 005 (the sibling) then drops the facet
`appliesTo` gates to make Anisotropy and Transparency total.

## What's working

- Re-seed Silicon (Isotropic, dispersive), Langasite (Uniaxial, dispersive, optically active)
  and the Vacuum spacer (Isotropic, transparent) with `complexity = Some` value trees, their
  physics reproduced through the dispersion-model ladder's evaluated rung.
- `LibraryFacets.anisotropyOf` now classifies all three from data — Silicon/Vacuum Isotropic,
  Langasite Uniaxial — and every built-in carries `complexity = Some`.
- Keep the coded presets view-only via a new `isEditableComplexity` predicate, gated at both
  material editability seams, so an opaque evaluated closure stays uneditable.
- Reproduce Silicon and Langasite ε bit-for-bit from the engine closures, guarded by
  reproduction tests at 1e-12; Langasite's dispersive gyration ρ (no value-tree closure escape)
  is a class-correct representative.
- Update the material/facet/propagation/out-of-band tests to the post-re-seed reality; add an
  `anisotropyOf` acceptance test and an ε reproduction test.

## Tests

- Gate execution belongs to the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). The roster for this step is
  `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (NOT gate authority): `dotnet build Berreman.slnx -c Release` —
  0 errors, only exempt warnings (4× NU1701, 4× SYSLIB0051), zero from our code;
  `BerremanTests` 119 passed / 5 skipped; `OpticalConstructor.Tests` 677 passed (was 675, +2
  net — an ε reproduction test and the `anisotropyOf` acceptance test, none removed);
  `OpticalConstructor.Ui.Tests` 656 passed across both categories (the view-only UI tests pass
  UNCHANGED). No `count_at_least` gate can regress: every suite is at or above its prior count.

## Architecture

- **The class is DATA, read from the value-tree CASE — never from the engine tensor.**
  `anisotropyOf` reads `IsotropicDispersive`/`UniaxialDispersive`/`IsotropicTransparent`; the
  ε closures are REPRODUCTION only (re-stating the published index inside `EpsAxisEvaluated`,
  the same case `DispersionModels.toEpsAxis` lowers transcendental models to), so no classifier
  evaluates the assembled ε (spec §0.4 / B.1).
- **Editability is a property of the complexity, not of its presence.** Since every entry now
  carries `complexity = Some`, `isEditableComplexity` distinguishes a losslessly-editable
  finite-term/constant tree from an opaque evaluated closure; both UI editability seams gate on
  it, so the coded presets stay view-only exactly as before.
- **The out-of-band diagnostic is unchanged.** Coded presets declare an UNBOUNDED band (a
  single spectrum-spanning segment), so no request leaves their defined range — the "coded
  preset never flags" behaviour holds with `definedSegmentIntervals` untouched.

## Deferred

- Making Anisotropy and Transparency facets TOTAL by dropping the `appliesTo` gates is step 005
  (Part B.2) — out of scope here; this slice only supplies the seed data.
- Part C (material-editor chart applicability, step 006) rewrites `NkDispersionChart`; the
  stale `complexity = None` comment in `NkDispersionChartTests.fs:25` is left for that slice.

## Gotchas

- **Scope extension beyond declared `touches: [Domain, Tests]`.** The re-seed makes the presets
  carry `complexity = Some`, and BOTH UI editability seams inferred editability from complexity
  presence, so keeping the `ui-smoke`/`ui-tests` gates green REQUIRED editing
  `OpticalConstructor.Ui` (`MaterialsWindowView`, `MaterialEditorView`) to gate on the new
  `isEditableComplexity` predicate. This is unavoidable — no Domain-only change can keep the
  presets view-only — and no `OpticalConstructor.Ui.Tests` change was needed (the view-only
  tests pass unchanged). Recorded so a reviewer sees the extension is a necessary consequence.
- **Langasite ρ is representative.** The engine's dispersive gyration is transcendental and
  `RhoWithDispValue` has no closure escape (unlike ε's `EpsAxisEvaluated`), so the value tree
  cannot reproduce it. Langasite's ε is reproduced faithfully; its ρ is a class-correct constant
  `UniaxialActive` gyration (mirroring `activeCrystalComplexity`, the spec's own reuse
  precedent). The langasiteSilicon exact-equality PropagationTest is updated: Silicon's
  half-space stays pinned to the engine preset (ε bit-for-bit), the Langasite film to the
  re-seeded material's own properties.
- **`anisotropyOfEntry` fallback no longer fires for these three.** They previously hit the
  `complexity = None → Biaxial` fallback in `NkDispersionChart`; now the chart draws the correct
  (Isotropic/Uniaxial) series. No test pins the affected series count.
- **Line endings.** `git diff --numstat` equals `--ignore-cr-at-eol` on every touched `.fs`, so
  no CR-only churn was introduced; `.gitattributes` normalises to LF at commit.

## Changelog

- 2026-07-13 — Step 004 (IMPLEMENT): re-seeded Silicon / Langasite / the Vacuum spacer from
  `complexity = None` to `complexity = Some` value trees (Isotropic dispersive / Uniaxial
  dispersive + optical activity / Isotropic transparent), `properties = complexity.toProperties`,
  their ε reproduced through the ladder's evaluated rung; added `isEditableComplexity` and gated
  both editability seams so the coded presets stay view-only; updated the material/facet/
  propagation/out-of-band tests and added an `anisotropyOf` acceptance test plus an ε
  reproduction guard (+2 constructor tests, none removed; Ui tests unchanged).

```yaml
gates:
  berreman_unit_tests: 0
  constructor_unit_tests: 0
  ui_smoke_tests: 0
  ui_tests: 0
```
