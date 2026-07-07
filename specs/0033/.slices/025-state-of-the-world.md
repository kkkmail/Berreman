# State of the world — spec 0033, slice 025

# Where we are

Slice 025 closes the import gap the Part-B lowering machinery (slices 007–013)
opened: where `MaterialImport.importRefractiveIndexInfo` previously understood
only tabulated pages and a closure-backed (view-only) `formula 1`, it now parses
refractiveindex.info dispersion formulas 1–7 (Sellmeier, Sellmeier-2,
polynomial, the RII formula, Cauchy, gases, Herzberger) and lowers each —
through the catalogue `DispersionModel` (1/2) or directly to the serializable
term data of `Dispersion.fs` (3–7, the `SumOfTerms` shape) — to an
`EpsAxisDispersion` carried in an `EpsWithDispValue`-backed `MaterialComplexity`
on the imported `MaterialEntry`. Formula imports are therefore EDITABLE in the
step-023 Material editor exactly like the re-expressed built-ins; formulas 8/9
(and unknown numbers) return the new typed `UnsupportedFormula` error. λ keeps
the µm RII convention through the sole `Units` seam.

# What's working

- importRefractiveIndexInfo parses RII formulas 2–7 beyond tabulated + formula 1: Sellmeier-2, polynomial, RII-formula, Cauchy, gases, Herzberger
- Each formula lowers exactly to EpsAxisDispersion term data (Sellmeier via the catalogue model's toEpsAxis; ε-shaped 3/4 as ComplexEps, n-shaped 5/6/7 as RealNK with k = 0) in an EpsWithDispValue-backed MaterialComplexity — formula imports are editable
- Formula 1 unified onto the same data route (evaluated index preserved exactly; entry name/description unchanged) — no closure-backed import path remains for formulas
- Formulas 8/9 and unknown numbers return the NEW typed UnsupportedFormula (formulaNumber, reason) ImportError; non-integer exponents reuse it with a diagnostic reason
- Imported segments carry the page's wavelength_range (µm) through the Units seam; tabulated + CSV import paths untouched
- +9 fixture-driven facts (constructor tests 407 → 416) pinning evaluated index against hand-computed published-formula values at two wavelengths per formula, the editable-complexity shape, the parsed range, and the 8/9 typed errors; all suites green

# Tests

All gates in the slice roster pass in the worker's local ADVISORY runs
(Invariant 6: the arc-runner gate engine re-runs them authoritatively after
exit). Full logs in `specs/0033/.artifacts/025-*.log`, plus the TDD red capture
`025-red-tdd.log` (FS0039 naming the missing `UnsupportedFormula` production
symbol before implementation).

- `build` — solution builds Release/x64, exit 0, 0 errors; the 93 warnings are
  the pre-existing NU190x/MSB3277/SYSLIB0051/FS3873/FS1125 noise, none from the
  new code.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 416 passed, 0 failed (407 baseline + 9 new facts).
- `ui-smoke` — 81 passed, 0 failed (= baseline; no UI code touched).
- `ui-tests` — 306 passed, 0 failed (= baseline; no UI code touched).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 416
  ui_smoke_tests:         81
  ui_tests:               306
```

# Architecture

- **One lowering pipeline, two entry ramps.** Formulas 1/2 ride the existing
  catalogue `Sellmeier` model and its exact `toEpsAxis` lowering (the constant
  c₀ absorbed as a resonance-free term A = c₀, B = 0 — exact for λ > 0);
  formulas 3–7 build raw `EpsAxisDispersion` term data directly (the
  `SumOfTerms` shape). No formula math is re-derived where the catalogue
  already encodes it, and no parallel eps type appears.
- **ε-shaped vs n-shaped axes.** Polynomial (3) and the RII formula (4) define
  n² → `ComplexEps` (whose `complexIndex` takes √ε); Cauchy (5), gases (6) and
  Herzberger (7) define n → `RealNK` with k = the zero formula — matching the
  `Dispersion.fs` axis semantics rather than forcing one encoding.
- **Exact rational reduction, no sampling.** The formula-4 resonant shape
  c·x^p/(x² − a) long-divides by (x² − a) (x² ≡ a) into a polynomial quotient
  plus a residual pole; an odd residual splits at the ±√a simple poles via
  `Complex.Sqrt` (conjugate poles for a < 0). The gases oscillator reduces by
  the same identity family (c/(d − x⁻²) = c/d + (c/d²)/(x² − 1/d)). The tests
  pin these against the published formulas computed inline.
- **Errors as values, typed.** `UnsupportedFormula of formulaNumber : int *
  reason : string` carries its diagnostic payload; it gates BEFORE coefficient
  parsing (a formula-8 page without coefficients is still typed unsupported)
  and also types non-integer exponents (not representable as finite term data —
  refusing beats a silently wrong lowering). No exhaustive `ImportError` match
  exists outside the module, so the new case is non-breaking.
- **The units discipline holds.** The term data's `wavelengthScale` is
  `toMeters Micrometer 1.0` and the segment interval comes from
  `toWaveLength Micrometer` — the sole `Units` seam; the only new literal is
  Herzberger's published 0.028 µm² pole (a formula constant, not a conversion).

# Deferred

- Anisotropic RII pages (ordinary/extraordinary `DATA` blocks on one page):
  the importer still reads the first formula/tabulated block as an isotropic
  entry — no slice has asked for multi-block pages yet.
- Formula 9 (Exotic) is typed unsupported per the slice mandate even though its
  shifted-pole rational form is in principle finite-term-representable; lifting
  it later is a non-breaking change (the typed error simply disappears).
- Real exponents (λ^2.5) in formulas 3/4/5 remain typed unsupported — finite
  term data cannot carry them; a closure-backed fallback tier was consciously
  NOT added (it would silently reintroduce view-only imports).
- Tabulated imports stay closure-backed (view-only): linear interpolation over
  samples is not finite term data.

# Gotchas

- **0^0 = 1.0 in .NET**: formula 4 pads absent coefficients with zeros, so the
  amplitude-zero check must run before `resonanceBase ** resonancePower` or the
  padding fabricates a pole at x² = 1. The check is in place and the padding is
  covered by the fixture (11 of 17 coefficients supplied).
- **`unsupportedReason` gates before the coefficients line lookup** — reorder
  and a formula-8 page missing coefficients degrades to `MalformedYaml`.
- **Zero-amplitude coefficient pairs drop before the integer-exponent check**,
  so zero padding can never trip the non-integer-exponent error.
- The `sellmeierAxis` `NotAFiniteTermSum → MalformedYaml` branch is unreachable
  (a Sellmeier model always lowers) but keeps the match total.
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (the 015–024 drift); the real file is under
  `src\ai_strategy_generator\multistep\`.
- `.manifest.state.json` (modified) and the untracked `.claude/` folder are the
  arc-runner's / harness's own files — left alone, as in slices 001–024.

# Changelog

- 2026-07-06 — slice 025: RII formula import lowered to editable term data
  (spec 0033 step 025): importRefractiveIndexInfo parses formulas 2–7
  (Sellmeier-2, polynomial, the RII formula, Cauchy, gases, Herzberger) beyond
  tabulated + formula 1, each lowered — through the catalogue Sellmeier
  DispersionModel (1/2, constant absorbed as a resonance-free term) or directly
  to Dispersion.fs term data (3–7; ComplexEps for the ε-shaped 3/4 with exact
  quotient+pole reduction of c·x^p/(x²−a), RealNK with k = 0 for the n-shaped
  5/6/7) — to an EpsAxisDispersion in an EpsWithDispValue-backed
  MaterialComplexity with properties = complexity.toProperties (formula imports
  EDITABLE; formula 1 unified onto the same route, evaluated index preserved
  exactly); segments carry the page's wavelength_range (µm) through the Units
  seam; formulas 8/9, unknown numbers and non-integer exponents return the NEW
  typed UnsupportedFormula (formulaNumber, reason) ImportError (gated before
  coefficient parsing); tabulated/CSV paths untouched; +9 fixture-driven facts
  incl. both acceptance criteria (constructor tests 407 → 416); all suites
  green.
- 2026-07-06 — slice 024: Materials + Library workbench bays WIRED (spec 0033
  step 024): BayNames.materials "Materials" + BayNames.library "Library" added
  to the Main ribbon between Selector and Experiments; MaterialProxy +
  SampleProxy threaded through initWith/initMainWith; per-render re-query
  projections; Add/Edit via the injected EditorLaunchers seam; confirm-gated
  Remove; View panels (n/k chart + band view); +12 pure and +5 headless tests
  (ui-tests 294 → 306, ui-smoke 76 → 81); all suites green.
- (earlier rounds: see slice 024's state-of-the-world changelog.)
