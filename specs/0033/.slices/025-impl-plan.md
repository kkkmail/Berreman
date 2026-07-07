# Impl plan — spec 0033, slice 025 (IMPLEMENT: RII formulas 2–7 import, editable term data)

## Goal

Extend `MaterialImport.importRefractiveIndexInfo` beyond tabulated + formula 1:
parse refractiveindex.info formulas 2 (Sellmeier-2), 3 (polynomial), 4 (the RII
formula), 5 (Cauchy), 6 (gases) and 7 (Herzberger), each lowered — through a
`DispersionModel` or raw term data (the `SumOfTerms` shape) — to an
`EpsAxisDispersion` carried in an `EpsWithDispValue`-backed `MaterialComplexity`
on the imported `MaterialEntry`, so imports are EDITABLE. Formulas 8/9 (and any
unknown number) return a NEW typed `UnsupportedFormula` case on `ImportError`.
λ stays in the µm RII convention through the sole `Units` seam.

## Approach

1. **Tests first (TDD red):** new fixtures `fixtures/formula1.yml` …
   `formula9.yml` (minimal RII-style pages, NOT real data) + one `[<Fact>]` per
   supported formula in `MaterialImportTests.fs` asserting the imported entry's
   evaluated index (`properties.epsWithDisp.getEps`) matches the published
   formula computed inline at two sample wavelengths, plus the
   editable-complexity shape (`complexity = Some`, eps `EpsWithDispValue`);
   one fact asserting formulas 8 and 9 produce
   `Error (UnsupportedFormula (8|9, _))`. Red = FS0039 on the new case.
2. **Production:** in `MaterialImport.fs`
   - `ImportError` += `UnsupportedFormula of formulaNumber : int * reason : string`.
   - `tryFormulaNumber` parses the `type: formula N` line (any N).
   - Per-formula lowering to `EpsAxisDispersion` (all coefficients in µm,
     `wavelengthScale` = metres/µm via `toMeters Micrometer 1.0`):
     - f1/f2 → the catalogue `Sellmeier` model (constant c₀ absorbed as a
       resonance-free term; f1 squares the resonances) → `toEpsAxis` (exact).
     - f3 → `ComplexEps` monomial terms (ε directly); integer exponents only.
     - f4 → `ComplexEps`: constant + two resonant terms c·x^p/(x²−a) reduced
       exactly (polynomial quotient + residual pole; odd residual splits at
       ±√a) + monomial tail.
     - f5 → `RealNK` (n = const + monomials, k = 0); integer exponents only.
     - f6 → `RealNK`: c/(d−x⁻²) = c/d + (c/d²)/(x²−1/d) exactly (d = 0 → −c·x²).
     - f7 → `RealNK`: the fixed 0.028 µm² Herzberger pole at powers −1/−2 +
       even monomials.
   - Entry assembly: fresh `MaterialId`, single-segment
     `IsotropicDispersive` tree over the page's `wavelength_range` (µm, via
     `toWaveLength Micrometer`; absent → zero nominal interval),
     `complexity = Some { eps; magnetic = None; active = None }`,
     `properties = complexity.toProperties`.
   - Formula 1 UNIFIES onto the same route (evaluated index preserved exactly;
     entry name/description byte-identical) — the bespoke closure goes away and
     formula-1 imports become editable like the rest. Recorded as a decision.
   - Tabulated + CSV paths untouched (interpolation is not finite term data).

## Files to modify

- `OpticalConstructor.Storage/MaterialImport.fs` (production)
- `OpticalConstructor.Tests/MaterialImportTests.fs` (+8 facts)
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (fixture Content)
- `OpticalConstructor.Tests/fixtures/formula{1..9}.yml` (new fixtures)

## Risks

- Adding an `ImportError` case: checked — no exhaustive match on it outside the
  module (`Report.fs` stringifies the whole error), so this is non-breaking.
- Partial-fraction reduction for f4's odd-exponent resonant terms: verified
  algebraically (x^p ≡ a^⌊p/2⌋·x^(p mod 2) mod (x²−a)); tests pin it at two
  wavelengths against the published formula.
- Non-integer exponents (f3/f4/f5 allow real exponents in the wild) are not a
  finite term sum → typed `UnsupportedFormula` with a diagnostic reason, never
  a silent wrong lowering.
- Baselines to not regress: constructor-unit-tests 407 (going to 415),
  unit-tests 119, ui-smoke 81, ui-tests 306.
