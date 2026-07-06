# Slice 007 — impl plan

## Goal

IMPLEMENT (no contract id): add the generic serializable dispersion formula
blocks to `Berreman/Berreman/Dispersion.fs` — `WaveLengthInterval`,
`DispersionTerm`, `DispersionFormula` (with `evaluate : WaveLength -> double`),
and the `ComplexDispersionTerm` / `ComplexDispersionFormula` mirrors over
`System.Numerics.Complex` — pure data + evaluation only; the engine
`…WithDisp` unions in that file stay byte-identical (spec 0033 §0.1). Tests in
`BerremanTests` evaluate a Cauchy-shaped Laurent polynomial and a
Sellmeier-shaped inverse term against hand-computed values with the existing
`MatrixComparison.allowedDiff` tolerance — no new epsilon logic.

## Shapes (pinned by the slice)

- `WaveLengthInterval` — record `{ lower : WaveLength; upper : WaveLength }`.
  "An interval is just an interval": NO ordering validation, NO clamping,
  NO contains-helper (§0.4 forbids segment validation; covering-segment
  semantics belong to later Part B steps).
- `DispersionTerm` — record `{ lambda : double; coefficients : double array;
  power : int; multiplier : double }` meaning
  `multiplier * (Σ_k coefficients.[k] * (x - lambda)^k) ^ power` where `x` is
  the wavelength already reduced to the coefficient unit. Inner polynomial by
  Horner; outer integer power by `pown` (supports negative exponents — the
  Laurent/inverse cases).
- `DispersionFormula` — record `{ terms : DispersionTerm list;
  wavelengthScale : double (* metres per coefficient unit *) }` with
  `member evaluate : WaveLength -> double`:
  `x = w.value / (wavelengthScale * 1.0<meter>)` (the canonical wavelength is
  metres — `WaveLength.value`), then sum of `term.evaluate x`.
- `ComplexDispersionTerm` / `ComplexDispersionFormula` — field-for-field
  mirrors with `Complex` replacing `double` on the value fields (`lambda`,
  `coefficients`, `multiplier`); `power : int` and
  `wavelengthScale : double` stay real (an exponent and a unit scale);
  `evaluate : WaveLength -> Complex`. Complex sum via explicit `fold` over
  `Complex.Zero` (no reliance on generic `sumBy` trait resolution against
  `Complex`).

Placement: appended at the end of the `Dispersion` module — no existing line
of the file changes except the `open` list (`System.Numerics` for `Complex`,
`Constants` for the `meter` measure used by the reduction).

## Files to modify

1. `Berreman/BerremanTests/DispersionFormulaTests.fs` (NEW) + a
   `<Compile Include … />` entry in `BerremanTests.fsproj` (before
   `OptimizationTests.fs`) — written FIRST for the TDD red (`FS0039` on the
   missing production types counts as red per the worker base):
   - Cauchy-shaped Laurent polynomial `A + B/x² + C/x⁴` (coefficient unit µm,
     `wavelengthScale = 1.0e-6`) at 500 nm — three terms: a constant
     (`power = 1`), and two `power = -1` monomial denominators; expected value
     computed independently inline as `a + b/x² + c/x⁴`.
   - Sellmeier-shaped inverse term `1 + B·x²/(x² − C)` expressed in term form
     via the partial fraction `(1 + B) + B·C·(x² − C)^(−1)` (BK7's first
     Sellmeier coefficients, µm²) at 587.6 nm — expected value computed
     independently inline as the direct rational form, which genuinely
     cross-checks the algebra against `evaluate`'s Horner/pown path.
   - A shifted-center positive-power term `m·(c₀ + c₁(x − λ₀))²` to pin the
     `(x - lambda)` centring and the outer power.
   - A complex mirror check (complex multiplier/coefficients, one inverse
     term) against an inline `Complex` expectation, difference measured by
     `.Magnitude`.
   - All comparisons: `abs (result − expected)` (or `.Magnitude`) `<`
     `MatrixComparison.allowedDiff`, FluentAssertions `BeLessThan` — the
     exact precedent of `MuellerMatrixTests.fs:24` / `OptimizationTests.fs:260`.
2. `Berreman/Berreman/Dispersion.fs` — the five types above appended after
   the engine unions; `///` docs state the term meaning and the
   metres-per-coefficient-unit semantics.

## Risks

- `pown poly power` with `poly = 0.0` and negative `power` → `infinity`;
  intentionally NOT guarded (§0.4 forbids clamping/validation; a pole is a
  pole).
- CLAUDE.md's elevate-every-primitive rule vs the slice's pinned raw-`double`
  fields: the slice spec pins the field types explicitly (they are
  dimensionless model coefficients whose unit is defined by
  `wavelengthScale`); the record itself is the named elevation. Recorded in
  Gotchas.
- Generic-math trait resolution on `Complex` (`sumBy`/`GenericZero`) is
  avoided via explicit fold — deterministic under `--warnaserror+:25`.
- LF-only endings on every touched file; `unit-tests` `count_at_least`
  baseline is 84 — new tests only add.
