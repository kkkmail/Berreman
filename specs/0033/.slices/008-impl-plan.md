# Slice 008 — impl plan

## Goal

IMPLEMENT (no contract id): add the serializable eps tree to
`Berreman/Berreman/Dispersion.fs` (spec 0033 Part B), consuming the slice-007
formula blocks and BUILDING the engine's `EpsWithDisp` through the existing
`Eps` constructors — the engine unions stay byte-identical (§0.1). Tests in
`BerremanTests` pin all six constant cases against directly-constructed `Eps`,
the segment-selection semantics (overlap → first, out-of-range → topmost
extrapolates), and the uniaxial / biaxial dispersive closures against per-axis
analytic values.

## Shapes (pinned by the slice)

- `ConstantEpsValue` — descriptive six-case DU, never a bare `Eps`:
  `IsotropicTransparent of RefractionIndex`,
  `IsotropicAbsorbing of ComplexRefractionIndex`,
  `UniaxialTransparent of ordinary : RefractionIndex * extraordinary : RefractionIndex`,
  `UniaxialAbsorbing of ordinary : ComplexRefractionIndex * extraordinary : ComplexRefractionIndex`,
  `BiaxialTransparent of nx : RefractionIndex * ny : RefractionIndex * nz : RefractionIndex`,
  `BiaxialAbsorbing` (complex triple). `member toEps : Eps` builds through
  `Eps.fromRefractionIndex` / `Eps.fromComplexRefractionIndex`
  (MaterialProperties.fs:76,79,87,95); **uniaxial maps to the triple
  (n_o, n_e, n_o)** — the slice pins it and it matches the langasite precedent
  `Dispersive.fs:52-55` (nVal1 = ordinary, nVal2 = extraordinary,
  nVal3 = ordinary).
- `EpsAxisDispersion` = `RealNK of n : DispersionFormula * k : DispersionFormula`
  (k = the zero formula — empty term list — for transparent media)
  `| ComplexEps of ComplexDispersionFormula`, with
  `member complexIndex : WaveLength -> ComplexRefractionIndex`:
  RealNK evaluates `n + i·k`; ComplexEps evaluates `sqrt eps` (F# `sqrt`
  resolves on `Complex` — the `OpticalTransformation.SquareRoot` precedent,
  MaterialProperties.fs:30).
- `IsotropicEpsSegment` / `UniaxialEpsSegment` / `BiaxialEpsSegment` — records,
  each `wavelengthInterval : WaveLengthInterval` SHARED across that segment's
  axes plus one `EpsAxisDispersion` per axis (`dispersion`;
  `ordinaryDispersion` / `extraordinaryDispersion`;
  `xDispersion` / `yDispersion` / `zDispersion`).
- `EpsDispersiveValue` = `IsotropicDispersive of IsotropicEpsSegment list`
  `| UniaxialDispersive of …` `| BiaxialDispersive of …` (homogeneous segment
  lists), with `member getEps : WaveLength -> Eps` — selects the segment via a
  private generic `selectSegment` (segments FIRST in the parameter order so the
  interval-projection lambda's type is already pinned): the FIRST segment whose
  interval covers the wavelength (inclusive endpoints, compared on
  `WaveLength.value` — the DU's structural comparison would order by case tag,
  not by length), top-of-list wins on overlap; when none covers, the topmost
  (head) segment extrapolates — no clamps, no validation (§0.4).
- `EpsWithDispValue` = `EpsWithDispValue of EpsDispersiveValue`
  `| EpsWithoutDispValue of ConstantEpsValue` with
  `member toEpsWithDisp : EpsWithDisp` — the constant case short-circuits to
  `EpsWithoutDisp` via `toEps`; the dispersive case wraps `getEps` in a
  `WaveLength -> Eps` closure under `EpsWithDisp`.

Placement: appended at the end of the `Dispersion` module after the slice-007
formula blocks — no existing line changes, no new opens (`Complex`,
`RefractionIndex`, `Eps` are already in scope).

## Files to modify

1. `Berreman/BerremanTests/EpsWithDispValueTests.fs` (NEW) + a
   `<Compile Include … />` entry in `BerremanTests.fsproj` after
   `DispersionFormulaTests.fs` — written FIRST for the TDD red (`FS0039` on the
   missing production types counts as red per the worker base). 11 facts:
   - Six constant-case facts: each `ConstantEpsValue` routed through
     `EpsWithoutDispValue … .toEpsWithDisp.getEps probe` equals the
     directly-constructed engine `Eps` (the uniaxial facts pin the
     (n_o, n_e, n_o) triple), via `MatrixComparison.verifyMatrixEqualityEps`.
   - Three segment-selection facts on an isotropic dispersive value with two
     segments — topmost 400–700 nm with the wavelength-DEPENDENT
     n(x) = 1.4 + 0.05·x, second 600–900 nm with constant n = 2.0: at 650 nm
     (overlap) the FIRST wins; at 800 nm (second only) the second is selected;
     at 1200 nm (none) the topmost extrapolates to 1.46 — distinguishable from
     clamping at 700 nm (1.435) and from nearest-segment (2.0).
   - Uniaxial dispersive: per-axis Cauchy n_o / n_e (zero k), Eps at 500 nm
     equals `Eps.fromComplexRefractionIndex (n_o, n_e, n_o)` with analytic
     values.
   - Biaxial dispersive: x = RealNK with nonzero constant k, y = transparent
     constant, z = `ComplexEps` (eps = 4 + 0.5i) — the z expectation computed
     from the closed principal-square-root form (independent of
     `Complex.Sqrt`), Eps equals the per-axis analytic triple.
2. `Berreman/Berreman/Dispersion.fs` — the eps tree appended (one new section
   banner, `///` docs on every type/member, explicit concrete signatures).

## Risks

- Record-literal inference: the three segment records share the
  `wavelengthInterval` field name — tests annotate every binding (the slice-007
  gotcha precedent).
- `selectSegment` on an EMPTY segment list throws (`List.head`) — §0.4 forbids
  validation and the engine closure signature `WaveLength -> Eps` is pinned; a
  degenerate empty list is a construction-time bug, recorded in Gotchas.
- Coverage endpoints are INCLUSIVE (`lower.value <= w.value <= upper.value`) —
  the natural reading of "covers"; recorded as the chosen interpretation.
- `count_at_least` baselines: berreman_unit_tests 89 → 100 (+11); the other
  three suites are untouched (350 / 54 / 249).
- LF-only endings on every touched file (BOM of `Dispersion.fs` preserved).
