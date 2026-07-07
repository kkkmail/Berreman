# Impl plan — spec 0033, slice 011 (toEpsAxis lowering, SumOfTerms, AnisotropicModel removal)

## Goal

Lower the editor-facing `DispersionModels.DispersionModel` catalogue to the
serializable per-axis term data `EpsAxisDispersion` (slice 008,
`Berreman/Berreman/Dispersion.fs:306`), add the raw `SumOfTerms` escape hatch,
remove `AnisotropicModel` (superseded by `EpsDispersiveValue`), and re-point
the `toOpticalProperties` / `toAnisotropicOpticalProperties` surface at
`EpsWithDispValue.toEpsWithDisp` (keeping `isotropicProperties` as the single
vacuum-μ/ρ wrapper). Only `OpticalConstructor.Domain/DispersionModels.fs` and
`OpticalConstructor.Tests/DispersionModelsTests.fs` are touched.

## Three letter-vs-mathematics conflicts and their resolutions

The slice how-to (and preliminary spec §6.1) contain three prescriptions the
shipped slice-007/008 core cannot satisfy literally; each is resolved to the
mathematically exact nearest form and recorded in the impl-log Gotchas:

1. **Sellmeier → `RealNK`** is impossible: `RealNK.complexIndex` reads the n
   formula's value directly and `DispersionFormula` is a finite term sum — it
   cannot apply the √ in `n = √(1 + Σ)`. `ComplexEps.complexIndex` computes
   exactly `√ε` — so Sellmeier lowers to `ComplexEps` with **real-valued**
   terms `1 + ΣBᵢ + Σ Bᵢcᵢ/(x²−cᵢ)` (the oscillator identity the slice
   names), and the √(1 + Σ) the slice asks for happens in `complexIndex`.
2. **"ComplexEps wrapping evaluate" for TaucLorentz/GaussianOscillator** is
   impossible: `ComplexDispersionFormula` is pure data, a closure cannot
   inhabit it, and the models are transcendental ("not finite term sums", the
   slice's own words). Resolution: `toEpsAxis` returns
   `Result<EpsAxisDispersion, EpsAxisLoweringError>`; the transcendental cases
   are a typed `NotAFiniteTermSum` error, and `toOpticalProperties` keeps them
   fully working by wrapping `evaluate` in the engine `EpsWithDisp` closure —
   the preliminary spec's "they stay named cases evaluated directly; both
   routes end at `WaveLength -> ComplexRefractionIndex`".
3. **Oscillator models are conventionally tabulated in eV**, but the core
   formula variable is linear in λ (`x = λ_m / wavelengthScale`,
   `Dispersion.fs:239`) — an eV abscissa is reciprocal (`E = k/x`). Rather
   than erroring on the conventional case, the lowering is exact for both
   abscissa kinds: linear units emit the spec's literal inverse terms; eV/cm⁻¹
   substitute `a = k/x` and reduce each oscillator to
   `s·x²/(c₀ + c₁x + c₂x²)`, lowered exactly by partial fractions (simple
   poles / double pole / long division / plain polynomial). Scales and the
   reciprocal numerator are derived through the `Units` seam (§D.11), no
   literal factors.

## Changes — DispersionModels.fs

- `DispersionModel` gains `| SumOfTerms of EpsAxisDispersion` (raw escape
  hatch; identity under `toEpsAxis`).
- `wavelengthUnitOf` → `Meter` for `SumOfTerms` (the terms embed their own
  metres-per-coefficient-unit scale); `thermoOpticOf` → `None`; `baseIndex`
  delegates to `EpsAxisDispersion.complexIndex`.
- New private `AbscissaKind` (`LinearInX` | `ReciprocalInX`) +
  `abscissaKindOf` deriving scale/numerator via `Units.toMeters/fromMeters`.
- New private term builders + `rationalXSquaredTerms` (exact partial
  fractions for `s·x²/(c₀+c₁x+c₂x²)`).
- New `EpsAxisLoweringError = NotAFiniteTermSum of reason : string`.
- New `toEpsAxis : DispersionModel -> Result<EpsAxisDispersion, EpsAxisLoweringError>`
  lowering: ConstantNK → RealNK constants; Cauchy → RealNK Laurent (linear)
  or plain polynomial (reciprocal), k = zero formula; Sellmeier → ComplexEps
  real terms; Lorentz/Drude → ComplexEps inverse terms (linear) or partial
  fractions (reciprocal); SumOfTerms → identity; TaucLorentz/Gaussian →
  `Error`.
- New `toEpsValue : DispersionModel -> Result<EpsWithDispValue, EpsAxisLoweringError>`:
  ConstantNK short-circuits to `EpsWithoutDispValue (IsotropicAbsorbing …)`
  (keeps the no-closure-overhead guarantee); lowerable models become a
  single-segment `IsotropicDispersive` tree (topmost segment extrapolates, so
  one nominal segment covers all λ).
- `toOpticalProperties` re-pointed: `toEpsValue → .toEpsWithDisp →
  isotropicProperties`, transcendental fallback wraps `evaluate`.
- `AnisotropicModel` **deleted**; `toAnisotropicOpticalProperties` becomes
  `EpsWithDispValue -> OpticalPropertiesWithDisp` (`toEpsWithDisp` through the
  same wrapper). `uniaxialEps`/`biaxialEps` stay (documented engine-constructor
  mapping helpers, still pinned by the AC-D5 test).

## Changes — DispersionModelsTests.fs

- AC-D5 uniaxial test rebuilt on `EpsWithoutDispValue (UniaxialAbsorbing …)`
  plus a per-axis dispersive `UniaxialDispersive` segment-tree assertion.
- New AC-B5 grid tests (toEpsAxis complexIndex ≡ evaluate on a sampled
  400–800 nm grid, tol 1e-8): Sellmeier µm + eV, Cauchy µm + eV, ConstantNK,
  Lorentz µm (linear literal), Lorentz eV (two oscillators), Lorentz eV
  critical damping (double-pole branch), Drude eV, Drude eV γ=0 (polynomial
  branch).
- Typed-error tests: TaucLorentz/Gaussian → `NotAFiniteTermSum` and their
  `toOpticalProperties` closure fallback ≡ evaluate on the grid.
- SumOfTerms tests: identity lowering, `evaluate` delegates to
  `complexIndex`, unit/thermo-optic totality choices pinned.
- `toEpsValue` shape tests (ConstantNK constant short-circuit, Sellmeier
  single-segment tree, TaucLorentz error).

## Risks

- JSON round-trip test deserializes `DispersionModel`; the new case's payload
  (`EpsAxisDispersion` → records of double/Complex) is only reflected lazily
  by FSharp.SystemTextJson, so the existing Sellmeier round-trip should stay
  green — verified by running the suite.
- Partial-fraction residue math: self-verified by the eV grid-equality tests
  including the degenerate branches.
- Negative int literals as function args need parens (`(-1)`), pipe-into-match
  precedence — known 010 gotchas.

## Verification

TDD: add the new tests first, confirm red (FS0039 on `toEpsAxis` /
`SumOfTerms`), implement, re-run green. Then the full local advisory runs:
solution build, BerremanTests, OpticalConstructor.Tests, ui-smoke, ui-tests;
logs captured under `specs/0033/.artifacts/011-*.log`.
