# Impl plan — spec 0033, slice 012 (AC-B6)

## Goal

Add `ForouhiBloomer` and `BrendelBormann` cases to
`DispersionModels.DispersionModel`
(`Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`),
following the existing coefficient-record convention (coefficient scalars +
`wavelengthUnit` + `thermoOptic option`), evaluated inside the same private
`baseIndex` dispatch, with AC-B6 tests in `OpticalConstructor.Tests`
reproducing documented reference values for one published coefficient set
each (Forouhi–Bloomer 1986; Rakić et al. 1998 Au).

## Physics

- **Forouhi–Bloomer (Phys. Rev. B 34, 7018 (1986); five parameters
  n∞/A/B/C/Eg, abscissa = photon energy E):**
  `k(E) = A·(E−Eg)²/(E²−B·E+C)` above the band gap (zero below — the Θ(ω) ∝
  (ħω−Eg)² density-of-states factor), and the Kramers–Kronig closed form
  `n(E) = n∞ + (B₀·E+C₀)/(E²−B·E+C)` with `Q = √(4C−B²)/2`,
  `B₀ = (A/Q)(−B²/2 + Eg·B − Eg² + C)`, `C₀ = (A/Q)((Eg²+C)·B/2 − 2·Eg·C)`.
  Equations cross-checked against the Wikipedia overview page and the Horiba
  TN13 note (eqs. 4.2/5.2/5.3), both of which cite the 1986 paper.
- **Brendel–Bormann (Rakić, Djurišić, Elazar & Majewski, Appl. Opt. 37,
  5271 (1998)):** `ε(E) = 1 − f₀·ωp²/(E² + i·Γ₀·E) + Σⱼ χⱼ(E)` with the
  Gaussian-broadened (Voigt) oscillators
  `χⱼ = i·√π·fⱼ·ωp²/(2√2·αⱼ·σⱼ)·[w(zₐ)+w(z_b)]`, `αⱼ = √(E² + i·Γⱼ·E)`
  (principal root, first quadrant), `zₐ,b = (αⱼ ∓ ωⱼ)/(√2·σⱼ)`, `w` = the
  Faddeeva function `e^{−z²}·erfc(−iz)`. Signs are this codebase's k ≥ 0
  convention (`Im ε ≥ 0`), the exact convention of the existing Lorentz /
  Drude cases and of the refractiveindex.info calculation script for
  Au/Rakic-BB.
- **Faddeeva:** private Weideman (1994) rational series (N = 24) on
  Im z ≥ 0; the coefficients are computed once at module init from the
  defining cosine transform (no hard-coded coefficient table). Both zₐ and
  z_b always have Im ≥ 0 here, so the Im z ≥ 0 restriction is safe.

## Lowering decision

The slice letter says "lower both via toEpsAxis as ComplexEps wrapping
evaluate". Slice 011 already recorded that this phrase cannot type-check
(`ComplexEps` carries pure `ComplexDispersionFormula` term data; a closure
cannot inhabit it) and resolved it as: non-finite-term models surface the
typed `NotAFiniteTermSum` error from `toEpsAxis` and `toOpticalProperties`
wraps `evaluate` in the engine `EpsWithDisp` closure. Slice 011's
state-of-the-world explicitly defers FB/BB to "the same typed-error
transcendental route". This slice follows that recorded decision:
ForouhiBloomer (piecewise — band-gap step in k, like TaucLorentz) and
BrendelBormann (transcendental — Faddeeva) both return
`Error (NotAFiniteTermSum …)` and evaluate through the closure route.

## Files to modify

1. `…Domain/DispersionModels.fs` — two coefficient records, two DU cases,
   `wavelengthUnitOf`/`thermoOpticOf` lines, `baseIndex` evaluation (plus the
   private Faddeeva helper), `toEpsAxis` typed-error lines.
2. `…Tests/DispersionModelsTests.fs` — AC-B6 tests:
   - FB a-Si (Horiba TN13 published set for the FB-1986 formula:
     ε∞ = 3.453 → n∞ = √3.453, A = 0.865, B = 6.703, C = 13.237 eV²,
     Eg = 0.906 eV) reproduces the note's displayed n = 3.182, k = 0.000 at
     E = 0.6 eV, plus hand-derived closed-form values above the gap.
   - BB Au (Rakić 1998 BB parameters as used by refractiveindex.info:
     ωp = 9.03, f₀ = 0.770, Γ₀ = 0.050 and five (fⱼ, Γⱼ, ωⱼ, σⱼ) oscillators)
     reproduces the RII tabulated n/k (CC0) at four wavelengths spanning
     0.5–2 µm within 2e-3.
   - BB σ→0 degeneracy: a BB oscillator with tiny σ collapses to the
     matching Lorentz oscillator (s = f·ωp²) — validates the Voigt closed
     form and the Faddeeva implementation independently of the tabulated
     values.
   - FB/BB join the transcendental typed-error lowering test
     (toEpsAxis Error + toOpticalProperties wraps evaluate on the grid).

## Risks

- Faddeeva accuracy/branch mistakes → covered by the σ→0 Lorentz-degeneracy
  test and four independent RII reference points.
- Non-exhaustive-match fallout from the two new DU cases: all matches over
  `DispersionModel` live inside DispersionModels.fs (checked by grep);
  `toEpsValue` uses a wildcard.
- The Storage JSON-schema `kind` enum does not list the new cases — same
  pre-existing situation as slice 011's `SumOfTerms` (Storage is outside this
  slice's `touches`); recorded as deferred.

## Gates

build, unit-tests, constructor-unit-tests, ui-smoke, ui-tests (advisory
local runs; the arc-runner gate engine is authoritative).
