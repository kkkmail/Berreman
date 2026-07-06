# Impl log — spec 0033, slice 012 (AC-B6: ForouhiBloomer / BrendelBormann)

## Progress

- [x] Read task file, worker system prompt, project prompt, slice spec,
      steering .spec-md, slice-011 state-of-the-world (recorded lowering
      decision), DispersionModels.fs, Dispersion.fs, Units.fs, existing tests.
- [x] Collected published reference data (see Decisions).
- [x] Red: 5 AC-B6 tests added; build fails FS0039 on the missing
      `ForouhiBloomer` / `BrendelBormann` constructors
      (`012-red-tdd.log`).
- [x] Implementation: two coefficient records, two DU cases,
      `wavelengthUnitOf` / `thermoOpticOf` lines, `baseIndex` evaluation,
      private Weideman Faddeeva helper, `toEpsAxis` typed-error cases.
- [x] Green: constructor tests 364 → 369, first run, no test edits after red.
- [x] Full advisory gate runs — all five pass (see Testing state).
- [x] State-of-the-world written.

## Decisions

1. **Lowering route.** The slice letter's "lower both via toEpsAxis as
   ComplexEps wrapping evaluate" cannot type-check (`ComplexEps` carries pure
   term data — slice 011 impl-log Decision 2, state-of-the-world
   Architecture). Following slice 011's recorded deferral ("they will join
   the same typed-error transcendental route"), ForouhiBloomer and
   BrendelBormann surface `Error (NotAFiniteTermSum …)` from `toEpsAxis`;
   `toOpticalProperties` keeps them fully working by wrapping `evaluate` in
   the engine `EpsWithDisp` closure (the pre-existing Error branch — no new
   code needed there). Pinned by the AC-B6 typed-error grid test.
2. **Forouhi–Bloomer form.** Original 1986 five-parameter amorphous form with
   the band-gap step in k (k = 0 for E ≤ Eg — the Θ ∝ (ħω−Eg)² derivation;
   Horiba TN13 eq. 4.2 carries the same step; same piecewise shape as the
   existing TaucLorentz case). n uses the closed Kramers–Kronig form with
   B₀/C₀/Q (TN13 eqs. 5.2/5.3 = Wikipedia's). The record stores n∞ (the
   paper's parameterisation); the Horiba set tabulates ε∞, so the test passes
   `nInf = sqrt 3.453`.
3. **Brendel–Bormann form.** Rakić 1998 equation set with the constant 1
   baked in (the published model has no ε∞ parameter — the interband χⱼ
   supply the high-frequency behaviour); signs in this codebase's
   Im ε ≥ 0 / k ≥ 0 convention, matching the existing Lorentz/Drude cases
   and the refractiveindex.info Au calculation script:
   ε(E) = 1 − f₀ωp²/(E² + iΓ₀E) + Σⱼ i√π·fⱼωp²/(2√2·αⱼσⱼ)·[w(zₐ)+w(z_b)],
   αⱼ = √(E² + iΓⱼE) principal root, zₐ,b = (αⱼ ∓ ωⱼ)/(√2σⱼ).
4. **Faddeeva.** Private Weideman-1994 rational series, N = 24, valid on
   Im z ≥ 0 (all arguments here have Im ≥ 0 because αⱼ is the principal —
   first-quadrant — root of E² + iΓⱼE). The coefficients are computed once at
   module init from the defining cosine transform (θₖ = kπ/M sampling of
   e^(−t²)(L²+t²) under t = L·tan(θ/2)) — no hand-copied coefficient table.
   Validated three ways: the four RII gold reference points, the
   no-oscillator = Drude equality (1e-12), and the σ→0 oscillator = Lorentz
   degeneracy (1e-4, deviation is O(σ²)).
5. **Reference data.**
   - FB: Horiba TN13 "Forouhi-Bloomer alias Amorphous Dispersion Formula"
     (implements the 1986 formula; cites Forouhi & Bloomer, Phys. Rev. B 34,
     7018 (1986) as its Ref. 2) — published a-Si set ε∞ = 3.453, A = 0.865,
     B = 6.703, C = 13.237, Eg = 0.906 (S.R. 0.6–5 eV), with the note's
     displayed evaluation n = 3.182, k = 0.000 at E = 0.6 eV. The closed form
     gives n(0.6) = 3.1818311 (within 1.7e-4 of the displayed 3-decimal
     value; asserted at 5e-4). Above the gap the test pins the closed form
     itself: Q = 1.4157852, B₀ = −2.4292240, C₀ = 14.1313281 →
     n(3 eV) = 5.0742287, k(3 eV) = 1.7823699 (computed to 10 digits before
     coding; asserted at 1e-6).
   - BB: Rakić et al. 1998 Au BB parameters (ωp = 9.03 eV, f₀ = 0.770,
     Γ₀ = 0.050; five oscillators f = 0.054/0.050/0.312/0.719/1.648,
     Γ = 0.074/0.035/0.083/0.125/0.179, ω = 0.218/2.885/4.069/6.137/27.97,
     σ = 0.742/0.349/0.830/1.246/1.795) and the CC0 refractiveindex.info
     Au/Rakic-BB tabulated rows computed from them:
     λ = 0.49712 µm → (0.89849, 1.8312); 0.62346 µm → (0.20533, 3.1621);
     1.0129 µm → (0.28761, 6.2718); 2.0306 µm → (0.86294, 12.953).
     Tolerance 2e-3 absolute (covers the table's 5-significant-digit rounding
     of both λ and n/k, and the 1.6e-6 relative difference between the seam's
     `evNmProduct = 1239.84` and the script's h·c = 1239.8419 nm·eV).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
  — `ForouhiBloomerCoefficients` / `BrendelBormannCoefficients` records (the
  existing coefficients + `wavelengthUnit` + `thermoOptic option`
  convention), the two `DispersionModel` cases (placed after
  `GaussianOscillator`, before the `ConstantNK`/`SumOfTerms` escape hatches),
  `wavelengthUnitOf` / `thermoOpticOf` arms, the private
  `faddeevaTermCount` / `faddeevaScale` / `faddeevaCoefficients` / `faddeeva`
  helpers, the two `baseIndex` evaluation arms, and the two `toEpsAxis`
  typed-error arms (+ doc-comment updates).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`
  — new AC-B6 section: FB a-Si reference values; BB Rakić-gold reference
  values; BB-intraband = Drude equality; narrow-BB = Lorentz degeneracy;
  FB/BB typed lowering error + `toOpticalProperties`-wraps-`evaluate` grid
  equality (also covers `wavelengthUnitOf`/`thermoOpticOf` for both).

All matches over `DispersionModel` live inside `DispersionModels.fs`
(verified by grep before editing), so no other file needed the new cases.

## Testing state

TDD: red first (FS0039 on the missing constructors, `012-red-tdd.log`), then
the implementation, then green on the first run — the tests were not modified
after red. All five gates in the slice roster pass in the worker's local
ADVISORY runs (the arc-runner gate engine re-runs them authoritatively after
exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors
  (94 warnings, all pre-existing MSB3277/FS1125 noise from Ui/SeriesData).
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched this slice).
- `constructor-unit-tests` — 369 passed, 0 failed (+5 over the 364 baseline).
- `ui-smoke` — 54 passed (= baseline).
- `ui-tests` — 249 passed (= baseline).

## Artifacts

- `specs/0033/.artifacts/012-red-tdd.log` — red build (FS0039).
- `specs/0033/.artifacts/012-build.log` — solution build.
- `specs/0033/.artifacts/012-unit-tests.log` — BerremanTests run.
- `specs/0033/.artifacts/012-constructor-unit-tests.log` — constructor tests.
- `specs/0033/.artifacts/012-ui-smoke.log` — ui-smoke run.
- `specs/0033/.artifacts/012-ui-tests.log` — ui view tests.

## Gotchas

- **The slice letter's "ComplexEps wrapping evaluate" is unimplementable as
  written** — already established and recorded in slice 011; this slice
  followed the recorded typed-error route rather than re-litigating it.
  ForouhiBloomer is *piecewise* rational (band-gap step in k), so it is
  honestly not a finite term sum; even without the step its n(E) has
  complex-conjugate poles that the real `RealNK` term data cannot carry.
- **Rakić's paper prints its equations in the e^{+iωt} convention** (Im ε < 0
  for absorption). The implementation conjugates to this codebase's k ≥ 0
  convention — identical to what the existing Lorentz/Drude cases already do
  and to the refractiveindex.info calculation script, whose CC0 tabulation
  the test reproduces. Verify against those, not the paper's raw sign.
- **The Storage JSON-schema `kind` enum**
  (`…Storage/schema/optical-constructor-project.schema.json:100`) does not
  list `ForouhiBloomer` / `BrendelBormann` — the same pre-existing situation
  as slice 011's `SumOfTerms`. Storage is outside this slice's `touches`;
  recorded as deferred, no test exercises schema validation of these cases.
- **The Weideman half-plane restriction (Im z ≥ 0) is a real precondition**
  of the private `faddeeva`; it holds by construction because `Complex.Sqrt`
  returns the principal (first-quadrant) root of E² + iΓE for Γ, E ≥ 0. Do
  not reuse the helper for arbitrary z without the reflection identity.
- `.manifest.state.json` shows as modified in `git status` — the
  arc-runner's own file (same as slices 001–011), left alone.
