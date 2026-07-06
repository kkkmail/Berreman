# Impl log — spec 0033, slice 011

## Progress

- [x] Recon: slice spec, arc prompts, Dispersion.fs (slice 007/008 blocks),
      DispersionModels.fs, Units.fs, consumers (tests :62/:70; MaterialImport /
      Report use only `isotropicProperties` — unaffected).
- [x] Red: 14 new AC-B5 facts + rebuilt AC-D5 fact added to
      `DispersionModelsTests.fs`; build failed on the missing production
      symbols (`FS0039` on `toEpsAxis` / `toEpsValue` / `SumOfTerms` /
      `NotAFiniteTermSum`, `FS0001` on the `AnisotropicModel` →
      `EpsWithDispValue` re-pointing) — `011-red-tdd.log`.
- [x] Green: `DispersionModels.fs` lowering implemented; constructor suite
      364/364.
- [x] Local advisory runs of every gate in the slice roster (see Testing
      state); logs in `.artifacts/011-*.log`.
- [x] State-of-the-world written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DispersionModels.fs`
  — `SumOfTerms` case; `wavelengthUnitOf`/`thermoOpticOf`/`baseIndex` extended;
  new lowering block (`EpsAxisLoweringError`, private `AbscissaKind` /
  `abscissaKindOf` / term builders / `rationalXSquaredTerms`, `toEpsAxis`);
  new `toEpsValue` + re-pointed `toOpticalProperties`; `AnisotropicModel`
  deleted; `toAnisotropicOpticalProperties` re-signed over `EpsWithDispValue`;
  module header doc updated.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DispersionModelsTests.fs`
  — AC-D5 uniaxial test rebuilt on the serializable value; 14 new AC-B5 facts
  (grid equality per analytic model incl. eV/µm abscissas and degenerate
  branches, typed transcendental error + closure fallback, SumOfTerms
  identity, toEpsValue shapes, toOpticalProperties tree routing).
- `specs/0033/.slices/011-impl-plan.md`, `011-impl-log.md`,
  `011-state-of-the-world.md` (worker round outputs).

## Decisions

1. **Sellmeier lowers to `ComplexEps`, not `RealNK`** (how-to letter says
   RealNK). `RealNK.complexIndex` reads the n formula's value directly and
   `DispersionFormula` is a finite term sum — it cannot express
   `n = √(1 + Σ)`. `ComplexEps.complexIndex` computes exactly `√ε`, so the
   oscillator-identity terms the slice prescribes are emitted as real-valued
   complex terms and the "n formula evaluating sqrt(1 + sum)" the slice names
   is realised by `complexIndex` itself. Grid tests prove exactness.
2. **`toEpsAxis` returns `Result<EpsAxisDispersion, EpsAxisLoweringError>`**;
   TaucLorentz/GaussianOscillator are `Error (NotAFiniteTermSum reason)`. The
   how-to's "ComplexEps wrapping evaluate" cannot type-check: `ComplexEps`
   carries the pure-data `ComplexDispersionFormula` (Dispersion.fs:263) and a
   closure cannot inhabit it — the slice's own parenthetical ("they are not
   finite term sums") and the preliminary spec §6.1 ("they stay named cases
   evaluated directly") point to the closure route, which
   `toOpticalProperties` provides by wrapping `evaluate` in the engine
   `EpsWithDisp`. Errors-as-values per CLAUDE.md; the error carries a reason.
3. **Reciprocal abscissas (eV, cm⁻¹) lower exactly** instead of erroring: the
   core formula variable is linear in λ, so an eV-tabulated oscillator
   (`E = k/x`) is re-expressed as `s·x²/(c₀+c₁x+c₂x²)` and split by exact
   partial fractions (`rationalXSquaredTerms`: simple poles, double pole,
   long division, plain polynomial). Without this, the conventional eV
   tabulation the coefficient records document would not be lowerable at all.
   Scale and numerator derive through the `Units` seam (§D.11), no literals.
4. **`toEpsValue` is the named re-pointing seam**: ConstantNK short-circuits
   to `EpsWithoutDispValue (IsotropicAbsorbing …)` (keeps the
   no-closure-overhead guarantee of the existing test), lowerable models
   become a single-segment `IsotropicDispersive` tree (topmost-segment
   extrapolation makes the nominal interval irrelevant), and
   `toOpticalProperties` = `toEpsValue` → `toEpsWithDisp` →
   `isotropicProperties`.
5. **`uniaxialEps`/`biaxialEps` kept**: the slice mandates removing only
   `AnisotropicModel`; the helpers document the engine-constructor mapping
   (AC-D5) and stay pinned by the test.
6. **`SumOfTerms` totality choices**: `wavelengthUnitOf` reports the canonical
   `Meter` (each embedded formula carries its own metres-per-coefficient-unit
   scale); `thermoOpticOf` is `None` (a raw escape hatch has no thermo-optic
   record); `baseIndex` delegates to `complexIndex`. All pinned by tests.

## Testing state

TDD red first (`011-red-tdd.log`), then green. All five gates in the slice
roster pass in the worker's local ADVISORY runs; the arc-runner gate engine
re-runs them authoritatively after exit.

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors
  (`011-build.log`).
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched) (`011-unit-tests.log`).
- `constructor-unit-tests` — 364 passed, 0 failed (+14 over the 350 baseline)
  (`011-constructor-unit-tests.log`).
- `ui-smoke` — 54 passed (= baseline) (`011-ui-smoke.log`).
- `ui-tests` — 249 passed (= baseline) (`011-ui-tests.log`).

## Artifacts

- `specs/0033/.artifacts/011-red-tdd.log` — TDD red evidence.
- `specs/0033/.artifacts/011-green-tests-build.log` — first green build.
- `specs/0033/.artifacts/011-build.log`, `011-unit-tests.log`,
  `011-constructor-unit-tests.log`, `011-ui-smoke.log`, `011-ui-tests.log` —
  advisory gate runs.

## Gotchas

- The slice how-to contains two prescriptions the slice-007/008 core cannot
  satisfy literally (Sellmeier-as-RealNK needs a √ the term data cannot
  express; "ComplexEps wrapping evaluate" would put a closure inside pure
  data). Decisions 1–2 above record the resolutions; the binding acceptance
  (grid equality for the five analytic models + `AnisotropicModel` removal)
  is met exactly.
- `Berreman.Constants` is NOT opened in `DispersionModels.fs`, so `<meter>`
  literals don't resolve there — strip units with `float` (or route through
  `Units.toWaveLength`) instead of `1.0<meter>` division.
- The `meter`-measure trap aside, negative int literals as curried arguments
  need parens (`complexPolyTerm m (-1) …`) — the slice-010 gotcha again.
- `git status` shows `.manifest.state.json` modified — the arc-runner's own
  file (same as slices 001–010), left alone. An unrelated pre-existing
  untracked `.claude/` folder also shows; not touched.
