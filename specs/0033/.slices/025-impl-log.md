# Impl log — spec 0033, slice 025 (IMPLEMENT: RII formulas 2–7 import, editable term data)

## Progress

- [x] Read task file, worker system prompt, project prompt, slice spec, gates snapshot
- [x] Context: MaterialImport.fs, DispersionModels.fs (toEpsAxis/toEpsValue),
      Dispersion.fs term data, MaterialLibrary (MaterialComplexity/MaterialEntry),
      Units seam, MaterialImportTests + fixtures, Report.fs (ImportError consumer)
- [x] Impl plan written
- [x] TDD red: fixtures formula{1..9}.yml + 9 new facts written FIRST; red build
      captured to `.artifacts/025-red-tdd.log` (FS0039 × 2 naming the missing
      production symbol `UnsupportedFormula`)
- [x] Production code (see Files modified)
- [x] Gates (advisory local runs) — all five green
- [x] State of the world

## Files modified

- `OpticalConstructor.Storage/MaterialImport.fs` — the bulk:
  - `ImportError` gains the NEW typed case
    `UnsupportedFormula of formulaNumber : int * reason : string` (slice mandate;
    checked first that no exhaustive match on `ImportError` exists outside the
    module — `Report.fs` stringifies the whole error, so the addition is
    non-breaking).
  - A formula-machinery section between the tabulated helpers and
    `importRefractiveIndexInfo`: `micrometerScale` (metres/µm via
    `toMeters Micrometer 1.0` — the Units seam, no literal factor), real/complex
    constant + monomial term builders, `tryIntegerExponent`, `coefficientPairs`,
    generic `monomialTerms`, `resonantTerms` (exact reduction of c·x^p/(x²−a):
    polynomial quotient by x² ≡ a + residual pole; odd residual splits at ±√a),
    per-formula lowerings (`sellmeierAxis`, `polynomialAxis`, `riiFormulaAxis`,
    `cauchyAxis`, `gasesAxis`, `herzbergerAxis`), `unsupportedReason` /
    `lowerFormulaAxis`, `tryFormulaNumber`, `rangeInterval`, `formulaFamily`,
    `entryOfAxis`.
  - `importRefractiveIndexInfo` rewritten: `type: formula N` dispatch (N read
    from the page, unsupported numbers typed BEFORE coefficient parsing so a
    formula-8 page without coefficients still gets the typed error), formulas
    1–7 lowered to a single-segment `IsotropicDispersive` tree over the page's
    `wavelength_range` (µm) carried in an `EpsWithDispValue`-backed
    `MaterialComplexity` with `properties = complexity.toProperties`; the
    tabulated path unchanged.
- `OpticalConstructor.Tests/MaterialImportTests.fs` — +9 facts: one per
  supported formula (1–7) pinning the imported entry's evaluated index (engine
  `getEps` path) against the published formula computed inline from the fixture
  coefficients at two sample wavelengths each, plus the editable-complexity
  shape assert (`complexity = Some`, eps `EpsWithDispValue`); one fact pinning
  the parsed `wavelength_range` on the formula-2 segment; one fact asserting
  formulas 8 AND 9 produce `Error (UnsupportedFormula (8|9, _))`.
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` — the nine fixture
  `Content` entries (CopyToOutputDirectory PreserveNewest, existing style).
- `OpticalConstructor.Tests/fixtures/formula{1..9}.yml` (NEW) — minimal
  RII-style pages (NOT real material data), one per formula number.

## Decisions

1. **Formula 1 UNIFIED onto the data route** (the one judgment call): the slice
   says "extend beyond tabulated + formula 1", enumerating only 2–7, but the
   old formula-1 closure carried the comment "view-only until Part G lowers
   them to data" — this step IS that lowering, and keeping a bespoke closure
   path one `squared` away from the formula-2 lowering would be pure
   duplication. Formula 1 now rides the same Sellmeier→`toEpsAxis` route:
   evaluated index preserved EXACTLY (the oscillator identity is exact), entry
   name/description byte-identical, and formula-1 imports become editable like
   the rest. A new formula-1 fixture test pins the behaviour.
2. **Formulas 1/2 lower through the catalogue `DispersionModel`** (Sellmeier,
   resonances squared/unsquared), with the constant c₀ absorbed as a
   resonance-free term (A = c₀, B = 0: c₀·λ²/λ² = c₀ — exact for λ > 0);
   formulas 3–7 build the raw `EpsAxisDispersion` term data directly (the
   `SumOfTerms` shape) — exactly the slice's "through a DispersionModel or
   SumOfTerms" split.
3. **ε-shaped formulas (3/4) → `ComplexEps`; n-shaped formulas (5/6/7) →
   `RealNK` with k = the zero formula** — matching the `Dispersion.fs` axis
   semantics (`ComplexEps.complexIndex` takes √ε; `RealNK` is n,k directly).
4. **Non-integer exponents (formulas 3/4/5 allow real exponents in the wild)
   reuse the `UnsupportedFormula` case** with a diagnostic reason: they are not
   representable as finite term data, and a silent wrong lowering would violate
   the acceptance; the case carries the formula number so the caller can tell.
   Zero-amplitude coefficient pairs drop before the exponent check (formula-4
   zero padding must not trip 0^0).
5. **The imported segment carries the page's `wavelength_range` (µm)** through
   `toWaveLength Micrometer` when present (zero nominal interval otherwise —
   the single-segment tree extrapolates from the topmost segment, so bounds are
   descriptive, not clamping). Real page data is better editor data than the
   private zero nominal of `DispersionModels`.
6. **Formula 9 is typed unsupported per the slice mandate** even though its
   shifted-pole rational form is in principle finite-term-representable; the
   reason string says "not lowered", not "not lowerable" (formula 8's transform
   genuinely is not).

## Testing state

All five gates pass in ADVISORY local runs (Invariant 6: the arc-runner gate
engine re-runs them authoritatively after exit):

- `build` — Release/x64, exit 0, 0 errors (93 warnings: the pre-existing
  MSB3277/NU190x/SYSLIB0051/FS3873/FS1125 noise; 023 recorded the same 93).
- `unit-tests` — 119 passed, 5 skipped, 0 failed (= 119 baseline; no core file
  touched).
- `constructor-unit-tests` — 416 passed, 0 failed (407 baseline + 9 new facts).
- `ui-smoke` — 81 passed, 0 failed (= baseline; no UI code touched).
- `ui-tests` — 306 passed, 0 failed (= baseline; no UI code touched).

## Artifacts

- `specs/0033/.artifacts/025-red-tdd.log` — the TDD red capture (FS0039 on
  `UnsupportedFormula`)
- `specs/0033/.artifacts/025-build.log`
- `specs/0033/.artifacts/025-unit-tests.log`
- `specs/0033/.artifacts/025-constructor-unit-tests.log`
- `specs/0033/.artifacts/025-ui-smoke.log`
- `specs/0033/.artifacts/025-ui-tests.log`

## Gotchas

- **`resonantTerms` algebra**: x^p mod (x² − a) = a^⌊p/2⌋·x^(p mod 2), so
  c·x^p/(x²−a) = c·(quotient polynomial) + c·a^⌊p/2⌋·x^(p mod 2)/(x²−a); the
  odd residual splits at the ±√a simple poles (`Complex.Sqrt`, so a < 0 stays
  exact with conjugate poles). Verified by hand at p ∈ {0,1,2,3} and pinned by
  the formula-4 fixture (which deliberately uses one even- and one
  odd-exponent resonant term).
- **Formula-4 zero padding ordering**: absent coefficients default to 0 and the
  amplitude-zero check runs BEFORE `resonanceBase ** resonancePower` — .NET's
  0.0 ** 0.0 = 1.0 would otherwise fabricate a pole at x² = 1 from padding.
- **`unsupportedReason` gates before coefficient extraction**, so a formula-8/9
  page missing its coefficients line still returns the typed
  `UnsupportedFormula`, not `MalformedYaml`.
- The `sellmeierAxis` error branch (`NotAFiniteTermSum → MalformedYaml`) is
  unreachable — a Sellmeier model always lowers — but keeps the match total
  without a partial pattern.
- **Attempt-01 was re-dispatched**: the first worker console
  (`025-attempt-01-worker-console-20260706T115529Z.log`) was killed AFTER the
  full round completed (production code, tests, fixtures, advisory gate runs,
  impl-log, state-of-the-world all on disk and consistent) but BEFORE the
  structured exit JSON was emitted. The re-dispatched session verified rather
  than re-implemented: all five gate capture logs show green, every changed
  source file's mtime (08:07–08:12) precedes the gate runs (08:14–08:16), and
  the working tree is unchanged since — so the tested tree IS the current tree.
  No code was modified in the second session.
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (the 015–024 drift); the real file is under
  `src\ai_strategy_generator\multistep\`.
- `.manifest.state.json` (modified, CRLF) and the untracked `.claude/` folder
  are the arc-runner's / harness's own files — left alone, as in slices
  001–024. Line endings verified: `git diff --numstat` identical with and
  without `--ignore-cr-at-eol`; all nine new fixtures are LF.
