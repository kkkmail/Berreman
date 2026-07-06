# State of the world — spec 0033, slice 012

# Where we are

Slice 012 is the sixth Part B (serializable dispersion) step of arc 0033 and
completes the `DispersionModels.DispersionModel` catalogue breadth the
steering spec names for Part B: `ForouhiBloomer` (the 1986 five-parameter
amorphous n(E)/k(E) form) and `BrendelBormann` (Rakić 1998's
Gaussian-broadened Voigt oscillator ε(E), evaluated through a private
Weideman Faddeeva-function helper) now sit beside Sellmeier / Cauchy /
Lorentz / Drude / TaucLorentz / GaussianOscillator / ConstantNK / SumOfTerms,
following the same coefficient-record convention (coefficient scalars +
`wavelengthUnit` + `thermoOptic option`) and the same private `baseIndex`
dispatch. Both join the slice-011 typed-error route of `toEpsAxis` (they are
not finite term sums), so `toOpticalProperties` wraps `evaluate` in the
engine `EpsWithDisp` closure for them. AC-B6 tests reproduce documented
reference values for one published coefficient set each. Next in Part B:
`MaterialComplexity` / `MaterialEntry.complexity` (slice 013).

# What's working

- Add ForouhiBloomer catalogue case (1986 five-parameter amorphous form: band-gap-stepped k, closed Kramers–Kronig n via B₀/C₀/Q) evaluated in baseIndex
- Add BrendelBormann catalogue case (Rakić 1998 Voigt oscillators: Drude intraband + Gaussian-broadened Lorentzians via the Faddeeva function, k ≥ 0 convention)
- Add private Weideman-1994 Faddeeva helper (N = 24 rational series, coefficients computed from the defining cosine transform at module init — no magic table)
- Route both models through the typed NotAFiniteTermSum lowering error; toOpticalProperties keeps them working by wrapping evaluate (slice-011 recorded route)
- Add 5 AC-B6 tests: FB a-Si published set reproduces n = 3.182 / k = 0 at 0.6 eV and the closed form at 3 eV; BB Rakić-gold reproduces four CC0 refractiveindex.info n/k rows (0.5–2 µm, 2e-3); BB degenerates to Drude (no oscillators) and to Lorentz (σ→0); constructor tests 364 → 369

# Tests

TDD: red first (FS0039 on the missing `ForouhiBloomer`/`BrendelBormann`
constructors — `012-red-tdd.log`), then the implementation, then green on the
first run with no test edits after red. All gates in the slice roster pass in
the worker's local (ADVISORY) run; the arc-runner gate engine re-runs them
authoritatively after exit. Full logs in `specs/0033/.artifacts/012-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors; no new warnings
  from touched files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched this slice).
- `constructor-unit-tests` — 369 passed, 0 failed (+5 over the 364 baseline).
- `ui-smoke` — 54 passed, 0 failed (= baseline).
- `ui-tests` — 249 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 369
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **Both new models are named cases evaluated directly, not term data** —
  ForouhiBloomer is piecewise (band-gap step in k; its n also has
  complex-conjugate poles the real `RealNK` term data cannot carry) and
  BrendelBormann is transcendental (Faddeeva-function Voigt broadening), so
  `toEpsAxis` surfaces the reason-carrying `NotAFiniteTermSum` and the
  pre-existing `toOpticalProperties` Error branch wraps `evaluate` — exactly
  the slice-011 recorded resolution of the unimplementable "ComplexEps
  wrapping evaluate" prescription. No new lowering machinery was invented.
- **The Faddeeva helper is private to `DispersionModels`** and restricted to
  Im z ≥ 0 (Weideman's rational series, N = 24). Its coefficients are
  computed once from their defining cosine transform rather than pasted as a
  magic table, and its correctness is pinned three independent ways (RII
  reference rows, the exact Drude equality with no oscillators, the σ→0
  Lorentz degeneracy whose deviation is O(σ²)). No solver-file change —
  constraint 0.1 holds; `touches` stays [Domain, Tests].
- **Convention fidelity** — Brendel–Bormann is implemented in the codebase's
  Im ε ≥ 0 / k ≥ 0 sign convention (the conjugate of Rakić's printed
  e^{+iωt} equations), matching the existing Lorentz/Drude cases and the
  refractiveindex.info calculation script; ε∞ is the published constant 1,
  not a record field. ForouhiBloomer stores the paper's n∞ parameterisation
  (Horiba tabulates ε∞ = n∞²).
- **Abscissa handling is unchanged** — both models read their eV abscissa
  through the sole `Units.fromMeters` seam like every other oscillator model;
  no literal conversion factor was added.

# Deferred

- `MaterialComplexity` / `MaterialEntry.complexity` and the re-expressed
  built-ins (slice 013).
- The Storage JSON-schema `dispersionModel.kind` enum does not yet list
  `SumOfTerms` (slice 011), `ForouhiBloomer` or `BrendelBormann` — the
  serialization/schema breadth of the new value types is the spec's storage
  seam, outside this slice's `touches` (same recorded state as slice 011).
- RII import breadth (formulas 2–7 through `DispersionModel`/`SumOfTerms`,
  Part G, slice 025) — ForouhiBloomer/BrendelBormann are now available to it.
- Wiring/UI (Parts C–F).

# Gotchas

- **Rakić's paper prints ε in the e^{+iωt} convention** (Im ε < 0 absorbing).
  The implementation and tests use this codebase's k ≥ 0 conjugate; validate
  against the RII tabulation (CC0), not the paper's raw signs.
- **The FB k-branch has the band-gap step** (k = 0 for E ≤ Eg, per the
  Θ ∝ (E−Eg)² derivation and Horiba TN13 eq. 4.2), while n keeps the closed
  Kramers–Kronig form everywhere — the standard published shape of the
  five-parameter model; the below-gap reference point (k displayed 0.000)
  depends on it.
- **`faddeeva` requires Im z ≥ 0** — guaranteed here because `Complex.Sqrt`
  of E² + iΓE (Γ, E ≥ 0) is the principal first-quadrant root. Reusing the
  helper for lower-half-plane arguments needs the reflection identity it
  deliberately does not implement.
- **BB reference tolerance is 2e-3 by design** — the RII table rounds both λ
  and n/k to 5 significant digits, and the seam's `evNmProduct = 1239.84`
  differs from the script's h·c = 1239.8419 nm·eV by 1.6e-6 relative;
  tightening the assertion below ~1e-3 would test the rounding, not the
  physics.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–011), left alone.

# Changelog

- 2026-07-06 — slice 012: ForouhiBloomer + BrendelBormann catalogue cases
  (spec 0033 Part B, AC-B6): 1986 five-parameter amorphous form (stepped k,
  closed-form n) and Rakić-1998 Voigt oscillators (private Weideman Faddeeva
  helper, coefficients derived at init); both typed NotAFiniteTermSum under
  toEpsAxis with toOpticalProperties wrapping evaluate; +5 tests reproducing
  published reference sets (Horiba TN13 a-Si; Rakić/RII gold n,k at
  0.5–2 µm) plus Drude/Lorentz degeneracy pins; constructor tests 364 → 369;
  all suites green.
- 2026-07-06 — slice 011: DispersionModel → serializable eps tree lowering
  (spec 0033 Part B): SumOfTerms escape hatch (identity under toEpsAxis),
  toEpsAxis (Cauchy/ConstantNK → RealNK, Sellmeier/Lorentz/Drude → ComplexEps;
  exact partial-fraction lowering for eV/cm⁻¹ reciprocal abscissas; typed
  NotAFiniteTermSum for TaucLorentz/Gaussian), toEpsValue + re-pointed
  toOpticalProperties/toAnisotropicOpticalProperties through
  EpsWithDispValue.toEpsWithDisp (ConstantNK still EpsWithoutDisp;
  isotropicProperties stays the single vacuum-μ/ρ wrapper), AnisotropicModel
  removed; +14 AC-B5 grid/shape tests, AC-D5 rebuilt on the value tree;
  constructor tests 350 → 364; all suites green.
- 2026-07-05 — slice 010: serializable mu (Polder / gyromagnetic) tree
  (spec 0033 Part B): GyrationAxis (AlongX | AlongY | AlongZ,
  defaultValue = AlongZ Faraday; transverse = Voigt), ONE generic
  PolderValue<'g> (muDiagonal / muParallel / gyration / axis, map),
  private polderMu (rows [mu, +i·g, 0], [-i·g, mu, 0], [0, 0, muParallel]
  via Mu.create, cyclic permutations per axis), ConstantMuValue
  (ScalarMu → mu × identity | GyromagneticMu), MuWithDispValue.toMuWithDisp
  (constant short-circuits to MuWithoutDisp, dispersive evaluates per call) —
  all appended to Berreman/Berreman/Dispersion.fs, engine unions
  byte-identical; +8 tests (axis permutations pinned, scalar identity,
  short-circuit shape, default axis, dispersive = constant assembly at
  500 nm); BerremanTests 111 → 119; all suites green.
- 2026-07-05 — slice 009: serializable rho (gyration) tree (spec 0033 Part B):
  Handedness (enantiomorph = one overall sign flip), five named generic
  gyration records + GyrationClass<'g> (rotation-producing classes only) +
  GyrotropicValue<'g> + two-case RhoWithDispValue in Dispersion.fs; four new
  crystal-class Rho builders (222 / monoclinic-2 / monoclinic-m / triclinic-1,
  all Rho.fromIm) and the toRhoWithDisp type extension (per-component sign,
  UniaxialActive → diagonal type_3_4_6_Crystal, dispersive case evaluates
  formulas at the wavelength) in OpticalProperties/Active.fs; +11 tests
  (quartz class-32 diag(+5.9e-5, +5.9e-5, −10.1e-5), every class pinned,
  handedness negation, dispersive evaluation); BerremanTests 100 → 111; all
  suites green.
- 2026-07-05 — slice 008: serializable eps tree (spec 0033 Part B):
  ConstantEpsValue (six descriptive constant cases, toEps via the engine
  constructors, uniaxial → (n_o, n_e, n_o)), EpsAxisDispersion
  (RealNK | ComplexEps with complexIndex), the three shared-interval segment
  records, EpsDispersiveValue.getEps (first covering segment wins, topmost
  extrapolates), and EpsWithDispValue.toEpsWithDisp building the engine's
  EpsWithDisp — appended to Berreman/Berreman/Dispersion.fs, engine unions
  byte-identical; +11 tests (six constant-case equalities, three
  segment-selection facts, uniaxial/biaxial per-axis analytic closures);
  BerremanTests 89 → 100; all suites green.
- 2026-07-05 — slice 007: serializable dispersion formula blocks
  (spec 0033 Part B opener): WaveLengthInterval, DispersionTerm,
  DispersionFormula.evaluate (canonical-wavelength reduction + term sum), and
  the Complex mirrors added to Berreman/Berreman/Dispersion.fs — pure
  data + evaluation, engine unions byte-identical; +5 hand-computed-value
  tests (Cauchy Laurent, Sellmeier inverse via exact partial fraction,
  shifted-centre power, complex Lorentz pole, interval endpoints) under
  MatrixComparison.allowedDiff; BerremanTests 84 → 89; all suites green.
- 2026-07-05 — slice 006: MaterialProxy IMPLEMENTED (STORE_XDUO_0001):
  MaterialProxy.createInMemory (samplesReferencing) closes over a ref
  Map<MaterialId, MaterialEntry> seeded from builtInEntries — writes persist;
  removeMaterial hard-blocks referenced materials naming the referencing
  samples via the live SampleProxy-backed samplesReferencing lookup; pure
  byQuery search seam + SampleStructure.referencedMaterials added; the
  step-003 mock replaced; +6 tests (350 constructor tests); registry
  lifecycle → implemented; all suites green.
- 2026-07-05 — slice 005: SampleProxy IMPLEMENTED (STORE_XDUO_0002):
  SampleProxy.createInMemory closes over a ref Map<SampleId, Sample> seeded
  from seedEntries — writes persist inside the closure; the step-004
  validate-only mock replaced; +5 fixed-Guid round-trip tests, every
  SampleProxy test isolated on a fresh proxy; registry lifecycle →
  implemented; all suites green.
- 2026-07-05 — slice 004: SampleProxy write-seam DECLARED (STORE_XDUO_0002):
  SampleQuery (text + SubstrateKind facet), the new three-case reason-carrying
  SampleError, the [<ReferenceEquality>] six-function proxy record, the
  fixed-SeedSamples in-memory mock (createInMemorySampleProxy), and +14
  mock/stub tests incl. the six-function acceptance; all suites green.
- 2026-07-05 — slice 003: MaterialProxy write-seam DECLARED (STORE_XDUO_0001):
  DispersionFilter + MaterialQuery, MaterialError extended with three
  reason-carrying cases, the [<ReferenceEquality>] six-function proxy record,
  the fixed-list in-memory mock, and +15 mock/stub tests incl. the
  six-function acceptance; TestWindows error rendering kept exhaustive; all
  suites green.
- 2026-07-05 — slice 002: MaterialId/SampleId elevated to Guid-backed DUs;
  fixed-literal seed ids (MaterialIds / SeedSamples); resolveMaterial is a
  MaterialId lookup; UnknownMaterialId carries reason; imports mint ids;
  JSON/drag boundaries parse the Guid string form; +4 round-trip tests, all
  suites green.
- 2026-07-05 — slice 001: sample stacks made DATA (SampleStructure + typed
  material resolution); id-branching sampleToSystem replaced by total
  ResolvedSample mapping; 4 new material built-ins; 11 samples re-seeded
  structurally; host resolves once per run; +5 tests, all suites green.
