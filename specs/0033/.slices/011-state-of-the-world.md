# State of the world — spec 0033, slice 011

# Where we are

Slice 011 is the fifth Part B (serializable dispersion) step of arc 0033 and
the first on the OpticalConstructor side: it connects the editor-facing
`DispersionModels.DispersionModel` catalogue to the serializable eps tree the
slice-007/008 core steps built. `toEpsAxis` lowers every finite-term analytic
model (Sellmeier, Cauchy, ConstantNK, Lorentz, Drude — in any tabulation
unit, including the conventional eV) to the per-axis term data
`EpsAxisDispersion`; the transcendental TaucLorentz/GaussianOscillator surface
a typed `NotAFiniteTermSum` error and keep evaluating through the closure
route. The new raw `SumOfTerms` escape-hatch case carries term data verbatim
(identity under the lowering). `AnisotropicModel` is gone — anisotropy now
lives inside `EpsWithDispValue` — and the `toOpticalProperties` /
`toAnisotropicOpticalProperties` surface composes through
`EpsWithDispValue.toEpsWithDisp` with `isotropicProperties` as the single
vacuum-μ/ρ wrapper. Next in Part B: ForouhiBloomer/BrendelBormann (slice 012)
and `MaterialComplexity` (slice 013).

# What's working

- Add SumOfTerms escape-hatch case to DispersionModel carrying a raw EpsAxisDispersion (identity under toEpsAxis; Meter unit, no thermo-optic, complexIndex evaluation)
- Add toEpsAxis lowering each analytic model to serializable term data: Cauchy → RealNK Laurent terms (k = zero formula), ConstantNK → RealNK constants, Sellmeier → ComplexEps via the oscillator identity (complexIndex supplies the √(1+Σ)), Lorentz/Drude → ComplexEps inverse terms
- Lower eV/cm⁻¹ (reciprocal-abscissa) tabulations exactly via partial fractions (rationalXSquaredTerms: simple/double poles, long division, polynomial); scales derived through the Units seam, no literal factors
- Make TaucLorentz/GaussianOscillator a typed NotAFiniteTermSum lowering error; toOpticalProperties keeps them working by wrapping evaluate in the engine EpsWithDisp closure
- Remove AnisotropicModel; re-point toOpticalProperties/toAnisotropicOpticalProperties at EpsWithDispValue.toEpsWithDisp through the new toEpsValue seam (ConstantNK still short-circuits to EpsWithoutDisp)
- Add 14 AC-B5 grid-equality/shape tests + rebuild the AC-D5 uniaxial test on the serializable value (constructor tests 350 → 364)

# Tests

TDD: red first (FS0039 on the missing `toEpsAxis`/`toEpsValue`/`SumOfTerms`
symbols, FS0001 on the re-pointed anisotropic surface — `011-red-tdd.log`),
then the implementation, then green. All gates in the slice roster pass in the
worker's local (ADVISORY) run; the arc-runner gate engine re-runs them
authoritatively after exit. Full logs in `specs/0033/.artifacts/011-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors; no new warnings
  from touched files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched this slice).
- `constructor-unit-tests` — 364 passed, 0 failed (+14 over the 350 baseline).
- `ui-smoke` — 54 passed, 0 failed (= baseline).
- `ui-tests` — 249 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 364
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The lowering is a `Result`, not a partial pretence** — `toEpsAxis :
  DispersionModel -> Result<EpsAxisDispersion, EpsAxisLoweringError>`. The
  slice letter's "ComplexEps wrapping evaluate" cannot type-check (`ComplexEps`
  carries the pure-data `ComplexDispersionFormula`; a closure cannot inhabit
  it), so the transcendental cases are a reason-carrying typed error and stay
  "named cases evaluated directly" (preliminary spec §6.1): both routes end at
  `WaveLength -> ComplexRefractionIndex`.
- **Sellmeier is `ComplexEps` with real-valued terms** — `RealNK`'s n formula
  is a finite term sum and cannot take the square root; `ComplexEps.complexIndex`
  evaluates exactly the `√(1 + Σ)` the slice names. The oscillator identity
  `Bᵢa²/(a²−cᵢ) = Bᵢ + Bᵢcᵢ/(a²−cᵢ)` is used as prescribed.
- **Reciprocal abscissas lower exactly, not approximately** — the core formula
  variable is linear in λ while oscillator models are conventionally in eV;
  each eV oscillator becomes `s·x²/(c₀+c₁x+c₂x²)` and splits by exact partial
  fractions (one private helper covering quadratic/linear/constant
  denominators and the double-pole degeneracy). AC-B5 grid tests pin every
  branch against `evaluate`.
- **`toEpsValue` is the single re-pointing seam** — ConstantNK →
  `EpsWithoutDispValue` (no closure overhead, preserving the existing
  guarantee), lowerable models → a single-segment `IsotropicDispersive` tree
  (the topmost segment extrapolates, so the nominal interval carries no
  physics), and `toOpticalProperties`/`toAnisotropicOpticalProperties` are
  thin compositions through `toEpsWithDisp` and `isotropicProperties` (still
  the one vacuum-μ/ρ site).
- **Anisotropy is data now** — `EpsWithDispValue`'s uniaxial/biaxial constant
  cases and per-axis dispersive segment trees supersede `AnisotropicModel`;
  `uniaxialEps`/`biaxialEps` remain as the documented engine-constructor
  mapping helpers (AC-D5).

# Deferred

- ForouhiBloomer / BrendelBormann catalogue cases (slice 012) — they will join
  the same typed-error transcendental route of `toEpsAxis`.
- `MaterialComplexity` / `MaterialEntry.complexity` and the re-expressed
  built-ins (slice 013).
- JSON serialization of the new value types — the spec's storage seam; nothing
  in this arc's steps serializes them yet (`SumOfTerms` payloads included).
- Wiring/UI (Parts C–F) and RII import breadth through
  `DispersionModel`/`SumOfTerms` (Part G, slice 025).

# Gotchas

- **Two how-to prescriptions were mathematically unimplementable against the
  shipped slice-007/008 core** (Sellmeier-as-RealNK needs a √ the term data
  cannot express; a closure cannot live inside `ComplexEps`). The impl-log
  Decisions 1–2 record the resolutions; the binding acceptance (grid equality
  for the five analytic models, `AnisotropicModel` removal) is met exactly.
- **`Berreman.Constants` is not opened in `DispersionModels.fs`** — `<meter>`
  literals do not resolve there; strip units with `float` or go through
  `Units.toWaveLength` instead of dividing by `1.0<meter>`.
- **The discriminant check in `rationalXSquaredTerms` is exact** — critical
  damping (`d = 2r`) hits the double-pole branch exactly in floating point
  (pinned by a test); a merely near-degenerate pair stays two simple poles and
  only costs precision in that pathological corner.
- **`toEpsAxis` is isothermal** — like `evaluate`, a `Some`-thermo-optic model
  lowers at its reference temperature (Δn = 0); the operating temperature
  enters only at `evaluateAt` and is never part of the term data.
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–010), left alone.

# Changelog

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
