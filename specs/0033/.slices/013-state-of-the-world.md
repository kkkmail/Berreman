# State of the world — spec 0033, slice 013

# Where we are

Slice 013 closes spec 0033 Part B (serializable dispersion values) on the
domain side: the edit-model option tree `MaterialComplexity` now sits in
`OpticalConstructor.Domain/MaterialLibrary.fs` — eps ALWAYS present
(`EpsWithDispValue`), magnetic (`MuWithDispValue`) and active
(`RhoWithDispValue`) OPTIONAL — with a pure `toProperties` that composes the
engine's `OpticalPropertiesWithDisp` through `toEpsWithDisp` /
`toMuWithDisp` / `toRhoWithDisp` and defaults absent options to the vacuum
μ/ρ via the single vacuum-convention site
(`DispersionModels.isotropicProperties`). `MaterialEntry` gained
`complexity : MaterialComplexity option`: `Some` is the editable source of
truth (the nine expressible built-ins are re-expressed and seed
`properties = complexity.toProperties` by construction), `None` marks the
engine-preset, view-only entries (silicon, langasite — dispersion coded in
`OpticalProperties/Dispersive.fs:98-99` — and the vacuum spacer). This is the
edit model the Part F material-editor ladder lifts aspect-by-aspect. Next:
slice 014 (Part A/J wiring per the manifest).

# What's working

- Add MaterialComplexity edit model (eps always present; magnetic/active optional) with pure toProperties composing OpticalPropertiesWithDisp through the engine value trees
- Default absent magnetic/active options to vacuum mu/rho through the single DispersionModels.isotropicProperties site; assemble present options via toMuWithDisp/toRhoWithDisp
- Add MaterialEntry.complexity (Some = editable source of truth with properties = complexity.toProperties held by construction; None = engine-preset view-only entry)
- Re-express nine built-ins as complexities (four glasses IsotropicTransparent, EUV Mo/Si IsotropicAbsorbing, uniaxial/biaxial/active crystals per-axis BiaxialTransparent, active crystal plus constant PlanarActive gyration, RightHanded) value-identical to the original presets
- Add 4 AC-B7 tests: vacuum defaults (values + short-circuit shape), Polder/PlanarActive assembly, all nine re-expressed entries reproduce their presets at reference wavelengths, None/Some partition of the seeds; constructor tests 369 → 373

# Tests

TDD: red first (FS0039 naming the missing `magnetic`/`active` labels,
`toProperties`, and `MaterialEntry.complexity` — `013-red-tdd.log`), then the
implementation, then green on the first run with no test edits after red. All
gates in the slice roster pass in the worker's local (ADVISORY) run; the
arc-runner gate engine re-runs them authoritatively after exit. Full logs in
`specs/0033/.artifacts/013-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched this slice).
- `constructor-unit-tests` — 373 passed, 0 failed (+4 over the 369 baseline).
- `ui-smoke` — 54 passed, 0 failed (= baseline).
- `ui-tests` — 249 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 373
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **The complexity is data over the Part B value trees, not a new tensor
  path**: `toProperties` only composes the existing `EpsWithDispValue` /
  `MuWithDispValue` / `RhoWithDispValue` assemblies and the recorded
  single-site vacuum-μ/ρ wrapper — no engine change, no new vacuum literals;
  constraint 0.1 holds and `touches` stays effectively [Domain, Tests] plus a
  mechanical Storage compile ripple.
- **Source-of-truth by construction**: each re-expressed seed binds its
  complexity once and sets `properties = complexity.toProperties`, so the
  sync invariant the Part F editor must keep at save time is already literal
  in the seeds, and drift is impossible for built-ins.
- **Axis-placement decision (recorded)**: `UniaxialTransparent` maps to the
  (o, e, o) diagonal, which cannot represent `Eps.uniaxialCrystal`
  (unique axis x) or the active crystal's `planarCrystal` (unique axis z);
  those two complexities use exact per-axis `BiaxialTransparent` encodings —
  the acceptance's tensor reproduction outranks the slice letter's case name.
  The active crystal additionally derives n₁₁/n₃₃ through `planarCrystal`'s
  own `EpsValue` sqrt round-trip so the seeded system stays value-identical
  under `PropagationTests`' exact `Assert.Equal<OpticalSystem>` pin.
- **View-only entries carry `None`, not a flag**: no naked bool; the option on
  the edit model itself is the affordance switch Part F reads (Edit REMOVED,
  not greyed, per the steering spec).

# Deferred

- The Storage JSON schema / library-file DTO does not carry `complexity`;
  exported-then-imported entries come back `None` (view-only) — the
  serialization breadth of the Part B value trees is the spec's storage seam
  (same recorded state as slices 011/012; constraint 0.3 adds no schema work
  outside Part G).
- Imports (`MaterialImport`) stay closure-backed with `complexity = None`
  until Part G (slice 025) lowers RII formulas through
  `DispersionModel`/`SumOfTerms`.
- The vacuum spacer entry stays `None` per the slice's explicit enumeration;
  lifting it to an editable complexity is a one-line change if a later part
  wants it.
- Wiring/UI (Parts C–F) — the material editor that drives `MaterialComplexity`.

# Gotchas

- **Do not re-encode the uniaxial/active crystals as `UniaxialTransparent`**:
  the engine's (o, e, o) mapping permutes their unique axes and changes
  off-normal physics; `PropagationTests` pins the seeded systems with exact
  equality and will fail. The `BiaxialTransparent` encodings are deliberate
  (impl-log Decision 1).
- **Do not "simplify" the active-crystal binding to `RefractionIndex 2.315`
  directly** — the `EpsValue.fromRefractionIndex → .refractionIndex` round
  trip reproduces the preset's sqrt(n²) arithmetic; skipping it risks a 1-ulp
  mismatch under the exact seed-equality test.
- **Adding fields to `MaterialEntry` ripples into Storage**: record literals
  in `MaterialImport.fs` / `Report.fs` must name every field; this slice added
  `complexity = None` there (minimal compile-restoring change outside the
  declared `touches`).
- `.manifest.state.json` shows as modified in `git status` — the arc-runner's
  own file (same as slices 001–012), left alone.

# Changelog

- 2026-07-06 — slice 013: MaterialComplexity edit model + MaterialEntry.complexity
  (spec 0033 Part B, AC-B7): pure toProperties with vacuum μ/ρ defaults through
  the single isotropicProperties site and toMuWithDisp/toRhoWithDisp assembly;
  nine built-ins re-expressed as complexities value-identical to their engine
  presets (per-axis BiaxialTransparent for the uniaxial/active crystals —
  recorded axis-placement decision; PlanarActive gyration for the active
  crystal); silicon/langasite/vacuum stay None (view-only); +4 AC-B7 tests;
  constructor tests 369 → 373; all suites green.
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
