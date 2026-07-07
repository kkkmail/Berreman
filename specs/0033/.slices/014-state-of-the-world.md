# State of the world — spec 0033, slice 014

# Where we are

Slice 014 is the small ribbon-vocabulary step of spec 0033: the Main-screen
element-binding bay formerly labelled "Library" is now labelled "Selector"
(`BayNames.selector = "Selector"` in
`OpticalConstructor.TestWindows/TableAndElementRotationView.fs`), freeing
the word Library for the samples workbench the later parts build. Nothing
behavioural moved: the bay is still the kind-constrained, confirm-gated
binding surface rendered by `LibraryControls` over the read-only
`LibraryProxy`, and both keep their names — only the ribbon label (and the
strings/comments that named the bay by it) changed. Next: slice 015 per
the manifest.

# What's working

- Rename the element-binding ribbon bay label from Library to Selector (BayNames.selector = "Selector"; BayNames.all and the mainBays row follow)
- Keep the bay's behaviour untouched: kind-constrained, confirm-gated binding through LibraryControls and the read-only LibraryProxy, both keeping their names
- Re-point the Details-bay hint and the in-file comments that named the bay to the Selector label
- Strengthen the headless tests: the rendered ribbon shows RibbonTab_Selector and no RibbonTab_Library; the pure ribbon test pins the literal "Selector" and asserts no bay labelled Library
- All suites green at the 013 baselines (119 / 373 / 54 / 249) — a pure rename adds no tests

# Tests

TDD: red first (7 × FS0039 naming the missing `BayNames.selector`,
`014-red-tdd.log`), then the production rename, then green on the first
run with no test edits after red. All gates in the slice roster pass in
the worker's local (ADVISORY) runs; the arc-runner gate engine re-runs
them authoritatively after exit. Full logs in
`specs/0033/.artifacts/014-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched this slice).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline).
- `ui-smoke` — 54 passed, 0 failed (= baseline; the renamed Selector-bay
  render test now also asserts the Selector tab present / Library tab
  absent on the rendered window).
- `ui-tests` — 249 passed, 0 failed (= baseline; the ribbon-membership
  and seven-bay order tests assert the new label and the absence of the
  old one).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 373
  ui_smoke_tests:         54
  ui_tests:               249
```

# Architecture

- **A label is data, not structure**: the bay's identity in the ribbon is
  its `name` string, so the rename is one `BayNames` binding plus its two
  structural uses (`all`, the `mainBays` row) — the `Ribbon` control, the
  MVU messages (`SelectBay`, `RequestBindValueId`, …), and the
  `libraryState` host flattening are all label-agnostic and unchanged.
- **The Library word now unambiguously means the data seam** (the
  `Library` domain module, `LibraryProxy`, `LibraryEntry_*` UiIds, the
  seeded grouping-tree root) rather than a ribbon surface — the exact
  separation the samples workbench needs.

# Deferred

- Nothing from this slice. The historical "Library bay" doc comments in
  the untouched `OpticalConstructor.Controls` project (Ribbon/fsproj/
  LibraryControls headers) still describe the control's spec-0027 origin;
  they are outside this slice's `touches` and harmless — a later Controls
  slice can refresh them opportunistically.

# Gotchas

- **Only the LABEL was renamed.** `LibraryControls`, `LibraryProxy`, the
  `Library` domain module and the `LibraryEntry_*` / `LibraryTree` /
  `LibraryBoundReadout` UiIds keep their names by explicit slice
  instruction — do not "finish the rename" without a spec step.
- The `TreeLabel "Library"` in `ElementId.fs:511` is the seeded
  grouping-tree ROOT label (library data shown inside the Selector bay's
  tree), not the bay label — deliberately untouched.
- The slice letter's line numbers had drifted (mainBays at :1582 vs :1626
  in the current file); the rename went by the unique symbols.
- `.manifest.state.json` shows as modified in `git status` — the
  arc-runner's own file (same as slices 001–013), left alone.

# Changelog

- 2026-07-06 — slice 014: ribbon bay label Library → Selector
  (spec 0033 step 014): BayNames.selector = "Selector" replacing
  BayNames.library; BayNames.all + mainBays row + Details-bay hint +
  bay-naming comments follow; behaviour unchanged (kind-constrained,
  confirm-gated binding via LibraryControls / read-only LibraryProxy,
  names kept); headless tests assert RibbonTab_Selector present and no
  RibbonTab_Library; all suites green at the 013 baselines.
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
