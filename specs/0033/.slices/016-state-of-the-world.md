# State of the world — spec 0033, slice 016

# Where we are

Slice 016 continues the samples-workbench UI tier of spec 0033: the second
declared UI component, `SampleLibraryControls` (UICOMP_XDUO_0002), now
lives in `OpticalConstructor.Controls` beside `MaterialsControls`
(step 015). It is the samples-workbench list surface — a searchable
samples list with a search box, a substrate-kind facet selector, Add /
Edit / Remove / View verbs, and the multilayer entry point
(`MakeMultilayerButton`) — declared domain-free in the step-015 shape
(host-flattened rows, injected `Handlers`, stable intent-named UiIds) and
proven by headless structure tests. It is deliberately NOT wired into any
parent view yet (a later WIRE_UI step); the contract registry records it
as declared at step 16. Next: slice 017 per the manifest.

# What's working

- Declare SampleLibraryControls (UICOMP_XDUO_0002) in OpticalConstructor.Controls: search box, substrate-kind facet selector, the samples list, Add / Edit / Remove / View verbs, and the multilayer entry point, in the domain-free step-015 shape (Row/FacetOption/State/Handlers/UiIds)
- Ship the eight stable intent-named [<Literal>] ids (SampleSearchBox, SampleSubstrateFilter, SamplesList, AddSampleButton, EditSampleButton, RemoveSampleButton, ViewSampleButton, MakeMultilayerButton) plus prefixed per-row / per-substrate-option ids
- Keep Add and Make-multilayer always live (creation entry points) while Edit / Remove / View disable without a listed selection; a headless test pins the inert-when-disabled behaviour
- Add 5 tests: 3 pure contract (ui-tests 252 → 255) + 2 headless structure proofs (ui-smoke 56 → 58) — every UiIds control found, each simulated click dispatches its matching stub handler, Add click invokes the add handler
- All suites green: build clean, BerremanTests 119 and constructor tests 373 at baseline

# Tests

TDD: red first (120 × FS0039 naming the missing `SampleLibraryControls`,
`016-red-tdd.log`), then the production module, then green on the first
run with no test edits after red. All gates in the slice roster pass in
the worker's local (advisory) runs; the arc-runner gate engine re-runs
them authoritatively after exit. Full logs in
`specs/0033/.artifacts/016-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors, no warnings
  from the new files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline).
- `ui-smoke` — 58 passed, 0 failed (56 baseline + 2: the mount-everything
  + click-dispatch proof, and the no-selection inert-verbs proof).
- `ui-tests` — 255 passed, 0 failed (252 baseline + 3 pure contract
  tests).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 373
  ui_smoke_tests:         58
  ui_tests:               255
```

# Architecture

- **The component's surface is its ids + state→view projection** (the
  ADD_COMPONENT contract): `State` mirrors the domain `SampleQuery` seam
  (text + `SubstrateKind option`) as host-supplied `code` strings, so the
  later wiring step maps the substrate code back to the domain facet
  (the host's "all" option → `None`) while `OpticalConstructor.Controls`
  stays domain-free. The control renders the rows it is given and never
  touches `SampleProxy` — the host flattens.
- **No editability tier** (deliberate divergence from step 015): the
  slice letter names no view-only behaviour and the domain `Sample`
  carries no "no edit model" state, so `Row` is `{ sampleId; label }`
  and Edit disables (never disappears) without a selection. If view-only
  samples arrive later, the MaterialsControls `Editable | ViewOnly` DU
  pattern is the ready-made extension.
- **Make-multilayer is a second creation entry point**, not a transform
  of the selection: always enabled, `makeMultilayer : unit -> unit`; the
  host decides at wiring time what flow it opens (the `Repeated` /
  `PeriodGroup` builder shape).
- **`FacetOption` is module-local, not shared with MaterialsControls** —
  the Controls bays are self-contained modules by established idiom;
  same shape, separate types keeps the components' evolution decoupled.
- **AutomationId over `Control.Name` for variable-membership lists** (the
  rows, the substrate options, the verb buttons) — the step-015
  precedent, kept on the verbs for consistency and future safety even
  though this verb row's membership is fixed today.

# Deferred

- Wiring SampleLibraryControls into a parent view / the samples
  workbench ribbon is the later WIRE_UI step (per the ADD_COMPONENT
  family contract); no host consumes the component yet — including the
  multilayer entry point's target flow.
- The registry lifecycle stays `declared` (implementStep null) —
  supervisor-maintained, advanced by the implementing step.

# Gotchas

- The task file pointed at the non-existent system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` (same
  as slice 015); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\add_component_worker.system-md`.
- **Edit / Remove / View disable (never disappear) without a selection**
  — there is no view-only removal here, unlike MaterialsControls; a
  headless test pins that disabled verbs dispatch NOTHING while Add and
  Make-multilayer stay live.
- The verb buttons / rows / facet options carry AutomationId (not
  Name): naming controls in a variable-membership child list would
  reintroduce the FuncUI "Cannot set Name — already styled" throw when
  the host's filter rewrites the rows.
- New `.fs` files are LF on disk, matching the committed siblings; the
  two edited fsprojs show zero CRLF churn.
- `.manifest.state.json` (modified) and `.claude/` (untracked) are the
  arc-runner's / harness's own files — left alone, as in slices 001–015.

# Changelog

- 2026-07-06 — slice 016: SampleLibraryControls DECLARED
  (UICOMP_XDUO_0002, spec 0033 step 016): the domain-free
  samples-workbench list surface in OpticalConstructor.Controls (search
  box, substrate-kind facet selector, samples list, Add/Edit/Remove/View
  verbs plus the MakeMultilayerButton entry point; step-015 shape; eight
  [<Literal>] intent-named ids; no editability tier — Edit disables, not
  disappears, without a selection; AutomationId on variable-membership
  lists); +3 pure contract tests (ui-tests 252 → 255) and +2 headless
  structure proofs (ui-smoke 56 → 58); component not wired (later
  WIRE_UI); all suites green.
- 2026-07-06 — slice 015: MaterialsControls DECLARED (UICOMP_XDUO_0001,
  spec 0033 step 015): the domain-free materials-workbench list surface in
  OpticalConstructor.Controls (search box, category/dispersion facet
  selectors, materials list, Add/Edit/Remove/View verbs; LibraryControls
  shape; eight [<Literal>] intent-named ids; Edit verb removed — not
  greyed — for view-only selections; AutomationId on variable-membership
  lists); +3 pure contract tests (ui-tests 249 → 252) and +2 headless
  structure proofs (ui-smoke 54 → 56); component not wired (later
  WIRE_UI); all suites green.
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
