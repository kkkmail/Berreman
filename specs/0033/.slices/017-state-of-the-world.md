# State of the world — spec 0033, slice 017

# Where we are

Slice 017 opens Part D (shared chart control and the dual-axis n/k view) of
spec 0033: the renderer-neutral chart model (`ExperimentChart` / `ChartFont` /
`ChartStyle`) and the pop-out ScottPlot chart window (`ChartWindow` /
`ChartWindowIds`) are REAL-MOVED out of `OpticalConstructor.TestWindows` into
`OpticalConstructor.Controls`, so both hosts (TestWindows and Ui) share ONE
type identity through project references — no file-linking, no per-assembly
twins. `OpticalConstructor.Controls` gains the same `ScottPlot.Avalonia`
5.1.59 both hosts already used; Ui's `Charts/ChartSettings.fs` is reconciled
onto the shared model (it delegates the shared renderer mapping and stays the
sole Ui-side settings record — no third settings type). Next: step 018 adds
the second Y axis through the moved style spine, then step 019 builds the
n/k dispersion chart on it.

# What's working

- REAL-MOVE ExperimentChart.fs (ExperimentChart/ChartFont/ChartStyle) and ChartWindow.fs (ChartWindowIds/ChartWindow) from OpticalConstructor.TestWindows into OpticalConstructor.Controls under the OpticalConstructor.Controls namespace; no chart type left declared in TestWindows
- Add ScottPlot.Avalonia 5.1.59 to OpticalConstructor.Controls (the version both hosts already used) and drop TestWindows' now-orphaned direct reference (assets flow transitively through the Controls project reference)
- Re-point both hosts: TableAndElementRotationView and ExperimentControlsTests resolve the moved types through their existing `open OpticalConstructor.Controls`; ChartFontTests / ChartStyleTests opens re-pointed to OpticalConstructor.Controls
- Reconcile Ui's ChartSettings onto the one shared model: new public ChartRender module beside the moved window owns the hex→ScottPlot.Color mapping, ChartSettings.scottPlotColor delegates to it; no third settings type introduced
- Both existing charts stay green: all five gates pass at the slice-016 baselines (build 0 errors; 119 / 373 / 58 / 255)

# Tests

No new tests — the slice is a pure move plus a delegation, pinned by the
existing chart suites (`ChartFontTests`, `ChartStyleTests`, the
`ExperimentControlsTests` ChartWindow construction/render/polar proofs, and
`ChartSettingsTests`, which passes UNCHANGED against the reconciled module).
All gates in the slice roster pass in the worker's local (advisory) runs; the
arc-runner gate engine re-runs them authoritatively after exit. Full logs in
`specs/0033/.artifacts/017-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors; no warnings from
  the moved files (the MSB3277/FS1125 noise is pre-existing).
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline).
- `ui-smoke` — 58 passed, 0 failed (= baseline).
- `ui-tests` — 255 passed, 0 failed (= baseline).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 373
  ui_smoke_tests:         58
  ui_tests:               255
```

# Architecture

- **One type identity, reached by reference** — the chart model/window now
  live where both hosts can see them (`OpticalConstructor.Controls`), instead
  of in TestWindows where Ui could never reference them. File-linking was
  explicitly rejected (it mints distinct type identities per assembly); the
  move is real, and the Controls compile order puts the pure model
  (`ExperimentChart.fs`) before the ScottPlot IO seam (`ChartWindow.fs`).
- **The ScottPlot package follows the seam.** Controls owns the
  `ScottPlot.Avalonia` 5.1.59 reference; TestWindows consumes it transitively;
  Ui keeps its direct reference because it uses ScottPlot directly
  (`ChartSettings` / `Plot1DView` / `ChartHosts`). One version resolves
  solution-wide.
- **`ChartRender` is the shared style→ScottPlot mapping seam** beside the
  window: it hosts exactly what both hosts duplicated (hex→`Color`), and is
  the natural landing spot when step 018's right-axis work wants more shared
  mappings.
- **Reconciliation is bounded by the §H.1 contract.** `ChartSettings` remains
  the Ui-side settings record — its surface is pinned by
  `OpticalConstructor.Tests/ChartSettingsTests.fs`, a project outside this
  slice's `touches` — so the reconcile re-points the duplicated mapping and
  documents the one-shared-model relationship rather than reshaping the
  record. The style spine merge continues in steps 018/019, which restructure
  `ChartStyleState`/`SeriesStyle` anyway.

# Deferred

- Second Y axis through the moved style spine (`ChartElement` / `AxisStyle` /
  `SeriesStyle` / `dataBounds` / `defaultState` / `rebuildPlot`) — step 018.
- Spectral-axis helper move + the `nkDispersionChart` builder — step 019.
- `ExperimentControls.ChartSeries` is a pre-existing structural twin of
  `ExperimentChart.ChartSeries`, now colocated in Controls by the move;
  unifying them changes the ExperimentControls host contract and was not in
  this slice's scope — a candidate for the Part D follow-ups.

# Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (same drift as slices 015/016); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- **The committed chart-file blobs carried legacy CRLF** (they predate the
  `.gitattributes` LF policy), so a byte-copy reproduced CRLF; the two new
  Controls files were LF-normalized before the final build and commit clean.
  No EOL churn on any edited tracked file
  (`git diff --numstat` = `--ignore-cr-at-eol`).
- `TableAndElementRotationView` needed ZERO edits: `ExperimentChart` used to
  resolve through its enclosing namespace, and after the move it resolves
  through the file's existing `open OpticalConstructor.Controls`. Ditto
  `ExperimentControlsTests`.
- `ExperimentControls.UiIds.chart = "ExperimentChart"` is a string literal
  (an automation id), not a type reference — untouched by the move.
- `.manifest.state.json` (modified) and `.claude/` (untracked) are the
  arc-runner's / harness's own files — left alone, as in slices 001–016.

# Changelog

- 2026-07-06 — slice 017: shared chart control REAL-MOVED (spec 0033 Part D,
  AC-D1): ExperimentChart.fs (ExperimentChart/ChartFont/ChartStyle) and
  ChartWindow.fs (ChartWindowIds/ChartRender/ChartWindow) moved from
  OpticalConstructor.TestWindows into OpticalConstructor.Controls (namespace
  OpticalConstructor.Controls; model compiles before the ScottPlot seam);
  ScottPlot.Avalonia 5.1.59 moved to Controls (TestWindows' direct ref
  dropped — transitive via the Controls reference); both hosts re-pointed
  (TableAndElementRotationView/ExperimentControlsTests zero-edit via their
  existing opens; ChartFontTests/ChartStyleTests opens re-pointed); Ui's
  ChartSettings reconciled onto the one shared model (scottPlotColor
  delegates to the new shared ChartRender.colorOf; no third settings type);
  no chart type left declared in TestWindows; no new tests; all suites green
  at the 016 baselines (119 / 373 / 58 / 255).
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
