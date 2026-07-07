# State of the world — spec 0033, slice 018

# Where we are

Slice 018 continues Part D (shared chart control and the dual-axis n/k view) of
spec 0033: the renderer-neutral chart model that slice 017 moved into
`OpticalConstructor.Controls` now carries a SECOND, independent Y axis through
its whole style spine — a two-case `AxisSide` on every series, a tri-state
(X / Y-left / Y-right) axis-mutator surface, per-side data bounds — and the
pop-out `ChartWindow` maps it onto ScottPlot 5.1.59's native right axis
(`plot.Axes.Right`), including a series-panel picker to flip a series between
sides. Next: step 019 moves the spectral-axis helper and builds the n/k
dispersion chart on this dual-axis spine.

# What's working

- ChartStyle model (Controls/ExperimentChart.fs) carries a second Y axis: AxisSide DU (left/right) on every SeriesStyle, ChartElement.YAxis split per side, tri-state ChartAxis mutators (X, Y-left, Y-right) replacing the boolean isX, ChartStyleState.yAxisLeft/yAxisRight
- dataBounds returns per-axis ChartBounds (shared x, INDEPENDENT yLeft/yRight from only each side's series); defaultState seeds all three axes from it (series start left; right seeds the unit fallback)
- ChartWindow assigns each scatter's Axes.YAxis to plot.Axes.Left/plot.Axes.Right (rebuildPlot + applySeriesStyle), drives the right axis in applyAxisLimits (guarded: in-use-or-manual), the axis format/font appliers and setCartesianAxesVisible; series panel gains the Axis: Left/Right picker (ChartWindowSeriesAxis)
- Polar toggle behaviour and toCsv untouched (CSV output now pinned by test); +4 ChartStyle model tests (ui-tests 255 → 259) and +1 headless proof that a flipped series' scatter lands on plot.Axes.Right (ui-smoke 58 → 59)

# Tests

All gates in the slice roster pass in the worker's local ADVISORY runs; the
arc-runner gate engine re-runs them authoritatively after exit. Full logs in
`specs/0033/.artifacts/018-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors; the 94 warnings are
  the pre-existing MSB3277/FS1125 noise, none from the edited files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline; project untouched).
- `ui-smoke` — 59 passed, 0 failed (58 baseline + the new right-axis scatter
  assignment proof).
- `ui-tests` — 259 passed, 0 failed (255 baseline + 4 new ChartStyle model
  tests: independent per-side bounds, tri-state mutator independence,
  default-left + setSeriesAxisSide, toCsv output pin).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 373
  ui_smoke_tests:         59
  ui_tests:               259
```

# Architecture

- **The side is style, not data.** Axis-side assignment lives ONLY in
  `SeriesStyle` (per the slice text); `ChartSeries` / `ExperimentChart` /
  `toCsv` are byte-identical in behaviour, so no host outside this slice's
  `touches` needed edits. A chart host that wants a series born on the right
  axis (step 019's n/k chart) seeds it with `setSeriesAxisSide` over
  `defaultState`'s output.
- **Tri-state composes the two-case DU.** `ChartAxis = AxisX | AxisY of
  AxisSide` gives the mutators exactly three targets while reusing the same
  `AxisSide` the series carry — one vocabulary for "which vertical axis",
  no boolean flags (CLAUDE.md elevation rule).
- **Per-side bounds are a record, not a longer tuple.** `dataBounds` returns
  `ChartBounds { x; yLeft; yRight }` (padded ranges) and takes
  `(ChartSeries * AxisSide) list`, keeping the pure model free of the mutable
  window style; the window pairs visibility-filtered series with their sides
  at the seam.
- **ScottPlot's native right axis, driven lazily.** The window maps sides onto
  `plot.Axes.Left/Right` (5.1.59's `SetLimitsY(lo, hi, IYAxis)` and
  `Scatter.Axes.YAxis`); the right axis only receives LIMITS once a visible
  series uses it or the user pins a manual range, because an unset ScottPlot
  axis renders no ticks — this preserves the exact look of every existing
  single-axis chart.

# Deferred

- Spectral-axis helper move + the `nkDispersionChart` builder on this dual-axis
  spine — step 019.
- A right-axis label (`plot.Axes.Right.Label`) — nothing sets it yet; step
  019's n/k chart is its first real consumer.
- `ExperimentControls.ChartSeries` remains a structural twin of
  `ExperimentChart.ChartSeries` (unchanged by this slice) — still a Part D
  follow-up candidate.

# Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (same drift as slices 015–017); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- **Unused ScottPlot axes have no ticks because their range is unset**;
  pushing limits creates ticks. That is why `applyAxisLimits` guards the right
  axis on "in use or manual" — an unconditional push would paint phantom 0–1
  tick labels on the right edge of every single-axis chart. Once driven, a
  right axis whose last series is hidden keeps its limits/ticks (no clean
  "unset" API in 5.1.59) — accepted cosmetic edge.
- In polar mode the side re-assertion is skipped (`not polar` guard in
  `applySeriesStyle`): polar scatters hold polar-projected coordinates on the
  hidden default axes, and re-axing them would misplace the projection.
- The element picker's fixed part count grew 4 → 5 (`Header, X, Y-left,
  Y-right, Legend`); the existing axis-editing smoke test survives because
  `XAxis` kept picker index 1, and the new headless test picks the second
  series at index 6.
- `.manifest.state.json` (modified) and `.claude/` (untracked) are the
  arc-runner's / harness's own files — left alone, as in slices 001–017.

# Changelog

- 2026-07-06 — slice 018: second Y axis through the shared chart style spine
  (spec 0033 Part D): AxisSide (LeftAxis|RightAxis) on SeriesStyle with
  setSeriesAxisSide; ChartElement.YAxis of AxisSide; tri-state ChartAxis
  (AxisX | AxisY side) axis mutators + axisStyleOf; ChartStyleState split into
  yAxisLeft/yAxisRight; dataBounds → per-side ChartBounds (shared x,
  independent yLeft/yRight) with defaultState seeding all three axes;
  ChartWindow maps sides onto ScottPlot's native right axis (scatter
  Axes.YAxis in rebuildPlot/applySeriesStyle, per-axis SetLimitsY guarded
  in-use-or-manual, format/font/visibility appliers include Axes.Right,
  series-panel Axis picker ChartWindowSeriesAxis); polar + toCsv unchanged
  (CSV pinned); +4 model tests (ui-tests 255 → 259), +1 headless right-axis
  proof (ui-smoke 58 → 59); all suites green.
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
- 2026-07-05 — slice 011: DispersionModel → serializable eps tree lowering
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
