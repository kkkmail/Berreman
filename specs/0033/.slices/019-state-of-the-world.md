# State of the world — spec 0033, slice 019

# Where we are

Slice 019 closes the chart-side arc of Part D that slices 017 (shared chart
control REAL-MOVED into Controls) and 018 (second Y axis through the style
spine) prepared: the pure spectral-axis helpers now live in
`OpticalConstructor.Domain.SpectralAxis` (moved out of the Ui's
MaterialPreview), and TestWindows gains the first real dual-axis consumer —
a pure `nkDispersionChart` builder that turns an entry's
`OpticalPropertiesWithDisp` + display unit + canonical-meter range into the
shared `ExperimentChart`, with n born on the LEFT axis and k flipped to the
RIGHT through the 018 `setSeriesAxisSide` seam.

# What's working

- SpectralAxis (axisLabel / spectralRange / axisTicks) REAL-MOVED from Ui's MaterialPreview into OpticalConstructor.Domain; MaterialPreview, MaterialsView and the AC-D7 tests re-pointed, no duplication
- TestWindows references Analytics; new pure NkDispersionChart builder: OpticalPropertiesWithDisp + display unit + Range<WaveLength> -> shared ExperimentChart (n = Re[√ε₁₁], k = Im[√ε₁₁] via the engine getEps path, x-axis from SpectralAxis in the display unit)
- Paired style seed nkDispersionStyle assigns n LEFT / k RIGHT on the 018 dual-axis spine (the side is style, not data)
- Dispersive entries yield curves and non-dispersive entries flat lines through the SAME builder; +4 headless model tests (ui-tests 259 → 263), all other suites at their baselines

# Tests

All gates in the slice roster pass in the worker's local ADVISORY runs; the
arc-runner gate engine re-runs them authoritatively after exit. Full logs in
`specs/0033/.artifacts/019-*.log`.

- `build` — solution builds Release/x64, exit 0, 0 errors; the 94 warnings are
  the pre-existing MSB3277/FS1125 noise (same count as slice 018), none from
  the edited files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 373 passed, 0 failed (= baseline; only a one-line
  open re-point in MaterialPreviewTests, no test bodies changed).
- `ui-smoke` — 59 passed, 0 failed (= baseline).
- `ui-tests` — 263 passed, 0 failed (259 baseline + 4 new NkDispersionChart
  model tests: the acceptance shape n→left / k→right spanning the requested
  400…800 nm range, silicon curve, vacuum flat line n=1/k=0, AC-D7 eV/nm
  rescale with identical y data).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 373
  ui_smoke_tests:         59
  ui_tests:               263
```

# Architecture

- **The spectral axis is Domain vocabulary now.** `SpectralAxis` sits right
  after `Units.fs` in the Domain spine and stays the RELABEL/RESCALE-only seam
  (AC-D7): ranges are canonical meters, ticks are a display projection. Both
  chart hosts (Ui and TestWindows) reach it without new cross-project edges —
  Domain already referenced Berreman + Analytics.
- **The n/k chart's axis assignment ships as a paired style seed, not chart
  data.** `nkDispersionChart` returns the plain shared `ExperimentChart`
  (slice wording); `nkDispersionStyle` = `ChartStyle.defaultState` +
  `setSeriesAxisSide kSeriesIndex RightAxis` — exactly the host pattern the
  018 state-of-the-world prescribed, so `ExperimentChart` stays domain-neutral
  and `toCsv`/hosts unchanged.
- **§7 deviation — the engine data builders were bypassed for the engine's
  own getEps seam.** The slice named `calculateN11Re`/`calculateXi11Im`, but
  their sampling path (`getWaveLengthValue`) mis-scales λ by 10⁻⁹ (meter
  magnitude re-wrapped as nm), which draws dispersive entries FLAT —
  contradicting this step's acceptance. The builder samples the identical
  meter grid via `SpectralAxis.axisTicks Nanometer` and extracts n,k exactly
  as `MaterialImport.exportCsv` does (`Complex.Sqrt (getEps w)[0,0]`), the
  established in-repo workaround (also `SourceSpec.SpectralProfile.sample`).
  No dispersion formula re-derived.

# Deferred

- Wiring `nkDispersionChart`/`nkDispersionStyle` into an actual TestWindows
  window / the pop-out `ChartWindow` (and the first real right-axis label,
  `plot.Axes.Right.Label`) — a later WIRE step; this slice ships the pure
  builder + model proofs per the acceptance.
- The `getWaveLengthValue` λ×10⁻⁹ engine defect still underlies the Ui's
  `SeriesData.plot*Series` (§H.11) and MaterialsView's `dispersionPreview`
  (both pre-existing consumers of `calculate*`). A root fix touches the
  SOLVER's wavelength-sweep sampling too (`getWaveLength` → incident light) —
  needs its own slice against `Analytics`/`BerremanTests`, outside this
  slice's `touches`.
- `ExperimentControls.ChartSeries` remains a structural twin of
  `ExperimentChart.ChartSeries` — still a Part D follow-up candidate (since 017).

# Gotchas

- **`Analytics.getWaveLengthValue` re-wraps the meter magnitude in the range
  case's native unit** (`WaveLength.create`, `Fields.fs:289-292`), so every
  `calculate*` dispersion builder — and the main `calculate` sweep's
  wavelength axis — evaluates at λ×10⁻⁹. Empirically pinned this round: the
  first advisory ui-tests run failed with `silicon n span across the range
  = 0` (the λ² terms vanish below double precision, so the flatness is
  EXACT). Two prior slices already documented the workaround
  (`SourceSpec.fs:120-125`, `MaterialImport.fs:179-181`); this slice's
  builder follows it.
- `OpticalConstructor.Tests` is not in the slice `touches`, but the REAL-MOVE
  required a one-line `open OpticalConstructor.Domain.SpectralAxis` in
  `MaterialPreviewTests.fs` (its AC-D7 cases reference the moved helpers
  unqualified) — the slice-017 "callers re-point via opens" precedent.
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (same drift as slices 015–018); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\implement_worker.system-md`.
- `.manifest.state.json` (modified) and `.claude/` (untracked) are the
  arc-runner's / harness's own files — left alone, as in slices 001–018.

# Changelog

- 2026-07-06 — slice 019: spectral-axis helpers + the n/k dispersion chart
  (spec 0033 step 019): axisLabel/spectralRange/axisTicks REAL-MOVED from
  Ui's MaterialPreview into OpticalConstructor.Domain.SpectralAxis (callers
  re-pointed: MaterialPreview header, MaterialsView, MaterialPreviewTests
  open); TestWindows gains the Analytics reference and the pure
  NkDispersionChart builder (OpticalPropertiesWithDisp + display unit +
  Range<WaveLength> → shared ExperimentChart; n = Re[√ε₁₁] left / k = Im[√ε₁₁]
  right via the paired nkDispersionStyle seed on the 018 setSeriesAxisSide
  seam; x-axis from SpectralAxis; engine getEps sampling per the
  exportCsv/SourceSpec precedent because calculateN11Re/calculateXi11Im
  mis-scale λ by 1e-9 and draw dispersive entries flat); dispersive → curve,
  non-dispersive → flat line through one builder; +4 headless model tests
  (ui-tests 259 → 263); all suites green.
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
