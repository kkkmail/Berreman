# State of the world — spec 0033, slice 022

# Where we are

Slice 022 declares the Sample editor window (ADD_COMPONENT
UICOMP_XDUO_0003): where slice 021 gave the sample library its pure,
windowless edit model (`SampleStackEditor`), this slice puts the first real
editing surface over it — `SampleEditorWindow` in
OpticalConstructor.TestWindows, a FuncUI/Elmish `HostWindow` whose stack
table, bulk toolbar and steppers dispatch the step-21 `SampleStackMsg`
vocabulary, whose material choices come from `MaterialProxy.listMaterials`,
and whose Save persists through `SampleProxy` (add for new, update for
existing). The component is declared with headless UiIds-driven proofs and
registered (supervisor-maintained) but NOT wired into any parent view or
launcher — wiring is a later WIRE_UI slice.

# What's working

- SampleEditorWindow (UICOMP_XDUO_0003) declared in TestWindows: SampleEditorView (pure MVU over the step-21 SampleStackEditor) + HostWindow composition root; all 13 slice-mandated UiIds plus derived row/group/option id families
- Stack table renders each PeriodGroup as ONE collapsible super-row (rotating-triangle expander via literal RotateTransform) with an inline repeat-count stepper (SetRepeatCount by whole periods) and its unit-cell layers nested beneath; toggle multi-select rows plus a bulk wrap-toolbar (select-by-material, set thickness, set material, set orientation, remove, move up/down, make repeat block with the mandated RepeatCountStepper fold count)
- Material choice by MaterialId over MaterialProxy.listMaterials; the per-layer orientation editor (φ/θ/ψ) is INCLUDED only for anisotropic materials (tensor test at 600 nm through the engine getProperties path) and absent — not greyed — for isotropic ones
- Optional QWOT entry derives t = λ/(4n) read-only into canonical metres (n = Re[√ε₁₁] via the engine getEps path; Thickness.nm storage — the DBR λ/4 precedent) and Set-thickness applies it while valid
- Save persists through the injected SampleEditorContext: addSample minting SampleId.create for a new sample, updateSample in place for an existing one, close on Ok, typed reason surfaced on Error; Cancel discards; Save/Cancel share one row with positive/negative styling
- +13 pure contract tests and +8 headless UiIds-driven proofs incl. all three acceptance criteria: 2-layer selection × K=3 → films readout 6 = 2*K (then 8 via the inline stepper), select-by-material bulk set-thickness updates only matching layers, Save persists through SampleProxy add/update (ui-tests 263 → 276, ui-smoke 59 → 67)

# Tests

All gates in the slice roster pass in the worker's local ADVISORY runs
(Invariant 6: the arc-runner gate engine re-runs them authoritatively after
exit). Full logs in `specs/0033/.artifacts/022-*.log`, plus the TDD red
capture `022-red-tdd.log` (100 × FS0039 naming the missing production
symbols before implementation).

- `build` — solution builds Release/x64, exit 0, 0 errors; the 86 warnings
  are the pre-existing MSB3277/NU190x noise, none from the new files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 407 passed, 0 failed (= baseline; no Domain
  code added).
- `ui-smoke` — 67 passed, 0 failed (59 baseline + 8 new headless proofs).
- `ui-tests` — 276 passed, 0 failed (263 baseline + 13 new pure tests).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 407
  ui_smoke_tests:         67
  ui_tests:               276
```

# Architecture

- **The window is a thin projection over the Domain edit model.** Every
  bulk verb routes through `applySampleStackMsg`; the typed
  `SampleStackEditError` reasons surface verbatim as the status line, so
  the Domain's `count >= 1` / foldability rules are the single source of
  validation. Only two arms live at the view level because the step-21 DU
  has no arm for them (Domain is outside this slice's `touches`): AddLayer
  appends a `SingleLayer` by immutable record update, and toggle-deselect
  removes a position from the selection Set (adding still goes through
  `SelectLayer` so validity stays Domain-checked).
- **IO through a Context, Elmish-compatible by reference equality.**
  `SampleEditorContext` (`[<ReferenceEquality>]`) bundles the
  `SampleProxy` write seam with the host's `requestClose`; the Model holds
  it and is itself `[<ReferenceEquality>]` — required because
  `MaterialEntry.properties` carries dispersion FUNCTION cases, and
  correct because `update` returns a fresh record so every dispatch
  re-renders. Tests substitute recording stub contexts.
- **The window is the composition root**: it resolves the material entries
  once from `MaterialProxy.listMaterials`, wires `requestClose =
  this.Close`, and runs `Program.mkSimple`. Save chooses the proxy
  function by the `EditorTarget` DU (`NewSample` mints via
  `SampleId.create`; `ExistingSample id` updates in place) — no naked
  bool, no id reuse ambiguity.
- **Anisotropy is a tensor property, not a name check**: eps/mu
  off-diagonal or unequal diagonal, or any nonzero rho, evaluated at a
  600 nm reference through the engine `getProperties` path — so the
  dispersive presets (silicon isotropic, langasite anisotropic) classify
  correctly without a parallel material taxonomy.

# Deferred

- Wiring the window into a host (the samples workbench's Add/Edit verbs,
  a launcher entry) — a later WIRE_UI slice; nothing references
  SampleEditorWindow yet.
- Substrate-plate / lower-half-space editing: the editor covers the films
  stack (the step-21 message set's scope); the `SubstrateKind` facet edits
  the geometry tag only.
- An Add-layer / per-position-deselect arm in the Domain
  `SampleStackMsg` DU — if a later slice adds them, the two view-level
  arms shrink to plain dispatches.
- Per-layer QWOT (each selected layer's own n): the derivation currently
  takes n from the chosen material — the single read-only readout the
  slice letter describes.

# Gotchas

- **Headless font metrics are far wider than desktop.** A horizontal
  StackPanel offers children infinite width; long rows run controls past
  the window edge and a headless click at the translated centre hits
  nothing, silently (the control still reports effectively-visible). This
  window uses WrapPanels (label-above for the material picker; each
  label+box pair one wrap item in the toolbars). Any future headless test
  that clicks a long control row needs the same layout discipline.
- **FuncUI's Elmish host requires `'model : equality`** — a model holding
  engine dispersion values must be `[<ReferenceEquality>]`.
- **Save/Cancel close the window during the press event**: a click helper
  must skip the MouseUp once `window.IsVisible` is false.
- The toolbar fold stepper carries the mandated `RepeatCountStepper`
  literal; the per-group inline steppers carry the derived
  `RepeatCountStepper[Plus|Minus]_<groupIndex>` family (one literal, two
  stepper surfaces — recorded as Decision 3 in the impl-log).
- The slice letter's "beside the existing pop-out ChartWindow.fs" is
  stale (017 moved ChartWindow.fs to Controls); the step header's
  `declaring_project: OpticalConstructor.TestWindows` was followed.
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` does
  not exist (same drift as slices 015–021); the real file is under
  `src\ai_strategy_generator\multistep\`.
- `.manifest.state.json` (modified) and the untracked `.claude/` folder
  are the arc-runner's / harness's own files — left alone, as in slices
  001–021.

# Changelog

- 2026-07-06 — slice 022: SampleEditorWindow DECLARED (UICOMP_XDUO_0003,
  spec 0033 step 022): the Sample editor window in
  OpticalConstructor.TestWindows — SampleEditorView (pure MVU over the
  step-21 SampleStackEditor; 13 mandated UiIds + derived families) +
  HostWindow composition root; name/description fields, SubstrateKind
  facet, stack table with collapsible period super-rows
  (rotating-triangle expander, inline SetRepeatCount steppers, nested
  cell rows), toggle multi-select + bulk wrap-toolbar dispatching
  SampleStackEditor messages, material choice from
  MaterialProxy.listMaterials, per-layer orientation editor only for
  anisotropic materials (600 nm tensor test), QWOT t = λ/(4n) read-only
  into canonical metres with Set-thickness applying it, Save through
  SampleProxy (addSample minting SampleId.create / updateSample) via a
  [<ReferenceEquality>] context with requestClose, Cancel discards;
  component declared, not wired; +13 pure and +8 headless UiIds-driven
  tests incl. all three acceptance criteria (ui-tests 263 → 276,
  ui-smoke 59 → 67); all suites green.
- 2026-07-06 — slice 021: pure sample-stack edit model (spec 0033 step 021):
  OpticalConstructor.Domain.SampleStackEditor — SampleStackEditState
  (structure + Set<LayerPosition> selection; AtSingleLayer | AtCellLayer, a
  repetition adds no position), eleven-arm SampleStackMsg
  (SelectLayer/SelectByMaterial/ClearSelection, thickness/material/orientation
  edits over the selection, RemoveSelected, MoveSelectedUp/Down with
  permutation-remapped selections, MakeRepeatBlock folding a contiguous
  top-level run into one Repeated group, SetRepeatCount resizing by whole
  periods), applySampleStackMsg returning Result with typed reason-carrying
  errors (InvalidRepeatCount restating the Ui validateRepeatCount count >= 1
  rule, SelectionNotFoldable, NotARepeatGroup); StackEditor.groupLayers
  untouched; +30 windowless tests incl. both acceptance criteria
  (constructor tests 377 → 407); all suites green.
- 2026-07-06 — slice 020: crystal orientation on sample layers (spec 0033
  step 020): CrystalOrientation (PrimaryAxes | EulerRotation of
  RotationConvention * phi * theta * psi) with toRotation : Rotation in
  Library (identity / Rotation.create); SampleLayer.orientation defaulting to
  PrimaryAxes at all 16 construction sites; ResolvedLayer (layerWithDisp +
  orientation) threads the orientation through resolveSampleMaterials, and
  sampleToSystem applies non-identity orientations at build time via
  Layer.rotate → OpticalProperties.rotate (substrate plates included; nothing
  stored rotated); +4 orientation tests (constructor tests 373 → 377,
  EulerRotation = direct rotate, PrimaryAxes = stored tensors, rotatePiX
  named-shortcut equivalence, substrate path); all suites green.
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
- 2026-07-05 — slice 012: ForouhiBloomer + BrendelBormann catalogue cases
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
