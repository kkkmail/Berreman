# State of the world — spec 0033, slice 024

# Where we are

Slice 024 puts the two workbenches ON SCREEN: where slices 015/016 declared the
domain-free list surfaces (`MaterialsControls` / `SampleLibraryControls`) and
slices 022/023 declared the editor windows, this slice wires them into the Main
screen's ribbon as the **Materials** bay and the **Library** bay (the samples
workbench — the label freed by the step-014 Selector rename). The
`TableAndElementRotationView` host now holds the step-005/006 write seams
(`MaterialProxy` / `SampleProxy`) in its model — threaded through
`initWith`/`initMainWith` and composed mechanically in
`OpticalConstructor.App/Program.fs` (the final WIRE_UI step owns the composition
acceptance) — and projects each bay by re-querying its proxy on every render,
the `libraryState`/`flattenNode` discipline, so every verb's write refreshes the
list in the same render pass.

# What's working

- Materials + Library workbench bays wired into the Main ribbon (BayNames.materials "Materials", BayNames.library "Library"; workbenches sit with the Selector, Details stays last)
- MaterialProxy + SampleProxy threaded into the host model via initWith/initMainWith; App composition updated mechanically with the two in-memory stores
- Search boxes and facet selectors drive the searchMaterials/searchSamples query seams; rows re-queried per render so writes show in the same render pass; complexity=None presets render ViewOnly (Edit verb removed)
- Add/Edit open the step-022/023 editor windows through the injected EditorLaunchers seam; Make-multilayer opens the sample editor on a new sample
- Remove is confirm-gated inline; MaterialStillReferenced surfaces as an inline message naming the referencing samples — store, selection and list untouched; never a cascade
- View panels: read-only metadata + the step-19 dual-axis n/k chart for materials (shared NkDispersionChart.inlineCanvas) and the Details-bay band view (shared sampleBandsState) for samples
- +12 pure and +5 headless UiIds-driven tests incl. all four acceptance criteria (ui-tests 294 → 306, ui-smoke 76 → 81); all suites green

# Tests

All gates in the slice roster pass in the worker's local ADVISORY runs
(Invariant 6: the arc-runner gate engine re-runs them authoritatively after
exit). Full logs in `specs/0033/.artifacts/024-*.log`, plus the TDD red capture
`024-red-tdd.log` (FS0039/FS0003 naming the missing production symbols before
implementation).

- `build` — solution builds Release/x64, exit 0, 0 errors; the 91 warnings are
  the pre-existing NU190x/MSB3277/SYSLIB0051/FS3873 noise, none from the new
  code.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0 failed
  (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 407 passed, 0 failed (= baseline; no Domain code
  touched).
- `ui-smoke` — 81 passed, 0 failed (76 baseline + 5 new headless proofs: search
  filters the tree rows; Add opens the real MaterialEditorWindow / Edit the real
  SampleEditorWindow; referenced-material remove surfaces the message with the
  store unchanged; unreferenced material AND sample removes drop the row in the
  same render pass; both View panels render).
- `ui-tests` — 306 passed, 0 failed (294 baseline + 12 new pure tests).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 407
  ui_smoke_tests:         81
  ui_tests:               306
```

# Architecture

- **The bays re-query their proxy in the state projection, not in update.**
  `materialsState`/`samplesState` run `searchMaterials`/`searchSamples` on every
  render (exactly as `libraryState` resolves through `LibraryProxy`), so "every
  verb re-queries its proxy" is a structural property: any dispatched message
  re-renders, and the projection reads the store's current truth. No row cache
  to invalidate.
- **Window opening is an injected launcher seam** (`EditorLaunchers`, a
  `[<ReferenceEquality>]` function record on the model; `defaults` opens the
  real step-022/023 windows). Tests substitute recording launchers by record
  update — the headless Add/Edit acceptance still constructs the REAL editor
  windows while recording them, so the proof is end-to-end without global
  mutable hooks.
- **Confirm-gated remove carries the requested id**
  (`RemoveConfirm<'id> = NoRemoveConfirm | ConfirmingRemove of 'id`): a
  selection change between Remove and Confirm can never delete a different
  entry, and any query/selection message disarms a stale confirm.
- **Typed errors stay typed to the edge**: the model stores
  `MaterialError option` / `SampleError option`; the inline message row renders
  each case's diagnostic `reason` (the store's `MaterialStillReferenced` reason
  already names the referencing samples — spec 0033 step 006), so the bay adds
  no parallel error prose.
- **Facet codes live only at the control boundary**: `Msg` carries the domain
  facets (`MaterialCategory option` / `DispersionFilter` /
  `SubstrateKind option`); the public `…Code`/`…OfCode` maps translate for the
  domain-free controls and are pinned by tests.
- **One inline n/k renderer**: the editor's private preview canvas REAL-MOVED to
  `NkDispersionChart.inlineCanvas (autoId) (chart)`; the editor preview and the
  Materials View panel both draw through it (per-side bounds from the 018
  `ChartStyle.dataBounds`). The sample View panel reuses the Details bay's
  band construction verbatim via the extracted public `sampleBandsState`.
- **Compile order**: the four editor files moved before
  `TableAndElementRotationView.fs` in the fsproj (they depend only on
  Domain/Controls/NkDispersionChart), so the launcher defaults reference the
  window types directly — no forward-reference hooks.

# Deferred

- The full composition acceptance (launcher entry, window sizing for the taller
  workbench bays, real disk-backed stores) — the final WIRE_UI step; this slice
  updated `Program.fs` mechanically so the solution builds and the Main window
  carries live stores.
- Row-level virtualization / paging for large stores: the list surface renders
  whatever the search returns inside its 220 px scroll viewport (the step-015/016
  control's own design); fine for the in-memory stores.
- A toggle-deselect on the workbench rows (clicking the selected row again keeps
  it selected; only View toggles) — a later UX refinement if wanted.
- Duplicate-name creation flows ("Add copies the selection") — the editor owns
  creation; the workbench only launches it.

# Gotchas

- **`MaterialProxy.createInMemory` is an OPTIONAL type extension** (declared in
  module `Library`, type in `MaterialLibrary`): callers must OPEN
  `OpticalConstructor.Domain.Library` — the view file scopes that open inside a
  private `DefaultStores` nested module to keep `Sample`/`Thickness` name
  collisions away from its Avalonia names; `Program.fs` takes the plain open.
  `SampleProxy.createInMemory` is intrinsic and works fully qualified.
- **`MaterialLibrary.MaterialId` in expression position names the union CASE**,
  so `MaterialId.tryCreate` is unreachable qualified — the row handler parses
  the Guid and constructs the case directly (the documented type/case
  collision).
- **Bay order is pinned twice**: `LayerBandsControlsTests` pins Details LAST and
  `LibraryControlsTests` pins the first-five prefix — the workbenches were
  placed between Selector and Experiments to satisfy both. The step-014 "no bay
  labelled Library" pins were consciously updated: "Library" is offered again as
  the SAMPLES WORKBENCH (this slice's mandate), distinct from the Selector.
- **Two `LayerBandsControls` instances can coexist** (Details bay + the Library
  bay's View panel) with identical control names; the panel exists only while
  `viewedSample` is set, and tests scope band assertions to `SampleViewPanel`
  descendants.
- **Headless row clicks must land inside the list's scroll viewport** — rows
  order by Guid (arbitrary), so the headless tests narrow the search box first
  (the 022 layout lesson, applied to lists).
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\implement_worker.system-md` does not exist
  (same drift as slices 015–023); the real file is under
  `src\ai_strategy_generator\multistep\`.
- `.manifest.state.json` (modified) and the untracked `.claude/` folder are the
  arc-runner's / harness's own files — left alone, as in slices 001–023.

# Changelog

- 2026-07-06 — slice 024: Materials + Library workbench bays WIRED (spec 0033
  step 024): BayNames.materials "Materials" + BayNames.library "Library" (the
  samples workbench — the step-014 freed label) added to BayNames.all and
  mainBays (between Selector and Experiments; Details stays last);
  MaterialProxy + SampleProxy threaded into the host model through
  initWith/initMainWith with the App composition updated mechanically;
  materialsState/samplesState flatten searchMaterials/searchSamples into
  MaterialsControls/SampleLibraryControls rows on EVERY render (writes show in
  the same render pass; complexity=None presets ViewOnly); search boxes + facet
  selectors drive the query seams via domain-typed Msgs (codes lifted at the
  control boundary); Add/Edit open the step-022/023 editor windows through the
  injected EditorLaunchers seam (defaults = the real windows; Make-multilayer
  opens a new sample); Remove confirm-gated inline via RemoveConfirm<'id>
  carrying the requested id; MaterialStillReferenced surfaces as an inline
  message naming the referencing samples with the store untouched; View panels
  show read-only metadata + the step-19 dual-axis n/k chart (via the REAL-MOVED
  shared NkDispersionChart.inlineCanvas) for materials and the Details-bay band
  view (extracted public sampleBandsState) for samples; editor files moved
  before the main view in compile order; +12 pure and +5 headless UiIds-driven
  tests incl. all four acceptance criteria (ui-tests 294 → 306, ui-smoke
  76 → 81); all suites green.
- 2026-07-06 — slice 023: MaterialEditorWindow DECLARED (UICOMP_XDUO_0004,
  spec 0033 step 023): the Material editor window in
  OpticalConstructor.TestWindows over a NEW pure Domain edit model
  (MaterialComplexityEditor — state + 17-arm message DU +
  Result-returning apply; facets stored independently of their toggles so
  unchecking restores the default losslessly); anisotropy 3-way selecting
  the ConstantEpsValue shape / EpsDispersiveValue flavor, absorbing and
  dispersive toggles, OPTIONAL active + magnetic unlocks; per-segment
  dispersion editor (interval bounds + full DispersionModel picker incl.
  ForouhiBloomer/BrendelBormann/raw SumOfTerms, lowered via toEpsAxis —
  non-lowerable picks surface the typed NotAFiniteTermSum reason);
  anisotropy-constrained gyration-class panel + handedness switch;
  Polder-mu panel (scalar/gyromagnetic + axis); inline step-19 dual-axis
  n/k preview with the restated imaginaryIndexGainWarning rule; Save
  through MaterialProxy (addMaterial minting MaterialId.create /
  updateMaterial) storing complexity = Some with properties = toProperties;
  complexity = None entries open view-only with no Save affordance;
  ofComplexity/toComplexity round-trips every editable built-in
  value-identically; component declared, not wired; +18 pure and +9
  headless UiIds-driven tests incl. all four acceptance criteria (ui-tests
  276 → 294, ui-smoke 67 → 76); all suites green.
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
