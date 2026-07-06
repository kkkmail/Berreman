# State of the world — spec 0033, slice 023

# Where we are

Slice 023 declares the Material editor window (ADD_COMPONENT
UICOMP_XDUO_0004): where slice 013 gave materials their serializable edit
model (`MaterialComplexity`) and slice 022 put the first editing surface over
the sample library, this slice puts the Part F progressive-complexity ladder
on screen — `MaterialEditorWindow` in OpticalConstructor.TestWindows, a
FuncUI/Elmish `HostWindow` over a NEW pure Domain edit model
(`MaterialComplexityEditor`: state + message DU + `Result` apply, the
SampleStackEditor discipline), whose Save persists through
`MaterialProxy.addMaterial` / `updateMaterial` with `complexity = Some model`
and `properties = model.toProperties`. The component is declared with
headless UiIds-driven proofs and registered (supervisor-maintained) but NOT
wired into any parent view or launcher — wiring is a later WIRE_UI slice.

# What's working

- MaterialEditorWindow (UICOMP_XDUO_0004) declared in TestWindows: MaterialEditorView (pure MVU over the new Domain MaterialComplexityEditor) + HostWindow composition root; all 13 slice-mandated UiIds plus derived option/box id families
- Progressive-unlock ladder in Domain: anisotropy 3-way (ConstantEpsValue shape / EpsDispersiveValue flavor), absorbing toggle (transparent vs absorbing constants), dispersive toggle (constant vs segment list), OPTIONAL active + magnetic toggles — each lifts exactly one aspect off its default; facets stored independently of their toggles so unchecking restores the default losslessly and re-checking restores the edits
- Per-segment dispersion editor: wavelengthInterval bounds + a DispersionModel picker over the full catalogue incl. ForouhiBloomer, BrendelBormann and the raw SumOfTerms escape hatch, lowered via toEpsAxis; non-lowerable picks are accepted and surface the typed NotAFiniteTermSum reason (Save blocked honestly)
- Symmetry-class gyration panel constrained by the anisotropy choice (isotropic ⇒ cubic 23/432; uniaxial ⇒ the diagonal two-component class; biaxial ⇒ 222/monoclinic-2/monoclinic-m/triclinic-1) with a handedness switch; the activity toggle renders only while the choice offers a rotating class (REMOVED, not greyed); Polder-mu panel: scalar or gyromagnetic with the magnetization-axis picker
- Live preview embeds the step-19 dual-axis n/k chart (nkDispersionChart + nkDispersionStyle, per-side bounds via ChartStyle.dataBounds) as an inline canvas; a negative sampled k surfaces the advisory gain warning (the imaginaryIndexGainWarning rule restated in Domain)
- Save mints MaterialId.create for a new entry / updates an existing one in place, storing complexity = Some with properties = toProperties; complexity = None entries (silicon/langasite/vacuum) open VIEW-ONLY with no ladder and no Save affordance; ofComplexity/toComplexity round-trips every editable built-in value-identically
- +18 pure contract tests and +9 headless UiIds-driven proofs incl. all four acceptance criteria (ui-tests 276 → 294, ui-smoke 67 → 76)

# Tests

All gates in the slice roster pass in the worker's local ADVISORY runs
(Invariant 6: the arc-runner gate engine re-runs them authoritatively after
exit). Full logs in `specs/0033/.artifacts/023-*.log`, plus the TDD red
capture `023-red-tdd.log` (100 × FS0039 naming the missing production
symbols before implementation).

- `build` — solution builds Release/x64, exit 0, 0 errors; the 93 warnings
  are the pre-existing NU190x/MSB3277/SYSLIB0051/FS3873 noise, none from the
  new files.
- `unit-tests` (BerremanTests) — 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 407 passed, 0 failed (= baseline; the new
  Domain module's windowless tests run under `ui-tests`, the 022 precedent).
- `ui-smoke` — 76 passed, 0 failed (67 baseline + 9 new headless proofs).
- `ui-tests` — 294 passed, 0 failed (276 baseline + 18 new pure tests).

Nothing deferred.

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 407
  ui_smoke_tests:         76
  ui_tests:               294
```

# Architecture

- **The ladder is lossless by construction.** The Domain state stores every
  facet independently of its unlock toggle; `toComplexity` reads a facet
  only while its toggle is on. "Unchecking restores the default losslessly"
  is therefore a structural property of the derivation, not per-toggle
  bookkeeping — and re-checking restores the user's edits for free.
- **The derivation targets only the serializable value trees** of
  `Berreman/Dispersion.fs` (`ConstantEpsValue` / `EpsDispersiveValue` /
  `RhoWithDispValue` / `MuWithDispValue`); engine tensors come from the
  existing `toProperties` / `toEpsAxis` seams — no tensor math re-derived.
  Three axis slots (indices; per-segment models) are always stored and the
  anisotropy choice decides how many are read, so switching anisotropy never
  destroys entered values and per-axis engine trees seed losslessly through
  the raw `SumOfTerms` identity.
- **Honest negatives for the transcendental catalogue cases.**
  ForouhiBloomer / BrendelBormann are pickable (the mandate) but carry no
  finite term data, so a complexity cannot represent them; the pick is
  preserved and `toComplexity` returns the typed `SegmentNotLowerable`
  carrying the engine's `NotAFiniteTermSum` reason — surfaced in the
  derived-model readout and blocking Save with the reason (the Part G
  unsupported-formula precedent).
- **`ofComplexity` seeds verbatim; only messages snap.** The gyration-class
  picker is constrained by the anisotropy choice at MESSAGE time (unlocking
  activity / switching anisotropy snaps an un-offered class to the first
  offered); seeding never snaps (§0.4 forbids clamping), so the built-in
  round trip is value-identical — including activeCrystal's `PlanarActive`,
  which the picker does not offer but the state preserves.
- **IO through a `[<ReferenceEquality>]` Context** (`MaterialProxy` +
  `requestClose`), the functional-proxy convention; the window is the
  composition root; tests substitute recording stubs. View-only entries
  (`complexity = None`, or a complexity the editor cannot seed — typed
  `UnsupportedComplexity` for dispersive rho/mu) open with NO ladder and NO
  Save affordance, previewing the entry's own preset properties.
- **The preview embeds the step-19 chart MODEL** (`nkDispersionChart` +
  `nkDispersionStyle`) rendered as an inline dual-axis canvas with per-side
  bounds from the 018 `ChartStyle.dataBounds` — the `ExperimentControls`
  inline-canvas approach; the pop-out ChartWindow is a native ScottPlot
  surface and cannot live inside a re-rendering FuncUI view.

# Deferred

- Wiring the window into a host (the materials workbench's Add/Edit verbs
  from step 015, a launcher entry) — a later WIRE_UI slice; nothing
  references MaterialEditorWindow yet.
- The REAL-MOVE of the Ui validation helpers ("move with the editors",
  Part F): Ui is outside this slice's `touches`, so the gain rule is
  restated in Domain (doc-linked to Ui/Validation.fs:92) — a later
  Ui-touching slice can perform the move and re-point both.
- Per-axis segment model editing: the state carries three model slots per
  segment (seeded per-axis via SumOfTerms), but the picker sets all active
  axes at once; a per-axis picker surface is a later refinement.
- Per-component gyration magnitudes: the class picker seeds every component
  at the default 1.5e-6 (the active-crystal precedent); component-level
  entry boxes are a later refinement.
- Dispersive (formula-valued) gyration and Polder-mu editing: the editor
  covers the constant cases; a seeded dispersive rho/mu opens view-only with
  the typed reason.
- Coefficient-level editing of the picked dispersion models (the picker
  applies representative published/default coefficient sets; FB = Horiba
  TN13 a-Si, BB = Rakić 1998 gold — the AC-B6 sets).

# Gotchas

- **ForouhiBloomer / BrendelBormann cannot land in a `MaterialComplexity`**
  (transcendental — no finite term data). The picker offers them per the
  mandate; the derivation surfaces the typed reason and Save is blocked
  honestly. Making them storable needs a closure-carrying complexity case —
  a spec change.
- **"Activity toggle REMOVED for non-rotating choices" is currently
  vacuous-but-live:** every anisotropy choice offers at least one rotating
  class (the engine's `GyrationClass` holds only rotation-producing
  classes), so the toggle always renders; the removal is data-driven off
  `availableGyrationClasses`, not a dead branch.
- The headless lossless acceptance observes the `MaterialComplexitySummary`
  readout, which embeds a STRUCTURAL DIGEST (`hash complexity`) — equal
  summaries denote equal derived complexities in-process; do not compare
  the digest across runs.
- FuncUI's Elmish host requires `'model : equality` — the view Model holds
  engine dispersion functions and is `[<ReferenceEquality>]` (the 022
  precedent). Every ladder control carries an AutomationId, never `Name`
  (toggle-driven membership changes; FuncUI cannot rename recycled styled
  controls); all rows are WrapPanels (the 022 headless-font-metrics lesson).
- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` does not
  exist (same drift as slices 015–022); the real file is under
  `src\ai_strategy_generator\multistep\`.
- `.manifest.state.json` (modified) and the untracked `.claude/` folder are
  the arc-runner's / harness's own files — left alone, as in slices 001–022.

# Changelog

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
