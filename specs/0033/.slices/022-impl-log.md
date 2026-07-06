# Impl log — spec 0033, slice 022 (ADD_COMPONENT UICOMP_XDUO_0003 SampleEditorWindow)

## Progress

- [x] Read task file, worker system prompt (add_component_worker + the
      arc-runner base), project prompt, slice spec, slice-021 SoW, the
      016 impl-log (ADD_COMPONENT precedent), SampleStackEditor.fs,
      ElementId.fs (Sample/SampleProxy/SubstrateKind/CrystalOrientation),
      MaterialLibrary.fs (MaterialProxy/MaterialEntry), Templates.fs
      (dbrCell/dbrPeriods QWOT precedent), SampleLibraryControls(+Tests),
      TestWindows view/window pairs, TestApp.fs, both fsprojs.
- [x] Red: SampleEditorWindowTests.fs + its fsproj entry added first; the
      build fails with 100 × FS0039 naming the missing `SampleEditorView` /
      `SampleEditorWindow` symbols (`022-red-tdd.log`).
- [x] Implementation: SampleEditorView.fs (~700 lines, pure MVU + FuncUI
      view) + SampleEditorWindow.fs (HostWindow composition root) + the
      TestWindows fsproj entries.
- [x] Two mid-round fixes (see Decisions 8–9): `[<ReferenceEquality>]` on
      the Model (Elmish equality constraint vs the dispersion function
      cases), and wrap-panel layout after a headless hit-test diagnostic
      showed the wide headless font metrics pushing controls off-screen.
- [x] Green: all five gates pass in the advisory local runs; 21 new tests
      (13 pure `ui-tests` + 8 headless `ui-smoke`), no test edits after
      red beyond the temporary diagnostic (added and removed).
- [x] Contract registry: `specs/0033/.contracts-json` already records
      `UICOMP_XDUO_0003` as `declared` / `declaringStep: 22`
      (supervisor-maintained, the 016 precedent) — nothing to write.
- [x] LF policy verified (see Gotchas).
- [x] State-of-the-world written.

## Decisions

1. **View + Window pair, in TestWindows.** The slice letter says "beside
   the existing pop-out ChartWindow.fs", but slice 017 REAL-MOVED
   ChartWindow.fs into OpticalConstructor.Controls; the step header's
   `declaring_project: OpticalConstructor.TestWindows` is authoritative.
   The component follows the TestWindows convention (ElementMovementView /
   SnapToBeamWindow): `SampleEditorView.fs` is the pure Model/Msg/update +
   FuncUI projection, `SampleEditorWindow.fs` the `HostWindow` running
   `Program.mkSimple init update view |> Program.withHost`.
2. **The context is the IO seam.** `[<ReferenceEquality>]
   SampleEditorContext = { samples : SampleProxy; requestClose }` — the
   functional-proxy/Context convention. `update` persists through
   `context.samples` on `SaveClicked` (`addSample` minting
   `Library.newSampleId ()` for `NewSample`, `updateSample` for
   `ExistingSample id`) and calls `requestClose` only on `Ok`; a proxy
   `Error` surfaces its `reason` as the status and the window stays open.
   `CancelClicked` closes without any write. Tests substitute recording
   stubs of the same shape.
3. **One mandated `RepeatCountStepper`, two stepper surfaces.** The
   toolbar fold-count stepper (feeding `MakeRepeatBlock`) carries the
   mandated literal id; each period group's INLINE stepper (dispatching
   `SetRepeatCount` by whole periods) carries the derived
   `RepeatCountStepper[Plus|Minus]_<groupIndex>` family. The acceptance
   drives both: fold at K=3 (2 layers → 6 films), then the inline stepper
   3→4 (→ 8 films).
4. **Anisotropy is decided by evaluating tensors at 600 nm.** The
   dispersive presets (silicon / langasite, `complexity = None`) have no
   single tensor until a wavelength is chosen, so `isAnisotropicEntry`
   evaluates `entry.properties.getProperties (WaveLength.nm 600)` (the
   repo's common visible-band reference) and reports anisotropic when eps
   or mu has any off-diagonal / unequal-diagonal component, or when ANY
   rho component is nonzero (gyrotropy is orientation-sensitive; the
   isotropic default rho is the zero matrix). Pinned by a pure test over
   eight built-ins (glass/vacuum/silicon/EUV-Mo isotropic;
   uniaxial/biaxial/active/langasite anisotropic).
5. **QWOT: derived read-only, applied by Set thickness.** `QwotEntryBox`
   takes λ (nm); n = Re[√ε₁₁] at λ from the CHOSEN material through the
   engine `getEps` path (the NkDispersionChart extraction); the readout
   (`QwotDerivedText`) shows t = λ/(4n) in canonical metres, "—" while
   there is no chosen material / parsable λ. While the derivation is
   valid, `SetLayerHeightButton` applies IT to the selection; otherwise
   the plain nm entry (`SampleLayerHeightBox`) applies. Thickness is
   stored via `Thickness.nm` (the DBR λ/4 precedent, Templates.fs:103,106).
6. **View-level arms where the step-21 DU has no arm.** The Domain
   message set has no Add-layer and no per-position deselect, and Domain
   is not in this slice's `touches`: `AddLayerClicked` appends a
   `SingleLayer` (chosen material, else the first listed; 100 nm;
   `PrimaryAxes`) by immutable record update over the same data;
   `ToggleLayer` removes an already-selected position from the selection
   Set directly (adding goes through `SelectLayer` so position validity
   stays Domain-checked). The per-layer orientation editor applies
   `SetOrientationOfSelected` over a single-position selection and then
   restores the user's multi-selection. All-zero Euler angles normalise
   to `PrimaryAxes` (the identity is data, not a zero rotation).
7. **AutomationId on every variable-membership control** (stack rows,
   super-rows, cell rows, expanders, inline steppers, material options,
   thickness cells, orientation editors) — the window re-renders on EVERY
   Elmish message, so FuncUI recycling makes `Name` unsafe there (the
   SampleLibraryControls / ExperimentControls precedent). Fixed
   structural controls (name/description/QWOT/thickness boxes, the
   readouts, the stack-table panel) keep `Name`.
8. **`[<ReferenceEquality>]` on the Model.** FuncUI's Elmish loop
   requires `'model : equality`, and `MaterialEntry.properties` carries
   dispersion FUNCTION cases (no structural equality). Reference equality
   satisfies the constraint and makes every dispatch re-render — exactly
   this window's contract (`update` returns a fresh record).
9. **Wrap panels, found by a real hit-test.** The first headless run
   failed 3 tests; a temporary diagnostic (window.GetVisualsAt over the
   clicked point) showed the material option at x=1582 in a 1080-wide
   window — the headless platform's font metrics are far wider than
   desktop, so horizontal StackPanels ran controls off-screen where
   clicks land on nothing. The material picker became label-above +
   WrapPanel (measured at finite width, so it wraps), and both toolbars
   became WrapPanels (each label+box pair one wrap item). The diagnostic
   was removed after the fix.
10. **Component declared, not wired** (ADD_COMPONENT obligation): no
    parent view, launcher, or Ribbon bay references SampleEditorWindow;
    the headless tests construct the window directly over fresh
    in-memory proxies (`SampleProxy.createInMemory`,
    `MaterialProxy.createInMemory (samplesReferencing samples)`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/SampleEditorView.fs`
  (new) — `UiIds` (13 mandated literals + derived row/group/option id
  families), `EditorTarget`, `SampleEditorContext`, `Model`, `Msg`,
  `init` / `toSample` / `update` (routing the bulk verbs through
  `applySampleStackMsg`; typed error reasons → status), the pure helpers
  (`isAnisotropicEntry`, `refractionIndexAt`, `qwotThickness`,
  `qwotDerived`, `thicknessLabel`, `filmsCount`, `substrateCode`), and
  the FuncUI view (name/description rows, SubstrateKind facet, wrapping
  material picker, the stack table with collapsible period super-rows —
  rotating-triangle expander via a literal `RotateTransform` 0°/90° —
  inline steppers and nested cell rows, per-layer orientation editors on
  anisotropic layers only, films-count readout, wrap toolbars, QWOT row,
  status line, Save/Cancel row with positive/negative styling).
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/SampleEditorWindow.fs`
  (new) — the `HostWindow` composition root: resolves the material
  choices once from `MaterialProxy.listMaterials`, injects the context
  (`requestClose = this.Close`), sets the `SampleEditorWindow`
  Name/AutomationId, runs the Elmish loop.
- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj`
  — the two `<Compile>` entries after SnapToReflectedWindow.fs.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`
  (new) — 13 pure contract tests (`ui-tests`): the mandated UiIds, init
  shapes (new/existing), `toSample`, toggle multi-select, AddLayer
  defaults, fold-count clamp, typed-error → status (empty-selection fold;
  count-below-1 step), `isAnisotropicEntry` over eight built-ins, QWOT
  derivation (value + no-material/no-λ), `thicknessLabel`, Save
  add/update/Cancel through recording stubs, failing-save keeps open.
  8 headless proofs (`ui-smoke`) driving the REAL window by UiIds: every
  mandated id present; the 2×K acceptance (2 layers → select both → K=3 →
  fold → "6", inline stepper → "8", collapse hides cell rows);
  select-by-material bulk set-thickness (5/50/5 nm); Save persists a new
  sample (add) and closes; Save updates an existing sample in place;
  Cancel discards; orientation editor present for uniaxial / absent for
  glass; QWOT readout + apply.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
  — `<Compile Include="SampleEditorWindowTests.fs" />` after
  NkDispersionChartTests.fs.

## Testing state

TDD: red first (100 × FS0039 naming the missing production symbols,
`022-red-tdd.log`), then the production module + window, then green. The
only post-red test-file churn was a TEMPORARY hit-test diagnostic (added
to locate the off-screen-click failure, removed once fixed) — no shipped
test was weakened. All five gates in the slice roster pass in the
worker's local ADVISORY runs (per Invariant 6 the arc-runner gate engine
is the sole gate authority and re-runs them after exit):

- `build` — `dotnet build Berreman.slnx -c Release` exit 0, 0 errors (the
  86 warnings are the pre-existing MSB3277/NU190x noise, none from the
  new files).
- `unit-tests` — BerremanTests 119 passed, 5 skipped (pre-existing), 0
  failed (= 119 baseline; no core file touched).
- `constructor-unit-tests` — 407 passed, 0 failed (= baseline; this slice
  adds no Domain code).
- `ui-smoke` — 67 passed, 0 failed (59 baseline + 8 new headless proofs).
- `ui-tests` — 276 passed, 0 failed (263 baseline + 13 new pure tests).

Acceptance checks (inside the ui-smoke run): repeating a 2-layer
selection K=3 times shows the films readout expanding to 6 = 2*K (and 8
after the inline stepper); select-by-material + bulk set-thickness
updates only the two glass rows (5 nm) leaving the vacuum row at 50 nm;
Save persists through SampleProxy — addSample for a new sample (store
grows 11 → 12, name/description/structure verified), updateSample for an
existing one (renamed in place, store stays 11).

## Artifacts

- `specs/0033/.artifacts/022-red-tdd.log` — red build (FS0039 × 100).
- `specs/0033/.artifacts/022-build.log` — solution build.
- `specs/0033/.artifacts/022-unit-tests.log` — BerremanTests run.
- `specs/0033/.artifacts/022-constructor-unit-tests.log` — constructor tests.
- `specs/0033/.artifacts/022-ui-smoke.log` — ui-smoke run.
- `specs/0033/.artifacts/022-ui-tests.log` — ui view tests.

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` does not
  exist (same drift as slices 015–021); the real file is
  `AI-Strategy-Generator\src\ai_strategy_generator\multistep\add_component_worker.system-md`
  and was read from there.
- No `## Operator note` content is in flight for this attempt (the project
  prompt's heading is present but empty).
- The slice's "beside the existing pop-out ChartWindow.fs" is stale: slice
  017 REAL-MOVED ChartWindow.fs into OpticalConstructor.Controls. The
  `declaring_project: OpticalConstructor.TestWindows` header is
  authoritative — the new window lives in TestWindows beside the other
  test windows (Decision 1).
- **Headless font metrics are much wider than desktop.** A horizontal
  StackPanel offers children infinite width, so long option rows run past
  the window edge and headless clicks at their translated centre hit
  NOTHING (silently — the control is still "effectively visible").
  Symptom: clicks dispatch for early-row controls but not later ones. Use
  WrapPanels measured at finite width for any long control row a headless
  test must click (Decision 9).
- **FuncUI's Elmish host requires model equality** — a model holding
  `MaterialEntry` values (dispersion function cases) must be
  `[<ReferenceEquality>]` (Decision 8); this is the same convention the
  Domain proxies document for Elmish-held records.
- **Save/Cancel close the window during the press event** — the test
  click helper skips the MouseUp when `window.IsVisible` turned false
  after MouseDown, or Avalonia raises on the closed window.
- **Line endings:** the two edited fsprojs show ZERO CRLF churn
  (`git diff --numstat` identical with and without `--ignore-cr-at-eol`);
  the three NEW `.fs` files are LF on disk (0 CRLF sequences), matching
  the `.gitattributes` policy.
- `.manifest.state.json` (modified) and the untracked `.claude/` folder
  are the arc-runner's / harness's own files (same as slices 001–021),
  left alone.
