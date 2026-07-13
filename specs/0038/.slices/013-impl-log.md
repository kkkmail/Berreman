# Step 013 — impl log (ADD_COMPONENT UICOMP_XDUO_0009 MaterialsWindow)

## Progress

- [x] Read the worker task file, the ADD_COMPONENT system prompt + shared base,
  the project prompt, and the slice spec. (The task file's system-prompt path
  was stale again — located the real file, see Gotchas.)
- [x] Surveyed the impacted code and tests: the workbench file, WindowLauncher,
  FacetedTreeControls, Facets/LibraryFacets engines, EmbeddedChart,
  NkDispersionChart, AppContext/Program, and the six test files that touch the
  Materials bay surface.
- [x] Wrote the impl plan.
- [x] Implement `MaterialsWindowView.fs` (pure MVU + engine projection) +
  `MaterialsWindow.fs` (HostWindow composition root); added to the Ui fsproj
  between CategoryEditorWindow and the workbench.
- [x] Rewire the ribbon: dropped `materialsBay` from `mainBays`,
  `BayNames.materials` from the roster, the `Mat…` messages/arms, the bay-only
  model fields and projections/rows/helpers; `EditorLaunchers` now carries
  `openSampleEditor` + `openMaterialsWindow` (material/category editor
  launchers moved into the window's composition); the tab-strip row gained the
  right-aligned `Materials…` button (`WorkbenchIds.openMaterialsButton`,
  DockPanel right-dock beside the ribbon).
- [x] App composition threading: the factory threads through
  `EditorLaunchers.defaults` over the injected app-scope proxies (the step-008
  precedent); `Program.fs` comment records it; Ui + App build clean (0 errors,
  only the step-001-catalogued pre-existing warnings).
- [x] Tests: new `MaterialsWindowTests.fs` (18 pure + 7 headless, incl. all four
  slice-acceptance observations) and reworked bay-coupled suites —
  `MainWorkbenchTests` (samples-side kept; Materials halves moved to the window
  suite; roster/full-surface/strip-button pins reworked), `MainSceneMsgTests`
  (7 Mat* disarm tests → window-model equivalents in the window suite),
  `WireUiCompositionTests` (material proofs now strip button → real window →
  real editors), `AppContextTests` (two-surface proof via the Library bay),
  `EmbeddedChartTests` (view-panel host site → the window),
  `WindowLauncherTests` (the two step-008 material acceptance proofs now run
  end-to-end through the REAL Materials window), `ExperimentControlsTests` /
  `LayerBandsControlsTests` (bay-roster pins).
- [x] Diagnostic build + all four suites green; logs in
  `specs/0038/.artifacts/013-diag-*.log`; LF verified (no CRLF churn; new
  files CRLF=0).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindow.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` (comment-level)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/{OpticalConstructor.Ui.Tests.fsproj,
  MainWorkbenchTests.fs, MainSceneMsgTests.fs, WireUiCompositionTests.fs,
  AppContextTests.fs, EmbeddedChartTests.fs, WindowLauncherTests.fs,
  ExperimentControlsTests.fs, LayerBandsControlsTests.fs}`

## Decisions

- **Live category names**: the step-011 `materialFacets` category def reads
  the STATIC seeded catalogue (`categoryName`), so the window swaps in a
  live-`CategoryProxy` extractor under the SAME facet key
  (`liveMaterialFacets`) — that is what makes "a category rename re-labels the
  category facet" true (the discrete value key doubles as the branch label,
  step 009, so branch/offer/chip all re-label).
- **Tree shape**: the control's tree = a `Materials (N)` group of selectable
  entry leaves FIRST (the browsed objects; near the top for headless clicks),
  then the engine `buildTree` facet/branch grouping. Only `entry:` node codes
  select (the view panel's subject); the OFFERS are the apply surface.
- **Single-key selection per facet** in this window (re-apply replaces the
  chip); the engine's key-set OR stays available to a later slice.
- **Text filter = an ordinary engine constraint** (`textFilterDef` over the
  entry name) applied FIRST; its UI surface stays the filter box (no chip),
  and facet chips take after-counts over the searched population.
- **Gating is sticky**: `TreeBuildRequest` persists once the user clicks
  Show/Search (the gate protects the FIRST heavy render); a gated pass
  projects an EMPTY tree (skips `buildTree` entirely).
- **`EditorLaunchers` reshaped**, not paralleled: the bay-only
  `openMaterialEditor` / `openCategoryEditor` fields moved into
  `MaterialsWindow`'s own context; the workbench seam gained
  `openMaterialsWindow` for the strip button.
- **Threshold default**: `MaterialsWindow` bakes
  `TreeAutoBuildThreshold.defaultValue` with an optional ctor override — the
  app-configured `AppContext.settings` value threads at step 47 (the step-008
  modality-default precedent).

## Testing state

Diagnostic verification (not gate authority — Invariant 6): all four suites
green after the change.

- build: `dotnet build Berreman.slnx -c Release` — 0 errors, **no MSB3277**;
  the only warnings are the step-001-catalogued pre-existing set in untouched
  files (FS1125 SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow,
  SYSLIB0051 vendored MathNet ×2, NU1701 Wolfram.NETLink ×2). Zero warnings
  from the touched projects (Ui / App / Ui.Tests).
- ui-smoke: **131/131** (checkpoint 128, +3).
- ui-tests: **367/367** (checkpoint 363, +4).
- constructor-unit-tests: **559/559** (== checkpoint).
- unit-tests (BerremanTests): **119 passed / 5 pre-existing skips** (== checkpoint).
- One first-run failure was found and fixed in the ROUND (not shipped): the
  headless category-rename test's refresh dispatch produced a structurally
  EQUAL model, which the FuncUI Elmish host skips — see Gotchas.
- Line endings: `git diff --numstat` == `--ignore-cr-at-eol` (no CRLF churn);
  the three new files carry zero CRLF sequences.

## Artifacts

- Diagnostic logs land in `specs/0038/.artifacts/013-diag-*.log`.

## Gotchas

- The task file's system-prompt path
  `C:\GitHub\AI-Strategy-Generator\add_component_worker.system-md` is stale
  (the steps 007–012 gotcha recurred): the ADD_COMPONENT worker prompt lives at
  `src/ai_strategy_generator/multistep/add_component_worker.system-md` in the
  tool repo; read in full together with the shared base `arc-runner.system-md`.
- The slice's "today TestWindows/TableAndElementRotationView.fs:2339" location
  is stale: step 003 REAL-MOVED the workbench into
  `OpticalConstructor.Ui/TableAndElementRotationView.fs`; `materialsBay` sits
  at :2341 there and TestWindows carries no copy (verified by grep).
- The `## Operator note` section of the project prompt is EMPTY this round —
  no operator constraints in flight.
- **The FuncUI Elmish host skips a structurally-equal model**: a dispatch whose
  update returns a record equal to the previous one re-renders NOTHING — a
  "refresh" dispatch must actually change the model. The window's projections
  re-query the proxies per render, so any real verb (they all change state)
  shows external writes in the same pass; tests that need a refresh after an
  out-of-band store write must dispatch a CHANGING commit (the category-rename
  headless test commits a narrowing filter).
- **`MaterialsWindowKey` is app-global registry state in tests**: every test
  that opens the real window (directly or through the strip button) closes it —
  a leaked registration would make a LATER strip click ACTIVATE a stale window
  over the wrong stores (extends the step-008 gotcha).
- **The step-011 category facet reads the STATIC seeded catalogue** — the
  window swaps in a live-`CategoryProxy` extractor under the same facet key
  (`liveMaterialFacets`); without it a category rename would NOT re-label the
  facet. Note a rename changes the discrete value KEY too (the key doubles as
  the label, step 009), so an applied category chip keeps matching the OLD key
  until removed — the chip names exactly what the user clicked.
- **`EditorLaunchers` changed shape** (step-008 consumers beware):
  `openMaterialEditor` / `openCategoryEditor` are GONE (they live in
  `MaterialsWindow`'s own context now); `openMaterialsWindow` is new. Any
  out-of-tree substitute record must follow.
- Step 002–012 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the window registry is app-global
  test state in Ui.Tests; the appsettings.json write-back into test output
  copies is expected).
