# Step 013 — state of the world

## Where we are

Step 013 lands the first WINDOW of spec 0038 Part E: the single-instance
Materials window (UICOMP_XDUO_0009) — the step-012 `FacetedTreeControls`
instantiated over the step-011 material facet catalogue through the step-009
engine, beside a view panel and the workbench's former Materials verbs, opened
through the step-008 `WindowLauncher` under `MaterialsWindowKey`. The Materials
BAY is gone from the ribbon; the tab-strip row's right-aligned `Materials…`
button is the constructor-side entry point (the launcher-form button lands in
step 45; step 47 owns the composition acceptance). Next: Part F instantiates
the same control as the Library window over the whole entry corpus.

## What's working

- Add MaterialsWindowView.fs + MaterialsWindow.fs (UICOMP_XDUO_0009): a
  HostWindow mounting a pure MVU model that projects MaterialProxy +
  CategoryProxy through the step-11 material facets (category names resolved
  LIVE) into FacetedTreeControls state — offers with count-previews, breadcrumb
  chips with after-counts, two named representations, Enter/LostFocus text
  filter, Show/Search gating over TreeAutoBuildThreshold, and selectable
  entry leaves.
- View panel beside the tree: selected entry metadata plus the embeddable
  dual-axis n/k chart (Controls/EmbeddedChart); verbs Add / Edit / Remove /
  Categories… rewired to the window — Add/Edit/Categories through the step-8
  WindowLauncher editor keys, Remove keeping its inline confirm gate and the
  typed MaterialStillReferenced block; every verb's write shows in the same
  render pass.
- Remove the Materials bay from the ribbon: materialsBay, BayNames.materials,
  the Mat… messages/arms, and the bay-only model fields are gone; the ribbon
  tab-strip row gains the right-aligned Materials… button
  (EditorLaunchers.openMaterialsWindow → MaterialsWindowKey); the window
  factory threads through EditorLaunchers.defaults so the App builds unchanged.
- 25 new headless/pure tests pin the slice acceptance: the strip button opens
  ONE window (second click activates), facet constraints narrow the corpus in
  the same render pass, the verbs operate over the shared app-scoped stores
  through the real editors, and a category rename re-labels the category facet.
- Suites 559 / 119 / 131 (+3) / 367 (+4); build clean, no MSB3277, zero
  warnings from touched projects.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this
  worker exits (ADD_COMPONENT Invariant 6 — the worker acts, it runs no
  checks). The roster for this step is `build`, `unit-tests`,
  `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c
  Release` succeeded with 0 errors, **no MSB3277**, and zero warnings from the
  touched projects (Ui / App / Ui.Tests) — the only warnings are the
  step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests ×4, FS3873 Dispersion, FS0044 ChartWindow, SYSLIB0051
  vendored MathNet ×2, NU1701 Wolfram.NETLink ×2). Suites: ui-smoke
  **131/131** (checkpoint 128, +7 new window proofs −4 retargeted bay proofs:
  strip-button single-instance with close-reopen, facet-narrowing with chip
  restore, shared-store verbs through the real editors incl. the refused
  referenced remove, category-rename re-label incl. the create picker,
  Edit/Categories real-editor opens, view panel with the embedded chart, and
  gating with zero rows until Show/Search), ui-tests **367/367** (checkpoint
  363, +18 new window pure tests −14 retired bay/disarm tests),
  OpticalConstructor.Tests **559/559** (== checkpoint), BerremanTests **119
  passed / 5 pre-existing skips** (== checkpoint). Logs in
  `specs/0038/.artifacts/013-diag-*.log`.
- Nothing deferred.

## Architecture

- **The window is a projection host, not a new engine**: the MVU model holds
  only elevated intent state (applied constraints, committed `TextQuery`, the
  active representation record, a sticky `TreeBuildRequest`, the selected
  `MaterialId`, a window-local `MaterialRemoveGate`, the last
  `MaterialError`); every render re-queries the proxies and re-runs the
  step-009 folds, so any write — the window's own verbs or another window's —
  shows on the next dispatch-driven pass.
- **Live category names via def substitution**: `liveMaterialFacets` swaps the
  step-011 category def's extractor for one resolving names through the LIVE
  `CategoryProxy` under the SAME facet key — the engine stays untouched and
  "a category rename re-labels the facet" holds because the discrete value key
  doubles as the branch label (step 009).
- **Tree = entries first, facets after**: a `Materials (N)` group of
  selectable `entry:`-coded leaves (the browsed objects, clickable without
  scrolling) leads; the engine `buildTree` facet/branch grouping follows as
  display. Only `entry:` codes select (the view panel's subject); the OFFERS
  are the apply surface; a gated pass projects an EMPTY tree (the point is
  skipping the heavy render).
- **Single-key selection per facet this round** (re-apply replaces the chip);
  the engine's key-set OR within a facet stays available data-side. The text
  filter is an ordinary engine constraint (applied first) whose UI surface is
  the filter box, not a chip; facet chips take after-counts over the searched
  population.
- **The launcher seam reshaped, not paralleled**: `EditorLaunchers` lost the
  bay-only `openMaterialEditor`/`openCategoryEditor` (they moved into
  `MaterialsWindow`'s own context, still through the step-008 editor keys over
  the ONE registry) and gained `openMaterialsWindow`; `defaults` bakes the
  window factory, so the App composition threads mechanically (step 47 owns
  acceptance; the ctor takes an optional `TreeAutoBuildThreshold` for that
  threading).
- **Strip-button placement**: a DockPanel in `mainControlBar` docks the
  keyed, AutomationId-only `Materials…` button RIGHT (docked first, so the
  ribbon can never push it off-screen) beside the untouched generic Ribbon —
  no change to the Controls project.

## Deferred

- The Library window over the whole entry corpus and `EntryProtection` —
  Part F; Browse+Select modes and the Select-state re-target — step 016.
- The launcher-form Materials button (step 45) and threading the
  app-configured `AppContext.settings.treeAutoBuildThreshold` into the window
  (step 47; the ctor override exists, defaults bake the Domain default — the
  step-008 modality-default precedent).
- Multi-key (OR) selection within one facet from this window's UI; host-side
  branch-click-to-apply (offers are the apply surface this round).
- `MaterialsControls` (the step-015 list surface) is no longer referenced by
  the Ui project — it stays in Controls with its own tests until a later
  sweep retires it.
- The pre-existing warnings in untouched files remain for spec 0038 Part N's
  sweep (carried from steps 002–012).

## Gotchas

- **The task file's system-prompt path was stale again** — the ADD_COMPONENT
  worker prompt lives under
  `src/ai_strategy_generator/multistep/add_component_worker.system-md` in the
  tool repo; located and read in full together with the shared base
  `arc-runner.system-md` (the step-007..012 gotcha recurred). The slice's
  "today TestWindows/TableAndElementRotationView.fs:2339" was also stale — the
  workbench moved to Ui at step 003.
- **The FuncUI Elmish host skips a structurally-equal model**: a "refresh"
  dispatch whose update returns an equal record renders nothing. All real
  verbs change state, so proxy re-query-per-render still shows every write in
  the same pass — but a test refreshing after an out-of-band store write must
  dispatch a CHANGING commit (the category-rename headless test commits a
  narrowing filter).
- **A category rename re-keys the category facet's discrete value** (the key
  IS the label, step 009): an applied category chip keeps matching the OLD
  key until removed. Recorded interpretation — the chip names exactly the
  value the user clicked.
- **`EditorLaunchers` changed shape**: any out-of-tree substitute record must
  drop `openMaterialEditor`/`openCategoryEditor` and add
  `openMaterialsWindow` (the step-008 "ctor argument changed shape" class of
  break).
- **`MaterialsWindowKey` is app-global registry state in tests** — every test
  that opens the real window (directly or via the strip button) must CLOSE
  it, or a later strip click ACTIVATES a stale window over the wrong stores
  (extends the step-008 registry gotcha).
- **No registry edit was needed**: `.contract-ids/XDUO-json` and
  `specs/0038/.contracts-json` already carry UICOMP_XDUO_0009 (declared,
  step 13) — the supervisor maintains both (the step-007 precedent).
- Step 002–012 carried-over gotchas remain valid (baselines come from
  `.checkpoints-json`, not the SoW YAML; the appsettings.json write-back into
  test output copies is expected; FuncUI Enter commits need `e.Handled` —
  the step-012 control already does).

## Changelog

- 2026-07-11 — Step 013 (ADD_COMPONENT UICOMP_XDUO_0009, attempt 1): added the
  single-instance Materials window (`MaterialsWindowView.fs` +
  `MaterialsWindow.fs` — FacetedTreeControls over the step-011 facets with
  live category names, view panel with the embedded n/k chart, Add / Edit /
  Remove / Categories… verbs through the step-008 launcher keys, inline
  confirm gate + typed MaterialStillReferenced block), removed the Materials
  bay from the ribbon (BayNames/mainBays/Mat… arms/bay fields), added the
  right-aligned `Materials…` strip button opening the window under
  `MaterialsWindowKey`, and moved/authored the test coverage (25 window tests;
  bay suites reworked). Build clean (no MSB3277); suites 559 / 119 / 131 / 367.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 559
  ui_smoke_tests: 131
  ui_tests: 367
```
