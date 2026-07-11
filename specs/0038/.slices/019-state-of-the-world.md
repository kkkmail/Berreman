# Step 019 — state of the world

## Where we are

Step 019 completes spec 0038 Part G's sample-editor leg: the editor's inline WrapPanel
material picker (search box + category facet + clickable options) is GONE, replaced by a
per-layer **Choose material…** verb that opens — or re-targets — the single-instance
MATERIALS window in Select state targeted at that row's `LayerPosition` (the step-016
`SelectionContext` with `SampleLayerTarget`, composed in the view exactly like the step-017
workbench Choose… flow). The return is the step-016 targeted `BindMaterialToLayer` — a row
deleted while the modeless window was open degrades to a no-op plus the status line — and the
picked id also becomes the toolbar's chosen material, so the bulk verbs keep one coherent
picking flow. The editor's material list is no longer a load-once snapshot: it RE-QUERIES
`MaterialProxy.listMaterials` on every Select-session return (chosen or cancelled) and on
window activation (`RefreshMaterials`; live cross-window notifications stay out of scope).
Threading the app category store to the editor's Materials-window composition touched the
`LibraryWindow` / `EditorLaunchers` signatures. Next in the arc: step 020 opens Part H
(versioning / lifecycle) per the manifest.

## What's working

- Replace the sample editor's WrapPanel material picker with a per-layer
  Choose material… verb: opens/re-targets the single-instance Materials
  window in Select state targeted at that LayerPosition (SampleLayerTarget),
  vanished-row return stays a no-op plus status line.
- Re-query MaterialProxy.listMaterials on every Select-session return and on
  window activation (new RefreshMaterials arm), replacing the load-once
  snapshot; ChooseMaterial stays the by-MaterialId contract and the picked id
  now doubles as the toolbar's chosen material.
- Compose openMaterialsSelect in SampleEditorWindow over WindowLauncher
  (MaterialsWindowKey, SelectOpen + Retarget); thread the app CategoryProxy
  through EditorLaunchers/LibraryWindow into the editor.
- 7 new tests (2 pure + 5 headless acceptances incl. the second-Choose
  re-target and the real-Material-editor activation re-query); build clean,
  suites 571 / 119 / 153 (+4) / 414 (+1).

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). The roster for this step is
  `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release`
  succeeded with 0 errors, **no MSB3277**, and zero warnings from the touched projects
  (Ui / Ui.Tests) — the only warnings are the step-001-catalogued pre-existing set in
  untouched files (NU1701 Wolfram ×2, SYSLIB0051 vendored MathNet ×2, FS3873 Dispersion,
  FS1125 SeriesDataTests ×4, FS0044 Controls/ChartWindow). Suites: ui-smoke **153/153**
  (checkpoint 149: the retired picker-filter acceptance is replaced by five step-019
  acceptances — Choose material… opens the REAL Materials window in Select state with the
  sample-layer banner and the return binds THAT row then converges on the chosen material;
  a second Choose re-targets the live window at the new row; deleting the target row first
  makes the return a no-op plus the status line; a material saved through the REAL Material
  editor appears after `window.Activate()`; a mid-session store write appears through the
  Select-return re-query), ui-tests **414/414** (checkpoint 413: the retired picker pure
  test is replaced by the RefreshMaterials re-query proof and the bind-sets-chosen /
  vanished-row-chooses-nothing proof), OpticalConstructor.Tests **571/571** (== checkpoint),
  BerremanTests **119 passed / 5 pre-existing skips** (== checkpoint). Logs in
  `specs/0038/.artifacts/019-diag-*.log`.
- Nothing deferred.

## Architecture

- **One picking flow, composed in the view**: `chooseMaterialForLayer` mirrors the step-017
  `chooseFromLibrary` shape — the SelectionContext (fixed `KindConstraint
  CatalogueKind.Sample`, `SampleLayerTarget position`, `onSelected` → targeted bind +
  re-query, `onCancelled` → re-query) is composed where the render's dispatch lives, and the
  IO goes through the new context field `openMaterialsSelect` (the functional-proxy seam;
  tests substitute recording stubs). The verb's subscription keys on its row-indexed id, and
  the position is a pure function of that id, so FuncUI recycling cannot leave a stale
  position behind.
- **The re-query is ONE arm** (`RefreshMaterials`) reached from all three triggers — the
  Select return's onSelected (dispatched after the bind), onCancelled, and the host's
  `Activated` hook (SampleEditorWindow moved to `mkProgram` + `Cmd.ofEffect` to capture
  dispatch, the MaterialsWindow precedent). The arm deliberately preserves `status`: the
  vanished-row message rides the same return as the refresh.
- **Bind-sets-chosen (recorded interpretation)**: with the picker gone, the Select-flow pick
  is the only material-choosing surface, so the successful `BindMaterialToLayer` also
  records `chosenMaterial` — keeping Select-by-material, bulk Set material, the
  substrate/lower "Set from chosen" verbs and the QWOT n-source reachable instead of
  permanently disabled. The `ChooseMaterial of MaterialId` contract itself is unchanged.
- **Single-instance discipline holds**: the editor's Select opens go through the SAME
  `MaterialsWindowKey` as the workbench strip button, so a live Materials window is
  RE-TARGETED (`MaterialsWindow.Retarget`, step 016) rather than duplicated — proven by the
  second-Choose acceptance. The categories store had to thread from the app scope
  (`EditorLaunchers` → `LibraryWindow` → `SampleEditorWindow`) so an editor-created
  Materials window shares the ONE live category catalogue; a locally-minted store would have
  poisoned the app-global registry with a divergent instance.
- **The verb renders beside its row, not inside it**: a layer row's Border subscribes
  `onPointerPressed` (ToggleLayer) and FuncUI subscribes Tunnel|Bubble, so an ancestor row
  swallows a nested clickable's press in the tunnel phase — the same reason the step-022
  group super-row carries no row-level press handler.
- **Compile order**: `SampleEditorWindow.fs` moved after `MaterialsWindow.fs` in the Ui
  fsproj (its composition constructs that window); the pure `SampleEditorView.fs` stays put.

## Deferred

- Threading the app-configured `SelectWindowsModal` into the editor's Select opens — the
  ctor takes `?selectWindowModality` (Domain default today); step 47 owns the composition
  acceptance.
- Live cross-window notifications (a Materials write pushing into open editors) — explicitly
  out of scope for this step; the re-query-on-return/activation is the contract.
- Live category names inside the Library window's lifted material-category facet (carried
  from step 015); `SampleLibraryControls` retirement sweep; the pre-existing warnings in
  untouched files (Part N).

## Gotchas

- **The task file's system-prompt path was stale again** — the IMPLEMENT worker prompt lives
  under `src/ai_strategy_generator/multistep/implement_worker.system-md` in the tool repo
  (the step-007..018 gotcha recurred). The slice's `TestWindows/SampleEditorView.fs:740-758`
  reference resolved to the relocated `Ui/SampleEditorView.fs` — the pre-move blob (commit
  5bc7083) shows 740–758 is exactly the `materialRow` picker block.
- **`SampleEditorContext` gained `materials` + `openMaterialsSelect`, and
  `SampleEditorWindow` / `LibraryWindow` ctors plus `EditorLaunchers.openLibraryWindow` /
  `openLibrarySelectWindow` gained the `CategoryProxy`** — an out-of-tree stub/substitute
  must add the fields (`openMaterialsSelect = fun _ _ -> ()`) and thread a categories store.
- **The sample editor's picker UiIds are GONE** (`SampleMaterialPicker`,
  `SampleMaterialSearchBox`, `SampleMaterialCategoryFilter`, `SampleMaterialOption_*`,
  `SampleMaterialCategory_*`) along with `filteredMaterials` / `SetMaterialSearchText` /
  `SelectMaterialCategory`; the new per-row family is `ChooseMaterialButton_<i>[_<j>]`. A
  test that used to click an option now either drives the real Select flow or dispatches
  `ChooseMaterial` through a captured-dispatch mount (`mountEditorLoop`).
- **Do not nest a clickable inside a layer row's Border** — the row's tunnel-phase
  ToggleLayer handler swallows it; put verbs beside the row (this step's shape).
- **`RefreshMaterials` must not clear the editor status** — the vanished-row message and the
  re-query ride the same Select return.
- **Add layer's no-choice fallback is the FIRST listed material (Silicon today)** — tests
  that add layers without choosing must not assume glass any more.
- **Headless `window.Activate()` raises `Window.Activated`** — the activation re-query is
  drivable headless with no reflection; `Show()` also activates, so the hook fires once at
  open (a harmless same-list refresh).
- Step 002–018 carried-over gotchas remain valid (baselines come from `.checkpoints-json`,
  not this SoW's YAML; the FuncUI Elmish host skips a structurally-equal model; a test that
  opens a Select-state window must CLOSE it or resolve its session — the registry is
  app-global; never fire a Select window's callbacks from anywhere but its update; never pin
  a numeric assertion on a store thickness magnitude; the appsettings.json write-back into
  test output copies is expected).

## Changelog

- 2026-07-11 — Step 019 (IMPLEMENT, attempt 1): the sample editor's WrapPanel material picker
  is replaced by the per-layer Choose material… verb over the step-016/017 Select machinery —
  opens/re-targets the single-instance Materials window in Select state targeted at the row's
  LayerPosition (SampleLayerTarget; vanished row = no-op + status line), the picked id binds
  the row AND becomes the toolbar's chosen material; the material list re-queries
  MaterialProxy.listMaterials on every Select return and window activation (new
  RefreshMaterials; load-once snapshot gone); SampleEditorWindow composes openMaterialsSelect
  over WindowLauncher (MaterialsWindowKey, SelectOpen/Retarget) and the app CategoryProxy
  threads through EditorLaunchers/LibraryWindow. 7 new tests. Build clean (no MSB3277);
  suites 571 / 119 / 153 / 414.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 571
  ui_smoke_tests: 153
  ui_tests: 414
```
