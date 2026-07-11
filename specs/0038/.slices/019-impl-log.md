# Step 019 — impl log

## Progress

- [x] Read the slice, the step-016/017/018 SoWs, and the touched files.
- [x] Product: SampleEditorView — picker → per-layer Choose material… verb, RefreshMaterials.
- [x] Product: SampleEditorWindow — categories + activation re-query + Select-open composition.
- [x] Product: LibraryWindow / EditorLaunchers — thread the CategoryProxy.
- [x] Tests: picker tests rewritten; five new step-019 headless acceptances + two pure tests.
- [x] Diagnostic build + all four suites green (not gate authority — Invariant 6).
- [x] State of the world.

## Files modified

Product (OpticalConstructor.Ui):

- `SampleEditorView.fs` — removed the WrapPanel material picker vertical (materialRow /
  materialSearchRow / materialCategoryRow, the `materialSearchText` / `materialCategory`
  model fields, `SetMaterialSearchText` / `SelectMaterialCategory` messages,
  `filteredMaterials`, `materialCategoryCode`, and the picker UiIds). Added: the
  `ChooseMaterialButton_<i>[_<j>]` id family; `chooseMaterialForLayer` (composes the
  step-016 `SelectionContext<MaterialEntry>` with `KindConstraint CatalogueKind.Sample` +
  `SampleLayerTarget position`; `onSelected` → `BindMaterialToLayer` + `RefreshMaterials`,
  `onCancelled` → `RefreshMaterials`); `chooseMaterialVerb` (rendered BESIDE each layer row —
  outside its toggling Border — owner window resolved from the click's TopLevel);
  `RefreshMaterials` message + arm (re-queries `context.materials.listMaterials`, keeps the
  status line, keeps the current list on a typed store refusal); `SampleEditorContext` gained
  `materials : MaterialProxy` and `openMaterialsSelect : Window -> SelectionContext<MaterialEntry> -> unit`;
  the successful `BindMaterialToLayer` arm now ALSO sets `chosenMaterial` (see Gotchas).
- `SampleEditorWindow.fs` — ctor gained `categories : CategoryProxy` and optional
  `?selectWindowModality`; switched `mkSimple` → `mkProgram` with the `Cmd.ofEffect`
  dispatch capture (the MaterialsWindow precedent); `this.Activated` dispatches
  `RefreshMaterials`; composes `openMaterialsSelect` over `WindowLauncher.create` under
  `MaterialsWindowKey` with `SelectOpen (owner, retarget)` — the retarget closure downcasts
  to `MaterialsWindow` and calls its step-016 `Retarget`.
- `LibraryWindow.fs` — ctor gained `categories : CategoryProxy`, threaded into
  `SampleEditorWindow`.
- `MaterialsWindow.fs` — comment-only: the step-019 composition note now names the real seam.
- `TableAndElementRotationView.fs` — `EditorLaunchers.openLibraryWindow` /
  `openLibrarySelectWindow` gained the `CategoryProxy` parameter (defaults + both dispatch
  sites pass `model.categories`).
- `OpticalConstructor.Ui.fsproj` — `SampleEditorWindow.fs` moved AFTER `MaterialsWindow.fs`
  (its composition now constructs the Materials window); the pure `SampleEditorView.fs`
  stays in its old slot.

Tests (OpticalConstructor.Ui.Tests):

- `SampleEditorWindowTests.fs` — `freshProxies` returns the categories store too; `textOf`
  upgraded to the descendant-tolerant shape; added `commitFilter` (the MaterialsWindowTests
  Enter-commit shape), `stubMaterialsProxy`, `mountEditorLoop` (captured-dispatch mount) and
  `observeOpenedWindows`. The pure picker-narrowing test was REPLACED by two step-019 pure
  tests (RefreshMaterials re-query surfaces a later write and preserves a pending status;
  BindMaterialToLayer records the picked id as chosen — vanished row chooses nothing). The
  picker-filter headless acceptance was REPLACED by five step-019 acceptances: Choose
  material… opens the REAL Materials window in Select state (banner + pair) and the return
  binds THAT row then converges on the chosen material; a second Choose re-targets the live
  window at the new row; deleting the target row first makes the return a no-op plus the
  status line; a material saved through the REAL Material editor appears after
  `window.Activate()` (the activation re-query); a material added mid-session appears through
  the Select-return re-query. Four picker-clicking tests rewritten (Add layer takes the
  first listed material; the select-by-material and QWOT tests drive `mountEditorLoop` and
  dispatch the unchanged `ChooseMaterial`).
- `MaterialsWindowTests.fs` — `mountEditorWithDispatch`'s context gained the two new fields.
- `WindowLauncherTests.fs` / `LibraryWindowTests.fs` — ctor updates (categories), dropped
  the picker-option clicks (Add layer falls back to the first listed material).
- `MainWorkbenchTests.fs` — the recording `EditorLaunchers` stub arities.

## Testing state

All diagnostics green locally (gate authority stays with the arc-runner's gate engine):

- `dotnet build Berreman.slnx -c Release` — **0 errors, no MSB3277**; the only warnings are
  the step-001-catalogued pre-existing set in untouched files (NU1701 Wolfram ×2,
  SYSLIB0051 vendored MathNet ×2, FS3873 Dispersion, FS1125 SeriesDataTests ×4; FS0044
  Controls/ChartWindow on non-incremental builds). The touched projects compile with zero
  warnings.
- BerremanTests: **119 passed / 5 pre-existing skips** (== checkpoint 119).
- OpticalConstructor.Tests: **571 passed** (== checkpoint 571).
- ui-smoke: **153 passed** (checkpoint 149: −1 retired picker acceptance, +5 step-019
  acceptances).
- ui-tests: **414 passed** (checkpoint 413: −1 retired picker pure test, +2 step-019 pure
  tests).

## Artifacts

- `specs/0038/.artifacts/019-diag-build.log`
- `specs/0038/.artifacts/019-diag-unit-tests.log`
- `specs/0038/.artifacts/019-diag-constructor-tests.log`
- `specs/0038/.artifacts/019-diag-ui-smoke.log`
- `specs/0038/.artifacts/019-diag-ui-tests.log`

## Gotchas

- The task file's system-prompt path was stale again (`implement_worker.system-md` lives
  under `src/ai_strategy_generator/multistep/` in the tool repo — the step-007..018 gotcha
  recurred). The slice's `TestWindows/SampleEditorView.fs:740-758` reference resolved to the
  relocated `Ui/SampleEditorView.fs` — verified against the pre-move blob at commit 5bc7083:
  lines 740–758 are EXACTLY the `materialRow` function (the whole "Material:" picker block).
- **Recorded interpretation — the successful `BindMaterialToLayer` also sets
  `chosenMaterial`.** The slice keeps the by-MaterialId `ChooseMaterial` contract unchanged,
  but with the inline picker gone nothing in the UI would ever set the chosen material, and
  the toolbar bulk verbs (Select by material / Set material / substrate + lower "Set from
  chosen" / the QWOT n source) would be permanently disabled dead UI. Making the Select-flow
  pick double as the chosen material keeps every verb reachable through ONE picking flow;
  the vanished-row return stays a strict no-op (plus status) and chooses nothing.
- **A clickable must not nest inside a layer row's Border** — the row subscribes
  `onPointerPressed` (ToggleLayer) and FuncUI subscribes Tunnel|Bubble, so an ancestor row
  would swallow (and mis-dispatch) a nested verb's press in the tunnel phase. The Choose
  material… verb therefore renders BESIDE its row in a horizontal wrapper (the group
  super-row precedent: its Border carries no press handler for the same reason).
- **Compile-order move**: `SampleEditorWindow.fs` now compiles AFTER `MaterialsWindow.fs`
  (its composition constructs the Materials window). An out-of-tree edit adding a reference
  from any file between the old and new slots back to `SampleEditorWindow` would need a
  fresh look at the ordering.
- **`SampleEditorContext` gained two fields and `SampleEditorWindow` / `LibraryWindow` /
  `EditorLaunchers` gained the categories parameter** — an out-of-tree substitute must add
  `materials` + `openMaterialsSelect` (stub `fun _ _ -> ()`) and thread a `CategoryProxy`.
- **`RefreshMaterials` must NOT clear the status line** — the same Select return that
  reports a vanished row also dispatches the re-query; clearing status there would erase the
  no-op message the user (and the acceptance test) must see.
- **Headless `window.Activate()` DOES raise `Window.Activated`** (Avalonia.Headless) — the
  activation re-query acceptance drives it directly; no reflection or fallback needed.
- The step-016 registry gotcha extends here: every step-019 acceptance that opens the
  Materials window through the REAL app-global `WindowRegistry` resolves its session (Select
  verb) or closes the window before exiting, or a later test's Select open would re-target a
  stale instance over dead stores.
