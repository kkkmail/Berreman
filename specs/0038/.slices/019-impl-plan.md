# Step 019 — impl plan

## Goal

Replace the sample editor's inline WrapPanel material picker with a per-layer
**Choose material…** verb that opens the single-instance MATERIALS window in
Select state targeted at that `LayerPosition` (step-016 `SelectionContext` with
`SampleLayerTarget`; vanished row ⇒ no-op + status line), and replace the
editor's load-once material snapshot with re-queries of
`MaterialProxy.listMaterials` on every Select-session return and on window
activation. Live cross-window notifications stay out of scope. The by-MaterialId
selection contract (`ChooseMaterial of MaterialId`) is unchanged.

## Approach

1. **`SampleEditorView.fs`** — remove the picker block (`materialRow` +
   `materialSearchRow` + `materialCategoryRow`, the search/category model fields
   and messages, `filteredMaterials`, and the picker UiIds); add the per-row
   `ChooseMaterialButton_<i>[_<j>]` id family and a verb box rendered BESIDE each
   layer row (outside the row Border — a clickable inside the row's pressed
   Border would be swallowed by the row's tunnel-phase ToggleLayer handler); the
   verb composes a `SelectionContext<MaterialEntry>` in the view (the step-017
   `chooseFromLibrary` precedent): `KindConstraint CatalogueKind.Sample`,
   `SampleLayerTarget position`, `onSelected` → `BindMaterialToLayer` +
   `RefreshMaterials`, `onCancelled` → `RefreshMaterials`, and reaches IO
   through a new context field. `SampleEditorContext` gains
   `materials : MaterialProxy` (the re-query seam) and
   `openMaterialsSelect : Window -> SelectionContext<MaterialEntry> -> unit`
   (the composition root bakes the launcher; tests substitute stubs). New
   `RefreshMaterials` msg re-queries `listMaterials` through the context. The
   successful `BindMaterialToLayer` also records the picked id as
   `chosenMaterial` — the toolbar bulk verbs' (Select by material / Set
   material / Set from chosen / QWOT n) one remaining source now that the
   inline picker is gone; the vanished-row path stays a strict no-op + status.

2. **`SampleEditorWindow.fs`** — ctor gains `categories : CategoryProxy` (needed
   to build the Materials window) and optional `?selectWindowModality`; switches
   to `mkProgram` + `Cmd.ofEffect` dispatch capture (the MaterialsWindow
   precedent); hooks `this.Activated` → `RefreshMaterials`; composes
   `openMaterialsSelect` over the step-008 `WindowLauncher` under
   `MaterialsWindowKey` with `SelectOpen (owner, retarget)` — a live window is
   re-targeted through `MaterialsWindow.Retarget` (step 016).

3. **`LibraryWindow.fs`** — ctor gains `categories : CategoryProxy`, threaded
   into `SampleEditorWindow`.

4. **`TableAndElementRotationView.fs`** — `EditorLaunchers.openLibraryWindow` /
   `openLibrarySelectWindow` gain the `CategoryProxy` parameter (the workbench
   Model already holds `categories`); dispatch sites updated.

5. **Tests** — `SampleEditorWindowTests`: proxies triple, context stubs gain the
   two fields; picker tests rewritten onto the new flow; new pure tests
   (RefreshMaterials re-query; bind-sets-chosen; vanished-row leaves chosen
   alone) and new ui-smoke acceptances (Choose material… opens the Materials
   window in Select state and the return binds THAT row + converges on the
   chosen material; deleted-row no-op; activation re-query shows a material
   saved through a REAL Material editor window; Select-return re-query shows a
   material added while the editor was open). `MaterialsWindowTests`
   (`mountEditorWithDispatch` context), `WindowLauncherTests`,
   `LibraryWindowTests`, `MainWorkbenchTests` updated for the signature changes.

## Files to modify

- Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorView.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui/SampleEditorWindow.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindow.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WindowLauncherTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs
- Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MainWorkbenchTests.fs

## Risks

- Headless `window.Activate()` must raise `Window.Activated` for the
  activation-re-query acceptance — verify empirically; fall back to an
  equivalent activation trigger if the headless platform does not raise it.
- The `categories` threading touches `EditorLaunchers`' record shape — every
  out-of-tree stub must be updated (MainWorkbenchTests holds the only one).
- Removing the picker retires several existing tests — rewrite them onto the
  new flow so no gate count regresses (checkpoint baselines: 571 / 119 / 149 /
  413).
