# Step 0452 — impl plan (WIRE_UI, S131-MATERIALS-WIRE)

The Materials window gains an `Online sources…` verb that opens step 451's single-instance `MaterialSourcesWindow`
over the window's own import seam; a pull's or an update's caption comes back as `MaterialsImportApplied`, so the
window's import status shows it and its next render re-queries the store (spec 0050 S13.1; .spec-md §10.1, §10.4).

## Files

1. `OpticalConstructor.Controls/UiIds.fs` — `materialsOnlineSourcesButton` (`"MaterialsOnlineSourcesButton"`) in the
   Materials window group under a one-line comment citing spec 0050 S13.1.
2. `OpticalConstructor.Ui/MaterialsWindowView.fs`:
   - `MaterialsWindowContext.openOnlineSources : unit -> unit` (one-line doc);
   - `MaterialsWindowMsg.OpenOnlineSources` (one-line doc);
   - `updateMaterialsWindow`: `OpenOnlineSources` calls `context.openOnlineSources ()` and answers the model unchanged;
   - `verbsRow`: in Browse mode, after the export verbs, while `catalogueImports.materialSources` is not empty, the
     `Online sources…` verb button dispatching `OpenOnlineSources`. Select mode never shows it.
3. `OpticalConstructor.Ui/MaterialsWindow.fs` — the context's `openOnlineSources` is
   `openBrowse WindowLauncher.MaterialSourcesWindowKey (fun () -> MaterialSourcesWindow(catalogueImports, fun summary ->
   dispatch (MaterialsWindowView.MaterialsImportApplied summary)) :> Window)`; the class doc names Online sources.
4. `OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs` — `stubContext` records `openOnlineSources`; three ui-tests
   facts and one ui-smoke acceptance fact, exactly the four identities the census names.

## Risks

- `openBrowse` builds a fresh `WindowLauncher` per call, so the "single instance" of the Online sources window must come
  from the launcher's registry being shared. Check how `WindowLauncher.create` keeps its registry (module-level or per
  instance) before relying on it; the smoke fact asserts a second press opens no second window.
- `MaterialSourcesWindowFixture` (step 451's double) lives in `ScrollExtentTests.fs`, which compiles AFTER
  `MaterialsWindowTests.fs`, and neither that file nor the project file is in this step's authorized edits. The
  Materials tests need their own private double over step 443's `InMemoryMaterialSource`, as each earlier source test
  module has.
- Every stub context (other test files building `MaterialsWindowContext`) must gain the new field; only
  `MaterialsWindowTests.fs` is authorized, so check for other constructors of the record first.
