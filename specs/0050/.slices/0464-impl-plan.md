# Step 0464 — impl plan (WIRE_UI, S132-AGF-WIRE)

The Materials window gains the `Import glass catalogue…` verb (spec 0050 S13.2). A dropped `.agf` file already decides
through `DropMaterialEntryFiles` (step 110) and `decideDroppedFiles` (step 463); this step adds the verb, its message and
the window's semantic seam, so the button and the tests send the same message (001 §P).

## Files

- `OpticalConstructor.Controls/UiIds.fs`: `materialsImportGlassCatalogueButton` in the Materials window group.
- `OpticalConstructor.Ui/MaterialsWindowView.fs`:
  - `ImportGlassCatalogue of FileName`;
  - one private helper that both `DropMaterialEntryFiles` and `ImportGlassCatalogue` call;
  - the public `pickGlassCatalogueFile`, the open dialog, in `pickEntryExportFile`'s shape;
  - the Browse-only verb after `Online sources…`.
- `OpticalConstructor.Ui/MaterialsWindow.fs`: `member _.ImportGlassCatalogue`, and the class doc.
- `OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`: the three ui-tests facts and the ui-smoke acceptance fact.

## Risks

- The glasses reference the built-in Glass category. A fresh store must hold the Built-in catalogue before a decide,
  as the step-110 drop facts arrange; otherwise the decide refuses with an unresolved reference.
- The headless acceptance drives `window.ImportGlassCatalogue`, not the file picker. The picker is an IO edge that
  the headless platform does not answer.
