# Step 0012 — IMPLEMENT — impl plan

## Approach

The domain-free materials control loses its view-only Edit gating, and nothing replaces it
(.spec-md §3.2, obligation `S12-CONTROLS-EDIT`). No product view mounts `materialsControlsView`;
only `MaterialsControlsTests` does. The Materials window keeps its own gate,
`MaterialsWindowModel.editableSelection`, which this step does not touch.

1. `OpticalConstructor.Controls/MaterialsControls.fs`:
   - delete `MaterialEditability` and `MaterialsControlsRow.editability`;
   - delete the module doc's view-only sentences;
   - `verbRow` always lists Add / Edit / Remove / View, and Edit is offered on a selection exactly
     as Remove and View are;
   - delete `verbRow`'s doc clause on Edit;
   - `verbButton` drops `withKey` and its comment, because the verb row's membership no longer
     changes.
2. `OpticalConstructor.Ui.Tests/MaterialsControlsTests.fs`:
   - `knownRows` lose `editability`, and their doc loses the view-only row;
   - `matchesId`'s doc loses the Edit-removal example;
   - the view-only fact becomes `every selected row offers the Edit verb and a click dispatches
     editMaterial`. With the silicon row selected, the Edit verb is present, and a click records
     `"edit"`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/MaterialsControls.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsControlsTests.fs`

## Risks

- **The `View` module may become unused.** Once `verbButton` drops `withKey`, `clickBox` is the
  only user of `Avalonia.FuncUI.DSL.View.withKey`. `clickBox` is fully qualified and stays, so
  the opens are unchanged.
- **ELEV.** The step deletes a union and a record field and adds no public names, so it cannot
  add a finding. Controls' dependents (Ui, App, TestWindows) are re-analysed, and they never
  referenced the deleted names.
- **Test counts.** ui-smoke and the MaterialsControlsTests filter keep the same counts: one fact
  is migrated in place.
