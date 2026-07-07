# Impl plan — spec 0033, slice 015 (ADD_COMPONENT UICOMP_XDUO_0001 MaterialsControls)

## Goal

Declare the domain-free `MaterialsControls` component in
`OpticalConstructor.Controls` (the materials-workbench list surface): a
searchable materials list with a search box, category and dispersion facet
selectors, and Add / Edit / Remove / View verbs — plus a headless structure
test in `OpticalConstructor.Ui.Tests`. This is a Contract-family
`ADD_COMPONENT` step: declare the surface (ids + state→view projection);
do NOT wire it into a parent view (that is a later `WIRE_UI` step).

## Approach

Copy the `LibraryControls` shape (pure `Row` / `State` records, a
`Handlers` record of unit-returning functions, one
`[<RequireQualifiedAccess>] module UiIds`), with the eight slice-mandated
ids as `[<Literal>]`s: `MaterialSearchBox`, `MaterialCategoryFilter`,
`MaterialDispersionFilter`, `MaterialsList`, `AddMaterialButton`,
`EditMaterialButton`, `RemoveMaterialButton`, `ViewMaterialButton`.

- **Domain-free**: rows and facet options are host-flattened strings
  (`Row = { materialId; label; editability }`,
  `FacetOption = { code; label }`); the control never touches
  `MaterialProxy`. The state mirrors the domain `MaterialQuery` seam
  (text + category option + dispersion facet) without importing it.
- **Editability is a two-case DU** (`Editable | ViewOnly`), not a naked
  bool — CLAUDE.md's elevate-every-primitive rule; "view-only" = a
  material with no edit model (spec 0033 step 013: silicon / langasite /
  vacuum have `complexity = None`).
- **Edit affordance removed, not greyed**: the verbs operate on the
  selected row; when the selected row is `ViewOnly` the Edit button is
  genuinely absent from the child list (not `isEnabled false`, not
  `isVisible false`). To make true removal safe under FuncUI's diffing,
  the verb buttons (and the dynamic row / facet-option boxes) carry
  `AutomationProperties.AutomationId` (freely mutable) instead of
  `Control.Name` — the `ExperimentControls.idOptionBox` precedent for
  lists whose membership changes. Fixed structural controls (the search
  TextBox, the two facet WrapPanels, the rows StackPanel) keep `Name`.
- Button look copies the sibling bars' idle/chosen palette so the bays
  MATCH (232/232/232 idle, 150/185/235 chosen, 120/120/120 border).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/MaterialsControls.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/OpticalConstructor.Controls.fsproj` (add Compile)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsControlsTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (add Compile)
- Contract registry `specs/0033/.contracts-json` already records
  `UICOMP_XDUO_0001` as declared / declaringStep 15 (supervisor-maintained).

## Tests (TDD: red first)

1. Write `MaterialsControlsTests.fs` first; the red build fails with
   FS0039 naming the missing `MaterialsControls` (counts as red per the
   worker protocol).
2. Pure contract tests (`ui-tests` gate): empty-state facts, the eight
   literal ids + the row/facet-option prefix helpers, `selectedRow`.
3. Headless structure test (`ui-smoke` trait): mount `view` over a known
   State (three rows, one view-only, an editable row selected) with
   recording stub handlers; assert every UiIds control exists (Name OR
   AutomationId, the `matchesId` idiom); simulated clicks dispatch the
   MATCHING handler (Add first — the acceptance click — then Edit /
   Remove / View / a row). A second mount with the view-only row selected
   asserts `EditMaterialButton` is absent from the tree while
   Add / Remove / View remain.

## Risks

- FuncUI styled-`Name` recycling throw when the Edit button appears or
  disappears → mitigated by AutomationId on every control in a
  variable-membership child list (see above).
- `TextBox.onTextChanged` fires for programmatic sets too — fine for a
  search box (the `MaterialsView.fs:169` idiom); the test only asserts
  verb dispatches.
- Gate counts: new tests only ADD to the `count_at_least` baselines
  (ui-smoke 54, ui-tests 249); no existing test is touched.
