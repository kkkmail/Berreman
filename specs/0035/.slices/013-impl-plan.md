# Step 013 — IMPLEMENT — impl-plan

## Goal

Make the Sample editor's material picker (`SampleEditorView.materialRow`,
`OpticalConstructor.TestWindows/SampleEditorView.fs`) **searchable** — a search box and a
catalogue-driven category facet (the step-1 seeded categories) that narrow the resolved
material list the window loads once from `MaterialProxy.listMaterials`. The by-`MaterialId`
selection contract (`ChooseMaterial of MaterialId`) is unchanged.

## Approach

The picker currently renders `m.materials` directly. I insert two filter surfaces above the
options wrap panel — mirroring the Materials bay facet (`OpticalConstructor.Ui/MaterialsView.fs`)
and the sibling `substrateRow` in this same file — and filter the options through the existing
Domain search seam (no new filter coded):

- **Model:** add `materialSearchText : string` and `materialCategory : CategoryId option`
  (`None` = the "All" facet). `init` seeds them empty / `None`.
- **Msg / update:** `SetMaterialSearchText of string` and `SelectMaterialCategory of CategoryId option`,
  each a one-line record update (a plain string / option field — NOT a large-state rebuild, so no
  FuncUI render-loop risk per the `funcui-numberbox-render-loop` memory; identical in shape to the
  working `SetName` / `nameBox`).
- **Filtering:** a pure `filteredMaterials (m : Model) : MaterialEntry list` that reuses
  `MaterialLibrary.byQuery` over `{ entries = m.materials }` with `dispersion = AnyDispersion`
  (only the search box + category facet are offered here). The picker maps over
  `filteredMaterials m` instead of `m.materials`.
- **Category facet:** `materialCategoryFacets` = `"All"` (None) then the seeded
  `standardCategories` filtered to `SelectableOnCreate`, in catalogue order (catalogue-driven, so
  a new seeded category flows through without a code edit) — exactly the Materials bay derivation.
- **UiIds:** `materialSearchBox = "SampleMaterialSearchBox"`,
  `materialCategoryFilter = "SampleMaterialCategoryFilter"`, and the derived
  `materialCategoryOption (code)` (code = `"All"` or the CategoryId's Guid string, via the public
  `materialCategoryCode`).

## Files to modify

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/SampleEditorView.fs`
  — UiIds, Model, Msg, update, init, the three pure helpers, and `materialRow`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`
  — a pure `update`-level test (`filteredMaterials` narrows by fragment and by category; a
  filtered option still dispatches `ChooseMaterial` with its `MaterialId`) and a headless
  `ui-smoke` acceptance test driving the real window by its UiIds.

## Risks

- **Render loop** on the search box — mitigated: simple string field, idempotent echo (mirrors
  the existing `nameBox`); the memory's danger case is a box that rebuilds a whole `DispersionModel`.
- **Category facet code stability** — the facet id keys on the CategoryId's Guid string (stable
  across runs, seeded literal Guids), not a list index.
- **Count baselines** — only new tests are added (no existing test removed), so ui-smoke / ui-tests
  counts strictly increase.
