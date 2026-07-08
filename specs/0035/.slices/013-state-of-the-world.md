# Step 013 — state of the world

## Where we are

Step 013 of spec 0035 makes the **Sample editor's material picker searchable**. Earlier slices in
this arc built the editable-category catalogue (step 1 seeded `standardCategories`), the category
write-seam/store, the materials search seam (`MaterialQuery` / `byQuery`), and the Materials bay
facet UI. This slice brings that same search-and-facet affordance to the sample editor's material
picker (`SampleEditorView.materialRow`, `OpticalConstructor.TestWindows`): a search box and a
catalogue-driven category facet now narrow the material list the window resolves once from
`MaterialProxy.listMaterials`, while selection stays by `MaterialId`. It touches
`OpticalConstructor.TestWindows` and `OpticalConstructor.Ui.Tests` only.

## What's working

- Add a search box and a catalogue-driven category facet to the Sample editor's material picker.
- Filter the once-resolved material list by a case-insensitive name fragment and by category,
  reusing the Domain `MaterialLibrary.byQuery` seam (no new filter coded).
- Derive the category facet from the step-1 seeded `SelectableOnCreate` categories plus "All",
  mirroring the Materials bay facet, so a new seeded category flows through without a code edit.
- Keep the by-`MaterialId` selection contract (`ChooseMaterial of MaterialId`) unchanged — a
  filtered option still chooses its material by id.
- Add stable intent-named UiIds (`SampleMaterialSearchBox`, `SampleMaterialCategoryFilter`, the
  derived `SampleMaterialCategory_<code>`).
- Add a pure contract test and a headless ui-smoke acceptance test for the filtering behaviour.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This session ran no gate commands.
- Step 013 roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`. The
  change is exercised by `ui-tests` (one new pure `SampleEditorWindowTests` fact) and `ui-smoke`
  (one new headless acceptance fact driving the real window). `build` covers the
  `SampleEditorView.fs` view changes. `unit-tests` and `constructor-unit-tests` projects are
  untouched.
- Deferred: none — the slice's whole surface (search box + category facet + filtering + both
  tests) landed this round.

## Architecture

- **Filter is view-only over the once-resolved list.** `filteredMaterials` narrows `m.materials`
  in the view via the pure Domain `byQuery`; it does not re-call `MaterialProxy` or
  `searchMaterials`. This honours the step's "filter the resolved material list the window loads
  once" wording and reuses the tested search seam instead of duplicating filter logic.
- **Catalogue-driven facet.** The category options are `"All"` plus
  `standardCategories |> filter SelectableOnCreate`, exactly the `OpticalConstructor.Ui/MaterialsView`
  derivation — a single mirroring so both surfaces stay consistent and extensible.
- **State stays in the Elmish model.** The search text and the selected category are two new
  `Model` fields with one-line `update` arms; the view is a pure projection, so the behaviour is
  testable at the `update`/`filteredMaterials` level without a window (the first test layer).
- **Elevated types preserved.** The new state is a `string` fragment and a `CategoryId option`
  (the "All" facet is `None`, not a magic string); the facet id keys on the CategoryId's Guid.

## Deferred

- Nothing deferred for later slices from this step.

## Gotchas

- **Two "Materials bay" precedents.** `MaterialsControls` (reusable, domain-free, host-run
  `searchMaterials`) vs the `MaterialsView` panel (inline catalogue-driven category buttons). This
  picker is a FuncUI-DSL window using its own `clickBox`/`substrateRow` idiom, so it mirrors the
  MaterialsView derivation. Detailed in the impl-log `Gotchas`.
- **No render loop.** The search box commits a plain string field on `onTextChanged` (idempotent
  echo, the `nameBox` shape), not a large-state rebuild — the `funcui-numberbox-render-loop`
  danger case does not apply. Committed on `onTextChanged` (not `onLostFocus`) so the headless
  `setText` helper can drive it and to match this window's other text boxes.
- **MaterialId proof in the headless test.** A layer's material name carries no UiId, so the test
  proves the filtered choice kept its `MaterialId` by adding a layer of it and asserting the
  anisotropic-only orientation editor (`UiIds.layerOrientation 0`) renders for the chosen crystal.

## Changelog

- 2026-07-08 — Step 013 (IMPLEMENT): made the Sample editor material picker searchable — added a
  search box and a catalogue-driven category facet (step-1 seeded `SelectableOnCreate` categories
  + "All") that filter the once-resolved material list via the Domain `byQuery` seam, keeping the
  by-`MaterialId` selection contract; added stable UiIds, a pure contract test, and a headless
  ui-smoke acceptance test.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 446
  ui_smoke_tests: 96
  ui_tests: 328
```
