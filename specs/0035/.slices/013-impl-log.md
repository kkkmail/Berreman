# Step 013 — IMPLEMENT — impl-log

## Progress

- [x] Add the picker Model fields (`materialSearchText`, `materialCategory`), the `Msg` cases
      (`SetMaterialSearchText`, `SelectMaterialCategory`), the `update` arms, and the `init` seeds.
- [x] Add the pure helpers (`filteredMaterials`, `materialCategoryCode`, `materialCategoryFacets`)
      and the UiIds (`materialSearchBox`, `materialCategoryFilter`, `materialCategoryOption`).
- [x] Rewrite `materialRow` to render the search box + catalogue-driven category facet + the
      filtered material options (extracted `materialSearchRow` / `materialCategoryRow` helpers).
- [x] Add the pure contract test and the headless `ui-smoke` acceptance test.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.TestWindows/SampleEditorView.fs`
  - **UiIds**: added `materialSearchBox = "SampleMaterialSearchBox"`,
    `materialCategoryFilter = "SampleMaterialCategoryFilter"`, and the derived
    `materialCategoryOption (code) = "SampleMaterialCategory_" + code`.
  - **Model**: added `materialSearchText : string` and `materialCategory : CategoryId option`
    (`None` = the "All" facet); `init` seeds them `""` / `None`.
  - **Msg / update**: added `SetMaterialSearchText of string` and
    `SelectMaterialCategory of CategoryId option`, each a one-line record update.
  - **Pure helpers**: `filteredMaterials (m : Model) : MaterialEntry list` reuses the Domain
    search seam `MaterialLibrary.byQuery` over `{ entries = m.materials }` with
    `dispersion = AnyDispersion` (no new filter coded); `materialCategoryCode` maps a
    `CategoryId option` to its stable code (`All` / Guid string); `materialCategoryFacets` is the
    "All" facet then the step-1 seeded `SelectableOnCreate` categories in catalogue order.
  - **View**: `materialRow` now stacks the label, `materialSearchRow` (a `TextBox` bound by
    `onTextChanged`), `materialCategoryRow` (a NAMED WrapPanel of AutomationId'd facet boxes), and
    the material-options WrapPanel — which now maps over `filteredMaterials m` instead of
    `m.materials`. The `ChooseMaterial of MaterialId` selection contract is untouched.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SampleEditorWindowTests.fs`
  - Pure contract test `the material picker narrows by a search fragment and by a category facet,
    still choosing by MaterialId` (ui-tests gate): asserts `filteredMaterials` narrows by fragment
    and by the Crystal facet, that a filtered option still dispatches `ChooseMaterial` with its
    `MaterialId`, and that the "All" facet restores the full list.
  - Headless `ui-smoke` acceptance test `acceptance: the material picker filters by a search
    fragment and a category facet and still selects by MaterialId`: drives the real window by its
    UiIds — types `"glass"` (crystal option disappears, glass survives), clears + clicks the
    Crystal facet (glass disappears, crystal survives), then chooses the filtered crystal option
    and Add-layer, proving the choice by the added layer's anisotropic orientation editor
    (`UiIds.layerOrientation 0`, present ONLY for anisotropic materials).

## Testing state

Per the IMPLEMENT worker contract (Invariant 6 — "the worker acts; it runs no checks"), gate
execution belongs to the arc-runner's deterministic gate engine, which runs the step's roster
(`build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`) after this session
exits. I did NOT run those gates. LF line endings verified read-only (`git diff` shows 0 CR
bytes; `--numstat --ignore-cr-at-eol` matches the change).

Reasoning-level confidence:
- **Compile**: `byQuery` / `MaterialQuery` / `AnyDispersion` / `standardCategories` /
  `SelectableOnCreate` / `CategoryId` all come from the already-`open`ed
  `OpticalConstructor.Domain.MaterialLibrary`. The `{ text; category; dispersion }` literal
  resolves to `MaterialQuery` (only record carrying all three labels) and is also driven by
  `byQuery`'s parameter type. `init` sets every record field including the two new ones. Every
  other Model construction is a `{ m with … }` copy that carries the new fields automatically.
- **View helpers** (`materialSearchRow`, `materialCategoryRow`) reuse the existing `labelBlock` /
  `clickBox` / `TextBox` idioms verbatim; `TextBox.onTextChanged (SetMaterialSearchText >> dispatch)`
  mirrors the working `SetName >> dispatch` on `nameBox`.
- **No render loop**: the search box commits a plain string field (idempotent echo), not a
  large-state rebuild — the `funcui-numberbox-render-loop` danger case does not apply (see
  Gotchas).
- **Counts**: only new tests are ADDED (no test removed) → `ui-smoke` 95→96, `ui-tests` 327→328;
  `berreman_unit_tests` (119) and `constructor_unit_tests` (446) untouched (their projects are
  not modified). All `count_at_least` gates strictly clear their prior baseline.

Expected baselines this round: `berreman_unit_tests: 119`, `constructor_unit_tests: 446`,
`ui_smoke_tests: 96`, `ui_tests: 328`.

## Artifacts

None — a UI-view slice with no captured logs/traces/dumps. (Artifacts folder for this arc if
needed: `C:\GitHub\Berreman\specs\0035\.artifacts`.)

## Gotchas

- **Which "Materials bay" to mirror.** Two precedents exist: the reusable domain-free
  `MaterialsControls` (search box + category/dispersion `FacetOption` facets, host runs
  `searchMaterials`) and the `OpticalConstructor.Ui/MaterialsView` panel (inline category buttons
  built from `standardCategories |> filter SelectableOnCreate`, filtering via
  `byCategory`/`byNameContains`). This picker lives in `OpticalConstructor.TestWindows` and drives
  the FuncUI DSL directly with the same `clickBox` idiom as its own `substrateRow`, so I mirrored
  the **MaterialsView** derivation (catalogue-driven `SelectableOnCreate` facets + "All"). The
  `Vacuum` category is `HiddenOnCreate`, so it gets no facet button — but vacuum entries still
  appear under "All", exactly as in the Materials bay.
- **Filter is view-only over the once-resolved list.** The step is explicit that the search box
  and facet "filter the resolved material list the window loads once from
  `MaterialProxy.listMaterials`" — so `filteredMaterials` narrows `m.materials` in the view and
  does NOT re-call the proxy / `searchMaterials`. It reuses the pure Domain `byQuery` for the
  actual filtering so no filter logic is duplicated. `m.materials` itself is never mutated by the
  filter, so verbs that read the full list (`AddLayer`'s `not (List.isEmpty m.materials)` enable
  guard, the toolbar) are unaffected.
- **No FuncUI render-loop from the search box.** Per `funcui-numberbox-render-loop`, a box that
  commits on `onTextChanged` only loops when the commit rebuilds a large chunk of model state (a
  whole `DispersionModel`). `SetMaterialSearchText` sets a plain string field (like the working
  `nameBox`/`SetName`); FuncUI skips re-setting the unchanged echoed text, so no loop. Committed
  on `onTextChanged` (not `onLostFocus`) to match every other text box in this window and to let
  the headless `setText` helper drive it.
- **MaterialId-level proof in the headless test.** There is no UiId on a layer row's material-name
  text, so I prove the chosen (filtered) material carried its `MaterialId` through by adding a
  layer of it and asserting the row's per-layer orientation editor (`UiIds.layerOrientation 0`)
  renders — that editor exists ONLY for anisotropic materials, and the chosen `uniaxialCrystal`
  is anisotropic while the filtered-out glasses are not.
- **Facet id keys on the Guid, not a list index.** `materialCategoryOption` keys on the
  CategoryId's Guid string (deterministic seeded literals) so the id is stable across runs and
  restyling, per the CLAUDE.md "stable IDs that describe intent, not layout" rule.
- Line endings kept as LF (verified `git diff` has 0 CR bytes).
