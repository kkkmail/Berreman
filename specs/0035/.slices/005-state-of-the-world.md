# State of the world — slice 005 (ADD_COMPONENT — UICOMP_XDUO_0005 CategoryControls)

## Where we are

Slice 005 is the fifth step of arc 0035 (Part A — the open, editable, Guid-keyed material-category
set). Step 1 turned the category into DATA (a seeded, `CategoryId`-keyed catalogue); step 2 DECLARED
the mutating write-seam (`CategoryProxy` + `CategoryError`); step 3 IMPLEMENTED the in-memory store;
step 4 added the pure, Avalonia-free `CategoryEditor` Domain edit model (inline add / rename / remove
+ the `commit` diff projection). This slice ADDS the domain-free `OpticalConstructor.Controls`
component — `CategoryControls` — that the Category-manager surface projects that editor onto, plus its
headless structure test. It is Controls + Ui.Tests only; the component is authored and registered but
NOT wired into a parent view (that is the later WIRE_UI step).

## What's working

- Add the domain-free `OpticalConstructor.Controls.CategoryControls` FuncUI component, a shape copy of the `MaterialsControls` / `LibraryControls` precedents (pure `Row` + `State` records, a `Handlers` record of unit-returning functions, one `[<RequireQualifiedAccess>] module UiIds` of `[<Literal>]` intent-named ids).
- Render an editable category list with an inline name box per row and Add / Rename(save) / Remove verbs; a built-in row keeps its editable name but OMITS Remove (removed, not greyed).
- Surface the store's typed refusals through an inline block-message slot (`CategoryBlockMessage`) that renders only while a message is present.
- Add the headless `CategoryControlsTests` structure proof: mount `view` over a known State, find every UiIds control, confirm the built-in row's Remove is absent, and observe a simulated Add click invoking the add handler.
- Register `CategoryControls.fs` in `OpticalConstructor.Controls.fsproj` and `CategoryControlsTests.fs` in `OpticalConstructor.Ui.Tests.fsproj`.

## Tests

Per the ADD_COMPONENT family Invariant 6 (**act only; run no checks**), this worker authored the
component + its headless test and registered them, and did NOT run the gates locally — the
arc-runner's deterministic gate engine runs the step's roster (`build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`) after the worker exits and is the sole authority.

- `build`: the component is a line-for-line shape copy of the known-good, compiling `MaterialsControls`
  precedent (same imports, the `AttrBuilder<_>.CreateProperty` AutomationId idiom, the
  `Border.onPointerPressed` + `SubPatchOptions.OnChangeOf` click seam); every FuncUI attr used has a
  direct in-repo precedent. Both new files are LF (no CRLF churn); the component imports no domain type.
- `ui-smoke` (`ui_smoke_tests`): +1 test — the `[<Trait("Category","ui-smoke")>]` structure proof.
- `ui-tests` (`ui_tests`): +2 tests — the two pure contract tests (empty state; the stable UiIds).
- `unit-tests` (`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`): untouched
  by this slice (the two new files live in Controls + Ui.Tests; BerremanTests and OpticalConstructor.Tests
  are neither referenced nor changed). This slice only ADDS tests, so no `count_at_least` baseline can
  regress.

## Architecture

- **A domain-free control, the `MaterialsControls` / `LibraryControls` precedent.** Controls carries no
  `OpticalConstructor.Domain` type; the host flattens each `MaterialCategory` (a slice-004
  `CategoryEditor` working row) into a `Row` (`categoryId` / `name` / `isBuiltIn`) and injects the
  behaviour as a `Handlers` function record (the functional-proxy seam a test substitutes stubs for).
  The control renders the rows it is given and never touches `CategoryProxy`.
- **Per-row ids derive from the literal base.** The four verb/box literals (`CategoryNameBox`,
  `RemoveCategoryButton`, `CategorySaveButton`, `CategoryCancelButton`) are `[<Literal>]` base intent
  names; the `row*` helpers prefix each with the row's category id (the `MaterialsControls.row` /
  `LibraryControls.entry` collision-safe precedent), because the name box and verbs are per-row.
  `CategoriesList` / `AddCategoryButton` / `CategoryBlockMessage` are true singletons.
- **AutomationId (not Name) on the per-row controls.** Row membership changes when the host adds or
  removes a category, so a reused styled control can shift onto a different row's slot; Avalonia forbids
  re-setting `Name` on a reused control, so the per-row name box + verb buttons carry
  `AutomationProperties.AutomationId` (mutable, reuse-safe) — the reason `MaterialsControls` gives.
- **Removed, not greyed.** A built-in row omits the Remove verb from its child list entirely (the
  `MaterialsControls` view-only-Edit precedent) rather than disabling it, so the intent reads from the
  tree structure; the test asserts the built-in row's `rowRemoveButton` id is absent.

## Deferred

- Wiring `CategoryControls` into the Category-manager window / the Materials-bay `Categories…` verb and
  mapping its `Handlers` onto the `CategoryEditor` messages / `commit` and `CategoryProxy` dispatch — a
  later WIRE_UI step (spec 0035 A.2), not this ADD_COMPONENT slice.
- The inline name box fires `setCategoryName` via `TextBox.onTextChanged`; the structure test asserts
  the box is present and drives the click verbs (Add / Save / Remove / Cancel), but does not simulate
  keystroke text entry (the `MaterialsControls` search-box precedent) — the host's MVU wiring exercises
  the name-change path in the WIRE_UI step.

## Gotchas

- **The one ambiguity — per-row controls vs the seven fixed ids.** The slice lists the four verb/box
  ids as fixed literals but also mandates "an inline name box per row" and per-row verbs; a per-row
  control cannot carry a single fixed id. Resolved (the interpretation most consistent with the
  surrounding code) by making those four `[<Literal>]` base names that the `row*` helpers prefix per
  row. The structure test "finds every UiIds control" by asserting the three singletons directly and
  the four per-row families for each known row.
- **The block-message slot renders only while a message is present** (the `LibraryControls.confirmPanel`
  precedent): `blockMessage = ""` means the `CategoryBlockMessage` control is absent, not an empty box.
  The known test State seeds a non-empty message so the slot is found.
- **`matchesId` matches by `Name` OR `AutomationId`.** The singletons use `Name` (`StackPanel.name` /
  `TextBlock.name`); the per-row controls use `AutomationId`. The test helper checks both.

## Changelog

- 2026-07-07 — Added the domain-free `OpticalConstructor.Controls.CategoryControls` component (`Row` /
  `State` / `empty` / `Handlers` / `UiIds` + `row*` derive helpers / `view`) — an editable category
  list with an inline name box per row, Add / Rename(save) / Remove verbs, a built-in row whose Remove
  is removed (not greyed), and an inline `CategoryBlockMessage` slot — mirroring the `MaterialsControls`
  shape and registered in `OpticalConstructor.Controls.fsproj` after `SampleLibraryControls.fs`. Added
  the headless `CategoryControlsTests` (a pure contract test for the empty state + the stable UiIds and
  a `[<Trait("Category","ui-smoke")>]` structure proof that every UiIds control mounts, the built-in
  row's Remove is absent, and a simulated Add click invokes the add handler), registered in
  `OpticalConstructor.Ui.Tests.fsproj` after `WireUiCompositionTests.fs`.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 440
  ui_smoke_tests: 84
  ui_tests: 312
```
