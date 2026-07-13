# Step 002 — impl-log

## Progress

- [x] `UiIds.FacetedTree.treeNodeChevron` added.
- [x] `FacetedTreeControls`: `toggleNode` on `Handlers`; per-parent chevron in `nodeRows`.
- [x] `MaterialsWindowView`: `ExpandedNodes`, model field, `ToggleNode` Msg + update, projected
      expansion, `toggleNode` handler.
- [x] `LibraryWindowView`: same.
- [x] Tests updated (recorder, mount-helper auto-expand, inline/launcher expands, new acceptance
      tests in both windows and the control).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/FacetedTreeControls.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FacetedTreeControlsTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`

## Testing state

Gate execution belongs to the arc-runner's deterministic gate engine (IMPLEMENT Invariant 6 —
the worker acts, it runs no checks). This session ran no gate commands as gates. Diagnostic
verification only (NOT gate authority):

- `dotnet build Berreman.slnx -c Release` — 0 errors. The 4 warnings are the pre-existing exempt
  ones (NU1701 Wolfram.NETLink ×2, SYSLIB0051 vendored MathNet ×2), none from touched files.
- `OpticalConstructor.Ui.Tests` — 651/651 passed across both categories (was 646 baseline;
  +5 new tests, none removed). Includes the new collapse/chevron proofs in the control and both
  windows.
- `OpticalConstructor.Tests` (constructor-unit-tests) — 675/675 passed (unchanged; Domain untouched).
- All 12 edited source files verified LF (no CRLF churn).

Net Ui-test delta: +5 new tests (1 control chevron proof, 2 per window: pure collapse-toggle +
headless collapse acceptance). No `count_at_least` gate can regress.

## Artifacts

- None captured — the diagnostic build/test output was inspected inline, not persisted.

## Gotchas

- **Collapsed-by-default hides the browsed entry leaves.** The `entries` group and every facet
  group now open `CollapsedNode`, so the entry leaves and facet branches do NOT render until their
  parent's chevron is clicked. This broke ~25 headless proofs across SEVEN test files (not just the
  two window suites): `MaterialsWindowTests`, `LibraryWindowTests`, `EmbeddedChartTests`,
  `SampleEditorWindowTests`, `WireUiCompositionTests`, `WindowLauncherTests`, `AppContextTests`.
  A single leaf-click failure LEFT A SINGLE-INSTANCE WINDOW OPEN/REGISTERED, which cascaded into
  the strip/launcher/wire single-instance tests ("Expected 1 Actual 0"). Fixed by expanding the
  entries group before every leaf interaction (auto-expanded in the two window suites' shared mount
  helpers; explicit chevron clicks in the launcher-opened windows of the other five files).
- **Expansion PERSISTS in the Model** (`ExpandedNodes`), so one expand after mount survives every
  later re-render — expand once, not per interaction.
- **The facet-group chevron can scroll out of view.** `LibraryWindowTests`' by-kind branch proof
  clicks a FACET chevron; with `entries` expanded (17 leaves) the facet row scrolls below the
  viewport and the click misses (clickOn finds it but the pointer lands off-screen). Fixed by
  collapsing `entries` first (facets rise to the top) before expanding the kind facet — leaf
  presence is checked earlier and needs no viewport.
- **`textOf(treeNode code)` still reads the label**, not the chevron: the chevron is a SIBLING of
  the label box inside the row `StackPanel`, never its descendant.
- **Chevron id prefix is distinct** (`FacetTreeChevron_`, not `FacetTreeNode_`) so the
  `treeRowCount` helpers that count `FacetTreeNode_`-prefixed label rows are unaffected.
- **Adding `toggleNode` to `Handlers`** forced updates to all three `Handlers` constructions (the
  two windows' `facetedHandlers` and the control test's `recorder` stub).
