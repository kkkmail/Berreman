# Step 003 — impl-plan

## Goal

Make the selected faceted-tree node **visibly distinct**: the row whose `code`
equals the host's selected node code paints with the existing `chosenBackground`
plus a **non-hue** cue (thicker border) so it reads colourblind-safe. Every other
row stays idle. Both host windows feed the selection down from the Model's
already-tracked selected id.

## Approach

Follow the step-002 split exactly: **the control renders, the host owns the
state.** The control gains one pure `selectedCode : string` field (empty = none,
the `activeRepresentation` convention); each host projects it from its Model.

### `OpticalConstructor.Controls/FacetedTreeControls.fs`
- Add `selectedCode : string` to `State` (empty string = none) and initialise it
  in `empty`.
- Add `idleBorderThickness` / `selectedBorderThickness` constants beside the
  colour helpers (the non-hue cue).
- Thread `selectedCode` into `nodeRows`; the label Border paints
  `chosenBackground` + `selectedBorderThickness` when `node.code = selectedCode`,
  else `idleBackground` + `idleBorderThickness`. Re-subscribe the pointer handler
  on `(code, isSelected)` (the `clickBox` discipline).
- `treeArea` passes `state.selectedCode` into `nodeRows`.

### `OpticalConstructor.Ui/MaterialsWindowView.fs`
- `facetedState` sets `selectedCode` from `m.selectedId`
  (`Some id -> entryNodeCode id`, else `""`).

### `OpticalConstructor.Ui/LibraryWindowView.fs`
- `facetedState` sets `selectedCode` from `m.selectedEntryId`
  (`Some id -> entryNodeCode id`, else `""`).

### Tests (`OpticalConstructor.Ui.Tests`)
- `FacetedTreeControlsTests`: add `selectedCode = ""` to `knownState`; assert
  `empty.selectedCode = ""`; new headless proof — mount with one node selected and
  verify only that row renders `chosenBackground` + thicker border while another
  stays idle.
- `MaterialsWindowTests` / `LibraryWindowTests`: pure proof that `facetedState`
  projects `selectedCode` from the Model's selected id (none → ""); headless proof
  that clicking an entry leaf renders exactly that row highlighted (chosen + thick)
  while a sibling leaf stays idle.

## Files to modify
- `OpticalConstructor.Controls/FacetedTreeControls.fs`
- `OpticalConstructor.Ui/MaterialsWindowView.fs`
- `OpticalConstructor.Ui/LibraryWindowView.fs`
- `OpticalConstructor.Ui.Tests/FacetedTreeControlsTests.fs`
- `OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`
- `OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`

## Risks
- Adding a `State` record field breaks every FULL construction — there are exactly
  four (`empty`, `knownState`, both windows' `facetedState`); all updated. Copy-and-
  update sites (`{ knownState with … }`) are unaffected.
- The non-hue cue (border thickness) must not change label TEXT (the `textOf`
  helpers read it) — a thicker border leaves text untouched, unlike a leading glyph.
- Highlight patching on a reused, keyed row: proven by the `RotationControls`
  accent test (same keyed-Border attribute-diff pattern).
