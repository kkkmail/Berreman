# Step 003 — impl-log

## Progress

- Added a `selectedCode : string` field to `FacetedTreeControls.State` (empty = none,
  the `activeRepresentation` convention) and initialised it in `empty`.
- Painted the selected tree row visibly distinct in `nodeRows`: the label whose
  `code` equals `state.selectedCode` now renders `chosenBackground` (was
  `idleBackground`) PLUS a thicker border — a non-hue, colourblind-safe cue.
- Threaded `selectedCode` through `nodeRows` (new param) and its recursive call;
  `treeArea` passes `state.selectedCode`.
- Both hosts project `selectedCode` from their Model's already-tracked selected id:
  `MaterialsWindowView.facetedState` from `m.selectedId`,
  `LibraryWindowView.facetedState` from `m.selectedEntryId` (each via
  `entryNodeCode`, `None -> ""`). No handler signature changed — `selectNode`
  already existed.
- Tests: control highlight proof (headless) + `empty`/`knownState` field updates;
  per window a pure `selectedCode` projection proof and a headless highlight proof.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/FacetedTreeControls.fs`
  — `State.selectedCode`, `empty`, `idle/selectedBorderThickness`, `nodeRows`
  selected-row painting + threading, `treeArea` call.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs`
  — `facetedState` projects `selectedCode` from `m.selectedId`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/LibraryWindowView.fs`
  — `facetedState` projects `selectedCode` from `m.selectedEntryId`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/FacetedTreeControlsTests.fs`
  — `Avalonia.Media` open, Border-reading helpers, `knownState`/`empty` updates,
  highlight proof.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`
  — `Avalonia.Media` open, Border helpers, pure projection proof, headless
  highlight proof.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LibraryWindowTests.fs`
  — `Avalonia.Media` open, Border helpers, pure projection proof, headless
  highlight proof.

## Decisions

- **Non-hue cue = thicker border (1.0 → 3.0), not a leading marker glyph.** A glyph
  prepended to the label text would change the rendered TEXT, which the tests'
  `textOf`/`labelInside` helpers read (and the step-002 gotcha warns of) — a border
  thickness is achromatic, testable via `Border.BorderThickness.Top`, and leaves the
  label text untouched. The slice offered either; the border keeps every existing
  text assertion valid.
- **`selectedCode` lives on the control's `State`, projected by the host** — the
  same control-renders / host-owns-state split as `selectNode`/`toggleNode`. The
  control never remembers a selection; it highlights exactly the code the host
  gives it, so behaviour stays testable without a window.
- **Re-subscribe the row's pointer handler on `(code, isSelected)`** (was `code`),
  matching the `clickBox` discipline so a reused, restyled box can't keep a stale
  handler. The handler payload (`selectNode node.code`) is unchanged either way, so
  this is defensive, not behavioural.
- **Highlight verified via the real render, not just the model:** each window's
  headless proof narrows to a small leaf set (Materials: "glass"; Library:
  "polarizer"), clicks one leaf, and reads the rendered `Border.Background` /
  `BorderThickness` — exactly one row chosen+thick, a sibling idle+thin.

## Testing state

- IMPLEMENT Invariant 6 — the worker acts and runs no gates; gate execution is the
  arc-runner's deterministic engine after this session exits. Diagnostic
  verification only (NOT gate authority): `dotnet build Berreman.slnx -c Release`
  → 0 errors, 4 warnings, all pre-existing exempt (NU1701 Wolfram.NETLink,
  SYSLIB0051 vendored MathNet) — none from a touched file.
- `OpticalConstructor.Ui.Tests` → 656/656 (was 651; +5: 1 control highlight proof,
  and per window a pure projection proof + a headless highlight proof).
- `OpticalConstructor.Tests` → 675/675 (Domain untouched — no regression).
- `commit_ready: true` — every slice requirement is addressed this round.

## Artifacts

- None (no captured logs/dumps needed; the build/test output is inline above).
