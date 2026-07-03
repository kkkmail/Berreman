# 0027-007 — Rotation-test tweaks (implementation log)

Implements `006-tweak-retation-tests.txt`: three changes to the diagnostic rotation test windows.
Build is green and all tests pass — `OpticalConstructor.Ui.Tests` **154/154**, `OpticalConstructor.Tests`
245/245 (unchanged — no Domain source was touched).

## #1 — Test Optical Table Rotations: the table is rotatable ONLY when selected

`TableRotationView`: rotation is now gated on selection. A new `rotateIfSelected` guard wraps the
R1/R2/R3 button handlers and the `RotateView*` wheel actions, so a rotation request is dropped
unless `selection = TableSelected`. Pan, zoom and reset are not rotations and stay available
regardless. The hint line now says rotation needs the table selected.

Tests: a new `the table is rotatable ONLY when it is selected` (button + wheel inert when
unselected, working when selected); the existing rotation tests (pure + the real-injection ones)
now select the table first (a `selectedModel` helper / a `selectTable` headless click).

## #2 — Test Optical Element Rotations: add table zoom + move (as in #1)

`ElementRotationView`: the table view is no longer fixed. The model carries a live
`view : TableViewState` (panned / zoomed, never rotated — table rotation is #3's job), and the
elements are projected through it, so panning / zooming moves / scales them with the table.

- A plain drag now **pans** the table (`Panning` drag state, like the table test); a clean click
  still selects the nearest element.
- A plain or `Ctrl` wheel now **zooms the table** (new `ZoomTable` wheel action), bounded [0.2, 5].
  The element-rotation and per-element-zoom gestures are unchanged.
- **Reset** now resets the table view too (top-down / no pan / default zoom) alongside the selected
  element; the button is relabelled "Reset".

Tests updated/added: the wheel map now expects `ZoomTable` for plain/`Ctrl`; `plain wheel zooms the
table view, not the elements`; the click-vs-drag test asserts the pan delta; the reset test asserts
the view is reset too.

## #3 — New test: Test Table + Element Rotations (test #1 and #2 together)

A fourth launcher button opens a new window (`TableAndElementRotationView` /
`TableAndElementRotationWindow`). The table view is fully live (rotate when selected, pan, zoom) and
the three optical elements sit on it. The rotation gestures act on **whatever is selected**:

- Click empty table → **TableSelected**; the R1/R2/R3 buttons and `Shift`/`Ctrl+Shift`/`Alt`+wheel
  rotate the **view**.
- Click an element → **ElementSelected i**; the same gestures rotate that **element** (lock-respecting;
  R3 still starts locked, `Unlock/Lock R3` toggles it; it is inert while the table is selected).
- `Ctrl+Alt`+wheel zooms the selected element (nothing while the table is selected);
  `Ctrl+Alt+Shift`+wheel zooms all; a plain/`Ctrl` wheel zooms the table; a drag pans it; `Reset`
  restores the whole scene.

The load-bearing behaviour you called out: **rotating the table does not change any element's own
rotation angles — only how they project.** This falls out of the design — each element's box is
computed in table-frame from `Placement.orientedBasis` (its own R1/R2/R3) and then projected through
the rotatable table view, so a table rotation re-projects the elements ("snapped to the table")
while their stored angles are untouched. A dedicated test asserts exactly this: after a 90° table
rotation, every element's `r1/r2/r3` is unchanged but element 0's projected centre has moved.

Tests: 15 (pure MVU + real headless pointer injection) — selection routing of rotation, the
snapped-to-table invariant, the R3 lock, element-vs-all zoom, table pan/zoom, click selection of
table vs element, reset, and the real-input paths.

## Files

- New: `OpticalConstructor.TestWindows/{TableAndElementRotationView.fs, TableAndElementRotationWindow.fs}`;
  `OpticalConstructor.Ui.Tests/TableAndElementRotationTests.fs`.
- Changed: `TableRotationView.fs` (#1), `ElementRotationView.fs` (#2), `OpticalConstructor.App/Program.fs`
  (4th launcher button), the two `.fsproj`s, and the table/element test files.
