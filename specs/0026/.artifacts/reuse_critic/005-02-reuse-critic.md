# Reuse critique -- 005.slice-md cycle 1

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (focused on `Berreman/OpticalConstructor/OpticalConstructor.Ui`, `.Ui.Tests`, `.Domain`).
- Files inspected: ~22/200 (the slice diff + new files, the slice-001/002/004 helper modules they build on, and the sibling Ui render/test modules).
- Extensions: the templated bound `.py,.md,.json` does not fit this pure-F# repo (no `.py`; helpers live in `.fs`). Most-defensible read taken: walked the `.fs` helper modules plus the `.json` schema and `.md` inputs.

## Findings

### F1: A third copy of the `SchematicColor -> IBrush` converter

- **Worker added:** `toBrush` and `toBrushA` in `ConstructorView.fs:597-601`
  (`SolidColorBrush(Color.FromRgb …) :> IBrush`).
- **Existing helper:** the same converter already exists twice —
  `Controls.fs:37` `toBrush (c : SchematicColor) : IBrush` and
  `ResultsView.fs:83` `brushOf (c : Schematic.SchematicColor) : IBrush`.
- **Why it matters:** this is the third near-identical copy of "map a pure
  `SchematicColor` to a solid Avalonia brush". Worse, `Controls.fs:31-38`
  deliberately uses `ImmutableSolidColorBrush` and documents *why* (the brush
  is built off the UI/headless thread and a mutable `SolidColorBrush` "would
  throw Call from invalid thread there"). The new copy uses the mutable
  `SolidColorBrush`, matching `ResultsView` but diverging from the
  thread-safety rationale `Controls` records. The new copy happens to be
  evaluated inside `view`/render closures (on-thread), so it is not a bug
  today, but it re-opens a decision the project already made and spreads a
  fourth maintenance point for the same one-liner. The `Color.FromArgb`
  opacity variant (`toBrushA`) is genuinely net-new (neither existing copy
  handles alpha) and is not duplicated.
- **Suggested action:** extract one shared `SchematicColor -> IBrush`
  converter (the rubric's "extract a shared helper" option) — the natural home
  is next to `SchematicColor`/`toHex` in `Schematic.fs:35`, or a small shared
  Ui helper — using `ImmutableSolidColorBrush` per the `Controls.fs` precedent,
  with `toBrushA` as the alpha overload. Point `Controls.toBrush`,
  `ResultsView.brushOf`, and the new call sites at it. Advisory; the judge
  decides whether centralising now is worth the cross-module touch.

### F2: Element rendering re-derived ad hoc, bypassing the slice-004 `Drawer.draw`

- **Worker added:** `elementView` (`ConstructorView.fs:665-678`) draws each
  element as a plain `Ellipse` whose radius it recomputes inline as
  `(p.box.a1 / 2.0) … * s`; the same `box.a1 / 2` extent is independently
  recomputed in `hitTest` (`ConstructorView.fs:183-184`) and `indicatorView`
  (`ConstructorView.fs:687`).
- **Existing helper:** `Drawer.draw` (`Drawer.fs:98-166`) — the slice-004
  "ONE standard cylinder drawer used by every element (C.4.2)" — already
  returns pure top-down `DrawerGeometry` (`frame` silhouette, `axisStart`/
  `axisEnd` caps, `capRadius`, role `fill`, `frameStroke`, and the optional
  `boundingBoxEdges`). Its module header states the FuncUI `Canvas` binding
  that turns those values into Avalonia geometry "are slice 005" — i.e. this
  slice. `ConstructorTable.drawnSideRayCount` (`ConstructorTable.fs:148`) is
  the matching helper for the show-all-rays case.
- **Why it matters:** the worker reuses only `Drawer.shadeFor`
  (`ConstructorView.fs:669`) and `Drawer.showBoundingBoxDefault`
  (`:116`) but not `Drawer.draw` itself, so the canonical drawer geometry is
  reinvented as an ellipse and the box-edge / cap geometry is dropped. The
  symptom is two dead model fields: `showBoundingBox`
  (`ConstructorView.fs:90`, seeded `:116`) and `showCentralRayOnly`
  (`:86`, seeded `:115`) are never read by `update` or `view`, because the
  helpers that consume them (`Drawer.draw`, `ConstructorTable.drawnSideRayCount`)
  are never called. The state-of-the-world claims "the geometry reuses
  `ConstructorTable.project` / `Drawer`" (Architecture §), which overstates the
  `Drawer` reuse to the judge. Slice 006/007 extending this surface will hit a
  page that draws circles where the rest of the codebase has a tested cylinder
  drawer.
- **Suggested action:** bind `Drawer.draw` (and read `showBoundingBox` through
  it) in `elementView`, deriving the radius from `DrawerGeometry.capRadius`
  rather than recomputing `box.a1 / 2` in three places; gate the side rays on
  `showCentralRayOnly` via `ConstructorTable.drawnSideRayCount`. If the
  ellipse-and-CR-only render is an intentional MVP for this interaction slice,
  record that explicitly so the dead fields are not mistaken for reuse. Note
  this is partly architecture-flavoured (under-rendering); flagged here only
  for the bypassed `Drawer.draw`/`drawnSideRayCount` helpers — the
  architecture critic owns the rendering-completeness call.

### F3: In-plane 2-D rotation duplicates the table-normal rotation math (low confidence)

- **Worker added:** `rotateInPlane` (`ConstructorView.fs:609-612`):
  `(px*c - py*s, px*s + py*c)` on a `(float, float)` canvas vector.
- **Existing helper:** `RayModel.rotateAboutTableNormal`
  (`RayModel.fs:98-101`) is the identical planar-rotation kernel on a
  `Vector3` about +Z.
- **Why it matters:** the same rotation algebra now lives in two modules. The
  duplication is minor: the existing one is `private` to `RayModel` and typed
  on `Vector3` (table-frame), whereas the worker's operates on screen-space
  `float` pairs at the Avalonia boundary, so it cannot be called as-is.
- **Suggested action:** acceptable to leave as-is given the type/visibility
  mismatch; if a shared scalar 2-D rotate is ever wanted, hoist one and have
  both call it. Low priority.

## Bottom line

The diff reuses the engine, the units spine, the lock/emission/rotation
setters, `Table.resetView`/`ConstructorTable.project`, the `RayModel` defaults,
the `Shell` host-mutable pattern, and the `HeadlessSession`/`Component` test
harness cleanly — most of the reuse mandate is honoured. The two substantive
findings are F1 (a third `SchematicColor -> IBrush` copy that re-opens the
documented immutable-brush decision) and F2 (element rendering reinvented as
ellipses while the slice-004 `Drawer.draw` it was meant to bind sits unused,
leaving two dead model fields). Neither blocks correctness and the gates pass;
my read is these are worth a fix-forward note rather than a re-spawn, but the
judge decides.
