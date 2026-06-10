# 004 — State of the world: Optical table, drawer & top-down rendering geometry; standardized controls

## Where we are

Slice 004 realises **Part C** (the optical table + drawer + top-down rendering
geometry) and **Part J** (standardized controls) of Spec 0026 (Optical Constructor
UI), as PURE, headless-testable projections on top of slice 001 (`Placement.fs`) and
slice 002 (`RayModel.fs`). It adds the `OpticalTable` plate (+ top-down view state),
the single standard cylinder `Drawer`, the top-down `ConstructorTable` layout +
drawing-weight constants, and the one `Controls` flavours module; it extends the
project aggregate + schema with the table and proves the table round-trip, the drawer
geometry/shadings/box-toggle, the active-element indicator threshold, the CR-only
default, and control-flavour usage headlessly. It deliberately ships NO FuncUI
`Canvas` page and NO interaction gestures (slice 005), and NO ribbon controls (slice
006); the existing cross-section `Schematic.fs` is left untouched (the top-down
surface is net-new).

## What's working

- Add `Table.fs`: the `OpticalTable` plate (width/length/thickness in canonical SI +
  display unit; default `1.2 × 2.0 × 0.10 m`, editable `withSize`) and the pure
  `TableViewState` (screen R1/R2/R3, pan, zoom; default top-down `(0,0,0)` + reset).
- Add `Drawer.fs`: the ONE standard cylinder drawer keyed by placement, three
  rotations, kind, and value id — light-grey transparent interior, darker source,
  near-black detector with a still-visible frame, 12 box edges when show-box is on.
- Add `ConstructorTable.fs`: the named drawing-weight constants (C.5.1), the grey plate
  drawn to scale + the top-down view transform, the per-ray stroke map, the WCAG
  `contrastRatio`, the active-element indicator, and the CR-only default.
- Add `Controls.fs`: the default/primary/destructive button flavours (overriding only
  the distinguishing properties), a toggle, a numeric-with-units, and the
  destructive-gate CTAs; rewire `ConstructionView.fs:40` to draw its CTAs from here.
- Extend `OpticalConstructorProject` + the JSON schema with the `table`; prove the
  edited plate size round-trips through `saveProject`/`openProject` and validates.
- Add `ConstructorViewTests.fs` (11 tests, AC-C4/AC-C5/AC-C6/AC-J1 + table/view-state
  defaults) and two AC-C1 round-trip tests in `ProjectJsonRoundtripTests.fs`.

## Tests

All seven applicable gates pass locally (logs under
`C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error`
  matches — the gate's `stdout_match` veto). Log `004-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched). Baseline `berreman_unit_tests = 84` held. Log `004-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 222 / Total 222; 220 prior + 2 new
  AC-C1 table round-trip tests). Log `004-constructor-unit-tests.log`.
- `ui-tests` — PASS (exit 0, Passed 44 / Total 44; +11 new `ConstructorViewTests`,
  Category!=ui-smoke). Log `004-ui-tests.log`.
- `ui-smoke` — PASS (exit 0, Passed 1; the headless host boots and renders one frame
  per existing view with the new Ui modules linked). Log `004-ui-smoke.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings
  present).

Coverage: AC-C1 default `1.2 × 2.0 × 0.10 m` + edited-size round-trip through the
schema-validated canonical project; AC-C4 cylinder geometry + source/detector shadings
+ visible detector frame + show-box toggle adding the 12 box edges (off by default);
AC-C5 indicator ≥ 2 px and ≥ 3:1 WCAG contrast vs the plate (+ a contrast-ratio sanity
check); AC-C6 CR-only default + side-ray redraw-on-change; C.5 chief-ray-prominent /
reflected-lighter drawing weights; AC-J1 flavours from one module + a new flavour
overriding only the distinguishing property + distinct CTA colours. None deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 222
```

## Architecture

- **The table lives in two layers: the persisted plate vs the ephemeral view.**
  Only the physical `OpticalTable` (width/length/thickness + display unit) is added to
  `OpticalConstructorProject` and the schema (C.1.2 / AC-C1); the `TableViewState`
  (screen R1/R2/R3, pan, zoom) is pure UI state owned by the interaction layer (slice
  005) and is NOT persisted. The slice owns the default top-down `(0,0,0)` + default
  zoom and the reset target as pure values (C.2.1 / C.2.5).
- **The drawer is a pure top-down projection in table-frame meters, view-independent.**
  `Drawer.draw` returns `TablePoint` geometry (cylinder silhouette + cap axis + box
  edges) and a pure `Fill`/`Stroke`; the view transform (zoom/pan/screen-tilt) is
  applied by `ConstructorTable.project` / slice 005. It reuses `Placement.orientedBasis`
  for the rotations rather than re-deriving rotation math (constraint 0.1). The cylinder
  silhouette is R1-spin-invariant (a cylinder looks the same spun about its axis); the
  square box edges DO encode R1/R2/R3, which is why the box toggle is the orientation
  tell (C.4.3).
- **One styling source for the schematic.** `ConstructorTable.fs` holds every named
  drawing-weight/colour constant (C.5.1/C.5.2) — central vs side ray weights, the
  reflected-group opacity, the element frame/interior, the grid, the plate grey, and the
  active-element indicator — so a single edit restyles the whole top-down surface. The
  drawer and (future) ray layer consume those constants; nothing styles ad hoc.
- **Active-element clarity is proved from pure colour values (AC-C5).** A pure WCAG
  `relativeLuminance` / `contrastRatio` pair lets the ≥ 3:1 focus-floor threshold be
  asserted headlessly against the plate colour, with no Avalonia render. The indicator
  is a strong dark blue (≈ 8:1 vs the light-grey plate) at 2.5 px.
- **`Controls.fs` flavours are pure style records + thin FuncUI functions.** Each button
  flavour is a `ButtonFlavor` record built by overriding only its distinguishing
  properties (`{ defaultButtonFlavor with background = … }`, J.1.2), so AC-J1 is provable
  on the records without a headless render; the flavour FUNCTIONS apply them. The CTA
  colours/brushes are the single destructive-gate definition that `ConstructionView.fs`
  now draws from (J.2 / R-9). Brushes are IMMUTABLE (`ImmutableSolidColorBrush`) so the
  module-level CTA values construct off the UI/test thread.
- **Schema/record extension is additive.** A new optional root `table` property + the
  `opticalTable` `$def` (width/length/thickness numbers + `unitOfMeasure`); the reserved
  anchors and slice 001's `placements` are untouched. The record field is mandatory
  (always serialized), matching the slice-001 `placements` precedent.

## Deferred

- The FuncUI `Canvas` MVU page that binds the drawer/table/ray geometry to Avalonia, and
  the pan/zoom/rotate/select/drag gestures that drive `TableViewState` — slice 005 (Part
  E). AC-C2 and AC-C3 are owned there; this slice ships the pure geometry + default view.
- The ribbon controls that HOST the toggles — the *Trace / View* show-CR toggle and the
  table-resize control — slice 006 (Part D). This slice ships the CR-only default state
  and the redraw-on-change geometry, and the editable-plate `withSize`.
- Per-device/material drawer looks behind `valueId` — Part F / later. The single standard
  drawer is used for every element now; `valueId` is accepted from the start so the look
  can branch later without rewiring (C.4.1).
- The screen-tilt R1/R2/R3 projection of `TableViewState` (the non-top-down view) — slice
  005's interaction layer; `project` implements the top-down (scale + Y-flip + pan) case.
- No caching / retained-mode scene graph / drawing-backend abstraction (0.6); no
  repurposing or removal of `Schematic.fs` (C.0).

## Gotchas

- **A mutable Avalonia `SolidColorBrush` throws "Call from invalid thread" at module
  init.** `Controls.positiveCtaBrush`/`negativeCtaBrush` are module-level values; building
  them from a mutable `SolidColorBrush` (an `AvaloniaObject` with thread affinity) failed
  in the headless test thread AND broke the existing `ConstructionEditTests` delete-gate
  render. The fix is `ImmutableSolidColorBrush` (what `Brushes.SeaGreen` already is) —
  thread-safe, no platform/Dispatcher needed. Any future module-level brush MUST be
  immutable.
- **Adding the mandatory `table` field broke every `OpticalConstructorProject` literal.**
  9 sites were swept with `table = OpticalConstructor.Domain.Table.defaultTable` (1
  production: `Templates.fs`; 8 in tests). Future top-level fields will need the same
  sweep; `{ p with … }` copies are unaffected. The field is fully-qualified in the sweep
  because the construction sites do not all `open OpticalConstructor.Domain`.
- **`ConstructionView.fs` was edited even though it is not in the "files in scope" list.**
  Its `:40` `positiveCta`/`negativeCta` ARE in the slice's "File:line references owned"
  list (J.2 / R-9), so the two-line rewire to draw from `Controls` is in scope; it is
  behaviour-preserving (SeaGreen `#2E8B57` / IndianRed `#CD5C5C` unchanged).
- **The view state is NOT persisted.** Only the plate persists (C.1.2). A reopened
  project shows the same plate but the default top-down view — by design (the spec
  requires only the plate to persist; pan/zoom is session state).
- **The drawing weights/colours are the single restyle seam.** Later slices MUST read
  `ConstructorTable.*WeightPx` / `*Opacity` / `*Color` (and `Drawer` reuses the frame /
  interior ones) rather than hard-coding pixel weights — a single edit there restyles the
  whole schematic (C.5.1).
- **EOL: the Edit/Write tooling emits CRLF on this host; the repo is LF
  (autocrlf=false).** Every touched file was normalized back to LF after editing
  (verified: 0 CR bytes; `git diff` shows only the real changes, e.g.
  `ConstructionView.fs` is a 7-line diff, not whole-file churn). A future editor MUST
  re-normalize after any tool-driven edit.

## Changelog

- 2026-06-06 (slice 004): Land Part C + Part J — the optical table, the standard cylinder
  drawer, the top-down rendering geometry, and the standardized controls. New `Table.fs`
  (`OpticalTable` 1.2 × 2.0 × 0.10 m + `withSize`; `TableViewState` top-down default +
  reset), `Drawer.fs` (one cylinder drawer: light-grey interior / darker source /
  near-black detector with visible frame / 12 box edges), `ConstructorTable.fs` (named
  drawing-weight constants, grey plate to scale + view transform, per-ray strokes, WCAG
  `contrastRatio`, active-element indicator, CR-only default), and `Controls.fs`
  (default/primary/destructive button flavours + toggle + numeric-with-units +
  destructive-gate CTAs). Extend `OpticalConstructorProject` + the JSON schema with the
  `table`; rewire `ConstructionView.fs:40` CTAs to draw from `Controls`. Add
  `ConstructorViewTests.fs` (11 tests, AC-C4/AC-C5/AC-C6/AC-J1 + table/view defaults) and
  two AC-C1 round-trip tests. All seven gates pass (berreman_unit_tests 84 held,
  constructor_unit_tests 220 → 222, ui-tests 33 → 44, ui-smoke green).
