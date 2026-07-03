# 0027-013 — Reusable table, spherical-cap renderer, snap-to-reflected test (implementation log)

Implements `012-more-test-tweaks.txt` (its three parts). Full solution builds clean;
`OpticalConstructor.Ui.Tests` **203/203** (was 196 — +7), `OpticalConstructor.Tests` **245/245**
(unchanged — domain untouched).

## Part 1 — one reusable optical table (`TableScene`)

The tests each re-derived the same table (canvas size, projection, plate drawing, gestures). That is now
**one** shared, stateless module — `OpticalConstructor.TestWindows/TableScene.fs` — that every scene calls
into instead of re-creating it. It is deliberately stateless: each test keeps its own model (its table,
view, selection, elements) and the scene only supplies the shared *behaviour* over a `TableViewState` +
`OpticalTable`:

- geometry: `canvasWidth`/`canvasHeight` (820×560), `center`, `pixelsPerMeter` (200), `project`, `toPoint`;
- selection: `tableHit` (whether a screen point lands on the plate, to (de)select the table);
- gestures (pure `TableViewState` transforms): `rotateViewBy axis deg`, `zoomViewBy notches`,
  `panViewBy dx dy`, `resetView`;
- drawing: `plateViews view table selected` (the plate as a 3-D box — top face + twelve edges — highlighted
  when the table is the selected object) and `sourceDetectorRayViews view` (the default central ray plus
  source/detector markers).

So "the table is selectable / un-selectable, rotatable, zoomable, draggable" is now realised once and reused.
`RendererTestView` was migrated onto it as the first consumer (its old local `center`/`pixelsPerMeter`/
`projectPt`/`plateViews` are gone — it now calls `TableScene.project` and `TableScene.plateViews … (model.selected = None)`,
highlighting the plate when the table is the rotation target). The new snap-to-reflected scene (Part 3) is
built on `TableScene` from the start. `TableScene` is the standard the remaining older scenes will be folded
onto as they are next touched.

## Part 2 — renderer: L and CM are cylinders with SPHERICAL caps, configurable transparent rails

S, LP, D were correct (flat-capped cylinders); L and CM were wrong (flat 2-D figures). The fix in
`RendererTestView.fs`: **everything is a cylinder** whose axis is the blue N1 normal; the *only* difference
for a lens / curved mirror is that its caps are a **part of a sphere** instead of flat circles.

- New `domeViews` helper draws a spherical cap of base radius `r`, bulging by `bulge` along N1 with
  `z(ρ) = bulge · √(1 − (ρ/r)²)` — the rim ring, an inner latitude ring, meridians to the apex, lightly
  filled. Positive bulge = away from the element (convex), negative = into it (concave).
- **Lens** = the cylinder with **two** spherical caps: both bulge **out** for a converging (convex) sign,
  both curve **in** for a diverging (concave) sign (`opticalSign` ±1).
- **Curved mirror** = the cylinder with **one** spherical reflective cap (concave for a focusing sign,
  convex for diverging) plus a flat back cap.
- **Flat** elements (S / LP / CP / Sa / FM / D) keep flat caps.

**The cap-connecting rails are now a configurable, partially-transparent parameter.** The old code hard-wired
4 rails (`[0;6;12;18]`); the rail count is now `Model.rails` (default **12**), driven by an on-screen
`Slider` (min **4**, max **60**, snap-to-tick) in the control bar with a live numeric readout. New
`railViews` helper draws `rails` evenly-spaced rails (angle `2π·i/rails`) connecting the two caps, each at
**alpha 0.35** (partially transparent) per the request. `SetRails` clamps to `[railsMin, railsMax]`;
`shapeRenderer`/`rendererOf` now take the rail count. Both normals (blue N1, yellow N2) are still always
drawn (`normalsViews`), and the element code still sits beside each shape.

Tests (`RendererTestTests`, +2): the seed has a convex **and** a concave lens and a concave **and** a convex
curved mirror (both cap signs visible at once); the rail count defaults to 12 and clamps to `[4, 60]`.

## Part 3 — snap-to-reflected-light test

New scene `OpticalConstructor.TestWindows/SnapToReflectedView.fs` + `SnapToReflectedWindow.fs`
("Test — Snap to Reflected Light", launcher button added). Exactly three elements: **source, flat mirror at
R2 = 45°, detector**. The light is **reflected** off the mirror, never transmitted, and the detector
**snaps onto the reflected beam**.

- The snap is `RayModel.snapChain` over the downstream stops with a **`BeamTree.Reflected`** branch at the
  mirror (the detector stop is the end). `snapChain` turns the beam by `reflect` off the mirror's oriented
  face, so the detector lands on the reflected segment; gaps `[1.0 m ; 0.5 m]` put the mirror at the table
  centre and the detector 0.5 m along the reflected ray (at R2 = 45° → `(0, −0.5)`, on the plate).
- **R2 or R3 steers the beam; R1 does not.** The reflected direction comes from `RayModel.faceNormal`, whose
  oriented N1 (`Placement.r1Axis`) is built from R3 then R2 only — R1 spins about that normal and leaves it
  unchanged. So rotating the mirror's R2/R3 moves the snapped detector; rotating R1 leaves it bit-identical.

The scene reuses `TableScene` (table look + gestures), the shared `RotationControls` bar (the mirror starts
selected so the bar/wheel act on it), and `SceneInput.canvasPoint` for pointer→canvas mapping — same
selection model (Table / Element / Nothing) and wheel map as the other scenes.

Tests (`SnapToReflectedTests`, 6): the scene is source/flat-mirror-at-45/detector; at R2 = 45° the beam
arrives +X and leaves −Y (reflected ⟂ incident — it did not go straight through) with the detector on the
reflected ray; R2 and R3 each move the snapped detector (Δ > 1e-3); **R1 (35°) leaves the detector
unchanged**; plus a headless window-open smoke.

## Files

- New: `OpticalConstructor.TestWindows/{TableScene,SnapToReflectedView,SnapToReflectedWindow}.fs`;
  `OpticalConstructor.Ui.Tests/SnapToReflectedTests.fs`.
- Changed: `RendererTestView.fs` (migrated to `TableScene`; spherical-cap lens/mirror; configurable
  transparent rails + slider/`SetRails`); `RendererTestTests.fs` (+2 tests); the TestWindows / Ui.Tests
  `.fsproj`s (new files); `OpticalConstructor.App/Program.fs` (the snap-to-reflected launcher button,
  launcher height bumped).
