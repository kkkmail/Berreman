# 0027-011 — Main-screen tweaks + element-movement & renderer test screens (implementation log)

Implements `010-comments.txt`. Build is green; `OpticalConstructor.Ui.Tests` **190/190** (was 181 — +9
across the new work). The static test scenes keep their behaviour; the main screen is still the shared
`TableAndElementRotationView` scene seeded by `initMain`.

## Main screen

**Detector now snaps to the right edge.** `initMain` placed the light source / detector at x = ∓0.8 — both
inset from the plate edges, so the detector looked unsnapped. They now sit on the **central-ray endpoints**
(`RayModel.defaultSourcePoint` / `defaultDetectorPoint`), which lie exactly on the plate edges (the
source→detector distance equals the table length). So the light source snaps to the left edge and the
detector to the right edge.

**The add/remove buttons are now a reusable control, styled like the rotation bar.** They were default
Avalonia `Button`s (a different look). New control **`OpticalConstructor.Controls/ElementPaletteControls.fs`**
(a sibling of `RotationControls`): `view (state) (handlers)` renders one "+ label" button per addable kind
plus "Remove selected", all as the SAME bordered click-box the rotation bar uses (id'd via
`ElementPaletteControls.UiIds`). The scene maps its palette to it (`paletteState`/`paletteHandlers`, keyed
by the short `kindCode`). `kindName`/`kindCode` were made public on `TableAndElementRotationView` (the
codes are reused by the renderer test).

## General — controls moved to the top

Per the request, the control bar is now docked at the **top** (`Dock.Top`) on every scene: the three
existing test views (`TableRotationView`, `ElementRotationView`, `TableAndElementRotationView` — the last
of which IS the main screen) were flipped from `Dock.Bottom`, and the two new test screens are authored
with the controls on top.

## New test screen — element movement

`OpticalConstructor.TestWindows/ElementMovementView.fs` + `ElementMovementWindow.fs` ("Test — Element
Movement", launcher button added). A SINGLE Sample element in the middle of the table that the user slides
along the central ray (§E.4 "slide along the ray"): a left-drag that **begins on the element** slides it to
the pointer's x; **←/→** nudge it (`Shift` = a larger step); the position is clamped to the plate and the
element rides the ray (y = 0). A fixed top-down view keeps the screen→table inverse trivial, so we can work
out the movement quirks first; the readout shows the along-beam position and a "Reset position" button
re-centres it. The main screen is deliberately **not** changed to support movement yet. Quirk noted:
arrow-key nudging needs the canvas focused (click it once). Tests (`ElementMovementTests`, 4): centre seed;
`SlideBy`/`SlideTo` clamp to ±1.0 m; a drag moves the element ONLY when it begins on it.

## New test screen — swappable renderers

`OpticalConstructor.TestWindows/RendererTestView.fs` + `RendererTestWindow.fs` ("Test — Renderers",
launcher button added). The element drawing is a real abstraction:

```fsharp
type ElementRenderer = { name : string; draw : (Vector3 -> ScreenPoint) -> bool -> Element -> IView list }
```

with two implementations swapped at runtime (`RendererKind = Wireframe | Shape`, `Msg.SwapRenderer`, the
top "Swap renderer" click-box `RendererTestView.UiIds.swapRenderer`):

- **`wireframeRenderer`** — today's look: the element's projected bounding-box edges + the N1/N2 normals.
- **`shapeRenderer`** — a round `Ellipse` coloured by the element's nature, labelled beside it with the
  short element **code** (S / LP / CP / Sa / L / FM / CM / D, from `TableAndElementRotationView.kindCode`).

The scene seeds a light source, a polarizer, a sample, a flat mirror and a detector so the codes/shapes are
all visible; clicking selects the nearest element. Only this screen exposes the swap for now. Tests
(`RendererTestTests`, 5): the swap toggles `Wireframe ⇄ Shape`; the seed has several kinds; a far click
deselects; a **headless** proof that the shape renderer draws the "LP" code label and the wireframe renderer
does not; and a window-open smoke.

## Files

- New: `OpticalConstructor.Controls/ElementPaletteControls.fs`;
  `OpticalConstructor.TestWindows/{ElementMovementView,ElementMovementWindow,RendererTestView,RendererTestWindow}.fs`;
  `OpticalConstructor.Ui.Tests/{ElementMovementTests,RendererTestTests}.fs`.
- Changed: `TableAndElementRotationView.fs` (detector snap, palette control, `kindName`/`kindCode` public,
  controls→top), `TableRotationView.fs` / `ElementRotationView.fs` (controls→top), the three `.fsproj`s
  (new files + the `Controls` palette file), `OpticalConstructor.App/Program.fs` (two launcher buttons +
  window hosts), `TableAndElementRotationTests.fs` (palette smoke updated to the new control's id).

## Follow-up fixes (review feedback)

Three regressions/gaps reported after the above:

**Selection broke in every scene (and the main screen).** Moving the controls to `Dock.Top` pushed the
canvas DOWN within the window, but the pointer handlers used `e.GetPosition null` (top-level coordinates),
so clicks/hit-tests landed below where the projection expected them — off by the control-bar height. (Pan
still worked because it uses deltas; the pure unit tests pass because they feed canvas coordinates
directly.) Fix: a shared `SceneInput.canvasPoint canvasName e` (new `OpticalConstructor.TestWindows/
SceneInput.fs`) that locates the named canvas in the visual tree and returns the pointer **relative to
it**; every scene's `toScreen` now uses it. One headless injection test (`clicking an element then
Shift+wheel…`) injected at a fixed window point that the new layout shifted — it now targets the
element's real window position via the canvas offset (`atCanvas`).

**The renderer test lacked table zoom / drag / rotations.** It now carries the same gesture model as the
combined scene: a drag pans the view, a clean click selects the nearest element, a plain/Ctrl wheel zooms,
and Shift/Ctrl+Shift/Alt+wheel rotates the SELECTION (the selected element, else the table view). Its
elements seed with R3 **unlocked** so you can tip them and watch the shape renderer respond.

**(critical) The shape renderer drew elements as if R3 = 90 (facing up).** It drew a face-on disc sized
from the face extent, which reads as the element tipped toward the top-down viewer. An optical element at
rest has its face perpendicular to the beam, so it should appear **edge-on / along the beam**. Fix: the
shape is now an ellipse sampled IN the element's oriented depth×face plane (semi-axes along N1 = the beam
and N3 = across the face) and projected — so at rest it is thin/edge-on (a source, being long along the
beam, reads as a wide oval; a thin polarizer as a narrow one), and it opens into a disc only as the
element is tipped about R3. Colour still conveys nature; the code label still sits beside it.

Tests: `OpticalConstructor.Ui.Tests` **192/192** (renderer gesture tests added: wheel zoom/rotate-selection,
drag-pan; the renderer `SelectAt` test became a clean-click test). Full solution builds clean. Files:
new `SceneInput.fs`; `TableRotationView.fs` / `ElementRotationView.fs` / `TableAndElementRotationView.fs` /
`ElementMovementView.fs` (use `SceneInput.canvasPoint`); `RendererTestView.fs` (gestures + oriented shape);
`RendererTestTests.fs` / `TableAndElementRotationTests.fs` (test updates); the TestWindows `.fsproj`
(`SceneInput.fs`).

## Second review round — correct shapes + snap-to-beam test

**The alternative renderer's shapes were wrong.** A flat optical element must read as a CYLINDER whose
axis is the blue N1 normal; lenses / curved mirrors must show their +/- nature; both normals must always
be drawn. Rework of the shape renderer (`RendererTestView.fs`):
- flat elements (light source, polarizer, sample, flat mirror, detector) → a 3-D **cylinder**: two end-cap
  circles in the N2–N3 face plane (radius = the face extent) at ±half the depth along N1, joined by side
  rails. A source (long along the beam) reads as a rod, a thin polarizer as a coin — edge-on at rest,
  opening as it tips.
- **lens** → biconvex (fat centre) for a converging sign, biconcave (pinched centre) for diverging, drawn
  in the N1–N3 plane; **curved mirror** → a concave (focusing) / convex arc with a thin backing line.
  Schematic only — not curved to a real focal length. The sign comes from a seeded `opticalSign` on the
  test element (a bare placement carries no signed radius of curvature), so the scene now seeds a convex
  and a concave lens and a concave and a convex mirror alongside the cylinders.
- **both normals** (blue N1, yellow N2) are now drawn by the shape renderer too (extracted `normalsViews`).

**New test screen — snap to beam** (`SnapToBeamView` + window + launcher button, built via a context-
forked agent and integrated). A light source plus a polarizer / sample / detector at fixed along-ray gaps.
Rotating the source's R2 (buttons or wheel, Shift = larger) re-aims the emitted direction (its oriented
N1 = `Placement.r1Axis`), and `RayModel.snapChain` re-snaps every downstream element along the new beam,
preserving along-ray spacing — i.e. all elements snap to the beam (spec B.3). Untilted pass-through stops
keep the beam straight, so the whole chain swings about the source. Tests prove the rest chain lies on the
+X ray and that after a rotation each element is the same distance from the source but off the old ray.

Tests: `OpticalConstructor.Ui.Tests` **196/196**. Full solution builds clean. Files: `RendererTestView.fs`
(cylinder/lens/mirror geometry + `opticalSign` + both normals), `RendererTestTests.fs` (seed); new
`SnapToBeamView.fs` / `SnapToBeamWindow.fs` / `SnapToBeamTests.fs`; the TestWindows / Ui.Tests `.fsproj`s;
`OpticalConstructor.App/Program.fs` (the snap-to-beam launcher button).
