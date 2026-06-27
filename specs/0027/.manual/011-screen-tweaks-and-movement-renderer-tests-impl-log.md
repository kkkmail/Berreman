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
