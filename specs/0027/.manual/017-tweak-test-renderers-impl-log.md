# 0027-017 — Renderer test: roll seam + cap-detail and transparency controls (implementation log)

Implements `016-tweak-test-renderers.txt`. Full solution builds clean; `OpticalConstructor.Ui.Tests`
**213/213** (was 210), `OpticalConstructor.Tests` **245/245** (domain untouched). All changes are in the
renderer test screen (`RendererTestView`) and its tests.

## BUG — the cylinder rails appeared not to rotate with R1

The rails are rigidly attached to the oriented (N1, N2, N3) basis, so under an R1 spin every rail *does*
move — but a **symmetric ring of identical rails has no visible phase** (rotating an N‑fold ring by 2π/N
looks unchanged), and at the new default of 72 rails an R1 step is essentially one symmetry period, so the
ring looked frozen. Task 014's "marked rail" sat at −N2, exactly under the yellow normal, so its motion was
masked by the normal.

Fix: rail 0 is now an always‑visible **RED roll SEAM** at **+N2** (the cap "top", opposite the yellow
normal), painted opaque and a touch heavier, distinct from both normals (blue N1 / yellow N2). It follows
+N2, which rotates *with* R1, so the seam visibly orbits the cylinder in sync with the spin — the roll is
now readable at any rail count. (It rotates the same angular sense as the yellow normal, i.e. "follows the
yellow normal", satisfying task 014's intent too; it is placed opposite the normal only so the two don't
overlap. Co‑locating it with the normal would be a one‑line change if preferred.)

## Cap detail controls — inner circles + meridian radials

The lens / curved‑mirror spherical caps were drawn with a fixed 1 inner circle and 8 meridians. Both are
now on‑screen controls (the whole point of this page is to see "how it looks"):

- **Cap circles** — the inner latitude rings, a 1 … 8 integer slider, **default 1** (the current look).
  `capSurface` now draws `circles` rings at radii `r·k/(circles+1)`.
- **Cap radials** — the curved meridians, a discrete slider over the presets `[4; 8; 12; 24; 36]`,
  **default 4** (the radials count is now `radialOptions`, replacing the hard‑wired `capMeridians = 8`).

`RendererTestView.Model` gained `capCircles` / `capRadials`; `SetCapCircles` clamps to `[1, 8]`,
`SetCapRadials` snaps to the nearest preset, `SetCapRadialsIndex` is the slider's index setter.

## Transparency controls

Three live transparency knobs (0 … 1), defaulting to the **current** look, so the overlapping translucent
shapes can be tuned away from the figure‑ground "flip". The **yellow and blue normals are never affected**
(always fully visible), per the request:

- **Rail opacity** — the translucent side rails (default 0.35).
- **Face opacity** — the solid coloured end‑cap / spherical‑cap rim fill (default 0.85).
- **Line opacity** — the spherical‑cap meridians + inner circles (default 0.70).

These thread through a new `ShapeConfig` record (`{ rails; circles; radials; railOpacity; faceOpacity;
lineOpacity }`) built from the model by `shapeConfig` and passed to `shapeRenderer` / `rendererOf` and the
draw helpers, so the geometry functions stay free of the Model. The red roll seam keeps a fixed high
opacity (it is a functional indicator, not a tunable).

## Controls layout

The control bar is now three rows (slider helpers `presetSlider` / `intSlider` / `opacitySlider` keep it
compact): row 1 swap + rail count; row 2 cap circles + cap radials; row 3 the three transparency sliders,
each with a live numeric readout. The window grew to `canvasHeight + 150` to fit them.

## Tests

`OpticalConstructor.Ui.Tests` **213/213** (+3 in `RendererTestTests`):
- cap circles default 1 / clamp `[1, 8]`; radials default 4 / snap to `[4; 8; 12; 24; 36]` (value and slider
  index);
- the three transparency knobs default to `0.35 / 0.85 / 0.70` and clamp to `[0, 1]`;
- a headless proof that the page actually exposes the rail / cap‑circles / cap‑radials / three transparency
  sliders (by their automation ids).
The existing swap / rails / seed / gesture / code‑label tests are unchanged and still pass.

## Files

- Changed: `OpticalConstructor.TestWindows/RendererTestView.fs` (red roll seam; configurable cap circles /
  radials; three transparency knobs via `ShapeConfig`; three‑row control bar), `RendererTestWindow.fs`
  (taller window), `OpticalConstructor.Ui.Tests/RendererTestTests.fs` (+3 tests).
