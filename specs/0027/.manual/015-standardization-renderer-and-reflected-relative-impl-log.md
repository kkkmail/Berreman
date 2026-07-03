# 0027-015 — Consistent shared controls, renderer cap/rail fixes, reflected-beam-relative orientation (implementation log)

Implements `014-more-standardization.txt` (its three parts). Full solution builds clean;
`OpticalConstructor.Ui.Tests` **210/210** (was 207), `OpticalConstructor.Tests` **245/245** (domain
untouched).

## Part A — use the shared table + controls consistently; a standardized position-on-the-ray control

**Every non-main test screen now draws the ONE shared table (`TableScene`).** The screens that still kept
their own projection / plate / ray / gesture code were migrated onto `TableScene` (the same migration the
renderer and snap-to-reflected screens already had):

- `TableRotationView` — local `center`/`pixelsPerMeter`/`projected`/`plateViews`/`referenceViews` and the
  `TableView.tableHit` call replaced with `TableScene.project`/`plateViews`/`sourceDetectorRayViews`/`tableHit`.
- `ElementRotationView` — local projection + `panFrom`/`zoomTableBy`/`tableViews` replaced with
  `TableScene.project`/`panViewBy`/`zoomViewBy`/`plateViews`/`sourceDetectorRayViews`.
- `ElementMovementView` — local projection + plate/ray drawing replaced with `TableScene` (the fixed
  top-down inverse `tableXOfScreen` now reads `TableScene.center`/`pixelsPerMeter`).
- `SnapToBeamView` — already standardized onto `TableScene` + the shared `RotationControls` bar (acting on
  the source) in this pass.

`canvasWidth`/`canvasHeight` on each screen are now `= TableScene.canvasWidth/Height` (one source of truth).
The projection constants are identical (820×560 @ 200 px/m, same centre), so every migration is
coordinate-preserving — no test coordinates changed (only the test refs to the removed
`TableRotationView.center` / `ElementMovementView.center` were repointed to `TableScene.center`). **The main
screen (`TableAndElementRotationView`) was deliberately left untouched** per the instruction ("do not waste
time on the main screen until we iron out all the controls").

**New shared control: `OpticalConstructor.Controls/RayPositionControls.fs`** — the position-on-the-ray bar
(§E.4: the along-ray position is a distance the host clamps to the plate). It is the position analogue of
`RotationControls`: a pure projection of a `State { position; minPosition; maxPosition; enabled }` with
`Handlers { moveBy; setPosition; reset }`, rendering − / + step buttons (0.05 m, 0.20 m with Shift), an
editable metres field, and a Reset, with stable automation ids. `ElementMovementView` now mounts it (its
`SlideBy`/`SlideTo`/`ResetPosition` messages wire straight onto the handlers); the screen's drag and ←/→
gestures are unchanged. So if any of these breaks there is now ONE control to fix.

## Part B — renderer cylinder rails + L/CM spherical caps

- **Discrete rails.** The rail count is now a fixed set of presets `[4; 8; 12; 24; 36; 72]`, **default 72**,
  driven by a snap-to-tick slider that rides over the preset INDEX (`SetRailsIndex`) so it only ever lands
  on a preset, with a numeric readout. `SetRails` snaps any value to the nearest preset.
- **L / CM caps are genuine spherical caps with CURVED meridians.** The old caps drew 4 STRAIGHT rim→apex
  segments; the new `capSurface` draws the meridians as **polylines that follow the sphere sagitta**
  `d(ρ) = apexD + (rimD − apexD)·(1 − √(1 − (ρ/r)²))` — actually curved — using `capMeridians = 8`
  (independent of the rail count; 72 would be far too busy on a cap).
- **Nothing sticks out of the bounding box when the renderer is swapped.** The caps are placed by their rim
  / apex distances along N1, both kept within ±halfLen: a convex lens pushes its apexes out to the box faces
  with the rims inset (biconvex, thin edges); a concave lens keeps the rims at the faces with the apexes
  receding (biconcave); the curved mirror's single reflective cap follows the same fit (apex or rim at the
  face, never beyond) over a flat back cap. The previous `+`-lens that bulged past the box is gone.
- **R1 vs the yellow normal.** The rails are rigidly attached to the oriented (N1,N2,N3) basis, so they
  cannot truly counter-rotate against the yellow (N2) normal — they co-rotate with it; the "opposite
  direction" reads from a symmetric ring of identical rails (any rigid spin moves diametrically-opposite
  rails opposite *linearly*). To make the roll **readable and follow the yellow normal**, the rail at t = π
  — which sits along −N2, exactly where the yellow normal points — is now painted yellow and a touch
  heavier; that one marked rail visibly tracks the yellow normal as the element spins about R1.

## Part C — snap-to-reflected: elements on the reflected beam are oriented RELATIVE to the beam

A downstream element on the reflected beam (here the detector) is now drawn oriented relative to that beam.
Since R2/R3 are stored as absolute table angles, the screen ADDS the beam's own absolute orientation to the
element's dialled (relative) R2/R3 when drawing: `beamOrientation dir = (atan2 dir.y dir.x, asin dir.z)`
(the inverse of the rest-basis forward map `dir = (cosR3·cosR2, cosR3·sinR2, sinR3)`), and the drawn
placement is `{ p with r2 = beamR2 + p.r2; r3 = beamR3 + p.r3 }`. So at dialled 0 the detector faces the
reflected beam; rotating the mirror's R2 or R3 re-aims the beam and the detector follows; rotating the
mirror's R1 leaves the beam (and the detector) unchanged; and the detector's OWN dialled R2/R3 tilt it
relative to the beam, not the table. The source and the mirror keep their absolute orientation (the mirror's
R2 = 45° DEFINES the reflection).

## Tests

`OpticalConstructor.Ui.Tests` **210/210**:
- renderer: rails default 72 + snap-to-preset + slider-index; the seed has a convex & a concave lens and a
  concave & a convex mirror (both cap signs).
- snap-to-reflected (+4): the detector faces the reflected beam at rest; rotating the mirror R2 keeps it
  facing the new beam; mirror R1 leaves it unchanged; the detector's own dialled R2 tilts it 30° relative to
  the beam.
- `RayPositionControls` (+3): the 0.05 / 0.20 step, `parseMeters`, and a headless proof that
  `ElementMovementView` mounts the SHARED control (by its automation ids).

## Files

- New: `OpticalConstructor.Controls/RayPositionControls.fs`; `OpticalConstructor.Ui.Tests/RayPositionControlsTests.fs`.
- Changed: `TableRotationView.fs` / `ElementRotationView.fs` / `ElementMovementView.fs` / `SnapToBeamView.fs`
  (→ `TableScene`; movement → `RayPositionControls`); `RendererTestView.fs` (discrete rails, curved
  spherical caps, in-box fit, yellow roll-rail); `SnapToReflectedView.fs` (beam-relative downstream
  orientation); the `Controls` / `Ui.Tests` `.fsproj`s; `RendererTestTests.fs` / `SnapToReflectedTests.fs` /
  `TableRotationTests.fs` / `ElementMovementTests.fs` (test updates).
