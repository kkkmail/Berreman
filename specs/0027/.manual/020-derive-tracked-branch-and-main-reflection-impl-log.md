# 0027-020 — Derive the tracked beam (R/T) from the element, and reflect on the Main screen (impl log)

Follow-up to `018`. The branch a snap chain follows must be DERIVED from the element (a flat mirror tracks
REFLECTED light, a polarizer / sample / lens TRANSMITTED), not hardcoded — and the Main screen must use the
same approach so adding a mirror reflects the beam. Full solution builds clean; `OpticalConstructor.Ui.Tests`
**222/222**, `OpticalConstructor.Tests` **245/245**.

## The tracked branch is domain info (A.7), not a guess

The domain already models "tracks R / T / both": every `ElementPlacement` carries an `Emission`
(`EmitReflectedOnly | EmitTransmittedOnly | EmitBoth`), `defaultEmission` makes mirrors `EmitReflectedOnly`,
and `RayModel.outgoingGroups` already encodes the kind invariant (mirror → `[Reflected]`, detector → `[]`,
others → their emission). New **`RayModel.primaryBranch : ElementPlacement -> BeamTree.BeamBranch`** picks
the SINGLE branch the linear chain follows from `outgoingGroups`: transmitted if it emits a transmitted
group, else reflected (a mirror), else transmitted (the terminal detector). So a snap chain "figures out"
from a flat mirror's emission that it must reflect.

## The snap-to-reflected test now derives the branch

`SnapToReflectedView.snapped` was hardcoding `if i = 0 then Reflected else Transmitted`. It now sets each
stop's `branch = RayModel.primaryBranch placement` — so the mirror reflects and the detector transmits
because of WHAT THEY ARE, not their position. `reflectorIndex` is likewise derived (the first element whose
`primaryBranch` is `Reflected`) rather than the constant `1`. Behaviour is identical (the existing reflected
/ R1-invariance / beam-relative tests still pass); a new test asserts the derivation directly
(`primaryBranch` of a flat / curved mirror is `Reflected`, of a polarizer / source / detector `Transmitted`,
and the scene's reflector is found at the mirror).

## The Main screen now snaps the beam (reflects at mirrors)

The Main scene drew a straight source→detector ray regardless of the elements, so adding a mirror did
nothing. It now SNAPS, following the same approach:

- A `snapChain` flag on the model (`true` only for `initMain`; the static test windows stay free-placement
  and unchanged). 
- `mainSnap` orders the non-source elements along the beam (by their along-beam x), takes consecutive
  x-distances as the gaps, derives each element's branch with `primaryBranch`, and runs
  `RayModel.snapChain` — returning each element's drawn position by index plus the beam polyline. A flat
  mirror reflects, so everything downstream snaps onto the reflected segment.
- `selectionAt` (shared) now hits-tests against the DRAWN (snapped) positions, so clicking an element on the
  bent beam selects it; for the free test scene the positions are unchanged, so its tests are unaffected.
- `mainTableViews` draws the bent beam polyline (no more straight ray / separate source-detector markers —
  the source and detector are elements and draw themselves); `mainElementViews` draws each element at its
  snapped centre through the shared renderer.
- Rotating the source (its R2) re-aims the beam and the whole chain re-snaps; rotating a mirror re-aims its
  reflection. The Move bay slides a selected element's along-beam position (its gap).

**Downstream elements AUTO-ORIENT to the beam (as in the snap-to-reflected test).** A transmissive element
(polarizer / sample / lens / detector) is drawn facing its incoming beam: `RayModel.beamOrientation` (the
absolute R2/R3 that point N1 along a direction — promoted from the snap-to-reflected view into the shared
domain) plus the element's OWN dialled R2/R3. So at dialled 0 it is perpendicular to the (possibly
reflected) beam, and a dialled value tilts it relative to the beam. The SOURCE (it emits along its own aim)
and MIRRORS (their orientation defines the reflection) keep their own orientation — `autoOrientsToBeam`
encodes this. The drawn placement is the public `drawnPlacement`.

New tests: on the Main screen, adding a flat mirror tilted to R2 = 45° puts the downstream detector OFF the
straight ray (the beam reflected) while a polarizer keeps it straight; and the detector downstream of that
mirror is drawn FACING the reflected beam (its N1 = the incoming direction), with the source / mirror not
auto-oriented.

## Follow-up fix — out-of-plane (R3) snapping was flattened to the table

Reported: with the mirror's R3 changed, downstream orientation was right but the PLACEMENT z (perpendicular
to the table) never changed. `RayModel.snapChain` does produce a full 3-D position (an R3-tilted mirror
reflects the beam OUT of the table plane), but the Main drawing stuffed that position into the 2-D
`placementPoint` (x, y only), so z was dropped and the elements stayed flat — while the snap-to-reflected
test draws at the full 3-D snapped position. Fix: `ElementRenderer.Drawable` now carries an explicit 3-D
`centre : Vector3` (with `centreOfPlacement` = the 2-D point at z = 0 for on-table elements), and every
renderer helper offsets from that centre's x/y/**z**. The renderer test passes `centreOfPlacement` (z = 0,
unchanged); the Main screen passes the full snapped `node.position`. So an element snapped off the plane is
drawn off the plane (revealed when the table view is tilted — at top-down the projection drops z, as it
must). New test: tilting a mirror's R3 (after unlocking it) snaps the downstream detector to a non-zero z;
an in-plane (R2-only) reflection keeps z = 0. `OpticalConstructor.Ui.Tests` **224/224**.

## Files

- Changed: `OpticalConstructor.Domain/RayModel.fs` (`primaryBranch`); `SnapToReflectedView.fs` (derive the
  branch + reflector); `TableAndElementRotationView.fs` (`snapChain` flag, `mainSnap` / `snappedCentres`,
  snapped selection + Main drawing); `SnapToReflectedTests.fs` (+1), `TableAndElementRotationTests.fs` (+1).
