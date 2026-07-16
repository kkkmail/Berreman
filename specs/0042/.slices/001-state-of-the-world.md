# State of the world — spec 0042, slice 001 (IMPLEMENT)

## Where we are

Slice 001 is the first slice of arc 0042. It seeds the Mueller-reconstruction
groundwork: a pure `MuellerReconstruction` domain module carrying the elevated
`Retardance` phase-delay type and an ideal-retarder Mueller-matrix builder,
`retarderMueller`, composed entirely from the existing `Propagation` seams. No
prior slice code exists to build on; later slices extend the reconstruction
pipeline on top of this module.

## What's working

- Add `OpticalConstructor.Domain.MuellerReconstruction` with the elevated `Retardance`
  DU (`.value`, `.degrees`, `static member degree`) — a phase delay kept distinct
  from the azimuth `Angle`.
- Add `retarderMueller (axis) (retardance)` building an ideal retarder's Mueller
  matrix via the existing `Propagation.muellerOfRows` + `rotateMueller` seams — no
  new 4×4 algebra.
- Wire the module in last in `OpticalConstructor.Domain.fsproj`; add the explicit
  Domain `ProjectReference` and `MuellerReconstructionTests.fs` to `BerremanTests`.
- Add 5 BerremanTests facts covering the zero-retardance identity, the quarter-wave
  QWP form at azimuth 0° and 45°, and the `Retardance` degrees/value round-trip.

## Tests

Gate roster for this slice (from `001.gates`): `build`, `unit-tests`
(`berreman_unit_tests`), `constructor-unit-tests` (`constructor_unit_tests`).

Per the `IMPLEMENT` worker's **Invariant 6 (act only; run no checks)**, this worker
did NOT execute the gates — the arc-runner gate engine runs them after exit. New
coverage authored in `BerremanTests/MuellerReconstructionTests.fs`:

- zero retardance ⇒ `Propagation.identityMueller` (arbitrary azimuth, and azimuth 0);
- quarter-wave (d = 90°) at azimuth 0° ⇒ standard QWP form `[[1,0,0,0],[0,1,0,0],[0,0,0,1],[0,0,-1,0]]`;
- quarter-wave at azimuth 45° ⇒ standard rotated QWP form (exercises `rotateMueller`);
- `Retardance` degrees/value round-trip.

The two acceptance identities are directly asserted; sign convention was
hand-verified (residuals ~1e-16, well within `allowedDiff` = 1e-5).

```yaml
gates:
  berreman_unit_tests:  0
  constructor_unit_tests: 0
```

(First slice of the arc — 0 is the valid zero baseline; the gate engine records the
real captured counts when it runs the gates post-exit.)

## Architecture

- **Elevated `Retardance`.** A retarder's phase delay is its own single-case DU, not a
  bare `float` and not overloaded onto the azimuth `Angle`. This follows the repo's
  "elevate every primitive" discipline and makes an azimuth/phase mix-up a type error.
- **Reuse over reinvention.** `retarderMueller` is expressed purely as
  `rotateMueller axis (muellerOfRows core)`, reusing the existing Stokes/Mueller
  rotation seam (R(−θ)·M·R(θ)) rather than introducing a second rotation implementation.
  The diagonal retarder core is the only new data.
- **Compile-order placement.** The module compiles last in the Domain project (after
  `ExperimentDataLoad.fs`) so `Propagation`, `Fields`, `Geometry`, and `Experiments`
  are all in scope; nothing else in the Domain depends on it.

## Deferred

- The broader Mueller-reconstruction pipeline (e.g. fitting/reconstruction of a
  sample's Mueller matrix from measured data, other optical elements beyond the ideal
  retarder) is out of this slice's scope and left for later slices.
- Circular/elliptical or lossy (diattenuating) retarder variants are not modelled — only
  the ideal (lossless) linear retarder specified by the slice.

## Gotchas

- **Invariant 6 vs. base step 5.** The base protocol's "run gates locally" step is
  explicitly superseded by the `IMPLEMENT` delta's Invariant 6; this worker ran no
  build/test. Hence the zero baseline in the `gates:` block above.
- **Sign convention.** `retarderMueller` inherits `rotateMueller`'s R(−θ)·M·R(θ)
  convention; the QWP acceptance forms were hand-checked against it (azimuth 0 and 45°).
- **`Propagation` in scope unqualified** because `MuellerReconstruction` shares the
  `OpticalConstructor.Domain` namespace and compiles after `Propagation.fs`.
- **Explicit Domain reference is intentional.** `BerremanTests` already reached Domain
  transitively through `OpticalConstructor.Optimization`; the explicit `ProjectReference`
  is mandated by the slice and dedupes by path (no MSB3277 conflict).

## Changelog

- 2026-07-15 — slice 001 (IMPLEMENT): add `MuellerReconstruction` module (`Retardance`,
  `retarderMueller`) to `OpticalConstructor.Domain`; wire Domain into `BerremanTests`
  with an explicit `ProjectReference`; add `MuellerReconstructionTests.fs` (zero-retardance
  identity + QWP acceptance at azimuth 0°/45° + `Retardance` round-trip).
