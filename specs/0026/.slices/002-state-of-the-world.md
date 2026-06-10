# 002 — State of the world: Ray model, snapping & schematic tracing

## Where we are

Slice 002 is the second CRITICAL, land-first slice of Spec 0026 (Optical
Constructor UI): it realises **Part B**, the pure, headless-testable ray-model
layer that draws the light path. It adds a new
`OpticalConstructor.Domain/RayModel.fs` module on top of slice 001's `Placement.fs`
(element geometry) and the existing `BeamTree.fs` (engine topology), reusing both
rather than re-typing geometry. It carries NO Avalonia type and introduces NO
drawing weights/colours (Part C / slice 004) and NO interaction gestures (Part E /
slice 005); it exposes pure geometry only. Slices 004 and 005 consume the
`RayBundle` / `RayGroupKind` / `SnappedElement` / `RayChain` / orphaning surface and
the named constants defined here.

## What's working

- Add `RayModel.fs`: a ray group as one central ray plus eight side rays, the
  45°-spaced "rifle-target" circle of radius `0.4 × face_extent/2` at normal
  incidence (`bundleAt`, AC-B1).
- Name the incident / reflected / transmitted ray groups, follow each element's A.7
  emission metadata, terminate the ray at a detector, and map each drawn group to
  its engine `Reflected`/`Transmitted` branch (`outgoingGroups`, `toBeamBranch`,
  AC-B2). Enforce the mirror RRG-only invariant STRUCTURALLY and UNCONDITIONALLY (the
  mirror arm returns `[ Reflected ]` independent of `p.emission`), so the draw layer
  can neither invent the transmitted branch the engine's `BeamNode.attach` refuses nor
  suppress the mandatory reflected branch in any emission state (B.2.1 / B.7.1).
- Snap elements onto the central ray as a polyline walk by along-ray gap, so tilting
  an upstream element re-aims everything downstream while preserving every
  inter-element distance (`snapChain`, AC-B3).
- Place the light source left-middle and the detector right-middle exactly 2.0 m
  apart along the CR, with a light-source move/reset that re-snaps the chain
  preserving distances (`defaultSourcePoint`/`defaultDetectorPoint`/`resetSource`,
  AC-B4).
- Add the orphaning state machine: ray removal/branch-switch orphans exactly its
  elements, ordinary edits never orphan, and a restored ray re-snaps an orphan only
  within the named `resnapDistance` (`removeRay`/`restoreRay`, AC-B5).
- Trace rays schematically through tilted elements via vector reflection and Snell
  refraction with named `n = 1.0` / `n = 1.5` constants (`reflect`/`refract`/
  `outgoingDirection`, AC-B6). Add `RayModelTests.fs` (6 headless tests, AC-B1..B6).

## Tests

- `build` — PASS (exit 0, "Build succeeded.", 0 Error(s); no lowercase `error`).
  Log `.artifacts/002-build.log`.
- `unit-tests` — PASS (exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched). Baseline `berreman_unit_tests = 84` held. Log
  `.artifacts/002-unit-tests.log`.
- `constructor-unit-tests` — PASS (exit 0, Passed 218 / Total 218; +6 new
  `RayModelTests`). Baseline 212 → 218. Log
  `.artifacts/002-constructor-unit-tests.log`.
- `impl-log-structure`, `state-of-world-structure` — PASS (required ATX headings
  present).

Coverage: AC-B1 eight-ray 45° circle; AC-B2 mirror RRG-only (default mirror, an
`EmitBoth`-toggled mirror with no phantom TRG, AND an `EmitTransmittedOnly`-toggled
mirror that still draws a non-empty RRG — the invariant is unconditional in emission)
+ detector termination + engine-branch correspondence; AC-B3 distance-preserving
downstream travel; AC-B4
default 2.0 m placement + light-source reset; AC-B5 orphaning rules; AC-B6 Snell
n=1/1.5 bending. None deferred.

```yaml
gates:
  berreman_unit_tests:    84
  constructor_unit_tests: 218
```

## Architecture

- **RayModel is a pure geometry layer above Placement and BeamTree, not a parallel
  engine (0.1 / 0.7).** It reuses `Placement.Vector3` for directions and positions,
  `Placement.r1Axis` for the element face normal, and the engine
  `BeamTree.BeamBranch` (`Reflected`/`Transmitted`) for the branch each drawn group
  corresponds to. The quantitative result still comes from `BeamTree.solve` — this
  is the visual layer only (B.0). No Avalonia type appears in the module (0.3).
- **The mirror RRG-only rule is a STRUCTURAL, UNCONDITIONAL kind-invariant, not a
  soft emission toggle (B.2.1 / B.7.1).** `outgoingGroups` gives `FlatMirror |
  CurvedMirror` their own match arm that returns `[ Reflected ]` independent of
  `p.emission` entirely. So a mirror draws an RRG and only an RRG in ANY emission
  state: no TRG even when toggled to `EmitBoth` (`Emission.withTransmitted true`), and
  never the empty set even when toggled to `EmitTransmittedOnly`
  (`Emission.withReflected false`) — both representable on the plain `ElementPlacement`
  record. This mirrors the engine, where `BeamNode.attach` takes a `Reflected` attach
  on a mirror unconditionally and rejects a `Transmitted` one with
  `Error MirrorBranchMustBeReflected` (BeamTree.fs:74-77); the draw layer can thus
  neither invent a branch the engine refuses to route nor suppress the mandatory
  reflected one B.2.1 requires. Non-mirror kinds still follow their emission metadata
  unchanged. (An earlier intersect/filter form, `emittedGroups p.emission |>
  List.filter (= Reflected)`, drew `[]` for an `EmitTransmittedOnly` mirror — the
  unconditional arm removes that corner.)
- **Positions are `Vector3` in canonical meters; along-ray distances are
  `float<meter>`.** The eight-ray circle tips out of the table plane, so the 2-D
  `TablePoint` cannot hold ray geometry; reusing `Vector3` (documented as SI meters)
  avoids a parallel point type. The only float↔meter casts sit in the
  position↔distance boundary helpers; `Units.fs` stays the sole conversion seam
  (0.2).
- **The face normal re-bases `Placement.r1Axis` onto the incident ray.** R2 is
  measured from the incident ray (A.4.3): `faceNormal` rotates the rest-frame N1
  about the table normal by the incident-ray angle, reproducing the additive
  `tableFrameOrientation` law and tipping out of plane under R3. The rotation
  convention lives once, in Placement.
- **Snapping is a gap-indexed polyline walk.** `snapChain` advances by fixed
  along-ray gaps and lets the tracked branch set each onward direction, so
  distance-preserving downstream travel (B.3.2) and the light-source drag/reset
  (B.4.2) both hold by construction rather than by special-casing.
- **`removeRay` is the single orphaning operation.** Both spec triggers (branch
  switch, ray removal) reduce to it; restore re-snaps an orphan iff its perpendicular
  distance to the ray ≤ `resnapDistance`. `restoreRayWithin` parameterises the
  distance so it is configurable; `resnapDistance` is the named default (0.02 m).
- **Schematic transmission deviates at one interface.** A tilted element refracts
  the drawn transmitted ray once (n=1.0→1.5) so the CR visibly bends (B.6.1); the
  exit refraction is intentionally omitted (schematic, not physical-grade, 0.5).

## Deferred

- Drawing weights / colours and the visual distinction between reflected and
  transmitted groups (B.6.2) — Part C / slice 004 consumes `RayGroupKind` and
  `SnappedElement` and decides the styling.
- Interaction gestures that wire rotation/move/reset and branch-switch/ray-removal
  to re-snapping — Part E / slice 005 drives `moveSource`/`resetSource`/`removeRay`/
  `restoreRay` and the `RayChain`.
- Side-ray spreading through focusing elements (lens, curved mirror, prism, wedge)
  is a drawing concern (B.1.1 "MAY visibly spread"); the bundle here keeps side rays
  parallel to the CR. The negative constraint (no spread for flat elements) holds.
- The concrete table-frame origin / extents — Part C / slice 004 (`Table` type). The
  default LS/D points assume a table-centred origin and are left stable for that
  edit.

## Gotchas

- **`Vector3` components are canonical SI meters when used as a position** (the type
  itself is unit-free, carrying a direction in Placement). Distances are
  `float<meter>`; mixing them needs the `pointToVector3` / `* 1.0<meter>` boundary
  helpers, not ad-hoc arithmetic.
- **A flat mirror at normal incidence (R2 = 0) retro-reflects (−X).** The face
  normal faces the beam squarely, so `reflect` sends the ray straight back; a useful
  steer needs R2 ≠ 0 (e.g. R2 = 45° turns the beam 90°). AC-B3 drives the chain with
  a tilted mirror for this reason.
- **`RaySegmentSpec.branch` is independent of an element's emission.** It names which
  outgoing ray the SNAP CHAIN follows past the element (the tracked ray), not which
  groups the element draws; `outgoingGroups` (driven by emission) governs the latter.
- **Table-frame origin is assumed at the table centre.** The default source/detector
  x-coordinates (−1 m / +1 m) depend on this; slice 004's `Table` may re-anchor them.
- **`withR2` respects the R2 lock** (inherited from Placement): `r2Locked` defaults
  false so it applies, but a locked element silently ignores the change — match the
  slice-001 lock semantics when wiring Part E.

## Changelog

- 2026-06-05 (slice 002): Land Part B — the ray-model / snapping / schematic-tracing
  geometry. New `RayModel.fs` (central + eight side rays, IRG/RRG/TRG groups + engine
  correspondence, detector termination, gap-indexed snap chain with
  distance-preserving downstream travel, default 2.0 m LS/D placement + light-source
  reset, orphaning state machine with `resnapDistance`, vector reflection + Snell
  refraction with n=1.0/1.5). Add `RayModelTests.fs` (6 tests, AC-B1..B6). Build
  green at Release/x64; all gates pass (constructor-unit-tests 218, berreman_unit_tests
  84 held).
- 2026-06-05 (slice 002, cycle 2): Enforce the mirror RRG-only kind-invariant
  STRUCTURALLY in `outgoingGroups` — intersect a mirror's A.7 emission with
  `[ Reflected ]` so a mirror toggled to `EmitBoth` still draws no TRG, matching the
  engine's `BeamNode.attach` `MirrorBranchMustBeReflected` rule (B.2.1 / B.7.1).
  Pin it with a toggled-emission AC-B2 assertion. Both edits stay within the slice's
  owned files (`RayModel.fs`, `RayModelTests.fs`); reuse-critic F1/F2 deferred as
  out-of-scope `Placement.fs` edits. All gates re-run green (constructor-unit-tests
  218, berreman_unit_tests 84 held).
- 2026-06-05 (slice 002, cycle 3, FINAL): Make the mirror arm of `outgoingGroups`
  UNCONDITIONAL — `| FlatMirror | CurvedMirror -> [ Reflected ]` — replacing the
  cycle-2 intersect/filter form, which returned `[]` for an `EmitTransmittedOnly`
  mirror (`Emission.withReflected false`) and so SUPPRESSED the mandatory RRG B.2.1
  requires. The arm is now genuinely independent of `p.emission` and matches the
  engine's unconditional `Reflected` attach (BeamTree.fs:74-77). Pin the corner with a
  second AC-B2 assertion that an `EmitTransmittedOnly` mirror still yields exactly a
  non-empty `[ Reflected ]`. Both edits stay within the slice's owned files;
  `Placement.fs` untouched (reuse-critic F1/F2 stay deferred). Test count unchanged at
  218; all gates re-run green (berreman_unit_tests 84 held).
