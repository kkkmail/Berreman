# 002 — impl-log: Ray model, snapping & schematic tracing (Part B)

## Progress

- [x] RayModel.fs — constants, Ray/RayBundle, ray groups, Snell tracing, snap chain, default placement, orphaning
- [x] Domain fsproj — register RayModel.fs after Placement.fs
- [x] RayModelTests.fs — AC-B1..AC-B6 (6 tests)
- [x] Tests fsproj — register RayModelTests.fs after PlacementTests.fs
- [x] gates green (build, unit-tests, constructor-unit-tests)
- [x] cycle-2 retry: enforce the mirror RRG-only invariant STRUCTURALLY in
  `outgoingGroups` + pin it with a toggled-emission AC-B2 assertion (gates re-run green)
- [x] cycle-3 retry (FINAL): make the mirror arm UNCONDITIONAL (`-> [ Reflected ]`)
  so an `EmitTransmittedOnly` mirror still draws an RRG (not `[]`) + pin that corner
  with a second AC-B2 assertion (gates re-run green)

## Files modified

New:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/RayModel.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/RayModelTests.fs`

Edited:
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Decisions / issues encountered

- **Positions reuse `Placement.Vector3` (components = canonical meters), distances
  are `float<meter>`.** The eight-ray circle tips out of the table plane (its
  cross-section has a +Z component), so the 2-D `TablePoint` is insufficient for ray
  geometry. Rather than introduce a parallel 3-D point type, RayModel reuses the
  slice-001 `Vector3` for positions and directions and documents its components as
  canonical SI meters (constraint 0.2). Along-ray distances and the named distance
  constants stay `float<meter>`; the single float↔meter casts (`/ 1.0<meter>`,
  `* 1.0<meter>`) are confined to the position↔distance boundary helpers. No unit
  conversion is hand-rolled — `Units.fs` remains the sole conversion seam.
- **The face normal reuses `Placement.r1Axis` rather than re-deriving the R2/R3
  law.** `faceNormal incoming p` takes the rest-frame oriented N1 (which already
  folds in R2 about the table normal and R3 out of plane, slice 001) and re-bases it
  onto the actual incident ray by rotating about the table normal. For an in-plane
  ray at angle φ this yields a face normal at φ + R2 — exactly the additive
  `tableFrameOrientation` law (A.4.3), so the rotation convention is defined once.
- **Schematic transmission deviates at a single interface (B.6 / 0.5).** The drawn
  transmitted ray refracts once (n = 1.0 → 1.5) so a tilted element visibly bends
  the CR (B.6.1 — "the central and side rays genuinely change direction"); the exit
  refraction is omitted as a schematic simplification (constraint 0.5 — schematic,
  not physical-grade). At normal incidence both reflect and refract return the
  incoming direction, so a square element does not steer a normal beam. A flat
  mirror's reflected ray (the AC-B3 driver) reflects off the same face normal.
- **Snapping is a polyline walk by along-ray gap, never by absolute position.**
  `snapChain` advances each element by its fixed `gap` along the current direction
  and lets the tracked branch set the direction onward. Distance preservation
  (B.3.2 / AC-B3) and the light-source drag/reset (B.4.2 / AC-B4) then both hold by
  construction: re-aiming an upstream segment or moving the source re-walks the same
  gaps, so inter-element spacing is invariant.
- **`removeRay` is the SOLE orphaning operation (B.5.1 / B.5.2).** Both spec
  triggers — switching the tracked branch (T→R) and removing one in-use ray —
  reduce to "a ray is gone, orphan its elements"; every other edit
  (`withPlacement`, `moveSource`, `snap`) preserves attachment. `restoreRay`
  re-snaps an orphan iff the restored ray's perpendicular distance to its placement
  point is ≤ `resnapDistance` (named, 0.02 m); `restoreRayWithin` takes the distance
  so it is also configurable (Part E).
- **Table-frame origin assumed at the table centre (+X right).** B.4.1 says "left"
  / "right" / "vertically centred" without pinning the origin; the table type
  arrives in Part C / slice 004. I placed the source at (−1 m, 0) and the detector
  at (+1 m, 0) so the default CR spans exactly 2.0 m centred on the table. Recorded
  here per "Don't ask the user".

### Cycle 2 (retry) — mirror RRG-only invariant made structural

The code-judge routed cycle 1 back on one finding (B.2.1 / B.7.1): `outgoingGroups`
routed `FlatMirror | CurvedMirror` through `emittedGroups p.emission`, so a mirror
whose `emission` was toggled to `EmitBoth` (`Emission.withTransmitted true`, a
representable state on the plain `ElementPlacement` record, Placement.fs:134-138)
would draw a TRG — exactly the branch the engine's `BeamNode.attach` refuses with
`Error MirrorBranchMustBeReflected` (BeamTree.fs:74-77). The fix is two edits, both
in this slice's owned files:

- **`RayModel.fs` `outgoingGroups`:** split the mirror kinds into their own match
  arm and intersect their emitted groups with `[ Reflected ]`
  (`emittedGroups p.emission |> List.filter (fun g -> g = Reflected)`). The mirror
  kind-invariant is now enforced STRUCTURALLY by the draw layer — independent of the
  per-element emission toggle — mirroring `attach`. A mirror can never draw a TRG in
  any emission state. Non-mirror kinds are unchanged (still follow A.7 emission).
- **`RayModelTests.fs` AC-B2:** added an assertion that a mirror with its emission
  forced to `EmitBoth` (precondition `Assert.Equal(EmitBoth, …)` confirms the
  phantom-TRG state IS representable) still yields an RRG and no TRG, and folded
  `mirrorBothGroups` into the engine-branch-correspondence and never-Transmitted
  loops. This pins the invariant the default-only mirror could not exercise.

Per the retry hint, the reuse critic's F1/F2 (`rotateAboutTableNormal` /
`add`/`sub`/`scale` hoists onto `Placement.fs`) are OUT OF SCOPE for this slice
(`Placement.fs` is "Edited: none") and are deliberately left untouched here. The
constructor-test count is unchanged at 218 — the new assertions strengthen the
existing AC-B2 test rather than adding a method.

### Cycle 3 (FINAL retry) — mirror RRG-only made fully UNCONDITIONAL

The cycle-2 fix closed the phantom-TRG hole (an `EmitBoth` mirror) but chose the
*intersect/filter* example form — `emittedGroups p.emission |> List.filter (fun g ->
g = Reflected)`. The code-judge (`002-03-code-judge.md`) and the architecture critic
routed it back on the MIRROR-IMAGE corner: `Emission.withReflected false` drives any
emission to `EmitTransmittedOnly` (a representable state on the plain
`ElementPlacement` record, Placement.fs:126-130), and on that state the filter
returns `[]` — a mirror that draws NEITHER an RRG nor a TRG. B.2.1 says "a mirror
produces only an RRG", i.e. it *produces* an RRG unconditionally, so silently
dropping the mandatory reflected branch is the same B.2.1 / B.7.1 divergence,
mirrored (suppressing the mandatory branch instead of inventing a forbidden one).
The fix is two edits, both in this slice's owned files (`Placement.fs` untouched):

- **`RayModel.fs` `outgoingGroups`:** replace the mirror arm's filter form with the
  unconditional structural form `| FlatMirror | CurvedMirror -> [ Reflected ]`. The
  mirror's RRG is now genuinely independent of `p.emission` (the cycle-2 goal the
  filter form missed in one corner), one token shorter, and matches the engine where
  `BeamNode.attach` takes a `Reflected` attach on a mirror UNCONDITIONALLY
  (BeamTree.fs:74-77). The draw layer can now neither invent the forbidden
  `Transmitted` branch nor suppress the mandatory `Reflected` one in ANY emission
  state. The doc-comment was rewritten to describe the unconditional rule and both
  guarded corners (`EmitBoth` → no TRG; `EmitTransmittedOnly` → still an RRG).
- **`RayModelTests.fs` AC-B2:** added a second assertion that a mirror with emission
  forced to `EmitTransmittedOnly` (precondition `Assert.Equal(EmitTransmittedOnly, …)`
  confirms the RRG-suppressing state IS representable) still yields exactly
  `[ Reflected ]` — `Assert.NotEmpty`, `Assert.Equal(1, List.length …)`,
  `Assert.Equal(Reflected, List.head …)`, `Assert.DoesNotContain(Transmitted, …)`.
  Under the old filter form this list was `[]`, so every one of these assertions
  fails — the assertion names the bug (TDD discipline). `mirrorTransmittedGroups` is
  also folded into the engine-branch-correspondence and never-Transmitted loops.

The constructor-test count is again unchanged at 218 — the new assertions strengthen
the existing AC-B2 `[<Fact>]` rather than adding a method. The reuse critic's F1/F2
stay deferred (out-of-scope `Placement.fs` edits), per the retry hint.

## Testing state

All five applicable gates pass locally, re-run after the cycle-3 fix (logs under
`C:\GitHub\Berreman\specs\0026\.artifacts\`):

- `build` — exit 0, "Build succeeded.", 0 Error(s), 0 lowercase `error` matches
  (the gate's `stdout_match` veto). Log `002-build.log`.
- `unit-tests` — exit 0, Passed 84 / Skipped 5 / Total 89; `BerremanTests`
  untouched. Baseline `berreman_unit_tests = 84` held. Log `002-unit-tests.log`.
- `constructor-unit-tests` — exit 0, Passed 218 / Failed 0 / Total 218 (212 prior +
  6 `RayModelTests`; cycles 2 and 3 both added assertions INSIDE AC-B2, so the test
  COUNT is unchanged at 218). Baseline `constructor_unit_tests` 212 → 218.
  Log `002-constructor-unit-tests.log`.
- `impl-log-structure`, `state-of-world-structure` — built-in structural checks;
  this impl-log and the sibling state-of-the-world carry all required ATX headings.

Coverage: AC-B1 (eight-ray 45° circle, radius 0.4×face/2, CR centred), AC-B2
(mirror RRG-only — for the DEFAULT mirror, a mirror toggled to `EmitBoth` (no phantom
TRG), AND a mirror toggled to `EmitTransmittedOnly` (still a non-empty RRG, not `[]`),
so the invariant is fully structural/unconditional in emission — / non-mirror both /
detector terminates / engine-branch correspondence), AC-B3 (distance-preserving downstream travel under an upstream R2
change), AC-B4 (default 2.0 m LS/D placement + light-source reset re-snap), AC-B5
(orphaning: remove orphans exactly its elements, ordinary edits never orphan,
restore re-snaps only within `resnapDistance`), AC-B6 (Snell n=1.0/1.5 bend, normal
incidence passes straight). None deferred.

## Artifacts

- `002-build.log`, `002-unit-tests.log`, `002-constructor-unit-tests.log`
