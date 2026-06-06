# Reuse critique -- 002.slice-md cycle 3

## Coverage

- Helper roots walked: `C:\GitHub\Berreman` (reachable reuse range only: the
  `Berreman/` engine core + `OpticalConstructor.Domain`; a new `Domain` module
  cannot depend on `.Ui`/`.Storage`/`.Optimization`/`.Tests-of-other-projects`,
  so those layers are out of reuse range for `RayModel.fs`).
- Files inspected this cycle: `RayModel.fs`, `RayModelTests.fs`, `Placement.fs`,
  `BeamTree.fs` (branch/attach region), `Geometry.fs` (engine vector/rotation
  surface), plus `PlacementTests.fs`/`SourceProjectionTests.fs` for the test
  precedent; two definition sweeps (`reflect|refract|[Ss]nell` across 57 `.fs`,
  `tol|close|eps|distV|approx` across the test dir). `max_files` (200) not reached.
- Extensions: walked `.fs`, not the configured `.py,.md,.json` (a pure-F# repo
  has no reuse surface in those extensions). Same defensible substitution recorded
  in cycles 1 and 2.

## Findings

**The cycle-3 delta is reuse-clean.** The only code change since cycle 2 is the
FINAL judge-directed fix: the mirror arm of `outgoingGroups` became the
unconditional `| FlatMirror | CurvedMirror -> [ Reflected ]` (`RayModel.fs:187`),
replacing the cycle-2 `emittedGroups p.emission |> List.filter (= Reflected)`
intersect form, plus a new AC-B2 assertion that an `EmitTransmittedOnly` mirror
still yields exactly `[ Reflected ]` (`RayModelTests.fs:86-92`). That assertion
reuses the existing `Emission.withReflected` setter (`Placement.fs:126-130`)
correctly. The change removes code and introduces no duplication. F1 and F2 below
are **carried forward from cycles 1-2** -- I re-verified both against current
source and both remain accurate and unchanged -- and F3 is a **new, in-scope**
near-miss this critic surfaces for the first time.

### F1: `rotateAboutTableNormal` re-derives the private `Placement.rotateAbout`

- **Worker added:** `RayModel.rotateAboutTableNormal` (`RayModel.fs:98-101`), a
  hand-rolled in-plane rotation about +Z consumed only by `faceNormal`
  (`RayModel.fs:209-210`).
- **Existing helper:** `Placement.rotateAbout (axis) (angle) (v)` -- the project's
  one Rodrigues rotation primitive, at `Placement.fs:239-249`. Substituting
  `k = (0,0,1)` gives `kxv = (-v.y, v.x, 0)`, `kv = v.z`, so the Rodrigues body
  reduces term-for-term to `(v.x·c - v.y·s, v.x·s + v.y·c, v.z)` -- exactly the
  worker's formula. `rotateAboutTableNormal a v` *is* `rotateAbout tableNormal a v`.
- **Why it matters:** a near-miss duplication of the project's single rotation
  convention on the shared `Placement.Vector3` type. The slice's own SoW asserts
  "the rotation convention lives once, in Placement"
  (`002-state-of-the-world.md`, `faceNormal` bullet), which a second primitive
  contradicts: a future handedness/sign fix to `rotateAbout` would silently not
  reach `RayModel`. The only thing blocking direct reuse is the `private` modifier
  on `rotateAbout` -- the helper exists, it is merely hidden.
- **Scope status:** the cycle-1 judge deferred this and instructed "do NOT touch
  `Placement.fs`"; slice 002 scopes `Placement.fs` as "Edited: (none)" and the
  worker complied. Not actionable inside this slice's owned files.
- **Suggested action (advisory):** for the slice that owns/edits `Placement.fs`:
  drop `private` from `rotateAbout` (or expose a thin `rotateAboutTableNormal`
  beside it) and have `faceNormal` call it, deleting `RayModel.fs:98-101`.
  Cleanup-grade, not a re-spawn item. The judge decides routing.

### F2: `add` / `sub` / `scale` (and test `distV`) re-roll `Vector3` arithmetic

- **Worker added:** module-private `add`/`sub`/`scale` on `Vector3`
  (`RayModel.fs:62-69`) and the test's Euclidean `distV` (`RayModelTests.fs:26-30`),
  all open-coding component arithmetic on the shared vector record.
- **Existing helper:** `OpticalConstructor.Domain.Placement.Vector3`
  (`Placement.fs:25-47`) is the canonical pure 3-vector and already carries
  `create`/`dot`/`cross`/`norm`/`normalized` -- but (re-confirmed this cycle) still
  no `+`/`-`/scalar-`*`. `Placement.rotateAbout` itself open-codes the same
  `{x=…;y=…;z=…}` arithmetic inline (`Placement.fs:245-249`), and the SoW says
  slices 004/005 will also consume `Vector3` -- so the type is set to grow a third
  and fourth private copy of the same arithmetic.
- **Why it matters:** `Vector3` is the agreed reuse seam across slices
  001/002/004/005; letting each consumer privately re-implement add/sub/scale
  defeats the shared type and risks divergent edge handling. This is the rubric's
  near-miss class -- *the existing one could be extended*.
- **Scope status:** same as F1 -- the fix is an *extraction onto* `Placement.fs`,
  which the cycle-1 judge deferred as out of scope and graded cleanup. Note this is
  an extraction (extend the type), not a drop-in reuse of an existing member, so it
  is lower-confidence than F1.
- **Suggested action (advisory):** in the slice that owns `Placement.fs`, lift
  `add`/`sub`/`scale` onto `Vector3` as `(+)`/`(-)`/`( * )` members beside
  `dot`/`cross`; `distV` then becomes `(a - b).norm`. Cleanup, not a blocker.

### F3: `emittedGroups` re-decides what `Emission.emitsReflected`/`emitsTransmitted` already answer

- **Worker added:** `emittedGroups` (`RayModel.fs:163-167`), which re-matches all
  three `Emission` cases (`EmitReflectedOnly`/`EmitTransmittedOnly`/`EmitBoth`) to
  decide which of `Reflected`/`Transmitted` an element emits.
- **Existing helper:** `Emission.emitsReflected` and `Emission.emitsTransmitted`
  (`Placement.fs:113-122`) -- first-class predicates on the same `Emission` type
  that already answer exactly "does this emission emit the reflected / transmitted
  branch?" They are not theoretical: `PlacementTests.fs:92-104` already consumes
  them as the canonical reuse seam for this question.
- **Why it matters:** a near-miss -- the emission→{R,T} decision now lives in two
  places (the predicate pair and `emittedGroups`'s re-match). If the `Emission` DU
  ever grows a case or the predicate semantics shift, `emittedGroups` is a second
  site that must be kept in lockstep. `emittedGroups` could instead be built on the
  predicates: `[ if e.emitsReflected then Reflected; if e.emitsTransmitted then
  Transmitted ]`, which is behaviourally identical on all three current cases and
  preserves the `[Reflected; Transmitted]` order the non-mirror tests assume.
- **Scope status:** UNLIKE F1/F2, this fix is entirely within `RayModel.fs` (the
  worker's owned file) -- no `Placement.fs` edit needed, so it is in-scope and
  actionable. It is, however, the weakest finding here: the explicit DU match is
  defensibly clearer and gains F#'s exhaustiveness check, so reasonable authors
  differ on whether the predicate form is an improvement. Cleanup-grade at most.
- **Suggested action (advisory):** optionally rebuild `emittedGroups` on the
  existing predicates, or leave as-is and accept the explicit match. Not a blocker.

## Considered and explicitly NOT findings

- **Test `close`/`tol` (`RayModelTests.fs:22-23`).** These are not duplication --
  the established pattern in `OpticalConstructor.Tests` is a per-file private
  `tol`/`close`; `PlacementTests.fs:26-27` defines the byte-identical pair and
  `SourceProjectionTests.fs:21` its own `tol`. The shared `MatrixComparison.fs`
  lives in the unrelated `BerremanTests` project (no project reference; Math.NET
  matrix comparison, not scalar). The worker correctly *followed the local
  precedent*; this is pattern-consistent, not a reuse miss.
- **`RayGroupKind` vs engine `BeamTree.BeamBranch` (`BeamTree.fs:21`).**
  `RayGroupKind` adds `Incident`, which `BeamBranch` lacks; the worker bridges with
  `toBeamBranch` (`RayModel.fs:195-199`). Constraint 0.7 lists "the ray-group
  model" as net-new; the mapping is the correct reuse bridge, not a parallel branch
  type.
- **`reflect`/`refract`/`crossSectionBasis`/`distancePointToRay`/`inPlaneAngle`
  (`RayModel.fs:108,215,226,359`).** The sweep confirms no geometric
  `d − 2(d·n)n` / Snell-vector / orthonormal-cross-section / point-to-line helper
  exists in any reachable layer (engine `Geometry.fs` carries only Math.NET-backed
  `RealVector3` ops and no reflect/refract/Snell). Net-new per 0.7; correctly
  authored fresh. Reusing `Placement.Vector3` rather than the Math.NET `RealVector3`
  is the documented, serializability-driven choice (`Placement.fs:21-24`), not a
  miss.

## Bottom line

The cycle-3 delta -- the unconditional mirror arm plus its `EmitTransmittedOnly`
assertion -- removes code and adds no duplication, so it is reuse-clean. F1 and F2
remain accurate but require out-of-scope `Placement.fs` edits the prior judge
deferred, and F3 is a new but cleanup-grade in-scope near-miss; my read is that
none is re-spawn-grade, but the code judge owns that call.
