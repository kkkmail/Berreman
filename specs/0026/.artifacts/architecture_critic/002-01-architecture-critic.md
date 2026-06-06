# Architecture critique -- 002.slice-md cycle 3

## Summary

Cycle 3 makes the single edit the cycle-2 judge routed back for and nothing
else: the mirror arm of `outgoingGroups` is now the unconditional structural
form `| FlatMirror | CurvedMirror -> [ Reflected ]` (`RayModel.fs:187`),
replacing the cycle-2 `emittedGroups p.emission |> List.filter (= Reflected)`
that collapsed to `[]` for an `EmitTransmittedOnly` mirror. A second AC-B2
assertion pins that exact corner (`RayModelTests.fs:86-92`). The slice is clean,
correctly layered, reuse-disciplined, and the SoW is honest. I have no
re-spawn-grade finding; my notes are all forward guidance for slices 004/005.

## Consistency

**The route-back fix is implemented as the goal asked, not merely the example
means** (`RayModel.fs:184-189`). The mirror arm now precedes the emission-driven
arm and returns `[ Reflected ]` independent of `p.emission` entirely, so the
match is exhaustive over all eight `CatalogueKind` cases with no wildcard — a
future kind forces a compile error here rather than silently falling through.
This is genuinely "regardless of `p.emission`" (the cycle-1/2 stated goal) and
mirrors the engine, where `BeamNode.attach` takes a `Reflected` attach on a
mirror unconditionally and rejects a `Transmitted` one with
`MirrorBranchMustBeReflected` (`BeamTree.fs:74-77`). The draw layer can now
neither invent the forbidden transmitted branch (the cycle-1 hole) nor suppress
the mandatory reflected one (the cycle-2 corner). The new assertion is
well-constructed: it establishes the precondition that `EmitTransmittedOnly` is
representable on a plain `ElementPlacement` (`RayModelTests.fs:87`) before
asserting the result is exactly a non-empty `[ Reflected ]` — so it pins the
behaviour, not just a count. Float tolerance (`1.0e-9` absolute) continues to
match the slice-001 sibling test precedent (`PlacementTests.fs`).

## Spec fit

The cycle-3 delta is tightly scoped. `git diff HEAD` is exactly: the two owned
files (`RayModel.fs`, `RayModelTests.fs`), the two `.fsproj` compile
registrations that adding a new module mechanically requires (and both are
placed in correct dependency order — `RayModel.fs` after `Placement.fs`/
`BeamTree.fs`, `RayModelTests.fs` after `PlacementTests.fs`), and the
supervisor-managed `.manifest.state.json`. `Placement.fs` is untouched, so the
reuse critic's F1/F2 stay correctly deferred. All six ACs remain implemented and
pinned one-test-each, and AC-B2 is now strictly stronger than cycle 2 (it covers
default, `EmitBoth`, and `EmitTransmittedOnly` mirrors). The SoW changelog entry
(`002-state-of-the-world.md:167-176`) records the change accurately, including
that it replaced the cycle-2 intersect form and why. No scope creep, no
under-delivery.

## Evolvability

These are unchanged forward notes, repeated only so 004/005 are not cornered;
none is a cycle-3 regression or a slice-002 blocker.

- **`outgoingDirection` still trusts `RaySegmentSpec.branch` with no kind
  guard** (`RayModel.fs:243-250`). A spec `{ placement = mirror; branch =
  Transmitted }` would `refract` straight through a mirror in `snapChain` — the
  state `BeamNode.attach` rejects. The draw-side invariant is now structural but
  the routing side is not. Slice 005, which constructs `RaySegmentSpec`s from
  real branches, should normalise/reject a `Transmitted` branch on a mirror at
  construction, mirroring `attach`. The SoW Gotchas already documents the
  branch-vs-emission distinction, so this is signposted.
- **`RayId` carries an `int` id with no defined stability contract**
  (`RayModel.fs:343-346`). `removeRay`/`restoreRay` compare `e.ray = Some rayId`
  by structural equality, so if 005 mints branch ids that are not stable across
  edits, an orphan could fail to re-snap to the "same" restored ray. Worth a
  one-line note in 005 on what the id is keyed to.
- **`outgoingGroups` runs `LightSource` through its emission metadata**
  (`RayModel.fs:188-189`), yielding `[ Reflected; Transmitted ]` for the default
  source. A source has no incident group to reflect or transmit (B.2.1 frames
  IRG/RRG/TRG around a ray *falling on* an element); it originates the CR.
  `snapChain` never calls `outgoingGroups` on the source, so this is latent, not
  active — but slice 004's draw layer should decide whether a source draws an
  outgoing pair at all or simply emits the CR. Low-confidence, but it is the one
  observation prior cycles did not surface.
- The reuse critic's **F1** (`rotateAboutTableNormal`, `RayModel.fs:98-101`,
  re-deriving private `Placement.rotateAbout`) and **F2** (`add`/`sub`/`scale`,
  `RayModel.fs:62-69`, re-rolling `Vector3` arithmetic) remain accurate
  cleanup-grade extractions for the later slice that owns `Placement.fs`.

## Risks

- **`faceNormal` re-bases on the in-plane angle only** (`atan2 v.y v.x`,
  `RayModel.fs:95`, `:209-210`): an upstream R3 that tips the incoming ray out of
  the table plane has its `z` dropped from the re-basing. Acceptable for a
  schematic, unexercised by the tests, unchanged across all three cycles.
- **Single-interface transmission** leaves a refracted ray permanently deviated
  (`RayModel.fs:247-250`) — a deliberate schematic choice (0.5), documented in
  the SoW and guarded by the doc-comment so a maintainer does not "correct" it.

## Bottom line

I lean ship. Cycle 3 did precisely the one thing cycle 2 routed back for, in the
truly-structural form the goal named, with an assertion that pins the previously
unguarded `EmitTransmittedOnly` corner and a precondition that proves the state
is reachable. Layering, reuse discipline, and SoW honesty remain exemplary, and
the scope is minimal — the two owned files plus mechanical registration. I have
no open correctness concern against any state the gates exercise; the snap-walk
asymmetry, the `RayId` contract, the light-source emission semantics, and the
F1/F2 hoists are all forward guidance for slices 004/005, not slice-002 work.
The verdict is the judge's to make.
