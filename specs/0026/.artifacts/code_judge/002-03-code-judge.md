# Code judge -- 002.slice-md cycle 3

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0026\.slices\002.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0026\.slices\002-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0026\.slices\002-impl-log.md`
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / impl-log-structure pass / state-of-world-structure pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0026\.artifacts\architecture_critic\002-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0026\.artifacts\reuse_critic\002-02-reuse-critic.md`

## Rationale

All five applicable gates are green, and the cycle-3 delta does exactly the one
thing the cycle-2 judge routed back for. I read the source directly: the mirror arm
of `outgoingGroups` is now the unconditional structural form
`| FlatMirror | CurvedMirror -> [ Reflected ]` (`RayModel.fs:187`), replacing the
cycle-2 `emittedGroups p.emission |> List.filter (= Reflected)` intersect form that
collapsed to `[]` for an `EmitTransmittedOnly` mirror. The arm precedes the
emission-driven arm and is genuinely independent of `p.emission`, and the match
carries no wildcard — so it is exhaustive over every `CatalogueKind` case (the
passing build confirms exhaustiveness), and it mirrors the engine, where
`BeamNode.attach` takes a `Reflected` attach on a mirror unconditionally and rejects
a `Transmitted` one with `MirrorBranchMustBeReflected` (BeamTree.fs:74-77; B.2.1 /
B.7.1). The draw layer can now neither invent the forbidden transmitted branch (the
cycle-1 hole) nor suppress the mandatory reflected one (the cycle-2 corner). The new
AC-B2 assertion (`RayModelTests.fs:86-92`) pins precisely that previously-unguarded
corner: it first establishes that `EmitTransmittedOnly` is representable on a plain
`ElementPlacement`, then asserts the result is exactly a non-empty `[ Reflected ]` —
pinning behaviour, not merely a count.

Every slice-002 acceptance criterion is implemented and exercised by a test in the
diff: AC-B1 (eight-ray 45° circle, radius `0.4 × face/2`, CR centred) via `bundleAt`;
AC-B2 (mirror RRG-only across default / `EmitBoth` / `EmitTransmittedOnly`,
non-mirror both, detector termination, engine-branch correspondence) via
`outgoingGroups` / `toBeamBranch`; AC-B3 (distance-preserving downstream travel under
an upstream R2 change) via `snapChain`; AC-B4 (default 2.0 m LS/D placement +
light-source reset) via `defaultSourcePoint` / `defaultDetectorPoint` / `moveSource`
/ `resetSource` / `snap`; AC-B5 (orphaning: remove orphans exactly its ray's
elements, ordinary edits never orphan, restore re-snaps only within `resnapDistance`)
via `removeRay` / `restoreRay` / `withPlacement`; AC-B6 (Snell `n = 1.0` / `1.5` bend
toward the normal, normal incidence passes straight) via `faceNormal` / `reflect` /
`refract` / `outgoingDirection`. The named constants the hand-off requires
(`bundleRadiusFraction = 0.4`, `resnapDistance = 0.02 m`,
`refractiveIndexOutside`/`Inside = 1.0`/`1.5`, `defaultSourceDetectorDistance =
2.0 m`) are all present and used. No new public surface lands untested.

Both critics lean ship and neither raises a re-spawn-grade finding. The architecture
critic confirms the route-back fix is implemented "as the goal asked, not merely the
example means," that the scope is minimal (the two owned files plus the two mechanical
`.fsproj` registrations plus the supervisor-managed `.manifest.state.json`), and that
SoW honesty is exemplary. Its remaining notes — `outgoingDirection` trusting
`RaySegmentSpec.branch` with no kind guard, the `RayId` stability contract,
`LightSource` running through emission metadata (latent: `snapChain` never calls
`outgoingGroups` on the source), and the in-plane-only `faceNormal` re-basing — are
explicitly forward guidance for slices 004/005 or accepted schematic simplifications
(constraint 0.5), not slice-002 requirement violations. None corresponds to an unmet
B-part AC: the drawn groups (the B.2.1 / B.7.1 obligation) flow from `outgoingGroups`,
which is now structurally guarded, while `RaySegmentSpec.branch` is the snap-chain
tracked-ray selector the SoW Gotchas already distinguishes from emission.

The reuse critic's F1 (`rotateAboutTableNormal` re-deriving private
`Placement.rotateAbout`) and F2 (`add`/`sub`/`scale` re-rolling `Vector3` arithmetic)
are accurate but require edits to `Placement.fs`, which this slice scopes as
"Edited: (none)"; the cycle-1 judge deferred both as out-of-scope cleanup, the worker
complied, and `Placement.fs` is confirmed untouched. F3 (`emittedGroups` re-matching
what `Emission.emitsReflected`/`emitsTransmitted` answer) is in-scope but the critic
itself grades it "the weakest finding here … cleanup-grade at most," noting the
explicit DU match is defensibly clearer and gains F#'s exhaustiveness checking. None
of these is one re-spawn away from a correctness change to a gated behaviour.

This is the final review cycle (`cycles_remaining == 0`), so the verdict must be
`done-green` or `escalate-to-human`. Escalation requires the slice to still fall
short of done-green ground; it does not. Every done-green condition holds: all gates
pass, no critic finding implies an unmet slice-spec requirement, the SoW and impl-log
line up with the verified diff, and all new public surface is covered by tests in the
diff. The slice is clean, correctly layered, reuse-disciplined, and honestly
documented. Ship it.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and the cycle-3 delta does exactly what the cycle-2 judge routed back for: the mirror arm of outgoingGroups is now the unconditional structural form `| FlatMirror | CurvedMirror -> [ Reflected ]` (RayModel.fs:187), exhaustive over CatalogueKind with no wildcard, mirroring BeamNode.attach's unconditional Reflected attach (BeamTree.fs:74-77; B.2.1 / B.7.1) so the draw layer can neither invent the forbidden transmitted branch nor suppress the mandatory reflected one. A new AC-B2 assertion (RayModelTests.fs:86-92) pins the previously-unguarded EmitTransmittedOnly corner by behaviour, not count. All six ACs are implemented and tested (bundleAt; outgoingGroups/toBeamBranch; snapChain; default placement + reset; orphaning; Snell tracing); all named constants present and used; no new public surface untested. Both critics lean ship with no re-spawn-grade finding: the architecture critic's notes are forward guidance for slices 004/005 or accepted schematic simplifications (constraint 0.5); reuse F1/F2 are out-of-scope Placement.fs edits the cycle-1 judge already deferred (Placement.fs confirmed untouched), and F3 is cleanup-grade at most. SoW and impl-log match the verified diff. Final cycle (cycles_remaining 0); the slice stands on done-green ground.", "retry_hint": ""}
```
