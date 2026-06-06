# Resolver decision -- 002.slice-md

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0026\.slices\002.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0026\.slices\002-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0026\.slices\002-impl-log.md`
- Judge MD: `C:\GitHub\Berreman\specs\0026\.artifacts\code_judge\002-03-code-judge.md`
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0026\.artifacts\architecture_critic\002-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0026\.artifacts\reuse_critic\002-02-reuse-critic.md`
- Verified at source: `RayModel.fs:180-186` (`outgoingGroups`), `Placement.fs:110-138` (`Emission` / `withReflected`).
- Escalation reason: `too many failures (2/2): code-judge route-back-to-worker on cycle 2` (category `failures-cap`).

## Diagnosis

The cause is concrete, single, and verified against source rather than taken on
the critics' word. In `outgoingGroups` the mirror arm reads
`| FlatMirror | CurvedMirror -> emittedGroups p.emission |> List.filter (fun g -> g = Reflected)`
(`RayModel.fs:183-184`). For the default `EmitReflectedOnly` and the tested
`EmitBoth` this yields `[ Reflected ]`, correct. But `Emission.withReflected false`
drives any emission to `EmitTransmittedOnly` (`Placement.fs:126-130`), whose
`emitsReflected` is `false` (`Placement.fs:113-116`), so `emittedGroups` omits
`Reflected` and the filter returns `[]` — a mirror that draws **neither** an RRG
nor a TRG. `ElementPlacement` is a plain record, so `{ mirror with emission =
EmitTransmittedOnly }` is representable: the same "phantom state is representable"
argument that justified the cycle-1 fix. B.2.1 requires a mirror to *produce* an
RRG unconditionally, so this silently suppresses the mandatory reflected branch —
the mirror image of the cycle-1 phantom-TRG bug, the same B.2.1 / B.7.1 class.

The escalation category is `failures-cap`, not a structural or spec-level
objection. The slice spec is not ambiguous here, the gates are all green, all six
ACs are implemented and individually tested, and both critics plus the code judge
**agree** on one and the same remedy — there is no critic/judge disagreement for
me to adjudicate. The judge's cycle-2 verdict was `route-back-to-worker` with a
fully-formed retry hint; the loop escalated only because the per-launch failure
budget (2/2) was spent before that route-back could run, not because the issue
resists a hint. This is precisely the shape the resolver exists for: the same
finding raised across cycles, a one-token correct fix already identified, blocked
only by budget.

The fix meets every `issue-hint` gate. (1) Concrete cause: the emission-dependent
filter form in `outgoingGroups` returns `[]` for an `EmitTransmittedOnly` mirror.
(2) Small: replace the filter with the unconditional `[ Reflected ]` (one token
shorter) and add one AC-B2 assertion — two owned files (`RayModel.fs`,
`RayModelTests.fs`), within the 2-file cap, no new design, no new dependency, no
scope change. (3) High confidence: the unconditional form is truly independent of
`p.emission` (the cycle-1 hint's stated goal) and matches the engine, where
`BeamNode.attach` takes a `Reflected` attach on a mirror unconditionally
(`BeamTree.fs:74-77`); the reuse critic confirms no reuse concern either way and
the F1/F2 `Placement.fs` items stay correctly deferred. (4) In-scope: both edits
land inside the files this slice already owns. I am explicitly *not* hinting on the
advisory carry-overs (snap-walk `outgoingDirection` asymmetry, `faceNormal`
in-plane re-basing, `RayId` semantics, F1/F2) — those are forward guidance for
slice 005 and out of this slice's remit, exactly as the judge scoped them.

## Verdict

issue-hint

## Hint

FINAL retry — make two edits, both in this slice's owned files, and do NOT touch
`Placement.fs` (the reuse-critic F1/F2 `rotateAboutTableNormal` / `Vector3`
arithmetic hoists stay deferred as out-of-scope). In `RayModel.fs:183-184` replace
the mirror arm `emittedGroups p.emission |> List.filter (fun g -> g = Reflected)`
with the unconditional structural form `| FlatMirror | CurvedMirror -> [ Reflected ]`.
The current filter form returns `[]` for an `EmitTransmittedOnly` mirror
(representable via `Emission.withReflected false`, `Placement.fs:126-130`) — a
mirror that draws no RRG at all, violating B.2.1's 'a mirror produces only an RRG';
the unconditional form is one token shorter, truly independent of `p.emission`, and
matches the engine's unconditional `Reflected` attach (`BeamTree.fs:74-77`). In
`RayModelTests.fs` add one AC-B2 assertion that a mirror with emission forced to
`EmitTransmittedOnly` still yields exactly `[ Reflected ]` (non-empty, no TRG).
Re-run the gates and update the impl-log / state-of-the-world changelog accordingly.

```json
{"verdict": "issue-hint", "rationale": "failures-cap escalation over a single, concrete, source-verified corner the judge and both critics agree on. outgoingGroups' mirror arm (RayModel.fs:183-184) uses emittedGroups p.emission |> List.filter (fun g -> g = Reflected), which returns [] for an EmitTransmittedOnly mirror (representable via Emission.withReflected false, Placement.fs:126-130, on a plain ElementPlacement record) -- a mirror drawing NEITHER an RRG nor a TRG, suppressing the branch B.2.1 says a mirror must always produce. The fix is one token (replace the filter with the unconditional | FlatMirror | CurvedMirror -> [ Reflected ]) plus one AC-B2 assertion, both inside this slice's two owned files, with no new design or dependency. It is truly independent of p.emission (the cycle-1 hint's stated goal) and matches the engine's unconditional Reflected attach (BeamTree.fs:74-77). The slice escalated only because the per-launch failure budget (2/2) ran out before the judge's route-back could fire, not because the issue resists a hint; this is the CRITICAL land-first layer slices 004/005 consume via outgoingGroups, so closing the corner now prevents it calcifying. High confidence; one-shot resolver cap spent well here.", "operator_reply_text": "FINAL retry -- make two edits, both in this slice's owned files, and do NOT touch Placement.fs (the reuse-critic F1/F2 hoists stay deferred as out-of-scope). In RayModel.fs:183-184 replace the mirror arm `emittedGroups p.emission |> List.filter (fun g -> g = Reflected)` with the unconditional structural form `| FlatMirror | CurvedMirror -> [ Reflected ]`. The current filter form returns [] for an EmitTransmittedOnly mirror (representable via Emission.withReflected false, Placement.fs:126-130) -- a mirror that draws no RRG at all, violating B.2.1's 'a mirror produces only an RRG'; the unconditional form is one token shorter, truly independent of p.emission, and matches the engine's unconditional Reflected attach (BeamTree.fs:74-77). In RayModelTests.fs add one AC-B2 assertion that a mirror with emission forced to EmitTransmittedOnly still yields exactly [ Reflected ] (non-empty, no TRG). Re-run the gates and update the impl-log / state-of-the-world changelog accordingly.", "confidence": "high"}
```
