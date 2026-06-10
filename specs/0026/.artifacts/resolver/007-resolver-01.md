# Resolver decision -- 007.slice-md

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0026\.slices\007.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0026\.slices\007-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0026\.slices\007-impl-log.md`
- Judge MD: `C:\GitHub\Berreman\specs\0026\.artifacts\code_judge\007-03-code-judge.md`
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0026\.artifacts\architecture_critic\007-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0026\.artifacts\reuse_critic\007-02-reuse-critic.md`
- Supervisor log tail: `C:\GitHub\Berreman\specs\0026\.manifest-arc-runner.log`
- Escalation reason: `too many failures (2/2): code-judge route-back-to-worker on cycle 1` (category `failures-cap`)

## Diagnosis

This is a `failures-cap` escalation, and the cap tripped prematurely. The
supervisor log shows slice 007 (idx=6) burned **failure 1/2 on a worker
exit-1 crash** ("worker exited non-zero (exit=1) without arc-runner
intervention", 09:27:20) and **failure 2/2 on the single code-judge
`route-back-to-worker` of cycle 1** (10:19:49). Only ONE substantive review
round happened; the other slot was a crash unrelated to any finding. Every
deterministic gate is green (build / unit-tests / constructor-unit-tests /
ui-smoke / ui-tests / both structure gates), the judge returned
`route-back-to-worker` (NOT `escalate-to-human`) believing cycles remained,
and both critics explicitly recommended "a light re-spawn touch." Nobody in
the review loop asked for a human. This is exactly the scenario the resolver
exists for, and it matches the established calibration on this arc (slices
002 and 006 both issued hints for failures-cap on a unanimous concrete
corner).

I verified the load-bearing claims directly in source rather than trusting
the critiques. **Deciding finding (unanimous: judge + both critics):**
`editActive` (`ConstructorView.fs:200-206`) ends in an unconditional
`commit` at `:204`, while the worker's own no-op guard `commitIfChanged`
(`:196-197`) is used by slides (`:771`) and arrow-keys (`:647`). Because
`withR1/withR2/withR3` are inert on a locked axis and R3 is locked by
default (A.1.2), a wheel-rotate about R3 leaves the project unchanged yet
still pushes an identical snapshot — a `Ctrl+Z` that does nothing. This
directly contradicts the slice's own stated High risk ("without ... pushing
on no-op edits") and dirties AC-K1 ("each action pushes one undoable
snapshot"). `toggleEmission` (`:306-310`) routes *through* `editActive`, so
**routing `editActive` through `commitIfChanged` is a literal one-line fix
that closes both no-op cases at once**, with no design change — the
between-edits invariant it relies on holds at edit start. No test exercises
a locked-axis rotation, so the gate stays green while the defect ships.

**Second finding (F1, advisory-borderline per both critics).**
`GroupsLibrary.validate` (`GroupsLibrary.fs:53-66`) reads only top-level
`results.Errors` (`:62`) and omits the `results.Details` recursion that
`SchemaValidation.collectMessages` (`SchemaValidation.fs:42-56`) performs, so
a nested groups-file violation (e.g. a malformed member placement) yields
strictly poorer diagnostics on the groups path. The spec names this seam by
line as a reuse target (R-4 / G.3.1, `SchemaValidation.fs:61`), yet the SoW
and module doc describe the copy as "reuse." `collectMessages` is `private`,
so genuine reuse touches the named seam — but the concrete harm (dropped
nested detail) can also be closed inside `GroupsLibrary.validate` by
recursing `results.Details`. Both critics rated F1 advisory ("None of these
bind the verdict"; "borderline for forcing a re-spawn"); F2/F3 are advisory
tails. The judge additionally wants two disclosures recorded: that "table
resize" (K.1.1) has **no UI surface** to wire this slice (confirmed: no
resize `Msg`, no `Table.withDimensions` caller in the Ui), and that the
redundant in-beam state (`project.placements` + `groups[].member.inBeam`,
diffed at `ConstructorView.fs:442-447`) is a real duplicate-placement hazard,
not the benign "best-effort durability" the Deferred note implies.

**Why a small hint plausibly unblocks.** The deciding fix is one verified
line. F1's concrete harm is closable in the `GroupsLibrary`/`SchemaValidation`
seam the spec already names. The remaining items are SoW disclosures the
worker rewrites every round anyway, and the judge explicitly "is not
requiring the redesign this cycle." The fix set is mechanical, localized, and
within the judge's own stated bar — so the next cycle should pass. I am
highly confident on the deciding finding and confident the F1+disclosure
fixes meet the judge's bar; the only stretch is that genuine F1 reuse touches
the spec-named seam (`SchemaValidation.fs:61`) in addition to `GroupsLibrary.fs`,
which I disclose in the hint with a cap-respecting fallback (recurse
`Details` inside `GroupsLibrary.validate`). The two primary files I direct
edits to are `ConstructorView.fs` and `GroupsLibrary.fs`; the SoW is the
worker's own deliverable.

## Verdict

issue-hint

## Hint

In `ConstructorView.fs`, route `editActive` (line 204) through the existing `commitIfChanged` instead of the unconditional `commit`, so a locked-axis rotation (R3 is locked by default) and a both-off `toggleEmission` no longer push an empty undo step — this one-line change closes the slice's own High 'push on a no-op edit' risk and the AC-K1 defect with no redesign. In `GroupsLibrary.fs`, make `validate` genuinely reuse the schema seam the spec names by line (R-4 / G.3.1): use `SchemaValidation`'s nested message collection (`collectMessages`, which recurses `results.Details`; expose it — the spec already directs reusing SchemaValidation.fs:61) instead of the top-level-only `results.Errors`, so nested groups-file errors keep their detail. If you prefer not to touch `SchemaValidation`, at minimum recurse `results.Details` inside `GroupsLibrary.validate` so the nested-error detail is no longer dropped. In your state-of-the-world, stop calling that block 'reuse' until it does, record in Gotchas that 'table resize' (K.1.1) has no UI surface to wire in this slice, and reframe the redundant in-beam-state note in Deferred as a real duplicate-placement hazard rather than benign best-effort durability. Do not redesign the in-beam single-source-of-truth or the value-equality remove this cycle, and keep every gate green — the rest of the diff is sound and this is the final slice of the arc.

```json
{"verdict": "issue-hint", "rationale": "Premature failures-cap: one of the two failure slots was a worker exit-1 crash unrelated to any finding; the only substantive round was the cycle-1 judge route-back-to-worker (NOT escalate-to-human), with gates all green and both critics recommending a light re-spawn. The deciding finding is verified one-line and unanimous: editActive (ConstructorView.fs:204) commits unconditionally while the worker's own commitIfChanged (:196) guards slides (:771) and arrow-keys (:647); a locked-axis rotation (R3 default-locked) and a both-off toggleEmission (:306, which calls editActive) push an empty undo step, contradicting the slice's own High risk and AC-K1. Routing editActive through commitIfChanged closes both at once. F1 is advisory-borderline per both critics: GroupsLibrary.validate (:53-66) reads only results.Errors (:62), dropping the Details recursion that SchemaValidation.collectMessages (:42-56) performs, so nested groups-file errors get poorer diagnostics on a seam the spec names by line (R-4/G.3.1); closable by reusing collectMessages or recursing Details locally, plus SoW honesty. Disclosures (table-resize has no UI surface; in-beam redundancy is a real duplicate-placement hazard) are SoW-only and within the judge's stated bar. Fix is mechanical, localized, no new design; highly confident it unblocks on one more round.", "operator_reply_text": "In ConstructorView.fs, route editActive (line 204) through the existing commitIfChanged instead of the unconditional commit, so a locked-axis rotation (R3 is locked by default) and a both-off toggleEmission no longer push an empty undo step -- this one-line change closes the slice's own High 'push on a no-op edit' risk and the AC-K1 defect with no redesign. In GroupsLibrary.fs, make validate genuinely reuse the schema seam the spec names by line (R-4/G.3.1): use SchemaValidation's nested message collection (collectMessages, which recurses results.Details; expose it -- the spec already directs reusing SchemaValidation.fs:61) instead of the top-level-only results.Errors, so nested groups-file errors keep their detail. If you prefer not to touch SchemaValidation, at minimum recurse results.Details inside GroupsLibrary.validate so the nested-error detail is no longer dropped. In your state-of-the-world, stop calling that block 'reuse' until it does, record in Gotchas that 'table resize' (K.1.1) has no UI surface to wire in this slice, and reframe the redundant in-beam-state note in Deferred as a real duplicate-placement hazard rather than benign best-effort durability. Do not redesign the in-beam single-source-of-truth or the value-equality remove this cycle, and keep every gate green -- the rest of the diff is sound and this is the final slice of the arc.", "confidence": "high"}
```
