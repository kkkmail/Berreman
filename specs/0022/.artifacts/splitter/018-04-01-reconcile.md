# Reconciler decision -- round 04 iter 01

The verifier returned **NEEDS-REVISION** with one BLOCKER and two MINOR
findings; the claim-checker returned **zero REFUTED** entries, 19 CONFIRMED
and 11 NOT-CHECKED (semantic). I weighed each finding against the
claim-check verdicts and the arc-runner's gate rules.

## §g BLOCKER -- kept, routed to splitter

This is the load-bearing finding and it survives the check fully. The
claim-checker confirmed the entire factual chain on disk (claims #10–#15):
`.spec-bundle.gates = ["build","unit-tests","constructor-unit-tests"]`, every
manifest `slice_gates:` block and every `.slices/NNN.gates` snapshot lists
`constructor-unit-tests`, yet `.gates/Berreman/` contains only `build.gates`
and `unit-tests.gates`, no slice authors a `constructor-unit-tests.gates`
descriptor, and slice 001 creates only the test *project*
(`OpticalConstructor.Tests.fsproj`), not the gate definition. Per
`arc-runner.user-md` ("If a slice introduces one, propose a new
`.gates/Berreman/<name>.gates` descriptor first") and the rubric's "every gate
id resolves -> Missing = BLOCKER", an unresolvable gate id in every slice's
roster blocks slice 001's gate run, and therefore the whole arc, from its
first round. This is substantive and must be closed before any worker round
can pass. It is a slicing/manifest-level defect the splitter can resolve in one
of two clean ways: (a) add authoring of `.gates/Berreman/constructor-unit-tests.gates`
(a `count_at_least` `dotnet test` descriptor mirroring `unit-tests.gates`, `cwd`
on `OpticalConstructor/OpticalConstructor.Tests`) to slice 001's
files-in-scope/Requirements and ensure the gate resolves before slice 001's
roster runs; or (b) drop `constructor-unit-tests` from `.spec-bundle.gates`,
the manifest `slice_gates:` blocks and the `.slices/NNN.gates` snapshots,
folding the constructor tests under the existing `unit-tests` gate (consistent
with the reconciler round-2 consolidation of `OpticalConstructor.StorageTests`
into the single `OpticalConstructor.Tests` host). Either is within the
splitter's remit, so I route to the splitter rather than escalate to a human.

## §i MINOR -- kept, routed to splitter

NOT-CHECKED (semantic, #29); it stands on the verifier's reasoning, which is
sound. Slice 008 implements the Part F analysis functions plus the engine edit
but mandates only the `MuellerMatrixTests.fs` guard in-slice, routing
AC-F3/F5/F7/F8 into slice 011 and leaving AC-F1/F2/F4/F6/F9 without an
unambiguously owned mandatory test. The verifier itself calls this non-blocking,
but since I am already opening a splitter round it is cheap to fold in: the
splitter should give AC-F1/F4/F6/F9 (and the AC-F2 `.binz`-sidecar persistence)
a small owned smoke assertion in slice 008's own test set rather than deferring
all behavioural coverage to slice 011.

## §c MINOR -- dropped

NOT-CHECKED (semantic, #24). The verifier explicitly recommends **no
pre-emptive split** of slice 002 ("Worth keeping in mind during review; not
worth re-splitting", "No pre-emptive split required"). This is an
execution-time watch-item, not a slicing change, so I drop it as an actionable
finding. The natural split (units spine D.1–D.4 vs beam-tree topology
A.4/B.2–B.4/B.8/B.11) is noted for the arc-runner's review if the worker
overruns.

## Slice-006 "optimizers" doc nit -- dropped

CONFIRMED (#19) but flagged by the verifier as a harmless non-finding. Not
worth a fix.

## Budget note

split rounds consumed = 4 against `splitter_max_rounds` = 5, so the round I am
opening (round 5) is the **final** permitted split round. If the
`constructor-unit-tests` gate gap is not closed in round 5, the next reconcile
will have no split budget left and must escalate to the operator.

---

```json
{"decision":"route-to-splitter","next_role":"split","next_round":5,"next_vn":null,"rounds_consumed":{"split":4,"verify":3,"splitter-claim-check":3,"reconcile":3},"verifier_findings_to_apply":["§g BLOCKER constructor-unit-tests gate id referenced by .spec-bundle.gates + every slice roster has no .gates/Berreman/constructor-unit-tests.gates descriptor and no slice authors one (CONFIRMED #10-#15)","§i MINOR Part F AC-F1/F4/F6/F9 (and AC-F2 .binz persistence) lack an owned in-slice test home in slice 008"],"verifier_findings_to_drop":["§c MINOR slice 002 heaviness -- verifier itself recommends no re-split; execution-time watch-item, not a slicing change","§i non-finding slice 006 hand-off 'optimizers' doc nit -- CONFIRMED #19 but harmless, not worth a fix"],"next_task_file_path":null,"reasoning_summary":"Zero REFUTED; §g BLOCKER (unresolvable constructor-unit-tests gate) and §i MINOR (Part F test ownership) are substantive; routing to splitter for the final round-5 fix-up."}
```
