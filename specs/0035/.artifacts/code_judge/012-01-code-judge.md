# Code judge — 012.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0035\.slices\012.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0035\.slices\012-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0035\.slices\012-impl-log.md`
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none — Berreman declares no project critics and none were supplied this cycle)

## Rationale

The slice asks for one production change and matching test coverage: extend the
shared `validateSample` so a `Sample` whose structure has no films **and** no
substrate is rejected as `Error (InvalidSample _)` on every save, even when its
name is non-blank; and update any test that saved a structurally-empty sample to
carry content, plus add a rejection test. The diff delivers exactly this.
`validateSample` (ElementId.fs:602-610) keeps the blank-name rule first, then
adds a `match s.structure.films, s.structure.substrate with [], None -> true`
branch returning `InvalidSample`. This is the correct pattern-match idiom
(`SampleStructure.films : StackItem list`, `substrate : SampleLayer option`,
confirmed at ElementId.fs:103-108), lives in the single shared guard, and honors
the project's "pattern-match, no `.IsNone`" rule. Both write functions thread
through `validateSample` before any store lookup, so the guard holds on every
save with no call-site change.

The two new `SampleProxyTests` facts genuinely exercise the new branch rather
than an adjacent error path. `addSample` runs `validateSample` before the
duplicate-id check (ElementId.fs:652-653) and the test's `mintedEmpty ()` carries
a fresh id plus a non-blank name, so the only reachable rejection is the
structural rule. `updateSample` runs `validateSample` before the id-existence
check (ElementId.fs:662-663), and the test uses the KNOWN seeded `plate.id` with
a non-blank name over `emptyStructure`, so `UnknownSampleId` cannot fire and the
`InvalidSample` result is the new structural rule specifically. Both assert
`Error (InvalidSample _)` with a non-blank reason. This is new externally-observable
behavior (a rejection that wasn't previously asserted), and it is covered by
tests in the diff — the `done-green` test-coverage criterion is met.

On the spec's "update the tests that save a structurally-empty sample" clause:
the worker's SoW and impl-log both record that no such test existed — every
real-proxy `addSample`/`updateSample` call is built over seeded structures
(`glassPlate1mm`/`minted`/`fixedFresh`/`glassFilm600`) that already carry a
substrate or films, so there was nothing to convert and the intent was satisfied
by adding the missing rejection coverage. This claim is independently
corroborated by the gates: `constructor-unit-tests` passes at 446 (step-011
baseline 444 + 2). Had any pre-existing test saved an empty sample expecting
`Ok`, the tightened rule would flip it to `InvalidSample` and the gate would be
red; it is green, so the audit holds. The documented interpretation is the most
defensible one and is recorded in the impl-log Gotchas as required.

All five gates pass, the SoW and impl-log line up precisely with the diff (files
touched, ordering rationale, and the count arithmetic all match), and there is no
critic finding to weigh. The acceptance criteria are fully met with no unmet
slice-spec requirement, no layering violation, and no untested new surface. This
lands on `done-green` ground.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass. The diff extends the single shared validateSample (ElementId.fs:602-610) so a sample with films = [] and substrate = None is rejected as InvalidSample, checked after the blank-name rule; both addSample and updateSample run validateSample before any store lookup, so the guard applies on every save with no call-site change. Two new SampleProxyTests facts exercise the new branch specifically: addSample via a minted non-blank-named empty sample (validation precedes the duplicate check) and updateSample via the seeded plate.id with an empty structure (validation precedes the id-existence check), each asserting Error (InvalidSample _). The spec's 'update tests that save an empty sample' clause was satisfied by adding coverage because no such test existed — corroborated by constructor-unit-tests passing at 446 (444 baseline + 2), which would be red had any prior test saved an empty sample expecting Ok. SoW and impl-log match the diff; no critic findings.", "retry_hint": ""}
```
