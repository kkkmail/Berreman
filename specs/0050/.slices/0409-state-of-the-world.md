# Step 0409 state of the world

## Where we are

Spec 0050 S11.5 now has the semantic message for setting an opened material's
component state. The two authorized source changes and three worker documents
are complete; supervisor verification remains pending.

## What's working

- Added `SetOpenedMaterialParameter` as the last study message case.
- Routed it through the existing study lookup and pure material-parameter update,
  preserving all typed refusals and caller-owned recording.
- Added the authorized fact covering persisted n1 freedom, unchanged other
  components, and unchanged paths and file bytes after each refused message.

## Tests

No build, tests, analyzer, invariant or other executable checks were run.
`implement_worker.system-md` Invariant 6 reserves gate execution and verdicts
for the supervisor after worker exit. No passing result, measured duration or
verified readiness is claimed.

The pending roster is `build` (600 s), `elevation` (600 s), the complete
`constructor-unit-tests` suite (180 s), and all twenty `invariant-s40-*` gates
(300 s each), as frozen in `0409.gates`.

Exactly one fact was added with the authorized identity:
`OpticalConstructor.Tests.StudyMessageTests.SetOpenedMaterialParameter frees an opened component in the stored study, and a refused one records nothing`.
It opens the fixture's absorbing material through `OpenMaterial`, sends the
new message through the existing `recordStudy` path, and reads the stored study.
It asserts n1 is unknown from 1.70 within [1.60, 1.80], all other components stay
known and unchanged, and material metadata and remaining study fields persist.
Unopened material, reversed bounds, absent component and absent study each
return their prescribed error and preserve paths, file bytes and stored study.

Checkpoint 407 records 1738 constructor cases. The YAML carries that measured
baseline unchanged; one added fact implies an expected 1739, not a measured
result. No census, oracle, gate descriptor, floor or deadline was changed.

```yaml
gates:
  constructor_unit_tests: 1738
```

## Architecture

`StudyMsg.applyTo` remains pure. The new branch uses `studyNamed` followed by
`withMaterialParameterState material parameter.name parameter.state`; it adds
no validation or IO path. The existing caller records only successful results.
No stored format, schema, additional field or compatibility behavior changes.

## Deferred

No slice implementation is deferred. The supervisor owns the pending gate
verification under the IMPLEMENT worker protocol.

## Gotchas

- The in-memory fixture stores snapshot envelopes separately; the reused
  `kitFileBytes` helper serializes those through the existing entry-file writer
  and reads other file bytes through the kit proxy.
- The pre-existing supervisor modification to `.manifest.state.json` is
  preserved. No version, notebook, analyzer configuration, external repository
  or Git history was changed.
- `commit_ready: false` reflects absent supervisor gate evidence, not partial
  implementation.

## Changelog

- 2026-11-14: Added the opened-material parameter message and one persistence
  and refusal regression fact; left executable verification to the supervisor.
