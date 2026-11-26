# Step 0072 state of the world

## Where we are

Implemented the first S2.3 slice: extract the in-memory samples store's shared
payload, search and refusal behavior before the SQLite samples adapter is added.
The production change is confined to ElementId.fs and SampleStore.fs, as declared
by the slice. Acceptance execution belongs to the arc-runner supervisor.

## What's working

- Exposed SampleMetadata and moved the versioning payload to Sample.versionPayload.
- Moved the name and substrate filter to SampleQuery.selectFrom, preserving order.
- Moved sample identity, version and pin refusals to shared SampleError members.
- Updated the in-memory store to call the shared members and removed local bodies.

## Tests

The constructor test project and its dependencies built in Release x64 with zero
errors and no warnings in our code. Two existing SYSLIB0051 warnings came from
vendored MathNetNumerics/Numerics/Exceptions.cs. All 30 existing SampleProxyTests
passed with zero failures and zero skips. Logs and TRX are under
specs/0050/.artifacts/0072-local-*.

No acceptance gates have been run or reported by this worker. The supervisor
must execute build, elevation, constructor-unit-tests and the eleven s10
invariants from 0072.gates. Existing facts are unchanged; the required constructor
suite count delta is zero. Commit readiness remains unasserted pending that run.

The following count is inherited from the latest recorded supervisor checkpoint
(step 70), not a measurement from this worker. Checkpoints remain supervisor-owned.

```yaml
gates:
  constructor_unit_tests: 845
```

## Architecture

The existing functions move to members of the values they describe. Sample
physics remains SampleStructure; name, description and SubstrateKind remain
metadata. The query retains ordinal case-insensitive name matching followed by
the optional substrate filter. Refusal factories retain the exact existing
messages and sort pinned version numbers when rendering the diagnostic. The
store passes VersionNumber values directly to the factory.

The proxy and stored shapes do not change. No migration is needed; SampleMetadata
becomes a public reusable payload type within the existing SampleStore module.

## Deferred

No implementation requirement is deferred. The acceptance run is the supervisor's
assigned next stage.

## Gotchas

- Sample.versionPayload is an extension in SampleStore; consumers open that
  module, following the existing MaterialStore pattern.
- The supervisor's pre-existing .manifest.state.json edit is preserved.
- Tests, checkpoints, acceptance oracle, gate roster, analyzer configuration
  and version rings are unchanged.
- All worker-owned source and handoff files use LF; persistent verification
  output is kept in the supplied per-arc artifacts directory.

## Changelog

- 2026-11-06: Extracted shared sample payload, search and refusal members;
  affected project build and all 30 existing SampleProxyTests passed locally.
