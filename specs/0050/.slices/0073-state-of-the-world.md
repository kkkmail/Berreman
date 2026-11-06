# Step 0073 state of the world

## Where we are

Implemented the S2.3 samples contract extraction in SampleProxyTests.fs. The
existing in-memory suite now runs its 21 store facts through an abstract adapter
contract, following the materials pattern from step 0066. This slice adds no
store; acceptance execution belongs to the arc-runner supervisor.

## What's working

- Extracted SampleProxyContract with 21 unchanged store facts and an abstract freshProxy.
- Moved the pin helper onto the contract to pin through each adapter's proxy.
- Retained SampleProxyTests as the in-memory subclass with nine non-store facts.
- Moved nine unchanged shared fixtures into a private module above the classes.
- Preserved all 30 concrete fact method ids and the 845-test constructor suite count.

## Tests

The affected test project and dependencies built in Release x64 with zero errors
and no warnings from our code. Two existing SYSLIB0051 warnings came from vendored
MathNetNumerics/Numerics/Exceptions.cs. The full local constructor suite passed:
845 succeeded, zero failed and zero skipped. Source review preserved every fact
body and assertion apart from factory/member dispatch and comments. TRX review
confirmed every original SampleProxyTests method id and no discovery under the
abstract contract. Logs and TRX are in specs/0050/.artifacts/0073-local-*;
source and discovery reviews are in 0073-source-review.log and
0073-discovery-review.log in that same directory.

No acceptance gate results are asserted. The supervisor must execute build,
elevation, constructor-unit-tests and the eleven s10 invariants from 0073.gates.
Commit readiness remains unasserted pending that execution.

This count is inherited from the latest available supervisor checkpoint (step
71), and the local run independently matched it. Checkpoints remain unchanged.

```yaml
gates:
  constructor_unit_tests: 845
```

## Architecture

SampleProxyContract supplies freshProxy and the existing pin helper. Each fresh
store contains the Built-in samples at version 1, active and unpinned. Its facts
exercise only the SampleProxy surface, permitting another store adapter to
inherit the same assertions. SampleProxyTests supplies BuiltinCatalogue.sampleStore
and retains validation, emission, stub and material-pin facts that do not use
that store adapter. All shared fixtures live in private SampleProxyTestData.

The source file retains its compile position. Product APIs, storage formats,
packages and schemas are unchanged; no migration or compatibility path is added.

## Deferred

No implementation requirement is deferred. Acceptance execution is the
supervisor's assigned next stage.

## Gotchas

- xUnit discovers inherited facts under the concrete SampleProxyTests class;
  the abstract contract introduces no separate test class to the run.
- The supervisor-owned .manifest.state.json edit is preserved.
- No project, version ring, gate roster, acceptance oracle, analyzer setting or
  checkpoint was edited.
- Worker-owned source and handoff files use LF; captured output stays in the
  supplied per-arc artifacts folder.

## Changelog

- 2026-11-06: Extracted the reusable samples-store contract, preserving all 30
  fact identities; the local build succeeded and all 845 constructor tests passed.
