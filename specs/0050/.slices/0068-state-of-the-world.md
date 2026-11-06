# Step 0068 state of the world

## Where we are

Slice 0068 implements S22-MATERIAL-MEMBERS in spec 0050 S2.2. The material
version payload and refusal messages now have shared member implementations
ready for the second store. Implementation is complete; the supervisor retains
authority over the compiled acceptance roster.

## What's working

- Exposed MaterialMetadata and moved the payload into MaterialEntry.versionPayload.
- Centralized unknown-material, sample-reference and pinned-version refusals.
- Routed the in-memory store and material resolver through those shared members.
- Preserved payload fields, message text, sorting and refusal priority.

## Tests

The local Release x64 build of OpticalConstructor.Tests succeeded, including
Domain, Storage and Ui. There were no warnings from our code; two existing
SYSLIB0051 warnings came from vendored MathNetNumerics/Numerics/Exceptions.cs.

All 94 existing cases in MaterialProxyTests, DispersionModelsTests and
LibraryFacetsTests passed, with zero failures or skips. These exercise version
decisions, removal refusals, reference-before-pin priority and material resolution.
No tests were added, deleted or changed, so the full constructor case count is
expected to remain unchanged. Logs and TRX are under
specs/0050/.artifacts/0068-local-*.

The supervisor must execute build, elevation, constructor-unit-tests and all ten
s09 invariants from 0068.gates. No acceptance results or commit readiness are
asserted by this worker. Local verification follows higher-priority development
instructions and is separate from acceptance execution.

The following is the inherited supervisor baseline from checkpoint 66, not a
new measurement or worker-authored baseline:

```yaml
gates:
  constructor_unit_tests: 811
```

## Architecture

MaterialMetadata remains in MaterialStore, now public, and the MaterialEntry
extension exposes the same VersionPayload of MaterialComplexity and metadata.
MaterialError.unknownMaterial and stillReferenced are declared directly after
the error DU; versionsPinned is beside unknownMaterialVersion after VersionNumber
is available. The original local payload and refusal formatting were deleted.

Sample names are quoted before sorting, version numbers sort numerically, and
sample references still block removal before pinned versions are considered.
No durable shape, serialized format or proxy contract changes; no migration or
compatibility path is introduced.

## Deferred

No implementation requirement is deferred. Acceptance execution belongs to the
supervisor under the worker protocol.

## Gotchas

- Consumers of versionPayload must open MaterialStore for its type extension.
- The known-findings list and analyzer configuration are unchanged.
- The supervisor's pre-existing .manifest.state.json edit was left untouched.
- Both edited source files use LF, with no whitespace or line-ending churn.
- All worker artifacts belong under specs/0050/.artifacts; handoff documents are
  at the exact paths supplied in the task.

## Changelog

- 2026-11-06: Moved material payload and refusal construction into shared members;
  updated existing callers without changing behavior or facts; the affected
  project built and 94 existing cases passed locally.
