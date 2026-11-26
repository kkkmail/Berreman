# Step 0070 state of the world

## Where we are

Slice 0070 implements S22-CATEGORY-MEMBERS in spec 0050 S2.2. Category refusal
construction now lives in shared CategoryError members, ready for the second
category store. This slice's implementation is complete; the supervisor owns
the compiled acceptance roster and checkpoint publication.

## What's working

- Added unknownCategory, duplicateCategory, builtInNotRemovable and stillReferenced.
- Routed in-memory category refusals through the shared CategoryError members.
- Preserved message text, quoted-name sorting, counts and refusal priority.
- Preserved all existing category facts and proxy signatures.

## Tests

The local Release x64 build of OpticalConstructor.Tests and its dependencies
succeeded with zero errors and no warnings from our code. Two existing SYSLIB0051
warnings came from vendored MathNetNumerics/Numerics/Exceptions.cs.

All 12 existing CategoryProxyTests cases passed, with no failures or skips.
No test source changed. Local logs and the TRX are in specs/0050/.artifacts/0070-local-*.
Source diff review found no whitespace errors or line-ending churn.

These are local development checks under the higher-priority coding instructions,
not acceptance-gate results. The supervisor must execute build, elevation,
constructor-unit-tests and all ten s09 invariants from 0070.gates.

The following baseline is inherited from supervisor checkpoint 68, not a new
measurement or worker-authored checkpoint. No constructor count change is expected.

```yaml
gates:
  constructor_unit_tests: 835
```

## Architecture

The CategoryError augmentation is immediately above CategoryProxy's in-memory
augmentation in MaterialLibrary.fs. The store projects material entries to their
EntryName values; the shared stillReferenced member quotes and sorts those names
and constructs the diagnostic. The other members own their corresponding
diagnostics. The original local helper and inline refusal formatting are deleted.

No durable shape, serialized format, public proxy contract or behavior changes.
No migration or compatibility path is needed or introduced.

## Deferred

No implementation requirement is deferred. Acceptance execution is the
supervisor's assigned protocol stage.

## Gotchas

- A duplicate lookup's existing.id equals the attempted category.id; a removal
  lookup's category.id equals the requested id. Moving those reads preserves text.
- The supervisor's pre-existing .manifest.state.json edit remains untouched.
- Test facts, analyzer configuration, known findings and version rings are unchanged.
- All edited source and handoff files use LF; artifacts use the supplied folder.

## Changelog

- 2026-11-06: Moved category refusals into four shared members; preserved store
  behavior and facts; the affected project built and all 12 category cases passed locally.
