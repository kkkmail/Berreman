# Step 0052 state of the world

## Where we are

Spec 0050 S1.7 step 0052 has re-pointed the nine named constructor test files to
the shipped BuiltinCatalogue fixture. Source changes and worker outputs are
complete. Acceptance remains unresolved because preserving an existing fact
name conflicts with the slice's forbidden-text oracle; supervisor verification
is also pending. No gate result is claimed.

## What's working

- Re-point sample, source, propagation and Library tests to BuiltinCatalogue.
- Preserve sample version pins and injected material/sample store dependencies.
- Use stable sample ids for placed emissions and four preset ids for tunables.
- Compare recursively collected Sources tree ids independently of group order.
- Preserve fact names, assertion calls, counts and numeric expectations.
- Update seed documentation and remove unused store/material imports.

## Tests

No build, test, analyzer, acceptance-grep or invariant gates were run, per
IMPLEMENT Invariant 6. The supervisor owns the resolved roster: build,
elevation, constructor-unit-tests (all discovered), and the seven s07 invariants
elev-list-empty, no-vstest, no-kit-kit-json, no-eps-axis-evaluated,
complexity-mandatory, no-seeded-string-preset-ids and no-seeding-seam.

Source review found no fact addition, deletion or rename, and numeric
expectations are unchanged. The Sources tree equality sorts both lists while
retaining duplicates; other ordered assertions remain ordered. Ordinary and
CR-insensitive source diff counts agree for all nine files. These observations
are not gate results.

The count below is inherited from 0051-state-of-the-world.md, which cites
checkpoint 46. It is historical evidence only, not a new baseline or a count
measured by this worker. The supervisor owns baseline maintenance.

```yaml
gates:
  constructor_unit_tests: 805
```

## Architecture

All source edits are in the nine test files authorized by the slice.
BuiltinCatalogue supplies samples, sources, materials, categories, presets,
fresh material/sample stores, and Library proxies. Stable SampleIds replace
reads of seed objects solely for their ids; sample and source values are loaded
through the existing fixture. Original VersionsInUse and reference callbacks
remain the inputs to the fresh stores.

LibraryProxyTests now exercises the derived tree through its catalogue proxy.
SourceLibraryTests retains its recursive leaf traversal and sorts both sides
only for the derived Sources tree, whose fixed/tunable grouping differs from
catalogue row order. SourceProxy.createInMemory remains the empty user-source
store. No product file, fixture member, fact, project configuration or durable
shape is added; no migration or compatibility path is introduced.

## Deferred

No implementation work is assigned to a later slice. The acceptance conflict
requires a supervisor/operator ruling: exempt the preserved fact name from the
forbidden-text oracle, or authorize renaming that one fact. Gates await the
supervisor as required by the worker protocol.

## Gotchas

- PropagationTests retains the fact named `the curated Library seeds exactly the
  named SeedSamples`. The slice explicitly says "Every fact keeps its name and
  its assertions", yet its acceptance grep prohibits `SeedSamples` anywhere in
  that file. Both obligations cannot hold simultaneously. Its body and comments
  use the fixture; the fact name is deliberately preserved. No test renaming,
  text-obfuscation workaround or gate change was made. This conflict was found
  by reading source, not by executing the acceptance grep.
- `commit_ready` is false because this conflict is unresolved and gate results
  cannot be certified by this worker.
- The pre-existing specs/0050/.manifest.state.json change is supervisor-owned
  and remains untouched.
- Historical seed terminology in other fact names and assertion messages is
  preserved by the slice's explicit requirement.

## Changelog

- 2026-11-06: Re-pointed nine constructor test files to BuiltinCatalogue, kept
  existing facts and numeric expectations, updated source-tree ordering, and
  recorded the conflicting fact-name and text-oracle requirements.
