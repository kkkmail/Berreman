# Step 0053 state of the world

## Where we are

Spec 0050 S1.7 step 0053 is implemented. The four named facet and library-file
test files now read the shipped catalogue through BuiltinCatalogue. Acceptance
awaits the supervisor's gate engine; this worker claims no passing gate result.

## What's working

- Re-point facet corpora and source counts to BuiltinCatalogue lists.
- Select samples, materials, categories and presets by their existing known ids.
- Round-trip every shipped material, category, sample and preset at version one.
- Preserve all fact names, assertions, count expressions and numeric expectations.
- Replace seed-related documentation with fixture references.

## Tests

No build, test, analyzer, acceptance-grep or invariant command was run, per
Invariant 6 and implement_worker.system-md. The supervisor's resolved roster
is build, elevation, constructor-unit-tests (all discovered) and seven s07
invariants: elev-list-empty, no-vstest, no-kit-kit-json, no-eps-axis-evaluated,
complexity-mandatory, no-seeded-string-preset-ids and no-seeding-seam.

Source review shows no added or removed facts and no changed numeric
expectations. Existing ordered assertions remain appropriate: facet branches
and manifest rows have their own sorting; the filtered sample expectations are
singletons. Ordinary and CR-insensitive source diff counts agree. These are
source-review observations, not executed acceptance results.

The count below is inherited from step 0052's recorded operator verification,
not measured or authored as a new baseline here. The supervisor owns test
discovery, execution and checkpoint updates.

```yaml
gates:
  constructor_unit_tests: 805
```

## Architecture

Only LibraryFacetsTests.fs, LibraryEntryFilesTests.fs, EntryFileJsonTests.fs and
CatalogueFolderTests.fs change. BuiltinCatalogue supplies their lists and known-id
lookups. Library corpora combine every sample with every preset. The formerly
first polarizer now uses PresetIds.polarizerLp so catalogue ordering cannot
change the tested entry. JSON facts seal the decoded entries using the existing
test catalogue id and version one, preserving serialization coverage.

No product file, fixture member, fact or durable shape is added. Synthetic
inputs, serialization operations, temporary test-folder behavior and error
assertions remain in place. No migration or compatibility path is introduced.

## Deferred

No implementation requirement is deferred. Verification and acceptance await
the supervisor as required by the worker protocol.

## Gotchas

- The brief's illustrative ids mention xenon and the ellipsometer, but existing
  tests also explicitly select Ti:sapphire and the intensity detector. Those
  tests retain their original ids through the fixture, preserving their inputs.
- Fact names containing seed terminology remain unchanged, as required.
- The pre-existing .manifest.overrides.jsonl and .manifest.state.json edits
  are supervisor-owned and remain untouched.
- commit_ready is false solely because supervisor gate results are pending;
  implementation and the required handoff files are complete.

## Changelog

- 2026-11-06: Re-pointed four facet and library-file test files to the shipped
  BuiltinCatalogue while preserving their facts, assertions and numeric inputs.
