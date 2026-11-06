# Step 0056 state of the world

## Where we are

Spec 0050 S1.7 step 0056 is implemented. The eight authorized UI test files now
resolve their remaining catalogue sample payloads through BuiltinCatalogue.sample
and their direct sample-id reads through SampleIds. Acceptance awaits the
supervisor's gate engine; no passing gate result is claimed by this worker.

## What's working

- Re-point sample payloads to the shipped BuiltinCatalogue fixture by known ids.
- Replace direct sample-id reads with the corresponding SampleIds members.
- Preserve existing sample projections, fact names, assertions and numeric values.
- Update catalogue comments and add the fixture imports needed by five consumers.

## Tests

No build, test, analyzer, acceptance-grep or invariant command was run, per
Invariant 6 and implement_worker.system-md. The supervisor's resolved roster is
build, elevation, ui-smoke (all discovered), ui-tests (AppContextTests,
ElementMenuTests, ExperimentControlsTests, LayerBandsControlsTests,
LibraryControlsTests and SourceAndDetectorMoveTests), plus seven s07 invariants:
elev-list-empty, no-vstest, no-kit-kit-json, no-eps-axis-evaluated,
complexity-mandatory, no-seeded-string-preset-ids and no-seeding-seam.
WireUi0040CompositionTests and WireUiCompositionTests retain their ui-smoke traits.

Source review shows no added or removed facts, changed fact names or traits, or
changed numeric test inputs and expectations. Existing ordered assertions remain
intact. Ordinary and CR-insensitive source diff counts agree. These are source
review observations, not executed acceptance results.

The ui-smoke count below is inherited from the supervisor's checkpoint record.
That record has no ui_tests baseline; zero retains the preceding slice's absence
marker. Neither value is a measurement from this round or a worker-authored
acceptance baseline. The supervisor owns execution and checkpoint updates.

```yaml
gates:
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

Only AppContextTests.fs, ElementMenuTests.fs, ExperimentControlsTests.fs,
LayerBandsControlsTests.fs, LibraryControlsTests.fs, SourceAndDetectorMoveTests.fs,
WireUi0040CompositionTests.fs and WireUiCompositionTests.fs change in source.
The UI test project already links BuiltinCatalogue.fs. The existing fixture
loads the shipped catalogue; its sample lookup preserves the exact well-known
identities. SampleItem/entryId projections remain where the original setup
consumed sample payloads. Direct .id reads use SampleIds without decoding payloads.

No product file, fixture member or fact is added. No durable shape changes and
no migration or compatibility path is introduced. There is no scope deviation.

## Deferred

No implementation requirement is deferred. Verification and acceptance await
the supervisor as required by the worker protocol.

## Gotchas

- The pre-existing specs/0050/.manifest.state.json modification is supervisor-owned
  and remains untouched.
- Positional reads address scene elements, chart series, rendered rows or captured
  experiments; none selects a catalogue entry by its former seed-list position.
- Scene-default detectors, editor-template descriptions and assertion diagnostic
  strings retain their terminology. Catalogue comments now name fixture members.
- commit_ready remains false solely because supervisor gate results are pending;
  the worker does not green-light gates.

## Changelog

- 2026-11-06: Re-pointed the remaining eight UI sample consumers to BuiltinCatalogue
  and SampleIds, preserving test identities, assertions and numerical expectations.
