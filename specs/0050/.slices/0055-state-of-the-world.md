# Step 0055 state of the world

## Where we are

Spec 0050 S1.7 step 0055 is implemented. The nine authorized Library-window,
scene and script-acceptance test files now consume the shipped catalogue through
BuiltinCatalogue. Acceptance awaits the supervisor's gate engine; this worker
claims no passing gate result.

## What's working

- Re-point Library-window corpus and source counts to the catalogue fixture.
- Populate scene and editor contexts through BuiltinCatalogue store doubles.
- Resolve sample payloads by known ids and direct sample-id reads through SampleIds.
- Compose the FSX acceptance bench over catalogue stores with unchanged script inputs.
- Preserve fact names, assertions, numeric expectations and ordering contracts.
- Update catalogue comments and remove unused store-extension imports.

## Tests

No build, test, analyzer, acceptance-grep or invariant command was run, per
Invariant 6 and implement_worker.system-md. The supervisor's resolved roster is
build, elevation, ui-smoke (all discovered), ui-tests (the nine selected classes),
and seven s07 invariants: elev-list-empty, no-vstest, no-kit-kit-json,
no-eps-axis-evaluated, complexity-mandatory, no-seeded-string-preset-ids and
no-seeding-seam. FsxAcceptanceTests remains included in the selected UI tests.

Source review shows no added or removed facts, changed fact names or traits,
or changed numerical test inputs and expectations. Existing product ordering
assertions and sorted sample-id comparisons remain intact. Ordinary and
CR-insensitive source diff counts agree. These are source-review observations,
not executed acceptance results.

The ui-smoke count is inherited from the supervisor checkpoint record. That
record has no ui_tests baseline; zero retains the preceding slice's absence
marker. Neither value is a measurement from this round or a worker-authored
baseline. The supervisor owns discovery, execution and checkpoint updates.

```yaml
gates:
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

Only LibraryWindowTests.fs, InverseConstructorTests.fs, LightSourceEditorTests.fs,
SolverHandoffWindowTests.fs, ScrollExtentTests.fs, WindowLauncherTests.fs,
OutOfBandBadgeTests.fs, WorkbenchInfoAndCaptureTests.fs and FsxAcceptanceTests.fs
change. The UI project already links BuiltinCatalogue.fs.

The existing fixture supplies samples, presets, source lists, known-id sample
lookups, library construction and isolated material/sample/category stores.
Reference callbacks and VersionsInUse arguments are preserved. SourceProxy
remains the empty user-source store; liveLibrary still composes those user
sources and samples over the catalogue library. Script materials and samples
are saved into the fresh stores as before.

No product file, fixture member, fact or durable shape is added. There is no
migration or compatibility path and no scope deviation.

## Deferred

No implementation requirement is deferred. Verification and acceptance await
the supervisor as required by the worker protocol.

## Gotchas

- The pre-existing specs/0050/.manifest.state.json edit is supervisor-owned and
  remains untouched.
- Scene endpoints, editor templates and fact names retain their seed terminology;
  comments describing catalogue data now name fixture members.
- Positional reads address scene elements, chart series, captured experiments,
  rendered tree groups and offers, not code catalogue entries.
- The sample identities remain glassFilm200Id, glassFilm600Id, glassPlate1mmId
  and multilayerQwId at their existing use sites.
- commit_ready is false solely because supervisor gate results are pending;
  implementation and required handoff files are complete.

## Changelog

- 2026-11-06: Re-pointed nine Library-window, scene and FSX acceptance UI test
  files to BuiltinCatalogue while preserving their facts and expectations.
