# Step 0054 state of the world

## Where we are

Spec 0050 S1.7 step 0054 is implemented. The six authorized UI test files now
read the shipped catalogue through BuiltinCatalogue. Acceptance awaits the
supervisor's gate engine; this worker claims no passing gate result.

## What's working

- Re-point material helpers and chart inputs to catalogue lookups by known id.
- Populate material, sample and category stores through BuiltinCatalogue doubles.
- Read category stubs, count expectations and sample-editor inputs from the fixture.
- Preserve sample identities, fact names, assertions and numeric expectations.
- Update catalogue comments and remove unused store-extension imports.

## Tests

No build, test, analyzer, acceptance-grep or invariant command was run, per
Invariant 6 and implement_worker.system-md. The supervisor's resolved roster
is build, elevation, ui-smoke (all discovered), ui-tests (the five selected
classes), and seven s07 invariants: elev-list-empty, no-vstest, no-kit-kit-json,
no-eps-axis-evaluated, complexity-mandatory, no-seeded-string-preset-ids and
no-seeding-seam. EmbeddedChartTests remains covered by ui-smoke.

Source review shows no added or removed facts, no changed fact names or traits,
and no changed numerical inputs or expectations. Existing product-ordering
assertions remain intact. Ordinary and CR-insensitive source diff counts agree.
These are source-review observations, not executed acceptance results.

The ui-smoke count below is inherited from the supervisor's checkpoint record.
That record has no ui_tests baseline; zero retains step 0049's absence marker.
Neither value is a measurement from this round or a worker-authored baseline.
The supervisor owns test discovery, execution and checkpoint updates.

```yaml
gates:
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

Only MaterialEditorWindowTests.fs, MaterialsWindowTests.fs,
EmbeddedChartTests.fs, NkDispersionChartTests.fs, CategoryEditorWindowTests.fs
and SampleEditorWindowTests.fs change. The UI project already links
BuiltinCatalogue.fs; no fixture member or project reference is needed.

The fixture supplies materials/categories, known-id material/sample lookups,
library construction and isolated store doubles. Recording stubs, synthetic
editor inputs, live reference callbacks and VersionsInUse arguments retain
their behavior. Sample edits still target glassFilm600Id and glassFilm200Id;
silicon, vacuum and glass material lookups retain their original ids.

No product file, fixture member, fact or durable shape is added. No migration
or compatibility path is introduced.

## Deferred

No implementation requirement is deferred. Verification and acceptance await
the supervisor as required by the worker protocol.

## Gotchas

- The pre-existing specs/0050/.manifest.state.json edit is supervisor-owned
  and remains untouched.
- NkDispersionChartTests contains material lookups, not store construction;
  those lookups now use BuiltinCatalogue.material.
- Seed terminology in fact names, chart styles and NewSeededMultilayer remains
  intentionally unchanged. Catalogue/store comments name fixture members.
- List-position reads address charts, rendered trees, breadcrumbs and synthetic
  editor rows; no catalogue entry was selected by its former seed-list position.
- commit_ready is false solely because supervisor gate results are pending;
  implementation and the required handoff files are complete.

## Changelog

- 2026-11-06: Re-pointed six material, category, sample-editor and chart UI test
  files to BuiltinCatalogue while preserving their facts and expectations.
