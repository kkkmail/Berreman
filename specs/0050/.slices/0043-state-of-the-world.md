# Step 0043 state of the world

## Where we are

Spec 0050 S1.6 now supplies MainWorkbenchTests from the shared Built-in catalogue
fixture introduced in step 0041, following the default-scene composition from
step 0042. This completes the slice's S16-WORKBENCH-FIXTURE implementation;
formal gate acceptance remains with the arc-runner.

## What's working

- Replaced the workbench's self-seeding fixtures with fresh Built-in catalogue stores.
- Composed Main with imported presets, live samples and fresh user sources.
- Reused imported materials and categories in the create-picker fact.
- Preserved every fact name, assertion and trait, including the multilayer band fixture.

## Tests

Local Release/x64 solution build succeeded with zero errors and no warnings
from our code. Two existing SYSLIB0051 warnings came from vendored MathNet's
`Exceptions.cs`. All 208 UI smoke tests and all four selected non-smoke
MainWorkbenchTests passed, with zero failures and zero skips. These exercise
all five existing workbench facts; test names and counts are unchanged. Logs
and TRX reports are under `specs/0050/.artifacts/0043-local-*`, with exact paths
and commands in the implementation log. The reviewed code diff has no whitespace
errors or CRLF churn, and no prohibited self-seeding calls remain in the file.

The supervisor still owns `build`, `elevation`, `ui-smoke`, `ui-tests`,
`invariant-s06-elev-list-empty`, `invariant-s06-no-vstest`,
`invariant-s06-no-kit-kit-json`, `invariant-s06-no-eps-axis-evaluated`,
`invariant-s06-complexity-mandatory` and
`invariant-s06-no-seeded-string-preset-ids`. No formal gate result is claimed.

The YAML preserves the preceding handoff's baseline values: checkpoint 41 has
208 smoke cases and no `ui_tests` key. Zero records that absence, not the local
four-test result. Baseline updates remain the supervisor's responsibility.

```yaml
gates:
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

`freshStores` returns `BuiltinCatalogue.freshStores ()` as the existing
`InMemoryCatalogueStores` bundle. `mainWith` passes its materials, samples and
categories into `initMainWith`, with `liveLibrary` composed from its imported
library entries, its samples and a fresh `SourceProxy.createInMemory ()`.
The existing experiments proxy remains unchanged. The shared fixture is already
file-linked into the UI test project; no new implementation or reference is
needed. No durable shape changes or migration apply.

## Deferred

No implementation work was deferred. Formal supervisor acceptance is pending,
with no waiver or carried failure.

## Gotchas

- `SeedSamples.multilayerQw` intentionally remains in the band-state fact, as
  explicitly required by this slice.
- The catalogue must be present under the test output's `Catalogue/Builtin/`,
  supplied by the existing Storage content wiring.
- Local verification follows the coding-session instructions despite the worker
  role's act-only wording; formal gate collections remain empty.
- The pre-existing runner modification to `.manifest.state.json` is untouched.
  No version bump, git mutation, external-repository edit or catalogue write occurred.

## Changelog

- 2026-11-05: Repointed MainWorkbenchTests to fresh catalogue-backed stores; local build, 208 UI smoke tests and four selected workbench tests succeeded.
