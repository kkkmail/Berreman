# Where we are

Slice 0097 completes S31-IMPORT-LOG: catalogue import metadata is represented in Domain and can be listed, recorded/replaced and forgotten through both in-memory and SQLite CatalogueImportProxy implementations. All local checks from the resolved gate roster passed; supervisor acceptance and commit remain external.

## What's working

- Added elevated import time/count types and the catalogue import log record.
- Implemented ordinal folder listing, replacement by catalogue id and idempotent forgetting in both stores.
- Mapped existing SQLite columns through marked boundaries, preserving timestamp ticks and restoring UTC kind.
- Added the shared log contract under both concrete store classes and extended typed database-failure coverage.

## Tests

- Release solution build passed in 35.32 seconds with zero warnings and zero errors.
- Elevation passed across seven affected product projects with zero new findings in 142 seconds; ELEV004 excluded as prescribed.
- All 950 constructor cases passed, zero failed/skipped, in 29.786 seconds: exactly two above the prior 948.
- The TRX audit matched both required new test identities to passing results under CatalogueImportInMemoryTests and CatalogueImportSqliteTests.
- All eleven resolved s12 section invariants passed. Source whitespace, UTF-8/LF and CR-at-EOL diff-count checks passed.

These are observed results, not edits to supervisor checkpoint or acceptance baselines.

```yaml
gates:
  constructor_unit_tests: 950
```

Evidence is in specs/0050/.artifacts: 0097-build.log, 0097-elevation.log and 0097-elevation-reports/, 0097-constructor-unit-tests.log/.trx, 0097-invariants.log and 0097-test-coverage.log. The initial failing build is retained as 0097-build-initial.log.

## Architecture

The proxy gains loggedImports, logImport and forgetImport. Each in-memory factory owns an independent map keyed by CatalogueId. SQLite reuses the existing CatalogueImports table and LibraryDatabase.run boundary, with every operation materialized inside one run; replacement sets all tracked row values through the importRow projection. StorageBoundary-marked importRow/toLogEntry members own the primitive column conversions. No database schema, format or migration changed. Callers supply the elevated UTC time; this seam does not read an ambient clock or automatically log unconfirmed imports.

## Deferred

None within this slice. Every specified shape, store operation and test effect is implemented and locally verified.

## Gotchas

- SQLite does not retain DateTime.Kind; toLogEntry restores Utc without converting the stored ticks. The shared test checks UTC kind and subsecond precision explicitly.
- Replacement may change folder, hash, time and count while retaining catalogue identity. The B/a replacement fixture distinguishes ordinal folder order from culture-sensitive ordering.
- Open Softellect.Sys.Primitives before MaterialLibrary to avoid shadowing the domain VersionNumber; this caused the initial build error and was corrected before verification continued.
- Higher-priority workspace verification instructions take precedence over the worker prompt's act-only wording. Local passes do not replace supervisor acceptance.
- The supervisor's pre-existing .manifest.state.json modification was left untouched. No Git mutation, version bump, external-repository write, baseline edit, analyzer-policy change or test retirement occurred.

## Changelog

- 2026-11-06: Implemented the catalogue import log in both stores; build, elevation, all 950 constructor cases and eleven section invariants passed locally.
