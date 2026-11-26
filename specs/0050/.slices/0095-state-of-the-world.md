# Where we are

Slice 0095 implements STORE_XDUO_0008 in S3.1. The library database now has its own catalogue import adapter, preserving each incoming envelope and provenance. Product startup retains the complete decision list for explicit confirmation. Implementation and all resolved local checks are complete; supervisor acceptance remains external.

## What's working

- Added direct SQLite catalogue reads, version writes and retirement with typed storage failures.
- Preserved original envelopes, provenance, ascending version/hash history and retirement pin state.
- Retired ofStores and composed startup imports through createInDatabase without confirming them.
- Shared import contract tests between memory and SQLite, including rejection and envelope fidelity coverage.
- Updated explicit UI and backup population fixtures and marked STORE_XDUO_0008 implemented.

## Tests

- Release solution build passed (63.84 seconds), zero errors and no warnings from our projects. Two SYSLIB0051 warnings originated in unchanged vendored MathNetNumerics/Numerics/Exceptions.cs.
- Elevation passed across seven affected/dependent projects (145 seconds), zero new/known findings; ELEV004 excluded as prescribed.
- All 931 constructor cases passed, zero failures/skips (25.666 seconds): exactly seven above the prior 924, including all 13 concrete import-adapter cases.
- All 210 UI smoke cases passed, zero failures/skips (59.860 seconds).
- All 41 selected UI cases passed, zero failures/skips (13.307 seconds): AppContextTests, LauncherTests, TableAndElementRotationTests and WireUiFinalCompositionTests. Their test identities and case counts are unchanged.
- All eleven s12 invariants and whitespace/UTF-8/LF checks passed. Diff counts agree with and without CR-at-EOL ignored.

These are observed round counts; no supervisor checkpoint or baseline was edited.

```yaml
gates:
  constructor_unit_tests: 931
  ui_smoke_tests: 210
  ui_tests: 41
```

Evidence is under specs/0050/.artifacts: 0095-build.log, 0095-elevation.log and 0095-elevation-reports/, 0095-constructor-unit-tests.log/.trx, 0095-ui-smoke.log/.trx, 0095-ui-tests.log/.trx, and 0095-invariants.log.

## Architecture

Every SQLite proxy field executes one LibraryDatabase.run. Successful writes finish with one SaveChanges, reusing tryStoredVersion, tryLatestStored, stageVersion and stageLatest. Held-entry metadata comes from the latest stored envelope; historical version/hash pairs are projected directly from relational columns. StorageError values and boundary-caught exceptions become ImportStoreUnavailableErr. No cache, second connection, retry, transaction option or durable-format change was introduced.

The in-memory implementation and its decoded stores are now entirely owned by createInMemory. AppContext retains its matching CatalogueImportContext and all pending rows. Test population explicitly applies the complete default choice list, while startup identification assertions observe empty stores beforehand.

## Deferred

None within the slice. Supervisor acceptance and commits remain external.

## Gotchas

- LibraryBackupProxyTests.importLibrary was an additional ofStores caller; retiring that API required its routine conversion to createInDatabase without changing its assertions or test identities.
- The relocated database-import fact's value-resolution assertions are shared contract assertions; its relational counts, unchanged-reuse rows and payload fidelity assertions live in the SQLite-specific fidelity fact.
- The higher-priority workspace verification requirement takes precedence over the worker prompt's act-only instruction. Local results do not replace supervisor acceptance.
- The pre-existing .manifest.state.json modification belongs to the supervisor and is untouched. No version bump, Git mutation, external-repository write, analyzer-policy change or checkpoint edit was made.

## Changelog

- 2026-11-06: Implemented SQLite catalogue imports, shared contract tests and explicit fixture confirmation; build, elevation, 931 constructor cases, 210 UI smoke cases, 41 selected UI cases and eleven s12 invariants passed locally.
