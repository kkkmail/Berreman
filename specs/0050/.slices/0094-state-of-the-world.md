# Where we are

Slice 0094 implements S31-ENVELOPE-SEAM in S3.1. Catalogue imports identify every row without writes; explicit choices apply through the envelope seam. Implementation and all resolved local checks are complete; supervisor acceptance remains external.

## What's working

- Replaced decoded save fields with held-entry reads, whole-version puts and retirement.
- Added choice validation, reference checks, dependency-ordered writes, forks and rebinding outcomes.
- Retained startup decisions without applying defaults; made test fixture confirmation explicit.
- Updated import regression tests and moved the undecodable-entry fact to the in-memory adapter suite.

## Tests

- Regression compilation failed on the newly required production symbols as expected.
- Final Release build passed with zero warnings/errors (23.57 seconds).
- Elevation passed over seven affected/dependent projects with zero new/known findings (148 seconds; ELEV004 excluded).
- All 924 constructor cases passed, zero failures/skips (18.783 seconds), exactly three above the prior observed 921.
- All 210 UI smoke cases passed, zero failures/skips (42.292 seconds).
- All 38 selected UI cases passed, zero failures/skips (5.312 seconds), covering AppContextTests, MainWorkbenchTests and TableAndElementRotationTests. UI test identities/counts are unchanged.
- All eleven s12 invariants passed. Whitespace review passed; all 14 changed/new source/project files are LF and diff counts match with/without CR-at-EOL ignored.

Local checks follow the higher-priority workspace verification requirement. These are observed results, not edits to supervisor checkpoints or acceptance records.

```yaml
gates:
  constructor_unit_tests: 924
  ui_smoke_tests: 210
  ui_tests: 38
```

Evidence is under specs/0050/.artifacts: 0094-regression-red.log, 0094-build.log, 0094-build-final.log, 0094-elevation.log and 0094-elevation-reports/, 0094-constructor-unit-tests.log/.trx, 0094-ui-smoke.log/.trx, 0094-ui-tests.log/.trx and 0094-invariants.log. Output-directory overrides change only artifact placement.

## Architecture

CatalogueImportContext owns the proxy and injected sealing/id providers. Domain uses the existing decision table and reference projection; Storage supplies canonical sealing and GUID generation. The in-memory adapter records original envelopes in ascending version order, latest metadata and lifecycle, delegating only first-id writes to its four stores. No stored format changes or migrations are introduced.

AppContext retains catalogueImport and catalogueDecisions so production startup preserves both the decisions and the matching write context. Neither composition nor a production helper confirms them. Diagnostic DefaultStores and test fixtures explicitly apply all defaults.

## Deferred

None within the slice. Supervisor acceptance and commits remain external.

## Gotchas

- The retention requirement needs AppContext fields for its import context and pending decisions; no retry, cache or optional policy was added.
- LibraryBackupProxyTests, LibraryWindowTests and LauncherTests also required caller/expectation repairs beyond the enumerated files. Their assertions follow explicit fixture confirmation or unconfirmed production startup, and test counts are unchanged.
- ofStores owns in-memory history even when backed by database stores; later versions and retirement stay in that seam, as specified here.
- The supervisor-owned pre-existing .manifest.state.json modification is untouched.
- No Git mutations, version bumps, other-repository edits/builds, analyzer policy changes or checkpoint edits were made.

## Changelog

- 2026-11-06: Implemented envelope-based, explicitly confirmed imports and adapted production composition and fixtures; Release build, elevation, 924 constructor cases, 210 UI smoke cases, 38 selected UI cases and eleven s12 invariants passed locally.
