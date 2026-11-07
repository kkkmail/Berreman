# Where we are

Slice 0102 completes S32-DROP-ORIGIN: dropped library entry files have their own import origin and the catalogue proxy exposes the shared per-entry decision operation. Implementation and all resolved local checks are complete; formal supervisor acceptance and commit remain external.

# What's working

- Added dropped-file origins with an honest origin renderer and file-specific collision labels.
- Preserved the held latest version for dropped files and retained the existing catalogue-only latest-move rule.
- Extracted `CatalogueImportProxy.decideEntry` and reused it for each catalogue envelope without changing ordering, errors or unlisted rows.
- Added the named dropped-file regression fact while preserving every existing catalogue assertion.

# Tests

- Expected-red build failed on the absent dropped-file origin and renderer before implementation.
- Final Release solution build passed in 45.76 seconds with zero warnings/errors.
- Elevation passed over seven affected/dependent projects in 147 seconds with zero findings, ELEV004 excluded as prescribed.
- All 969 constructor cases passed with zero failures/skips in 32.951 seconds; the exact new acceptance ID passed and the count increased by one from the last recorded 968.
- All eleven s13 invariants from the resolved snapshot passed. Source whitespace and line-ending diff statistics passed.

These are measured local results. Supervisor acceptance baselines/checkpoints were not edited.

```yaml
gates:
  constructor_unit_tests: 969
```

Evidence is beneath `specs/0050/.artifacts/`: `0102-build-red.log`, `0102-build.log`, `0102-elevation.log`, `0102-elevation-reports/`, `0102-constructor-unit-tests.log`, `0102-tests/constructor-unit-tests.trx`, `0102-invariants.log` and `0102-test-coverage.log`.

# Architecture

`ImportOrigin` adds `DroppedFileOrigin of FileName`, documented to carry the leaf returned by `FileName.leafName`. Its `describe` member renders the catalogue name or file text. `ImportDecision.rowName` uses that renderer for name collisions. The existing latest-move match already requires `CatalogueImportOrigin`, so dropped versions preserve latest without a separate decision rule.

`CatalogueImportProxy.decideEntry` owns the existing `tryHeldEntry`, conditional `heldEntriesNamed` and pure `ImportDecision.decide` sequence. `decideCatalogue` calls it in its original ordered fold, then appends its existing unlisted rows. No store field, write behavior, persisted shape, schema or migration changed.

# Deferred

None within slice 0102.

# Gotchas

- Dropped-file callers supply a leaf `FileName`; path normalization and UI drop handling are outside this slice's origin/decision contract.
- The initial expected-red build also reported two existing SYSLIB0051 warnings in vendored MathNet. The final build reported zero warnings/errors; vendored sources were untouched.
- Higher-priority session instructions require local verification despite the worker protocol's act-only wording. Formal supervisor acceptance remains external.
- The supervisor's pre-existing `.manifest.state.json` edit was left untouched. No Git mutation, version bump, external-repository edit, analyzer-policy change or baseline change occurred.

# Changelog

- 2026-11-06: Added dropped-file import origins, collision rendering and the shared per-entry proxy member; passed Release build, elevation, 969 constructor cases and eleven section invariants.
