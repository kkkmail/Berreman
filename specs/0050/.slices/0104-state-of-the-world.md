# Where we are

Slice 0104 completes S32-DROPPED-DECISIONS in S3.2. Dropped library entry files are read, validated and decided against the held library without writes. Implementation and every resolved local check are complete; formal supervisor acceptance and commit remain external.

# What's working

- Added the injected standalone entry reader and diagnostic dropped-file/reference errors.
- Aggregated file refusals in drop order and collapsed equal id/version duplicates while rejecting conflicting files.
- Validated references against held entries and the complete drop before producing shared import decisions with each file's leaf-name origin.
- Added four contract facts inherited by both stores and extended error rendering; proved ordering, unchanged stores and read-error propagation.

# Tests

- The expected-red Release build failed on the absent members/error cases and dependent type inference, before implementation.
- Final Release solution build passed in 45.48 seconds with zero warnings/errors.
- Elevation passed over seven affected/dependent projects in 147 seconds with zero findings, ELEV004 excluded as prescribed.
- All 979 constructor cases passed with zero failures/skips in 37.730 seconds. TRX confirms all eight new cases and the changed error-rendering case passed exactly once; the count rose by eight from 971.
- All eleven s13 invariant commands in the resolved snapshot passed. Source whitespace and line-ending diff statistics are clean.

These are measured local results. Supervisor acceptance baselines and checkpoints were not edited.

```yaml
gates:
  constructor_unit_tests: 979
```

Evidence is under `specs/0050/.artifacts/`: `0104-build-red.log`, `0104-build-initial.log`, `0104-build.log`, `0104-elevation.log`, `0104-elevation-reports/`, `0104-constructor-unit-tests.log`, `0104-tests/constructor-unit-tests.trx`, `0104-invariants.log` and `0104-test-coverage.log`.

# Architecture

`CatalogueImportContext.readEntryFile` is a replaceable function field. The existing Storage composition root delegates to `LibraryFileText.readEntryFile` and maps storage errors to `DroppedFileRefusedErr` with the file name and the existing error description. Its `create` signature is unchanged.

`decideDroppedFiles` uses `LibraryEntryKind.tryOfFileName`, reads all recognized files before validation, and reports all file refusals together. A transient map keyed by id/version retains the first equal file and rejects contradictory hashes. Reference validation uses `content.referencedEntryIds`, the complete dropped-id set and `proxy.tryHeldEntry`; unresolved references name the incoming entry, file and missing id. The final phase calls `proxy.decideEntry` in retained drop order. Library read errors propagate unchanged, and no write/log member is invoked. Empty drops return an empty list.

The shared tests reuse the built-in catalogue, sealed content, canonical file text, atomic writer and existing write observers. Files live in GUID-named folders beneath the test output's CatalogueImportTests directory. No persisted format, schema version or migration changed.

# Deferred

None within slice 0104.

# Gotchas

- Built-in test setup calls importFrom and explicitly applies its defaults, since importFrom itself now only produces pending decisions.
- Duplicate identity includes version; two versions of one id stay separate. References can point forward within the drop and resolve by id, as required by this slice.
- The duplicate map needs an explicit envelope type because HeldEntryVersion also exposes contentHash. The initial production build exposed this ambiguity; the annotation fixed it without changing assertions.
- Higher-priority session instructions require local verification despite the worker protocol's act-only wording. Formal supervisor acceptance remains external.
- The expected-red build reported two pre-existing SYSLIB0051 warnings in vendored MathNet; the final build reported none. No vendored source was edited.
- The supervisor's pre-existing manifest state edit was preserved. No Git mutation, version bump, external-repository edit, analyzer-policy change or baseline change occurred.

# Changelog

- 2026-11-07: Added dropped-entry decisions and validation with both-store coverage; passed Release build, elevation, 979 constructor cases and eleven section invariants.
