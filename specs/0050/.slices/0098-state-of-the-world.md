# Where we are

Slice 0098 completes S31-STARTUP-IMPORT. Startup catalogue discovery now compares manifest hashes and returns complete pending decision lists without changing library entries, latest pointers, lifecycle or the import log. All local checks from the resolved gate roster passed; supervisor acceptance and commit remain external.

## What's working

- Added pending, folder and startup reports with pending catalogues exposed in folder order.
- Implemented manifest-first discovery that skips entry reads for unchanged catalogues and holds every changed row for confirmation.
- Processed the shipped folder before logged folders, preserving log order and isolating folder failures while returning log-read failures.
- Injected canonical manifest hashing, UTC time and catalogue-folder IO through the existing Storage composition boundary.
- Added five shared startup facts over both stores, including cancellation, version/latest safeguards, retirement and deterministic clocks.

## Tests

- Expected-red compilation confirmed the missing startup surface before implementation.
- Final Release solution build passed with zero warnings and zero errors in 16.11 seconds.
- Elevation passed over seven affected/dependent product projects with zero new findings in 142 seconds; ELEV004 excluded as prescribed.
- All 960 constructor cases passed, zero failed/skipped, in 31.401 seconds: exactly ten above the previous observed 950.
- TRX audit confirmed all ten required test identities passed under CatalogueImportInMemoryTests and CatalogueImportSqliteTests.
- All eleven resolved s12 section invariants passed. Whitespace and CR-at-EOL diff-count checks passed.

These counts are measured local results, not edits to supervisor checkpoints or acceptance baselines.

```yaml
gates:
  constructor_unit_tests: 960
```

Evidence is under specs/0050/.artifacts: 0098-build-red.log, 0098-build.log, 0098-elevation.log and 0098-elevation-reports/, 0098-constructor-unit-tests.log/.trx, 0098-test-coverage.log and 0098-invariants.log. The two audit scripts are preserved alongside those results.

## Architecture

CatalogueImportContext gains hashManifest, now and openCatalogue dependencies. Its private folder inspector is shared by importFolder and importOnStart; startup reads one log snapshot and maps folders sequentially, retaining each Result in its folder report. Changed catalogues delegate to importFrom/importCatalogue, so the per-entry decision table remains the only decision source. Pending metadata includes every decision, including Reuse, additions and retirement, but no startup path calls apply, retireEntry or logImport. Storage reuses the existing manifestHash and CatalogueProxy.create members and supplies the ambient UTC clock at its marked IO composition boundary. The new records and union are runtime data; no stored schema or migration changed.

## Deferred

None within this slice. Every specified shape, dependency, orchestration behavior and test effect is implemented and locally verified.

## Gotchas

- importOnStart takes the built-in catalogue folder itself. CatalogueProxy.builtInFolderUnder remains Storage's existing conversion from a shipped catalogue root.
- CatalogueImported denotes a settled result as specified; startup returns only CatalogueUnchanged or CatalogueAwaitsChoices. Confirmation in this slice's tests is the single confirmImportDefaults helper, which explicitly applies every row and then logs. Step 0099 is specified to replace that fixture sequence with settle.
- Tests inject a deterministic clock and create GUID-named folders under AppContext.BaseDirectory/CatalogueImportTests. The missing-entry check deletes only from an isolated test catalogue copy.
- Manifest hash failures reuse the typed CatalogueUnreadableErr/CatalogueFileRejectedErr path with manifest identity and diagnostics.
- Higher-priority workspace verification instructions take precedence over the worker prompt's act-only wording. Local passes do not replace supervisor acceptance.
- The supervisor's pre-existing .manifest.state.json edit was left untouched. No Git mutation, version bump, external-repository write, baseline edit, analyzer-policy change or test retirement occurred.

## Changelog

- 2026-11-06: Implemented read-only startup catalogue discovery and all-row pending reports; Release build, elevation, all 960 constructor cases and eleven section invariants passed locally.
