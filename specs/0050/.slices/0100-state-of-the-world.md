# Where we are

Slice 0100 wires S31-APP-IMPORTS-ON-START into AppContext using the catalogue import and settlement components completed by slices 0098–0099. Every composition inspects the shipped and registered catalogues by manifest hash and retains all changed-catalogue rows for explicit confirmation. The slice implementation and its resolved local verification are complete; supervisor acceptance and commit remain external.

# What's working

- Wired every startup to the database catalogue import context and retained complete pending catalogue reports.
- Logged aggregate and per-folder import errors while preserving the held library state.
- Replaced obsolete startup fields and moved populated fixtures to explicit full-list settlement.
- Added changed-version, immutable-history, unchanged-manifest and collision/reuse pending-row coverage.
- Updated startup API documentation and the live-library repository guidance.

# Tests

- Release solution build passed in 31.05 seconds with zero warnings and zero errors.
- Elevation passed across four affected/dependent projects in 94 seconds with zero findings; ELEV004 excluded as prescribed.
- All 210 ui-smoke cases passed, zero failed/skipped, in 59.968 seconds.
- All 43 selected ui-tests cases passed, zero failed/skipped, in 26.831 seconds: AppContextTests, LauncherTests, TableAndElementRotationTests and WireUiFinalCompositionTests.
- AppContext facts increased from nine to eleven, with no removals; both specified new tests were confirmed passed in TRX. The unchanged remaining selection therefore rises by two cases, from 41 to 43.
- All eleven resolved s12 invariants passed; whitespace and LF checks passed for every edited source/documentation file.

These are measured local results, not edits to supervisor acceptance baselines or checkpoints. No required verification remains outstanding locally.

```yaml
gates:
  ui_smoke_tests: 210
  ui_tests: 43
```

Evidence is under specs/0050/.artifacts: 0100-build-red.log, 0100-build.log, 0100-elevation.log and 0100-elevation-reports/, 0100-ui-smoke.log/.trx, 0100-ui-tests.log/.trx, 0100-invariants.log and 0100-test-coverage.log.

# Architecture

AppContext now owns catalogueImports : CatalogueImportContext and pendingImports : PendingCatalogueImport list. Its shared compose path calls importOnStart with CatalogueProxy.builtInFolderUnder locations.shippedCatalogueFolder for both create and recreateLibrary. An aggregate failure logs its description and leaves no pending imports; folder failures log their folder and description while other pending reports survive. Composition never calls apply or settle. LibraryDatabase.origin remains available to existing database and backup consumers.

The former catalogueImport/catalogueDecisions fields are deleted, with their callers migrated. TestAppScope.settlePendingImports is test-only and calls the existing settle operation with each pending catalogue's entire default choice list. Both fresh in-memory contexts and populated file-backed fixtures use it explicitly; unconfirmed-startup tests call AppContext.create directly. No durable shape, database schema, import decision algorithm or migration changed.

# Deferred

None within slice 0100. The pending data is ready for the separately scoped S3.2 dialog; this slice adds no product confirmation path.

# Gotchas

- Every changed-catalogue row, including Reuse, stays pending until settlement; apply alone would fail to record the completed manifest hash.
- New startup tests write isolated copies through CatalogueProxy beneath the test output. Committed catalogues and user data remain untouched.
- LibraryWindowTests.fs contains a file-backed population fixture outside the four enumerated edit paths. Its obsolete-field calls were replaced with the shared explicit settlement helper to satisfy the slice's file-backed fixture requirement and preserve compilation/ui-smoke coverage. No assertion or test count changed there.
- The initial expected-red build exposed the absent AppContext fields and one corrected test id-conversion mistake. It also rebuilt vendored MathNet and reported two existing SYSLIB0051 warnings there; the final Release build reported zero warnings/errors. Vendored sources were not changed.
- Higher-priority workspace instructions require local verification despite the worker protocol's act-only wording. Supervisor gate acceptance remains external.
- The supervisor's pre-existing specs/0050/.manifest.state.json edit was left untouched. No Git mutation, version bump, external-repository change, baseline edit or analyzer-policy change occurred.

# Changelog

- 2026-11-06: Wired manifest-hash startup imports with complete pending reports, migrated populated fixtures to explicit settlement, added two startup facts, and passed Release build, elevation, 210 smoke cases, 43 selected UI cases and eleven s12 invariants.
