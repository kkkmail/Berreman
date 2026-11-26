# Where we are

Slice 0105 completes S32-HELD-ENVELOPE in S3.2. Both catalogue import stores now expose the envelope of any held version for the import dialog's diff preview. Implementation and all resolved local checks are complete; formal supervisor acceptance and commit remain external.

# What's working

- Added the documented `CatalogueImportProxy.tryHeldEnvelope` read for any held version, regardless of lifecycle.
- Reused in-memory version history and the database's existing version reader and typed error boundary.
- Added the shared contract fact over both stores, covering non-latest and retired entries plus absent versions and identities.
- Extended the disposed-database test and updated the recording proxy for the new field.

# Tests

- The expected-red Release build failed with five FS0039 diagnostics identifying the absent `tryHeldEnvelope` API.
- Final Release solution build passed in 45.49 seconds with zero warnings/errors.
- Elevation passed over seven affected/dependent projects in 149 seconds with zero findings, ELEV004 excluded as prescribed.
- All 981 constructor cases passed with zero failures/skips in 39.033 seconds. TRX confirms both new inherited acceptance IDs and the extended database-failure case passed exactly once. The measured count increased by two from 979 to 981.
- All eleven s13 invariant commands in the resolved snapshot passed. Source whitespace review and normal/ignore-CR diff statistics are clean.

These are measured local results. Supervisor baselines and checkpoints were not edited.

```yaml
gates:
  constructor_unit_tests: 981
```

Evidence is in `specs/0050/.artifacts/`: `0105-build-red.log`, `0105-build.log`, `0105-elevation.log`, `0105-elevation-reports/`, `0105-constructor-unit-tests.log`, `0105-tests/constructor-unit-tests.trx`, `0105-invariants.log` and `0105-test-coverage.log`.

# Architecture

The new proxy field has the elevated signature `LibraryEntryId -> VersionNumber -> Result<LibraryEntryEnvelope option, CatalogueImportError>`. In memory, it finds the identity in the existing map and the requested version in that entry's envelopes. SQLite delegates to `tryStoredVersion` through `runImport`, so each call uses one `database.run`, with both storage-result errors and boundary failures mapped to `ImportStoreUnavailableErr` carrying the original `describe` diagnostic. Neither implementation filters on latest or lifecycle.

The shared fact reuses the built-in catalogue, explicit default application and `nextImportEnvelope` sealing helper. It adds Silicon version 2 with `KeepLatestVersion`, compares both full envelopes, checks absent version 3 and a test-local identity, then repeats the reads after retirement. The recording proxy answers `Ok None`. No cache, additional query, durable format, schema version or migration was introduced.

# Deferred

None within slice 0105.

# Gotchas

- `importFrom` produces decisions; fixture setup must apply their defaults to populate the stores.
- A version held without moving latest must remain readable. Retirement changes lifecycle, not the stored envelope or its availability.
- Higher-priority session instructions require local verification despite the worker protocol's act-only wording. Formal acceptance remains with the supervisor.
- The expected-red build reported two pre-existing vendored MathNet SYSLIB0051 warnings also recorded in the previous slice. The final build reported none; vendored sources were not changed.
- The supervisor's pre-existing manifest edit was preserved. No Git mutation, version bump, external-repository edit, analyzer-policy change or checkpoint change occurred.

# Changelog

- 2026-11-07: Added held-envelope reads to both import stores with lifecycle-independent contract coverage; passed Release build, elevation, 981 constructor cases and eleven section invariants.
