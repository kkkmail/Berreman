# Where we are

Slice 0103 completes S32-ENTRY-FILE-READ in S3.2. Standalone library entry files and catalogue rows now share the same file reader and extension lookup. Implementation and all resolved local checks are complete; formal supervisor acceptance and commit remain external.

# What's working

- Added case-insensitive entry-kind lookup through the shared file-extension API.
- Extracted UTF-8 entry reading with typed IO errors and existing schema, version and hash validation.
- Reused the reader and extension rule in catalogue processing while preserving manifest checks and row ordering.
- Added both named acceptance facts and preserved every existing catalogue assertion.

# Tests

- Expected-red build failed on the two absent members before production implementation (eleven FS0039 diagnostics).
- Final Release solution build passed in 47.10 seconds with zero warnings/errors.
- Elevation passed over seven affected/dependent projects in 145 seconds with zero findings, ELEV004 excluded as prescribed.
- All 971 constructor cases passed with zero failures/skips in 34.463 seconds. TRX confirms both exact new acceptance IDs passed, increasing the previous count of 969 by two.
- All eleven s13 invariant commands from the resolved snapshot passed. Source whitespace and line-ending diff statistics passed.

These are measured local results. Supervisor acceptance baselines and checkpoints were not edited.

```yaml
gates:
  constructor_unit_tests: 971
```

Evidence is under `specs/0050/.artifacts/`: `0103-build-red.log`, `0103-build.log`, `0103-elevation.log`, `0103-elevation-reports/`, `0103-constructor-unit-tests.log`, `0103-tests/constructor-unit-tests.trx`, `0103-invariants.log` and `0103-test-coverage.log`.

# Architecture

`LibraryEntryKind.tryOfFileName` uses `FileName.tryGetExtension()` and compares the six kinds' existing extensions with `StringComparison.OrdinalIgnoreCase`. Catalogue enumeration still considers only top-level files and now delegates recognition to this member.

`LibraryFileText.readEntryFile` owns the UTF-8 entry read, catches filesystem exceptions as `FileIoExn`, and delegates to the existing `parseEntry` for schema, version and hash checks. Each manifest row calls this member and maps its error to `CatalogueFileRejectedErr` naming the row file. Manifest reading, missing/unlisted checks and catalogue validation retain their order. No persisted shapes, formats, schema versions or migrations changed.

# Deferred

None within slice 0103.

# Gotchas

- A missing catalogue row is still rejected by the preflight listing as `MissingEntryFileErr`; a missing standalone entry is `FileIoExn`. Manifest IO remains `CatalogueIoErr`.
- The new reader fact deletes the manifest before reading entries, exercises a sharing lock, and checks unsupported schema versions as well as the required wrong-kind and payload-hash failures. Fixtures remain under the test output directory.
- Higher-priority session instructions require local verification despite the worker protocol's act-only wording. Formal supervisor acceptance remains external.
- The initial expected-red build also reported two existing SYSLIB0051 warnings in vendored MathNet. The final build reported zero warnings/errors; vendored sources were untouched.
- The supervisor's pre-existing `.manifest.state.json` edit was left untouched. No Git mutation, version bump, external-repository edit, analyzer-policy change or baseline change occurred.

# Changelog

- 2026-11-07: Shared standalone/catalogue entry reading and extension recognition; passed Release build, elevation, 971 constructor cases and eleven section invariants.
