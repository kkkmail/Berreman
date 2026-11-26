# Step 0442 — implementation log

## Progress

- Read the protocol, the project prompt, rulings R0001–R0005 (`.manual/030`, `031`, `033`, `035`, `036`) and the
  slice. None of the rulings touches this step's files; all are preserved.
- Wrote the impl-plan.
- Implemented (1) Domain `PulledRecord` + `pulledRecordsOf` + in-memory answer, (2) `pulledSourceRows` +
  `stageVersion` maintenance, (3) `createInDatabase.pulledRecordsOf`, (4) the stub and both facts.
- Development build: exit 0, 80 s; warnings only the exempt NU1902 (12) and vendored SYSLIB0051 (4).
- constructor-unit-tests (whole suite, as the gate's `all_discovered` selects it): `Passed!`, 1765 of 1765, the three
  new facts passed (TRX).
- The twenty-one s43 invariants: each decoded `git grep` finds nothing (exit 1 for all 21).
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol`; no CR in the diff.

- **Domain** (`CatalogueImport.fs`):
  - `PulledRecord = { version : LibraryVersionRef; pull : SourcePullInfo }` is declared before
    `CatalogueImportProxy`, with `static member inRecordOrder`. It sorts by the record's text ordinally, then the
    entry GUID's text ordinally, then ascending version.
  - `CatalogueImportProxy.pulledRecordsOf` has a one-line doc citing S13.1.
  - `createInMemory` answers it from every envelope in its map, retired entries included. Each record's version is
    the envelope's existing `libraryVersionRef`.
- **Storage** (`LibraryDatabaseStore.fs`):
  - `LibraryEntryEnvelope.pulledSourceRows` sits beside `facetRows`, with the `StorageBoundary` mark. It answers one
    row (step 60's seven columns from the content and `info`) for `SourcePullProvenance info`, and `[]` otherwise.
  - `stageVersion` keeps the version's pulled-source row exactly the envelope's:
    - `Find` by `(EntryId, Version)`;
    - add when absent;
    - remove when the envelope has none;
    - otherwise restore a Deleted row and `SetValues`.
- **Storage** (`CatalogueImportStore.fs`): `createInDatabase.pulledRecordsOf` is one `runImport`:
  - it reads the `PulledSources` rows whose `Source` is the source's text, then each version through
    `tryStoredVersion`;
  - a missing version, or a provenance that is not a pull of the source, is `LibraryRowInvalidErr` naming the
    `LibraryVersionRef`;
  - errors map through `storageResult` to `ImportStoreUnavailableErr`, and the answer is ordered by `inRecordOrder`.
- **Tests:**
  - `CatalogueImportProxyTests.recordingProxy` answers `pulledRecordsOf = fun _ -> Ok []`.
  - `CatalogueImportContract` gains `the pulled records of a source are its held pulled versions, by record, entry
    and version`. It runs over both `CatalogueImportInMemoryTests` and `CatalogueImportSqliteTests`.
  - `LibraryDatabaseTests` gains `a pulled version's pulled-source row is stored beside its version row, and no other
    version has one`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/CatalogueImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/LibraryDatabaseStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueImportStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueImportProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryDatabaseTests.fs`

Every file is within the step's authorized edits.

## Testing state

These are development runs, evidence only, each with its gate's own command and selection.

- **build:** exit 0, 80 s against 600 s. The only warnings are the exempt NU1902 (12) and the vendored MathNet's
  pre-existing SYSLIB0051 (4).
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base): `ELEV-GATE: OK - 0 new finding(s)`, 214 s
  against 600 s.
- **constructor-unit-tests** (`all_discovered`, the whole suite as the gate selects it): `Passed!`, 1765 of 1765,
  112 s against 180 s.
  - The count rises by 3 from 1762, as the acceptance requires.
  - TRX: the three new identities passed (0.47 s, 3.11 s, 11.48 s).
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 1418 s against 3600 s.
  - 14 succeeded and 1 skipped, the Explicit five-seed ensemble (the skip policy is `allow`).
  - Both `… equals the committed kit file by file` facts passed.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing (exit 1 for all 21).
- **Line endings:** `git diff --numstat` equals `--ignore-cr-at-eol`; no CR in the diff.
- No development run exceeded its gate's effective deadline.

## Artifacts

- `specs/0050/.artifacts/0442-dev-build.log`
- `specs/0050/.artifacts/0442-dev-elevation.log`
- `specs/0050/.artifacts/0442-dev-constructor-unit-tests.log` and `0442-dev-constructor-unit-tests.trx`
- `specs/0050/.artifacts/0442-dev-kit-acceptance.log` and `0442-dev-kit-acceptance.trx`
- `specs/0050/.artifacts/0442-dev-invariants-b64.txt`

## Gotchas

- **EF identity map.** The `PulledSource` key is the version's own key, so a changed row cannot be removed and
  re-added in one context. It is updated with `SetValues`, and a Deleted row is restored first, as the facet code
  does.
- **The ordering is in F#,** not in SQL. SQLite's BINARY collation compares UTF-8 bytes, `Guid.CompareTo` is not
  text order, and EF stores the GUID upper-case. One member orders both stores.
- **"Not a pull" includes a pull of another source.** A row whose `Source` disagrees with its version's provenance is
  as invalid as a non-pull, so both are `LibraryRowInvalidErr`.
- **Two strengthenings within the new facts, no identity added:**
  - the contract fact retires C and asserts again;
  - the database fact pins the pulled version between its two assertions, exercising the in-place re-stage.
