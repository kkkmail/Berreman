# Step 0442 — implementation plan

S13.1's pulled-source rows (spec 0050 S13.1; 001 §C, §D). Every stored version under `SourcePullProvenance` gets
its `PulledSource` row, derived from the version's provenance the way its facet rows are derived from its envelope.
The import seam then answers the versions a source's records minted.

## Approach

1. **Domain** (`CatalogueImport.fs`):
   - Before `CatalogueImportProxy`, add `PulledRecord = { version : LibraryVersionRef; pull : SourcePullInfo }`.
   - Add one static member on it that applies the order of (1): record text ordinally, then the entry GUID's text
     ordinally, then ascending version. Both stores call it, so the order is written once.
   - `CatalogueImportProxy` gains `pulledRecordsOf : MaterialSourceName -> Result<PulledRecord list, CatalogueImportError>`.
   - `createInMemory` answers it from every envelope in its map, whatever the entry's lifecycle. Each record's
     `version` is the envelope's existing `libraryVersionRef`.
2. **Storage** (`LibraryDatabaseStore.fs`):
   - `LibraryEntryEnvelope.pulledSourceRows : LibraryPulledSourceRow list`, beside `facetRows` and carrying the
     `StorageBoundary` mark. It answers one row for `SourcePullProvenance info`, none otherwise.
   - `stageVersion` makes the version's pulled-source row exactly the envelope's:
     - the row is keyed by `(EntryId, Version)`, so a tracked row is updated in place;
     - a Deleted one is restored first, as facet rows are;
     - a version with no pulled row loses any it had.
   - `stageRemoval` is unchanged; the cascade already removes the rows.
3. **Storage** (`CatalogueImportStore.fs`): `createInDatabase` answers `pulledRecordsOf` in one `runImport`:
   - It reads the `PulledSources` rows of the source's text and loads each version through `tryStoredVersion`.
   - A missing version, or a provenance that is not a pull of that source, is `LibraryRowInvalidErr` naming the entry
     and the version.
   - Every `StorageError` is mapped through the existing `storageResult` to `ImportStoreUnavailableErr`.
   - The answer is ordered by the shared member.
4. **Tests**:
   - `CatalogueImportProxyTests.recordingProxy` answers `Ok []`.
   - `CatalogueImportContract` gains the pulled-records fact, so it runs over both stores.
   - `LibraryDatabaseTests` gains the pulled-source-row fact.

## Files

- `OpticalConstructor.Domain/CatalogueImport.fs`
- `OpticalConstructor.Storage/LibraryDatabaseStore.fs`
- `OpticalConstructor.Storage/CatalogueImportStore.fs`
- `OpticalConstructor.Tests/CatalogueImportProxyTests.fs`
- `OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs`
- `OpticalConstructor.Tests/LibraryDatabaseTests.fs`

## Risks

- **EF identity map.** Removing a tracked row and then adding another with the same key throws. The pulled row's key
  is its version's, so a changed row is updated with `SetValues`, never re-added.
- **The existing removal fact** adds a `PulledSource` row by hand to a built-in version. It adds the row after
  `stageVersion`, so the new maintenance does not touch it.
- **The ordering must not depend on SQLite's collation or on `Guid` comparison.** It is sorted in F#, ordinally.
