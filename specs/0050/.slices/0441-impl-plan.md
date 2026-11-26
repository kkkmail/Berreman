# Step 0441 — implementation plan

A pulled library version records the hash of the record it was built from (spec 0050 S13.1; 001 §B, §C, §D).
Later, updating a source can then mint a version only where a record changed.

## Approach

1. **Domain** (`OpticalConstructor.Domain/LibraryEntryFiles.fs`):
   - `SourcePullInfo` gains `recordHash : ContentHash` directly after `record`, with a one-line field doc citing
     S13.1.
   - `ContentHash` is declared about 120 lines later in the same file, so it moves, unchanged, to just above
     `SourcePullInfo`.
   - Nothing else changes: no provenance case, no member, and `attributionOpt`, `protection` and `categoryOrigin`
     stay as they are.
2. **Schema** (`OpticalConstructor.Storage/schema/library-common.schema.json`):
   - The `sourcePullProvenance` object gains the required key `recordHash`, `$ref` `#/$defs/contentHash`.
   - The entry schemas stay at version 1, as step 405 did.
   - The database model and `LibrarySchemaVersion.current` (1) are untouched. Its payload is the file text, so it
     carries the hash through the file.
3. **Construction sites.** All three set the field, and no assertion changes:
   - `BuiltinCatalogue.licensedMaterialContent` hashes its record id's text (`test/<name>`).
   - The literal in `LibraryEntryFilesTests` hashes a test-local text.
   - The literal in `EntryFileJsonTests` hashes a test-local text.
4. **New fact** in `EntryFileJsonTests`:
   - Name: `a pulled provenance keeps its record's hash through an entry file, and a file without it is refused`.
   - Silica under `SourcePullProvenance` with `recordHash` = SHA-256 of `abc`. Its known digest is asserted.
   - It round-trips content → `sealed` → `fileText` → `parseEntry`, and the decoded provenance holds the hash.
   - The same text with the provenance's `recordHash` key removed answers `SchemaValidationErr`.
   - It sits after `assertSchemaInvalid`, so it can use the existing `editedText` and `assertSchemaInvalid` helpers.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/library-common.schema.json`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/BuiltinCatalogue.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/EntryFileJsonTests.fs`

## Risks

- **Committed files holding a pulled provenance would now fail the schema.** `git grep` finds none outside the
  tests (`data/` and `Catalogue/` hold none).
- **The new record field changes the binary shape of `SourcePullInfo`.** All three constructions are in the tests;
  no product code builds one yet.
- **Moving `ContentHash`** keeps its body and docs byte for byte; only its position in the file changes.
