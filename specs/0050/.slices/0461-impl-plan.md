# Step 0461 — impl plan

**Goal.** An entry imported from a user's glass-catalogue file records which file, and which bytes
of it, it came from (spec 0050 S13.2): `EntryProvenance` gains its seventh case,
`FileImportProvenance of EntryFileImportInfo`, with `EntryFileImportInfo = { file : FileName; fileHash : ContentHash }`.
The glass import that builds the case is step 463; this step lands the case, its storage, its schema
shape and its tests.

## Files

- `OpticalConstructor.Domain/LibraryEntryFiles.fs` — the case and its `and EntryFileImportInfo`
  record (docs cite S13.2); `protection` → `UserManaged`, `categoryOrigin` → `UserCategory`,
  `attributionOpt` → `None`.
- `OpticalConstructor.Storage/LibraryDatabaseStore.fs` — `kindValue` 7, `catalogueIdOpt` `None`,
  `pulledSourceRows` `[]` (a complete match FS0025 reports).
- `OpticalConstructor.Storage/schema/library-common.schema.json` — a `fileImportProvenance` def
  (`file` non-empty string, `fileHash` → `#/$defs/contentHash`) in the `provenance` `oneOf`; the
  schemas stay version 1.
- FS0025 fallout per the how_to ("any other complete match … answers a file import as it answers
  `UserProvenance`"): `OpticalConstructor.Domain/CatalogueImport.fs` (`pulledRecordsOf` → `None`)
  and `OpticalConstructor.Storage/CatalogueImportStore.fs` (the pulled-source row check → its error).
  Both are outside `effects.edits`; the how_to's clause authorizes them and the impl-log records it.
- Tests: `LibraryEntryFilesTests` (the two named facts widened with the new case),
  `EntryFileJsonTests` (`every provenance round-trips through an entry file` gains a material under it).
  No test identity added, renamed or removed.

## Risks

- The canonical JSON options unwrap single-case unions, so `FileName` and `ContentHash` write as
  plain strings; the schema shape must match exactly or the schema validation in the round trip fails.
- ELEV: the new record has two fields of elevated types (no primitive); the new case name is two
  words and unique (`FileImportProvenance` — check no other DU case shares it).
