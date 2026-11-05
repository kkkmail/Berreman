# Step 0026 — IMPLEMENT — impl plan

## Approach

The catalogue manifest model (001 §A "Catalogue folder layout"; .spec-md §5), all in
`OpticalConstructor.Domain/LibraryEntryFiles.fs`, the file the step authorizes:

1. `CatalogueManifestRow = { kind; id; version; fileName; contentHash }`, declared just before
   `LibraryEntryEnvelope`, so the envelope's `manifestRow` is an intrinsic member (a cross-type
   reuse towards an earlier type).
2. `LibraryEntryError` gains `DuplicateEntryFileNameErr` and `DuplicateEntryVersionErr`, both
   `of reason : string`, the reason naming both entries (name, id, version).
3. `CatalogueManifest = { schemaVersion; id; name; version; entries }`, declared after the
   envelope, with `currentSchemaVersion` (1), `extension` (`.catalogue-json`, the one place the
   text is written), `fileName` (`(FileStem.ofText "").withExtension extension`) and `tryCreate`.
   `tryCreate` refuses a repeated id + version first (the more specific diagnosis: a duplicated
   entry also repeats its file name), then two entries of one kind whose file names are equal under
   `StringComparison.OrdinalIgnoreCase`; the rows are sorted by kind tag, then
   `String.CompareOrdinal` on the file name. The manifest's own hash is not stored.

Tests in `LibraryEntryFilesTests` (the three the step names) over envelopes with test-local
64-character hex hashes.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs`

## Risks

- Record field-label inference: the row repeats `kind` / `id` / `version` / `fileName` /
  `contentHash` and the manifest repeats `id` / `name` / `version`. Only `LibraryEntryFilesTests`
  opens the module; every construction there is annotated.
- ELEV: new type and case names checked unique against Berreman and STL (none found).
