# Step 0307 — implementation plan

Sniffing and the preview for the column-mapping wizard (spec 0050 S9.2; 001 §K, §O).

## Approach

1. **One decode in `Kit.fs`.**
   - Move `FileByteContent.kitFileText` from `KitFolders.fs` into `Kit.fs`, after `LabFileSource`. Its body is
     unchanged: decode as UTF-8, then drop a leading mark.
   - Add `FileByteContent.byteOrderMark`: `ByteOrderMarkPresent` when the bytes start with EF BB BF.
   - The text-level mark drop it uses (`KitFileText.withoutByteOrderMark`) moves with it, as an intrinsic member of
     `KitFileText`. One rule for dropping the mark stays, and every caller keeps it.
   - `KitFolders.fs` keeps `kitDataFile`, which now calls the moved decode.
   - `KitFolderStore.fs`: the lock read's own `StreamReader` decode (BOM detection) is deleted. It reads the stream's
     bytes, with the same share mode, and calls `kitFileText`. The guard (0050 .manual/009) is untouched.
2. **`ImportProfiles.fs`, after `ColumnRoles.tryOfChoices`.**
   - Add the types `LabFileSniff`, `PreviewCell`, `PreviewRow` and `LabFilePreview`.
   - Add `LabFileSource.sniffedLayout`. The mark comes from the bytes. The delimiter and header are read over the first
     ten non-empty lines of `kitFileText`, exactly by the slice's rule.
3. **`LabFileImport.fs`.** Add `KitFileText.previewWith delimiter header` through the private `labFileRows` (step 302).
   - Column names are the header's trimmed cells, or `Column <n>` across the widest of the first twenty rows.
   - Rows are the first twenty data rows. `EmptyDataFileErr` and the read's errors pass through.
4. **Tests.**
   - `ImportProfileTests`: the sniff fact over the slice's four texts.
   - `LabFileImportTests`: the preview fact over step 303's Phone ROI text, decoded as the wizard reads it, with and
     without a header, plus a thirty-row text.

## Files

- `OpticalConstructor.Domain/Kit.fs`, `KitFolders.fs`, `ImportProfiles.fs`
- `OpticalConstructor.Storage/KitFolderStore.fs`, `LabFileImport.fs`
- `OpticalConstructor.Tests/ImportProfileTests.fs`, `LabFileImportTests.fs`

## Risks

- **ELEV001.** `PreviewCell cell.value` over a `DataFileCell` is a call-site re-wrap. The preview builds each cell from
  its trimmed text, as the mapping reads it (`mapText` trims every cell), so no re-wrap is written.
- **Record inference.** `LabFileSniff`'s fields are a subset of `ImportProfile`'s. Any unannotated literal declared
  later could change its inferred type, and the build will show it.
- **Lock read.** Text is unchanged for UTF-8, which is all the app writes. `StreamReader`'s UTF-16/32 detection is
  gone, as the single decode requires.
- **Fixture.** The slice says "five column names" of step 303's Phone ROI text, but that header has ten columns. This
  plan follows the fixture and records the choice.
