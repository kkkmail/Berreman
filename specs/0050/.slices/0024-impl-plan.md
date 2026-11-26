# Step 0024 — IMPLEMENT — impl plan

## Approach

Add one new Domain file, `OpticalConstructor.Domain/LibraryEntryFiles.fs` (`module LibraryEntryFiles`
in `namespace OpticalConstructor.Domain`), compiled directly after `ElementId.fs`. It declares the
identities, provenance and name rule of the library-entry files (001 §A, §B; .spec-md §3.11, §5):

1. `LibraryEntryKind` (six two-word cases, int wire tag 1..6, `tryOfWire`, `extension`,
   `currentSchemaVersion`, `all`).
2. `FileSchemaVersion of int`, `EntrySchema = { kind; version }`.
3. `LibraryEntryId of Guid`, then type extensions `MaterialId.toLibraryEntryId`,
   `SampleId.toLibraryEntryId`, `CategoryId.toLibraryEntryId` and
   `ElementValueId.tryToLibraryEntryId`. No reverse conversion (ELEV006).
4. `CatalogueId of Guid`, `CatalogueName of string`, `KitId of Guid`.
5. Provenance: `MaterialSourceName`, `MaterialSourceRecordId`, `MaterialSourceVersion`,
   `EntryLicence`, `EntryCitation` (each `of string`), the `SourcePullInfo` record and the
   `EntryProvenance` union with `protection` and `categoryOrigin`.
6. `ContentHash of string`.
7. `FileStem of string` with `ofText` (replaces `: * ? " < > | / \` with `_`) and `withExtension`,
   plus the `EntryName.fileStem` type extension.

Tests: new `OpticalConstructor.Tests/LibraryEntryFilesTests.fs` (`module LibraryEntryFilesTests`)
with the four facts the slice names, appended after `CanonicalJsonTests.fs`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/LibraryEntryFiles.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj` (one `<Compile>` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/LibraryEntryFilesTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `<Compile>` line)

## Risks

- Name uniqueness (ELEV014/015/016/019) across Berreman and STL: each new type, case and extension
  member name was grepped across both trees and none exists.
- ELEV001: `LibraryEntryId this.value` is written only inside a member of the source type, which
  the analyzer sanctions (`NoCallSiteRewrap.fs`).
- The s04 invariants: no `kit.kit-json`, `EpsAxisEvaluated`, `complexity = None/Some` or old string
  preset id in any new file. The test's non-GUID id avoids the `src-`/`det-`/`pol-` patterns.
