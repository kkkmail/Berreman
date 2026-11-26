# Step 0463 — implementation plan

Obligation `S132-AGF-IMPORT`: a dropped Zemax glass catalogue (`.agf`) goes through the one dropped-file import path.
Each glass is decided against the library as an entry under `DroppedFileOrigin` and `FileImportProvenance`. Nothing is
written until the import dialog's Apply (spec 0050 S13.2; 001 §B, §D; .spec-md §3.7, §8, §10.3).

## Approach

1. **Domain, `CatalogueImport.fs`.**
   - Before `CatalogueImportContext`, add `GlassCatalogueRead = { file : FileName; fileHash : ContentHash; glasses : MaterialEntry list }`.
   - Add the context field `readGlassCatalogue : FileName -> Result<GlassCatalogueRead, CatalogueImportError>`.
   - In `decideDroppedFiles` step (a), an `.agf` file (ordinal, ignoring case, through `file.tryGetExtension ()` as
     `LibraryEntryKind.tryOfFileName` reads it) is read through `readGlassCatalogue`.
   - Each glass is sealed as `{ LibraryEntryContent.ofMaterialEntry VersionNumber.first (FileImportProvenance …) glass with id = this.newEntryId () }`,
     in file order. This is the shape of the step-444 pull.
   - A read or seal error is a `DroppedFileRefusedErr` that joins the other refused files.
   - Every other file is read as today. Steps (b)–(d) run unchanged over every envelope in drop order.
2. **Storage, `CatalogueImportStore.fs`.** `create` (signature unchanged, already `IoBoundary`) fills `readGlassCatalogue`:
   - `File.ReadAllBytes` inside `try`, an exception becoming `DroppedFileRefusedErr "<file>: <message>"`;
   - `ContentHash.ofBytes`, `file.leafName` and `(GlassCatalogueText.ofBytes bytes).importGlasses`, an `ImportError`
     becoming `DroppedFileRefusedErr "<file>: <describe>"`.
3. **Compile order.** `MaterialImport.fs` (step 462's reader) compiles after `CatalogueImportStore.fs` in
   `OpticalConstructor.Storage.fsproj`, so `create` cannot see it.
   - Fix: move the one `MaterialImport.fs` line up to just before `CatalogueImportStore.fs`.
   - `MaterialImport.fs` opens no Storage module, so nothing else moves.
   - This is a compile-only deviation outside the effects list (precedent: steps 0313, 0460), recorded in the log.
4. **Tests.** Three new `[<Fact>]` members of `CatalogueImportContract`, each running over `CatalogueImportInMemoryTests`
   and `CatalogueImportSqliteTests` (+6). Each:
   - imports and applies the Built-in catalogue;
   - copies `fixtures/test-glasses.agf` into `freshImportFolder ()`;
   - mints ids from a fixed sequence of test-local GUIDs.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/CatalogueImport.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CatalogueImportStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CatalogueImportInMemoryTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj` (one compile line moved; deviation)

## Risks

- Record-label inference: `GlassCatalogueRead` shares `file` / `fileHash` with `EntryFileImportInfo`, so both
  constructions are annotated.
- `observeStartupImport` pins `newEntryId` to one id, so the tests set the sequence after observing.
- The existing refusal fact (`notes.txt`, the missing `.material-json`) must keep its exact reason text and its read
  order. Non-`.agf` files keep today's path unchanged.
