# Step 0444 — impl plan

**Anchor:** IMPLEMENT (S13.1, contract obligation S131-PULLS). A pulled record becomes a user material
version under `SourcePullProvenance`; updating a source re-fetches each pulled entry's record and mints
the next version only where the record's hash changed.

## Approach

1. `OpticalConstructor.Domain/CatalogueImport.fs`: open `MaterialSources` (compiled just before it,
   step 443). `CatalogueImportContext` gains `materialSources : MaterialSourceProxy list` with a
   one-line doc.
2. `OpticalConstructor.Storage/CatalogueImportStore.fs`: `CatalogueImportContext.create` sets
   `materialSources = []`. Its signature does not change. The other construction sites are record
   updates (AppContext, the UI tests, StudyPublishingTests), so they compile unchanged.
3. New `OpticalConstructor.Domain/MaterialSourcePulls.fs`, compiled between `CatalogueImport.fs` and
   `KitLibrary.fs`. It declares, in the brief's order:
   - `MaterialSourcePullError` (three `*Err` cases, each with `describe`);
   - `SourcePullChange` (four documented cases) and `SourcePullResult`;
   - `SourceRefreshInfo` and `SourceRefresh`;
   - the extension on `CatalogueImportContext`: `pullRecord`, `refreshPulls` and
     `updateSourceSnapshot`.
4. Shared private helpers carry the one refresh rule:
   - choose each entry's newest pulled row, in `PulledRecord.inRecordOrder`, paired with its held
     entry;
   - split the retired entries off;
   - refresh one entry: absent, unchanged, or minted.

   `pullRecord` and `refreshPulls` both call them, so the rule is written once. The write path is the
   existing `sealEntry` plus `proxy.putEntryVersion` with `MoveLatestVersion`, as fork and Promote do.
   No library verb is added.
5. Tests: new `OpticalConstructor.Tests/MaterialSourcePullTests.fs` (`module MaterialSourcePullTests`),
   appended at the end of the compile list. It holds the census's 8 facts, over the in-memory import
   proxy (Built-in imported and applied), with a fixed `newEntryId` and step 443's double.

## Risks

- **Accessibility of private helpers inside an optional type extension.** Use module-level
  `let private` functions with two or more parameters (ELEV013 concerns single-parameter free
  functions).
- **Retired entries.** `putEntryVersion` with `MoveLatestVersion` re-activates an entry in the
  in-memory store. The retired check must therefore run before any write.
- **The cancelled fact needs two published records.** The double reads the cancellation before each
  record, so with only one record the cancel would land too late (443 gotcha).
- **A pulled row whose entry the library does not hold** is a contradiction in either store. Its
  refusal is `PullImportErr (ImportRejectedErr …)`, the same refusal the brief gives for a missing
  latest envelope.
