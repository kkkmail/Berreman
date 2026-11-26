# Step 0448 — impl plan

**Scope.** Tests only. One new file, `OpticalConstructor.Tests/MaterialSourcePullSqliteTests.fs` (`module
MaterialSourcePullSqliteTests` in `namespace OpticalConstructor.Tests`), appended last in the test project's compile
list. It proves step 444's pulls end to end over step 447's real refractiveindex.info adapter and the real SQLite
library database (`:memory:`), as obligation S131-PULLS-SQLITE asks. No product file changes.

**Composition, per fact.**

- `use database = BuiltinCatalogue.freshLibraryDatabase ()`, disposed at the fact's end.
- The import context is `CatalogueImportContext.create (CatalogueImportProxy.createInDatabase database)`.
- The material store is `MaterialProxy.createInDatabase database`.
- The adapter is `MaterialSourceProxy.createRefractiveIndexInfo` over `MaterialSourceFixture.freshSources
  "MaterialSourcePullSqliteTests"`. Its snapshot is written by `writeSnapshot` with `siPage ()`. Its downloader is a
  `copyingDownloader` of an archive that `writeArchive` writes beside the folder. Its clock is `fixedClock` of
  2026-11-04. Nothing reaches the network.

**The four facts, as the brief names them.**

1. Pull → `PullAddedEntry` at version 1. Then check:
   - the material store's `tryGetMaterial` (name and physics);
   - `tryLatestEnvelope`'s `SourcePullProvenance` and its `UserManaged` protection;
   - the one `PulledSources` row, read through `run`;
   - `attributionLinesOf`.
2. Unchanged update → `SourceRefreshed` (2026-11-04, one `PullFoundUnchanged` at version 1). `resolveVersion` of
   version 2 answers `None`, and one row remains.
3. Changed update → `PullMintedVersion` at version 2. Then check:
   - version 2's physics is the changed page's import (n 3.790 at 0.70 µm), under the same name;
   - its provenance is 2026-11-04 with the changed hash;
   - version 1 is unchanged and the latest is 2;
   - there are two rows, versions 1 and 2.
4. A second pull → `PullFoundUnchanged` at version 1, and the entry holds one version.

**Choices.**

- **The pulled entry id.** The context's `newEntryId` is set to one fixed GUID, as `MaterialSourcePullTests` does.
  The pull's answer is then fully deterministic, and the material store is reached through `MaterialId.toLibraryEntryId`
  without re-wrapping a GUID.
- **The page hash** is `ContentHash.ofBytes` of the page text's UTF-8 bytes, which are exactly what the fixture and
  the archive write.
- **Rows are compared as elevated values.** Each `LibraryPulledSourceRow` is mapped at the read into a private record
  of elevated values: `LibraryVersionRef`, source, record, version, licence and citation.

**Risks.** The `PulledSources` rows must be materialised inside `run`, because the context is disposed afterwards.
SQLite `:memory:` lives on the one open connection, so every proxy shares one database.
