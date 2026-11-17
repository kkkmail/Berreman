# Step 0455 — impl plan (PTB OCDB adapter, spec 0050 S13.2, obligation S132-PTB-ADAPTER)

## Approach

1. **Move the snapshot body** out of `createRefractiveIndexInfo` in
   `OpticalConstructor.Storage/MaterialSourceStore.fs` into three private shapes:
   - `SourceSnapshotLayout = { source; folder }` with members `snapshotVersion`, `heldVersion`, `installFrom` and
     `updateFromArchive` (plus private helpers for the sibling paths `.zip.tmp`, `.tmp`, `.old` and the stamp file);
   - `SourceArchiveEntryChoice` = `KeepArchiveEntry` | `DropArchiveEntry`;
   - `SourceArchiveSpec = { url; requiredFile; keep }`, paths relative to the archive's one top folder.

   `createRefractiveIndexInfo` builds its layout (`<sources>/refractiveindex.info`) and its spec
   (`database/catalog-nk.yml` required; it and every file under `database/data/` kept) and calls them. Search and
   fetch keep their catalog and page reads. S13.1's eight facts are unchanged.
2. **Add `MaterialSourceProxy.createPtbOcdb`** beside it:
   - layout `<sources>/ptb-ocdb`, `MaterialSourceName "PTB OCDB"`, `EntryLicence "CC-BY-4.0"`;
   - archive `https://github.com/PTB-SR/ocdb/archive/refs/heads/master.zip`;
   - spec requires `ocdb/db/COPYING` and keeps it plus the `.txt` files directly in `ocdb/db/data/`;
   - search: one hit per `.txt` file of `ocdb/db/data/`, ordinal order, `name = "<stem> (PTB OCDB)"`;
   - fetch: listed hit only, bytes hashed, `PtbOcdbText` import (step 454) renamed to the hit, citation with
     `headlineOpt`.
3. **Domain doc** of `MaterialSourceProxy` names `createPtbOcdb`.
4. **Tests:**
   - fixture `ptbRecord`, `changedPtbRecord`, `writePtbSnapshot` and `writePtbArchive`. The zip writer and the
     snapshot writer are shared with the refractiveindex.info fixture, not copied.
   - The four `MaterialSourceStoreTests` facts the step names.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialSourceStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialSources.fs` (doc only)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceFixture.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceStoreTests.fs`

## Risks

- **ELEV012:** a lambda mapping the two-case `SourceArchiveEntryChoice` to `bool` (a `List.filter`) could be flagged,
  so the entry selection uses `List.choose`.
- **ELEV014 / 016 / 019:** the new names were checked against Berreman and STL sources and are free.
- **Archive folder entries:** only file entries are now kept. The extractor creates each file's parent folders, so
  the snapshot's files are unchanged. An empty data folder of the archive is no longer recreated, and a git archive
  holds none.
- **`.old` deletion:** it moves from `updateSnapshot` into `installFrom`'s "leftovers deleted". The behaviour is the
  same.
