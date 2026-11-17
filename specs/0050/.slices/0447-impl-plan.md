# Step 0447 — impl plan

## Approach

Step 0447 implements `SVC_BigBoy_0002`, the refractiveindex.info `MaterialSourceProxy`. It works over an offline
snapshot of the GitHub database, held under `<sources>/refractiveindex.info/` beside its `.snapshot-json` stamp.

- **The stamp's schema.** A new `schema/material-source-snapshot.schema.json` follows the shape of
  `kit-history.schema.json`:
  - `schemaVersion` is checked unconditionally through the common `positiveVersion` definition;
  - only version 1's body is validated.

  `SchemaValidation.validateSourceSnapshot` loads it once, behind a `lazy`, through `tryLoadLibrarySchema`.
- **The store.** A new `MaterialSourceStore` module holds the following, in order:
  - `SourceSnapshotStamp`, with `currentSchemaVersion`, `extension` and `fileName`;
  - `SourceSnapshotText`, with `fileText` and `parseStamp`. Both use the shared `nodeOf`, `canonicalFileText` and
    `parseVersionedText`;
  - `SourceArchiveUrl` and `SourceArchiveDownloader`. `SourceArchiveDownloader.http` is the one marked HTTP seam;
  - `MaterialSourceProxy.createRefractiveIndexInfo`.
- **Reads.**
  - Every file is decoded through `FileByteContent.kitFileText`.
  - The catalog's hits come from `RefractiveIndexInfoCatalogText.hits`. A search applies `MaterialSourceQuery.filter`.
  - A fetch parses the page through `RefractiveIndexInfoText.importMaterial` and takes its citation from
    `referencesOpt`. Its hash is `ContentHash.ofBytes` of the page's bytes.
- **Update.**
  - It first deletes the leftovers of an interrupted run, then downloads into `refractiveindex.info.zip.tmp`.
  - The zip's entries are listed through `KitArchiveProxy.create ()`'s `listZipEntries`. The update keeps the one
    top folder's `database/catalog-nk.yml` and everything under `database/data/`, and extracts them through
    `extractKitZip` into `refractiveindex.info.tmp`. Each `KitArchiveProgress` is reported as a
    `SourceSnapshotProgress`.
  - The stamp is written into the staged top folder through `writeBytesAtomically`.
  - Installing moves the held snapshot to `.old`, then moves the staged folder into place. The update then deletes
    `.old`, `.tmp` and the zip.
  - On a failure or a cancellation it deletes what this run made and moves `.old` back if it was moved.
- **The domain doc.** `MaterialSourceProxy`'s doc names `createRefractiveIndexInfo` beside `createInMemory`, in one
  line.
- **Tests.**
  - `MaterialSourceFixture` holds the page texts, `freshSources`, `writeSnapshot`, `writeArchive`,
    `copyingDownloader` and `fixedClock`.
  - `MaterialSourceStoreTests` holds the eight facts the brief names.
  - Both files are appended at the end of the compile list.
- The contract registry entry `SVC_BigBoy_0002` becomes `implemented` at step 447.

## Files

- `OpticalConstructor.Storage/schema/material-source-snapshot.schema.json` (new)
- `OpticalConstructor.Storage/SchemaValidation.fs`
- `OpticalConstructor.Storage/MaterialSourceStore.fs` (new)
- `OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: one `Compile` line and one `Content` item
- `OpticalConstructor.Domain/MaterialSources.fs`: the doc line only
- `OpticalConstructor.Tests/MaterialSourceFixture.fs` (new)
- `OpticalConstructor.Tests/MaterialSourceStoreTests.fs` (new)
- `OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: two `Compile` lines
- `specs/0050/.contracts-json`: the lifecycle

## Risks

- **Compile order.** The brief places `MaterialSourceStore.fs` "directly after `KitArchiveStore.fs`". It also opens
  `MaterialImport`, which compiles three files after `KitArchiveStore.fs`. The store must therefore compile after
  `MaterialImport.fs`, so it goes directly after that file. The deviation is recorded in the impl-log.
- **ELEV rules.** These apply to every new member and to the private conversions:
  - cross-type conversions are named members on the source type, never call-site re-wraps;
  - the `string`-typed IO lives in member bodies or in the marked `http` seam.
- **The install's commit point.** Once the staged folder has moved into place, the update has succeeded. The
  deletion of `.old`, `.tmp` and the zip that follows is cleanup, and the next run's first step repeats it.
  Rolling back after that point could restore a half-deleted `.old`.
