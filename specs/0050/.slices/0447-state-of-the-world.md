# Where we are

Step 0447 is S13.1's fourth step (obligation S131-RII-ADAPTER, contract `SVC_BigBoy_0002`). The earlier steps laid
the groundwork:

- step 443 declared the `MaterialSourceProxy` seam and its in-memory double;
- step 444 added the pulls;
- step 445 taught `MaterialImport` to read refractiveindex.info's catalog and references;
- step 446 added the material-sources folder setting.

This step adds the real refractiveindex.info adapter, `MaterialSourceProxy.createRefractiveIndexInfo`, in
`OpticalConstructor.Storage/MaterialSourceStore.fs`. It works over an offline snapshot of the GitHub database:
`database/catalog-nk.yml` and `database/data/**`, under `<sources>/refractiveindex.info/` beside its `.snapshot-json`
stamp. The contract is now `implemented`.

# What's working

- Added the refractiveindex.info adapter, `MaterialSourceProxy.createRefractiveIndexInfo`, over an offline snapshot
  of the database stamped by `.snapshot-json`. It answers the snapshot's version, searches the catalog with the
  shared rule, and fetches a page through `MaterialImport` with its hash, the stamp's version, CC0 and its citation.
- Added "Update snapshot": it downloads the database's archive through `SourceArchiveDownloader`, extracts the
  catalog and data through the app's one zip reader (`KitArchiveProxy`), reporting and reading the cancellation per
  file, and replaces the snapshot whole. A failed or cancelled update keeps the held snapshot and leaves nothing
  behind.
- Added the stamp's schema, `material-source-snapshot.schema.json`, validated on read; another schema version is
  `UnsupportedSnapshotSchemaErr`.
- Made `SourceArchiveDownloader.http` the one HTTP seam: a marked `[<WireCarrier>]` that no test calls.
- Added `MaterialSourceFixture` and the eight `MaterialSourceStoreTests` facts. constructor-unit-tests went from
  1783 to 1791.

# Tests

The development runs used each gate's own command and selection. The supervisor's gate engine re-runs the roster
and is the authority.

- **build:** exit 0, 48 s against 600 s; on the final source, exit 0, 46 s.
  - The log holds no `error` text.
  - The 6 warnings are all NU1902, which is exempt.
- **elevation** (ELEV004 excluded, all 13 projects):
  - The first run failed with one new finding: ELEV022, `HttpClient.Send` outside a `[<WireCarrier>]`.
  - The fix marks `SourceArchiveDownloader` `[<WireCarrier>]` (see Architecture).
  - Re-run on the final source: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 232 s against 600 s.
- **constructor-unit-tests** (`all_discovered`, the whole suite): `Passed!`, 1791 of 1791, 109 s against 180 s.
  On the final build it passed again, 1791 of 1791, in 110 s.
  - The count rose by 8 from the last checkpoint's 1783, as the acceptance requires.
  - `OpticalConstructor.Tests.MaterialSourceStoreTests` passes 8 of 8:
    - `a refractiveindex.info source without a snapshot answers no version and refuses search and fetch`;
    - `a snapshot's catalog is searched with the shared rule`;
    - `a fetched page is MaterialImport's material under its hit's name, with the page's hash, the stamp's version,
      CC0 and its citation`;
    - `an update downloads the archive and replaces the snapshot with its catalog and data, file by file`;
    - `an update cancelled before its next file keeps the held snapshot`;
    - `an archive without the database catalog is refused and the held snapshot kept`;
    - `a failed download keeps the held snapshot`;
    - `a snapshot stamp of another schema version is refused`.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing. The four new, still untracked files,
  scanned with the same patterns, find nothing too.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1791
```

# Architecture

- **One module holds the adapter's IO.** `MaterialSourceStore` holds the stamp (`SourceSnapshotStamp`,
  `SourceSnapshotText`), the URL (`SourceArchiveUrl`), the download seam (`SourceArchiveDownloader`) and
  `createRefractiveIndexInfo`. Every .NET IO and HTTP exception is caught there and mapped to `MaterialSourceError`.
- **Reuse, not copies:**
  - the stamp renders through `nodeOf` and `canonicalFileText`;
  - it reads through `parseVersionedText` and its own schema, with no up-converter and no fallback reader;
  - files decode through `FileByteContent.kitFileText`;
  - the catalog's hits and a page's material and citation come from `MaterialImport`;
  - the search rule is `MaterialSourceQuery.filter`;
  - the hash is `ContentHash.ofBytes`;
  - the stamp is written by `writeBytesAtomically`;
  - the zip is listed and extracted by `KitArchiveProxy.create ()`. No second extractor is written, and every entry
    is a `KitRelativePath`, so none escapes.
- **The update replaces the snapshot whole.** In order:
  1. delete an interrupted run's `.zip.tmp`, `.tmp` and `.old`;
  2. download into `refractiveindex.info.zip.tmp`;
  3. keep only `<top>/database/catalog-nk.yml` and everything under `<top>/database/data/`;
  4. extract into `refractiveindex.info.tmp`;
  5. write the stamp of the clock's UTC date into `.tmp/<top>`;
  6. move the held snapshot to `.old` and `.tmp/<top>` into its place.

  The move-in is the commit point: deleting `.old`, `.tmp` and the zip afterwards is cleanup. A failed move-in moves
  `.old` back, and every other failure deletes what the run made.
- **`SourceArchiveDownloader` is the online sources' one wire carrier (ELEV022).** It carries
  `[<WireCarrier>]` on its type, because the mark is class-only, and its `http` member keeps the brief's
  `[<ElevationBoundary(nameof IoBoundary, …)>]`.
  - `http` sends a GET with no body (`ResponseHeadersRead` and the token) and streams its answer into the file.
  - The carrier's public surface holds only elevated types, so the guard's surface half passes.
  - Tests pass a downloader that copies a zip they wrote.
- **No stored format changes,** except the new `.snapshot-json` stamp at schema version 1. It has no migration: any
  other version is a typed error. No package is added, and there is no retry, cache, mirror, second URL or setting.

# Deferred

Nothing.

# Gotchas

- **An `HttpClient` call outside a `[<WireCarrier>]` passes the build but fails elevation (ELEV022).** The carrier
  mark is class-only, so it goes on the type that owns the call.
- **`MaterialSourceStore.fs` compiles directly after `MaterialImport.fs`, not directly after `KitArchiveStore.fs` as
  the brief says.** It opens `MaterialImport`, which compiles three files after `KitArchiveStore.fs`.
- **The test literals must use the fixtures' exact code points:** `–` is U+2013, `µ` is U+00B5, and `Si.yml`'s em
  dash is U+2014.
- **An interrupted run's `.old` is deleted when the next update starts, as the brief says.** If a process died
  between the install's two moves, the held snapshot lies in `.old` and is lost. It is a re-downloadable cache.

# Changelog

- 2026-11-16, step 0447: added `MaterialSourceStore` with the refractiveindex.info adapter
  `MaterialSourceProxy.createRefractiveIndexInfo`, its `.snapshot-json` stamp and schema, and the
  `[<WireCarrier>]` download seam `SourceArchiveDownloader.http`; added `MaterialSourceFixture` and eight
  `MaterialSourceStoreTests` facts; `SVC_BigBoy_0002` is implemented; constructor-unit-tests went from 1783 to 1791.
