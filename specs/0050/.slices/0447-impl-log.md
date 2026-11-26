# Step 0447 — impl log

## Progress

- [x] the stamp schema `material-source-snapshot.schema.json` and `SchemaValidation.validateSourceSnapshot`
- [x] `MaterialSourceStore.fs`:
  - `SourceSnapshotStamp`;
  - `SourceSnapshotText`;
  - `SourceArchiveUrl`;
  - `SourceArchiveDownloader` and its `http`;
  - `MaterialSourceProxy.createRefractiveIndexInfo`
- [x] the `MaterialSourceProxy` doc line in `MaterialSources.fs`
- [x] `MaterialSourceFixture.fs` and `MaterialSourceStoreTests.fs`: the class passes 8 of 8
- [x] `SVC_BigBoy_0002` set to `implemented`, implement step 447, in `specs/0050/.contracts-json`
- [x] build: exit 0, 48 s, then 46 s on the final source
- [x] constructor-unit-tests: 1791 of 1791, 109 s, then 110 s on the final build
- [x] elevation: the first run found 1 new finding (ELEV022). After the fix, the re-run answers
  `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 232 s.
- [x] the 21 s43 invariants: no findings
- [x] state-of-the-world written

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/schema/material-source-snapshot.schema.json` (new)
  - Draft 2020-12, `$id` `https://berreman/schema/material-source-snapshot.json`, in `kit-history.schema.json`'s
    shape.
  - `schemaVersion` is always checked through `library-common`'s `positiveVersion`.
  - Under `if` it is 1, `then` requires exactly `schemaVersion`, `source` and `sourceVersion`. The last two are
    strings with `minLength` 1.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/SchemaValidation.fs`
  - `validateSourceSnapshot` over `validateWith`.
  - Its schema loads once behind a `lazy` through `tryLoadLibrarySchema`, as `validateKitHistory`'s does, so the
    `$ref` resolves.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
  - One `<Compile Include="MaterialSourceStore.fs" />`, directly after `MaterialImport.fs` (see Decisions).
  - One `Content` / `PreserveNewest` item after `kit-history.schema.json`'s.
  - No comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/MaterialSourceStore.fs` (new), `module MaterialSourceStore`:
  - `SourceSnapshotStamp` with `currentSchemaVersion` (1), `extension` (`.snapshot-json`) and `fileName`;
  - `SourceSnapshotText` with `.value`;
  - `fileText`, which is `nodeOf` rendered by `canonicalFileText`;
  - `parseStamp`, which is `parseVersionedText validateSourceSnapshot …`;
  - `SourceArchiveUrl`;
  - `[<ReferenceEquality>] [<WireCarrier>] SourceArchiveDownloader`, with the marked `static member http`;
  - private conversions, each a member on its source type:
    - `KitFileText.toSourceSnapshotText`;
    - `KitFileText.toRefractiveIndexInfoCatalogText`;
    - `KitFileText.toRefractiveIndexInfoText`;
    - `KitArchiveFileCount.toSourceSnapshotFileCount`;
    - `KitArchiveProgress.toSourceSnapshotProgress`;
    - `ClockTime.sourceSnapshotVersion`;
    - `MaterialSourceName.snapshotFolderIn`;
  - `MaterialSourceProxy.createRefractiveIndexInfo`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MaterialSources.fs`: the doc of `MaterialSourceProxy`. Its
  second line now reads "Storage's `createRefractiveIndexInfo` is the refractiveindex.info adapter, `createInMemory`
  the double." No code changes.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceFixture.fs` (new), `module MaterialSourceFixture`:
  `siPage`, `glassPage`, `changedSiPage`, `freshSources`, `writeSnapshot`, `writeArchive`, `copyingDownloader` and
  `fixedClock`, each with a one-line doc. A private `databaseFiles` lists the three database files once, for
  `writeSnapshot` and `writeArchive`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/MaterialSourceStoreTests.fs` (new), `module
  MaterialSourceStoreTests` (class `OpticalConstructor.Tests.MaterialSourceStoreTests`), with the eight facts the
  brief names (ADD_TEST).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: the two `Compile` lines,
  appended in that order at the end of the compile list, with no comment.
- `specs/0050/.contracts-json`: `SVC_BigBoy_0002`'s lifecycle `declared` → `implemented`, and `implementStep`
  `null` → `447`. The declaration metadata and consumers are kept.

## Decisions

- **The operator rulings R0001 to R0005 bind (0050 `.manual/030`, `031`, `033`, `035` and `036`, the "Binding ruling"
  sections).** None of them names this step's files. This step leaves untouched the behaviour they keep:
  - the series dispatch (R0001);
  - the analyzer-slot law (R0002);
  - AT-1's and AT-2's injected element identities (R0003);
  - the single evaluation, the concurrent residual and the fit bound (R0004 and R0005);
  - the single `rhoT` read and the server collector (R0005).

  No file they authorize is edited.
- **Compile order deviates from the brief.** The brief says `MaterialSourceStore.fs` is "compiled directly after
  `KitArchiveStore.fs`". It also says the module opens `MaterialImport`. `MaterialImport.fs` compiles three files
  after `KitArchiveStore.fs`, after `GroupsLibrary.fs` and `Sidecar.fs`, and F# cannot open a module that compiles
  later.
  - The one `Compile` line therefore sits directly after `MaterialImport.fs`, the earliest place where every open
    the brief names resolves.
  - Moving `MaterialImport.fs` instead would reorder an unrelated module that the brief does not ask to move.
- **ELEV022 (the wire guard) requires a `[<WireCarrier>]`.** The first elevation run reported one new finding:
  `ELEV022 MaterialSourceStore.fs:84:36 - System.Net.Http.HttpClient.Send puts bytes on a wire outside a
  [<WireCarrier>] module`.
  - STL's `RawSinkOnlyInCarrier` accepts a raw sink only when the enclosing top-level member, its type or its
    module carries `Softellect.Sys.WireCarrierAttribute`.
  - The attribute targets classes only, so it cannot go on the `http` property. It goes on its type,
    `SourceArchiveDownloader`, the declaration that owns the one HTTP seam. That follows AGENTS.md's "a mark goes
    on the declaration, never on a module".
  - The carrier's public surface carries no plaintext. `http : SourceArchiveDownloader` has one field function,
    over `SourceArchiveUrl`, `FileName`, `CancellationToken` and `Result<unit, MaterialSourceError>`: elevated
    single-case unions, a token and `unit`. ELEV022's surface half therefore passes. The URL is `https://`, so
    ELEV023 does not apply.
  - The brief's `[<ElevationBoundary(nameof IoBoundary, …)>]` on `http` is kept, word for word.
  - No analyzer, `.elevation-analyzers.json` or known-findings entry is touched.
- **The install's commit point is the move-in.** The held snapshot moves to `.old`, then `.tmp/<top>` moves to the
  snapshot folder. Once that second move succeeds, the update has installed the new snapshot and answers
  `SourceSnapshotUpdated`.
  - Deleting `.old`, `.tmp` and the zip afterwards is cleanup. A failure there is swallowed, as `KitArchiveStore`
    keeps a job's own answer when its cleanup fails, and the next run's first step deletes any leftover.
  - Rolling back after the commit point could move back a `.old` that a failed recursive delete had half emptied.
  - A failed move-in moves `.old` back. `.old` can exist at that point only if this run moved it there, because
    the run's first step deletes any leftover `.old`.
- **What a run made is deleted on every answer.**
  - The zip and `.tmp` go after a download error and after a cancellation, as well as after errors and
    exceptions later in the run.
  - The brief's "a download error is answered as it is" is kept: the error is returned unchanged, and only the
    run's partial zip is removed.
  - The sources folder itself is created before the download, because the downloader writes into it. It is not
    deleted, since it is the configured folder, not the run's.
- **Error mapping.**
  - `listZipEntries` errors are `SourceArchiveInvalidErr`, as the brief says, and so is an archive whose entries
    are not one top folder holding `database/catalog-nk.yml`.
  - `extractKitZip` errors are `SourceSnapshotIoErr` naming the snapshot folder: writing the snapshot failed.
  - The stamp's render and write errors are `SourceSnapshotIoErr`.
  - Every exception is `SourceSnapshotIoErr` naming the folder (or the file read) and the message.
- **Literals are written once.**
  - The source name, licence and archive URL are each written once, in `createRefractiveIndexInfo`.
  - The snapshot folder is the source name's (`MaterialSourceName.snapshotFolderIn`).
  - `.zip.tmp`, `.tmp` and `.old` are suffixes of the snapshot folder.
  - `database/catalog-nk.yml` and `database/data` are one `KitRelativePath` each, shared by the reads and the
    update's entry selection.
  - The citation's prefix is `sourceName.value`, the same text as the brief's `"refractiveindex.info, <record>"`.
- **The version is `ClockTime.value` formatted `yyyy-MM-dd` in the invariant culture,** with no
  `ToUniversalTime`. `ClockTime` is UTC by its own contract ("An instant supplied in UTC by the injected clock"),
  and `KitLockHolder.describe` formats it the same way.
- **ELEV001 and ELEV005 shapes.**
  - A cross-type move (a `KitFileText` into a stamp, catalog or page text; a `KitArchiveFileCount` or progress into
    the snapshot's; a `ClockTime` into a version; a `MaterialSourceName` into a folder) is a private member on the
    source type, which these rules sanction as `Ctor this.value`.
  - Paths are born elevated from `Path.Combine` or interpolated strings, never by re-wrapping another type's
    `.value`.
  - The `string`-speaking IO lives in member bodies (locals are not inspected) or in the marked `http`.
- **The fetch looks up the hit before checking its path.** A record the hits do not list, and one whose text
  `KitRelativePath.tryCreate` refuses, both answer `SourceRecordMissingErr`. `../x.yml` is caught by the hit lookup
  first, and the path check also guards a catalog that lists such a record.
- **Tests extended within their facts.**
  - The no-snapshot fact also checks the source's name and that the reads left the sources folder empty
    ("building it touches nothing").
  - The search fact compares each search with `query.filter` of the fixture catalog's `hits`, the shared rule
    itself.
  - The update fact checks the snapshot's top-level names and its `database/` names (no `tools`, no `README.md`),
    the new stamp's version and the changed page's hash.
  - The refused-archive and failed-download facts also check that the sources folder holds nothing else.
  - The schema-version fact also checks that `searchRecords` refuses with the same error.
  - No test identity was added beyond the eight the census names.

## Testing state

The development runs used each gate's own command and selection. The supervisor's gate engine re-runs the roster and
is the authority.

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`):
  - exit 0, 48 s against 600 s; after the final edits, exit 0, 46 s;
  - the log holds no `error` text;
  - the 6 warnings are all NU1902, which is exempt.
- **`OpticalConstructor.Tests.MaterialSourceStoreTests`** alone: 8 of 8, on both builds.
- **constructor-unit-tests** (`all_discovered`, the whole suite, the gate's command with `--report-trx`):
  - `Passed!`, 1791 of 1791, 109 s against 180 s;
  - on the final build, `Passed!`, 1791 of 1791, 110 s;
  - the count rose by 8 from the last checkpoint's 1783, as the acceptance requires.
- **elevation** (ELEV004 excluded, all 13 projects, no comparison base):
  - first run: `ELEV-GATE: FAILED - 1 new finding(s)`, ELEV022 on `HttpClient.Send`, 213 s. It was fixed with the
    `[<WireCarrier>]` mark (see Decisions);
  - re-run on the final source: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 232 s against 600 s; Storage 0 findings.
- **The twenty-one s43 invariants:** each decoded `git grep` finds nothing. The four new, still untracked files,
  scanned with the same patterns, find nothing too.
- No development run exceeded its gate's effective deadline.

## Artifacts

- `specs/0050/.artifacts/0447-dev-build.log`
- `specs/0050/.artifacts/0447-dev-constructor-unit-tests.log`
- `specs/0050/.artifacts/0447-dev-elevation.log` (the ELEV022 run)
- `specs/0050/.artifacts/0447-dev-build-2.log`
- `specs/0050/.artifacts/0447-dev-constructor-unit-tests-2.log`
- `specs/0050/.artifacts/0447-dev-elevation-2.log`

## Gotchas

- **`HttpClient` anywhere outside a `[<WireCarrier>]` fails elevation (ELEV022).** The build does not show it. The
  carrier mark is class-only, so it goes on the type that owns the call, never on the member.
- **`MaterialImport.fs` compiles after `KitArchiveStore.fs`.** A Storage module that opens both must compile after
  `MaterialImport.fs`.
- **The test literals must use the fixtures' exact code points:**
  - `–` is U+2013;
  - `µ` is U+00B5;
  - `Si.yml`'s em dash is U+2014.

  `MaterialSourceStoreTests.fs` was checked: it holds exactly those three.
- **An interrupted run's `.old` is deleted at the next run's start, as the brief says.** If a process died between
  the two moves, the held snapshot is in `.old`, and the next update deletes it before downloading. The snapshot
  is a re-downloadable cache, so nothing that cannot be fetched again is lost.
