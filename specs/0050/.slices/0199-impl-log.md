# Step 0199 — impl log (IMPLEMENT_CONTRACT FILE_BigBoy_0001 KitArchiveProxy)

## Progress

- [x] Domain: moved the double's file-path and progress helpers to members, and named `create` in the proxy doc
- [x] Storage: `KitArchiveStore.fs` with `KitArchiveProxy.create ()`, compiled after `KitHistoryStore.fs`
- [x] Tests: `KitArchiveProxyFolderTests` (11 contract facts + 4 of its own)
- [x] `.contracts-json`: `FILE_BigBoy_0001` → `implemented`, `implementStep` 199 (declaration metadata kept)
- [x] Development runs: build, constructor-unit-tests, elevation, the fourteen s22 invariants

Probe (before coding): `ZipArchive.CreateEntry` stores a name as given, so `..\evil.txt` reads back verbatim as
`FullName` (`.artifacts/0199-zip-entry-name-probe.log`). The escape refusal can therefore name the entry as written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitArchiveStore.fs` (new): `module KitArchiveStore`.
  - Two private `KitFolder` members, `archiveFolderAt` / `archiveFileAt`, join a path's `/` segments under a folder
    through `FolderName.combine` / `FileName.combine`.
  - `type KitArchiveProxy with static member create () : KitArchiveProxy`.
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`: one `<Compile>` line after
  `KitHistoryStore.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitArchive.fs`:
  - `KitTreeEntry.filePathsOf` and `KitArchiveProgress.afterFiles`, moved out of `createInMemory`'s locals;
  - the double now calls them;
  - one doc line naming `KitArchiveProxy.create` beside `createInMemory`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveProxyTests.fs`:
  - the record `KitArchiveSentKit`, used by test data only;
  - the private module `KitArchiveFolderTestData`;
  - `type KitArchiveProxyFolderTests () = inherit KitArchiveProxyContract ()` with its four facts.
- `specs/0050/.contracts-json`: `FILE_BigBoy_0001` is `implemented` at step 199.

## Decisions

- **Reuse, never copy.** The real proxy needs the double's `filePaths` and `progressOf` bodies, so they became Domain
  members both proxies call, `KitTreeEntry.filePathsOf` and `KitArchiveProgress.afterFiles`. `KitArchive.fs` is in
  this step's edits, and the double's behaviour is unchanged.
- **`writeKitZip`** streams into `<zip>.tmp`: a `FileStream` in `FileMode.Create` under a `ZipArchive` in Create mode.
  - A folder entry is `CreateEntry(zipEntryName)`. A file entry is `CreateEntry(zipEntryName, Optimal)`, filled from
    `File.OpenRead`.
  - Cancellation is checked before each file and progress reported after each.
  - Once the archive is disposed, `File.Move(tmp, zip, true)` puts the zip in place.
  - A cancellation or failure deletes the `.tmp`; a failure is `KitArchiveIoErr` naming the file or the zip.
  - Each file's copy is its own function, so its streams close before the next file (no `use` across the recursion).
    The recursion is a self tail call.
- **`extractKitZip` remembers what it created.**
  - A missing target: it keeps the outermost missing folder of the target, so `received` in
    `received/Optics QZ` too. Cleanup deletes that folder recursively, as the double removes "the folders it made to
    hold the target".
  - An existing target: it was checked empty, so cleanup removes everything now inside it, which is exactly what
    this extraction made. The double's `removeCreated` uses the same rule (everything new under the target).
- **Elevation at origin.** `ZipEntryName zipEntry.FullName` is bound at the interop line and then read through
  `KitTreeEntry.tryOfZipEntryName`. No signature carries a bare primitive, so no `ElevationBoundary` mark was needed.
- **Error reasons** name the kit folder (`the kit folder '…'`), the file (`the file '…'`), the zip (`the zip '…'`) or
  the target folder (`the folder '…'`), then the exception's message.

## Testing state

Development runs only; the gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors. The only warnings are the two
  vendored `MathNetNumerics/Numerics/Exceptions.cs` SYSLIB0051 warnings, already on the start tree (steps 194–198
  record them). Log: `.artifacts/0199-build-01.log`.
- **constructor-unit-tests**: `Test run summary: Passed!`, total 1248, succeeded 1248, failed 0, skipped 0. That is
  1233 + 15, the census. The TRX lists all 15 `KitArchiveProxyFolderTests` identities as Passed: the 11 inherited
  contract facts plus `a zip entry that would escape the folder is refused and nothing is extracted`,
  `a sent kit, received elsewhere and reopened, equals the original kit file by file`,
  `the zip's entries are forward-slashed kit-relative names` and
  `a zip that cannot replace its file is KitArchiveIoErr and leaves no temporary file`. Log:
  `.artifacts/0199-constructor-unit-tests-01.log`.
- **elevation** (`ASG_ARC_BASE_COMMIT=f395597…`, `-ExcludeRules ELEV004`): `ELEV-GATE: OK - 0 new finding(s) in 12
  project(s)`. Storage shows findings=0. Log: `.artifacts/0199-elevation-01.log`.
- **The fourteen `invariant-s22-*` greps**: each finds nothing.
- **Line endings**: every touched file is LF. `git diff --numstat` equals `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0199-zip-entry-name-probe.log`
- `specs/0050/.artifacts/0199-build-01.log`
- `specs/0050/.artifacts/0199-constructor-unit-tests-01.log`
- `specs/0050/.artifacts/0199-elevation-01.log`

## Gotchas

- `ZipArchive.GetEntry` is ordinal and case-sensitive, and the slice requires it over `zipEntryName`.
  `tryOfZipEntryName` reads `\` as `/` (through `KitRelativePath.tryCreate`), so a foreign zip that stores
  `data\x.csv` lists as `data/x.csv`. Its extraction then finds no entry and answers `KitArchiveIoErr` naming it.
  Our own zips are always forward-slashed, so a sent kit never meets this.
- The folder tests' roots live under `AppContext.BaseDirectory/KitArchiveProxyFolderTests/<guid>`, never `%TEMP%`, as
  in `KitProxyFolderTests`.
- `KitArchiveSentKit` has `kit` and `zip` fields, like `KitZipJob`. Job literals stay annotated (`: KitZipJob`).
