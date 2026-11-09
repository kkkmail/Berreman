# Step 0197 — impl log (ADD_CONTRACT FILE_BigBoy_0001 KitArchiveProxy)

## Progress

- [x] Read the protocol, project prompt, slice, AGENTS.md, step 196's `KitArchive.fs`, `KitFolders.fs` (step 125's
  `InMemoryKitFolder` boundary), `Kit.fs` (`KitRelativePath.tryCreate`, `KitFolder.tryKitPathOf`) and
  `JobRunner.fs:runLoop`. Read steps 198 and 199 only to make sure the contract facts carry over to them.
- [x] Red: tests written first. The test project failed on the undefined seam with 162 FS0039 errors and nothing else
  (`0197-red-build.log`).
- [x] Seam declared in `KitArchive.fs` after `KitName.zipFileName`, in the slice's order (1)–(9).
- [x] First elevation run: one new finding, ELEV010 on `tryOfZipEntryName (name : string)`
  (`0197-elevation-first.log`). Fixed by elevating the entry name (see Decisions). Second run `ELEV-GATE: OK`.
- [x] Development runs: build, constructor-unit-tests, elevation and the fourteen s22 invariant greps.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitArchive.fs`:
  - `open System.Threading`.
  - New `KitArchiveError` and `ZipEntryName`, and `KitTreeEntry.tryOfZipEntryName` / `zipEntryName`.
  - New `KitArchiveFileCount`, `KitArchiveProgress`, `KitArchiveObserver` (`unobserved`), `KitArchiveOutcome`,
    `KitZipJob`, `KitUnzipJob` and `KitArchiveProxy`.
  - The in-memory double: the internal `ArchiveDiskItem`, `InMemoryArchiveDisk` and
    `KitArchiveProxy.createInMemory`. Private extension members name the disk's paths.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveProxyTests.fs` (new):
  - the `KitArchiveTestDisk` record and `sampleKit`;
  - `[<AbstractClass>] KitArchiveProxyContract` with the six facts;
  - `KitArchiveProxyTests` over `InMemoryArchiveDisk.create ()`, rooted at `C:/disk`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveTests.fs`: two new facts, one on zip entry names and
  one on error rendering.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `KitArchiveProxyTests.fs`
  appended last in the compile list, with no comment.

All four files are in the step's authorized `edits`. `.contracts-json` already records `FILE_BigBoy_0001` as
`declared` at step 197, so the registry needed no edit.

## Decisions

- **`ZipEntryName` (deviation from the slice's signature, forced by ELEV010).** The slice writes
  `tryOfZipEntryName (name : string) : Result<KitTreeEntry, KitArchiveError>`. The elevation gate rejects that
  signature: ELEV010 fires because the string sits beside `KitArchiveError`. Its analyzer states that no boundary mark
  exempts it, and `tryOfZipEntryName` is not one of the sanctioned elevation-pattern names. The analyzers win over any
  document (0048 `.manual/002` R6). So the entry name is elevated as `ZipEntryName of string` with `.value`:
  - `tryOfZipEntryName (name : ZipEntryName)` and `zipEntryName : ZipEntryName` keep the slice's two symbols and their
    meaning;
  - the error still names the entry as written;
  - the name is born once, in `zipEntryName`, so the in-memory `listZipEntries` round-trips
    `tryOfZipEntryName entry.zipEntryName` with no re-wrap;
  - step 199's Storage wraps `ZipArchiveEntry.FullName` at the interop line, and reads `.value` for `CreateEntry`.
- **The double's state boundary.** `InMemoryArchiveDisk`'s public surface is exactly `create`, `placeFile`,
  `placeFolder`, `tryDiskFile` and `paths`. Its state is one private map: a case-folded, forward-slashed key to the
  name as first placed and what it holds (a file's bytes, a folder, or a zip's entries with each file's bytes). Four
  `internal` members are the only way to change that state: look up, place (with every holding folder), remove, and
  list what lies under a folder. `createInMemory` builds the four fields over those members, so the state is still
  mutated only inside the disk's members, as `InMemoryKitFolder` keeps its own.
- **Folders exist explicitly.** Placing anything makes every folder holding it an item. A folder stays after its files
  are removed, as on a disk. The drive or root segment (`C:`, a leading `/`) is never an item.
- **Cancellation and failure cleanup.** `extractKitZip` snapshots the disk's names before it writes anything. On
  cancellation or failure it removes every name that is new since then and is the target, inside the target, or a
  folder it made to hold the target. `writeKitZip` collects the bytes and stores the zip only after the last file, so
  a cancelled or failed job leaves a zip already at that name as it was.
- **Progress and cancellation** follow `JobRunner.runLoop`: the token is read before each FILE entry (folders are not
  steps), and progress is told after each file. The total is the job's file entries.
- **A zip name held by a folder** is `KitArchiveIoErr` (a real `File.Move` onto a folder fails too). So is a target
  name held by a file.
- **The contract facts place `out` before zipping** (`zipUnderOut`). A real disk cannot write a zip into a missing
  folder, and a save dialog always names one that exists. With `out` placed, the inherited facts stay valid for step
  199's real store, and "`paths` of `out` hold no zip" is checkable there.
- Job values in tests are annotated (`: KitZipJob`, `: KitUnzipJob`) because both records carry `zip` and `entries`.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine.

- **Red first:** 162 FS0039 errors, all on the undefined seam (`0197-red-build.log`).
- **Build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exits 0, with 0 warnings and 0 errors
  (`0197-build.log`).
- **constructor-unit-tests:** 1228 passed, 0 failed, 0 skipped (`0197-constructor-unit-tests.log`). That is
  checkpoint 195's 1220 + 8. The TRX (`0197-testresults/constructor-unit-tests.trx`) lists all eight declared identities
  as Passed:
  - six under `OpticalConstructor.Tests.KitArchiveProxyTests`;
  - two under `OpticalConstructor.Tests.KitArchiveTests`.
- **Elevation**, with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`): `ELEV-GATE: OK`, 0 new findings in 12 of 12
  projects, 0 known findings, ELEV004 excluded (`0197-elevation.log`).
  - Domain's suppressed (ELEV004-only) count moved from 314 to 328. The new `.value` reads are the reasons in the
    double's errors (FileName has no render member) and the path text a zip entry name is made from.
  - ELEV004 is measured, not gated (0048 `.manual/002` R7).
- **The fourteen s22 invariant greps:** none finds anything.
- **Line endings:** every touched file is LF with no BOM (0 CR bytes). `git diff --numstat` matches
  `--ignore-cr-at-eol`.

## Gotchas

- `tryOfZipEntryName` takes a `ZipEntryName`, not a `string`; see Decisions. Steps 198–199 call it with
  `entry.zipEntryName` or with `ZipEntryName <archive entry FullName>`.
- `KitRelativePath.tryCreate` turns `\` into `/` before checking. So `a\..\..\b.txt` is refused for its `..` segments,
  and a benign `data\x.csv` would read as `data/x.csv`.
- The in-memory `listKitTree` lists a zip held inside the kit as a file, but `tryDiskFile` answers `None` for it,
  because the double keeps a zip as entries, not bytes. Zipping a kit that holds such a zip is `KitArchiveIoErr`
  naming it. Step 198's `KitZipInsideKitErr` keeps the kit's own zip out of a send.

## Artifacts

- `specs/0050/.artifacts/0197-red-build.log`: the red build.
- `specs/0050/.artifacts/0197-build.log`: the green solution build.
- `specs/0050/.artifacts/0197-constructor-unit-tests.log` and `0197-testresults/constructor-unit-tests.trx`.
- `specs/0050/.artifacts/0197-elevation-first.log` (the ELEV010 finding) and `0197-elevation.log` (OK).
