# Step 0199 — state of the world (IMPLEMENT_CONTRACT FILE_BigBoy_0001 KitArchiveProxy)

## Where we are

Spec 0050, section S6 (send and receive kits).

- Step 196 added the send rule.
- Step 197 declared the `KitArchiveProxy` seam with its in-memory double and contract facts.
- Step 198 added `sendKit`, `receiveKit` and `KitZipPicker` over the seam.

This step implements the contract `FILE_BigBoy_0001` for real. The new Storage `KitArchiveProxy.create ()` works over
System.IO.Compression and passes the same `KitArchiveProxyContract` on a real disk. The contract is now `implemented`.
Step 200 adds the job host and the Avalonia save / open dialogs.

## What's working

- Add `KitArchiveProxy.create ()` in Storage: real kit zips over System.IO.Compression, streamed through `<zip>.tmp` and
  moved into place only when whole
- Extraction refuses a non-empty target and every escaping entry; a cancelled or failed job removes what it created
- Every .NET IO and compression exception is caught at the seam and mapped to `KitArchiveIoErr` naming the file, zip or folder
- Share `KitTreeEntry.filePathsOf` and `KitArchiveProgress.afterFiles` between the double and the real proxy
- Add `KitArchiveProxyFolderTests`: the 11 contract facts on real folders plus 4 of its own (constructor-unit-tests 1233 → 1248)

## Tests

Development runs; the gate engine owns the verdict (Invariant 6). Details are in `0199-impl-log.md`.

- `build`: exit 0, 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 warnings already on the start
  tree.
- `constructor-unit-tests`: 1248 passed, 0 failed, 0 skipped. All 15 `KitArchiveProxyFolderTests` identities pass in
  the TRX.
- `elevation` (from the arc origin `f395597…`): `ELEV-GATE: OK`, 0 new findings in 12 projects.
- The fourteen `invariant-s22-*` greps find nothing.

```yaml
gates:
  constructor_unit_tests: 1248
```

## Architecture

- **The real proxy is one Storage extension**, `KitArchiveStore`, beside `createInMemory`. It does IO only, and
  `sendKit` / `receiveKit` (step 198) compose the kit's rule above it for both proxies.
- **Places on disk** come from two private `KitFolder` members. They join a `KitRelativePath`'s `/` segments through
  `FolderName.combine` / `FileName.combine`. Every entry is a `KitRelativePath`, so no place can lie outside the
  folder, and no second check exists.
- **Zips are atomic without being held in memory.** A zip streams into `<zip>.tmp` and goes in place with
  `File.Move(tmp, zip, true)`, the same write-then-replace rule as `FileName.writeBytesAtomically`.
- **Extraction remembers what it created.** That is the outermost folder it made to hold the target, or, for a target
  it found empty, the target's contents. A cancellation or failure removes exactly that.

## Deferred

Nothing. The job host and the Avalonia save / open dialogs that fill `KitZipPicker` are step 200's, a separate step of
the plan.

## Gotchas

- `ZipArchive` stores an entry name as given (`..\evil.txt` reads back verbatim, see
  `.artifacts/0199-zip-entry-name-probe.log`), so the escape refusal names the entry as written.
- `GetEntry` is ordinal. A foreign zip storing `data\x.csv` lists as `data/x.csv`, and its extraction then answers
  `KitArchiveIoErr` because the entry is not found. Our own zips are always forward-slashed.
- A zip can be written only into a folder that exists. The folder tests place `out` / `exports` first, as the
  contract facts do.

## Changelog

- 2026-11-09 — Step 0199:
  - implemented `KitArchiveProxy.create ()` (Storage `KitArchiveStore.fs`) over System.IO.Compression;
  - moved the double's file-path and progress helpers to Domain members;
  - added `KitArchiveProxyFolderTests` (15 facts);
  - marked `FILE_BigBoy_0001` `implemented`.
