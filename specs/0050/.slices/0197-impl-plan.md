# Step 0197 — impl plan (ADD_CONTRACT FILE_BigBoy_0001 KitArchiveProxy)

## Approach

Declare the zip / unzip seam in `OpticalConstructor.Domain/KitArchive.fs`, after `KitName.zipFileName` (step 196),
in the slice's order (1)–(9):

1. `KitArchiveError` (five `of reason : string` cases ending in `Err`) with `describe` in step 39's form.
2. `type KitTreeEntry with` `tryOfZipEntryName` (trailing `/` = folder; the rest through
   `KitRelativePath.tryCreate`, whose refusal becomes `KitZipEntryEscapesErr` naming the entry as written) and
   `zipEntryName`.
3. `KitArchiveFileCount of int` and `KitArchiveProgress`.
4. `[<ReferenceEquality>] KitArchiveObserver` with `unobserved` (`ignore`, `CancellationToken.None`).
5. `KitArchiveOutcome` = `KitArchiveFinished of files` | `KitArchiveCancelled`.
6. `KitZipJob`, `KitUnzipJob`.
7. `[<ReferenceEquality>] KitArchiveProxy` with exactly `listKitTree`, `writeKitZip`, `listZipEntries`,
   `extractKitZip`, each with a one-line doc.
8. `InMemoryArchiveDisk`: private state (one map from a case-folded, forward-slashed name to the name as placed and
   what it holds: a file's bytes, a folder, or a zip's entries with each file's bytes). Public surface exactly
   `create`, `placeFile`, `placeFolder`, `tryDiskFile`, `paths`. A few `internal` members (item lookup, place, remove,
   items under a folder) are the state's boundary, so the proxy's closures in (9) mutate it only through the disk's
   members, as `InMemoryKitFolder` keeps its own.
9. `KitArchiveProxy.createInMemory disk`: the four closures. `listKitTree` locates names through
   `KitFolder.tryKitPathOf`. `writeKitZip` checks the token before each file and reports after it, and stores the zip
   only at the end. `extractKitZip` refuses a non-empty target, creates the target, writes entries, and on
   cancellation or failure removes every name it created (the target and its new parents included).

Name normalisation lives in module-private extension members on `FileName` / `FolderName` / `KitFolder` (unique
names, so ELEV019 is not touched).

## Tests

- New `OpticalConstructor.Tests/KitArchiveProxyTests.fs`, appended last in the compile list: the test-local
  `KitArchiveTestDisk` record, `sampleKit`, `[<AbstractClass>] KitArchiveProxyContract` with the six facts, and
  `KitArchiveProxyTests` over `InMemoryArchiveDisk.create ()` rooted at `C:/disk`.
- `KitArchiveTests.fs` gains the two facts (zip entry names; error rendering).
- Expected count: 1220 + 8 = 1228.

## Risks

- Record field name overlap (`zip`, `entries` in both job records): annotate job values in tests.
- ELEV: no new module-level functions; every helper is a member or a local closure; no DU rendered raw.
- A real disk (step 199) cannot write a zip into a missing folder, so the contract facts place `out` before zipping.
  That keeps the inherited facts valid on a real disk.
