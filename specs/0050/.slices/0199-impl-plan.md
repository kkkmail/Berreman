# Step 0199 — impl plan (IMPLEMENT_CONTRACT FILE_BigBoy_0001 KitArchiveProxy)

## Approach

Implement the real `KitArchiveProxy.create ()` over System.IO.Compression in a new
`OpticalConstructor.Storage/KitArchiveStore.fs` (`module KitArchiveStore`). It is compiled directly after
`KitHistoryStore.fs`, with one `<Compile>` line and no comment. The four fields follow step 197's contract.

- **`listKitTree`**: enumerates `Directory.EnumerateFileSystemEntries(…, AllDirectories)`. Each name is made
  kit-relative and forward-slashed, then read through `KitRelativePath.tryCreate`. The result is sorted ordinally. A
  missing folder is `KitArchiveIoErr` naming it.
- **`writeKitZip`**: streams into `<zip>.tmp` (a `FileStream` under a `ZipArchive` in Create mode).
  - Entry names come from `zipEntryName` alone.
  - Before each file it checks cancellation, and after each file it reports progress.
  - It disposes the archive, then calls `File.Move(tmp, zip, true)`.
  - A cancellation or any exception deletes the `.tmp`. An exception is `KitArchiveIoErr` naming the file or the zip.
- **`listZipEntries`**: `ZipFile.OpenRead`, each `FullName` elevated to `ZipEntryName` at the interop line, then
  `KitTreeEntry.tryOfZipEntryName`. The first refusal wins. IO and `InvalidDataException` failures are
  `KitArchiveIoErr`.
- **`extractKitZip`**:
  - A target that holds anything is `KitZipTargetNotEmptyErr`.
  - It remembers the outermost folder it creates for the target.
  - A file is written with `FileMode.CreateNew` from `archive.GetEntry(zipEntryName)`; a missing entry is
    `KitArchiveIoErr`.
  - A cancellation or any exception removes what it created and answers `KitArchiveCancelled` or `KitArchiveIoErr`.

Places on disk are joined from a path's `/` segments under the kit folder or target, by two private members on
`KitFolder`.

## Reuse

The double's local `filePaths` and `progressOf` would otherwise be copied into the store. They move to Domain members,
`KitTreeEntry.filePathsOf` and `KitArchiveProgress.afterFiles`, and both proxies call them ("Reuse, never copy").
`KitArchive.fs` is in this step's edits. The `KitArchiveProxy` doc names `KitArchiveProxy.create` beside the double
in one line.

## Tests

`KitArchiveProxyFolderTests` goes in `KitArchiveProxyTests.fs` and inherits `KitArchiveProxyContract`, so the 11
contract facts run over a real folder under `AppContext.BaseDirectory`. It adds four facts of its own:

- an escaping zip entry is refused;
- a sent kit received elsewhere and reopened equals the original;
- the zip's entries are forward-slashed;
- a zip that cannot replace its file is refused and leaves no `.tmp`.

That is 15 new identities, the census.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitArchiveStore.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/OpticalConstructor.Storage.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitArchive.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveProxyTests.fs`
- `specs/0050/.contracts-json` (`FILE_BigBoy_0001` → `implemented`, step 199)

## Risks

- **ELEV010 on primitives beside our types.** Keep every signature elevated. Local helpers take `FileName`,
  `FolderName` and `KitRelativePath`.
- **`use` inside a recursive loop** would keep streams open across iterations. Each copy goes in its own function.
- **On Windows, `File.Move` over a zip held with `FileShare.None`** must throw, so the replace test can see the
  failure.
