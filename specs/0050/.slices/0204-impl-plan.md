# Step 0204 — impl-plan (IMPLEMENT: a kit's lab file read whole, as bytes)

## Approach

`KitProxy` (`KitFolders.fs`) gains one field, `readLabFile : KitRelativePath -> Result<LabFileSource, KitError>`,
the file at a kit-relative path as its absolute name in the kit and its bytes as stored (S6's copy carries bytes,
which `readDataFile`'s decoded text cannot: it drops a byte-order mark and cannot hold a binary file).

- **In memory** (`InMemoryKitFolder.createKitProxy`): a private member `readInMemoryLabFile` answers
  `FileName "<kitFolder>/<path>"` and the UTF-8 bytes of the stored text, or `MissingKitFileErr` naming the path.
  `readInMemoryKitDataFile` becomes that read plus `FileByteContent.kitDataFile`, so the in-memory double has one
  read body and its hash is, by construction, `ContentHash.ofBytes` of `readLabFile`'s content.
- **On disk** (`KitProxy.create`, `KitFolderStore.fs`): a local `readLabSource` joins the path's segments under the
  kit folder with the platform separator, reads the bytes whole, maps `FileNotFound` / `DirectoryNotFound` to
  `MissingKitFileErr` and every other exception to `KitIoErr` through `tryKitFolderIo`, exactly as `readDataFile`
  did. `readDataFile` becomes `readLabSource` plus `kitDataFile`, so the read exists once.
- **`readOnlyView`** passes `readLabFile` through (it writes nothing).

No other `KitProxy` record is built in full: the test and KitLibrary records are copy-and-update expressions.

## Tests (KitProxyTests.fs)

- `KitProxyContract`: `a lab file is read whole by its kit-relative path, with its absolute name in the kit` —
  runs under `KitProxyTests` and `KitProxyFolderTests` (+2).
- `KitProxyFolderTests`: `a lab file's bytes are read as stored, a byte-order mark included` (+1).
- Count 1261 → 1264.

The on-disk `absoluteName` adapter is `Path.Combine(folder, path)`; for `data/raw/run 1.csv` on Windows that keeps
`/` separators, while the proxy (by the slice) names the file with the platform separator. The adapter normalises
`/` to the platform separator before combining, so `absoluteName` is the platform name of the kit-relative path.
Every existing use of `absoluteName` either goes through `tryKitPathOf` (which normalises) or opens the file, so
no existing assertion changes.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitFolders.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitFolderStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitProxyTests.fs`

## Risks

- The shared lock guard (0050 `.manual/009`) is in `KitFolderStore.fs`; this step does not touch it.
- ELEV: the new local closure and private member carry only elevated types; no new `.value` outside boundary lines
  beyond the existing path joins.
