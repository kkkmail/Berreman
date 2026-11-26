# Step 0168 — IMPLEMENT_CONTRACT STORE_BigBoy_0005 KitHistoryProxy — impl-plan

## Approach

Add the real folder store behind the Domain `KitHistoryProxy` record (declared at step 165, its JSON text and
schema at step 167). In `KitHistoryStore.fs`, after the existing text members, a `type KitHistoryProxy with static
member create (preferences : FolderName) : KitHistoryProxy`. It only computes the file path
(`KitHistory.fileName.combine preferences`); every IO call happens inside the three closures.

- `readHistory`: a missing file, an IO failure reading the bytes, or a text `parseKitHistory` refuses each logs one
  `Logger.logWarn` naming the file and the cause and answers `KitHistory.empty`. The bytes are decoded by the
  existing `FileByteContent.kitFileText` (UTF-8, one leading BOM dropped; `KitFolders.fs`), bridged to
  `KitHistoryText` by a private named member on `KitFileText`, so no second decoder is written. A read never
  writes, moves or deletes anything.
- `writeHistory`: render `history.fileText` first (a render failure touches nothing), then create the preferences
  folder, then write the UTF-8 bytes (no BOM) through the shared `FileName.writeBytesAtomically` (step 126). A
  render or write `StorageError` becomes `KitHistoryWriteErr` naming the file and `describe`; an exception creating
  the folder becomes `KitHistoryWriteErr` naming the folder and the message.
- `probeFolder`: `Directory.Exists` on the folder's text, mapped to `KitFolderPresent` / `KitFolderMissing`.

The `KitHistoryProxy` doc in `KitHistory.fs` names `KitHistoryProxy.create` beside `createInMemory` in one line.

## Tests

In `KitHistoryProxyTests.fs`, `KitHistoryProxyFolderTests` inherits `KitHistoryProxyContract` (four inherited facts)
over a per-instance root `Path.Combine(AppContext.BaseDirectory, "KitHistoryProxyFolderTests", <GUID>)`:
`<root>/Preferences` (not created), `<root>/kits/present` (created), `<root>/kits/missing` (never created). Four
own facts exactly as the slice names them. Eight new identities in total.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitHistoryStore.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs` (doc line only)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryProxyTests.fs`
- `specs/0050/.contracts-json` (lifecycle `implemented`, implementStep 168)

## Risks

- Act-only role: no build or test run here, so every API used is checked against its source (STL `Primitives.fs`,
  `Logging.fs`; `CatalogueFolderStore.fs`; `KitFolders.fs`) and annotations are explicit.
- ELEV: no primitive in the `create` signature; the only `.value` reads are at the IO calls.
