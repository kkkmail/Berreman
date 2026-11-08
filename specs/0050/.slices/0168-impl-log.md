# Step 0168 — IMPLEMENT_CONTRACT STORE_BigBoy_0005 KitHistoryProxy — impl-log

## Progress

- [x] Read the protocol (base and `IMPLEMENT_CONTRACT` delta), the project prompt, the slice and its gate snapshot.
  Also read the step 165 contract tests, the step 167 store, and the reused helpers:
  `CatalogueFolderStore.writeBytesAtomically`, `KitFolders.FileByteContent.kitFileText`, STL's `FileName.combine`
  and `Logger.logWarn`, and the ELEV001 / ELEV005 analyzer sources.
- [x] `KitHistoryStore.fs`: `KitHistoryProxy.create`.
- [x] `KitHistory.fs`: the proxy doc names `create` beside the double.
- [x] `KitHistoryProxyTests.fs`: `KitHistoryProxyFolderTests`.
- [x] `.contracts-json`: `STORE_BigBoy_0005` is `implemented` at step 168, and its declaration at step 165 is kept.
- [x] State of the world.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitHistoryStore.fs`
  - New opens:
    - `System.IO`, `System.Text`;
    - `Softellect.Sys.Logging`;
    - `OpticalConstructor.Domain.Kit`, `OpticalConstructor.Domain.KitFolders`;
    - `OpticalConstructor.Storage.CatalogueFolderStore`.
  - `type KitFileText with member private this.toKitHistoryText : KitHistoryText`. This one-line bridge lets the
    store reuse `FileByteContent.kitFileText`, which decodes UTF-8 and drops one leading BOM, instead of adding a
    second decoder.
  - `type KitHistoryProxy with static member create (preferences : FolderName) : KitHistoryProxy`.
    - Building it computes only `KitHistory.fileName.combine preferences`.
    - `readHistory`:
      - When `File.Exists` is false, it logs one `Logger.logWarn` that names the file and "the file does not
        exist", and answers `KitHistory.empty`.
      - Otherwise it reads the bytes inside a `try`; an exception becomes `FileIoExn`. It decodes them with
        `kitFileText` and then applies `parseKitHistory`. Any `Error` logs one warning that names the file and
        `describe` (for `FileIoExn` that is the exception's message), and answers `KitHistory.empty`.
      - A read never writes, moves or deletes anything.
    - `writeHistory`:
      - It renders `history.fileText` first. A render error is `KitHistoryWriteErr` naming the file and
        `describe`.
      - It then calls `Directory.CreateDirectory preferences` inside a `try`. An exception is
        `KitHistoryWriteErr` naming the folder and the exception's message.
      - It then writes `Encoding.UTF8.GetBytes` of the text (no BOM) through `FileName.writeBytesAtomically`.
        A write error is `KitHistoryWriteErr` naming the file and `describe`.
    - `probeFolder`: `KitFolderPresent` if `Directory.Exists` is true for the folder's text, otherwise
      `KitFolderMissing`.
    - There is no retry, no cache, no backup copy and no import of `recent.json`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/KitHistory.fs`
  - The `KitHistoryProxy` doc is now one line naming Storage's `KitHistoryProxy.create` at the composition root and
    `createInMemory` as the double.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitHistoryProxyTests.fs`
  - New opens: `System.IO`, `System.Text`, `System.Text.Json.Nodes`, `OpticalConstructor.Storage.CanonicalJson`,
    `OpticalConstructor.Storage.KitHistoryStore`.
  - `type KitHistoryProxyFolderTests () = inherit KitHistoryProxyContract ()`.
    - Each instance has its own root:
      `Path.Combine(AppContext.BaseDirectory, "KitHistoryProxyFolderTests", <GUID "N">)`.
    - `freshHistory` is `KitHistoryProxy.create <root>/Preferences`, which does not exist yet.
    - `presentFolder` is `<root>/kits/present`, which the constructor creates.
    - `missingFolder` is `<root>/kits/missing`, which is never created.
    - The fixture history has one starred entry whose name is `Optics QZ — µ`, so the file's bytes are non-ASCII
      UTF-8.
  - Its four own facts, named exactly as the slice gives them:
    - `a history written by one store is read back by a new store over the same preferences folder`:
      - the preferences folder is absent before the write;
      - the write is `Ok ()`;
      - a new `create` reads the history back equal;
      - the folder then holds exactly `kit-history.json`;
      - the file's bytes are the canonical text's UTF-8 bytes, with no BOM.
    - `a missing history file reads as the empty history and creates nothing`: the read answers `KitHistory.empty`
      and the folder still does not exist.
    - `an unreadable history file reads as the empty history and is left as it was`:
      - three files are tried: `{`, the fixture's `fileText` with `schemaVersion` 2, and the fixture's `fileText`
        with the first entry's `star` set to 3 (both edits use JsonNode, as in step 167's tests);
      - each reads as `KitHistory.empty`;
      - each keeps its bytes, and the folder still holds only `kit-history.json`.
    - `a write the preferences folder cannot take is KitHistoryWriteErr, never a throw`: with a file at
      `<root>/Preferences`, `writeHistory` answers `Error (KitHistoryWriteErr reason)`, and `reason` contains that
      path.
- `specs/0050/.contracts-json`: `STORE_BigBoy_0005` changes from lifecycle `declared` with `implementStep` null to
  `implemented` with `implementStep` 168. `declaringStep` stays 165, and `consumers` and `temporaries` are kept.

## Decisions

- **Render before creating the folder.** The brief lists "creates `preferences` … renders … writes". The store
  renders first, so a render failure changes nothing on disk, as `CatalogueFolderStore.writeCatalogueFolder`
  does. The folder is still created before the atomic write, so the order seen on disk is the one the brief gives.
- **BOM decoding is reused.** `FileByteContent.kitFileText` (`KitFolders.fs`, S4.1) is exactly "decode UTF-8,
  drop only a leading BOM". The store reaches it through a private `to<Target>` member on the source type
  (`KitFileText.toKitHistoryText`). ELEV001 and ELEV005 both allow `Ctor this.value` inside a member of the source
  type (checked in `C:\GitHub\Softellect\Analyzers\NoCallSiteRewrap.fs` and `NoWrapUnwrapRoundTrip.fs`). Copying
  the BOM logic or using `File.ReadAllText` was rejected: `ReadAllText` would also honour UTF-16 and UTF-32 BOMs,
  which is not "decoded as UTF-8".
- **A missing file is checked, not caught.** `File.Exists`, which never throws, separates the expected first-run
  case from a real read failure. Both still log exactly one warning, as the brief requires.
- **The UTF-8 write is inline, not shared.** The write is `Encoding.UTF8.GetBytes text.value |> FileByteContent`
  followed by `writeBytesAtomically`, which is the shape `KitFolderStore.writeText` already uses.
  `LibraryFileText.writeAtomically` would need a cross-type conversion that does not exist.
- **Test fixture.** The contract's `kitA` / `kitB` are private `let`s of the abstract class, so the derived class
  cannot reach them. It builds its own one-entry history, with non-ASCII text to exercise UTF-8 on disk. Nothing
  in the contract class was changed.

## Testing state

No checks or gates were run. The `IMPLEMENT_CONTRACT` role is act-only (invariant 6), so build, elevation,
constructor-unit-tests and the thirteen s19 invariants belong to the arc-runner after this worker exits.

Because no build could run here, every API used was checked against its source:
- STL `Primitives.fs`: `FileName.combine`, `FileByteContent`;
- STL `Logging.fs`: `Logger.logWarn`;
- the Storage helpers named above;
- the step 167 test idioms (JsonNode edits, `Assert.Fail`, `Assert.Contains`).

Annotations are explicit wherever overload resolution could be ambiguous.

New identities: exactly eight under `OpticalConstructor.Tests.KitHistoryProxyFolderTests`, which is four inherited
`KitHistoryProxyContract` facts plus the four own facts. The test count is expected to rise by 8. That has not
been measured.

The edited files are LF with no BOM. `git diff --numstat` matches `--ignore-cr-at-eol`, and `git diff --check` is
clean. None of the s19 invariant words were added (checked by reading the diff, not by running the gates).

## Artifacts

None. No logs or captures were produced, because no checks were run.
