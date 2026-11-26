# Step 0218 — impl plan (IMPLEMENT: a kit's item files through `KitProxy`)

## Approach

Step 217 gave the item its file (`Item.itemFile`, `ItemFile.fileText`, `KitFileText.parseItemFile`). This step
adds the two typed `KitProxy` members that read and write a kit's items over the per-kind verbs. The members go on
step 148's `type KitProxy with` extension in `KitContentFiles.fs`. No proxy verb is added (step 127).

- `writeItem (item : Item) : Result<unit, KitError>`:
  - renders `item.itemFile.fileText`;
  - writes it whole with `writeKindFile ItemKitFile item.label.fileStem`;
  - a rendering `StorageError` becomes `KitFileRejectedErr`, naming the file and the error's `describe`.
- `readItems () : Result<Item list, KitError>`:
  - reads every stem `listKindFiles ItemKitFile` answers with `readKindFile`, and parses each with `parseItemFile`;
  - refuses the whole read with `KitFileRejectedErr`, naming the file, for:
    - a refusal of the read or the parse;
    - a label whose `fileStem` is not the file's stem, ignoring case;
    - an `ItemId` already read;
  - orders its answer by the label's text, ordinal.

**Reuse.** `readExperiments` and `readSceneNames` already repeat the list → read → parse → refuse-by-file fold, and
`writeExperiment` / `writeScene` repeat the render → refuse-by-file → write pipeline. A third copy would be a
duplicated function body (AGENTS.md, "Reuse, never copy"). So:

- one private module function, `readEveryKindFile`, carries the fold, with an `accept` step for the reader's own
  check;
- one private module function, `writeRenderedKindFile`, carries the write.

They are functions, not members, so `KitProxy` gains only the two members the slice names ("no … other member is
added").

All three readers and all three writers use them. The behaviour of the existing members is unchanged: same calls,
same order, same error texts.

**Tests.** A new `KitItemFileTests.fs` (`module KitItemFileTests`) is appended at the end of the compile list.

- Each fact builds a fresh kit with the shared `KitContentRoundTripFixture.createContentTestKit` (the step 148
  precedent `ExperimentCollectionRoundTripTests` uses it). That fixture makes
  `Path.Combine(AppContext.BaseDirectory, "KitItemFileTests", <new GUID>)`, then `KitProxy.create`, then
  `initialiseKit`.
- The items are CPL1 (circular polarizer), LP1 (real linear polarizer) and Phone (intensity detector), made through
  `ItemDraft.toItem`.
- There are four facts, the slice's exact identities.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitItemFileTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one compile line, last)

## Risks

- **The repeated-id refusal and listing order.** Files are listed in ordinal order, so `CPL9.item-json` is read
  *before* `LP1.item-json`, and the repeat is detected on `LP1.item-json`. The refusal therefore names both files
  that hold the id (the one being read and the one read before it), so the fact's "naming that file" holds for
  `CPL9.item-json`.
- **The shared-helper refactor touches the existing experiment and scene readers and writers.**
  `ExperimentCollectionRoundTripTests`, `SceneRoundTripTests` and the whole constructor suite guard them.
- **ELEV:** no primitive appears in any new signature. The helpers are generic only in `'Parsed` / `'Content`, and
  the members carry `KitProxy`'s own types.
