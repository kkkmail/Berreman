# Step 0237 — impl plan (IMPLEMENT: `KitLibraryContext.recordRelabelledItem`)

## Approach

Add one member to step 219's `type KitLibraryContext with` extension in
`OpticalConstructor.Storage/KitContentFiles.fs`, right after `recordItem`:

`member this.recordRelabelledItem (previous : Item) (relabelled : Item) : Result<unit, KitLibraryError>`

- **Stems differ ignoring case.** Record `relabelled` through `this.recordItem` (snapshot + pin + write), then
  delete `previous.label.fileStem` through `this.kit.proxy.deleteKindFile ItemKitFile`. When that delete fails,
  delete the new file through the same verb and log that undo's own failure with `Logger.logError` and its
  `describe`. Answer the first error as `KitLibraryKitErr`. The item never sits in two files of one `ItemId`, which
  `readItems` refuses (step 218).
- **Stems equal ignoring case** (`lp1` → `LP1`). The kit root is one Windows folder, so delete the old file first,
  then record `relabelled`. When the record fails, write `previous` back through `this.kit.proxy.writeItem`, log
  that write's own failure, and answer the record's error.
- **Read-only kit.** The first write or delete answers `KitReadOnlyErr` (the different-stem path stops in
  `snapshotReferences`, the same-stem path in the read-only view's `deleteKindFile`), and nothing is written.

`open Softellect.Sys.Logging` is added to the file for `Logger.logError` (Storage already references
`Softellect.Sys`). No other member is added.

## Tests (`KitLibraryTests`, the three declared identities)

Over `freshSnapshotFixture ()` (Built-in library, in-memory kit opened read-write) and `cpl1Item` (step 216's
`ItemDraft` over the circular-polarizer model, `DialMounted DialPositive`), recorded through `recordItem`:

1. `a relabelled item moves to the file of its new label` — relabel `CPL 1` (`relabelledAmong`, step 236), record
   through `recordRelabelledItem`; `paths` holds `CPL 1.item-json`, not `CPL1.item-json`; `readItems` answers the
   item under its new label and id. Also: a read-only reopening of the same folder refuses with `KitReadOnlyErr`
   and leaves the paths unchanged.
2. `a relabel that changes only the case rewrites the one file` — recorded as `cpl1`, relabelled `CPL1`; exactly one
   item file, `CPL1.item-json`; `readItems` answers the item labelled `CPL1`. Also: a record that fails after the
   delete (refused `listSnapshots`) writes the previous item back; a read-only reopening refuses and writes nothing.
3. `a relabel whose old file cannot be deleted leaves the item under its old label` — proxy record update refusing
   `deleteKindFile` of stem `CPL1` with `KitIoErr`; relabel `CPL 1` answers `KitLibraryKitErr (KitIoErr _)`;
   `readItems` answers CPL1 alone under its old label.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Storage/KitContentFiles.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitLibraryTests.fs`

## Risks

- ELEV: a local helper with a `string` parameter could trip ELEV003P/ELEV010; keep the undo logging inline.
- The in-memory folder keeps a held path's case on rewrite, which is why the same-stem branch deletes first;
  test 2 asserts the resulting single file name exactly.
- LF line endings on both files.
