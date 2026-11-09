# Step 0219 — impl plan (IMPLEMENT: `KitLibraryContext.recordItem`)

## Approach

Add one member, `recordItem`, to step 149's `type KitLibraryContext with` extension in
`OpticalConstructor.Storage/KitContentFiles.fs`.

- It follows the existing `recordExperiment` / `recordScene` shape exactly.
- It calls `this.snapshotReferences item.libraryReferences` (steps 139, 215). That covers the model's exact version,
  then its bound physics, each with its closure.
- It then calls `this.kit.proxy.writeItem item` (step 218), mapping a `KitError` to `KitLibraryKitErr`.
- Its doc comment is one to three lines citing spec 0050 S7.1.

Nothing else in production changes. A read-only kit is refused by `snapshotReferences` itself, before any IO, as
the two existing recorders are.

## Tests

`KitLibraryTests` gains four facts under their exact identities. They reuse the class's own fixtures:

- `freshSnapshotFixture` / `snapshotFixture`: the Built-in library, the pin recorder, and the in-memory kit;
- `catalogueEnvelope`, `assertSnapshots` and `observeContentWrites`. The last asserts, at the moment of the content
  write, that every snapshot and pin is already in place.

The facts:

1. **CPL1** over `LibraryModelChoice` of the shipped circular polarizer.
   - Recording it snapshots and pins that model's version 1 alone, then writes `CPL1.item-json`.
   - The folder gains exactly `library/<GUID>@v1.item-model-json` and `CPL1.item-json`.
   - `readItems` answers CPL1.
   - A refused write keeps the snapshot and writes no item. A retry then succeeds.
   - A read-only reopening against an empty library resolves the model from its snapshot (`toItemModel`).
2. **QZ** over `StackSampleChoice` of the glass plate v1.
   - Snapshots, in order: the stack model, the sample, its glass material and that material's category, each once.
   - It reopens against an empty library: the model from its snapshot, then its sample binding.
3. **Laser** over `SourcePresetChoice` of HeNe 633.
   - Snapshots the source-binding model and the preset's latest version (1).
   - The ideal-polarizer item LP1 over `PolarizerPresetChoice` is covered too, so the snapshot tests cover source,
     ideal-polarizer and stack items (the brief's closing obligation).
   - A library that holds the preset but not the model refuses with `UnheldEntryErr` naming the model. It writes no
     snapshot, pin or file, so the preset never stands in for the model.
4. **Read-only:** for every kind, `recordItem` answers `KitLibraryKitErr (KitReadOnlyErr _)`. The recorder stays
   empty, `paths` is unchanged, and there are no snapshots and no items.

Every choice is built over `BuiltinCatalogue.itemModel` (the shipped step-210 entries). No model is manufactured.

## Risks

- The `paths` delta may include a folder entry. Measure it and assert on the measured value.
- `ItemModelPayload` has no references, so the model's closure is itself alone. Assert that through the exact
  snapshot list.
- ELEV: the member is on a type, so it speaks its own types. The tests are out of ELEV scope.
