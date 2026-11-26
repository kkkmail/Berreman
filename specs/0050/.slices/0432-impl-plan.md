# Step 0432 — implementation plan

AT-2's first part lands in `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`,
the step's only authorized edit. Operator rulings R0001–R0003 bind; R0003 requires AT-2's added
reflected detector to take its identity from `ElementIdProvider` (fixed, consumed, fails on an extra
request), as AT-1's added slots do.

## Approach

1. **Shared with AT-1** (AGENTS.md "Reuse, never copy"):
   - `testClockAt (instant : ClockTime) : ClockProvider * ClockTime ref` replaces AT-1's inline mutable clock;
   - `kitStartOver context clock kitId author folder : KitStartModel * OpenedKit option ref` replaces AT-1's
     inline `KitStartContext`;
   - `mintingFrom`, one fixed-id minter for AT-1's and AT-2's item and slot identities (AT-1 had two inline copies);
   - `addItemThroughForm`, the Add item form's OpenAddItemForm / ChooseNewItemModel / SetNewItemLabel / AddItem
     sequence; AT-1 passes its `SetNewItemDial`, AT-2 nothing (the form defaults to `FixedMounting`);
   - `assertKitFile`, the `.kit-json` assertions both builds' first facts make;
   - `AcceptanceItem.presetOpt` becomes `bindingOpt` (the preset OR sample a model binds) and `choiceFor`
     matches `StackSampleChoice` by the sample's id (step 216's payloads).
   AT-1's message sequence and assertions stay as they are.
2. **AT-2 build** `buildAbsorbingKit (run : FolderName) : BuiltAcceptanceKit` — folder, context, clock, the two
   entry files under `<run>/entries/`, the Library-window drop and the import dialog, the start screen, the
   window model with fixed item and slot ids, the `{obj1}` template, the eight items, the table.
3. `absorbingBuild`, a module-level `Lazy` of `buildAbsorbingKit (FolderName "AT-2")`.
4. The fact `AT-2 builds the absorbing-biaxial-noisy kit's library entries, items and table through the app's
   messages`: `.kit-json`, `readItems`, `library/` and the table.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` (only).

## Risks

- **`library/` snapshot count.** How-to (4) says "exactly four snapshots". `recordItem` snapshots each item's
  model version AND its bound physics with their closure (`KitContentFiles.fs:268`, `KitLibrary.fs:148`), so the
  Laser also snapshots the source-preset binding model (AT-1's committed kit holds it,
  `data/optics-mueller/library/239b0cfb…@v1.item-model-json`), each plate the stack-sample binding model, and the
  material its Crystal category (`LibraryEntryContent.references`). The fact asserts the seven the app writes —
  the dropped material and sample against their dropped files, the five shipped entries against
  `Catalogue/Builtin/` — and the deviation is recorded.
- The build runs once per test run under a fixed run name, so it deletes a previous run's folder first.
- No local gate run (IMPLEMENT Invariant 6): the code is checked against the APIs by reading them.
