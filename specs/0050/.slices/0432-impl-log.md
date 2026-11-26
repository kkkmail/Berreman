# Step 0432 — implementation log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0003, the slice and the gate snapshot.
- Read the APIs the build drives: `LibraryWindowView` (drop), `ImportDialogView`, `KitStartView`,
  `TableAndElementRotationView` (Add item form, `AssignSlot`, `AddElement`, `PlaceOnBeam`, `availableCaptures`,
  `ElementIdProvider`), `LibraryEntryFiles` / `EntryFileJson` (entry files), `KitContentFiles` / `KitLibrary`
  (snapshots), `BenchNoise` (`NoiseParam.decentLab.parameterAxes`), `ItemModels` (axes).
- Shared AT-1/AT-2 compositions added and AT-1 moved onto them (no AT-1 message or assertion changed).
- AT-2 data, build, `absorbingBuild` and the fact added.
- Doc comments of every new private function cut to one line citing spec 0050 S12.2.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` — the step's only authorized
  edit; LF kept (`git diff --numstat` equals `--ignore-cr-at-eol`).

## Decisions

- **Shared with AT-1 (the slice's (1)).** `testClockAt (instant : ClockTime) : ClockProvider * ClockTime ref` and
  `kitStartOver context clock kitId author folder : KitStartModel * OpenedKit option ref` replace the inline clock and
  `KitStartContext` of `buildOpticsMuellerKitWith`. AT-1 passes `initialTime`, `kitId`, `kitAuthor`, and sets
  `openedOpt` from the cell after `CreateKit`, so its close-on-failure handler is unchanged.
- **More reuse, same AT-1 behaviour (AGENTS.md "Reuse, never copy").**
  - `mintingFrom` replaces AT-1's two inline fixed-id closures and serves AT-2's two. The only text change is the
    extra-request failure, which now gives the count as a number ("expected 9 item-id requests"), not as a word.
  - `addItemThroughForm` is the Add item form's OpenAddItemForm → ChooseNewItemModel → SetNewItemLabel →
    `formEdits` → AddItem. AT-1 passes `[ SetNewItemDial spec.dial ]`, so it sends exactly the messages it did.
    AT-2 passes `[]`, so it sends the slice's four (the form defaults to `FixedMounting`).
  - `assertKitFile` holds the `.kit-json` assertions of AT-1's items-and-setups fact. That fact calls it with its
    old values and keeps its clock and no-experiments checks.
  - `AcceptanceItem.presetOpt` is renamed `bindingOpt` (the preset or sample a model binds). `choiceFor` gains
    the `StackSampleChoice` case, matching by the model's id and the sample's value id, as step 216's payloads
    require.
- **R0003.** The reflected detector's id comes from a test `ElementIdProvider` with one fixed id
  (`45dba7c2-156b-4d31-895a-1179ff672cfc`). A second request fails, and the build asserts that the id was used.
- **`library/` holds seven snapshots, not four (deviation from how-to (4), recorded per AGENTS.md "the working code
  is what is").** `KitLibraryContext.recordItem` snapshots `item.libraryReferences` (the model version, then the
  bound physics) and each one's reference closure (`KitContentFiles.fs:268`, `KitLibrary.fs:60-200`). So:
  - the Laser also snapshots the source-preset binding model `239b0cfb…`. AT-1's committed kit holds it for its
    own Laser: `data/optics-mueller/library/239b0cfb-9938-5f3b-a767-c4252ea671b6@v1.item-model-json`;
  - each plate snapshots the stack-sample binding model `4d13ecb1…`;
  - the material references its category latest (`LibraryEntryContent.references`), so the shipped
    `Crystal.category-json` (`5d7e9f1a…`) is snapshotted too. The drop seam already requires the Crystal
    category to be held (`decideDroppedFiles` checks `tryHeldEntry`).

  The fact asserts exactly these seven at version 1, plus the slice's byte rule: the material and the sample each
  equal their dropped file; the heNe633 preset, the Mueller polarimeter model, both binding models and the category
  each equal their `Catalogue/Builtin/` file. Asserting "four" cannot pass without production changes outside the
  step's effects.
- **Folder lifecycle.** `absorbingBuild` uses the fixed run name `AT-2`. Each build therefore deletes a previous
  run's `KitAcceptanceTests/AT-2` folder, then creates the kit folder and the `entries/` folder fresh and empty. A
  failed build closes its kit and leaves its folder for diagnosis. No fact closes the shared kit: closing it would
  write into the folder.
- **Extra guards in the build.** The drop must yield exactly one `ApplyDroppedEntries` request with two rows (the
  material's, then the sample's), each defaulting to `AddIncomingEntry`. The dialog's Apply and the window's
  `LibraryImportApplied` must report no error. Both entries must resolve at version 1. Every `AssignSlot` must leave
  no status. `availableCaptures` of the plate's slot must be T, R and Both.

## Testing state

No build, test, analyzer or invariant gate was run in this round: IMPLEMENT Invariant 6 leaves gate execution and
verdicts to the supervisor. The code was checked against the APIs it calls by reading them (record fields, union
cases, member names, open-order shadowing of the newly opened `ImportDialogView` and `EntryFileJson`). No
requirement is blocked.

Static count: `KitAcceptanceTests` declares 7 facts with `Category=kit-acceptance` (6 before, +1).

## Artifacts

None.

## Gotchas

- `.manual/033` (R0003) names this step: the added reflected detector takes its id from the element-id provider.
  Keep that injection and AT-1's through 0494.
- `SetupSlot.withItem` keeps the slot's prebound `valueId` for a standalone model (`LibraryModelChoice`). The
  reflected detector therefore still carries `AddElement Detector`'s seed `PresetIds.detectorIntensity`. When step
  433 records setups, `Setup.libraryReferences` will snapshot that preset (AT-1's `f1d38e6a…@v1.polarizer-json` is
  the same effect from its `AddElement LinearPolarizer` slots). This fact's exact seven-snapshot set must then grow
  by that entry. Assigning the item does not clear the seed.
- The fixed run name means a second, concurrent test process over the same output folder would collide. The gate
  runs one.
