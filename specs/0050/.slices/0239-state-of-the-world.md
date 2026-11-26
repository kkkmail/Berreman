# Step 0239 — state of the world

## Where we are

Spec 0050, section S7.2 ("The Items panel, the flow strip and item mapping on copy"), step 0239 (IMPLEMENT),
obligation `S72-ADD-ITEM`.

- Step 0238 made the flow strip model state, with an Items step.
- This step gives the Items panel its model state: the item the panel shows, and the "Add item from library model…"
  form. It also adds `AddItem` as a semantic message of the constructor window (001 §P).
- No view reads the state yet:
  - the parameter editor and the label, notes and photo edits are step 0240's;
  - the panel view is 0241's;
  - the strip view is 0242's;
  - threading the app's item models into the real windows is 0243's.

## What's working

- Added `ItemIdProvider.system`, which mints each new item's id from a fresh GUID.
- Added the add form (`AddItemForm`) and its draft. The form offers, in order:
  - the standalone item models;
  - the source presets;
  - the polarizer presets, one per entry id;
  - the samples at version 1.

  Each preset and sample is bound to its binding model from the same item-model listing.
- Added model state for the item-model store, the id provider, the selected item and the open form. The test scenes
  hold the Built-in item models.
- Added `SelectKitItem`, `OpenAddItemForm`, `ChooseNewItemModel`, `SetNewItemLabel`, `SetNewItemDial`,
  `CancelAddItemForm` and `AddItem`.
  - `AddItem` records the item through `recordItem`.
  - It refuses a label the kit holds.
  - It refuses any write while the kit is open read-only.
- Added five `ItemsPanelModelTests` facts (constructor-unit-tests 1366 → 1371).

## Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build;
- elevation;
- constructor-unit-tests, over everything discovered;
- ui-smoke, over everything discovered;
- ui-tests, selected to `OpticalConstructor.Ui.Tests.KitWindowTests`;
- the fourteen s24 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0239-*`:

- **build:** exit 0, `0 Warning(s)`, `0 Error(s)`, on the full rebuild from the restored source.
- **constructor-unit-tests:** `Passed!`, 1371 succeeded, 0 failed, 0 skipped, in 70 s (estimate 180 s). That is
  checkpoint 238's 1366 plus the 5 declared facts, all `Passed` in the TRX under their declared ids:
  - `Add item from library model lists the item models, the presets and the samples, and mints the item's id`;
  - `the add form takes a model, a label and a dial, and Cancel closes it writing nothing`;
  - `AddItem records the item in the kit with its model's snapshot and selects it`;
  - `AddItem of a label the kit holds is told and records nothing`;
  - `a kit opened read-only adds no item`.
- **ui-smoke:** `Passed!`, 242 succeeded, 0 failed, 0 skipped, in 3 min 29 s (attempt timeout 600 s).
- **ui-tests (`KitWindowTests`):** `Passed!`, 13 succeeded, 0 failed, 0 skipped, in 20 s.
- **elevation** from the arc origin `f395597`: exit 0, `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in
  206 s. Known findings: 0.
- **The fourteen s24 invariant greps:** none finds anything. The untracked new test file was also checked directly.
- **Mutation check:** ten mutations of the new code were each caught by a declared fact. The source was then
  restored byte for byte, rebuilt and re-run. The mutations were:
  - offer order;
  - polarizer de-duplication;
  - the read-only guard;
  - `recordItem`;
  - the blank label;
  - `SelectKitItem`;
  - the minted id;
  - `addTo`;
  - Cancel;
  - the `initMain` item-model store.

No run came near a deadline. No test is removed or renamed, and no assertion is removed or weakened.

```yaml
gates:
  constructor_unit_tests: 1371
  ui_smoke_tests: 242
  ui_tests: 13
```

## Architecture

- **One listing feeds every offer.**
  - `OpenAddItemForm` lists the item-model store once.
  - The standalone forms become `LibraryModelChoice`s.
  - The three binding forms are found in that same list by `bindingKindOpt` and joined to their typed library
    entries.
  - No model is manufactured, and no preset or sample stands in as one (the slice's "MUST").
- **The form is window state.** `AddItemForm` and `selectedItemOpt` are never stored, and scene capture and restore
  leave them alone. Only `AddItem` writes, and only through `guardKitWrite`, `ItemDraft.addTo` and
  `KitLibraryContext.recordItem`.
- **The id is minted when the form opens, not when the item is added.** `itemIds` is injected, as the clock is, so a
  test supplies fixed ids. Re-opening the form mints a new id.
- **The item models are injected like the other stores.**
  - `initWith` holds an empty in-memory store.
  - `initMain` and `initTableAndElementRotation` hold the Built-in stores' item models, through `DefaultStoreSet`.
  - The App's `withAppContext` does not yet thread `AppContext.itemModels`. That is step 0243's wiring.

## Deferred

Nothing in this step's scope is deferred. The views, the App wiring and the item edits named under "Where we are"
belong to steps 0240–0243's own slices; they are not left over from this step.

## Gotchas

- **A failed item-model listing opens an empty form.**
  - The form still opens under a minted id, with `The item models could not be listed — …`.
  - Every binding model comes from that same listing, and none may be manufactured, so no preset or sample can be
    offered either.
  - Each failed library listing drops only its own kind.
- **"The eight item models" are the standalone ones.** The Built-in catalogue ships eleven. Three are binding forms,
  which appear only joined to their presets and samples.
- **No Built-in polarizer serves both kinds.** The "one per entry id" de-duplication shows only over a library that
  lists a compound polarizer under both. Fact 1 builds such a library.
- **The real windows offer no item models yet.** `initWith`'s store is empty, and the App's `withAppContext` does not
  thread `AppContext.itemModels` (step 0243).
- **The id is minted at open.** Each `OpenAddItemForm` draws a new id from `itemIds`, so a test with a fixed provider
  must fill the form it opened rather than open another.

## Changelog

- 2026-11-09 — Step 0239:
  - added `ItemIdProvider.system`, `AddItemForm` and the model's `itemModels`, `itemIds`, `selectedItemOpt` and
    `addItemFormOpt`;
  - added `SelectKitItem`, `OpenAddItemForm`, `ChooseNewItemModel`, `SetNewItemLabel`, `SetNewItemDial`,
    `CancelAddItemForm` and `AddItem`;
  - added five `ItemsPanelModelTests` facts (constructor-unit-tests 1366 → 1371).
