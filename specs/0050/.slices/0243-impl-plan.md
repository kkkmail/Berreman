# Step 0243 — impl plan

Obligation `S72-WIRE` (WIRE_UI). The constructor windows' Items panel reads the app's item models over the kit's
snapshots, so an item added in a real window is a kit file.

## Approach

1. `OpticalConstructor.App/Program.fs`:
   - `ConstructorScene.withAppContext` already sets `catalogueImports` and the other app seams on each window's seed
     (steps 154, 189). It now also takes the `OpenedKit` and sets
     `itemModels = context.itemModels.overlaidWith opened.overlay` (steps 220, 222), beside `catalogueImports`.
   - Both windows pass their `opened` to it.
   - `itemIds` stays `initWith`'s `ItemIdProvider.system` (step 239). The photo picker stays the system dialog the
     view opens (step 241). No other seam changes.
   - The docs of `MainConstructorWindow` and `InverseConstructorWindow` gain one line each: their Items panel reads
     the app's item models.
2. `OpticalConstructor.Ui.Tests/KitWindowTests.fs`: one fact, trait `ui-smoke`, as the brief words it:
   - Show a `MainConstructorWindow` over `TestAppScope.freshAppContext owner` (`FolderName "KitWindowTests"`) and a
     kit from `TestAppScope.openedKitIn`.
   - Press `flowItemsStep`, `addItemButton` and the circular polarizer's offer. Set `newItemLabelField`'s text to
     `CPL1` and press `addItemConfirmButton`.
   - Assert that CPL1's `kitItemRow` shows. CPL1's id is minted by the system provider, so the fact reads it from
     the kit.
   - Assert that the kit's folder holds `CPL1.item-json` and
     `library/33486eec-836a-4cbf-ac79-e3f14439ad4b@v1.item-model-json`.
   - Close the window. Open a second `MainConstructorWindow` over the same folder through `KitOpening.openKitAt`
     (the class's `openedAt`). After `flowItemsStep` is pressed, CPL1's `kitItemRow` shows.

## Files

`Program.fs` and `KitWindowTests.fs`, both in the authorized `edits`. The other authorized test files
(`AppContextTests`, `LauncherTests`, `TableAndElementRotationTests`, `WireUi*CompositionTests`) construct the windows.
They are edited only if a run shows they need it; their assertions are kept.

## Risks

- Before this step the window's `itemModels` is `initWith`'s empty in-memory store, so the form offers no item model
  and the new fact is red at the offer press. That is the expected red.
- The offer list is a nested `ScrollViewer` (step 242 gotcha). The circular polarizer is in the first group, near
  the top, so one `BringIntoView` should reach it. If not, the press needs scrolling through every viewer.
- `MainConstructorWindow` sizes itself to 820 × 770, shrunk to the screen. The Items panel scrolls in both
  directions.
