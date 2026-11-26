# Step 0239 — impl plan (IMPLEMENT: the Items panel's state and AddItem)

## Approach

The step adds the Items panel's model state and its semantic messages to the constructor window. It draws nothing:
the panel view is step 0241's, and the App wiring of `AppContext.itemModels` is step 0243's.

1. **`OpticalConstructor.Ui/KitOpening.fs`.** Add `type ItemIdProvider with static member system`, beside
   `ClockProvider.system`. It mints `ItemId (Guid.NewGuid ())` on every call.
2. **`OpticalConstructor.Ui/TableAndElementRotationView.fs`.**
   - Add `AddItemForm` before the model, with `member this.itemDraftOpt : ItemDraft option`.
   - Add four model fields after `flowStep`: `itemModels`, `itemIds`, `selectedItemOpt` and `addItemFormOpt`.
     `initWith` sets them to the in-memory item-model store, `ItemIdProvider.system`, `None` and `None`.
   - Add `itemModels` to `DefaultStores.DefaultStoreSet`, taken from the in-memory catalogue stores. Thread it into
     `initTableAndElementRotation` and `initMain` by record update, beside `sources` and `catalogueImports`.
   - Add seven messages after `ShowFlowStep`: `SelectKitItem`, `OpenAddItemForm`, `ChooseNewItemModel`,
     `SetNewItemLabel`, `SetNewItemDial`, `CancelAddItemForm` and `AddItem`.
   - Add their arms to `updateTableAndElementRotation`.
     - The offers are built by one private model member, placed after `guardKitWrite`. They are, in order:
       1. the standalone item models;
       2. the source presets, under the listed source-binding model;
       3. the polarizer presets of the linear, then the circular listing, one per entry id, under the listed
          ideal-polarizer model;
       4. the samples at version 1, under the listed stack model.
     - `AddItem` goes through `guardKitWrite`, then `ItemDraft.addTo`, then `KitLibraryContext.recordItem`.
3. **Tests.** Create `OpticalConstructor.Tests/ItemsPanelModelTests.fs` (`module ItemsPanelModelTests`) with the five
   declared facts, appended last in `OpticalConstructor.Tests.fsproj`'s compile list.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ItemsPanelModelTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **Binding models are taken from the listing, never manufactured.** Each binding model is the first listed version
  whose form takes that binding (`bindingKindOpt`). A failed listing therefore offers no binding model, so no preset
  or sample can be offered either.
- **ELEV.** No new free function has a single parameter of our types. The offer builder is a private member of the
  model, so ELEV013 does not apply. Every new field and case is elevated. The new case names were grepped across
  Berreman and STL Sys and are unique.
- **Other tests build the model by record expression.** Ui.Tests that do so would fail to compile once the record
  gains fields. The grep found none outside `initWith`: every test goes through `init*` and record updates.
