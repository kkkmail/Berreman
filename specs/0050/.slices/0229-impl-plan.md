# Step 0229 — impl plan

## Approach

Step 228 put the copy's item mapping behind messages (`KitCopySource.items` / `itemRows`, `ChooseCopiedItem`). This
step draws the mapping table in the kit bar's copy panel and proves it headless.

1. **Ids.**
   - `ConstructorUiId.kitCopyItemList` (`"KitCopyItemList"`) in the kit-bar group of `UiIds.fs`, after the step-226
     copy ids.
   - `ConstructorUiId.kitCopyItemChoice (sourceItem : ItemId) (choice : ItemCopyChoice)` in
     `ConstructorUiIdOfDomain.fs`, beside `kitCopyExperimentToggle`: `KitCopyItemChoice_<source GUID>_Add` or
     `KitCopyItemChoice_<source GUID>_<kit item GUID>`. `Kit` is reached qualified (`Kit.ItemId`), as `Scene.SceneName`
     is, so no new `open` can shadow a name in that file.
2. **View** (`KitWindowView.fs`).
   - A new private `kitCopyItemRows`, documented in up to three lines citing S7.2. It returns nothing while
     `itemRows` is empty. Otherwise it returns:
     - the wrapping intro line;
     - a vertical `StackPanel` carrying `kitCopyItemList`, holding one block per row.
   - Each block is the line `<label> — <entryReferenceOpt's describe>`, then a `WrapPanel` of `Add` and
     `Use <label>` per `model.kitItems` item.
   - The row's choice is an `actionButton` on `positiveBackground`; the others are `verbButton`s. Each sends
     `ChooseCopiedItem (source id, choice)`, which is constant per id, so `actionButton`'s `OnChangeOf id` closure
     is safe.
   - The table goes inside the step-226 `ScrollViewer` (`MaxHeight` 160), below the toggles, so the bar's height is
     unchanged.
3. **Tests.**
   - `KitWindowTests`, three census facts:
     - the ids;
     - the list over a real source: E1 uses Laser600, E2 uses QZ, the target holds its own Laser600; E1 and E2 are
       chosen through messages;
     - the ui-smoke press fact with two branches:
       - Add, against a target holding its own Laser600;
       - Use, against a bench holding Laser600 and a third-id T with different `intensityScale`.

     Both branches close and reopen the target. They resolve through `Experiment.itemsUsedBy` and assert the
     descriptor's `ItemId`, the resolved id and parameters, and that the source's unused spare (and, for Use, the
     mapped piece) is absent.
   - `ScrollExtentTests.tallestKitBar`: all forty experiments chosen, twenty source items each with a row, and the
     kit holding three items, so every row offers `Add` and three `Use` choices.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`

## Risks

- A long label in a `Use <label>` box does not wrap, because the shared box helpers do not wrap. The fixtures keep
  labels short, as lab labels are (`LP1`, `QZ`).
- The scroll law now also meets twenty blocks inside the 160 px viewer. `BringIntoView` scrolls the inner viewer, as
  it does for the forty toggles.
- A same-key `Border` switches between `verbButton` and `actionButton` when the choice changes. FuncUI patches the
  attributes in place, and the message is the same either way.
