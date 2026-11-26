# Step 0280 — impl plan

## Approach

Step 279 left the slot bar's state in the window model (`slotChipOpenOpt`, `model.slotChipItems`, `ToggleSlotChip`;
`SwapSlot` closes the list), and step 278 added `RecordExperiment`. This step draws the bar.

1. `OpticalConstructor.Controls/UiIds.fs`: a new group after the flow strip holding `slotBar`, `slotChipChoiceList` and
   `recordExperimentButton`.
2. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`:
   - `SlotRole.uiKey`, the role's `text` without spaces, declared before the `ConstructorUiId` extension that holds
     `elementHover`, so that extension can use it;
   - on that extension, `slotChip` and `slotChipChoice`;
   - `ItemLabel.caption` and `Angle.degreeCaption`, which reuses `BarDegrees.text`;
   - `RoledSlot.chipCaption`, which reuses `turnAngle`.
3. New `OpticalConstructor.Ui/SlotBarView.fs`, compiled directly before `TableAndElementRotationView.fs`. It declares
   `SlotChipState`, `SlotBarState`, `SlotBarHandlers` and `slotBarView`:
   - a vertical `StackPanel` holding a `WrapPanel` of chips plus Record experiment
     (`MaterialsWindowView.verbButton`);
   - below that, the open chip's `WrapPanel` of fitting items: the held one an `actionButton` on `positiveBackground`,
     the others `verbButton`s;
   - each chip is a focusable tab-stop `Border` that focuses itself and toggles on a press, and reads a typed text
     through `PickLetter.tryCreate`.
4. `TableAndElementRotationView.fs`:
   - `model.slotBarState` on the extension that holds `slotChipItems`;
   - a private `slotBarHandlers` beside `experimentHandlers`;
   - `mainView`'s `tableSurface` becomes a `DockPanel` with the bar docked on top and today's panel as the fill.
5. Tests:
   - new `SlotBarTests.fs`, appended last in the Ui.Tests compile list, with the six facts the slice names;
   - `KitWindowTests.assertWriteControls` also checks `recordExperimentButton`: absent when read-only, present when
     read-write.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SlotBarView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SlotBarTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

## Risks

- **Main table position.** The bar moves the table down. Every Main test that places the pointer on the canvas has to
  locate it through the canvas, not by window coordinates. The selected ui-tests classes and the whole of ui-smoke
  cover this.
- **The scroll law.** `ScrollExtentTests` must still reach every control. The bar is a `WrapPanel` inside a vertical
  `StackPanel`, so its width is bounded and it wraps.
- **The idle colours.** `verbButton`'s idle colours are private to `MaterialsWindowView.fs`, which is not among this
  step's authorized edits.
- **Keyboard behaviour in the headless platform.** Tab navigation and text input must reach the focused `Border`.
