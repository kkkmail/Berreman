# Step 0283 — impl plan

## Approach

The grid generator's panel is a projection of its own state and handler records, like the slot bar (step 280). It is
compiled between `SlotBarView.fs` and `TableAndElementRotationView.fs`. The window model gains one read member,
`setupGridState`, over step 282's `setupGridOpt` / `setupGrid`. The existing messages drive it, and no message, field
or option is added.

1. `UiIds.fs`:
   - `setupGridOpenButton` goes in the slot-bar group;
   - a new "setup grid generator" group holds `setupGridPanel`, `setupGridCombinationList`,
     `setupGridGenerateButton`, `setupGridCloseButton` and `setupGridCombination (position)`.
2. `ConstructorUiIdOfDomain.fs`:
   - `setupGridItemChoice`, `setupGridAngleField` and `setupGridAngle` on the `ConstructorUiId` extension that holds
     `slotChip`;
   - `GridCombination.caption` at the end, after `RoledSlot.chipCaption`.
3. New `SetupGridView.fs`:
   - `GridCombinationMark`, `GridCombinationState`, `SetupGridSlotState`, `SetupGridState` and `SetupGridHandlers`;
   - `setupGridView`, which reuses `MaterialsWindowView.verbButton`, `actionButton`, `brush`, `messageColor`,
     `positiveBackground` and `negativeBackground`.
4. `SlotBarView.fs`: `openSetupGrid` in the handlers, and the `Setup grid…` verb ends the chip row whatever the kit's
   access.
5. `TableAndElementRotationView.fs`:
   - `model.setupGridState` next to `setupGrid`;
   - a private `setupGridHandlers` beside `slotBarHandlers`, which gains `openSetupGrid`;
   - the table surface docks the slot bar and the open panel on top.
6. Tests:
   - new `SetupGridPanelTests.fs`, last in the compile list, with five facts;
   - `ScrollExtentTests` gains the Main-with-grid fact;
   - `SlotBarTests`' UiId fact also checks `setupGridOpenButton`.

## Risks

- **Stale handler on Generate.** `MaterialsWindowView.actionButton` subscribes its press once per id
  (`SubPatchOptions.OnChangeOf id.value`), so Generate would keep sending the grid it was first drawn with. The
  established cure (`ItemsPanelView`, `KitStartView`) is to key the action row by what it sends. I key it by the
  ticked combinations, which are all `generate` reads.
- **A role can move to another slot when the table changes.** A held item's `actionButton` keeps the slot it was drawn
  for. Each slot's row is keyed by its slot id, so a different slot recreates the row.
- `RotationControls.BarDegrees` does not resolve (step 282's gotcha), so the handler record uses `BarDegrees`.
- ELEV: helpers stay local to `setupGridView`, so no single-parameter free function is added. Every new type, case and
  member name was checked unique against Berreman and STL.
