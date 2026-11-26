# Step 0279 — impl plan

The slot bar's state in the window model (spec 0050 S8.4; 001 §J). The bar that draws it is step 280's.

## Approach

`OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. `TableAndElementRotationModel` gains `slotChipOpenOpt : Library.ElementId option` (one-line doc), set to `None` by
   `initWith`, the only full record construction.
2. The model extension holding `tryItemModelChoice` gains
   `member model.slotChipItems (slot : Setups.SetupSlot) : Item list = slot.fittingItems model.kitItems model.tryItemModelChoice`.
3. `TableAndElementRotationMsg` gains `ToggleSlotChip of Library.ElementId` (one-line doc). Its arm writes nothing:
   for a slot of `elements` it opens that chip or closes it when it is the open one; an id the table lacks answers the
   model unchanged.
4. `SwapSlot`'s arm computes its answer as before, then sets `slotChipOpenOpt = None` on it, so every outcome (switched,
   created, refused, read-only refused, write failed) closes the list.

`OpticalConstructor.Tests/SetupModelTests.fs`: two facts over the existing LP1/QZ/Laser/LPd/QZ2 drafts (step 210's
shipped models through `BuiltinCatalogue.itemModel`), a linear polarizer and a sample added:

- `a slot's chip lists the kit's items that fit it`
- `ToggleSlotChip opens one chip's list at a time, and a swap closes it`

## Risks

- ELEV: new names must be unique (`slotChipOpenOpt`, `slotChipItems`, `ToggleSlotChip`); members are exempt from the
  primitive rules. No `.value` added.
- The model holds function fields, so it has no structural equality; "changes nothing" is checked field by field.
