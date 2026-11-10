# Step 0264 — implementation plan

## Approach

`OpticalConstructor.Ui/TableAndElementRotationView.fs`:

1. `TableAndElementRotationMsg` gains `SwapSlot of Library.ElementId * ItemId`, after `AssignSlot`, with a one-line doc
   citing spec 0050 S8.2 and 001 §P `SwapSlot`.
2. The slot / item / choice resolution of `AssignSlot`'s arm moves into one private function before the update
   (`slotAssigned model slotId itemIdOpt`), so `AssignSlot` and `SwapSlot` refuse alike without a copied body
   ("Reuse, never copy"). `AssignSlot`'s behaviour and texts are unchanged.
3. `SwapSlot (slotId, itemId)`'s arm: a refusal sets `kitStatus` to `Not swapped — <e.describe>` and nothing else.
   Otherwise the swapped slots are the table's with the filled slot in place, each `recordedWith model.kitItems`; the
   fresh name is `active.name.nextFreeAmong` the held names when a setup is active, else `SetupName.firstFree` of them.
   `Setup.swapTarget swapped model.kitSetups fresh`:
   - `SwitchToHeldSetup s` → `updateTableAndElementRotation (ActivateSetup s.name) model` (the update is `let rec`),
     which writes nothing, so a read-only kit takes it;
   - `CreateSwappedSetup s` → behind `guardKitWrite`; `library.recordSetup s`: `Ok` → `model.withSetupRecorded s`
     with `elements = swapped` and `kitStatus = Created setup '<name>' by the swap.`; `Error e` →
     `Not saved to the kit — <e.describe>` only. The setup swapped from is never written.

`OpticalConstructor.Tests/SetupModelTests.fs`: a `QZ2` draft (the 2 mm glass plate at version 1 on the shipped
stack-sample model), a fixture stocking `QZ` and `QZ2` through `AddItem` over a table of the source, an added sample
and the seeded detector, and the three facts the step names:

- switch: `Bench A` (QZ) and `Bench B` (QZ2) through `AssignSlot` + `CreateSetup`, `Bench A` re-activated; the swap
  to QZ2 makes `Bench B` active and the table its slots; no kit file's bytes change.
- create: with only `Bench A` held and active, the swap writes `Bench A (2).setup-json` holding QZ2 at revision 1 and
  activates it, `Bench A`'s bytes unchanged; in a second fresh kit, a table that is no setup yet swaps into `Setup 1`.
- read-only: the folder of the switch fixture reopened read-only; the swap to `Bench B` switches; with an extra
  (empty) polarizer slot on the table the swap is new content and is refused with the read-only text, the table,
  `kitSetups` and every file unchanged.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`

## Risks

- New content under the read-only kit: the kit holds only QZ and QZ2 and both setups hold one of them, so "new
  content" is made by an extra slot (`swapTarget` compares the kinds and contents slot by slot).
- ELEV: the moved helper is private, takes only our types, and has three parameters (not ELEV013).
- `withSetupRecorded` sorts by ordinal name: `Bench A` before `Bench A (2)`.
