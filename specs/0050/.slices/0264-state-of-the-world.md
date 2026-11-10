# Step 0264 — state of the world

## Where we are

This is spec 0050, section S8.2 (setups as slots), step 0264 (IMPLEMENT). Step 0258 gave the domain
`Setup.swapTarget`. Steps 0260–0262 gave the constructor `CreateSetup`, `ActivateSetup`, `SaveSetup` and the commit's
setup recording, and step 0263 added `AssignSlot`. This step adds the quick swap as a semantic message, `SwapSlot`:
another item in one slot makes the table another setup, either a held one (switched to without writing) or a new one
(recorded under the next free name). Step S8.4 adds the slot bar that sends it.

## What's working

- Added `SwapSlot of Library.ElementId * ItemId`: the table as the setup that holds this item in that slot.
- Switched to the kit's setup that already holds the swapped content through `ActivateSetup`'s update, writing
  nothing, so a read-only kit takes it.
- Created, recorded and activated a setup of the next free name (`<active> (2)`, or `Setup 1` with none active) for new
  content, behind the read-only guard, never writing the setup swapped from.
- Shared `AssignSlot`'s slot / item / model resolution with `SwapSlot` (`Not swapped — …` on a refusal).
- Added three `SetupModelTests` facts: the switch, the creation (next free name and `Setup 1`) and the read-only kit.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. The development runs below used each
gate's own command over the final tree, with logs in `specs/0050/.artifacts/0264-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 22 s incremental (deadline 600 s). The first build of the round recompiled
  the vendored MathNet and showed its two existing `SYSLIB0051` warnings (`MathNetNumerics/Numerics/Exceptions.cs`,
  untouched). The final build showed none.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 214 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1429 succeeded, 0 failed, 0 skipped, 78 s (deadline 180 s). The count rises
  1426 → 1429 (+3), as the acceptance states, and the three new identities are `Passed` in the TRX.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 210 s (deadline 600 s). Count unchanged.
- **ui-tests (selected `SetupSaveLoadTests`):** `Passed!`, 4 succeeded, 0 failed, 0 skipped, 18 s (deadline 360 s).
  No UI test was touched, so the class's count is unchanged.
- **The fifteen s26 invariants** each find nothing (`0264-dev-invariants.log`).
- No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1429
  ui_smoke_tests: 245
  ui_tests: 4
```

## Architecture

- **One slot resolution.** The private `slotAssigned` (slot, then kit item, then `tryItemModelChoice`, then
  `SetupSlot.withItem`) is what both `AssignSlot` and `SwapSlot` call. The private `withSlotFilled` puts the filled
  slot in place. `AssignSlot`'s behaviour and texts are unchanged.
- **A switch is a load.** `SwitchToHeldSetup s` delegates to `ActivateSetup s.name` through the `let rec` update.
  It shares its drawing, its reset of selection and drag, its `Loaded setup '<name>'.` status and its never writing.
- **Only creation writes.** `CreateSwappedSetup s` goes through `guardKitWrite` and `KitLibraryContext.recordSetup`, and
  then `withSetupRecorded` keeps `kitSetups` in ordinal name order. The table becomes the swapped slots, recorded with
  the kit's items, so a dial-mounted item holds its reading.

## Deferred

Nothing in this step's scope. The slot bar that sends `SwapSlot` is step S8.4's, as the brief states.

## Gotchas

- **The Main scene seeds no sample.** `initMain` seeds only the source and the detector (`mainSeedElements`). The
  src / sample / det table in the same file is `initInverseWithKit`'s. So the brief's "a source, a sample and the
  seeded detector" means adding the sample (`withSample`), and `platesStocked` asserts those three kinds.
- **`swapTarget` matches kinds and contents only, slot by slot.** Rotations and positions are not compared, so a
  rotated table with the same items switches to the held setup and the drawn table takes that setup's angles. Making
  "new content" from a kit whose setups hold every fitting item takes another slot, not another angle.
- **Ordinal file order:** `Bench A (2).setup-json` sorts before `Bench A.setup-json` (space before dot), and
  `withSetupRecorded` puts `Bench A` before `Bench A (2)`.
- **The read-only reopen** opens the folder while the fact still holds it read-write. This works because a read-only
  open takes no lock. The scene's own kit from `initMain` and the reopened kit are both closed afterwards.

## Changelog

- 2026-11-10: Step 0264 added `SwapSlot`, which switches to a held setup by `ActivateSetup` and writes nothing, or
  creates, records and activates a setup of the next free name behind the read-only guard. It shares `AssignSlot`'s
  slot resolution. Constructor 1429 (+3), smoke 245, selected UI 4 (unchanged).
