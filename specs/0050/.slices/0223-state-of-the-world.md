# Step 0223 — state of the world

## Where we are

Spec 0050, section S6 ("Send and receive kits; copy experiments between kits"), step 0223 (IMPLEMENT). This is the
S6 prerequisite and closure for 001 §H / §S6: the constructor window holds its kit's items, so step 225's
experiment copy can carry item mapping before S7.

- Step 218 gave `KitProxy.readItems` / `writeItem`.
- This step reads the items when the kit opens (`OpenedKit.items`) and holds them in the window
  (`TableAndElementRotationModel.kitItems`).
- Both the load and the reload set `kitItems`.

## What's working

- Opening a kit reads its items in label order into `OpenedKit.items`. An item file that does not read refuses the
  open and leaves the kit unlocked.
- The window model holds `kitItems`: `loadKit` sets it from the opened kit, and `ReloadKitChanges` re-reads it.
  A reload failure keeps the model and the offer.
- Added `KitOpeningTests` and `KitWindowModelTests` facts for items at open, the refused open, and reloading an
  item another writer wrote.

## Tests

These are development runs over the final source, recorded in `0223-impl-log.md`. The gate engine gives the
verdict (Invariant 6).

- **build:** exit 0, 0 warnings, 0 errors on the final rebuild.
- **constructor-unit-tests** (all discovered): **1323** passed, 0 failed, 0 skipped.
  - That is 1321 + 2, the two declared identities.
  - Each is `Passed` in the TRX.
- **ui-smoke:** **237** passed in 3 m 10 s, inside the 600 s attempt timeout (`.manual/015`).
- **ui-tests** (selected `KitWindowTests`): **8** passed.
- **elevation** (from the arc origin `f395597`, ELEV004 excluded):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 197 s against its 600 s estimate;
  - suppressed findings stay at 1311.
- **The fourteen `invariant-s22-*` gates:** each holds; their greps find nothing.
- **The facts bite.** Three mutants were each caught, then restored byte for byte:
  - no items at open;
  - reload keeps the old items;
  - an item read failure swallowed at open.
- No deadline was exceeded. Not in this step's roster: `unit-tests`, because no solver code changed.

```yaml
gates:
  constructor_unit_tests: 1323
  ui_smoke_tests: 237
  ui_tests: 8
```

## Architecture

- **The open reads items like experiments.** `openedOver` reads them right after the experiments, in the same
  `Result` chain.
  - A refusal shares the one failure branch that closes the kit (step 150), so no new release path exists.
- **The window holds the items with the other kit content.** `kitItems` is set only by `withKitContent`, the one
  function `loadKit` and `ReloadKitChanges` share (step 187 (4)). An open and a reload set it the same way.
- **The reload offer needed no change.** `.item-json` is already in `KitFileKind.all` (step 123), so step 130's
  check offers a changed item file.

## Deferred

Nothing in this step's scope is deferred. Step 225 (the experiment copy message) must pass `model.kitItems`, per
this step's brief; that is step 225's own work, not left over from this one.

## Gotchas

- `DialPositive` / `DialNegative` are in `OpticalConstructor.Domain.ItemModels`, not `Kit`. A test that drafts an
  item opens both.
- `Result.orElse` is not in the pinned FSharp.Core.
- `KitItemFileTests` compiles after `KitOpeningTests` and `KitWindowModelTests`, and its fixtures are private.
  - The two touched classes each draft their own CPL1 / LP1 through `ItemDraft.toItem` over
    `BuiltinCatalogue.itemModel`, the per-class fixture pattern of every item test class.
  - The GUIDs differ per class.

## Changelog

- 2026-11-09 — Step 0223: the window holds its kit's items. Added `OpenedKit.items`, read at open, where an
  unreadable item file refuses the open and unlocks the kit. Added `TableAndElementRotationModel.kitItems`, set by
  `loadKit` and `ReloadKitChanges`. Added two facts; constructor-unit-tests went from 1321 to 1323.
