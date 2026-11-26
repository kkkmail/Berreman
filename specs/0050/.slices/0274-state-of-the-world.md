# Step 0274 — state of the world

## Where we are

Spec 0050 section S8.3 (the naming template and the grid generator). Steps 268–271 gave the template grammar, reading
and writing names and `setupNameOf` / `setupNameAmong`; 272–273 named the window's setups by the kit's template and
let the kit bar edit it. This step lands the grid generator as pure Domain (`SetupGrids.fs`): options per slot, their
product, the impossible combinations unticked, and every ticked combination made a setup at once, named by the
template. Step 276 adds the `GenerateSetupGrid` message and the host that records; S8.4 adds the controls.

## What's working

- New `SetupGrids` module: `SlotChoice`, `SlotOptions`, `SlotPick`, `GridCombination` and `SetupGrid`
- `GridCombination.slotsOver` turns and fills each picked slot as `AssignSlot` does, then records every slot
- `SetupGrid` lists the product of its axes (first axis slowest), ticks / unticks, and starts with impossible combinations unticked
- `SetupGrid.generate` makes one setup per ticked combination, reusing a held or earlier-made one, named by the template or `Setup N`
- `SetupError.SlotItemTwiceErr` refuses one item in two picked slots; a failing combination refuses the whole grid
- Four `SetupGridTests` facts added; `KitTests`' setup-error render fact covers the new case

## Tests

All run locally on the final tree. None exceeded its effective deadline.

- build: pass (76 s full, 22 s final incremental; deadline 600 s). No warning from our code.
- constructor-unit-tests: pass, 1452 succeeded of 1452 (baseline 1448, +4: `a grid lists the product of its slots'
  options, the first slot slowest, and leaves out what is unticked`, `a grid starts with the combinations it cannot
  make unticked`, `a grid's setups take the template's names, and a combination a held setup holds is that setup`, `a
  ticked combination the grid cannot make refuses the whole grid`). The changed `every setup error renders its
  condition and its payload` passes. 68 s against a 180 s deadline.
- elevation (arc origin `f395597…`): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` (202 s; deadline 600 s).
- invariant-s27-* (all fifteen): pass.

```yaml
gates:
  constructor_unit_tests: 1452
```

## Architecture

- `SetupGrids.fs` compiles directly after `NamingTemplates.fs` and opens `Kit`, `ItemModels`, `Setups` and
  `NamingTemplates`. It is pure: the caller supplies the table, the kit's items, `choiceOf` (the library's resolution)
  and the held setups. Nothing is written.
- `slotsOver` checks, per pick in axis order: the slot is in the table, then the item is in `items`, is not in another
  picked slot, and has a model in `choiceOf`, then `withItem`. The refusal texts are the Ui `resolveSlotItem` /
  `slotAssigned` texts, so the grid and `AssignSlot` refuse alike. Every slot is then `recordedWith items`, as a commit
  records the table.
- `generate` folds with `Result.bind`, so it answers every setup or the first refusal, never part of a grid. Reuse is
  by equal current `slots` in `held` and then in this run's earlier setups. A new setup is `Setup.create` of
  `template.setupNameAmong … taken (SetupName.firstFree taken)`.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `SlotItemTwiceErr` is checked among picked slots only, as the step words it. `AssignSlot` does not refuse a second
  slot either, so an unpicked slot holding the picked item is not refused.
- A slot named by two axes takes the later pick; the step names no error for that, and none was added.
- In the test fixture, a grid without an `lpB` axis leaves the analyzer empty, so `lab` cannot name its setups and
  they are `Setup N`. One local red came from an extra assertion of mine that forgot this; it was corrected to the
  spec's behaviour.
- `SlotOptions` and `SlotPick` share the field `slot`. Annotate record constructions so F# picks the intended record.

## Changelog

- 2026-11-10 — Step 0274: the grid generator (`SetupGrids`: `SlotChoice`, `SlotOptions`, `SlotPick`, `GridCombination.slotsOver`,
  `SetupGrid` with `combinations` / `ticked` / `untick` / `tick` / `create` / `generate`) and `SetupError.SlotItemTwiceErr`;
  constructor-unit-tests 1448 → 1452.
