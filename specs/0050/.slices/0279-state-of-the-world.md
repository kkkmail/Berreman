# Step 0279 — state of the world

## Where we are

Spec 0050 section S8.4 covers the slot bar, quick swap and the direct problem. Step 277 added the chips' pure halves in
`Setups.fs`, and step 278 added `RecordExperiment`. This step puts the slot bar's state in the window model:
`slotChipOpenOpt` (which chip's list is open), `model.slotChipItems` (what each chip lists) and the semantic message
`ToggleSlotChip` (001 §J "each dropdown lists compatible items only"; .spec-md §4, every user step is a message).
`SwapSlot` now closes the open list. Step 280 adds the bar that draws this state.

## What's working

- Add `TableAndElementRotationModel.slotChipOpenOpt`, the slot whose chip list is open, `None` from `initWith`
- Add `model.slotChipItems`, which reuses step 277's `fittingItems` over `kitItems` and `tryItemModelChoice` to list the kit's items that fit a slot
- Add `ToggleSlotChip`, which opens one slot's chip list at a time and closes the open one; it writes nothing, and an id the table lacks changes nothing
- Make `SwapSlot` close the open chip list on every outcome: switched, created or refused
- Add two `SetupModelTests` facts covering the chip lists, the toggles and the swaps that close them

## Tests

Each gate ran locally on the final tree, using the snapshot's own commands. No run went over its effective deadline.
These are development runs; the arc-runner's gate engine runs the gates themselves.

- **build:** pass in 77 s (deadline 600 s), with 0 errors. No warning came from our code; the only warnings are the two
  vendored MathNet `SYSLIB0051` ones that were already there.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` in 196 s (deadline 600 s), with known findings 0.
- **constructor-unit-tests:** pass, 1466 of 1466, in 69 s (deadline 180 s). The baseline was 1464, so the count rose
  by 2. In the TRX both new facts show `Passed`:
  - `a slot's chip lists the kit's items that fit it`
  - `ToggleSlotChip opens one chip's list at a time, and a swap closes it`

  Every existing `SwapSlot` fact passes unchanged.
- **ui-smoke:** pass, 245 of 245, in 205 s (deadline 600 s).
- **ui-tests:** the selected class, `SetupSaveLoadTests`, passes 4 of 4 in 16 s (deadline 360 s). No UI test changed,
  so its count is unchanged.
- **invariant-s28-\* (all sixteen):** each grep finds nothing (`specs/0050/.artifacts/0279-invariants.log`).

```yaml
gates:
  constructor_unit_tests: 1466
  ui_smoke_tests: 245
  ui_tests: 4
```

## Architecture

- `slotChipItems` is a one-line member on the model extension that holds `tryItemModelChoice`. It calls
  `slot.fittingItems model.kitItems model.tryItemModelChoice`, so a chip lists exactly what `SwapSlot` and `AssignSlot`
  accept: the same resolution and the same `withItem` check, with nothing re-transcribed.
- In `SwapSlot`'s arm, the old body is bound unchanged to `answered`. The arm then answers
  `{ answered with slotChipOpenOpt = None }`, so one line covers every outcome:
  - the switch, through `ActivateSetup`;
  - the create, and a failed write;
  - the refusal, and the read-only guard's refusal.
- `ToggleSlotChip` is not behind `guardKitWrite` because it writes nothing.
- No other message, field or option was added.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `kitItems` is held in ordinal label order (`kitItemsWith`), which gives LP1, LPd, Laser, QZ, QZ2. The facts assert
  both the explicit lists and that they follow `kitItems`' order.
- The toggle fact goes beyond the slice's single created swap and walks all three swap outcomes with the list open:
  created, refused, and switched back to the held setup. `swapTarget` matches setups on each slot's kind and content.
  Swapping QZ2 → QZ → QZ2 therefore switches back to the first setup the swaps created.
- The model holds function-valued fields, so it has no structural equality. "Changes nothing" is checked field by
  field and against the kit folder's bytes.
- `ActivateSetup` does not touch `slotChipOpenOpt`. The slice limits the close to `SwapSlot`, and a swap that switches
  goes through `ActivateSetup` but is closed by `SwapSlot`'s own line.

## Changelog

- 2026-11-10: Step 0279 adds the slot bar's state: `slotChipOpenOpt`, `model.slotChipItems` and `ToggleSlotChip`.
  `SwapSlot` now closes the open chip list on every outcome. constructor-unit-tests went from 1464 to 1466.
