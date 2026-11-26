# Step 0277 — state of the world

## Where we are

Spec 0050 section S8.4 covers the slot bar, quick swap and the direct problem. This step is its first, and it adds the
slot bar's pure halves in `OpticalConstructor.Domain/Setups.fs`:

- which of the kit's items a slot's chip lists (`SetupSlot.fittingItems`);
- which item a letter typed on a chip picks (`PickLetter`, `SetupSlot.pickedByLetter`);
- the angle an object's chip shows (`SetupSlot.turnAngle`), as in 001 §J "Obj 1: QZ ▾ @ 45° ▾ … each dropdown lists
  compatible items only; … letters pick".

`turnAngle` is now the one angle rule. The naming template's `{phi<k>}` (step 271) calls it, and the chips will too.
The chips' views come in later S8.4 steps.

## What's working

- Add `PickLetter`, a typed chip letter upper-cased in the invariant culture, with `tryCreate` refusing empty text or a non-letter/digit
- Add `SetupSlot.fittingItems`, the kit's items whose resolved model `withItem` accepts, in their order
- Add `SetupSlot.pickedByLetter`: the next fitting item whose label starts with the letter, wrapping, never the slot's own
- Add `SetupSlot.turnAngle` (dial reading, else R1); `NamingTemplate.nameReadingOf` now calls it and writes the same names
- Add three `SetupTests` facts covering the chip lists, letter picks and object angles

## Tests

Every gate in the snapshot ran locally on the final tree, using the snapshot's own commands. None went over its
effective deadline.

- build: pass, 74 s (deadline 600 s), 0 errors. No warning came from our code; the only warnings are the two vendored
  MathNet `SYSLIB0051` ones that were already there.
- constructor-unit-tests (whole suite): pass, 1460 of 1460, in 72 s (deadline 180 s). The baseline was 1457 at step
  276, so the count rose by 3. In the TRX these show `Passed`:
  - `a slot lists the kit's items that fit it, in their order`
  - `a typed letter picks the next fitting item whose label starts with it, wrapping, and never the slot's own`
  - `an object's angle is its dial reading, or its R1`
  - all 14 `NamingTemplateTests`, unchanged
- elevation, over all 12 projects: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 197 s (deadline 600 s).
- invariant-s28-* (all sixteen): pass. Each must-find-nothing grep finds nothing.

```yaml
gates:
  constructor_unit_tests: 1460
```

## Architecture

- All four symbols sit at the end of `Setups.fs`, after step 261's `pinFor`, `slotsIn` and `itemsUsedBy`. No other
  member, cache or wildcard arm was added.
- `fittingItems` reuses `withItem` instead of re-transcribing the `slotKinds` check. That makes the chip's list exactly
  what `SwapSlot` (`resolveSlotItem` then `withItem`) accepts, by construction. An item whose model `choiceOf` cannot
  resolve is left out.
- `pickedByLetter` reads each label through `PickLetter.tryCreate`, so the invariant upper-casing rule exists once.
- `turnAngle` replaced the local `angleIn` in `nameReadingOf`, which is deleted. The `{phi<k>}` arm keeps its condition
  (Obj k holds a kit item), so every name it writes is unchanged.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `SetupTests` already had an item labelled LP2 (`dialLp`), but it is dial-mounted. The slot-bar facts use a new fixed
  `fixedLp2`, because the slice wants every item fixed unless named. They reuse the file's `laser` (Laser), `fixedLp`
  (LP1) and `plate` (QZ), which are exactly the items the slice describes.
- Ghost is made over the LP preset, so resolved it would fit `lpA`. Its absence from `lpA.fittingItems` therefore
  shows that an unresolvable item is left out, not merely an incompatible one.
- A letter never picks the slot's own item. When the slot holds the only item whose label starts with that letter
  (CPL1 and `c`), the answer is `None`, not the held item.
- `PickLetter.tryCreate` uses `System.Char`, fully qualified. `Setups.fs` does not open `System`.

## Changelog

- 2026-11-10: Step 0277 adds the slot bar's pure halves to `Setups.fs`: `PickLetter`, `SetupSlot.turnAngle`,
  `fittingItems` and `pickedByLetter`. The naming template's angle now reads `turnAngle`. constructor-unit-tests went
  from 1457 to 1460.
