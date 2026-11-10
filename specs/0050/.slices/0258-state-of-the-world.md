# Step 0258 — state of the world

## Where we are

Spec 0050 section S8.2 (setups as slots), step 0258 (IMPLEMENT).

- Step 0256 made setups Domain data, and step 0257 gave slots their light-path roles.
- This step adds the pure halves of AssignSlot and SwapSlot:
  - which slots an item fits;
  - what a slot becomes when it holds an item;
  - how a dial reading is drawn and recorded;
  - which setup a swap lands on.
- Nothing writes yet. Steps 263 and 264 add the host that records.

## What's working

- Added `ParameterState.currentValue`, and `Item.dialZeroAngle` over it. `ParameterValues.ofParameters` now reads
  through `currentValue`, and `ParameterValue.angle` is intrinsic in `ItemModels.fs`.
- Added `ItemModelChoice.libraryEntryOpt` and `slotKinds`: the library entry a slot binds to, and the slot kinds an
  item fits.
- Added `SetupSlot.withItem`: it refuses an item that does not fit, records a dial-mounted item's reading, and binds
  the slot as the table binds a library entry.
- Added `SetupSlot.withoutItem`, plus `drawnWith` and `recordedWith`, which map readings and physical angles through
  the item's current zero.
- Added `SetupSwap` and `Setup.swapTarget`: switch to the held setup holding the swapped content, or create a new one.
- Added the five census tests, so the constructor suite went from 1416 to 1421.

## Tests

Development runs on the final tree used each gate's own command. They do not claim a verdict; the supervisor runs
the gates.

- **build:**
  - Exit 0, with 0 errors and 0 owned-code warnings, in 43 s (deadline 600 s).
  - A non-incremental Domain rebuild showed only the two existing vendored MathNet `SYSLIB0051` warnings.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, in 202 s (deadline 600 s).
- **constructor-unit-tests:**
  - `Passed!`, with 1421 succeeded, 0 failed and 0 skipped, in 69 s (deadline 180 s).
  - All five census identities passed, with names verbatim.
- **The fifteen s26 invariants:** each finds nothing.
- No run exceeded its deadline. Logs are `specs/0050/.artifacts/0258-dev-*.log`.

```yaml
gates:
  constructor_unit_tests: 1421
```

## Architecture

- **One rule for a parameter's current number.** `ParameterState.currentValue` holds it, and the evaluator
  (`ofParameters`) and the dial (`dialZeroAngle`) both read it there. `ParameterValue.angle` moved down to
  `ItemModels.fs` so that `Kit.fs` can use it.
- **Slot kinds come from one source per item.** A preset- or sample-bound choice fits the kinds its library entry's
  `forKinds` gives, the same rule the table uses for binding. A library model fits its form's kinds, listed with no
  wildcard arm.
- **`withItem` binds like the table.** It sets `valueId` and `placementEmissionForEntry` exactly as
  `withBoundSampleEmission` does, so a drawn element and its item cannot disagree. An item model leaves the binding
  alone.
- **The table draws physical angles; the setup records readings.**
  - `drawnWith` turns a reading into R1, and `recordedWith` turns R1 back into a reading.
  - Both go through `DialSign.physicalAngleOf` / `readingOf` and the item's current `dialZero`.
  - A private helper, `tryDialReadingIn`, is the one place that finds a dial reading's dial-mounted item.
- **`swapTarget` matches on content only.** It compares each slot's kind and content, in order, so a different dial
  reading is different content.

## Deferred

Nothing. Every requirement of the slice is implemented. The host-side recording (steps 263 and 264) belongs to later
steps by the spec's own plan.

## Gotchas

- **Naming the slot's kind in the error.** Domain has no rendering member for `CatalogueKind`; `kindName` is in Ui.
  `IncompatibleSlotItemErr`'s reason therefore names the kind through a local, exhaustive match in `withItem`, for
  example `'Laser' in the linear polarizer slot 'lp'`.
- **A binding form with no binding fits no slot.** A `LibraryModelChoice` of the source-preset, ideal-polarizer or
  stack-sample item model has `slotKinds = []`, and `ItemDraft.addTo` already refuses such a choice.
- **`drawnWith` sets R1 directly.** A recorded reading defines the drawn angle, so a locked R1 is still drawn at the
  reading's angle.
- **Content-only matching.** `swapTarget` ignores the R1 of a `PlacementTurn` slot, because it compares kinds and
  content only.

## Changelog

- 2026-11-10: Step 0258 added the pure slot assignment and swap: `currentValue`, `dialZeroAngle`, `slotKinds`,
  `libraryEntryOpt`, `withItem`, `withoutItem`, `drawnWith`, `recordedWith`, `SetupSwap` and `swapTarget`, plus five
  tests (1416 → 1421).
