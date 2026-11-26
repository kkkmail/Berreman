# Step 0280 — state of the world

## Where we are

Spec 0050 section S8.4 covers the slot bar, quick swap and the direct problem. Steps 277–279 added its pure halves:
`fittingItems`, `pickedByLetter` and `PickLetter`; `RecordExperiment`; and the bar's model state, `slotChipOpenOpt`,
`slotChipItems` and `ToggleSlotChip`. This step draws the bar.

It sits directly above the Main table (001 §J "Quick swap", §O). It shows one focusable chip per slot in light order,
the open chip's list of fitting items, and Record experiment. Tab moves between the chips and a typed letter picks an
item.

## What's working

- Add the slot bar above the Main table (`SlotBarView.slotBarView`):
  - one chip per slot in light order, each reading `<role>: <item> ▾`, with an object adding `@ <angle>`;
  - Record experiment, shown only while the kit takes writes.
- Pressing a chip focuses it and opens its list of the kit's fitting items. Pressing an item sends `SwapSlot`, and
  pressing the held item closes the list.
- Tab moves between the chips in light order, and a letter typed on a chip swaps in the next fitting item.
- Add the stable ids `SlotBar`, `SlotChipChoiceList`, `RecordExperimentButton`, `SlotChip_<role>` and
  `SlotChipChoice_<role>_<item GUID>`, plus the chip captions (`chipCaption`, `degreeCaption`, `ItemLabel.caption`).
- Add `SlotBarTests`, with four ui-tests facts and two ui-smoke facts. The read-only `KitWindowTests` fact also checks
  that Record experiment is withheld.

## Tests

Each gate ran locally on the final tree, using the snapshot's own commands. No run went over its effective deadline.
These are development runs; the arc-runner's gate engine runs the gates themselves.

- **build:** pass, 0 errors and 0 warnings, in 16 s incremental (deadline 600 s). The first full build took 51 s.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` in 203 s (deadline 600 s), with known findings 0.
  It was re-run after the last source edit.
- **constructor-unit-tests:** pass, 1466 of 1466, in 69 s (deadline 180 s). The count is unchanged, since this step
  adds no domain test.
- **ui-smoke:** pass, 247 of 247, in 201 s (deadline 600 s). That is 245 before plus 2:
  - `pressing a chip and an item swaps the table to a kit setup that holds it`;
  - `pressing Record experiment twice records two experiments over the active setup`.

  Both show `Passed` in the TRX.
- **ui-tests:** pass, 369 of 369 over the 18 selected classes, in 171 s (deadline 360 s). That is 365 before plus 4:
  - `the slot bar UiIds are the stable intent-named ids`;
  - `the slot bar shows one chip per slot in light order above the table, each naming its item and an object its angle`;
  - `an open chip lists only the kit's items that fit its slot`;
  - `Tab moves between the chips in light order, and a typed letter picks the next fitting item`.

  The changed `KitWindowTests.a read-only window withholds every write control` also passes. All five show `Passed` in
  the TRX.
- **invariant-s28-\* (all sixteen):** each grep finds nothing, including with `--untracked`, which covers the two new
  files (`specs/0050/.artifacts/0280-invariants.log`).
- **Mutation check:** with the chips' tab stop turned off, the Tab fact and the chip fact both went red. The code was
  then restored.

```yaml
gates:
  constructor_unit_tests: 1466
  ui_smoke_tests: 247
  ui_tests: 369
```

## Architecture

- `SlotBarView.fs` is compiled directly before `TableAndElementRotationView.fs`. It is a pure projection of its own
  state and handler records, the way the Experiments bay is: `SlotChipState`, `SlotBarState` and `SlotBarHandlers` feed
  `slotBarView`.
- The host builds the bar's two inputs:
  - `model.slotBarState` builds the state;
  - a private `slotBarHandlers` beside `experimentHandlers` builds the handlers. It resolves a typed letter against the
    model with `pickedByLetter` over `slotChipItems` and dispatches `SwapSlot`; no pick dispatches nothing.
- `mainView`'s table surface is now a `DockPanel`: the bar is docked on top and the old panel fills the rest. The canvas,
  its gestures, the element-menu overlay and the page's scrolling are unchanged. A full-surface bay shows no table and
  no bar.
- No message, model field or option was added.
- The chips are read from `model.recordedSlots`, which is `model.elements` as a setup records them, in the same order
  and with the same ids. So a dial-mounted object shows the reading the naming template writes (step 271). For that
  reason `slotBarState` sits beside `recordedSlots`, not beside `slotChipItems`.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- `verbButton`'s idle colours are private to `MaterialsWindowView.fs`, which this step may not edit. `SlotBarView.fs`
  therefore re-declares the pair privately, as about 20 view modules already do. It reuses the public
  `positiveBackground` and `brush`.
- `Setups.PickLetter.tryCreate` does not compile, because the single union case shadows the type. Open
  `OpticalConstructor.Domain.Setups` and write `PickLetter.tryCreate`.
- The chip's text handler resolves the letter against the model of its own render. It therefore subscribes with
  `SubPatchOptions.Always`, and so does the press handler.
- The bar moves the Main table down by its height. Tests must find the table through its canvas, never by fixed window
  coordinates. All the selected Main classes do.
- The `ELEV004` tally, which the gate measures but does not enforce, rose by 3 in the Ui project: the chip's accessible
  name, its view key and `chipCaption`'s label.
- A plain `git grep` skips untracked files, so before the commit the section invariants do not see new files.

## Changelog

- 2026-11-11: Step 0280 adds the slot bar above the Main table. It shows chips in light order and the open chip's
  fitting items, sends `SwapSlot` on a pick, supports Tab and typed letters, and offers Record experiment only on a
  writable kit. ui-smoke went from 245 to 247 and the selected ui-tests from 365 to 369.
