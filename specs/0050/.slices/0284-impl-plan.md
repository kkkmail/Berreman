# Step 0284 — impl plan

## Approach

The step proves the direct problem and the quick swap through a constructor window over a kit folder, and records the
slot bar in `AGENTS.md`. No product code changes: `kitWindowView` already shows `mainView`, holding the slot bar (step
280) and the grid panel (step 283), on the Setups and Experiments steps (step 242).

1. `AGENTS.md`: one bullet of at most six lines after the gotcha "Names come from the kit's naming template", with the
   slice's text (**The slot bar is the quick swap**). Nothing else in the file changes.
2. `KitWindowTests.fs`: one new fact with trait `ui-smoke`,
   `acceptance: in a kit window, Record experiment twice, a chip swap and Record experiment again write two setups and
   three experiments, and the reopened window lists them`.
   - The kit folder is `TestAppScope.freshKitFolder owner` (a new GUID under the test output, owner `KitWindowTests`),
     opened read-write through `openKitAt` and loaded through `loadKit` (`kitWindowModel`), as the copy facts open theirs.
   - `LP1`, `QZ` and `QZ2` are added through the Items panel's messages (`OpenAddItemForm`, `ChooseNewItemModel`,
     `SetNewItemLabel`, `AddItem`), each choosing an offer the open form lists.
   - A linear polarizer and a sample are added, LP1 and QZ assigned (`AssignSlot`), `SetSetupName "Bench"`,
     `CreateSetup`, `ExpChooseElement` on the polarizer; the model is on `SetupsFlowStep`.
   - In `withKitWindow` (`kitWindowView`): press Record experiment twice, the Obj 1 chip and QZ2's box, Record
     experiment again; the kit bar's status and the chip's caption are checked along the way.
   - After closing the kit: the folder holds `Bench.setup-json` and `Bench (2).setup-json` (the latter holding QZ2), and
     `E1`/`E2` over `'Bench' revision 1`, `E3` over `'Bench (2)' revision 1` (read back with `reopenedCopy`).
   - Reopened through `openKitAt` and `loadKit`: `kitSetups` hold Bench and Bench (2), the collection E1, E2 and E3; the
     reopened kit window on the Experiments step lists both saved setups and the three experiment rows.

## Files

- `AGENTS.md`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

Both are the slice's authorized edits; nothing else is touched.

## Risks

- `KitWindowTests` compiles before `SlotBarTests`, so the slot-bar fixture helpers (`withItemAdded`,
  `linearPolarizerChoice`) cannot be called; moving them would edit files outside this step's effects. The new private
  helper picks the offer from the add form's own list instead of building a choice, so it is not a copy of
  `withItemAdded`'s choice-taking shape.
- The kit window is 1000 × 700, smaller than the slot-bar facts' 1000 × 900; `pressAtCentre` brings each box into view.
- `kitSetups` order on reload: assert the order the model holds after observing it (name order expected).
