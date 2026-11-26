# Step 0280 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and the gates snapshot. There is no operator note.
- Read step 279's state of the world and the code the slice names: the model, `slotChipItems`, the messages,
  `experimentHandlers`, `mainView`, `MaterialsWindowView.verbButton` / `actionButton` and `Setups.fs`. Also read the
  test helpers it cites:
  - `ScrollExtentTests.constructorWindow`;
  - `LibraryWindowTests.clickOn`;
  - the `EmbeddedChartTests` Enter press;
  - the `FacetedTreeControlsTests` typing;
  - `ItemsPanelTests`;
  - `KitWindowTests.assertWriteControls`.
- Wrote the impl plan.
- `UiIds.fs`: added the slot-bar group after the flow strip, holding `slotBar`, `slotChipChoiceList` and
  `recordExperimentButton`.
- `ConstructorUiIdOfDomain.fs`:
  - `SlotRole.uiKey`, declared just before the `ConstructorUiId` extension that holds `elementHover`, which uses it;
  - `slotChip` and `slotChipChoice` on that extension;
  - at the end of the file, `ItemLabel.caption`, `Angle.degreeCaption` and `RoledSlot.chipCaption`.
- New `SlotBarView.fs`, compiled directly before `TableAndElementRotationView.fs` by one `<Compile>` line in the Ui
  fsproj.
- `TableAndElementRotationView.fs`:
  - `model.slotBarState`;
  - the private `slotBarHandlers`, right after `experimentHandlers`;
  - `mainView`'s `tableSurface` is now a `DockPanel` with the bar docked on top. The comment above it gains one line.
- Tests:
  - new `SlotBarTests.fs`, last in the Ui.Tests compile list, with the six facts;
  - `KitWindowTests.assertWriteControls` also checks Record experiment. Its doc now cites S8.4.
- First build failed on `Setups.PickLetter.tryCreate` (FS0039). Fixed by opening `OpticalConstructor.Domain.Setups` in
  `SlotBarView.fs`.
- All six `SlotBarTests` facts passed on their first run.
- Mutation check: I turned `Border.isTabStop` to `false`, rebuilt and re-ran the class.
  - The Tab fact went red, and so did the chip fact, which asserts each chip is a focusable tab stop.
  - I restored the file, and the backup is deleted.
- Simplified `chipCaption`'s label to `item.label.value`. It had made a caption only to unwrap it again.
- Rebuilt and ran every gate in the snapshot on the final tree (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/SlotBarView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SlotBarTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

Every file is in the slice's authorized edits.

## Decisions

- **Chips are read from `model.recordedSlots`, not literally from `model.elements`.** This is a deviation from the
  slice's wording.
  - Why: the slice also asks each object chip to show "the angle the naming template writes for it (step 271)", and
    the template names the recorded slots (`tableSetup`, `SwapSlot`).
  - `recordedSlots` is `model.elements` mapped by `recordedWith`: the same slots, ids and order. Only a dial-mounted
    item's reading differs, and it is the current one rather than the reading at the last record.
  - With `model.elements`, an object on a dial would keep showing a stale angle after the user turned it on the table.
  - Consequence: `slotBarState` sits right after `recordedSlots` on that extension. `recordedSlots` is declared in a
    later extension than `slotChipItems`, which the slice names. `slotChipItems` stays where step 279 put it.
- **The idle colours are re-declared privately in `SlotBarView.fs`.** The chip has to look like
  `MaterialsWindowView.verbButton`'s box.
  - That module keeps `idleBackground` and `idleBorder` private, and `MaterialsWindowView.fs` is not among this step's
    authorized edits.
  - The tree already holds this same private pair in about 20 view modules.
  - The public `MaterialsWindowView.positiveBackground` and `MaterialsWindowView.brush` are reused, not copied.
- **`chipCaption`:** an object slot holding an item the kit lacks also gets ` @ <angle>`. The slice says "an
  `ObjectSlotRole` holding an item", and the slot does hold one. The angle belongs to the slot, not the item.
- **Typed text:** the event is handled only when `PickLetter.tryCreate` answers. Any other text is left to bubble on.
- **A press focuses the chip:** the handler takes the nearest `Border` from the event source, including the source
  itself, and focuses it. That is the chip or the chip holding the pressed caption. Then it calls `toggleChip`.
- **Chip accessible name:** each chip also carries `AutomationProperties.Name` (its caption), as AGENTS.md asks of
  every interactive control. `verbButtonOn` does the same.
- **Subscriptions:** the chip's press and text handlers subscribe with `SubPatchOptions.Always`. `pickByLetter` is
  resolved against the model of the render it came from, so a stale handler would pick against an old model.
- **`open OpticalConstructor.Domain.Setups`** in `SlotBarView.fs`: `Setups.PickLetter.tryCreate` resolves `PickLetter`
  to the union case. It is the same shadowing `TableAndElementRotationView.fs` notes for `QuickPickThreshold`.
- **Read-only check:** in `KitWindowTests`, Record experiment is checked as present or absent, the way Save template
  is, because the bar withholds it by leaving it out rather than disabling it.
- **Assertions added inside the authorized facts:**
  - the chip fact checks each chip is focusable and a tab stop;
  - the open-chip fact checks the open chip and the held item are on `positiveBackground` and the other item is not;
  - the press fact checks the press focused the chip;
  - the Tab fact records each model the program answers, so "`x` changes nothing" is checked as "no message was
    sent", and the sample still holds QZ.

## Testing state

Development runs used the snapshot's own commands on the final tree. None exceeded its effective deadline.

| Gate | Result | Time (deadline) |
|---|---|---|
| build | pass, 0 errors and 0 warnings (incremental) | 16 s (600 s); the first full build was 51 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0; re-run after the last source edit | 203 s (600 s); the first run was 198 s |
| constructor-unit-tests | pass, 1466 of 1466, unchanged | 69 s (180 s) |
| ui-smoke | pass, 247 of 247 (245 before, +2) | 201 s (600 s) |
| ui-tests (18 selected classes) | pass, 369 of 369 (365 before, +4) | 171 s (360 s) |
| invariant-s28-* (16) | every grep finds nothing, also with `--untracked` | — |

- The TRX reports show all seven named facts `Passed`:
  - in `ui-tests.trx`, the four `SlotBarTests` facts without a trait and
    `KitWindowTests.a read-only window withholds every write control`;
  - in `ui-smoke.trx`, the two `ui-smoke` facts.
- The ui-tests figure of 365 before is the selected classes without the four new facts. The `KitWindowTests` fact count
  is unchanged.
- Elevation's "suppressed" tally is `ELEV004`, which the gate excludes and only measures. The Ui project's tally went
  from 512 to 515, and it is still 515 on the final tree:
  - the chip's accessible name (`chip.caption.value`);
  - the chip's view key (`id.value`), as `verbButtonOn` keys its box;
  - `chipCaption`'s label.

## Artifacts

- `specs/0050/.artifacts/0280-build.log`
- `specs/0050/.artifacts/0280-elevation.log`
- `specs/0050/.artifacts/0280-constructor-unit-tests.log`
- `specs/0050/.artifacts/0280-ui-smoke.log`
- `specs/0050/.artifacts/0280-ui-tests.log`
- `specs/0050/.artifacts/0280-slotbar-tests.log`: the class alone, first green run
- `specs/0050/.artifacts/0280-mutation-tests.log`: the tab-stop mutation, two facts red
- `specs/0050/.artifacts/0280-invariants.log`

## Gotchas

- `kitItems` is held in ordinal label order, which gives LP1, LP2, Laser, QZ, QZ2. The fixture asserts that order.
- `Setups.PickLetter.tryCreate` does not compile, because the single case shadows the type. Open the module and write
  `PickLetter.tryCreate`.
- A plain `git grep` skips untracked files, so before the commit it does not see the two new files. The invariants were
  run both ways.
- The bar moves the Main table down by its height. Every selected Main test that places the pointer on the table goes
  through the canvas's own position, and they all pass.
