# Step 0284 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and the gates snapshot. There is no operator note.
- Read step 283's state of the world, `KitWindowTests.fs`, `SlotBarTests.fs`, `TestAppScope.fs`, the `SwapSlot`,
  `RecordExperiment` and `CreateSetup` arms, the add form (`AddItemForm.offers`) and the saved-setups section.
- Wrote the impl plan.
- `AGENTS.md`: added the six-line bullet **The slot bar is the quick swap** right after "Names come from the kit's
  naming template", with the slice's text. Every member it names exists (`SetupSlot.fittingItems`,
  `SetupSlot.pickedByLetter`, `SetupGridDraft`, `SlotBarView.fs`, `SetupGridView.fs`).
- `KitWindowTests.fs`:
  - `itemFilesIn` now calls a new `kitFilesIn folder pattern`, so the new fact lists the setup and experiment files
    through the same helper rather than a second copy of it;
  - new private helpers `withOfferAdded`, `linearPolarizerOffer`, `plateOffer` and the record `DirectProblemPieces`;
  - the new `ui-smoke` fact `acceptance: in a kit window, Record experiment twice, a chip swap and Record experiment
    again write two setups and three experiments, and the reopened window lists them`.
- First build green (77 s). The first run of the fact failed with "The add form must offer a model for LP1."
  `kitWindowModel` leaves the in-memory, empty item-model store of `initMainWithKit`. The App's windows set
  `itemModels = context.itemModels.overlaidWith opened.overlay` in the private `ConstructorScene.withAppContext`. The
  fact now does the same on its own model (`open OpticalConstructor.Domain.KitLibrary` for the extension). Second run:
  passed.
- Mutation check: I made the slot bar's item press send `AssignSlot` instead of `SwapSlot`
  (`TableAndElementRotationView.fs` line 4556), rebuilt and re-ran the fact. It went red at the swap's status line
  ("KitStatus must be in the window": `AssignSlot` clears the status and creates no setup). I restored the file from a
  copy, deleted the copy, and checked that `git status` shows no change to it.
- Inlined a one-use helper and removed a comment that only restated the code.

## Files modified

- `AGENTS.md`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

Both are in the slice's authorized edits. `TableAndElementRotationView.fs` was changed only for the mutation check, and
it is restored byte-for-byte.

## Decisions

- **Kit folder:** `TestAppScope.freshKitFolder owner` (`<test output>/TestAppScope/KitWindowTests/Kits/<new GUID>`). The
  slice writes `Path.Combine(AppContext.BaseDirectory, "KitWindowTests", <a new GUID>)` "as its copy facts open their
  kits (step 226)". The copy facts open theirs through `freshKitFolder`, which is a new GUID under the test output in
  the `KitWindowTests` owner. Reusing it keeps one rule for test kit folders, and a second path builder would be a copy.
- **Items through the panel's messages, from the form's own offers:** `withOfferAdded` sends `OpenAddItemForm`, then
  `ChooseNewItemModel` with the offer the open form lists, then `SetNewItemLabel` and `AddItem` with the form's draft.
  - `KitWindowTests` compiles before `SlotBarTests`, so `SlotBarTests.withItemAdded` and `linearPolarizerChoice` cannot
    be called from it.
  - Moving them would edit `SlotBarTests.fs`, `SetupGridPanelTests.fs` and the fsproj compile order, none of which are in
    this step's authorized edits.
  - Taking the offer from the form, instead of building an `ItemModelChoice`, is closer to what the panel does and
    builds no choice.
  - QZ2 is the 2 mm plate, as in the slot-bar and grid facts.
- **Item models:** the fact's model reads `ctx.itemModels.overlaidWith opened.overlay`, as `ConstructorScene.withAppContext`
  sets it for the App's windows. `kitWindowModel` itself is unchanged, so no existing fact's fixture moves.
- **"The reopened window lists them":** beyond the slice's model check (`kitSetups` hold Bench and Bench (2), the
  collection holds E1, E2 and E3), the fact shows the reopened model in `kitWindowView` on the Experiments step. It
  checks both `SavedSetup_*` buttons and the three experiment rows are visible, which is what the fact's name says.
- **Checks along the way in the window:**
  - the kit bar's status after the second Record (`Recorded E2 over 'Bench' revision 1.`), after the swap
    (`Created setup 'Bench (2)' by the swap.`) and after the third Record (`Recorded E3 over 'Bench (2)' revision 1.`);
  - the Obj 1 chip's caption after the swap (`Obj 1: QZ2 @ 0° ▾`).
- **File contents:** read back read-only through the existing `reopenedCopy`. Both setups are at revision 1, Bench's
  sample slot holds QZ and Bench (2)'s holds QZ2, and E1, E2 and E3 reference the setups the slice names.

## Testing state

These are development runs on the final tree, using the snapshot's own commands. The gate engine runs the gates
themselves. No run went over its effective deadline.

| Gate | Result | Time (deadline) |
|---|---|---|
| build | pass, 0 errors and 0 warnings (incremental, after the restore) | 28 s (600 s); the first full build was 77 s, with only the vendored MathNet `SYSLIB0051` warnings |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 202 s (600 s) |
| ui-smoke | pass, 249 of 249 (248 before, +1) | 219 s (600 s) |
| ui-tests (`KitWindowTests`) | pass, 16 of 16, unchanged (HEAD's class has 16 facts without the ui-smoke trait) | 23 s (360 s) |
| invariant-s28-* (16) | every grep finds nothing, also with `--untracked` | — |

- `ui-smoke.trx` shows the new fact `Passed`.
- The ui-smoke run also includes `SlotBarTests.pressing a chip and an item swaps the table to a kit setup that holds
  it`, which fails under the mutant. It passed, which confirms the restored `OpticalConstructor.Ui` was rebuilt.
- Elevation's suppressed tally (ELEV004, measured only) for the Ui project is 518, the same as step 283's tree. This
  step changes no product source.

## Artifacts

- `specs/0050/.artifacts/0284-build.log`: the final full build
- `specs/0050/.artifacts/0284-elevation.log`
- `specs/0050/.artifacts/0284-ui-smoke.log`
- `specs/0050/.artifacts/0284-ui-tests.log`
- `specs/0050/.artifacts/0284-invariants.log`
- `specs/0050/.artifacts/0284-new-fact.log`: the new fact alone, green
- `specs/0050/.artifacts/0284-mutation-tests.log`: the `AssignSlot` mutant, red at the swap

## Gotchas

- `kitWindowModel` (`initMainWithKit` + `loadKit`) has an empty item-model store, so its add form offers no binding
  model. A fact that adds items through the add form must give the model the app's item models, as the App does.
- `ItemModelProxy.overlaidWith` is an extension in `OpticalConstructor.Domain.KitLibrary`, so the module must be opened.
- In ordinal order `Bench (2).setup-json` sorts before `Bench.setup-json`, because a space sorts before a dot.
