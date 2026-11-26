# Step 0262 — impl log

## Progress

- Read the protocol, the project prompt, the slice, step 0260 / 0261's logs and the code they touched.
- UiIds: `activeSetupText` (`"ActiveSetupText"`) and `saveSetupButton` (`"SaveSetupButton"`) beside the setup-row ids.
- View model: `SaveSetup` message and arm; model members `recordedSlots`, `tableWrittenOver`, `withSetupRecorded`
  shared by `ExpCommit`, `CreateSetup` and `SaveSetup`.
- View: the active-setup line (wrapping `TextBlock`) first in the setup section; `Save setup` after `Create setup`.
- Tests: two new `SetupModelTests` facts; the read-only model fact sends `SaveSetup` with a setup active; the smoke
  fact renamed and extended; the new live-window ui-test; `KitWindowTests` read-only fact checks Save setup.
- Solution build: exit 0, 0 warnings, 0 errors (26 s incremental).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/SetupModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/SetupSaveLoadTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

## Decisions

- **Shared model members instead of a third copy** (AGENTS.md "Reuse, never copy"). `ExpCommit` and `CreateSetup`
  each carried the recorded-slots expression and the sorted replace-in-`kitSetups` update; `ExpCommit` carried the
  brief's `active.revisedWith … (active.pinFor …)`. `SaveSetup` needs all three, so they became
  `model.recordedSlots`, `model.tableWrittenOver active` and `model.withSetupRecorded setup`, and the two existing
  arms call them. Behaviour is unchanged (`CreateSetup`'s cons-then-sort equals filter-cons-sort, as a created name is
  never held). The `CreateSetup` arm comment that repeated `recordedSlots`' doc was deleted.
- **"In place of its name's"** keeps `kitSetups` in ordinal name order, as `ExpCommit` and `CreateSetup` already do.
- **The active-setup line sits outside the horizontal row**, first in the setup section after its heading: AGENTS.md
  forbids a wrapping `TextBlock` inside a horizontal `StackPanel`.
- **"Withheld" is disabled, never hidden** (`availableWorkbenchButton`, step 185). `KitWindowTests`' "also finds no
  `saveSetupButton`" is read as: the read-only window offers no Save setup — it is in `writeControlIds`, so the
  read-only window finds it disabled and the read-write window finds it enabled.
- **A setup is active in both read-only facts.** So the refusal is the kit's access and not a missing setup, a
  `Stored bench` setup is recorded through the model's `CreateSetup` while the kit is read-write and activated
  (`ActivateSetup`, which writes nothing) in the read-only window. In `KitWindowModelTests` the table then differs from
  it (a polarizer is added), so an unguarded `SaveSetup` would write; the fixture's other refused messages keep their
  assertions (`CreateSetup` still types `Bench`, a name the kit does not hold).
- **The live-window ui-test turns the polarizer through the view's dispatch** (captured by a view wrapper), the same
  `RotSetAxis` the Rotate bay sends; Create setup, the Experiments bay's Add and Save setup are pressed in the window.
  `clickSetupControl` was split so the Add box, which carries no automation name, is pressed by `pressSetupControl`.

## Testing state

Final tree, each gate's own command (logs `specs/0050/.artifacts/0262-dev-*.log`):

- build: exit 0, 0 warnings, 0 errors (26 s incremental).
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 201 s.
- constructor-unit-tests: 1423 passed, 0 failed, 0 skipped, 72 s (1421 → 1423, +2).
- ui-smoke: 245 passed, 0 failed, 0 skipped, 203 s (renamed identity, so the count is unchanged).
- ui-tests (KitWindowTests, SetupSaveLoadTests): 19 passed, 0 failed, 0 skipped, 28 s (18 → 19, +1).
- fifteen s26 invariants: nothing found.
- Every census identity checked in the TRX files: the two new constructor facts, the changed read-only model fact,
  the renamed smoke fact, the new ui-test and the changed read-only window fact are `Passed`; the old smoke identity
  is gone.
- Whole non-smoke UI suite (beyond the gate): 677 of 678 passed, 218 s. The one failure,
  `ItemsPanelTests.the add form lists the library models by kind and offers Add and Cancel on one row, Cancel to the
  right`, threw in Avalonia's headless session start-up (`HeadlessUnitTestSession.EnsureIsolatedApplication` →
  `Dispatcher.VerifyAccess`, "a different thread owns it") before its body ran; the class re-run alone passed 5 of 5
  (8 s). It is outside this step's selection and touches nothing this step changed.
- No run exceeded its deadline.

## Artifacts

- `specs/0050/.artifacts/0262-dev-*.log` — development runs of each gate's command, the whole non-smoke UI suite and
  the ItemsPanelTests re-run.

## Gotchas

- `.manifest.state.json` was already modified by the supervisor when the round started; it is not this step's edit.
- The headless session start-up race above (`EnsureIsolatedApplication`) is a one-off seen only in the whole-suite run;
  the earlier 0262 runs and the re-run did not show it.
