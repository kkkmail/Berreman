# Step 0262 — state of the world

## Where we are

Spec 0050 section S8.2 (setups as slots), step 0262 (IMPLEMENT). Steps 0256–0261 made `Setups.Setup` the kit's
setup file, gave the constructor `CreateSetup` / `ActivateSetup` and made an experiment reference a setup revision,
a commit recording the table as the active setup. This step adds `SaveSetup`: the table is written over the active
setup, in place until an experiment references its revision and as the next revision after, and the setup row shows
which setup the table draws.

## What's working

- Added `SaveSetup`: writes the table over the active setup by the one version rule, in place while unreferenced and
  minting the next revision once an experiment references it.
- Refused `SaveSetup` with no active setup, over an unchanged table (nothing rewritten) and under a read-only kit, each
  told in the kit status.
- Showed the active setup (`Active setup: <name>, revision <n>`) first in the setup row, and added `Save setup`,
  withheld with no active setup or a read-only kit.
- Shared the recorded slots, the version-rule revision and the setup bookkeeping between `ExpCommit`, `CreateSetup`
  and `SaveSetup` as model members.
- Added `ConstructorUiId.activeSetupText` and `ConstructorUiId.saveSetupButton`.

## Tests

No gate verdict is claimed (Invariant 6); the supervisor runs the frozen roster. Development runs over the final tree
used each gate's own command, with logs in `specs/0050/.artifacts/0262-dev-*.log`.

- **build:** exit 0, 0 warnings, 0 errors, 26 s incremental (deadline 600 s).
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 201 s (deadline 600 s).
- **constructor-unit-tests:** `Passed!`, 1423 succeeded, 0 failed, 0 skipped, 72 s (deadline 180 s). The count rises
  1421 → 1423 as the acceptance states. `SaveSetup writes the active setup in place until an experiment references
  its revision, then mints the next`, `SaveSetup with no active setup, or over an unchanged table, writes nothing and
  says why` and `a kit opened read-only refuses every write message and writes nothing` are `Passed` in the TRX.
- **ui-smoke:** `Passed!`, 245 succeeded, 0 failed, 0 skipped, 203 s (deadline 600 s). `the setup row renders the
  active setup, its name field, Create setup, Save setup and the saved setups` is `Passed`; the old identity is gone.
- **ui-tests (the two selected classes):** `Passed!`, 19 succeeded, 0 failed, 0 skipped, 28 s (deadline 360 s). The
  classes held 18 non-smoke facts before this step and 19 after (+1). `Save setup over a table an experiment uses
  keeps the earlier revision in the setup's kit file` and `a read-only window withholds every write control` are
  `Passed`.
- **The fifteen s26 invariants** each find nothing.
- **Whole non-smoke UI suite** (beyond the gate, to catch fallout from the setup row's new controls): 677 of 678
  passed, 218 s. The one failure, `ItemsPanelTests.the add form lists the library models by kind and offers Add and
  Cancel on one row, Cancel to the right`, threw inside Avalonia's headless session start-up
  (`HeadlessUnitTestSession.EnsureIsolatedApplication`, "a different thread owns it") before its body ran; the class
  re-run alone passed 5 of 5. It is outside this step's selection and code.
- No run exceeded its deadline.

```yaml
gates:
  constructor_unit_tests: 1423
  ui_smoke_tests: 245
  ui_tests: 19
```

## Architecture

- **One version rule for every write of the table.** `ExpCommit` and `SaveSetup` both go through
  `model.tableWrittenOver active` (`Setup.revisedWith` with `Setup.pinFor`), so a commit and a save over the same
  table and experiments give the same setup. `revisedWith` answers the active setup itself when the slots are
  unchanged, which is how `SaveSetup` knows to write nothing.
- **One bookkeeping path for a recorded setup.** `model.withSetupRecorded` puts the setup in place of its name's in
  `kitSetups` (ordinal name order) and makes it active; `ExpCommit`, `CreateSetup` and `SaveSetup` use it.
- **Save setup's availability is the model's.** Offered only while a setup is active and the kit takes writes
  (`kitWrites`); the update arm is still guarded by step 185's `guardKitWrite`, so a message sent past the view is
  refused too.

## Deferred

Nothing in this step's scope.

## Gotchas

- **The active-setup line is outside the horizontal setup row,** first after the `Setup:` heading: AGENTS.md forbids a
  wrapping `TextBlock` inside a horizontal `StackPanel`.
- **"Finds no `saveSetupButton`"** (`KitWindowTests`) is read as "offers none": Save setup is withheld (disabled, never
  hidden, as `availableWorkbenchButton` does since step 185), so it joined `writeControlIds` and the read-only window
  finds it disabled while the read-write window finds it enabled.
- **Both read-only facts activate a recorded setup first.** `Stored bench` is created through the model while the
  kit is read-write and activated in the read-only window (`ActivateSetup` writes nothing), so Save setup is refused
  for the kit's access and not for a missing setup. In `KitWindowModelTests` the table then differs from it, so an
  unguarded `SaveSetup` would write; `KitWindowTests.assertWriteControls` activates it before preparing the draft.
- **A headless start-up race was seen once** in the whole non-smoke UI run (`EnsureIsolatedApplication` in
  `ItemsPanelTests`); the class passed alone. If a gate run shows it, it is the Avalonia session start, not a test.
- **The live-window ui-test turns the polarizer through the dispatch the view receives** (the same `RotSetAxis` the
  Rotate bay sends); Create setup, the Experiments bay's Add and Save setup are pressed in the window.
  `SetupSaveLoadTests.clickSetupControl` was split into `pressSetupControl`, because the Add box carries no automation
  name.

## Changelog

- 2026-11-10: Step 0262 added `SaveSetup` and the setup row's active-setup line and Save setup button, sharing the
  version rule and setup bookkeeping with the commit. Constructor 1423 (+2), smoke 245, selected UI 19 (+1).
