# Step 0189 — impl log

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `wire_ui_worker.system-md`), the project prompt, the slice, the
  gate snapshot, manuals 013 and 014, and step 0188's state of the world and log. Read the code the slice names:
  `Program.fs` (`ConstructorScene.mount`), `LibraryWindow.fs:85` (the dispatch capture), `KitWindowView.fs`,
  `KitWindowShell` and the kit messages (`CheckKitChanges`, `ReloadKitChanges`, `CloseKit`) in
  `TableAndElementRotationView.fs`, `ExplorerProxy.system` (`KitOpening.fs`), `TestAppScope.fs` and
  `KitWindowTests.fs`.
- [x] Wired `ConstructorScene.mount` (`Program.fs`):
  - `Program.mkProgram` replaces `Program.mkSimple`;
  - init answers `seed ()` with `kitShell = { explorer = ExplorerProxy.system; requestClose = window.Close }`, plus a
    `Cmd.ofEffect` that stores the Elmish dispatch;
  - update is `updateTableAndElementRotation` with `Cmd.none`;
  - the view still sets `window.Title <- model.kitWindowTitle.value`, and now answers `KitWindowView.kitWindowView`
    instead of `mainView`;
  - `window.Activated.Add` dispatches `CheckKitChanges KitCheckOnActivation`;
  - `mount` has a two-line doc citing spec 0050 S5.3;
  - the two window docs say they run the kit window. Their constructors and the `Closed` release are unchanged.
- [x] `KitWindowTests`:
  - added the two declared facts, both `ui-smoke`;
  - added the helpers `outsideExperiment`, `pressAtCentre` and `pressRibbonTab`;
  - `pressBox` now calls `pressAtCentre`.
- [x] First local run of `KitWindowTests`. `Close kit …` passed. The acceptance fact failed at `Assert.Single`: in
  the scratch `initMain ()` scene, the unbound source varies nothing, so `ExpCommit` committed nothing. The fix
  adds a linear polarizer to the scratch scene and commits over it, as `ExperimentControlsTests` does. All 9
  `KitWindowTests` then passed.
- [x] Ran the whole ui-smoke suite, the whole non-smoke UI suite (the four selected classes too) and
  constructor-unit-tests: all green. No existing fact in `AppContextTests`, `LauncherTests`,
  `TableAndElementRotationTests` or the three `WireUi*` classes broke, so none of those files was edited.
- [x] Cleanup, no behaviour change: the stored dispatch is named `hostDispatch`, so the view keeps its `dispatch`
  parameter as before. The activation comment cites `.manual/001 §E`.
- [x] Rebuilt the solution over the final source and reran ui-smoke and the four selected ui-tests classes. Ran
  elevation from the arc origin and the fourteen s21 invariant greps. All green (see `Testing state`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`:
  - `ConstructorScene.mount` runs the kit window: `mkProgram`, the real shell, `kitWindowView`, and the activation
    check;
  - the doc of `mount` and of the two constructor windows.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`:
  - the two declared facts;
  - `outsideExperiment`, `pressAtCentre` and `pressRibbonTab`;
  - `pressBox` now goes through `pressAtCentre`.

Both files are in the step's authorized effects. The other five authorized test files were not touched.

## Decisions

- **The captured dispatch is a `ref` cell (`hostDispatch`).** `mount` is a module function, and an F# closure cannot
  capture a `let mutable` local. `LibraryWindow`'s `let mutable dispatch` is a class field, so it can.
  - `Program.run` runs the init Cmd synchronously, so the cell holds the real dispatch before `mount` returns.
  - The window is shown only after its constructor returns, so the first `Activated` always finds the real
    dispatch.
- **`Activated` is subscribed after `Program.run`**, beside the dispatch it needs, as `LibraryWindow` subscribes
  `Closed` after its run.
- **`requestClose = window.Close`** is the method group, as `LibraryWindow`'s `requestClose = this.Close` is.
  - `CloseKit` calls it inside the Elmish update.
  - The window's existing `Closed` handler (step 154) then releases the kit. Nothing else releases it.
- **The shell is set in init, over the seed.** `seed ()` stays the window's own (`model.loadKit opened`), so the
  two window types are unchanged. A test scene keeps `KitWindowShell.inert`.
- **The acceptance fact's `E2`.** It is committed in a scratch `initMain ()` model, then given `id = ExperimentId 2`,
  as the slice says.
  - The scratch scene's source is unbound and varies nothing, so the commit varies a linear polarizer added to
    the scene (`AddElement LinearPolarizer`). This follows `ExperimentControlsTests`' collection facts.
  - The scratch kit is closed afterwards.
- **The second window.** A plain `Window` is shown and `Activate ()`d. Then the constructor window is
  `Activate ()`d again. In headless Avalonia, `Activate ()` raises `Activated` even when the window was never
  deactivated, so the second check is the activation itself. No button is pressed before the offer appears.
- **`kitChangesText` "names" `E2.experiment-json`** is checked with `Assert.Contains`: only the experiment changed,
  and the offer's sentence wraps the names.
- **The Experiments tab carries its id as its `Name`, not as its AutomationId** (`Ribbon.tab`). So it is pressed
  through `pressRibbonTab`, which finds the `Border` by `Name`, as `TableAndElementRotationTests` and
  `LauncherTests.clickBorder` do. The row check reads `experimentEditButton E2`, the row id
  `LauncherTests`' Main-button fact uses.
- **A press that closes its window gets no release.** `Close kit` closes the window on the pointer press, and a
  headless `MouseUp` on a closed window has no platform impl. `pressAtCentre` skips the release when the window
  is no longer visible, as `LauncherTests.clickBorder` does. Every press that leaves the window open behaves as
  before. `pressBox`'s existing callers (the step-188 smoke fact) are unchanged in effect.
- **Neither new fact presses `kitShowInExplorerButton`.** A real window's explorer is the system's.
- **`Close kit …` counts `Closed` with its own subscription**, added before `Show ()`. It asserts exactly one raise,
  that the window is no longer visible, and that `.oc/lock` is gone. It also asserts the lock exists while the kit
  is open, so the "released" check is not vacuous.

## Testing state

Local runs, kept as development evidence. The arc-runner's gate engine is the authority (Invariant 6). Logs are
in `specs/0050/.artifacts/`.

- `dotnet build Berreman.slnx -c Release`: 0 errors. The only warnings are the two existing `SYSLIB0051` warnings
  from the vendored `MathNetNumerics/Numerics/Exceptions.cs` (`0189-build.log`).
- `KitWindowTests` (whole class): 9 passed, 0 failed (`0189-kitwindowtests.log`).
- ui-smoke (`--filter-trait "Category=ui-smoke"`): 230 passed, 0 failed, 0 skipped (`0189-ui-smoke.log`). That is
  checkpoint 187's 228 plus the two new facts.
- ui-tests over the four selected classes (`AppContextTests`, `KitWindowTests`, `LauncherTests`,
  `TableAndElementRotationTests`): 54 passed, 0 failed, 0 skipped (`0189-ui-tests.log`).
- The whole non-smoke UI suite: 649 passed, 0 failed, 0 skipped (`0189-ui-tests-whole.log`).
- constructor-unit-tests: 1207 passed, 0 failed, 0 skipped (`0189-constructor-unit-tests.log`). This is unchanged:
  the step adds no constructor test.
- Elevation (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, with `ASG_ARC_BASE_COMMIT` set to the arc origin
  `f395597c…`): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 187 s (`0189-elevation.log`). The App project
  had 0 findings.
- The fourteen s21 invariants, run as their `git grep` commands: none finds anything.
- After the `hostDispatch` cleanup, over the final source:
  - the rebuild had 0 errors and no owned-code warning (`0189-build-final.log`). The built App contains
    `hostDispatch`, so the tests ran the final source;
  - ui-smoke: 230 passed, 0 failed, 0 skipped (`0189-ui-smoke-final.log`);
  - the four selected ui-tests classes: 54 passed, 0 failed, 0 skipped (`0189-ui-tests-final.log`);
  - elevation from the arc origin: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 191 s
    (`0189-elevation-final.log`).

## Artifacts

- `specs/0050/.artifacts/0189-build.log`
- `specs/0050/.artifacts/0189-build-tests.log`
- `specs/0050/.artifacts/0189-kitwindowtests.log`
- `specs/0050/.artifacts/0189-ui-smoke.log`
- `specs/0050/.artifacts/0189-ui-tests.log`
- `specs/0050/.artifacts/0189-ui-tests-whole.log`
- `specs/0050/.artifacts/0189-constructor-unit-tests.log`
- `specs/0050/.artifacts/0189-elevation.log`
- `specs/0050/.artifacts/0189-build-final.log`
- `specs/0050/.artifacts/0189-ui-smoke-final.log`
- `specs/0050/.artifacts/0189-ui-tests-final.log`
- `specs/0050/.artifacts/0189-elevation-final.log`
