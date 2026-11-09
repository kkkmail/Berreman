# Step 0191 — impl log

## Progress

- [x] Read the protocol (`arc-runner.system-md`, `wire_ui_worker.system-md`), the project prompt, the slice, the
  gate snapshot, manual 013, step 0190's slice, log and state of the world, step 0180's slice, and step 0189's log
  (headless activation). Read the code the slice names:
  - `Program.fs` (`KitConstructorWindows`, `ConstructorScene.mount`);
  - `WindowLauncher.fs` and `KitOpening.fs` (`windowKey`, `activateKitWindow`);
  - `KitStartWindow.fs` and `KitStartView.fs`;
  - `LauncherTests.fs`, `WireUiFinalCompositionTests.fs` and `TestAppScope.fs`.
- [x] Wired `KitConstructorWindows.showConstructorIn` (`Program.fs`) through `WindowLauncher.create`, under
  `opened.folder.windowKey`:
  - the factory is step 154's mode match: `MainConstructorWindow(context, opened)` or
    `InverseConstructorWindow(context, opened)`;
  - it opens with `context.settings.selectWindowModality` and `BrowseOpen`;
  - `Ok (CreatedWindow _)` is the open;
  - `Ok (ActivatedWindow _)` / `Ok (RetargetedWindow _)` close the opened kit again (`opened.library.kit.close ()`);
  - `Error (WindowFactoryFailedErr (_, reason))` closes the opened kit and logs the reason with `Logger.logError`;
  - a failed close is logged, as `openKitAt` logs one;
  - the doc is three lines citing spec 0050 S5.3.
- [x] Added the two declared `ui-smoke` facts to `LauncherTests`, plus two private helpers:
  - `withWindowClosingLauncher` runs the real launcher over a picker answering a `KitFolder ref`. It closes every
    window the fact opened, observed through `Window.WindowOpenedEvent`, in `finally`;
  - `mainWindowThrough` returns the one `MainConstructorWindow` that a press on Main's start screen opens.
- [x] Built. Ran `LauncherTests`: 25 passed.
- [x] Red check. I temporarily put back the old unregistered `Show ()` in `showConstructorIn` and rebuilt. Both new
  facts failed for the missing behaviour, and the other 23 passed:
  - the acceptance fact failed with "bringing the kit's window forward must close the start screen";
  - the reopen fact failed with `Expected: KitWindowActivated / Actual: NoKitWindowOpen`.

  I then restored the wired file and rebuilt the solution, and all 25 passed again.
- [x] Ran the whole ui-smoke suite, the whole non-smoke UI suite, constructor-unit-tests, elevation from the arc
  origin, and the fourteen s21 invariant greps. All green (see `Testing state`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`: `KitConstructorWindows.showConstructorIn` and
  its doc.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/LauncherTests.fs`:
  - `open OpticalConstructor.Domain.KitLibrary` (for `KitLibraryKitErr`);
  - the helpers `withWindowClosingLauncher` and `mainWindowThrough`;
  - the facts `acceptance: choosing a kit whose window is open activates that window and opens no second one` and
    `a kit can be opened again once its window is closed`, both trait `ui-smoke`.

Both files are in the step's authorized effects. `WireUiFinalCompositionTests.fs`, also authorized, needed no
change:

- its root factory already refuses `KitWindowKey` (step 190);
- its constructor windows are built directly, never through `showConstructorIn`, so they register nothing;
- its `KitStartWindow`s take a `showConstructor` that ignores its kit.

## Decisions

- **The registration sits at the kit hand-off only.** `showConstructorIn` is the one place the app builds a
  constructor window over a chosen kit. Tests and other composition sites that build `MainConstructorWindow` /
  `InverseConstructorWindow` directly stay unregistered, so their windows never collide on a key. Every fact that
  goes through the launcher closes its windows, which forgets the key.
- **`WindowNotRegisteredErr` gets its own arm.** `openOrActivate` of the real launcher never answers it, because
  it only comes from `forgetWindow`. The match stays exhaustive without a wildcard. The arm still closes the opened
  kit and logs, naming the folder, because that case carries no reason.
- **The modality is the app's** (`context.settings.selectWindowModality`), as the slice says. It is never
  consulted under `BrowseOpen`.
- **"Written with backslashes and in upper case."** The variant is
  `folder.value.value.Replace('/', '\\').ToUpperInvariant()`. On Windows the test folder already uses backslashes,
  so the spellings differ by case; `Assert.NotEqual` proves they differ.
- **"Activates that window"** is observed as a raise of the Main window's `Activated` during the press. In headless
  Avalonia, `Activate ()` raises `Activated` even on a window that was never deactivated (step 0189's log), so the
  count rises only through the registry's activation.
- **"The lock still names this process"** is checked two ways:
  - the lock file's bytes are unchanged since the Main window opened;
  - a read-write `openKitAt` of the folder answers `KitLibraryKitErr (KitLockedErr holder)`, with
    `holder.owner.processId = ProcessId.getCurrentProcessId ()`.

  This is the same probe `KitOpeningTests` uses. It writes nothing to a locked kit.
- **The reopen fact also asks the registry directly.** After the first window closes,
  `folder.activateKitWindow ()` is `NoKitWindowOpen`, so the key was forgotten. After the reopen it is
  `KitWindowActivated`, so the new window is registered. That second call only brings the open window forward.
- **The windows are closed in `finally`, including on a failed assertion.** Each fact also closes its constructor
  window explicitly and asserts that `.oc/lock` is gone. This keeps the "released" check from being vacuous. A
  second `Close ()` in the helper's `finally` is harmless, as `withSnapshotLauncher` already relies on.

## Testing state

Local runs, kept as development evidence. The arc-runner's gate engine is the authority (Invariant 6). Logs are in
`specs/0050/.artifacts/`.

- `dotnet build Berreman.slnx -c Release`: 0 errors. The only warnings are the two existing `SYSLIB0051` warnings
  from the vendored `MathNetNumerics/Numerics/Exceptions.cs` (`0191-build.log`). The rebuild after the red check
  had 0 errors and 0 warnings (`0191-build-final.log`).
- `LauncherTests` (whole class, both traits): 25 passed, 0 failed (`0191-launchertests.log`). Over the final
  build: 25 passed (`0191-launchertests-final.log`).
- Red check over the unregistered `Show ()`: 2 failed (the two new facts), 23 passed (`0191-red-launchertests.log`).
- ui-smoke (`--filter-trait "Category=ui-smoke"`): 232 passed, 0 failed, 0 skipped (`0191-ui-smoke.log`). That is
  checkpoint 189's 230 plus the two new facts.
- The whole non-smoke UI suite, which includes the selected `LauncherTests` structure facts: 649 passed, 0 failed,
  0 skipped (`0191-ui-tests-whole.log`). This is unchanged.
- constructor-unit-tests: 1209 passed, 0 failed, 0 skipped (`0191-constructor-unit-tests.log`). This is unchanged:
  the step adds no constructor test.
- Elevation (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, with `ASG_ARC_BASE_COMMIT` set to the arc origin
  `f395597c…`): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 192 s (`0191-elevation.log`). The App project
  has 0 findings, and its suppressed count is still 12, as at steps 0189 and 0190.
- The fourteen s21 invariants, run as their `git grep` commands: none finds anything.
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol` for both source files, so there is no CRLF churn.

## Artifacts

- `specs/0050/.artifacts/0191-build.log`
- `specs/0050/.artifacts/0191-launchertests.log`
- `specs/0050/.artifacts/0191-red-build.log`
- `specs/0050/.artifacts/0191-red-launchertests.log`
- `specs/0050/.artifacts/0191-build-final.log`
- `specs/0050/.artifacts/0191-launchertests-final.log`
- `specs/0050/.artifacts/0191-ui-smoke.log`
- `specs/0050/.artifacts/0191-ui-tests-whole.log`
- `specs/0050/.artifacts/0191-constructor-unit-tests.log`
- `specs/0050/.artifacts/0191-elevation.log`
