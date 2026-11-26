# Step 0179 — impl log

## Progress

- [x] `kitStartWindow` id in `UiIds.fs`, in the start-screen group, with a one-line doc.
- [x] `KitStartWindow.fs`, and its `<Compile>` line directly after `AppContext.fs` (no comment).
- [x] `ScrollExtentTests`: `the kit start screen leaves everything reachable`.
- [x] `KitStartWindowTests`: `the start screen's title names its mode and it carries its stable id` (no trait) and
      `acceptance: New kit opens the new kit in the constructor of the screen's mode, bumps the history and closes the
      screen` (ui-smoke).
- [x] Build, elevation, the new classes, ui-smoke, constructor-unit-tests and the s20 invariant greps, run locally.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: adds `ConstructorUiId.kitStartWindow`
  (`"KitStartWindow"`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartWindow.fs` (new): `type KitStartWindow(context : AppContext,
  mode : ConstructorMode, pickerFor : TopLevel -> KitFolderPicker, showConstructor : ConstructorMode -> OpenedKit -> unit)`,
  a `HostWindow`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile Include="KitStartWindow.fs" />`
  after `AppContext.fs`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: opens `Domain.Kit`, `Domain.KitFolders`
  and `Domain.KitHistory`; adds `fortyKitHistory` and the new fact.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`: adds `windowScope`,
  `withKitStartWindows` and the two new facts. No existing fact or assertion changed.

## Decisions

- **The window.** It is titled `Open a kit — Main` / `Open a kit — Inverse` by an inline match on `mode`.
  `ConstructorMode` has no render member, and `TableAndElementRotationView.fs` is outside this step's edits. A private
  one-parameter helper would be ELEV013P. `Name` and `AutomationId` are `kitStartWindow`. The accessible name
  (`AutomationProperties.Name`) is the title, as `ImportDialogWindow` does, per AGENTS.md's "AutomationId and Name on
  every test-relevant control". It asks for 720 × 560, then calls `WindowFit.keepOnScreen this`.
- **The context and the hand-off.** The context is composed exactly as the brief lists it:
  - the history, `settings.kitHistoryLimit`, `ClockProvider.system`, `KitAuthor System.Environment.UserName`,
    `catalogueImports` and `pickerFor this`;
  - `showKit` = `opened.resolveAndShow context.catalogueImports (fun shown -> showConstructor mode shown; this.Close ())
    (fun error -> dispatch (KitOpenFailed error))`.

  The dispatch is captured from the init `Cmd.ofEffect`, as `LibraryWindow` does. No `Closed` hook is added, so closing
  the screen opens and closes nothing else.
- **The scroll fixture.** It holds forty in-memory entries over `C:/kits/bench-NN`. Index `n` in 1..40 is missing when
  `n % 5 = 0` and starred when `n % 7 = 0`. The app scope is built once, outside the headless session, and the window is
  rebuilt per screen size by `assertReachable`. The picker is `kitPickerOf` a fresh, uncreated folder, and
  `showConstructor` ignores its arguments.
- **The window-test fixture.** `withKitStartWindows` hands a fact a fresh-folder generator (`TestAppScope.freshKitFolder`)
  and the record of `showConstructor`. At the end it closes every recorded kit, deletes every fresh folder on disk, then
  fails on a refused close, mirroring step 178's `withKitStartRoot`. Each screen gets its own `windowScope ()`, a fresh
  app scope with a fresh `KitHistoryProxy.createInMemory []`. So "the history's one entry" is that screen's folder.
- **The acceptance fact** runs both modes in one fact, as the brief describes it. It checks:
  - the recorded mode is the screen's;
  - the kit is read-write and named for the folder's leaf (`namedKitFile`), and `.kit-json` exists;
  - `Closed` fired once and the window is no longer visible;
  - the history's entries are exactly `[ folder ]`.
- **The literal check.** The id's literal `"KitStartWindow"` is asserted inside the new title fact. The existing UiIds
  fact is left unchanged, because the step's test effects create two facts and edit none.

## Testing state

Local runs, kept as development evidence. The gate engine is the authority.

- `dotnet build Berreman.slnx -c Release` built with 0 warnings and 0 errors, recompiling Controls, Ui, App and the test
  projects (`.artifacts/0179-build.log`).
  - The first attempt failed with FS0039: `open System` let `System.AppContext` shadow the Ui `AppContext`. The fix
    removed the open and qualified `System.Environment.UserName`.
  - The first build's two `SYSLIB0051` warnings come from the vendored `MathNetNumerics/Numerics/Exceptions.cs`. They are
    already in the step-0001 logs, so they are not this step's.
- Elevation (`Run-ElevationGate.ps1 -ExcludeRules ELEV004`, no base, so all 12 projects): `ELEV-GATE: OK - 0 new
  finding(s)`, known findings 0 (`.artifacts/0179-attempt-01-elevation-local.log`).
- ui-tests, the gate's selection (not-trait ui-smoke / kit-acceptance, `KitStartWindowTests` + `ScrollExtentTests`):
  16 of 16 passed. That is 14 before this step plus the 2 new facts (`.artifacts/0179-attempt-01-ui-tests-local.log`).
- ui-smoke (whole trait): 227 of 227 passed, step 178's 226 + 1 (`.artifacts/0179-attempt-01-ui-smoke-local.log`).
- constructor-unit-tests (whole): 1186 of 1186 passed, unchanged (`.artifacts/0179-attempt-01-constructor-local.log`).
- The three new facts were also run by name: 3 of 3 passed (`.artifacts/0179-attempt-01-new-facts-local.log`). Both
  classes whole: 21 of 21 (`.artifacts/0179-attempt-01-new-classes-local.log`).
- The fourteen s20 invariant greps find nothing. They were run as `git grep` over the tracked tree, plus a plain grep of
  the untracked `KitStartWindow.fs`.
- Line endings are LF in every touched file (`git diff --numstat` equals `--ignore-cr-at-eol`, and no file has a CR).

## Artifacts

- `specs/0050/.artifacts/0179-build.log`
- `specs/0050/.artifacts/0179-attempt-01-elevation-local.log`
- `specs/0050/.artifacts/0179-attempt-01-ui-tests-local.log`
- `specs/0050/.artifacts/0179-attempt-01-ui-smoke-local.log`
- `specs/0050/.artifacts/0179-attempt-01-constructor-local.log`
- `specs/0050/.artifacts/0179-attempt-01-new-classes-local.log`
- `specs/0050/.artifacts/0179-attempt-01-new-facts-local.log`

## Gotchas

- **`open System` in `namespace OpticalConstructor.Ui` shadows the Ui `AppContext`** with `System.AppContext` (FS0039 on
  every field). Qualify `System.Environment` instead of opening `System`.
- **The hand-off closes the window inside the Elmish update.** `kitStartOpened` calls `showKit` mid-update, and a kit
  with no snapshot row reaches `this.Close ()` at once. The loop then renders onto a closed host, as `LibraryWindow`'s
  `requestClose` already does. The button clicks on release, so the screen closes during `pressAt`'s `MouseUp`; the
  acceptance fact closes a still-visible window in its `finally`, so a failed hand-off cannot leak a window.
- `specs/0050/.manifest.state.json` was already modified when the round started. It is the supervisor's, and this step
  leaves it alone.
