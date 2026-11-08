# Step 0173 — impl log

## Progress

- [x] Read:
  - the protocol (`arc-runner.system-md`, `implement_worker.system-md`) and the project prompt;
  - the slice, its `.gates` and manual 013;
  - `ConstructorUiIdOfDomain.fs`, `Program.fs`'s `LauncherWindow` and `KitOpeningTests.fs`;
  - `Domain/Kit.fs` (`KitLockHolder.describe`, `KitError`) and `Domain/KitLibrary.fs` (`KitLibraryError.describe`).
- [x] Searched `Berreman/` for both literals. Each appeared once, in `LauncherWindow.showKitError`
  (`Program.fs`). Nothing else renders either text: `TestLauncherWindow`'s "The diagnostic could not be opened" and the
  launcher's "The library could not be opened" are different texts and stay as they are.
- [x] Wrote the fact first: `KitOpeningTests.the open captions name the error and the lock holder`. It is red because
  neither member existed, which grep confirmed.
- [x] Added `KitLibraryError.openFailedCaption` and `KitLockHolder.openElsewhereCaption` in `ConstructorUiIdOfDomain.fs`,
  directly after `KitError.caption`, each with a one-line doc citing spec 0050 S5.2.
- [x] `LauncherWindow.showKitError` now picks one of the two captions and renders its `.value` as both the TextBlock's
  `Text` and its automation name. Both literals are deleted from `Program.fs`.
- [x] Checked line endings. All three files are `i/lf w/lf` with no CR bytes, and `git diff --numstat` is the same with
  and without `--ignore-cr-at-eol`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: two extension members. +10.
- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs`: `showKitError` renders the captions. +5 / −5.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitOpeningTests.fs`: `open OpticalConstructor.Ui.ConstructorUiIdOfDomain`
  and one fact. +19.

## Decisions

- **The extended types are fully qualified.** The two extensions are written as
  `type OpticalConstructor.Domain.KitLibrary.KitLibraryError with` and
  `type OpticalConstructor.Domain.Kit.KitLockHolder with`, the same way the neighbouring `KitError` and `KitRelativePath`
  extensions are. The file's `open` list is unchanged.
- **The local holds a `ControlCaption`.** In `showKitError`, the local is now `caption : ControlCaption` (it was
  `message : string`). `.value` is read only at the two Avalonia setters, which are the interop line.
- **The test opens the extension module directly.** The test file opens `OpticalConstructor.Ui.ConstructorUiIdOfDomain`
  rather than the whole `OpticalConstructor.Ui` namespace, as `LibraryControlsTests` does. Opening the namespace would
  also auto-open `Catalogue` and `ScreenPointAvalonia`.
- **The holder fixture is declared locally.** Step 129's `exampleLockHolder` is `private` to `KitTests.fs`, and this
  step's authorized edits do not include that file. The fact therefore builds its own holder from the values the slice
  names: `UserName "lab"`, `MachineName "BENCH-1"`, `ProcessId 4242`, `2026-11-03 12:00:00 UTC`.
- **The two assertions follow the slice's wording.** The open-failure caption is compared with
  `$"The kit could not be opened: {error.describe}"`. The lock-holder caption is compared with the full literal, which
  ends with the period.

## Testing state

No gates were run. Under invariant 6 this worker does not run checks; the arc-runner's gate engine runs the step's
roster after the worker exits:
- build;
- elevation;
- constructor-unit-tests, all discovered;
- ui-smoke, all discovered;
- ui-tests, `OpticalConstructor.Ui.Tests.LauncherTests`;
- the fourteen s20 invariants.

Instead, the code was checked by reading it:
- `KitLibraryError.describe : string` and `KitLockHolder.describe : string` exist.
- `ControlCaption.value` is a string.
- Both `match` arms in `showKitError` return `ControlCaption`.
- `Program.fs` opens `OpticalConstructor.Ui`, so the `[<AutoOpen>]` extensions are in scope there.
- `OpticalConstructor.Tests` references `OpticalConstructor.Ui`, so it sees `ControlCaption` through that reference;
  `ChartSettingsTests` already uses it.
- `openFailedCaption` and `openElsewhereCaption` occur nowhere else (ELEV019). The only other `KitLockHolder`
  extension, `KitFolderStore.lockFileText`, has a different name.
- No s20 invariant pattern occurs in the diff.

`LauncherTests`' facts of steps 154 to 156 are unchanged. They assert the rendered text, which stays the same:
- `The kit is open elsewhere: {holder.describe}.`;
- `StartsWith "The kit could not be opened: "`.

The constructor count should rise by 1 over step 0172's.

## Artifacts

None. The slice produced no run captures.

## Gotchas

- **Out-of-scope texts that look similar.** `Program.fs` still holds `The library could not be opened: …`, the
  library-open failure (step 2.4), and `TestLauncherWindow.fs` holds `The diagnostic could not be opened: …`. Neither
  is one of the two kit-open texts, so the slice's "nothing else changes" leaves them as they are.
- **The start screen has no copy of either text yet.** Only the launcher rendered them. The doc comments name the start
  screen because the slice makes these members the shared home for it.
- **`specs/0050/.manifest.state.json` belongs to the supervisor.** It was already modified when this worker started,
  and it was not touched.
