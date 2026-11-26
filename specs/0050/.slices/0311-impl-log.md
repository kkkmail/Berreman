# Step 0311 — impl log

## Progress

- Read the task, the system prompts, the project prompt, AGENTS.md, the slice and steps 308–310, 312.
- Read `ImportWizardView.fs`, `ImportDialogWindow.fs`, `KitWindowShell` (`TableAndElementRotationView.fs`),
  `ScrollExtentTests.fs`, `ImportWizardTests.fs` and the `ImportDialogTests` window acceptance fact.

- Wrote the two facts first, red by construction (`ImportWizardWindow` and `withImportWizard` did not exist).
- Added `ImportWizardWindow.fs` (the window and `ImportWizardOpening.KitWindowShell.withImportWizard`) and its
  `<Compile>` line directly after `KitWindowView.fs`.
- Development build: exit 0, only the two pre-existing vendored MathNet SYSLIB0051 warnings (each printed twice).
- Selected ui-tests (`ImportWizardTests`, `ScrollExtentTests`, ui-smoke excluded): 25 of 25 passed; the new law ran
  in 0.66 s.
- Mutation check of the new law: with the preview's `ScrollViewer` removed from `ImportWizardView.fs`, the fact failed
  (`ImportWizard at 800×600: 311 unreachable`). The view was restored byte for byte (`git diff --exit-code` clean)
  and the solution rebuilt before every gate run below.
- First gate pass: ui-tests (selected) 25/25 in 14 s; constructor-unit-tests 1525/1525 in 70 s; elevation
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` in 204 s (suppressed 1428, +2 over step 310: the window's two
  `ConstructorUiId.importWizardWindow.value` reads, ELEV004, excluded, as every window sets its id); the sixteen s30
  invariants hold (and the untracked new file matches none of their patterns).
- **ui-smoke failed 251/252**: the new acceptance fact threw `ObjectDisposedException: 'ImportWizardWindow'` in
  `pressControl`'s `MouseUp`. Attach acts on pointer-press (`MaterialsWindowView.actionButton`), so the press closes
  the wizard and the release lands on a disposed window. `ImportDialogTests.pressControl` releases only
  `if window.IsVisible`; the slice says to press "as `ImportDialogTests` presses a box", so the private
  `ImportWizardTests.pressControl` (step 309) got the same guard. No existing fact's window closes on a press, so
  their behaviour is unchanged. Rebuilt and re-ran ui-smoke and ui-tests.
- Second pass over the final tree: build exit 0 (0 errors, 0 warnings incremental); ui-smoke 252/252 in 219 s;
  ui-tests (selected) 25/25 in 13 s; the sixteen invariants hold again. Elevation and constructor-unit-tests were not
  re-run: the only change since is inside the test project's private helper, which neither gate reads (the test
  projects are outside ELEV scope; `OpticalConstructor.Tests` does not compile `ImportWizardTests.fs`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardWindow.fs` (new): `ImportWizardWindow` (inherits
  `HostWindow`, title `Import data file`, `Name`/`AutomationId` `importWizardWindow`, 1000 × 700 then
  `WindowFit.keepOnScreen`, `Program.mkProgram` over `initImportWizard` / `updateImportWizard` / `importWizardView`)
  and `module ImportWizardOpening` with `KitWindowShell.withImportWizard owner dispatch`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile>` line after
  `KitWindowView.fs`, no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: `wideLabFileRequest` (twelve comma
  columns, forty rows, read in memory) and `the import wizard leaves everything reachable`; opens `ImportProfiles` and
  `ImportWizardView`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs`: the `ui-smoke` fact
  `acceptance: the import wizard window maps a picked file and its Attach reaches the kit window as messages`; opens
  `ImportWizardOpening`.

## Testing state

Development runs of each gate's own command; the arc-runner's gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`; full build warnings only the two pre-existing vendored MathNet SYSLIB0051 | 79 s full, 17 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0 | 204 s | 600 s |
| constructor-unit-tests | Passed!, 1525 of 1525 | 70 s | 180 s |
| ui-smoke | Passed!, 252 of 252 (251 + 1) | 219 s | 600 s |
| ui-tests (selected: `ImportWizardTests`, `ScrollExtentTests`) | Passed!, 25 of 25 (24 + 1) | 13 s | 360 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

No run exceeded its effective deadline. `commit_ready: true`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0311-dev-build.log` (the last, incremental build), `0311-dev-elevation.log`,
  `0311-dev-constructor-unit-tests.log`, `0311-dev-ui-smoke.log` (the passing re-run), `0311-dev-ui-tests.log`;
- `0311-dev-mutation-no-scroll.log`: the new law red with the preview's ScrollViewer removed.

## Gotchas

- **The running app still opens nothing on Attach data file… until step 312.** Step 310's state-of-the-world says
  step 311 sets the App's shell, but this slice's effects are the Ui and Ui.Tests projects only; step 312 adds
  `.withImportWizard window dispatch` to `ConstructorScene.mount` in `OpticalConstructor.App/Program.fs`.
- **Attach acts on pointer-press**, so a press that closes the window must not be followed by a release; the private
  `ImportWizardTests.pressControl` now releases only while the window is visible, as `ImportDialogTests` does.
- **"A test-local lab file" is read in memory**: `wideLabFileRequest` answers the twelve-column, forty-row bytes for a
  test-local `FileName`, like the class fixtures of step 308. Nothing is written to disk. The preview shows the first
  twenty rows (`previewWith`), which already makes the grid taller than both small screens.
- **The acceptance fact uses `ImportProfileName.phoneRoiCsv`** as the slice names it; it equals the class's private
  `phoneRoiCsv`. It finds the wizard through `owner.OwnedWindows`, which proves `Show owner`, and the window's own
  context leaves the class context's recorder empty.
- ui-smoke per-test durations include queueing on the one headless session (every fact starts at the same instant),
  so the new fact's 18.6 s is not its own cost.
- `specs/0050/.manifest.state.json` was dirty from the supervisor before the round began; untouched.
