# Step 0231 — impl log

## Progress

- Read the system prompts, the project prompt, AGENTS.md, the slice, its gates snapshot, manual 015 (the ui-smoke
  600 s deadline) and step 0230's state of the world. No operator note is in flight.
- Read the seams this step projects: `KitArchiveJob.fs` (`kitArchiveProgressView`, `KitArchiveProgressIds`,
  `KitArchiveJobState`, `KitZipPicker.pickZipToOpenThen`, step 200), `KitStartView.fs` (step 230's `zipJob`,
  `OpenKitZip`, `CancelOpenKitZip`), and the Send kit row this mirrors (`KitWindowView.kitSendRows`, its
  `KitWindowTests` facts).
- **UiIds.** Added `openKitZipButton`, `openKitZipProgressBar`, `openKitZipProgressText` and
  `openKitZipCancelButton` to the start-screen group, after `openKitButton`, each with a one-line doc.
- **KitStartView.**
  - `kitStartActionsRow` gains `Open kit from zip…` after `Open kit…`, only while `m.zipJob.job.isRunning` does not
    hold. Its press is `zipPicker.pickZipToOpenThen (fun zip -> picker.pickThen (fun target -> dispatch (OpenKitZip
    opening)))`, the `KitZipOpening` annotated (step 230's gotcha: `KitUnzipJob` shares the labels).
  - New private `kitStartZipRows`: `kitArchiveProgressView m.zipJob { bar; text; cancel } (fun () -> dispatch
    CancelOpenKitZip)` while the extraction runs, `[]` otherwise. `kitStartView` puts it directly after the first row.
  - Docs updated on both functions and on `kitStartView`, citing spec 0050 S6. Nothing else on the screen changed.
- **KitStartWindowTests.**
  - `withKitStartScreen`'s body moved into `withKitStartProgram update model0 check`; `withKitStartScreen` is that
    over `updateKitStart`. One body, so the smoke fact can record the messages the screen sends.
  - Helpers: `zipProgressIds` (the row's three ids) and a private `OpenKitZipDialog` union (`ZipDialogAsked` /
    `FolderDialogAsked`) recording which dialog was asked, in order.
  - Fact `the Open kit from zip UiIds are the stable intent-named ids` (no trait): the four texts.
  - Fact `a running extraction shows its progress bar, its count and Cancel, and hides Open kit from zip` (no trait):
    `zipJob` started with a test-local handle and progressed by 3 of 4 files. The text starts `3 / 4`, the bar is
    0.75 and not indeterminate, Cancel reads `Cancel`, and the zip button is absent. The bar lies under the first row
    and above the filter box, and the history list is unchanged. Over the idle model, the button reads
    `Open kit from zip…` and none of the three progress ids exists.
  - Fact `Open kit from zip asks for a zip, then a folder, and starts one extraction of that zip into that folder`
    (`ui-smoke`): a recording zip picker answering `<root>/exports/Bench A.zip`, a recording folder picker answering
    the fresh `<root>/received`, a deferred host keeping each job and answering a cancel-recording handle, and an
    update that records each message.
    - Pressing the button asks the zip dialog, then the folder dialog; one job is kept; the one message sent is
      `OpenKitZip` of that zip and that folder; the progress text shows `Starting…`; the button is hidden; nothing
      is cancelled.
    - Pressing Cancel calls the handle's cancel once.
    - Then (the slice's "a cancel of either sends nothing"): with the zip dialog cancelled, the folder dialog is not
      asked and nothing is sent; with the folder dialog cancelled, only the zip dialog was asked and nothing is sent.
    - No kit is shown and the target folder never exists.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitStartView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitStartWindowTests.fs`

## Testing state

Development runs over the final source. The gate engine gives the verdict (Invariant 6).

- **build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exit 0, 0 errors.
  - The first build printed 2 SYSLIB0051 warnings from the vendored C# `MathNetNumerics/Numerics/Exceptions.cs`. That
    project prints them whenever it recompiles, and step 0230's first build printed the same two
    (`0230-dev-build.log`). None comes from our F# code.
  - The final build over the restored source: **0 warnings, 0 errors** (`0231-dev-build-final.log`).
- **ui-tests** over `KitStartWindowTests`: **8** passed, 0 failed, 0 skipped. That is 6 + the 2 new facts, each
  `Passed` in the TRX. Re-run on the final binaries: 8 passed.
- **ui-smoke** (all discovered): **240** passed, 0 failed, 0 skipped. That is 239 + the new fact, `Passed` in the TRX.
  - It took 213 s, run alongside elevation, against the 600 s `attempt_timeout_seconds`; no deadline was exceeded.
  - The class's 4 smoke facts were re-run on the final binaries: 4 passed.
- **constructor-unit-tests** (all discovered): **1353** passed, 0 failed, 0 skipped. Unchanged: this step adds none.
- **elevation** (ELEV004 excluded, all 12 projects): `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 201s`, with 0
  known findings and 1317 suppressed findings.
- **The fourteen `invariant-s22-*` greps:** each finds nothing (git grep exit 1).
- **LF:** the three edited files hold no CR byte, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- **The facts bite.** Four mutants of `KitStartView.fs` were each rebuilt and run over the whole class (12 facts):
  - `guard-dropped` (the zip button shown while running): the running-extraction fact and the smoke fact failed.
  - `dialogs-swapped` (folder asked before zip): the smoke fact failed.
  - `no-progress-row` (the row never drawn): the running-extraction fact and the smoke fact failed.
  - `cancel-inert` (Cancel sends nothing): the smoke fact failed.
  - The source was then restored byte for byte (sha256 `8de4728e…` both) and the solution rebuilt.

## Artifacts

- `specs/0050/.artifacts/0231-dev-build.log`, `0231-dev-build-final.log`
- `specs/0050/.artifacts/0231-dev-ui-tests.log`, `0231-dev-ui-tests-final.log`, `ui-tests-0231-dev.trx`
- `specs/0050/.artifacts/0231-dev-ui-smoke.log`, `0231-dev-ui-smoke-class-final.log`, `ui-smoke-0231-dev.trx`
- `specs/0050/.artifacts/0231-dev-constructor-unit-tests.log`, `constructor-unit-tests-0231-dev.trx`
- `specs/0050/.artifacts/0231-dev-elevation.log`
- `specs/0050/.artifacts/0231-mutants/`: `mutate.py`, the original source and its sha256, and each mutant's build and
  test logs

## Gotchas

- **The zip and folder pickers chain synchronously in tests.** `startPick` runs `Async.StartImmediate`, so an
  `async { return Some … }` double answers inside the press. The smoke fact sees `OpenKitZip` before `pressAt`
  returns, exactly as the New kit smoke fact does.
- **The test-local handle duplicates no helper on purpose.** `KitWindowTests` keeps a private `recordingHandle` and
  `zippedFiles`. Sharing them through `TestAppScope` would edit files outside this step's authorized effects, so the
  facts build the handle and the progress inline, as record literals, as `ScrollExtentTests` does.
- **`withKitStartScreen` is now `withKitStartProgram updateKitStart`.** Every existing fact keeps calling
  `withKitStartScreen` unchanged; only the smoke fact passes a recording update.
- **The real window still answers no zip.** `KitStartWindow` composes `KitZipPicker.none` (step 230). Pressing the
  button there asks nothing yet; step 232 gives it the real dialog.
- **The slice's "a cancel of either sends nothing" is covered inside the smoke fact.** It is two more screens in the
  same fact, not a new test identity.
- **`specs/0050/.manifest.state.json` is dirty from the supervisor**, not from this step. It was left untouched.
