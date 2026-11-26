# Step 0200 — state of the world (IMPLEMENT: the archive job host and the zip dialogs)

## Where we are

Spec 0050, section S6 (send and receive kits).

- Steps 196–199 built the send rule, the `KitArchiveProxy` seam with its double and contract, `sendKit` /
  `receiveKit` / `KitZipPicker`, and the real System.IO.Compression proxy.
- This step adds the Ui layer that the two surfaces will compose:
  - a job host that runs a zip or unzip off the UI thread through `JobRunner`, with per-file progress and Cancel;
  - the surface's job state over `JobRunner`'s model;
  - the shared progress row;
  - the Avalonia Send kit… save dialog and Open kit from zip… open dialog that fill `KitZipPicker`.

## What's working

- Add `KitArchiveJobHost`: `posting` runs a job through `JobRunner.startBackground`, posting per-file progress and its end. A raise ends in `KitArchiveIoErr`.
- Add the other three hosts: `onUiThread` (the UI dispatcher), `immediate` (the caller's thread) and `inert`.
- Add `KitArchiveJobState` over `JobRunner.updateJob` (`idle`, `started`, `progressed`, `cancel`) and the shared `kitArchiveProgressView` row.
- Add `KitZipPicker.ofWindow` (save `Send kit`, open `Open kit from zip`, one `Zip archive` type) and `pickZipToWriteThen` / `pickZipToOpenThen`.
- Move the dialog-start body into `KitOpening.startPick`, which `KitFolderPicker.pickThen` now calls.
- Add `KitArchiveJobTests`, five facts (constructor-unit-tests 1248 → 1253).

## Tests

These are development runs, recorded in `0200-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors. The final incremental build had 0 warnings; the first full build had only the two
  vendored MathNet `SYSLIB0051` warnings already on the start tree.
- **constructor-unit-tests:** 1253 passed, 0 failed.
  - All five `KitArchiveJobTests` identities pass.
  - `pickThen hands on the folder the picker answers and nothing on a cancel` passes as written.
- **ui-smoke:** 236 passed.
- **ui-tests** (`KitStartWindowTests`, `KitWindowTests`): 12 passed.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 projects.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1253
  ui_smoke_tests: 236
```

The ui-tests count (12) is only the two selected classes, so it is not a suite baseline.

## Architecture

- **One host record, four constructors.**
  - `KitArchiveJobHost.startArchiveJob` takes the job as `KitArchiveObserver -> Result<…>`. A surface passes
    `sendKit` / `receiveKit` partially applied, and the host supplies the observer.
  - `posting` is the one real body. `onUiThread` is `posting` over `Dispatcher.UIThread.Post`, so Avalonia appears
    only at that line.
  - `immediate` shares `posting`'s observer and handle helpers.
- **The state follows the runner's MVU model.** `KitArchiveJobState.job` is `JobModel<KitArchiveFileCount>`, driven
  only by `updateJob`. The view binds `progress` / `progressText`. A surface returns to `idle` at the end and tells
  the outcome in its own status.
- **One body starts every dialog.** `KitOpening.startPick` is called by `KitFolderPicker.pickThen` and by both
  `KitZipPicker` `…Then` members. The dialogs themselves stay the IO edge: each catches the provider, answers `None`
  and creates no folder.

## Deferred

Nothing in this step's scope. Composing the host, the state, the progress row and the pickers into the surfaces
(Send kit…, Open kit from zip…) is a later step of the plan.

## Gotchas

- **The cancellation source is never disposed.** A Cancel may arrive after the job ended, and `Cancel` on a disposed
  source throws. A plain source holds no timer or wait handle.
- **`KitArchiveProgressIds` fields (`bar`, `text`, `cancel`) can capture record inference** downstream. Annotate other
  record literals with those labels.
- **`actionButton` keeps the press closure it saw at first render** (`OnChangeOf` the id). Pass a closure that
  dispatches a message.
- **`immediate` lets a raising job raise into its caller.** Only `posting` / `onUiThread` map a raise to
  `KitArchiveIoErr`.

## Changelog

- 2026-11-09 — step 0200:
  - `KitArchiveJob.fs`: the job host, the job state, the progress row and the zip dialogs;
  - `KitOpening.startPick`, which `pickThen` now calls;
  - `KitArchiveJobTests` (5 facts).
