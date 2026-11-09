# Step 0202 — state of the world (IMPLEMENT: Send kit… and its progress row in the kit bar)

## Where we are

Spec 0050, section S6 (send and receive kits).

- Steps 196–201 built the send rule, the archive seam and its real zip writer, `KitZipPicker`, the Ui job host and
  job state, the shared progress row, and the kit window's Send kit messages over `kitSend`.
- This step makes the send visible:
  - the kit bar's Kit menu gains `Send kit…`, which opens the save dialog with the kit's zip name and sends `SendKit`;
  - a running send shows its progress bar, its count and Cancel directly under the menu.
- Step 233 composes the real archive and the `onUiThread` host into the windows, so the App's windows still run the
  inert shell's send until then.

## What's working

- Add `sendKitButton`, `sendKitProgressBar`, `sendKitProgressText` and `sendKitCancelButton` to `ConstructorUiId`'s
  kit-bar group.
- Add `Send kit…` first in the Kit menu, for a kit with a folder and no running send. Its dialog suggests
  `<kit name>.zip`, and a cancelled dialog sends nothing.
- Show the running send's progress row (bar, `n / total` count, Cancel → `CancelSendKit`) under the Kit menu.
- Add three `KitWindowTests` facts: the ids, the running row, and pressing Send kit then Cancel (ui-smoke 236 → 237,
  selected ui-tests 6 → 8).

## Tests

These are development runs, recorded in `0202-impl-log.md`. The gate engine gives the verdict (Invariant 6).

- **build:** exit 0, 0 errors. The only warnings are the vendored MathNet `SYSLIB0051` ones already on the start tree.
- **constructor-unit-tests:** 1259 passed, 0 failed (unchanged).
- **ui-smoke:** 237 passed, 0 failed.
  - `pressing Send kit asks for a zip named for the kit and starts the send, and Cancel cancels it` passes.
- **ui-tests** (`KitWindowTests`): 8 passed, 0 failed.
  - `the Send kit UiIds are the stable intent-named ids` passes.
  - `a running send shows its progress bar, its count and Cancel, and hides Send kit` passes.
- **elevation** (from the arc origin): `ELEV-GATE: OK`, 0 new findings in 12 projects.
- **The fourteen `invariant-s22-*` greps:** each finds nothing.

```yaml
gates:
  constructor_unit_tests: 1259
  ui_smoke_tests: 237
```

The ui-tests count (8) is only the selected class, so it is not a suite baseline.

## Architecture

- **The view only projects `kitSend` and dispatches.**
  - The verb's press hands the picker the kit's `zipFileName` and dispatches `SendKit` with the answered zip.
  - The row reads the model's `kitSend` through the shared `kitArchiveProgressView`.
  - No state lives in the view, so the button and a test send the same message.
- **Send kit and its progress row are exclusive.** Both read `kitSend.job.isRunning`, so a window offers one send
  at a time, as the `SendKit` arm enforces.
- **The view file opens `KitArchiveJob`.** The extension `pickZipToWriteThen` needs that open. Its one record
  literal, `KitArchiveProgressIds`, is annotated, so its labels capture nothing.

## Deferred

Nothing in this step's scope.

- Composing the real archive and the `onUiThread` host into the windows is step 233's, by the spec's plan.
- Open kit from zip… is a later S6 step.

## Gotchas

- **With a picker that answers at once, `SendKit` is dispatched inside the press** (`startPick` is
  `Async.StartImmediate`). One `RunJobs` renders the row.
- **`CancelSendKit` leaves the progress row showing.** The model is unchanged until `SendKitEnded` returns `kitSend`
  to idle.
- **`verbButton` re-patches its press every render; `actionButton` keeps its first.** Send kit's closure reads the
  current kit name. Cancel's closure only dispatches.

## Changelog

- 2026-11-09 — step 0202: the Send kit ids, the Kit menu's `Send kit…` and its progress row, and three
  `KitWindowTests` facts.
