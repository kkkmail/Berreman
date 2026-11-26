# Step 0202 — impl-plan (IMPLEMENT: Send kit… and its progress row in the kit bar)

## Approach

Step 201 gave the kit window its Send kit messages (`SendKit`, `SendKitProgressed`, `SendKitEnded`,
`CancelSendKit`), `kitSend` and the shell's `zipPicker` / `archive` / `archiveJobs`. Step 200 gave the shared
`kitArchiveProgressView`. This step makes them visible in the kit bar:

1. `OpticalConstructor.Controls/UiIds.fs`, in the kit-bar group: `sendKitButton`, `sendKitProgressBar`,
   `sendKitProgressText`, `sendKitCancelButton`, each with a one-line doc citing spec 0050 S6.
2. `OpticalConstructor.Ui/KitWindowView.fs`:
   - the Kit menu's `WrapPanel` gains `Send kit…` first. It is a `MaterialsWindowView.verbButton` carrying
     `sendKitButton`, present when `kitFolderOpt` is `Some` and no send runs. Its press calls
     `model.kitShell.zipPicker.pickZipToWriteThen model.kit.kitFile.name.zipFileName (SendKit >> dispatch)`.
   - directly under the verb row, while `kitSend.job.isRunning`, it shows `kitArchiveProgressView` over
     the four ids, cancelling through `dispatch CancelSendKit`.
   - nothing else in the bar changes.
3. `OpticalConstructor.Ui.Tests/KitWindowTests.fs`: three facts over models loaded from folder kits,
   mounted over `kitWindowView` through the class's `withKitWindow`:
   - `the Send kit UiIds are the stable intent-named ids` (no trait);
   - `a running send shows its progress bar, its count and Cancel, and hides Send kit` (no trait);
   - `pressing Send kit asks for a zip named for the kit and starts the send, and Cancel cancels it`
     (`ui-smoke`), pressing at the box's centre through the class's `pressBox`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`

## Risks

- **Record-label capture.** `KitArchiveProgressIds` has `bar`, `text` and `cancel`, so the literal is annotated
  with its type. `KitArchiveJob` is qualified, not opened, as step 201 does in the view file.
- **A stale press closure.** `verbButton` patches its press on every render (`SubPatchOptions.Always`), so the
  Send kit closure always sees the current model's kit name and shell. `actionButton` (Cancel) keeps its first
  closure, which only dispatches `CancelSendKit`.
- **The picker's answer path.** `startPick` runs `Async.StartImmediate`, so a picker that answers at once
  dispatches `SendKit` inside the press. The smoke fact then runs the dispatcher's jobs before it checks the
  progress row.
