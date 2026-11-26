# Step 0202 — impl-log (IMPLEMENT: Send kit… and its progress row in the kit bar)

## Progress

- [x] UiIds: the four Send kit ids.
  - Added to the kit-bar group (step 188), after `kitChangesKeepMineButton`, each with a one-line doc citing S6:
    - `sendKitButton` (`"SendKitButton"`);
    - `sendKitProgressBar` (`"SendKitProgressBar"`);
    - `sendKitProgressText` (`"SendKitProgressText"`);
    - `sendKitCancelButton` (`"SendKitCancelButton"`).
- [x] KitWindowView: the Send kit… verb.
  - The Kit menu's `WrapPanel` gains `Send kit…` first. It is a `MaterialsWindowView.verbButton` carrying
    `sendKitButton`.
  - It is present when `kitFolderOpt` is `Some` and `kitSend.job.isRunning` does not hold.
  - Its press calls `model.kitShell.zipPicker.pickZipToWriteThen model.kit.kitFile.name.zipFileName (SendKit >> dispatch)`.
- [x] KitWindowView: the progress row.
  - New private `kitSendRows` sits directly under the verb row in the bar's children.
  - While `kitSend.job.isRunning` holds, it shows
    `kitArchiveProgressView model.kitSend { bar = sendKitProgressBar; text = sendKitProgressText; cancel = sendKitCancelButton } (fun () -> dispatch CancelSendKit)`.
  - Nothing else in the bar changes.
- [x] KitWindowTests: three facts, mounted over `kitWindowView` through the class's `withKitWindow`, each model
  loaded from a folder kit (`TestAppScope.openedKitIn`). Three private helpers were added: `recordingHandle`,
  `zippedFiles` and `sendProgressIds`.
  - `the Send kit UiIds are the stable intent-named ids` (no trait).
  - `a running send shows its progress bar, its count and Cancel, and hides Send kit` (no trait).
  - `pressing Send kit asks for a zip named for the kit and starts the send, and Cancel cancels it`
    (`ui-smoke`), pressed at each box's centre through the class's `pressBox` / `pressAtCentre`, which mirror
    `LibraryWindowTests.clickOn`.
- [x] Development runs: build, ui-tests (`KitWindowTests`), ui-smoke, constructor-unit-tests, elevation, the 14
  invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: the four ids.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitWindowView.fs`:
  - the Send kit… verb and `kitSendRows`;
  - `open OpticalConstructor.Domain.KitArchive` (for `KitName.zipFileName`);
  - `open OpticalConstructor.Ui.KitArchiveJob` (for `KitZipPicker.pickZipToWriteThen`, `KitArchiveProgressIds` and
    `kitArchiveProgressView`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitWindowTests.fs`:
  - the same two opens;
  - the three helpers;
  - the three facts.

## Decisions

- **`KitArchiveJob` is opened in `KitWindowView.fs`, not qualified as step 201 does in the 4,300-line
  `TableAndElementRotationView.fs`.**
  - `pickZipToWriteThen` is an extension member, and F# reaches one only through an open of its module.
  - `KitWindowView.fs` builds one record literal, the `KitArchiveProgressIds` itself, and that literal is annotated.
    So the `bar` / `text` / `cancel` capture that step 200's gotcha names cannot strike there.
- **The fact's zip name is written out, not derived through `zipFileName`.**
  - The smoke fact expects `FileName $"{kit name}.zip"`. Deriving it through `zipFileName` would only test that the
    view calls the function the test calls.
  - A fresh test kit is named for its GUID folder leaf, whose file stem is itself.
- **The smoke fact's zip lies under a GUID fact folder of the owner's exports folder.** That keeps it outside the kit,
  as step 201's facts do. The deferred host never runs the job, so the fact also asserts that no zip was written.
- **The running fact checks a little more than the slice lists:**
  - the bar is not indeterminate once a file is told;
  - Cancel's label;
  - the scratch kit still shows Close kit (so the bar rendered);
  - rendering cancels nothing.

  The smoke fact also checks that Send kit is gone while the send runs and that the text reads `Starting…` before
  the first file. Every listed assertion is present.
- **The bar's value is compared at 12 decimal places** (`Assert.Equal(0.4, bar.Value, 12)`). `MatrixComparison.fs`
  is in `BerremanTests`, which `OpticalConstructor.Ui.Tests` does not reference.

## Testing state

These are development runs only. The gate engine gives the verdict (Invariant 6).

- **build** (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors.
  - The only warnings are the vendored MathNet `SYSLIB0051` ones already on the start tree. None come from our code.
- **ui-tests** (selected class `OpticalConstructor.Ui.Tests.KitWindowTests`): 8 passed, 0 failed (6 → 8, +2). Both
  new identities are `Passed` in the TRX.
- **ui-smoke:** 237 passed, 0 failed (236 → 237, +1).
  - `pressing Send kit asks for a zip named for the kit and starts the send, and Cancel cancels it` is `Passed` in
    the TRX.
- **constructor-unit-tests:** 1259 passed, 0 failed (unchanged: this step adds no unit test).
- **elevation** (`ASG_ARC_BASE_COMMIT` = the `.arc-base-json` origin `f395597c…`):
  - `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, 202 s;
  - known findings 0.
- **The 14 `invariant-s22-*` greps:** each finds nothing.
- **Line endings:** LF. `git diff --numstat` matches `--ignore-cr-at-eol` exactly.

## Artifacts

- `specs/0050/.artifacts/0202-build-01.log`
- `specs/0050/.artifacts/0202-ui-tests-01.log`
- `specs/0050/.artifacts/0202-ui-smoke-01.log`
- `specs/0050/.artifacts/0202-constructor-unit-tests-01.log`
- `specs/0050/.artifacts/0202-elevation-01.log`

## Gotchas

- **A picker that answers at once dispatches `SendKit` inside the press.** `startPick` runs
  `Async.StartImmediate`, so `pressBox`'s `RunJobs` is enough for the progress row to render. A real dialog answers
  later, on the UI thread.
- **`verbButton` re-patches its press on every render (`SubPatchOptions.Always`).** So Send kit's closure always reads
  the current kit name and shell, a reload's rename included.
- **`actionButton` keeps its first closure.** That is safe for Cancel, which only dispatches `CancelSendKit`.
- **`CancelSendKit` leaves the row showing.** The model is unchanged until `SendKitEnded` returns `kitSend` to idle.
  The smoke fact therefore checks only the handle's one cancel after pressing Cancel.
