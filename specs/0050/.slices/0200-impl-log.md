# Step 0200 — impl log (IMPLEMENT: the archive job host and the zip dialogs)

## Progress

- Read the task, both system prompts, the project prompt, the slice and its `.gates` snapshot. The project prompt's
  Operator note section is empty.
- Read `JobRunner.fs`, `KitOpening.fs`, Domain `KitArchive.fs`, `MaterialsWindowView.actionButton` /
  `pickEntryExportFile`, `TableAndElementRotationView.pickDataFile` and the existing `pickThen` fact.
- Checked that none of the new names exists yet (ELEV014 / 015 / 019).
- Wrote `KitArchiveJob.fs`, added `startPick` to `KitOpening.fs`, and added both `<Compile>` lines.
- Wrote `KitArchiveJobTests.fs`. Red was the compile failure: the production symbols did not exist before this step.
- Development build and runs: all green (see Testing state). Then two doc citations and one no-op test line, rebuilt
  and re-ran everything: still green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitArchiveJob.fs` (new, `module OpticalConstructor.Ui.KitArchiveJob`):
  - `KitArchiveFileCount.jobStepCount`
  - `KitArchiveProgress.jobProgress`
  - `KitArchiveJobHandle`
  - `KitArchiveJobHost` (`posting`, `onUiThread`, `immediate`, `inert`)
  - `KitArchiveJobState` (`idle`, `started`, `progressed`, `cancel`)
  - `KitArchiveProgressIds`, `kitArchiveProgressView`
  - `KitZipPicker.ofWindow`, `pickZipToWriteThen`, `pickZipToOpenThen`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile>` line, directly after
  `KitOpening.fs` and before `TableAndElementRotationView.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs`:
  - new `startPick<'Picked>`;
  - `KitFolderPicker.pickThen` is now `startPick (this.pickKitFolder ()) onPicked`.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveJobTests.fs` (new, `module KitArchiveJobTests`): the
  five slice facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line, last in
  the list, with no comment.

## Decisions

- **Elapsed time is read when the file is reported, not when the post runs.** `posting` builds the `JobProgress` on
  the worker, then posts `progressed progress`. A dispatcher backlog therefore cannot stretch a file's elapsed time.
- **`posting` maps an end that is not one result to a typed error.** The run is always
  `RanToCompletion [ job observer ]`. The match still covers every `JobOutcome` shape, so an empty or longer result
  list becomes `KitArchiveIoErr`, never a throw on the worker.
- **The cancellation source is never disposed** (documented on `handleOf`):
  - a Cancel can reach the UI thread after the job ended, and `Cancel` on a disposed source throws;
  - a plain source holds no timer or wait handle, so there is nothing to release.
- **The suggested start folder resolves inside its own `try`.** A folder that does not resolve, or a provider that
  throws while resolving it, only leaves the dialog without a start location. If the dialog itself throws, the pick
  answers `None`, as the slice asks.
- **The Cancel button sits in a right-docked `Border`.** `MaterialsWindowView.actionButton` answers a plain `IView`,
  which cannot take `DockPanel.dock`. The `DockPanel`'s fill is the vertical stack.
- **The private helpers are static members** (`observerOf`, `handleOf`) shared by `posting` and `immediate`, so
  there is one observer and one handle body.
- **The picker fact also checks the suggested name.** It records the name handed to `pickZipToWrite`. That is an
  added assertion inside the authorized identity, not a new identity.

## Testing state

These are development runs. The gate engine gives the verdict (Invariant 6). The final runs are over the final tree.

- **build** (`dotnet build Berreman.slnx -c Release`): exit 0, 0 errors.
  - First full build: the only warnings were the two vendored MathNet `SYSLIB0051` warnings already on the start
    tree.
  - Final incremental build: 0 warnings.
- **constructor-unit-tests** (whole suite): 1253 passed, 0 failed (1248 → 1253, +5).
  - All five `KitArchiveJobTests` identities pass in the TRX under the slice's exact names.
  - `KitOpeningTests.pickThen hands on the folder the picker answers and nothing on a cancel` passes as written.
- **ui-smoke**: 236 passed, 0 failed.
- **ui-tests** (`KitStartWindowTests`, `KitWindowTests`): 12 passed, 0 failed.
- **elevation** (from the arc origin `f395597…` via `ASG_ARC_BASE_COMMIT`, ELEV004 excluded):
  `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`.
- **The fourteen `invariant-s22-*` greps** find nothing. They were run with `--untracked` as well, so the two new
  files are covered.
- **LF:** every touched and new file has 0 CR bytes, and `git diff --numstat` equals the `--ignore-cr-at-eol` result.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0200-dev-build-01.log`, `0200-dev-build-02.log`
- `0200-dev-constructor-unit-tests-01.log`, `0200-dev-constructor-unit-tests-02.log`
- `0200-dev-ui-smoke-01.log`, `0200-dev-ui-smoke-02.log`
- `0200-dev-ui-tests-01.log`, `0200-dev-ui-tests-02.log`
- `0200-dev-elevation-01.log`, `0200-dev-elevation-02.log`

## Gotchas

- **`KitArchiveProgressIds` has the fields `bar`, `text` and `cancel`.** A later module that opens `KitArchiveJob` and
  writes a record literal with a `text` or `cancel` field for another record may infer this type. Annotate that
  literal.
- **`actionButton` subscribes its press `OnChangeOf` the id.** The `cancel` closure seen at first render is kept, so a
  surface should pass a closure that dispatches a message, never one that captures a state.
- **`immediate` maps no exception.** A raising job raises into its caller, which is a headless test. Only `posting`
  (and so `onUiThread`) turns a raise into `KitArchiveIoErr`.
