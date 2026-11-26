# Step 0200 — impl plan (IMPLEMENT: the archive job host and the zip dialogs)

## Approach

A new Ui module, `OpticalConstructor.Ui.KitArchiveJob` (`KitArchiveJob.fs`), compiled directly after `KitOpening.fs`,
holds everything the step names:

1. `KitArchiveFileCount.jobStepCount` and `KitArchiveProgress.jobProgress`: the Domain archive progress read as the
   runner's `JobStepCount` / `JobProgress`.
2. `KitArchiveJobHandle` (`cancelArchiveJob`) and `KitArchiveJobHost` (`startArchiveJob`) with four hosts:
   `posting` (a `CancellationTokenSource` and a `Stopwatch` per job, run through `JobRunner.startBackground` as
   `RanToCompletion [ result ]`, progress and end posted through `post`, a raise mapped to `KitArchiveIoErr`),
   `onUiThread` (`posting` through `Dispatcher.UIThread.Post`), `immediate` (the caller's thread) and `inert`.
3. `KitArchiveJobState` over `JobModel<KitArchiveFileCount>` and the handle: `idle`, `started`, `progressed` through
   `updateJob`, and `cancel ()` while running.
4. `KitArchiveProgressIds` and `kitArchiveProgressView`: a `DockPanel` with the Cancel action button docked right and
   a vertical stack of a `ProgressBar` and its text.
5. `KitZipPicker.ofWindow` (save `Send kit` / open `Open kit from zip`, one `Zip archive` type, start in the suggested
   folder, `None` on cancel, no local path or a throw) and `pickZipToWriteThen` / `pickZipToOpenThen`.

`KitOpening.fs` gains `startPick<'Picked>`; `KitFolderPicker.pickThen` becomes a call of it, so one body starts every
dialog.

Tests: new `OpticalConstructor.Tests/KitArchiveJobTests.fs` (`module KitArchiveJobTests`, five facts named in the
slice), appended last in the test project's compile list.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitArchiveJob.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` (one `<Compile>` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/KitOpening.fs` (`startPick`, `pickThen` over it)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitArchiveJobTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj` (one `<Compile>` line)

## Risks

- ELEV001 / ELEV005 on the wraps (`JobStepCount`, `JobElapsed`, `FileName`): elevate at the `let` that first holds
  the value, never inside a call argument; deconstruct rather than `.value`-rewrap.
- The cancellation source is not disposed: a Cancel may arrive after the job ended, and `Cancel` on a disposed source
  throws. A plain source holds no timer or wait handle.
- The posting cancel fact must wait on events with a ten-second limit, never sleep.
