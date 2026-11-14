# Step 0393 — impl plan

## Approach

A study fit runs off the UI thread on `JobRunner`, with per-iteration progress and Cancel, and its fit run is written
once, on completion (spec 0050 S11.3; 001 §N, §P). It is hosted as step 200 hosts an archive job.

1. `StudyEngine.fs`: `StudyFitError` gains two cases, each with its `describe` line:
   - `FitRunKitErr of error : KitError`;
   - `FitRunRaisedErr of reason : string`.
2. New `OpticalConstructor.Ui/StudyFitJob.fs`, compiled directly after `KitArchiveJob.fs`. It declares, in order:
   - `StudyFitJobHandle`;
   - `StudyFitJobHost`, with `posting` and `onUiThread`: one `CancellationTokenSource` and one `Stopwatch` per job,
     and the fit run through `startBackground` as `RanToCompletion [ fit (WatchedIterations o) ]`;
   - `StudyFitJobState`, with `idle`;
   - `StudyFitMsg`, with four cases;
   - `StudyFitEnvironment`, with `create`: `kit.readFitRuns`, then `StudyFitParam.runFit` with one fixed seed;
   - `StudyFitMsg.updateWith`, the only place a fit run is written.
3. New `OpticalConstructor.Tests/StudyFitJobTests.fs`, appended at the end of the compile list. It holds five facts
   over an in-memory kit, using step 384's test-local `qzLrFitRun`.
   - The facts send messages through one lock, the way Elmish dispatches one message at a time. A background
     `post` therefore never races the `RunFit` update that started it.
   - Each fact waits on a `ManualResetEventSlim` with a ten-second limit.
4. `StudyEngineTests`' `every study-fit error renders its condition and its payload` covers the two new cases.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/StudyFitJob.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyFitJobTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **ELEV.** A cross-type re-wrap (`JobStepCount k.value`) is avoided: the iteration count is deconstructed by pattern,
  as `KitArchiveFileCount.jobStepCount` does. Every elevated value is bound at its origin.
- **Threading in the tests.** `posting` with an immediate `post` calls back on the worker thread. One lock serialises
  the updates, and each fact waits on events, never a sleep.
- **Exhaustive matches.** Outside `StudyEngine.fs`, nothing matches every `StudyFitError` case (checked by grep).
