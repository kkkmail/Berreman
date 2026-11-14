# Step 0393 — state of the world

## Where we are

Spec 0050, section S11.3 (engines, staged and joint fits, append-only fit runs).

- Steps 386–391 built the engines and joined them into one fit, `StudyFitParam.runFit`.
- Step 392 drove the staged hand-off through it.
- This step hosts that fit as step 200 hosts an archive job. It runs off the UI thread on `JobRunner`, with
  per-iteration progress and Cancel, and its fit run is written once, when the fit completes (001 §N, §P).
- The study view that sends `RunFit` and binds `StudyFitJobState` is S11.5's.

## What's working

- Added the `StudyFitError` cases `FitRunKitErr` (the kit refused the fit run) and `FitRunRaisedErr` (the fit raised).
- Added `OpticalConstructor.Ui/StudyFitJob.fs`: the fit runs through `JobRunner.startBackground` with a cancellation
  source and a stopwatch per job. Each iteration is reported as an iteration count with no known total.
- Added `StudyFitMsg` (`RunFit`, `CancelStudyFit`, `StudyFitProgressed`, `StudyFitEnded`) and its `updateWith`, the
  only place a fit run is written. It runs one fit at a time; a cancel or a refusal writes nothing and keeps the
  committed run.
- Added `StudyFitEnvironment.create`: it reads the study's fit runs from the kit and runs `runFit` over the real SVD
  with one fixed seed.
- Added five `StudyFitJobTests` facts over an in-memory kit, and widened the study-fit error rendering fact.

## Tests

Run locally on 2026-11-14 on the final tree. Every run finished within its effective deadline.

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors.
  - The final build took 17 s, incremental after the mutation runs; the first build took 81 s, which recompiled
    MathNet. The deadline is 600 s.
  - The log holds no `error`. Its warnings are the exempt NU1902 advisories, plus the vendored MathNet `SYSLIB0051`
    pair, which appears only when `MathNetNumerics/Numerics` recompiles (step 0185's operator note).
- `constructor-unit-tests`, whole suite: 1696/1696, 113 s (deadline 180 s).
  - The count is 1691 → 1696, +5, as the acceptance requires.
  - The TRX shows each of these `Passed` under its exact id:
    - `OpticalConstructor.Tests.StudyFitJobTests.RunFit reports each iteration, then writes the fit run once the fit
      completes`;
    - `OpticalConstructor.Tests.StudyFitJobTests.a cancelled fit writes nothing and keeps the last committed fit run`;
    - `OpticalConstructor.Tests.StudyFitJobTests.a fit run that would overwrite an existing one is refused and nothing
      is written`;
    - `OpticalConstructor.Tests.StudyFitJobTests.a fit that raises ends with FitRunRaisedErr carrying its message`;
    - `OpticalConstructor.Tests.StudyFitJobTests.RunFit while a fit runs starts nothing`;
    - `OpticalConstructor.Tests.StudyEngineTests.every study-fit error renders its condition and its payload`
      (widened).
- `ui-smoke`: 258/258, 227 s (deadline 600 s).
- `ui-tests`, selected `OpticalConstructor.Ui.Tests.KitWindowTests`: 16/16, 23 s (deadline 360 s).
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests` and `BerremanTests.MuellerInverseTests`: 45/45,
  11 min 14 s (deadline 7200 s). The first four minutes ran beside elevation.
- `elevation`, standalone with no `-Base`, so all 13 projects: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s),
  222s` (deadline 600 s). It suppressed 1579 findings, the same count as step 392, so there is no new ELEV004 either.
- The twenty `invariant-s38-*` must-find-nothing greps, decoded from the snapshot and run with `git grep`: all twenty
  find nothing.
- Mutation checks on `StudyFitJob.fs`. Each fails only the facts that observe it, and the source was restored byte for
  byte (`cmp`) and rebuilt:
  - M1, the running check dropped: `RunFit while a fit runs starts nothing` fails.
  - M2, a completed run not written: the four facts that read the kit back fail.
  - M3, a raise losing its message: `a fit that raises …` fails.
  - M4, a refused write committed anyway: `a fit run that would overwrite …` fails.
  - M5, Cancel never reaching the fit: `a cancelled fit …` fails at its ten-second wait.
  - M6, progress dropped: `RunFit reports each iteration …` fails.
- Every file written is LF (0 CR bytes), and `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1696
  ui_smoke_tests: 258
  ui_tests: 16
  berreman_unit_tests: 45
```

## Architecture

- **`StudyFitJobHost` mirrors `KitArchiveJobHost` (step 200)**, as the brief directs.
  - `posting` makes a `CancellationTokenSource` and a `Stopwatch` per fit, and runs it as
    `RanToCompletion [ fit (WatchedIterations o) ]`.
  - Each `reportIteration (IterationCount k)` posts `{ completed = JobStepCount k; total = None; elapsed }`.
  - A raise ends as `FitRunRaisedErr e.Message`, and an outcome holding no single result as `FitRunRaisedErr` naming
    its count.
  - `onUiThread` is `posting` through `Dispatcher.UIThread.Post`.
  - The iteration count is unwrapped by pattern and each elevated value is bound at its origin, so there is no
    call-site re-wrap.
- **`StudyFitMsg.updateWith` is the only fit-run write.**
  - `RunFit` while running changes nothing.
  - `CancelStudyFit` cancels only a running fit.
  - `StudyFitEnded (Ok run)` writes the run, then commits its name.
  - A kit refusal, or any non-cancel error, returns the job to `Idle` with no progress, its committed name kept, and
    holds the error.
  - `FitCancelledErr` is `RunCancelled`.
- **`StudyFitEnvironment.create`** builds a fresh `StudyFitParam` per fit: `createMathNetSvd ()`, `fromSeed (NoiseSeed
  1)`, and the kit's fit runs for the study. So every refit draws the same starts.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **Threading in a host with an immediate `post`.** The worker's progress and end call back on the worker thread,
  possibly before the `RunFit` update that started the fit has been stored.
  - The tests send every message under one lock, as Elmish dispatches one at a time.
  - Any other synchronous host must serialise its messages the same way.
  - `onUiThread` is safe, because the dispatcher queues its posts.
- **`FileName.toKitRelativePath` is private to `KitFolders`.** The tests read the fit run's bytes through `readLabFile`
  at the literal path `QZ-LR@20261101T120000Z.fitrun-json`.
- **Not exercised by a fact:** `StudyFitEnvironment.create`'s `fitWith`. The brief's facts use test-local stubs.
  `runFit` itself is covered by steps 391–392, and S11.5's view composes `create`.
- **Time:** the suite is at 113 s of its 180 s. The six-fact development run (the five new facts and the widened one) took 2.8 s.

## Changelog

- 2026-11-14 — Step 0393: the study-fit job.
  - `StudyFitJob.fs` hosts `runFit` on `JobRunner`, with per-iteration progress and Cancel, and writes the fit run
    once on completion.
  - `StudyFitError` gains `FitRunKitErr` and `FitRunRaisedErr`.
  - Five `StudyFitJobTests` facts added; the rendering fact widened.
