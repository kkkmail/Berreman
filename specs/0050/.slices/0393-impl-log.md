# Step 0393 — impl log

## Progress

- [x] Read the protocol, project prompt, slice and gates. There are no operator rulings and no operator note this step.
- [x] `StudyFitError` gains `FitRunKitErr of error : KitError` and `FitRunRaisedErr of reason : string`, each rendered
  as the brief words it.
- [x] New `OpticalConstructor.Ui/StudyFitJob.fs`, its `<Compile>` line placed directly after `KitArchiveJob.fs`. In
  the brief's order it declares:
  - `StudyFitJobHandle`;
  - `StudyFitJobHost` (`posting`, `onUiThread`);
  - `StudyFitJobState` (`idle`);
  - `StudyFitMsg`;
  - `StudyFitEnvironment` (`create`);
  - `StudyFitMsg.updateWith`.
- [x] New `OpticalConstructor.Tests/StudyFitJobTests.fs`, five facts, appended at the end of the compile list.
- [x] `every study-fit error renders its condition and its payload` widened with the two new cases.
- [x] First build green; the five new facts and the widened fact pass (6/6, 2.8 s).
- [x] Mutation checks M1–M6: each fails only the facts that observe it. The source was restored byte for byte
  (`cmp`) and rebuilt.
- [x] Final gate runs on the restored tree: build, constructor-unit-tests (whole), ui-smoke, ui-tests (KitWindowTests),
  unit-tests (selected), elevation, and the twenty s38 invariants. All green (Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StudyEngine.fs`: the two new cases and their
  `describe` lines.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/StudyFitJob.fs` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile>` line.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyFitJobTests.fs` (new).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs`: the widened rendering fact.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line.

## Decisions

- **The iteration count is unwrapped by pattern.** The observer reads `fun (IterationCount iterations) -> …` and binds
  `JobStepCount iterations` and `JobElapsed stopwatch.Elapsed` at their origin. This replaces the brief's
  `JobStepCount k.value`, and it is how `KitArchiveFileCount.jobStepCount` (step 200) avoids a call-site re-wrap.
- **An outcome holding no single result** (`RanToCompletion` or `CancelledWith` of zero or several) ends as
  `FitRunRaisedErr "the fit job answered <n> results instead of one"`. As in step 200, `CancelledWith [ r ]` answers
  `r`, although this host's run is always `RanToCompletion`.
- **A refused end returns the job to `Idle` with no progress**, so its committed result is kept and no stale
  progress line is left. This covers a kit refusal and any non-cancel error. It is `{ state.job with status = Idle;
  progress = None }`, and the error is held in `errorOpt`.
- `refusedWith` is a local function inside `updateWith`, not a member of `StudyFitJobState`. A private member of that
  type is not visible from `StudyFitMsg`'s extension, and adding an internal member would add surface the contract
  list does not name.
- **One seed per fit.** `create`'s `fitWith` makes `UniformDeviateProvider.fromSeed (NoiseSeed 1)` afresh on every
  call, so every refit draws the same starts. `NoiseSeed 1` is bound at its origin.
- **The tests serialise messages under one lock**, the way Elmish handles one message at a time.
  - With an immediate `post`, the worker's progress and end call back on the worker thread. The lock holds them until
    the `RunFit` update that started the fit has been stored.
  - Every wait is a `ManualResetEventSlim` or a `WaitHandle` with a ten-second limit, never a sleep.
  - The host is counted (a wrapper around `posting`'s `startStudyFit`), and so is the fit. "Starts nothing" is
    therefore checked synchronously as well as after the end.
- **Fit-run bytes** are read through `readLabFile` at the literal path `QZ-LR@20261101T120000Z.fitrun-json`. This is
  the in-memory folder's stored bytes. `FileName.toKitRelativePath` is private to `KitFolders`.

## Testing state

Local gate runs on the final tree (2026-11-14). Every run finished within its effective deadline:

- `build` (`dotnet build Berreman.slnx -c Release -nologo -v:m`): exit 0, 0 errors.
  - The final build took 17 s, incremental after the mutations; the first took 81 s. The deadline is 600 s.
  - The log holds no `error`. Its warnings are NU1902 only, plus the vendored MathNet `SYSLIB0051` pair whenever
    MathNet recompiles.
  - The restored `StudyFitJob.fs` was recompiled: the Ui dll (01:30:16) is newer than the restore (01:29:45).
  - Log: `0393-build.log`.
- `constructor-unit-tests`, whole suite: `Test run summary: Passed!`, 1696/1696, 113 s (deadline 180 s).
  - The count is 1691 → 1696, +5.
  - The TRX shows all five new ids and the widened rendering fact `Passed`.
  - Log: `0393-constructor-unit-tests.log`.
- `ui-smoke`: 258/258, 227 s (deadline 600 s). Log: `0393-ui-smoke.log`.
- `ui-tests` (`KitWindowTests`): 16/16, 23 s (deadline 360 s). Log: `0393-ui-tests.log`.
- `unit-tests` (`MuellerReconstructionTests`, `MuellerInverseTests`): 45/45, 11 min 14 s (deadline 7200 s). Log:
  `0393-unit-tests.log`.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s), 222s` (deadline 600 s), with 1579 suppressed, the
  same as step 392. Log: `0393-elevation.log`.
- The twenty `invariant-s38-*` greps, decoded from the snapshot: all twenty find nothing. Log: `0393-invariants.log`.
- LF: 0 CR bytes in every file written, and `git diff --numstat` equals `--ignore-cr-at-eol`.

Mutation checks on `StudyFitJob.fs` (logs `0393-mut-M*-test.log`, summary `0393-mutations-summary.log`):

- **M1**, `RunFit` ignores a running fit: only `RunFit while a fit runs starts nothing` fails.
- **M2**, a completed run is not written: the four facts that read the kit back fail.
- **M3**, a raise keeps only its type name: only `a fit that raises …` fails.
- **M4**, a refused write is committed: only `a fit run that would overwrite …` fails.
- **M5**, Cancel never reaches the fit: only `a cancelled fit …` fails, at its ten-second wait.
- **M6**, progress is dropped: only `RunFit reports each iteration …` fails.

## Artifacts

- `specs/0050/.artifacts/0393-build-local.log`: the first solution build.
- `specs/0050/.artifacts/0393-dev-tests.log`: the six-fact development run.
- `specs/0050/.artifacts/0393-build.log`: the final solution build.
- `specs/0050/.artifacts/0393-constructor-unit-tests.log`, `0393-ui-smoke.log`, `0393-ui-tests.log`,
  `0393-unit-tests.log`, `0393-elevation.log`, `0393-invariants.log`: the gate runs.
- `specs/0050/.artifacts/0393-mut-M*-build.log`, `0393-mut-M*-test.log`, `0393-mutations-summary.log`: the mutation
  runs.
- `specs/0050/.artifacts/0393-scripts/`:
  - the mutation script and its runner;
  - the pre-mutation copy of `StudyFitJob.fs`;
  - the decoded invariant list.

## Gotchas

- **An immediate-`post` host calls back on the worker thread**, possibly before the `RunFit` update that started the
  fit is stored. The tests serialise all messages under one lock. `onUiThread` needs no lock, because the dispatcher
  queues.
- **`StudyFitEnvironment.create` is not run by a fact.** The brief's facts stub `fitWith`. `runFit` itself is
  covered by steps 391–392, and S11.5 composes `create`.
- **The brief's `JobStepCount k.value`** would be a call-site re-wrap. The observer deconstructs `IterationCount` by
  pattern instead, as step 200's `KitArchiveFileCount.jobStepCount` does.
- **A shell slip:** a `$f` path built from `grep -l` hung `sed` reading stdin, and the command was stopped. Always
  pass explicit paths.
