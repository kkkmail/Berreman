# Step 0437 — impl log

## Progress

- Read the protocol, the project prompt, rulings R0001–R0005 (their "Binding ruling" sections), the slice, its gates
  and 0436's state of the world. No ruling authorizes or needs a production edit here. All five are kept by leaving
  product code untouched: R0001's dispatch, R0002's analyzer law, R0003's identity provider (AT-2's reflected detector
  still takes `reflectedDetectorSlot` through `elementIds`), R0004's single evaluation and concurrent residual, and
  R0005's single `rhoT` read, server collector and 1800 s bound (each ensemble fit keeps the build's 1800 s deadline).
- Before: `KitAcceptanceTests` held 11 kit-acceptance facts (0436's checkpoint `kit_acceptance_tests: 11`).
- Edited `KitAcceptanceTests.fs` (the step's one authorized edit):
  1. `testClockAt` answers the clock and its setter, not the bare ref cell. `BuiltAcceptanceKit` gains
     `setTime : ClockTime -> unit`, filled by both builds. AT-1 passes it to `finish` as before; AT-2 moves to
     `absorbingFitTime` through it.
  2. `ensembleFitTimes : (NoiseSeed * ClockTime) list` puts the k-th seed of `noisySeeds` at 2026-06-01T12:0k:00Z, so
     seed 1's time is the build's `absorbingFitTime`.
  3. `attachNoisyObservations`'s split-and-match moved, unchanged, into
     `attachSeedObservations (folder) (seed) (model)`. The build writes the five files, then calls it for seed 1; the
     ensemble calls it for seeds 2–5, so all five go through the same `SplitByColumn` / `MatchByTemplate` messages
     (step 434). It gains one check: each experiment holds that seed's file and its own key's rows
     (`KeyedDataRows`, steps 318 (b) and 320).
  4. `assertAbsorbingRecovery`'s `promotedMaterialWith` read moved to `recoveredCrystalOf (study) (run)`.
     `assertAbsorbingRecovery` and the ensemble both call it.
  5. The recovery fact's stored-study read moved to `storedAbsorbingStudy (reader)`, which the ensemble reuses.
  6. New fact `AT-2's five-seed ensemble through the study engine keeps every reported quantity's scatter within the
     noisy suite's bands`: `[<Fact(Explicit = true)>]`, trait `kit-acceptance`, one `///` line citing S12.2.
     - `buildAbsorbingKit (FolderName "AT-2-ensemble")`, which ends with seed 1's fit.
     - For each later seed, in order:
       - the clock moves to its time;
       - `attachSeedObservations` re-attaches that seed's file;
       - `assertAbsorbingStudyData` shows the study reads exactly that seed's observations;
       - `runStudyFitThroughShell` runs `absorbingFitRequest` with the 1800 s deadline;
       - the committed run must be `absorbingStudyName.fitRunNameAt <time>`.
     - It then reads the stored runs: five names in seed order, equal to the window's `kitFitRuns`.
     - It promotes the five crystals through the stored study (`recoveredCrystalOf`) and builds
       `table = absorbingReportedQuantities |> List.map (statisticsFor absorbing crystals)`.
     - It requires every row's scatter above zero.
     - It requires `absorbingBandBreaches` over `rowFor table q.name`'s scatter to be empty; an `Error` of `rowFor`
       fails the fact.
     - The kit closes in a `finally`.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs` (+89 / −28, LF, no CR churn).

## Decisions

- **The clock setter rides on `BuiltAcceptanceKit`.** The slice names `buildAbsorbingKit (FolderName
  "AT-2-ensemble")` as step (1) and the clock's moves as step (2), after the build. The build's clock therefore has
  to be movable from outside it. AT-1's `buildOpticsMuellerKitWith` continuation was the alternative. It would have
  renamed the build the slice names, so I did not mirror it.
- **Reuse, never copy.** Seeds 2–5 attach through the very helper seed 1's build uses. The crystal is promoted through
  the very read `assertAbsorbingRecovery` uses, and the stored study through the very read the recovery fact uses.
  No body is duplicated.
- **The bias and ordering statements stay in `AbsorbingBiaxialNoisyTests`**, as the slice says. The ensemble asserts
  only non-vacuity (scatter > 0) and the bands.
- **Run folder.** The ensemble uses its own `AT-2-ensemble` folder, which `buildAbsorbingKit` deletes at the next
  run's start and otherwise leaves for diagnosis, as AT-2's own folder is left. The kit closes in a `finally`.

## Testing state

These are development runs, recorded as evidence and not as gate verdicts. Every run used the gate's own command
and selection.

- **build** (`dotnet build Berreman.slnx -c Release`): exit 0, 0 errors, 85 s against 600 s.
  - Warnings: the exempt NU1902 and the vendored MathNet's four pre-existing `SYSLIB0051`.
  - Nothing from `OpticalConstructor.Ui.Tests` but NU1902.
- **Discovery:** the new fact alone, filtered by its census id, without `--explicit`.
  - It is discovered under `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's five-seed ensemble through the study
    engine keeps every reported quantity's scatter within the noisy suite's bands`.
  - It is skipped ("Not run (due to explicit test filtering)", TRX `NotExecuted`), in 2 s.
- **The ensemble on demand** (`--explicit only`, the one fact): `Passed!`, 1 of 1, in 4043 s (1 h 07 min).
  - The five fit runs were written in seed order: `@20260601T120100Z` to `@20260601T120500Z` at 11:26:22, 11:37:12,
    11:52:01, 12:04:30 and 12:18:19.
  - The build plus seed 1's fit took 923 s from the run folder's creation at 11:10:59.
  - Seeds 2–5 took 650, 889, 749 and 829 s each, the re-attachment included, every one inside its 1800 s deadline
    (R0005's bound).
  - Every scatter was above zero and every band held.
- **kit-acceptance** (the gate command over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`): `Passed!`.
  - Total 12: 11 succeeded and 1 skipped, the new explicit fact. 852 s against 3600 s.
  - The count is unchanged at 11, as the acceptance requires.
- **ui-tests** (`StudyViewTests` and `DataStepTests`): `Passed!`, 18 of 18, in 30 s against 360 s.
- **ui-smoke** (whole, as its gate picks it): `Passed!`, 264 of 264, in 219 s against 600 s.
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK`, 0 new findings, 233 s against 600 s.
- **The twenty-one s42 invariants:** each decoded `git grep` finds nothing.
- No development run exceeded its gate's effective deadline. The ensemble is not a gate run; the gate only discovers
  it.

## Artifacts

- `specs/0050/.artifacts/0437-build-dev.log` — development build.
- `specs/0050/.artifacts/0437-kit-acceptance-discovery.log` — the new fact discovered and skipped by default.
- `specs/0050/.artifacts/0437-kit-acceptance-ensemble.log` — the on-demand ensemble run (`--explicit only`).
- `specs/0050/.artifacts/0437-kit-acceptance-dev.log` — the kit-acceptance gate command over the class.
- `specs/0050/.artifacts/0437-ui-tests-dev.log`, `0437-ui-smoke-dev.log`, `0437-elevation-dev.log`.
- The ensemble's kit stays at `OpticalConstructor.Ui.Tests/bin/x64/Release/net10.0/KitAcceptanceTests/AT-2-ensemble/`
  for diagnosis (five fit runs). The next run deletes it.

## Gotchas

- xUnit v3 on MTP reports an `Explicit` fact as skipped ("Not run (due to explicit test filtering)") unless the run
  passes `--explicit on|only`. The kit-acceptance gate's TRX `skip_policy: allow` covers it, and the gate's succeeded
  count is unchanged at 11.
