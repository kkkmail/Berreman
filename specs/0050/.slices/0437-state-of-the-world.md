# Where we are

Step 0437 is AT-2's sixth part (spec 0050 S12.2; 001 §Q AT-2 "4. Full five-seed ensemble"). Steps 0432–0436 built the
absorbing-biaxial-noisy kit through the app's messages and ended it with the fit of seed 1's observations. This step
adds the full five-seed ensemble as an xUnit v3 `Explicit` fact. It runs on demand only. It re-attaches seeds 2–5 to
the same five plate experiments through the split messages, fits each through the shell, and judges the five
recovered crystals' scatter by the noisy suite's bands. The kit-acceptance gate discovers the fact and skips it.

# What's working

- New explicit fact: AT-2's five-seed ensemble through the study engine, on demand only (`[<Fact(Explicit = true)>]`).
- Seeds 2–5 re-attach to the plates through `SplitByColumn` / `MatchByTemplate`, one fit run each at 12:0k.
- Every reported quantity's scatter is above zero and `absorbingBandBreaches` over `rowFor`'s scatter is empty.
- Seed attachment, crystal promotion and the stored-study read are shared helpers, not copies.
- The build's clock setter rides on `BuiltAcceptanceKit`, so the ensemble moves the build's own clock.

# Tests

These are development runs, evidence only and not gate verdicts; the supervisor's gates are authoritative. Each run
used the gate's own command and selection.

- **build:** exit 0, 0 errors, 85 s against 600 s. Warnings are only the exempt NU1902 and the vendored MathNet's four
  pre-existing `SYSLIB0051`.
- **elevation** (ELEV004 excluded, all 13 projects): `ELEV-GATE: OK`, 0 new findings, 233 s against 600 s. The diff is
  test-only, which elevation does not analyze.
- **kit-acceptance** over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 852 s against 3600 s.
  - Total 12: 11 succeeded and 1 skipped.
  - The skipped one is `AT-2's five-seed ensemble through the study engine keeps every reported quantity's scatter
    within the noisy suite's bands`: discovered, "Not run (due to explicit test filtering)", TRX `NotExecuted`.
  - The count is unchanged at 11.
- **ui-tests** (`StudyViewTests`, `DataStepTests`): `Passed!`, 18 of 18, 30 s against 360 s.
- **ui-smoke** (all discovered): `Passed!`, 264 of 264, 219 s against 600 s.
- **The twenty-one s42 invariants:** each decoded `git grep` finds nothing.
- **The ensemble on demand** (`--explicit only`): `Passed!`, 1 of 1, in 4043 s.
  - The build and seed 1's fit took 923 s.
  - Seeds 2–5 took 650, 889, 749 and 829 s each, every one inside the 1800 s deadline (R0005's bound).
  - The five runs `AbsorbingTriclinic@20260601T120100Z` … `T120500Z` were written in seed order.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  kit_acceptance_tests: 11
  ui_smoke_tests: 264
  ui_tests: 18
```

# Architecture

- **One file changed:** `OpticalConstructor.Ui.Tests/KitAcceptanceTests.fs`, the step's one authorized edit. No
  production code changed.
- **The clock:**
  - `testClockAt` answers `ClockProvider * (ClockTime -> unit)`, the clock and its setter, instead of the ref cell.
  - `BuiltAcceptanceKit.setTime` carries the setter out of both builds.
  - AT-1's `finish` receives it as before; AT-2 moves to `absorbingFitTime` through it; the ensemble moves it after
    the build.
- **Seed attachment:**
  - `attachSeedObservations (folder) (seed) (model)` is the one split-and-match of a seed's file, with its checks.
  - `attachNoisyObservations` writes the five files and calls it for seed 1; the ensemble calls it for seeds 2–5.
  - It now also checks each experiment holds that seed's file and its own key's rows (`KeyedDataRows`).
- **Shared reads:**
  - `recoveredCrystalOf (study) (run)` is the one `promotedMaterialWith` read, shared by `assertAbsorbingRecovery` and
    the ensemble.
  - `storedAbsorbingStudy (reader)` is the one stored-study read, shared by the recovery fact and the ensemble.
- **Fit times:** `ensembleFitTimes : (NoiseSeed * ClockTime) list` puts the k-th seed at 2026-06-01T12:0k:00Z. The
  run names come from `StudyName.fitRunNameAt`.
- **What each test asserts:**
  - The ensemble asserts non-vacuity (scatter > 0) and the bands, through `RecoveryEnsembles.statisticsFor` and
    `rowFor`.
  - The bias and ordering statements stay in `AbsorbingBiaxialNoisyTests`, the one place they are asserted.

# Deferred

Nothing.

# Gotchas

- **An xUnit v3 `Explicit` fact needs `--explicit on|only`.** Without it, MTP reports the fact as skipped, and
  kit-acceptance's TRX `skip_policy: allow` covers that. To run the ensemble:

  ```
  dotnet test --no-build -c Release --explicit only --filter-trait "Category=kit-acceptance" \
    --filter-method "OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's five-seed ensemble through the study engine keeps every reported quantity's scatter within the noisy suite's bands"
  ```

- **The ensemble costs about 67 minutes** on the arc's machine: the build, then five fits of 650–923 s.
- **The ensemble's kit is left for diagnosis** under `bin/.../KitAcceptanceTests/AT-2-ensemble/`. The next run deletes
  it, as AT-2's own `AT-2` folder is handled. The kit is closed in a `finally`.
- **`MatchByTemplate` re-attaches an experiment that already holds data.** `Experiments.attachDataFile` replaces
  `dataOpt`, and the split matches by name whatever the experiment holds. That is how the five measurements share the
  five plate experiments.
- **Keep R0001–R0005 through 0438–0494**, unchanged by this step. That covers the 1800 s bound each ensemble fit is
  held to.

# Changelog

- 2026-11-16, attempt 01:
  - added AT-2's five-seed ensemble as an explicit kit-acceptance fact, which passed on demand in 4043 s;
  - shared the seed attachment, the crystal promotion and the stored-study read;
  - moved the build clock's setter onto `BuiltAcceptanceKit`;
  - kit-acceptance holds 11 succeeded and 1 skipped.
