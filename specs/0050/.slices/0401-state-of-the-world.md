# Step 0401 — state of the world

## Where we are

Spec 0050 S11.4 (fit results) continues. Step 399 gave the nonlinear engine its `EngineProblems`, and step 400 gave
the object stage its own. This step gives the calibration stage its problems, read off stage one's three AIR fringe
fits in the instruments' own constants.

To get there, `calibrateLinear`'s run selection moved into `StagedStudy.airRunsWith`, which both members now read.
`calibrateLinear` answers exactly what it did before. Step 402 turns the engines' problems into each entry's σ,
checks realizability and names the suspect entries.

## What's working

- Moved the calibration's AIR-run selection into `StagedStudy.airRunsWith`, an `AirFamilyRuns` of per-run
  `AirRunRows`. `calibrateLinear` now reads its `stageOneRows` and answers exactly what it did.
- Added `StagedStudy.calibrationProblemsWith`: the LP-LP, LP-CPL and CPL-LP AIR fringes as `FittedProblem`s.
  - Their coordinates are the instruments' stage-1 entries, plus the LP-LP visibility.
  - Each starts at the constant stage one wrote, within its entries' bounds.
- Each AIR run gets its own trace's residual, with its RMSE. A Dark run gets none.
- Shared the run-residual RMSE body (`runResidualFrom`) between the nonlinear and calibration engines.
- Added three `StudyEngineTests` facts over step 389's fixture. Constructor tests rose from 1713 to 1716.

## Tests

Development runs on 2026-11-14. These are not gate verdicts; the supervisor re-runs every gate. Every run finished
within its effective deadline.

| Gate | Development outcome | Duration / deadline |
|---|---|---|
| build | exit 0, 0 errors, only the exempt `NU1902` advisory | 45 s (full), 9 s (incremental) / 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` (no base: all 13) | 216 s / 600 s |
| constructor-unit-tests | 1716/1716 passed, whole suite (1713 + 3) | 138 s / 180 s |
| unit-tests (selected `BerremanTests.MuellerReconstructionTests`) | 25/25 passed | 4 s / 7200 s |
| invariant-s39-* (20) | each ran its own command: `INVARIANT HOLDS ... found nothing` | < 1 s each / 300 s |

The three new facts all passed:

- `the calibration fit's problems are the three AIR fringes in the instruments' own constants`
- `each instrument constant of a calibration fit gets its sigma from its fringe`
- `each AIR run of a calibration fit has its own trace's residual`

Step 389's two facts over `calibrateLinear` pass unchanged. A mutation check confirmed that each new fact fails on a
defect in its own part of `calibrationProblemsWith` (see the impl-log).

```yaml
gates:
  constructor_unit_tests: 1716
  berreman_unit_tests: 25
```

`berreman_unit_tests` is the selected class's own count, the same 25 that steps 398 and 400 measured.

## Architecture

- **One run selection for the calibration.** `airRunsWith` is the only place a calibration's Dark rows and AIR runs
  are chosen. The family comes from each run's prep and analyzer (`familyWith`), and each run's rows are its kept
  capture rows. `AirFamilyRuns.stageOneRows` is the one concatenation stage one reads, row for row what
  `calibrateLinear` handed `calibrateFromAirRows` before.
- **A calibration problem is stage one's own fit, in the instruments' constants.** Coordinate k of a family's fringe
  is its runs' k-th stage-1 constant, by step 389's `stageOneConstants`.
  - It is an `EntryCoordinate` of every layout entry taking that constant.
  - Its start is that constant's value in stage one's answer, the same function `calibrateLinear` writes the entry
    from.
  - Centre zero and unit one, so a σ is in the entry's own units (radians).
  - The LP-LP fringe adds the free `FringeVisibilityCoordinate`, starting at `freeCosineFit`'s visibility.
- **Residuals reuse step 398's.** LP-LP uses `fringeResidualAt` of the zero and the visibility, and LP-CPL the same
  with the visibility cos δ. CPL-LP uses `SourceCplModel.lpFrameResidualOn` of {θ = offset + 45°; δ}, phased on
  `lpZero`. A run's residual is the same function over its own trace, so a family's residual at its start is its runs'
  residuals run after run.
- `AirRunRows`, `AirFamilyRuns` and `airRunsWith` sit directly before `calibrateLinear`, not after
  `objectProblemsWith`, because F# needs a member declared before its caller (a recorded deviation, as in step 400).
  `calibrationProblemsWith` follows `objectProblemsWith`.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- Stage one reports a zero in [0°, 180°), so CPL2's −2° truth is 178° (3.106 rad), outside its `dialZero` entry's
  ±0.5 rad bounds. That coordinate's start therefore lies outside its bounds. The slice prescribes both, and nothing
  in this step searches. A profile of it would hold only the solution point.
- A coordinate with no layout entry (the visibility, or a constant whose parameter is known or calibrated) is
  unbounded. Step 402's `sigmasFor` reads only layout entries, so such a coordinate gives no entry a σ.
- A fitted family with no run is refused (`EmptyCaptureSetErr`) rather than given a guessed start. With the real
  solver, `calibrateFromAirRows` refuses first (`SingularDesignErr` on an empty trace).
- At step 389's exact rows every fringe residual is about 1e-16. The first fact therefore repeats its checks over rows
  moved by 2 · (−1)ⁱ (`jittered 2.0`, now parametric), where the RMSEs are about 1e-3.
- The new `run` / `rows` record labels precede `ObjectRunSystem` and `RunResidual`. Construction sites are annotated,
  and the build shows no inference change.

## Changelog

- 2026-11-14 — Step 0401: moved the calibration's AIR-run selection into `StagedStudy.airRunsWith` (`AirRunRows`,
  `AirFamilyRuns.stageOneRows`), with `calibrateLinear` reading it. Added `StagedStudy.calibrationProblemsWith` (three
  AIR fringe problems and per-run residuals) and the shared private `runResidualFrom` in `StudyEngine.fs`, and three
  `StudyEngineTests` facts over step 389's fixture.
