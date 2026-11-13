# Step 0342 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization.

- Steps 335–338 moved the fit machinery into `MuellerFitting`.
- Steps 339–340 moved the noise model into `BenchNoise`.
- Step 341 moved the ensemble statistics into `RecoveryEnsembles`.
- This step, under obligation S101-COVARIANCE-MOVE, moves a fit's own account of its uncertainty into the product,
  for the Planner's layer 2 (001 §M):
  - the fit report becomes `MuellerFitData.report`;
  - the new `OpticalConstructor.Optimization.FitUncertainty` holds the rest.
  
  Each moved name is now declared once, in the product, and `InverseFitHarness.fs` declares none of them.

## What's working

- Moved `fitReportOf` onto `MuellerFitData` as the member `report` (ELEV013).
- Added the `FitUncertainty` module, compiled directly after `RecoveryEnsembles.fs`. It holds:
  - `ParameterWeights`, a `[<NumericCarrier>]`;
  - `PredictedVariance`;
  - `predictedRelativeErrors`;
  - `combinationVariance`;
  - `differenceWeights`.
- `differenceWeights` takes `ParameterName`s, is built without mutation, and answers a `Result`: an unknown axis is the
  new `MuellerFitError.UnknownAxisErr`, not a throw.
- Repointed `MuellerInverseTests` and `BiaxialInverseTests` to `first.fit.report`. The ensemble fact's pair variance
  now goes through `differenceWeights … |> expectOk` and is read as `.value`. Numbers, bands and messages are
  unchanged.

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves. Logs are under `specs/0050/.artifacts/0342-dev-*.log`.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; the only warnings are the 2 vendored MathNet SYSLIB0051 | 79 s | 600 s |
| elevation (no base, 12 of 12 projects) | `ELEV-GATE: OK - 0 new finding(s)`, known findings 0 | 206 s | 600 s |
| constructor-unit-tests (whole) | Passed!, 1553 of 1553, 0 failed, 0 skipped | 69 s | 180 s |
| unit-tests (`BerremanTests.MuellerInverseTests`) | Passed!, 20 of 20, 0 failed, 0 skipped | 705 s (11 min 45 s) | 7200 s |
| 19 × invariant-s33-* | every decoded `git grep` found nothing, also with `--untracked` | < 1 s each | 300 s |

- **No run exceeded its deadline.** The constructor suite and elevation ran concurrently with the first minutes of the
  unit-tests run.
- **Elevation's suppressed count went from 1497 to 1500.** The +3 are ELEV004 `.value` reads in `FitUncertainty.fs`,
  which the gate command excludes (R7).
- **The change-expectation fact:** `an ensemble of noisy experiments recovers every constant, and its scatter IS what
  one fit's own covariance predicts`. Its TRX outcome is `Passed`, in 9 min 48 s.
  - It drives all four moved names: `report`, `predictedRelativeErrors`, `combinationVariance` and
    `differenceWeights`.
- **Counts:**
  - `MuellerInverseTests` is at 20, the same as step 341.
  - The constructor suite is at 1553, the same as step 341.
  - No test was added, removed or renamed.
- **`BiaxialInverseTests` was not run.** It takes about 997 s and is not selected for this step. It compiles, and its
  two edits are the same substitutions the selected class exercises.
- **Acceptance greps:**
  - `FitUncertainty.fs` declares `ParameterWeights`, `PredictedVariance`, `predictedRelativeErrors`,
    `combinationVariance` and `differenceWeights`.
  - `MuellerFitting.fs` declares `MuellerFitData.report` and `UnknownAxisErr`.
  - Each of these names is declared exactly once in the tree, and `InverseFitHarness.fs` declares none of them.
- **LF:** every edited file and the new file have 0 CR bytes.

```yaml
gates:
  berreman_unit_tests: 20
  constructor_unit_tests: 1553
```

## Architecture

- **`FitUncertainty` is the product's fit-uncertainty module.** It depends on:
  - `MuellerFitting`, for `MuellerFitData`, `RelativeError` and `MuellerFitError`;
  - `FitQuality`, for `FitReport` and its covariance;
  - Domain's `ParameterName` and `ParameterScaling`.
  
  The Planner's layer 2 can reuse it without referencing BerremanTests.
- **The report is a member of the fit, not a free function.** A function whose only parameter is one of our types
  belongs on that type (ELEV013). `report` reuses the `ScaledVector` → `DesignVector` re-wrap that
  `residualJacobianAt` already makes at the same FitQuality seam.
- **Weights are a numeric carrier; the variance is elevated.** `ParameterWeights` stays a `float[]`, because it is read
  whole against the covariance (R3). `PredictedVariance` is a scalar with a meaning, so it is elevated.
- **Errors are values.** An unknown axis is `UnknownAxisErr of ParameterName` on the existing `MuellerFitError`, so a
  test unwraps it with the same `expectOk`. No product code matches on `MuellerFitError`, so the new case breaks no
  exhaustive match (the build has no FS0025).
- **The arithmetic is unchanged.**
  - `report` calls `FitQuality.reportFrom` on the same three arguments.
  - The scaled-space conversion in `predictedRelativeErrors` is unchanged.
  - The quadratic form sums the same products in the same `i`, `j` order.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **`MuellerFitData.report` recomputes on every read**, because it takes a finite-difference Jacobian. Bind it once,
  as both ensemble facts do (`let quality = first.fit.report`).
- **`differenceWeights` puts −1.0 at a coinciding axis.** `minus` wins when `plus = minus`, which preserves the
  harness's write order. An unknown `plus` is reported before an unknown `minus`.
- **The unknown-axis branch has no dedicated test.** The step authorizes no new test identity. The selected fact
  exercises the happy path.

## Changelog

- 2026-11-12 — Step 0342: moved `fitReportOf` onto `MuellerFitData` as `report`. Moved `predictedRelativeErrors`,
  `combinationVariance` and `differenceWeights` into the new `OpticalConstructor.Optimization.FitUncertainty`, with
  the `ParameterWeights` carrier and the `PredictedVariance` scalar. Added `MuellerFitError.UnknownAxisErr` and
  repointed the two inverse suites.
