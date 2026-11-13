# Step 0341 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization.

- Steps 335–338 moved the fit machinery into `MuellerFitting`.
- Steps 339–340 moved the noise model into `BenchNoise`.
- This step, under obligation S101-ENSEMBLE-MOVE, moves the ensemble statistics into the new
  `OpticalConstructor.Optimization.RecoveryEnsembles`. Each moved name is now declared once, in the product, and
  `InverseFitHarness.fs` declares none of them.

## What's working

- Moved `RecoveredQuantity`, `RecoveryStatistics`, `NoisyExperiment`, `statisticsFor`, `standardErrorOfMean` and
  `rowFor` from `InverseFitHarness` into the new `RecoveryEnsembles` module, compiled directly after `BenchNoise.fs`.
- Elevated the ensemble's names and values:
  - quantities are named by `ParameterName` and valued in `QuantityValue`;
  - `mean` and `absoluteScatter` are `QuantityValue`s;
  - the new `EnsembleSize` sizes `standardErrorOfMean`.
- `rowFor` answers a `Result`: an unknown name is the new `MuellerFitError.UnknownQuantityErr`, not a throw.
- The three noisy-ensemble suites and `BiaxialSample` read the product through `expectOk`. Their numbers, bands and
  messages are unchanged.

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves. Logs are under `specs/0050/.artifacts/0341-dev-*.log`.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; the only warnings are the 2 vendored MathNet SYSLIB0051 | 80 s | 600 s |
| elevation (no base, 12 of 12 projects) | `ELEV-GATE: OK - 0 new finding(s)`, known findings 0 | 213 s | 600 s |
| constructor-unit-tests (whole) | Passed!, 1553 of 1553, 0 failed, 0 skipped | 72 s | 180 s |
| unit-tests (`BerremanTests.MuellerInverseTests`) | Passed!, 20 of 20, 0 failed, 0 skipped | 694 s (11 min 34 s) | 7200 s |
| 19 × invariant-s33-* | every decoded `git grep` found nothing | < 1 s each | 300 s |

- **No run exceeded its deadline.** The constructor suite and elevation ran concurrently with the unit-tests run for
  its first five minutes or so.
- **The change-expectation fact passed:** `an ensemble of noisy experiments recovers every constant, and its scatter
  IS what one fit's own covariance predicts`. Its TRX outcome is `Passed`, in 9 min 37 s.
  - It drives all six moved names: `RecoveredQuantity.ofAxis` and `derived`, `statisticsFor`,
    `standardErrorOfMean`, `rowFor` (both by literal name and by `ParameterName`), and `NoisyExperiment` (through
    `recoverFromNoisy`).
  - Its bands are unchanged and still pass, which is consistent with the statistics being bit-identical.
- **Counts:**
  - `MuellerInverseTests` is at 20, the same as step 339's run of the class.
  - The constructor suite is at 1553, the same as step 340.
  - No test was added, removed or renamed.
- **`BiaxialInverseTests` and `AbsorbingBiaxialNoisyTests` were not run.** Each takes about 1000 s and neither is
  selected for this step. Both compile, and their edits are the same mechanical substitutions the selected class
  exercises.
- **Acceptance greps:**
  - `RecoveryEnsembles.fs` declares `RecoveredQuantity`, `RecoveryStatistics`, `NoisyExperiment`, `EnsembleSize`,
    `statisticsFor`, `standardErrorOfMean` and `rowFor`.
  - `InverseFitHarness.fs` declares none of them.
- **LF:** every edited file and the new file have 0 CR bytes.

```yaml
gates:
  berreman_unit_tests: 20
  constructor_unit_tests: 1553
```

## Architecture

- **`RecoveryEnsembles` is the product's ensemble-statistics module.** It depends on:
  - `MuellerFitting`, for `QuantityValue`, `RelativeError`, `MuellerFitData` and `MuellerFitError`;
  - `BenchNoise`, for `NoiseSeed`;
  - Domain's `ParameterName` and `ParameterAxis`.
- **Errors are values.** `rowFor`'s unknown name is `UnknownQuantityErr of ParameterName` on the existing
  `MuellerFitError`, so a test unwraps it with the same `expectOk` it uses for the other moved calls. No product code
  matches on `MuellerFitError`, so the new case breaks no exhaustive match.
- **The arithmetic is unchanged.** `statisticsFor` performs the harness's operations on the same doubles, in the
  same order:
  - `List.average` for the mean;
  - the Bessel-corrected variance;
  - `sqrt variance` for the absolute scatter;
  - that value over `|truth|` for the relative scatter;
  - `RelativeError.between` for the bias and the worst case.
  
  `mean` and `absoluteScatter` are born as `QuantityValue`s where they are computed (elevate at origin).

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **The step text writes `standardErrorOfMean` as `... |> RelativeError`.** `RelativeError`'s case constructor is
  `RelativeErr`, so the code pipes into `RelativeErr`. The value is identical.
- **Two test-local `let rowFor` helpers remain in the tree:**
  - `CatalogueImportInMemoryTests.fs:1221`;
  - `LibraryControlsTests.fs:311`.
  
  Each is unrelated to the moved function, sits inside a test body in a test project outside ELEV's scope, and
  predates this step. They are not duplicates of the moved declaration.
- **`BiaxialSample` and `AbsorbingBiaxialNoisyTests` now open `OpticalConstructor.Domain.ItemModels`**, for
  `ParameterName`. `BiaxialSample` also opens `MuellerFitting` and `RecoveryEnsembles`. The compiler asked for each.
- **The doc on `MuellerInverseTests.recoveredQuantities` still says "(see `RecoveredQuantity`)".** That type's own
  doc keeps the birefringence rationale in one line.

## Changelog

- 2026-11-12 — Step 0341: moved the ensemble statistics (`RecoveredQuantity`, `RecoveryStatistics`, `NoisyExperiment`,
  `EnsembleSize`, `statisticsFor`, `standardErrorOfMean`, `rowFor`) into `OpticalConstructor.Optimization.RecoveryEnsembles`.
  Added `MuellerFitError.UnknownQuantityErr` and repointed the test callers through `expectOk`.
