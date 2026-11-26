# Step 0339 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization.

- Steps 335–338 gave `OpticalConstructor.Optimization/MuellerFitting.fs` the following:
  - the observation generator, the residual closures, the Jacobian adapter and the scaling;
  - the search box, the fit record and the fit runners;
  - `QuantityValue`, `RelativeError` and `recoveryErrors`.
- This step moves the bench noise model's TRANSPARENT path, under obligation S101-NOISE-MOVE. It goes into a new
  module, `OpticalConstructor.Optimization.BenchNoise` (`BenchNoise.fs`), compiled directly after `MuellerFitting.fs`.
  - The noise data types and the transparent functions are declared only in `BenchNoise.fs`.
  - `InverseFitHarness.fs` keeps the absorbing path (`misreadAbsolute`, `noisyObserveAbsoluteWith`), which step 340
    moves.

## What's working

- Moved the bench noise model's data and transparent path from `InverseFitHarness` into the new
  `OpticalConstructor.Optimization.BenchNoise`:
  - `AngleUncertainty`, `ElementUncertainty`, `ThroughputUncertainty` and `NoiseParam`;
  - `NoiseSeed` and `UniformDeviateProvider`;
  - `disturb`, `misread` and `noisyObserveWith`.
- `misread` and `noisyObserveWith` now return `Result` values with the typed errors `MuellerNormalizationErr` (new in
  `MuellerFitError`) and `ObservationForwardErr`. The tests unwrap them with `expectOk`.
- Added `UniformDeviate` as the provider's draw type. Each budget gains `parameterValue`, and
  `NoiseParam.parameterAxes` projects the budget through Domain's `ItemParameterAxis.detectorNoiseAxes`.
- Added the constructor fact `the promoted bench budgets equal the detector catalogue defaults`. It checks:
  - the `decentLab` and `noiseless` budgets;
  - that the three shipped detector models' noise axes equal `decentLab.parameterAxes`;
  - that the four types are declared in the Optimization assembly.
- Deleted the harness's measurement-error banner. The comments in the test suites now point at `BenchNoise` and
  specs/0044/.manual/011-noisy-measurement-tests.md.
- The seeded draws keep their generator and order, so the numbers are bit-identical. `MuellerInverseTests` passed
  20 of 20, and the constructor suite passed 1553 of 1553, one more than before.

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves. Logs are under `specs/0050/.artifacts/0339-dev-*.log`.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (final build) | 47 s | 600 s |
| elevation (no base, 12 of 12 projects) | `ELEV-GATE: OK - 0 new finding(s)`, known findings 0 | 233 s | 600 s |
| constructor-unit-tests (whole) | Passed!, 1553 of 1553, 0 failed, 0 skipped | 87 s | 180 s |
| unit-tests (`BerremanTests.MuellerInverseTests`) | Passed!, 20 of 20, 0 failed, 0 skipped | 11 min 50 s (710 s) | 7200 s |
| 19 × invariant-s33-* | every `git grep` found nothing | < 1 s each | 300 s |

- No run exceeded its deadline.
  - The unit-tests run took longer than the class's ~456 s estimate because elevation ran concurrently for its first
    four minutes.
  - The ensemble fact alone took 9 min 47 s.
- **The first elevation run failed**, with one ELEV010 finding: `AngleUncertainty.degree (d : double)` mixed a bare
  float with our type. `degree` now takes `float<deg>`, as `Angle.degree` does, and the re-run is clean.
  - Optimization's 11 suppressed findings are ELEV004 `.value` reads, which the gate command excludes.
- **Both change-expectation facts passed:**
  - `the measurement-error model is reproducible, bounded, and files data against the NOMINAL angles`;
  - `an ensemble of noisy experiments recovers every constant, and its scatter IS what one fit's own covariance
    predicts`.
- **The new constructor fact passed:** `the promoted bench budgets equal the detector catalogue defaults`.
- **Counts:**
  - `MuellerInverseTests` declares 20 facts, the same as HEAD.
  - The constructor suite rose from 1552 to 1553, which is the one authorized create.
- **The first full build** recompiled the vendored MathNet and showed its two known SYSLIB0051 warnings, as step
  0337 recorded. The final build had 0 warnings.

```yaml
gates:
  berreman_unit_tests: 20
  constructor_unit_tests: 1553
```

## Architecture

- **`BenchNoise` is the product's bench model.** Domain keeps only the detector parameter axes (.spec-md §3.23).
  `NoiseParam.parameterAxes` is the one bridge: each budget converts to a `ParameterValue` through its own
  `parameterValue`, and `ItemParameterAxis.detectorNoiseAxes` builds the axes. Axis names and order come from Domain,
  so no axis construction is duplicated.
- **`UniformDeviate` is the draw type.** The provider's field is `unit -> UniformDeviate`, not `unit -> double`,
  which would be ELEV008. Callers read `(draws.nextDeviate ()).value` at the arithmetic.
- **Errors are values.** The harness's `failwith`s became `MuellerNormalizationErr` and `ObservationForwardErr`.
  `noisyObserveWith` is an ordered `Result` fold, the same shape as `observeWith`, and the first error stops the walk.
- **`AngleUncertainty.degree` takes `float<deg>`.** This is a deviation from the step text's "unchanged" body: the
  step's `double` parameter is ELEV010, and the analyzers win (AGENTS.md; spec 0048 `.manual/002` R6). The value is
  unchanged, because `Angle.degree d` equals `Angle.degree (d * 1.0<deg>)`.

## Deferred

Nothing. Every requirement of the step landed in this round. The absorbing path (`misreadAbsolute`,
`noisyObserveAbsoluteWith`) belongs to step 340 by the spec's own split.

## Gotchas

- **Step 340 must call `AngleUncertainty.degree` with a `float<deg>`** (for example `0.2<deg>`), not a bare double.
- **Step 340's `misreadAbsolute` must read `(draws.nextDeviate ()).value`**, as the harness copy now does.
- **The step text's line numbers were stale.** It cited the banner at `InverseFitHarness.fs:59-110`, but at this
  step's start it was at 14–65. The deletion followed the content.
- **The harness still references the moved functions by name.** `misreadAbsolute`'s doc mentions "`misread`" and
  `noisyObserveAbsoluteWith`'s mentions "`noisyObserveWith`". Both now resolve to `BenchNoise`'s functions, through
  the harness's open.
- **`BuiltInCatalogueIntegrityTests` still says "step 339 owns its parity with the bench".** The new `ItemModelsTests`
  fact is that parity check. The integrity test is outside this step's effects, so the comment is unchanged.
- **`ItemModelsTests` opens `BenchNoise`.** That brings in the record labels `rotation`, `element` and `throughput`.
  No record in the file uses them, and the build is clean.

## Changelog

- 2026-11-12 — Step 0339: moved the bench noise model's data and transparent path from `InverseFitHarness` into
  `OpticalConstructor.Optimization.BenchNoise`.
  - Added `UniformDeviate`, `MuellerNormalizationErr`, the budgets' `parameterValue` and `NoiseParam.parameterAxes`.
  - `misread` and `noisyObserveWith` now answer `Result`.
  - `AngleUncertainty.degree` now takes `float<deg>` (ELEV010).
  - Added one constructor fact.
  - The numbers are bit-identical.
