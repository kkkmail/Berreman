# Step 0337 — state of the world

## Where we are

Spec 0050 section S10.1 moves the bench noise model and the fit harness into Optimization.

- Steps 335 and 336 gave `OpticalConstructor.Optimization/MuellerFitting.fs` its observation generator, residual
  closures, Jacobian adapter and `scalingAround`.
- This step moves the fit itself, after `scalingAround`:
  - the search box `SearchBox`;
  - the fit record, renamed from the harness's `InverseFit` to `MuellerFitData<'Parameters>`;
  - the three runners `fitWithResidual`, `fitObservations` and `fitAbsorbingObservations`.
- Errors are now values: the harness threw on a solver failure, and the product answers `Error (NonlinearFitErr e)`.
- Every inverse suite now fits through the product and unwraps with `expectOk`.
- Steps 341 and 342 move `NoisyExperiment.fit`, `fitReportOf` and `predictedRelativeErrors`, which already take
  `MuellerFitData`.

## What's working

- Moved `SearchBox`, the fit record (renamed `MuellerFitData<'Parameters>`) and `fitWithResidual`, `fitObservations`
  and `fitAbsorbingObservations` from `InverseFitHarness` into `OpticalConstructor.Optimization.MuellerFitting`.
- The three runners answer `Result<MuellerFitData<'Parameters>, MuellerFitError>`. A solver failure is the new case
  `NonlinearFitErr`, no longer a throw.
- Every inverse suite fits through the product, unwrapping with `expectOk`.
- `MuellerInverseTests`' comments now point at `MuellerFitting` for the fit driver.
- Numbers and counts are unchanged: the Feasible and Absorbing inverse classes ran 12 of 12, and the constructor suite
  1552 of 1552.

## Tests

These are development runs of each gate's own command from `0337.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; 0 warnings from our code | 75 s (first), 11 s (final) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 209 s | 600 s |
| constructor-unit-tests (all) | Passed!, 1552 of 1552, 0 failed, 0 skipped | 77 s | 180 s |
| unit-tests (`AbsorbingBiaxialInverseTests`, `FeasibleBiaxialInverseTests`) | Passed!, 12 of 12, 0 failed, 0 skipped | 270 s (final), 290 s (first) | 7200 s |
| invariant-s33-\* (19) | each `INVARIANT HOLDS`, exit 0 | under 1 s each | 300 s |

- **build.**
  - The first build compiled everything, the vendored MathNet included. The only warnings were MathNet's two known
    `SYSLIB0051` warnings in `MathNetNumerics/Numerics/Exceptions.cs`; this step does not touch that code.
  - The final build after a comment reflow in `MuellerInverseTests.fs` recompiled `BerremanTests` with 0 warnings and
    0 errors.
- **Counts.**
  - constructor-unit-tests counts 1552, the same as step 0336.
  - The TRX splits the selected classes into 5 `FeasibleBiaxialInverseTests` and 7 `AbsorbingBiaxialInverseTests`,
    the same as step 0336.
  - Both tests named in the step's effects ran and passed:
    - `the feasible measurement set recovers all NINE constants from a perturbed start`;
    - `the absorbing measurement set recovers all FIFTEEN material constants from a perturbed start`.
  - No test was added, removed or renamed, and no expected value or tolerance changed.
- **unit-tests ran twice.**
  - The first run (4 min 50 s, 12 of 12) used the first build's binary.
  - A comment in `MuellerInverseTests.fs` was then reflowed. The selected classes were re-run over the final binary:
    4 min 30 s, 12 of 12 (7 Absorbing, 5 Feasible), 0 failed, 0 skipped.
- **Elevation** had no comparison base, so it covered all 12 projects.
  - `OpticalConstructor.Optimization` reports 0 findings and 2 suppressed. Both are ELEV004 (excluded by the gate
    command), at the `.value` reads in the moved `chiSquared` / `rmsResidual` members (`MuellerFitting.fs:134`,
    `:138`). The step requires their bodies unchanged.
  - The tree-wide suppressed total is 1477, up from 1475; the difference is those two.
- **One declaration each.**
  - A grep over `Berreman/` for `type|let (SearchBox|MuellerFitData|InverseFit|fitWithResidual|fitObservations|fitAbsorbingObservations)`
    finds the five product declarations in `MuellerFitting.fs`.
  - `InverseFitHarness.fs` declares none of them, and no type named `InverseFit`.
  - The one other hit is `MuellerInverseTests`' class-local `fitObservations` wrapper. The step keeps it, and it calls
    the product.
- **LF.** No edited file has a CR byte, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- No run exceeded its effective deadline. Elevation, unit-tests and constructor-unit-tests ran concurrently.

```yaml
gates:
  berreman_unit_tests: 12
  constructor_unit_tests: 1552
```

## Architecture

- **`MuellerFitting` now owns the fit.** The product module runs the whole path from observations to a
  `MuellerFitData`: residual closure, scaled box, ALGLIB Levenberg–Marquardt and the recovered constants. The study
  engine (S11) and the Planner can call it directly. The test harness keeps only the noise model, the ensemble
  statistics and the assertions.
- **The fit record is `MuellerFitData`, not `InverseFit`.** `InverseFit` is already the product module
  `OpticalConstructor.Optimization.InverseFit`, and names are unique, not qualified (ELEV014 / ELEV015).
- **Errors are values.** `MuellerFitError` gains `NonlinearFitErr of NonlinearSolverError`. The product never throws
  across its boundary, and the tests choose to fail with `expectOk`.
- **Bit-identical by construction.** The request, the solver and the record are the harness's, unchanged. `Ok` wraps
  exactly what the harness returned.

## Deferred

Nothing. Every requirement of the slice landed. The slice assigns this to later steps: steps 341 and 342 move
`NoisyExperiment.fit`, `fitReportOf` and `predictedRelativeErrors`.

## Gotchas

- **`AbsorbingBiaxialNoisyTests` did not open `MuellerFitting`.** It is the one inverse suite that needed the open
  added for `fitAbsorbingObservations`. It sits before the `BerremanTests.*` opens, so the sample modules still
  shadow the product's generic names.
- **`MuellerFitData` and `NonlinearRequest` share the `residual` label.** F# resolves the request literal by its other
  labels, as it did beside `InverseFit` in the harness.
- **The vendored MathNet warnings.** A build that recompiles MathNet prints two `SYSLIB0051` warnings from
  `MathNetNumerics/Numerics/Exceptions.cs`. They are pre-existing and outside this step.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0337: moved `SearchBox`, the fit record (renamed `MuellerFitData`) and `fitWithResidual`,
  `fitObservations` and `fitAbsorbingObservations` from `InverseFitHarness` into
  `OpticalConstructor.Optimization.MuellerFitting`.
  - The runners answer `Result<_, MuellerFitError>`, with the new case `NonlinearFitErr`.
  - Callers unwrap with `expectOk`.
  - Numbers and counts are unchanged.
