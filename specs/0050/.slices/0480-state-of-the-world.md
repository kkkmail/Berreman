# Step 0480 — state of the world

## Where we are

Spec 0050 S14.2, the Measurement Planner. Steps 477 to 479 built the planner's numerical layer, and step 473 moved
spec 0044's quartz design into `BerremanTestSupport`. Step 481's acceptance facts run one noisy fit of each existing
design. This step therefore moves the noisy-experiment runners out of `MuellerInverseTests` and
`AbsorbingBiaxialNoisyTests` into the shared test support, as moves: one declaration of each name, and every number
unchanged.

## What's working

- Moved quartz's `solverParameters`, `forward`, `blindStart` and `wideBox` into `QuartzSample`, plus `scalingAround`
  renamed `quartzScalingAround`.
- Added `QuartzSample.noisyQuartzObservations` and `recoverQuartzFromNoisy`, the class's noisy runners, now answering
  `Result<_, MuellerFitError>`.
- Moved the absorbing suite's `forward` and `recoverFromNoisy` into `AbsorbingBiaxialSample` as `absorbingForward` and
  `recoverAbsorbingFromNoisy`, answering `Result`.
- `MuellerInverseTests` and `AbsorbingBiaxialNoisyTests` read the moved names and declare none. Their callers pipe
  through `expectOk`.
- Each moved doc was cut to one to three lines citing spec 0050 S14.2. All 21 facts pass unchanged.

## Tests

These are development runs; the arc-runner's gate engine is the authority. No run exceeded its effective deadline.

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, no `error`. The only warnings are the exempt `NU1902`s and the vendored MathNet's pre-existing `SYSLIB0051`s. | 84 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; 0 known, 1770 suppressed (unchanged) | 231 s | 600 s |
| unit-tests | `Passed!` 21 of 21 (`AbsorbingBiaxialNoisyTests`, `MuellerInverseTests`) | 1018 s | 7200 s |
| kit-acceptance | `Passed!` 14 succeeded, 1 skipped (`KitAcceptanceTests`) | 1418 s | 3600 s |
| invariant-s46-* (21) | each decoded `git grep` finds nothing | — | 300 s each |

- **Unit-test identities.** The 21 are exactly the step's `change_expectation` identities, all `Passed` in the TRX:
  20 in `MuellerInverseTests` (unchanged since 0473) and 1 in `AbsorbingBiaxialNoisyTests`.
- **Kit-acceptance.** The skip is AT-2's Explicit five-seed ensemble; the gate's skip policy is `allow`.
  - AT-2's grid-generator fact, which includes the fit through the shell, took 793 s, inside R0005's 1800 s bound.
- **Concurrency.** kit-acceptance and unit-tests ran at the same time, as at 0477–0479.
- **Rebuild.** A rebuild after two comment-only module-doc edits was green in 8 s. `BerremanTestSupport.dll` was
  rebuilt and its reference assembly was unchanged.

```yaml
gates:
  berreman_unit_tests: 21
  kit_acceptance_tests: 14
```

## Architecture

- **The runners answer the product's `Result`.** `BerremanTestSupport` references no test framework (step 344), so
  each runner composes `Result.bind` / `Result.map` over the product calls:
  - quartz: `noisyObserveWith` then `MuellerFitting.fitObservations`;
  - absorbing: `noisyObserveAbsoluteWith` then `fitAbsorbingObservations`.
  The tests unwrap with `InverseFitHarness.expectOk` (step 335).
- **The `MuellerFitData` record is passed through, not rebuilt.**
  - The class's quartz runner unpacked `scaling, residual, solution` and rebuilt `{ recovered = ofScaled scaling
    solution.solution; … }`. `fitWithResidual` builds exactly that record, so the numbers are bit-identical by
    construction.
  - The forward proxy is stateless (a fresh `OpticalSystemSolver` per call), so one module-level value replacing one
    per class instance changes nothing.
- **The quartz scaling is named `quartzScalingAround`.** No reader's `open QuartzSample` shadows
  `MuellerFitting.scalingAround` (step 336), matching `AbsorbingBiaxialSample.absorbingScalingAround`.
- **What stays in `MuellerInverseTests`:** its `fitObservations` wrapper, `runFit`, `observe`, `perturbedStart`,
  `nearBox` and `recoveredQuantities`, now over the moved `forward` and `quartzScalingAround`.

## Deferred

Nothing. Every requirement of the slice landed in this round.

## Gotchas

- **Namesakes outside this step's edits.** None opens `QuartzSample`, and all are untouched:
  - `BiaxialSample.solverParameters`;
  - `LangasiteInverseTests`' `solverParameters` / `forward`;
  - `BiaxialInverseTests`' `blindStart` / `wideBox`;
  - `AbsorbingBiaxialInverseTests`' `forward`;
  - `KitAcceptanceTests`' private `absorbingForward` (step 434's helper, explicitly left alone by the slice).
  `BerremanTestSupport` is outside the ELEV `inScopePatterns`, so the duplicate-name rules do not see them, as at 0473.
- **The `quartz` prefix is load-bearing.** Without it, every reader's `open QuartzSample` would shadow
  `MuellerFitting.scalingAround`.
- **A stale doc was corrected.** The `fitObservations` wrapper's doc said every fit, "noiseless or noisy", goes through
  it. The ensemble now goes through `recoverQuartzFromNoisy`, which runs the same fit, and the doc says so.

## Changelog

- 2026-11-18: step 0480. Moved the quartz and absorbing noisy-experiment runners into `QuartzSample` and
  `AbsorbingBiaxialSample`: seven names plus two, answering `Result`. The two classes read them and declare none. The
  21 facts are unchanged and pass.
