# Step 0479 — state of the world

## Where we are

Spec 0050 S14.2, the Measurement Planner's numerical layer. Steps 477 and 478 gave `PlannerInformation.fs` the Fisher
information of stacked candidate rows and its greedy D / A-optimal ranking, over rows written as literals. This step
derives a candidate's rows from its measurement configurations. It uses the study engine's own forward and throughput
residual, with its noise from the detector's bench budget (001 §M, §Q). The Plan view (step 491) consumes it later.

## What's working

- Added `ParameterValue.angleUncertainty` / `elementUncertainty` / `throughputUncertainty`, converting a parameter
  number to each promoted uncertainty quantity.
- Added `ParameterValues.benchNoise`, a detector's noise budget from its three noise values:
  - It refuses a missing, non-finite or negative value, and an element or throughput above one, through
    `ItemEvaluationError`.
  - It ignores other entries.
- Added `NoiseParam.noiseVarianceOver`, the mean expected squared error of a candidate's throughput-residual entries:
  - each reading's uniform variance u² / 3;
  - the rotation budget turned through the entry's offset-frame sensitivity to incidence and azimuth.
- Added `PlannerInformation.candidateRowsOver`: the offset-frame Jacobian of the noiseless throughput residual at the
  nominal point, in the scaling's units, with that noise.
- Covered both with the `PlannerInformationTests` facts on the noise budget's variance and on a candidate's rows
  (constructor suite 1873 → 1875).

## Tests

These are development runs; the arc-runner's gate engine is the authority. No run exceeded its effective deadline.

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, no `error`; the only warnings are the exempt `NU1902`s and the vendored MathNet's pre-existing 4 `SYSLIB0051`s | 86 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; 0 known, 1770 suppressed (unchanged) | 238 s | 600 s |
| constructor-unit-tests | `Passed!` 1875 of 1875 (whole suite, `all_discovered`) | 115 s | 180 s |
| kit-acceptance | `Passed!` 14 succeeded, 1 skipped (`KitAcceptanceTests`) | 1460 s | 3600 s |
| unit-tests | `Passed!` 31 of 31 (`MuellerInverseTests`, `BiaxialInverseTests`) | 1374 s | 7200 s |
| invariant-s46-* (21) | each decoded `git grep` finds nothing | — | 300 s each |

- The constructor suite rose by 2 over 0478's 1873. Both census identities are `Passed` in the TRX, named exactly as the
  census names them.
- The kit-acceptance skip is AT-2's Explicit five-seed ensemble; the gate's skip policy is `allow`.
- AT-2's grid-generator fact, which includes the fit through the shell, took 809 s, inside R0005's 1800 s bound.
- kit-acceptance and unit-tests ran at the same time, as at 0477 and 0478, which explains unit-tests' longer wall time
  than 0478's 1103 s. The constructor suite ran beside the elevation run.

```yaml
gates:
  constructor_unit_tests: 1875
  berreman_unit_tests: 31
  kit_acceptance_tests: 14
```

## Architecture

- **`benchNoise`** is an extension of Domain's `ParameterValues` declared in `BenchNoise.fs`, after `NoiseParam`.
  - It reuses `checkedFor` over the budget's own noise axes (`NoiseParam.noiseless.parameterAxes`, read for names and
    kinds only) and adds the rotation's sign check.
  - It converts through the three new `ParameterValue` members.
  - It is a one-way conversion from a general parameter map to the numerical budget. There is no reverse, and no second
    noise record.
- **`noiseVarianceOver`** is an optional extension of `NoiseParam` in `PlannerInformation.fs`.
  - Its angle sensitivity is the two-column `(ScaledVector [| 0; 0 |]).offsetJacobianOf` of a total closure.
  - The closure takes `observeWith` through a forward turned by `misalignedBy` and files the readings against the nominal
    configurations. It answers `throughputResidualFor` at the nominal point (`scalingAround []`, `ScaledVector [||]`).
  - A forward error answers the full-width 1.0e3 penalty `residualFor` uses, held as a private `[<Literal>]` because
    `MuellerFitting` does not export it.
- **`candidateRowsOver`** is a generic module function. It adds no cache, no seed and no option.

## Deferred

Nothing. Every requirement of the slice landed in this round.

## Gotchas

- **Tolerance deviation.** The rows fact holds the two non-zero Jacobian entries to 1e-7 relative, not the slice's 1e-9.
  - The offset frame's step (`√ε · 100` scaled units, 1.49e-9 of an index at the 1e-3 scale) is resolved by an index
    near 1.5 to 7.45e-8 relative at worst.
  - `specs/0050/.artifacts/0479-scratch/fd-check.fsx` measures 5.96e-8 at both 1.544 and 1.553.
  - Every other comparison keeps the slice's numbers (1e-12 absolute, 1e-9 relative).
- **Centre the scaling at the nominal point.** A scaling centred away from the nominal starts the offset frame far from
  zero and loses a further ~1.5e-7 of the step.
- **No configurations gives `NoiseVariance 0.0`** (the mean over no entries), not NaN. `PlannerInformation.over` refuses
  it as noiseless.
- **`candidateRowsOver` takes `observeWith` twice.** Once for its Jacobian and once inside `noiseVarianceOver`, m
  forward solves each, small beside the Jacobian's (k + 1)·m.
- **The 1.0e3 penalty lives in two files.** `MuellerFitting.fs` holds it inline, outside this step's edits, and
  `PlannerInformation.fs` names it `unsolvedPenalty`. Changing one means changing the other.

## Changelog

- 2026-11-17: step 0479. Added `ParameterValue`'s three uncertainty conversions, `ParameterValues.benchNoise`,
  `NoiseParam.noiseVarianceOver` and `PlannerInformation.candidateRowsOver`, with two `PlannerInformationTests` facts
  (constructor suite 1873 → 1875).
