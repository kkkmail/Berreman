# Step 0398 — state of the world

## Where we are

Spec 0050, section S11.4: fit results (uncertainty, correlation, realizability, product check, profile likelihood).

- Step 397 added `FittedProblem`, the one record every engine's σ, correlation and χ² profiles are read from.
- This step writes stage one's two AIR cosine models in the instruments' own constants, as residuals over an AIR trace,
  and makes the trace builder public:
  - the free cosine fit in its zero and visibility;
  - the LP-frame fit in the CPL source's θ and δ.
- Step 401 builds calibration `FittedProblem`s from these residuals, so each calibrated value gets its σ like any other
  fitted parameter.

## What's working

- Made `StagedMuellerPath.reduceAirTraces` public, with its body unchanged and its doc citing S11.4.
- Added `AirTrace.fringeResidualAt zero visibility`, which is `freeCosineFit`'s residual in the zero and visibility it
  reports.
- Added `SourceCplModel.lpFrameResidualOn trace lpZero`, which is the LP-frame fit's residual in the source model that
  `sourceCplFromLpFrame` reads back.
- Added three `StagedMuellerPathTests` facts:
  - the fringe residual is the fit's own residual, with the same RMSE;
  - the LP-frame residual vanishes on the read-back model;
  - two experiments' trace is each experiment's trace in turn.

## Tests

Run locally on 2026-11-14 as development runs; the supervisor's gate engine is authoritative. Every run finished within
its effective deadline.

- `build`: exit 0, 0 errors. The first build took 81 s, and the final incremental one 9 s (deadline 600 s).
  - The warnings are only the exempt NU1902 advisories and the vendored MathNet `SYSLIB0051` pair.
- `constructor-unit-tests`, whole suite: 1708/1708, 117 s then 116 s on the final binaries (deadline 180 s). Nothing
  else ran at the same time.
  - The count is 1705 → 1708, +3, as the acceptance requires.
  - The TRX shows all three new IDs `Passed`.
- `unit-tests`, selected `BerremanTests.MuellerReconstructionTests`: 25/25, 3 s, then 2.5 s on the final binaries
  (deadline 7200 s).
  - Nothing it calls changed: `reduceAirTraces`' body is byte-identical, and the members are additions.
  - Its numbers are therefore unchanged.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 227 s, then 225 s on the final tree (deadline
  600 s).
- The twenty `invariant-s39-*` greps all find nothing.
- Mutation checks over one test-project build. The file was restored byte for byte (verified with `cmp`), and the
  final build recompiled it.
  - Phasing the fringe on α + z fails the fringe fact.
  - Negating the u term fails the LP-frame fact.
  - Pooling every experiment's rows fails the two-experiments fact.
  - The class's ten other facts still passed.

```yaml
gates:
  constructor_unit_tests: 1708
  berreman_unit_tests: 25
```

## Architecture

- **A reparameterisation, not a second fit.** Both members restate an existing two-coefficient linear model in physical
  constants:
  - the free fit's p = V cos 2z and q = V sin 2z;
  - the LP fit's 1 − q = (1 − cos δ) sin² 2θ and u = (1 − cos δ) sin 2θ cos 2θ.

  Each map is one-to-one on the physical branch, so the minimum and the residual are the linear fit's. The covariance
  `FitQuality` gives in these constants is the propagated one, with no second formula.
- **One phase convention.** Both members form 2(αᵢ − z) in degrees, as `2.0 * (a - zero.degrees) *
  Angle.radiansPerDegree`. That is exactly how `lpFrameSourceModel` phases its design, and how `freeCosineFit` reads
  α.
- **The members are optional extensions in `StagedMuellerPath`.** `AirTrace` and `SourceCplModel` live in Domain's
  `MuellerReconstruction`, and their residuals live with the stage-one path that fits them. They are visible wherever
  `StagedMuellerPath` is opened.
- **`ResidualVector` comes from `MuellerInverse`.** `StagedMuellerPath.fs` now opens
  `OpticalConstructor.Domain.MuellerInverse`, as `FitQuality.fs` does.

## Deferred

Nothing. Every requirement of the step landed this round. Building the calibration `FittedProblem`s (401), the suspects
(402), the profile pass (403) and storage (404) are separate steps of the spec.

## Gotchas

- **The physical branch is 0° < θ < 90°.** It is named in the doc, not enforced. `lpFrameResidualOn` stays total off
  the branch, but `sourceCplFromLpFrame` reads such a model back as a different one.
  - At δ = 0 both coefficients collapse: q = 1, u = 0, and θ is unobservable.
  - A profile or σ over θ near 0° or 90° should expect a singular Jacobian there.
- **`calibrateStageOne` still filters `(AIR#0)` labels.** "Its AIR-label filter gone since step 388" refers to
  `reduceAirTraces`. The file-level selection in `calibrateStageOne` is untouched.
- **`reduceAirTraces` groups by label in first-appearance order.** Its output for interleaved rows follows the order in
  which each label first appears, not alphabetical order.
- **`MuellerReconstructionTests` alone takes about 3 s.** The long selected runs of earlier steps were
  `MuellerInverseTests`.

## Changelog

- 2026-11-14 — Step 0398: `reduceAirTraces` public; `AirTrace.fringeResidualAt` and
  `SourceCplModel.lpFrameResidualOn` added in `StagedMuellerPath.fs`; three `StagedMuellerPathTests` facts added.
