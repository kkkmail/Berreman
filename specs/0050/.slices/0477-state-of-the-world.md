# Step 0477 — state of the world

## Where we are

Spec 0050 section S14.2, Layer 2 of the Measurement Planner (001 §M): "the Fisher information from the Jacobians of
every candidate experiment …; rank; smallest singular vectors; Cramér–Rao bounds per target".

- Step 0476 made the study engine's covariance and offset-frame Jacobian callable (`FitQuality.covarianceOf`,
  `ScaledVector.offsetJacobianOf`).
- This step adds the numerical core that reads them. It stacks candidates' residual Jacobians scaled to one reference
  noise. It then answers:
  - the rank and the confounded and weakest combinations;
  - the condition number, the log pseudo-determinant and the trace;
  - the Cramér–Rao σ per target.
- Later steps feed it real candidate Jacobians and cache the Domain shapes (step 485).

## What's working

- Added `PlannerInformation.over`. It stacks each candidate's rows scaled by √(reference noise / its noise), refusing a noiseless or misshapen candidate whole.
- Answered rank, confounded and weakest combinations, condition, log determinant, trace and Cramér–Rao sigmas from `ResidualJacobian.rank` / `conditionNumber` and `FitQuality.covarianceOf`; no second inverse or rank floor.
- Added `NoiseVariance`, `CandidateRows` and the `NoiselessPlannerCandidateErr` / `PlannerShapeErr` cases to the planner's Domain.
- Added `ReducedChiSquared.noiseVariance`, reading a fit's residual level as a candidate's noise.
- Added `PlannerInformationTests`, four facts over literal Jacobians.

## Tests

Development runs only, all on the final build; the arc-runner's gate engine is the authority.

- **build:** exit 0, with the exempt `NU1902` advisories only. There is no FS or MSB warning.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 230 s (deadline 600 s).
  - ELEV004 is excluded, and the known findings are 0.
  - The first run failed on three ELEV010 findings, fixed as recorded under Gotchas.
- **constructor-unit-tests** (whole suite, `all_discovered`): `Passed!`, 1870 of 1870 in 120 s (deadline 180 s).
  - The count rises by 4 over checkpoint 1866.
  - All four new identities are `Passed`.
- **unit-tests**, selected `MuellerInverseTests` and `BiaxialInverseTests`: `Passed!`, 31 of 31 (20 + 11) in 1025 s
  (deadline 7200 s).
- **kit-acceptance**, selected `KitAcceptanceTests`: `Passed!`, 14 succeeded with 1 skipped (total 15) in 1420 s
  (deadline 3600 s).
  - The skipped fact is AT-2's Explicit five-seed ensemble; the gate's skip policy is `allow`.
  - Both committed-kit equality facts passed.
  - AT-2's fit through the shell took 793 s, inside R0005's 1800 s bound.
  - `data/` is untouched.
- **21 × invariant-s46-\*:** every decoded `git grep` finds nothing.
- Line endings are LF only (0 CR bytes in all seven touched files).

```yaml
gates:
  constructor_unit_tests: 1870
  berreman_unit_tests: 31
  kit_acceptance_tests: 14
```

## Architecture

- **One reference noise.** Every candidate's rows are multiplied by √(σ²_ref / σ²_i), with the first candidate's noise
  as the reference.
  - F = Σ J_iᵀJ_i / σ_i² is then exactly (stacked rowsᵀ · stacked rows) / σ²_ref.
  - The Cramér–Rao covariance is `FitQuality.covarianceOf rows (ReducedChiSquared σ²_ref)`: the covariance a fit of
    those candidates reports when its residual level is their noise.
  - With no candidate the reference is `NoiseVariance 1.0`.
- **Reuse, not copies.**
  - Rank: `ResidualJacobian.rank` (step 394's 1e-6 relative floor).
  - Condition: `ResidualJacobian.conditionNumber`.
  - Covariance: `FitQuality.covarianceOf` (step 476).
  - Direction weights: `FitUncertainty.ParameterWeights` (step 342).
  - The only new MathNet call is the full SVD in `SingularSpectrum.ofRows`, which the condition number and rank do not
    provide: they compute singular values only. It is marked `InteropBoundary` on its own declaration.
- **Spectrum shape.** MathNet's full SVD gives `VT` as n × n but `S` as min(m, n) values.
  - The values are padded with zeros to one per coordinate, so a wide Jacobian's null-space directions are the
    trailing `VT` rows, with singular value 0.
  - `confounded` is the directions from index `rank` on, and `weakestOpt` is the one at `rank − 1`.
- **Log pseudo-determinant.** It is Σ_{k<rank} 2 ln s_k − rank · ln σ²_ref, the log product of F's nonzero
  eigenvalues s² / σ²_ref.
- **Where things live.**
  - The Domain holds only the shapes the planner's cache will store (`NoiseVariance`, `CandidateRows`, the two error
    cases).
  - Every number is computed in `OpticalConstructor.Optimization`, which alone sees `FitQuality`.

## Deferred

Nothing. Every part (1), (2)(a)–(g) and all four facts landed.

## Gotchas

- **ELEV010 overrides three literal signatures of the slice.** The rule checks members, and its doc states no
  boundary mark exempts it. AGENTS.md rules that the analyzers win.
  - `SingularSpectrum.ofRows` takes `(jacobian : ResidualJacobian) (coordinates : ParameterName list)`, not
    `(rows : float[][]) (coordinateCount : int)`.
  - `SingularDirection.text` returns the new single-case `CombinationText` (with `.value`), not `string`. The rendering
    rule (`+0.71 a -0.71 b`) is the slice's.
  - The private range helper became the parameterless private `directions`.
- **No `ReducedChiSquared this.noise.value`.** That shape is ELEV001: the receiver is `this.noise`, not `this`. A
  `toReducedChiSquared` member beside `noiseVariance` would bridge a bijection. `scaledSigmasOpt` therefore
  destructures its own noise, as `FitQuality.covarianceOf` destructures its arguments.
- **A new `PlannerError` case breaks exhaustive test matches.** FS0025 is an error under `--warnaserror+:25`.
  - `PlannerInputsTests.fs` and `PlannerReportTests.fs` each gained the two cases on their existing or-pattern line,
    as at steps 470 and 471.
  - An or-pattern wrapped across lines inside `Error (…)` does not parse (FS0583).
- **Two totalizations not in the slice.**
  - `boundsOver` answers `None` when the scaling's axes are not one per coordinate.
  - `conditionOpt` answers `None` with no coordinate, and `ofRows` takes its identity branch there, so MathNet never
    sees a column-less matrix.
- **Singular-vector signs are arbitrary.** Readers of `confounded` / `weakestOpt` must not depend on a sign. The
  confounded-pair fact accepts both, as the slice states.

## Changelog

- 2026-11-17 — Step 0477: Layer 2's numerical core (S142-INFORMATION). Added `PlannerInformation` (Fisher
  information over stacked, noise-scaled candidate Jacobians: rank, confounded and weakest directions, condition, log
  determinant, trace, Cramér–Rao bounds), `NoiseVariance`, `CandidateRows`, two `PlannerError` cases,
  `ReducedChiSquared.noiseVariance` and four constructor facts.
