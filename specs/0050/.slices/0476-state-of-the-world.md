# Step 0476 — state of the world

## Where we are

Spec 0050 section S14.2, Layer 2. It needs the study engine's own forward model and algebra, reused and not copied
(S14.2; 001 §M). This step makes the four rules Layer 2 needs callable where they live. Each is a move, so after the
step each rule has one body and every number BerremanTests and the study engine compute is unchanged. Steps 397 (the
fitted problem), 339 (the bench noise), 387 (the scaled study problem) and 214 (`ParameterInterval`) built the code
that was moved.

## What's working

- Made `FitQuality.covarianceOf` public: Cov = reducedχ² · (JᵀJ)⁻¹, the former private `covarianceMatrix`, body unchanged. `reportFrom` calls it.
- Moved the fitted problem's offset frame into `ScaledVector.offsetResidualOf`, `offsetOrigin` and `offsetJacobianOf`. `FittedProblem.jacobian` and `report` call them inside their caches, which still evaluate each once per problem (R0004).
- Moved `disturb`'s angle shift into `MeasurementConfiguration.misalignedBy`. `disturb` is `c.misalignedBy (wobble ()) (wobble ())`, with the incidence still drawn first.
- Moved the study scaling's per-entry unit into `ParameterInterval.searchUnit`, which `scaledProblemWith` reads.
- Added two facts: the offset frame's Jacobian and `covarianceOf` (`FitResultsTests`), and the search unit (`StudyEngineTests`).

## Tests

Development runs only; the arc-runner's gate engine is the authority.

- **build:** exit 0 in 83 s (deadline 600 s).
  - The only non-NuGet warnings are the vendored-MathNet `SYSLIB0051` ones, which were already there.
  - No FS or MSB warning.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)` in 283 s (deadline 600 s).
  - ELEV004 is excluded and the known findings are 0.
  - The suppressed count is 1767, the same as step 0475.
- **constructor-unit-tests** (whole suite, the gate's `all_discovered`): `Passed!`, 1866 of 1866 in 113 s
  (deadline 180 s).
  - The count rises by 2 over checkpoint 471's 1864.
  - Both new identities are `Passed` in the TRX.
- **unit-tests, selected `BerremanTests.MuellerInverseTests`:** `Passed!`, 20 of 20 in 408 s (deadline 7200 s).
  - The count is unchanged at 20.
  - This run overlapped the elevation and kit-acceptance runs.
- **kit-acceptance, selected `OpticalConstructor.Ui.Tests.KitAcceptanceTests`:** `Passed!`, 14 succeeded with 1 skipped
  (total 15) in 1453 s (deadline 3600 s).
  - The skipped fact is AT-2's five-seed ensemble, which is xUnit v3 Explicit; the gate's skip policy is `allow`.
  - The count is unchanged at 14.
  - `the built optics-mueller kit equals the committed kit file by file` passed, and so did its absorbing-biaxial-noisy
    twin, so the fit, noise and study numbers are bit-identical.
  - AT-2's grid-generator fact, which includes AT-2's fit through the shell, took 814 s, inside R0005's 1800 s bound.
  - The read-only reopen fact took 563 s.
  - `data/` is untouched.
- **21 × invariant-s46-\*:** every decoded `git grep` finds nothing.
- Line endings are LF only (0 CR bytes in all six files).

```yaml
gates:
  constructor_unit_tests: 1866
  berreman_unit_tests: 20
  kit_acceptance_tests: 14
```

## Architecture

- **Where each rule lives.** Each one is a member on the type it is a fact about, so it is not a free function (ELEV013).
  - The offset frame and the search unit live in the files that use them.
  - `misalignedBy` lives beside `disturb`, in the `type X with` form these files already use (`IterationWatch`,
    `SearchBox`, `StagedStudy`).
- **The offset is a private literal.** The 100 is `[<Literal>] frameOffset : float`, shared by `offsetResidualOf` and
  `offsetOrigin`, so the frame's point and its origin cannot drift apart.
- **R0004's single evaluation stays with the caches.** It is kept by `FittedProblemEvaluations`' `ConditionalWeakTable`s.
  The moved members are pure, so Layer 2 can call them on any point.
- **The same doubles flow.**
  - `solved + (moved - 100.0)` per coordinate is unchanged.
  - `report` still passes `problem.residual start` as the final residuals.
  - `searchUnit` computes the same quotient.
  - `misalignedBy` makes the same record update.

## Deferred

Nothing. All four moves, both facts and every caller landed.

## Gotchas

- **`disturb` is not where the slice says.** The slice cites `InverseFitHarness.fs:disturb`, but `disturb` lives in
  `OpticalConstructor.Optimization/BenchNoise.fs` (step 339). The move was made there, in the file the effects authorize.
- **The draw order now rests on argument order.** `disturb` once relied on the record-field order and now relies on
  F#'s left-to-right evaluation of curried arguments, so the incidence comes first. Swapping the two `wobble ()`
  arguments, or binding them in the other order, would change every seeded experiment.
- **Domain has its own `(JᵀJ)⁻¹`.** `MuellerInverse.fs` (Domain) keeps its own inverse in its `ResidualJacobian`
  diagnostics. That file is a lower assembly outside this step's `touches`, and the slice names only `covarianceMatrix`,
  so it was not changed.
- **The test reaches `FitQuality` through the namespace.** `FitResultsTests` opens `OpticalConstructor.Optimization`,
  as `OptimizationInterfaceTests.fs` does, and writes `FitQuality.covarianceOf` and `FitQuality.ReducedChiSquared`
  qualified.

## Changelog

- 2026-11-17 — Step 0476: moved four rules to where Layer 2 can call them, each keeping one body (S142-REUSE-SEAMS):
  `FitQuality.covarianceOf`, `ScaledVector.offsetResidualOf` / `offsetOrigin` / `offsetJacobianOf`,
  `MeasurementConfiguration.misalignedBy` and `ParameterInterval.searchUnit`. Added two constructor facts.
