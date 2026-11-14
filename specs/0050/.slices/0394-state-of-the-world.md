# Step 0394 — state of the world

## Where we are

Spec 0050, section S11.4: fit results (uncertainty, correlation, realizability, product check, profile likelihood).

- Steps 386–393 built the fit and hosted it as a job.
- This step opens S11.4 with the result shapes it reports (001 §L), where the inputs are in place:
  - `ResidualJacobian` already had the condition, the column norms and the correlation. It now reports its numerical
    rank too.
  - A linear solve's system now reads its solution's residuals row by row, as well as their RMSE.
- `FitResultsTests` is the new home of S11.4's facts over the result shapes and synthetic least-squares problems.

## What's working

- Added `ResidualJacobian.rank`: the count of singular values above 1e-6 of the largest, through the same SVD seam as
  `conditionNumber`. It is 0 for an empty Jacobian.
- Added `LeastSquaresSystem.residualsAt`, which gives `A·x − b`. `residualRmseAt` now takes its norm over √m. The
  operations and their order are unchanged, so every reported RMSE is the same doubles.
- Added `FitResultsTests`, with two facts: the rank over independent, proportional, noise-proportional, weak and empty
  Jacobians, and the residuals and RMSE of a three-row system.

## Tests

Run locally on 2026-11-14 on the final tree. Every run finished within its effective deadline.

- `build`: exit 0, 0 errors, 18 s incremental (the first build took 81 s; the deadline is 600 s).
  - The warnings are the exempt NU1902 advisories, plus the vendored MathNet `SYSLIB0051` pair, which appears only
    when MathNet recompiles.
- `constructor-unit-tests`, whole suite: 1698/1698, 114 s (deadline 180 s).
  - The count is 1696 → 1698, +2, as the acceptance requires.
  - The TRX shows each of these `Passed`:
    - `OpticalConstructor.Tests.FitResultsTests.a Jacobian's rank counts the directions its data constrains, above
      forward-difference noise`
    - `OpticalConstructor.Tests.FitResultsTests.the residuals of a solution over a least-squares system are its design
      times the solution less its right-hand side`
- `unit-tests` (`FeasibleBiaxialInverseTests`, `MuellerReconstructionTests`): 30/30 (5 + 25), 2 min 52 s (deadline
  7200 s). The classes are unchanged and their numbers unchanged.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 229 s (deadline 600 s), with 1579 suppressed and 0
  known.
- The twenty `invariant-s39-*` greps all find nothing.
- Mutation checks:
  - Lowering the rank floor to 1e-12 fails only the rank fact.
  - Flipping the residual's sign fails only the residuals fact.
  - The source was restored byte for byte and rebuilt.

```yaml
gates:
  constructor_unit_tests: 1698
  berreman_unit_tests: 30
```

## Architecture

- **The rank is a property of `J` alone**, beside the condition number and the correlation. Like them, it survives a
  noiseless fit's vanishing χ².
- **The floor is relative (1e-6 of the largest singular value), so the rank is unit-free.** It sits two orders above
  the noise of a forward-difference Jacobian at ALGLIB's step:
  - a pair of columns confounded up to that noise counts once;
  - a direction 1e4 times weaker than the strongest still counts.
- **An `m × 0` Jacobian (no free parameter) is "empty" too**, with rank 0. This keeps the member total, because
  `RealMatrix.create` cannot build a matrix from zero-length rows.
- **`residualsAt` is the one place `A·x − b` is formed.** `residualRmseAt`, and through it `createMathNetSvd` and the
  product check, read it.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The rank floor differs from MathNet's own rank.** `createMathNetSvd`'s `svd.Rank` uses MathNet's own
  machine-epsilon tolerance, while `ResidualJacobian.rank` uses the 1e-6 relative floor.
  - So the two ranks can differ on a nearly confounded design.
  - This is by design: a Jacobian taken by forward differences carries ~1e-8 noise that the design matrix of a linear
    solve does not.
- **The `berreman_unit_tests` count is this step's selection, not a suite total.** It is 30 here, against 45 over step
  393's different class pair. The checkpoint ledger records no floor for it.

## Changelog

- 2026-11-14 — Step 0394: the residual Jacobian's rank and a least-squares system's residuals.
  - Added `ResidualJacobian.rank` (the singular values above 1e-6 of the largest).
  - Added `LeastSquaresSystem.residualsAt` (A·x − b), which `residualRmseAt` now reads.
  - Added two `FitResultsTests` facts.
