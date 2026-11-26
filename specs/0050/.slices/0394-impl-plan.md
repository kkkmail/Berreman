# Step 0394 — impl plan

## Approach

S11.4's fit results report each solve's residual-Jacobian rank, and the linear solves read their residual row by row.

1. `MuellerInverse.fs`: `ResidualJacobian` gains `member this.rank : MatrixRank`, directly after `conditionNumber`.
   - It goes through the same `RealMatrix.create` + `Svd(false)` seam.
   - It counts the singular values above `1.0e-6 ×` the largest.
   - It is `MatrixRank 0` for an empty Jacobian, or when the largest singular value is not above zero.
   - A three-line doc cites S11.4 and says why the floor is relative and where it sits (forward-difference noise
     at ALGLIB's step).
2. `MuellerReconstruction.fs`: `LeastSquaresSystem` gains `member this.residualsAt (solution) : RealVector`
   (`A·x − b`), directly before `residualRmseAt`.
   - `residualRmseAt` becomes `(this.residualsAt solution).norm / sqrt m`.
   - These are the same operations in the same order, so every RMSE stays bit-identical.
3. New `OpticalConstructor.Tests/FitResultsTests.fs` (`module FitResultsTests`), appended last in the test
   project's compile list with no comment. It holds the two facts the brief words.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerInverse.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitResultsTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **The rank test's near-confounded case.** The columns agree to a part in 1e9. The second singular value is
  about 1e-9 of the first, far below the 1e-6 floor. The `[1; 0], [0; 1e-4]` case sits at 1e-4, far above it.
  Neither case is borderline.
- **Bit-identity of the RMSE.** The refactor only names the intermediate vector. No operation is reordered, so
  `MuellerReconstructionTests` keeps its numbers.
- **ELEV.**
  - The new members are members, so they are exempt from the primitive-signature rules.
  - `MatrixRank` is built by a pipe at its origin (`|> MatrixRank`), as `conditionNumber` builds its value. There is
    no call-site re-wrap.
