# 0336 — impl plan

## Approach

A move (spec 0050 S10.1, 001 §Q AT-2): `residualFor`, `throughputResidualFor`, `residualJacobianAt` and
`scalingAround` leave `BerremanTests/InverseFitHarness.fs` and land in
`OpticalConstructor.Optimization/MuellerFitting.fs`, after `observeWith`, with signatures and bodies unchanged.

1. Append the four to `MuellerFitting` after `observeWith`. Their bodies need `forwardModels`, `ofScaled`,
   `residualVector` (`OpticalConstructor.Domain.MuellerInverse`, already opened), `MuellerMatrix` (`Berreman.Fields`,
   opened), and `FitQuality` / `OptimizationInterface` (siblings in `OpticalConstructor.Optimization`, the namespace
   `MuellerFitting` lives in). No new open should be needed.
2. Cut each doc to its contract in one to three lines; the throughput closure keeps one line saying the log term
   restores the isotropic absorption normalization discards (specs/0044/.manual/018); `residualFor` keeps the total,
   fixed-length hazard line.
3. Delete the four from `InverseFitHarness.fs`. `fitObservations` / `fitAbsorbingObservations` already open
   `MuellerFitting`, so they resolve the product closures with no edit.
4. Re-word the shadowing comment above `MuellerInverseTests.scalingAround` to name `MuellerFitting.scalingAround`.
5. Every consumer (`MuellerInverseTests`, `BiaxialInverseTests`, `FeasibleBiaxialInverseTests`,
   `AbsorbingBiaxialInverseTests`) already opens `OpticalConstructor.Optimization.MuellerFitting` (step 335), so no
   open changes; the build confirms it.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`
- `Berreman/BerremanTests/InverseFitHarness.fs`
- `Berreman/BerremanTests/MuellerInverseTests.fs` (comment only)

## Risks

- **Shadowing order.** Test files open `MuellerFitting` before `InverseFitHarness` and `BiaxialSample`; with the
  harness copy gone, `scalingAround` must still resolve to `BiaxialSample.scalingAround` (opened last) in the biaxial
  suites and to the class-local one in `MuellerInverseTests`. Open order is unchanged, so it does.
- **ELEV on product code.** The bodies now sit in an analyzed project. The same idioms
  (`request.residual (ScaledVector x)`, `let (DesignVector x) = …`) already pass in `MuellerInverseSolver.fs` and
  `FitQuality.fs`; the elevation run confirms.
- **Bit-identity.** Bodies unchanged, so the numbers are unchanged.
