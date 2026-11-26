# Step 0342 — impl plan

Obligation S101-COVARIANCE-MOVE. A fit's own account of its uncertainty moves out of
`BerremanTests/InverseFitHarness.fs` into `OpticalConstructor.Optimization`, so the Planner's layer 2 can reuse
it (001 §M, §Q AT-2). It is a MOVE: after the step, each moved name is declared exactly once, in the product.

## Approach

1. **`MuellerFitData.report`.** `fitReportOf` becomes `member this.report : FitQuality.FitReport` on `MuellerFitData`
   in `MuellerFitting.fs` (ELEV013), with the same body: `FitQuality.reportFrom` over the residual, the solution as a
   `DesignVector`, and the final residuals.
2. **`MuellerFitError`** gains `UnknownAxisErr of ParameterName` (no fitted parameter of that name).
3. **New `FitUncertainty.fs`** (`module FitUncertainty` in `namespace OpticalConstructor.Optimization`), compiled
   directly after `RecoveryEnsembles.fs` with one `<Compile>` line and no comment. It declares:
   - `ParameterWeights` (a `[<NumericCarrier>]` over `float[]`) and `PredictedVariance` (over `double`), each with
     a `value` member;
   - `predictedRelativeErrors`, moved with its signature and body;
   - `combinationVariance : FitReport -> ParameterWeights -> PredictedVariance`, the same `wᵀCw` double sum in the
     same order;
   - `differenceWeights : ParameterScaling<'P> -> ParameterName -> ParameterName -> Result<ParameterWeights, MuellerFitError>`,
     built by `Array.init` without mutation: 1.0 at `plus`, −1.0 at `minus` (−1.0 wins when they coincide, as
     today), 0.0 elsewhere. An unknown name is `UnknownAxisErr`.
4. **`InverseFitHarness.fs`** deletes the four declarations.
5. **Callers.**
   - `fitReportOf first.fit` becomes `first.fit.report` in `MuellerInverseTests` and `BiaxialInverseTests`.
   - The ensemble fact's `pairVariance` becomes
     `combinationVariance quality (differenceWeights first.fit.scaling (ParameterName "n_e") (ParameterName "n_o") |> expectOk)`,
     read as `pairVariance.value`.
   - Both files open `OpticalConstructor.Optimization.FitUncertainty`.

## Files

- `OpticalConstructor.Optimization/OpticalConstructor.Optimization.fsproj`
- `OpticalConstructor.Optimization/FitUncertainty.fs` (new)
- `OpticalConstructor.Optimization/MuellerFitting.fs`
- `BerremanTests/InverseFitHarness.fs`
- `BerremanTests/MuellerInverseTests.fs`
- `BerremanTests/BiaxialInverseTests.fs`

## Risks

- **Bit-identical numbers.** The report, the conversion out of the scaled space and the quadratic form keep their
  operations and order exactly as they are.
- **ELEV.** The moved code is now inside the analyzer's scope.
  - `report` reuses the `DesignVector` wrap already in `residualJacobianAt`, which is ELEV-clean.
  - The new type, case and function names were checked to be unique.
- **Test count.** No test is added or removed, so the count stays the same.
