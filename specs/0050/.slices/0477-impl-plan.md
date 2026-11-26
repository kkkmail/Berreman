# Step 0477 — impl plan

Layer 2's numerical core (spec 0050 S14.2; 001 §M): the Fisher information of candidate experiments at a nominal
point, its rank, its confounded and weakest combinations, its condition, log determinant, trace and the Cramér–Rao
bound per target. Every number comes from code that already exists:

- the rank is `ResidualJacobian.rank` (step 394's floor);
- the condition is `ResidualJacobian.conditionNumber`;
- the bound is `FitQuality.covarianceOf` (step 476), with the reference noise as its reduced χ²;
- the directions' weights are `FitUncertainty.ParameterWeights` (step 342).

No second inverse, rank floor or condition number is written.

## Approach

1. **Domain, `MeasurementPlanner.fs`.**
   - `PlannerError` gains `NoiselessPlannerCandidateErr` and `PlannerShapeErr`, each `of reason : string` with its
     `describe` arm.
   - `NoiseVariance` and `CandidateRows` follow `PlannerInputs`' step-472 members (`looks`, `structuralReport`).
2. **Optimization, new `PlannerInformation.fs`.** It is appended last to the project's compile list, and holds, in
   the slice's order:
   - `SingularValue`;
   - `SingularSpectrum`, the NumericCarrier with the boundary-marked `ofRows` over MathNet's `Svd(true)`;
   - `SingularDirection` with `text`;
   - `InformationLogDeterminant` and `CovarianceTrace`;
   - `TargetBound`;
   - `PlannerInformation` with `over` and its members;
   - the `ReducedChiSquared.noiseVariance` extension.
3. **Tests, new `PlannerInformationTests.fs`.** It is appended last to the test project's compile list and holds the
   four named facts over literal Jacobians.

## Files

- `OpticalConstructor.Domain/MeasurementPlanner.fs`
- `OpticalConstructor.Optimization/PlannerInformation.fs` (new) and `OpticalConstructor.Optimization.fsproj`
- `OpticalConstructor.Tests/PlannerInformationTests.fs` (new) and `OpticalConstructor.Tests.fsproj`
- Consequential edits, same as steps 470 and 471. `PlannerInputsTests.fs:226` and `PlannerReportTests.fs:333` match
  `PlannerError` exhaustively. FS0025 is an error under `--warnaserror+:25`, so each or-pattern gains the two new cases.
  No assertion or identity changes.

## Risks

- **ELEV001.** `ReducedChiSquared this.noise.value` is the call-site rewrap ELEV001 reports: the receiver is
  `this.noise`, not `this`. A `toReducedChiSquared` member beside `noiseVariance` would bridge a bijection. So the
  member destructures its own noise (`let (NoiseVariance noise) = this.noise`), as `FitQuality.covarianceOf`
  destructures its arguments.
- **SVD shape.** MathNet's full SVD gives `VT` as n × n, and `S` holds only min(m, n) values. The values are padded
  with zeros to the coordinate count, so the null-space directions of a wide Jacobian are the trailing `VT` rows.
- **Signs.** Singular-vector signs are arbitrary, so the tests accept ± as the slice states.
