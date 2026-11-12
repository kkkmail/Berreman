# 0337 — impl plan

## Approach

A move (spec 0050 S10.1, 001 §Q AT-2): the search box, the fit record and the three fit runners leave
`BerremanTests/InverseFitHarness.fs` and land in `OpticalConstructor.Optimization/MuellerFitting.fs`, after
`scalingAround`. Errors become values: the harness's `failwith` on a solver failure becomes
`Error (NonlinearFitErr e)`, and the test callers unwrap with `expectOk`.

1. `MuellerFitError` gains `NonlinearFitErr of NonlinearSolverError`.
2. After `scalingAround`, add:
   - `SearchBox` with `halfWidth`, unchanged;
   - `MuellerFitData<'Parameters>`, the harness's `InverseFit<'Parameters>` renamed (the name `InverseFit` is
     already the product module `OpticalConstructor.Optimization.InverseFit`). Same `[<ReferenceEquality>]` record,
     same `chiSquared` / `rmsResidual` members;
   - `fitWithResidual`, answering `Result<MuellerFitData<'Parameters>, MuellerFitError>`. The request is today's
     (zero start, ±`halfWidth`, `IterationCount 400`, `StepTolerance 1.0e-12`, ALGLIB LM);
   - `fitObservations` / `fitAbsorbingObservations`, the same parameters, answering `fitWithResidual`'s `Result`.
   - Each doc cut to one to three lines.
3. Delete the five from `InverseFitHarness.fs`. `NoisyExperiment.fit`, `fitReportOf` and `predictedRelativeErrors`
   take `MuellerFitData<'Parameters>`.
4. Callers append `|> expectOk`:
   - `BiaxialInverseTests` (`recoverFromNoisy` and its two fit facts);
   - `FeasibleBiaxialInverseTests`;
   - `AbsorbingBiaxialInverseTests`;
   - `AbsorbingBiaxialNoisyTests.recoverFromNoisy` (it needs `open OpticalConstructor.Optimization.MuellerFitting`,
     which it lacks today);
   - `MuellerInverseTests`' wrapper calls `MuellerFitting.fitObservations … |> expectOk`.
5. `MuellerInverseTests` comments: the class doc names `OpticalConstructor.Optimization.MuellerFitting`, the wrapper
   doc names `MuellerFitting.fitObservations`, and the trailing comment on `open BerremanTests.InverseFitHarness`
   goes.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`
- `Berreman/BerremanTests/InverseFitHarness.fs`
- `Berreman/BerremanTests/MuellerInverseTests.fs`
- `Berreman/BerremanTests/BiaxialInverseTests.fs`
- `Berreman/BerremanTests/FeasibleBiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialInverseTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialNoisyTests.fs`
- `Berreman/BerremanTests/AbsorbingBiaxialSample.fs` (only if the compiler asks; `SearchBox` resolves through its
  existing `MuellerFitting` open)

## Risks

- **Record-label inference.** `NonlinearRequest` and `MuellerFitData` both have a `residual` field. The harness
  already built `NonlinearRequest` with `InverseFit` in scope, so F# disambiguates by the other labels; the build
  confirms it.
- **Shadowing.** `MuellerInverseTests` has a class-local `fitObservations` that shadows the product one; its body
  calls the product one qualified, as it called the harness one.
- **ELEV on product code.** The new types and functions carry no bare primitive in a signature. `SearchBox`'s
  `halfWidth` and `MuellerFitData`'s `chiSquared` / `rmsResidual` are members on their own types, which may speak
  their own primitives. `NonlinearFitErr` ends in `Err`.
- **Bit-identity.** The request, the solver and the record are unchanged, so the numbers are unchanged.
