# Step 0381 — impl plan

## Approach

The study engine must solve from its parameters' own starts within their own bounds, while the S10.1 fit wrapper
`MuellerFitting.fitWithResidual` starts at zero inside a symmetric box. The one bounded Levenberg–Marquardt request
(`IterationCount 400`, `StepTolerance 1.0e-12`) moves out of `fitWithResidual` into one member both call.

In `OpticalConstructor.Optimization/MuellerFitting.fs`, after `SearchBox`:

1. `ScaledSearch = { start; lowerBounds; upperBounds }`, all `ScaledVector`.
2. `ScaledSearch.solveWith (watch : IterationWatch) (residual) : Result<NonlinearSolution, NonlinearSolverError>`
   builds the `NonlinearRequest` and solves it through `watch.levenbergMarquardtSolver` (step 380).
3. `SearchBox.centredSearch (dimension : int) : ScaledSearch`: the zero start within ± `halfWidth`. `ScaledSearch` is
   declared after `SearchBox`, so this is an intrinsic augmentation (`type SearchBox with`) in the same module.
   (As implemented, the parameter is `scaling : ParameterScaling<'Parameters>`, because ELEV010 refused the `int`; see
   the impl-log's Decisions.)
4. `fitWithResidual` becomes `(box.centredSearch scaling.dimension).solveWith UnwatchedIterations residual`, mapped
   to the same `MuellerFitData` / `NonlinearFitErr` as today.

The request is unchanged: the same zero start, the same ± half-width bounds, `IterationCount 400` and
`StepTolerance 1.0e-12`. It goes through the same proxy, because `createAlglibLevenbergMarquardt ()` is already
`UnwatchedIterations.levenbergMarquardtSolver`. Every BerremanTests number is therefore bit-identical.
`createAlglibLevenbergMarquardt` stays, because `LangasiteInverseTests` and `MuellerInverseTests` call it.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/MuellerFitting.fs`: `ScaledSearch`, `solveWith`,
  `centredSearch`, and `fitWithResidual` rewired.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyEngineTests.fs` (new): `module StudyEngineTests`, with two
  facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: the new file goes at the end
  of the compile list, with no comment.

## Risks

- **Record-label ambiguity.** `ScaledSearch.lowerBounds` / `upperBounds` share names with `NonlinearRequest`'s. Both
  record expressions are type-annotated, so inference cannot pick the wrong record.
- **Extension visibility.** `levenbergMarquardtSolver` is an extension member in `MuellerInverseSolver`, so
  `MuellerFitting.fs` opens that module. `OptimizationInterface` stays qualified, as the file already does for
  `DesignVector`, so its `OptimizationRequest` labels do not enter scope.
- **ELEV.** `centredSearch` takes an `int`, which is allowed because it is a member on our type. The vectors are born
  wrapped (`|> ScaledVector`), as `fitWithResidual` already does.
