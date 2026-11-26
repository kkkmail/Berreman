# Step 0396 — impl plan

## Approach

Step 0396 declares the S11.4 result shapes that a fit run records. Steps 397-403 fill them and step 404 stores them.
All of them go in `FitRuns.fs`, after `ObjectMatrixFit` and before `FitRun`, in the twelve-item order the how-to
gives:

1. `FitEntryRef`
2. `FitCoordinate`
3. `FitProblemSubject`
4. `JacobianColumnNorms` / `CorrelationMatrix` (NumericCarriers)
5. `FitProblemQuality`
6. `FitEntrySigma`
7. `RunResidual`
8. `MuellerRealizability` / `ObjectRealizability` (+ `realizability`)
9. `SeriesProductCheck`
10. `SuspectReason`
11. `ProfileOutcome` / `ProfilePoint` / `ParameterProfile`
12. `FitRunResults`

The only behaviour is `ObjectRealizability.realizability`. It returns `RealizableMueller` when every eigenvalue is at
least `-1.0e-12 * (sum of the eigenvalues)`, else `UnrealizableMueller`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/FitRuns.fs`: the shapes.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/FitResultsTests.fs`: one fact, the step's single test
  identity. The file is already in the `.fsproj`.

## Risks

- **ELEV.**
  - Every record field and DU case field must be elevated.
  - The two carriers hold bulk numbers and are marked `[<Softellect.Sys.NumericCarrier>]`, as `ResidualJacobian` is.
  - No new names may collide; git grep finds none in Berreman or STL.
- **Record label ambiguity.** Labels such as `items`, `rank`, `rmse`, `entry` and `value` are shared with earlier
  records. The test annotates its record expression.
- **No other type or member.** The file form is step 404's, so no converter, `tryOfWire` or `text` is added.
