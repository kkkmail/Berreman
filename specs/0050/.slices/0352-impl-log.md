# Step 0352 — impl log

## Progress

- [x] Read the protocol, project prompt, slice, AGENTS.md, `StagedMuellerPath.fs`, `MuellerReconstruction.fs`, the
      section 7.2 fact, `MuellerReference.fs` and `StagedMuellerPathTests.fs`.
- [x] `MuellerReconstruction.fs`: `LeastSquaresSystem.residualRmseAt`; `createMathNetSvd` reports through it.
- [x] `StagedMuellerPath.fs`: `ProductRmseRatio`, `ProductCheckData`, `productCheckOf`, `StagedMuellerData.productCheck`.
- [x] `MuellerReference.fs`: the two RMSEs, the ratio and the `dataMatrixTol` doc line.
- [x] `MuellerReconstructionTests.fs`: the section 7.2 fact reads the product check and asserts the ratio.
- [x] `StagedMuellerPathTests.fs`: the new fact.
- [x] Development runs of build, elevation, constructor-unit-tests, unit-tests (selected class) and the 20 s35
      invariants.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/MuellerReconstruction.fs`:
  - **`LeastSquaresSystem.residualRmseAt (solution : RealVector) : ResidualRmse`** is new. It is
    `RealMatrix.create` of the design rows' values times `solution`, minus `RealVector.create rhs`; the difference's
    `norm` over `sqrt (float design.Length)`. Its doc is two lines (spec 0050 S10.3).
  - **`createMathNetSvd`**: the `residual`, `rows` and `rmse` bindings are gone. The `Ok` record's `rmse` is
    `system.residualRmseAt (RealVector x)`. Its checks, errors and `try/with` are unchanged. No other declaration
    changed.
- `Berreman/OpticalConstructor/OpticalConstructor.Optimization/StagedMuellerPath.fs`, before `StagedMuellerData`:
  - **`ProductRmseRatio of float`**, with `value` and `static member between productRmse fitRmse`
    (`productRmse.value / fitRmse.value`). This is the one place the ratio is formed, as `RelativeError.between` is.
  - **`ProductCheckData`** = `{ cascade; difference; fitRmse; productRmse }`, with `member this.rmseRatio`.
  - **`productCheckOf lr qz combined combinedSystem`**:
    - `cascade` is `cascadeProduct lr qz`;
    - `difference` is `frobeniusDiff combined.matrix cascade`;
    - `fitRmse` is `combined.rmse`;
    - `productRmse` is `combinedSystem.residualRmseAt cascade.vecColumnMajor`.
  - **`StagedMuellerData`** gains `productCheck : ProductCheckData` after `qzLr`. Its doc and `reconstructStaged`'s
    doc name the product check.
  - **`reconstructStaged`**: the local `reconstructObject kind` is split into `systemOf kind` (the same
    concatenation of the five families' `buildDesign` parts) and `reconstructObject kind system` (the same
    `reconstruct` and error map). The QZ+LR system is bound once as `combinedSystem`, solved, and passed to
    `productCheckOf lr.matrix qz.matrix qzLr combinedSystem` after the three solves.
- `Berreman/BerremanTestSupport/MuellerReference.fs`:
  - It opens `OpticalConstructor.Optimization.StagedMuellerPath`.
  - New, after the gains:
    - `combinedFitRmse` = `ResidualRmse 0.10162659513724465` (`combined_check.fit_rmse`);
    - `productRmse` = `ResidualRmse 0.12833930975940816` (`combined_check.product_rmse`);
    - `productCheckRmseRatio` = `ProductRmseRatio.between productRmse combinedFitRmse`.
  - The two RMSEs were checked against `reference_step2_linear_summary.json:285-286`.
  - `dataMatrixTol`'s doc gains one line: it also bands the ratio, 5.7e-13 observed when spec 0050 was authored,
    2026-11-04.
- `Berreman/BerremanTests/MuellerReconstructionTests.fs`, the section 7.2 fact:
  - It binds `check = staged.productCheck`. The fact's own `cascadeProduct` and `frobeniusDiff` calls are gone.
  - The three metrics are `check.difference`'s, against `MuellerReference` at `dataMatrixTol` as before.
  - The two largest-delta positions compare `staged.qzLr.matrix` with `check.cascade`.
  - One new assertion: `check.rmseRatio` within `dataMatrixTol` of `MuellerReference.productCheckRmseRatio`. Its
    message names `fit_rmse`, `product_rmse`, the ratio and the reference ratio.
  - The opening comment says "product check" where it said "cascade".
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StagedMuellerPathTests.fs`:
  - Opens `Berreman.Fields` and `OpticalConstructor.Domain`, for `StokesVector`, `MuellerMatrix` and `Propagation`.
  - The module doc names the product check.
  - New fact `the product check compares the combined matrix with the cascade and the RMSEs over the combined rows`.
    It uses the brief's literals: `lr` = identity, `qz` = diag(1, 0.5, 0.5, 1), `combined` = identity at rank 16
    and RMSE 0.1, and the two `kron4` rows reading M₀₀ and M₁₁ with rhs 1.0 and 0.8. It checks each of these within
    1e-12, through xUnit's own `Assert.Equal(double, double, tolerance)`:
    - the cascade, element by element against `qz`;
    - `maxAbs` 0.5 and `frobenius` √0.5;
    - `fitRmse` 0.1 and `productRmse` √0.045;
    - the ratio √0.045 / 0.1.

## Decisions

- **`systemOf` and `reconstructObject` are two locals, not one returning a pair.** The combined system is bound
  once, and that same value is both solved and handed to the product check. The other two solves read
  `reconstructObject Qz (systemOf Qz)` and `reconstructObject Lr (systemOf Lr)`. Both locals do the same work in the
  same order as the old single local, so every design and every solve is unchanged.
- **`residualRmseAt` divides by `design.Length`, where the old code used `a.RowCount`.** These are the same integer:
  `RealMatrix.create` builds one row per design row, and both are non-zero once the checks before it pass. The
  product, difference and norm are the same MathNet operations on the same arrays, so every solver RMSE is the same
  double.
- **`createMathNetSvd` follows the brief's literal form**, `system.residualRmseAt (RealVector x)`. ELEV001 fires only
  on a `Ctor src.value` re-wrap (`Softellect/Analyzers/NoCallSiteRewrap.fs`), and `x` is a MathNet vector, not a
  `.value`.
- **The new fact uses xUnit's `Assert.Equal(expected, actual, tolerance)`.** `OpticalConstructor.Tests` has no shared
  tolerance helper, and `ExperimentMatrixTests` already uses this overload. No new epsilon logic is written.
- **Not used:** BerremanTests' `MatrixComparison`. That project is not referenced by `OpticalConstructor.Tests`.

## Testing state

These are development runs of each gate's own command from `0352.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error`; warnings only NU1902 (exempt) and the vendored MathNet SYSLIB0051 | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1509 (ELEV004 excluded) | 211 s | 600 s |
| constructor-unit-tests | Passed!, 1558 of 1558, 0 failed (1557 + the new fact, Passed in the TRX) | 73 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25, 0 failed (count unchanged) | 3 s | 7200 s |
| invariant-s35-\* (20) | each decoded `git grep` found nothing (exit 1, empty), both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline.
- **`MuellerReconstructionTests`' numbers are bit-identical by construction.**
  - The three cascade metrics and the two positions come from the same matrices, through the same
    `cascadeProduct` and `frobeniusDiff` calls, now made inside `productCheckOf`.
  - Every solver RMSE is the same operations on the same arrays.
  - The facts the brief names all pass, unchanged, in the 25:
    - `the real createMathNetSvd proxy solves a known over-determined full-rank system`;
    - `reconstruct recovers a seeded MuellerMatrix …`;
    - `freeCosineFit recovers …`;
    - the section 7.2 fact.
- The new ratio assertion passes at `dataMatrixTol`.
- LF only: `git diff --numstat` equals `--ignore-cr-at-eol`, and no edited file contains a CR.
- `specs/0050/.manifest.state.json` was already modified when this step started; it is the supervisor's, not this
  step's.

## Artifacts

- `.artifacts/0352-build.log`
- `.artifacts/0352-elevation.log`
- `.artifacts/0352-constructor-unit-tests.log`
- `.artifacts/0352-unit-tests.log`
- `.artifacts/0352-invariants.log`
