# Step 0352 — state of the world

## Where we are

This step is part of spec 0050 S10.3: the staged Mueller pipeline and its reference values are promoted.
- Steps 0349–0351 moved Stages 1, 2 and 3 of the section 7.2 pipeline into
  `OpticalConstructor.Optimization.StagedMuellerPath.reconstructStaged`.
- This step makes the product check part of the staged path's answer: M_{QZ+LR} against the cascade M_LR · M_QZ,
  and the product-check RMSE ratio the reference reports (`matrix_fit_linear.py:372-373`, `:334`). The section 7.2
  fact reads it and no longer computes the cascade itself. S11.4 will reuse it.
- Step 0353 is next.

## What's working

- Added `LeastSquaresSystem.residualRmseAt`, the one `‖A·x − b‖ / √m`. `createMathNetSvd` reports its RMSE through
  it, so every solver RMSE is the same double as before.
- Added `ProductRmseRatio`, `ProductCheckData` and `productCheckOf`: the cascade M_LR · M_QZ, its difference from
  M_{QZ+LR}, the combined fit's RMSE, and the cascade's RMSE over the combined rows, with their ratio.
- `reconstructStaged` now carries the product check in `StagedMuellerData.productCheck`. It is computed over the
  same QZ+LR system the combined solve fitted.
- Re-pointed the section 7.2 fact's cascade check to `staged.productCheck`. It also gained one assertion: the
  RMSE ratio is within `dataMatrixTol` of the reference's `combined_check`.
- Added the reference `fit_rmse`, `product_rmse` and their ratio to `MuellerReference`.
- Added one `StagedMuellerPathTests` fact: the product check over a fixed two-row system.

## Tests

These are development runs of each gate's own command from `0352.gates`, over the final tree. The arc-runner's gate
engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, no `error`; warnings only NU1902 (exempt) and the vendored MathNet SYSLIB0051 | 76 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`; known findings 0; suppressed 1509 (ELEV004 excluded) | 211 s | 600 s |
| constructor-unit-tests | Passed!, 1558 of 1558 (1557 + 1 new, Passed in the TRX) | 73 s | 180 s |
| unit-tests (`BerremanTests.MuellerReconstructionTests`) | Passed!, 25 of 25 (count unchanged) | 3 s | 7200 s |
| invariant-s35-\* (20) | each grep found nothing, both as written and with `--untracked` | under 1 s each | 300 s |

- No run went past its effective deadline.
- `MuellerReconstructionTests`' numbers are bit-identical by construction:
  - the cascade metrics and positions come from the same matrices, through the same calls;
  - every solver RMSE is the same operations on the same arrays.
- The new ratio assertion passes at `dataMatrixTol`.
- The new constructor fact raises the count by exactly 1, as the acceptance requires.

```yaml
gates:
  constructor_unit_tests: 1558
  berreman_unit_tests: 25
```

## Architecture

- **The residual RMSE has one implementation.** It is the member `LeastSquaresSystem.residualRmseAt`, shared by the
  SVD proxy and the product check (AGENTS.md "Reuse, never copy").
- **The ratio is formed in one place**, `ProductRmseRatio.between`, as `RelativeError.between` is.
- **`productCheckOf` is a pure function** of M_LR, M_QZ, the combined reconstruction and its system.
  `reconstructStaged` binds the QZ+LR system once (`systemOf QzLrProduct`), solves it, and hands that same value to
  the product check.
- **Contract changes:**
  - Created: `LeastSquaresSystem.residualRmseAt`, `ProductRmseRatio`, `ProductCheckData` and `productCheckOf`.
  - Changed shape: `StagedMuellerData` (`productCheck`) and `BerremanTestSupport.MuellerReference` (three values).
  - No project reference changed.

## Deferred

- Nothing. Every item of the brief, parts (0) to (5), is in place.

## Gotchas

- **`residualRmseAt` divides by `design.Length`**, where the old inline code used `a.RowCount`. These are the same
  integer: `RealMatrix.create` makes one row per design row, and both are non-zero after `createMathNetSvd`'s checks.
- **`MuellerReference.productRmse` shares its name with the `ProductCheckData.productRmse` field.** F# keeps record
  labels and module values apart, and the tests read the value qualified (`MuellerReference.productCheckRmseRatio`),
  so nothing is ambiguous.
- **The new constructor fact uses xUnit's `Assert.Equal(double, double, tolerance)`.** `OpticalConstructor.Tests`
  has no shared tolerance helper, and BerremanTests' `MatrixComparison` is not referenced from it.
- **The brief's reference path, under a `Berreman/Data/…` folder, must never be written into a file under
  `Berreman/`.** `invariant-s35-t9-no-data-mueller-matrix` forbids it. The doc comments cite only
  `matrix_fit_linear.py:<line>`.

- **ELEV004's suppressed count rose from 1506 to 1509.** The three new `.value` reads are `row.value` in
  `residualRmseAt` and the two operands in `ProductRmseRatio.between`. ELEV004 is excluded from the gate (spec 0048
  `.manual/002` R7), and no enforced rule found anything.

## Changelog

- 2026-11-12 — Step 0352 (spec 0050 S10.3): the product check (M_{QZ+LR} against M_LR · M_QZ, with the product /
  fit RMSE ratio) joined `StagedMuellerData`, through `productCheckOf` and the shared
  `LeastSquaresSystem.residualRmseAt`. The section 7.2 fact reads it and asserts the ratio against
  `MuellerReference`. One constructor fact was added. Development runs: build, elevation, constructor-unit-tests
  (1558), unit-tests (`MuellerReconstructionTests`, 25) and the 20 s35 invariants.
