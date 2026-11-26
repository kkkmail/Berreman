# Step 0352 — impl plan

The product check, M_{QZ+LR} against the cascade M_LR · M_QZ, joins the staged path's answer, with the
product-check RMSE ratio the reference reports (`matrix_fit_linear.py:372-373`, `:334`).

## Approach

1. `MuellerReconstruction.fs`: `LeastSquaresSystem.residualRmseAt (solution : RealVector) : ResidualRmse`, the
   `‖A·x − b‖ / √m` expression `createMathNetSvd` evaluates today, in its order (`RealMatrix.create` of the design
   rows' values times the solution, minus `RealVector.create` of `rhs`, `norm` over `sqrt` of the row count).
   `createMathNetSvd`'s `residual` / `rows` / `rmse` bindings become `system.residualRmseAt (RealVector x)`; nothing
   else in the file changes.
2. `StagedMuellerPath.fs`, before `StagedMuellerData`: `ProductRmseRatio` (with `between`), `ProductCheckData` (with
   `rmseRatio`) and `productCheckOf lr qz combined combinedSystem`.
3. `StagedMuellerData` gains `productCheck` after `qzLr`. `reconstructStaged`'s local `reconstructObject` is split so
   the `QzLrProduct` system stays in hand: a `systemOf kind` builds the system and `reconstructObject` solves it, so
   the combined solve and the product check read the same system value.
4. `MuellerReference.fs`: open `StagedMuellerPath`, add `combinedFitRmse`, `productRmse`, `productCheckRmseRatio`
   after the gains, one doc line on `dataMatrixTol`.
5. Section 7.2 fact: the cascade check reads `staged.productCheck` (its `cascadeProduct` / `frobeniusDiff` calls go),
   plus one ratio assertion within `dataMatrixTol`.
6. `StagedMuellerPathTests`: the new fact over the brief's two-row system.

## Risks

- Bit-identity: `residualRmseAt` rebuilds `RealMatrix.create` / `RealVector.create` from the same arrays, so the
  product and norm are the same doubles; `a.RowCount` equals `design.Length` (both checked non-zero before).
- ELEV: new public names must be unique (`ProductRmseRatio`, `ProductCheckData`, `productCheckOf`, `residualRmseAt`,
  `rmseRatio`, `between` is a static member, not a free name). The test-support names `combinedFitRmse`,
  `productRmse`, `productCheckRmseRatio` must not collide with an existing public name in our assemblies.
