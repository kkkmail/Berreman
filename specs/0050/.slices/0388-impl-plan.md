# Step 0388 — impl plan

## Approach

The study's linear engine (S11.3) needs the staged Mueller path's pieces callable. Each piece is MOVED out of a
private helper (or an inline expression) of the reference path into a public declaration, and the reference path
calls it back, so the same operations run in the same order and `MuellerReconstructionTests` stays bit-identical.

### `OpticalConstructor.Domain/MuellerReconstruction.fs`

- `Family.text` (`LP-LP` … `CPL-CPL`), `MatrixKind.text` (`AIR`, `QZ`, `LR`, `QZ+LR`), `SolverError.describe`.
- `AnalyzerModel.linearAt zero sign` / `circularAt zero sign retardance` — the step-349 literals of
  `StageOneCalibrationData.lpAnalyzer` / `cplAnalyzer`, moved (the `- retardance.degrees * 1.0<deg>` expression kept).
- `LeastSquaresSystem.concat` (intrinsic: refers only to the record) — the `Array.concat` pair `reconstructStaged`'s
  `systemOf` built.
- `LeastSquaresSystem.ofProjections` — an intrinsic type extension declared after `designRow` (declaration order:
  `designRow` is later than the record); the record assembly `buildDesign` ended with, which it now calls.

### `OpticalConstructor.Optimization/StagedMuellerPath.fs`

- `StageOneAirRows` + `calibrateFromAirRows`: `calibrateStageOne`'s body over caller-selected AIR rows; the private
  `reduceAirTraces` loses its `(AIR#0)` filter, which `calibrateStageOne` now applies to each family file before
  calling `calibrateFromAirRows` (`List.filter` keeps order, so the traces are the same).
- `airAnchorGainOf`: the private `fitAirGain` + `AirBlockRows.meanTime` moved, the empty check (`EmptyCaptureSetErr`
  naming the run, same text) with them. `AirBlockRows` is deleted (its only job was non-emptiness and `meanTime`);
  `airBlock` becomes a plain run filter, `airAnchor` / `airAnchorPair` / `airAnchors` call `airAnchorGainOf`.
- `projectCaptureRow`: the private `prepareRow` moved; `reconstructStaged` projects each row through a local closure
  giving the reading's `objectPhi` and `(ruleOf row).gainAt row.capturedAt`. `systemOf` calls `LeastSquaresSystem.concat`.
- `StageOneCalibrationData.lpAnalyzer` / `cplAnalyzer` call `linearAt` / `circularAt`.
- `StagedMuellerError.describe`.

### `OpticalConstructor.Tests/StagedMuellerPathTests.fs`

Five new facts, exactly the five ids the effects list, over the step-351 synthetic captures.

## Risks

- Bit-identity: every moved expression keeps its operands and order; the only reordering is argument evaluation of
  pure functions (gain computed before the projection), which cannot change a number.
- ELEV: new names checked unique in the repo; members may speak primitives; no new tuple over three; no rewrap.
- Intrinsic type extension must sit in the same module and file as `LeastSquaresSystem` — it does.
