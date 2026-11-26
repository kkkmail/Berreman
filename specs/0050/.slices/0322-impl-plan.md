# Step 0322 — impl plan

## Approach

The experiment matrix as pure Domain (001 §O "Experiment matrix view: rows source × objects × φ, columns analyzer";
spec 0050 S9.3). The view and its badges are step 327's.

1. New `OpticalConstructor.Domain/ExperimentMatrices.fs`, `module ExperimentMatrices` in `namespace
   OpticalConstructor.Domain`, compiled directly after `DataSplits.fs` (one `<Compile>` line, no comment). In order:
   - `MatrixObject = { item : ItemLabel; angle : Angle }`;
   - `MatrixRowKey = { sources : ItemLabel list; objects : MatrixObject list }` and
     `MatrixColumnKey = { analyzers : ItemLabel list }`;
   - `MatrixCell = { row; column; experiments : ExperimentId list }`;
   - `ExperimentMatrix = { rows; columns; cells; unplaced }` with `tryFindCell row column` and
     `static member ofExperiments experiments setups items`.
2. `ofExperiments`, the one rule:
   - each experiment's `slotsIn setups`; an error puts its id in `unplaced`;
   - the slots in light order (`SetupSlot.inLightOrder`): the `PrepSlotRole` items' labels are the row's `sources`; each
     `ObjectSlotRole` slot holding an item gives its label and the slot's `turnAngle`; the `AnalyzerSlotRole` items'
     labels are the column's `analyzers`;
   - a slot holding nothing, or an item `items` lacks, adds nothing; the source and detector slots do not enter;
   - rows, columns and cells by first appearance (`List.distinct` keeps first-appearance order); a cell's ids in the
     collection's order.
3. Tests: new `OpticalConstructor.Tests/ExperimentMatrixTests.fs`, `module ExperimentMatrixTests`, appended last in
   the test project's compile list, over test-local copies of step 271's bench (`src`, `lpA`, `sampleA`, `sampleB`,
   `lpB`, `det`) and items (`Laser`, `CPL1`, `CPL2`, `QZ`, `LR`, `QZd`), as `DataSplitTests` holds them. Two facts, as
   the slice names them.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentMatrices.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentMatrixTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **Unique names (ELEV014/015/016).** `MatrixObject`, `MatrixRowKey`, `MatrixColumnKey`, `MatrixCell`,
  `ExperimentMatrix`, `ExperimentMatrices`: a grep of the tree and of STL finds none of them today.
- **Angle equality.** Rows compare `Angle` structurally; equal settings give equal doubles, which is the matrix's
  intent (setups at the same angle share a row).
- **ELEV013/003.** No free function is declared; the rule lives on `ExperimentMatrix` as a static member, its helpers
  local.
- **LF line endings** in every touched file.
