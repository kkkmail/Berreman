# Step 0322 — impl log

## Progress

- [x] Read the task, both system prompts, the project prompt and the slice. The task carries no operator note and no
  ruling for this step.
- [x] `ExperimentMatrices.fs` and its compile line (directly after `DataSplits.fs`).
- [x] `ExperimentMatrixTests.fs` and its compile line (last in the test project's list).
- [x] Development runs of the gates: build, constructor-unit-tests, the sixteen s31 invariants, elevation.
- [x] Mutation checks (five, all caught); the final tree rebuilt and re-tested.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ExperimentMatrices.fs` (new): `module ExperimentMatrices` in
  `namespace OpticalConstructor.Domain`. Its module doc cites 001 §O and spec 0050 S9.3; each type and member doc cites
  spec 0050 S9.3. In the slice's order:
  - `MatrixObject = { item : ItemLabel; angle : Angle }`;
  - `MatrixRowKey = { sources : ItemLabel list; objects : MatrixObject list }`;
  - `MatrixColumnKey = { analyzers : ItemLabel list }`;
  - `MatrixCell = { row; column; experiments : ExperimentId list }`;
  - `ExperimentMatrix = { rows; columns; cells; unplaced }`, with
    - `member this.tryFindCell (row : MatrixRowKey) (column : MatrixColumnKey) : MatrixCell option`;
    - `static member ofExperiments (experiments : Experiment list) (setups : Setup list) (items : Item list) : ExperimentMatrix`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `<Compile>` line for
  `ExperimentMatrices.fs`, directly after `DataSplits.fs`, no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentMatrixTests.fs` (new): `module ExperimentMatrixTests`
  in `namespace OpticalConstructor.Tests`, with the two facts the slice names.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line,
  appended last, no comment.

## Decisions

- **How `ofExperiments` places an experiment.** Each experiment's `slotsIn setups` (step 261):
  - an error puts its id in `unplaced`;
  - otherwise its slots in light order (`SetupSlot.inLightOrder`) give a one-experiment cell. Each slot's label is
    looked up in `items`, so an empty slot or an item `items` lacks adds nothing. Then, each in light order:
    - `sources`: the labels of the `PrepSlotRole` slots;
    - `objects`: each `ObjectSlotRole` slot's label at the slot's `turnAngle` (step 277);
    - `analyzers`: the labels of the `AnalyzerSlotRole` slots.
  - Each role match names every `SlotRole` case, so a new role is a compile-time decision, not a silent drop.
- **First-appearance order.** `rows`, `columns` and the cells' (row, column) keys are `List.distinct` over the placed
  experiments in the collection's order. `List.distinct` discards later occurrences, so each list keeps first
  appearance. A cell gathers its experiments' ids by filtering the placed list, so they keep the collection's order.
- **No tuple wider than two, no repeated element type** (ELEV011): the locals carry `RoledSlot * ItemLabel`,
  `ExperimentId * Result<…>` and `MatrixRowKey * MatrixColumnKey`.
- **The tests' bench is a test-local copy of step 271's**, as the slice says, and as `DataSplitTests` holds its own:
  `src`, `lpA`, `sampleA`, `sampleB`, `lpB`, `det` and `Laser`, `CPL1`, `CPL2`, `QZ`, `LR`, `QZd` (dial-mounted,
  `DialNegative`, its zero at 10°). The fixtures in `NamingTemplateTests` and `DataSplitTests` are private, and this
  step's effects do not authorize editing those files. Each setup's slots are listed out of light order (the Laser,
  the detector, the analyzer, `sampleB`, `sampleA`, `lpA`), so the light order is the rule's, not the input's.
- **What the first fact adds beyond the slice's words.**
  - Every expected cell is found by `tryFindCell` at its own row and column.
  - Without CPL2 in `items`, E1 sits in the column of no analyzer: an item `items` lacks adds nothing.
  - The slice's assertions are all kept.
- **What the second fact asserts.**
  - `Series` (`QZ` at 45° in `sampleA`, `LR` at 45° in `sampleB`) gives the one row `CPL1 · QZ@45 → LR@45`.
  - `Dial` (`QZd` in `sampleA` at R1 30°) gives a row whose one object is `QZd` at −20°, within 1e-12, as
    `DataSplitTests` compares a dial reading.
  - The experiment over `Gone` revision 1 is the only unplaced one, and the cells hold exactly E1 and E3.

- **Line widths.** Four test lines past 140 characters were wrapped into named locals (`qz90`, `lackingCpl2`, `lr45`,
  `qzd30`) and a multi-line record. The lines still past 140 are the two fact names, which the slice fixes, and fixture
  lines copied verbatim from `DataSplitTests`.

## Testing state

Development runs of each gate's own command (the gate engine runs the gates themselves):

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; no lowercase `error` in stdout. The first build recompiled the vendored MathNet (2 `SYSLIB0051` warnings, untouched); the final build had 0 warnings | 76 s first, 16 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1435) | 199 s | 600 s |
| constructor-unit-tests | Passed!, 1551 of 1551 (1549 + 2) | 70 s first, 69 s final | 180 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- Both facts of the slice's test effects are `Passed` in the TRX under their exact ids.
- **Mutations** (`.artifacts/0322-dev-mutation.log`). Each rebuilt the test project, ran `ExperimentMatrixTests`, and
  restored the source byte-identical:
  - M1, the Prep items are not the row's sources: both facts fail.
  - M2, an object sits at its placement's R1, not its `turnAngle`: the dial fact fails.
  - M3, a refused revision is not listed as unplaced: the second fact fails.
  - M4, each experiment keeps its own cell, never merged: the first fact fails.
  - M5, the source slot's item enters the row's sources: both facts fail.
- The final tree was rebuilt (0 warnings, 0 errors) and the whole suite re-run: 1551 of 1551. M5 had failed both facts,
  so their passing shows the final binaries are unmutated.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes); `git diff --numstat` equals the `--ignore-cr-at-eol` count.

## Artifacts

- `.artifacts/0322-dev-build.log` (first build) and `.artifacts/0322-dev-build-final.log`.
- `.artifacts/0322-dev-constructor-unit-tests.log` and `.artifacts/0322-dev-constructor-unit-tests-final.log`, with
  their TRX files under `.artifacts/0322-dev-trx/`.
- `.artifacts/0322-dev-elevation.log` and `.artifacts/0322-dev-invariants.log`.
- `.artifacts/0322-dev-mutation.log` and its script, `.artifacts/0322-mutation-tools/mutate.py`.
