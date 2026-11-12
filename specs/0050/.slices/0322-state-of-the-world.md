# Step 0322 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments, and the experiment matrix (001 §K, §O).

- Steps 317–321 gave the kit window a split of one lab file into many experiments, through the one import wizard.
- This step adds the experiment matrix as pure Domain (001 §O "Experiment matrix view: rows source × objects × φ,
  columns analyzer"). Following step 268, the source is the polarization source, the Prep slots.
- The matrix view at the top of the Data step, with a data badge per experiment, is step 327's. It reads
  `ExperimentMatrix.ofExperiments` and adds the row and column captions.

## What's working

- Added `ExperimentMatrices` (Domain), compiled directly after `DataSplits.fs`. It declares `MatrixObject`,
  `MatrixRowKey`, `MatrixColumnKey`, `MatrixCell` and `ExperimentMatrix`.
- `ExperimentMatrix.ofExperiments` reads each experiment's setup revision (`slotsIn`) in light order:
  - its row is the Prep items, then the objects at their `turnAngle` (a dial reading, else R1);
  - its column is the analyzer items;
  - an experiment whose setup the kit lacks is listed as unplaced.
- Rows, columns and cells are listed in first-appearance order, and each cell holds its experiments in the
  collection's order. `tryFindCell` answers a row and column's cell.
- Tests: new `ExperimentMatrixTests` with the slice's two facts (constructor-unit-tests 1549 → 1551).

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; no lowercase `error` in stdout; the final build had 0 warnings | 76 s first, 16 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1435) | 199 s | 600 s |
| constructor-unit-tests | Passed!, 1551 of 1551, 0 failed, 0 skipped | 70 s first, 69 s final | 180 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0322-dev-invariants.log`) | under 1 s each | 300 s |

- **constructor-unit-tests: 1551**, which is 1549 + 2, as the slice requires. The TRX
  (`.artifacts/0322-dev-trx/constructor-unit-tests-final.trx`) shows both new facts as `Passed`:
  - `the matrix puts each experiment in the row of its sources and objects at their angles and the column of its analyzers`;
  - `two objects in series make one row, a dial-mounted object sits at its reading, and an experiment whose setup the kit lacks is unplaced`.
- **Mutations** (`.artifacts/0322-dev-mutation.log`). Each ran over a rebuilt tree, with the source restored
  byte-identical, and each was caught:
  - M1, the Prep items dropped from the row;
  - M2, an object at its placement's R1 instead of its `turnAngle`;
  - M3, a refused revision not listed as unplaced;
  - M4, cells never merged;
  - M5, the light source's item entering the row.
- The first build recompiled the vendored MathNet and printed two `SYSLIB0051` warnings from
  `MathNetNumerics/Numerics/Exceptions.cs`, which this step did not touch. The final build printed 0 warnings.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes).

```yaml
gates:
  constructor_unit_tests: 1551
```

## Architecture

- **One rule, in one static member.** `ExperimentMatrix.ofExperiments` is the only constructor of a matrix. Its
  helpers are local, so the module declares no free function.
  - It reuses `Experiment.slotsIn` (step 261), `SetupSlot.inLightOrder` (step 257) and `SetupSlot.turnAngle`
    (step 277). It adds no second lookup of a revision, a role or an angle.
- **Keys are values.** A row is its Prep labels and its objects (label and angle), and a column is its analyzer
  labels, all compared structurally.
  - Two setups that hold the same items at the same angles share a row, whatever their names.
  - A setup with an empty analyzer slot sits in the column `[]`.
- **The matrix carries ids, not experiments.** The view (step 327) looks each experiment up in the collection for its
  name and data badge. Captions are the view's: the Domain adds no text member.
- **Exhaustive role matches.** Each role match names every `SlotRole` case. A future role then has to be placed
  deliberately: it cannot vanish through a wildcard.

## Deferred

Nothing. Every requirement of the slice landed. The matrix view, its captions and its badges are step 327's, by the
spec's plan.

## Gotchas

- **Angles compare exactly.** A row's object angle is the slot's stored `turnAngle`. Two settings that differ in the
  last bit make two rows. For a dial, the reading is computed from R1 and the zero, so a test compares it within
  1e-12, as `DataSplitTests` does.
- **The bench is a test-local copy** of step 271's, as the slice says, and as `DataSplitTests` keeps its own. The
  fixtures in `NamingTemplateTests` and `DataSplitTests` are private, and this step's effects do not cover those files.
- **Mutation runs leave mutated binaries.** Restoring the source is not enough: the solution was rebuilt before the
  final test run.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file, and it was not
  touched here.

## Changelog

- 2026-11-12 — Step 0322: added the experiment matrix as pure Domain (`ExperimentMatrices`).
  - Rows are Prep items × objects at their angles, and columns are analyzer items. Each cell holds the experiments
    measured there, in first-appearance order, and experiments whose setup the kit lacks are listed as unplaced.
  - Tests: two `ExperimentMatrixTests` facts (1549 → 1551).
