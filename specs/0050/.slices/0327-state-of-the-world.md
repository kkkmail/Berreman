# Step 0327 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments, exclusions, corrections, the Data step and
the experiment matrix (001 §K, §O).

- Step 322 gave the domain `ExperimentMatrix.ofExperiments`, with rows source × objects × φ, columns analyzer, and the
  unplaced experiments listed apart. Until now no surface showed it.
- Steps 323–326 built the Data step: the data files, the split panel, and the review panel with its setup correction.
- This step opens the Data step on the experiment matrix. Each experiment sits in the row of its sources and its objects
  at their angles, and in the column of its analyzers. It shows its data badge and reviews the experiment when pressed.
  The experiments whose setup the kit lacks are named apart.

## What's working

- The Data step now opens on the experiment matrix (`ExperimentMatrix`):
  - the analyzer columns' captions head the grid;
  - each row is captioned by its sources and its objects at their angles;
  - each cell stacks one `<name> · <data badge>` entry per experiment.
- Pressing an entry sends `ReviewExperimentData`, and the reviewed experiment's entry is on the positive fill.
- Experiments whose setup revision the kit lacks are listed as `Not in the matrix (setup missing): …`. With no
  experiment, the step shows `(no experiments yet)`.
- Added `MatrixRowKey.caption`, `MatrixColumnKey.caption` and the ids `experimentMatrix` and
  `experimentMatrixEntry <id>`.
- Tests:
  - +1 ui-tests fact and +1 ui-smoke fact in `DataStepTests`, and the ids fact extended;
  - the Data step scroll fact now holds a split, a corrected review and a three-experiment matrix.

## Tests

These are development runs of each gate's own command from `0327.gates`, over the final tree
(`.artifacts/0327-dev-gates-summary.log`). The arc-runner's gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 36 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 323 s wall (215 s analyzers) | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552, 0 failed, 0 skipped | 72 s | 180 s |
| ui-smoke | Passed!, 257 of 257, 0 failed, 0 skipped | 226 s | 600 s |
| ui-tests (`DataStepTests`, `ScrollExtentTests`) | Passed!, 25 of 25, 0 failed, 0 skipped | 22 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- **The slice's test effects pass under their exact ids** (`.artifacts/0327-dev-trx/`):
  - created (ui-tests): `the matrix shows each experiment in its row and column with its data badge, and lists the
    unplaced ones`;
  - created (ui-smoke): `pressing a matrix entry reviews that experiment`;
  - changed: `the Data step UiIds are the stable intent-named ids`, which also pins `ExperimentMatrix` and
    `ExperimentMatrixEntry_1`;
  - changed: `ScrollExtentTests.the kit window's Data step leaves everything reachable`, its two assertions kept.
- **Counts.**
  - ui-smoke rose by 1, from 256 to 257.
  - Within ui-tests, `DataStepTests` without the smoke trait rose by 1, from 8 to 9. The selection also counts
    `ScrollExtentTests`' 16, for 25 in all.
  - constructor-unit-tests is unchanged at 1552.
- **Mutations** (`.artifacts/0327-dev-mutation.log`, driver `.artifacts/0327-mutation-tools/mutate.py`). All 20 were
  caught, each source was restored byte-identical, and the final runs came after them. They covered:
  - the caption joiners, the empty `—` and a dropped angle;
  - an entry without its badge, an entry that edits instead of reviewing, and the reviewed fill (missing, or on every
    entry);
  - the grid rows and columns off by one, and the column captions dropped;
  - the unplaced line dropped, the matrix moved below the data files, and the placeholder dropped;
  - both ids renamed, the grid without its id, and the matrix built over no items or no setups.
- **Elevation's suppressed count** (ELEV004, excluded in the command, measured not gated per R7) went from 1449 to 1453.
  Those are the new `.value` reads that render captions.
- No run exceeded its effective deadline. Elevation ran concurrently with the test gates.
- Every touched file is LF only: `git diff --numstat` equals `--ignore-cr-at-eol`.

```yaml
gates:
  constructor_unit_tests: 1552
  ui_smoke_tests: 257
  ui_tests: 25
```

## Architecture

- **A pure projection of step 322's matrix.** The view calls
  `ExperimentMatrix.ofExperiments model.experimentCollection.experiments model.kitSetups model.kitItems`. It sends only
  the existing `ReviewExperimentData`, and the update is untouched.
- **The captions are extension members in `ConstructorUiIdOfDomain`**, beside the other Domain-derived captions:
  - `MatrixRowKey.caption` writes each object through step 280's `Angle.degreeCaption`;
  - `MatrixColumnKey.caption` joins the analyzers.

  Controls references no Domain type, so this is the lowest project that sees both.
- **The grid layout:**
  - one `Auto` column per matrix column plus the caption column, and one `Auto` row per matrix row plus the header row;
  - an occupied cell is a vertical `StackPanel` of entries;
  - an empty cell emits no control.
- **The entries use the existing verb builders.** `verbButton` is used for an unreviewed entry, and `actionButton` on
  `positiveBackground` for the reviewed one, as step 324's split offer toggle does. The press depends only on the
  experiment's id, which the control's id and key carry.
- **The block is a list of the panel's children**, heading first, with no wrapping container: no other control is
  added.

## Deferred

Nothing. Every requirement of the slice landed.

## Gotchas

- **A Prep's angle never enters a matrix row**; only an object's (a Sample slot's) does.
  - The test bench therefore holds its polarizer under study, LP1, as a black-box Mueller item in the Sample slot:
    Setup 1 at 0°, Bench B at 30°.
  - A turned Prep polarizer would put both experiments in one cell.
- **`setup1` and `benchB` in `DataStepTests` now hold the bench's slots.** Steps 320–326's facts read only their
  references, and the split fact still reads `(new setup)` because its kit holds only `Dark`.
- **The class's mock now attaches every file as E1's attachment.** `AttachDataFile` records the experiment into the kit,
  so `withMatrixKit` first writes `setup1`, `benchB` and the bench's items into the opened folder kit. Otherwise the
  attach answers `Not saved to the kit — …`, which the fact rejects by checking `kitStatus`.
- **The matrix shows whenever an experiment exists**, even when all are unplaced. The grid is then only its empty
  header row, followed by the unplaced line.
- **The scroll fact builds its window in a local `dataStepWindow`.** After the reachability law it opens the window once
  more and checks the matrix, split and review controls are present, so "also holds" is proven.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0327: the Data step's experiment matrix.
  - The step opens on the matrix: rows source × objects × φ, columns analyzer, and one `<name> · <badge>` entry per
    experiment that reviews it when pressed. The experiments whose setup the kit lacks are listed apart.
  - Two caption members and two UiIds.
  - Tests: +1 ui-tests and +1 ui-smoke fact in `DataStepTests`, the ids fact extended, and the Data step scroll fact
    extended.
