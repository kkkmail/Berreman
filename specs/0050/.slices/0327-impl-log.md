# Step 0327 — impl log

## Progress

- [x] Read the protocol, the project prompt, the slice and the surrounding code (steps 315, 322–326).
- [x] UiIds: `experimentMatrix` (`ExperimentMatrix`), and the `dataStepPanel` doc names the matrix.
- [x] ConstructorUiIdOfDomain: `MatrixRowKey.caption`, `MatrixColumnKey.caption`, `experimentMatrixEntry`.
- [x] DataStepView: the experiment matrix first in the panel.
- [x] DataStepTests: the ids fact extended and two facts created.
- [x] ScrollExtentTests: the Data step fact extended.
- [x] Development build: exit 0, 0 warnings, 0 errors (`.artifacts/0327-dev-build.log`).
- [x] DataStepTests: 12 of 12 over both traits; the Data step scroll fact passes.
- [x] Mutation check (`.artifacts/0327-dev-mutation.log`): 20 of 20 caught, each source restored byte-identical.
- [x] Final development runs of each gate's command over the final tree, after the mutations: all green
  (`.artifacts/0327-dev-gates-summary.log`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: `experimentMatrix` (`ExperimentMatrix`) at the end of
  the Data step group, and the `dataStepPanel` doc now starts with the matrix.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`:
  - on `ConstructorUiId`, after `dataIncludePointButton`: `experimentMatrixEntry (experimentId : ExperimentId)`
    (`ExperimentMatrixEntry_<id>`);
  - `ExperimentMatrices.MatrixRowKey.caption`: the sources joined by ` + `, ` · `, then the objects as
    `<label>@<angle.degreeCaption>` joined by ` → `; `—` for no source or no object;
  - `ExperimentMatrices.MatrixColumnKey.caption`: the analyzers joined by ` + `, `—` for none.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataStepView.fs`:
  - new private `matrixEntry`: the verb `<name> · <badge text>`, sending `ReviewExperimentData`; the reviewed
    experiment's is `actionButton` on `positiveBackground`, every other is `verbButton`;
  - new private `experimentMatrixBlock`, first in the panel's children:
    - the heading `Experiment matrix:`;
    - with no experiment, the grey line `(no experiments yet)`;
    - otherwise the `Grid` carrying `experimentMatrix` over
      `ExperimentMatrix.ofExperiments model.experimentCollection.experiments model.kitSetups model.kitItems`. Row 0
      holds the column captions in columns 1… (semi-bold). Each matrix row is a grid row: its caption in column 0, then
      one vertical `StackPanel` of entries per occupied cell. An empty cell adds nothing;
    - when `unplaced` is not empty, the wrapping line `Not in the matrix (setup missing): <names joined by ", ">`, in
      the message colour;
  - the module doc and the `dataStepView` doc name the matrix.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataStepTests.fs`:
  - `open OpticalConstructor.Domain.ExperimentMatrices`, and the module doc names the matrix;
  - new fixtures: the bench items `cpl1`, `lp1` and `cpl2` (`benchItems`), `benchSlots`, and `withMatrixKit`;
  - `benchB` and `setup1` now hold `benchSlots`, at 30° and 0°;
  - the mock `reviewData` attaches every file as E1's attachment (it refused every attach before; no fact relied on
    that);
  - the ids fact pins `ExperimentMatrix` and `ExperimentMatrixEntry_1`;
  - new fact (ui-tests): `the matrix shows each experiment in its row and column with its data badge, and lists the
    unplaced ones`;
  - new fact (ui-smoke): `pressing a matrix entry reviews that experiment`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: `the kit window's Data step leaves
  everything reachable` now builds the window in a local `dataStepWindow`.
  - It holds three copies of the collected experiment over its setup, all placed in the matrix. The first is reviewed,
    with three points, point 2 excluded, and one setup correction. A pending split has three groups: matched, offered
    and unmatched.
  - Its two assertions are kept, and two are added on the matrix (nothing unplaced, three placed).
  - After the reachability law, a presence check opens the window at 800 × 600 and finds every control the fixture
    should show: the matrix, its three entries, the split's key line and three group rows, the review panel, its three
    point rows, point 2's Include, and the correction trail.

## Decisions

- **The polarizer under study sits in the bench's object slot**, so "Bench B (its polarizer turned)" gives its own row.
  - A matrix row holds the Prep items' labels and the objects' labels at their angles (step 322). A turned Prep
    polarizer would leave Setup 1 and Bench B on one row, and the fact names two row captions.
  - LP1 is therefore a black-box Mueller item (the models that fit a Sample slot are black box, retarder plate, empty
    slot and stack sample). It sits in the bench's Sample slot at R1 0° (Setup 1) and 30° (Bench B).
  - CPL1 (Prep) and CPL2 (analyzer) are circular-polarizer items, as in `ExperimentMatrixTests`.
  - The rows read `CPL1 · LP1@0°` and `CPL1 · LP1@30°`, and the column reads `CPL2`. 30° matches the class's correction
    note `LP1 was at 30`.
- **The shared `setup1` and `benchB` fixtures gain the bench's slots**, rather than duplicating `Setup 1` / `Bench B`
  under other names.
  - Steps 320–326's facts read only their references, which are unchanged.
  - The split fact still reads `(new setup)`: its kit holds only `Dark`.
- **The slots hold their items as `withItem` holds a fixed-mounted, unbound item** (`ItemInSlot (id, PlacementTurn)`).
  This avoids copying `ExperimentMatrixTests`' private `holding` helper into another test project.
- **E1's data is attached through `AttachDataFile` over the class's mock**, the real path:
  - `recordExperiment` writes E1 to the kit, so `withMatrixKit` first writes `setup1`, `benchB` and the bench's items
    into the opened folder kit;
  - `checkExperimentData` then reads `e1Series` through the mock and validates it as a dark line (R1's default range);
  - the badge is `validated (3 points)`, and the fact checks `kitStatus` is `None`, so a refused record cannot pass
    silently.
- **An empty cell holds nothing**: no `StackPanel` is emitted for a column the row has no cell in.
- **The matrix shows whenever an experiment exists**, even when every one is unplaced. The grid is then only its empty
  header row, followed by the unplaced line.
- **Entry verbs**: `verbButton` (re-patches its press every render) and `actionButton` (re-patches on id change), as in
  step 324's split offer toggle. The press depends only on the experiment's id, which the id carries.
- **The scroll fact gained a presence check.** Without it, "also holds" is unproven: the law passes as easily over an
  empty panel. It reuses the same window builder and changes no other fact.

## Testing state

Development runs of each gate's own command from `0327.gates`, over the final tree after the mutations. The driver is
`.artifacts/0327-tools/run-gates.py`, and the summary is `.artifacts/0327-dev-gates-summary.log`. Elevation ran beside
the test gates.

- build: exit 0, 0 warnings, 0 errors, 36 s (deadline 600 s).
- elevation: `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, 323 s wall (the analyzers report
  215 s), deadline 600 s.
  - The suppressed (ELEV004, excluded in the command) count went from 1449 to 1453. Those are the new `.value` reads
    rendering captions; R7 measures them and does not gate them.
- constructor-unit-tests: 1552 of 1552, 72 s (deadline 180 s).
- ui-smoke: 257 of 257, 226 s (deadline 600 s). Step 326 had 256; the +1 is `pressing a matrix entry reviews that
  experiment`.
- ui-tests (`DataStepTests`, `ScrollExtentTests`): 25 of 25, 22 s (deadline 360 s).
  - `DataStepTests` without the smoke trait: 9, step 326's 8 plus the created matrix fact.
  - `ScrollExtentTests` without the smoke trait: 16.
  - The changed ids fact and the changed `the kit window's Data step leaves everything reachable` pass under their
    exact ids (`.artifacts/0327-dev-trx/ui-tests.trx`).
- invariant-s31-\* (16): each `INVARIANT HOLDS`, under 1 s each (deadline 300 s).

No run exceeded its effective deadline. Every touched file is LF only: `git diff --numstat` equals
`--ignore-cr-at-eol`, and no CR byte is present.

Mutations (20 of 20 caught, each restored byte-identical):

| Mutation | Caught by |
|---|---|
| M1 sources joined by `, ` | matrix fact (captions) |
| M2 objects joined by ` + ` | matrix fact (captions) |
| M3 no source as blank, not `—` | matrix fact (captions) |
| M4 object without its angle | matrix fact (captions) |
| M5 analyzers joined by `, ` | matrix fact (captions) |
| M6 entry without its badge | matrix fact |
| M7 entry edits instead of reviewing | smoke fact |
| M8 reviewed entry keeps the verb look | smoke fact |
| M9 every entry on the positive fill | smoke fact, matrix fact |
| M10 rows start on the header row | matrix fact (cells) |
| M11 entries in the caption column | matrix fact (cells) |
| M12 unplaced line dropped | matrix fact |
| M13 matrix after the data files | matrix fact |
| M14 no placeholder without experiments | matrix fact |
| M15 entry id renamed | ids fact |
| M16 grid without its id | matrix fact |
| M17 matrix reads no item label | matrix fact |
| M18 matrix places nothing | smoke fact, matrix fact |
| M19 matrix id renamed | ids fact |
| M20 column captions dropped | matrix fact |

## Artifacts

- `.artifacts/0327-dev-ui-build.log`: the first Ui-only build.
- `.artifacts/0327-dev-build.log`: the solution build.
- `.artifacts/0327-dev-datastep-all.log`, `.artifacts/0327-dev-scroll-data.log`: the first focused test runs.
- `.artifacts/0327-dev-mutation.log`, driver `.artifacts/0327-mutation-tools/mutate.py` (adapted from step 326's).
- `.artifacts/0327-dev-gates-summary.log` and one log per gate: `0327-dev-build.log`, `0327-dev-elevation.log`,
  `0327-dev-constructor-unit-tests.log`, `0327-dev-ui-smoke.log`, `0327-dev-ui-tests.log`, `0327-dev-invariant-*.log`;
  driver `.artifacts/0327-tools/run-gates.py`.
- `.artifacts/0327-dev-trx/`: `constructor-unit-tests.trx`, `ui-smoke.trx`, `ui-tests.trx`.

## Gotchas

- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.
- **A Prep's angle never enters a matrix row.** Only objects (Sample slots) carry `@<angle>`. A fixture whose two setups
  differ only in a Prep polarizer's turn lands both experiments in one cell.
- **`AttachDataFile` records the experiment into the kit**, so a fixture attaching over a setup must first write that
  setup and the items its slots hold into the kit, or the attach answers `Not saved to the kit — …`.
