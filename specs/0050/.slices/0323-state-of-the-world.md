# Step 0323 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments, and the Data step (001 §O, §K).

- Steps 317–321 gave the kit window a split of one lab file into many experiments, through the one import wizard
  (`OpenSplitWizard`). Step 322 added the experiment matrix as pure Domain.
- This step adds the Data step to the Inverse constructor's flow strip, after Experiments. Following 001 §O, it moves
  the Experiments bay's "Data files" section into it, and gives it the split verb.
- Steps 324–327 add the step's further panels, the matrix view (step 327) among them.

## What's working

- Added `DataFlowStep` to `ConstructorFlowStep`. `stepsOf` shows it in the Inverse constructor only, so the Main strip
  stops at Experiments.
- Leaving the Inverse constructor from the Data step lands on Experiments and its bay.
- The new `DataStepView.dataStepView` shows every experiment's data-file row and `Split a file by key…`.
  - The rows (`attachDataFileButton`, `dataFileRow`) moved here from `TableAndElementRotationView`.
  - The split verb opens the split wizard (`OpenSplitWizard`) on a picked lab file.
  - Both verbs are withheld over a read-only kit.
- The Experiments bay no longer holds a data-file section. The kit window shows the Data step keyed `data`.
- Added the UiIds `FlowDataStep`, `DataStepPanel` and `DataSplitFileButton`.
- Tests:
  - constructor-unit-tests 1551 → 1552;
  - ui-smoke 253 → 254;
  - selected ui-tests 52 → 53;
  - two facts renamed, and three changed as the slice lists.

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 25 s first, 17 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1435) | 210 s | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552, 0 failed, 0 skipped | 72 s first, 71 s final | 180 s |
| ui-smoke | Passed!, 254 of 254, 0 failed, 0 skipped | 222 s first (beside elevation), 219 s final | 600 s |
| ui-tests (DataFilePickerFolderTests, InverseConstructorTests, KitWindowTests, ScrollExtentTests) | Passed!, 53 of 53 | 32 s first, 31 s final | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0323-dev-invariants.log`) | under 1 s each | 300 s |

- The TRX files under `.artifacts/0323-dev-trx/` show each test in the slice's effects as `Passed` under its exact id.
  This covers the created, changed and renamed facts.
- Counts:
  - constructor-unit-tests: 1551 + 1;
  - ui-smoke: 253 + 1;
  - ui-tests: one fact added (`the kit window's Data step leaves everything reachable`), while the two renames keep
    their counts.
- **Mutations** (`.artifacts/0323-dev-mutation.log`). Five were run, and each was caught by a new or changed fact, with
  the source restored byte-identical:
  - M1: the Main strip keeps Data;
  - M2: leaving the Inverse constructor keeps the Data step;
  - M3: Forward accepts `ShowFlowStep DataFlowStep`;
  - M4: the split verb is always offered;
  - M5: the Data step shows the workbench.
- After the mutations, the solution was rebuilt and every test gate re-run green.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes).

```yaml
gates:
  constructor_unit_tests: 1552
  ui_smoke_tests: 254
  ui_tests: 53
```

## Architecture

- **One list, filtered per constructor.** `ConstructorFlowStep.all` stays 001 §O's whole order.
  - `stepsOf mode` is the one place that decides which steps a constructor shows, and `FlowStripView` draws it.
  - The update keeps the Data step out of the Main constructor in two ways: `ShowFlowStep DataFlowStep` is a no-op
    under `ForwardConstructor`, and `SwitchConstructorMode ForwardConstructor` moves off it.
  - So a Main window can never sit on a step its strip does not show.
- **The Data step keeps the open bay.** `ShowFlowStep DataFlowStep` sets `flowStep` alone. Returning to Setups or
  Experiments then finds the bay as it was, as `ShowFlowStep ItemsFlowStep` does.
- **Moved, not copied.**
  - `attachDataFileButton` and `dataFileRow` live only in `DataStepView`. Their bodies are byte-identical but for the
    brush adapter, which is now `MaterialsWindowView.brush`.
  - `pickDataFile` stays in `TableAndElementRotationView`, where the Items panel's photo also calls it (see Gotchas).
  - The split verb reuses `availableWorkbenchButton`, now `internal`, which is the existing verb box that is disabled
    when withheld. No new button helper exists.
- **The update is the write authority.** The split verb's handler only picks a file. `OpenSplitWizard` guards the
  kit's writes itself (step 321), as `OpenImportWizard` does.

## Deferred

Nothing. Every requirement of the slice landed. The Data step's further panels (the matrix view, badges and the rest)
belong to steps 324–327, by the spec's plan.

## Gotchas

- **`pickDataFile` was not moved** (a recorded deviation from the slice's "MOVE … `pickDataFile`").
  - The slice calls it private, but it is public, and `ItemsPanelView.fs:411` calls it for the item photo.
    `ItemsPanelView.fs` compiles before `FlowStripView.fs`/`DataStepView.fs` and is outside this step's effects.
  - Moving it would break that caller or need an unauthorized edit. So `DataStepView` calls it where it is. Its doc now
    names the Data step's verbs.
- **The slice's "colours from `MaterialsWindowView`".** The moved code takes the brush adapter from there. Its RGB
  values are unchanged, because `MaterialsWindowView`'s idle colours are private and that file is outside the effects.
  The split verb's box has the same colours, padding and margin as `MaterialsWindowView.verbButtonOn`.
- **AGENTS.md's "Main ribbon" gotcha** still says the Experiments bay's "Data files" section is the Inverse
  constructor's alone. That section now lives in the Inverse constructor's Data step. AGENTS.md is outside this step's
  effects, so the line is left for the operator.
- **A kit's experiments are read when it opens.** The new ui-smoke fact writes `E1`, closes the kit and reopens it.
  Writing after opening would not show `E1`.
- **The headless host raises no file picker.** The ui-smoke fact checks that the verbs are present, enabled and
  labelled, and presses neither.
- **Mutation runs leave mutated binaries.** Restoring the source is not enough: the solution was rebuilt before the
  final test runs.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0323: the Inverse constructor's Data step.
  - `DataFlowStep` and `stepsOf` show Data in the Inverse constructor alone.
  - `DataStepView` shows the data files moved from the Experiments bay, and the split verb.
  - The kit window and the flow strip are wired to the step, and three UiIds were added.
  - Tests: +1 constructor (1552), +1 ui-smoke (254) and +1 ui-tests (53); two facts renamed and three changed.
