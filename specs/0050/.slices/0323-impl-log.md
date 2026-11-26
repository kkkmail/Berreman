# Step 0323 — impl log

## Progress

- [x] Read the task, both system prompts, the project prompt and the slice. The task carries no operator note and no
  ruling for this step.
- [x] Production: `ConstructorFlowStep.DataFlowStep` / `stepsOf`, the update rules, `DataStepView.fs`, the kit
  window, the flow strip, the UiIds.
- [x] Tests: `KitWindowModelTests`, `KitWindowTests`, `ScrollExtentTests`.
- [x] Development runs of the gates: build, elevation, constructor-unit-tests, ui-smoke, ui-tests (selected), the sixteen
  s31 invariants.
- [x] Mutation checks (five, all caught); the final tree rebuilt and re-tested.

## Files modified

- `OpticalConstructor.Ui/TableAndElementRotationView.fs`
  - `ConstructorFlowStep` gains `DataFlowStep`, after `ExperimentsFlowStep`. Its caption is `Data` and its
    `flowUiId` is `flowDataStep`. `all` lists Items, Setups, Experiments and Data.
  - New `static member stepsOf (mode : ConstructorMode) : ConstructorFlowStep list`. Inverse is `all`; Forward is
    `all` without `DataFlowStep`.
  - `ShowFlowStep DataFlowStep` sets `flowStep` alone under `InverseConstructor`, and changes nothing under
    `ForwardConstructor`.
  - `SwitchConstructorMode ForwardConstructor` from `DataFlowStep` also sets `flowStep = ExperimentsFlowStep` and
    `ribbon = BayNames.experiments` (a new guarded case ahead of the unchanged general case).
  - `attachDataFileButton` and `dataFileRow` are deleted here (moved).
  - `collectionBuilderView` loses its `Data files:` section and its mode match. Its doc and the section comment above
    say the data files are the Data step's.
  - `pickDataFile`'s doc names the Data step's verbs.
  - `availableWorkbenchButton` goes from `private` to `internal`, so the Data step's split verb reuses it, and its doc
    names that verb.
  - `buildDataFilePickerOptions` stays where it is.
- `OpticalConstructor.Ui/DataStepView.fs` (new): `module OpticalConstructor.Ui.DataStepView`. Its module doc cites
  001 §O and spec 0050 S9.3. It holds:
  - `attachDataFileButton` and `dataFileRow`, moved. The bodies are byte-identical but for `brush` →
    `MaterialsWindowView.brush`, checked by script against the cut text kept in
    `.artifacts/0323-moved-functions.txt`.
  - `dataStepView`: a `ScrollViewer` (both bars `Auto`) around a vertical `StackPanel`, which carries
    `dataStepPanel` with margin 8. It holds:
    - the heading `Data files:`;
    - the `experimentDataFiles` panel, with one `dataFileRow` per experiment, or the hint when there is none;
    - a row with `Split a file by key…` (`dataSplitFileButton`). It is `availableWorkbenchButton model.kitWrites`, and
      a confirmed `pickDataFile` sends `OpenSplitWizard file`.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile Include="DataStepView.fs" />` line, directly
  after `FlowStripView.fs`, no comment.
- `OpticalConstructor.Ui/KitWindowView.fs`: `DataFlowStep` shows `DataStepView.dataStepView` in a `Border` keyed
  `data`. The doc names the Data step.
- `OpticalConstructor.Ui/FlowStripView.fs`: one box per step of `ConstructorFlowStep.stepsOf model.constructorMode`,
  and the doc says so.
- `OpticalConstructor.Controls/UiIds.fs`: a new group after the flow strip's, under one comment line citing spec 0050
  S9.3: `flowDataStep` (`FlowDataStep`), `dataStepPanel` (`DataStepPanel`) and `dataSplitFileButton`
  (`DataSplitFileButton`), each with a one-line doc. The doc of `experimentDataFiles` names the Data step.
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: the shared doc of `attachDataFile` and `dataFileStatus` names
  the Inverse constructor's Data step.
- `OpticalConstructor.Tests/KitWindowModelTests.fs`:
  - `the flow strip's steps follow 001's order and a window opens on Setups` now lists Data and its caption, with its
    other assertions kept;
  - new `the Data step is the Inverse constructor's alone, and leaving the Inverse constructor leaves it`.
- `OpticalConstructor.Ui.Tests/KitWindowTests.fs`:
  - three changed facts;
  - new `[<Trait("Category", "ui-smoke")>]` `pressing the Data step shows the data files and the Split a file by key
    button`.
- `OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`:
  - two renamed facts;
  - new `the kit window's Data step leaves everything reachable`.

## Decisions

- **`pickDataFile` stays in `TableAndElementRotationView`** (a deviation from the slice's "MOVE … `pickDataFile`").
  - The slice calls it private, but it is public. `ItemsPanelView.fs:411` uses it for the Items panel's photo, and
    `ItemsPanelView.fs` compiles before `FlowStripView.fs`/`DataStepView.fs`.
  - Moving it would break that caller, or would need an edit outside this step's effects (`ItemsPanelView.fs`) and a
    compile order the slice does not give.
  - So `DataStepView` calls it: a call, not a copy (AGENTS.md "Reuse, never copy"). The two private functions moved
    as the slice asks.
- **"Colours from `MaterialsWindowView`".** The moved bodies used `TableAndElementRotationView`'s private `brush`.
  They now use `MaterialsWindowView.brush`, the adapter the import wizard's view uses, with the same RGB values. No
  helper is copied. `MaterialsWindowView`'s idle colours are private, and that file is outside this step's effects.
- **The split verb reuses `availableWorkbenchButton`** (made `internal`). It is the existing verb box that is
  disabled and dimmed, but never hidden, when withheld. Writing another such box would copy the attach verb's body.
  - The box's colours, padding and margin match `MaterialsWindowView.verbButtonOn`'s.
  - Withheld, the box is disabled and takes no press, and `OpenSplitWizard` guards the kit's writes itself
    (`guardKitWrite`, step 321). So its handler checks no availability, and it captures no stale `kitWrites` under
    `OnChangeOf id`.
- **`SwitchConstructorMode`.** It is a guarded case
  (`SwitchConstructorMode ForwardConstructor when model.flowStep = DataFlowStep`) ahead of the general one, which is
  unchanged.
  - The new model fact pins that the other switches keep the step and the bay: Inverse→Forward from Setups/Details,
    and Forward→Inverse.
- **`a read-only window withholds every write control`.**
  - `writeControlIds` drops `attachDataFile E1`, because the workbench no longer shows it. The `mainView` window now
    asserts that `attachDataFile E1` is absent.
  - The new `dataWriteControlIds` (`attachDataFile E1`, `dataSplitFileButton`) are checked in a `dataStepView`
    window mounted over the same model: disabled read-only, enabled read-write.
- **`the flow strip shows …`.**
  - `onStep` became `onStepOf model`, with `onStep = onStepOf loaded` for the Main window. The strip's ids and the
    literal labels come per constructor, so the literal `[ "Items"; "Setups"; "Experiments" ]` assertion is kept.
  - The fill check runs over `stepsOf` the window's constructor: the Main window has no Data box to read.
  - Each Main step asserts that no `flowDataStep` exists.
  - The inverse window on Experiments finds the Data box below Experiments.
  - On `DataFlowStep`, it finds `dataStepPanel` as the surface (right of the strip), with no `tableElementCanvas`,
    Experiments pane or Items panel.
- **The new ui-smoke fact.** It writes `E1` into a fresh kit, then reopens it read-write: a kit's experiments are read
  when it opens.
  - It presses `flowDataStep` in the inverse kit window. It then finds `dataStepPanel` visible, the Data box filled,
    the table gone, and `attachDataFile E1` and `dataSplitFileButton` visible, enabled and labelled.
  - It presses no picker verb: a headless host raises no file picker.
- **`the Main window opens on Add; … Data step alone`.**
  - `texts` now hosts a given view through `viewWindow`.
  - The forward and inverse `mainView` texts hold neither `Data files:` nor `Attach data file…`.
  - `dataStepView` over the inverse model holds both.
  - `stepsOf ForwardConstructor` lacks `DataFlowStep`.

## Gotchas

- **The deviation.** `pickDataFile` was not moved (see Decisions): the slice calls it private, but it is public and
  `ItemsPanelView.fs` calls it.
- **AGENTS.md is stale on one line.** Its "Main ribbon" line still places the "Data files" section in the Experiments
  bay. AGENTS.md is outside this step's effects, so the line is left for the operator.
- **No operator note or ruling** was carried by this step's task.
- `.manifest.state.json` was modified before the round began. It is the supervisor's file and was not touched.

## Testing state

Development runs of each gate's own command. The gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 25 s first, 17 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1435) | 210 s | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552 (1551 + 1) | 72 s first, 71 s final | 180 s |
| ui-smoke | Passed!, 254 of 254 (253 + 1) | 222 s first (run beside elevation), 219 s final | 600 s |
| ui-tests (the four selected classes) | Passed!, 53 of 53 | 32 s first, 31 s final | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- The TRX files (`.artifacts/0323-dev-trx/`) show every fact in the slice's test effects as `Passed` under its exact
  id:
  - constructor-unit-tests:
    - `the flow strip's steps follow 001's order and a window opens on Setups`;
    - `the Data step is the Inverse constructor's alone, and leaving the Inverse constructor leaves it`.
  - ui-smoke: `pressing the Data step shows the data files and the Split a file by key button`.
  - ui-tests:
    - `the flow strip UiIds are the stable intent-named ids`;
    - `the flow strip shows Items, Setups and Experiments left of the surface its step shows`;
    - `a read-only window withholds every write control`;
    - `Inverse: the Experiments bay leaves everything reachable`;
    - `the Main window opens on Add; the Attach data file section belongs to the Inverse window's Data step alone`;
    - `the kit window's Data step leaves everything reachable`.
- Counts. The test diff adds three facts:
  - one to constructor-unit-tests (1551 → 1552, checkpoint 321);
  - one ui-smoke fact (253 → 254, checkpoint 320's `ui_smoke_tests`);
  - one ui-tests fact in `ScrollExtentTests`, so the selected classes rise from 52 to 53.

  The two renames keep their counts.
- `DataFilePickerFolderTests` and `InverseConstructorTests` pass as written (in the selected ui-tests run).
- **Mutations** (`.artifacts/0323-dev-mutation.log`, script `.artifacts/0323-mutation-tools/mutate.py`). Each one
  rebuilt the test project, ran the affected class, and restored the source byte-identical:
  - M1, `stepsOf ForwardConstructor` keeps Data: the new model fact fails.
  - M2, leaving the Inverse constructor from Data keeps `DataFlowStep`: the new model fact fails.
  - M3, `ShowFlowStep DataFlowStep` under Forward sets the step: the new model fact fails.
  - M4, the split verb is always offered: `a read-only window withholds every write control` fails.
  - M5, the kit window's Data step shows the workbench: the new ui-smoke fact fails.
- After the mutations, the solution was rebuilt with the gate's command. The Ui assembly's timestamp (09:12:11) is
  later than the restored source's (09:11:50). The three test gates were re-run green, as above.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes); `git diff --numstat` equals the `--ignore-cr-at-eol` count.
- The vendored MathNet's two `SYSLIB0051` warnings appeared only in the first Ui-project compile, which rebuilt
  `MathNetNumerics/Numerics`. This step did not touch it. The solution builds printed 0 warnings.

## Artifacts

- `.artifacts/0323-moved-functions.txt`: the two functions as cut from `TableAndElementRotationView.fs`.
- `.artifacts/0323-dev-build-ui.log`: the first compile check of the Ui project.
- `.artifacts/0323-dev-build.log`: the solution build.
- `.artifacts/0323-dev-build-final.log`: the solution build after the mutations.
- `.artifacts/0323-dev-elevation.log` and `.artifacts/0323-dev-invariants.log`.
- `.artifacts/0323-dev-constructor-unit-tests.log` and `-final.log`, `.artifacts/0323-dev-ui-smoke.log` and
  `-final.log`, `.artifacts/0323-dev-ui-tests.log` and `-final.log`, with the TRX files under `.artifacts/0323-dev-trx/`.
- `.artifacts/0323-dev-mutation.log` and its script, `.artifacts/0323-mutation-tools/mutate.py`.
