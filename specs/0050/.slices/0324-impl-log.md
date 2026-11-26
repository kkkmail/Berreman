# Step 0324 — impl log

## Progress

- [x] Read the task, both system prompts, the project prompt and the slice. The task carries no operator note and no
  ruling for this step.
- [x] Production: the three fixed UiIds, the four Domain-derived members, and `DataStepView`'s split panel.
- [x] Tests: new `DataStepTests` (four facts), one compile entry.
- [x] Development runs of the gates: build, elevation, constructor-unit-tests, ui-smoke, ui-tests (selected), the sixteen
  s31 invariants.
- [x] Mutation checks (eight); the final tree rebuilt and re-tested.

## Files modified

- `OpticalConstructor.Controls/UiIds.fs`: in the Data step group (step 323), after `dataSplitFileButton`, each with a
  one-line doc citing spec 0050 S9.3:
  - `dataSplitKeyColumn` (`DataSplitKeyColumn`);
  - `dataSplitApplyButton` (`DataSplitApplyButton`);
  - `dataSplitCancelButton` (`DataSplitCancelButton`).
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: appended after the S9.2 wizard extensions, each with a one-line doc:
  - `type Kit.DataRowKey with member this.uiKey : ConstructorUiKey` (its text);
  - `type ConstructorUiId with` `static member dataSplitGroupRow (key : Kit.DataRowKey)` (`DataSplitGroup_<key>`) and
    `static member dataSplitOfferToggle (key : Kit.DataRowKey)` (`DataSplitOffer_<key>`), both through `key.uiKey`;
  - `type DataSplits.DataSplitError with member this.caption : ControlCaption` (its `describe`).
- `OpticalConstructor.Ui/DataStepView.fs`:
  - `open OpticalConstructor.Domain.DataSplits`.
  - New private `splitGroupRow model dispatch held group`: a `DockPanel` carrying `dataSplitGroupRow group.key`, keyed by
    it, `lastChildFill false`; the label `<key> — <n> point(s) — ` docked left, then
    - `ExperimentForGroup name`: `→ <name>`;
    - `SetupOfferForGroup offer`: the toggle `dataSplitOfferToggle group.key`, labelled
      `Create <experiment> over '<setup>'` plus ` (new setup)` when `model.kitSetups` lacks the setup, an
      `actionButton` on `positiveBackground` while `held.creating` holds the key and a `verbButton` otherwise, both
      sending `ToggleSplitOffer group.key`;
    - `GroupWithoutMatch problem`: a wrapping `unmatched — <problem.caption>` (max width 360) in `messageColor`.
  - New private `dataSplitPanel model dispatch held`: a bordered (grey, 1 px, radius 3, padding 8, left-aligned)
    vertical panel holding the wrapping semi-bold `dataSplitKeyColumn` line
    `Split <leaf> by '<key>' through '<profile>'`, one `splitGroupRow` per group in order, then a horizontal row of
    `Apply split` (`actionButton dataSplitApplyButton … positiveBackground`, `MatchByTemplate`, inside a `Border` that is
    disabled and at 0.4 opacity while `model.kitWrites` is `ControlWithheld`) and `Cancel`
    (`actionButton dataSplitCancelButton … negativeBackground`, `CancelDataSplit`).
  - `dataStepView` appends `dataSplitPanel` under the Split verb's row while `model.dataSplitOpt` is `Some`; its doc
    names the panel.
- `OpticalConstructor.Ui.Tests/DataStepTests.fs` (new): `module DataStepTests` in `namespace OpticalConstructor.Ui.Tests`,
  the four facts the slice names (see Testing state).
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`: one `<Compile Include="DataStepTests.fs" />` line,
  appended at the end of the list (after `SetupGridPanelTests.fs`), no comment.

## Decisions

- **The update is untouched.** The panel only sends `ToggleSplitOffer`, `MatchByTemplate` and `CancelDataSplit`
  (steps 319–320), and no other control or message is added.
- **`(new setup)`** is decided as `splitWritesOver` decides whether Apply records the setup:
  `List.contains offer.setup model.kitSetups`. The suffix is part of the toggle's label, so the toggle reads
  `Create K2 over 'Bench B' (new setup)` as the slice's fact expects.
- **The toggle's two looks** follow `ItemsPanelView.itemsPanelChoice` and the import wizard's `choiceBox`: `actionButton`
  on the positive fill when ticked, `verbButton` otherwise, under the same key. `actionButton` keeps its first handler,
  which is safe because the toggle's message depends on its key alone.
- **Apply split disabled when withheld.** `MaterialsWindowView.actionButton` takes no availability, and
  `MaterialsWindowView.fs` is outside this step's effects. So Apply is wrapped in a `Border` with
  `isEnabled model.kitWrites.isEnabled` and the 0.4 dimming the other withheld verbs use; a disabled ancestor makes the
  action box effectively disabled, so it takes no press. `MatchByTemplate` guards the kit's writes itself (step 320).
  Cancel and the toggles stay enabled: they write nothing.
- **The gap** between Apply split and Cancel is `actionButton`'s own right margin (10 px), as in the import wizard's
  Attach/Cancel row (step 309).
- **The row is a `DockPanel` with `lastChildFill false`** (AGENTS.md: a label beside a wrapping text is docked, never in
  a horizontal `StackPanel`). Without a fill, the offer toggle keeps its caption's width instead of stretching to the
  panel's; the reason text wraps at its max width, as `dataFileRow`'s texts do inside the step's two-way scroll viewer.
- **`DataStepTests` is appended at the end of the compile list** — "appended to the compile list after
  `KitWindowTests.fs`" read as appended (the list's end, which is after `KitWindowTests.fs`). There it reuses
  `SlotBarTests`' internal helpers (`controlById`, `assertAbsent`, `boxLabelOf`, `topLeftOf`, `pressBox`, `latestOf`)
  by call, as `SetupGridPanelTests` does, instead of copying them.
- **Test fixture.** A fresh folder kit opened by `TestAppScope.openedKitIn` under the class's owner (`DataStepTests`) gets
  `Dark` and `E1` through the kit proxy and is closed, then reopened read-write at its folder through `openKitAt`: a
  kit's experiments are read when it opens (step 323).
  `Dark` is written too because `recordExperiment` refuses an experiment whose setup revision the kit lacks
  (`Experiment.itemsUsedBy`), so Apply would stop at `E1` without it. The inverse model is built as
  `KitWindowTests.kitWindowModel` builds it (`initInverseWithKit`, the app's catalogue imports and sources, `loadKit`).
  `phone.csv` (two rows of `E1`) is written in a GUID folder under the test output and deleted at the end.
- **The extra read-only check.** `the split panel names its key column …` also mounts the same split with the kit's
  access read-only (`kitWrites` derives from `kit.access`) and asserts Apply split is not effectively enabled while
  Cancel is. It covers the slice's "disabled when `model.kitWrites` is `ControlWithheld`", and adds no test identity.

## Gotchas

- **`kitWrites` is a member, not a field** (`model.kit.access`): the read-only rendering sets
  `{ model.kit with access = ReadOnlyKitAccess }`.
- **Fixture helpers are private elsewhere.** `KitWindowTests.kitWindowModel` is private and `SlotBarTests.withMainWindow`
  hosts `mainView` alone; both files are outside this step's effects. `DataStepTests` keeps its own few lines that build
  the inverse model and a recording host over `dataStepView`. The kit's creation is `TestAppScope.openedKitIn`, and every
  window query and press is a call into `SlotBarTests`.
- **No operator note or ruling** was carried by this step's task.
- `.manifest.state.json` was modified before the round began. It is the supervisor's file and was not touched.

## Testing state

Development runs of each gate's own command over the final tree. The gate engine runs the gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 78 s first (failed: a test typo), 17 s, 29 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1436) | 226 s first, 216 s final | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552 (unchanged) | 84 s first, 75 s final | 180 s |
| ui-smoke | Passed!, 255 of 255 (254 + 1) | 237 s first, 228 s final | 600 s |
| ui-tests (`DataStepTests`) | Passed!, 3 of 3 | 10 s first, 9 s final | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0324-dev-invariants.log`) | under 1 s each | 300 s |

- The first build failed in the new test file alone (`kitWrites` is a member, not a record field); the production
  projects compiled. The fix sets the kit context's access. The vendored MathNet's two `SYSLIB0051` warnings appeared
  only in that first build, which recompiled `MathNetNumerics/Numerics`; this step does not touch it, and the later
  solution builds printed 0 warnings.
- The TRX files (`.artifacts/0324-dev-trx/`) show each fact of the slice's test effects as `Passed` under its exact id:
  - ui-tests: `the Data step UiIds are the stable intent-named ids`; `the split panel names its key column and lists
    each group matched, offered or unmatched, with Apply and Cancel in one row`; `pressing an offer ticks it, and Cancel
    forgets the split`;
  - ui-smoke: `the Data step renders a split and Apply split attaches its matched group`.
- Counts: ui-smoke 254 → 255 (checkpoint 322's `ui_smoke_tests`); ui-tests over the selected class `DataStepTests`: 3, all
  new; constructor-unit-tests 1552, unchanged (checkpoint 322).
- **Mutations** (`.artifacts/0324-dev-mutation.log`, script `.artifacts/0324-mutation-tools/mutate.py`). Each one
  rebuilt the Ui test project, ran `DataStepTests` under the matching trait, and restored `DataStepView.fs`
  byte-identical; every one was caught:
  - M1, a ticked offer keeps the verb look: `pressing an offer ticks it …` fails.
  - M2, Cancel sends nothing: `pressing an offer ticks it …` fails.
  - M3, no `(new setup)`: both facts reading the label fail.
  - M4, Apply stays enabled over a read-only kit: `the split panel names its key column …` fails.
  - M5, Apply sends `CancelDataSplit`: the ui-smoke fact fails.
  - M6, the reason is not in the message colour: `the split panel names its key column …` fails.
  - M7, the held split is not shown: the ui-smoke fact fails.
  - M8, Cancel left of Apply: `the split panel names its key column …` fails.
- After the mutations, the test fixture's first open became `TestAppScope.openedKitIn`; the solution was rebuilt with
  the gate's command (`DataStepView.fs`'s diff unchanged at 127/14) and every gate above re-run green.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes); `git diff --numstat` equals the `--ignore-cr-at-eol` count.

## Artifacts

- `.artifacts/0324-dev-build.log` and `.artifacts/0324-dev-build-final.log`: the solution builds.
- `.artifacts/0324-dev-elevation.log` and `-final.log`, `.artifacts/0324-dev-invariants.log`.
- `.artifacts/0324-dev-constructor-unit-tests.log` and `-final.log`, `.artifacts/0324-dev-ui-smoke.log` and `-final.log`,
  `.artifacts/0324-dev-ui-tests.log` and `-final.log`, with the TRX files under `.artifacts/0324-dev-trx/`.
- `.artifacts/0324-dev-mutation.log` and its script, `.artifacts/0324-mutation-tools/mutate.py`.
