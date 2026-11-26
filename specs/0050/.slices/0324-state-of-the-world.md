# Step 0324 — state of the world

## Where we are

This step is in spec 0050 section S9.3: one file serving many experiments, and the Data step (001 §K, §O).

- Steps 317–321 split one lab file into groups by its key column and matched each group to an experiment, an offer of a
  setup and experiment, or nothing (`SplitByColumn`, `ToggleSplitOffer`, `CancelDataSplit`, `MatchByTemplate`), with no
  surface showing the held split. Step 323 added the Inverse constructor's Data step and its Split verb.
- This step shows the held split in the Data step: its key column, each group matched, offered (with its toggle) or
  unmatched (with its reason), and Apply split beside Cancel (.spec-md §10.3 identify → confirm, §10.5).
- Steps 325–327 add the step's further panels, the matrix view (step 327) among them.

## What's working

- Added the Data step's split panel under the Split verb: the file, key column and profile, then one row per group.
- A matched group reads `→ <experiment>`, an unmatched one its reason in the message colour.
- An offered group's toggle reads `Create <experiment> over '<setup>'`, plus ` (new setup)` when the kit lacks the setup.
  It sends `ToggleSplitOffer` and sits on the positive fill while ticked.
- Apply split (positive, `MatchByTemplate`, disabled over a read-only kit) and Cancel (negative, `CancelDataSplit`)
  share one row with a gap.
- Added the UiIds `DataSplitKeyColumn`, `DataSplitApplyButton`, `DataSplitCancelButton`, `DataSplitGroup_<key>` and
  `DataSplitOffer_<key>`, with `DataRowKey.uiKey` and `DataSplitError.caption`.
- Tests: new `DataStepTests`, with three ui-tests facts and one ui-smoke fact (ui-smoke 254 → 255).

## Tests

These are development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 29 s final | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded | 226 s first, 216 s final | 600 s |
| constructor-unit-tests | Passed!, 1552 of 1552, 0 failed, 0 skipped | 84 s first, 75 s final | 180 s |
| ui-smoke | Passed!, 255 of 255, 0 failed, 0 skipped | 237 s first, 228 s final | 600 s |
| ui-tests (DataStepTests) | Passed!, 3 of 3 | 10 s first, 9 s final | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` (`.artifacts/0324-dev-invariants.log`) | under 1 s each | 300 s |

- The TRX files under `.artifacts/0324-dev-trx/` show the four facts of the slice's test effects as `Passed` under their
  exact ids.
- Counts: ui-smoke 254 + 1; the selected ui-tests class holds 3 new facts; constructor-unit-tests unchanged at 1552.
- **Mutations** (`.artifacts/0324-dev-mutation.log`). Eight were run on `DataStepView.fs`, each caught by a new fact, with
  the source restored byte-identical: the ticked look, Cancel's message, `(new setup)`, Apply's read-only disabling,
  Apply's message, the reason's colour, the panel's presence and the Apply/Cancel order.
- After the mutations, the solution was rebuilt and every gate re-run green.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes).

```yaml
gates:
  constructor_unit_tests: 1552
  ui_smoke_tests: 255
  ui_tests: 3
```

## Architecture

- **A pure projection of the held split.** The panel reads `model.dataSplitOpt`, `model.kitSetups` and `model.kitWrites`
  and sends the three existing split messages. The update, which steps 319–320 test as pure model behaviour, is
  untouched.
- **One rule for "(new setup)".** The view asks what Apply asks: whether `kitSetups` holds the offer's setup
  (`splitWritesOver`, step 320). So the label says exactly when Apply would record a new setup file.
- **Shared looks, no new button helper.** The toggle and the action row use `MaterialsWindowView.actionButton` and
  `verbButton`, the chosen-option look of the import wizard and the Items panel. Apply's withheld state is a disabled,
  dimmed wrapper `Border` around the shared action box, because `actionButton` takes no availability.
- **Ids by key.** A group's row and toggle are keyed and automation-id'd by the group's `DataRowKey` through
  `DataRowKey.uiKey` (`DataSplitGroup_<key>`, `DataSplitOffer_<key>`), as `attachDataFile` is keyed by its experiment.

## Deferred

Nothing. Every requirement of the slice landed. The Data step's further panels (the matrix view, badges and the rest)
belong to steps 325–327, by the spec's plan.

## Gotchas

- **`DataStepTests` sits at the end of the compile list**, after `KitWindowTests.fs` and the three files that follow it.
  The slice says "appended to the compile list after `KitWindowTests.fs`", read here as appended at the end, which lets
  the class call `SlotBarTests`' internal window helpers instead of copying them (as `SetupGridPanelTests` does).
- **A kit records an experiment only over a setup it holds.** The smoke fixture writes `E1`'s `Dark` setup beside `E1`.
  Otherwise `MatchByTemplate` stops at `E1` with "Not saved to the kit".
- **A kit's experiments are read when it opens**, so the fixture writes `E1`, closes the kit and reopens it read-write.
- **`kitWrites` derives from `kit.access`.** A read-only rendering sets the kit context's access, not a model field.
- **Apply's disabled state is on its wrapper.** The `DataSplitApplyButton` box itself keeps `IsEnabled` true and is
  `IsEffectivelyEnabled` false under a read-only kit; a check of the withheld Apply must read `IsEffectivelyEnabled`.
- **AGENTS.md's "Main ribbon" line** still places the "Data files" section in the Experiments bay (noted at step 323).
  AGENTS.md is outside this step's effects.
- `.manifest.state.json` was already modified when the round started. It is the supervisor's file and was not touched.

## Changelog

- 2026-11-12 — Step 0324: the Data step's split panel.
  - The held split's key column, its groups matched, offered (with a toggle) or unmatched, and Apply split beside
    Cancel, under five new UiIds.
  - Tests: new `DataStepTests` (+3 ui-tests, +1 ui-smoke to 255).
