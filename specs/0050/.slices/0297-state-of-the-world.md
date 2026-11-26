# Step 0297 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.

- Step 288 gave experiments scans of axes and blocks; step 292 added Add variable and Add block.
- Step 294 made role, session and notes editable by message; step 295 gave each experiment one data badge.
- Step 296 made the Main Experiments bay show and build all of it under stable ids.

This step closes the section's acceptance (001 §K; 001 §E notes in the files). It proves, through the kit window over
a real kit folder that is closed and reopened, that a two-variable experiment recorded through the bay keeps its scan,
role, session and notes in its file. It also records the experiment model in `AGENTS.md`. No product code changes.

## What's working

- Add the ui-smoke fact `a two-variable experiment recorded through the bay keeps its scan, role, session and notes in
  its kit file` to `KitWindowTests`. It presses the bay by id in a kit window over a kit folder and reads `E1` back
  from the reopened folder: one R2 × R1 block over the sample, `CalibrationRun`, session `Day 1`, notes `LR re-seated`.
- In the window, the new row's badge `ExperimentBadge_1` reads `no data`, and no kit write is refused.
- Add the `AGENTS.md` gotcha **An experiment is a scan over a setup revision**: blocks of axes, the product order, the
  template name as the file's stem, role / session / notes and the mapped attachment in the file, and
  `ExperimentDataBadge.ofCheck`.

## Tests

Development runs used the gates' own commands and this step's selectors, over the final build. The arc-runner's gate
engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no `error` in stdout | 16 s (incremental; the first full build took 77 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 206 s | 600 s |
| ui-smoke | Passed!, 251 of 251, 0 failed, 0 skipped | 223 s | 600 s |
| ui-tests (`KitWindowTests`, the gate's exact command) | Passed!, 16 of 16, 0 failed, 0 skipped | 23 s | 360 s |
| invariant-s29-\* (16) | each decoded `git grep` finds nothing (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **ui-smoke: 251**, which is 250 plus the new fact. The TRX shows it `Passed`.
- **ui-tests: 16** over `KitWindowTests`, unchanged; the new fact carries the ui-smoke trait.
- **Elevation ran before a test-only rename** (`alongOf` → `slotOfKind`, the inner `sample` → `sampleSlot`). Test
  projects are outside the analyzers' scope, so the result stands. Build, ui-tests and ui-smoke ran after the rename.
- **Mutation check (red).** `SetExperimentNotes` was made to keep the held notes. The new fact failed with `Expected:
  ExperimentNotes "LR re-seated"`, `Actual: ExperimentNotes ""`. The product file was restored byte for byte (SHA-256
  checked) and the solution rebuilt before the runs above.

```yaml
gates:
  ui_smoke_tests: 251
  ui_tests: 16
```

## Architecture

- No product change. The fact drives the window composition steps 242 and 296 already ship: `kitWindowView` shows
  `mainView`, whose Experiments bay holds the candidates, variables, Add variable, Add, the role row and the
  Session / Notes fields.
- The table is set up by model messages, as step 284's fact sets it up (`AddElementBoundTo`, `ShowFlowStep`). Every
  bay action after that is a press or keystroke on a control found by its `ConstructorUiId`.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The bay's committing fields carry their id as `Name`, not as an AutomationId.** `ExperimentControls.committingField`
  (the range fields, Session and Notes) sets `TextBox.name id.value`. `KitWindowTests.controlById` matches AutomationIds
  only, so the fact finds the fields through `namedControl`. The first run failed on exactly that
  (`ExperimentSessionField must be in the window`). Product files were outside this step's edits.
- **The experiment is `E1`, and the setup `Setup 1`.** The standard template cannot name a table with no item in Prep 1.
  `ExpCommit` records the table as a new setup when none is active, then the experiment over it.
- **After Add, the bay edits the new experiment** (`draft.editingId`), so the role row and the Session / Notes fields
  appear for it with no Edit press. The fact relies on that, as step 296's facts do.
- **The sample is appended after `det` in `elements`**, but `addElement` places it on the main beam between the source
  and the detector. The fact asserts the x order, not the list order.
- **The slice says "after the gotcha 'Names come from…'"**, as step 284's slice did. The new bullet sits directly after
  it, so it comes before step 284's slot-bar bullet.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0297: S9.1's acceptance in a kit window. A two-variable experiment recorded through the bay, with
  role, session and notes, reads back from the reopened kit folder; the row badge reads `no data`. `AGENTS.md` gains
  **An experiment is a scan over a setup revision**. ui-smoke rises by one; the selected ui-tests are unchanged.
