# Step 0296 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments.

- Step 288 gave experiments scans of axes and blocks.
- Step 292 added the Add variable and Add block messages.
- Step 294 made role, session and notes editable by message.
- Step 295 gave each experiment one data badge.

This step makes the Main Experiments bay show and build v2:

- the draft's scan lines, with Add variable and Add block;
- the loaded experiment's role, session and notes;
- each collected row's name, role and data badge.

All of it sits under stable `ConstructorUiId`s, and the button and the test send the same message.

## What's working

- The Experiments bay lists the draft's closed blocks and held axes under the range, with Add variable and Add block
  beside Add and New.
- A loaded experiment in a writable kit shows a role row of seven boxes and Session / Notes fields. They send
  `SetExperimentRole`, `SetExperimentSession` and `SetExperimentNotes` for that experiment.
- Each collected row reads `<name> [<role>] — <description>` and shows its data badge text.
- Added `ExperimentRoleMark`, `ExperimentRoleOption` and seven new bay ids. In the Ui: `ExperimentRole.uiKey` /
  `.caption`, plus `SessionName.caption` and `ExperimentNotes.caption`.
- Added four ui-tests and one ui-smoke fact. The Inverse scroll-extent fact now scrolls a bay with an experiment
  loaded.

## Tests

Development runs used the gates' own commands and this step's selectors, over the final build. The arc-runner's gate
engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 9 s (incremental; the first full build took 76 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 206 s | 600 s |
| constructor-unit-tests | Passed!, 1495 of 1495, 0 failed, 0 skipped | 69 s | 180 s |
| ui-smoke | Passed!, 250 of 250, 0 failed, 0 skipped | 206 s | 600 s |
| ui-tests (7 selected classes, the gate's exact command) | Passed!, 124 of 124, 0 failed, 0 skipped | 133 s | 360 s |
| invariant-s29-\* (16) | every decoded `git grep` finds nothing | < 1 s each | 300 s |

- **constructor-unit-tests: 1495**, unchanged. Nothing in that project changed.
- **ui-smoke: 250**, which is 249 plus `pressing Add variable between two axes and Add records one two-variable
  experiment`.
- **ui-tests: 124** over this step's seven classes: ElementMenu 15, ExperimentControls 43, FsxAcceptance 14,
  InverseConstructor 14, KitWindow 16, ScrollExtent 14 and WorkbenchInfoAndCapture 8.
  - ExperimentControls is 39 plus this step's four new facts:
    - `the bay's scan, role, session and notes UiIds are the stable intent-named ids`;
    - `Add variable and Add block list the held axes and blocks under the range`;
    - `an experiment loaded in the editor shows its role, session and notes, and a press on a role sends it`;
    - `each collected experiment's row shows its name, its role and its data badge`.
  - The changed `Inverse: the Experiments bay with its data-file section leaves everything reachable` passes.
  - Step 0295's selection had five classes (89). This one adds ElementMenu (15) and KitWindow (16): 89 + 15 + 16 + 4
    = 124.
- **Elevation's first run found one ELEV013P.** The private `scanSummary` took only the state. It became a member
  (see Architecture), and the rerun is clean.
- **Mutation check (red).** A constant key on the bookkeeping block failed the loaded-experiment fact: `Expected:
  DarkRun`, `Actual: CalibrationRun`, because a stale box subscription wrote to the previously loaded experiment. The
  file was restored byte for byte (SHA-256 checked) and rebuilt, and the suites were rerun green.

```yaml
gates:
  constructor_unit_tests: 1495
  ui_smoke_tests: 250
  ui_tests: 124
```

## Architecture

- **The bay stays domain-free.** `TableAndElementRotationModel.experimentState` projects the Domain draft and
  experiments into captions, marks and availabilities. `experimentHandlers` resolves a pressed role code against
  `ExperimentRole.all` by `uiKey` and sends the step-294 messages for `draft.editingId`.
- **Scan lines.**
  - Each closed block reads `Block <k>: <axes' describes joined by " × ">, capture <capture label>`, or `Block <k>:
    Dark line, capture <label>` for a block of no axis.
  - The held axes read `This block: <their describes joined by " × ">`.
- **`scanAvailability` is `addAvailability`, as the slice says.** The buttons follow the kit's writability and the
  detectors' capture, though step 292's messages also accept a read-only kit.
- **`bookkeepingAvailability` is offered only while the draft edits a collected experiment and the kit is writable.**
  - While it is withheld, the role row and fields are hidden.
  - `roleOptions` is empty while nothing is loaded, so no role box exists.
- **The bookkeeping block is keyed by the loaded row's id** (`ExperimentBookkeeping_<id>`).
  - The TextBoxes' key handlers never re-subscribe (FuncUI's default), and a role box re-subscribes only when its
    colour changes. Without the key, either one could write to the experiment loaded before.
  - Recreating the block on a load gives every control a subscription for the experiment it edits. The mutation check
    proves the key is load-bearing.
- **The scan summary is `member internal this.scanSummaryPanel : IView`** on `ExperimentControlsState`. A private free
  function of the state alone is ELEV013P. This is the member pattern `ItemsPanelView` uses.
- **`numberField` now delegates to a private `committingField`.** It takes the width as an attribute, so the Session
  and Notes fields reuse the same Enter / blur commit. Its behaviour is unchanged.
- **The row badge is the Domain `ExperimentDataBadge.text`** through `model.dataBadgeOf` (`no data`, `mapped`, …), as
  the slice says. The data-file section keeps showing the longer Ui `caption`.

## Deferred

Nothing. Every requirement of the step landed in this round.

## Gotchas

- **The block text repeats Domain logic.**
  - Step 288's block text lives in a private `blockText` inside `Experiment.description` (Domain `Experiments.fs`),
    which is outside this step's authorized edits.
  - `experimentState` builds the same form from the public `ScanAxis.describe` and `MeasurementMode.label`: about three
    lines.
  - This follows step 0274's precedent for a file outside the effects. A later step that may edit `Experiments.fs` can
    expose `ScanBlock.text` and point both at it.
- **`SessionName.caption` and `ExperimentNotes.caption` go beyond the listed contracts.**
  - The project's rule requires a cross-type reuse to be a member on the source type, so they are in the authorized
    `ConstructorUiIdOfDomain.fs`.
  - `ExperimentNotes.caption` sits opposite the view's private `ControlCaption.toExperimentNotes` (step 294).
    Elevation does not flag the pair.
- **The loaded-experiment fact asserts more than the slice's minimum, all inside the new fact.** After the Calibration
  press, it also:
  - types a session and notes (Enter commits);
  - records E2;
  - loads E1 straight from its row;
  - checks that a Dark press goes to E1 and E2 stays Science.
  That is the stale-closure guard above.
- **The Inverse scroll-extent fact also dispatches `ExpAddScanAxis` after `ExpEdit`.** That way the scan summary
  holds a line. Three precondition asserts pin the role row (7 options), bookkeeping offered and one scan line. Its
  `assertReachable` is kept.
- **`committedPolarizer` commits in the Main scratch kit.** The template cannot name that setup, so the experiment is
  `E1`, which the row fact asserts.
- **A full rebuild that recompiles the vendored MathNet** shows its two known `SYSLIB0051` warnings
  (`specs/0050/.manual/004`). An incremental build shows none.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0296: the Experiments bay reads v2.
  - Scan lines with Add variable and Add block.
  - The loaded experiment's role row, Session and Notes, sending `SetExperimentRole` / `SetExperimentSession` /
    `SetExperimentNotes`.
  - Each row's name, role and data badge.
  - Seven new `ConstructorUiId`s and `ExperimentRole.uiKey` / `.caption`.
  - Four ui-tests and one ui-smoke fact. The Inverse scroll-extent fact loads an experiment.
