# Step 0288 — state of the world

## Where we are

Spec 0050 section S9.1 covers Experiments v2: scans, roles, sessions and attachments. Steps 285–287 laid the run's
role, session, notes and name, and the pure scan shapes. This step lands Experiment v2 itself:
- an `Experiment` now holds a `scan`, a `role`, a `sessionOpt` and `notes` in place of `varied`, `measurement`,
  `range` and `sourceWaveLengthOpt`;
- the draft builds a scan of any number of axes and blocks;
- the experiment file round-trips it, still under schema version 1.

Step 293 will draw every scan. Until then, only a scan of one block of one axis is charted.

## What's working

- Make `Experiment` hold a `scan`, a `role`, a `sessionOpt` and `notes`. `VariedElement` and `variedLabel` are deleted;
  `ScanAxis` succeeds them.
- `description` keeps today's single-axis and dark-line texts, and joins a multi-block scan's blocks with `; `.
- The draft gains `outerAxes`, `closedBlocks`, `axisOpt` and `scanOpt`. `addScanAxis` and `addScanBlock` build a
  multi-variable scan; `edit` splits it back and `commit` gives it back.
- A committed experiment is a `ScienceRun` with no session and `ExperimentNotes.none`. An edited one keeps its role,
  session, notes and data file.
- `requestedWavelengthsOf` spans every wavelength axis. `loadAndValidate` validates a one-axis scan as before and
  refuses a multi-variable scan for an X,Y file.
- The experiment schema now requires `scan`, `role` (1–7), `sessionOpt` and `notes`. Every fixture is re-pointed.

## Tests

These are development runs on the final tree, using the snapshot's commands and selectors. The arc-runner's gate
engine runs the gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors (final incremental build) | 27 s | 600 s |
| constructor-unit-tests | Passed!, 1483 of 1483, 0 failed, 0 skipped | 75 s | 180 s |
| ui-smoke | Passed!, 249 of 249, 0 failed, 0 skipped | 206 s | 600 s |
| ui-tests (10 selected classes) | Passed!, 132 of 132, 0 failed, 0 skipped | 134 s | 360 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 195 s | 600 s |

- **constructor-unit-tests.** 1483 is the last checkpoint's 1480 plus the three new facts. The TRX shows `Passed`
  for each new and changed fact:
  - new: `a committed experiment is a one-axis scan of the draft, a science run with no session and no notes`
  - new: `an added axis and an added block commit as one multi-variable scan, and editing splits it back`
  - new: `loading data validates a one-axis scan as before and refuses an X,Y file for a multi-variable scan`
  - changed: `an experiment file round-trips through its canonical text`
  - changed: `a schema-invalid or malformed experiment file is refused before it binds`
  - changed: `requestedWavelengthsOf: a wavelength-sweep experiment requests its range; a dark line is fixed`
- **ui-tests.** Every class ran: ElementMenu 15, ExperimentControls 35, FsxAcceptance 14, InverseConstructor 14,
  KitWindow 16, MainWorkbench 5, OutOfBandBadge 2, ScrollExtent 14, SolverHandoffWindow 9 and
  WorkbenchInfoAndCapture 8. This step adds and removes no UI test, so the count is unchanged.
- **Extra check, not a gate.** LauncherTests and KitStartWindowTests sit outside the selection but had their
  fixtures re-pointed. Their non-smoke facts pass, 16 of 16.
- **invariant-s29-\*** (16): every decoded `git grep` finds nothing. `git grep VariedElement|variedLabel|\.varied\b`
  also finds nothing.
- **Mutation check (red).** Two mutations were built and the two classes run:
  - `edit` taking the last block's FIRST axis as the edited one turned the multi-variable fact red;
  - `loadAndValidate` reading `axis :: _` as one axis turned the data-load fact red (40 total, 2 failed).
  The files were then restored byte for byte (SHA-256 checked), rebuilt, and every suite above re-ran green.
- **First elevation run.** It found two ELEV013 findings (`addScanAxis` and `addScanBlock` as free functions of one
  `ExperimentCollection`). They were fixed by making both members; see Architecture.

```yaml
gates:
  constructor_unit_tests: 1483
  ui_smoke_tests: 249
  ui_tests: 132
```

## Architecture

- **`addScanAxis` and `addScanBlock` are members of `ExperimentCollection`** (`c.addScanAxis`, `c.addScanBlock`),
  beside `newDraft`. They are not free functions of the module.
  - The slice's contract names `Experiments.addScanAxis (c : ExperimentCollection) : ExperimentCollection`. ELEV013
    refuses a free function whose only parameter is one of our types, and AGENTS.md rules that the analyzers win
    (spec 0048 `.manual/002` R6).
  - The member keeps the contract's behaviour and name. Precedent in this arc: steps 178, 225 and 234.
- **`addScanBlock` reuses `addScanAxis`.** It moves the edited axis into the outer axes, then closes them on the
  draft's capture, so the four-field clear exists once.
- **`commit` with no edited axis commits nothing, even while editing.** Editing a dark line opens a draft with no
  axis, and committing it leaves the experiment unchanged. So "editing then committing gives the experiment back"
  holds for every scan the draft can split. The bay already withholds Add on `canCommit = false`.
- **`requestedWavelengthsOf` reuses `requestedWavelengthsFor`.** It builds a spanning `VariableRange`: the first
  wavelength axis's range with the lowest `min` and the highest `max`. The nm-to-`WaveLength` crossing stays in one
  place.
- **`loadAndValidate` resolves the detector inside its accepted branches**, through a local `validateOver`. A
  multi-variable scan therefore reads neither the library nor the data file.
- **Schema.** Version 1 keeps its shape rules:
  - `scan` is `{ blocks : array, sourceWaveLengthOpt }` with no other key;
  - `role` is an integer from 1 to 7, which `ExperimentRole`'s existing `ClosedSetTagConverter` writes;
  - `sessionOpt` is `null` or a string matching `\S`;
  - `notes` is a string.
  The blocks' inner shape is the binder's to refuse, as `range` was before; the malformed-file fact covers it.

## Deferred

Nothing. Every requirement of the slice landed this round. Charting a scan of more than one axis or block is step
293's, as the slice states.

## Gotchas

- **`Experiments.ExperimentNotes.none` does not compile** (FS0039 `'none' is not defined`). With a module
  qualification, the name resolves to the union case `ExperimentNotes` rather than the type. The full namespace
  path fails the same way.
  - Five test files that wrote `Experiments.` everywhere now `open OpticalConstructor.Domain.Experiments` right after
    `open OpticalConstructor.Domain`, so every later open keeps precedence: KitStartTests, KitWindowModelTests,
    SetupTests, KitStartWindowTests and KitWindowTests.
  - Any type whose name equals its single case has the same trap: `ExperimentId`, `ElementLabel`, `SessionName`.
- **Fixtures outside the listed files.** The closing build found five more fixtures, re-pointed by slice §6:
  KitOpeningTests, KitStartTests, KitStartWindowTests, LauncherTests and ElementMenuTests.
- **Fixtures that needed no change.** SolverHandoffWindowTests and ScrollExtentTests only copy-update `id` or
  `dataFileOpt`, or build no experiment.
- **The old schema-invalid edit `experiment.notes = "unexpected"` is now valid**, because `notes` is a field. It
  became `experiment.unexpected`.
- **The Bash tool breaks a quoted heredoc that holds an apostrophe.** Use the Edit tool for multi-line test edits.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11: Step 0288 lands Experiment v2.
  - An experiment holds a scan, a role, a session and notes; `VariedElement` is deleted.
  - The draft builds and splits multi-axis, multi-block scans.
  - The schema, the out-of-band request, the data-load validation and the collected chart now read the scan.
  - constructor-unit-tests went from 1480 to 1483.
