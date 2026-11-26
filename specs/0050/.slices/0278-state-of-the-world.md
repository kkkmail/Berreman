# Step 0278 — state of the world

## Where we are

Spec 0050 section S8.4 is the slot bar, quick swap and the direct problem. Step 277 added the slot bar's chip members
(`PickLetter`, `fittingItems`, `pickedByLetter`, `turnAngle`). This step adds the direct problem's semantic message:
`RecordExperiment` appends a new experiment of the draft over the active setup on every send, so one setup holds any
number of experiments, each its own `<id>.experiment-json` (001 §J, §O, §P). The table is recorded first, as a commit
records it. `ExpCommit` and `GenerateSetupGrid` now share its two halves. The slot bar's button that sends the message
is step 280's.

## What's working

- Add `TableAndElementRotationMsg.RecordExperiment`, which appends a new experiment of the draft over the active setup on every send and never edits the draft's own
- Record the table first as `ExpCommit` does: the first free setup for a table that is no setup yet, the next revision over one an experiment references, and nothing for an unchanged setup
- Refuse a read-only kit, a draft that cannot be committed, and a table no detector captures, writing nothing in each case
- Share `ExpCommit`'s two halves (`tableSetup` / `withTableRecorded` and `withExperimentsRecorded`) with `GenerateSetupGrid`, every text and outcome unchanged
- Add four `SetupModelTests` facts and send `RecordExperiment` in the read-only `KitWindowModelTests` fact

## Tests

Each gate ran locally on the final tree, using the snapshot's own commands. No run went over its effective deadline.
These are development runs; the arc-runner's gate engine runs the gates themselves.

- **build:** pass in 72 s (deadline 600 s). No warning came from our code; the only warnings are the two vendored
  MathNet `SYSLIB0051` ones that were already there.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)` in 188 s (deadline 600 s), with known findings 0.
- **constructor-unit-tests:** pass, 1464 of 1464, in 73 s (deadline 180 s). The baseline was 1460, so the count rose
  by 4. In the TRX all four new facts and the changed read-only fact show `Passed`:
  - `RecordExperiment appends a new experiment over the active setup each time, so one setup holds several
    experiments`
  - `RecordExperiment over a draft that edits an experiment appends another and leaves the edited one as it was`
  - `RecordExperiment records the table as ExpCommit does: a first setup for a table that is no setup yet, and the next
    revision over one an experiment references`
  - `RecordExperiment without a chosen element and variable is told and writes nothing`
  - `a kit opened read-only refuses every write message and writes nothing` (changed)

  Every `ExpCommit` and `GenerateSetupGrid` fact passes unchanged.
- **ui-smoke:** pass, 245 of 245, in 203 s (deadline 600 s).
- **ui-tests:** the six selected classes pass, 89 of 89, in 111 s (deadline 360 s). The classes are
  `ExperimentControlsTests`, `FsxAcceptanceTests`, `InverseConstructorTests`, `ScrollExtentTests`,
  `SetupSaveLoadTests` and `SolverHandoffWindowTests`. No UI test changed, so their count is unchanged.
- **invariant-s28-\* (all sixteen):** each grep finds nothing (`specs/0050/.artifacts/0278-invariants.log`).

```yaml
gates:
  constructor_unit_tests: 1464
  ui_smoke_tests: 245
  ui_tests: 89
```

## Architecture

- `ExpCommit`'s first half, the table recorded as the active setup, is split in two:
  - a pure member, `model.tableSetup`, the setup the table is recorded as;
  - a private function, `withTableRecorded setup model`, which writes it (unless it is the unchanged active setup) and
    sets `kitSetups` and `activeSetupOpt`.

  `ExpCommit` must decide whether it writes at all from the setup's reference before it writes, so the computation
  could not live inside the recording function.
- The second half is the private `withExperimentsRecorded committed experiments model`. It records each experiment in
  order and places it by id in `committed`'s collection. The first failure stops the run with the model as written so
  far.
- Step 276's local fold is lifted to a private module-level `writtenInOrder`, so the grid's setups and every
  experiment write share one ordered collector.
- `appendedOver held committed` names the experiments a commit appended. The grid and `RecordExperiment` share it.
- No other message, field or option was added.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- On a failure, `ExpCommit` still answers the original model, even when its setup was written and only the experiment
  write failed. `RecordExperiment`, as the slice says, keeps the model with the setup recorded and only the collection
  unchanged.
- `RecordExperiment` checks `canCommit` before the capture. A draft with no element therefore reads "choose the
  element…" and not "no detector…", even though `draftCapture` is also `None` for it.
- The success status joins the appended ids, but one reference over a committable draft always appends exactly one,
  so it reads `Recorded E2 over 'Bench' revision 1.`
- The id is rendered as `experiment.id.fileStem.value`, which is one more suppressed ELEV004 in the Ui project (511
  before, 512 now). The rule is excluded from the gate (R7). Rendering it without the unwrap would need a member in
  `ConstructorUiIdOfDomain.fs`, which this step's edits do not cover.
- After `RecordExperiment` the draft edits nothing, so a following `ExpCommit` appends rather than editing the
  recorded experiment.

## Changelog

- 2026-11-10: Step 0278 adds `TableAndElementRotationMsg.RecordExperiment`, which appends a new experiment of the
  draft over the active setup on every send after recording the table as `ExpCommit` does. `ExpCommit` and
  `GenerateSetupGrid` now share its two halves. constructor-unit-tests went from 1460 to 1464.
