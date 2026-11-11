# Step 0296 — impl plan

## Approach

The Experiments bay reads v2 (spec 0050 S9.1). Controls stays domain-free; the host projects the Domain draft and
experiments into the new state fields and sends the step-292 / step-294 messages.

1. `OpticalConstructor.Controls/ExperimentControls.fs`
   - New `ExperimentRoleMark` (`RoleChosen` | `RoleNotChosen`) and `ExperimentRoleOption { key; caption; mark }`.
   - `ExperimentRow` gains `badge` after `description`.
   - `ExperimentControlsState` gains `scanLines`, `scanAvailability`, `roleOptions`, `sessionText`, `notesText` and
     `bookkeepingAvailability`, empty or withheld in `emptyExperimentControls`.
   - `ExperimentControlsHandlers` gains `addScanAxis`, `addScanBlock`, `setRole`, `setSession` and `setNotes`.
   - View:
     - the scan-summary panel under the range rows;
     - `Add variable` / `Add block` beside Add / New, drawn by `actionButton`;
     - a bookkeeping block shown while `bookkeepingAvailability` is offered: the role row of `optionBoxV` boxes and
       the Session / Notes fields, which commit on Enter or blur like `numberField`;
     - the badge text in each collection row.
   - The bookkeeping block is keyed by the loaded row's id. Its fields' and boxes' subscriptions then belong to the
     experiment they edit: a stale closure must never write to the previously loaded experiment.
2. `OpticalConstructor.Controls/UiIds.fs` gets the seven ids.
3. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: `ExperimentRole.uiKey` and `ExperimentRole.caption`.
4. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - `experimentState` fills the new fields.
   - `experimentHandlers` sends `ExpAddScanAxis`, `ExpAddScanBlock`, `SetExperimentRole`, `SetExperimentSession` and
     `SetExperimentNotes`, the last three for the edited experiment.
5. Tests
   - `ExperimentControlsTests`: four ui-tests and one ui-smoke, and the local handler record gains the five fields.
   - `WorkbenchInfoAndCaptureTests`: the handler record gains the five fields as `ignore`.
   - `ScrollExtentTests` Inverse: loads an experiment into the editor (`ExpEdit`).

## Risks

- **Block text.** Step 288's block text is private inside `Experiment.description` (Domain), and `Experiments.fs` is
  outside this step's authorized edits. The Ui projects `Block <k>: …` from the axes' public `describe` and the
  capture's `label`, in the same form. This is recorded as a gotcha, after the 0274 precedent.
- **Stale handler closures.** These are FuncUI subscriptions with `Never` / `OnChangeOf`. Keying the bookkeeping block
  by the edited experiment's id handles them.
- **Layout reachability.** `ScrollExtentTests` is the check.
