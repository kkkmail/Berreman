# Step 0327 — impl plan

The Data step opens on the experiment matrix (001 §O "Experiment matrix view"; spec 0050 S9.3), over step 322's
`ExperimentMatrix.ofExperiments`, with step 315's `ReviewExperimentData` and S9.1's data badge.

## Approach

1. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`:
   - `MatrixRowKey.caption`: the source labels joined by ` + ` (else `—`), ` · `, then the objects as
     `<label>@<angle.degreeCaption>` joined by ` → ` (else `—`);
   - `MatrixColumnKey.caption`: the analyzer labels joined by ` + ` (else `—`);
   - `ConstructorUiId.experimentMatrixEntry (experimentId : ExperimentId)`: `ExperimentMatrixEntry_<id>`.
2. `OpticalConstructor.Controls/UiIds.fs`: `experimentMatrix` (`ExperimentMatrix`) in the Data step group.
3. `OpticalConstructor.Ui/DataStepView.fs`, first in the panel:
   - the heading `Experiment matrix:`;
   - with no experiment, the line `(no experiments yet)`;
   - otherwise a `Grid` carrying `experimentMatrix`. Its first row holds the column captions. Each matrix row follows,
     its caption first, then per column a vertical `StackPanel` with one verb per experiment of the cell. A verb reads
     `<name> · <badge text>`, sends `ReviewExperimentData`, and is on the positive fill when it is the reviewed
     experiment (`verbButton` / `actionButton`, the split toggle's precedent);
   - when some experiments are unplaced, the wrapping line `Not in the matrix (setup missing): <names>`.
4. Tests:
   - `DataStepTests`: the ids fact pins `ExperimentMatrix` and `ExperimentMatrixEntry_1`, and two facts are created
     (one ui-tests, one ui-smoke).
     - `setup1` and `benchB` gain the bench's slots, LP1 at 0° and 30°.
     - The class's mock attaches any picked file as E1's attachment, so `AttachDataFile` runs its real path.
   - `ScrollExtentTests`: the Data step fact also holds a pending split of three groups, a reviewed experiment (three
     points, one excluded, one correction) and three experiments placed in the matrix.

## Risks

- **Prep angles are not in a row key.** Only an object's angle is, so "Bench B (its polarizer turned)" gives a second
  row only when the turned polarizer sits in an object (Sample) slot. LP1 is therefore a black-box Mueller item in the
  sample slot: the polarizer under study.
- **Attach writes to the kit.** `AttachDataFile` records E1 through `recordExperiment`, which needs the kit to hold
  `Setup 1` and its items. The fact writes them into the opened folder kit first.
- **Shared fixtures.** `setup1` and `benchB` are shared with steps 320–326's facts, which read only their references.
  The split offer still reads `(new setup)`, because that fact's kit holds only `Dark`.
