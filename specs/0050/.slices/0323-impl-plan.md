# Step 0323 — impl plan

## Approach

The Inverse constructor's flow strip gains a Data step after Experiments (001 §O; spec 0050 S9.3). The Experiments
bay's "Data files" section moves into it, and a new "Split a file by key…" verb opens step 321's split wizard.

1. `TableAndElementRotationView.fs`
   - `ConstructorFlowStep` gains `DataFlowStep`, with its caption and `flowUiId`. `all` lists four steps.
   - New `stepsOf (mode : ConstructorMode)`: Forward is `all` without Data, Inverse is `all`.
   - `ShowFlowStep DataFlowStep` sets only `flowStep`, and only under `InverseConstructor`.
   - `SwitchConstructorMode ForwardConstructor` from `DataFlowStep` returns to `ExperimentsFlowStep` and the
     Experiments bay.
   - `attachDataFileButton` and `dataFileRow` move out. `collectionBuilderView` loses its data-file section and its
     mode match.
2. `DataStepView.fs` (new), compiled after `FlowStripView.fs`.
   - It holds the moved functions, which now use `MaterialsWindowView.brush` (the TableAndElementRotationView `brush`
     is private).
   - It adds `dataStepView`: a scroller, `dataStepPanel`, the heading, the `experimentDataFiles` rows or the hint, and
     the Split verb.
3. `KitWindowView.fs`: the Data step shows `dataStepView`, keyed `data`. `FlowStripView.fs` shows
   `stepsOf model.constructorMode`.
4. `UiIds.fs`: three new ids. `ConstructorUiIdOfDomain.fs`: the docs name the Data step.
5. Tests, as the slice lists:
   - `KitWindowModelTests`: one changed fact and one new fact.
   - `KitWindowTests`: three changed facts and one new ui-smoke fact.
   - `ScrollExtentTests`: two renamed facts (one with changed assertions) and one new fact.

## Risk / deviation

The slice calls `pickDataFile` private. It is public, and `ItemsPanelView.fs:411` uses it for the item photo.
`ItemsPanelView` compiles before `FlowStripView`/`DataStepView`, and it is not in this step's effects. Moving
`pickDataFile` would break the build, or would need an unauthorized edit and a different compile order. So it stays in
`TableAndElementRotationView`, and `DataStepView` calls it, as "reuse, never copy" asks. `attachDataFileButton` and
`dataFileRow` move.

The read-only test checks `attachDataFile E1` and `dataSplitFileButton` in a `dataStepView` window. In the `mainView`
window, `attachDataFile E1` must now be absent.
