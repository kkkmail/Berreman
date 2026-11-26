# Step 0315 — impl plan

## Approach

The Data step (spec 0050 S9.3) gets its model state and its four semantic messages; the views that send them are
step 325's.

1. `OpticalConstructor.Ui/TableAndElementRotationView.fs`, before the model:
   - `ReviewedData = { experimentId; points : Result<MappedPoints, ExperimentDataError> }`;
   - `DataStepDraft = { exclusionReason; correctionNote }` with `static member empty`.
2. `TableAndElementRotationModel` gains `reviewedDataOpt` and `dataDraft`. `initWith` sets `None` and
   `DataStepDraft.empty`.
3. The messages `ReviewExperimentData`, `SetExclusionReason`, `ExcludePoint` and `IncludePoint`:
   - Review and SetExclusionReason write nothing and are not guarded.
   - ExcludePoint and IncludePoint call `guardKitWrite` first.
4. One private read, `reviewedDataOf`, serves both `ReviewExperimentData` and the re-read after a write.
   - `points` is `readMapped data` for an experiment with data.
   - With no data it is `Error (EmptyDataFileErr "<name> has no data file")`.
5. One private write, `withExperimentDataRecorded`. It records the changed experiment through
   `KitLibraryContext.recordExperiment`. On `Ok`:
   - it stores the experiment with `withStoredExperiment`;
   - it re-badges through `checkExperimentData`;
   - it re-reads the review when the review is of this experiment;
   - it sets the told status.
   On `Error` it keeps the model with `Not saved to the kit — …`.
6. The existing `recordExperimentEdit` and the new write share one core, `withExperimentRecorded`, so the
   record-and-store step is not copied (AGENTS.md "Reuse, never copy").
7. `ExcludePoint` refuses typed, in the slice's order:
   - an experiment the collection lacks changes nothing;
   - then a blank reason;
   - then an experiment with no data;
   - then a `readMapped` error;
   - then a point above `pointCount`.
8. `IncludePoint` changes nothing for an absent experiment, an experiment with no data, or a point with no exclusion.

## Tests

- `ExperimentModelTests` gets a helper over the scan kit. It commits:
  - `E1` over the polarizer's R1 at 0…90°, with a test-local intensity file outside the kit attached through
    `AttachDataFile`;
  - `E2` with no data.
- The three new facts are the ones the slice names.
- `KitWindowModelTests`' read-only fact also sends `ExcludePoint` and `IncludePoint`, under the same assertions.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- The new DU case and type names must be unique (ELEV014 and ELEV016). Grep before adding them.
- The badge must validate. E1's setup must be in `kitSetups`, the detector must be an intensity detector, and the data
  must lie in E1's range.
- Experiment names must be `E1` and `E2`. That holds when the fresh kit's template names none; it is checked by run.
