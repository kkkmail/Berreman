# Step 0292 — impl plan

## Approach

Two semantic messages build a multi-variable scan in the Main scene's Experiments draft (spec 0050 S9.1; 001 §K, §P).
Step 288 already gave the domain everything:

- `ExperimentCollection.addScanAxis` and `.addScanBlock`, members because of ELEV013;
- `ExperimentDraft.axisOpt` / `.scanOpt`;
- a `commit` / `commitOverSetups` that records `draft.scanOpt`.

So `ExpCommit`, `RecordExperiment` and `GenerateSetupGrid` already commit the whole scan, and they stay as they are.

1. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - Add `ExpAddScanAxis` and `ExpAddScanBlock` to `TableAndElementRotationMsg`, after `ExpSetRangePoints`, each with a
     one-line doc citing S9.1.
   - `ExpAddScanAxis` is `model.experimentCollection.addScanAxis`, with no `guardKitWrite`.
   - `ExpAddScanBlock`, with an edited axis and a `model.draftCapture`:
     `(Experiments.chooseMeasurement capture collection).addScanBlock`. It has no `guardKitWrite`. Otherwise the model
     is unchanged.
2. New `OpticalConstructor.Tests/ExperimentModelTests.fs`, `module ExperimentModelTests` in
   `namespace OpticalConstructor.Tests`, appended to the fsproj compile list with no comment.
   - The kit is a GUID folder under `<test output>/ExperimentModelTests/`, opened read-write, as `SetupModelTests` does.
   - The table holds the seeded source and detector, `AddElement LinearPolarizer`, `AddElement Sample`, and `AddItem`
     of a QZ glass plate 1 mm item, assigned to the sample by `AssignSlot`.
   - It has the two facts the slice names.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentModelTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`

## Risks

- **A capture with no axis.**
  - A draft with an element that varies nothing has a `draftCapture` but no axis.
  - Coercing its measurement there would be a change while Add block adds nothing.
  - So the arm matches the axis as well as the capture.
- **A capture the closing block must coerce.**
  - With only the end detector, the table offers T alone, and `ExpChooseElement` already defaults to it.
  - To prove the closing block is read on the capture the detectors allow, the first fact also requests Both before
    `ExpAddScanBlock`. That is recorded as a decision.
- **ELEV.** The new arms use only existing elevated types. Test files are outside the analyzers' scope.
