# Step 0294 — impl plan

**Goal.** Make a recorded experiment's role, session and notes editable through three semantic messages, each of
which writes the experiment's kit file (spec 0050 S9.1; 001 §P, §E, §K). The bay controls that send them belong to
step 296.

## Approach

1. **Domain (`Experiments.fs`).**
   - Add a `type Experiment with` block right after `Experiment` with `withRole`, `withSessionOpt` and `withNotes`.
     Each returns the experiment with only that field changed.
   - Add `ExperimentCollection.withStoredExperiment`, an intrinsic member. It replaces the experiment of the same id,
     returns the collection unchanged when no experiment has that id, and leaves the draft untouched.
2. **UI (`TableAndElementRotationView.fs`).**
   - Add `SetExperimentRole`, `SetExperimentSession` and `SetExperimentNotes` to `TableAndElementRotationMsg`, each with
     a one-line doc.
   - Add one private helper, `recordExperimentEdit`. It follows the shape of `recordKitItemEdit`:
     - `guardKitWrite` runs first;
     - an id the collection lacks returns the model as it is;
     - otherwise the changed experiment goes through `KitLibraryContext.recordExperiment`;
     - on success the collection takes it (`withStoredExperiment`) and `kitStatus` clears, as the other write arms do;
     - on failure the model is kept, with `Not saved to the kit — <e.describe>`.
   - The three arms call the helper with `withRole`, `withSessionOpt (SessionName.tryCreate text.value)` and
     `withNotes (ExperimentNotes text.value)`.
3. **Tests.**
   - Add two facts to `ExperimentModelTests`. They run over its folder kit with E1 committed through `ExpCommit` on the
     scan table.
   - Extend `KitWindowModelTests`' read-only fact with the three messages in its `refused` list. The existing
     assertions cover them.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Risks

- **ELEV names.**
  - `Item.withNotes` already exists, but on a different receiver, so ELEV019 does not apply.
  - The new DU cases must be unique across all DUs (ELEV016). A grep found no `SetExperiment*` case.
- **Elevation of a private helper.** It takes only our types, so ELEV003P does not fire.
- **The experiment name in the tests** comes from the kit's naming template. The facts therefore read the file through
  `readExperiments` and the stored name rather than hard-coding `E1`.
