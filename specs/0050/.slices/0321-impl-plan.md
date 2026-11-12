# Step 0321 — impl plan

## Approach

One wizard serves attach and split (spec 0050 S9.3; 001 §O; .spec-md §6 "Reuse before invention").

1. `OpticalConstructor.Ui/ImportWizardView.fs`
   - Add `ImportWizardTarget = AttachWizardTarget of Experiments.ExperimentId | SplitWizardTarget` before
     `ImportWizardRequest`, with one-line docs on each case.
   - `ImportWizardRequest` and `ImportWizardApplied` replace `experimentId` with `target : ImportWizardTarget`.
   - `ApplyImportWizard` hands on `request.target` unchanged.
   - `importWizardView` reads the target in two places only:
     - the heading: `Import <leaf>` for attach, `Split <leaf> by its key column` for split;
     - the caption of `importWizardAttachButton`: `Attach` or `Split`.
2. `OpticalConstructor.Ui/TableAndElementRotationView.fs`
   - `ImportWizardApplied.messages` answers, after `SaveImportProfile p` when there is a new profile:
     - `AttachDataFile (id, file, profile)` for `AttachWizardTarget id`;
     - `SplitByColumn (file, profile)` for `SplitWizardTarget`.
   - `OpenImportWizard` builds its request with `target = AttachWizardTarget experimentId`.
   - New `OpenSplitWizard of FileName`, behind `guardKitWrite`. It opens the shell's wizard over
     `{ target = SplitWizardTarget; file; readPickedFile; kitProfiles = kit's; defaultProfile = Phone ROI CSV }` and
     answers the model unchanged.
3. Tests
   - `ImportWizardTests`:
     - the class's request targets `AttachWizardTarget (ExperimentId 1)`, through a private `observeTargeted`;
     - the two Attach facts expect that target;
     - new fact `a split wizard says it splits and hands the kit window the file and the profile to split by`.
   - `KitWindowModelTests`:
     - the opening fact expects the attach target;
     - the Attach fact builds its applied value with the attach target;
     - new fact `opening the split wizard hands it the picked file with Phone ROI CSV chosen, and its Apply splits by the
       chosen profile`;
     - the read-only fact also sends `OpenSplitWizard`.
   - `ScrollExtentTests`: `wideLabFileRequest` targets `AttachWizardTarget (Experiments.ExperimentId 1)`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`

## Risks

- **ELEV016.** The new DU case names (`AttachWizardTarget`, `SplitWizardTarget`, `OpenSplitWizard`) must be unique.
  A grep of the tree finds none of them today.
- **The split heading's second line.** The attach heading's second line is "Nothing is attached until you press
  Attach.", and the existing fact pins it exactly. The split heading's second line names its own button: "Nothing is
  split until you press Split." This is recorded as a decision.
- **LF line endings** in every touched file.
