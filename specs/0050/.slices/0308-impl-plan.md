# Step 0308 — impl plan

## Approach

The column-mapping wizard's pure MVU, in a new `OpticalConstructor.Ui/ImportWizardView.fs`
(`module OpticalConstructor.Ui.ImportWizardView`), compiled directly after `SetupGridView.fs` and before
`TableAndElementRotationView.fs`. It holds, in the shape of step 107's `ImportDialogView`:

- `ImportWizardRequest` (reference equality), `ImportWizardApplied`, `ImportWizardContext` (reference equality),
  `WizardProfileChoice`, `ImportWizardModel` (with its `preview` member) and `ImportWizardMsg`;
- the initializer, which reads the picked file once and takes its sniffed layout (step 307), the detector's profile
  and one unmapped column per preview column;
- `updateImportWizard`, whose only context-reaching message is `ApplyImportWizard` (and `CancelImportWizard`'s close).

Reuse, not new rules: `LabFileSource.sniffedLayout` and `KitFileText.previewWith` (step 307),
`ChosenImportProfile.tryOfName` and `ColumnRoles.tryOfChoices` (step 300), `ImportProfileName.availableAmong`
(step 299) and `ImportProfileName.tryCreate` (step 291).

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` (one `<Compile>` line)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs` (new, six facts)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (one `<Compile>` line
  after `ImportDialogTests.fs`)

## Risks

- **ELEV013.** A free `initImportWizard (context : ImportWizardContext)` takes one of our types and nothing else, so
  the analyzer refuses it. Step 107 met the same rule for `initImportDialog` and made it a member of the context;
  this step follows that precedent (`context.initImportWizard`) and records the deviation.
- **Unique names (ELEV014/015/016).** Every new type and case name is checked against the tree; the elevation run
  confirms it across Softellect.*.
- **Layout from a kit profile.** Picking a kit profile changes the layout, so the columns are remade over the new
  preview by the same rule as `SetImportDelimiter` / `SetImportHeader`, keeping the invariant "one choice per preview
  column".
