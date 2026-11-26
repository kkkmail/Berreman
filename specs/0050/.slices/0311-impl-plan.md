# Step 0311 — impl plan

## Approach

The wizard's MVU (step 308), view (step 309) and the kit window's messages (step 310) exist. This step gives the
wizard its own window and the shell member that opens it over the kit window's dispatch.

1. New `OpticalConstructor.Ui/ImportWizardWindow.fs`, `namespace OpticalConstructor.Ui`, compiled directly after
   `KitWindowView.fs` (one `<Compile>` line, no comment):
   - `type ImportWizardWindow(request, onApplied)` inherits `HostWindow`, in the shape of `ImportDialogWindow`: title
     `Import data file`, `Name` and `AutomationId` `ConstructorUiId.importWizardWindow`, 1000 × 700 then
     `WindowFit.keepOnScreen this`, and `Program.mkProgram` over `context.initImportWizard`, `updateImportWizard`
     and `importWizardView` with `{ request; onApplied; requestClose = this.Close }`.
   - `module ImportWizardOpening` with `type KitWindowShell with member this.withImportWizard owner dispatch`: the
     shell whose `openImportWizard` shows an `ImportWizardWindow` owned by `owner`, its `onApplied` dispatching
     `applied.messages` in order.
2. `ScrollExtentTests` gains `the import wizard leaves everything reachable` over a twelve-column, forty-row comma
   lab file read in memory (the window, and a `HostWindow` hosting the mapping model with column 12 a scan role).
3. `ImportWizardTests` gains the `ui-smoke` acceptance fact: `KitWindowShell.inert.withImportWizard owner record`'s
   `openImportWizard` of the class's request shows an owned window carrying the id; heading, grid, Attach/Cancel row;
   pressing `Phone ROI CSV` then Attach records exactly `[ AttachDataFile (ExperimentId 1, file, phoneRoiCsv) ]` and
   closes the window.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ImportWizardWindow.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ImportWizardTests.fs`

## Risks

- The reachability law over 12 × 20 preview cells plus the combo row: the top choices block is not in the scroll
  viewer, so at 1000 × 540 the docked blocks plus the button row must leave the viewer room; measured, not assumed.
- The ui-smoke suite is long (219 s at step 310) against a 600 s deadline; one more fact.
- ELEV: the extension member is on our type (members may speak primitives); the window constructor mirrors
  `ImportDialogWindow`, which is ELEV-clean.
