# Step 0450 — impl plan

## Approach

Build the Online sources surface as a pure Elmish module, `OpticalConstructor.Ui.MaterialSourcesView`, over the import
context's `materialSources` (step 444). The update itself, `updateSourceSnapshot`, runs through an injected
`SourceSnapshotJobHost` built the way `KitArchiveJobHost` is (step 200):

- `posting` runs the job on `JobRunner.startBackground`;
- `onUiThread` posts through Avalonia's dispatcher;
- `immediate` runs on the caller's thread for tests;
- `inert` runs nothing.

The model holds the shown source, its snapshot version, the query draft, the hits, the runner's `JobModel`, the
handle, a status caption and an error. The update follows the slice's how_to message by message. The view is a pure
projection with the slice's ids.

## Files

- `OpticalConstructor.Ui/MaterialSourcesView.fs` (new). It holds:
  - the extensions `SourceSnapshotFileCount.jobStepCount` and `SourceSnapshotProgress.jobProgress`, as `KitArchiveJob`
    has;
  - the handle, the host, the context, the model and the messages;
  - the init (a member of the context, see Risks);
  - the update and the view.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile>` line after `MaterialsWindowView.fs`.
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: the captions `MaterialSourcePullError.caption`,
  `SourcePullResult.caption`, `SourceRefreshInfo.caption` and `MaterialSourceName.caption`, and the three id members.
- `OpticalConstructor.Controls/UiIds.fs`: the Online sources group of nine fixed ids.
- `OpticalConstructor.Tests/MaterialSourcesModelTests.fs` (new) and its `<Compile>` line, last.

## Risks

- **ELEV013:** a free `initMaterialSources (context : MaterialSourcesContext)` takes one of our types and nothing
  else. Steps 0107 and 0308 measured this finding and made the init a member of the context
  (`initImportDialog`, `initImportWizard`). Follow that precedent.
- **ELEV005:** the how_to's `JobStepCount filesDone.value` is a call-site rewrap. Use `jobStepCount` and
  `jobProgress` members on the source types, exactly as `KitArchiveJob.fs` does.
- **The caption rewrap `ControlCaption name.value`:** use a `MaterialSourceName.caption` member instead.
- **Disabling Update snapshot:** `verbButton` has no enabled state. Wrap it in a `Border` whose `isEnabled` follows the
  job, as `DataStepView`'s availability box does. The model also refuses a second start.
- **Switching source mid-update:** the completion would put the updating source's version under another source. Hold
  `ShowMaterialSource` while an update runs, and record this as a deviation.
