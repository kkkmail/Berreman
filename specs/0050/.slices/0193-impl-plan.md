# Step 0193 — impl plan

## Approach

The Data locations screen is a pure Elmish surface over two injected seams, `DataLocations` (step 81, with
`DataLocationKind` from step 192) and `ExplorerProxy` (step 182). It follows `KitStartView.fs` (step 176) for its
shape and `kitStartView` (step 178) for its scrolling.

1. **Ids and captions** (`ConstructorUiIdOfDomain.fs`, `UiIds.fs`):
   - `DataLocationKind.caption` and `.uiKey`;
   - `ConstructorUiId.dataLocationRow`, `dataLocationPathText` and `dataLocationOpenFolderButton`, each over a kind;
   - `dataLocationsStatusText` in a new UiIds group after the kit-bar group.

   A row's path is a `ControlCaption`. It is converted by a named member on the source type (`FolderName.pathCaption`
   and `FileName.pathCaption`), never re-wrapped at the call site (AGENTS.md: cross-type reuse is a member on the
   source type).
2. **`DataLocationsView.fs`**, compiled right after `KitWindowView.fs`:
   - the `DataLocationRow` record;
   - `DataLocations.rows`, which uses `LibraryDatabase.tryFileOf` for the library row and `FileName.tryGetFolderName`
     (STL's own `Path.GetDirectoryName`) for its folder;
   - the context, the model and the message;
   - `initDataLocations`, `updateDataLocations` and `dataLocationsView`.
3. **Tests:**
   - `OpticalConstructor.Tests/DataLocationsTests.fs` holds the four model facts;
   - `OpticalConstructor.Ui.Tests/DataLocationsWindowTests.fs` holds two un-traited facts and one `ui-smoke` fact.

   Each new file goes at the end of its project's compile list.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataLocationsView.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataLocationsTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataLocationsWindowTests.fs` (new)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`

## Risks

- **ELEV013.** `initDataLocations (context : DataLocationsContext)` is a public free function of one of our types,
  which ELEV013 refuses unless an exemption applies. Step 194 calls it in exactly that form. I will measure it with
  the analyzer before deciding anything.
- **Record labels.** `context`, `rows`, `status` and `explorer` are labels on other records too. The new records are
  declared in their own module, and the test files build their models through `initDataLocations`.
- **Status insertion.** The status is inserted ahead of the rows. Each row's `DockPanel` is keyed by its id, so
  FuncUI does not patch one row into another.
