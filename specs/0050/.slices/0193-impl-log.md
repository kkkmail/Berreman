# Step 0193 — impl log

## Progress

- Read the protocol, the project prompt, the slice and the gate snapshot. Manual 013 applies from 0160 on: elevation
  is scoped from the arc origin. No other manual ruling names step 0193, and no operator note is in flight.
- Read step 0194's brief, which consumes this step. It calls `initDataLocations { locations = locations; explorer =
  explorer }` as a free function, so the contract's shape is load-bearing (see Gotchas).
- Grepped `Berreman/` and `Softellect/` for every new name and found none in use (ELEV014/015/016/019). The names
  checked are the four types, the case, the three functions, the four ids and the `pathCaption` extensions.
- Wrote the ids, captions and `DataLocationsView.fs`, then the seven declared facts. The facts were red by
  construction, because they name symbols this step creates.
- First build: 3 errors. `LibraryDatabase` is declared in `LibraryDatabaseStore`, and `tryFileOf` extends it from
  `LibraryBackupStore`, so the view opens both modules. Second build: 1 error. The constructor fact reads
  `kind.caption`, an extension in the auto-opened `ConstructorUiIdOfDomain`, so the fact opens `OpticalConstructor.Ui`.
  Third build: clean.
- The new facts passed: 4 of 4 constructor and 3 of 3 UI. Full constructor-unit-tests: 1215. Full ui-smoke: 233.
- **First elevation run: 1 new finding.** ELEV013 at `DataLocationsView.fs:94:5`: `initDataLocations takes one
  DataLocationsContext and nothing else` (`0193-elevation.log`). This was the risk the impl-plan named.
- **Repair.** `initDataLocations` is now `type DataLocationsContext with member this.initDataLocations :
  DataLocationsModel`, declared in `DataLocationsView` after `DataLocationsModel`. It keeps its name and its body. The
  three test callers moved to `context.initDataLocations`. See Gotchas for the deviation.
- Rebuilt clean. Re-ran elevation: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects. Re-ran constructor-unit-tests,
  the selected ui-tests and ui-smoke: all green (Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: a new group after the kit-bar group (step 188),
  `// --- The Data locations window (DataLocationsView, DataLocationsWindow). ---`. It holds `dataLocationsStatusText`
  (`"DataLocationsStatusText"`) with a one-line doc.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`:
  - adds `open OpticalConstructor.Domain.WorkbenchSettings`;
  - appends, each with a one-line doc citing S5.3:
    - `FolderName.pathCaption` and `FileName.pathCaption` (`ControlCaption this.value`). A row's path is converted by
      a member on the source type, never re-wrapped at the call site (AGENTS.md cross-type reuse; ELEV001/005);
    - `DataLocationKind.caption`, with the seven captions of the slice;
    - `DataLocationKind.uiKey`, with the seven keys of the slice;
    - `ConstructorUiId.dataLocationRow`, `dataLocationPathText` and `dataLocationOpenFolderButton`
      (`DataLocationRow_<key>`, `DataLocationPath_<key>`, `DataLocationOpenFolder_<key>`).
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataLocationsView.fs` (new), `module
  OpticalConstructor.Ui.DataLocationsView`. Its module doc and every declaration's doc are one to three lines citing
  S5.3.
  - `DataLocationRow = { kind; path : ControlCaption; folderOpt : FolderName option }`.
  - `type DataLocations with member this.rows`: one row per `DataLocationKind.all`, in that order.
    - The library row matches `LibraryDatabase.tryFileOf this.libraryConnection`. `Some file` shows
      `file.pathCaption` (the full path) and opens `file.tryGetFolderName`, which is STL's own `Path.GetDirectoryName`.
      `None` shows `In memory (<connection string>)` and has no folder.
    - Every other row shows its folder's `pathCaption` and opens that folder. No disk is read.
  - `[<ReferenceEquality>] DataLocationsContext = { locations; explorer : ExplorerProxy }`.
  - `DataLocationsModel = { context; rows; status : ControlCaption option }`.
  - `type DataLocationsContext with member this.initDataLocations : DataLocationsModel`: the context's rows and no
    status (ELEV013 repair, see Gotchas).
  - `DataLocationsMsg = OpenDataLocationFolder of DataLocationKind`.
  - `updateDataLocations` finds the kind's row.
    - A folder is handed to `m.context.explorer.showFolder`. `Ok ()` clears `status`. `Error e` sets it to `The
      folder could not be opened — <e.describe>`.
    - A row without a folder answers `m` unchanged.
  - `dataLocationsView`, a pure projection:
    - a `ScrollViewer` (both bars `Auto`) over a `MinWidthPanel` (`PanelMinWidth 480.0`) over a vertical
      `StackPanel`;
    - the status, when `Some`, as a wrapping `TextBlock` carrying `dataLocationsStatusText` in `messageColor`;
    - one `DockPanel` per row carrying `dataLocationRow kind`, keyed by that id. A row with a folder docks a
      right-hand `StackPanel` holding the `verbButton` `Open folder` (`dataLocationOpenFolderButton kind`, sending
      `OpenDataLocationFolder kind`). The fill is a vertical `StackPanel` of the semi-bold `kind.caption` and a
      wrapping `TextBlock` carrying `dataLocationPathText kind`;
    - no edit field, copy button or cache.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile
  Include="DataLocationsView.fs" />` directly after `KitWindowView.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataLocationsTests.fs` (new), `module DataLocationsTests`.
  Its `DataLocations` are built in the class, under `Path.Combine(AppContext.BaseDirectory, "DataLocationsTests")`,
  and are never created. The explorer is `ExplorerProxy.createInMemory`, recording.
  - `the rows list every location in order, the library by its file`: the seven kinds in `DataLocationKind.all`
    order. The library row shows `GetFullPath(<folder>/library.db)` and opens `<folder>`. The kits row shows and
    opens the kits folder. The extensions assert that neither the kits folder nor the file exists, and that the
    opened screen holds exactly these rows and no status.
  - `an in-memory library is listed without a folder`: the library row reads `In memory (Data Source=:memory:)` with
    `folderOpt = None`. The extension asserts that every other row has a folder.
  - `Open folder hands the row's folder to the explorer`: `OpenDataLocationFolder KitsLocation` records the kits
    folder and leaves no status. The extension asserts that a model carrying an earlier refusal has it cleared. Over
    the in-memory library, `OpenDataLocationFolder LibraryDatabaseLocation` records nothing and answers the model
    unchanged.
  - `a folder the explorer cannot open is told`: an explorer answering `Error (MissingFolderErr "gone")` leaves a
    status that contains `gone` and starts `The folder could not be opened — `. The rows are unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `DataLocationsTests.fs`
  appended at the end of the compile list, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataLocationsWindowTests.fs` (new), `module
  DataLocationsWindowTests`. It mounts the view as `ScrollExtentTests.constructorWindow` does (`HostWindow` plus
  `Program.mkSimple`), headless at 800 × 600, over `TestAppScope.testLocations (FolderName
  "DataLocationsWindowTests")` and a recording explorer.
  - `the Data locations UiIds are the stable intent-named ids`: `DataLocationsStatusText`, `DataLocationRow_Kits`,
    `DataLocationPath_Kits` and `DataLocationOpenFolder_Kits`.
  - `every location is listed with its path, and Open folder only where there is a folder`:
    - every kind's row and path text exist, and each path text equals its row's path;
    - the kits path is the test kits folder's text, and the library path is `In memory (Data Source=:memory:)`;
    - the library row has no `dataLocationOpenFolderButton`, and the six other rows each have one;
    - no status is shown.
  - `ui-smoke`: `pressing Open folder hands that location's folder to the explorer`. A press at the centre of the
    exports row's button, as `LibraryWindowTests.clickOn` presses, records exactly the exports folder and shows no
    status.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`:
  `DataLocationsWindowTests.fs` appended at the end of the compile list, with no comment.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine. Logs
are under `specs/0050/.artifacts/`. The figures below are from the final tree, after the ELEV013 repair.

- **Build:** `dotnet build Berreman.slnx -c Release` exits 0, with 0 errors and 0 warnings (`0193-build-2.log`).
  Each project this step edits was recompiled in that run. The first-tree build is in `0193-build.log`.
- **constructor-unit-tests:** 1215 passed, 0 failed, 0 skipped (`0193-constructor-unit-tests.log`). That is
  checkpoint 191's 1211 + 4, the four declared facts.
- **ui-smoke:** 233 passed, 0 failed, 0 skipped (`0193-ui-smoke.log`). That is checkpoint 190's 232 + 1, the declared
  smoke fact.
- **ui-tests, the selected class `OpticalConstructor.Ui.Tests.DataLocationsWindowTests`:** 2 passed, 0 failed
  (`0193-ui-tests.log`). These are the two declared un-traited facts.
- **New facts alone:** 4 of 4 (`0193-constructor-new-facts.log`) and 3 of 3 (`0193-ui-new-facts.log`), on the first
  tree.
- **Elevation**, run with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`):
  - final tree: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects, ELEV004 excluded, 0 known findings
    (`0193-elevation-2.log`);
  - first tree: 1 new ELEV013 (`0193-elevation.log`), repaired as above.
- **The fourteen s21 invariant greps** (with `--untracked`, so the new files count): none finds anything.
- **Line endings:** no CR and no BOM in any touched file. `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0193-build.log`, `0193-build-2.log`
- `specs/0050/.artifacts/0193-constructor-new-facts.log`, `0193-ui-new-facts.log`
- `specs/0050/.artifacts/0193-constructor-unit-tests.log`, `0193-ui-smoke.log`, `0193-ui-tests.log`
- `specs/0050/.artifacts/0193-testresults/` (the three TRX reports)
- `specs/0050/.artifacts/0193-elevation.log` (first tree, 1 finding), `0193-elevation-2.log` (final tree, OK)

## Gotchas

- **Deviation: `initDataLocations` is a member of `DataLocationsContext`, not a free function.**
  - **Conflict.** The slice's (f), and its contract `OpticalConstructor.Ui.DataLocationsView.initDataLocations`,
    name a module-level `initDataLocations (context : DataLocationsContext) : DataLocationsModel`. ELEV013 refuses a
    public free function of one of our types. Measured: `0193-elevation.log`. None of its exemptions applies:
    - the module is not named for the type;
    - the module is not `RequireQualifiedAccess`;
    - the body reads no module value.
  - **Authority.** AGENTS.md, from operator ruling 0048 `.manual/002` R6: where a document disagrees with the
    analyzers, the analyzers win. A module value added only to trip the exemption would be silencing the finding,
    which AGENTS.md forbids, and step 0021 refused the same trick.
  - **Repair.** The function is now `type DataLocationsContext with member this.initDataLocations :
    DataLocationsModel`, an intrinsic augmentation declared in `DataLocationsView` after `DataLocationsModel`. Its name
    and its body are unchanged. This is the analyzer's own suggestion, and step 0021 did the same for
    `serializeProject`.
  - **For step 0194.** Its brief writes `initDataLocations { locations = locations; explorer = explorer }`. In the
    member form that is `({ locations = locations; explorer = explorer } : DataLocationsContext).initDataLocations`
    as `Program.mkSimple`'s init (`fun () -> context.initDataLocations`).
- **Interpretation: paths are converted by `pathCaption`.** `DataLocationRow.path` is a `ControlCaption`, and no
  `FolderName` / `FileName` → `ControlCaption` member existed. `FolderName.pathCaption` and `FileName.pathCaption`
  were added in `ConstructorUiIdOfDomain.fs`, which is in this step's edits. They are not in the contract list.
- **The library row's folder is `FileName.tryGetFolderName`.** That is STL's `Path.GetDirectoryName` member, which
  the slice names, and it answers `Some` for every full path `tryFileOf` produces.
- **Interpretation: Open folder sits in a docked `StackPanel`.** The slice says "whose right side holds" the
  `verbButton`. A `verbButton` is an `IView` and cannot take a dock attribute, so it sits inside a right-docked
  `StackPanel`, as the kit start screen's row verbs do. A row without a folder has no right-hand panel at all.
- **Each row's `DockPanel` is keyed by its id.** The status is inserted ahead of the rows, and the key keeps FuncUI
  from patching one row into another when that happens.
- **The ui-smoke fact's explorer is the in-memory double.** No fact shows a real folder, because that would open
  Explorer on the test machine (`ExplorerProxyTests`).
- `specs/0050/.manifest.state.json` was already modified by the supervisor before this worker started, and it was not
  touched.
