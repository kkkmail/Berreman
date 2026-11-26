# Step 0194 — impl log

## Progress

- Read the protocol, the project prompt, the slice and the gate snapshot. Manual 013 applies from 0160 on (elevation
  from the arc origin). No other manual ruling names step 0194, and no operator note is in flight.
- Read step 0193's impl-log and state of the world. `initDataLocations` is now a member of `DataLocationsContext`
  (ELEV013), so the window's init is `fun () -> context.initDataLocations` over a typed context record.
- Grepped `Berreman/` and `C:\GitHub\Softellect` for `DataLocationsWindow`, `DataLocationsWindowKey` and
  `dataLocationsWindow`: none in use (ELEV014/015/016).
- Grepped every match over `WindowKey`: the only exhaustive one is `WireUiFinalCompositionTests.rootWindowFactory`, so
  the new case breaks no product match.
- Wrote the id, the key, the window, the factory case and the three facts. The facts were red by construction, because
  they name symbols this step creates.
- First build: clean, 0 errors, and no warning from our code. Every test run passed on the first tree (Testing state).
- Elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings. No repair was needed.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: the Data locations group (step 193) gains
  `dataLocationsWindow` (`"DataLocationsWindow"`), with a one-line doc citing 001 §G and S5.3.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/WindowLauncher.fs`:
  - `WindowKey` gains `DataLocationsWindowKey` after `CategoryEditorKey`, documented in one line: the single-instance
    Data locations window (001 §G, spec 0050 S5.3);
  - the `WindowKey` doc said "The four parameterless cases"; there are five now, so it says "The parameterless cases".
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataLocationsWindow.fs` (new), `namespace OpticalConstructor.Ui`.
  `type DataLocationsWindow(locations : DataLocations, explorer : ExplorerProxy)` inherits `HostWindow`, as
  `KitStartWindow` does. Its doc is two lines citing 001 §G and S5.3.
  - Title `Data locations`. `Name` and `AutomationId` are `ConstructorUiId.dataLocationsWindow`. The automation name
    is the title, as `KitStartWindow` sets it.
  - It asks for 720 × 460, then calls `WindowFit.keepOnScreen this`.
  - It runs `Program.mkSimple` over `context.initDataLocations`, `updateDataLocations` and `dataLocationsView`, with
    `Program.withHost this`. The context is `{ locations = locations; explorer = explorer } : DataLocationsContext`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile
  Include="DataLocationsWindow.fs" />` directly after `KitStartWindow.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`:
  - opens `OpticalConstructor.Domain.KitFolders` for `ExplorerProxy`;
  - `rootWindowFactory` answers `DataLocationsWindowKey` with `DataLocationsWindow(TestAppScope.testLocations owner,
    ExplorerProxy.createInMemory ignore)`;
  - `the root window-launcher factory opens and renders Materials, Library, the three editors, and the SolverHandoff`
    adds `DataLocationsWindowKey` to the keys it opens (change_expectation). Its name is unchanged.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`: `the Data locations window leaves
  everything reachable`, placed after the kit window's fact. It is `assertReachable "DataLocations"` over a
  `DataLocationsWindow` over `TestAppScope.testLocations (FolderName "ScrollExtentTests")` and
  `ExplorerProxy.createInMemory ignore`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/DataLocationsWindowTests.fs`: opens
  `OpticalConstructor.Ui.WindowLauncher` and gains two facts.
  - `the Data locations window is titled and carries its stable id` (no trait): a window over the class's test
    locations. Its `Title` is `Data locations`, and its `Name` and `AutomationId` are `dataLocationsWindow`. It is
    closed in a `finally`.
  - `the Data locations window opens once through the registry` (`ui-smoke`):
    - `WindowLauncher.create` (Browse, modeless) over a factory that builds a `DataLocationsWindow` for
      `DataLocationsWindowKey` and refuses every other key;
    - the first `openOrActivate DataLocationsWindowKey` answers `CreatedWindow` of a shown `DataLocationsWindow`;
    - the second answers `ActivatedWindow` of the same window;
    - the window is closed in a `finally` and the close is pumped, so the app-global registry key does not leak.

## Testing state

These are development runs only. Under Invariant 6, the gate verdict belongs to the arc-runner's gate engine. Logs
are under `specs/0050/.artifacts/`, and the three TRX reports under `specs/0050/.artifacts/0194-testresults/`.

- **Build:** `dotnet build Berreman.slnx -c Release -nologo -v:m` exits 0, with 0 errors (`0194-build.log`). Its only
  warnings are `SYSLIB0051` in the vendored `MathNetNumerics/Numerics/Exceptions.cs`. That is not our code, and it
  appears whenever MathNet recompiles (e.g. `0192-build.log`). Nothing this step edits warns.
- **constructor-unit-tests:** 1215 passed, 0 failed, 0 skipped (`0194-constructor-unit-tests.log`). That is
  checkpoint 192's 1215. This step adds no constructor fact.
- **ui-smoke:** 234 passed, 0 failed (`0194-ui-smoke.log`). That is checkpoint 192's 233 + 1, the declared
  `the Data locations window opens once through the registry`. The widened root-factory fact passes.
- **ui-tests, the selected classes `DataLocationsWindowTests` and `ScrollExtentTests`:** 15 passed, 0 failed
  (`0194-ui-tests.log`). That is 13 + 2, the two declared un-traited facts. The TRX lists both new identities.
- **Elevation**, run with `ASG_ARC_BASE_COMMIT=f395597…` (`.arc-base-json`): `ELEV-GATE: OK`, 0 new findings in 12
  of 12 projects, ELEV004 excluded, 0 known findings (`0194-elevation.log`).
- **The fourteen s21 invariant greps** (with `--untracked`, so the new file counts): none finds anything.
- **Line endings:** no CR and no BOM in any touched file. `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0194-build.log`
- `specs/0050/.artifacts/0194-constructor-unit-tests.log`, `0194-ui-smoke.log`, `0194-ui-tests.log`
- `specs/0050/.artifacts/0194-testresults/` (the three TRX reports)
- `specs/0050/.artifacts/0194-elevation.log`

## Gotchas

- **Interpretation: the composition factory reuses `owner`.** The slice writes
  `TestAppScope.testLocations (FolderName "WireUiFinalCompositionTests")`. The module already binds `owner = FolderName
  "WireUiFinalCompositionTests"`, so the factory passes `owner`. It is the same value, and the constant is not written
  twice (AGENTS.md: reuse, never copy).
- **The init is the member form.** The slice's `initDataLocations { locations = locations; explorer = explorer }` is
  `context.initDataLocations` over a typed `DataLocationsContext`, after step 193's ELEV013 repair.
- **The registry is app-global.** The smoke fact closes its window and pumps the close before it returns. A leaked
  `DataLocationsWindowKey` would turn the root-factory sweep's `CreatedWindow` into an `ActivatedWindow` and fail it.
- **The window is not yet opened by any product site.** The slice gives it a key and a window. The Launcher's footer
  link that opens it is not in this step's effects (`Program.fs` and `LauncherWindow` are not edited). Step 0195's
  slice wires that link.
- `specs/0050/.manifest.state.json` was already modified by the supervisor before this worker started, and it was not
  touched.
