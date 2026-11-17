# Step 0446 — impl plan

Obligation S131-SOURCES-LOCATION: the online sources' offline snapshots get a data location of their own,
read like every other 001 §G location.

## Approach

1. `OpticalConstructor.Domain/WorkbenchSettings.fs`: `DataLocations` gains `materialSourcesFolder : FolderName`
   last; `defaultsUnder` answers `%LOCALAPPDATA%/Softellect/Berreman/OpticalConstructor/Sources`.
   `DataLocationKind` gains `MaterialSourcesLocation` last, and `all` lists it last.
2. `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: `caption` = `Online source snapshots`, `uiKey` =
   `MaterialSources`. `OpticalConstructor.Ui/DataLocationsView.fs`: `rows` answers it through `folderRow`.
3. `OpticalConstructor.App/AppConfig.fs`: `ConfigKey.materialSourcesFolder = "MaterialSourcesFolder"`;
   `readDataLocations` reads it through `readSetting`; `resolveLocations` resolves it with `resolveFolder`.
4. Test run: `appsettings.json` gains `MaterialSourcesFolder = TestRun/Sources`; `TestAppScope.testLocations`
   adds `Sources` under the owner's root (so `fileLocations` moves it too). `DataLocationsTests.locationsOver`
   sets a `Sources` folder under `factFolder`.
5. Widen the five named facts and nothing else: the defaults fact, the field/kind order fact (eight kinds),
   the rows fact (eight rows, the last the sources folder), the AppConfig missing-defaults fact (written back
   verbatim, answered expanded, through `assertSavedLocations`), and the test-run appsettings fact (six
   folders under the test output).

## Files

All nine in the step's authorized `edits`; no other file constructs a `DataLocations` record
(`git grep exportsFolder\s*=` confirms: Domain, AppConfig, TestAppScope, DataLocationsTests, AppConfigTests).

## Risks

- Record construction sites outside the list would fail the build — checked, none.
- ELEV016 DU-case uniqueness: `MaterialSourcesLocation` is new across the tree (grep finds nothing).
- Test counts must stay unchanged: only assertions are widened, no fact added.
