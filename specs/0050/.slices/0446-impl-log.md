# Step 0446 — impl log

## Progress

- [x] Domain: `DataLocations.materialSourcesFolder`, `DataLocationKind.MaterialSourcesLocation`
- [x] Ui: caption / uiKey, `DataLocations.rows`
- [x] App: `ConfigKey.materialSourcesFolder`, `readDataLocations`, `resolveLocations`
- [x] test run: `appsettings.json`, `TestAppScope.testLocations`, `DataLocationsTests.locationsOver`
- [x] the five widened facts
- [x] build: exit 0, 83 s
- [x] constructor-unit-tests (whole, `all_discovered`): 1783 of 1783, 106 s
- [x] ui-smoke (whole): 264 of 264, 226 s
- [x] ui-tests over the three selected classes: 25 of 25, 37 s
- [x] elevation: `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, 216 s
- [x] the 21 s43 invariants: every one holds
- [x] state-of-the-world written

## Files modified

All nine are in the step's authorized `edits`. No other file constructs a `DataLocations` record; a `git grep` for
`exportsFolder\s*=` finds only these.

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/WorkbenchSettings.fs`
  - `DataLocations` gains `materialSourcesFolder : FolderName` as its last field, with a one-line doc citing S13.1.
  - `defaultsUnder` sets it to `FolderName "%LOCALAPPDATA%/Softellect/Berreman/OpticalConstructor/Sources"`.
  - `DataLocationKind` gains `MaterialSourcesLocation` as its last case, and `all` lists it last.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: `caption` gives
  `Online source snapshots` and `uiKey` gives `MaterialSources`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/DataLocationsView.fs`: `rows` builds the new kind's row with
  `folderRow kind this.materialSourcesFolder`, like every other folder.
- `Berreman/OpticalConstructor/OpticalConstructor.App/AppConfig.fs`
  - `ConfigKey.materialSourcesFolder = "MaterialSourcesFolder"`, a `[<Literal>]` with a one-line doc.
  - `readDataLocations` reads it through `readSetting` with `FolderName.tryCreate` and `_.configValue`.
  - `resolveLocations` resolves it through `resolveFolder`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/appsettings.json`: adds
  `"MaterialSourcesFolder": "TestRun/Sources"`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/TestAppScope.fs`: `testLocations` sets
  `materialSourcesFolder = root.combine sources`, where `sources` is `FolderName "Sources"`. `fileLocations`
  copies `testLocations` with `with`, so it moves the new folder too.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/WorkbenchSettingsTests.fs`
  - `DataLocations.defaultsUnder holds the 001 §G locations as configured text` also asserts the sources default.
  - `every location is a data location kind, in the order of its field` lists
    `"materialSourcesFolder", MaterialSourcesLocation` last. It expects 8 distinct kinds (7 before), with
    `MaterialSourcesLocation` last.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataLocationsTests.fs`
  - `locationsOver` sets `materialSourcesFolder` to `factFolder/Sources`.
  - `the rows list every location in order, the library by its file` also asserts:
    - 8 rows;
    - the last row is `MaterialSourcesLocation`;
    - its path shows the sources folder and Open folder opens it.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppConfigTests.fs`
  - `readDataLocations writes every missing location back as its default and answers it expanded`:
    - the expected record has `materialSourcesFolder` expanded;
    - after the save, `MaterialSourcesFolder` must hold the default verbatim;
    - the default must contain `%LOCALAPPDATA%`.
  - `the test run's appsettings.json keeps the library in memory and every folder under the test output`: the
    expected record has `materialSourcesFolder = <install>/TestRun/Sources`. That makes six folders under the
    test output.

## Testing state

Each gate ran with its own command and selection. The supervisor's gate engine re-runs the roster and is the
authority.

- build: exit 0, 83 s against 600 s.
  - Warnings: 12 × NU1902, which is exempt.
  - Also 4 × SYSLIB0051 from the vendored `MathNetNumerics/Numerics`, which appear whenever it recompiles. Step
    0445 logged the same warnings, and none come from this step.
- constructor-unit-tests: `Passed!`, 1783 of 1783, 106 s against 180 s. Checkpoint 444's baseline is 1783, so
  the count is unchanged.
- ui-smoke: `Passed!`, 264 of 264, 226 s.
  - The deadline is 600 s (`attempt_timeout_seconds`).
  - The count equals the checkpointed baseline of 264.
- ui-tests over `AppConfigTests`, `AppContextTests` and `DataLocationsWindowTests`: `Passed!`, 25 of 25, 37 s
  against 360 s.
- The five widened facts each show `Passed` in their TRX.
- elevation (ELEV004 excluded, no comparison base, all 13 projects): `ELEV-GATE: OK - 0 new finding(s)`, 216 s
  against 600 s.
- The 21 s43 invariants: each gate command answers `INVARIANT HOLDS`.
- No run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0446-build.log`
- `specs/0050/.artifacts/0446-constructor-unit-tests.log`
- `specs/0050/.artifacts/0446-ui-smoke.log`
- `specs/0050/.artifacts/0446-ui-tests.log`
- `specs/0050/.artifacts/0446-elevation.log`
- `specs/0050/.artifacts/0446-invariants.log`

## Gotchas

- `AppConfigTests`' shared `assertSavedLocations` helper is also called by two facts that the test census does
  not list:
  - `readDataLocations keeps a configured location…`;
  - `readDataLocations replaces a refused folder…`.

  So the `MaterialSourcesFolder` write-back check is made inline in the one authorized fact, and the helper is
  unchanged. Widening the helper would have changed the expectations of unlisted test identities.
- The App's own `appsettings.json` is `{ "appSettings": {}, "connectionStrings": {} }` and is not in `edits`. A
  missing key's default is written back on first read, as for every other location.
- Operator rulings R0001–R0005 concern steps 0427–0436. This step touches none of their files or behaviours.
