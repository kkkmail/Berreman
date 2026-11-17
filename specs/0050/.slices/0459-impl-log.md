# Step 0459 — impl log

## Progress

- Read the task, both system prompts, the project prompt and rulings R0001–R0005. None of them touches this step:
  their extra authority is confined to 0427 / 0428 / 0436, and later steps only preserve their rules. Neither file
  this step edits is one they govern.
- Read the slice, `AppContext.fs`, `MaterialSourceStore.fs` (the three adapters' builders and
  `SourceSnapshotLayout.snapshotVersion`), `TestAppScope.fs`, the target fact in `AppContextTests.fs`, and the UI
  consumers of `catalogueImports.materialSources` (`MaterialsWindowView`, `MaterialSourcesView`).
- `AppContext.compose`, shared by `create` and `recreateLibrary`, now sets `materialSources` to three sources, in
  order:
  - `createRefractiveIndexInfo`;
  - `createPtbOcdb`;
  - `createHenkeCompounds`.

  Each is built over `locations.materialSourcesFolder`, `SourceArchiveDownloader.http` and `ClockProvider.system`.
  A one-line comment cites S13.2: the order matters because the window opens on the first, and building the sources
  touches nothing. The `catalogueImports` field doc names all three sources and cites S13.1 and S13.2.
  `CatalogueImportContext.create` is untouched, so every other composition keeps `[]`.
- `AppContextTests`' fact `the app scope offers refractiveindex.info over its configured sources folder, holding no
  snapshot` keeps its name. Its single-source assertion widens:
  - `catalogueImports.materialSources` names `refractiveindex.info`, `PTB OCDB` and `CXRO Henke`, in that order (one
    list equality);
  - every source's `snapshotVersion ()` answers `Ok None`;
  - the sources folder of `TestAppScope.testLocations (FolderName "AppContextTests")` still does not exist.

  Its comment now cites S13.1 and S13.2. No fact runs `updateSnapshot`, so nothing reaches the network.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/AppContextTests.fs`

These are exactly the step's two authorized edits.

## Testing state

These are development checks only. The gate engine runs the roster after this session and is the authority. Every
figure is from the final source.

- **build**: exit 0 in 83 s (600 s deadline), 0 errors.
  - Warnings: 12 × NU1902 (exempt) and 4 × SYSLIB0051. The SYSLIB0051 warnings come from the vendored
    `MathNetNumerics/Numerics/Exceptions.cs`; they are pre-existing and recorded at step 0458.
  - There is no FS or MSB warning.
  - Log: `.artifacts/0459-dev-build.log`.
- **ui-tests, the gate's own command over the selected class** (`OpticalConstructor.Ui.Tests.AppContextTests`, with
  `ui-smoke` and `kit-acceptance` excluded): `Passed!`, 18 of 18, in 38 s (360 s deadline).
  - The class has 19 facts. One carries `Category=ui-smoke`, so the gate counts 18. Step 0449's "19 of 19" was a
    run without the trait filters (confirmed with `--list-tests`: 19 without, 18 with).
  - This step adds and removes no fact, so the count is unchanged.
  - Log: `.artifacts/0459-dev-ui-tests.log`.
- **ui-smoke, as the gate selects it** (`all_discovered`): `Passed!`, 267 of 267, in 216 s (600 s deadline).
  Log: `.artifacts/0459-dev-ui-smoke.log`.
- **constructor-unit-tests, as the gate selects it** (`all_discovered`): `Passed!`, 1823 of 1823, in 105 s (180 s
  deadline). Log: `.artifacts/0459-dev-constructor-unit-tests.log`.
- **elevation, all 13 projects** (standalone, no comparison base): `ELEV-GATE: OK - 0 new finding(s) in 13
  project(s), 233s`, with 0 known findings (600 s deadline). Log: `.artifacts/0459-dev-elevation.log`.
- **The 21 s44 invariants**: each decoded `git grep` finds nothing (exit 1), both as written and with `--untracked`.
  Log: `.artifacts/0459-dev-invariants.log`.
- **LF**: `git diff --numstat` equals `--ignore-cr-at-eol` (16 / 5 over the two files).

No development run exceeded its gate's effective deadline.

## Artifacts

- `.artifacts/0459-dev-build.log`
- `.artifacts/0459-dev-ui-tests.log`
- `.artifacts/0459-dev-ui-smoke.log`
- `.artifacts/0459-dev-constructor-unit-tests.log`
- `.artifacts/0459-dev-elevation.log`
- `.artifacts/0459-dev-invariants.log`

## Gotchas

- **The ui-tests count is 18, not 19.** The gate excludes the `ui-smoke` trait, and one `AppContextTests` fact (the
  headless acceptance at the file's end) carries it. Step 0449's SoW reports 19 from an unfiltered-trait run.
- **"Nothing exists under the sources folder"** is asserted as the folder's absence, as step 0449's fact did. All
  three adapters' `snapshotVersion` only call `File.Exists` on their stamp, so reading them creates nothing.
- **No red run was taken for the widened expectation.** Against the old one-source composition, its list equality
  could not hold (one name against three), and the fact passes over the new composition.
- **Other app-scope facts see three sources now.** Of the UI code, only `MaterialsWindowView` reads the list, and
  only to show `Online sources…` when it is non-empty, so nothing it renders changes. `MaterialsWindowTests`,
  `ScrollExtentTests` and `MaterialSourcesWindowTests` compose their own sources. ui-smoke stays at 267 of 267.
