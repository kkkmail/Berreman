# Step 0193 — state of the world

# Where we are

Step 0193 (`IMPLEMENT`) is in section S5.3. It builds the Data locations screen of 001 §G: "a Launcher Data locations
panel shows them, each with Open folder". The screen has its rows, its message, its model and its view, as a pure
Elmish update over two injected seams:
- `DataLocations` with `DataLocationKind` (steps 81 and 192);
- `ExplorerProxy` (step 182).

Step 0194 hosts the screen in the single-instance `DataLocationsWindow`.

# What's working

- Add `DataLocations.rows`. It lists the seven locations in `DataLocationKind.all` order, the library by its file and
  folder, and an in-memory library without a folder. It reads no disk.
- Add `DataLocationsView`: the context, the model, `initDataLocations`, `OpenDataLocationFolder` and its update, and
  the scrolling view. Open folder goes through the explorer seam and a refusal is told.
- Add `DataLocationKind.caption` / `uiKey`, the `dataLocationRow` / `dataLocationPathText` /
  `dataLocationOpenFolderButton` ids, `dataLocationsStatusText`, and the `FolderName` / `FileName` `pathCaption`
  members.
- Add `DataLocationsTests` (4 constructor facts) and `DataLocationsWindowTests` (2 ui-tests and 1 ui-smoke fact).

# Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests and ui-smoke (all discovered);
- ui-tests over `OpticalConstructor.Ui.Tests.DataLocationsWindowTests`;
- the fourteen s21 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0193-*.log`:

- build: 0 errors, 0 warnings;
- constructor-unit-tests: 1215 passed. That is 1211 + 4, against checkpoint 191;
- ui-smoke: 233 passed. That is 232 + 1, against checkpoint 190;
- ui-tests over the selected class: 2 passed;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s21 invariant greps: none finds anything.

No test is removed or renamed, and no assertion is weakened. The YAML carries the latest recorded checkpoint floors;
the supervisor measures this step's own.

```yaml
gates:
  constructor_unit_tests: 1211
  ui_smoke_tests: 232
```

# Architecture

- **`initDataLocations` is a member of `DataLocationsContext`** (`context.initDataLocations`), not the slice's free
  function. ELEV013 refuses a public free function of one of our types, and the analyzers win over a document
  (operator, 0048 `.manual/002` R6). Its name and body are unchanged. Measured: `0193-elevation.log`, then
  `0193-elevation-2.log` after the repair.
- **A row's path is converted at its source type.** `FolderName.pathCaption` and `FileName.pathCaption` are in
  `ConstructorUiIdOfDomain.fs`; nothing re-wraps a `.value` at the call site.
- **One rule for the library's file.** The library row reads `LibraryDatabase.tryFileOf` (step 192), and opens its
  folder through STL's `FileName.tryGetFolderName`.
- **The screen is read-only.** It has no edit field, copy button or cache; the locations are edited in
  `appsettings.json` (001 §G).

# Deferred

Nothing. Every requirement of the slice is in this round.

# Gotchas

- Step 0194's brief writes `initDataLocations { locations = locations; explorer = explorer }`. Write it as `({
  locations = locations; explorer = explorer } : DataLocationsContext).initDataLocations` instead (the member form,
  see Architecture).
- `LibraryDatabase` is declared in `LibraryDatabaseStore`, and `tryFileOf` extends it from `LibraryBackupStore`. A
  caller must open both modules.
- Each row's `DockPanel` is keyed by its id, so a status shown above the rows never patches one row into another.
- The ui-smoke fact presses Open folder over the in-memory explorer double. No fact opens a real folder in Explorer.

# Changelog

- 2026-11-09, attempt 01:
  - the Data locations rows, model, update and view;
  - the ids and captions;
  - the seven declared facts;
  - `initDataLocations` as a context member (ELEV013).
