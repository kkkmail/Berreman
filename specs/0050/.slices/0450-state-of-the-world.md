# Where we are

Step 0450 builds the Online sources surface of spec 0050 S13.1 (obligation S131-SOURCES-VIEW). It is the pure Elmish
model, update and view over the import context's material sources. Earlier steps built what it rides:

- step 443 declared the `MaterialSourceProxy` seam and its in-memory double;
- step 444 added `pullRecord`, `refreshPulls` and `updateSourceSnapshot` on `CatalogueImportContext`;
- step 449 composed refractiveindex.info into the app scope.

Step 451 hosts this surface in the `MaterialSourcesWindow`, and step 452 opens that window from the Materials window.

# What's working

- Added `MaterialSourcesView`. It searches the shown source, pulls a record through `pullRecord`, and runs Update
  snapshot through an injected `SourceSnapshotJobHost`, one update at a time.
- The update reports per-file progress, can be cancelled, and returns the job to Idle on a failure.
- `SourceSnapshotJobHost` mirrors `KitArchiveJobHost` with `posting`, `onUiThread`, `immediate` and `inert`. A raise
  ends as `PullSourceErr (SourceSnapshotIoErr _)`.
- Every caption a pull or a refresh produces reaches the opener through `pullsApplied`.
- Added the Online sources UiIds group in Controls. `ConstructorUiIdOfDomain` gained the pull, refresh and error
  captions and the choice, hit-row and Pull ids.
- Added `MaterialSourcesModelTests` with the six facts the slice names.

# Tests

These are development checks. The gate engine runs the step's roster after this session and is the authority.

- **build:** `dotnet build Berreman.slnx -c Release` exits 0 in 18 s, against a 600 s deadline. There are 0 errors,
  and the only warnings are the exempt NU1902.
- **constructor-unit-tests, the whole suite as the gate selects it:** `Passed!`, 1801 of 1801 with 0 skipped, in
  103 s against a 180 s deadline. That is 1795 + 6. The new class alone passes 6 of 6.
- **ui-tests, filtered to `OpticalConstructor.Ui.Tests.MaterialsWindowTests`:** `Passed!`, 61 of 61, in 13 s against
  a 360 s deadline.
- **elevation, over all 13 projects:** `ELEV-GATE: OK - 0 new finding(s)` in 227 s against a 600 s deadline.
  - A restating comment was deleted after this run. Deleting a comment changes no declaration, and the solution and
    the new class were re-run after it.
- **The twenty-one s43 invariants:** each decoded `git grep`, with `--untracked` so the new files are searched,
  finds nothing.
- **ui-smoke:** not run. No existing view changed, and no smoke fact renders the new surface until step 451's window.
  Its count is unchanged.
- No development run exceeded its gate's effective deadline.

```yaml
gates:
  constructor_unit_tests: 1801
  ui_tests: 61
```

# Architecture

- **The surface follows the established window seam.**
  - `MaterialSourcesContext` holds the import seam, the job host and the `pullsApplied` callback.
  - The model is a plain record over that context.
  - The update is pure apart from the context's fields.
  - The view is a projection inside a `ScrollViewer` over a `MinWidthPanel` with a minimum width of 520, as
    `dataLocationsView` scrolls.
- **One update at a time, enforced in the model.**
  - `UpdateSourceSnapshot` and `ShowMaterialSource` change nothing while the job runs.
  - `CancelSourceSnapshot` reaches the handle only while the job runs.
- **The init is the member `MaterialSourcesContext.initMaterialSources`** (ELEV013). This follows `initImportDialog`
  (step 0107) and `initImportWizard` (step 0308).
- No stored format, schema, package, gate or analyzer configuration changed. R0001–R0005 are untouched.

# Deferred

Nothing.

# Gotchas

- **`initMaterialSources` is a member, not the free function the how_to names.** A free function of one of our records
  and nothing else is refused by ELEV013.
  - Callers write `context.initMaterialSources`.
  - Step 451's `Program.mkProgram` initializer is `fun () -> context.initMaterialSources, …`.
  - The contract symbol lives at `OpticalConstructor.Ui.MaterialSourcesView.MaterialSourcesContext.initMaterialSources`.
- **The progress mapping goes through `SourceSnapshotFileCount.jobStepCount` and `SourceSnapshotProgress.jobProgress`,**
  as `KitArchiveJob.fs` does. The how_to's `JobStepCount filesDone.value` would be an ELEV005 rewrap. The resulting
  values are the same.
- **`ShowMaterialSource` is held while an update runs,** so a finished update never labels another source with its
  version.
- **Two shared buttons have no state of their own.**
  - The shown source's choice is an `actionButton` on `positiveBackground`, because `verbButton` has no fill.
  - Update snapshot is disabled through a wrapping `Border`, as in `DataStepView`.
- **The shared `verbButton` / `actionButton` boxes carry their id as AutomationId and key, but no `Name`.**
  `MaterialsWindowView.fs` is outside this step's edits. Every other control carries its id as both `Name` and
  `AutomationId`, and the UI tests' finders accept either.
- **The `immediate` host runs the whole update inside `startSourceSnapshot`.** Its callbacks therefore fire before the
  update returns the Running model. A test records them and feeds them through `updateMaterialSources` afterwards; it
  never dispatches them re-entrantly.

# Changelog

- 2026-11-16, step 0450: added the Online sources surface. It searches, pulls, and runs Update snapshot with per-file
  progress and Cancel through `SourceSnapshotJobHost`. Added its UiIds and captions and the six
  `MaterialSourcesModelTests` facts; constructor-unit-tests go from 1795 to 1801.
