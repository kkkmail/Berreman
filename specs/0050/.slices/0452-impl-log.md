# Step 0452 — impl log (WIRE_UI, S131-MATERIALS-WIRE)

## Progress

- Read the task, the arc-runner base prompt, the WIRE_UI delta, the project prompt, rulings R0001–R0005 (their
  "Binding ruling" sections) and the slice. The rulings confine their extra authority to steps 0427, 0428 and 0436;
  here they only ask that their rules be preserved. This step touches none of the files they govern.
- Surveyed `MaterialsWindowView.fs` (context, messages, update, `verbsRow`), `MaterialsWindow.fs`,
  `MaterialSourcesWindow.fs` (step 451), `MaterialSourcesView.fs` (step 450), `MaterialSourcePulls.fs` (step 444),
  `WindowLauncher.fs`, `UiIds.fs`, `MaterialsWindowTests.fs`, `MaterialSourcesWindowTests.fs` and the
  `MaterialSourcesWindowFixture` in `ScrollExtentTests.fs`.
- Added the id, the context field, the message, its update arm and the Browse-mode verb; baked the opener in
  `MaterialsWindow`.
- Added the four census facts to `MaterialsWindowTests` and the recording `openOnlineSources` to `stubContext`.
- Build: green. ui-tests filtered to `MaterialsWindowTests`: 48 of 48, the three new facts among them.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: `materialsOnlineSourcesButton`
  (`"MaterialsOnlineSourcesButton"`), in the Materials window group under a one-line comment citing spec 0050 S13.1.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindowView.fs`:
  - `MaterialsWindowContext.openOnlineSources : unit -> unit`;
  - `MaterialsWindowMsg.OpenOnlineSources`;
  - the `updateMaterialsWindow` arm, which calls the context and answers `m`;
  - in `verbsRow`, Browse mode, after the export verbs, the `Online sources…` verb while
    `catalogueImports.materialSources` is not empty;
  - the `verbsRow` doc, two lines added.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialsWindow.fs`: the context's `openOnlineSources`
  is `openBrowse WindowLauncher.MaterialSourcesWindowKey (fun () -> MaterialSourcesWindow(catalogueImports, fun summary
  -> dispatch (MaterialsWindowView.MaterialsImportApplied summary)) :> Window)`. The class doc now names Online sources
  and S13.1.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialsWindowTests.fs`:
  - opens `MaterialSources` and `MaterialSourcePulls`;
  - `stubContext` records `online-sources-open`;
  - a private `siliconSource` double (step 443's `InMemoryMaterialSource`) and `offeringSources`;
  - three ui-tests facts in a new "Online sources" section;
  - the ui-smoke acceptance fact at the end of the file.

These are exactly the step's four authorized edits. `specs/0050/.manifest.state.json` was already modified when the
round started; it is the supervisor's, and this round did not touch it.

## Decisions

- **The button is a `verbButton`,** the same keyed box every Materials verb uses. Its press dispatches
  `OpenOnlineSources`, the same message the fact sends (001 §P).
- **The offered fact presses the real button on the mounted view.** It goes through `mountExportView`, the file's
  existing host. It checks three things:
  - the observed message is `OpenOnlineSources` alone;
  - the model answered equals the model before;
  - the stub recorded exactly one open.
  The Select model and the source-less model are rendered too, and neither shows the button.
- **The pull fact uses `freshExportContext`.** That is the in-memory library with the Built-in catalogue imported, so
  the pulled Silicon lands beside the built-in materials, as it would in the app.
  - Before the pull, `filteredEntries` holds no `Si (Silicon), Aspnes`.
  - `pullRecord` answers one result, whose caption is `Pulled Si (Silicon), Aspnes as version 1.`.
  - `MaterialsImportApplied` of that caption sets `importStatus`.
  - `filteredEntries` then lists exactly one entry with the pulled entry id and the pulled name.
  - The mounted view shows the caption in `materialsImportStatusText`.
- **The acceptance fact does not settle the pending start-up imports.** The how_to composes
  `AppContext.create (TestAppScope.fileLocations (FolderName "MaterialsWindowTests")) WorkbenchSettings.defaults` and
  nothing more. The Online sources verb depends only on the import seam's sources. `fileLocations` mints a fresh
  instance folder, so the sources folder holds no snapshot. Nothing presses Update snapshot, so nothing is downloaded.
- **Single instance is the launcher's process-wide registry.** `openBrowse` builds a fresh `WindowLauncher` per call,
  but `WindowLauncher.create` reads and writes the one `WindowRegistry.live`, so a second press activates the live
  window. The acceptance fact checks that with `Assert.Same`.
- **No parallel race on the shared registry.** `MaterialsWindowTests` belongs to the `Library picker isolation`
  collection (`DisableParallelization = true`), so its acceptance fact never runs beside
  `MaterialSourcesWindowTests`' registry fact.

## Testing state

These are development checks. The gate engine runs the roster after this session and is the authority.

- **build:** exits 0 in 85 s, against a 600 s deadline, with 0 errors. The only warnings are the exempt NU advisories
  and the vendored MathNet's SYSLIB0051, which predates this step.
- **ui-tests, filtered to `MaterialsWindowTests` as the gate selects:** `Passed!`, 48 of 48, in 9 s against a 360 s
  deadline. The step's three new identities are among them. That is the class's earlier 45 plus 3.
- **ui-smoke, the whole suite as the gate selects:** `Passed!`, 267 of 267, in 226 s against a 600 s deadline. That is
  checkpoint 451's 266 plus 1, the acceptance fact. It ran beside elevation.
- **constructor-unit-tests, the whole suite as the gate selects:** `Passed!`, 1801 of 1801, in 107 s against a
  180 s deadline. The count is unchanged.
- **elevation, all 13 projects (no comparison base):** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, in 234 s
  against a 600 s deadline.
- **The twenty-one s43 invariants:** each decoded `git grep`, run with `--untracked`, finds nothing.
- No development run exceeded its effective deadline.

## Artifacts

- `specs/0050/.artifacts/0452-build-dev.log`
- `specs/0050/.artifacts/0452-ui-tests-dev.log`
- `specs/0050/.artifacts/0452-ui-smoke-dev.log`
- `specs/0050/.artifacts/0452-elevation-dev.log`
- `specs/0050/.artifacts/0452-constructor-unit-tests-dev.log`

## Gotchas

- **The Silicon double is a private mirror of step 451's `MaterialSourcesWindowFixture.siliconSource`.** That fixture
  lives in `ScrollExtentTests.fs`, which compiles after `MaterialsWindowTests.fs`. Neither that file nor the project
  file is among this step's edits, so moving it here was not authorized. The mirror follows the precedent of
  `MaterialSourceProxyTests`, `MaterialSourcePullTests` and `MaterialSourcesModelTests`, each of which keeps its own
  Silicon record. Its doc names the fixture it mirrors.
- **A pull's caption reaches the window through the opener's callback.** `MaterialSourcesWindow`'s `pullsApplied` is
  the Materials window's `dispatch` of `MaterialsImportApplied`. That callback reads the window's captured `dispatch`
  when it is called, so it is never the pre-init `ignore`.
