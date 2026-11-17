# Step 0451 — implementation log

## Progress

- Read the protocol, the project prompt, AGENTS.md, rulings R0001–R0005 and the slice.
  - The rulings bind steps 0427, 0428 and 0436. This step edits none of their files, and their behaviour is
    preserved.
- Read step 450's `MaterialSourcesView` and its SoW, then the code this step follows:
  - the windows `MaterialsWindow`, `KitStartWindow` and `DataLocationsWindow`;
  - the launcher;
  - the tests `WireUiFinalCompositionTests`, `DataLocationsWindowTests` and `ScrollExtentTests`;
  - step 443's double and step 444's `pullRecord`.
- Added `ConstructorUiId.materialSourcesWindow` and `WindowKey.MaterialSourcesWindowKey`, then the window.
- Wired the window into `rootWindowFactory`, then added the reachability fact and the new test class.
- Ran the development checks listed under Testing state.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Controls/UiIds.fs`: `materialSourcesWindow`
  (`"MaterialSourcesWindow"`) heads step 450's Online sources group, with a one-line doc citing S13.1.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/WindowLauncher.fs`: `WindowKey` gains `MaterialSourcesWindowKey`,
  documented in one line. The module doc's single-instance list names the Online sources window.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/MaterialSourcesWindow.fs` (new): `type MaterialSourcesWindow`.
  - Its constructor is `(imports : CatalogueImportContext, pullsApplied : ControlCaption -> unit)`, and it inherits
    `HostWindow`.
  - It is titled `Online sources` and carries `materialSourcesWindow` as `Name` and `AutomationId`; its automation
    name is its title.
  - It opens at 720 × 640 and calls `WindowFit.keepOnScreen`.
  - Dispatch is captured from the init command.
  - It runs `Program.mkProgram` over `context.initMaterialSources`, `updateMaterialSources` and `materialSourcesView`.
    The context is `{ imports; jobs = SourceSnapshotJobHost.onUiThread …; pullsApplied }`.
  - `Closed` dispatches `CancelSourceSnapshot`.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj`: one `<Compile>` line, directly
  after `MaterialSourcesView.fs` and before `MaterialsWindow.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`: `rootWindowFactory`
  answers `MaterialSourcesWindowKey` with `MaterialSourcesWindow(ctx.catalogueImports, ignore)`. Every fact keeps its
  name, keys and assertions.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/ScrollExtentTests.fs`:
  - `module internal MaterialSourcesWindowFixture` holds the shared Online sources fixture, following the file's
    `StudyFitViewFixture` precedent. It provides:
    - the source name `Test source`;
    - the version `2026-11-01`;
    - the Silicon hit (`main/Si/nk/Aspnes.yml`, `Si (Silicon), Aspnes`);
    - `siliconSource ()`, step 443's double with that snapshot installed;
    - `importsOffering`, which is `CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy` with
      `materialSources` set by record update.
  - Added the fact `the Online sources window leaves everything reachable`: `assertReachable "OnlineSources"` over the
    window, offering the double and ignoring its caption.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/MaterialSourcesWindowTests.fs` (new):
  `module MaterialSourcesWindowTests`, the five facts the census names.
  - Three facts have no trait.
  - `pressing Search then Pull pulls the record and tells the opener` and `the Online sources window opens once through
    the registry` carry `ui-smoke`.
  - Windows are shown headless at 800 × 600, as `DataLocationsWindowTests` shows its window. Boxes are pressed at their
    centre, as `LibraryWindowTests.clickOn` presses them.
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj`:
  `MaterialSourcesWindowTests.fs` is appended last in the compile list, with no comment.

Declared action `ADD_TEST OpticalConstructor.Ui.Tests.MaterialSourcesWindowTests`: done. The module declared is
`MaterialSourcesWindowTests`.

## Testing state

These are development checks; the gate engine runs the step's roster after this session and is the authority. Logs
and TRX files are under the artifacts folder.

- **build:** `dotnet build Berreman.slnx -c Release` exits 0 in 85 s, against a 600 s deadline, with 0 errors.
  - The warnings are the exempt NU1902.
  - SYSLIB0051 comes from the vendored MathNet C# when MathNet recompiles. It is in 659 earlier artifact logs and is
    not our code.
- **ui-tests, filtered to the selected classes `MaterialSourcesWindowTests` and `ScrollExtentTests`:** `Passed!`, 24
  of 24, in 26 s against a 360 s deadline.
  - The four new identities are all in the TRX and passed.
- **ui-smoke, filtered to the touched classes `MaterialSourcesWindowTests` and `WireUiFinalCompositionTests`:** 5 of 5
  pass.
- **ui-smoke, the whole suite as the gate selects it (`all_discovered`, `whole_suite: light`):** `Passed!`, 266 of 266,
  in 216 s against a 600 s deadline.
  - That is 264, checkpoint 449's baseline, plus 2.
- **constructor-unit-tests, the whole suite as the gate selects it (`all_discovered`, `whole_suite: light`):**
  `Passed!`, 1801 of 1801 with 0 skipped, in 104 s against a 180 s deadline. The count is unchanged.
- **elevation, from the arc origin `f395597c` (`ASG_ARC_BASE_COMMIT`):** `ELEV-GATE: OK - 0 new finding(s) in 13
  project(s)`, in 227 s against a 600 s deadline.
- **The twenty-one s43 invariants:** each decoded `git grep`, run with `--untracked` so the two new files are searched,
  finds nothing.
- No development run exceeded its gate's effective deadline.
- **Line endings:** every changed and new file has 0 CR bytes.

## Artifacts

All of these are in `specs/0050/.artifacts/`:

- `0451-build.log`
- `0451-ui-tests.log` and `0451-ui-tests-results/ui-tests.trx`
- `0451-ui-smoke.log` and `0451-ui-smoke-results/ui-smoke.trx` (the filtered run)
- `0451-ui-smoke-whole.log` and `0451-ui-smoke-whole-results/ui-smoke.trx`
- `0451-constructor-unit-tests.log` and `0451-constructor-unit-tests-results/constructor-unit-tests.trx`
- `0451-elevation.log`
- `0451-invariants.txt` (the decoded invariant commands)

## Gotchas

- **The jobs callbacks are lambdas, not `SourceSnapshotProgressed >> dispatch`.**
  - The composition would bind `dispatch`'s value when the context is built, before the init command sets it, so every
    update message would go to `ignore`.
  - The lambdas read the field when called, as `KitStartWindow` does (`archiveJobs`). The meaning is the one the
    how_to names.
- **The init is `context.initMaterialSources`.** Step 450 made it a member of `MaterialSourcesContext` because of
  ELEV013; see its SoW.
- **The pull runs over the bare in-memory import context the how_to names,** without importing the Built-in library
  first. The smoke fact proves the in-memory store accepts it as version 1.
- **No fact covers "cancel a running update when it closes".** The census names none, and adding one would be a new test
  identity.
  - The window composes the real `onUiThread` host, and an update of the one-record double ends before a close could
    race it.
  - The behaviour is the window's one-line `Closed` hook (`dispatch CancelSourceSnapshot`) plus step 450's model facts.
    Those facts show `CancelSourceSnapshot` reaching the running update's handle and doing nothing when idle.
- **The shared fixture lives in `ScrollExtentTests.fs`** as `module internal MaterialSourcesWindowFixture`.
  - `ScrollExtentTests` compiles before `MaterialSourcesWindowTests`, so both build the window over one fixture and do
    not copy it. This is the file's own `StudyFitViewFixture` precedent.
  - `MaterialSourcesModelTests` (step 450) keeps its private fixture in `OpticalConstructor.Tests`, which this project
    does not reference and this step may not edit.
- **The `pressAt` / `tryFindControl` helpers mirror `DataLocationsWindowTests`'s private ones,** as each UI test
  class in this project keeps its own. `DataLocationsWindowTests.fs` is outside this step's edits.
