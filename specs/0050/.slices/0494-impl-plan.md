# Step 0494 — impl plan (WIRE_UI: close the composition root)

## Approach

No product behaviour changes. The root already composes everything 001 asks of it (steps 83–492); this step rewrites
the stale docs, brings AGENTS.md up to the facts spec 0050 changed, and proves the composition end to end headless.

1. **Docs.**
   - `OpticalConstructor.App/Program.fs`: the file doc's first paragraph (spec 0038's launcher narrative) becomes at
     most four lines naming `Startup`, `AppContext.create`, `LauncherWindow`, `KitStartWindow`, `DataLocationsWindow`
     and `KitConstructorWindows.showConstructorIn`, citing spec 0050 Close; the FuncUI-clone paragraph stays. The docs
     of `Startup.workbenchSettings, dataLocations` and `Startup.context` become one to three lines each.
   - `OpticalConstructor.Ui/AppContext.fs`: the type doc (the file's only top doc) becomes one to three lines: the one
     app scope, composed at the root over the configured locations (the SQLite library stores and the catalogue import
     of every start), injected into the launcher, the start screen and every window. No in-memory / re-seed /
     experiment-collection / scene-store / `STORE_XDUO_*` wording remains in the two files.
2. **AGENTS.md**, only where spec 0050 changed a stated fact: (a) the test-locations sentence in `## Build & test`;
   (b) the `## Project layout` notes for Domain and Storage and four new lines (`BerremanTestSupport/`,
   `Catalogue/Builtin/`, `OpticalConstructor.Database`, `data/`); (c) three Gotchas bullets after "The Main scene's
   Library is LIVE" (library is data; kit folders; start screen), each at most six lines; (d) one sentence on the flow
   strip appended to "Main ribbon".
3. **Tests**, `OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`, three new `ui-smoke` facts with the
   slice's exact names; module doc gains one to three lines citing spec 0050 Close. Every press is a mouse
   down/up at the box centre (`LibraryWindowTests.clickOn`), every opened window observed through
   `Window.WindowOpenedEvent` (`LauncherTests.clickAndObserve`) and closed in a `finally`; waits are
   `Dispatcher.UIThread.RunJobs` only.
   - (a) file library under the test output (`TestAppScope.fileLocations`), `AppContext.create` as `Startup` composes
     it, the shown launcher's own start-up import dialog applied, then silicon read from the database; Inverse start
     screen → New kit… → Inverse window (title, `.kit-json`, `.oc/lock`, history), every Inverse flow step pressed and
     its surface shown, Close kit releases the lock; Main start screen → Open kit… → Main window over the same kit,
     its three steps and none of the six Inverse ones, Close kit; Data locations link → the window's library-file
     and kits rows.
   - (b), (c) one private function over `CommittedKit`: `TestAppScope.openCommittedReadOnly`, the model mounted as
     `KitWindowTests.withKitWindow` mounts it (`kitWindowView`, `updateTableAndElementRotation`,
     `KitWindowShell.inert`), caption ` (read-only)`, every Inverse step pressed, window closed, kit closed as
     `KitAcceptanceTests.closeCommittedReadOnly` does, `assertCommittedUnchanged`.

## Files

- `Berreman/OpticalConstructor/OpticalConstructor.App/Program.fs` (docs only)
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/AppContext.fs` (docs only)
- `AGENTS.md`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs`

## Risks

- A fresh library's start-up import only *awaits confirmation* (`importOnStart` → `CatalogueAwaitsChoices`, spec 0050
  §3.7), so silicon is in the database only after the confirmation. The fact settles it through the launcher's own
  import dialog (shown on the launcher's first open, Apply), the app's real path; recorded in the impl-log.
- The invariant greps search `Berreman/` for removed names; no doc or test may mention them.
- Elevation runs over the two product files (comment-only edits); test projects are out of the analyzers' scope.
