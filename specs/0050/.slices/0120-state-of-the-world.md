# Where we are

Slice 0120 wires S33-LIBRARY-EXPORT after the shared export helpers and Materials window integration in 0118–0119. The Library window delegates selected, shown, and whole-user-library exports to CatalogueImportContext and projects selected-entry licence attribution. Attempt 03 repairs the single-file export fixture's missing destination directory, identified from the supervisor's failing file-existence assertion. All seven authorized test facts and their assertions are preserved; supervisor verification of the repair is pending.

# What's working

- Added Browse-mode Export, Export shown to catalogue, and Export my library verbs using existing pickers and configured export folders.
- Rendered shared success and typed refusal captions while preserving selection, mode, and confirmation gates.
- Displayed selected-sample material attribution through the existing catalogue proxy and shared panel.
- Added five stable intent ids and seven authorized facts, including real-view attribution input and a database-backed export/import round trip.
- Imported CatalogueImportStore in the Library view so both folder-export callbacks can resolve the existing CatalogueExportTarget.ofFolder extension.
- Created the selected-file export fixture's parent directory while preserving absent-folder refusal coverage and all export assertions.

# Tests

The attempt-03 task reports green review and passed build gates, then ui-tests exit 2: 71 total, 70 succeeded, 1 failed, 0 skipped. The failure is `ExportSelectedLibraryEntry writes the selected entry's file and pins its version`, at LibraryWindowTests.fs:406 before this repair. Source inspection traced the absent file to the fixture's uncreated parent directory. The single-file export setup now creates it before invoking the unchanged export path. No post-repair gate result is available.

No build, tests, analyzers, or invariant gates were run by this worker: the WIRE_UI protocol assigns gate execution exclusively to the supervisor. The unchanged resolved roster is build, elevation, ui-smoke, selected ui-tests (LibraryWindowTests and ScrollExtentTests), and the eleven s14 invariants. The seven authorized facts and every assertion remain intact. No new test identity was added in this retry. Post-repair compilation, warning status and runtime results are not claimed.

The following repeats the latest persisted smoke baseline from checkpoint 118, not a result from this round. The checkpoint file has no ui_tests baseline; the supervisor must measure selected test counts. No checkpoint or census was edited.

```yaml
gates:
  ui_smoke_tests: 215
```

# Architecture

Attempt 03 changes fixture setup only. CatalogueImportContext.writeEntryFile already delegates to LibraryFileText.writeAtomically, which writes a temporary sibling file in the destination directory. The selected-entry fixture now provides that directory, matching MaterialsWindowTests and the save-picker precondition. The shared folder-path helper stays unchanged because catalogue-export refusal coverage requires an absent destination. No production behavior, persistence shape, or migration is added.

LibraryWindowView resolves a selected entry's latest held envelope only in Browse mode, using the existing elevated id conversion and catalogue proxy. The three semantic messages call exportEntry, exportEntries, and exportUserLibrary. Dependency closure, version pinning, serialization and refusal rules remain in the existing domain/storage seams. Shared MaterialsWindowView helpers provide the asynchronous pickers and wrapping attribution panel; no new persistence shape or migration is introduced.

Tests reuse BuiltinCatalogue content factories and sealing, in-memory catalogue stores, the existing headless session and view helpers, and TestAppScope.fileLocations. Export files use fresh GUID folders under the test output's LibraryWindowTests folder. The database round trip asserts both pending rows, unchanged stores/log before confirmation, and unchanged material/sample values after settling the complete choice list.

# Deferred

No slice requirement is deferred. Gate execution is the supervisor's next stage; no gate pass or commit readiness is asserted by this worker.

# Gotchas

- A freshly allocated GUID folder path is not an existing directory. Single-file export setup creates it locally; the shared helper must keep returning an uncreated path for the refused catalogue-export fact.
- Opening the domain CatalogueImport module does not expose the optional CatalogueExportTarget.ofFolder extension; its CatalogueImportStore module must also be opened.
- The new verbs stay in the existing wrapping row and scrollable right panel; ScrollExtentTests remains in the supervisor's selected roster.
- A missing or unreadable latest envelope makes selected export inert. Attribution read errors omit the panel without replacing the selected-entry description.
- The shared verbButtonOn handler already refreshes callbacks when selection changes, as established by slice 0119.
- The pre-existing specs/0050/.expected-red-observed-json and .manifest.state.json modifications belong to the supervisor and were left untouched.
- Source diff counts agree with and without CR-at-EOL ignoring; no source line-ending churn was introduced.

# Changelog

- 2026-11-07: Implemented Library export and attribution wiring, stable ids, and seven authorized test facts; handed gate execution to the supervisor.
- 2026-11-07 (attempt 02): Repaired the two supervisor-reported FS0039 errors by importing the existing CatalogueImportStore extension module; preserved acceptance and handed verification back to the supervisor.
- 2026-11-07 (attempt 03): Repaired the selected-entry export fixture by creating its destination directory; preserved every assertion and handed verification back to the supervisor.
