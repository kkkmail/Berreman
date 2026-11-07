# Where we are

Slice 0120 wires S33-LIBRARY-EXPORT after the shared export helpers and Materials window integration in 0118–0119. The Library window now delegates selected, shown, and whole-user-library exports to CatalogueImportContext and projects selected-entry licence attribution. Attempt 02 adds the missing extension-module import identified by the supervisor's build failure. All seven authorized test facts are preserved; supervisor verification of the repair is pending.

# What's working

- Added Browse-mode Export, Export shown to catalogue, and Export my library verbs using existing pickers and configured export folders.
- Rendered shared success and typed refusal captions while preserving selection, mode, and confirmation gates.
- Displayed selected-sample material attribution through the existing catalogue proxy and shared panel.
- Added five stable intent ids and seven authorized facts, including real-view attribution input and a database-backed export/import round trip.
- Imported CatalogueImportStore in the Library view so both folder-export callbacks can resolve the existing CatalogueExportTarget.ofFolder extension.

# Tests

The supervisor's attempt-01 build failed with two FS0039 errors on the folder-export callbacks. Attempt 02 repairs their missing module import; no post-repair gate result is available. The supplied output also contains two SYSLIB0051 warnings from vendored MathNetNumerics/Exceptions.cs; those files are outside the authorized edits and remain unchanged.

No build, tests, analyzers, or invariant gates were run: the WIRE_UI worker protocol assigns gate execution exclusively to the supervisor. The unchanged resolved roster is build, elevation, ui-smoke, selected ui-tests (LibraryWindowTests and ScrollExtentTests), and the eleven s14 invariants. Exactly seven facts were added without a ui-smoke trait; existing tests and assertions remain intact. Compilation, warning status and runtime results are not claimed.

The following repeats the latest persisted smoke baseline from checkpoint 118, not a result from this round. The checkpoint file has no ui_tests baseline; the supervisor must measure selected test counts. No checkpoint or census was edited.

```yaml
gates:
  ui_smoke_tests: 215
```

# Architecture

The retry reuses the existing storage extension by opening its defining module, matching MaterialsWindowView and LibraryWindowTests. No new factory, conversion, dependency, or behavior is added.

LibraryWindowView resolves a selected entry's latest held envelope only in Browse mode, using the existing elevated id conversion and catalogue proxy. The three semantic messages call exportEntry, exportEntries, and exportUserLibrary. Dependency closure, version pinning, serialization and refusal rules remain in the existing domain/storage seams. Shared MaterialsWindowView helpers provide the asynchronous pickers and wrapping attribution panel; no new persistence shape or migration is introduced.

Tests reuse BuiltinCatalogue content factories and sealing, in-memory catalogue stores, the existing headless session and view helpers, and TestAppScope.fileLocations. Export files use fresh GUID folders under the test output's LibraryWindowTests folder. The database round trip asserts both pending rows, unchanged stores/log before confirmation, and unchanged material/sample values after settling the complete choice list.

# Deferred

No slice requirement is deferred. Gate execution is the supervisor's next stage; no gate pass or commit readiness is asserted by this worker.

# Gotchas

- Opening the domain CatalogueImport module does not expose the optional CatalogueExportTarget.ofFolder extension; its CatalogueImportStore module must also be opened.
- The new verbs stay in the existing wrapping row and scrollable right panel; ScrollExtentTests remains in the supervisor's selected roster.
- A missing or unreadable latest envelope makes selected export inert. Attribution read errors omit the panel without replacing the selected-entry description.
- The shared verbButtonOn handler already refreshes callbacks when selection changes, as established by slice 0119.
- The pre-existing specs/0050/.manifest.state.json modification belongs to the supervisor and was left untouched.
- Source diff counts agree with and without CR-at-EOL ignoring; no source line-ending churn was introduced.

# Changelog

- 2026-11-07: Implemented Library export and attribution wiring, stable ids, and seven authorized test facts; handed gate execution to the supervisor.
- 2026-11-07 (attempt 02): Repaired the two supervisor-reported FS0039 errors by importing the existing CatalogueImportStore extension module; preserved acceptance and handed verification back to the supervisor.
