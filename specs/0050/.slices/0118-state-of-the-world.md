# Where we are

Slice 0118 implements S33-SHARED-WINDOW-PIECES in spec 0050 S3.3. Both library windows use the shared ordinary verb helpers, the reusable folder picker supports a suggested start location, and app composition supplies the configured catalogue-export folder. Attempt 04 addresses the supervisor-reported FS0366 errors by replacing direct storage interface implementations in the test fixture with runtime proxies. All four new facts, their behavioral assertions and the production implementation are retained. Execution and acceptance belong to the supervisor under the IMPLEMENT worker protocol.

# What's working

- Extracted MaterialsWindowView.verbButtonOn and retained verbButton as its unit-callback adapter.
- Moved the folder picker into MaterialsWindowView.pickLibraryFolder with a configurable title and suggested start folder.
- Redirected Library verbs and catalogue-folder registration to the shared helpers, preserving styling, accessibility, ids, and messages.
- Configured the catalogue-export suggestion in the composition shared by AppContext.create and recreateLibrary.
- Added the declared AppContextTests fact covering the configured app folder and the standalone factory's None default.
- Replaced direct storage interface implementations with runtime test proxies, retaining public provider registration and isolated headless sessions in a nonparallel collection.

# Tests

The supervisor reported three FS0366 build errors for the non-client-implementable members of IStorageFolder, IStorageItem and IStorageProvider after attempt 03. Attempt 04 removes those direct implementations. No build, tests, analyzers, or invariants were executed by this worker, as required by IMPLEMENT worker invariant 6; the repair awaits supervisor verification. The resolved roster remains build, elevation, ui-smoke, selected ui-tests, and the eleven invariant-s14-* gates in 0118.gates. The selected classes remain AppContextTests, ImportDialogTests, LibraryWindowTests, MaterialsWindowTests, and ScrollExtentTests. The declared AppContext fact and three retry-authorized picker facts add four expected discovered ui-tests cases in total; measured counts remain the supervisor's responsibility. Existing window and import-dialog facts were left unchanged.

The confirmation fact calls MaterialsWindowView.pickLibraryFolder with Some start, observes the provider's requested path and returned folder in SuggestedStartLocation, checks title and single-selection options, and expects only the first selected FolderName on the UI thread. Cancellation covers None start and an empty result without changing the caller's prior selection. The unavailable-provider fact records the factory request, simulates PlatformNotSupportedException, and expects neither a callback nor a selection change.

```yaml
gates: {}
```

No baseline or checkpoint was authored or changed by the worker.

# Architecture

MaterialsWindowView owns the shared pointer-aware verb renderer and its unit-action adapter. LibraryWindowView declares no verbButton, verbButtonOn, or pickCatalogueFolder. Its distinct colored backup-selection renderer continues to serve backup rows. The shared ordinary renderer retains the existing Library accessible-caption property as well as keyed controls, stable automation ids, and handled press delivery.

pickLibraryFolder is the existing UI IO boundary moved to its lower reusable home. It resolves Some start folders through StorageProviderExtensions.TryGetFolderFromPathAsync, opens a single-folder picker, elevates the first selected local path immediately, and posts the callback on the UI thread. Cancel or an unavailable provider leaves the caller unchanged.

Tests substitute only Avalonia's storage boundary. LibraryPickerStorageTestProxy derives from System.Reflection.DispatchProxy and creates runtime IStorageFolder and IStorageProvider doubles without declaring their prohibited client implementations in F# source. Its callback is configured by the factory; unexpected storage calls identify their method. The folder fixture supplies its path and metadata, and the provider fixture captures lookups and options and returns completed tasks. No package or reference changes are needed.

LibraryPickerTestAppBuilder reuses TestAppBuilder and registers IStorageProviderFactory through the public AppBuilder.With API. Each LibraryPickerTestWindow owns its provider function. runLibraryPickerTest creates and disposes a HeadlessUnitTestSession for each picker fact, and AppContextTests belongs to an xUnit collection with DisableParallelization so those sessions cannot race the suite's assembly session. The production helper remains unchanged. Completed tasks and dispatcher draining provide deterministic execution without sleeps, native dialogs, or filesystem writes. Assertions inspect captured requests and callbacks outside the production catch boundary, and each window is closed.

AppContext.compose updates only exportFolderOpt on the constructed CatalogueImportContext. Both normal creation and library recreation receive Some locations.cataloguesFolder. The factory and all other compositions retain None. No durable format, serialized contract, schema, or migration path changes.

# Deferred

No implementation requirement was deferred. Required gate execution belongs to the supervisor and remains pending.

# Gotchas

- The original Library buttons explicitly set AutomationProperties.Name. The shared renderer preserves their accessible captions; this property is now also available to Materials and import-dialog verbs.
- Catalogue-folder registration intentionally supplies None for its start location. The app's configured suggestion is carried in its import context for export consumers.
- The pre-existing supervisor modification to specs/0050/.manifest.state.json was left untouched. No git mutation or version bump was performed.
- Source diffs show no carriage-return churn. All worker documents use the supplied canonical paths.
- The empty gates YAML block avoids inventing measured baselines; the supervisor owns those counts.
- The retry explicitly authorizes additional helper coverage in an already selected class. AppContextTests contains all three new picker facts, so the gate roster and project entries require no changes.
- AvaloniaLocator's mutable APIs appear in the runtime assembly but are private API and absent from the consumer reference surface. Use AppBuilder.With to configure this fixture's factory; runtime reflection alone does not establish compile-time availability.
- Avalonia's storage interfaces carry NotClientImplementable. The fixture uses runtime proxies only in tests; do not implement the reference-only sentinel members or alter package references to bypass the compiler errors. The DispatchProxy base must stay public, non-sealed and parameterless.

# Changelog

- 2026-11-07: Shared library-window verb and folder-picker helpers, wired the configured catalogue-export folder into app composition, and added the declared regression fact; handed off for supervisor verification.
- 2026-11-07 (attempt 02): Added controlled-provider coverage of the production folder picker for start-folder resolution, confirmed selection, cancellation, and unavailable-provider behavior; retained the AppContext fact and left gate execution to the supervisor.
- 2026-11-07 (attempt 03): Replaced unsupported locator calls with public app-builder provider registration, isolated headless sessions, and nonparallel test scheduling; preserved all assertions and left verification to the supervisor.
- 2026-11-07 (attempt 04): Replaced direct storage interface implementations with DispatchProxy test doubles; retained the four facts, behavioral assertions and production implementation, with verification reserved for the supervisor.
