# Where we are

Slice 0088 wires the S2.4 app composition to the configured SQLite library and
propagates library-open failures to the product launcher. Implementation and the
three declared regression facts are complete, with all prescribed local checks
passing. Formal acceptance remains with the arc-runner supervisor.

## What's working

- Compose app library stores over one configured SQLite database handle.
- Import the shipped Built-in catalogue only when creating a library.
- Return library-open errors unchanged and show them on the launcher.
- Disable all four launcher actions without registering click handlers on failure.
- Move app-scope test composition to SQLite and add the three required regression facts.

## Tests

- Final Release solution build: zero warnings/errors, 11.78 seconds. The initial
  full compilation reported two existing SYSLIB0051 warnings in vendored MathNet
  Exceptions.cs; neither product nor test F# code produced warnings.
- Elevation: zero findings across all five affected product projects, 137 seconds,
  with ELEV004 excluded as required.
- UI smoke: all 208 passed, no failures/skips, 40.464 seconds.
- Selected UI tests: all 40 passed, no failures/skips, 5.697 seconds. The three
  declared regression facts raise this selection from 37 to 40.
- All eleven s11 invariant commands from the resolved gate snapshot passed.
- Owned source diffs have no whitespace errors or CRLF churn.

These are observed local counts; the supervisor owns baseline and checkpoint
updates. The ui-smoke count matches its prior recorded checkpoint.

```yaml
gates:
  ui_smoke_tests: 208
  ui_tests: 40
```

Logs, TRX results, analyzer reports and invariant results are under
`specs/0050/.artifacts/0088-*`. The initial launcher-test failure is retained:
its broad Button count included four scrollbar-template buttons. Removing that
redundant assertion preserved all four semantic-ID, disabled-state and inert-Click
checks; the corrected fact passes. No production code changed after the successful
elevation and smoke runs.

## Architecture

AppContext.create returns Result<AppContext, StorageError> from the existing
LibraryDatabase.openOrCreate boundary. Its five library stores capture the open
database; CreatedLibraryDatabase triggers CatalogueImportProxy.ofStores over the
configured shipped catalogue, while OpenedLibraryDatabase skips import. Import
errors retain the prior logging and partial-import policy. The experiment,
collection, scene, export and settings seams retain their existing composition.

LauncherWindow consumes that result. Error text carries the centralized
ConstructorUiId.launcherLibraryErrorText identity and accessible name. A vertical
scroller keeps wrapped diagnostics and the four buttons reachable in the fixed
window. Successful startup retains the existing window and editor launchers.

## Deferred

No slice implementation or local verification requirement is deferred. The
supervisor runs formal acceptance afterward.

## Gotchas

- Keep the database alive through the composed proxy closures; a local use binding
  in AppContext.create would dispose it before any window could use the stores.
- File fixtures isolate writable locations under a GUID directory in the test
  output. Their shipped catalogue remains the shared read-only output asset.
- The schema-mismatch regression checks unchanged file bytes and the original
  incompatible stamp; the launcher regression raises Click on all disabled buttons.
- Existing composition fixtures already call TestAppScope.freshAppContext and
  need no edits solely to pick up database storage.
- The supervisor-owned manifest-state modification is untouched. No versions,
  schema migrations, analyzer settings or other repositories were changed.
- Local checks follow the coding environment's higher-priority verification
  requirement; they do not replace the supervisor's acceptance or checkpoints.

## Changelog

- 2026-11-06: Wired database app startup, created-only catalogue import, launcher error handling and isolated database composition tests; final build, elevation, 208 smoke tests, 40 selected UI tests and eleven invariants passed locally.
