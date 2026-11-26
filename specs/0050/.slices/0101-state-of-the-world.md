# Where we are

Slice 0101 wires S31-LIBRARY-REGISTER-UI into the existing Library window using the catalogue import context delivered by slices 0099–0100. Browse and Select windows receive that context from their owning application scope. Registration retains every pending decision; unregister removes only the registration log row. The implementation and its six named acceptance assertions are complete; supervisor acceptance and commit remain external.

# What's working

- Added catalogue-folder registration, live registered-folder rows, unregister controls and status with stable automation IDs.
- Retained every import row, including Reuse and name collisions, for explicit confirmation without library or log writes.
- Threaded the shared catalogue import context through the launcher, both constructors, workbench Browse/Select opens and test fixtures.
- Added five pure facts and one real-window unregister smoke assertion while preserving all existing tests.

# Tests

- Final Release solution build passed in 12.96 seconds with zero warnings and zero errors. The initial build reported two existing SYSLIB0051 warnings in vendored MathNet; no vendored source was changed.
- Elevation passed over five affected/dependent projects with zero findings, ELEV004 excluded, in 136 seconds.
- All 211 ui-smoke cases passed in 65.072 seconds, zero failures/skips; the count rises by one from the preceding slice's 210.
- All 114 selected ui-tests cases passed in 31.704 seconds, zero failures/skips, across the eight classes in the resolved slice selection. Five regular facts were added and none removed or renamed.
- All six exact new acceptance IDs passed in TRX. All eleven s12 invariants, source LF and whitespace checks passed.

These are measured local results. Supervisor acceptance baselines and checkpoints were not edited.

```yaml
gates:
  ui_smoke_tests: 211
  ui_tests: 114
```

Evidence is under `specs/0050/.artifacts/`: `0101-local-build.log`, `0101-final-build.log`, `0101-local-elevation.log`, `0101-elevation-reports/`, `0101-local-ui-smoke.log`, `0101-local-ui-tests.log`, `0101-tests/`, `0101-local-invariants.log` and `0101-test-coverage.log`.

# Architecture

`LibraryWindowContext` and `TableAndElementRotationModel` now carry `CatalogueImportContext`. `EditorLaunchers` and `LibraryWindow` accept it directly after the backup store, and default scenes retain the context used to populate their stores. The production composition root supplies `context.catalogueImports` throughout.

`RegisterCatalogueFolder` calls the existing inspection member and appends complete pending reports. `UnregisterCatalogueFolder` resolves the folder from registered rows before calling `forgetImport`, so its success caption names the removed row while library entries remain intact. Both preserve existing confirmation gates. The folder picker is a UI IO boundary and cancelled or unavailable providers leave the model alone. Pointer and ordinary verbs share one styling body.

No domain decision algorithm, durable contract, database schema or migration changed. Runtime constructor/context shapes replace their previous signatures in the same change.

# Deferred

None within slice 0101. Its specified output is the complete pending row list for the separately scoped import-dialog consumer; no automatic settlement was introduced.

# Gotchas

- Already-registered fixtures must settle every pending row explicitly; calling registration alone leaves both the library and import log unchanged.
- Re-registering after unregister still holds Reuse rows for confirmation. The pure fact checks that no new log row appears before settlement.
- A missing unregister row or failed row lookup produces a typed window refusal, without guessing the folder name or deleting entries.
- Fixture folders live under the test output's `LibraryWindowTests` directory. The real-window smoke uses `TestAppScope.fileLocations` and the app's actual database stores.
- Higher-priority session instructions require local verification despite the worker protocol's act-only wording. Formal supervisor acceptance remains external.
- The pre-existing supervisor edit to `.manifest.state.json` was left untouched. No Git mutation, version bump, analyzer-policy edit, baseline edit or external-repository change occurred.

# Changelog

- 2026-11-06: Wired catalogue-folder registration and unregister across Library launch paths; added six acceptance facts and verified the build, elevation, 211 smoke cases, 114 selected UI cases and eleven section invariants.
