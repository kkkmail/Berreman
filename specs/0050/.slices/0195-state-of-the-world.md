# Step 0195 — state of the world

# Where we are

Step 0195 (`WIRE_UI`) is in section S5.3. It wires the Launcher's footer to step 194's Data locations window, as 001
§O asks ("A footer link opens Data locations") and .spec-md §10.1 states. The link opens the single-instance window
under `DataLocationsWindowKey` over the launcher's configured locations and the system explorer. It is enabled in
every outcome, a library that failed to open included, because the locations do not depend on the library.

# What's working

- Add `ConstructorUiId.launcherDataLocationsLink` to the launcher group.
- The Launcher's footer row gains a link-styled `Data locations` button on its left, beside the unchanged version line.
- The link opens `DataLocationsWindow(locations, ExplorerProxy.system)` through `WindowLauncher`. A second click brings
  the open window forward.
- The link is enabled whether the library opened or not.
- Add the ui-smoke facts `acceptance: the Data locations link opens one Data locations window listing the configured
  locations` and `the Data locations link stays enabled when the library fails to open`.

# Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests and ui-smoke (all discovered);
- ui-tests over `LauncherTests`;
- the fourteen s21 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0195-*.log`:

- build: 0 errors, and no warning from our code (only the vendored MathNet's `SYSLIB0051`);
- constructor-unit-tests: 1215 passed, unchanged against checkpoint 193;
- ui-smoke: 236 passed. That is 234 + 2, against checkpoint 193;
- ui-tests over `LauncherTests`: 8 passed;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s21 invariant greps: none finds anything.

No test is removed or renamed, and no assertion is removed. Three existing structure facts set the footer link aside
by its id (Gotchas). The YAML carries the latest recorded checkpoint floors; the supervisor measures this step's own.

```yaml
gates:
  constructor_unit_tests: 1215
  ui_smoke_tests: 234
```

# Architecture

- **One launcher record, any outcome.** `LauncherWindow` builds its Data locations `WindowLauncher` once. A Browse open
  goes over the host registry, so the link and any other opener of `DataLocationsWindowKey` meet in one window.
- **The link is outside the surface buttons.** It is never disabled and never goes through `surfaceButton`. A
  library-open failure still disables only the four surfaces and Review imports.
- **The footer is a row.** The bottom-docked `DockPanel` holds the link on the left and the version line filling the
  rest. The version line's properties are unchanged.

# Deferred

Nothing. Every requirement of the slice is in this round.

# Gotchas

- `LauncherTests.buttons` sees every `Button`, the footer link included. A fact about the four surfaces uses
  `surfaceButtons`, which sets aside only `launcherDataLocationsLink`.
- The window registry is app-global. A fact that clicks the link must close the window it opened and pump the close.
- No fact presses Open folder through the launcher, because the launcher's explorer is the system's. The in-memory
  explorer is exercised by `DataLocationsWindowTests`.

# Changelog

- 2026-11-09, attempt 01: the launcher's Data locations footer link, its id, and the two declared facts. Three
  existing launcher structure facts now set the link aside.
