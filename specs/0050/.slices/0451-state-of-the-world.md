# Where we are

Step 0451 hosts step 450's Online sources surface in its own window, spec 0050 S13.1, obligation S131-SOURCES-WINDOW.
`MaterialSourcesWindow` is single-instance under `MaterialSourcesWindowKey`, fits its screen, and joins the
`ScrollExtentTests` law. Step 450 built the surface it hosts (`MaterialSourcesView`). Step 452 opens the window from the
Materials window.

# What's working

- Added `MaterialSourcesWindow`, titled `Online sources`, with `materialSourcesWindow` as its `Name` and
  `AutomationId`. It opens at 720 × 640 and is kept on screen.
- The window runs the Online sources surface with Update snapshot on the UI-thread job host. Closing it cancels a
  running update.
- Added `WindowKey.MaterialSourcesWindowKey` and `ConstructorUiId.materialSourcesWindow`. The composition test's root
  factory answers the new key.
- Added `MaterialSourcesWindowTests`, with three ui-tests facts and two ui-smoke facts: Search then Pull, and
  open-once.
- Added the `ScrollExtentTests` fact `the Online sources window leaves everything reachable`.

# Tests

These are development checks; the gate engine runs the roster after this session and is the authority.

- **build:** exits 0 in 85 s, against a 600 s deadline, with 0 errors. The warnings are the exempt NU1902 and the
  vendored MathNet's SYSLIB0051, which predates this step.
- **constructor-unit-tests, the whole suite as the gate selects it:** `Passed!`, 1801 of 1801, in 104 s against a 180 s
  deadline. The count is unchanged.
- **ui-smoke, the whole suite as the gate selects it:** `Passed!`, 266 of 266, in 216 s against a 600 s deadline. That
  is checkpoint 449's 264 plus 2, the step's two smoke facts.
- **ui-tests, filtered to `MaterialSourcesWindowTests` and `ScrollExtentTests`:** `Passed!`, 24 of 24, in 26 s against
  a 360 s deadline. The step's four new identities are among them.
- **elevation, from the arc origin:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, in 227 s against a 600 s
  deadline.
- **The twenty-one s43 invariants:** each decoded `git grep`, run with `--untracked`, finds nothing.
- No development run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1801
  ui_smoke_tests: 266
  ui_tests: 24
```

# Architecture

- **The window is a thin composition root, as `MaterialsWindow` and `KitStartWindow` are.**
  - It builds `MaterialSourcesContext` from its two constructor arguments and `SourceSnapshotJobHost.onUiThread`.
  - The model, update and view are step 450's, unchanged.
- **The `Closed` hook is the one cancel path on close.** It sends the same semantic `CancelSourceSnapshot` message the
  Cancel button sends.
- **Single-instance comes from the launcher,** under the new `WindowKey` case, as for the Data locations window.
- No stored format, schema, package, gate or analyzer configuration changed. R0001–R0005 are untouched.

# Deferred

Nothing.

# Gotchas

- **The update callbacks are lambdas that read `dispatch` when called.**
  - The how_to's `SourceSnapshotProgressed >> dispatch` would bind the pre-init `ignore`.
  - The lambdas follow `KitStartWindow`'s `archiveJobs`.
- **The Online sources fixture is `module internal MaterialSourcesWindowFixture` in `ScrollExtentTests.fs`.**
  `ScrollExtentTests` compiles before `MaterialSourcesWindowTests`, so both build the window over one fixture.
  - It is a source named `Test source` holding Silicon's record (`main/Si/nk/Aspnes.yml`) at `2026-11-01`.
  - Its import context is `CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy` offering that
    source. The Built-in library is not imported, and the pull still lands as version 1.
- **No fact presses Update snapshot.** Cancel-on-close is the window's `Closed` hook plus step 450's model facts.
  Adding a fact for it would be a new test identity outside the census.
- **Step 452's opener supplies `pullsApplied`,** so the Materials window re-renders what a pull added. Here the root
  factory and the reachability fact pass `ignore`.

# Changelog

- 2026-11-17, step 0451: added `MaterialSourcesWindow` (Online sources) with its `WindowKey` and `ConstructorUiId`, and
  the root factory's new key. Added `MaterialSourcesWindowTests` (3 ui-tests facts, 2 ui-smoke facts) and the Online
  sources reachability fact. ui-smoke goes from 264 to 266.
