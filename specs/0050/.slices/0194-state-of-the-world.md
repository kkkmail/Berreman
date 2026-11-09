# Step 0194 — state of the world

# Where we are

Step 0194 (`IMPLEMENT`) is in section S5.3. It hosts step 193's Data locations screen in its own window, as 001 §G
asks. The window is single-instance in the host registry under `DataLocationsWindowKey`, as Materials and Library
are. It is fitted to its screen and has joined `ScrollExtentTests` (.spec-md §4). Step 0195 wires the Launcher's
footer link to it.

# What's working

- Add `DataLocationsWindow`, a `HostWindow` titled `Data locations` that runs the Data locations screen. It is fitted
  to its screen and carries the `DataLocationsWindow` id.
- Add `WindowKey.DataLocationsWindowKey` and `ConstructorUiId.dataLocationsWindow`.
- The root window factory answers the new key, and its ui-smoke sweep now opens and renders the window.
- Add the facts `the Data locations window is titled and carries its stable id`,
  `the Data locations window opens once through the registry` (ui-smoke) and
  `the Data locations window leaves everything reachable`.

# Tests

No gate verdict is claimed here (Invariant 6). After exit, the arc-runner runs the frozen roster:

- build and elevation;
- constructor-unit-tests and ui-smoke (all discovered);
- ui-tests over `DataLocationsWindowTests` and `ScrollExtentTests`;
- the fourteen s21 invariants.

The worker's development runs over the final tree are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0194-*.log`:

- build: 0 errors, and no warning from our code (only the vendored MathNet's `SYSLIB0051`);
- constructor-unit-tests: 1215 passed, unchanged against checkpoint 192;
- ui-smoke: 234 passed. That is 233 + 1, against checkpoint 192;
- ui-tests over the two selected classes: 15 passed. That is 13 + 2;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s21 invariant greps: none finds anything.

No test is removed or renamed, and no assertion is weakened. The YAML carries the latest recorded checkpoint floors;
the supervisor measures this step's own.

```yaml
gates:
  constructor_unit_tests: 1215
  ui_smoke_tests: 233
```

# Architecture

- **The window is a thin host.** All its behaviour is the step-193 screen. The window adds only its title, ids, size,
  `WindowFit.keepOnScreen` and the Elmish host, as `KitStartWindow` does.
- **The init is the member form.** It is `context.initDataLocations` over a typed `DataLocationsContext`, after step
  193's ELEV013 repair.
- **Single-instance comes from the registry.** The window holds no state of its own. `WindowLauncher.create` over the
  host registry activates the live window on a second open.

# Deferred

Nothing. Every requirement of the slice is in this round. The Launcher link is step 0195's own slice.

# Gotchas

- The registry is app-global. A fact that opens `DataLocationsWindowKey` must close the window and pump the close.
  Otherwise the root-factory sweep would get `ActivatedWindow` and fail.
- The composition factory reuses the module's `owner` (`FolderName "WireUiFinalCompositionTests"`), and does not
  write the constant a second time.
- `WireUiFinalCompositionTests.rootWindowFactory` is the only exhaustive match over `WindowKey`. A new key must be
  added there too.

# Changelog

- 2026-11-09, attempt 01: the Data locations window, its key and id, the root-factory case, and the three declared
  facts.
