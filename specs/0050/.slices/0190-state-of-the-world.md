# Step 0190 — state of the world

# Where we are

Step 0190 (`IMPLEMENT`) begins the "one window per kit" part of section S5.3 (001 §E, "Concurrency: one window per
kit"). The host-layer `WindowRegistry`, which already keeps Materials and Library single-instance, now has a key for
a constructor window: its kit's canonical folder. The start screen asks that registry first. If a window holds the
chosen kit, the screen brings it forward and opens nothing. Step 0191 registers the real constructor windows under
the key at the kit hand-off.

# What's working

- Add `WindowKey.KitWindowKey`: a constructor window, keyed by its kit's canonical folder.
- Add `KitWindowPresence`, `KitFolder.windowKey` and `KitFolder.activateKitWindow`. Activation runs over the
  existing registry and never builds a window.
- Start-screen opens (`CreateKit`, `OpenKit`, `OpenKitReadOnly`) first activate a window that holds the kit. In that
  case nothing is opened, bumped or handed over, and the offer and status are cleared.
- `KitStartWindow` closes the screen when it brings a kit's window forward. The root window factory refuses
  `KitWindowKey`.
- Add the facts `a kit whose window is open is activated, not opened, bumped or handed over` and
  `two spellings of one kit folder are one window key, and no window holds a new kit`.

# Tests

No gate verdict is claimed here (Invariant 6). The arc-runner runs the frozen roster after exit:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests over `OpticalConstructor.Ui.Tests.KitStartWindowTests`;
- the fourteen s21 invariants.

The worker's development runs are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0190-*.log`:

- build: 0 errors, no owned-code warning;
- constructor-unit-tests: 1209 passed, which is 1207 + 2, the two declared facts;
- ui-smoke: 230 passed;
- `KitStartWindowTests`: 6 passed;
- the whole non-smoke UI suite: 649 passed;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s21 invariant greps: none finds anything.

Expected counts:

- constructor-unit-tests +2 (1207 → 1209);
- ui-smoke unchanged at 230.

No test is removed or renamed, and no assertion is weakened. The YAML carries the latest recorded checkpoint's floors
(checkpoint 188); the supervisor measures this step's.

```yaml
gates:
  constructor_unit_tests: 1207
  ui_smoke_tests: 230
```

# Architecture

- **One registry.** Kit windows meet in the same `WindowRegistry` as every other managed window, through
  `WindowLauncher.create`. `KitFolder.activateKitWindow` is a view over that registry with a factory that answers
  `WindowNotRegisteredErr`, so it can only activate a window, never open one.
- **The key is the canonical folder.** `windowKey` is `KitWindowKey this.canonical` (step 181). Every spelling of one
  folder (slashes, a trailing separator, case) is one key, compared without touching disk.
- **The model stays window-free.** `KitStartView` reaches the registry only through
  `KitStartContext.activateKitWindow`. The window composes the real call and its own `Close`, and tests pass doubles.
  The open is a thunk, so an activated kit's folder is never read or locked.

# Deferred

Nothing. Every requirement of the slice is in this round. Registering the constructor windows under `KitWindowKey`
is step 0191's own scope, not deferred work.

# Gotchas

- Until step 0191 registers the constructor windows, `activateKitWindow` always answers `NoKitWindowOpen` in the
  app. The activated path is covered here through the start screen's injected context.
- `activateKitWindow` runs without an Avalonia platform: with no live key it only reads the empty registry map.
- An `Activate` that throws is `NoKitWindowOpen`. The open then meets this process's own lock and is offered
  read-only.
- `KitOpening.fs` qualifies `WindowLauncher.` and `WorkbenchSettings.` rather than opening them. This keeps their
  case names out of the module's scope.

# Changelog

- 2026-11-08 — attempt 01:
  - `KitWindowKey` and `KitFolder.windowKey` / `activateKitWindow` over the existing window registry;
  - the start screen activates a kit's open window instead of opening the kit, and closes;
  - the root factory refuses `KitWindowKey`;
  - the two declared facts.
