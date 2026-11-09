# Step 0189 — state of the world

# Where we are

Step 0189 (`WIRE_UI`) closes the kit-window part of section S5.3 (the kit window: title, Kit menu, lock, reload and
mode). Steps 0183–0188 built the pieces:

- the explorer seam and the Kit menu's messages;
- the outside-change check and the reload offer in the model;
- the kit bar view, `KitWindowView.kitWindowView`.

This step composes those pieces into the two real constructor windows, `MainConstructorWindow` and
`InverseConstructorWindow`, through `ConstructorScene.mount` (`Program.fs`). Each window now:

- shows the kit bar above the workbench;
- reaches the system explorer and its own close;
- checks its kit for files changed outside the app on every activation.

# What's working

- Run the constructor windows' Elmish loop with `Program.mkProgram`, storing the dispatch from the init Cmd.
- Give each constructor window the real Kit menu shell: `ExplorerProxy.system` and the window's own `Close`.
- Mount `KitWindowView.kitWindowView`, so the kit bar shows above the workbench. The title still follows the model.
- Check the kit for outside changes on every window activation (`CheckKitChanges KitCheckOnActivation`).
- Add the `KitWindowTests` smoke facts: activation offers an outside-written `E2` and Reload reads it in; Close kit
  closes the window and releases the kit.

# Tests

No gate verdict is claimed (Invariant 6: the `WIRE_UI` role acts only). The arc-runner runs the frozen roster after
exit:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests over `AppContextTests`, `KitWindowTests`, `LauncherTests` and `TableAndElementRotationTests`;
- the fourteen s21 invariants.

The worker's local development runs are in the impl-log's `Testing state`, with logs under
`specs/0050/.artifacts/0189-*.log`:

- build: 0 errors, no owned-code warning;
- constructor-unit-tests: 1207 passed;
- ui-smoke: 230 passed;
- the four selected ui-tests classes: 54 passed;
- the whole non-smoke UI suite: 649 passed;
- elevation from the arc origin: `ELEV-GATE: OK`, 0 new findings in 12 of 12 projects;
- the fourteen s21 invariant greps: none finds anything.

Expected counts:

- constructor-unit-tests unchanged at 1207;
- ui-smoke +2 over the prior checkpoint (228 → 230):
  - `acceptance: activating a constructor window offers the kit files changed outside the app, and Reload reads them in`;
  - `Close kit closes the window and releases the kit`.

No test is removed or renamed. The YAML carries the latest recorded checkpoint's floors (checkpoint 187); the
supervisor measures this step's.

```yaml
gates:
  constructor_unit_tests: 1207
  ui_smoke_tests: 228
```

# Architecture

- **The window is the composition root of the kit window.** `ConstructorScene.mount` is the only place that pairs
  the pure workbench model with:
  - the real explorer (`ExplorerProxy.system`);
  - the real close (`window.Close`);
  - the host's `Activated` event.

  The model and `kitWindowView` stay window-free. Tests that mount the view directly keep
  `KitWindowShell.inert` or a recording shell.
- **The dispatch reaches the host through a `ref` cell** filled by the init `Cmd.ofEffect`, as `LibraryWindow`
  fills its field. `Program.run` is synchronous, so the cell is set before the window can be shown or activated.
- **The kit is checked on activation, and only on activation.** No timer, file watcher or polling is added. The
  first activation (on `Show ()`) finds nothing, because opening the kit recorded its stamps (step 131).
- **Close kit stays a model message.** Its shell closes the window, and the window's existing `Closed` handler
  (step 154) releases the kit. There is still exactly one release path.

# Deferred

Nothing. Every requirement of the slice is in this round.

# Gotchas

- In headless Avalonia, `Window.Activate ()` raises `Activated` even when the window was never deactivated. The
  acceptance fact still shows and activates a second window first, as the slice describes.
- A press that closes its window must not be followed by a headless `MouseUp`, because the closed window has no
  platform impl. `KitWindowTests.pressAtCentre` and `LauncherTests.clickBorder` both skip the release once the
  window is no longer visible.
- The ribbon's tabs carry their id as `Name`, not as an AutomationId. AutomationId lookups (`controlById`) do not
  find them; `pressRibbonTab` finds them by `Name`.
- The default Main scene's source is unbound and varies nothing, so committing an experiment over it commits
  nothing. A test that needs a committed experiment in a scratch `initMain ()` scene varies a polarizer added to
  the scene.
- The kit bar takes height above the page's `ScrollViewer`. No existing fact in the authorized files needed a
  change for it: every real-window press already brings its control into view, or sits in the ribbon at the top.

# Changelog

- 2026-11-08 — attempt 01:
  - `ConstructorScene.mount` runs the kit window: `mkProgram`, the real shell, `kitWindowView`, and the activation
    check;
  - the two declared `KitWindowTests` smoke facts;
  - `pressBox` skips the release after a press that closes the window.
