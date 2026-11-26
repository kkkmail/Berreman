# Step 0180 — state of the world

# Where we are

Spec 0050 section S5.2 builds the kit start screen and the Launcher. Steps 176–178 built the screen's model, update and
view, and step 179 its window, `KitStartWindow`. This step wires the window into the Launcher:

- Main and Inverse each open one start screen of their mode;
- the chosen kit opens the matching constructor window through `KitConstructorWindows.showConstructorIn`;
- the Launcher's own kit opening is deleted (001 §F, §O; .spec-md §4).

Materials, Library, the library error, Recreate library and the pending-imports notice are unchanged.

# What's working

- Wire the Launcher's Main and Inverse to open the kit start screen of their mode.
- Move the constructor-window match into `KitConstructorWindows.showConstructorIn`, the start screen's
  `showConstructor`.
- Delete the Launcher's kit opening: its picker call, error line, and Open read-only / Cancel row.
- Retire `launcherKitErrorText`, `launcherOpenKitReadOnlyButton` and `launcherCancelKitOpenButton`.
- Drive the ten launcher kit facts through the start screen's New kit…, Open kit… and offer boxes, and rename the error
  fact.
- Render the start screen in both modes in the root-composition smoke fact.

# Tests

No build, test, analyzer or invariant gate was run by this worker: the WIRE_UI protocol gives gate execution to the
gate engine. The engine runs:

- build and elevation;
- constructor-unit-tests and ui-smoke, all discovered;
- ui-tests, selected to `LauncherTests`;
- the fourteen s20 invariants.

Checked without running a gate:

- the step's `git grep` for the three retired ids finds nothing;
- every edited file is LF.

Test identities:

- one ui-smoke fact is renamed (`… is shown on the start screen and opens no window`);
- ten facts change expectation;
- none are added or removed, so the ui-smoke count is unchanged.

The counts below are inherited from checkpoint 178. They were not measured this round, and no `ui_tests` baseline is
invented.

```yaml
gates:
  constructor_unit_tests: 1186
  ui_smoke_tests: 227
```

# Architecture

- The Launcher is the composition root of both start screens. It hands each one the app scope, the mode, its own picker
  seam and `KitConstructorWindows.showConstructorIn context`. The start screen stays free of constructor types, as step
  179 intended.
- `KitConstructorWindows` is a private module in `Program.fs`, because only the App project knows `MainConstructorWindow`
  and `InverseConstructorWindow`.
- The launcher facts observe windows through one shared `windowsOpenedBy`, which `clickAndObserve` and the new
  `pressAndObserve` both use.

# Deferred

Nothing. Every requirement of the step landed this round.

# Gotchas

- The start screen's offer and status come from step 178's view. It sets only AutomationIds, and its offer pair are
  `actionButton` Borders spaced by margin, not row spacing. So the held-kit acceptance checks AutomationIds, the boxes'
  texts and the margin gap. The launcher's `Name`, `AutomationProperties.Name` and `Spacing = 12` checks have no
  counterpart there.
- The start screen closes itself during the press that opens a kit, inside the Elmish update. The facts check
  `IsVisible`, and close only a still-visible screen in `finally`.
- The snapshot fixture's window record includes the start screen, so its counts are one higher than before.

# Changelog

- 2026-11-08 — step 0180: Main and Inverse open `KitStartWindow`. The launcher's kit opening and its three ids are gone.
  The launcher kit facts go through the start screen, and the composition fact renders it in both modes.
