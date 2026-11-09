# Step 0188 — state of the world

# Where we are

Step 0188 is in section S5.3 (the kit window: title, Kit menu, lock, reload and mode). It follows step 0187's
reload offer in the model. It renders the kit bar above the workbench in a view of its own, `KitWindowView`. The
bar shows:

- the kit's caption;
- the Kit menu: Show in Explorer, Reload changes, Close kit;
- the constructor and its switch;
- the reload offer, with Reload and Keep mine;
- the kit status.

Every control sends the semantic message steps 186–187 added. Step 189 composes `kitWindowView`, the real shell
and the activation check into the constructor windows.

# What's working

- Add the kit bar's ten `ConstructorUiId`s. `kitStatus` is now the kit bar's status line.
- Add `KitWindowView.kitWindowView`, the kit bar docked above `mainView`, with every control under its id.
- Offer Reload and Keep mine as one opposing pair on a row, positive and negative, parted by a gap.
- Show the kit status in the kit bar alone: the Experiments bay's builder no longer shows it.
- Add the declared `KitWindowTests` facts (four ui-tests, one ui-smoke) and the kit window's scroll-extent law.
- Render the caption row and the status row as members of the model, keeping the kit bar ELEV-clean.

# Tests

In the supervisor's attempt-01 run, build and review were green. `elevation` was red with two `ELEV013P`
findings in `KitWindowView.fs`. Attempt 02 moves those two rows onto the model with no behavioural change.

No gate was run by the worker (Invariant 6: the `IMPLEMENT` role acts only). The arc-runner runs the frozen
roster after exit:

- build and elevation;
- constructor-unit-tests (all discovered);
- ui-smoke (all discovered);
- ui-tests over `InverseConstructorTests`, `KitWindowTests`, `SceneSaveLoadTests` and `ScrollExtentTests`;
- the fourteen s21 invariants.

Expected counts:

- constructor-unit-tests unchanged at checkpoint 186's 1207;
- ui-smoke 227 → 228, adding `pressing the kit bar's buttons sends their messages`;
- the selected ui-tests classes +5:
  - `the kit window UiIds are the stable intent-named ids`;
  - `the kit bar shows the kit, its Kit menu and the constructor`;
  - `a changed kit offers Reload and Keep mine on one row, Keep mine to the right`;
  - `the kit status shows in the kit bar whichever bay is open`;
  - `the kit window: the kit bar and the page stay reachable`.

No test is removed or renamed. The YAML carries checkpoint 186's measured floors; the supervisor measures this
step's.

```yaml
gates:
  constructor_unit_tests: 1207
  ui_smoke_tests: 227
```

# Architecture

- **A new surface in its own file** (.spec-md §4). `KitWindowView.fs` compiles directly after `KitStartView.fs`. It
  reaches the workbench only through `mainView` and the workbench messages, so `TableAndElementRotationView.fs`
  does not absorb the bar.
- **The Kit menu is the bar's verb row, not an Avalonia `Menu`.** A popup cannot be proven headless; the element
  menu is a model-driven overlay for the same reason. The ribbon's bay order stays fixed.
- **The bar sits outside the page's `ScrollViewer`**, as the `DockPanel`'s top child, so it stays in view while
  the page scrolls. Everything in it wraps (the caption fill, the `WrapPanel`, the question), so it fits at
  800 × 600 and 1000 × 540.
- **One control carries `kitStatus`.** The builder's status line is deleted, not hidden. The status shows whichever
  bay is open.
- **A view helper over the model alone is a member of the model** (ELEV013). `kitCaptionRow` and `kitStatusRows`
  are `member internal` extensions in `KitWindowView.fs`, as `selectStatusRow` is in the workbench view. The rows
  that also take `dispatch` stay private module functions.

# Deferred

Nothing. Every requirement of the slice is in this round. Step 189 composes `kitWindowView`, the system explorer,
the window's close and the activation check into `MainConstructorWindow` and `InverseConstructorWindow`
(`Program.fs`). That work is outside this step's effects.

# Gotchas

- Until step 189 lands, the real constructor windows still mount `mainView`, so they show no kit status.
  `LauncherTests`' read-only acceptance expects `KitStatus` absent, and that still holds.
- `actionButton` subscribes its press once per id. Reload and Keep mine send constant messages, so this is safe.
  The mode switch is a `verbButton`, which re-subscribes on every patch.
- The offer block, the mode text and the status are keyed. When one appears or disappears, FuncUI recreates it
  instead of patching another control into it.
- `KitWindowTests` keeps its own recording shell. The step-186 one is private to `OpticalConstructor.Tests`, which
  `OpticalConstructor.Ui.Tests` does not reference.
- ELEV013 also binds private functions (`ELEV013P`). A private `let` whose only parameter is one of our types
  fails `elevation` exactly as a public one does.

# Changelog

- 2026-11-08 — attempt 01:
  - added the kit bar's ten ids and `KitWindowView.kitWindowView`;
  - moved the kit status from the Experiments bay's builder to the kit bar;
  - added the five declared `KitWindowTests` facts and the kit window's `ScrollExtentTests` fact.
- 2026-11-08 — attempt 02: cleared `elevation`'s two `ELEV013P` findings by moving `kitCaptionRow` and
  `kitStatusRows` onto `TableAndElementRotationModel` as internal extension members.
