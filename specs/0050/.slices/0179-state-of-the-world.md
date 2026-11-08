# Step 0179 — state of the world

# Where we are

Spec 0050 section S5.2 builds the kit start screen and the Launcher. Steps 176–178 built the screen's model, update and
view (`KitStartView`). This step adds its window, `KitStartWindow`, which joins them to the app scope:

- it composes the screen's `KitStartContext` from `AppContext`;
- it is titled by its mode and fits its screen;
- it hands each opened kit through `OpenedKit.resolveAndShow` to the constructor of its mode, then closes itself.

The window joins the `ScrollExtentTests` law. Later S5.2 steps make the Launcher open this window in place of the
Launcher's own folder pick.

# What's working

- Add `KitStartWindow`, a `HostWindow` titled `Open a kit — Main` / `Open a kit — Inverse`, with id `KitStartWindow`.
  It asks for 720 × 560 and calls `WindowFit.keepOnScreen`.
- Compose the screen's context from the app scope: history, history limit, system clock, user author, catalogue imports
  and the window's picker.
- Hand an opened kit through `resolveAndShow`. A shown kit opens the constructor of the screen's mode and closes the
  screen; a failed re-binding is shown on the screen; a cancelled dialog keeps the screen open.
- Add `ConstructorUiId.kitStartWindow` to the start-screen id group.
- Test that New kit opens the screen's constructor, bumps the history and closes the screen (ui-smoke), that the title
  and stable id hold, and that the forty-kit screen stays reachable at both small sizes.

# Tests

Local runs only (development evidence; the gate engine decides):

- **build**: green, 0 warnings from our code.
- **elevation**: `ELEV-GATE: OK`, 0 new findings over all 12 projects, ELEV004 excluded.
- **constructor-unit-tests**: 1186 of 1186 passed, unchanged.
- **ui-smoke**: 227 of 227 passed, up 1 with the new acceptance fact.
- **ui-tests** (`KitStartWindowTests` and `ScrollExtentTests`): 16 of 16 passed, up 2 with the title/id fact and the
  scroll-extent fact.
- **s20 invariants**: all fourteen find nothing.

```yaml
gates:
  constructor_unit_tests: 1186
  ui_smoke_tests: 227
  ui_tests: 16
```

# Architecture

- `KitStartWindow.fs` is the Ui project's last compiled file, after `AppContext.fs`, because the window reads the app
  scope. It is the screen's composition root, as `LibraryWindow` is the Library's. The pure model, update and view stay
  in `KitStartView`.
- The constructor to open is a seam: `showConstructor : ConstructorMode -> OpenedKit -> unit`. The window creates no
  constructor window itself, so a test records the hand-off and the composition root (the Launcher) decides which window
  to show. The picker is a seam too, `pickerFor : TopLevel -> KitFolderPicker`, as the Launcher's is.
- The window's title is an inline match on `ConstructorMode`, which has no render member. `TableAndElementRotationView.fs`
  was outside this step's edits.

# Deferred

Nothing. Every requirement of the step landed this round.

# Gotchas

- `open System` in `namespace OpticalConstructor.Ui` shadows the Ui `AppContext` with `System.AppContext`. Qualify
  `System.Environment` instead.
- The hand-off runs inside the Elmish update, so a kit without snapshot rows closes the window mid-update. The next
  render lands on a closed host, as with `LibraryWindow`'s `requestClose`.
- The Button clicks on release, so the acceptance fact sees the screen close during `pressAt`'s `MouseUp`. The fact
  closes any still-visible window in its `finally`.
- Each acceptance screen gets its own fresh app scope and in-memory history, so "the history's one entry" is that
  screen's folder.

# Changelog

- 2026-11-08 — step 0179: `KitStartWindow` and `ConstructorUiId.kitStartWindow`, with three new facts: the scroll-extent
  law, the title and id, and the ui-smoke New kit acceptance.
