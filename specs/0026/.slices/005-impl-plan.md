# 005 — Impl plan: centralized commands, mouse/keyboard interaction & the constructor MVU surface

## Approach

This is Part E (the centralized command model + mouse/keyboard) plus the FuncUI
`Canvas` MVU page (`ConstructorView.fs`) and the Part C view interactions that depend
on it (pan/zoom/reset, table selection). The work is four production edits and two
test edits.

1. **`Commands.fs` (new)** — the single command registry (AC‑E1, constraint 0.4).
   - A `Command` DU (one case per command), a `CommandScope` (active‑element / table /
     global), and the three‑state `Binding` (`KeyboardOnly` / `MouseOnly` /
     `KeyboardAndMouse`) — so a command reachable both ways declares both bindings on
     one definition (`SlideAlongRay` is the canonical key‑plus‑mouse case).
   - Pure, comparable `KeyGesture` / `MouseGesture` value shapes (no Avalonia type) so
     the key map and mouse map are plain `Map` lookups projected from the one registry.
   - `parseKeyGesture` / `formatKeyGesture` and `withKeyOverrides` so the configurable
     key map (E.8.1) is applied over the default registry without a second wiring site.
   - The registry stays persistence‑agnostic (string‑keyed overrides) so it has no
     dependency on `UserEnvironment`.

2. **`UserEnvironment.fs` (edit)** — add the configurable key map (E.8.1).
   - A pure, serializable `KeyMap` (`rotationStepDegrees` defaulting to 5° per E.3.1 +
     a `KeyBindingOverride list`) and a `keyMap` field on `EnvironmentSettings`, plus
     `defaultKeyMap`. Reuses the slice‑003 `ProjectJson.options`; persists with the
     settings store (no parallel preferences file). Only `defaults` is a full literal,
     so it is the sole construction site to extend.

3. **`optical-constructor-environment.schema.json` (edit)** — add the `keyMap` field to
   the root `required` + `properties` and the `keyMap` / `keyBindingOverride` `$defs`,
   mirroring how slice 003 added `language`.

4. **`ConstructorView.fs` (new)** — the FuncUI `Canvas` MVU page.
   - Pure, serializable `Model` (the canonical `OpticalConstructorProject` + the
     ephemeral `Table.TableViewState` + selection + the configured `KeyMap` + transient
     drag/menu/confirm state — no Avalonia handle, matching `Shell.RootModel`), a pure
     `Msg`, and a pure `update` (Avalonia‑free, like `ConstructionPage`), so every AC is
     provable by dispatching messages — headless pointer events do not fire.
   - Implements active‑element commands (E.2), rotations with lock inertness (E.3),
     slide/reassign/plain‑drag‑inert + hint (E.4), drag‑to‑place snap (E.7), table/view
     commands (E.5), global commands (E.6), and the Part C interactions (pan/zoom/reset,
     table selection). Rotations reuse `Placement.withR1/withR2/withR3` (lock‑respecting);
     reset view reuses `Table.resetView`; geometry reuses `ConstructorTable.project` /
     `Drawer`. The view re‑renders on every model change, so the table redraws on drop
     (E.4.3) for free.
   - The Avalonia `view` translates pointer/wheel/key events into the pure `Msg`s via the
     registry; brushes are built on the UI thread inside the view.

5. **Tests** — `CommandRegistryTests.fs` (new, `ui-tests` + one `ui-smoke` mount) covers
   AC‑E1..E5, AC‑C2, AC‑C3 against the registry and `update`; a focused key‑map
   round‑trip is added to the existing `EnvironmentRoundTripTests.fs` (`constructor-unit-tests`).

## Files

- New: `OpticalConstructor.Ui/Commands.fs`, `OpticalConstructor.Ui/ConstructorView.fs`,
  `OpticalConstructor.Ui.Tests/CommandRegistryTests.fs`.
- Edit: `OpticalConstructor.Ui/UserEnvironment.fs`,
  `OpticalConstructor.Ui/optical-constructor-environment.schema.json`,
  `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` (compile Commands + ConstructorView),
  `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` (compile the test),
  `OpticalConstructor.Tests/EnvironmentRoundTripTests.fs` (key‑map round‑trip).

## Risks

- AC‑E1 is the constraint‑0.4 lynchpin: both the key map and the mouse map MUST be
  projected from the one registry with no second binding site. Mitigated by making the
  registry the sole source of the `Map` lookups and asserting that in the tests.
- Headless pointer/wheel/key events do not fire, so the interaction ACs are proved
  against the pure `update` (dispatch the `Msg` directly), with a single `ui-smoke`
  test that the view mounts and renders one frame.
- Adding a field to `EnvironmentSettings` must keep the existing env round‑trip green
  (only `defaults` is a full literal; everything else is `{ defaults with … }`).
- EOL: the editor emits CRLF on this host; the repo is LF — normalize touched files.
